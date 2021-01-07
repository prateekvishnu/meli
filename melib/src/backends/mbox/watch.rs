/*
 * meli - mailbox module.
 *
 * Copyright 2017 - 2021 Manos Pitsidianakis
 *
 * This file is part of meli.
 *
 * meli is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meli is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */

use super::*;
use std::collections::BTreeSet;

#[derive(Debug)]
pub struct MboxWatcher {
    pub account_hash: AccountHash,
    pub event_consumer: BackendEventConsumer,
    pub mailbox_hashes: BTreeSet<MailboxHash>,
    pub mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
    pub mailboxes: Arc<Mutex<HashMap<MailboxHash, MboxMailbox>>>,
    pub polling_period: std::time::Duration,
    pub prefer_mbox_type: Option<MboxFormat>,
}

impl BackendWatcher for MboxWatcher {
    fn is_blocking(&self) -> bool {
        true
    }

    fn register_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
        _urgency: MailboxWatchUrgency,
    ) -> Result<()> {
        self.mailbox_hashes.insert(mailbox_hash);
        Ok(())
    }

    fn set_polling_period(&mut self, period: Option<std::time::Duration>) -> Result<()> {
        if let Some(period) = period {
            self.polling_period = period;
        }
        Ok(())
    }

    fn spawn(self: Box<Self>) -> ResultFuture<()> {
        let MboxWatcher {
            account_hash,
            event_consumer: sender,
            mailbox_hashes,
            mailbox_index,
            mailboxes,
            polling_period,
            prefer_mbox_type,
        } = *self;
        let (tx, rx) = channel();
        let mut watcher = watcher(tx, polling_period)
            .map_err(|e| e.to_string())
            .map_err(MeliError::new)?;
        for (_, f) in mailboxes
            .lock()
            .unwrap()
            .iter()
            .filter(|(k, _)| mailbox_hashes.contains(k))
        {
            watcher
                .watch(&f.fs_path, RecursiveMode::Recursive)
                .map_err(|e| e.to_string())
                .map_err(MeliError::new)?;
            debug!("watching {:?}", f.fs_path.as_path());
        }
        Ok(Box::pin(async move {
            loop {
                match rx.recv() {
                    /*
                     * Event types:
                     *
                     * pub enum RefreshEventKind {
                     *     Update(EnvelopeHash, Envelope), // Old hash, new envelope
                     *     Create(Envelope),
                     *     Remove(EnvelopeHash),
                     *     Rescan,
                     * }
                     */
                    Ok(event) => match event {
                        /* Update */
                        DebouncedEvent::NoticeWrite(pathbuf) | DebouncedEvent::Write(pathbuf) => {
                            let mailbox_hash = get_path_hash!(&pathbuf);
                            let file = match std::fs::OpenOptions::new()
                                .read(true)
                                .write(true)
                                .open(&pathbuf)
                            {
                                Ok(f) => f,
                                Err(_) => {
                                    continue;
                                }
                            };
                            get_rw_lock_blocking(&file, &pathbuf)?;
                            let mut mailbox_lock = mailboxes.lock().unwrap();
                            let mut buf_reader = BufReader::new(file);
                            let mut contents = Vec::new();
                            if let Err(e) = buf_reader.read_to_end(&mut contents) {
                                debug!(e);
                                continue;
                            };
                            if contents.starts_with(mailbox_lock[&mailbox_hash].content.as_slice())
                            {
                                if let Ok((_, envelopes)) = mbox_parse(
                                    mailbox_lock[&mailbox_hash].index.clone(),
                                    &contents,
                                    mailbox_lock[&mailbox_hash].content.len(),
                                    prefer_mbox_type,
                                ) {
                                    let mut mailbox_index_lck = mailbox_index.lock().unwrap();
                                    for env in envelopes {
                                        mailbox_index_lck.insert(env.hash(), mailbox_hash);
                                        (sender)(
                                            account_hash,
                                            BackendEvent::Refresh(RefreshEvent {
                                                account_hash,
                                                mailbox_hash,
                                                kind: RefreshEventKind::Create(Box::new(env)),
                                            }),
                                        );
                                    }
                                }
                            } else {
                                (sender)(
                                    account_hash,
                                    BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: RefreshEventKind::Rescan,
                                    }),
                                );
                            }
                            mailbox_lock
                                .entry(mailbox_hash)
                                .and_modify(|f| f.content = contents);
                        }
                        /* Remove */
                        DebouncedEvent::NoticeRemove(pathbuf) | DebouncedEvent::Remove(pathbuf) => {
                            if mailboxes
                                .lock()
                                .unwrap()
                                .values()
                                .any(|f| f.fs_path == pathbuf)
                            {
                                let mailbox_hash = get_path_hash!(&pathbuf);
                                (sender)(
                                    account_hash,
                                    BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: RefreshEventKind::Failure(MeliError::new(format!(
                                            "mbox mailbox {} was removed.",
                                            pathbuf.display()
                                        ))),
                                    }),
                                );
                                return Ok(());
                            }
                        }
                        DebouncedEvent::Rename(src, dest) => {
                            if mailboxes.lock().unwrap().values().any(|f| f.fs_path == src) {
                                let mailbox_hash = get_path_hash!(&src);
                                (sender)(
                                    account_hash,
                                    BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: RefreshEventKind::Failure(MeliError::new(format!(
                                            "mbox mailbox {} was renamed to {}.",
                                            src.display(),
                                            dest.display()
                                        ))),
                                    }),
                                );
                                return Ok(());
                            }
                        }
                        /* Trigger rescan of mailboxes */
                        DebouncedEvent::Rescan => {
                            for &mailbox_hash in mailboxes.lock().unwrap().keys() {
                                (sender)(
                                    account_hash,
                                    BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: RefreshEventKind::Rescan,
                                    }),
                                );
                            }
                            return Ok(());
                        }
                        _ => {}
                    },
                    Err(e) => debug!("watch error: {:?}", e),
                }
            }
        }))
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
