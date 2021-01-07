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
use crate::backends::{RefreshEventKind::*, *};
use std::collections::BTreeSet;
use std::ffi::OsStr;
use std::io;
use std::os::unix::fs::PermissionsExt;
extern crate notify;
use notify::{watcher, DebouncedEvent, RecursiveMode, Watcher};
use std::path::{Component, Path, PathBuf};
use std::sync::mpsc::channel;

#[derive(Debug)]
pub struct MaildirWatcher {
    pub account_hash: AccountHash,
    pub cache_dir: xdg::BaseDirectories,
    pub event_consumer: BackendEventConsumer,
    pub hash_indexes: HashIndexes,
    pub mailbox_hashes: BTreeSet<MailboxHash>,
    pub mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
    pub mailboxes: HashMap<MailboxHash, MaildirMailbox>,
    pub polling_period: std::time::Duration,
    pub root_path: PathBuf,
}

impl BackendWatcher for MaildirWatcher {
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
        let MaildirWatcher {
            account_hash,
            cache_dir,
            event_consumer: sender,
            hash_indexes,
            mailbox_hashes: _,
            mailbox_index,
            mailboxes,
            polling_period,
            root_path,
        } = *self;
        Ok(Box::pin(async move {
            let (tx, rx) = channel();
            let mut watcher = watcher(tx, polling_period).unwrap();
            watcher.watch(&root_path, RecursiveMode::Recursive).unwrap();
            debug!("watching {:?}", root_path);
            let root_mailbox_hash: MailboxHash = mailboxes
                .values()
                .find(|m| m.parent.is_none())
                .map(|m| m.hash())
                .unwrap();
            let mailbox_counts = mailboxes
                .iter()
                .map(|(&k, v)| (k, (v.unseen.clone(), v.total.clone())))
                .collect::<HashMap<MailboxHash, (Arc<Mutex<usize>>, Arc<Mutex<usize>>)>>();
            let mut buf = Vec::with_capacity(4096);
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
                        /* Create */
                        DebouncedEvent::Create(mut pathbuf) => {
                            debug!("DebouncedEvent::Create(path = {:?}", pathbuf);
                            if path_is_new!(pathbuf) {
                                debug!("path_is_new");
                                /* This creates a Rename event that we will receive later */
                                pathbuf = match move_to_cur(pathbuf) {
                                    Ok(p) => p,
                                    Err(e) => {
                                        debug!("error: {}", e.to_string());
                                        continue;
                                    }
                                };
                            }
                            let mailbox_hash = get_path_hash!(pathbuf);
                            let file_name = pathbuf
                                .as_path()
                                .strip_prefix(&root_path)
                                .unwrap()
                                .to_path_buf();
                            if let Ok(env) = add_path_to_index(
                                &hash_indexes,
                                mailbox_hash,
                                pathbuf.as_path(),
                                &cache_dir,
                                file_name,
                                &mut buf,
                            ) {
                                mailbox_index
                                    .lock()
                                    .unwrap()
                                    .insert(env.hash(), mailbox_hash);
                                debug!(
                                    "Create event {} {} {}",
                                    env.hash(),
                                    env.subject(),
                                    pathbuf.display()
                                );
                                if !env.is_seen() {
                                    *mailbox_counts[&mailbox_hash].0.lock().unwrap() += 1;
                                }
                                *mailbox_counts[&mailbox_hash].1.lock().unwrap() += 1;
                                (sender)(
                                    account_hash,
                                    BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Create(Box::new(env)),
                                    }),
                                );
                            }
                        }
                        /* Update */
                        DebouncedEvent::NoticeWrite(pathbuf) | DebouncedEvent::Write(pathbuf) => {
                            debug!("DebouncedEvent::Write(path = {:?}", &pathbuf);
                            let mailbox_hash = get_path_hash!(pathbuf);
                            let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                            let index_lock =
                                &mut hash_indexes_lock.entry(mailbox_hash).or_default();
                            let file_name = pathbuf
                                .as_path()
                                .strip_prefix(&root_path)
                                .unwrap()
                                .to_path_buf();
                            /* Linear search in hash_index to find old hash */
                            let old_hash: EnvelopeHash = {
                                if let Some((k, v)) =
                                    index_lock.iter_mut().find(|(_, v)| *v.buf == pathbuf)
                                {
                                    *v = pathbuf.clone().into();
                                    *k
                                } else {
                                    drop(hash_indexes_lock);
                                    /* Did we just miss a Create event? In any case, create
                                     * envelope. */
                                    if let Ok(env) = add_path_to_index(
                                        &hash_indexes,
                                        mailbox_hash,
                                        pathbuf.as_path(),
                                        &cache_dir,
                                        file_name,
                                        &mut buf,
                                    ) {
                                        mailbox_index
                                            .lock()
                                            .unwrap()
                                            .insert(env.hash(), mailbox_hash);
                                        (sender)(
                                            account_hash,
                                            BackendEvent::Refresh(RefreshEvent {
                                                account_hash,
                                                mailbox_hash,
                                                kind: Create(Box::new(env)),
                                            }),
                                        );
                                    }
                                    continue;
                                }
                            };
                            let new_hash: EnvelopeHash = get_file_hash(pathbuf.as_path());
                            let mut reader = io::BufReader::new(fs::File::open(&pathbuf)?);
                            buf.clear();
                            reader.read_to_end(&mut buf)?;
                            if index_lock.get_mut(&new_hash).is_none() {
                                debug!("write notice");
                                if let Ok(mut env) =
                                    Envelope::from_bytes(buf.as_slice(), Some(pathbuf.flags()))
                                {
                                    env.set_hash(new_hash);
                                    debug!("{}\t{:?}", new_hash, &pathbuf);
                                    debug!(
                                        "hash {}, path: {:?} couldn't be parsed",
                                        new_hash, &pathbuf
                                    );
                                    index_lock.insert(new_hash, pathbuf.into());

                                    /* Send Write notice */

                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Update(old_hash, Box::new(env)),
                                        }),
                                    );
                                }
                            }
                        }
                        /* Remove */
                        DebouncedEvent::NoticeRemove(pathbuf) | DebouncedEvent::Remove(pathbuf) => {
                            debug!("DebouncedEvent::Remove(path = {:?}", pathbuf);
                            let mailbox_hash = get_path_hash!(pathbuf);
                            let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                            let index_lock = hash_indexes_lock.entry(mailbox_hash).or_default();
                            let hash: EnvelopeHash = if let Some((k, _)) =
                                index_lock.iter().find(|(_, v)| *v.buf == pathbuf)
                            {
                                *k
                            } else {
                                debug!("removed but not contained in index");
                                continue;
                            };
                            if let Some(ref modif) = &index_lock[&hash].modified {
                                match modif {
                                    PathMod::Path(path) => debug!(
                                        "envelope {} has modified path set {}",
                                        hash,
                                        path.display()
                                    ),
                                    PathMod::Hash(hash) => debug!(
                                        "envelope {} has modified path set {}",
                                        hash,
                                        &index_lock[&hash].buf.display()
                                    ),
                                }
                                index_lock.entry(hash).and_modify(|e| {
                                    e.removed = false;
                                });
                                continue;
                            }
                            {
                                let mut lck = mailbox_counts[&mailbox_hash].1.lock().unwrap();
                                *lck = lck.saturating_sub(1);
                            }
                            if !pathbuf.flags().contains(Flag::SEEN) {
                                let mut lck = mailbox_counts[&mailbox_hash].0.lock().unwrap();
                                *lck = lck.saturating_sub(1);
                            }

                            index_lock.entry(hash).and_modify(|e| {
                                e.removed = true;
                            });

                            (sender)(
                                account_hash,
                                BackendEvent::Refresh(RefreshEvent {
                                    account_hash,
                                    mailbox_hash,
                                    kind: Remove(hash),
                                }),
                            );
                        }
                        /* Envelope hasn't changed */
                        DebouncedEvent::Rename(src, dest) => {
                            debug!("DebouncedEvent::Rename(src = {:?}, dest = {:?})", src, dest);
                            let mailbox_hash = get_path_hash!(src);
                            let dest_mailbox = {
                                let dest_mailbox = get_path_hash!(dest);
                                if dest_mailbox == mailbox_hash {
                                    None
                                } else {
                                    Some(dest_mailbox)
                                }
                            };
                            let old_hash: EnvelopeHash = get_file_hash(src.as_path());
                            let new_hash: EnvelopeHash = get_file_hash(dest.as_path());

                            let mut hash_indexes_lock = hash_indexes.lock().unwrap();
                            let index_lock = hash_indexes_lock.entry(mailbox_hash).or_default();
                            let old_flags = src.flags();
                            let new_flags = dest.flags();
                            let was_seen: bool = old_flags.contains(Flag::SEEN);
                            let is_seen: bool = new_flags.contains(Flag::SEEN);

                            if index_lock.contains_key(&old_hash) && !index_lock[&old_hash].removed
                            {
                                debug!("contains_old_key");
                                if let Some(dest_mailbox) = dest_mailbox {
                                    index_lock.entry(old_hash).and_modify(|e| {
                                        e.removed = true;
                                    });

                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Remove(old_hash),
                                        }),
                                    );
                                    let file_name = dest
                                        .as_path()
                                        .strip_prefix(&root_path)
                                        .unwrap()
                                        .to_path_buf();
                                    drop(hash_indexes_lock);
                                    if let Ok(env) = add_path_to_index(
                                        &hash_indexes,
                                        dest_mailbox,
                                        dest.as_path(),
                                        &cache_dir,
                                        file_name,
                                        &mut buf,
                                    ) {
                                        mailbox_index
                                            .lock()
                                            .unwrap()
                                            .insert(env.hash(), dest_mailbox);
                                        debug!(
                                            "Create event {} {} {}",
                                            env.hash(),
                                            env.subject(),
                                            dest.display()
                                        );
                                        if !env.is_seen() {
                                            *mailbox_counts[&dest_mailbox].0.lock().unwrap() += 1;
                                        }
                                        *mailbox_counts[&dest_mailbox].1.lock().unwrap() += 1;
                                        (sender)(
                                            account_hash,
                                            BackendEvent::Refresh(RefreshEvent {
                                                account_hash,
                                                mailbox_hash: dest_mailbox,
                                                kind: Create(Box::new(env)),
                                            }),
                                        );
                                    }
                                } else {
                                    index_lock.entry(old_hash).and_modify(|e| {
                                        debug!(&e.modified);
                                        e.modified = Some(PathMod::Hash(new_hash));
                                    });
                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: Rename(old_hash, new_hash),
                                        }),
                                    );
                                    if !was_seen && is_seen {
                                        let mut lck =
                                            mailbox_counts[&mailbox_hash].0.lock().unwrap();
                                        *lck = lck.saturating_sub(1);
                                    } else if was_seen && !is_seen {
                                        *mailbox_counts[&mailbox_hash].0.lock().unwrap() += 1;
                                    }
                                    if old_flags != new_flags {
                                        (sender)(
                                            account_hash,
                                            BackendEvent::Refresh(RefreshEvent {
                                                account_hash,
                                                mailbox_hash,
                                                kind: NewFlags(new_hash, (new_flags, vec![])),
                                            }),
                                        );
                                    }
                                    mailbox_index.lock().unwrap().insert(new_hash, mailbox_hash);
                                    index_lock.insert(new_hash, dest.into());
                                }
                                continue;
                            } else if !index_lock.contains_key(&new_hash)
                                && index_lock
                                    .get(&old_hash)
                                    .map(|e| e.removed)
                                    .unwrap_or(false)
                            {
                                if index_lock
                                    .get(&old_hash)
                                    .map(|e| e.removed)
                                    .unwrap_or(false)
                                {
                                    index_lock.entry(old_hash).and_modify(|e| {
                                        e.modified = Some(PathMod::Hash(new_hash));
                                        e.removed = false;
                                    });
                                    debug!("contains_old_key, key was marked as removed (by external source)");
                                } else {
                                    debug!("not contains_new_key");
                                }
                                let file_name = dest
                                    .as_path()
                                    .strip_prefix(&root_path)
                                    .unwrap()
                                    .to_path_buf();
                                debug!("filename = {:?}", file_name);
                                drop(hash_indexes_lock);
                                if let Ok(env) = add_path_to_index(
                                    &hash_indexes,
                                    dest_mailbox.unwrap_or(mailbox_hash),
                                    dest.as_path(),
                                    &cache_dir,
                                    file_name,
                                    &mut buf,
                                ) {
                                    mailbox_index
                                        .lock()
                                        .unwrap()
                                        .insert(env.hash(), dest_mailbox.unwrap_or(mailbox_hash));
                                    debug!(
                                        "Create event {} {} {}",
                                        env.hash(),
                                        env.subject(),
                                        dest.display()
                                    );
                                    if !env.is_seen() {
                                        *mailbox_counts[&dest_mailbox.unwrap_or(mailbox_hash)]
                                            .0
                                            .lock()
                                            .unwrap() += 1;
                                    }
                                    *mailbox_counts[&dest_mailbox.unwrap_or(mailbox_hash)]
                                        .1
                                        .lock()
                                        .unwrap() += 1;
                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash: dest_mailbox.unwrap_or(mailbox_hash),
                                            kind: Create(Box::new(env)),
                                        }),
                                    );
                                    continue;
                                } else {
                                    debug!("not valid email");
                                }
                            } else if let Some(dest_mailbox) = dest_mailbox {
                                drop(hash_indexes_lock);
                                let file_name = dest
                                    .as_path()
                                    .strip_prefix(&root_path)
                                    .unwrap()
                                    .to_path_buf();
                                if let Ok(env) = add_path_to_index(
                                    &hash_indexes,
                                    dest_mailbox,
                                    dest.as_path(),
                                    &cache_dir,
                                    file_name,
                                    &mut buf,
                                ) {
                                    mailbox_index
                                        .lock()
                                        .unwrap()
                                        .insert(env.hash(), dest_mailbox);
                                    debug!(
                                        "Create event {} {} {}",
                                        env.hash(),
                                        env.subject(),
                                        dest.display()
                                    );
                                    if !env.is_seen() {
                                        *mailbox_counts[&dest_mailbox].0.lock().unwrap() += 1;
                                    }
                                    *mailbox_counts[&dest_mailbox].1.lock().unwrap() += 1;
                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash: dest_mailbox,
                                            kind: Create(Box::new(env)),
                                        }),
                                    );
                                }
                            } else {
                                if was_seen && !is_seen {
                                    *mailbox_counts[&mailbox_hash].0.lock().unwrap() += 1;
                                }
                                (sender)(
                                    account_hash,
                                    BackendEvent::Refresh(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Rename(old_hash, new_hash),
                                    }),
                                );
                                debug!("contains_new_key");
                                if old_flags != new_flags {
                                    (sender)(
                                        account_hash,
                                        BackendEvent::Refresh(RefreshEvent {
                                            account_hash,
                                            mailbox_hash,
                                            kind: NewFlags(new_hash, (new_flags, vec![])),
                                        }),
                                    );
                                }
                            }

                            /* Maybe a re-read should be triggered here just to be safe.
                               (sender)(account_hash, BackendEvent::Refresh(RefreshEvent {
                                account_hash,
                                mailbox_hash: get_path_hash!(dest),
                                kind: Rescan,
                            }));
                            */
                        }
                        /* Trigger rescan of mailbox */
                        DebouncedEvent::Rescan => {
                            (sender)(
                                account_hash,
                                BackendEvent::Refresh(RefreshEvent {
                                    account_hash,
                                    mailbox_hash: root_mailbox_hash,
                                    kind: Rescan,
                                }),
                            );
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

fn add_path_to_index(
    hash_index: &HashIndexes,
    mailbox_hash: MailboxHash,
    path: &Path,
    cache_dir: &xdg::BaseDirectories,
    file_name: PathBuf,
    buf: &mut Vec<u8>,
) -> Result<Envelope> {
    debug!("add_path_to_index path {:?} filename{:?}", path, file_name);
    let env_hash = get_file_hash(path);
    {
        let mut map = hash_index.lock().unwrap();
        let map = map.entry(mailbox_hash).or_default();
        map.insert(env_hash, path.to_path_buf().into());
        debug!(
            "inserted {} in {} map, len={}",
            env_hash,
            mailbox_hash,
            map.len()
        );
    }
    let mut reader = io::BufReader::new(fs::File::open(&path)?);
    buf.clear();
    reader.read_to_end(buf)?;
    let mut env = Envelope::from_bytes(buf.as_slice(), Some(path.flags()))?;
    env.set_hash(env_hash);
    debug!(
        "add_path_to_index gen {}\t{}",
        env_hash,
        file_name.display()
    );
    if let Ok(cached) = cache_dir.place_cache_file(file_name) {
        debug!("putting in cache");
        /* place result in cache directory */
        let f = fs::File::create(cached)?;
        let metadata = f.metadata()?;
        let mut permissions = metadata.permissions();

        permissions.set_mode(0o600); // Read/write for owner only.
        f.set_permissions(permissions)?;
        let writer = io::BufWriter::new(f);
        bincode::Options::serialize_into(bincode::config::DefaultOptions::new(), writer, &env)?;
    }
    Ok(env)
}
