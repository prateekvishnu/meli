/*
 * meli - notmuch backend
 *
 * Copyright 2019 - 2021 Manos Pitsidianakis
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
pub struct NotmuchWatcher {
    pub account_hash: AccountHash,
    pub collection: Collection,
    pub event_consumer: BackendEventConsumer,
    pub index: Arc<RwLock<HashMap<EnvelopeHash, CString>>>,
    pub lib: Arc<libloading::Library>,
    pub mailbox_hashes: BTreeSet<MailboxHash>,
    pub mailbox_index: Arc<RwLock<HashMap<EnvelopeHash, SmallVec<[MailboxHash; 16]>>>>,
    pub mailboxes: Arc<RwLock<HashMap<MailboxHash, NotmuchMailbox>>>,
    pub path: PathBuf,
    pub polling_period: std::time::Duration,
    pub revision_uuid: Arc<RwLock<u64>>,
}

impl BackendWatcher for NotmuchWatcher {
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
        Ok(Box::pin(async move {
            extern crate notify;
            use notify::{watcher, RecursiveMode, Watcher};
            let NotmuchWatcher {
                account_hash,
                collection,
                event_consumer,
                index,
                lib,
                mailbox_hashes: _,
                mailbox_index,
                mailboxes,
                path,
                polling_period,
                revision_uuid,
            } = *self;

            let (tx, rx) = std::sync::mpsc::channel();
            let mut watcher = watcher(tx, polling_period).unwrap();
            watcher.watch(&path, RecursiveMode::Recursive).unwrap();
            loop {
                let _ = rx.recv().map_err(|err| err.to_string())?;
                {
                    let mut database = NotmuchDb::new_connection(
                        path.as_path(),
                        revision_uuid.clone(),
                        lib.clone(),
                        false,
                    )?;
                    let new_revision_uuid = database.get_revision_uuid();
                    if new_revision_uuid > *database.revision_uuid.read().unwrap() {
                        database.refresh(
                            mailboxes.clone(),
                            index.clone(),
                            mailbox_index.clone(),
                            collection.tag_index.clone(),
                            account_hash.clone(),
                            event_consumer.clone(),
                            new_revision_uuid,
                        )?;
                        *revision_uuid.write().unwrap() = new_revision_uuid;
                    }
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
