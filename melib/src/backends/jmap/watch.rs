/*
 * melib - JMAP
 *
 * Copyright 2020 Manos Pitsidianakis
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
pub struct JmapWatcher {
    pub mailbox_hashes: BTreeSet<MailboxHash>,
    pub polling_period: std::time::Duration,
    pub connection: Arc<FutureMutex<JmapConnection>>,
}

impl BackendWatcher for JmapWatcher {
    fn is_blocking(&self) -> bool {
        false
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
        let JmapWatcher {
            mailbox_hashes,
            polling_period,
            connection,
        } = *self;
        Ok(Box::pin(async move {
            {
                let mut conn = connection.lock().await;
                conn.connect().await?;
            }
            loop {
                {
                    let conn = connection.lock().await;
                    for &mailbox_hash in &mailbox_hashes {
                        conn.email_changes(mailbox_hash).await?;
                    }
                }
                crate::connections::sleep(polling_period).await;
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
