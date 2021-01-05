/*
 * meli - jmap module.
 *
 * Copyright 2021 Manos Pitsidianakis
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

use super::{HttpClient, JmapConnection, JmapServerConf, Store};
use crate::error::Result;
use std::convert::TryFrom;
use std::io::{BufRead, BufReader};
use std::sync::Arc;
use std::time::{Duration, Instant};

const DEFAULT_RETRY: u64 = 5000;

/// A single Server-Sent Event.
#[derive(Debug, Default)]
pub struct Event {
    /// Corresponds to the `id` field.
    pub id: Option<String>,
    /// Corresponds to the `event` field.
    pub event_type: Option<String>,
    /// All `data` fields concatenated by newlines.
    pub data: String,
}

/// Possible results from parsing a single event-stream line.
#[derive(Debug, PartialEq)]
pub enum ParseResult {
    /// Line parsed successfully, but the event is not complete yet.
    Next,
    /// The event is complete now. Pass a new (empty) event for the next call.
    Dispatch,
    /// Set retry time.
    SetRetry(Duration),
}

pub fn parse_event_line(line: &str, event: &mut Event) -> ParseResult {
    let line = line.trim_end_matches(|c| c == '\r' || c == '\n');
    if line == "" {
        ParseResult::Dispatch
    } else {
        let (field, value) = if let Some(pos) = line.find(':') {
            let (f, v) = line.split_at(pos);
            // Strip : and an optional space.
            let v = &v[1..];
            let v = if v.starts_with(' ') { &v[1..] } else { v };
            (f, v)
        } else {
            (line, "")
        };
        match field {
            "event" => {
                event.event_type = Some(value.to_string());
            }
            "data" => {
                event.data.push_str(value);
                event.data.push('\n');
            }
            "id" => {
                event.id = Some(value.to_string());
            }
            "retry" => {
                if let Ok(retry) = value.parse::<u64>() {
                    return ParseResult::SetRetry(Duration::from_millis(retry));
                }
            }
            _ => (), // ignored
        }

        ParseResult::Next
    }
}

impl Event {
    /// Creates an empty event.
    pub fn new() -> Event {
        Event::default()
    }

    /// Returns `true` if the event is empty.
    ///
    /// An event is empty if it has no id or event type and its data field is empty.
    pub fn is_empty(&self) -> bool {
        self.id.is_none() && self.event_type.is_none() && self.data.is_empty()
    }

    /// Makes the event empty.
    pub fn clear(&mut self) {
        self.id = None;
        self.event_type = None;
        self.data.clear();
    }
}

/// A client for a Server-Sent Events endpoint.
///
/// Read events by iterating over the client.
pub struct JmapEventSourceConnection {
    pub client: Arc<HttpClient>,
    pub store: Arc<Store>,
    pub server_conf: JmapServerConf,
    response: Option<BufReader<isahc::Body>>,
    url: isahc::http::Uri,
    last_event_id: Option<String>,
    last_try: Option<Instant>,
    pub retry: Duration,
}

impl JmapEventSourceConnection {
    pub fn new(conn: &JmapConnection) -> Result<Self> {
        let url =
            isahc::http::Uri::try_from(conn.session.lock().unwrap().event_source_url.as_str())
                .map_err(|err| err.to_string())?;
        debug!("event_source {}", &url);
        Ok(Self {
            client: conn.client.clone(),
            server_conf: conn.server_conf.clone(),
            store: conn.store.clone(),
            response: None,
            url: url,
            last_event_id: None,
            last_try: None,
            retry: Duration::from_millis(DEFAULT_RETRY),
        })
    }

    pub async fn next_request(&mut self) -> Result<()> {
        use isahc::{http, http::request::Request, prelude::*};

        let mut request = Request::get(&self.url)
            .timeout(std::time::Duration::from_secs(10))
            .redirect_policy(isahc::config::RedirectPolicy::Limit(10))
            .authentication(isahc::auth::Authentication::basic())
            .credentials(isahc::auth::Credentials::new(
                &self.server_conf.server_username,
                &self.server_conf.server_password,
            ))
            .header(http::header::ACCEPT, "text/event-stream");
        if let Some(ref id) = self.last_event_id {
            request = request.header("Last-Event-ID", id.as_str());
        }
        let request = request.body(()).map_err(|err| err.to_string())?;

        debug!(&request);
        let mut response = self.client.send_async(request).await?;
        debug!(&response);
        //debug_assert!(response.status().is_success());
        /*
                let mut headers = HeaderMap::with_capacity(2);
                headers.insert(ACCEPT, HeaderValue::from_str("text/event-stream").unwrap());
                if let Some(ref id) = self.last_event_id {
                    headers.insert("Last-Event-ID", HeaderValue::from_str(id).unwrap());
                }

                let res = self.client.get(self.url.clone()).headers(headers).send()?;
        */

        // Check status code and Content-Type.
        {
            let status = response.status();
            if !status.is_success() {
                let res_text = response.text_async().await?;
                return Err(debug!(format!("{} {}", status.as_str(), res_text)).into());
            }

            if let Some(content_type_hv) = response.headers().get(isahc::http::header::CONTENT_TYPE)
            {
                if content_type_hv.to_str().unwrap() != "text/event-stream" {
                    panic!(content_type_hv.to_str().unwrap().to_string());
                }
                /*
                let content_type = content_type_hv
                    .to_str()
                    .unwrap()
                    .to_string()
                    .parse::<mime::Mime>()
                    .unwrap();
                // Compare type and subtype only, MIME parameters are ignored.
                if (content_type.type_(), content_type.subtype())
                    != (mime::TEXT, mime::EVENT_STREAM)
                {
                    return Err(ErrorKind::InvalidContentType(content_type.clone()).into());
                }
                */
            }
        }

        self.response = Some(BufReader::new(response.into_body()));
        Ok(())
    }
} /*

      pub async fn next_event(&mut self) -> Result<Event> {
          let mut line = String::new();
          'main_loop: loop {
              match self.response.as_mut() {
                  None => {
                      // wait for the next request.
                      if let Some(last_try) = self.last_try {
                          let elapsed = last_try.elapsed();
                          if elapsed < self.retry {
                              crate::connection::sleep(self.retry - elapsed).await;
                          }
                      }
                      // Set here in case the request fails.
                      self.last_try = Some(Instant::now());

                      self.next_request().await?;
                  }
                  Some(reader) => {
                      let mut event = Event::new();
                      loop {
                          match reader.read_line(&mut line) {
                              // Got new bytes from stream
                              Ok(_n) if _n > 0 => {
                                  match parse_event_line(&line, &mut event) {
                                      ParseResult::Next => {} // okay, just continue
                                      ParseResult::Dispatch => {
                                          if let Some(ref id) = event.id {
                                              self.last_event_id = Some(id.clone());
                                          }
                                          return Ok(event);
                                      }
                                      ParseResult::SetRetry(ref retry) => {
                                          self.retry = *retry;
                                      }
                                  }
                                  line.clear();
                              }
                              Ok(0) => {
                                  // EOF or a stream error, retry after timeout
                                  self.last_try = Some(Instant::now());
                                  self.response = None;
                                  continue 'main_loop;
                              }
                              Err(err) => {
                                  // EOF or a stream error, retry after timeout
                                  self.last_try = Some(Instant::now());
                                  self.response = None;
                                  debug!(&err);
                                  continue 'main_loop;
                              }
                          }
                      }
                  }
              }
          }
      }
  }
  */
