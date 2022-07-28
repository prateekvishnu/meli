/*
 * meli
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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
use linkify::{Link, LinkFinder};
use std::process::{Command, Stdio};

use xdg_utils::query_default_app;

#[derive(PartialEq, Debug)]
enum ViewMode {
    Normal,
    Url,
    Attachment(usize),
    Raw,
    Subview,
}

impl ViewMode {
    fn is_attachment(&self) -> bool {
        match self {
            ViewMode::Attachment(_) => true,
            _ => false,
        }
    }
}

/// Contains an Envelope view, with sticky headers, a pager for the body, and subviews for more
/// menus
#[derive(Debug)]
pub struct EnvelopeView {
    pager: Option<Pager>,
    subview: Option<Box<dyn Component>>,
    dirty: bool,
    mode: ViewMode,
    mail: Mail,

    account_hash: AccountHash,
    cmd_buf: String,
    id: ComponentId,
}

impl fmt::Display for EnvelopeView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "view mail")
    }
}

impl EnvelopeView {
    pub fn new(
        mail: Mail,
        pager: Option<Pager>,
        subview: Option<Box<dyn Component>>,
        account_hash: AccountHash,
    ) -> Self {
        EnvelopeView {
            pager,
            subview,
            dirty: true,
            mode: ViewMode::Normal,
            mail,
            account_hash,
            cmd_buf: String::with_capacity(4),
            id: ComponentId::new_v4(),
        }
    }

    /// Returns the string to be displayed in the Viewer
    fn attachment_to_text(&self, body: &Attachment, context: &mut Context) -> String {
        let finder = LinkFinder::new();
        let body_text = String::from_utf8_lossy(&decode_rec(
            &body,
            Some(Box::new(|a: &Attachment, v: &mut Vec<u8>| {
                if a.content_type().is_text_html() {
                    let settings = &context.settings;
                    if let Some(filter_invocation) = settings.pager.html_filter.as_ref() {
                        let command_obj = Command::new("sh")
                            .args(&["-c", filter_invocation])
                            .stdin(Stdio::piped())
                            .stdout(Stdio::piped())
                            .spawn();
                        match command_obj {
                            Err(err) => {
                                context.replies.push_back(UIEvent::Notification(
                                    Some(format!(
                                        "Failed to start html filter process: {}",
                                        filter_invocation,
                                    )),
                                    err.to_string(),
                                    Some(NotificationType::Error(melib::ErrorKind::External)),
                                ));
                                return;
                            }
                            Ok(mut html_filter) => {
                                html_filter
                                    .stdin
                                    .as_mut()
                                    .unwrap()
                                    .write_all(&v)
                                    .expect("Failed to write to stdin");
                                *v = format!(
                            "Text piped through `{}`. Press `v` to open in web browser. \n\n",
                            filter_invocation
                        )
                                .into_bytes();
                                v.extend(html_filter.wait_with_output().unwrap().stdout);
                            }
                        }
                    }
                }
            })),
        ))
        .into_owned();
        match self.mode {
            ViewMode::Normal | ViewMode::Subview => {
                let mut t = body_text;
                if body.count_attachments() > 1 {
                    t = body
                        .attachments()
                        .iter()
                        .enumerate()
                        .fold(t, |mut s, (idx, a)| {
                            s.push_str(&format!("[{}] {}\n\n", idx, a));
                            s
                        });
                }
                t
            }
            ViewMode::Raw => String::from_utf8_lossy(body.body()).into_owned(),
            ViewMode::Url => {
                let mut t = body_text;
                for (lidx, l) in finder.links(&body.text()).enumerate() {
                    let offset = if lidx < 10 {
                        lidx * 3
                    } else if lidx < 100 {
                        26 + (lidx - 9) * 4
                    } else if lidx < 1000 {
                        385 + (lidx - 99) * 5
                    } else {
                        panic!("BUG: Message body with more than 100 urls, fix this");
                    };
                    t.insert_str(l.start() + offset, &format!("[{}]", lidx));
                }
                if body.count_attachments() > 1 {
                    t = body
                        .attachments()
                        .iter()
                        .enumerate()
                        .fold(t, |mut s, (idx, a)| {
                            s.push_str(&format!("[{}] {}\n\n", idx, a));
                            s
                        });
                }
                t
            }
            ViewMode::Attachment(aidx) => {
                let attachments = body.attachments();
                let mut ret = "Viewing attachment. Press `r` to return \n".to_string();
                ret.push_str(&attachments[aidx].text());
                ret
            }
        }
    }
}

impl Component for EnvelopeView {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let theme_default = crate::conf::value(context, "theme_default");
        let email_header_theme = crate::conf::value(context, "email_header");

        let y: usize = {
            if self.mode == ViewMode::Raw {
                clear_area(grid, area, crate::conf::value(context, "theme_default"));
                context.dirty_areas.push_back(area);
                get_y(upper_left).saturating_sub(1)
            } else {
                let (x, y) = write_string_to_grid(
                    &format!("Date: {}", self.mail.date_as_str()),
                    grid,
                    email_header_theme.fg,
                    email_header_theme.bg,
                    email_header_theme.attrs,
                    area,
                    Some(get_x(upper_left)),
                );
                for x in x..=get_x(bottom_right) {
                    grid[(x, y)]
                        .set_ch(' ')
                        .set_fg(theme_default.fg)
                        .set_bg(theme_default.bg);
                }
                let (x, y) = write_string_to_grid(
                    &format!("From: {}", self.mail.field_from_to_string()),
                    grid,
                    email_header_theme.fg,
                    email_header_theme.bg,
                    email_header_theme.attrs,
                    (set_y(upper_left, y + 1), bottom_right),
                    Some(get_x(upper_left)),
                );
                for x in x..=get_x(bottom_right) {
                    grid[(x, y)]
                        .set_ch(' ')
                        .set_fg(theme_default.fg)
                        .set_bg(theme_default.bg);
                }
                let (x, y) = write_string_to_grid(
                    &format!("To: {}", self.mail.field_to_to_string()),
                    grid,
                    email_header_theme.fg,
                    email_header_theme.bg,
                    email_header_theme.attrs,
                    (set_y(upper_left, y + 1), bottom_right),
                    Some(get_x(upper_left)),
                );
                for x in x..=get_x(bottom_right) {
                    grid[(x, y)]
                        .set_ch(' ')
                        .set_fg(theme_default.fg)
                        .set_bg(theme_default.bg);
                }
                let (x, y) = write_string_to_grid(
                    &format!("Subject: {}", self.mail.subject()),
                    grid,
                    email_header_theme.fg,
                    email_header_theme.bg,
                    email_header_theme.attrs,
                    (set_y(upper_left, y + 1), bottom_right),
                    Some(get_x(upper_left)),
                );
                for x in x..=get_x(bottom_right) {
                    grid[(x, y)]
                        .set_ch(' ')
                        .set_fg(theme_default.fg)
                        .set_bg(theme_default.bg);
                }
                let (x, y) = write_string_to_grid(
                    &format!("Message-ID: <{}>", self.mail.message_id_raw()),
                    grid,
                    email_header_theme.fg,
                    email_header_theme.bg,
                    email_header_theme.attrs,
                    (set_y(upper_left, y + 1), bottom_right),
                    Some(get_x(upper_left)),
                );
                for x in x..=get_x(bottom_right) {
                    grid[(x, y)]
                        .set_ch(' ')
                        .set_fg(theme_default.fg)
                        .set_bg(theme_default.bg);
                }
                clear_area(
                    grid,
                    (set_y(upper_left, y + 1), set_y(bottom_right, y + 2)),
                    crate::conf::value(context, "theme_default"),
                );
                context
                    .dirty_areas
                    .push_back((upper_left, set_y(bottom_right, y + 1)));
                y + 1
            }
        };

        if self.dirty {
            let body = self.mail.body();
            match self.mode {
                ViewMode::Attachment(aidx) if body.attachments()[aidx].is_html() => {
                    let attachment = &body.attachments()[aidx];
                    self.subview = Some(Box::new(HtmlView::new(&attachment, context)));
                }
                ViewMode::Normal if body.is_html() => {
                    self.subview = Some(Box::new(HtmlView::new(&body, context)));
                    self.mode = ViewMode::Subview;
                }
                _ => {
                    let text = { self.attachment_to_text(&body, context) };
                    let cursor_pos = if self.mode.is_attachment() {
                        Some(0)
                    } else {
                        self.pager.as_ref().map(Pager::cursor_pos)
                    };
                    let colors = crate::conf::value(context, "mail.view.body");
                    self.pager = Some(Pager::from_string(
                        text,
                        Some(context),
                        cursor_pos,
                        None,
                        colors,
                    ));
                }
            };
            self.dirty = false;
        }
        if let Some(s) = self.subview.as_mut() {
            s.draw(grid, (set_y(upper_left, y + 1), bottom_right), context);
        } else if let Some(p) = self.pager.as_mut() {
            p.draw(grid, (set_y(upper_left, y + 1), bottom_right), context);
        }
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Some(ref mut sub) = self.subview {
            if sub.process_event(event, context) {
                return true;
            }
        } else if let Some(ref mut p) = self.pager {
            if p.process_event(event, context) {
                return true;
            }
        }
        match *event {
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt('')) if !self.cmd_buf.is_empty() => {
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                return true;
            }
            UIEvent::Input(Key::Char(c)) if c >= '0' && c <= '9' => {
                self.cmd_buf.push(c);
                return true;
            }
            UIEvent::Input(Key::Char('r'))
                if self.mode == ViewMode::Normal || self.mode == ViewMode::Raw =>
            {
                self.mode = if self.mode == ViewMode::Raw {
                    ViewMode::Normal
                } else {
                    ViewMode::Raw
                };
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Char('r'))
                if self.mode.is_attachment() || self.mode == ViewMode::Subview =>
            {
                self.mode = ViewMode::Normal;
                self.subview.take();
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Char('a'))
                if !self.cmd_buf.is_empty() && self.mode == ViewMode::Normal =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));

                {
                    if let Some(u) = self.mail.body().attachments().get(lidx) {
                        match u.content_type() {
                            ContentType::MessageRfc822 => {
                                self.mode = ViewMode::Subview;
                                let colors = crate::conf::value(context, "mail.view.body");
                                self.subview = Some(Box::new(Pager::from_string(
                                    String::from_utf8_lossy(&decode_rec(u, None)).to_string(),
                                    Some(context),
                                    None,
                                    None,
                                    colors,
                                )));
                            }

                            ContentType::Text { .. }
                            | ContentType::PGPSignature
                            | ContentType::CMSSignature => {
                                self.mode = ViewMode::Attachment(lidx);
                                self.dirty = true;
                            }
                            ContentType::Multipart { .. } => {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(
                                        "Multipart attachments are not supported yet.".to_string(),
                                    ),
                                ));
                                return true;
                            }
                            ContentType::Other { .. } => {
                                let attachment_type = u.mime_type();
                                let filename = u.filename();
                                if let Ok(command) = query_default_app(&attachment_type) {
                                    let p = create_temp_file(
                                        &decode(u, None),
                                        filename.as_ref().map(|s| s.as_str()),
                                        None,
                                        true,
                                    );
                                    let (exec_cmd, argument) = super::desktop_exec_to_command(
                                        &command,
                                        p.path.display().to_string(),
                                        false,
                                    );
                                    match Command::new(&exec_cmd)
                                        .arg(&argument)
                                        .stdin(Stdio::piped())
                                        .stdout(Stdio::piped())
                                        .spawn()
                                    {
                                        Ok(child) => {
                                            context.temp_files.push(p);
                                            context.children.push(child);
                                        }
                                        Err(err) => {
                                            context.replies.push_back(UIEvent::StatusEvent(
                                                StatusEvent::DisplayMessage(format!(
                                                    "Failed to start `{} {}`: {}",
                                                    &exec_cmd, &argument, err
                                                )),
                                            ));
                                        }
                                    }
                                } else {
                                    context.replies.push_back(UIEvent::StatusEvent(
                                        StatusEvent::DisplayMessage(if let Some(filename) = filename.as_ref() {
                                            format!(
                                                "Couldn't find a default application for file {} (type {})",
                                                filename,
                                                attachment_type
                                            )
                                        } else {
                                            format!(
                                                "Couldn't find a default application for type {}",
                                                attachment_type
                                            )
                                        }),
                                ));
                                    return true;
                                }
                            }
                            ContentType::OctetStream { .. } => {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(
                                        "application/octet-stream isn't supported yet".to_string(),
                                    ),
                                ));
                                return true;
                            }
                        }
                    } else {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!(
                                "Attachment `{}` not found.",
                                lidx
                            )),
                        ));
                        return true;
                    }
                };
                return true;
            }
            UIEvent::Input(Key::Char('g'))
                if !self.cmd_buf.is_empty() && self.mode == ViewMode::Url =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                let url = {
                    let finder = LinkFinder::new();
                    let t = self.mail.body().text();
                    let links: Vec<Link> = finder.links(&t).collect();
                    if let Some(u) = links.get(lidx) {
                        u.as_str().to_string()
                    } else {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!("Link `{}` not found.", lidx)),
                        ));
                        return true;
                    }
                };

                let url_launcher = context
                    .settings
                    .pager
                    .url_launcher
                    .as_ref()
                    .map(|s| s.as_str())
                    .unwrap_or(
                        #[cfg(target_os = "macos")]
                        {
                            "open"
                        },
                        #[cfg(not(target_os = "macos"))]
                        {
                            "xdg-open"
                        },
                    );
                match Command::new(url_launcher)
                    .arg(url)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                {
                    Ok(child) => context.children.push(child),
                    Err(err) => context.replies.push_back(UIEvent::Notification(
                        Some(format!("Failed to launch {:?}", url_launcher)),
                        err.to_string(),
                        Some(NotificationType::Error(melib::ErrorKind::External)),
                    )),
                }
                return true;
            }
            UIEvent::Input(Key::Char('u')) => {
                match self.mode {
                    ViewMode::Normal => self.mode = ViewMode::Url,
                    ViewMode::Url => self.mode = ViewMode::Normal,
                    _ => {}
                }
                self.dirty = true;
                return true;
            }
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty
            || self.pager.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
            || self.subview.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
    }
    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn kill(&mut self, id: ComponentId, context: &mut Context) {
        debug_assert!(self.id == id);
        context
            .replies
            .push_back(UIEvent::Action(Tab(Kill(self.id))));
    }

    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
