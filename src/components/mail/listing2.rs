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
use crate::conf::accounts::JobRequest;
use crate::types::segment_tree::SegmentTree;
use melib::backends::EnvelopeHashBatch;
use smallvec::SmallVec;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::ops::{Deref, DerefMut};

use mailbox_list::MailboxList;

#[derive(Debug, Copy, PartialEq, Clone)]
pub enum Modifier {
    SymmetricDifference,
    Union,
    Difference,
    Intersection,
}

impl Default for Modifier {
    fn default() -> Self {
        Modifier::SymmetricDifference
    }
}

#[derive(Debug, Default, Clone)]
pub struct DataColumns {
    pub columns: [CellBuffer; 12],
    pub widths: [usize; 12], // widths of columns calculated in first draw and after size changes
    pub segment_tree: [SegmentTree; 12],
}

#[derive(Debug, Default)]
/// Save theme colors to avoid looking them up again and again from settings
struct ColorCache {
    theme_default: ThemeAttribute,

    unseen: ThemeAttribute,
    highlighted: ThemeAttribute,
    selected: ThemeAttribute,
    even: ThemeAttribute,
    odd: ThemeAttribute,
    even_unseen: ThemeAttribute,
    even_highlighted: ThemeAttribute,
    even_selected: ThemeAttribute,
    odd_unseen: ThemeAttribute,
    odd_highlighted: ThemeAttribute,
    odd_selected: ThemeAttribute,
    attachment_flag: ThemeAttribute,
    thread_snooze_flag: ThemeAttribute,
    tag_default: ThemeAttribute,

    /* Conversations */
    subject: ThemeAttribute,
    from: ThemeAttribute,
    date: ThemeAttribute,
    padding: ThemeAttribute,
    unseen_padding: ThemeAttribute,
}

#[derive(Debug)]
pub struct EntryStrings {
    pub date: DateString,
    pub subject: SubjectString,
    pub flag: FlagString,
    pub from: FromString,
    pub tags: TagString,
}

macro_rules! column_str {
    (struct $name:ident($($t:ty),+)) => {
        #[derive(Debug)]
        pub struct $name($(pub $t),+);

        impl Deref for $name {
            type Target = String;
            fn deref(&self) -> &String {
                &self.0
            }
        }
        impl DerefMut for $name {
            fn deref_mut(&mut self) -> &mut String {
                &mut self.0
            }
        }
    };
}

column_str!(struct DateString(String));
column_str!(struct FromString(String));
column_str!(struct SubjectString(String));
column_str!(struct FlagString(String));
column_str!(struct TagString(String, SmallVec<[Option<Color>; 8]>));

#[derive(Debug)]
struct AccountMenuEntry {
    name: String,
    hash: AccountHash,
    index: usize,
    entries: SmallVec<[(usize, u32, bool, MailboxHash); 16]>,
}

#[derive(PartialEq, Debug)]
enum ListingFocus {
    Menu,
    Mailbox,
}

#[derive(PartialEq, Copy, Clone, Debug)]
enum MenuEntryCursor {
    Status,
    Mailbox(usize),
}

#[derive(PartialEq, Copy, Clone, Debug)]
enum ShowMenuScrollbar {
    Never,
    True,
    False,
}

#[derive(Debug)]
pub struct Listing {
    mailbox_list: MailboxList,
    accounts: Vec<AccountMenuEntry>,
    status: Option<AccountStatus>,
    dirty: bool,
    visible: bool,
    cursor_pos: (usize, MenuEntryCursor),
    menu_cursor_pos: (usize, MenuEntryCursor),
    menu_content: CellBuffer,
    menu_scrollbar_show_timer: crate::jobs::Timer,
    show_menu_scrollbar: ShowMenuScrollbar,
    startup_checks_rate: RateLimit,
    id: ComponentId,
    theme_default: ThemeAttribute,

    sidebar_divider: char,
    sidebar_divider_theme: ThemeAttribute,

    menu_visibility: bool,
    cmd_buf: String,
    /// This is the width of the right container to the entire width.
    ratio: usize, // right/(container width) * 100
    menu_width: WidgetWidth,
    focus: ListingFocus,
}

impl fmt::Display for Listing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "mail")
    }
}

impl Component for Listing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        if !is_valid_area!(area) {
            return;
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let total_cols = get_x(bottom_right) - get_x(upper_left);

        let right_component_width = if self.menu_visibility {
            if self.focus == ListingFocus::Menu {
                (self.ratio * total_cols) / 100
            } else {
                match self.menu_width {
                    WidgetWidth::Set(ref mut v) | WidgetWidth::Hold(ref mut v) => {
                        if *v == 0 {
                            *v = 1;
                        } else if *v >= total_cols {
                            *v = total_cols.saturating_sub(2);
                        }
                        total_cols.saturating_sub(*v)
                    }
                    WidgetWidth::Unset => {
                        self.menu_width =
                            WidgetWidth::Set(total_cols - ((self.ratio * total_cols) / 100));
                        (self.ratio * total_cols) / 100
                    }
                }
            }
        } else {
            total_cols
        };
        let mid = get_x(bottom_right) - right_component_width;
        if self.dirty && mid != get_x(upper_left) {
            for i in get_y(upper_left)..=get_y(bottom_right) {
                grid[(mid, i)]
                    .set_ch(self.sidebar_divider)
                    .set_fg(self.sidebar_divider_theme.fg)
                    .set_bg(self.sidebar_divider_theme.bg)
                    .set_attrs(self.sidebar_divider_theme.attrs);
            }
            context
                .dirty_areas
                .push_back(((mid, get_y(upper_left)), (mid, get_y(bottom_right))));
        }

        let account_hash = self.accounts[self.cursor_pos.0].hash;
        if right_component_width == total_cols {
            if let Some(s) = self.status.as_mut() {
                s.draw(grid, area, context);
            } else {
                self.mailbox_list.draw(grid, area, context);
            }
        } else if right_component_width == 0 {
            self.draw_menu(grid, area, context);
        } else {
            self.draw_menu(
                grid,
                (upper_left, (mid.saturating_sub(1), get_y(bottom_right))),
                context,
            );
            if let Some(s) = self.status.as_mut() {
                s.draw(grid, (set_x(upper_left, mid + 1), bottom_right), context);
            } else {
                self.mailbox_list
                    .draw(grid, (set_x(upper_left, mid + 1), bottom_right), context);
            }
        }
        self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.theme_default = crate::conf::value(context, "theme_default");
                let account_hash = context.accounts[self.cursor_pos.0].hash();
                self.sidebar_divider =
                    *account_settings!(context[account_hash].listing.sidebar_divider);
                self.sidebar_divider_theme = conf::value(context, "mail.sidebar_divider");
                self.menu_content = CellBuffer::new_with_context(0, 0, None, context);
                self.set_dirty(true);
            }
            UIEvent::Timer(n) if *n == self.menu_scrollbar_show_timer.id() => {
                if self.show_menu_scrollbar == ShowMenuScrollbar::True {
                    self.show_menu_scrollbar = ShowMenuScrollbar::False;
                    self.set_dirty(true);
                    self.menu_content.empty();
                }
                return true;
            }
            UIEvent::StartupCheck(ref f) => {
                if self.mailbox_list.coordinates.1 == *f {
                    if !self.startup_checks_rate.tick() {
                        return false;
                    }
                }
            }
            UIEvent::Timer(n) if *n == self.startup_checks_rate.id() => {
                if self.startup_checks_rate.active {
                    self.startup_checks_rate.reset();
                    return self.process_event(
                        &mut UIEvent::StartupCheck(self.mailbox_list.coordinates.1),
                        context,
                    );
                }
            }
            UIEvent::AccountStatusChange(account_hash) => {
                let account_index: usize = context
                    .accounts
                    .get_index_of(account_hash)
                    .expect("Invalid account_hash in UIEventMailbox{Delete,Create}");
                if self.cursor_pos.0 == account_index {
                    self.change_account(context);
                } else {
                    self.accounts[account_index].entries = context.accounts[&*account_hash]
                        .list_mailboxes()
                        .into_iter()
                        .filter(|mailbox_node| {
                            context.accounts[&*account_hash][&mailbox_node.hash]
                                .ref_mailbox
                                .is_subscribed()
                        })
                        .map(|f| (f.depth, f.indentation, f.has_sibling, f.hash))
                        .collect::<_>();
                    self.set_dirty(true);
                    self.menu_content.empty();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                            self.get_status(context),
                        )));
                }
            }
            UIEvent::MailboxDelete((account_hash, mailbox_hash))
            | UIEvent::MailboxCreate((account_hash, mailbox_hash)) => {
                let account_index = context
                    .accounts
                    .get_index_of(account_hash)
                    .expect("Invalid account_hash in UIEventMailbox{Delete,Create}");
                self.menu_content.empty();
                self.accounts[account_index].entries = context.accounts[&*account_hash]
                    .list_mailboxes()
                    .into_iter()
                    .filter(|mailbox_node| {
                        context.accounts[&*account_hash][&mailbox_node.hash]
                            .ref_mailbox
                            .is_subscribed()
                    })
                    .map(|f| (f.depth, f.indentation, f.has_sibling, f.hash))
                    .collect::<_>();
                let mut fallback = 0;
                if let MenuEntryCursor::Mailbox(ref mut cur) = self.cursor_pos.1 {
                    *cur = std::cmp::min(
                        self.accounts[self.cursor_pos.0]
                            .entries
                            .len()
                            .saturating_sub(1),
                        *cur,
                    );
                    fallback = *cur;
                }
                if self.mailbox_list.coordinates == (*account_hash, *mailbox_hash) {
                    self.mailbox_list.coordinates = (
                        self.accounts[self.cursor_pos.0].hash,
                        self.accounts[self.cursor_pos.0].entries[fallback].3,
                    );
                    self.mailbox_list.refresh_mailbox(context, true);
                }
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.get_status(context),
                    )));
                self.set_dirty(true);
                return true;
            }
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.set_dirty(true);
            }
            UIEvent::Resize => {
                self.set_dirty(true);
            }
            UIEvent::Action(Action::ViewMailbox(ref idx)) => {
                if let Some((_, _, _, mailbox_hash)) =
                    self.accounts[self.cursor_pos.0].entries.get(*idx)
                {
                    let account_hash = self.accounts[self.cursor_pos.0].hash;
                    self.cursor_pos.1 = MenuEntryCursor::Mailbox(*idx);
                    self.status = None;
                    self.mailbox_list
                        .set_coordinates((account_hash, *mailbox_hash), context);
                    self.menu_content.empty();
                    self.set_dirty(true);
                }
                return true;
            }
            _ => {}
        }

        if self.focus == ListingFocus::Mailbox && self.status.is_some() {
            if let Some(s) = self.status.as_mut() {
                if s.process_event(event, context) {
                    return true;
                }
            }
        }
        if self.focus == ListingFocus::Mailbox
            && self.status.is_none()
            && self.mailbox_list.process_event(event, context)
        {
            return true;
        }

        let shortcuts = self.get_shortcuts(context);
        if self.focus == ListingFocus::Mailbox {
            match *event {
                UIEvent::Input(Key::Mouse(MouseEvent::Press(MouseButton::Left, x, _y)))
                    if self.menu_visibility =>
                {
                    match self.menu_width {
                        WidgetWidth::Hold(wx) | WidgetWidth::Set(wx)
                            if wx + 1 == usize::from(x) =>
                        {
                            self.menu_width = WidgetWidth::Hold(wx - 1);
                        }
                        WidgetWidth::Set(_) => return false,
                        WidgetWidth::Hold(x) => {
                            self.menu_width = WidgetWidth::Set(x);
                        }
                        WidgetWidth::Unset => return false,
                    }
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(Key::Mouse(MouseEvent::Hold(x, _y))) if self.menu_visibility => {
                    match self.menu_width {
                        WidgetWidth::Hold(ref mut hx) => {
                            *hx = usize::from(x).saturating_sub(1);
                        }
                        _ => return false,
                    }
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(Key::Mouse(MouseEvent::Release(x, _y))) if self.menu_visibility => {
                    match self.menu_width {
                        WidgetWidth::Hold(_) => {
                            self.menu_width = WidgetWidth::Set(usize::from(x).saturating_sub(1));
                        }
                        _ => return false,
                    }
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(Key::Left) if self.menu_visibility => {
                    self.focus = ListingFocus::Menu;
                    if self.show_menu_scrollbar != ShowMenuScrollbar::Never {
                        self.menu_scrollbar_show_timer.rearm();
                        self.show_menu_scrollbar = ShowMenuScrollbar::True;
                    }
                    self.ratio = 50;
                    self.set_dirty(true);
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_mailbox"])
                        || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_mailbox"]) =>
                {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.mailbox_list.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.mailbox_list.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    let target = match k {
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_mailbox"]) => {
                            match self.cursor_pos.1 {
                                MenuEntryCursor::Status => amount.saturating_sub(1),
                                MenuEntryCursor::Mailbox(idx) => idx + amount,
                            }
                        }
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_mailbox"]) => {
                            match self.cursor_pos.1 {
                                MenuEntryCursor::Status => {
                                    return true;
                                }
                                MenuEntryCursor::Mailbox(idx) => {
                                    if idx >= amount {
                                        idx - amount
                                    } else {
                                        return true;
                                    }
                                }
                            }
                        }
                        _ => return true,
                    };
                    if self.accounts[self.cursor_pos.0]
                        .entries
                        .get(target)
                        .is_some()
                    {
                        self.cursor_pos.1 = MenuEntryCursor::Mailbox(target)
                    } else {
                        return true;
                    }
                    self.change_account(context);
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_account"])
                        || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_account"]) =>
                {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.mailbox_list.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.mailbox_list.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    match k {
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_account"]) => {
                            if self.cursor_pos.0 + amount < self.accounts.len() {
                                self.cursor_pos =
                                    (self.cursor_pos.0 + amount, MenuEntryCursor::Mailbox(0));
                            } else {
                                return true;
                            }
                        }
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_account"]) => {
                            if self.cursor_pos.0 >= amount {
                                self.cursor_pos =
                                    (self.cursor_pos.0 - amount, MenuEntryCursor::Mailbox(0));
                            } else {
                                return true;
                            }
                        }
                        _ => return false,
                    }
                    self.change_account(context);

                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(
                        k == shortcuts[Listing::DESCRIPTION]["toggle_menu_visibility"]
                    ) =>
                {
                    self.menu_visibility = !self.menu_visibility;
                    self.set_dirty(true);
                }
                _ => {}
            }

            if self.status.is_none() {
                match event {
                    UIEvent::Action(ref action) => match action {
                        Action::Listing(ListingAction::SetPlain) => {
                            self.mailbox_list.set_style(IndexStyle::Plain, context);
                            return true;
                        }
                        Action::Listing(ListingAction::SetThreaded) => {
                            self.mailbox_list.set_style(IndexStyle::Threaded, context);
                            return true;
                        }
                        Action::Listing(ListingAction::SetCompact) => {
                            self.mailbox_list.set_style(IndexStyle::Compact, context);
                            return true;
                        }
                        Action::Listing(ListingAction::SetConversations) => {
                            self.mailbox_list
                                .set_style(IndexStyle::Conversations, context);
                            return true;
                        }
                        Action::Listing(ListingAction::Import(file_path, mailbox_path)) => {
                            let account = &mut context.accounts[self.cursor_pos.0];
                            if let Err(err) = account
                                .mailbox_by_path(&mailbox_path)
                                .and_then(|mailbox_hash| {
                                    Ok((
                                        std::fs::read(&file_path).chain_err_summary(|| {
                                            format!("Could not read {}", file_path.display())
                                        })?,
                                        mailbox_hash,
                                    ))
                                })
                                .and_then(|(bytes, mailbox_hash)| {
                                    account.save(&bytes, mailbox_hash, None)
                                })
                            {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(err.to_string()),
                                ));
                            }
                            return true;
                        }
                        Action::Listing(a @ ListingAction::SetSeen)
                        | Action::Listing(a @ ListingAction::SetUnseen)
                        | Action::Listing(a @ ListingAction::Delete)
                        | Action::Listing(a @ ListingAction::CopyTo(_))
                        | Action::Listing(a @ ListingAction::MoveTo(_))
                        | Action::Listing(a @ ListingAction::CopyToOtherAccount(_, _))
                        | Action::Listing(a @ ListingAction::MoveToOtherAccount(_, _))
                        | Action::Listing(a @ ListingAction::Tag(_)) => {
                            let focused = self.mailbox_list.get_focused_items(context);
                            self.mailbox_list.perform_action(context, focused, a);
                            let mut row_updates: SmallVec<[ThreadHash; 8]> = SmallVec::new();
                            for (k, v) in self.mailbox_list.selection().iter_mut() {
                                if *v {
                                    *v = false;
                                    row_updates.push(*k);
                                }
                            }
                        }
                        _ => {}
                    },
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Listing::DESCRIPTION]["scroll_up"]) =>
                    {
                        let amount = if self.cmd_buf.is_empty() {
                            1
                        } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                            self.cmd_buf.clear();
                            self.mailbox_list.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            amount
                        } else {
                            self.cmd_buf.clear();
                            self.mailbox_list.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            return true;
                        };
                        self.mailbox_list.set_movement(PageMovement::Up(amount));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Listing::DESCRIPTION]["scroll_down"]) =>
                    {
                        let amount = if self.cmd_buf.is_empty() {
                            1
                        } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                            self.cmd_buf.clear();
                            self.mailbox_list.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            amount
                        } else {
                            self.cmd_buf.clear();
                            self.mailbox_list.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            return true;
                        };
                        self.mailbox_list.set_movement(PageMovement::Down(amount));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Listing::DESCRIPTION]["prev_page"]) =>
                    {
                        let mult = if self.cmd_buf.is_empty() {
                            1
                        } else if let Ok(mult) = self.cmd_buf.parse::<usize>() {
                            self.cmd_buf.clear();
                            self.mailbox_list.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            mult
                        } else {
                            self.cmd_buf.clear();
                            self.mailbox_list.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            return true;
                        };
                        self.mailbox_list.set_movement(PageMovement::PageUp(mult));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Listing::DESCRIPTION]["next_page"]) =>
                    {
                        let mult = if self.cmd_buf.is_empty() {
                            1
                        } else if let Ok(mult) = self.cmd_buf.parse::<usize>() {
                            self.cmd_buf.clear();
                            self.mailbox_list.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            mult
                        } else {
                            self.cmd_buf.clear();
                            self.mailbox_list.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            return true;
                        };
                        self.mailbox_list.set_movement(PageMovement::PageDown(mult));
                        return true;
                    }
                    UIEvent::Input(ref key) if *key == Key::Home => {
                        self.mailbox_list.set_movement(PageMovement::Home);
                        return true;
                    }
                    UIEvent::Input(ref key) if *key == Key::End => {
                        self.mailbox_list.set_movement(PageMovement::End);
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Listing::DESCRIPTION]["search"]) =>
                    {
                        context
                            .replies
                            .push_back(UIEvent::CmdInput(Key::Paste("search ".to_string())));
                        context
                            .replies
                            .push_back(UIEvent::ChangeMode(UIMode::Command));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Listing::DESCRIPTION]["set_seen"]) =>
                    {
                        let mut event = UIEvent::Action(Action::Listing(ListingAction::SetSeen));
                        if self.process_event(&mut event, context) {
                            return true;
                        }
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Listing::DESCRIPTION]["refresh"]) =>
                    {
                        let account = &mut context.accounts[self.cursor_pos.0];
                        if let MenuEntryCursor::Mailbox(idx) = self.cursor_pos.1 {
                            if let Some(&mailbox_hash) = account.mailboxes_order.get(idx) {
                                if let Err(err) = account.refresh(mailbox_hash) {
                                    context.replies.push_back(UIEvent::Notification(
                                        Some("Could not refresh.".to_string()),
                                        err.to_string(),
                                        Some(NotificationType::Error(err.kind)),
                                    ));
                                }
                            }
                        }
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if !self.mailbox_list.unfocused()
                            && shortcut!(
                                key == shortcuts[Listing::DESCRIPTION]["union_modifier"]
                            )
                            && self.mailbox_list.modifier_command().is_some() =>
                    {
                        self.mailbox_list
                            .set_modifier_command(Some(Modifier::Union));
                    }
                    UIEvent::Input(ref key)
                        if !self.mailbox_list.unfocused()
                            && shortcut!(
                                key == shortcuts[Listing::DESCRIPTION]["diff_modifier"]
                            )
                            && self.mailbox_list.modifier_command().is_some() =>
                    {
                        self.mailbox_list
                            .set_modifier_command(Some(Modifier::Difference));
                    }
                    UIEvent::Input(ref key)
                        if !self.mailbox_list.unfocused()
                            && shortcut!(
                                key == shortcuts[Listing::DESCRIPTION]["intersection_modifier"]
                            )
                            && self.mailbox_list.modifier_command().is_some() =>
                    {
                        self.mailbox_list
                            .set_modifier_command(Some(Modifier::Intersection));
                    }
                    _ => {}
                }
            }
        } else if self.focus == ListingFocus::Menu {
            match *event {
                UIEvent::Input(Key::Right) => {
                    self.focus = ListingFocus::Mailbox;
                    self.ratio = 90;
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["open_mailbox"])
                        && self.menu_cursor_pos.1 == MenuEntryCursor::Status =>
                {
                    self.cursor_pos = self.menu_cursor_pos;
                    self.open_status(self.menu_cursor_pos.0, context);
                    self.set_dirty(true);
                    self.focus = ListingFocus::Mailbox;
                    self.ratio = 90;
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["open_mailbox"]) =>
                {
                    self.cursor_pos = self.menu_cursor_pos;
                    self.change_account(context);
                    self.focus = ListingFocus::Mailbox;
                    self.ratio = 90;
                    self.set_dirty(true);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                            self.get_status(context),
                        )));
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["scroll_up"])
                        || shortcut!(k == shortcuts[Listing::DESCRIPTION]["scroll_down"]) =>
                {
                    let mut amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.mailbox_list.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.mailbox_list.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["scroll_up"]) {
                        while amount > 0 {
                            match self.menu_cursor_pos {
                                (
                                    ref mut account_cursor,
                                    ref mut entry_cursor @ MenuEntryCursor::Status,
                                ) => {
                                    if *account_cursor > 0 {
                                        *account_cursor -= 1;
                                        *entry_cursor = MenuEntryCursor::Mailbox(
                                            self.accounts[*account_cursor]
                                                .entries
                                                .len()
                                                .saturating_sub(1),
                                        );
                                    } else {
                                        return true;
                                    }
                                }
                                (_, MenuEntryCursor::Mailbox(ref mut mailbox_idx)) => {
                                    if *mailbox_idx > 0 {
                                        *mailbox_idx -= 1;
                                    } else {
                                        self.menu_cursor_pos.1 = MenuEntryCursor::Status;
                                    }
                                }
                            }

                            amount -= 1;
                        }
                    } else if shortcut!(k == shortcuts[Listing::DESCRIPTION]["scroll_down"]) {
                        while amount > 0 {
                            match self.menu_cursor_pos {
                                /* If current account has mailboxes, go to first mailbox */
                                (
                                    ref account_cursor,
                                    ref mut entry_cursor @ MenuEntryCursor::Status,
                                ) if !self.accounts[*account_cursor].entries.is_empty() => {
                                    *entry_cursor = MenuEntryCursor::Mailbox(0);
                                }
                                /* If current account has no mailboxes, go to next account */
                                (
                                    ref mut account_cursor,
                                    ref mut entry_cursor @ MenuEntryCursor::Status,
                                ) if *account_cursor + 1 < self.accounts.len() => {
                                    *account_cursor += 1;
                                    *entry_cursor = MenuEntryCursor::Status;
                                }
                                /* If current account has no mailboxes and there is no next account, return true */
                                (_, MenuEntryCursor::Status) => {
                                    return true;
                                }
                                (
                                    ref mut account_cursor,
                                    MenuEntryCursor::Mailbox(ref mut mailbox_idx),
                                ) => {
                                    if (*mailbox_idx + 1)
                                        < self.accounts[*account_cursor].entries.len()
                                    {
                                        *mailbox_idx += 1;
                                    } else if *account_cursor + 1 < self.accounts.len() {
                                        *account_cursor += 1;
                                        self.menu_cursor_pos.1 = MenuEntryCursor::Status;
                                    } else {
                                        return true;
                                    }
                                }
                            }

                            amount -= 1;
                        }
                    }
                    if self.show_menu_scrollbar != ShowMenuScrollbar::Never {
                        self.menu_scrollbar_show_timer.rearm();
                        self.show_menu_scrollbar = ShowMenuScrollbar::True;
                    }
                    self.menu_content.empty();
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_mailbox"])
                        || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_mailbox"]) =>
                {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.mailbox_list.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.mailbox_list.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    let target = match k {
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_mailbox"]) => {
                            match self.menu_cursor_pos.1 {
                                MenuEntryCursor::Status => amount.saturating_sub(1),
                                MenuEntryCursor::Mailbox(idx) => idx + amount,
                            }
                        }
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_mailbox"]) => {
                            match self.menu_cursor_pos.1 {
                                MenuEntryCursor::Status => {
                                    return true;
                                }
                                MenuEntryCursor::Mailbox(idx) => {
                                    if idx >= amount {
                                        idx - amount
                                    } else {
                                        return true;
                                    }
                                }
                            }
                        }
                        _ => return true,
                    };
                    if self.accounts[self.menu_cursor_pos.0]
                        .entries
                        .get(target)
                        .is_some()
                    {
                        self.menu_cursor_pos.1 = MenuEntryCursor::Mailbox(target)
                    } else {
                        return true;
                    }
                    if self.show_menu_scrollbar != ShowMenuScrollbar::Never {
                        self.menu_scrollbar_show_timer.rearm();
                        self.show_menu_scrollbar = ShowMenuScrollbar::True;
                    }
                    self.menu_content.empty();
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_account"])
                        || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_account"])
                        || shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_page"])
                        || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_page"]) =>
                {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.mailbox_list.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.mailbox_list.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    match k {
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_account"])
                            || shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_page"]) =>
                        {
                            if self.menu_cursor_pos.0 + amount < self.accounts.len() {
                                self.menu_cursor_pos =
                                    (self.menu_cursor_pos.0 + amount, MenuEntryCursor::Mailbox(0));
                            } else {
                                return true;
                            }
                        }
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_account"])
                            || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_page"]) =>
                        {
                            if self.menu_cursor_pos.0 >= amount {
                                self.menu_cursor_pos =
                                    (self.menu_cursor_pos.0 - amount, MenuEntryCursor::Mailbox(0));
                            } else {
                                return true;
                            }
                        }
                        _ => return false,
                    }
                    if self.show_menu_scrollbar != ShowMenuScrollbar::Never {
                        self.menu_scrollbar_show_timer.rearm();
                        self.show_menu_scrollbar = ShowMenuScrollbar::True;
                    }
                    self.menu_content.empty();
                    self.set_dirty(true);

                    return true;
                }
                _ => {}
            }
        }
        match *event {
            UIEvent::Input(ref k)
                if shortcut!(k == shortcuts[Listing::DESCRIPTION]["new_mail"]) =>
            {
                let account_hash = context.accounts[self.cursor_pos.0].hash();
                let composer = Composer::with_account(account_hash, context);
                context
                    .replies
                    .push_back(UIEvent::Action(Tab(New(Some(Box::new(composer))))));
                return true;
            }
            UIEvent::StartupCheck(_)
            | UIEvent::MailboxUpdate(_)
            | UIEvent::EnvelopeUpdate(_)
            | UIEvent::EnvelopeRename(_, _)
            | UIEvent::EnvelopeRemove(_, _) => {
                self.dirty = true;
                /* clear menu to force redraw */
                self.menu_content.empty();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.get_status(context),
                    )));
            }
            UIEvent::Input(Key::Backspace) if !self.cmd_buf.is_empty() => {
                self.cmd_buf.pop();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufSet(
                        self.cmd_buf.clone(),
                    )));
                return true;
            }
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt('')) if !self.cmd_buf.is_empty() => {
                self.cmd_buf.clear();
                self.mailbox_list.set_modifier_active(false);
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                return true;
            }
            UIEvent::Input(Key::Char(c)) if c >= '0' && c <= '9' => {
                self.cmd_buf.push(c);
                self.mailbox_list.set_modifier_active(true);
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufSet(
                        self.cmd_buf.clone(),
                    )));
                return true;
            }
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty
            || self
                .status
                .as_ref()
                .map(Component::is_dirty)
                .unwrap_or_else(|| self.mailbox_list.is_dirty())
    }
    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        if let Some(s) = self.status.as_mut() {
            s.set_dirty(value);
        } else {
            self.mailbox_list.set_dirty(value);
        }
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if let Some(s) = self.status.as_ref() {
            s.get_shortcuts(context)
        } else {
            self.mailbox_list.get_shortcuts(context)
        };
        let mut config_map = context.settings.shortcuts.listing.key_values();
        if self.focus != ListingFocus::Menu {
            config_map.remove("open_mailbox");
        }
        map.insert(Listing::DESCRIPTION, config_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.mailbox_list.id()
    }
    fn set_id(&mut self, id: ComponentId) {
        self.mailbox_list.set_id(id);
    }

    fn get_status(&self, context: &Context) -> String {
        let mailbox_hash = match self.cursor_pos.1 {
            MenuEntryCursor::Mailbox(idx) => {
                if let Some((_, _, _, mailbox_hash)) =
                    self.accounts[self.cursor_pos.0].entries.get(idx)
                {
                    *mailbox_hash
                } else {
                    return String::new();
                }
            }
            MenuEntryCursor::Status => {
                return format!("{} status", &self.accounts[self.cursor_pos.0].name)
            }
        };

        let account = &context.accounts[self.cursor_pos.0];
        use crate::conf::accounts::MailboxStatus;
        match account[&mailbox_hash].status {
            MailboxStatus::Available | MailboxStatus::Parsing(_, _) => {
                let (unseen, total) = account[&mailbox_hash]
                    .ref_mailbox
                    .count()
                    .ok()
                    .unwrap_or((0, 0));
                format!(
                    "Mailbox: {}, Messages: {}, New: {}{}",
                    account[&mailbox_hash].name(),
                    total,
                    unseen,
                    if account[&mailbox_hash].status.is_parsing() {
                        "(Loading...)"
                    } else {
                        ""
                    }
                )
            }
            MailboxStatus::Failed(_) | MailboxStatus::None => account[&mailbox_hash].status(),
        }
    }
}

impl Listing {
    pub const DESCRIPTION: &'static str = "listing";
    pub fn new(context: &mut Context) -> Self {
        let account_entries: Vec<AccountMenuEntry> = context
            .accounts
            .iter()
            .enumerate()
            .map(|(i, (h, a))| {
                let entries: SmallVec<[(usize, u32, bool, MailboxHash); 16]> = a
                    .list_mailboxes()
                    .into_iter()
                    .filter(|mailbox_node| a[&mailbox_node.hash].ref_mailbox.is_subscribed())
                    .map(|f| (f.depth, f.indentation, f.has_sibling, f.hash))
                    .collect::<_>();

                AccountMenuEntry {
                    name: a.name().to_string(),
                    hash: *h,
                    index: i,
                    entries,
                }
            })
            .collect();
        let first_account_hash = account_entries[0].hash;
        let mut ret = Listing {
            mailbox_list: MailboxList::new((first_account_hash, 0), context),
            accounts: account_entries,
            status: None,
            visible: true,
            dirty: true,
            cursor_pos: (0, MenuEntryCursor::Mailbox(0)),
            menu_cursor_pos: (0, MenuEntryCursor::Mailbox(0)),
            menu_content: CellBuffer::new_with_context(0, 0, None, context),
            menu_scrollbar_show_timer: context.job_executor.clone().create_timer(
                std::time::Duration::from_secs(0),
                std::time::Duration::from_millis(1200),
            ),
            show_menu_scrollbar: ShowMenuScrollbar::Never,
            startup_checks_rate: RateLimit::new(2, 1000, context.job_executor.clone()),
            theme_default: conf::value(context, "theme_default"),
            id: ComponentId::new_v4(),
            sidebar_divider: *account_settings!(
                context[first_account_hash].listing.sidebar_divider
            ),
            sidebar_divider_theme: conf::value(context, "mail.sidebar_divider"),
            menu_visibility: true,
            ratio: 90,
            menu_width: WidgetWidth::Unset,
            focus: ListingFocus::Mailbox,
            cmd_buf: String::with_capacity(4),
        };
        ret.change_account(context);
        ret
    }

    fn draw_menu(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        clear_area(grid, area, self.theme_default);
        let total_height: usize = 3 * (self.accounts.len())
            + self
                .accounts
                .iter()
                .map(|entry| entry.entries.len() + 1)
                .sum::<usize>();
        let min_width: usize = 2 * width!(area);
        let (width, height) = self.menu_content.size();
        let cursor = match self.focus {
            ListingFocus::Mailbox => self.cursor_pos,
            ListingFocus::Menu => self.menu_cursor_pos,
        };
        if min_width > width || height < total_height || self.dirty {
            let _ = self.menu_content.resize(min_width * 2, total_height, None);
            let bottom_right = pos_dec(self.menu_content.size(), (1, 1));
            let mut y = 0;
            for a in 0..self.accounts.len() {
                if y > get_y(bottom_right) {
                    break;
                }
                y += self.print_account(((0, y), bottom_right), a, context);
                y += 3;
            }
        }

        let rows = height!(area);
        let (width, height) = self.menu_content.size();
        const SCROLLING_CONTEXT: usize = 3;
        let y_offset = (cursor.0)
            + self
                .accounts
                .iter()
                .take(cursor.0)
                .map(|entry| entry.entries.len() + 1)
                .sum::<usize>()
            + match cursor.1 {
                MenuEntryCursor::Status => 0,
                MenuEntryCursor::Mailbox(idx) => idx + 1,
            }
            + SCROLLING_CONTEXT;
        let skip_offset = if y_offset <= rows {
            0
        } else {
            rows * y_offset.wrapping_div(rows).saturating_sub(1) + y_offset.wrapping_rem(rows)
        };

        copy_area(
            grid,
            &self.menu_content,
            area,
            (
                (
                    0,
                    std::cmp::min((height - 1).saturating_sub(rows), skip_offset),
                ),
                (width - 1, std::cmp::min(skip_offset + rows, height - 1)),
            ),
        );
        if self.show_menu_scrollbar == ShowMenuScrollbar::True && total_height > rows {
            ScrollBar::default().set_show_arrows(true).draw(
                grid,
                (
                    pos_inc(upper_left!(area), (width!(area), 0)),
                    bottom_right!(area),
                ),
                context,
                /* position */
                skip_offset,
                /* visible_rows */
                rows,
                /* length */
                total_height,
            );
        }

        context.dirty_areas.push_back(area);
    }

    /*
     * Print a single account in the menu area.
     */
    fn print_account(&mut self, area: Area, aidx: usize, context: &mut Context) -> usize {
        debug_assert!(is_valid_area!(area));
        // Each entry and its index in the account
        let mailboxes: HashMap<MailboxHash, Mailbox> = context.accounts[self.accounts[aidx].index]
            .mailbox_entries
            .iter()
            .map(|(&hash, entry)| (hash, entry.ref_mailbox.clone()))
            .collect();

        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let cursor = match self.focus {
            ListingFocus::Mailbox => self.cursor_pos,
            ListingFocus::Menu => self.menu_cursor_pos,
        };

        let must_highlight_account: bool = cursor.0 == self.accounts[aidx].index;

        let mut lines: Vec<(usize, usize, u32, bool, MailboxHash, Option<usize>)> = Vec::new();

        for (i, &(depth, indentation, has_sibling, mailbox_hash)) in
            self.accounts[aidx].entries.iter().enumerate()
        {
            if mailboxes[&mailbox_hash].is_subscribed() {
                match context.accounts[self.accounts[aidx].index][&mailbox_hash].status {
                    crate::conf::accounts::MailboxStatus::Failed(_) => {
                        lines.push((depth, i, indentation, has_sibling, mailbox_hash, None));
                    }
                    _ => {
                        lines.push((
                            depth,
                            i,
                            indentation,
                            has_sibling,
                            mailbox_hash,
                            mailboxes[&mailbox_hash].count().ok().map(|(v, _)| v),
                        ));
                    }
                }
            }
        }

        let account_attrs = if must_highlight_account {
            if cursor.1 == MenuEntryCursor::Status {
                let mut v = crate::conf::value(context, "mail.sidebar_highlighted");
                if !context.settings.terminal.use_color() {
                    v.attrs |= Attr::REVERSE;
                }
                v
            } else {
                crate::conf::value(context, "mail.sidebar_highlighted_account_name")
            }
        } else {
            crate::conf::value(context, "mail.sidebar_account_name")
        };

        /* Print account name first */
        write_string_to_grid(
            &self.accounts[aidx].name,
            &mut self.menu_content,
            account_attrs.fg,
            account_attrs.bg,
            account_attrs.attrs,
            area,
            None,
        );

        if lines.is_empty() {
            write_string_to_grid(
                "offline",
                &mut self.menu_content,
                Color::Byte(243),
                account_attrs.bg,
                account_attrs.attrs,
                (pos_inc(upper_left, (0, 1)), bottom_right),
                None,
            );
            return 0;
        }

        let lines_len = lines.len();
        let mut idx = 0;
        let mut branches = String::with_capacity(16);

        for y in get_y(upper_left) + 1..get_y(bottom_right) {
            if idx == lines_len {
                break;
            }
            let (att, index_att, unread_count_att) = if must_highlight_account {
                if match cursor.1 {
                    MenuEntryCursor::Mailbox(c) => c == idx,
                    _ => false,
                } {
                    let mut ret = (
                        crate::conf::value(context, "mail.sidebar_highlighted"),
                        crate::conf::value(context, "mail.sidebar_highlighted_index"),
                        crate::conf::value(context, "mail.sidebar_highlighted_unread_count"),
                    );

                    if !context.settings.terminal.use_color() {
                        ret.0.attrs |= Attr::REVERSE;
                        ret.1.attrs |= Attr::REVERSE;
                        ret.2.attrs |= Attr::REVERSE;
                    }
                    ret
                } else {
                    (
                        crate::conf::value(context, "mail.sidebar_highlighted_account"),
                        crate::conf::value(context, "mail.sidebar_highlighted_account_index"),
                        crate::conf::value(
                            context,
                            "mail.sidebar_highlighted_account_unread_count",
                        ),
                    )
                }
            } else {
                (
                    crate::conf::value(context, "mail.sidebar"),
                    crate::conf::value(context, "mail.sidebar_index"),
                    crate::conf::value(context, "mail.sidebar_unread_count"),
                )
            };

            let (depth, inc, indentation, has_sibling, mailbox_idx, count) = lines[idx];
            /* Calculate how many columns the mailbox index tags should occupy with right alignment,
             * eg.
             *  1
             *  2
             * ...
             *  9
             * 10
             */
            let total_mailbox_no_digits = {
                let mut len = lines_len;
                let mut ctr = 1;
                while len > 9 {
                    ctr += 1;
                    len /= 10;
                }
                ctr
            };

            let has_sibling_str: &str = account_settings!(
                context[self.accounts[aidx].hash]
                    .listing
                    .sidebar_mailbox_tree_has_sibling
            )
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or(" ");
            let no_sibling_str: &str = account_settings!(
                context[self.accounts[aidx].hash]
                    .listing
                    .sidebar_mailbox_tree_no_sibling
            )
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or(" ");

            let has_sibling_leaf_str: &str = account_settings!(
                context[self.accounts[aidx].hash]
                    .listing
                    .sidebar_mailbox_tree_has_sibling_leaf
            )
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or(" ");

            let no_sibling_leaf_str: &str = account_settings!(
                context[self.accounts[aidx].hash]
                    .listing
                    .sidebar_mailbox_tree_no_sibling_leaf
            )
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or(" ");

            let (x, _) = write_string_to_grid(
                &format!("{:>width$}", inc, width = total_mailbox_no_digits),
                &mut self.menu_content,
                index_att.fg,
                index_att.bg,
                index_att.attrs,
                (set_y(upper_left, y), bottom_right),
                None,
            );
            {
                branches.clear();
                branches.push_str(no_sibling_str);
                let leading_zeros = indentation.leading_zeros();
                let mut o = 1_u32.wrapping_shl(31_u32.saturating_sub(leading_zeros));
                for _ in 0..(32_u32.saturating_sub(leading_zeros)) {
                    if indentation & o > 0 {
                        branches.push_str(has_sibling_str);
                    } else {
                        branches.push_str(no_sibling_str);
                    }
                    o >>= 1;
                }
                if depth > 0 {
                    if has_sibling {
                        branches.push_str(has_sibling_leaf_str);
                    } else {
                        branches.push_str(no_sibling_leaf_str);
                    }
                }
            }
            let (x, _) = write_string_to_grid(
                &branches,
                &mut self.menu_content,
                att.fg,
                att.bg,
                att.attrs,
                ((x, y), bottom_right),
                None,
            );
            let (x, _) = write_string_to_grid(
                context.accounts[self.accounts[aidx].index].mailbox_entries[&mailbox_idx].name(),
                &mut self.menu_content,
                att.fg,
                att.bg,
                att.attrs,
                ((x, y), bottom_right),
                None,
            );

            /* Unread message count */
            let count_string = if let Some(c) = count {
                if c > 0 {
                    format!(" {}", c)
                } else {
                    String::new()
                }
            } else {
                " ...".to_string()
            };

            let (x, _) = write_string_to_grid(
                &count_string,
                &mut self.menu_content,
                unread_count_att.fg,
                unread_count_att.bg,
                unread_count_att.attrs
                    | if count.unwrap_or(0) > 0 {
                        Attr::BOLD
                    } else {
                        Attr::DEFAULT
                    },
                (
                    (
                        /* Hide part of mailbox name if need be to fit the message count */
                        std::cmp::min(x, get_x(bottom_right).saturating_sub(count_string.len())),
                        y,
                    ),
                    bottom_right,
                ),
                None,
            );
            for c in self.menu_content.row_iter(x..(get_x(bottom_right) + 1), y) {
                self.menu_content[c]
                    .set_fg(att.fg)
                    .set_bg(att.bg)
                    .set_attrs(att.attrs);
            }
            idx += 1;
        }
        if idx == 0 {
            0
        } else {
            idx - 1
        }
    }

    fn change_account(&mut self, context: &mut Context) {
        let account_hash = context.accounts[self.cursor_pos.0].hash();
        self.accounts[self.cursor_pos.0].entries = context.accounts[self.cursor_pos.0]
            .list_mailboxes()
            .into_iter()
            .filter(|mailbox_node| {
                context.accounts[self.cursor_pos.0][&mailbox_node.hash]
                    .ref_mailbox
                    .is_subscribed()
            })
            .map(|f| (f.depth, f.indentation, f.has_sibling, f.hash))
            .collect::<_>();
        match self.cursor_pos.1 {
            MenuEntryCursor::Mailbox(idx) => {
                /* Account might have no mailboxes yet if it's offline */
                if let Some((_, _, _, mailbox_hash)) =
                    self.accounts[self.cursor_pos.0].entries.get(idx)
                {
                    self.mailbox_list
                        .set_coordinates((account_hash, *mailbox_hash), context);
                } else {
                    self.mailbox_list
                        .set_coordinates((account_hash, 0), context);
                }
                self.status = None;
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.get_status(context),
                    )));
            }
            MenuEntryCursor::Status => {
                self.open_status(self.cursor_pos.0, context);
            }
        }
        self.sidebar_divider = *account_settings!(context[account_hash].listing.sidebar_divider);
        self.set_dirty(true);
        self.menu_cursor_pos = self.cursor_pos;
        /* clear menu to force redraw */
        self.menu_content.empty();
        if *account_settings!(context[account_hash].listing.show_menu_scrollbar) {
            self.show_menu_scrollbar = ShowMenuScrollbar::True;
            self.menu_scrollbar_show_timer.rearm();
        } else {
            self.show_menu_scrollbar = ShowMenuScrollbar::Never;
        }
    }

    fn open_status(&mut self, account_idx: usize, context: &mut Context) {
        self.status = Some(AccountStatus::new(account_idx, self.theme_default));
        self.menu_content.empty();
        context
            .replies
            .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                self.get_status(context),
            )));
    }
}

mod mailbox_list {
    use super::*;
    use crate::jobs::{JobId, JoinHandle};
    use std::collections::HashSet;
    use std::convert::TryInto;

    const LOADING_STR: &str = "...";
    macro_rules! row_attr {
        ($color_cache:expr, $even: expr, $unseen:expr, $highlighted:expr, $selected:expr  $(,)*) => {{
            ThemeAttribute {
                fg: if $highlighted {
                    if $even {
                        $color_cache.even_highlighted.fg
                    } else {
                        $color_cache.odd_highlighted.fg
                    }
                } else if $selected {
                    if $even {
                        $color_cache.even_selected.fg
                    } else {
                        $color_cache.odd_selected.fg
                    }
                } else if $unseen {
                    if $even {
                        $color_cache.even_unseen.fg
                    } else {
                        $color_cache.odd_unseen.fg
                    }
                } else if $even {
                    $color_cache.even.fg
                } else {
                    $color_cache.odd.fg
                },
                bg: if $highlighted {
                    if $even {
                        $color_cache.even_highlighted.bg
                    } else {
                        $color_cache.odd_highlighted.bg
                    }
                } else if $selected {
                    if $even {
                        $color_cache.even_selected.bg
                    } else {
                        $color_cache.odd_selected.bg
                    }
                } else if $unseen {
                    if $even {
                        $color_cache.even_unseen.bg
                    } else {
                        $color_cache.odd_unseen.bg
                    }
                } else if $even {
                    $color_cache.even.bg
                } else {
                    $color_cache.odd.bg
                },
                attrs: if $highlighted {
                    if $even {
                        $color_cache.even_highlighted.attrs
                    } else {
                        $color_cache.odd_highlighted.attrs
                    }
                } else if $selected {
                    if $even {
                        $color_cache.even_selected.attrs
                    } else {
                        $color_cache.odd_selected.attrs
                    }
                } else if $unseen {
                    if $even {
                        $color_cache.even_unseen.attrs
                    } else {
                        $color_cache.odd_unseen.attrs
                    }
                } else if $even {
                    $color_cache.even.attrs
                } else {
                    $color_cache.odd.attrs
                },
            }
        }};
    }

    #[derive(Debug)]
    pub enum JobState {
        Offline {
            message: String,
        },
        Loading {
            job_id: JobId,
        },
        Loaded,
        Filtering {
            query: String,
            join_handle: JoinHandle<Result<SmallVec<[EnvelopeHash; 512]>>>,
        },
        Searching {
            query: String,
            join_handle: JoinHandle<Result<SmallVec<[EnvelopeHash; 512]>>>,
        },
    }

    impl Default for JobState {
        fn default() -> JobState {
            JobState::Offline {
                message: "uninitialized".to_string(),
            }
        }
    }

    #[derive(Debug, Default)]
    pub struct MailboxListState {
        pub cursor_pos: usize,
        pub new_cursor_pos: usize,
        pub selection: HashMap<ThreadNodeHash, bool>,
        pub movement: Option<PageMovement>,
        pub modifier_active: bool,
        pub modifier_command: Option<Modifier>,
        pub style: IndexStyle,
        pub filter_term: Option<String>,
        pub search_term: Option<String>,
        pub sort: (SortField, SortOrder),
        pub subsort: (SortField, SortOrder),
        pub job_state: JobState,
        pub fetching_envelopes: HashMap<JobId, EnvelopeHashBatch>,
    }

    impl MailboxListState {
        fn new() -> Self {
            MailboxListState {
                cursor_pos: 1,
                sort: (Default::default(), Default::default()),
                subsort: (SortField::Date, SortOrder::Desc),
                ..MailboxListState::default()
            }
        }
    }

    #[derive(Debug)]
    pub struct Entry {
        pub index: usize,
        pub root_envelope_hash: EnvelopeHash,
        pub thread_node_hash: ThreadNodeHash,
        pub thread_hash: ThreadHash,
        pub unseen: bool,
        pub snoozed: bool,
        pub has_attachments: bool,
        pub strings: Option<EntryStrings>,
    }

    #[derive(Debug)]
    pub struct MailboxList {
        pub coordinates: (AccountHash, MailboxHash),
        pub cached_states: HashMap<(AccountHash, MailboxHash), MailboxListState>,
        pub state: MailboxListState,
        pub data_columns: DataColumns,
        pub rows_drawn: SegmentTree,
        color_cache: ColorCache,
        pub entries: HashMap<usize, Entry>,
        pub rows: Vec<(ThreadHash, ThreadNodeHash)>,
        pub unloaded_envelope_hashes: HashMap<EnvelopeHash, usize>,
        /* Store page size to know how many rows to prefetch */
        pub last_recorded_page_size: usize,
        pub length: usize,
        pub id: ComponentId,
        pub unfocused: bool,
        pub view: ThreadView,
        pub force_draw: bool,
        pub dirty: bool,
    }

    impl MailboxList {
        pub fn new(coordinates: (AccountHash, MailboxHash), context: &mut Context) -> Self {
            let mut ret = MailboxList {
                coordinates: (0, 0),
                cached_states: HashMap::default(),
                state: MailboxListState::new(),
                data_columns: DataColumns::default(),
                rows_drawn: SegmentTree::default(),
                entries: HashMap::default(),
                rows: Vec::new(),
                color_cache: ColorCache::default(),
                unloaded_envelope_hashes: HashMap::default(),
                length: 0,
                last_recorded_page_size: 25,
                unfocused: false,
                view: ThreadView::default(),
                force_draw: true,
                dirty: true,
                id: ComponentId::new_v4(),
            };
            for column in ret.data_columns.columns.iter_mut() {
                *column = CellBuffer::new_with_context(0, 0, None, context);
            }
            ret.set_coordinates(coordinates, context);
            ret.update_color_cache(context);
            ret
        }

        pub fn set_style(&mut self, new_value: IndexStyle, context: &Context) {
            if self.state.style == new_value {
                return;
            }
            self.state.style = new_value;
            self.update_color_cache(context);
            self.initialize_columns();
        }

        fn update_color_cache(&mut self, context: &Context) {
            match self.state.style {
                IndexStyle::Conversations => {
                    self.color_cache = ColorCache {
                        theme_default: crate::conf::value(context, "mail.listing.conversations"),
                        subject: crate::conf::value(context, "mail.listing.conversations.subject"),
                        from: crate::conf::value(context, "mail.listing.conversations.from"),
                        date: crate::conf::value(context, "mail.listing.conversations.date"),
                        selected: crate::conf::value(
                            context,
                            "mail.listing.conversations.selected",
                        ),
                        unseen: crate::conf::value(context, "mail.listing.conversations.unseen"),
                        highlighted: crate::conf::value(
                            context,
                            "mail.listing.conversations.highlighted",
                        ),
                        attachment_flag: crate::conf::value(
                            context,
                            "mail.listing.attachment_flag",
                        ),
                        thread_snooze_flag: crate::conf::value(
                            context,
                            "mail.listing.thread_snooze_flag",
                        ),
                        tag_default: crate::conf::value(context, "mail.listing.tag_default"),
                        ..self.color_cache
                    };

                    if !context.settings.terminal.use_color() {
                        self.color_cache.highlighted.attrs |= Attr::REVERSE;
                        self.color_cache.tag_default.attrs |= Attr::REVERSE;
                    }
                }
                IndexStyle::Compact => {
                    self.color_cache = ColorCache {
                        even_unseen: crate::conf::value(
                            context,
                            "mail.listing.compact.even_unseen",
                        ),
                        even_selected: crate::conf::value(
                            context,
                            "mail.listing.compact.even_selected",
                        ),
                        even_highlighted: crate::conf::value(
                            context,
                            "mail.listing.compact.even_highlighted",
                        ),
                        odd_unseen: crate::conf::value(context, "mail.listing.compact.odd_unseen"),
                        odd_selected: crate::conf::value(
                            context,
                            "mail.listing.compact.odd_selected",
                        ),
                        odd_highlighted: crate::conf::value(
                            context,
                            "mail.listing.compact.odd_highlighted",
                        ),
                        even: crate::conf::value(context, "mail.listing.compact.even"),
                        odd: crate::conf::value(context, "mail.listing.compact.odd"),
                        attachment_flag: crate::conf::value(
                            context,
                            "mail.listing.attachment_flag",
                        ),
                        thread_snooze_flag: crate::conf::value(
                            context,
                            "mail.listing.thread_snooze_flag",
                        ),
                        tag_default: crate::conf::value(context, "mail.listing.tag_default"),
                        theme_default: crate::conf::value(context, "theme_default"),
                        ..self.color_cache
                    };
                    if !context.settings.terminal.use_color() {
                        self.color_cache.highlighted.attrs |= Attr::REVERSE;
                        self.color_cache.tag_default.attrs |= Attr::REVERSE;
                        self.color_cache.even_highlighted.attrs |= Attr::REVERSE;
                        self.color_cache.odd_highlighted.attrs |= Attr::REVERSE;
                    }
                }
                IndexStyle::Plain => todo!(),
                IndexStyle::Threaded => todo!(),
            }
        }

        pub fn unfocused(&self) -> bool {
            false
        }

        pub fn set_offline(&mut self, context: &mut Context) {
            let message = if let Err(err) = context.is_online(self.coordinates.0) {
                err.to_string()
            } else {
                use crate::conf::accounts::MailboxStatus;
                let account = &context.accounts[&self.coordinates.0];
                match account[&self.coordinates.1].status {
                    MailboxStatus::Available
                    | MailboxStatus::Parsing(_, _)
                    | MailboxStatus::None => "offline".to_string(),
                    MailboxStatus::Failed(ref err) => err.to_string(),
                }
            };
            self.data_columns.columns[0] =
                CellBuffer::new_with_context(message.len(), 1, None, context);
            write_string_to_grid(
                &message,
                &mut self.data_columns.columns[0],
                self.color_cache.theme_default.fg,
                self.color_cache.theme_default.bg,
                self.color_cache.theme_default.attrs,
                ((0, 0), (message.len() - 1, 0)),
                None,
            );
            self.state.job_state = JobState::Offline { message };
        }

        pub fn set_coordinates(
            &mut self,
            new_val: (AccountHash, MailboxHash),
            context: &mut Context,
        ) {
            if self.coordinates == new_val {
                return;
            }
            let mut is_cached_state = false;
            let new_state = if let Some(cached_state) = self.cached_states.remove(&new_val) {
                is_cached_state = true;
                cached_state
            } else {
                MailboxListState::new()
            };
            self.cached_states.insert(
                self.coordinates,
                std::mem::replace(&mut self.state, new_state),
            );
            self.coordinates = new_val;
            if context.is_online(self.coordinates.0).is_err() {
                self.set_offline(context);
            } else {
                use crate::conf::accounts::MailboxStatus;
                let account = &context.accounts[&self.coordinates.0];
                if account.mailbox_entries.contains_key(&self.coordinates.1) {
                    self.update_job_state(
                        &mut UIEvent::AccountStatusChange(self.coordinates.0),
                        context,
                    );
                    if is_cached_state {
                        self.initialize_rows(context);
                    } else {
                        let style =
                            *mailbox_settings!(context[new_val.0][&new_val.1].listing.index_style);
                        self.set_style(style, context);
                    }
                }
            }
        }

        pub fn set_movement(&mut self, mvm: PageMovement) {
            self.state.movement = Some(mvm);
            self.set_dirty(true);
        }

        pub fn selection(&mut self) -> &mut HashMap<ThreadHash, bool> {
            todo!()
        }
        pub fn perform_action(
            &mut self,
            context: &mut Context,
            thread_hashes: SmallVec<[ThreadHash; 8]>,
            a: &ListingAction,
        ) {
            todo!()
        }
        pub fn refresh_mailbox(&self, _context: &Context, force: bool) {
            todo!()
        }
        pub fn get_focused_items(&self, _context: &Context) -> SmallVec<[ThreadHash; 8]> {
            todo!()
        }
        pub fn set_modifier_active(&mut self, _new_val: bool) {}
        pub fn set_modifier_command(&mut self, _new_val: Option<Modifier>) {}
        pub fn modifier_command(&self) -> Option<Modifier> {
            None
        }

        fn update_job_state(&mut self, event: &mut UIEvent, context: &mut Context) {
            debug!("update_job_state {:?}", &self.state.job_state);
            if let Err(err) = context.is_online(self.coordinates.0) {
                self.set_offline(context);
                return;
            } else {
                use crate::conf::accounts::MailboxStatus;
                match context.accounts[&self.coordinates.0].load2(self.coordinates.1) {
                    Ok(Some(job_id)) => {
                        self.state.job_state = JobState::Loading { job_id };
                        return;
                    }
                    Ok(None) => match self.state.job_state {
                        JobState::Offline { .. } | JobState::Loading { .. } => {
                            self.state.job_state = JobState::Loaded;
                            self.initialize_rows(context);
                            return;
                        }
                        _ => {}
                    },
                    Err(err) => {
                        self.state.job_state = JobState::Offline {
                            message: err.to_string(),
                        };
                        return;
                    }
                }
            }
            match (&self.state.job_state, &event) {
                (_, UIEvent::StatusEvent(StatusEvent::JobFinished(ref finished_job_id)))
                    if self
                        .state
                        .fetching_envelopes
                        .keys()
                        .any(|j| *j == *finished_job_id) =>
                {
                    if let Some(batch) = self.state.fetching_envelopes.remove(finished_job_id) {
                        debug!("fetching_envelopes done {:?}", &batch);
                        for env_hash in batch.iter() {
                            let _row = self.unloaded_envelope_hashes.remove(&env_hash);
                        }
                        self.force_draw = true;
                        self.set_dirty(true);
                    }
                }
                (
                    JobState::Filtering {
                        ref join_handle,
                        query: _,
                    },
                    UIEvent::StatusEvent(StatusEvent::JobFinished(ref finished_job_id)),
                ) if join_handle.job_id == *finished_job_id => {}
                (
                    JobState::Searching {
                        ref join_handle,
                        query: _,
                    },
                    UIEvent::StatusEvent(StatusEvent::JobFinished(ref finished_job_id)),
                ) if join_handle.job_id == *finished_job_id => {}
                _ => {}
            }
        }

        fn initialize_columns(&mut self) {
            self.rows_drawn = SegmentTree::from(
                std::iter::repeat(1)
                    .take(self.length)
                    .collect::<SmallVec<_>>(),
            );
            match self.state.style {
                IndexStyle::Plain | IndexStyle::Threaded | IndexStyle::Compact => {
                    self.data_columns.columns[0].resize(8, self.length, None);
                    self.data_columns.columns[0].clear(None);
                    self.data_columns.segment_tree[0] = SegmentTree::from(
                        std::iter::repeat(1)
                            .take(self.length)
                            .collect::<SmallVec<_>>(),
                    );
                    self.data_columns.columns[1].resize(32, self.length, None);
                    self.data_columns.columns[1].clear(None);
                    self.data_columns.segment_tree[1] = SegmentTree::from(
                        std::iter::repeat(0)
                            .take(self.length)
                            .collect::<SmallVec<_>>(),
                    );
                    self.data_columns.columns[2].resize(64, self.length, None);
                    self.data_columns.columns[2].clear(None);
                    self.data_columns.segment_tree[2] = SegmentTree::from(
                        std::iter::repeat(0)
                            .take(self.length)
                            .collect::<SmallVec<_>>(),
                    );
                    self.data_columns.columns[3].resize(16, self.length, None);
                    self.data_columns.columns[3].clear(None);
                    self.data_columns.segment_tree[3] = SegmentTree::from(
                        std::iter::repeat(0)
                            .take(self.length)
                            .collect::<SmallVec<_>>(),
                    );
                    self.data_columns.columns[4].resize(128, self.length, None);
                    self.data_columns.columns[4].clear(None);
                    self.data_columns.segment_tree[4] = SegmentTree::from(
                        std::iter::repeat(LOADING_STR.len().try_into().unwrap_or(0))
                            .take(self.length)
                            .collect::<SmallVec<_>>(),
                    );
                }
                IndexStyle::Conversations => {
                    self.data_columns.columns[0].resize(516, 2 * self.length, None);
                    self.data_columns.columns[0].clear(None);
                }
            }
            self.force_draw = true;
            self.set_dirty(true);
        }

        fn initialize_rows(&mut self, context: &mut Context) {
            let length: usize = match self.state.style {
                IndexStyle::Plain | IndexStyle::Threaded => todo!(),
                IndexStyle::Conversations | IndexStyle::Compact => context.accounts
                    [&self.coordinates.0]
                    .collection
                    .get_threads(self.coordinates.1)
                    .roots()
                    .len(),
            };
            self.length = length;

            self.rows.clear();
            self.entries.clear();
            self.unloaded_envelope_hashes.clear();
            self.initialize_columns();

            match self.state.style {
                IndexStyle::Plain | IndexStyle::Threaded => todo!(),
                IndexStyle::Conversations | IndexStyle::Compact => {
                    let threads = context.accounts[&self.coordinates.0]
                        .collection
                        .get_threads(self.coordinates.1);
                    let mut roots = threads.roots();
                    let envelopes = &context.accounts[&self.coordinates.0].collection.envelopes;
                    threads.group_inner_sort_by(&mut roots, self.state.sort, &envelopes);
                    self.rows.extend(
                        roots.into_iter().map(|thread_hash| {
                            (thread_hash, threads.thread_ref(thread_hash).root())
                        }),
                    );
                }
            }
            self.draw_rows(
                context,
                self.state.cursor_pos % self.last_recorded_page_size,
                std::cmp::min(
                    self.state.cursor_pos % self.last_recorded_page_size
                        + self.last_recorded_page_size,
                    self.length.saturating_sub(1),
                ),
            );
        }

        fn make_entry_string(
            coordinates: (AccountHash, MailboxHash),
            e: &Envelope,
            from: &[Address],
            thread_hash: ThreadHash,
            threads: &Threads,
            context: &Context,
        ) -> EntryStrings {
            let thread = threads.thread_ref(thread_hash);
            let mut tags = String::new();
            let mut colors = SmallVec::new();
            let account = &context.accounts[&coordinates.0];
            if account.backend_capabilities.supports_tags {
                let tags_lck = account.collection.tag_index.read().unwrap();
                for t in e.labels().iter() {
                    if mailbox_settings!(context[coordinates.0][&coordinates.1].tags.ignore_tags)
                        .contains(t)
                        || account_settings!(context[coordinates.0].tags.ignore_tags).contains(t)
                        || context.settings.tags.ignore_tags.contains(t)
                        || !tags_lck.contains_key(t)
                    {
                        continue;
                    }
                    tags.push(' ');
                    tags.push_str(tags_lck.get(t).as_ref().unwrap());
                    tags.push(' ');
                    colors.push(
                        mailbox_settings!(context[coordinates.0][&coordinates.1].tags.colors)
                            .get(t)
                            .cloned()
                            .or_else(|| {
                                account_settings!(context[coordinates.0].tags.colors)
                                    .get(t)
                                    .cloned()
                                    .or_else(|| context.settings.tags.colors.get(t).cloned())
                            }),
                    );
                }
                if !tags.is_empty() {
                    tags.pop();
                }
            }
            let mut subject = e.subject().to_string();
            subject.truncate_at_boundary(150);
            if thread.len() > 1 {
                subject = format!("{} ({})", subject, thread.len())
            };
            EntryStrings {
                date: DateString(format_date(context, thread.date())),
                subject: SubjectString(subject),
                flag: FlagString(format!(
                    "{}{}",
                    if thread.has_attachments() { "📎" } else { "" },
                    if thread.snoozed() { "💤" } else { "" }
                )),
                from: FromString(address_list!((from) as comma_sep_list)),
                tags: TagString(tags, colors),
            }
        }

        fn make_entry(
            coordinates: (AccountHash, MailboxHash),
            index: usize,
            thread_hash: ThreadHash,
            thread_node_hash: ThreadNodeHash,
            root_envelope_hash: EnvelopeHash,
            from_address_list: &mut Vec<Address>,
            from_address_set: &mut HashSet<Vec<u8>>,
            threads: &Threads,
            context: &Context,
        ) -> std::result::Result<Entry, (Entry, SmallVec<[EnvelopeHash; 16]>)> {
            let thread = threads.thread_ref(thread_hash);
            let unseen = thread.unseen() > 0;
            let has_attachments = thread.has_attachments();
            let snoozed = thread.snoozed();
            let mut envelopes_to_load = SmallVec::new();
            if !context.accounts[&coordinates.0].contains_key(root_envelope_hash) {
                envelopes_to_load.push(root_envelope_hash);
            }
            from_address_list.clear();
            from_address_set.clear();
            for env_hash in threads
                .thread_group_iter(thread_hash)
                .filter_map(|(_, h)| threads.thread_nodes()[&h].message())
            {
                if !context.accounts[&coordinates.0].contains_key(env_hash) {
                    envelopes_to_load.push(env_hash);
                    continue;
                }
                let envelope = context.accounts[&coordinates.0]
                    .collection
                    .get_env(env_hash);
                for addr in envelope.from().iter() {
                    if from_address_set.contains(addr.address_spec_raw()) {
                        continue;
                    }
                    from_address_set.insert(addr.address_spec_raw().to_vec());
                    from_address_list.push(addr.clone());
                }
            }
            if !envelopes_to_load.is_empty() {
                return Err((
                    Entry {
                        index,
                        root_envelope_hash,
                        thread_node_hash,
                        thread_hash,
                        unseen,
                        snoozed,
                        has_attachments,
                        strings: None,
                    },
                    envelopes_to_load,
                ));
            }
            let root_envelope: &EnvelopeRef = &context.accounts[&coordinates.0]
                .collection
                .get_env(root_envelope_hash);

            let strings = Some(Self::make_entry_string(
                coordinates,
                &root_envelope,
                &from_address_list,
                thread_hash,
                &threads,
                context,
            ));
            Ok(Entry {
                index,
                root_envelope_hash,
                thread_node_hash,
                thread_hash,
                unseen,
                snoozed,
                has_attachments,
                strings,
            })
        }

        fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
            debug!("draw_list {:?}", &self.state.job_state);
            debug!(self.length);
            debug!(self.force_draw);
            debug!(self.state.style);
            let (upper_left, bottom_right) = area;
            if self.length == 0 {
                clear_area(grid, area, self.color_cache.theme_default);
                copy_area(
                    grid,
                    &self.data_columns.columns[0],
                    area,
                    ((0, 0), pos_dec(self.data_columns.columns[0].size(), (1, 1))),
                );
                context.dirty_areas.push_back(area);
                return;
            }
            let rows: usize = match self.state.style {
                IndexStyle::Plain | IndexStyle::Threaded | IndexStyle::Compact => {
                    get_y(bottom_right) - get_y(upper_left) + 1
                }
                IndexStyle::Conversations => (get_y(bottom_right) - get_y(upper_left) + 1) / 2,
            };
            self.last_recorded_page_size = rows;
            if rows == 0 {
                return;
            }

            if let Some(mvm) = self.state.movement.take() {
                match mvm {
                    PageMovement::Up(amount) => {
                        self.state.new_cursor_pos =
                            self.state.new_cursor_pos.saturating_sub(amount);
                    }
                    PageMovement::PageUp(multiplier) => {
                        self.state.new_cursor_pos =
                            self.state.new_cursor_pos.saturating_sub(rows * multiplier);
                    }
                    PageMovement::Down(amount) => {
                        if self.state.new_cursor_pos + amount + 1 < self.length {
                            self.state.new_cursor_pos += amount;
                        } else {
                            self.state.new_cursor_pos = self.length - 1;
                        }
                    }
                    PageMovement::PageDown(multiplier) => {
                        if self.state.new_cursor_pos + rows * multiplier + 1 < self.length {
                            self.state.new_cursor_pos += rows * multiplier;
                        } else if self.state.new_cursor_pos + rows * multiplier > self.length {
                            self.state.new_cursor_pos = self.length - 1;
                        } else {
                            self.state.new_cursor_pos =
                                (self.length.saturating_sub(1) / rows) * rows;
                        }
                    }
                    PageMovement::Right(_) | PageMovement::Left(_) => {}
                    PageMovement::Home => {
                        self.state.new_cursor_pos = 0;
                    }
                    PageMovement::End => {
                        self.state.new_cursor_pos = self.length.saturating_sub(1);
                    }
                }
            }

            let prev_page_no = (self.state.cursor_pos).wrapping_div(rows);
            let page_no = (self.state.new_cursor_pos).wrapping_div(rows);

            let top_idx = page_no * rows;
            self.draw_rows(
                context,
                top_idx,
                std::cmp::min(top_idx + rows, self.length.saturating_sub(1)),
            );

            /* If cursor position has changed, remove the highlight from the previous position and
             * apply it in the new one. */
            if self.state.cursor_pos != self.state.new_cursor_pos
                && prev_page_no == page_no
                && !self.force_draw
            {
                let old_cursor_pos = self.state.cursor_pos;
                self.state.cursor_pos = self.state.new_cursor_pos;
                for &idx in &[old_cursor_pos, self.state.new_cursor_pos] {
                    if idx >= self.length {
                        continue;
                    }
                    let new_area = match self.state.style {
                        IndexStyle::Plain | IndexStyle::Threaded | IndexStyle::Compact => (
                            set_y(upper_left, get_y(upper_left) + (idx % rows)),
                            set_y(bottom_right, get_y(upper_left) + (idx % rows)),
                        ),
                        IndexStyle::Conversations => (
                            set_y(upper_left, get_y(upper_left) + 2 * (idx % rows)),
                            set_y(bottom_right, get_y(upper_left) + 2 * (idx % rows) + 1),
                        ),
                    };
                    self.highlight_line(grid, new_area, idx, context);
                    context.dirty_areas.push_back(new_area);
                }
                return;
            } else if self.state.cursor_pos != self.state.new_cursor_pos {
                self.state.cursor_pos = self.state.new_cursor_pos;
            } else if self.state.cursor_pos == self.state.new_cursor_pos
                && prev_page_no == page_no
                && !self.force_draw
            {
                return;
            }
            if self.state.new_cursor_pos >= self.length {
                self.state.new_cursor_pos = self.length - 1;
                self.state.cursor_pos = self.state.new_cursor_pos;
            }
            self.force_draw = false;

            clear_area(grid, area, self.color_cache.theme_default);
            /* Page_no has changed, so draw new page */
            match self.state.style {
                IndexStyle::Conversations => {
                    copy_area(
                        grid,
                        &self.data_columns.columns[0],
                        (
                            upper_left,
                            set_x(
                                bottom_right,
                                std::cmp::min(
                                    get_x(bottom_right),
                                    get_x(upper_left) + self.data_columns.columns[0].size().0,
                                ),
                            ),
                        ),
                        (
                            (0, 2 * top_idx),
                            pos_dec(self.data_columns.columns[0].size(), (1, 1)),
                        ),
                    );

                    self.highlight_line(
                        grid,
                        (
                            pos_inc(upper_left, (0, 2 * (self.state.cursor_pos % rows))),
                            set_y(
                                bottom_right,
                                get_y(upper_left) + 2 * (self.state.cursor_pos % rows) + 1,
                            ),
                        ),
                        self.state.cursor_pos,
                        context,
                    );

                    /* calculate how many entries are visible in this page */
                    let rows = if top_idx + rows > self.length {
                        clear_area(
                            grid,
                            (
                                pos_inc(upper_left, (0, 2 * (self.length - top_idx))),
                                bottom_right,
                            ),
                            self.color_cache.theme_default,
                        );
                        self.length - top_idx
                    } else {
                        rows
                    };

                    /* fill any remaining columns, if our view is wider than
                     * self.data_columns.columns[0] */
                    let width = self.data_columns.columns[0].size().0;
                    let padding_fg = self.color_cache.padding.fg;

                    if width < width!(area) {
                        let y_offset = get_y(upper_left);
                        for y in 0..rows {
                            let bg_color =
                                grid[(get_x(upper_left) + width - 1, y_offset + 2 * y)].bg();
                            for x in (get_x(upper_left) + width)..=get_x(bottom_right) {
                                grid[(x, y_offset + 2 * y)].set_bg(bg_color);
                                grid[(x, y_offset + 2 * y + 1)]
                                    .set_ch('▁')
                                    .set_fg(self.color_cache.theme_default.fg)
                                    .set_bg(bg_color);
                            }
                        }
                    }
                }
                other => {
                    let width = width!(area);

                    for &i in &[0, 1, 2, 3, 4] {
                        /* Set column widths to their maximum value width in the
                         * range [top_idx, top_idx + rows]. By using a segment
                         * tree the query is O(logn), which is great!
                         */
                        self.data_columns.widths[i] = self.data_columns.segment_tree[i]
                            .get_max(top_idx, top_idx + rows)
                            as usize;
                    }
                    if self.data_columns.widths.iter().sum::<usize>() > width {
                        let diff = self.data_columns.widths.iter().sum::<usize>() - width;
                        if self.data_columns.widths[2] > 2 * diff {
                            self.data_columns.widths[2] -= diff;
                        } else {
                            self.data_columns.widths[2] = std::cmp::min(
                                self.data_columns.widths[2],
                                std::cmp::max(
                                    15,
                                    self.data_columns.widths[2].saturating_sub((2 * diff) / 3),
                                ),
                            );
                            self.data_columns.widths[4] = std::cmp::min(
                                self.data_columns.widths[4],
                                std::cmp::max(
                                    15,
                                    self.data_columns.widths[4].saturating_sub(diff / 3 + diff % 3),
                                ),
                            );
                        }
                    }
                    let mut x = get_x(upper_left);
                    let mut flag_x = 0;
                    for i in 0..5 {
                        let column_width = self.data_columns.columns[i].size().0;
                        if i == 3 {
                            flag_x = x;
                        }
                        if self.data_columns.widths[i] == 0 {
                            continue;
                        }
                        copy_area(
                            grid,
                            &self.data_columns.columns[i],
                            (
                                set_x(upper_left, x),
                                set_x(
                                    bottom_right,
                                    std::cmp::min(
                                        get_x(bottom_right),
                                        x + (self.data_columns.widths[i]),
                                    ),
                                ),
                            ),
                            (
                                (0, top_idx),
                                (column_width.saturating_sub(1), self.length - 1),
                            ),
                        );
                        for (r, row) in grid
                            .bounds_iter((
                                set_x(
                                    upper_left,
                                    std::cmp::min(
                                        get_x(bottom_right),
                                        x + (self.data_columns.widths[i]),
                                    ),
                                ),
                                (set_x(
                                    bottom_right,
                                    std::cmp::min(
                                        get_x(bottom_right),
                                        x + (self.data_columns.widths[i]) + 2,
                                    ),
                                )),
                            ))
                            .enumerate()
                        {
                            let bg_color = grid[pos_inc(upper_left, (0, r))].bg();
                            for c in row {
                                grid[c].set_bg(bg_color);
                            }
                        }
                        x += self.data_columns.widths[i] + 2; // + SEPARATOR
                        if x > get_x(bottom_right) {
                            break;
                        }
                    }

                    self.highlight_line(
                        grid,
                        (
                            set_y(
                                upper_left,
                                get_y(upper_left) + (self.state.cursor_pos % rows),
                            ),
                            set_y(
                                bottom_right,
                                get_y(upper_left) + (self.state.cursor_pos % rows),
                            ),
                        ),
                        self.state.cursor_pos,
                        context,
                    );

                    if top_idx + rows > self.length {
                        clear_area(
                            grid,
                            (
                                pos_inc(upper_left, (0, self.length - top_idx)),
                                bottom_right,
                            ),
                            self.color_cache.theme_default,
                        );
                    }
                }
            }

            context.dirty_areas.push_back(area);
        }

        fn draw_rows(&mut self, context: &mut Context, start: usize, end: usize) {
            if self.length == 0 {
                return;
            }
            debug_assert!(end >= start);
            if self.rows_drawn.get_max(start, end) == 0 {
                debug!("not drawing {}-{}", start, end);
                return;
            }
            debug!("drawing {}-{}", start, end);
            let mut envelopes_to_load: Vec<EnvelopeHash> = vec![];
            match self.state.style {
                IndexStyle::Plain | IndexStyle::Threaded => todo!(),
                IndexStyle::Conversations | IndexStyle::Compact => {
                    let threads = context.accounts[&self.coordinates.0]
                        .collection
                        .get_threads(self.coordinates.1);
                    let mut from_address_list = Vec::new();
                    let mut from_address_set: HashSet<Vec<u8>> = HashSet::new();
                    let mut stack: std::collections::VecDeque<ThreadNodeHash> =
                        std::collections::VecDeque::new();
                    for (i, (thread_hash, thread_node_hash, root_envelope_hash)) in self
                        .rows
                        .iter()
                        .enumerate()
                        .skip(start)
                        .take(end - start + 1)
                        .filter(|(i, _)| self.rows_drawn.get(*i) != 0)
                        .filter_map(|(i, &(thread_hash, thread_node_hash))| {
                            let thread_node = &threads.thread_nodes()[&thread_node_hash];
                            thread_node
                                .message()
                                .or_else(|| {
                                    stack.clear();
                                    stack.extend(thread_node.children().iter());
                                    let mut iter_ptr = thread_node_hash;
                                    while let Some(iter) = stack.pop_front() {
                                        if threads.thread_nodes()[&iter].message().is_some() {
                                            iter_ptr = iter;
                                            break;
                                        }
                                        stack.extend(
                                            threads.thread_nodes()[&iter].children().iter(),
                                        );
                                    }
                                    threads.thread_nodes()[&iter_ptr].message()
                                })
                                .map(|h| (i, (thread_hash, thread_node_hash, h)))
                        })
                        .collect::<SmallVec<[_; 1024]>>()
                    {
                        match Self::make_entry(
                            self.coordinates,
                            i,
                            thread_hash,
                            thread_node_hash,
                            root_envelope_hash,
                            &mut from_address_list,
                            &mut from_address_set,
                            &threads,
                            context,
                        ) {
                            Ok(entry) => {
                                self.entries.insert(i, entry);
                                self.force_draw = true;
                                self.rows_drawn.update(i, 0);
                            }
                            Err((entry, batch)) => {
                                self.entries.insert(i, entry);
                                for &env_hash in &batch {
                                    self.unloaded_envelope_hashes.insert(env_hash, i);
                                }
                                envelopes_to_load.extend(batch.into_iter());
                            }
                        }
                    }
                }
            };
            if let Ok(batch) = EnvelopeHashBatch::try_from(envelopes_to_load.as_slice()) {
                match context.accounts[&self.coordinates.0].fetch_batch(batch.clone()) {
                    Err(err) => {}
                    Ok(job_id) => {
                        self.state.fetching_envelopes.insert(job_id, batch);
                    }
                }
            }

            match self.state.style {
                IndexStyle::Conversations => {
                    let (width, _) = self.data_columns.columns[0].size();
                    for i in start..=end {
                        if let Some(Entry {
                            strings: Some(ref strings),
                            unseen,
                            ..
                        }) = self.entries.get(&i)
                        {
                            conversations::draw_entry(
                                &mut self.data_columns.columns[0],
                                strings,
                                ((0, 2 * i), (width - 1, 2 * i + 1)),
                                &self.color_cache,
                                *unseen,
                                false,
                                false,
                            );
                        } else {
                            write_string_to_grid(
                                LOADING_STR,
                                &mut self.data_columns.columns[0],
                                self.color_cache.theme_default.fg,
                                self.color_cache.theme_default.bg,
                                self.color_cache.theme_default.attrs,
                                ((0, 2 * i), (width - 1, 2 * i)),
                                None,
                            );
                            for x in 0..width {
                                self.data_columns.columns[0][(x, 2 * i + 1)]
                                    .set_ch('▁')
                                    .set_fg(self.color_cache.theme_default.fg)
                                    .set_bg(self.color_cache.theme_default.bg);
                            }
                        }
                    }
                }
                IndexStyle::Plain | IndexStyle::Threaded | IndexStyle::Compact => {
                    let Self {
                        ref entries,
                        ref mut data_columns,
                        ref color_cache,
                        ..
                    } = self;
                    for (i, strings) in (start..=end).into_iter().filter_map(|i| {
                        entries
                            .get(&i)
                            .and_then(|entry| entry.strings.as_ref().map(|s| (i, s)))
                    }) {
                        data_columns.segment_tree[0]
                            .update(i, i.to_string().grapheme_width().try_into().unwrap_or(255));
                        /* date */
                        data_columns.segment_tree[1]
                            .update(i, strings.date.grapheme_width().try_into().unwrap_or(255));
                        /* from */
                        data_columns.segment_tree[2]
                            .update(i, strings.from.grapheme_width().try_into().unwrap_or(255));
                        /* flags */
                        data_columns.segment_tree[3]
                            .update(i, strings.flag.grapheme_width().try_into().unwrap_or(255));
                        /* subject */
                        data_columns.segment_tree[4].update(
                            i,
                            (strings.subject.grapheme_width() + 2 + strings.tags.grapheme_width())
                                .try_into()
                                .unwrap_or(255),
                        );
                    }
                    for i in start..=end {
                        if let Some(Entry {
                            strings: Some(ref strings),
                            unseen,
                            snoozed,
                            has_attachments,
                            ..
                        }) = entries.get(&i)
                        {
                            single_row::draw_entry(
                                &mut data_columns.columns,
                                i,
                                strings,
                                *snoozed,
                                *has_attachments,
                                *unseen,
                                false,
                                false,
                                &color_cache,
                            );
                        } else {
                            single_row::draw_loading_entry(
                                &mut data_columns.columns,
                                i,
                                false,
                                &color_cache,
                            );
                        }
                    }
                }
            }
        }

        fn highlight_line(
            &mut self,
            grid: &mut CellBuffer,
            area: Area,
            idx: usize,
            context: &Context,
        ) {
            if self.length == 0 {
                return;
            }
            match self.state.style {
                IndexStyle::Plain | IndexStyle::Threaded | IndexStyle::Compact => {
                    self.highlight_single_line(grid, area, idx, context);
                }
                IndexStyle::Conversations => {
                    self.highlight_double_line(grid, area, idx, context);
                }
            }
        }

        fn get_thread_under_cursor(&self, cursor: usize) -> Option<ThreadHash> {
            self.rows.get(cursor).map(|&(t, _)| t)
        }
    }

    impl fmt::Display for MailboxList {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "mail")
        }
    }

    impl Component for MailboxList {
        fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
            if !self.is_dirty() {
                return;
            }
            if !is_valid_area!(area) {
                return;
            }
            let (upper_left, bottom_right) = area;
            {
                let mut area = if self.unfocused {
                    clear_area(
                        grid,
                        (
                            pos_inc(upper_left, (width!(area) / 3, 0)),
                            set_x(bottom_right, get_x(upper_left) + width!(area) / 3 + 1),
                        ),
                        self.color_cache.theme_default,
                    );
                    context.dirty_areas.push_back((
                        pos_inc(upper_left, (width!(area) / 3, 0)),
                        set_x(bottom_right, get_x(upper_left) + width!(area) / 3 + 1),
                    ));
                    (
                        upper_left,
                        set_x(bottom_right, get_x(upper_left) + width!(area) / 3 - 1),
                    )
                } else {
                    area
                };

                /*
                if !self.state.filter_term.is_empty() {
                    let (x, y) = write_string_to_grid(
                        &format!(
                            "{} results for `{}` (Press ESC to exit)",
                            self.filtered_selection.len(),
                            self.filter_term
                        ),
                        grid,
                        self.color_cache.theme_default.fg,
                        self.color_cache.theme_default.bg,
                        self.color_cache.theme_default.attrs,
                        area,
                        Some(get_x(upper_left)),
                    );
                    for c in grid.row_iter(x..(get_x(bottom_right) + 1), y) {
                        grid[c] = Cell::default();
                    }
                    clear_area(
                        grid,
                        ((x, y), set_y(bottom_right, y)),
                        self.color_cache.theme_default,
                    );
                    context
                        .dirty_areas
                        .push_back((upper_left, set_y(bottom_right, y + 1)));

                    area = (set_y(upper_left, y + 1), bottom_right);
                }
                */
                let (upper_left, bottom_right) = area;
                let rows: usize = match self.state.style {
                    IndexStyle::Plain | IndexStyle::Threaded | IndexStyle::Compact => {
                        get_y(bottom_right) - get_y(upper_left)
                    }
                    IndexStyle::Conversations => (get_y(bottom_right) - get_y(upper_left) + 1) / 2,
                };
                if let Some(modifier) = self.state.modifier_command.take() {
                    if let Some(mvm) = self.state.movement.as_ref() {
                        /*
                            match mvm {
                                PageMovement::Up(amount) => {
                                    for c in self.new_cursor_pos.saturating_sub(*amount)
                                        ..=self.new_cursor_pos
                                    {
                                        let thread = self.get_thread_under_cursor(c);
                                        match modifier {
                                            Modifier::SymmetricDifference => {
                                                self.selection.entry(thread).and_modify(|e| *e = !*e);
                                            }
                                            Modifier::Union => {
                                                self.selection.entry(thread).and_modify(|e| *e = true);
                                            }
                                            Modifier::Difference => {
                                                self.selection.entry(thread).and_modify(|e| *e = false);
                                            }
                                            Modifier::Intersection => {}
                                        }
                                        self.row_updates.push(thread);
                                    }
                                    if modifier == Modifier::Intersection {
                                        for c in (0..self.new_cursor_pos.saturating_sub(*amount))
                                            .chain((self.new_cursor_pos + 2)..self.length)
                                        {
                                            let thread = self.get_thread_under_cursor(c);
                                            self.selection.entry(thread).and_modify(|e| *e = false);
                                            self.row_updates.push(thread);
                                        }
                                    }
                                }
                                PageMovement::PageUp(multiplier) => {
                                    for c in self.new_cursor_pos.saturating_sub(rows * multiplier)
                                        ..=self.new_cursor_pos
                                    {
                                        let thread = self.get_thread_under_cursor(c);
                                        match modifier {
                                            Modifier::SymmetricDifference => {
                                                self.selection.entry(thread).and_modify(|e| *e = !*e);
                                            }
                                            Modifier::Union => {
                                                self.selection.entry(thread).and_modify(|e| *e = true);
                                            }
                                            Modifier::Difference => {
                                                self.selection.entry(thread).and_modify(|e| *e = false);
                                            }
                                            Modifier::Intersection => {}
                                        }
                                        self.row_updates.push(thread);
                                    }
                                }
                                PageMovement::Down(amount) => {
                                    for c in self.new_cursor_pos
                                        ..std::cmp::min(self.length, self.new_cursor_pos + amount + 1)
                                    {
                                        let thread = self.get_thread_under_cursor(c);
                                        match modifier {
                                            Modifier::SymmetricDifference => {
                                                self.selection.entry(thread).and_modify(|e| *e = !*e);
                                            }
                                            Modifier::Union => {
                                                self.selection.entry(thread).and_modify(|e| *e = true);
                                            }
                                            Modifier::Difference => {
                                                self.selection.entry(thread).and_modify(|e| *e = false);
                                            }
                                            Modifier::Intersection => {}
                                        }
                                        self.row_updates.push(thread);
                                    }
                                    if modifier == Modifier::Intersection {
                                        for c in (0..self.new_cursor_pos).chain(
                                            (std::cmp::min(
                                                self.length,
                                                self.new_cursor_pos + amount + 1,
                                            ) + 1)..self.length,
                                        ) {
                                            let thread = self.get_thread_under_cursor(c);
                                            self.selection.entry(thread).and_modify(|e| *e = false);
                                            self.row_updates.push(thread);
                                        }
                                    }
                                }
                                PageMovement::PageDown(multiplier) => {
                                    for c in self.new_cursor_pos
                                        ..std::cmp::min(
                                            self.new_cursor_pos + rows * multiplier + 1,
                                            self.length,
                                        )
                                    {
                                        let thread = self.get_thread_under_cursor(c);
                                        match modifier {
                                            Modifier::SymmetricDifference => {
                                                self.selection.entry(thread).and_modify(|e| *e = !*e);
                                            }
                                            Modifier::Union => {
                                                self.selection.entry(thread).and_modify(|e| *e = true);
                                            }
                                            Modifier::Difference => {
                                                self.selection.entry(thread).and_modify(|e| *e = false);
                                            }
                                            Modifier::Intersection => {}
                                        }
                                        self.row_updates.push(thread);
                                    }
                                    if modifier == Modifier::Intersection {
                                        for c in (0..self.new_cursor_pos).chain(
                                            (std::cmp::min(
                                                self.new_cursor_pos + rows * multiplier + 1,
                                                self.length,
                                            ) + 1)..self.length,
                                        ) {
                                            let thread = self.get_thread_under_cursor(c);
                                            self.selection.entry(thread).and_modify(|e| *e = false);
                                            self.row_updates.push(thread);
                                        }
                                    }
                                }
                                PageMovement::Right(_) | PageMovement::Left(_) => {}
                                PageMovement::Home => {
                                    for c in 0..=self.new_cursor_pos {
                                        let thread = self.get_thread_under_cursor(c);
                                        match modifier {
                                            Modifier::SymmetricDifference => {
                                                self.selection.entry(thread).and_modify(|e| *e = !*e);
                                            }
                                            Modifier::Union => {
                                                self.selection.entry(thread).and_modify(|e| *e = true);
                                            }
                                            Modifier::Difference => {
                                                self.selection.entry(thread).and_modify(|e| *e = false);
                                            }
                                            Modifier::Intersection => {}
                                        }
                                        self.row_updates.push(thread);
                                    }
                                    if modifier == Modifier::Intersection {
                                        for c in (self.new_cursor_pos + 1)..self.length {
                                            let thread = self.get_thread_under_cursor(c);
                                            self.selection.entry(thread).and_modify(|e| *e = false);
                                            self.row_updates.push(thread);
                                        }
                                    }
                                }
                                PageMovement::End => {
                                    for c in self.new_cursor_pos..self.length {
                                        let thread = self.get_thread_under_cursor(c);
                                        match modifier {
                                            Modifier::SymmetricDifference => {
                                                self.selection.entry(thread).and_modify(|e| *e = !*e);
                                            }
                                            Modifier::Union => {
                                                self.selection.entry(thread).and_modify(|e| *e = true);
                                            }
                                            Modifier::Difference => {
                                                self.selection.entry(thread).and_modify(|e| *e = false);
                                            }
                                            Modifier::Intersection => {}
                                        }
                                        self.row_updates.push(thread);
                                    }
                                    if modifier == Modifier::Intersection {
                                        for c in 0..self.new_cursor_pos {
                                            let thread = self.get_thread_under_cursor(c);
                                            self.selection.entry(thread).and_modify(|e| *e = false);
                                            self.row_updates.push(thread);
                                        }
                                    }
                                }
                            }
                        */
                    }
                }
                /*
                        if !self.row_updates.is_empty() {
                            /* certain rows need to be updated (eg an unseen message was just set seen)
                             * */
                            while let Some(row) = self.row_updates.pop() {
                                self.update_line(context, row);
                                let row: usize = self.order[&row];

                                let page_no = (self.cursor_pos).wrapping_div(rows);

                                let top_idx = page_no * rows;
                                /* Update row only if it's currently visible */
                                if row >= top_idx && row < top_idx + rows {
                                    let area = (
                                        set_y(upper_left, get_y(upper_left) + (3 * (row % rows))),
                                        set_y(bottom_right, get_y(upper_left) + (3 * (row % rows) + 2)),
                                    );
                                    self.highlight_line(grid, area, row, context);
                                    context.dirty_areas.push_back(area);
                                }
                            }
                            if self.force_draw {
                                /* Draw the entire list */
                                self.draw_list(grid, area, context);
                                self.force_draw = false;
                            }
                        } else {
                            /* Draw the entire list */
                            self.draw_list(grid, area, context);
                        }
                */
                /* Draw the entire list */
                self.draw_list(grid, area, context);
            }
            if self.unfocused {
                if self.length == 0 && self.dirty {
                    clear_area(grid, area, self.color_cache.theme_default);
                    context.dirty_areas.push_back(area);
                    return;
                }

                let area = (
                    set_x(upper_left, get_x(upper_left) + width!(area) / 3 + 2),
                    bottom_right,
                );
                self.view.draw(grid, area, context);
            }
            self.dirty = false;
        }

        fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
            debug!(
                "process_event: {:?} state: {:?}",
                event, &self.state.job_state
            );
            match (&self.state.job_state, &event) {
                (JobState::Offline { .. }, UIEvent::AccountStatusChange(account_hash))
                    if self.coordinates.0 == *account_hash =>
                {
                    self.update_job_state(event, context);
                    self.set_dirty(true);
                }
                (
                    JobState::Loading { ref job_id },
                    UIEvent::StatusEvent(StatusEvent::JobCanceled(ref canceled_job_id)),
                ) if *job_id == *canceled_job_id => {
                    self.update_job_state(event, context);
                    self.set_dirty(true);
                    return false;
                }
                (
                    JobState::Loading { ref job_id },
                    UIEvent::StatusEvent(StatusEvent::JobFinished(ref finished_job_id)),
                ) if *job_id == *finished_job_id => {
                    self.update_job_state(event, context);
                    self.set_dirty(true);
                    return false;
                }
                (_, UIEvent::StatusEvent(StatusEvent::JobFinished(ref finished_job_id)))
                    if self
                        .state
                        .fetching_envelopes
                        .keys()
                        .any(|j| *j == *finished_job_id) =>
                {
                    self.update_job_state(event, context);
                    self.set_dirty(true);
                    return false;
                }
                (
                    JobState::Filtering {
                        ref join_handle,
                        query: _,
                    },
                    UIEvent::StatusEvent(StatusEvent::JobFinished(ref finished_job_id)),
                ) if join_handle.job_id == *finished_job_id => {
                    self.update_job_state(event, context);
                    self.set_dirty(true);
                    return false;
                }
                (
                    JobState::Searching {
                        ref join_handle,
                        query: _,
                    },
                    UIEvent::StatusEvent(StatusEvent::JobFinished(ref finished_job_id)),
                ) if join_handle.job_id == *finished_job_id => {
                    self.update_job_state(event, context);
                    self.set_dirty(true);
                    return false;
                }
                (_, _) => {}
            }
            if self.unfocused && self.view.process_event(event, context) {
                return true;
            }

            let shortcuts = self.get_shortcuts(context);
            if self.length > 0 {
                match *event {
                    UIEvent::Input(ref k)
                        if !self.unfocused
                            && shortcut!(
                                k == shortcuts[CompactListing::DESCRIPTION]["open_thread"]
                            ) =>
                    {
                        if let Some(thread_hash) =
                            self.get_thread_under_cursor(self.state.cursor_pos)
                        {
                            self.view = ThreadView::new(
                                (
                                    self.coordinates.0,
                                    self.coordinates.1,
                                    self.state.cursor_pos,
                                ),
                                thread_hash,
                                None,
                                context,
                            );
                            self.unfocused = true;
                            self.dirty = true;
                        }
                        return true;
                    }
                    UIEvent::Input(ref k)
                        if self.unfocused
                            && shortcut!(
                                k == shortcuts[CompactListing::DESCRIPTION]["exit_thread"]
                            ) =>
                    {
                        self.unfocused = false;
                        self.dirty = true;
                        /* If self.row_updates is not empty and we exit a thread, the row_update events
                         * will be performed but the list will not be drawn. So force a draw in any case.
                         * */
                        self.force_draw = true;
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if !self.unfocused
                            && shortcut!(
                                key == shortcuts[Listing::DESCRIPTION]["select_entry"]
                            ) =>
                    {
                        if self.state.modifier_active && self.state.modifier_command.is_none() {
                            self.state.modifier_command = Some(Modifier::default());
                        } else {
                            let thread_hash = self.get_thread_under_cursor(self.state.cursor_pos);
                            //self.selection.entry(thread_hash).and_modify(|e| *e = !*e);
                            //self.row_updates.push(thread_hash);
                        }
                        return true;
                    }
                    UIEvent::Action(ref action) => {
                        match action {
                            Action::Sort(field, order) if !self.unfocused => {
                                debug!("Sort {:?} , {:?}", field, order);
                                self.state.sort = (*field, *order);
                                //if !self.filtered_selection.is_empty() {
                                //    // FIXME: perform sort
                                //    self.dirty = true;
                                //} else {
                                //    self.refresh_mailbox(context, false);
                                //}
                                return true;
                            }
                            Action::SubSort(field, order) if !self.unfocused => {
                                debug!("SubSort {:?} , {:?}", field, order);
                                self.state.subsort = (*field, *order);
                                // FIXME: perform subsort.
                                return true;
                            }
                            Action::Listing(ToggleThreadSnooze) if !self.unfocused => {
                                /*
                                let thread = self.get_thread_under_cursor(self.state.cursor_pos);
                                let account = &mut context.accounts[&self.coordinates.0];
                                account
                                    .collection
                                    .threads
                                    .write()
                                    .unwrap()
                                    .entry(self.coordinates.1)
                                    .and_modify(|threads| {
                                        let is_snoozed = threads.thread_ref(thread).snoozed();
                                        threads.thread_ref_mut(thread).set_snoozed(!is_snoozed);
                                    });
                                self.row_updates.push(thread);
                                self.refresh_mailbox(context, false);
                                */
                                return true;
                            }

                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            match *event {
                UIEvent::ConfigReload { old_settings: _ } => {
                    self.set_dirty(true);
                }
                UIEvent::EnvelopeRename(ref old_hash, ref new_hash) => {
                    /*
                    let account = &context.accounts[&self.coordinates.0];
                    let threads = account.collection.get_threads(self.coordinates.1);
                    if !account.collection.contains_key(&new_hash) {
                        return false;
                    }
                    let new_env_thread_node_hash = account.collection.get_env(*new_hash).thread();
                    if !threads.thread_nodes.contains_key(&new_env_thread_node_hash) {
                        return false;
                    }
                    let thread: ThreadHash =
                        threads.find_group(threads.thread_nodes()[&new_env_thread_node_hash].group);
                    drop(threads);
                    if self.order.contains_key(&thread) {
                        self.row_updates.push(thread);
                    }

                    self.dirty = true;

                    if self.unfocused {
                        self.view
                            .process_event(&mut UIEvent::EnvelopeRename(*old_hash, *new_hash), context);
                    }
                    */
                }
                UIEvent::EnvelopeRemove(ref _env_hash, ref thread_hash) => {
                    /*
                    if self.order.contains_key(thread_hash) {
                        self.refresh_mailbox(context, false);
                        self.set_dirty(true);
                    }
                    */
                }
                UIEvent::EnvelopeUpdate(ref env_hash) => {
                    /*
                    let account = &context.accounts[&self.coordinates.0];
                    let threads = account.collection.get_threads(self.coordinates.1);
                    if !account.collection.contains_key(&env_hash) {
                        return false;
                    }
                    let new_env_thread_node_hash = account.collection.get_env(*env_hash).thread();
                    if !threads.thread_nodes.contains_key(&new_env_thread_node_hash) {
                        return false;
                    }
                    let thread: ThreadHash =
                        threads.find_group(threads.thread_nodes()[&new_env_thread_node_hash].group);
                    drop(threads);
                    if self.order.contains_key(&thread) {
                        self.row_updates.push(thread);
                    }

                    self.dirty = true;

                    if self.unfocused {
                        self.view
                            .process_event(&mut UIEvent::EnvelopeUpdate(*env_hash), context);
                    }
                    */
                }
                UIEvent::ChangeMode(UIMode::Normal) => {
                    self.dirty = true;
                }
                UIEvent::Resize => {
                    self.dirty = true;
                }
                /*
                UIEvent::Input(Key::Esc)
                    if !self.unfocused
                        && self.selection.values().cloned().any(std::convert::identity) =>
                {
                    for v in self.selection.values_mut() {
                        *v = false;
                    }
                    self.dirty = true;
                    return true;
                }
                UIEvent::Input(Key::Esc) if !self.unfocused && !self.filter_term.is_empty() => {
                    /*
                    self.set_coordinates((self.new_cursor_pos.0, self.new_cursor_pos.1));
                    self.refresh_mailbox(context, false);
                    self.set_dirty(true);
                    */
                    return true;
                }
                */
                UIEvent::Action(Action::Listing(Search(ref filter_term))) if !self.unfocused => {
                    /*
                    match context.accounts[&self.coordinates.0].search(
                        filter_term,
                        self.sort,
                        self.coordinates.1,
                    ) {
                        Ok(job) => {
                            let handle = context.accounts[&self.coordinates.0]
                                .job_executor
                                .spawn_specialized(job);
                            self.search_job = Some((filter_term.to_string(), handle));
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification(
                                    Some("Could not perform search".to_string()),
                                    err.to_string(),
                                    Some(crate::types::NotificationType::Error(err.kind)),
                            ));
                        }
                    };
                    self.set_dirty(true);
                    */
                }
                UIEvent::Action(Action::Listing(Select(ref search_term))) if !self.unfocused => {
                    /*
                    match context.accounts[&self.coordinates.0].search(
                        search_term,
                        self.sort,
                        self.coordinates.1,
                    ) {
                        Ok(job) => {
                            let mut handle = context.accounts[&self.coordinates.0]
                                .job_executor
                                .spawn_specialized(job);
                            if let Ok(Some(search_result)) = try_recv_timeout!(&mut handle.chan) {
                                self.select(search_term, search_result, context);
                            } else {
                                self.select_job = Some((search_term.to_string(), handle));
                            }
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification(
                                    Some("Could not perform search".to_string()),
                                    err.to_string(),
                                    Some(crate::types::NotificationType::Error(err.kind)),
                            ));
                        }
                    };
                    self.set_dirty(true);
                    */
                    return true;
                }
                /*
                UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id))
                    if self
                        .search_job
                            .as_ref()
                            .map(|(_, j)| j == job_id)
                            .unwrap_or(false) =>
                    {
                        let (filter_term, mut handle) = self.search_job.take().unwrap();
                        match handle.chan.try_recv() {
                            Err(_) => { /* search was canceled */ }
                            Ok(None) => { /* something happened, perhaps a worker thread panicked */ }
                            Ok(Some(results)) => self.filter(filter_term, results, context),
                        }
                        self.set_dirty(true);
                    }
                UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id))
                    if self
                        .select_job
                            .as_ref()
                            .map(|(_, j)| j == job_id)
                            .unwrap_or(false) =>
                    {
                        let (search_term, mut handle) = self.select_job.take().unwrap();
                        match handle.chan.try_recv() {
                            Err(_) => { /* search was canceled */ }
                            Ok(None) => { /* something happened, perhaps a worker thread panicked */ }
                            Ok(Some(results)) => self.select(&search_term, results, context),
                        }
                        self.set_dirty(true);
                    }
                */
                _ => {}
            }
            false
        }

        fn is_dirty(&self) -> bool {
            self.dirty
        }

        fn set_dirty(&mut self, value: bool) {
            self.dirty = value;
        }

        fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
            let mut map = if self.unfocused {
                self.view.get_shortcuts(context)
            } else {
                ShortcutMaps::default()
            };

            let config_map = context.settings.shortcuts.compact_listing.key_values();
            map.insert(CompactListing::DESCRIPTION, config_map);
            let config_map = context.settings.shortcuts.listing.key_values();
            map.insert(Listing::DESCRIPTION, config_map);

            map
        }

        fn id(&self) -> ComponentId {
            self.id
        }

        fn set_id(&mut self, id: ComponentId) {
            self.id = id;
        }
    }

    fn format_date(context: &Context, epoch: UnixTimestamp) -> String {
        let d = std::time::UNIX_EPOCH + std::time::Duration::from_secs(epoch);
        let now: std::time::Duration = std::time::SystemTime::now()
            .duration_since(d)
            .unwrap_or_else(|_| std::time::Duration::new(std::u64::MAX, 0));
        match now.as_secs() {
            n if context.settings.listing.recent_dates && n < 60 * 60 => format!(
                "{} minute{} ago",
                n / (60),
                if n / 60 == 1 { "" } else { "s" }
            ),
            n if context.settings.listing.recent_dates && n < 24 * 60 * 60 => format!(
                "{} hour{} ago",
                n / (60 * 60),
                if n / (60 * 60) == 1 { "" } else { "s" }
            ),
            n if context.settings.listing.recent_dates && n < 7 * 24 * 60 * 60 => format!(
                "{} day{} ago",
                n / (24 * 60 * 60),
                if n / (24 * 60 * 60) == 1 { "" } else { "s" }
            ),
            _ => melib::datetime::timestamp_to_string(
                epoch,
                context
                    .settings
                    .listing
                    .datetime_fmt
                    .as_ref()
                    .map(String::as_str)
                    .or(Some("%Y-%m-%d %T")),
                false,
            ),
        }
    }

    mod conversations {
        use super::*;
        macro_rules! row_attr {
            ($field:ident, $color_cache:expr, $unseen:expr, $highlighted:expr, $selected:expr  $(,)*) => {{
                ThemeAttribute {
                    fg: if $highlighted {
                        $color_cache.highlighted.fg
                    } else if $selected {
                        $color_cache.selected.fg
                    } else if $unseen {
                        $color_cache.unseen.fg
                    } else {
                        $color_cache.$field.fg
                    },
                    bg: if $highlighted {
                        $color_cache.highlighted.bg
                    } else if $selected {
                        $color_cache.selected.bg
                    } else if $unseen {
                        $color_cache.unseen.bg
                    } else {
                        $color_cache.$field.bg
                    },
                    attrs: if $highlighted {
                        $color_cache.highlighted.attrs
                    } else if $selected {
                        $color_cache.selected.attrs
                    } else if $unseen {
                        $color_cache.unseen.attrs
                    } else {
                        $color_cache.$field.attrs
                    },
                }
            }};
            ($color_cache:expr, $unseen:expr, $highlighted:expr, $selected:expr  $(,)*) => {{
                ThemeAttribute {
                    fg: if $highlighted {
                        $color_cache.highlighted.fg
                    } else if $selected {
                        $color_cache.selected.fg
                    } else if $unseen {
                        $color_cache.unseen.fg
                    } else {
                        $color_cache.theme_default.fg
                    },
                    bg: if $highlighted {
                        $color_cache.highlighted.bg
                    } else if $selected {
                        $color_cache.selected.bg
                    } else if $unseen {
                        $color_cache.unseen.bg
                    } else {
                        $color_cache.theme_default.bg
                    },
                    attrs: if $highlighted {
                        $color_cache.highlighted.attrs
                    } else if $selected {
                        $color_cache.selected.attrs
                    } else if $unseen {
                        $color_cache.unseen.attrs
                    } else {
                        $color_cache.theme_default.attrs
                    },
                }
            }};
        }

        pub(super) fn draw_entry(
            grid: &mut CellBuffer,
            strings: &EntryStrings,
            area: Area,
            color_cache: &ColorCache,
            unseen: bool,
            highlighted: bool,
            selected: bool,
        ) {
            let (upper_left, bottom_right) = area;
            let width = width!(area);
            let row_attr = row_attr!(color_cache, unseen, highlighted, selected);
            /* draw flags */
            let (x, _) = write_string_to_grid(
                &strings.flag,
                grid,
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                area,
                None,
            );
            for c in grid.row_iter(x..(x + 3), get_y(upper_left)) {
                grid[c].set_bg(row_attr.bg);
            }
            let subject_attr = row_attr!(subject, color_cache, unseen, highlighted, selected);
            /* draw subject */
            let (mut x, _) = write_string_to_grid(
                &strings.subject,
                grid,
                subject_attr.fg,
                subject_attr.bg,
                subject_attr.attrs,
                (set_x(upper_left, x), bottom_right),
                None,
            );
            x += 1;
            for (t, &color) in strings.tags.split_whitespace().zip(strings.tags.1.iter()) {
                let color = color.unwrap_or(color_cache.tag_default.bg);
                let (_x, _) = write_string_to_grid(
                    t,
                    grid,
                    color_cache.tag_default.fg,
                    color,
                    color_cache.tag_default.attrs,
                    (set_x(upper_left, x + 1), bottom_right),
                    None,
                );
                grid[set_x(upper_left, x)].set_bg(color);
                if _x < width {
                    grid[set_x(upper_left, _x)].set_bg(color).set_keep_bg(true);
                }
                for c in grid.row_iter((x + 1).._x, get_y(upper_left)) {
                    grid[c].set_keep_fg(true).set_keep_bg(true);
                }
                grid[set_x(upper_left, x)].set_keep_bg(true);
                x = _x + 1;
            }
            for c in grid.row_iter(x..width, get_y(upper_left)) {
                grid[c].set_ch(' ').set_fg(row_attr.fg).set_bg(row_attr.bg);
            }
            let upper_left = pos_inc(upper_left, (0, 1));
            let date_attr = row_attr!(date, color_cache, unseen, highlighted, selected,);
            /* Next line, draw date */

            let (x, _) = write_string_to_grid(
                &strings.date,
                grid,
                date_attr.fg,
                date_attr.bg,
                date_attr.attrs,
                (upper_left, bottom_right),
                None,
            );
            for c in grid.row_iter(x..(x + 4), get_y(upper_left)) {
                grid[c].set_ch('▁').set_fg(row_attr.fg).set_bg(row_attr.bg);
            }
            let from_attr = row_attr!(from, color_cache, unseen, highlighted, selected,);
            /* draw from */
            let (x, _) = write_string_to_grid(
                &strings.from,
                grid,
                from_attr.fg,
                from_attr.bg,
                from_attr.attrs,
                (pos_inc(upper_left, (x + 4, 0)), bottom_right),
                None,
            );

            for c in grid.row_iter(x..width, get_y(upper_left)) {
                grid[c].set_ch('▁').set_fg(row_attr.fg).set_bg(row_attr.bg);
            }
        }

        impl MailboxList {
            pub(super) fn highlight_double_line(
                &mut self,
                grid: &mut CellBuffer,
                area: Area,
                idx: usize,
                context: &Context,
            ) {
                let (unseen, selection) = if let Some(ref entry) = self.entries.get(&idx) {
                    (
                        entry.unseen,
                        self.state
                            .selection
                            .get(&entry.thread_node_hash)
                            .cloned()
                            .unwrap_or(false),
                    )
                } else {
                    (false, false)
                };
                let row_attr = row_attr!(
                    self.color_cache,
                    unseen,
                    self.state.cursor_pos == idx,
                    selection
                );

                let padding_fg = if unseen {
                    self.color_cache.unseen_padding.fg
                } else {
                    self.color_cache.padding.fg
                };

                copy_area(
                    grid,
                    &self.data_columns.columns[0],
                    area,
                    (
                        (0, 2 * idx),
                        pos_dec(self.data_columns.columns[0].size(), (1, 1)),
                    ),
                );
                let (upper_left, bottom_right) = area;
                let (width, _) = self.data_columns.columns[0].size();
                let (x, y) = upper_left;
                if self.state.cursor_pos == idx || selection {
                    for x in x..=get_x(bottom_right) {
                        grid[(x, y)]
                            .set_fg(row_attr.fg)
                            .set_bg(row_attr.bg)
                            .set_attrs(row_attr.attrs);

                        grid[(x, y + 1)]
                            .set_fg(row_attr.fg)
                            .set_bg(row_attr.bg)
                            .set_attrs(row_attr.attrs);
                    }
                }
                if width < width!(area) {
                    /* fill any remaining columns, if our view is wider than self.content */
                    for x in (x + width)..=get_x(bottom_right) {
                        grid[(x, y)]
                            .set_fg(row_attr.fg)
                            .set_bg(row_attr.bg)
                            .set_attrs(row_attr.attrs);

                        grid[(x, y + 1)]
                            .set_fg(row_attr.fg)
                            .set_bg(row_attr.bg)
                            .set_attrs(row_attr.attrs);
                    }
                }
            }
        }
    }

    mod single_row {
        use super::*;
        macro_rules! row_attr {
            ($color_cache:expr, $even: expr, $unseen:expr, $highlighted:expr, $selected:expr  $(,)*) => {{
                ThemeAttribute {
                    fg: if $highlighted {
                        if $even {
                            $color_cache.even_highlighted.fg
                        } else {
                            $color_cache.odd_highlighted.fg
                        }
                    } else if $selected {
                        if $even {
                            $color_cache.even_selected.fg
                        } else {
                            $color_cache.odd_selected.fg
                        }
                    } else if $unseen {
                        if $even {
                            $color_cache.even_unseen.fg
                        } else {
                            $color_cache.odd_unseen.fg
                        }
                    } else if $even {
                        $color_cache.even.fg
                    } else {
                        $color_cache.odd.fg
                    },
                    bg: if $highlighted {
                        if $even {
                            $color_cache.even_highlighted.bg
                        } else {
                            $color_cache.odd_highlighted.bg
                        }
                    } else if $selected {
                        if $even {
                            $color_cache.even_selected.bg
                        } else {
                            $color_cache.odd_selected.bg
                        }
                    } else if $unseen {
                        if $even {
                            $color_cache.even_unseen.bg
                        } else {
                            $color_cache.odd_unseen.bg
                        }
                    } else if $even {
                        $color_cache.even.bg
                    } else {
                        $color_cache.odd.bg
                    },
                    attrs: if $highlighted {
                        if $even {
                            $color_cache.even_highlighted.attrs
                        } else {
                            $color_cache.odd_highlighted.attrs
                        }
                    } else if $selected {
                        if $even {
                            $color_cache.even_selected.attrs
                        } else {
                            $color_cache.odd_selected.attrs
                        }
                    } else if $unseen {
                        if $even {
                            $color_cache.even_unseen.attrs
                        } else {
                            $color_cache.odd_unseen.attrs
                        }
                    } else if $even {
                        $color_cache.even.attrs
                    } else {
                        $color_cache.odd.attrs
                    },
                }
            }};
        }

        pub(super) fn draw_entry(
            columns: &mut [CellBuffer; 12],
            idx: usize,
            strings: &EntryStrings,
            snoozed: bool,
            has_attachments: bool,
            unseen: bool,
            highlighted: bool,
            selected: bool,
            color_cache: &ColorCache,
        ) {
            let row_attr = row_attr!(color_cache, idx % 2 == 0, unseen, highlighted, selected);
            let min_width = (
                columns[0].size().0,
                columns[1].size().0,
                columns[2].size().0,
                columns[3].size().0,
                columns[4].size().0,
            );
            let (x, _) = write_string_to_grid(
                &idx.to_string(),
                &mut columns[0],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((0, idx), (min_width.0, idx)),
                None,
            );
            for c in columns[0].row_iter(x..min_width.0, idx) {
                columns[0][c].set_bg(row_attr.bg);
            }
            let (x, _) = write_string_to_grid(
                &strings.date,
                &mut columns[1],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((0, idx), (min_width.1.saturating_sub(1), idx)),
                None,
            );
            for c in columns[1].row_iter(x..min_width.1, idx) {
                columns[1][c].set_bg(row_attr.bg);
            }
            let (x, _) = write_string_to_grid(
                &strings.from,
                &mut columns[2],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((0, idx), (min_width.2, idx)),
                None,
            );
            for c in columns[2].row_iter(x..min_width.2, idx) {
                columns[2][c].set_bg(row_attr.bg);
            }
            let (x, _) = write_string_to_grid(
                &strings.flag,
                &mut columns[3],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((0, idx), (min_width.3, idx)),
                None,
            );
            for c in columns[3].row_iter(x..min_width.3, idx) {
                columns[3][c].set_bg(row_attr.bg);
            }
            let (x, _) = write_string_to_grid(
                &strings.subject,
                &mut columns[4],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((0, idx), (min_width.4, idx)),
                None,
            );
            let x = {
                let mut x = x + 1;
                for (t, &color) in strings.tags.split_whitespace().zip(strings.tags.1.iter()) {
                    let color = color.unwrap_or(color_cache.tag_default.bg);
                    let (_x, _) = write_string_to_grid(
                        t,
                        &mut columns[4],
                        color_cache.tag_default.fg,
                        color,
                        color_cache.tag_default.attrs,
                        ((x + 1, idx), (min_width.4, idx)),
                        None,
                    );
                    for c in columns[4].row_iter(x..(x + 1), idx) {
                        columns[4][c].set_bg(color);
                    }
                    for c in columns[4].row_iter(_x..(_x + 1), idx) {
                        columns[4][c].set_bg(color).set_keep_bg(true);
                    }
                    for c in columns[4].row_iter((x + 1)..(_x + 1), idx) {
                        columns[4][c]
                            .set_keep_fg(true)
                            .set_keep_bg(true)
                            .set_keep_attrs(true);
                    }
                    for c in columns[4].row_iter(x..(x + 1), idx) {
                        columns[4][c].set_keep_bg(true);
                    }
                    x = _x + 1;
                }
                x
            };
            for c in columns[4].row_iter(x..min_width.4, idx) {
                columns[4][c].set_ch(' ');
                columns[4][c].set_bg(row_attr.bg);
            }
            match (snoozed, has_attachments) {
                (true, true) => {
                    columns[3][(0, idx)].set_fg(color_cache.attachment_flag.fg);
                    columns[3][(2, idx)].set_fg(color_cache.thread_snooze_flag.fg);
                }
                (true, false) => {
                    columns[3][(0, idx)].set_fg(color_cache.thread_snooze_flag.fg);
                }
                (false, true) => {
                    columns[3][(0, idx)].set_fg(color_cache.attachment_flag.fg);
                }
                (false, false) => {}
            }
        }

        pub(super) fn draw_loading_entry(
            columns: &mut [CellBuffer; 12],
            idx: usize,
            highlighted: bool,
            color_cache: &ColorCache,
        ) {
            let row_attr = row_attr!(color_cache, idx % 2 == 0, false, highlighted, false);
            write_string_to_grid(
                LOADING_STR,
                &mut columns[4],
                row_attr.fg,
                row_attr.bg,
                row_attr.attrs,
                ((0, idx), (LOADING_STR.len() - 1, idx)),
                None,
            );
        }

        impl MailboxList {
            pub(super) fn highlight_single_line(
                &mut self,
                grid: &mut CellBuffer,
                area: Area,
                idx: usize,
                context: &Context,
            ) {
                let (unseen, selection) = if let Some(ref entry) = self.entries.get(&idx) {
                    (
                        entry.unseen,
                        self.state
                            .selection
                            .get(&entry.thread_node_hash)
                            .cloned()
                            .unwrap_or(false),
                    )
                } else {
                    (false, false)
                };

                let row_attr = row_attr!(
                    self.color_cache,
                    idx % 2 == 0,
                    unseen,
                    self.state.cursor_pos == idx,
                    selection,
                );
                let (upper_left, bottom_right) = area;

                for c in grid.row_iter(
                    get_x(upper_left)..(get_x(bottom_right) + 1),
                    get_y(upper_left),
                ) {
                    grid[c]
                        .set_fg(row_attr.fg)
                        .set_bg(row_attr.bg)
                        .set_attrs(row_attr.attrs);
                }
            }
        }
    }
}
