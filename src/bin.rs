/*
 * meli - bin.rs
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

/*! This crate contains the frontend stuff of the application. The application entry way on `src/bin.rs` creates an event loop and passes input to the `ui` module.

The mail handling stuff is done in the `melib` crate which includes all backend needs. The split is done to theoretically be able to create different frontends with the same innards.
 */

use std::alloc::System;

#[global_allocator]
static GLOBAL: System = System;

use ui;

pub use melib::*;
pub use ui::*;

#[macro_use]
extern crate chan;
use chan_signal;

use chan_signal::Signal;

use nix;

fn main() {
    /* Lock all stdio outs */
    //let _stdout = stdout();
    //let mut _stdout = _stdout.lock();
    /*
    let _stderr = stderr();
    let mut _stderr = _stderr.lock();
    */

    /* Catch SIGWINCH to handle terminal resizing */
    let signal = chan_signal::notify(&[Signal::WINCH]);

    /* Create the application State. This is the 'System' part of an ECS architecture */
    let mut state = State::new();

    let receiver = state.receiver();

    let worker_receiver = state.worker_receiver();

    /* Register some reasonably useful interfaces */
    let window = Box::new(Tabbed::new(vec![
        Box::new(listing::Listing::new(&state.context.accounts)),
        Box::new(AccountsPanel::new(&state.context)),
        Box::new(ContactList::default()),
    ]));

    let status_bar = Box::new(StatusBar::new(window));
    state.register_component(status_bar);

    let xdg_notifications = Box::new(ui::components::notifications::XDGNotifications {});
    state.register_component(xdg_notifications);
    state.register_component(Box::new(
        ui::components::notifications::NotificationFilter {},
    ));

    /* Keep track of the input mode. See ui::UIMode for details */
    'main: loop {
        state.render();

        'inner: loop {
            /* Check if any components have sent reply events to State. */
            let events: Vec<UIEvent> = state.context.replies();
            for e in events {
                state.rcv_event(e);
            }
            state.redraw();

            /* Poll on all channels. Currently we have the input channel for stdin, watching events and the signal watcher. */
            chan_select! {
                receiver.recv() -> r => {
                    match std::dbg!(r.unwrap()) {
                        ThreadEvent::Input(Key::Ctrl('z')) => {
                            state.switch_to_main_screen();
                            //_thread_handler.join().expect("Couldn't join on the associated thread");
                            let self_pid = nix::unistd::Pid::this();
                            nix::sys::signal::kill(self_pid, nix::sys::signal::Signal::SIGSTOP).unwrap();
                            state.switch_to_alternate_screen();
                            state.restore_input();
                            // BUG: thread sends input event after one received key
                            state.update_size();
                            state.render();
                            state.redraw();
                        },
                        ThreadEvent::Input(k) => {
                            match state.mode {
                                UIMode::Normal => {
                                    match k {
                                        Key::Char('q') | Key::Char('Q') => {
                                            drop(state);
                                            break 'main;
                                        },
                                        Key::Char(' ') => {
                                            state.mode = UIMode::Execute;
                                            state.rcv_event(UIEvent::ChangeMode(UIMode::Execute));
                                            state.redraw();
                                        }
                                        key  => {
                                            state.rcv_event(UIEvent::Input(key));
                                            state.redraw();
                                        },
                                    }
                                },
                                UIMode::Insert => {
                                    match k {
                                        Key::Char('\n') | Key::Esc => {
                                            state.mode = UIMode::Normal;
                                            state.rcv_event(UIEvent::ChangeMode(UIMode::Normal));
                                            state.redraw();
                                        },
                                        k => {
                                            state.rcv_event(UIEvent::InsertInput(k));
                                            state.redraw();
                                        },
                                    }
                                }
                                UIMode::Execute => {
                                    match k {
                                        Key::Char('\n') | Key::Esc => {
                                            state.mode = UIMode::Normal;
                                            state.rcv_event(UIEvent::ChangeMode(UIMode::Normal));
                                            state.redraw();
                                        },
                                        k => {
                                            state.rcv_event(UIEvent::ExInput(k));
                                            state.redraw();
                                        },
                                    }
                                },
                                UIMode::Fork => {
                                    break 'inner; // `goto` 'reap loop, and wait on child.
                                },
                            }
                        },
                        ThreadEvent::RefreshMailbox(event) => {
                            state.refresh_event(*event);
                            state.redraw();
                        },
                        ThreadEvent::UIEvent(UIEvent::ChangeMode(f)) => {
                            state.mode = f;
                            if f == UIMode::Fork {
                                break 'inner; // `goto` 'reap loop, and wait on child.
                            }
                        }
                        ThreadEvent::UIEvent(e) => {
                            state.rcv_event(e);
                            state.render();
                        },
                        ThreadEvent::ThreadJoin(id) => {
                            state.join(id);
                        },
                    }
                },
                signal.recv() -> signal => {
                    if state.mode != UIMode::Fork  {
                        if let Some(Signal::WINCH) = signal {
                            state.update_size();
                            state.render();
                            state.redraw();
                        }
                    }
                },
                worker_receiver.recv() -> _ => {
                    /* Some worker thread finished their job, acknowledge
                     * it and move on*/
                },
            }
        } // end of 'inner

        'reap: loop {
            match state.try_wait_on_child() {
                Some(true) => {
                    state.restore_input();
                    state.switch_to_alternate_screen();
                }
                Some(false) => {
                    use std::{thread, time};
                    let ten_millis = time::Duration::from_millis(1500);
                    thread::sleep(ten_millis);

                    continue 'reap;
                }
                None => {
                    state.mode = UIMode::Normal;
                    state.render();
                    break 'reap;
                }
            }
        }
    }
}
