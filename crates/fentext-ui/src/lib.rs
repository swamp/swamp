/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/piot/fentext-ui
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crossterm::cursor::MoveTo;
use crossterm::event::{Event, KeyCode, KeyEventKind};
use crossterm::style::{Color, Print, ResetColor};
use crossterm::terminal::{LeaveAlternateScreen, disable_raw_mode};
use crossterm::{
    cursor::{self},
    event, execute,
    style::SetForegroundColor,
    terminal::{Clear, ClearType, EnterAlternateScreen, enable_raw_mode},
};
use std::io;
use std::io::{Stdout, Write, stdout};
use std::time::Duration;

#[derive(Eq, PartialEq)]
pub enum Input {
    Left,
    Right,
    Up,
    Down,
    Esc,
    Action1,
    Action2,
    Start,
}

/// The `Tui` struct gives you direct control over the terminal for your game loop:
/// - Move the cursor around the screen.
/// - Write text wherever you need it.
/// - Get **gamepad-style input** (directional presses and action buttons) without blocking.
pub struct Tui {
    out: Stdout,
}

impl Drop for Tui {
    fn drop(&mut self) {
        // When the `Tui` struct is dropped, the terminal is restored to its
        // original state. This includes showing the cursor, leaving the
        // alternate screen, and disabling raw mode.
        //
        // Errors during cleanup are intentionally ignored here.
        let _ = execute!(self.out, cursor::Show);
        let _ = execute!(self.out, LeaveAlternateScreen);
        let _ = disable_raw_mode();
    }
}

impl Tui {
    /// # Errors
    ///
    pub fn new() -> io::Result<Self> {
        // Enable raw mode: enables key presses without buffering,
        // and disable automatic echoing of input characters.
        enable_raw_mode()?;

        let mut stdout = stdout();

        // Save the screen to a buffer that can be restored afterward.
        execute!(stdout, EnterAlternateScreen)?;

        // Hide the cursor. since we are not using text input in an action based
        // text game it is best to hide it.
        execute!(stdout, cursor::Hide)?;

        execute!(stdout, Clear(ClearType::All))?;

        Ok(Self { out: stdout })
    }

    pub fn move_to(&self, x: u16, y: u16) {
        execute!(&self.out, MoveTo(x, y)).expect("tui: move_to failed");
    }

    pub fn write(&self, str: &str, color: Color) {
        execute!(&self.out, SetForegroundColor(color), Print(str), ResetColor);
        //write!(&self.out, "{str}").expect("tui: write failed");
    }

    pub fn color(&self, color: &Color) {}

    #[must_use]
    pub fn poll(&self) -> Option<Input> {
        match event::poll(Duration::ZERO) {
            Ok(true) => {}
            Err(_) | Ok(false) => return None,
        }

        let Ok(event) = event::read() else {
            return None;
        };

        let Event::Key(key_event) = event else {
            return None;
        };

        if key_event.kind != KeyEventKind::Press {
            return None;
        }

        match key_event.code {
            KeyCode::Esc => Some(Input::Esc),
            KeyCode::Up => Some(Input::Up),
            KeyCode::Down => Some(Input::Down),
            KeyCode::Left => Some(Input::Left),
            KeyCode::Right => Some(Input::Right),
            KeyCode::Enter => Some(Input::Start),
            KeyCode::Char(c) => match c {
                'w' => Some(Input::Up),
                'a' => Some(Input::Left),
                's' => Some(Input::Down),
                'd' => Some(Input::Right),
                'z' | 'q' => Some(Input::Action1),
                'x' | 'e' => Some(Input::Action2),
                _ => None,
            },
            _ => None,
        }
    }
}
