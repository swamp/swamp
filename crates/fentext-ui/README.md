# fentext-ui

`fentext-ui` is a lightweight, opinionated Rust library designed for building responsive,
**real-time, action based, text-based games**.

## ✨ Features

* **Cursor Control**: Easily move the cursor to any screen coordinate.
* **Write Text**: Write text dynamically to the terminal.
* **Non-Blocking Gamepad-Style Input**: Poll for directional presses and action buttons without
  freezing your game loop.
* **Automatic Terminal Cleanup**: Safely restores the terminal state on exit.

## 🕹️ Why `fentext-ui`?

If you're looking to craft a fast, responsive, and truly **fentextic** 😉 UI for your next ASCII art
adventure or terminal-based roguelike, `fentext-ui` provides the functionality you need.
Built as a thin, opinionated wrapper around the excellent
[`crossterm`](https://crates.io/crates/crossterm/) library.

## 🚀 Getting Started

Add `fentext-ui` to your `Cargo.toml`:

```toml
[dependencies]
fentext-ui = "0.1.0"
```

## About Contributions

This is an open source project with a single copyright holder.
While the code is publicly available under [LICENSE](LICENSE), I am not accepting external
contributions at this time.

You are welcome to:

* Use the code according to the license terms
* Fork the project for your own use, following the license terms
* Report issues
* Provide feedback
* Share the project

If you have suggestions or find bugs, please feel free to open an issue for discussion. While I
cannot accept pull requests, I value your feedback and engagement with the project.

Thank you for your understanding and interest in the project! 🙏

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

_Copyright (c) 2025 Peter Bjorklund. All rights reserved._
