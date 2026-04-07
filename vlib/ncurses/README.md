## `ncurses`

Thin bindings for the system `ncurses` or `curses` library.

Supported targets:

- macOS
- Linux
- BSD and Solaris targets that provide a compatible curses library
- Windows currently reports `ncurses.is_supported() == false`

Most functions return `ncurses.ok` on success and `ncurses.err` on failure.
`getch()` and `wgetch()` return an `int`. Compare special keys with
`ncurses.key_*`.

```v
import ncurses

fn main() {
	if !ncurses.is_supported() {
		return
	}
	stdscr := ncurses.initscr()
	defer {
		ncurses.endwin()
	}
	ncurses.cbreak()
	ncurses.noecho()
	ncurses.keypad(stdscr, true)
	ncurses.addstr('Press a key...')
	ncurses.refresh()

	key := ncurses.getch()
	ncurses.mvaddstr(1, 0, 'Key code: ${key}')
	ncurses.refresh()
	ncurses.getch()
}
```

Use `newwin`, `box`, `waddstr`, `wrefresh`, and `wgetch` for additional windows.
Use `start_color`, `init_pair`, and `color_pair` for color attributes.
