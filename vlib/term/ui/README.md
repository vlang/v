## `term.ui`

A V module for designing terminal UI apps

#### Quickstart

```v
import term.ui as tui

struct App {
mut:
    tui  &tui.Context = 0
}

fn event(e &tui.Event, x voidptr) {
    mut app := &App(x)
    println(e)
}

fn frame(x voidptr) {
    mut app := &App(x)

    app.tui.clear()
    app.tui.set_bg_color(r: 63, g: 81, b: 181)
    app.tui.draw_rect(20, 6, 41, 10)
    app.tui.draw_text(24, 8, 'Hello from V!')
    app.tui.set_cursor_position(0, 0)

    app.tui.reset()
    app.tui.flush()
}

mut app := &App{}
app.tui = tui.init(
    user_data: app,
    event_fn: event,
    frame_fn: frame
    hide_cursor: true
)
app.tui.run()
```

See the `/examples/term.ui/` folder for more usage examples.

#### Configuration

- `user_data voidptr` - a pointer to any `user_data`, it will be passed as the last argument to
    each callback. Used for accessing your app context from the different callbacks.
- `init_fn fn(voidptr)` - a callback that will be called after initialization
    and before the first event / frame. Useful for initializing any user data.
- `frame_fn fn(voidptr)` - a callback that will be fired on each frame,
    at a rate of `frame_rate` frames per second.
`event_fn fn(&Event, voidptr)` - a callback that will be fired for every event received.
- `cleanup_fn fn(voidptr)` - a callback that will be fired once, before the application exits.
- `fail_fn  fn(string)` - a callback that will be fired
    if a fatal error occurs during app initialization.
- `buffer_size int = 256` - the internal size of the read buffer.
    Increasing it may help in case you're missing events, but you probably shouldn't lower
    this value unless you make sure you're still receiving all events. In general,
    higher frame rates work better with lower buffer sizes, and vice versa.
- `frame_rate int = 30` - the number of times per second that the `frame` callback will be fired.
    30fps is a nice balance between smoothness and performance,
    but you can increase or lower it as you wish.
- `hide_cursor bool` - whether to hide the mouse cursor. Useful if you want to use your own.
- `capture_events bool` - sets the terminal into raw mode, which makes it intercept some
    escape codes such as `ctrl + c` and `ctrl + z`.
    Useful if you want to use those key combinations in your app.
- `window_title string` - sets the title of the terminal window.
    This may be changed later, by calling the `set_window_title()` method.
- `reset []int = [1, 2, 3, 4, 6, 7, 8, 9, 11, 13, 14, 15, 19]` - a list of reset signals,
    to setup handlers to cleanup the terminal state when they're received.
    You should not need to change this, unless you know what you're doing.

All of these fields may be omitted, in which case, the default value will be used.
In the case of the various callbacks, they will not be fired if a handler has not been specified.


#### FAQ

Q: Why does this module not work on Windows?
A: As with many other things, Windows has a completely different and incompatible way of handling
input parsing and drawing primitives, and support has not been implemented yet.
Contributions are definitely welcome though.

Q: My terminal (doesn't receive events / doesn't print anything / prints gibberish characters),
what's up with that?
A: Please check if your terminal. The module has been tested with `xterm`-based terminals on Linux
(like `gnome-terminal` and `konsole`), and `Terminal.app` and `iterm2` on macOS.
If your terminal does not work, open an issue with the output of `echo $TERM`.

Q: There are screen tearing issues when doing large prints
A: This is an issue with how terminals render frames,
as they may decide to do so in the middle of receiving a frame,
and cannot be fully fixed unless your console implements the [synchronized updates spec](https://gitlab.com/gnachman/iterm2/-/wikis/synchronized-updates-spec).
It can be reduced *drastically*, though, by using the rendering methods built in to the module,
and by only painting frames when your app's content has actually changed.

Q: Why does the module only emit `keydown` events, and not `keyup` like `sokol`/`gg`?
A: It's because of the way terminals emit events. Every key event is received as a keypress,
and there isn't a way of telling terminals to send keyboard events differently,
nor a reliable way of converting these into `keydown` / `keyup` events.
