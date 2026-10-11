# `x.progress`

Terminal progress bars and spinners: one, or many at once.

## Quickstart

```v
import x.progress

fn main() {
	mut bar := progress.Bar.new(1000, text: 'work ')
	bar.start()
	for _ in 0 .. 1000 {
		bar.inc()
	}
	bar.wait()
}
```

All options are optional and are written inline, without naming a type: `Bar.new(100)`,
`Bar.new(100, text: 'work ', show_rate: true)`. (The options structs are `@[params]`.)

`bar.start()` begins drawing in a background thread, `bar.inc()` and `bar.add(n)` advance the
bar (from any thread), and `bar.wait()` returns once the bar has completed and was drawn for
the last time.

## Several bars

A `MultiBar` draws any number of bars and spinners as one block, with a single render thread.
Items are drawn in the order they were added. A finished item leaves the block, and the rest
move up.

```v
import x.progress

fn work(mut bar progress.Bar, n int) {
	for _ in 0 .. n {
		bar.inc()
	}
}

fn main() {
	mut mb := progress.MultiBar.new()
	mut a := mb.add_bar(100, text: 'alpha ', on_end: 'alpha done')
	mut b := mb.add_bar(250, text: 'bravo ', on_end: 'bravo done')
	mb.start()

	// advance the bars from any threads
	ta := spawn work(mut a, 100)
	tb := spawn work(mut b, 250)
	ta.wait()
	tb.wait()

	mb.println('log lines go above the bars without tearing them')
	mb.wait()
}
```

Bars must be held in `mut` variables, and passed to other threads as `mut`.
More items can be added while the group is running, until `wait()` is called.

## Spinners

A spinner is for work with no known size. It is driven exactly like a bar (`start`, `finish`,
`wait`, `set_text`, `println`, `is_done`, `elapsed`) and shows the same text and elapsed time,
but has no total, so there is no percentage, rate or ETA.

```v
import time
import x.progress

fn main() {
	mut sp := progress.Spinner.new(
		text:         'resolving '
		frames:       progress.spinner_line() // the default is spinner_dots()
		show_elapsed: true
		leave:        true // keep it on screen, with a done mark, when finished
	)
	sp.start()
	time.sleep(2 * time.second) // the work
	sp.finish()
	sp.wait()
}
```

In a `MultiBar`, spinners and bars mix freely:

```v
import time
import x.progress

fn main() {
	mut mb := progress.MultiBar.new()
	mut conn := mb.add_spinner(text: 'connecting ', on_end: 'connected')
	mut dl := mb.add_bar(100, text: 'download ')
	mb.start()
	time.sleep(500 * time.millisecond) // connecting...
	conn.finish()
	dl.add(100)
	mb.wait()
}
```

A bar finishes by itself when it reaches its maximum. **A spinner finishes only when you call
`finish()`** (or `MultiBar.stop()`), so `wait()` blocks until you do.

The frames come from `spinner_frames.v`: 76 sets, taken from the Go library
[schollz/progressbar](https://github.com/schollz/progressbar) (MIT license, see the notice in that
file) with the same numbering.
Use a preset (`spinner_dots()`, `spinner_line()`, `spinner_braille()`, `spinner_arrows()`,
`spinner_circle()`, `spinner_quarters()`, `spinner_grow()`, `spinner_bounce()`,
`spinner_pulse()`, `spinner_ellipsis()`, `spinner_earth()`, `spinner_moon()`), any set by number
with `spinner_set(27)` (`spinner_set_count()` of them), or your own `[]string`. The presets are
functions because V does not allow assigning a `const` array into a struct field.

The animation follows elapsed time (`interval`, 80ms by default), so it runs at the same speed
however often the display is redrawn. Frames of different widths are padded, so the text does not
jitter. The line is `<frame> <text> <elapsed>`; when it does not fit, the text is shortened with
`…`, and the frame and the time are kept.

## Units

With `show_rate: true` a bar shows its speed. By default that is items per second, scaled with
SI prefixes (`1.5kit/s`). For bytes, pick a preset:

```v
import x.progress

fn main() {
	mut bar := progress.Bar.new(250_000_000,
		text:      'download '
		show_rate: true
		rate_unit: progress.RateUnit.bytes_iec() // 12.3MiB/s
	)
	bar.start()
	bar.add(250_000_000)
	bar.wait()
}
```

| Preset                      | Prefix list | Looks like                     |
|-----------------------------|-------------|--------------------------------|
| `RateUnit.items()`          | SI, 1000    | `it/s`, `kit/s`, `Mit/s`, ...  |
| `RateUnit.bytes()`          | SI, 1000    | `B/s`, `kB/s`, `MB/s`, ...     |
| `RateUnit.bytes_iec()`      | IEC, 1024   | `B/s`, `KiB/s`, `MiB/s`, ...   |

The prefixes alone are `UnitPrefixList.si()` (the default) and `UnitPrefixList.iec()`, so any other
unit combines them: `RateUnit{ unit: 'frames', prefixes: progress.UnitPrefixList.iec() }`.
`period` and `period_string` change the time base, for example per minute.

## Behaviour

- **Output goes to stderr**, so stdout stays clean for piping.
- **Not a terminal** (a CI log, a file, a pipe): nothing live is drawn and no escape codes are
  written, including any ANSI escapes in labels, completion messages, and log lines.
  A bar or spinner that finishes prints one summary line instead.
- **Fits the terminal.** A line never reaches the last column; the bar graphic shrinks first.
  `length: 0` fills the available width. If there are more items than terminal rows, the rest
  are summarised as `... and N more`. ANSI styling is retained on live lines that fit;
  a shortened line is plain text to avoid splitting an escape or leaving a style active.
- **Nothing happens at import.** The cursor is hidden, and an at-exit hook plus SIGINT/SIGTERM
  hooks are registered, only when a live display starts.
- **Cursor and signals.** The cursor is restored on normal exit, on `exit()`, and on Ctrl+C or
  SIGTERM (the program then exits with 130 or 143). If your program already handles or ignores
  SIGINT/SIGTERM, that is left alone, like `term.show_cursor_on_exit()` does. The at-exit hook
  still restores the cursor if your handler ends the program with `exit()`. You can also set
  `MultiBarOptions{ hide_cursor: false }`.
- **When it draws live.** For stdout and stderr it uses V's own check
  (`term.can_show_color_on_*`): a terminal, `TERM` not `dumb`, and the `VCOLORS=always|never`
  override is honored. `NO_COLOR` does not apply, since it concerns colour and not cursor
  control. `MultiBarOptions.force` draws regardless.
- **Windows** (untested): V's runtime enables escape sequence processing when stdout is a
  terminal. If it is not enabled, the bars fall back to plain output. The terminal size is read
  from the stdout console handle.
- **Thread-safe.** Counters are atomic, and one thread draws.
  Increments saturate at the maximum, including very large or concurrent overshoots.
- **Rate and ETA** use a smoothed rate (an exponential moving average), not the overall average.
  Before a rate is known they show `--.-` and `--:--`.

## API summary

| Call                                    | What it does                                   |
|-----------------------------------------|------------------------------------------------|
| `Bar.new(max, ...)`                     | a bar that draws by itself                     |
| `Spinner.new(...)`                      | a spinner that draws by itself                 |
| `MultiBar.new(...)`                     | a group drawn together                         |
| `mb.add_bar(max, ...)`                  | add a bar to the group                         |
| `mb.add_spinner(...)`                   | add a spinner to the group                     |
| `.start()`, `.wait()`                   | begin drawing, and block until all is finished |
| `.finish()`                             | complete a bar early, or end a spinner         |
| `mb.stop()`                             | finish every item where it stands, and return  |
| `bar.inc()`, `bar.add(n)`, `bar.set(v)` | advance a bar (thread-safe)                    |
| `.set_text(s)`                          | change the label (thread-safe)                 |
| `.println(s)`, `mb.println(s)`          | print a line above the display                 |
| `.is_done()`, `.elapsed()`              | inspect                                        |
| `bar.value()`                           | how many items are done                        |

`BarOptions`: `text`, `on_end`, `leave`, `length`, `style`, `show_percent`, `show_rate`,
`rate_unit`, `show_elapsed`, `elapsed_fmt`, `show_left`, `left_fmt`, `smoothing`.

`SpinnerOptions`: `text`, `on_end`, `leave`, `frames`, `interval`, `done_frame`, `show_elapsed`,
`elapsed_fmt`.

`MultiBarOptions`: `delay`, `hide_cursor`, `output_fd`, `force`, `width`.

When a bar or spinner completes, `on_end` (if set) replaces it. Otherwise `leave: true` keeps
its final state, and otherwise it disappears.

### Styles

A bar's look is its `style`: `progress.ClassicStyle{}` (the default) or `progress.BlockStyle{}`,
which has 1/8 cell resolution. Both are structs with public fields (`start`, `end`, `fill`, ...).
Implement the one method `Style` interface for your own look.
