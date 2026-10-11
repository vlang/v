module progress

import os
import strings
import sync
import time

// MultiBarOptions configures how a group of bars is drawn. Every field is
// optional, and it is a `@[params]` struct: `MultiBar.new()`,
// `MultiBar.new(delay: 20 * time.millisecond)`.
@[params]
pub struct MultiBarOptions {
pub mut:
	// delay is the minimum time between redraws.
	delay time.Duration = 50 * time.millisecond
	// hide_cursor hides the cursor while a live display is running. It is
	// restored on exit, Ctrl+C and SIGTERM.
	hide_cursor bool = true
	// output_fd is where bars and println() lines are written (2 = stderr).
	output_fd int = 2
	// force draws a live display even if output_fd is not a terminal.
	force bool
	// width overrides terminal-width detection (0 = detect).
	width int
}

// Drawable is anything a MultiBar can draw: a Bar or a Spinner.
interface Drawable {
mut:
	reset_clock()
	finish()
	finished_at() i64
	// frame returns the line to show now and whether the item is done. For a
	// finished item the line is what it leaves behind (possibly empty).
	frame(term_width int, live bool) (string, bool)
}

// MultiBar draws any number of bars and spinners at once with one render
// thread, in the order they were added.
//
// Items that are still running are redrawn in place as a block. A bar that
// completes leaves the block (printing its on_end text, or its final state if
// `leave` is set) and the remaining bars move up.
//
// Typical use: create it, add bars and spinners, call start(), advance the bars
// from any threads (and finish() the spinners when their work is over), then
// wait(). More items may be added while it is running, up until wait() has
// been called.
@[heap]
pub struct MultiBar {
mut:
	opts    MultiBarOptions
	mu      &sync.Mutex = sync.new_mutex() // guards the fields below
	active  []Drawable
	pending []string // println() lines waiting to be drawn
	started bool
	running bool
	closing bool // set by wait()/stop(): end once no bar is active
	joined  bool
	t       thread
	// Render thread only (written before the thread starts, then private):
	live  bool // true: draw a live block; false: only emit final lines
	drawn int  // rows the live block currently occupies
	last  string
}

// MultiBar.new creates an empty group.
pub fn MultiBar.new(opts MultiBarOptions) &MultiBar {
	return &MultiBar{
		opts: opts
	}
}

// add_bar adds a bar for `max` items. Items are drawn in the order added.
pub fn (mut g MultiBar) add_bar(max i64, opts BarOptions) &Bar {
	mut b := new_bar(max, opts)
	b.group = &g
	g.register(b)
	return b
}

// add_spinner adds a spinner, for work with no known size. It runs until you
// call its finish() (or stop() on the MultiBar).
pub fn (mut g MultiBar) add_spinner(opts SpinnerOptions) &Spinner {
	mut sp := new_spinner(opts)
	sp.group = &g
	g.register(sp)
	return sp
}

fn (mut g MultiBar) register(it Drawable) {
	g.mu.lock()
	if g.started && !g.running {
		g.mu.unlock()
		panic('progress: item added after the MultiBar had finished')
	}
	g.active << it
	g.mu.unlock()
}

// start begins drawing in a background thread. Calling it more than once is
// harmless.
pub fn (mut g MultiBar) start() {
	g.mu.lock()
	if g.started {
		g.mu.unlock()
		return
	}
	g.started = true
	g.running = true
	g.live = g.opts.force || is_interactive(g.opts.output_fd)
	for mut it in g.active {
		it.reset_clock()
	}
	g.mu.unlock()
	if g.live && g.opts.hide_cursor {
		hide_cursor_on(g.opts.output_fd)
	}
	g.t = spawn g.run()
}

// wait blocks until every bar has completed and the final frame is drawn.
// It never returns if some bar is never completed; call that bar's finish()
// or use stop(). Call it from one thread only.
pub fn (mut g MultiBar) wait() {
	g.mu.lock()
	g.closing = true
	g.mu.unlock()
	g.join()
}

// stop completes every bar and spinner where it stands, draws the last frame and returns.
// Use it to bail out early, e.g. after an error.
pub fn (mut g MultiBar) stop() {
	g.mu.lock()
	for mut it in g.active {
		it.finish()
	}
	g.closing = true
	g.mu.unlock()
	g.join()
}

// println prints `msg` on its own line above the bars. Unlike a plain
// println(), it cannot tear the display, and it is safe from any thread.
pub fn (mut g MultiBar) println(msg string) {
	g.mu.lock()
	if g.running {
		g.pending << msg
		g.mu.unlock()
		return
	}
	g.mu.unlock()
	line := if g.opts.force || is_interactive(g.opts.output_fd) { msg } else { plain_text(msg) }
	os.fd_write(g.opts.output_fd, line + '\n')
}

fn (mut g MultiBar) join() {
	g.mu.lock()
	if !g.started || g.joined {
		g.mu.unlock()
		return
	}
	g.joined = true
	g.mu.unlock()
	g.t.wait()
}

fn (mut g MultiBar) run() {
	for {
		out, finished := g.frame()
		if out.len > 0 {
			os.fd_write(g.opts.output_fd, out)
		}
		if finished {
			break
		}
		time.sleep(g.opts.delay)
	}
	if g.live && g.opts.hide_cursor {
		show_cursor_on(g.opts.output_fd)
	}
}

// FinishedLine is what a bar left behind, with when it completed, so bars that
// finish within the same frame are still printed in the order they finished.
struct FinishedLine {
	at   i64
	text string
}

fn (g &MultiBar) size() (int, int) {
	w, h := terminal_size(g.opts.output_fd)
	return if g.opts.width > 0 {
		g.opts.width
	} else if w > 0 {
		w
	} else {
		80
	}, h
}

// frame decides what to write for this tick. Returns the bytes (possibly
// empty) and whether the display is over.
fn (mut g MultiBar) frame() (string, bool) {
	width, rows := g.size()

	g.mu.lock()
	mut perm := g.pending.clone() // lines that become permanent output
	g.pending.clear()
	mut lines := []string{cap: g.active.len}
	mut keep := []Drawable{cap: g.active.len}
	mut finished_lines := []FinishedLine{}
	for mut it in g.active {
		line, done := it.frame(width, g.live)
		if done {
			if line.len > 0 {
				finished_lines << FinishedLine{
					at:   it.finished_at()
					text: line
				}
			}
		} else {
			lines << line
			keep << it
		}
	}
	g.active = keep
	finished_lines.sort(a.at < b.at)
	for fl in finished_lines {
		perm << fl.text
	}
	finished := g.closing && g.active.len == 0
	if finished {
		g.running = false // from here on println() writes directly
	}
	g.mu.unlock()

	// More bars than terminal rows cannot be redrawn in place (the block
	// would scroll and the cursor-up arithmetic would break), so show what
	// fits plus a summary line.
	if g.live && rows > 2 && lines.len > rows - 1 {
		hidden := lines.len - (rows - 2)
		lines = lines[..rows - 2].clone()
		lines << '... and ${hidden} more'
	}

	mut sb := strings.new_builder(256)
	if g.live {
		joined := lines.join('\n')
		if perm.len == 0 && g.drawn == lines.len && joined == g.last {
			return '', finished // nothing changed: no bytes, no flicker
		}
		if g.drawn > 0 {
			// Back to the top-left of the block, then erase it and below.
			sb.write_u8(`\r`)
			if g.drawn > 1 {
				sb.write_string('\x1b[${g.drawn - 1}A')
			}
			sb.write_string('\x1b[J')
		}
		for p in perm {
			sb.write_string(p)
			sb.write_u8(`\n`)
		}
		sb.write_string(joined)
		g.drawn = lines.len
		g.last = joined
	} else {
		for p in perm {
			sb.write_string(plain_text(p))
			sb.write_u8(`\n`)
		}
	}
	return sb.str(), finished
}
