module progress

import arrays
import math
import term
import time

// SpinnerOptions configures one spinner. Every field is optional, and it is a
// `@[params]` struct: `Spinner.new()`, `Spinner.new(text: 'working ')`.
//
// A spinner has no total, so there is no percentage, rate or ETA; what it shares
// with a bar is the text, the elapsed time and what it leaves behind when it
// finishes.
@[params]
pub struct SpinnerOptions {
pub mut:
	// text is shown after the frame. Trailing spaces are trimmed (the
	// renderer puts one space between the parts).
	text string
	// on_end, when not empty, replaces the spinner once it finishes.
	on_end string
	// leave keeps the finished spinner on screen (when on_end is empty),
	// showing done_frame instead of an animation frame.
	leave bool
	// frames are the animation frames. See spinner_frames.v for ready-made
	// sets: spinner_dots() (default), spinner_line(), spinner_set(N), ... or
	// give your own. An empty list falls back to spinner_dots().
	frames []string = spinner_dots()
	// interval is how long each frame is shown. The display can only change
	// as fast as MultiBarOptions.delay (50ms by default). Zero or negative
	// falls back to 80ms.
	interval time.Duration = 80 * time.millisecond
	// done_frame replaces the animation once the spinner has finished (visible
	// with `leave`, or in plain, non-terminal output).
	done_frame string = '✓'
	// show_elapsed shows the elapsed time after the text, like a bar.
	show_elapsed bool
	elapsed_fmt  TimeFormat
}

// Spinner is an animation for work with no known size. It is driven exactly
// like a Bar: Spinner.new (or MultiBar.add_spinner), start(), finish(),
// wait(), plus set_text(), println(), is_done() and elapsed(), all from the
// embedded Item.
//
// A bar finishes by itself when it reaches its maximum; a spinner has no
// maximum, so it runs until you call finish() (or MultiBar.stop()).
// Hold it in a `mut` variable.
@[heap]
pub struct Spinner {
	Item
mut:
	opts    SpinnerOptions
	frame_w int // widest frame, so the text does not jitter as frames change width
}

fn new_spinner(opts SpinnerOptions) &Spinner {
	mut o := opts
	if o.frames.len == 0 {
		o.frames = spinner_dots()
	}
	if o.interval <= 0 {
		o.interval = 80 * time.millisecond
	}
	// Frames may carry trailing spaces of their own (some sets use them to pad
	// to a common width). Strip them and let render() pad uniformly, so the gap
	// before the text is the same for every frame set.
	o.frames = o.frames.map(it.trim_right(' '))
	w := math.max(term.printable_len(o.done_frame), arrays.max(o.frames.map(term.printable_len(it))) or {
		0
	})
	mut s := &Spinner{
		opts:    o
		frame_w: w
	}
	s.text = o.text
	s.reset_clock()
	return s
}

// Spinner.new creates a spinner that draws by itself. Call start(), do your
// work, then finish() and wait().
pub fn Spinner.new(opts SpinnerOptions) &Spinner {
	mut g := MultiBar.new()
	g.closing = true // a lone spinner's display ends when the spinner does
	return g.add_spinner(opts)
}

// frame_at is the animation frame shown `elapsed_ns` after the start.
fn (s &Spinner) frame_at(elapsed_ns i64) string {
	if elapsed_ns < 0 {
		return s.opts.frames[0]
	}
	step := elapsed_ns / i64(s.opts.interval)
	return s.opts.frames[int(step % i64(s.opts.frames.len))]
}

// render returns the single line for a spinner that has run for `elapsed`,
// guaranteed to fit within `term_width` columns. If it is too long the text is
// shortened (with an ellipsis); the frame and the time are kept.
fn (mut s Spinner) render(elapsed time.Duration, done bool, term_width int) string {
	avail := if term_width > 1 { term_width - 1 } else { 79 }
	glyph := if done { s.opts.done_frame } else { s.frame_at(i64(elapsed)) }
	pad := s.frame_w - term.printable_len(glyph)
	head := if pad > 0 { glyph + ' '.repeat(pad) } else { glyph }
	tail := if s.opts.show_elapsed { fmt_time(elapsed, s.opts.elapsed_fmt) } else { '' }

	mut text := s.get_text().trim_right(' ')
	if text.len > 0 {
		mut room := avail - term.printable_len(head) - 1 // the space before the text
		if tail.len > 0 {
			room -= term.printable_len(tail) + 1 // the space before the time
		}
		if term.printable_len(text) > room {
			text = if room >= 2 { cut_to(text, room - 1) + '…' } else { '' }
		}
	}

	mut line := head
	if text.len > 0 {
		line += ' ' + text
	}
	if tail.len > 0 {
		line += ' ' + tail
	}
	line = line.trim_right(' ')
	if term.printable_len(line) > avail {
		line = cut_to(line, avail)
	}
	return line
}

// frame is what MultiBar asks of every item each tick. See Bar.frame.
fn (mut s Spinner) frame(term_width int, live bool) (string, bool) {
	el := time.Duration(s.elapsed_ns())
	if s.is_done() {
		return s.final_line(el, term_width, live), true
	}
	return s.render(el, false, term_width), false
}

// final_line is what a finished spinner leaves behind, or '' for nothing.
fn (mut s Spinner) final_line(elapsed time.Duration, term_width int, live bool) string {
	if s.opts.on_end.len > 0 {
		return s.opts.on_end
	}
	// With no live display there is nothing to "leave behind", so plain
	// output (a log) still gets one summary line.
	if s.opts.leave || !live {
		return s.render(elapsed, true, term_width)
	}
	return ''
}
