module progress

import math
import strings
import sync.stdatomic
import term
import time

// BarOptions configures one bar. Every field is optional, and it is a `@[params]`
// struct, so options are written inline without naming the type, or left out
// entirely: `Bar.new(100)`, `Bar.new(100, text: 'work ', show_rate: true)`.
@[params]
pub struct BarOptions {
pub mut:
	// text is printed before the percentage. Include your own trailing space.
	text string
	// on_end, when not empty, replaces the bar once it completes.
	on_end string
	// leave keeps the finished bar on screen (when on_end is empty).
	// By default a finished bar disappears.
	leave bool
	// length is the width of the bar graphic in cells. 0 means "fill the
	// terminal". It is shrunk automatically if the line would not fit.
	length int = 30
	// style draws the bar graphic: ClassicStyle{} (default), BlockStyle{}, or your own.
	style Style = ClassicStyle{}

	show_percent bool = true
	show_rate    bool
	rate_unit    RateUnit
	show_elapsed bool
	elapsed_fmt  TimeFormat
	show_left    bool = true
	left_fmt     TimeFormat
	// smoothing is the rate estimator's time constant in seconds (see
	// RateEstimator.tau). It drives the rate and ETA displays.
	smoothing f64 = 2.0
}

// Snapshot is a consistent view of a bar at one instant. Rendering works from
// a Snapshot, so it can be tested without clocks or threads.
struct Snapshot {
	value   i64
	max     i64
	elapsed time.Duration
	// rate is items per second, or negative while not yet known.
	rate f64
	done bool
}

// fraction is the completed share, 0.0..=1.0 (a zero-sized bar is complete).
fn (s Snapshot) fraction() f64 {
	if s.max <= 0 {
		return 1.0
	}
	return math.clamp(f64(s.value) / f64(s.max), 0.0, 1.0)
}

// Bar is one progress bar. It is safe to advance from many threads at once.
//
// Create it with Bar.new (a bar on its own) or MultiBar.add_bar (several bars
// drawn together). Hold it in a `mut` variable.
//
// start(), wait(), finish(), is_done(), elapsed(), set_text() and println()
// come from the embedded Item, so a Spinner has exactly the same ones.
@[heap]
pub struct Bar {
	Item
pub:
	max i64
mut:
	opts  BarOptions
	count &stdatomic.AtomicVal[i64] = stdatomic.new_atomic(i64(0))
	est   RateEstimator // touched only by the render thread
}

fn new_bar(max i64, opts BarOptions) &Bar {
	if max < 0 {
		panic('progress: the maximum of a bar must not be negative (got ${max})')
	}
	mut b := &Bar{
		max:  max
		opts: opts
		text: opts.text
		est:  RateEstimator{
			tau: opts.smoothing
		}
	}
	b.reset_clock()
	if max == 0 {
		b.mark_done() // nothing to do is already done
	}
	return b
}

// Bar.new creates a bar for `max` items that draws by itself. Call start(),
// advance it with inc()/add(), then wait().
pub fn Bar.new(max i64, opts BarOptions) &Bar {
	mut g := MultiBar.new()
	g.closing = true // a lone bar's display ends when the bar does
	return g.add_bar(max, opts)
}

// inc advances the bar by one.
pub fn (mut b Bar) inc() {
	b.add(1)
}

// add advances the bar by `n`. Non-positive `n` is ignored. Reaching the
// maximum completes the bar; the count saturates at max without overflowing.
pub fn (mut b Bar) add(n i64) {
	if n <= 0 {
		return
	}
	mut prev := b.count.load()
	for {
		// Compare with the remaining work before adding, so even max_i64 is safe.
		next := if n >= b.max - prev { b.max } else { prev + n }
		if b.count.compare_and_swap(prev, next) {
			if next >= b.max {
				b.mark_done()
			}
			return
		}
		prev = b.count.load()
	}
}

// set moves the bar to an absolute position (clamped to 0..max).
pub fn (mut b Bar) set(v i64) {
	n := math.max(i64(0), math.min(v, b.max))
	b.count.store(n)
	if n >= b.max {
		b.mark_done()
	}
}

// value is how many items are done so far (never more than max).
pub fn (mut b Bar) value() i64 {
	return math.min(b.count.load(), b.max)
}

// snapshot reads the bar and feeds the rate estimator. Render thread only.
fn (mut b Bar) snapshot() Snapshot {
	v := math.min(b.count.load(), b.max)
	done := b.is_done()
	el := b.elapsed_ns()
	mut rate := -1.0
	if done {
		// A finished bar reports its overall average, which is the honest
		// figure for a summary line.
		if el > 0 {
			rate = f64(v) / time.Duration(el).seconds()
		}
	} else {
		b.est.update(v, el)
		rate = b.est.rate()
	}
	return Snapshot{
		value:   v
		max:     b.max
		elapsed: time.Duration(el)
		rate:    rate
		done:    done
	}
}

fn eta_string(snap Snapshot, f TimeFormat) string {
	if snap.done {
		return fmt_time(time.Duration(0), f)
	}
	if snap.rate > 0.0 {
		secs := f64(snap.max - snap.value) / snap.rate
		if secs < 100.0 * 3600.0 {
			return fmt_time(time.second.times(secs), f)
		}
	}
	return fmt_time_unknown(f)
}

// compose builds the line with the bar graphic exactly `cells` wide.
fn (b &Bar) compose(snap Snapshot, text string, cells int) string {
	o := b.opts
	f := snap.fraction()
	mut sb := strings.new_builder(96 + cells * 3)
	sb.write_string(text)
	if o.show_percent {
		sb.write_string('${(f * 100.0):6.2f}%')
	}
	o.style.draw(mut sb, f, cells)
	if o.show_rate {
		sb.write_u8(` `)
		sb.write_string(if snap.rate < 0.0 {
			fmt_rate_unknown(o.rate_unit)
		} else {
			fmt_rate(snap.rate, o.rate_unit)
		})
	}
	if o.show_left {
		sb.write_u8(` `)
		sb.write_string(eta_string(snap, o.left_fmt))
	}
	if o.show_elapsed {
		sb.write_u8(` `)
		sb.write_string(fmt_time(snap.elapsed, o.elapsed_fmt))
	}
	return sb.str()
}

// render returns the single line for `snap`, guaranteed to fit within
// `term_width` columns (a line that reaches the last column makes terminals
// wrap, which would break redrawing in place). The bar graphic shrinks first;
// if that is not enough the line is cut.
fn (mut b Bar) render(snap Snapshot, term_width int) string {
	text := b.get_text()
	avail := if term_width > 1 { term_width - 1 } else { 79 }
	over := term.printable_len(b.compose(snap, text, 0))
	mut cells := if b.opts.length > 0 { b.opts.length } else { avail - over }
	if over + cells > avail {
		cells = avail - over
	}
	cells = math.max(cells, min_cells)
	mut line := b.compose(snap, text, cells)
	if term.printable_len(line) > avail {
		line = cut_to(line, avail)
	}
	return line
}

// frame is what MultiBar asks of every item each tick: the line to show, and
// whether the item is done (then the line is its final one, possibly empty).
fn (mut b Bar) frame(term_width int, live bool) (string, bool) {
	snap := b.snapshot()
	if snap.done {
		return b.final_line(snap, term_width, live), true
	}
	return b.render(snap, term_width), false
}

// final_line is what a completed bar leaves behind, or '' for nothing.
fn (mut b Bar) final_line(snap Snapshot, term_width int, live bool) string {
	if b.opts.on_end.len > 0 {
		return b.opts.on_end
	}
	// With no live display there is nothing to "leave behind", so a plain
	// output (CI log, file) still gets one summary line per bar.
	if b.opts.leave || !live {
		return b.render(snap, term_width)
	}
	return ''
}
