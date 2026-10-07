module progress

import math
import sync
import sync.stdatomic
import time

// (On the clock: time.StopWatch is not used because it is a plain struct that
// start() would have to rewrite while worker threads may be reading it, so the
// start and end times are atomics.)

// Item is the part of a Bar and a Spinner that has nothing to do with *what*
// they show: a label, a clock, a completion time, and the MultiBar they belong
// to. Both embed it, so start, wait, finish, elapsed, set_text and println behave
// identically on either, which is the point: a spinner is driven exactly like
// a bar. You do not create an Item yourself.
@[heap]
pub struct Item {
mut:
	text  string
	mu    &sync.Mutex               = sync.new_mutex()             // guards text
	t0    &stdatomic.AtomicVal[i64] = stdatomic.new_atomic(i64(0)) // start, monotonic ns
	t_end &stdatomic.AtomicVal[i64] = stdatomic.new_atomic(i64(0)) // elapsed ns at completion; 0 while running
	group &MultiBar                 = unsafe { nil }
}

fn (mut c Item) reset_clock() {
	if c.t_end.load() == 0 {
		c.t0.store(i64(time.sys_mono_now()))
	}
}

fn (mut c Item) elapsed_ns() i64 {
	end := c.t_end.load()
	if end > 0 {
		return end
	}
	return i64(time.sys_mono_now()) - c.t0.load()
}

// finished_at is an absolute monotonic timestamp of completion, for ordering.
fn (mut c Item) finished_at() i64 {
	return c.t0.load() + c.t_end.load()
}

fn (mut c Item) mark_done() {
	ns := math.max(i64(time.sys_mono_now()) - c.t0.load(), i64(1))
	c.t_end.compare_and_swap(i64(0), ns) // first caller wins
}

fn (mut c Item) get_text() string {
	c.mu.lock()
	s := c.text
	c.mu.unlock()
	return s
}

// start begins drawing. For an item from MultiBar.add_bar / add_spinner this
// starts the whole MultiBar. Calling it more than once is harmless.
pub fn (mut c Item) start() {
	c.group.start()
}

// wait blocks until every item in the group has finished and been drawn for
// the last time. It never returns while an item is unfinished: a bar finishes
// at its maximum, but a spinner only when you call finish(). Use finish() on
// the stragglers, or MultiBar.stop().
pub fn (mut c Item) wait() {
	c.group.wait()
}

// finish completes the item where it stands: a bar at its current position, a
// spinner at whatever moment you decide the work is over.
pub fn (mut c Item) finish() {
	c.mark_done()
}

// is_done reports whether the item has finished.
pub fn (mut c Item) is_done() bool {
	return c.t_end.load() != 0
}

// elapsed is the time since start(), frozen once the item finishes.
pub fn (mut c Item) elapsed() time.Duration {
	return time.Duration(c.elapsed_ns())
}

// set_text changes the label. Thread-safe, so a worker can show what it is
// doing right now.
pub fn (mut c Item) set_text(s string) {
	c.mu.lock()
	c.text = s
	c.mu.unlock()
}

// println prints a line above the display without garbling it.
// See MultiBar.println.
pub fn (mut c Item) println(msg string) {
	c.group.println(msg)
}
