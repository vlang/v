// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

// Timer struct and functions
// start and end are # of ms since start of system.
// When end_ticks == 0, this means the timer is actively "running"
pub struct Timer {
	pause_start i64
pub:
	start_ticks i64
	end_ticks   i64
}

// start Starts the timer. If the timer was paused, restarts counting.
pub fn (t mut Timer) start() {
	if t.pause_start == 0 {
		t.start_ticks = ticks()
	} else {
		// We were paused, so pretend like the time we were paused didn't
		// happen
		t.start_ticks += ticks() - t.pause_start
	}
	t.end_ticks = 0
	t.pause_start = 0
}

pub fn (t mut Timer) restart() {
	t.end_ticks = 0
	t.pause_start = 0
	t.start_ticks = ticks()
}

pub fn (t mut Timer) pause() {
	t.pause_start = ticks()
	t.end_ticks = t.pause_start // so elapsed() still keeps track of the cared-amount of time
}

pub fn (t mut Timer) stop() {
	t.end_ticks = ticks()
	t.pause_start = 0
}

// elapsed If the Timer is stopped, returns the number of milliseconds between
// the last start and stop.
// If the Timer is still running, returns the number of milliseconds from the
// last start to now.
pub fn (t Timer) elapsed() i64 {
	if t.end_ticks == 0 {
		return ticks() - t.start_ticks
	} else {
		return t.end_ticks - t.start_ticks
	}
}

pub fn new_timer() Timer {
	return Timer{start_ticks: ticks()}
}

