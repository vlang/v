// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

pub struct StopWatchOptions {
	auto_start bool = true
}

// StopWatch is used to measure elapsed time.
pub struct StopWatch {
mut:
	elapsed u64
pub mut:
	start u64
	end   u64
}

// new_stopwatch initializes a new StopWatch with the current time as start.
pub fn new_stopwatch(opts StopWatchOptions) StopWatch {
	mut initial := u64(0)
	if opts.auto_start {
		initial = sys_mono_now()
	}
	return StopWatch{
		elapsed: 0
		start: initial
		end: 0
	}
}

// start starts the stopwatch. If the timer was paused, restarts counting.
pub fn (mut t StopWatch) start() {
	t.start = sys_mono_now()
	t.end = 0
}

// restart restarts the stopwatch. If the timer was paused, restarts counting.
pub fn (mut t StopWatch) restart() {
	t.start = sys_mono_now()
	t.end = 0
	t.elapsed = 0
}

// stop stops the timer, by setting the end time to the current time.
pub fn (mut t StopWatch) stop() {
	t.end = sys_mono_now()
}

// pause resets the `start` time and adds the current elapsed time to `elapsed`.
pub fn (mut t StopWatch) pause() {
	if t.start > 0 {
		if t.end == 0 {
			t.elapsed += sys_mono_now() - t.start
		} else {
			t.elapsed += t.end - t.start
		}
	}
	t.start = 0
}

// elapsed returns the Duration since the last start call
pub fn (t StopWatch) elapsed() Duration {
	if t.start > 0 {
		if t.end == 0 {
			return Duration(i64(sys_mono_now() - t.start + t.elapsed))
		} else {
			return Duration(i64(t.end - t.start + t.elapsed))
		}
	}
	return Duration(i64(t.elapsed))
}
