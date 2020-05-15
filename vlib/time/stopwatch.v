// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

pub struct StopWatch {
mut:
	pause_time u64
pub mut:
	start      u64
	end        u64
}

pub fn new_stopwatch() StopWatch {
	return StopWatch{pause_time: 0, start: time.sys_mono_now(), end: 0}
}

// start Starts the timer. If the timer was paused, restarts counting.
pub fn (t mut StopWatch) start() {
	if t.pause_time == 0 {
		t.start = time.sys_mono_now()
	} else {
		t.start += time.sys_mono_now() - t.pause_time
	}
	t.end = 0
	t.pause_time = 0
}

pub fn (t mut StopWatch) restart() {
	t.end = 0
	t.pause_time = 0
	t.start = time.sys_mono_now()
}

pub fn (t mut StopWatch) stop() {
	t.end = time.sys_mono_now()
	t.pause_time = 0
}

pub fn (t mut StopWatch) pause() {
	t.pause_time = time.sys_mono_now()
	t.end = t.pause_time // so elapsed keeps track of actual running time
}

// elapsed returns the Duration since the last start call
pub fn (t StopWatch) elapsed() Duration {
	if t.end == 0 {
		return Duration(time.sys_mono_now() - t.start)
	} else {
		return Duration(t.end - t.start)
	}
}

