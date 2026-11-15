module time

import sync

// Timer sends one time value on its buffered channel after a duration elapses.
// Create timers with new_timer. Call stop when abandoning an active timer to
// release its worker. The channel stays open after firing, stop, and reset.
pub struct Timer {
pub:
	c chan Time
mut:
	stop_ch chan chan bool
	done_ch chan bool
	mu      &sync.Mutex
}

fn run_timer(duration Duration, output chan Time, stop chan chan bool, done chan bool) {
	defer {
		done.close()
	}
	select {
		reply := <-stop {
			reply <- true
			return
		}
		duration {
		}
	}
	select {
		reply := <-stop {
			reply <- true
		}
		output <- now() {
		}
	}
}

// new_timer creates a Timer that sends the current time after at least d.
// Nonpositive durations expire immediately. Its channel holds at most one value.
pub fn new_timer(d Duration) &Timer {
	t := &Timer{
		c:       chan Time{cap: 1}
		stop_ch: chan chan bool{}
		done_ch: chan bool{}
		mu:      sync.new_mutex()
	}
	spawn run_timer(d, t.c, t.stop_ch, t.done_ch)
	return t
}

// stop_locked cancels the current worker and removes its pending time value.
// The caller holds mu so that a reset cannot start another worker while draining.
fn (mut t Timer) stop_locked() bool {
	reply := chan bool{cap: 1}
	mut was_active := false
	select {
		t.stop_ch <- reply {
			was_active = <-reply
		}
		_ := <-t.done_ch {
		}
	}
	_ := <-t.done_ch
	select {
		_ := <-t.c {
		}
		else {
		}
	}
	return was_active
}

// stop prevents further sends and discards any buffered time value without
// closing c. It returns true if it stopped an active timer, or false if the timer
// had already sent its value or been stopped. Concurrent calls are safe.
pub fn (mut t Timer) stop() bool {
	t.mu.lock()
	defer { t.mu.unlock() }
	return t.stop_locked()
}

// reset cancels the old expiration and starts a new one after d. It discards any
// buffered time value, so receives after reset returns use the new expiration.
// It returns true if the old timer was active, or false if it had already sent
// its value or been stopped. Concurrent stop and reset calls are serialized.
pub fn (mut t Timer) reset(d Duration) bool {
	t.mu.lock()
	defer { t.mu.unlock() }
	was_active := t.stop_locked()
	t.stop_ch = chan chan bool{}
	t.done_ch = chan bool{}
	spawn run_timer(d, t.c, t.stop_ch, t.done_ch)
	return was_active
}
