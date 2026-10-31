module time

// Timer represents a single event. After duration d, its channel C receives
// the current time. Mirrors Go's time.Timer.
pub struct Timer {
pub:
	c chan Time
mut:
	stop_ch chan bool
	done    bool
}

// new_timer creates a Timer that fires after d.
pub fn new_timer(d Duration) &Timer {
	mut t := &Timer{
		c:       chan Time{cap: 1}
		stop_ch: chan bool{cap: 1}
	}
	spawn timer_loop(mut t, d)
	return t
}

fn timer_loop(mut t Timer, d Duration) {
	time.sleep(d)
	select {
		_ := <-t.stop_ch {
			t.done = true
			return
		}
		else {
			t.c <- time.now()
			t.done = true
		}
	}
}

// stop prevents the Timer from firing. It returns true if the call stopped
// the timer, false if it already fired or was stopped.
pub fn (mut t Timer) stop() bool {
	if t.done {
		return false
	}
	t.stop_ch <- true
	t.done = true
	return true
}

// reset changes the timer to expire after d. It returns true if the timer had
// been active.
pub fn (mut t Timer) reset(d Duration) bool {
	was_active := !t.done
	t.done = false
	// best-effort: spawn a new firing goroutine
	spawn timer_loop(mut t, d)
	return was_active
}
