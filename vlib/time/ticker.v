module time

import sync

// Ticker delivers periodic time values on a buffered channel, dropping ticks
// when the receiver falls behind. Create tickers with new_ticker and call stop
// when finished to release the worker. The channel stays open after stop.
pub struct Ticker {
pub:
	c chan Time
mut:
	stop_ch chan bool
	done_ch chan bool
	mu      &sync.Mutex
}

// new_ticker returns a Ticker that ticks every d, which must be positive.
// The channel holds at most one tick. Call stop when finished with the ticker.
pub fn new_ticker(d Duration) &Ticker {
	if d <= 0 {
		panic('non-positive interval for new_ticker')
	}
	t := &Ticker{
		c:       chan Time{cap: 1}
		stop_ch: chan bool{}
		done_ch: chan bool{}
		mu:      sync.new_mutex()
	}
	spawn ticker_loop(t.c, t.stop_ch, t.done_ch, d, sys_mono_now())
	return t
}

fn ticker_loop(output chan Time, stop chan bool, done chan bool, d Duration, started u64) {
	defer {
		done.close()
	}
	mut next_tick := started + u64(d)
	for {
		current := sys_mono_now()
		remaining := if current < next_tick { Duration(next_tick - current) } else { Duration(0) }
		select {
			_ := <-stop {
				return
			}
			remaining {
			}
		}
		current_tick := sys_mono_now()
		if current_tick < next_tick {
			continue
		}
		select {
			output <- now() {
			}
			else {
			}
		}
		// Keep the original cadence and skip missed intervals instead of queuing them.
		next_tick = current_tick + u64(d) - (current_tick - next_tick) % u64(d)
	}
}

// stop prevents further ticks, waits for the worker to finish, and removes any
// buffered tick without closing c. Repeated and concurrent calls are safe.
pub fn (mut t Ticker) stop() {
	t.mu.lock()
	defer { t.mu.unlock() }
	if !t.stop_ch.closed {
		t.stop_ch.close()
	}
	_ := <-t.done_ch
	select {
		_ := <-t.c {
		}
		else {
		}
	}
}
