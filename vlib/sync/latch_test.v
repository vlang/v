import sync
import time

// Waiters are released exactly when the counter reaches zero.
fn test_latch_releases_at_zero() {
	n := 4
	mut l := sync.new_latch(n)
	done_ch := chan bool{cap: n}
	defer {
		done_ch.close()
	}
	for _ in 0 .. n {
		spawn fn [mut l, done_ch] () {
			l.wait()
			done_ch <- true
		}()
	}
	for i in 0 .. n - 1 {
		l.count_down()
		assert l.count() == n - 1 - i, 'counter must track countdowns'
	}
	// No waiter may pass while the counter is still positive.
	select {
		_ := <-done_ch {
			assert false, 'latch opened before the final countdown'
		}
		20 * time.millisecond {
		}
	}
	assert l.count() == 1, 'counter must be 1 before the last countdown'
	l.count_down()
	for _ in 0 .. n {
		_ := <-done_ch
	}
	assert l.count() == 0, 'counter must stay at zero'
}

// A zero latch starts open; spent latches stay open.
fn test_latch_zero_starts_open() {
	mut l := sync.new_latch(0)
	l.wait()
	l.count_down() // no-op past zero
	l.wait()
	assert l.count() == 0, 'counter must stay at zero'
}

// Countdowns past zero are no-ops and never go negative.
fn test_latch_countdown_past_zero_is_noop() {
	mut l := sync.new_latch(1)
	l.count_down()
	l.count_down()
	l.count_down()
	assert l.count() == 0, 'counter must not go negative'
}
