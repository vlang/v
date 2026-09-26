module time

// Leave enough headroom for heavily loaded CI runners to schedule both sides of
// the channel handoff before the timer expires.
const timer_test_timeout = 5 * second

fn test_timer_fires_once() {
	timer := new_timer(10 * millisecond)
	select {
		fired_at := <-timer.c {
			assert fired_at.unix() > 0
		}
		timer_test_timeout {
			assert false, 'timer did not fire'
		}
	}
	assert !timer.stop()
}

fn test_timer_stop_prevents_firing() {
	timer := new_timer(timer_test_timeout)
	assert timer.stop()
	assert !timer.stop()
	select {
		_ := <-timer.c {
			assert false, 'stopped timer fired'
		}
		20 * millisecond {
		}
	}
}
