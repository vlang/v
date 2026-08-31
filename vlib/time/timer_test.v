module time

fn test_timer_fires_once() {
	timer := new_timer(10 * millisecond)
	select {
		fired_at := <-timer.c {
			assert fired_at.unix() > 0
		}
		1 * second {
			assert false, 'timer did not fire'
		}
	}
	assert !timer.stop()
}

fn test_timer_stop_prevents_firing() {
	timer := new_timer(1 * second)
	assert timer.stop()
	assert !timer.stop()
	select {
		_ := <-timer.c {
			assert false, 'stopped timer fired'
		}
		20 * millisecond {}
	}
}
