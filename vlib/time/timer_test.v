module time

fn test_timer_fires() {
	mut timer := new_timer(millisecond)
	defer { timer.stop() }
	select {
		fired_at := <-timer.c {
			assert fired_at.unix() > 0
		}
		5 * second {
			assert false, 'timer did not fire'
		}
	}
	assert !timer.stop()
	assert !timer.c.closed
}

fn test_timer_stop() {
	mut timer := new_timer(hour)
	assert timer.stop()
	assert timer.done_ch.closed
	assert !timer.c.closed
}
