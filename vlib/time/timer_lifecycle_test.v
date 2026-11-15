module time

const timer_lifecycle_timeout = 5 * second

fn wait_for_timer_worker(done chan bool) {
	select {
		_ := <-done {
		}
		timer_lifecycle_timeout {
			assert false, 'timer worker did not finish'
		}
	}
}

fn test_timer_stop_active_reports_success() {
	mut failures := 0
	for _ in 0 .. 32 {
		mut timer := new_timer(hour)
		if !timer.stop() {
			failures++
		}
		wait_for_timer_worker(timer.done_ch)
		assert !timer.stop()
	}
	assert failures == 0, '${failures} active timers reported that stop failed'
}

fn test_timer_reset_stopped_reports_inactive() {
	mut timer := new_timer(hour)
	timer.stop()
	wait_for_timer_worker(timer.done_ch)
	was_active := timer.reset(hour)
	timer.stop()
	assert !was_active
}

fn test_timer_reset_expired_reports_inactive() {
	mut timer := new_timer(0)
	select {
		_ := <-timer.c {
		}
		timer_lifecycle_timeout {
			assert false, 'timer did not fire'
		}
	}
	wait_for_timer_worker(timer.done_ch)
	was_active := timer.reset(hour)
	timer.stop()
	assert !was_active
}

fn test_timer_reset_discards_old_event() {
	mut timer := new_timer(0)
	wait_for_timer_worker(timer.done_ch)
	timer.reset(hour)
	mut stale := false
	select {
		_ := <-timer.c {
			stale = true
		}
		else {
		}
	}
	timer.stop()
	assert !stale, 'reset left the previous expiration in the channel'
}

fn test_timer_stop_discards_old_event() {
	mut timer := new_timer(0)
	wait_for_timer_worker(timer.done_ch)
	timer.stop()
	select {
		_ := <-timer.c {
			assert false, 'stop left the previous expiration in the channel'
		}
		else {
		}
	}
}

fn test_timer_reset_releases_previous_worker() {
	mut timer := new_timer(hour)
	previous_done := timer.done_ch
	assert timer.reset(0)
	wait_for_timer_worker(previous_done)
	select {
		_ := <-timer.c {
		}
		timer_lifecycle_timeout {
			assert false, 'reset timer did not fire'
		}
	}
	assert !timer.stop()
}

fn test_timer_repeated_stop_and_reset() {
	mut timer := new_timer(hour)
	assert timer.stop()
	for _ in 0 .. 32 {
		assert !timer.reset(hour)
		assert timer.stop()
		assert timer.done_ch.closed
		assert !timer.stop()
	}
}

fn stop_timer_for_test(mut timer Timer, results chan bool) {
	results <- timer.stop()
}

fn test_timer_concurrent_stop_reports_one_success() {
	mut timer := new_timer(hour)
	results := chan bool{cap: 8}
	for _ in 0 .. 8 {
		spawn stop_timer_for_test(mut timer, results)
	}
	mut successes := 0
	for _ in 0 .. 8 {
		select {
			stopped := <-results {
				if stopped {
					successes++
				}
			}
			timer_lifecycle_timeout {
				assert false, 'concurrent stop blocked'
			}
		}
	}
	assert successes == 1
	assert timer.done_ch.closed
}

fn reset_and_stop_timer_for_test(mut timer Timer, done chan bool) {
	for _ in 0 .. 16 {
		timer.reset(hour)
		timer.stop()
	}
	done <- true
}

fn test_timer_concurrent_reset_and_stop() {
	mut timer := new_timer(hour)
	done := chan bool{cap: 4}
	for _ in 0 .. 4 {
		spawn reset_and_stop_timer_for_test(mut timer, done)
	}
	for _ in 0 .. 4 {
		select {
			_ := <-done {
			}
			timer_lifecycle_timeout {
				assert false, 'concurrent reset and stop blocked'
			}
		}
	}
	timer.stop()
	assert timer.done_ch.closed
	assert timer.c.len == 0
}

fn test_timer_nonpositive_durations_fire() {
	for d in [Duration(0), -millisecond, Duration(i64(-9223372036854775807) - 1)] {
		mut timer := new_timer(d)
		select {
			_ := <-timer.c {
			}
			timer_lifecycle_timeout {
				assert false, 'nonpositive timer did not fire'
			}
		}
		assert !timer.stop()
	}
}

fn test_timer_does_not_fire_before_its_duration() {
	duration := 20 * millisecond
	started := sys_mono_now()
	mut timer := new_timer(duration)
	defer { timer.stop() }
	select {
		_ := <-timer.c {
			assert sys_mono_now() - started >= u64(duration)
		}
		timer_lifecycle_timeout {
			assert false, 'timer did not fire'
		}
	}
}
