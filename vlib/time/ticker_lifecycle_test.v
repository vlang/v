module time

const ticker_lifecycle_timeout = 5 * second

fn stop_ticker_twice_for_test(mut ticker Ticker, done chan bool) {
	ticker.stop()
	ticker.stop()
	done <- true
}

fn test_ticker_repeated_stop_does_not_block() {
	mut ticker := new_ticker(hour)
	done := chan bool{cap: 1}
	spawn stop_ticker_twice_for_test(mut ticker, done)
	select {
		_ := <-done {
		}
		ticker_lifecycle_timeout {
			assert false, 'repeated stop blocked until the ticker interval elapsed'
		}
	}
}

fn test_ticker_stop_ends_sleeping_worker() {
	mut ticker := new_ticker(hour)
	ticker.stop()
	assert ticker.done_ch.closed, 'stop returned while its worker was still sleeping'
}

fn test_ticker_stop_with_full_channel() {
	mut ticker := new_ticker(millisecond)
	deadline := sys_mono_now() + u64(ticker_lifecycle_timeout)
	for ticker.c.len == 0 && sys_mono_now() < deadline {
		sleep(millisecond)
	}
	assert ticker.c.len == 1, 'ticker did not produce its first tick'
	ticker.stop()
	select {
		_ := <-ticker.c {
			assert false, 'stop left a stale tick in the channel'
		}
		else {
		}
	}
}

fn stop_ticker_for_test(mut ticker Ticker, done chan bool) {
	ticker.stop()
	done <- true
}

fn test_ticker_concurrent_stop() {
	mut ticker := new_ticker(hour)
	done := chan bool{cap: 8}
	for _ in 0 .. 8 {
		spawn stop_ticker_for_test(mut ticker, done)
	}
	for _ in 0 .. 8 {
		select {
			_ := <-done {
			}
			ticker_lifecycle_timeout {
				assert false, 'concurrent stop blocked'
			}
		}
	}
	assert ticker.done_ch.closed
	assert ticker.c.len == 0
	assert !ticker.c.closed
}

fn test_ticker_stop_after_receiving() {
	mut ticker := new_ticker(millisecond)
	select {
		_ := <-ticker.c {
		}
		ticker_lifecycle_timeout {
			assert false, 'ticker did not fire'
		}
	}
	ticker.stop()
	assert ticker.done_ch.closed
	assert ticker.c.len == 0
	select {
		_ := <-ticker.c {
			assert false, 'stopped ticker sent another tick'
		}
		else {
		}
	}
}

fn test_ticker_does_not_fire_before_first_interval() {
	duration := 20 * millisecond
	started := sys_mono_now()
	mut ticker := new_ticker(duration)
	defer { ticker.stop() }
	select {
		_ := <-ticker.c {
			assert sys_mono_now() - started >= u64(duration)
		}
		ticker_lifecycle_timeout {
			assert false, 'ticker did not fire'
		}
	}
}

fn test_ticker_largest_interval_is_interruptible() {
	mut ticker := new_ticker(Duration(9223372036854775807))
	done := chan bool{cap: 1}
	spawn stop_ticker_for_test(mut ticker, done)
	select {
		_ := <-done {
		}
		ticker_lifecycle_timeout {
			assert false, 'maximum-interval ticker could not be stopped'
		}
	}
	assert ticker.done_ch.closed
}
