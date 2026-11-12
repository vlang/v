module time

fn test_ticker_fires() {
	mut ticker := new_ticker(millisecond)
	defer { ticker.stop() }
	for _ in 0 .. 3 {
		select {
			tick := <-ticker.c {
				assert tick.unix() > 0
			}
			5 * second {
				assert false, 'ticker did not fire'
			}
		}
	}
}

fn test_ticker_stop() {
	mut ticker := new_ticker(hour)
	ticker.stop()
	assert ticker.done_ch.closed
	assert !ticker.c.closed
}
