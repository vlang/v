module time

fn test_ticker_fires() {
	t := new_ticker(10 * time.millisecond)
	defer { t.stop() }
	select {
		_ := <-t.c {
			assert true
		}
		else {
			time.sleep(100 * time.millisecond)
			select {
				_ := <-t.c {
					assert true
				}
				else {
					assert false
				}
			}
		}
	}
}

fn test_ticker_stop() {
	t := new_ticker(10 * time.millisecond)
	t.stop()
	// give goroutine time to exit
	time.sleep(50 * time.millisecond)
	assert t.done
}
