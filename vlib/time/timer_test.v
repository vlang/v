module time

fn test_timer_fires() {
	t := new_timer(10 * time.millisecond)
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

fn test_timer_stop() {
	t := new_timer(100 * time.millisecond)
	assert t.stop()
	assert t.done
}
