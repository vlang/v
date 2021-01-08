fn do_rec_i64(ch chan i64, sumch chan i64) {
	mut sum := i64(0)
	for _ in 0 .. 30000 {
		sum += <-ch
	}
	sumch <- sum
}

fn do_send_int(ch chan int) {
	for i in 0 .. 30000 {
		ch <- i
	}
}

fn test_select() {
	chi := chan int{cap: 10}
	recch := chan i64{cap: 10}
	chsum := chan i64{}
	go do_rec_i64(recch, chsum)
	go do_send_int(chi)
	mut sum := i64(0)
	mut sl := i64(0)
	for _ in 0 .. 60000 + recch.cap {
		select {
			ri := <-chi {
				sum += ri
			}
			recch <- sl {
				sl++
			}
		}
	}
	// Use Gauß' formula
	expected_sum :=  i64(30000) * (30000 - 1) / 2
	assert sum == expected_sum

	mut sumrec := <- chsum
	// Empty receive buffer
	for _ in 0 .. recch.cap {
		sumrec += <- recch
	}
	assert sumrec == i64(30000 + recch.cap) * (30000 + recch.cap - 1) / 2
}
