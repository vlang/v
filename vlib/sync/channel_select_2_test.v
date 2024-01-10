import time

fn do_rec_i64(ch chan i64) {
	mut sum := i64(0)
	for _ in 0 .. 300 {
		sum += <-ch
	}
	assert sum == 300 * (300 - 1) / 2
}

fn do_send_int(ch chan int) {
	for i in 0 .. 300 {
		ch <- i
	}
}

fn do_send_u8(ch chan u8) {
	for i in 0 .. 300 {
		ch <- u8(i)
	}
}

fn do_send_i64(ch chan i64) {
	for i in 0 .. 300 {
		ch <- i
	}
}

fn test_select() {
	chi := chan int{}
	chl := chan i64{cap: 1}
	chb := chan u8{cap: 10}
	recch := chan i64{cap: 0}
	spawn do_rec_i64(recch)
	spawn do_send_int(chi)
	spawn do_send_u8(chb)
	spawn do_send_i64(chl)
	mut sum := i64(0)
	mut rl := i64(0)
	mut sl := i64(0)
	for _ in 0 .. 1200 {
		select {
			ri := <-chi {
				sum += ri
			}
			recch <- sl {
				sl++
			}
			rl = <-chl {
				sum += rl
			}
			rb := <-chb {
				sum += rb
			}
		}
	}
	// Use Gauß' formula for the first 2 contributions
	// the 3rd contribution is `byte` and must be seen modulo 256
	expected_sum := 2 * (300 * (300 - 1) / 2) + 256 * (256 - 1) / 2 + 44 * (44 - 1) / 2
	assert sum == expected_sum
	time.sleep(20 * time.millisecond) // to give assert in coroutine enough time
}
