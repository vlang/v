// vtest flaky: true
// vtest retry: 15

// This test case runs concurrent 3 instances of `do_select` that
// communicate with 6 other threads doing send and receive operations.
// There are buffered and unbuffered channels - handled by one or two
// concurrend threads on the other side

fn do_select(ch1 chan int, ch2 chan int, chf1 chan f64, chf2 chan f64, sumch1 chan i64, sumch2 chan i64) {
	mut sum1 := i64(0)
	mut sum2 := i64(0)
	f1 := 17.0
	f2 := 7.0
	for _ in 0 .. 20000 + chf1.cap / 3 {
		select {
			chf1 <- f1 {}
			i := <-ch1 {
				sum1 += i
			}
			j := <-ch2 {
				sum2 += j
			}
			chf2 <- f2 {}
		}
	}
	sumch1 <- sum1
	sumch2 <- sum2
}

fn do_send_int(ch chan int, factor int) {
	for i in 0 .. 10000 {
		ch <- (i * factor)
	}
}

fn do_rec_f64(ch chan f64, sumch chan f64) {
	mut sum := 0.0
	for _ in 0 .. 10000 {
		sum += <-ch
	}
	sumch <- sum
}

fn test_select() {
	ch1 := chan int{cap: 3}
	ch2 := chan int{}
	// buffer length of chf1 mus be mutiple of 3 (# select threads)
	chf1 := chan f64{cap: 30}
	chf2 := chan f64{}
	chsum1 := chan i64{}
	chsum2 := chan i64{}
	chsumf1 := chan f64{}
	chsumf2 := chan f64{}
	go do_send_int(ch1, 3)
	go do_select(ch1, ch2, chf1, chf2, chsum1, chsum2)
	go do_rec_f64(chf1, chsumf1)
	go do_rec_f64(chf2, chsumf2)
	go do_rec_f64(chf2, chsumf2)
	go do_select(ch1, ch2, chf1, chf2, chsum1, chsum2)
	go do_send_int(ch2, 7)
	go do_send_int(ch2, 17)
	go do_select(ch1, ch2, chf1, chf2, chsum1, chsum2)

	sum1 := <-chsum1 + <-chsum1 + <-chsum1
	sum2 := <-chsum2 + <-chsum2 + <-chsum2
	mut sumf1 := <-chsumf1
	// empty channel buffer
	for _ in 0 .. chf1.cap {
		sumf1 += <-chf1
	}
	sumf2 := <-chsumf2 + <-chsumf2
	// Use Gauß' formula
	expected_sum := i64(10000) * (10000 - 1) / 2
	assert sum1 == 3 * expected_sum
	assert sum2 == (7 + 17) * expected_sum
	assert sumf1 == 17.0 * f64(10000 + chf1.cap)
	assert sumf2 == 7.0 * 20000
}
