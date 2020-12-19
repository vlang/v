fn do_rec(chi chan int, chf chan f64, sumchi chan i64, sumchf chan f64) {
	mut sum := i64(0)
	mut sumf := 0.0
	for _ in 0 .. 20000 {
		select {
			i := <-chi {
				sum += i
			}
			f := <-chf {
				sumf += f
			}
		}
	}
	sumchi <- sum
	sumchf <- sumf
}

fn do_send_int(ch chan int) {
	for i in 0 .. 10000 {
		ch <- i
	}
}

fn do_send_f64(ch chan f64) {
	for i in 0 .. 10000 {
		ch <- f64(i)
	}
}

fn test_select() {
	chi := chan int{cap: 0}
	chf := chan f64{}
	chsumi := chan i64{}
	chsumf := chan f64{}
	go do_send_int(chi)
	go do_send_f64(chf)
	go do_rec(chi, chf, chsumi, chsumf)
	go do_send_int(chi)
	go do_send_f64(chf)
	go do_rec(chi, chf, chsumi, chsumf)
	mut sumi := <-chsumi
	sumi += <-chsumi
	mut sumf := <-chsumf
	sumf += <-chsumf
	// Use Gauß' formula
	expected_sumi :=  2 * i64(10000) * (10000 - 1) / 2
	expected_sumf :=  2 * f64(10000) * (10000 - 1) / 2
	assert sumi == expected_sumi
	assert sumf == expected_sumf
}
