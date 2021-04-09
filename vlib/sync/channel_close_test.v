import time

fn do_rec(ch chan int, resch chan i64) {
	mut sum := i64(0)
	for {
		a := <-ch or {
			break
		}
		sum += a
	}
	assert ch.closed == true
	println(sum)
	resch <- sum
}

fn do_send(ch chan int) {
	for i in 0 .. 8000 {
		ch <- i
	}
	assert ch.closed == false
	ch.close()
	assert ch.closed == true
}

fn test_channel_close_buffered_multi() {
	ch := chan int{cap: 10}
	resch := chan i64{}
	go do_rec(ch, resch)
	go do_rec(ch, resch)
	go do_rec(ch, resch)
	go do_rec(ch, resch)
	go do_send(ch)
	mut sum := i64(0)
	for _ in 0 .. 4 {
		sum += <- resch
	}
	assert sum == i64(8000) * (8000 - 1) / 2
}

fn test_channel_close_unbuffered_multi() {
	ch := chan int{}
	resch := chan i64{}
	go do_rec(ch, resch)
	go do_rec(ch, resch)
	go do_rec(ch, resch)
	go do_rec(ch, resch)
	go do_send(ch)
	mut sum := i64(0)
	for _ in 0 .. 4 {
		sum += <-resch
	}
	assert sum == i64(8000) * (8000 - 1) / 2
}

fn test_channel_close_buffered() {
	ch := chan int{cap: 100}
	resch := chan i64{}
	go do_rec(ch, resch)
	go do_send(ch)
	mut sum := i64(0)
	sum += <-resch
	assert sum == i64(8000) * (8000 - 1) / 2
}

fn test_channel_close_unbuffered() {
	ch := chan int{}
	resch := chan i64{cap: 100}
	go do_rec(ch, resch)
	go do_send(ch)
	mut sum := i64(0)
	sum += <-resch
	assert sum == i64(8000) * (8000 - 1) / 2
}

fn test_channel_send_close_buffered() {
	ch := chan int{cap: 1}
	go fn (ch chan int) {
		ch <- 31
		ch <- 17 or {}
	}(ch)
	time.sleep(100 * time.millisecond)
	ch.close()
	mut r := <-ch
	r = <-ch or { 23 }
	assert r == 23
}

fn test_channel_send_close_unbuffered() {
	time.sleep(1 * time.second)
	ch := chan int{}
	go fn (ch chan int) {
		ch <- 177 or {}
	}(ch)
	time.sleep(100 * time.millisecond)
	ch.close()
	r := <-ch or { 238 }
	assert r == 238
}
