fn do_rec(ch chan int, resch chan i64) {
	mut sum := i64(0)
	for _ in 0 .. 2000 {
		sum += <-ch
	}
	println(sum)
	resch <- sum
}

fn do_send(ch chan int) {
	for i in 0 .. 2000 {
		ch <- i
	}
}

fn test_channel_multi_buffered() {
	ch := chan int{cap: 100}
	resch := chan i64{}
	spawn do_rec(ch, resch)
	spawn do_rec(ch, resch)
	spawn do_rec(ch, resch)
	spawn do_rec(ch, resch)
	spawn do_send(ch)
	spawn do_send(ch)
	spawn do_send(ch)
	spawn do_send(ch)
	mut sum := i64(0)
	for _ in 0 .. 4 {
		sum += <-resch
	}
	assert sum == i64(4) * 2000 * (2000 - 1) / 2
}
