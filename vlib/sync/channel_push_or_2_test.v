const n = 1000

fn f(ch chan int) {
	mut s := 0
	for _ in 0 .. n {
		s += <-ch
	}
	assert s == n * (n + 1) / 2
	ch.close()
}

fn do_send(ch chan int, val int) ?int {
	ch <- val ?
	return val + 1
}

fn test_push_propargate() {
	ch := chan int{}
	go f(ch)
	mut s := 1
	for {
		s = do_send(ch, s) or {
			break
		}
	}
	assert s == n + 1
}
