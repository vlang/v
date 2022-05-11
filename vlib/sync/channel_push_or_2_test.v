const n = 1000

fn f(ch chan f64) {
	mut s := 0.0
	for _ in 0 .. n {
		s += <-ch
	}
	assert s == f64(n * (n + 1) / 2)
	ch.close()
}

fn do_send(ch chan f64, val f64) ?f64 {
	ch <- val?
	return val + 1.0
}

fn test_push_propargate() {
	ch := chan f64{}
	go f(ch)
	mut s := 1.0
	for {
		s = do_send(ch, s) or { break }
	}
	assert s == f64(n + 1)
}
