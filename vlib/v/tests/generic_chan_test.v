fn mk_chan<T>(f fn () T) chan T {
	gench := chan T{cap: 1}
	// // This does not work, yet
	// go fn(ch2 chan T, f2 fn() T) {
	//     res := f2()
	//     ch2 <- res
	// }(gench, f)
	return gench
}

fn g(x f64, y f64) f64 {
	return x * x + y * y
}

fn test_generic_chan_return() {
	ch := mk_chan<f64>(fn () f64 {
		return g(3, 4)
	})
	ch <- 13.4
	res := <-ch
	assert res == 13.4
}
