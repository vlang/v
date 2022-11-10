const (
	num_iterations = 10000
)

fn do_send(ch chan int) {
	for i in 0 .. num_iterations {
		ch <- i
	}
}

fn test_channel_buffered() {
	ch := chan int{cap: 1000}
	spawn do_send(ch)
	mut sum := i64(0)
	for _ in 0 .. num_iterations {
		sum += <-ch
	}
	assert sum == u64(num_iterations) * (num_iterations - 1) / 2
}

fn test_builtin_enum() {
	x := ChanState.closed
	assert x == .closed
	println(x)
}
