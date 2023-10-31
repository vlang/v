const (
	num_iterations = 10000
)

fn do_send(ch chan int) {
	for i in 0 .. num_iterations {
		ch <- i
	}
}

fn test_channel_unbuffered() {
	ch := chan int{}
	spawn do_send(ch)
	mut sum := i64(0)
	for _ in 0 .. num_iterations {
		sum += <-ch
	}
	assert sum == u64(num_iterations) * (num_iterations - 1) / 2
}
