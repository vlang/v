
// Channel Benchmark
//
// `nobj` integers are sent thru a channel with queue length`buflen`
// using `nsend` sender threads and `nrec` receiver threads.
//
// The receive threads add all received numbers and send them to the
// main thread where the total sum is compare to the expected value.
const (
	nsend           = 2
	nrec            = 2
	buflen          = 100
	nobj            = 10000
	objs_per_thread = 5000
)

fn do_rec(ch chan int, resch chan i64, n int) {
	mut sum := i64(0)
	for _ in 0 .. n {
		mut r := 0
		for ch.try_pop(mut r) != .success {
		}
		sum += r
	}
	println(sum)
	resch <- sum
}

fn do_send(ch chan int, start int, end int) {
	for i in start .. end {
		for ch.try_push(i) != .success {
		}
	}
}

fn test_channel_polling() {
	ch := chan int{cap: buflen}
	resch := chan i64{}
	for _ in 0 .. nrec {
		go do_rec(ch, resch, objs_per_thread)
	}
	mut n := nobj
	for _ in 0 .. nsend {
		end := n
		n -= objs_per_thread
		go do_send(ch, n, end)
	}
	mut sum := i64(0)
	for _ in 0 .. nrec {
		sum += <-resch
		println('> running sum: $sum')
	}
	// use sum formula by Gauß to calculate the expected result
	expected_sum := i64(nobj) * (nobj - 1) / 2
	println('expected sum: $expected_sum | sum: $sum')
	assert sum == expected_sum
}
