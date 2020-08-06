// Channel Benchmark
//
// `nobj` integers are sent thru a channel with queue length`buflen`
// using `nsend` sender threads and `nrec` receiver threads.
//
// The receive threads add all received numbers and send them to the
// main thread where the total sum is compare to the expected value.

import sync
import time
import os

fn do_rec(mut ch sync.Channel, mut resch sync.Channel, n int) {
	mut sum := i64(0)
	for _ in 0 .. n {
		mut a := 0
		ch.pop(&a)
		sum += a
	}
	println(sum)
	resch.push(&sum)
}

fn do_send(mut ch sync.Channel, start, end int) {
	for i in start .. end {
		ch.push(&i)
	}
}

fn main() {
	if os.args.len != 5 {
		eprintln('usage:\n\t${os.args[0]} <nsend> <nrec> <buflen> <nobj>')
		exit(1)
	}
	nsend := os.args[1].int()
	nrec := os.args[2].int()
	buflen := os.args[3].int()
	nobj := os.args[4].int()
	stopwatch := time.new_stopwatch({})
	mut ch := sync.new_channel<int>(buflen)
	mut resch := sync.new_channel<i64>(0)
	mut no := nobj
	for i in 0 .. nrec {
		n := no / (nrec - i)
		go do_rec(mut ch, mut resch, n)
		no -= n
	}
	assert no == 0
	no = nobj
	for i in 0 .. nsend {
		n := no / (nsend - i)
		end := no
		no -= n
		go do_send(mut ch, no, end)
	}
	assert no == 0
	mut sum := i64(0)
	for _ in 0 .. nrec {
		mut r := i64(0)
		resch.pop(&r)
		sum += r
	}
	elapsed := stopwatch.elapsed()
	rate := f64(nobj)/elapsed*time.microsecond
	println('$nobj objects in ${f64(elapsed)/time.second} s (${rate:.2f} objs/µs)')
	// use sum formula by Gauß to calculate the expected result
	expected_sum := i64(nobj)*(nobj-1)/2
	println('got: $sum, expected: $expected_sum')
	assert sum == expected_sum
}
