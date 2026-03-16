// V spawn (OS threads) benchmark – for comparison with Go goroutines.
//
// Uses V's built-in `spawn` (OS threads) and `chan` (V1 channels).
// This measures V's current threading primitives vs Go's goroutines.
//
// Run: v -prod run spawn_benchmark.v
module main

import time

// ---------------------------------------------------------------------------
// Benchmark 1 – fan-out / fan-in
// ---------------------------------------------------------------------------

fn fan_worker(c chan int) {
	c <- 1
}

fn bench_fan_out_fan_in(n int) {
	c := chan int{cap: n}

	sw := time.new_stopwatch()

	for _ in 0 .. n {
		spawn fan_worker(c)
	}

	for _ in 0 .. n {
		_ = <-c
	}

	elapsed := sw.elapsed()
	us := elapsed.microseconds()
	ns_per := elapsed.nanoseconds() / n
	println('fan-out/fan-in  ${n} threads: ${us} us  (${ns_per} ns/thread)')
}

// ---------------------------------------------------------------------------
// Benchmark 2 – channel ping-pong
// ---------------------------------------------------------------------------

fn pinger(c1 chan int, c2 chan int, n int) {
	mut val := 0
	for _ in 0 .. n {
		val = <-c1
		val++
		c2 <- val
	}
}

fn bench_ping_pong(n int) {
	c1 := chan int{cap: 1}
	c2 := chan int{cap: 1}

	spawn pinger(c1, c2, n)

	sw := time.new_stopwatch()

	mut val := 0
	for _ in 0 .. n {
		c1 <- val
		val = <-c2
	}

	elapsed := sw.elapsed()
	us := elapsed.microseconds()
	ns_per := elapsed.nanoseconds() / n
	println('ping-pong       ${n} round-trips: ${us} us  (${ns_per} ns/round-trip)')
}

// ---------------------------------------------------------------------------
// Benchmark 3 – contended channel (many producers, one consumer)
// ---------------------------------------------------------------------------

fn producer(c chan int, count int) {
	mut val := 0
	for _ in 0 .. count {
		val++
		c <- val
	}
}

fn bench_contended_channel(num_producers int, msgs_per_producer int) {
	total := num_producers * msgs_per_producer
	c := chan int{cap: 64}

	sw := time.new_stopwatch()

	for _ in 0 .. num_producers {
		spawn producer(c, msgs_per_producer)
	}

	for _ in 0 .. total {
		_ = <-c
	}

	elapsed := sw.elapsed()
	us := elapsed.microseconds()
	ns_per := elapsed.nanoseconds() / total
	println('contended chan  ${num_producers} producers x ${msgs_per_producer} msgs: ${us} us  (${ns_per} ns/msg)')
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

fn main() {
	println('=== V spawn (OS threads) Benchmark ===')
	println('')

	bench_fan_out_fan_in(1000)
	bench_fan_out_fan_in(10000)
	// Skip 100k - too many OS threads

	println('')
	bench_ping_pong(10000)
	bench_ping_pong(100000)

	println('')
	bench_contended_channel(10, 1000)
	bench_contended_channel(100, 1000)

	println('')
}
