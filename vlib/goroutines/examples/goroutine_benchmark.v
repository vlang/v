// Goroutine benchmark: measures creation, scheduling, and channel throughput.
//
// Tests:
// 1. Goroutine creation + completion (fan-out/fan-in via channel)
// 2. Channel ping-pong between two goroutines
// 3. Many goroutines contending on a single channel
//
// Requires the v2 compiler: v2 run goroutine_benchmark.v
// For comparison, see goroutine_benchmark.go.
module main

import goroutines
import time

// ---------------------------------------------------------------------------
// Worker functions (must accept voidptr for goroutine_create)
// ---------------------------------------------------------------------------

fn fan_worker(arg voidptr) {
	c := unsafe { &goroutines.Chan(arg) }
	mut val := 1
	goroutines.chan_send(c, voidptr(&val), true)
}

struct PingPongArgs {
	c1 &goroutines.Chan
	c2 &goroutines.Chan
	n  int
}

fn pinger(arg voidptr) {
	a := unsafe { &PingPongArgs(arg) }
	mut val := 0
	for _ in 0 .. a.n {
		goroutines.chan_recv(a.c1, voidptr(&val), true)
		val++
		goroutines.chan_send(a.c2, voidptr(&val), true)
	}
}

struct ProducerArgs {
	c     &goroutines.Chan
	count int
}

fn producer(arg voidptr) {
	a := unsafe { &ProducerArgs(arg) }
	mut val := 0
	for _ in 0 .. a.count {
		val++
		goroutines.chan_send(a.c, voidptr(&val), true)
	}
}

// ---------------------------------------------------------------------------
// Benchmark 1 – fan-out / fan-in
// ---------------------------------------------------------------------------

fn bench_fan_out_fan_in(n int) {
	c := goroutines.chan_make(int(sizeof(int)), n)

	sw := time.new_stopwatch()

	for _ in 0 .. n {
		goroutines.goroutine_create(voidptr(&fan_worker), voidptr(c), 0)
	}

	mut recv_val := 0
	for _ in 0 .. n {
		goroutines.chan_recv(c, voidptr(&recv_val), true)
	}

	elapsed := sw.elapsed()
	us := elapsed.microseconds()
	ns_per := elapsed.nanoseconds() / n
	println('fan-out/fan-in  ${n} goroutines: ${us} us  (${ns_per} ns/goroutine)')
}

// ---------------------------------------------------------------------------
// Benchmark 2 – channel ping-pong
// ---------------------------------------------------------------------------

fn bench_ping_pong(n int) {
	c1 := goroutines.chan_make(int(sizeof(int)), 1)
	c2 := goroutines.chan_make(int(sizeof(int)), 1)

	args := &PingPongArgs{
		c1: c1
		c2: c2
		n:  n
	}
	goroutines.goroutine_create(voidptr(&pinger), voidptr(args), 0)

	sw := time.new_stopwatch()

	mut val := 0
	for _ in 0 .. n {
		goroutines.chan_send(c1, voidptr(&val), true)
		goroutines.chan_recv(c2, voidptr(&val), true)
	}

	elapsed := sw.elapsed()
	us := elapsed.microseconds()
	ns_per := elapsed.nanoseconds() / n
	println('ping-pong       ${n} round-trips: ${us} us  (${ns_per} ns/round-trip)')
}

// ---------------------------------------------------------------------------
// Benchmark 3 – contended channel (many producers, one consumer)
// ---------------------------------------------------------------------------

fn bench_contended_channel(num_producers int, msgs_per_producer int) {
	total := num_producers * msgs_per_producer
	c := goroutines.chan_make(int(sizeof(int)), 64)

	sw := time.new_stopwatch()

	for _ in 0 .. num_producers {
		args := &ProducerArgs{
			c:     c
			count: msgs_per_producer
		}
		goroutines.goroutine_create(voidptr(&producer), voidptr(args), 0)
	}

	mut recv_val := 0
	for _ in 0 .. total {
		goroutines.chan_recv(c, voidptr(&recv_val), true)
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
	println('=== V Goroutine Benchmark ===')
	println('')

	bench_fan_out_fan_in(1000)
	bench_fan_out_fan_in(10000)
	bench_fan_out_fan_in(100000)

	println('')
	bench_ping_pong(10000)
	bench_ping_pong(100000)

	println('')
	bench_contended_channel(10, 1000)
	bench_contended_channel(100, 1000)

	println('')
	goroutines.shutdown()
}
