// Goroutine benchmark: measures scheduling and channel throughput
// of V's goroutine runtime (translated from Go's GMP scheduler).
//
// Compile: v -enable-globals -cc gcc -gc none -prod run goroutine_benchmark.v
// For comparison, see goroutine_benchmark.go.
//
// Note: Under the v1 compiler, the goroutines module uses pthreads-based
// sync.Mutex which has limitations with ucontext-based context switching.
// The v2 compiler will use a native spinlock, enabling higher scale.
module main

import goroutines
import sync
import time

__global bench_sem = sync.Semaphore{}
__global bench_n = int(0)
__global bench_chan1 = &goroutines.Chan(unsafe { nil })
__global bench_chan2 = &goroutines.Chan(unsafe { nil })
__global bench_msgs_per = int(0)

// --- fan-out/fan-in ---

fn fan_worker(arg voidptr) {
	c := unsafe { &goroutines.Chan(arg) }
	mut val := 1
	goroutines.chan_send(c, voidptr(&val), true)
}

fn fan_collector(arg voidptr) {
	c := unsafe { &goroutines.Chan(arg) }
	mut recv_val := 0
	for _ in 0 .. bench_n {
		goroutines.chan_recv(c, voidptr(&recv_val), true)
	}
	bench_sem.post()
}

fn bench_fan_out_fan_in(n int) {
	bench_n = n
	c := goroutines.chan_make(int(sizeof(int)), 0)

	sw := time.new_stopwatch()

	goroutines.goroutine_create(voidptr(&fan_collector), voidptr(c), 0)
	for _ in 0 .. n {
		goroutines.goroutine_create(voidptr(&fan_worker), voidptr(c), 0)
	}

	bench_sem.wait()

	elapsed := sw.elapsed()
	us := elapsed.microseconds()
	ns_per := elapsed.nanoseconds() / n
	C.printf(c'fan-out/fan-in  %d goroutines: %lld us  (%lld ns/goroutine)\n', n, us, ns_per)
}

// --- ping-pong ---

fn ping_side(arg voidptr) {
	mut val := 0
	for _ in 0 .. bench_n {
		goroutines.chan_send(bench_chan1, voidptr(&val), true)
		goroutines.chan_recv(bench_chan2, voidptr(&val), true)
	}
	bench_sem.post()
}

fn pong_side(arg voidptr) {
	mut val := 0
	for _ in 0 .. bench_n {
		goroutines.chan_recv(bench_chan1, voidptr(&val), true)
		val++
		goroutines.chan_send(bench_chan2, voidptr(&val), true)
	}
}

fn bench_ping_pong(n int) {
	bench_n = n
	bench_chan1 = goroutines.chan_make(int(sizeof(int)), 1)
	bench_chan2 = goroutines.chan_make(int(sizeof(int)), 1)

	sw := time.new_stopwatch()

	goroutines.goroutine_create(voidptr(&pong_side), unsafe { nil }, 0)
	goroutines.goroutine_create(voidptr(&ping_side), unsafe { nil }, 0)

	bench_sem.wait()

	elapsed := sw.elapsed()
	us := elapsed.microseconds()
	ns_per := elapsed.nanoseconds() / n
	C.printf(c'ping-pong       %d round-trips: %lld us  (%lld ns/round-trip)\n', n, us, ns_per)
}

// --- contended channel ---

fn producer(arg voidptr) {
	c := unsafe { &goroutines.Chan(arg) }
	mut val := 0
	for _ in 0 .. bench_msgs_per {
		val++
		goroutines.chan_send(c, voidptr(&val), true)
	}
}

fn consumer_fn(arg voidptr) {
	c := unsafe { &goroutines.Chan(arg) }
	mut recv_val := 0
	for _ in 0 .. bench_n {
		goroutines.chan_recv(c, voidptr(&recv_val), true)
	}
	bench_sem.post()
}

fn bench_contended_channel(num_producers int, msgs_per_producer int) {
	total := num_producers * msgs_per_producer
	bench_n = total
	bench_msgs_per = msgs_per_producer
	c := goroutines.chan_make(int(sizeof(int)), 64)

	sw := time.new_stopwatch()

	goroutines.goroutine_create(voidptr(&consumer_fn), voidptr(c), 0)
	for _ in 0 .. num_producers {
		goroutines.goroutine_create(voidptr(&producer), voidptr(c), 0)
	}

	bench_sem.wait()

	elapsed := sw.elapsed()
	us := elapsed.microseconds()
	ns_per := elapsed.nanoseconds() / total
	C.printf(c'contended chan  %d producers x %d msgs: %lld us  (%lld ns/msg)\n', num_producers,
		msgs_per_producer, us, ns_per)
}

fn main() {
	C.printf(c'=== V Goroutine Benchmark ===\n\n')

	bench_fan_out_fan_in(10)
	bench_fan_out_fan_in(50)
	bench_fan_out_fan_in(100)
	bench_fan_out_fan_in(500)

	C.printf(c'\n')
	bench_ping_pong(1000)
	bench_ping_pong(10000)
	bench_ping_pong(100000)

	C.printf(c'\n')
	bench_contended_channel(4, 1000)
	bench_contended_channel(10, 1000)

	C.printf(c'\n')
	goroutines.shutdown()
}
