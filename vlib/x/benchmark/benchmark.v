module benchmark

import time
import math
import sync

// Benchmark represent all significant data for benchmarking. Provide clear way for getting result in convinient way by exported methods
@[noinit]
pub struct Benchmark {
pub mut:
	n                i64 // Number of iterations. Set explicitly or computed from expected time of benchmarking
	bench_func       fn () ! @[required] // function for benchmarking
	bench_time       time.Duration   // benchmark duration
	is_parallel      bool            // if true every bench_func run in separate coroutine
	benchmark_result BenchmarkResult // accumulator of benchmark metrics
	timer_on         bool            // inner flag of time recording
	start_time       time.Time       // start timestamp of timer
	duration         time.Duration   // expected time of benchmark process
	failed           bool            // flag of bench_func failure. true if one of bench_func run failed
	start_memory     usize           // memory status on start benchmark
	start_allocs     usize           // size of object allocated on heap
}

// BenchmarkDefaults is params struct for providing parameters of benchmarking to setup function
// - n - number of iterations. set if you know how many runs of function you need. if you don't know how many you need - set 0
// - duration - by default 1s. expecting duration of all benchmark runs. doesn't work if is_parallel == true
// - is_parallel - if true, every bench_func run in separate coroutine
@[params]
pub struct BenchmarkDefaults {
pub:
	duration    time.Duration = time.second
	is_parallel bool
	n           i64
}

// Benchmark.new - constructor of benchmark
// arguments:
// - bench_func - function to benchmark. required, if you have no function - you don't need benchmark
// - params - structure of benchmark parameters
pub fn setup(bench_func fn () !, params BenchmarkDefaults) !Benchmark {
	if bench_func == unsafe { nil } {
		return error('Benchmark function cannot be `nil`')
	}

	if params.duration > 0 && params.is_parallel {
		return error('can not predict number of parallel iterations')
	}

	return Benchmark{
		n:           params.n
		bench_func:  bench_func
		bench_time:  params.duration
		is_parallel: params.is_parallel
	}
}

// run_benchmark - function for start benchmarking
// run benchmark n times, or duration time
pub fn (mut b Benchmark) run() {
	// run bench_func one time for heat up processor cache and get elapsed time for n prediction
	b.run_n(1)

	// if one iteration failed no need to do more
	if b.failed {
		b.n = 1
		// show failed result. bad result is steel result
		b.benchmark_result.print()
	}

	// if n is provided we should run exactly n times. but 1 time we already run
	if b.n > 1 {
		b.run_n(b.n - 1)
	}

	// if n is zero then we should run bench_func enough time for estimate duration time of execution
	if b.n == 0 {
		b.n = 1
		// if one of runs failed - bench_func is not valid
		// but 1e9 times of evaluation is too much
		// so we need to repeat prediction-execition process while elapsed time less then expected time
		for !b.failed && b.duration < b.bench_time && b.n < 1000000000 {
			// we need predict new amount of executions to estimate expected time
			n := b.predict_n()

			// later we predict how many runs we need yet. so we run predicted times
			b.run_n(n)
			b.n += n
		}
	}

	// if n is provided, duration will be calculated. otherwise n will
	b.benchmark_result.n = b.n
	b.benchmark_result.t = b.duration

	// despite of the way of usage of benchmark result(send py api, send to chat, process, logging, etc), we print it
	b.benchmark_result.print()
}

// run_n - run bench_func n times
fn (mut b Benchmark) run_n(n i64) {
	// clear memory for avoid GC influence
	gc_collect()

	// reset and start timer for get elapsed time
	b.reset_timer()
	b.start_timer()

	// unwrap function from struct field
	mut f := b.bench_func

	if !b.is_parallel {
		// run n times consistently
		for i := i64(0); i < n; i++ {
			f() or {
				// if one execution failed print err, set failed flag and stop execution
				b.failed = true
				// workaround for consider unsuccesful runs
				b.n -= n - i
				eprintln('Error: ${err}')
				return
			}
		}
	}

	// spawn n coroutines, wait end of spawning and unpause all coroutines
	if b.is_parallel {
		// WaitGroup for spawn and pause enough coroutines
		mut spawnwg := sync.new_waitgroup()
		spawnwg.add(int(n))
		// WaitGroup for wait of end of execution
		mut workwg := sync.new_waitgroup()
		workwg.add(int(n))

		for i := i64(0); i < n; i++ {
			spawn run_in_one_time(mut workwg, mut spawnwg, f)
			spawnwg.done()
		}
		workwg.wait()
	}

	// stop timer and collect data
	b.stop_timer()
}

fn run_in_one_time(mut workwg sync.WaitGroup, mut spawnwg sync.WaitGroup, f fn () !) {
	defer {
		workwg.done()
	}
	spawnwg.wait()
	f() or { return } // TODO: add error handling
}

// predict_n - predict number of executions to estimate duration
// based on previous values
fn (mut b Benchmark) predict_n() i64 {
	// goal duration in nanoseconds
	mut goal_ns := b.bench_time.nanoseconds()
	// get number of previous iterations
	prev_iters := b.n
	// get elapsed time in nanoseconds
	mut prev_ns := b.duration.nanoseconds()

	// to avoid division by zero
	if prev_ns <= 0 {
		prev_ns = 1
	}

	// multiple first to avoid division with less then 0 result
	mut n := goal_ns * prev_iters
	n = n / prev_ns
	// grow at least in 1.2
	n += n / 5

	// to not grow to fast
	n = math.min(n, 100 * b.n)
	// to grow at least on 1
	n = math.max(n, b.n + 1)
	// to avoid run more then 1e9 times
	n = math.min(n, 1000000000)

	return n
}

// reset_timer - clear timer and reset memory start data
fn (mut b Benchmark) reset_timer() {
	// if timer_on we should restart it
	if b.timer_on {
		b.start_time = time.now()
		b.start_memory = gc_memory_use()
		b.start_allocs = gc_heap_usage().bytes_since_gc
	}
}

// starttimer - register start measures of memory
fn (mut b Benchmark) start_timer() {
	// you do not need to start timer that already started
	if !b.timer_on {
		b.start_time = time.now()
		b.start_memory = gc_memory_use()
		b.start_allocs = gc_heap_usage().bytes_since_gc
		b.timer_on = true
	}
}

// stop_timer - accumulate menchmark data
fn (mut b Benchmark) stop_timer() {
	if b.timer_on {
		// accumulate delta time of execution
		b.duration += time.since(b.start_time)
		// accumulate memory growth
		b.benchmark_result.mem += gc_memory_use() - b.start_memory
		// accumulate heap usage
		b.benchmark_result.allocs += gc_heap_usage().bytes_since_gc - b.start_allocs
		b.timer_on = false
	}
}

// BenchmarkResult - struct for represent result of benchmark
struct BenchmarkResult {
pub mut:
	n      i64           // iterations count
	t      time.Duration // elapsed time
	mem    usize         // all allocated memory
	allocs usize         // heap allocated memory
}

// ns_per_op - elapsed time in nanoseconds per iteration
fn (r BenchmarkResult) ns_per_op() i64 {
	if r.n <= 0 {
		return 0
	}
	return r.t.nanoseconds() / i64(r.n)
}

// allocs_per_op - heap usage per iteration
fn (r BenchmarkResult) allocs_per_op() i64 {
	if r.n <= 0 {
		return 0
	}
	return i64(r.allocs) / i64(r.n)
}

// alloced_bytes_per_op - memory usage per iteration
fn (r BenchmarkResult) alloced_bytes_per_op() i64 {
	if r.n <= 0 {
		return 0
	}
	return i64(r.mem) / i64(r.n)
}

// print - all measurements
fn (r BenchmarkResult) print() {
	println('Iterations: ${r.n:10}\t\tTotal Duration: ${r.t:10}\tns/op: ${r.ns_per_op():10}\tB/op: ${r.alloced_bytes_per_op():6}\tallocs/op: ${r.allocs_per_op():6}')
}
