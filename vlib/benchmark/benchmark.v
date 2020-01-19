module benchmark

import time
import term

/*
Example usage of this module:
```
import benchmark
mut bmark := benchmark.new_benchmark()
// by default the benchmark will be verbose, i.e. it will include timing information
// if you want it to be silent, set bmark.verbose = false
for {
   bmark.step() // call this when you want to advance the benchmark.
                // The timing info in bmark.step_message will be measured starting from the last call to bmark.step
   ....

   //bmark.fail() // call this if the step failed
   //bmark.step_message(('failed')

   bmark.ok() // call this when the step succeeded
   println( bmark.step_message('ok')
}
bmark.stop() // call when you want to finalize the benchmark
println( bmark.total_message('remarks about the benchmark') )
```
*/

const (
	BOK = term.ok_message('OK')
	BFAIL = term.fail_message('FAIL')
)

pub struct ConcurrentBenchmark {
pub mut:
	bench_start_time i64
	bench_end_time   i64
	ntotal           int
	nok              int
	nfail            int
	verbose          bool
	nexpected_steps  int
	bok              string
	bfail            string
}

pub struct ConcurrentInstance {
pub mut:
	benchmark       &ConcurrentBenchmark
	step_start_time i64
	step_end_time   i64
	step_number     int
}

pub fn new_concurrent_benchmark() ConcurrentBenchmark {
	return ConcurrentBenchmark{
		bench_start_time: benchmark.now()
		verbose: true
	}
}

pub fn (b mut ConcurrentBenchmark) set_total_expected_steps(n int) {
	b.nexpected_steps = n
}

pub fn (b mut ConcurrentBenchmark) stop() {
	b.bench_end_time = benchmark.now()
}

pub fn (b &ConcurrentBenchmark) step() ConcurrentInstance {
	return ConcurrentInstance {
		benchmark: b
		step_start_time: benchmark.now()
	}
}

pub fn (b mut ConcurrentInstance) fail() {
	b.step_end_time = benchmark.now()
	b.benchmark.ntotal++
	b.benchmark.nfail++
	b.step_number = b.benchmark.ntotal
}

pub fn (b mut ConcurrentInstance) ok() {
	b.step_end_time = benchmark.now()
	b.benchmark.ntotal++
	b.benchmark.nok++
	b.step_number = b.benchmark.ntotal
}

pub fn (b mut ConcurrentInstance) fail_many(n int) {
	b.step_end_time = benchmark.now()
	b.benchmark.ntotal += n
	b.benchmark.nfail += n
	b.step_number = b.benchmark.ntotal
}

pub fn (b mut ConcurrentInstance) ok_many(n int) {
	b.step_end_time = benchmark.now()
	b.benchmark.ntotal += n
	b.benchmark.nok += n
	b.step_number = b.benchmark.ntotal
}

pub fn (b mut ConcurrentInstance) neither_fail_nor_ok() {
	b.step_end_time = benchmark.now()
	b.benchmark.ntotal++
	b.step_number = b.benchmark.ntotal
}

pub fn (b &ConcurrentInstance) step_message_with_label(label string, msg string) string {
	mut timed_line := ''
	expected_steps := b.benchmark.nexpected_steps

	if expected_steps > 0 {
		mut sprogress := ''
		if expected_steps < 10 {
			sprogress = '${b.step_number:1d}/${expected_steps:1d}'
		}
		if expected_steps >= 10 && expected_steps < 100 {
			sprogress = '${b.step_number:2d}/${expected_steps:2d}'
		}
		if expected_steps >= 100 && expected_steps < 1000 {
			sprogress = '${b.step_number:3d}/${expected_steps:3d}'
		}
		timed_line = b.benchmark.tdiff_in_ms('[${sprogress}] $msg', b.step_start_time, b.step_end_time)
	}
	else {
		timed_line = b.benchmark.tdiff_in_ms(msg, b.step_start_time, b.step_end_time)
	}
	return '${label:-5s}${timed_line}'
}

pub fn (b &ConcurrentInstance) step_message(msg string) string {
	return b.step_message_with_label('', msg)
}

pub fn (b &ConcurrentInstance) step_message_ok(msg string) string {
	return b.step_message_with_label(BOK, msg)
}

pub fn (b &ConcurrentInstance) step_message_fail(msg string) string {
	return b.step_message_with_label(BFAIL, msg)
}

pub fn (b &ConcurrentBenchmark) total_message(msg string) string {
	mut tmsg := '${msg}\n                 ok, fail, total = ' + term.ok_message('${b.nok:5d}') + ', ' + if b.nfail > 0 { term.fail_message('${b.nfail:5d}') } else { '${b.nfail:5d}' } + ', ' + '${b.ntotal:5d}'
	if b.verbose {
		tmsg = '<=== total time spent $tmsg'
	}
	return '  ' + b.tdiff_in_ms(tmsg, b.bench_start_time, b.bench_end_time)
}

pub fn (b &ConcurrentBenchmark) total_duration() i64 {
	return (b.bench_end_time - b.bench_start_time)
}

// //////////////////////////////////////////////////////////////////
fn (b &ConcurrentBenchmark) tdiff_in_ms(s string, sticks i64, eticks i64) string {
	if b.verbose {
		tdiff := (eticks - sticks)
		return '${tdiff:6lld} ms $s'
	}
	return s
}

fn now() i64 {
	return time.ticks()
}
