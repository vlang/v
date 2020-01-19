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

pub struct Benchmark {
pub mut:
	bench_start_time i64
	bench_end_time   i64
	step_start_time  i64
	step_end_time    i64
	ntotal           int
	nok              int
	nfail            int
	verbose          bool
	nexpected_steps  int
	cstep            int
	bok              string
	bfail            string
}

pub fn new_benchmark() Benchmark {
	return Benchmark{
		bench_start_time: benchmark.now()
		verbose: true
	}
}

pub fn (b mut Benchmark) set_total_expected_steps(n int) {
	b.nexpected_steps = n
}

pub fn (b mut Benchmark) stop() {
	b.bench_end_time = benchmark.now()
}

pub fn (b mut Benchmark) step() {
	b.step_start_time = benchmark.now()
	b.cstep++
}

pub fn (b mut Benchmark) fail() {
	b.step_end_time = benchmark.now()
	b.ntotal++
	b.nfail++
}

pub fn (b mut Benchmark) ok() {
	b.step_end_time = benchmark.now()
	b.ntotal++
	b.nok++
}

pub fn (b mut Benchmark) fail_many(n int) {
	b.step_end_time = benchmark.now()
	b.ntotal += n
	b.nfail += n
}

pub fn (b mut Benchmark) ok_many(n int) {
	b.step_end_time = benchmark.now()
	b.ntotal += n
	b.nok += n
}

pub fn (b mut Benchmark) neither_fail_nor_ok() {
	b.step_end_time = benchmark.now()
}

pub fn (b &Benchmark) step_message_with_label(label string, msg string) string {
	mut timed_line := ''
	if b.nexpected_steps > 0 {
		mut sprogress := ''
		if b.nexpected_steps < 10 {
			sprogress = '${b.cstep:1d}/${b.nexpected_steps:1d}'
		}
		if b.nexpected_steps >= 10 && b.nexpected_steps < 100 {
			sprogress = '${b.cstep:2d}/${b.nexpected_steps:2d}'
		}
		if b.nexpected_steps >= 100 && b.nexpected_steps < 1000 {
			sprogress = '${b.cstep:3d}/${b.nexpected_steps:3d}'
		}
		timed_line = b.tdiff_in_ms('[${sprogress}] $msg', b.step_start_time, b.step_end_time)
	}
	else {
		timed_line = b.tdiff_in_ms(msg, b.step_start_time, b.step_end_time)
	}
	return '${label:-5s}${timed_line}'
}

pub fn (b &Benchmark) step_message(msg string) string {
	return b.step_message_with_label('', msg)
}

pub fn (b &Benchmark) step_message_ok(msg string) string {
	return b.step_message_with_label(BOK, msg)
}

pub fn (b &Benchmark) step_message_fail(msg string) string {
	return b.step_message_with_label(BFAIL, msg)
}

pub fn (b &Benchmark) total_message(msg string) string {
	mut tmsg := '${msg}\n                 ok, fail, total = ' + term.ok_message('${b.nok:5d}') + ', ' + if b.nfail > 0 { term.fail_message('${b.nfail:5d}') } else { '${b.nfail:5d}' } + ', ' + '${b.ntotal:5d}'
	if b.verbose {
		tmsg = '<=== total time spent $tmsg'
	}
	return '  ' + b.tdiff_in_ms(tmsg, b.bench_start_time, b.bench_end_time)
}

pub fn (b &Benchmark) total_duration() i64 {
	return (b.bench_end_time - b.bench_start_time)
}

// //////////////////////////////////////////////////////////////////
fn (b &Benchmark) tdiff_in_ms(s string, sticks i64, eticks i64) string {
	if b.verbose {
		tdiff := (eticks - sticks)
		return '${tdiff:6lld} ms $s'
	}
	return s
}

fn now() i64 {
	return time.ticks()
}
