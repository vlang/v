module benchmark

import time

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

struct Benchmark{
pub mut:
	bench_start_time i64
	bench_end_time i64
	step_start_time i64
	step_end_time i64
	ntotal int
	nok    int
	nfail  int
	verbose bool
}

pub fn new_benchmark() Benchmark{
	return Benchmark{
		bench_start_time: benchmark.now()
		verbose: true
	}
}

pub fn (b mut Benchmark) stop() {
	b.bench_end_time = benchmark.now()
}

pub fn (b mut Benchmark) step() {
	b.step_start_time = benchmark.now()
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

pub fn (b mut Benchmark) step_message(msg string) string {
	return b.tdiff_in_ms(msg, b.step_start_time, b.step_end_time)
}

pub fn (b mut Benchmark) total_message(msg string) string {
	mut tmsg := '$msg : ok, fail, total = ${b.nok:5d}, ${b.nfail:5d}, ${b.ntotal:5d}'
	if b.verbose {
		tmsg = '<=== total time spent $tmsg'
	}
	return b.tdiff_in_ms(tmsg, b.bench_start_time, b.bench_end_time)
}

////////////////////////////////////////////////////////////////////

fn (b mut Benchmark) tdiff_in_ms(s string, sticks i64, eticks i64) string {
	if b.verbose {
		tdiff := (eticks - sticks)
		return '${tdiff:6d} ms | $s'
	}
	return s
}

fn now() i64 {
	return time.ticks()
}

