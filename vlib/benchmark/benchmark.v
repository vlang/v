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

benchmark.start() and b.measure() are convenience methods,
intended to be used in combination. Their goal is to make
benchmarking of small snippets of code as *short*, easy to
write, and then to read and analyze the results, as possible.
Example:
```v
import benchmark
b := benchmark.start()

// your code 1 ...
b.measure('code_1')

// your code 2 ...
b.measure('code_2')
```
... which will produce on stdout something like this:
SPENT    17 ms in code_1
SPENT   462 ms in code_2
*/


const (
	BOK = term.ok_message('OK  ')
	BFAIL = term.fail_message('FAIL')
	BSKIP  = term.warn_message('SKIP')
	BSPENT = term.ok_message('SPENT')
)

pub struct Benchmark {
pub mut:
	bench_timer      time.Timer
	step_timer       time.Timer
	ntotal           int
	nok              int
	nfail            int
	nskip            int
	verbose          bool
	nexpected_steps  int
	cstep            int
	no_cstep         bool
	bok              string
	bfail            string
}

pub fn new_benchmark() Benchmark {
	return Benchmark{
		bench_timer: time.new_timer()
		verbose: true
	}
}

pub fn new_benchmark_no_cstep() Benchmark {
	return Benchmark{
		bench_timer: time.new_timer()
		verbose: true
		no_cstep: true
	}
}

pub fn new_benchmark_pointer() &Benchmark {
	return &Benchmark{
		bench_timer: time.new_timer()
		verbose: true
	}
}

pub fn (b mut Benchmark) set_total_expected_steps(n int) {
	b.nexpected_steps = n
}

pub fn (b mut Benchmark) stop() {
	b.bench_timer.stop()
}

pub fn (b mut Benchmark) step() {
	b.step_timer.restart()
	if !b.no_cstep {
		b.cstep++
	}
}

pub fn (b mut Benchmark) fail() {
	b.step_timer.stop()
	b.ntotal++
	b.nfail++
}

pub fn (b mut Benchmark) ok() {
	b.step_timer.stop()
	b.ntotal++
	b.nok++
}

pub fn (b mut Benchmark) skip() {
	b.step_timer.stop()
	b.ntotal++
	b.nskip++
}

pub fn (b mut Benchmark) fail_many(n int) {
	b.step_timer.stop()
	b.ntotal += n
	b.nfail += n
}

pub fn (b mut Benchmark) ok_many(n int) {
	b.step_timer.stop()
	b.ntotal += n
	b.nok += n
}

pub fn (b mut Benchmark) neither_fail_nor_ok() {
	b.step_timer.stop()
}

pub fn start() Benchmark {
	mut b := new_benchmark()
	b.step()
	return b
}

pub fn (b mut Benchmark) measure(label string) i64 {
	b.ok()
	res := b.step_timer.elapsed()
	println(b.step_message_with_label(BSPENT, 'in $label'))
	b.step()
	return res
}

pub fn (b &Benchmark) step_message_with_label(label string, msg string) string {
	mut timed_line := ''
	if b.nexpected_steps > 0 {
		mut sprogress := ''
		if b.nexpected_steps < 10 {
			sprogress = if b.no_cstep { 'TMP1/${b.nexpected_steps:1d}' } else {
				'${b.cstep:1d}/${b.nexpected_steps:1d}'
			}
		}
		if b.nexpected_steps >= 10 && b.nexpected_steps < 100 {
			sprogress = if b.no_cstep { 'TMP2/${b.nexpected_steps:2d}' } else {
				'${b.cstep:2d}/${b.nexpected_steps:2d}'
			}
		}
		if b.nexpected_steps >= 100 && b.nexpected_steps < 1000 {
			sprogress = if b.no_cstep { 'TMP3/${b.nexpected_steps:3d}' } else {
				'${b.cstep:3d}/${b.nexpected_steps:3d}'
			}
		}
		timed_line = b.tdiff_in_ms('[${sprogress}] $msg', b.step_timer.elapsed())
	}
	else {
		timed_line = b.tdiff_in_ms(msg, b.step_timer.elapsed())
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

pub fn (b &Benchmark) step_message_skip(msg string) string {
	return b.step_message_with_label(BSKIP, msg)
}

pub fn (b &Benchmark) total_message(msg string) string {
	mut tmsg := '${msg}\n                 ok, fail, skip, total = ' + term.ok_message('${b.nok:5d}') + ', ' + if b.nfail > 0 { term.red('${b.nfail:5d}') } else { '${b.nfail:5d}' } + ', ' + if b.nskip > 0 { term.bright_yellow('${b.nskip:5d}') } else { '${b.nskip:5d}' } + ', ' + '${b.ntotal:5d}'
	if b.verbose {
		tmsg = '<=== total time spent $tmsg'
	}
	return '  ' + b.tdiff_in_ms(tmsg, b.bench_timer.elapsed())
}

pub fn (b &Benchmark) total_duration() i64 {
	return b.bench_timer.elapsed()
}

// //////////////////////////////////////////////////////////////////
fn (b &Benchmark) tdiff_in_ms(s string, tdiff i64) string {
	if b.verbose {
		return '${tdiff:6d} ms $s'
	}
	return s
}
