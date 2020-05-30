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
	b_ok = term.ok_message('OK  ')
	b_fail = term.fail_message('FAIL')
	b_skip  = term.warn_message('SKIP')
	b_spent = term.ok_message('SPENT')
)

pub struct Benchmark {
pub mut:
	bench_timer      time.StopWatch
	verbose          bool
	no_cstep         bool
	step_timer       time.StopWatch
	ntotal           int
	nok              int
	nfail            int
	nskip            int
	nexpected_steps  int
	cstep            int
	bok              string
	bfail            string
}

pub fn new_benchmark() Benchmark {
	return Benchmark{
		bench_timer: time.new_stopwatch({})
		verbose: true
	}
}

pub fn new_benchmark_no_cstep() Benchmark {
	return Benchmark{
		bench_timer: time.new_stopwatch({})
		verbose: true
		no_cstep: true
	}
}

pub fn new_benchmark_pointer() &Benchmark {
	return &Benchmark{
		bench_timer: time.new_stopwatch({})
		verbose: true
	}
}

pub fn (mut b Benchmark) set_total_expected_steps(n int) {
	b.nexpected_steps = n
}

pub fn (mut b Benchmark) stop() {
	b.bench_timer.stop()
}

pub fn (mut b Benchmark) step() {
	b.step_timer.restart()
	if !b.no_cstep {
		b.cstep++
	}
}

pub fn (mut b Benchmark) fail() {
	b.step_timer.stop()
	b.ntotal++
	b.nfail++
}

pub fn (mut b Benchmark) ok() {
	b.step_timer.stop()
	b.ntotal++
	b.nok++
}

pub fn (mut b Benchmark) skip() {
	b.step_timer.stop()
	b.ntotal++
	b.nskip++
}

pub fn (mut b Benchmark) fail_many(n int) {
	b.step_timer.stop()
	b.ntotal += n
	b.nfail += n
}

pub fn (mut b Benchmark) ok_many(n int) {
	b.step_timer.stop()
	b.ntotal += n
	b.nok += n
}

pub fn (mut b Benchmark) neither_fail_nor_ok() {
	b.step_timer.stop()
}

pub fn start() Benchmark {
	mut b := new_benchmark()
	b.step()
	return b
}

pub fn (mut b Benchmark) measure(label string) i64 {
	b.ok()
	res := b.step_timer.elapsed().microseconds()
	println(b.step_message_with_label(b_spent, 'in $label'))
	b.step()
	return res
}

pub fn (b &Benchmark) step_message_with_label(label string, msg string) string {
	timed_line := b.tdiff_in_ms(msg, b.step_timer.elapsed().microseconds())
	if b.nexpected_steps > 1 {
		mut sprogress := ''
		if b.nexpected_steps < 10 {
			sprogress = if b.no_cstep { 'TMP1/${b.nexpected_steps:1d}' } else {
				'${b.cstep:1d}/${b.nexpected_steps:1d}'
			}
		} else if b.nexpected_steps >= 10 && b.nexpected_steps < 100 {
			sprogress = if b.no_cstep { 'TMP2/${b.nexpected_steps:2d}' } else {
				'${b.cstep:2d}/${b.nexpected_steps:2d}'
			}
		} else if b.nexpected_steps >= 100 && b.nexpected_steps < 1000 {
			sprogress = if b.no_cstep { 'TMP3/${b.nexpected_steps:3d}' } else {
				'${b.cstep:3d}/${b.nexpected_steps:3d}'
			}
		} else {
			sprogress = if b.no_cstep { 'TMP4/${b.nexpected_steps:4d}' } else {
				'${b.cstep:4d}/${b.nexpected_steps:4d}'
			}
		}
		return '${label:-5s} [${sprogress}] ${timed_line}'
	}
	return '${label:-5s}${timed_line}'
}

pub fn (b &Benchmark) step_message(msg string) string {
	return b.step_message_with_label('', msg)
}

pub fn (b &Benchmark) step_message_ok(msg string) string {
	return b.step_message_with_label(b_ok, msg)
}

pub fn (b &Benchmark) step_message_fail(msg string) string {
	return b.step_message_with_label(b_fail, msg)
}

pub fn (b &Benchmark) step_message_skip(msg string) string {
	return b.step_message_with_label(b_skip, msg)
}

pub fn (b &Benchmark) total_message(msg string) string {
	mut tmsg := '${msg}\n                 ok, fail, skip, total = ' + term.ok_message('${b.nok:5d}') + ', ' + if b.nfail > 0 { term.red('${b.nfail:5d}') } else { '${b.nfail:5d}' } + ', ' + if b.nskip > 0 { term.bright_yellow('${b.nskip:5d}') } else { '${b.nskip:5d}' } + ', ' + '${b.ntotal:5d}'
	if b.verbose {
		tmsg = '<=== total time spent $tmsg'
	}
	mut spaces := '    '
	if b.nexpected_steps > 1 {
		// NB: the formula below accounts for the progress bar [step/total]
		str_steps := '$b.nexpected_steps'
		x := 4 + str_steps.len * 2 + 5
		spaces = ' '.repeat(x)
	}
	return spaces + b.tdiff_in_ms(tmsg, b.bench_timer.elapsed().microseconds())
}

// .total_duration - returns the duration in ms
pub fn (b &Benchmark) total_duration() i64 {
	return b.bench_timer.elapsed().milliseconds()
}

// //////////////////////////////////////////////////////////////////
fn (b &Benchmark) tdiff_in_ms(s string, tdiff i64) string {
	if b.verbose {
		return '${f64(tdiff)/1000.0:9.3f} ms $s'
	}
	return s
}
