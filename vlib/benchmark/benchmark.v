module benchmark

import time
import term

pub const (
	b_ok    = term.ok_message('OK  ')
	b_fail  = term.fail_message('FAIL')
	b_skip  = term.warn_message('SKIP')
	b_spent = term.ok_message('SPENT')
)

pub struct Benchmark {
pub mut:
	bench_timer     time.StopWatch
	verbose         bool
	no_cstep        bool
	step_timer      time.StopWatch
	ntotal          int
	nok             int
	nfail           int
	nskip           int
	nexpected_steps int
	cstep           int
	bok             string
	bfail           string
}

// new_benchmark returns a `Benchmark` instance on the stack.
pub fn new_benchmark() Benchmark {
	return Benchmark{
		bench_timer: time.new_stopwatch({})
		verbose: true
	}
}

// new_benchmark_no_cstep returns a new `Benchmark` instance with step counting disabled.
pub fn new_benchmark_no_cstep() Benchmark {
	return Benchmark{
		bench_timer: time.new_stopwatch({})
		verbose: true
		no_cstep: true
	}
}

// new_benchmark_pointer returns a new `Benchmark` instance allocated on the heap.
// This is useful for long-lived use of `Benchmark` instances.
pub fn new_benchmark_pointer() &Benchmark {
	return &Benchmark{
		bench_timer: time.new_stopwatch({})
		verbose: true
	}
}

// set_total_expected_steps sets the the total amount of steps the benchmark is expected to take.
pub fn (mut b Benchmark) set_total_expected_steps(n int) {
	b.nexpected_steps = n
}

// stop stops the internal benchmark timer.
pub fn (mut b Benchmark) stop() {
	b.bench_timer.stop()
}

// step increases the step count by 1 and restarts the internal timer.
pub fn (mut b Benchmark) step() {
	b.step_timer.restart()
	if !b.no_cstep {
		b.cstep++
	}
}

// fail increases the fail count by 1 and stops the internal timer.
pub fn (mut b Benchmark) fail() {
	b.step_timer.stop()
	b.ntotal++
	b.nfail++
}

// ok increases the ok count by 1 and stops the internal timer.
pub fn (mut b Benchmark) ok() {
	b.step_timer.stop()
	b.ntotal++
	b.nok++
}

// skip increases the skip count by 1 and stops the internal timer.
pub fn (mut b Benchmark) skip() {
	b.step_timer.stop()
	b.ntotal++
	b.nskip++
}

// fail_many increases the fail count by `n` and stops the internal timer.
pub fn (mut b Benchmark) fail_many(n int) {
	b.step_timer.stop()
	b.ntotal += n
	b.nfail += n
}

// ok_many increases the ok count by `n` and stops the internal timer.
pub fn (mut b Benchmark) ok_many(n int) {
	b.step_timer.stop()
	b.ntotal += n
	b.nok += n
}

// neither_fail_nor_ok stops the internal timer.
pub fn (mut b Benchmark) neither_fail_nor_ok() {
	b.step_timer.stop()
}

// start returns a new, running, instance of `Benchmark`.
// This is a shorthand for calling `new_benchmark().step()`.
pub fn start() Benchmark {
	mut b := new_benchmark()
	b.step()
	return b
}

// measure prints the current time spent doing `label`, since the benchmark was started.
pub fn (mut b Benchmark) measure(label string) i64 {
	b.ok()
	res := b.step_timer.elapsed().microseconds()
	println(b.step_message_with_label(benchmark.b_spent, 'in $label'))
	b.step()
	return res
}

// step_message_with_label_and_duration returns a string describing the current step.
pub fn (b &Benchmark) step_message_with_label_and_duration(label string, msg string, sduration time.Duration) string {
	timed_line := b.tdiff_in_ms(msg, sduration.microseconds())
	if b.nexpected_steps > 1 {
		mut sprogress := ''
		if b.nexpected_steps < 10 {
			sprogress = if b.no_cstep {
				'TMP1/${b.nexpected_steps:1d}'
			} else {
				'${b.cstep:1d}/${b.nexpected_steps:1d}'
			}
		} else if b.nexpected_steps >= 10 && b.nexpected_steps < 100 {
			sprogress = if b.no_cstep {
				'TMP2/${b.nexpected_steps:2d}'
			} else {
				'${b.cstep:2d}/${b.nexpected_steps:2d}'
			}
		} else if b.nexpected_steps >= 100 && b.nexpected_steps < 1000 {
			sprogress = if b.no_cstep {
				'TMP3/${b.nexpected_steps:3d}'
			} else {
				'${b.cstep:3d}/${b.nexpected_steps:3d}'
			}
		} else {
			sprogress = if b.no_cstep {
				'TMP4/${b.nexpected_steps:4d}'
			} else {
				'${b.cstep:4d}/${b.nexpected_steps:4d}'
			}
		}
		return '${label:-5s} [$sprogress] $timed_line'
	}
	return '${label:-5s}$timed_line'
}

// step_message_with_label returns a string describing the current step using current time as duration.
pub fn (b &Benchmark) step_message_with_label(label string, msg string) string {
	return b.step_message_with_label_and_duration(label, msg, b.step_timer.elapsed())
}

// step_message returns a string describing the current step.
pub fn (b &Benchmark) step_message(msg string) string {
	return b.step_message_with_label('', msg)
}

// step_message_ok returns a string describing the current step with an standard "OK" label.
pub fn (b &Benchmark) step_message_ok(msg string) string {
	return b.step_message_with_label(benchmark.b_ok, msg)
}

// step_message_fail returns a string describing the current step with an standard "FAIL" label.
pub fn (b &Benchmark) step_message_fail(msg string) string {
	return b.step_message_with_label(benchmark.b_fail, msg)
}

// step_message_skip returns a string describing the current step with an standard "SKIP" label.
pub fn (b &Benchmark) step_message_skip(msg string) string {
	return b.step_message_with_label(benchmark.b_skip, msg)
}

// total_message returns a string with total summary of the benchmark run.
pub fn (b &Benchmark) total_message(msg string) string {
	the_label := term.colorize(term.gray, msg)
	mut tmsg := '${term.colorize(term.bold, 'Summary for $the_label:')} '
	if b.nfail > 0 {
		tmsg += term.colorize(term.bold, term.colorize(term.red, '$b.nfail failed')) + ', '
	}
	if b.nok > 0 {
		tmsg += term.colorize(term.bold, term.colorize(term.green, '$b.nok passed')) + ', '
	}
	if b.nskip > 0 {
		tmsg += term.colorize(term.bold, term.colorize(term.yellow, '$b.nskip skipped')) + ', '
	}
	tmsg += '$b.ntotal total. ${term.colorize(term.bold, 'Runtime:')} ${b.bench_timer.elapsed().microseconds() / 1000} ms.\n'
	return tmsg
}

// total_duration returns the duration in ms.
pub fn (b &Benchmark) total_duration() i64 {
	return b.bench_timer.elapsed().milliseconds()
}

// tdiff_in_ms prefixes `s` with a time difference calculation.
fn (b &Benchmark) tdiff_in_ms(s string, tdiff i64) string {
	if b.verbose {
		return '${f64(tdiff) / 1000.0:9.3f} ms $s'
	}
	return s
}
