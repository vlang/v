module bench

import os
import time

// C.getrusage declares the C getrusage symbol used by bench.
fn C.getrusage(who int, usage voidptr) int

// Step represents step data used by bench.
pub struct Step {
pub:
	name    string
	time_us i64
	ram_kb  i64
}

// Bench represents bench data used by bench.
pub struct Bench {
mut:
	steps    []Step
	total_sw time.StopWatch
	step_sw  time.StopWatch
}

// new creates a new value for bench.
pub fn new() Bench {
	return Bench{
		total_sw: time.new_stopwatch()
		step_sw:  time.new_stopwatch()
	}
}

// step records a serial pipeline step.
pub fn (mut b Bench) step(name string) {
	b.step_parallel(name, false)
}

// step_parallel records a pipeline step, appending "(parallel)" to its name
// when the step actually ran across threads.
pub fn (mut b Bench) step_parallel(name string, parallel bool) {
	elapsed_us := b.step_sw.elapsed().microseconds()
	ram_mb := f64(current_rss_kb()) / 1024.0
	ms := f64(elapsed_us) / 1000.0
	label := if parallel { '${name} (parallel)' } else { name }
	println('  ${label:-20s} ${ms:8.2f} ms   ${ram_mb:6.0f} MB resident RAM')
	b.steps << Step{
		name:    label
		time_us: elapsed_us
		ram_kb:  i64(ram_mb * 1024)
	}
	b.step_sw.restart()
}

// print_report updates print report state for Bench.
pub fn (b &Bench) print_report() {
	total_ms := f64(b.total_sw.elapsed().microseconds()) / 1000.0
	println('  ${'total':-20s} ${total_ms:8.2f} ms')
	println('')
}

// current_rss_kb returns current rss kb data for bench.
fn current_rss_kb() i64 {
	$if macos {
		return macos_peak_rss_kb()
	}
	$if linux {
		return linux_rss_kb()
	}
	return 0
}

// macos_peak_rss_kb supports macos peak rss kb handling for bench.
fn macos_peak_rss_kb() i64 {
	mut usage := [256]u8{}
	if C.getrusage(0, &usage[0]) == 0 {
		ru_maxrss := unsafe { *(&i64(&usage[32])) }
		return ru_maxrss / 1024
	}
	return 0
}

// linux_rss_kb supports linux rss kb handling for bench.
fn linux_rss_kb() i64 {
	content := os.read_file('/proc/self/status') or { return 0 }
	for line in content.split('\n') {
		if line.starts_with('VmRSS:') {
			parts := line.split_any(' \t').filter(it.len > 0)
			if parts.len >= 2 {
				return parts[1].i64()
			}
		}
	}
	return 0
}
