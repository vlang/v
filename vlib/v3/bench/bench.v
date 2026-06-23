module bench

import os
import time

#include <sys/resource.h>

struct C.rusage {
	ru_maxrss i64
}

fn C.getrusage(who int, usage &C.rusage) int

pub struct Step {
pub:
	name    string
	time_us i64
	ram_kb  i64
}

pub struct Bench {
mut:
	steps    []Step
	total_sw time.StopWatch
	step_sw  time.StopWatch
}

pub fn new() Bench {
	return Bench{
		total_sw: time.new_stopwatch()
		step_sw:  time.new_stopwatch()
	}
}

pub fn (mut b Bench) step(name string) {
	elapsed_us := b.step_sw.elapsed().microseconds()
	ram_mb := f64(current_rss_kb()) / 1024.0
	ms := f64(elapsed_us) / 1000.0
	println('  ${name:-20s} ${ms:8.2f} ms   ${ram_mb:6.0f} MB resident RAM')
	b.steps << Step{
		name:    name
		time_us: elapsed_us
		ram_kb:  i64(ram_mb * 1024)
	}
	b.step_sw.restart()
}

pub fn (b &Bench) print_report() {
	total_ms := f64(b.total_sw.elapsed().microseconds()) / 1000.0
	println('  ${'total':-20s} ${total_ms:8.2f} ms')
	println('')
}

fn current_rss_kb() i64 {
	$if macos {
		return macos_peak_rss_kb()
	}
	$if linux {
		return linux_rss_kb()
	}
	return 0
}

fn macos_peak_rss_kb() i64 {
	mut usage := C.rusage{}
	if C.getrusage(0, &usage) == 0 {
		return usage.ru_maxrss / 1024
	}
	return 0
}

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
