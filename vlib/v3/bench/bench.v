module bench

import runtime
import time

// Step represents step data used by bench.
pub struct Step {
pub:
	name        string
	time_us     i64
	ram_kb      i64
	peak_ram_kb i64
}

// Metric represents one structural compiler counter reported with a build.
pub struct Metric {
pub:
	name  string
	value i64
	unit  string
}

// Bench represents bench data used by bench.
pub struct Bench {
mut:
	steps    []Step
	metrics  []Metric
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
	ram_kb := current_rss_kb()
	peak_ram_kb := peak_rss_kb()
	ram_mb := f64(ram_kb) / 1024.0
	peak_ram_mb := f64(peak_ram_kb) / 1024.0
	ms := f64(elapsed_us) / 1000.0
	label := if parallel { '${name} (parallel)' } else { name }
	println('  ${label:-20s} ${ms:8.2f} ms   ${ram_mb:6.0f} MB RSS   ${peak_ram_mb:6.0f} MB peak')
	b.steps << Step{
		name:        label
		time_us:     elapsed_us
		ram_kb:      ram_kb
		peak_ram_kb: peak_ram_kb
	}
	b.step_sw.restart()
}

// metric records a structural compiler counter for the final benchmark report.
pub fn (mut b Bench) metric(name string, value i64, unit string) {
	b.metrics << Metric{
		name:  name
		value: value
		unit:  unit
	}
}

// print_report updates print report state for Bench.
pub fn (b &Bench) print_report() {
	total_ms := f64(b.total_sw.elapsed().microseconds()) / 1000.0
	println('  ${'total':-20s} ${total_ms:8.2f} ms')
	if b.metrics.len > 0 {
		println('  metrics:')
		for metric in b.metrics {
			suffix := if metric.unit.len > 0 { ' ${metric.unit}' } else { '' }
			println('    ${metric.name:-28s} ${metric.value}${suffix}')
		}
	}
	println('')
}

// current_rss_kb returns current rss kb data for bench.
fn current_rss_kb() i64 {
	bytes := runtime.used_memory() or { return 0 }
	return i64(bytes / 1024)
}
