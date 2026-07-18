module bench

import runtime
import time

const default_memory_limit_kb = i64(10) * 1024 * 1024

// Step represents step data used by bench.
pub struct Step {
pub:
	name             string
	time_us          i64
	ram_kb           i64
	peak_ram_kb      i64
	allocation_count u64
	allocated_bytes  u64
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
	steps                 []Step
	metrics               []Metric
	total_sw              time.StopWatch
	step_sw               time.StopWatch
	last_allocation_count u64
	last_allocated_bytes  u64
	memory_limit_kb       i64
}

// new creates a new value for bench.
pub fn new() Bench {
	allocations := current_allocation_stats()
	return Bench{
		total_sw:              time.new_stopwatch()
		step_sw:               time.new_stopwatch()
		last_allocation_count: allocations.allocation_count
		last_allocated_bytes:  allocations.allocated_bytes
		memory_limit_kb:       default_memory_limit_kb
	}
}

// disable_memory_limit disables the compiler RSS safety limit.
pub fn (mut b Bench) disable_memory_limit() {
	b.memory_limit_kb = 0
}

// step records a serial pipeline step.
pub fn (mut b Bench) step(name string) {
	b.step_parallel(name, false)
}

// step_parallel records a pipeline step, appending "(parallel)" to its name
// when the step actually ran across threads.
pub fn (mut b Bench) step_parallel(name string, parallel bool) {
	elapsed_us := b.step_sw.elapsed().microseconds()
	label := if parallel { '${name} (parallel)' } else { name }
	ram_kb := current_rss_kb()
	peak_ram_kb := peak_rss_kb()
	message := memory_limit_error(ram_kb, b.memory_limit_kb, label)
	if message.len > 0 {
		eprintln(message)
		exit(1)
	}
	ram_mb := f64(ram_kb) / 1024.0
	peak_ram_mb := f64(peak_ram_kb) / 1024.0
	ms := f64(elapsed_us) / 1000.0
	allocations := current_allocation_stats()
	allocation_count := allocations.allocation_count - b.last_allocation_count
	allocated_bytes := allocations.allocated_bytes - b.last_allocated_bytes
	allocation_suffix := if allocations.enabled {
		allocated_mb := f64(allocated_bytes) / (1024.0 * 1024.0)
		'   ${allocation_count} allocs   ${allocated_mb:8.2f} MB allocated'
	} else {
		''
	}
	println('  ${label:-20s} ${ms:8.2f} ms   ${ram_mb:6.0f} MB RSS   ${peak_ram_mb:6.0f} MB peak${allocation_suffix}')
	b.steps << Step{
		name:             label
		time_us:          elapsed_us
		ram_kb:           ram_kb
		peak_ram_kb:      peak_ram_kb
		allocation_count: allocation_count
		allocated_bytes:  allocated_bytes
	}
	// Exclude the benchmark line's own formatting allocations from the next phase.
	after_report := current_allocation_stats()
	b.last_allocation_count = after_report.allocation_count
	b.last_allocated_bytes = after_report.allocated_bytes
	b.step_sw.restart()
}

fn memory_limit_error(ram_kb i64, limit_kb i64, step string) string {
	if limit_kb <= 0 || ram_kb < limit_kb {
		return ''
	}
	ram_mb := ram_kb / 1024
	limit_gib := limit_kb / (1024 * 1024)
	return 'error: v3 compiler memory usage reached ${ram_mb} MiB RSS after ${step} ' +
		'(limit: ${limit_gib} GiB); use `-no-memory-limit` to disable this limit'
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

struct AllocationStats {
	enabled          bool
	allocation_count u64
	allocated_bytes  u64
}

fn current_allocation_stats() AllocationStats {
	$if prealloc {
		stats := prealloc_stats_snapshot()
		return AllocationStats{
			enabled:          stats.enabled
			allocation_count: stats.allocation_count
			allocated_bytes:  stats.allocated_bytes
		}
	} $else {
		return AllocationStats{}
	}
}
