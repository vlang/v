module main

import datatypes.lockfree
import time
import sync
import math
import os
import flag
import runtime

// Test configuration parameters
const buffer_size = 1024 // Size of the ring buffer

const items_per_thread = 1_000_000 // Items to produce per thread

const warmup_runs = 3 // Number of warmup runs

const test_runs = 5 // Number of test runs

const max_threads = runtime.nr_cpus() // Maximum number of threads

// Performance test results
struct PerfResult {
	scenario   string // Test scenario identifier
	throughput f64    // Throughput in million operations per second
	latency    f64    // Average latency in nanoseconds
	cpu_usage  f64    // CPU usage percentage
}

fn main() {
	println('Lock-Free Ring Buffer Performance Test')
	println('======================================')
	println('Maximum number of threads set to nr_cpus = ${max_threads}')
	mut fp := flag.new_flag_parser(os.args.clone())
	fp.skip_executable()
	show_help := fp.bool('help', 0, false, 'Show this help screen\n')
	debug := fp.bool('debug', 0, false, 'Show debug message, stat of ringbuffer')
	if show_help {
		println(fp.usage())
		exit(0)
	}

	// Test different scenarios
	mut results := []PerfResult{}

	// Single Producer Single Consumer
	results << test_scenario('SPSC', 1, 1, debug)

	// Multiple Producers Single Consumer
	for i in [2, 4, 8] {
		if i <= max_threads {
			results << test_scenario('MPSC (${i}P1C)', i, 1, debug)
		}
	}

	// Single Producer Multiple Consumers
	for i in [2, 4] {
		if i <= max_threads {
			results << test_scenario('SPMC (1P${i}C)', 1, i, debug)
		}
	}

	// Multiple Producers Multiple Consumers
	for i in [2, 4, 8] {
		if i * 2 <= max_threads {
			results << test_scenario('MPMC (${i}P${i}C)', i, i, debug)
		}
	}

	// Print final results
	print_results(results)
}

// Test specific scenario with given producers/consumers
fn test_scenario(scenario string, producers int, consumers int, debug bool) PerfResult {
	println('\nTesting scenario: ${scenario}')

	// Create ring buffer
	mut rb := lockfree.new_ringbuffer[int](buffer_size)

	// Warmup runs
	for _ in 0 .. warmup_runs {
		run_test(mut rb, producers, consumers, false, debug)
		rb.clear()
	}

	// Actual test runs
	mut total_time := time.Duration(0)
	mut total_ops := 0

	for _ in 0 .. test_runs {
		duration, ops := run_test(mut rb, producers, consumers, true, debug)
		total_time += duration
		total_ops += ops

		// Reset buffer after each run
		rb.clear()
	}

	// Calculate performance metrics
	avg_time := total_time / test_runs
	throughput := f64(total_ops) / avg_time.seconds() / 1_000_000 // MOPS
	latency := avg_time.nanoseconds() / f64(total_ops / test_runs) // ns/op

	return PerfResult{
		scenario:   scenario
		throughput: throughput
		latency:    latency
		cpu_usage:  0.0 // Actual value should be obtained from system
	}
}

// Execute single test run
fn run_test(mut rb lockfree.RingBuffer[int], producers int, consumers int, measure bool, debug bool) (time.Duration, int) {
	mut wg := sync.new_waitgroup()
	total_items := producers * items_per_thread

	// Key fix: Consumers should consume exact producer total
	items_per_consumer := total_items / consumers
	mut remaining := total_items % consumers

	// Start producers
	start_time := time.now()
	for i in 0 .. producers {
		wg.add(1)
		spawn producer_thread(mut rb, i, mut wg, debug)
	}

	// Start consumers
	mut consumed_counts := []int{len: consumers, init: 0}
	for i in 0 .. consumers {
		wg.add(1)
		// Distribute remaining items to first consumers
		mut count := items_per_consumer
		if remaining > 0 {
			count += 1
			remaining -= 1
		}
		spawn consumer_thread(mut rb, i, count, mut consumed_counts, mut wg, debug)
	}

	// Wait for all threads to complete
	wg.wait()
	end_time := time.now()
	if debug {
		println(rb.stat())
	}

	// Validate results
	mut total_consumed := 0
	for count in consumed_counts {
		total_consumed += count
	}

	if total_consumed != total_items {
		eprintln('Error: Produced ${total_items} items but consumed ${total_consumed}')
	}

	duration := end_time - start_time
	if measure {
		println('Completed ${total_items} items in ${duration.milliseconds()}ms')
	}

	return duration, total_items
}

// Producer thread implementation
fn producer_thread(mut rb lockfree.RingBuffer[int], id int, mut wg sync.WaitGroup, debug bool) {
	defer {
		wg.done()
	}

	// Generate items in producer-specific range
	start := id * items_per_thread
	end := start + items_per_thread

	// Use batch pushing for better performance
	batch_size := 32
	mut batch := []int{cap: batch_size}

	// dump('producer_thread')

	for i in start .. end {
		batch << i
		if batch.len == batch_size {
			rb.push_many(batch)
			batch.clear()
		}
	}

	// Push remaining items in final batch
	if batch.len > 0 {
		rb.push_many(batch)
	}
}

// Consumer thread with fixed consumption target
fn consumer_thread(mut rb lockfree.RingBuffer[int], id int, items_to_consume int, mut consumed_counts []int, mut wg sync.WaitGroup, debug bool) {
	defer {
		wg.done()
	}

	// Use batch consumption for better performance
	batch_size := 32
	mut count := 0

	for count < items_to_consume {
		// Calculate current batch size
		remaining := items_to_consume - count
		current_batch := math.min(batch_size, remaining)

		// Consume batch and update count
		items := rb.pop_many(u32(current_batch))
		count += items.len
		if count % 1000000 == 0 && debug {
			println('consume item count = ${count}')
		}

		// Validate sequence integrity
		if items.len > 0 {
			first := items[0]
			last := items[items.len - 1]
			if last - first != items.len - 1 {
				eprintln('Thread ${id}: Sequence error! Expected ${first}..${first + items.len - 1}, got ${first}..${last}')
			}
		}
	}

	consumed_counts[id] = count
}

// Print formatted performance results
fn print_results(results []PerfResult) {
	println('\nPerformance Results')
	println('====================================================================')
	println('Scenario\t\tThroughput (M ops/s)\tAvg Latency (ns)\tCPU Usage (%)')
	println('--------------------------------------------------------------------')

	for res in results {
		println('${res.scenario:20}\t${res.throughput:8.2f}\t\t\t${res.latency:8.2f}\t\t\t${res.cpu_usage:5.1f}')
	}
	println('====================================================================')

	// Find best performing scenario
	mut best_throughput := 0.0
	mut best_scenario := ''

	for res in results {
		if res.throughput > best_throughput {
			best_throughput = res.throughput
			best_scenario = res.scenario
		}
	}

	println('\nBest performance: ${best_scenario} with ${best_throughput:.2f} M ops/s')
}
