module main

import datatypes.lockfree
import time
import sync
import os
import flag
import runtime

// Test configuration parameters
const buffer_size = 1024 // Size of the ring buffer

const items_per_thread = 1_000_000 // Items to produce per thread

const warmup_runs = 3 // Number of warmup runs

const test_runs = 5 // Number of test runs

const max_threads = runtime.nr_jobs() // Maximum number of threads

// Performance test results
struct RingBufferPerfResult {
	scenario   string // Test scenario identifier
	throughput f64    // Throughput in million operations per second
	latency    f64    // Average latency in nanoseconds
	cpu_usage  f64    // CPU usage percentage
}

fn main() {
	println('Lock-Free Ring Buffer Performance Test')
	println('======================================')
	println('Maximum number of threads set to nr_jobs = ${max_threads}')
	mut fp := flag.new_flag_parser(os.args.clone())
	fp.skip_executable()
	show_help := fp.bool('help', 0, false, 'Show this help screen\n')
	debug := fp.bool('debug', 0, false, 'Show debug message, stat of ringbuffer')
	batch := fp.bool('batch', 0, true, 'Batch mode, batch size = 32')
	if show_help {
		println(fp.usage())
		exit(0)
	}

	// Test different scenarios
	mut results := []RingBufferPerfResult{}

	// Single Producer Single Consumer
	results << test_scenario('SPSC', 1, 1, batch, debug)

	// Multiple Producers Single Consumer
	for i in [2, 4, 8, 16] {
		if i <= max_threads {
			results << test_scenario('MPSC (${i}P1C)', i, 1, batch, debug)
		}
	}

	// Single Producer Multiple Consumers
	for i in [2, 4, 8, 16] {
		if i <= max_threads {
			results << test_scenario('SPMC (1P${i}C)', 1, i, batch, debug)
		}
	}

	// Multiple Producers Multiple Consumers
	for i in [2, 4, 8, 16] {
		if i * 2 <= max_threads {
			results << test_scenario('MPMC (${i}P${i}C)', i, i, batch, debug)
		}
	}

	// Print final results
	print_results(results)
}

// Test specific scenario with given producers/consumers
fn test_scenario(scenario string, producers int, consumers int, batch bool, debug bool) RingBufferPerfResult {
	println('\nTesting scenario: ${scenario}')

	// Create ring buffer
	mut rb := lockfree.new_ringbuffer[int](buffer_size)

	// Warmup runs
	for _ in 0 .. warmup_runs {
		run_test(mut rb, producers, consumers, false, batch, debug)
		rb.clear()
	}

	// Actual test runs
	mut total_time := time.Duration(0)
	mut total_ops := 0

	for _ in 0 .. test_runs {
		duration, ops := run_test(mut rb, producers, consumers, true, batch, debug)
		total_time += duration
		total_ops += ops

		// Reset buffer after each run
		rb.clear()
	}

	// Calculate performance metrics
	avg_time := total_time / test_runs
	throughput := f64(total_ops) / avg_time.seconds() / 1_000_000 // MOPS
	latency := avg_time.nanoseconds() / f64(total_ops / test_runs) // ns/op

	return RingBufferPerfResult{
		scenario:   scenario
		throughput: throughput
		latency:    latency
		cpu_usage:  0.0 // Actual value should be obtained from system
	}
}

// Execute single test run
fn run_test(mut rb lockfree.RingBuffer[int], producers int, consumers int, measure bool, batch bool, debug bool) (time.Duration, int) {
	mut wg := sync.new_waitgroup()
	total_items := producers * items_per_thread

	// Key fix: Consumers should consume exact producer total
	items_per_consumer := total_items / consumers
	mut remaining := total_items % consumers

	// Start producers
	start_time := time.now()
	for i in 0 .. producers {
		wg.add(1)
		spawn producer_thread(mut rb, i, mut wg, batch, debug)
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
		spawn consumer_thread(mut rb, i, count, mut consumed_counts, mut wg, batch, debug)
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
fn producer_thread(mut rb lockfree.RingBuffer[int], id int, mut wg sync.WaitGroup, batch bool, debug bool) {
	defer {
		wg.done()
	}

	// Generate items in producer-specific range
	start := id * items_per_thread
	end := start + items_per_thread

	if batch {
		// Use batch pushing for better performance
		batch_size := 32
		mut batch_buffer := []int{cap: batch_size}

		for i in start .. end {
			batch_buffer << i
			if batch_buffer.len == batch_size {
				rb.push_many(batch_buffer)
				batch_buffer.clear()
			}
		}

		// Push remaining items in final batch
		if batch_buffer.len > 0 {
			rb.push_many(batch_buffer)
		}
	} else {
		for i in start .. end {
			rb.push(i)
		}
	}
}

// Consumer thread with fixed consumption target
fn consumer_thread(mut rb lockfree.RingBuffer[int], id int, items_to_consume int, mut consumed_counts []int, mut wg sync.WaitGroup, batch bool, debug bool) {
	defer {
		wg.done()
	}

	if batch {
		// Use batch consumption for better performance
		batch_size := 32
		mut count := 0
		mut last_value := -1
		mut batch_buffer := []int{len: batch_size} // Reusable buffer

		for count < items_to_consume - batch_size {
			// Consume batch using pop_many
			rb.pop_many(mut batch_buffer)
			count += batch_size

			// Debug output
			if debug && count % 1000000 == 0 {
				println('consume item count = ${count}')
			}

			// Check sequence continuity
			validate_batch_detailed(id, batch_buffer, last_value)
			last_value = batch_buffer[batch_buffer.len - 1]
		}

		remaining := items_to_consume - count
		if remaining > 0 {
			mut remaining_buffer := []int{len: remaining}
			rb.pop_many(mut remaining_buffer)
			count += remaining

			validate_batch_detailed(id, remaining_buffer, last_value)
		}
	} else {
		for i in 0 .. items_to_consume {
			_ := rb.pop()
			// Debug output
			if debug && i % 1000000 == 0 {
				println('consume item count = ${i}')
			}
		}
	}
	consumed_counts[id] = items_to_consume
}

fn validate_batch_detailed(id int, batch []int, prev_last int) bool {
	if batch.len == 0 {
		return true
	}

	mut valid := true

	mut expected := batch[0] + 1
	for i in 1 .. batch.len {
		if batch[i] != expected {
			eprintln('[Thread ${id}] Sequence error: Position ${i} expected ${expected}, got ${batch[i]}')
			valid = false
		}
		expected += 1
	}

	return valid
}

// Print formatted performance results
fn print_results(results []RingBufferPerfResult) {
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
