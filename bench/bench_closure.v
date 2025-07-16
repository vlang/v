module main

import time
import sync
import os
import runtime
import v.util.version

// Define closure type alias
type ClosureFN = fn () int

// Test closures with different capture sizes
fn create_closure_small() ClosureFN {
	a := 0
	return fn [a] () int {
		return a
	}
}

fn create_closure_medium() ClosureFN {
	a, b, c, d := 1, 2, 3, 4
	return fn [a, b, c, d] () int {
		return a + b - c * d
	}
}

struct LargeData {
	array [10]int
}

fn create_closure_large() ClosureFN {
	data := LargeData{
		array: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]!
	}
	return fn [data] () int {
		mut sum := 0
		for i in 0 .. 10 {
			sum += data.array[i]
		}
		return sum
	}
}

// Result structs
struct TestResult {
	test_name   string
	iterations  int
	time_ms     i64
	ops_per_sec f64 // Operations per second
	notes       string
}

struct MemoryResult {
	test_name         string
	count             int
	start_mem_kb      int
	end_mem_kb        int
	delta_kb          int
	bytes_per_closure int
	check_sum         int
}

// Benchmark group - returns result structs
fn benchmark_closure_creation(iterations int) []TestResult {
	mut results := []TestResult{}

	// Test small closure creation
	mut start := time.ticks()
	for _ in 0 .. iterations {
		_ = create_closure_small()
	}
	small_time := time.ticks() - start
	mut ops_per_sec := f64(iterations) * 1000.0 / f64(small_time)
	results << TestResult{'Small Closure Creation', iterations, small_time, ops_per_sec, ''}

	// Test medium closure creation
	start = time.ticks()
	for _ in 0 .. iterations {
		_ = create_closure_medium()
	}
	medium_time := time.ticks() - start
	ops_per_sec = f64(iterations) * 1000.0 / f64(medium_time)
	results << TestResult{'Medium Closure Creation', iterations, medium_time, ops_per_sec, ''}

	// Test large closure creation
	large_iter := iterations / 10
	start = time.ticks()
	for _ in 0 .. large_iter {
		_ = create_closure_large()
	}
	large_time := time.ticks() - start
	ops_per_sec = f64(large_iter) * 1000.0 / f64(large_time)
	results << TestResult{'Large Closure Creation', large_iter, large_time, ops_per_sec, ''} //, "Equivalent iterations: ${iterations/10}"}
	return results
}

fn benchmark_closure_call(iterations int) []TestResult {
	mut results := []TestResult{}

	closure_small := create_closure_small()
	closure_medium := create_closure_medium()
	closure_large := create_closure_large()

	// Test small closure call
	mut start := time.ticks()
	for _ in 0 .. iterations {
		_ = closure_small()
	}
	small_time := time.ticks() - start
	mut ops_per_sec := f64(iterations) * 1000.0 / f64(small_time)
	results << TestResult{'Small Closure Call', iterations, small_time, ops_per_sec, ''}

	// Test medium closure call
	start = time.ticks()
	for _ in 0 .. iterations {
		_ = closure_medium()
	}
	medium_time := time.ticks() - start
	ops_per_sec = f64(iterations) * 1000.0 / f64(medium_time)
	results << TestResult{'Medium Closure Call', iterations, medium_time, ops_per_sec, ''}

	// Test large closure call
	large_iter := iterations / 10
	start = time.ticks()
	for _ in 0 .. large_iter {
		_ = closure_large()
	}
	large_time := time.ticks() - start
	ops_per_sec = f64(large_iter) * 1000.0 / f64(large_time)
	results << TestResult{'Large Closure Call', large_iter, large_time, ops_per_sec, ''}

	return results
}

fn benchmark_threaded_creation(threads int, iterations_per_thread int) TestResult {
	total_iterations := threads * iterations_per_thread

	mut wg := sync.new_waitgroup()
	wg.add(threads)

	start := time.ticks()

	for _ in 0 .. threads {
		go fn [mut wg, iterations_per_thread] () {
			defer { wg.done() }
			for _ in 0 .. iterations_per_thread {
				_ = create_closure_medium()
			}
		}()
	}

	wg.wait()
	elapsed := time.ticks() - start
	ops_per_sec := f64(total_iterations) * 1000.0 / f64(elapsed)

	return TestResult{
		test_name:   'Multi-threaded Creation'
		iterations:  total_iterations
		time_ms:     elapsed
		ops_per_sec: ops_per_sec
		notes:       'Threads: ${threads} Iterations per thread: ${iterations_per_thread}'
	}
}

fn baseline_call_performance(iterations int) TestResult {
	start := time.ticks()
	for _ in 0 .. iterations {
		_ = normal_function()
	}
	elapsed := time.ticks() - start
	ops_per_sec := f64(iterations) * 1000.0 / f64(elapsed)

	return TestResult{
		test_name:   'Normal Function Call'
		iterations:  iterations
		time_ms:     elapsed
		ops_per_sec: ops_per_sec
		notes:       'Baseline'
	}
}

fn benchmark_memory_usage(count int) MemoryResult {
	mut closures := []ClosureFN{}
	start_mem := runtime.used_memory() or { panic(err) }

	for i in 0 .. count {
		closure := create_closure_medium()
		closures << closure

		if i % 1000 == 0 {
			_ = closure()
		}
	}

	end_mem := runtime.used_memory() or { panic(err) }
	delta := int(end_mem) - int(start_mem)
	bytes_per_closure := delta / count

	// Calculate verification sum
	mut check_sum := 0
	n := if closures.len < 100 { closures.len } else { 100 }
	for idx in 0 .. n {
		check_sum += closures[idx]()
	}

	return MemoryResult{
		test_name:         'Closure Memory Overhead'
		count:             count
		start_mem_kb:      int(start_mem / 1024)
		end_mem_kb:        int(end_mem / 1024)
		delta_kb:          delta / 1024
		bytes_per_closure: bytes_per_closure
		check_sum:         check_sum
	}
}

fn normal_function() int {
	return 42
}

// Format performance data for readability
fn format_perf(ops_per_sec f64) string {
	if ops_per_sec >= 1_000_000 {
		return '${ops_per_sec / 1_000_000:5.2f} Mop/s'
	} else if ops_per_sec >= 1_000 {
		return '${ops_per_sec / 1_000:5.2f} Kop/s'
	} else {
		return '${ops_per_sec:5.2f} op/s'
	}
}

fn print_results_table(results []TestResult, title string) {
	println('|---------------------------|------------|----------|--------------|--------------|')
	for res in results {
		perf_str := format_perf(res.ops_per_sec)
		println('| ${res.test_name:-25} | ${res.iterations:10} | ${res.time_ms:8} | ${perf_str:-12} | ${res.notes:-12} |')
	}
}

fn main() {
	println('# V Language Closure Performance Benchmark Report')

	// Configurable test parameters
	base_iter := 100_000_000 // 100 million iterations
	creation_iter := 10_000_000 // 1 million iterations
	mem_count := 100_000
	threads := 8
	thread_iter := 125_000

	// Execute tests
	baseline_result := baseline_call_performance(base_iter)
	creation_results := benchmark_closure_creation(creation_iter)
	call_results := benchmark_closure_call(base_iter)
	thread_result := benchmark_threaded_creation(threads, thread_iter)
	mem_result := benchmark_memory_usage(mem_count)

	// Print result tables
	println('\n## 1. Closure Performance Analysis')
	println('| Test Name                 | Iterations | Time(ms) | Ops/sec      | Notes        |')
	print_results_table([baseline_result], '1. Performance Baseline')
	print_results_table(creation_results, '2. Closure Creation Performance')
	print_results_table(call_results, '3. Closure Call Performance')
	print_results_table([thread_result], '4. Multi-threaded Performance')

	// Print memory results
	println('\n## 2. Memory Overhead Analysis')
	println('| Test Name               | Closure Count | Start Mem(KB) | End Mem(KB) | Delta(KB) | Bytes/Closure |')
	println('|-------------------------|---------------|---------------|------------|-----------|---------------|')
	println('| ${mem_result.test_name:-20} | ${mem_result.count:13} | ${mem_result.start_mem_kb:13} | ${mem_result.end_mem_kb:10} | ${mem_result.delta_kb:9} | ${mem_result.bytes_per_closure:13} |')
	println('\n**Verification Sum: ${mem_result.check_sum}** (Calculated from random sample of 100 closures)')

	println('\n## Test Environment')
	println('- V Language Version: ${version.full_v_version(false)}')
	println('- CPU Cores: ${runtime.nr_cpus()}')
	println('- System Memory: ${runtime.total_memory()! / 1024 / 1024} MB')
	println('- Operating System: ${os.user_os()}')
	println('\n> Test Time: ${time.now().format_ss_micro()}')
}
