import x.json2
import benchmark

// Performance comparison test focusing on small loops
// This addresses the original issue: "slower in small loop. I guess it is `fulfill_nodes` related"
// Run with: v -prod -cc gcc run vlib/x/json2/tests/performance_comparison.v

const small_loop_iterations = 10 // Original problematic case
const medium_loop_iterations = 100
const large_loop_iterations = 1_000_000

struct TestStruct {
	val  int
	val2 string
	val3 TestStruct2
}

struct TestStruct2 {
	a int
	b string
}

fn benchmark_small_loop() ! {
	json_data := '{"val": 1, "val2": "test", "val3": {"a": 2, "b": "nested"}}'
	
	println('\n=== Small Loop Benchmark (${small_loop_iterations} iterations) ===')
	println('This was the original problematic case mentioned in bench.v')
	
	mut b := benchmark.start()
	
	for i := 0; i < small_loop_iterations; i++ {
		_ := json2.decode[TestStruct](json_data)!
	}
	
	b.measure('json2.decode[TestStruct] - Small loop (OPTIMIZED)')
}

fn benchmark_medium_loop() ! {
	json_data := '{"val": 1, "val2": "test", "val3": {"a": 2, "b": "nested"}}'
	
	println('\n=== Medium Loop Benchmark (${medium_loop_iterations} iterations) ===')
	
	mut b := benchmark.start()
	
	for i := 0; i < medium_loop_iterations; i++ {
		_ := json2.decode[TestStruct](json_data)!
	}
	
	b.measure('json2.decode[TestStruct] - Medium loop (OPTIMIZED)')
}

fn benchmark_large_loop() ! {
	json_data := '{"val": 1, "val2": "test", "val3": {"a": 2, "b": "nested"}}'
	
	println('\n=== Large Loop Benchmark (${large_loop_iterations} iterations) ===')
	
	mut b := benchmark.start()
	
	for i := 0; i < large_loop_iterations; i++ {
		_ := json2.decode[TestStruct](json_data)!
	}
	
	b.measure('json2.decode[TestStruct] - Large loop (OPTIMIZED)')
}

fn benchmark_array_small_loop() ! {
	json_data := '[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]'
	
	println('\n=== Array Decode - Small Loop (${small_loop_iterations} iterations) ===')
	
	mut b := benchmark.start()
	
	for i := 0; i < small_loop_iterations; i++ {
		_ := json2.decode[[]int](json_data)!
	}
	
	b.measure('json2.decode[[]int] - Small loop (OPTIMIZED)')
}

fn benchmark_array_large_loop() ! {
	json_data := '[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]'
	
	println('\n=== Array Decode - Large Loop (${large_loop_iterations} iterations) ===')
	
	mut b := benchmark.start()
	
	for i := 0; i < large_loop_iterations; i++ {
		_ := json2.decode[[]int](json_data)!
	}
	
	b.measure('json2.decode[[]int] - Large loop (OPTIMIZED)')
}

fn main() {
	println('=== JSON2 Performance Comparison Test ===')
	println('Testing OPTIMIZED array-based implementation')
	println('Original issue: "slower in small loop. I guess it is `fulfill_nodes` related"')
	println('')
	
	benchmark_small_loop() or { panic(err) }
	benchmark_medium_loop() or { panic(err) }
	benchmark_large_loop() or { panic(err) }
	benchmark_array_small_loop() or { panic(err) }
	benchmark_array_large_loop() or { panic(err) }
	
	println('\n=== Summary ===')
	println('These are the OPTIMIZED results using array-based storage.')
	println('To compare with original:')
	println('  1. Checkout the original version: git checkout HEAD~1')
	println('  2. Run this same test')
	println('  3. Compare the "Small loop" results - this was the bottleneck')
	println('')
	println('Expected improvements:')
	println('  - Small loops: 2-5x faster (addressing fulfill_nodes bottleneck)')
	println('  - Better cache locality from array-based storage')
	println('  - Reduced pointer chasing overhead')
}
