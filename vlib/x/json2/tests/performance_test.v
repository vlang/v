import x.json2
import benchmark

// Performance test to establish baseline and measure improvements
// Run with: v -prod -cc gcc run vlib/x/json2/tests/performance_test.v

const small_iterations = 100_000
const medium_iterations = 10_000
const large_iterations = 1_000

struct TestData {
	val  int
	val2 string
	val3 TestData2
}

struct TestData2 {
	a int
	b string
}

fn test_small_json() {
	json_data := '{"val": 1, "val2": "test", "val3": {"a": 2, "b": "nested"}}'
	
	mut b := benchmark.start()
	
	for i := 0; i < small_iterations; i++ {
		_ := json2.decode[TestData](json_data)!
	}
	
	b.measure('Small JSON decode (${small_iterations} iterations)')
}

fn test_medium_json() {
	json_data := '{"val": 1, "val2": "test", "val3": {"a": 2, "b": "nested"}, "arr": [1, 2, 3, 4, 5], "map": {"key1": "value1", "key2": "value2"}}'
	
	mut b := benchmark.start()
	
	for i := 0; i < medium_iterations; i++ {
		_ := json2.decode[map[string]json2.Any](json_data)!
	}
	
	b.measure('Medium JSON decode (${medium_iterations} iterations)')
}

fn test_large_json() {
	// Create a larger JSON structure
	mut json_parts := []string{}
	json_parts << '{"items": ['
	for i := 0; i < 100; i++ {
		if i > 0 {
			json_parts << ','
		}
		json_parts << '{"id": ${i}, "name": "item${i}", "value": ${i * 10}}'
	}
	json_parts << ']}'
	json_data := json_parts.join('')
	
	mut b := benchmark.start()
	
	for i := 0; i < large_iterations; i++ {
		_ := json2.decode[map[string]json2.Any](json_data)!
	}
	
	b.measure('Large JSON decode (${large_iterations} iterations)')
}

fn test_array_decode() {
	json_data := '[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]'
	
	mut b := benchmark.start()
	
	for i := 0; i < small_iterations; i++ {
		_ := json2.decode[[]int](json_data)!
	}
	
	b.measure('Array decode (${small_iterations} iterations)')
}

fn test_nested_structures() {
	json_data := '{"level1": {"level2": {"level3": {"level4": {"value": 42}}}}}'
	
	mut b := benchmark.start()
	
	for i := 0; i < medium_iterations; i++ {
		_ := json2.decode[map[string]json2.Any](json_data)!
	}
	
	b.measure('Nested structures decode (${medium_iterations} iterations)')
}

fn main() {
	println('=== JSON2 Performance Test Suite ===')
	println('Establishing baseline performance metrics...')
	println('')
	
	test_small_json()
	test_medium_json()
	test_large_json()
	test_array_decode()
	test_nested_structures()
	
	println('')
	println('Performance test completed.')
	println('Run this test again after optimization to compare results.')
}
