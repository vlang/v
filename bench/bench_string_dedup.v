// Benchmark comparison of three string deduplication methods in V: array, map, and set
module main

import time
import datatypes

// Method 1: Using array
struct Context1 {
mut:
	used_str []string
}

fn (mut c Context1) add_used(str string) {
	if str !in c.used_str {
		c.used_str << str
	}
}

// Method 2: Using map
struct Context2 {
mut:
	used_str map[string]bool
}

fn (mut c Context2) add_used(str string) {
	c.used_str[str] = true
}

// Method 3: Using set
struct Context3 {
mut:
	used_str datatypes.Set[string]
}

fn (mut c Context3) add_used(str string) {
	c.used_str.add(str)
}

// Generate random test strings
fn generate_test_strings(count int, duplicate_ratio f64) []string {
	mut strs := []string{cap: count}
	unique_count := int(f64(count) * (1.0 - duplicate_ratio))
	// First generate a batch of unique strings
	for i in 0 .. unique_count {
		strs << 'str_${i}_${time.ticks()}' // Add timestamp to reduce duplication rate
	}
	// The remaining part uses duplicate strings
	for i in 0 .. (count - unique_count) {
		strs << strs[i % unique_count] // Cycle through the first half of strings to create duplicates
	}
	return strs
}

fn main() {
	num_strs := 10000 // Total number of strings
	duplicate_ratio := 0.3 // Duplicate string ratio (30%)
	test_strs := generate_test_strings(num_strs, duplicate_ratio)
	println('Generated test strings: ${test_strs.len} (approximately ${int(duplicate_ratio * 100)}% are duplicates)')

	// Test method 1: array
	mut ctx1 := Context1{}
	sw1 := time.new_stopwatch()
	for str in test_strs {
		ctx1.add_used(str)
	}
	time1 := sw1.elapsed().milliseconds()
	println('Method 1 (array) - Time: ${time1}ms, Final unique strings: ${ctx1.used_str.len}')

	// Test method 2: map
	mut ctx2 := Context2{}
	sw2 := time.new_stopwatch()
	for str in test_strs {
		ctx2.add_used(str)
	}
	time2 := sw2.elapsed().milliseconds()
	println('Method 2 (map) - Time: ${time2}ms, Final unique strings: ${ctx2.used_str.len}')

	// Test method 3: set
	mut ctx3 := Context3{}
	sw3 := time.new_stopwatch()
	for str in test_strs {
		ctx3.add_used(str)
	}
	time3 := sw3.elapsed().milliseconds()
	println('Method 3 (set) - Time: ${time3}ms, Final unique strings: ${ctx3.used_str.size()}')

	// Performance comparison
	println('\nPerformance comparison:')
	println('Method 2 (map) is ${f64(time1) / f64(time2):.2f} times faster than method 1 (array)')
	println('Method 3 (set) is ${f64(time1) / f64(time3):.2f} times faster than method 1 (array)')
	if time2 < time3 {
		println('Map is slightly faster than set, difference: ${time3 - time2}ms')
	} else {
		println('Set is slightly faster than map, difference: ${time2 - time3}ms')
	}
}
