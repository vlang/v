import os
import rand
import benchmark

// Purpose: this script measures the performance of common lookup operations on the builtin map, vs the builtin array type in V.
// Usage:
//   MAX_ITEMS=1000 MAX_ITERATIONS=1_000_000 v -prod -cc gcc-11 crun vlib/v/tests/bench/bench_string_key_in_map_vs_string_value_in_array.v
//
// Typical result on Intel i3, Ubuntu 20.04:
//
// > max_iterations: 1000000 over max_items: 1000
// > m.len: 1001 | a_as_first.len: 1001 | a_as_last.len: 1001 | a_as_middle.len: 1001
// SPENT    17.344 ms in sum: 1000000 | m[key]
// SPENT    13.402 ms in sum: 1000000 | key in m
// SPENT   643.429 ms in sum: 1000000 | key in a_as_last
// SPENT   319.897 ms in sum: 1000000 | key in a_as_middle
// SPENT     1.842 ms in sum: 1000000 | key in a_as_first

fn main() {
	max_iterations := os.getenv_opt('MAX_ITERATIONS') or { '1000' }.int()
	max_items := os.getenv_opt('MAX_ITEMS') or { '100' }.int()
	eprintln('> max_iterations: ${max_iterations} over max_items: ${max_items}')
	key := 'abc'
	mut sum := 0
	mut m := map[string]bool{}
	m[key] = true
	for _ in 0 .. max_items {
		m[rand.string(20)] = true
	}
	a_as_first := m.keys()
	a_as_last := m.keys().reverse()
	mut a_as_middle := a_as_first[1..a_as_first.len / 2].clone()
	a_as_middle << key
	a_as_middle << a_as_first[a_as_first.len / 2..]
	eprintln('> m.len: ${m.len} | a_as_first.len: ${a_as_first.len} | a_as_last.len: ${a_as_last.len} | a_as_middle.len: ${a_as_middle.len}')

	mut b := benchmark.start()

	sum = 0
	for _ in 0 .. max_iterations {
		sum += int(m[key])
	}
	b.measure('sum: ${sum} | m[key]')

	sum = 0
	for _ in 0 .. max_iterations {
		sum += int(key in m)
	}
	b.measure('sum: ${sum} | key in m')

	b = benchmark.start()

	sum = 0
	for _ in 0 .. max_iterations {
		sum += int(key in a_as_last)
	}
	b.measure('sum: ${sum} | key in a_as_last')

	sum = 0
	for _ in 0 .. max_iterations {
		sum += int(key in a_as_middle)
	}
	b.measure('sum: ${sum} | key in a_as_middle')

	sum = 0
	for _ in 0 .. max_iterations {
		sum += int(key in a_as_first)
	}
	b.measure('sum: ${sum} | key in a_as_first')
}
