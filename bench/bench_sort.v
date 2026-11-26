module main

import benchmark
import arrays
import rand
import time

fn make_data(n int, kind string) []int {
	mut a := []int{len: n}
	match kind {
		'random' {
			for i in 0 .. n {
				a[i] = rand.intn(1000000) or { 0 }
			}
		}
		'sorted' {
			for i in 0 .. n {
				a[i] = i
			}
		}
		'reverse' {
			for i in 0 .. n {
				a[i] = n - i
			}
		}
		'few_unique' {
			for i in 0 .. n {
				a[i] = i % 10
			}
		}
		else {}
	}
	return a
}

fn main() {
	n := 100000
	kinds := ['random', 'sorted', 'reverse', 'few_unique']
	mut b := benchmark.new_benchmark()
	b.set_total_expected_steps(kinds.len)
	for kind in kinds {
		data := make_data(n, kind)
		mut arr := data.clone()
		sw := time.new_stopwatch()
		arrays.stable_sort(mut arr, fn (x int, y int) bool { return x < y })
		elapsed := sw.elapsed().milliseconds()
		println('${kind}: ${elapsed} ms')
		b.record_measure(kind)
	}
	b.stop()
}
