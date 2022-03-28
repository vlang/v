module main

// This program compares the performance of the native C.qsort function,
// used by the a.sort/0 method, vs the more flexible and platform independent
// (at the cost of being slightly slower) C.vqsort_r (adapted from musl),
// used by the a.sort_with_compare_context/2 method.
//
// Usage:
// ./v -prod -cc gcc cmd/tools/bench/sorting.v
// for seqkind in increasing decreasing random; do
//   for how in qsort aswcc; do
//     for x in 10_000_000 1000_000 100_000 10_000 1000 100 10 ; do
//       ./cmd/tools/bench/sorting $how $x $seqkind;
//     done;
//   done;
//   echo --------------------------------;
// done > cmd/tools/bench/sorting.results.txt
import os
import rand
import benchmark

fn my_context_compare_ints(const_a &int, const_b &int, context voidptr) int {
	res := *const_a - *const_b
	if res < 0 {
		return -1
	}
	if res > 0 {
		return 1
	}
	return 0
}

fn generate(kind string, mut a []int, maxn int) {
	match kind {
		'random' {
			for _ in 0 .. maxn {
				a << rand.int_in_range(1, maxn) or { 0 }
			}
		}
		'increasing' {
			for idx in 0 .. maxn {
				a << idx + 1
			}
		}
		'decreasing' {
			for idx in 1 .. maxn {
				a << maxn - idx
			}
		}
		else {}
	}
}

fn sort(how string, mut a []int) {
	match how {
		'aswcc' {
			a.sort_with_compare_context(my_context_compare_ints, voidptr(3))
		}
		'qsort' {
			a.sort()
		}
		else {}
	}
}

fn main() {
	rand.seed([u32(0), 1])
	how := os.args[1] or { 'qsort' }
	maxn := os.args[2] or { '1000' }.int()
	kind := os.args[3] or { 'random' }
	// println('> how: $how | maxn: $maxn')
	mut a := []int{cap: maxn}
	generate(kind, mut a, maxn)
	println('> a.len: ${a.len:12} | a: ${a#[0..7]:50} ... ${a#[-7..]:-50}')
	mut b := benchmark.start()
	sort(how, mut a)
	println('> a.len: ${a.len:12} | a: ${a#[0..7]:50} ... ${a#[-7..]:-50}')
	b.measure('$how $maxn $kind')
}
