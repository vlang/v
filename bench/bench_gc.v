// bench_gc.v - Benchmark comparing VGC vs Boehm GC
// Run with: v -gc boehm -o bench_boehm bench/bench_gc.v && ./bench_boehm
//           v -gc vgc   -o bench_vgc   bench/bench_gc.v && ./bench_vgc
import time

const n_allocs = 1_000_000
const n_iters = 5

struct Node {
mut:
	left  &Node = unsafe { nil }
	right &Node = unsafe { nil }
	value int
}

// Allocate many small objects (typical GC workload)
fn bench_small_allocs() {
	sw := time.new_stopwatch()
	mut sum := 0
	for _ in 0 .. n_allocs {
		s := 'hello world ${sum}'
		sum += s.len
	}
	elapsed := sw.elapsed()
	eprintln('  small allocs (${n_allocs}x string): ${elapsed.milliseconds()} ms  (sum=${sum})')
}

// Build a binary tree (tests pointer-heavy allocation)
fn make_tree(depth int) &Node {
	if depth <= 0 {
		return &Node{
			value: 1
		}
	}
	return &Node{
		left:  make_tree(depth - 1)
		right: make_tree(depth - 1)
		value: depth
	}
}

fn tree_sum(n &Node) int {
	if n == unsafe { nil } {
		return 0
	}
	return n.value + tree_sum(n.left) + tree_sum(n.right)
}

fn bench_tree() {
	sw := time.new_stopwatch()
	mut total := 0
	for _ in 0 .. 10 {
		t := make_tree(18)
		total += tree_sum(t)
	}
	elapsed := sw.elapsed()
	eprintln('  tree build+walk (depth=18, 10x): ${elapsed.milliseconds()} ms  (sum=${total})')
}

// Allocate and discard arrays (tests realloc / growing)
fn bench_arrays() {
	sw := time.new_stopwatch()
	mut total := 0
	for _ in 0 .. 100 {
		mut arr := []int{cap: 16}
		for j in 0 .. 100_000 {
			arr << j
		}
		total += arr.len
	}
	elapsed := sw.elapsed()
	eprintln('  array grow (100x 100k pushes): ${elapsed.milliseconds()} ms  (total=${total})')
}

fn main() {
	gc_mode := $if gcboehm ? {
		'boehm'
	} $else $if vgc ? {
		'vgc'
	} $else {
		'none'
	}
	eprintln('=== GC Benchmark (mode: ${gc_mode}) ===')

	for iter in 0 .. n_iters {
		eprintln('--- iteration ${iter + 1}/${n_iters} ---')
		bench_small_allocs()
		bench_tree()
		bench_arrays()
	}

	usage := gc_heap_usage()
	eprintln('--- heap usage ---')
	eprintln('  heap_size:  ${usage.heap_size / 1024} KB')
	eprintln('  free_bytes: ${usage.free_bytes / 1024} KB')
}
