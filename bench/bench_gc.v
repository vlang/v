// bench_gc.v - Benchmark comparing Boehm GC vs V GC (vgc)
//
// Usage (runner mode - compiles and compares both):
//   v run bench/bench_gc.v
//
// Usage (individual, to test a specific GC mode):
//   v -gc boehm -prod -o /tmp/bench_boehm bench/bench_gc.v && /tmp/bench_boehm
//   v -gc vgc   -prod -o /tmp/bench_vgc   bench/bench_gc.v && /tmp/bench_vgc
module main

import os
import time

const n_allocs = 1_000_000
const n_iters = 5
const tree_depth = 18
const tree_rounds = 10
const array_rounds = 100
const array_size = 100_000

struct Node {
mut:
	left  &Node = unsafe { nil }
	right &Node = unsafe { nil }
	value int
}

fn gc_mode_name() string {
	return $if gcboehm ? {
		'boehm'
	} $else $if vgc ? {
		'vgc'
	} $else {
		'none'
	}
}

// --- Workloads ---

// Allocate many small objects (typical GC workload)
fn bench_small_allocs() (i64, int) {
	sw := time.new_stopwatch()
	mut sum := 0
	for _ in 0 .. n_allocs {
		s := 'hello world ${sum}'
		sum += s.len
	}
	return sw.elapsed().milliseconds(), sum
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

fn bench_tree() (i64, int) {
	sw := time.new_stopwatch()
	mut total := 0
	for _ in 0 .. tree_rounds {
		t := make_tree(tree_depth)
		total += tree_sum(t)
	}
	return sw.elapsed().milliseconds(), total
}

// Allocate and discard arrays (tests realloc / growing)
fn bench_arrays() (i64, int) {
	sw := time.new_stopwatch()
	mut total := 0
	for _ in 0 .. array_rounds {
		mut arr := []int{cap: 16}
		for j in 0 .. array_size {
			arr << j
		}
		total += arr.len
	}
	return sw.elapsed().milliseconds(), total
}

// Allocate many maps
fn bench_maps() (i64, int) {
	sw := time.new_stopwatch()
	mut total := 0
	for round in 0 .. 20 {
		mut m := map[string]int{}
		for j in 0 .. 10_000 {
			m['key_${round}_${j}'] = j
		}
		total += m.len
	}
	return sw.elapsed().milliseconds(), total
}

// Mixed workload: interleave allocations and collections
fn bench_mixed() (i64, int) {
	sw := time.new_stopwatch()
	mut total := 0
	for _ in 0 .. 50 {
		// strings
		mut strs := []string{cap: 1000}
		for j in 0 .. 1000 {
			strs << 'item_${j}_${'x'.repeat(j % 50)}'
		}
		total += strs.len
		// tree
		t := make_tree(12)
		total += tree_sum(t)
		// array
		mut arr := []int{cap: 10_000}
		for j in 0 .. 10_000 {
			arr << j * 2
		}
		total += arr.len
	}
	return sw.elapsed().milliseconds(), total
}

// --- Workload runner (compiled with a specific GC) ---

fn run_workload() {
	mode := gc_mode_name()
	for iter in 0 .. n_iters {
		ms_alloc, sum_alloc := bench_small_allocs()
		ms_tree, sum_tree := bench_tree()
		ms_array, sum_array := bench_arrays()
		ms_map, sum_map := bench_maps()
		ms_mixed, sum_mixed := bench_mixed()

		// Machine-readable output: iter,test,ms,checksum
		println('${iter},allocs,${ms_alloc},${sum_alloc}')
		println('${iter},tree,${ms_tree},${sum_tree}')
		println('${iter},arrays,${ms_array},${sum_array}')
		println('${iter},maps,${ms_map},${sum_map}')
		println('${iter},mixed,${ms_mixed},${sum_mixed}')
	}
	usage := gc_heap_usage()
	println('heap,${usage.heap_size},${usage.free_bytes}')
	println('gc_mode,${mode}')
}

// --- Runner mode: compile with both GCs and compare ---

struct BenchData {
mut:
	times [5][]i64 // indexed by test (allocs=0, tree=1, arrays=2, maps=3, mixed=4)
	heap  i64
	free  i64
	mode  string
}

fn test_index(name string) int {
	return match name {
		'allocs' { 0 }
		'tree' { 1 }
		'arrays' { 2 }
		'maps' { 3 }
		'mixed' { 4 }
		else { -1 }
	}
}

fn test_name(idx int) string {
	return ['small allocs (${n_allocs}x string)',
		'tree build+walk (depth=${tree_depth}, ${tree_rounds}x)',
		'array grow (${array_rounds}x ${array_size} pushes)', 'map insert (20x 10k entries)',
		'mixed workload (50 rounds)'][idx]
}

fn median(mut vals []i64) i64 {
	if vals.len == 0 {
		return 0
	}
	vals.sort(a < b)
	return vals[vals.len / 2]
}

fn parse_output(output string) BenchData {
	mut d := BenchData{}
	for line in output.split_into_lines() {
		parts := line.split(',')
		if parts.len < 2 {
			continue
		}
		if parts[0] == 'heap' && parts.len >= 3 {
			d.heap = parts[1].i64()
			d.free = parts[2].i64()
		} else if parts[0] == 'gc_mode' {
			d.mode = parts[1]
		} else if parts.len >= 4 {
			ti := test_index(parts[1])
			if ti >= 0 {
				d.times[ti] << parts[2].i64()
			}
		}
	}
	return d
}

fn rpad(s string, width int) string {
	if s.len >= width {
		return s
	}
	return s + ' '.repeat(width - s.len)
}

fn lpad(s string, width int) string {
	if s.len >= width {
		return s
	}
	return ' '.repeat(width - s.len) + s
}

fn run_comparison() {
	v_exe := os.getenv_opt('VEXE') or { @VEXE }
	src := os.join_path(@VMODROOT, 'bench', 'bench_gc.v')
	tmp_boehm := os.join_path(os.temp_dir(), 'bench_gc_boehm')
	tmp_vgc := os.join_path(os.temp_dir(), 'bench_gc_vgc')

	println('=== GC Benchmark: Boehm vs VGC ===')
	println('')

	// Compile both
	for gc_mode in ['boehm', 'vgc'] {
		out := if gc_mode == 'boehm' { tmp_boehm } else { tmp_vgc }
		print('compiling with -gc ${gc_mode}...')
		flush_stdout()
		r := os.execute('${v_exe} -gc ${gc_mode} -prod -o ${out} ${src}')
		if r.exit_code != 0 {
			eprintln(' FAILED')
			eprintln(r.output)
			exit(1)
		}
		println(' ok')
	}

	println('')

	// Run both
	mut data := [2]BenchData{}
	for i, bin in [tmp_boehm, tmp_vgc] {
		gc_name := if i == 0 { 'boehm' } else { 'vgc' }
		print('running ${gc_name}...')
		flush_stdout()
		r := os.execute('${bin} --run-workload')
		if r.exit_code != 0 {
			eprintln(' FAILED')
			eprintln(r.output)
			exit(1)
		}
		data[i] = parse_output(r.output)
		println(' done')
	}

	// Print comparison table
	println('')
	println('  ${rpad('test', 44)} ${lpad('boehm', 9)} ${lpad('vgc', 9)} ${lpad('ratio',
		9)}')
	println('  ${'—'.repeat(44)} ${'—'.repeat(9)} ${'—'.repeat(9)} ${'—'.repeat(9)}')

	for ti in 0 .. 5 {
		mut boehm_vals := data[0].times[ti].clone()
		mut vgc_vals := data[1].times[ti].clone()
		mb := median(mut boehm_vals)
		mv := median(mut vgc_vals)
		ratio := if mb > 0 { f64(mv) / f64(mb) } else { 0.0 }
		winner := if ratio < 0.95 {
			' <-- vgc'
		} else if ratio > 1.05 {
			' <-- boehm'
		} else {
			''
		}
		label := '${ratio:.2f}x${winner}'
		println('  ${rpad(test_name(ti), 44)} ${lpad('${mb} ms', 9)} ${lpad('${mv} ms',
			9)} ${lpad(label, 9)}')
	}

	// Heap usage
	println('')
	println('  heap usage:')
	println('    boehm: ${data[0].heap / 1024} KB allocated, ${data[0].free / 1024} KB free')
	println('    vgc:   ${data[1].heap / 1024} KB allocated, ${data[1].free / 1024} KB free')
	println('')
	println('  (ratio < 1.0 = vgc faster, > 1.0 = boehm faster)')

	// Cleanup
	os.rm(tmp_boehm) or {}
	os.rm(tmp_vgc) or {}
}

fn main() {
	if '--run-workload' in os.args {
		// We were launched by the runner - execute the workload
		run_workload()
	} else {
		// Act as runner: compile with both GCs and compare
		run_comparison()
	}
}
