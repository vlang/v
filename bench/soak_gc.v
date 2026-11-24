// soak_gc.v - S0 stability soak harness for vgc (tracker vlang/v#32525).
//
// Compiles once per GC mode and runs a single scenario per invocation:
//   v -gc vgc -prod -o /tmp/soak_vgc bench/soak_gc.v
//   /tmp/soak_vgc --scenario tree --depth 13 --rounds 3
//   v -gc vgc -d vgc_greentea -prod -o /tmp/soak_gt bench/soak_gc.v
//   /tmp/soak_gt --scenario mutate --depth 13 --rounds 5 --seed 42
//
// The matrix driver (bench/soak_gc_matrix.v) compiles the modes and runs
// the cross-product. This file stays single-scenario so a crash log names
// the exact case via the SCENARIO / PASS lines. The fork scenario is a
// no-op BLOCKED on Windows (os.fork unsupported) and exercises post-fork
// collect on Unix.
module main

import os

struct SoakNode {
mut:
	left  &SoakNode = unsafe { nil }
	right &SoakNode = unsafe { nil }
	value int
}

@[noinline]
fn build_soak_tree(depth int) &SoakNode {
	if depth <= 0 {
		return &SoakNode{
			value: 1
		}
	}
	return &SoakNode{
		left:  build_soak_tree(depth - 1)
		right: build_soak_tree(depth - 1)
		value: depth
	}
}

@[noinline]
fn count_soak_tree(n &SoakNode) int {
	if isnil(n) {
		return 0
	}
	return 1 + count_soak_tree(n.left) + count_soak_tree(n.right)
}

@[noinline]
fn sum_soak_tree(n &SoakNode) int {
	if isnil(n) {
		return 0
	}
	return n.value + sum_soak_tree(n.left) + sum_soak_tree(n.right)
}

fn expected_count(depth int) int {
	return (1 << (depth + 1)) - 1
}

fn scenario_tree(depth int, rounds int) {
	want := expected_count(depth)
	for r in 0 .. rounds {
		root := build_soak_tree(depth)
		c0 := count_soak_tree(root)
		s0 := sum_soak_tree(root)
		assert c0 == want
		gc_collect()
		c1 := count_soak_tree(root)
		s1 := sum_soak_tree(root)
		assert c1 == want
		assert s1 == s0
		_ = r
	}
}

fn scenario_mutate(depth int, rounds int, seed u64) {
	// Rebuild one child per round so old subtrees become garbage while the
	// rest stays live. This exercises the span-queue re-enqueue path: the
	// scan of the representative can mark a sibling in the same span.
	mut state := seed
	if state == 0 {
		state = 0x12345678
	}
	mut root := build_soak_tree(depth)
	gc_collect()
	assert count_soak_tree(root) == expected_count(depth)
	for r in 0 .. rounds {
		// Simple LCG, no imports needed and fully deterministic per seed.
		state = state * u64(6364136223846793005) + u64(1442695040888963407)
		if (state >> 33) & 1 == 0 {
			root.left = build_soak_tree(depth - 1)
		} else {
			root.right = build_soak_tree(depth - 1)
		}
		gc_collect()
		assert !isnil(root)
		assert !isnil(root.left)
		assert !isnil(root.right)
		assert count_soak_tree(root) == expected_count(depth)
		_ = r
	}
}

fn thread_work(depth int) int {
	root := build_soak_tree(depth)
	s0 := sum_soak_tree(root)
	gc_collect()
	assert count_soak_tree(root) == expected_count(depth)
	assert sum_soak_tree(root) == s0
	return s0
}

fn scenario_threads(nthreads int, depth int) {
	if nthreads <= 1 {
		assert thread_work(depth) == thread_work(depth)
		return
	}
	mut threads := []thread int{cap: nthreads}
	for _ in 0 .. nthreads {
		threads << spawn thread_work(depth)
	}
	totals := threads.wait()
	want := thread_work(depth)
	for t in totals {
		assert t == want
	}
}

fn scenario_fork(depth int) {
	$if windows {
		println('BLOCKED scenario=fork reason=os.fork unsupported on Windows')
		// Skip the trailing PASS: a platform skip is BLOCKED, not a pass.
		exit(0)
	} $else {
		root := build_soak_tree(depth)
		s0 := sum_soak_tree(root)
		gc_collect()
		assert count_soak_tree(root) == expected_count(depth)
		// Flush before forking so the child does not re-emit buffered lines.
		flush_stdout()
		pid := os.fork()
		if pid == 0 {
			// Child: the inherited heap must still verify and collect.
			assert count_soak_tree(root) == expected_count(depth)
			assert sum_soak_tree(root) == s0
			gc_collect()
			assert count_soak_tree(root) == expected_count(depth)
			exit(0)
		} else if pid > 0 {
			mut status := 0
			os.wait4(pid, &status, 0, unsafe { nil })
			assert status == 0
			assert count_soak_tree(root) == expected_count(depth)
			gc_collect()
		} else {
			eprintln('fork failed')
			exit(1)
		}
	}
}

fn arg_val(name string, fallback string) string {
	args := os.args
	for i, a in args {
		if a == name && i + 1 < args.len {
			return args[i + 1]
		}
		if a.starts_with(name + '=') {
			return a[name.len + 1..]
		}
	}
	return fallback
}

fn main() {
	scenario := arg_val('--scenario', 'tree')
	depth := arg_val('--depth', '13').int()
	rounds := arg_val('--rounds', '3').int()
	nthreads := arg_val('--threads', '4').int()
	seed := arg_val('--seed', '42').u64()
	println('SCENARIO scenario=${scenario} depth=${depth} rounds=${rounds} threads=${nthreads} seed=${seed}')
	if depth < 0 || depth > 24 {
		eprintln('depth out of range 0..24: ${depth}')
		exit(1)
	}
	if rounds < 1 {
		eprintln('rounds must be >= 1')
		exit(1)
	}
	match scenario {
		'tree' {
			scenario_tree(depth, rounds)
		}
		'mutate' {
			scenario_mutate(depth, rounds, seed)
		}
		'threads' {
			scenario_threads(nthreads, depth)
		}
		'fork' {
			scenario_fork(depth)
		}
		else {
			eprintln('unknown scenario: ${scenario} (want tree|mutate|threads|fork)')
			exit(1)
		}
	}
	println('PASS scenario=${scenario} depth=${depth} rounds=${rounds} threads=${nthreads} seed=${seed}')
}
