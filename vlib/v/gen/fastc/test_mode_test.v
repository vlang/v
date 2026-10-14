module fastc

import os
import v.cmdexec

fn fastc_test_mode_root(name string) string {
	root := os.join_path(os.vtmp_dir(), 'fastc_test_mode_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_test_mode' }") or {
		panic(err)
	}
	return root
}

// A _test.v file builds, on the full runtime without asking for it, as a program whose main
// runs its tests in source order between testsuite_begin and testsuite_end: a failed assert
// reports itself and ends only its test, a test returning an error fails with its message as
// the C harness reports it, and the exit status is 1 when any test failed.
fn test_fastc_runs_a_test_file() {
	$if !macos && !linux {
		return
	}
	root := fastc_test_mode_root('runner')
	defer {
		os.rmdir_all(root) or {}
	}
	test_file := os.join_path(root, 'calc_test.v')
	os.write_file(test_file, "module main

fn may(n int) !int {
	if n < 0 {
		return error('negative')
	}
	return n
}

fn testsuite_begin() {
	println('begin')
}

fn testsuite_end() {
	println('end')
}

fn test_ok() {
	assert 1 + 1 == 2
	println('ok ran')
}

fn test_fails() {
	assert 2 == 3, 'deliberate'
	println('not reached')
}

fn test_after() {
	println('after ran')
}

fn test_result() ! {
	_ := may(-1)!
	println('not reached')
}

fn test_propagates() {
	x := may(-2) or { 0 }
	assert x == 0
	assert may(5)! == 5
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, test_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 1, run.output
	for line in ['begin', 'ok ran', 'calc_test.v:24: test_fails: assert: deliberate', 'FAIL: test_fails',
		'after ran', 'calc_test.v:33: fn test_result failed propagation with error: negative',
		'FAIL: test_result', 'end', '2 of 5 tests failed'] {
		assert run.output.contains(line), run.output
	}
	assert !run.output.contains('not reached'), run.output
	assert !run.output.contains('FAIL: test_propagates'), run.output
}

// A backend-qualified test file runs too, each test between before_each and after_each: a
// failed before_each skips its test and after_each still runs. The file's own main is an
// ordinary function a test can call.
fn test_fastc_test_file_hooks_and_own_main() {
	$if !macos && !linux {
		return
	}
	root := fastc_test_mode_root('hooks')
	defer {
		os.rmdir_all(root) or {}
	}
	test_file := os.join_path(root, 'hooks_test.c.v')
	os.write_file(test_file, "module main

__global (
	runs int
)

fn main() {
	println('own main')
}

fn before_each() {
	runs++
	println('before \${runs}')
	assert runs != 2, 'setup'
}

fn after_each() {
	println('after \${runs}')
}

fn test_one() {
	main()
}

fn test_skipped() {
	println('not reached')
}

fn test_three() {
	println('three')
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-enable-globals', '-o', program, test_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run_in_merged(program, [], '')
	assert run.exit_code == 1, run.output
	assert run.output.trim_space().split_into_lines().map(it.all_after('hooks_test.c.v:')) == [
		'before 1',
		'own main',
		'after 1',
		'before 2',
		'14: before_each: assert: setup',
		'after 2',
		'FAIL: test_skipped',
		'before 3',
		'three',
		'after 3',
		'1 of 3 tests failed',
	], run.output
}

// Constructs blobly_emb's tests use, printed against the C backend's output: iterator
// for-ins, a C-style for without an update, `f(x).len` and `== []`, nested array equality,
// fixed arrays from raw storage and as `[N]T{init: v}`, an `or` on a slice bound, grouped
// and container globals, `!` on a comparison operand, and equality of arrays of fixed arrays.
fn test_fastc_test_suite_constructs_match_the_c_backend() {
	$if !macos && !linux {
		return
	}
	root := fastc_test_mode_root('constructs')
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

__global (
	order  []int
	counts map[string]int
	slots  [4]u8
)

struct Count {
mut:
	i int
	n int
}

fn (mut c Count) next() ?int {
	if c.i >= c.n {
		return none
	}
	c.i++
	return c.i * 10
}

struct Holder {
mut:
	key [4]u8
}

fn tail(req []u8) []u8 {
	return req[1..]
}

fn key_sum(k [4]u8) int {
	mut s := 0
	for b in k {
		s += b
	}
	return s
}

fn first_word(s string) string {
	return s[0..s.index(' ') or { s.len }]
}

fn half(n int) !int {
	if n % 2 != 0 {
		return error('odd')
	}
	return n / 2
}

fn total() !int {
	return if half(8)! == 4 { 1 } else { 0 }
}

fn main() {
	mut runes := 0
	for r in 'aé b'.runes_iterator() {
		if r != ` ` {
			runes++
		}
	}
	mut sum := 0
	for i, v in Count{
		n: 3
	} {
		sum += v + i
	}
	mut k := 0
	for ; k < 3; {
		k++
	}
	order << 5
	counts['x']++
	slots[2] = 9
	mut h := Holder{}
	h.key[1] = 4
	nested := [[u8(1), 2], [u8(3)]]
	tail_two := tail([u8(1), 2, 3]).len == 2
	nested_equal := nested == [[u8(1), 2], [u8(3)]]
	tail_empty := tail([u8(1)]) == []
	pairs := [[1, 2]!, [3, 4]!]
	grid := [pairs]
	println('\${runes} \${sum} \${k} \${tail_two}')
	println('\${nested_equal} \${tail_empty} \${key_sum(h.key)} \${key_sum([4]u8{init: 2})}')
	println('\${first_word('hi there')} \${first_word('solo')} \${order.len} \${counts['x']} \${slots[2]} \${total()!}')
	println('\${pairs == [[1, 2]!, [3, 4]!]} \${pairs == [[1, 2]!, [3, 5]!]} \${grid == [[[1, 2]!, [3, 4]!]]}')
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-enable-globals', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['3 63 3 true', 'true true 4 8',
		'hi solo 1 1 9 1', 'true false true']
}
