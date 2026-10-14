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

// A _test.v file builds as a program whose main runs its tests in source order between
// testsuite_begin and testsuite_end: a failed assert reports itself and ends only its test,
// a test returning an error fails, and the exit status is 1 when any test failed.
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
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-d', 'fastc_real_builtin', '-o', program, test_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 1, run.output
	for line in ['begin', 'ok ran', 'calc_test.v:24: test_fails: assert: deliberate', 'FAIL: test_fails',
		'after ran', 'FAIL: test_result', 'end', '2 of 5 tests failed'] {
		assert run.output.contains(line), run.output
	}
	assert !run.output.contains('not reached'), run.output
	assert !run.output.contains('FAIL: test_propagates'), run.output
}

// Constructs blobly_emb's tests use, printed against the C backend's output: iterator
// for-ins, a C-style for without an update, `f(x).len` and `== []`, nested array equality,
// fixed arrays from raw storage and as `[N]T{init: v}`, an `or` on a slice bound, grouped
// and container globals, and `!` on a comparison operand.
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
	println('\${runes} \${sum} \${k} \${tail_two}')
	println('\${nested_equal} \${tail_empty} \${key_sum(h.key)} \${key_sum([4]u8{init: 2})}')
	println('\${first_word('hi there')} \${first_word('solo')} \${order.len} \${counts['x']} \${slots[2]} \${total()!}')
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-enable-globals', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['3 63 3 true', 'true true 4 8',
		'hi solo 1 1 9 1']
}
