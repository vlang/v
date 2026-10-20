module fastc

import os
import v.cmdexec
import v.pref

// An operator overload and an ordinary method named the way C spells the operator
// (`+` and `plus`, `==` and `eq`) must get different C names. FastC does not
// lower `**` expressions, so `**` is only declared here.
fn test_fastc_operator_and_method_named_like_its_c_spelling() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_operator_method_name_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

struct Num {
	v int
}

fn (a Num) + (b Num) Num {
	return Num{a.v + b.v}
}

fn (a Num) - (b Num) Num {
	return Num{a.v - b.v}
}

fn (a Num) * (b Num) Num {
	return Num{a.v * b.v}
}

fn (a Num) / (b Num) Num {
	return Num{a.v / b.v}
}

fn (a Num) % (b Num) Num {
	return Num{a.v % b.v}
}

fn (a Num) ** (b Num) Num {
	mut r := 1
	for _ in 0 .. b.v {
		r *= a.v
	}
	return Num{r}
}

fn (a Num) == (b Num) bool {
	return a.v == b.v
}

fn (a Num) < (b Num) bool {
	return a.v < b.v
}

fn (a Num) plus(n int) int {
	return a.v + n + 1000
}

fn (a Num) minus(n int) int {
	return a.v - n + 1000
}

fn (a Num) mul(n int) int {
	return a.v * n + 1000
}

fn (a Num) pow(n int) int {
	return a.v + n * 2 + 1000
}

fn (a Num) mul_(n int) int {
	return a.v * n * n + 1000
}

fn (a Num) div(n int) int {
	return a.v / n + 1000
}

fn (a Num) mod(n int) int {
	return a.v % n + 1000
}

fn (a Num) eq(n int) bool {
	return a.v == n + 1000
}

fn (a Num) lt(n int) bool {
	return a.v < n + 1000
}

fn main() {
	a := Num{6}
	b := Num{2}
	assert (a + b).v == 8
	assert (a - b).v == 4
	assert (a * b).v == 12
	assert (a / b).v == 3
	assert (a % Num{4}).v == 2
	assert a == Num{6}
	assert a != b
	assert b < a
	assert a.plus(2) == 1008
	assert a.minus(2) == 1004
	assert a.mul(2) == 1012
	assert a.pow(2) == 1010
	assert a.mul_(2) == 1024
	assert a.div(2) == 1003
	assert a.mod(4) == 1002
	assert !a.eq(6)
	assert a.lt(6)
	mut x := Num{9}
	x += Num{1}
	x *= Num{3}
	assert x.v == 30
	println("ok")
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-pthread', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
