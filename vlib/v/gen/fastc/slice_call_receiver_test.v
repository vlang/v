module fastc

import os
import v.cmdexec
import v.pref

// A slice with an omitted end (`f()[1..]`) reads the length of its receiver. The reader
// folds a call receiver into one token, which must still be evaluated only once.
fn test_fastc_slice_of_call_evaluates_the_call_once() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_slice_call_receiver_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

struct Counter {
mut:
	calls int
}

fn make_values(mut c Counter) []int {
	c.calls++
	return [1, 2, 3]
}

fn make_text(mut c Counter) string {
	c.calls++
	return "abc"
}

fn values_tail(mut c Counter) []int {
	return make_values(mut c)[1..]
}

fn main() {
	mut c := Counter{}
	values := make_values(mut c)[1..]
	assert values.len == 2 && values[0] == 2
	assert c.calls == 1
	text := make_text(mut c)[1..]
	assert text == "bc"
	assert c.calls == 2
	assert values_tail(mut c).len == 2
	assert c.calls == 3
	gated := make_values(mut c)#[1..]
	assert gated.len == 2
	assert c.calls == 4
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
