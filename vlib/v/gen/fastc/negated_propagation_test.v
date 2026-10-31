module fastc

import os
import v.cmdexec
import v.pref

// A unary minus over a group that propagates an error (`-i64(f()!)`, `-(f()!)`) must lower
// the `!` like any other operand, instead of copying it into the C.
fn test_fastc_negated_group_lowers_propagation() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_negated_propagation_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

fn maybe(ok bool) !int {
	if ok {
		return 5
	}
	return error("no")
}

fn negated_cast(ok bool) !i64 {
	return -i64(maybe(ok)!)
}

fn negated_group(ok bool) !int {
	value := -(maybe(ok)!)
	return value
}

fn negated_cast_minus_one(ok bool) !i64 {
	value := -i64(maybe(ok)!) - 1
	return value
}

fn main() {
	assert (negated_cast(true) or { 0 }) == -5
	assert (negated_cast(false) or { 1 }) == 1
	assert (negated_group(true) or { 0 }) == -5
	assert (negated_group(false) or { 1 }) == 1
	assert (negated_cast_minus_one(true) or { 0 }) == -6
	assert (negated_cast_minus_one(false) or { 1 }) == 1
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
