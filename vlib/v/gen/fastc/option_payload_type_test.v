module fastc

import os
import v.cmdexec
import v.pref

// The caller reads an option or result payload as the declared payload type, so a
// returned number of another type (`return 5` in a `!f64` function) is boxed as that type.
fn test_fastc_option_payload_is_boxed_as_the_declared_type() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_option_payload_type_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

fn real(ok bool) !f64 {
	if ok {
		return 5
	}
	return error("no")
}

fn small() ?f32 {
	return 1.5
}

fn small_value(x f32) ?f32 {
	return x
}

fn byte_value() ?u8 {
	return 200
}

fn wide() ?int {
	return 6000000000
}

fn main() {
	assert (real(true) or { 0.0 }) == 5.0
	assert (real(false) or { -1.0 }) == -1.0
	assert (small() or { 0.0 }) == f32(1.5)
	assert (small_value(2.5) or { 0.0 }) == f32(2.5)
	assert (byte_value() or { 0 }) == 200
	assert (wide() or { 0 }) == 6000000000
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
