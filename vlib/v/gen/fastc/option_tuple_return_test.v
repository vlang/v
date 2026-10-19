module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_option_tuple_fallback_can_return_an_enclosing_function_tuple() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_option_tuple_return_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
__global cleanups int
fn optional(found bool) ?(bool, string) {
	if !found { return none }
	return true, "ok"
}
fn take(found bool) (u64, u64) {
	defer { cleanups++ }
	set, text := optional(found) or { return u64(100), u64(200) }
	if !set || text != "ok" { return 0, 0 }
	return 41, 1
}
fn result_tuple(found bool) !(int, string) {
	if !found { return error("missing") }
	return 42, "ok"
}
fn propagate(found bool) (int, int) {
	value, text := result_tuple(found) or {
		assert err.msg() == "missing"
		return 7, 8
	}
	return value, text.len
}
fn fallback(found bool) (int, int) {
	value, text := result_tuple(found) or { 10, "fallback" }
	return value, text.len
}
fn main() {
	a, b := take(true)
	assert a == 41 && b == 1 && cleanups == 1
	c, d := take(false)
	assert c == 100 && d == 200 && cleanups == 2
	e, f := propagate(true)
	assert e == 42 && f == 2
	g, h := propagate(false)
	assert g == 7 && h == 8
	i, j := fallback(true)
	assert i == 42 && j == 2
	k, l := fallback(false)
	assert k == 10 && l == 8
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.enable_globals = true
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
