module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_multiline_values_continue_without_consuming_unary_statements() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_multiline_values_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
__global (calls int)
fn record() { calls++ }
fn missing() ?int { return none }
fn fallback(default &int) int {
	return missing() or {
		record()
		*default
	}
}
fn choose(enabled bool) int {
	return if enabled { 42 }
		else if !enabled { 21 }
		else { 0 }
}
fn pair(found bool) ?(int, int) {
	if !found { return none }
	return 20, 22
}
fn guarded(found bool) int {
	return if a, b := pair(found) { a + b }
		else { -1 }
}
fn main() {
	bytes := u64(10) * 2 + 100
		+ u64(3 + 1) * (u64(7) * 2 + 2)
	assert bytes == 184
	mut value := 0
	value = 7
		+ 5
		- 2
	assert value == 10
	product := 6
		* 7
	assert product == 42
	masked := u64(255)
		& u64(42)
	assert masked == 42
	default := 42
	assert fallback(&default) == 42 && calls == 1
	assert choose(true) == 42 && choose(false) == 21
	assert guarded(true) == 42 && guarded(false) == -1
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
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
