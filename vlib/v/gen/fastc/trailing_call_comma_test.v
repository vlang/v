module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_trailing_call_commas_preserve_tuple_and_conditional_arguments() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_trailing_call_comma_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
fn bounds(left int, right int) (int, int) { return left, right }
fn add(left int, right int) int { return left + right }
fn main() {
	left, right := bounds(
		if true { 17 } else { 0 },
		if false { 0 } else { 25 },
	)
	assert left == 17
	assert right == 25
	assert add(left, right,) == 42
	println("trailing call commas work")
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'trailing call commas work', run.output
}
