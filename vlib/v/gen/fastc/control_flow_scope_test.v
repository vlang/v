module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_unsafe_scopes_and_forward_labels_preserve_control_flow() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_control_flow_scope_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
__global finished int
fn branch(fail bool) int {
	defer { finished++ }
	if fail { goto cleanup }
	return 7
	cleanup:
	return 9
}
fn main() {
	mut total := 0
	value := 100
	unsafe { value := 3; total += value }
	unsafe { value := 4; total += value }
	assert total == 7 && value == 100
	assert branch(false) == 7 && finished == 1
	assert branch(true) == 9 && finished == 2
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.enable_globals = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
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
