module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_assignment_uses_old_postfix_value_and_mutates_storage_once() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_postfix_assignment_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
__global (inode_counter u64 = 9)
struct Stat {
mut:
	ino u64
}
struct Resource {
mut:
	stat Stat
}
fn main() {
	mut resource := Resource{}
	resource.stat.ino = inode_counter++
	assert resource.stat.ino == 9 && inode_counter == 10
	mut previous := u64(0)
	previous = inode_counter--
	assert previous == 10 && inode_counter == 9
	mut values := [u64(0), u64(0)]
	mut index := 0
	values[index++] = inode_counter++
	assert index == 1 && inode_counter == 10 && values[0] == 9 && values[1] == 0
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
