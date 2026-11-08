module fastc

import os
import v.cmdexec
import v.pref

@[unsafe]
fn test_fastc_unsafe_for_assignment_uses_existing_pointer_binding() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_unsafe_for_assignment_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Node {
	value int
	next &Node = unsafe { nil }
}
fn main() {
	tail := &Node{ value: 2 }
	first := &Node{ value: 40, next: tail }
	current := unsafe { &Node(nil) }
	mut sum := 0
	unsafe {
		for current = first; current != nil; current = current.next {
			sum += current.value
		}
		assert current == nil
	}
	assert sum == 42
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
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
	generate('module main\nfn main() { current := 0; for current = 0; current < 2; current++ {} }',
		'immutable_for_assignment.v', prefs) or {
		assert err.msg().contains('assignment to immutable loop variable `current`'), err.msg()
		return
	}
	assert false, 'immutable loop assignment outside unsafe must remain rejected'
}
