module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_call_type_tests_preserve_tags_and_short_circuiting() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_call_type_test_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Leaf { value int }
struct Branch { code int }
type Tree = Leaf | Branch
struct Holder { value Tree }
fn choose(mut calls int, leafy bool) Tree {
	calls++
	if leafy { return Leaf{ value: 42 } }
	return Branch{ code: 7 }
}
fn identity(value Tree) Tree { return value }
fn main() {
	mut calls := 0
	assert choose(mut calls, true) is Leaf
	assert calls == 1
	assert choose(mut calls, false) !is Leaf
	assert calls == 2
	assert choose(mut calls, true) is Leaf && choose(mut calls, false) !is Leaf
	assert calls == 4
	assert choose(mut calls, false) is Leaf || choose(mut calls, true) is Leaf
	assert calls == 6
	assert !(choose(mut calls, false) is Leaf)
	assert calls == 7
	assert true || choose(mut calls, false) is Leaf
	assert !(false && choose(mut calls, true) is Leaf)
	assert calls == 7
	kind := if identity(Leaf{ value: 42 }) is Leaf { "leaf" } else { "branch" }
	assert kind == "leaf"
	holder := Holder{ value: Leaf{ value: 42 } }
	assert holder.value is Leaf
	assert identity(holder.value) is Leaf
	assert (identity(Leaf{}) is Leaf) != (identity(Branch{}) is Leaf)
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
