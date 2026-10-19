module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_grouped_arithmetic_and_embedded_casts_preserve_evaluation() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_grouped_cast_expression_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Leaf { value int }
struct Branch { value int }
type Tree = Leaf | Branch
fn bump(mut calls int, value int) int {
	calls++
	return value
}
fn main() {
	mut calls := 0
	assert (bump(mut calls, 3) + 4) * (2 + 1) == 21
	assert calls == 1
	leaf := Tree(Leaf{ value: 7 })
	branch := Tree(Branch{ value: 5 })
	assert (leaf as Leaf).value + (branch as Branch).value == 12
	assert bump(mut calls, (leaf as Leaf).value) + ((2 + 1) * 3) == 16
	assert calls == 2
	assert !(false && bump(mut calls, (leaf as Leaf).value) > 0)
	assert calls == 2
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
}
