module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_selected_comptime_branch_skips_later_type_resolution() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_selected_comptime_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := r'module main
fn choose[T](value T) int {
    $if T is string { return 99 }
    $else $if T is $int { return 42 }
    $else $if T is MissingType { return 0 }
    $else { return 1 }
}
fn main() { assert choose(1) == 42; println("selected branch") }
'
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	ordinary := os.join_path(root, 'ordinary')
	ordinary_build := cmdexec.run(@VEXE, ['-b', 'c', '-gc', 'none', '-nocache', '-o', ordinary,
		root])
	assert ordinary_build.exit_code == 0, ordinary_build.output
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'program.c')
	os.write_file(c_path, c_source) or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	ordinary_run := cmdexec.run(ordinary, [])
	assert run.exit_code == 0, run.output
	assert ordinary_run.exit_code == 0, ordinary_run.output
	assert run.output == ordinary_run.output
	assert run.output == 'selected branch\n'
	mut message := ''
	_ := generate('module main\nfn main() { \$if i32 is string {} \$else \$if i32 is MissingType {} }', 'active_type.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('undeclared type `MissingType`'), message
}
