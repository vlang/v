module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_assertions_leave_following_unary_fallback_values_separate() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_assert_fallback_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
fn fail() !int { return error("entry missing") }
fn negative() int {
	return fail() or {
		assert err.msg() == "entry missing"
		-1
	}
}
fn positive() int {
	return fail() or {
		assert err.msg() == "entry missing", "unexpected error"
		+42
	}
}
fn dereferenced(default &int) int {
	return fail() or {
		assert err.msg() == "entry missing"
		*default
	}
}
fn main() {
	default := 42
	assert negative() == -1
	assert positive() == 42
	assert dereferenced(&default) == 42
	assert (20
		+ 22) == 42
	bytes := 20
		+ 22
	assert bytes == 42
	println("assert fallback boundaries work")
}
') or { panic(err) }
	ordinary := os.join_path(root, 'ordinary')
	ordinary_build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'c', '-nocache', '-gc', 'none',
		'-o', ordinary, root], @VEXEROOT, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert ordinary_build.exit_code == 0, ordinary_build.output
	ordinary_run := cmdexec.run(ordinary, [])
	assert ordinary_run.exit_code == 0, ordinary_run.output
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	generated := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'fastc')
	os.write_file(c_path, generated) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output == ordinary_run.output, run.output
	assert run.output == 'assert fallback boundaries work\n', run.output
}
