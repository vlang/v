module fastc

import os
import v.cmdexec
import v.pref

// A comparison of a signed and an unsigned integer compares their values, as cgen does,
// rather than converting the signed operand to unsigned as plain C would. Casts and calls
// as operands render specially, which must not skip that lowering.
fn test_fastc_mixed_sign_comparisons_compare_values() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_mixed_sign_comparison_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

fn minus_one() i64 {
	return -1
}

fn one() u64 {
	return 1
}

fn main() {
	a := i64(-1)
	b := u64(1)
	println(a < b)
	println(i64(-1) < u64(1))
	println(u64(1) > i64(-1))
	println(i64(-89) <= u64(567))
	println(u64(0) != i64(-1))
	println(minus_one() < one())
	println(a < u64(1))
	y := i64(-1) < u64(1)
	println(y)
	mut taken := false
	if i64(-1) < u64(1) {
		taken = true
	}
	println(taken)
	println(!(u64(1) < i64(-1)))
	println(i64(-1) < u64(1) && u64(2) >= i64(2))
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-pthread', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['true'].repeat(11), run.output
}
