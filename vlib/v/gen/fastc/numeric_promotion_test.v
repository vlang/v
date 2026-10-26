module fastc

import os
import v.cmdexec
import v.pref

// Mixed numeric operands are promoted as the checker promotes them: the operand
// that the other widens to wins (`u8 * int` is `int`, `int * i64` is `i64`),
// otherwise the left one does; an integer literal adopts the other operand's
// type unless it does not fit a narrow one; `f32` stays `f32` unless the other
// operand is `f64`. A prefix `&` addresses only its own operand, so
// `&u8(raw) + step` steps over `u8`s.
fn test_fastc_mixed_numeric_operands_are_promoted_like_the_checker() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_numeric_promotion_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

fn get() int {
	return 100
}

fn main() {
	a := u8(200)
	x := 100
	i := i64(5)
	b := u16(3)
	c := i8(2)
	y := a * x
	println(y)
	println(typeof(y).name)
	println(typeof(x * i).name)
	println(typeof(a + b).name)
	println(typeof(b + a).name)
	println(typeof(c * a).name)
	println(typeof(a + 1).name)
	println(typeof(a + 1000).name)
	z := a + 100
	println(z)
	w := a * get()
	println(w)
	f := f32(1.5)
	println(typeof(f * x).name)
	println(typeof(f * 2.0).name)
	println(typeof(f * 2.0 * 1.0).name)
	d := 2.5
	println(typeof(f * d).name)
	n := c * -1
	println(typeof(n).name)
	println(n)
	s := "abc"
	step := 1
	p := unsafe { &u8(s.str) + step }
	println(unsafe { *p })
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
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_file, '-lm'])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	expected := [
		'20000',
		'int',
		'i64',
		'u16',
		'u16',
		'i8',
		'u8',
		'int',
		'44',
		'20000',
		'f32',
		'f32',
		'f32',
		'f64',
		'i8',
		'-2',
		'98',
		'',
	].join('\n')
	assert run.output == expected, run.output
}
