module fastc

import os
import v.cmdexec
import v.pref

// `a ** b` is lowered as in the C backend: an integer power by squaring with V's
// results for negative exponents and the result wrapped to the operands' type, a
// float power through `pow`. `**` groups to the right, binds tighter than `*`,
// and a prefix minus applies to its result. A power of integer literals stays a
// constant, so it can size a fixed array, and `**=` updates its target once.
// `**p` dereferences twice.
fn test_fastc_power_expressions_and_assignments() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_power_expression_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

const kilo = 2 ** 10

const half = 1.5 ** 2

const dims = 2 ** 2

struct Holder {
mut:
	n   int
	f   f64
	arr []int
}

fn cube(x int) int {
	return x ** 3
}

fn main() {
	a := 3
	b := 2
	println(a ** b)
	println(2 ** 3 ** 2)
	println(-a ** 2)
	println(a * b ** 2 + 1)
	println((a + 1) ** (b + 1))
	println(a ** -1)
	println(1 ** -1)
	println(0 ** -1)
	println(u8(2) ** 9)
	println(i8(2) ** 7)
	println(u64(3) ** 40)
	println(2.0 ** 0.5)
	println(f32(1.5) ** 2)
	println(kilo)
	println(half)
	fixed := [dims]int{}
	println(fixed.len)
	if a ** b > 8 {
		println(cube(a ** 1))
	}
	println([a ** 2, 2 ** a])
	println("\${a ** 3} \${(a ** 2).str()}")
	mut x := 3
	x **= 3
	println(x)
	mut f := 2.0
	f **= 0.5
	println(f)
	mut h := Holder{
		n:   3
		f:   9.0
		arr: [2, 3]
	}
	h.n **= 2
	h.f **= 0.5
	h.arr[1] **= 2
	println("\${h.n} \${h.f} \${h.arr}")
	mut m := {
		"k": 5
	}
	m["k"] **= 2
	println(m["k"])
	p := &a
	pp := &p
	println(**pp)
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
		'9',
		'512',
		'-9',
		'13',
		'64',
		'0',
		'1',
		'-1',
		'0',
		'-128',
		'12157665459056928801',
		'1.4142135623730951',
		'2.25',
		'1024',
		'2.25',
		'4',
		'27',
		'[9, 8]',
		'27 9',
		'27',
		'1.4142135623730951',
		'9 3.0 [2, 9]',
		'25',
		'3',
		'',
	].join('\n')
	assert run.output == expected, run.output
}
