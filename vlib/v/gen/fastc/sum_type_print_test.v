module fastc

import os
import v.cmdexec
import v.pref

// `println` of a value without a `str()` method (a sum type, struct, array or map)
// prints what `'${value}'` gives, and a sum value prints as `Sum(payload)`, as in
// the C backend: a string payload is quoted, a rune one is in backticks, an alias
// variant keeps its own wrapper and a nested sum value its own `Sum(...)`.
fn test_fastc_println_of_sum_type_and_other_values_without_str_method() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_sum_type_print_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

struct Point {
	x int
	y int
}

struct Named {
	n int
}

fn (n Named) str() string {
	return "named \${n.n}"
}

type Ints = []int

type Val = int | string | Point | Named | f64 | Ints | rune

type Outer = Val | u8

struct Box {
mut:
	v Val
}

fn main() {
	mut b := Box{
		v: 3
	}
	println(b.v)
	x := Val("hi")
	println(x)
	println("\${b.v} and \${x}")
	println(b.v.str())
	b.v = Point{1, 2}
	println(b.v)
	println(Val(Named{5}))
	println(Val(1.5))
	println(Val(Ints([1, 2])))
	println(Val(`r`))
	println([Val(1), Val("a")])
	println(Outer(Val(7)))
	println(Outer(u8(9)))
	print(x)
	println("")
	println(Point{3, 4})
	println([1, 2, 3])
	m := {
		"k": 1
	}
	println(m)
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
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	expected := [
		'Val(3)',
		"Val('hi')",
		"Val(3) and Val('hi')",
		'Val(3)',
		'Val(Point{',
		'    x: 1',
		'    y: 2',
		'})',
		'Val(named 5)',
		'Val(1.5)',
		'Val(Ints([1, 2]))',
		'Val(`r`)',
		"[Val(1), Val('a')]",
		'Outer(Val(7))',
		'Outer(9)',
		"Val('hi')",
		'Point{',
		'    x: 3',
		'    y: 4',
		'}',
		'[1, 2, 3]',
		"{'k': 1}",
		'',
	].join('\n')
	assert run.output == expected, run.output
}
