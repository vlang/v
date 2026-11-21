module fastc

import os
import v.cmdexec

// Generic static methods (`fn Box.create[T]()`) are lowered to generic functions, so calls
// with type arguments work in their own module and through an import.
fn test_fastc_generic_static_methods() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_generic_static_methods_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'shapes')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'shapes', 'shapes.v'), 'module shapes

pub struct Math {}

pub fn Math.larger[T](a T, b T) T {
	return if a > b { a } else { b }
}

pub fn larger_int(a int, b int) int {
	return Math.larger[int](a, b)
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), "module main

import shapes

struct Box[T] {
	value T
	count int
}

fn Box.create[T](value T) Box[T] {
	return Box[T]{
		value: value
		count: 1
	}
}

fn Box.empty[T]() Box[T] {
	return Box[T]{}
}

fn main() {
	b := Box.create[string]('hi')
	println(b.value)
	e := Box.empty[int]()
	println(e.count)
	println(shapes.Math.larger[f64](1.5, 2.5))
	println(shapes.larger_int(7, 3))
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'hi\n0\n2.5\n7', run.output
}
