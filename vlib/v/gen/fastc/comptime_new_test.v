module fastc

import os
import v.cmdexec

// `$new(T)` allocates a zero value of `T` on the heap, also for the type parameter of a
// generic static method.
fn test_fastc_comptime_new_allocates_zero_values() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_comptime_new_allocates_zero_values_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'pool')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'pool', 'pool.v'), 'module pool

pub struct Pool[T] {
mut:
	values []&T
}

pub fn Pool.init[T](n int) Pool[T] {
	mut values := []&T{cap: n}
	for _ in 0 .. n {
		values << $new(T)
	}
	return Pool[T]{
		values: values
	}
}

pub fn (mut p Pool[T]) get(i int) &T {
	return p.values[i]
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), "module main

import pool

struct Request {
mut:
	id   int
	name string
}

fn main() {
	mut p := pool.Pool.init[Request](2)
	mut r := p.get(1)
	r.id = 7
	r.name = 'x'
	println(p.get(1).id)
	println(p.get(0).id)
	println(p.get(1).name)
	n := $new(int)
	println(*n)
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
	assert run.output.trim_space() == '7\n0\nx\n0', run.output
}
