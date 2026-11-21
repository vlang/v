module fastc

import os
import v.cmdexec

// Explicit calls of generic functions with several type arguments (`view[K, V, X](...)`) are
// specialized like single-argument ones, including results that name specialized structs
// nested in other instances.
fn test_fastc_explicit_calls_with_several_type_arguments() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_explicit_calls_with_several_type_argumen_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'hm')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'hm', 'hm.v'), 'module hm

pub struct Ctx[K] {
pub:
	seed int
}

pub struct Map[K, V, X] {
pub mut:
	keys    []K
	values  []V
	context X
}

fn view[K, V, X](n int, context X) Map[K, V, X] {
	return Map[K, V, X]{
		keys:    []K{len: n}
		values:  []V{len: n}
		context: context
	}
}

pub struct Offset[K, V, X] {
	n int
}

pub fn (o Offset[K, V, X]) map(context X) Map[K, V, X] {
	return view[K, V, X](o.n, context)
}

pub fn new_offset[K, V, X](n int) Offset[K, V, X] {
	return Offset[K, V, X]{
		n: n
	}
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import hm

struct Alloc {
	size int
}

fn main() {
	o := hm.new_offset[u32, Alloc, hm.Ctx[u32]](3)
	mut m := o.map(hm.Ctx[u32]{7})
	m.values[1] = Alloc{5}
	println(m.keys.len)
	println(m.values[1].size)
	println(m.context.seed)
}
') or { panic(err) }
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
	assert run.output.trim_space() == '3\n5\n7', run.output
}
