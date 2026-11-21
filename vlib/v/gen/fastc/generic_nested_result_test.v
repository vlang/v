module fastc

import os
import v.cmdexec

// An on-demand instance whose result nests one specialized struct in another
// (`OffsetMap[K, V, AutoCtx[K]]`) returns the matching copy.
fn test_fastc_results_naming_nested_specialized_structs() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_results_naming_nested_specialized_struct_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'hm')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'hm', 'hm.v'), 'module hm

pub struct AutoCtx[K] {
pub:
	seed int
}

pub struct OffsetMap[K, V, X] {
pub:
	cap     int
	context X
}

pub fn generic_init[K, V, X](cap int, context X) OffsetMap[K, V, X] {
	return OffsetMap[K, V, X]{
		cap:     cap
		context: context
	}
}

pub fn auto_init[K, V](cap int) OffsetMap[K, V, AutoCtx[K]] {
	return generic_init[K, V, AutoCtx[K]](cap, AutoCtx[K]{
		seed: 9
	})
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import hm

struct Alloc {
	n int
}

fn main() {
	m := hm.auto_init[u32, Alloc](16)
	println(m.cap)
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
	assert run.output.trim_space() == '16\n9', run.output
}
