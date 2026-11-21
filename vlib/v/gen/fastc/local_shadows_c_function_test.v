module fastc

import os
import v.cmdexec

// A local named like a C function (`getpid := C.getpid()`) gets its own C name, so the
// function stays callable and assignments reach the local.
fn test_fastc_locals_named_like_c_functions() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_locals_named_like_c_functions_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

#include <unistd.h>

fn C.getpid() int

fn main() {
	getpid := C.getpid()
	println(getpid > 0)
	mut total := 0
	for _ in 0 .. 2 {
		getpid2 := C.getpid()
		total += if getpid2 == getpid { 1 } else { 0 }
	}
	println(total)
}

fn exp_of(x int) int {
	mut getpid := x
	if getpid < 0 {
		getpid = -getpid
	}
	return getpid
}

fn init() {
	println(exp_of(-3))
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
	assert run.output.trim_space() == '3\ntrue\n2', run.output
}
