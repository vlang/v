module fastc

import os
import v.cmdexec

// A plain `struct C.pthread_t {}` names the C typedef `pthread_t`, as with the C backend,
// and its empty literal initializes a pointer typedef too.
fn test_fastc_c_typedef_struct_names() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_c_typedef_struct_names_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

#include <pthread.h>

struct C.pthread_t {}

fn C.pthread_self() C.pthread_t

struct Holder {
	handle C.pthread_t
}

fn main() {
	mut h := Holder{
		handle: C.pthread_self()
	}
	empty := C.pthread_t{}
	_ = empty
	println(sizeof(h.handle) == sizeof(voidptr))
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
	assert run.output.trim_space() == 'true', run.output
}
