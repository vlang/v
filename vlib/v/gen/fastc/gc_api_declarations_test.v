module fastc

import os
import v.cmdexec

// Programs that call the Boehm GC API through `fn C.GC_*` declarations get prototypes for it,
// which the C backend takes from `gc.h`.
fn test_fastc_declares_called_gc_api_functions() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_declares_called_gc_api_functions_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

fn C.GC_add_roots(voidptr, voidptr)
fn C.GC_remove_roots(voidptr, voidptr)
fn C.GC_get_stack_base(stack_base voidptr) int

struct StackBase {
	mem_base voidptr
	reg_base voidptr
}

fn main() {
	mut buf := []u8{len: 64}
	C.GC_add_roots(buf.data, unsafe { &u8(buf.data) + 64 })
	C.GC_remove_roots(buf.data, unsafe { &u8(buf.data) + 64 })
	mut sb := StackBase{}
	println(C.GC_get_stack_base(voidptr(&sb)) == 0)
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
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
