module fastc

import os
import v.cmdexec

// A native header that includes `gc.h` reaches the Clang-built native owner, where FastC's own
// GC API prototypes (V's `voidptr` spellings) would conflict with the collector's declarations.
fn test_fastc_native_owner_accepts_gc_header() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_native_owner_accepts_gc_header_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

#flag -framework CoreFoundation
#include "@VMODROOT/gc_user.h"

fn C.GC_get_stack_base(stack_base voidptr) int
fn C.q90_stack_ok() int

struct StackBase {
	mem_base voidptr
	reg_base voidptr
}

fn main() {
	mut sb := StackBase{}
	println(C.GC_get_stack_base(voidptr(&sb)) == 0)
	println(C.q90_stack_ok() == 1)
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'q90' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'gc_user.h'), '#include <gc/gc.h>\nstatic int q90_stack_ok(void) {\n	struct GC_stack_base base;\n	return GC_get_stack_base(&base) == GC_SUCCESS;\n}\n') or { panic(err) }
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
	assert run.output.trim_space() == 'true\ntrue', run.output
}
