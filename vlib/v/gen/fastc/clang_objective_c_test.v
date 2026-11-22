module fastc

import os
import v.cmdexec

// With `-cc clang` on macOS, a program with a native include compiles as Objective-C, as
// the C backend does, without FastC reading the header.
fn test_fastc_clang_compiles_programs_with_native_includes_as_objective_c() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_clang_objective_c_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'bridge.h'), '#ifndef __OBJC__\n#error "bridge.h needs Objective-C"\n#endif\nstatic inline int bridge_answer(void) { return 42; }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

#include "@VMODROOT/bridge.h"

fn C.bridge_answer() int

fn main() {
	println(C.bridge_answer())
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'objc'\n}\n") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-cc', 'clang', '-nocache',
		'-gc', 'none', '-o', program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                          @VEXE
		'VFLAGS':                        ''
		'VOSARGS':                       ''
		'V_MACOS_V3_NO_FALLBACK':        '1'
		'V_C_ERROR_BUG_REPORT_DISABLED': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42', run.output
}
