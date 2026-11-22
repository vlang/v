module fastc

import os
import v.cmdexec

// A comparison of `sizeof` values is a `bool`, also when it starts with `sizeof(`.
fn test_fastc_sizeof_comparisons_are_bool() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_sizeof_comparisons_are_bool_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

fn main() {
	println(sizeof(u8) == 1)
	x := sizeof(u16) == sizeof(i16)
	println(x)
	println(sizeof(u32) > 2)
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
	assert run.output.trim_space() == 'true\ntrue\ntrue', run.output
}
