module fastc

import os
import v.cmdexec

// `$if T in [i8, int, $float]` and `$if T !in [...]` test a generic parameter against a
// list of types and type groups.
fn test_fastc_comptime_type_in_list() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_comptime_type_in_list_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

fn kind[T](value T) string {
	$if T in [i8, i16, int, $float] {
		return 'number'
	} $else $if T !in [string, bool] {
		return 'other'
	}
	return 'text or bool'
}

fn main() {
	println(kind(3))
	println(kind(2.5))
	println(kind('s'))
	println(kind(true))
	println(kind(u64(1)))
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
	assert run.output.trim_space() == 'number\nnumber\ntext or bool\ntext or bool\nother', run.output
}
