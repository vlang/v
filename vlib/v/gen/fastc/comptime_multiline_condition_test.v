module fastc

import os
import v.cmdexec

// A `$if` condition may continue on the next line with `||` or `&&`.
fn test_fastc_comptime_conditions_continue_on_new_lines() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_comptime_conditions_continue_on_new_line_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

fn kind[T](v T) string {
	$if T is $int || T is $float
		|| T is string {
		return 'scalar'
	} $else $if T is $array
		&& T is $array_dynamic {
		return 'array'
	} $else {
		return 'other'
	}
}

fn main() {
	println(kind(3))
	println(kind('x'))
	println(kind([1, 2]))
	println(kind(true))
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
	assert run.output.trim_space() == 'scalar\nscalar\narray\nother', run.output
}
