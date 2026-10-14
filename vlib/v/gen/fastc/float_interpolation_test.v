module fastc

import os
import v.cmdexec

// `${x:.2}`, `${x:8.2}`, `${x:-8.2}` and `${x:.2f}` format a float as the C backend does:
// the precision rounds, `f` keeps trailing zeros, and the plain form trims them.
fn test_fastc_float_interpolation_formats_match_the_c_backend() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_float_format_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_float_format' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

fn main() {
	x := 3.14159
	y := -2.5
	z := 1234567.891
	w := 0.000012345
	println('[\${x:.2}] [\${x:8.2}] [\${x:-8.2}] [\${x:.2f}] [\${y:.3}] [\${y:.1f}]')
	println('[\${z:.3}] [\${w:.4}] [\${x:.0}] [\${f32(1.5):.1}] [\${100.0:.2}]')
}
") or {
		panic(err)
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '[3.14] [    3.14] [3.14    ] [3.14] [-2.5] [-2.5]\n[1.23e+06] [0] [3] [1.5] [100]'
}
