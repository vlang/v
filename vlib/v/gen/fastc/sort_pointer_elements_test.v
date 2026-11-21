module fastc

import os
import v.cmdexec

// Sorting an array of pointers names its compare helper with an identifier-safe spelling.
fn test_fastc_sorts_arrays_of_pointers() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_sorts_arrays_of_pointers_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

struct Pin {
	y int
	x int
}

fn main() {
	p1 := &Pin{2, 1}
	p2 := &Pin{1, 5}
	p3 := &Pin{1, 2}
	mut pins := [p1, p2, p3]
	pins.sort(a.x < b.x)
	for p in pins {
		println('\${p.y} \${p.x}')
	}
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
	assert run.output.trim_space() == '2 1\n1 2\n1 5', run.output
}
