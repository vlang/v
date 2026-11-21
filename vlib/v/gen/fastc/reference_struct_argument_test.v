module fastc

import os
import v.cmdexec

// A reference passed to a struct parameter is dereferenced, also through an alias of the
// struct, as with the C backend.
fn test_fastc_references_passed_to_struct_parameters() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_references_passed_to_struct_parameters_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

struct Pin {
	x int
	y int
}

type CorePin = Pin

struct Mouse {
mut:
	left_click_pin ?&CorePin
}

fn describe(pin Pin, extra int) string {
	return '\${pin.x},\${pin.y}+\${extra}'
}

fn click(m Mouse) string {
	pin := m.left_click_pin or { return 'none' }
	return describe(pin, 1)
}

fn main() {
	p := &Pin{3, 4}
	println(click(Mouse{ left_click_pin: p }))
	println(click(Mouse{}))
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
	assert run.output.trim_space() == '3,4+1\nnone', run.output
}
