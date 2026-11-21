module fastc

import os
import v.cmdexec

// An `or` block on an option local already narrowed to its value (`b := button or { return }`
// after `if button == none {...} else if ...`) is dead; the value is the narrowed local.
fn test_fastc_or_blocks_of_narrowed_options() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_or_blocks_of_narrowed_options_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'input')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'input', 'input.v'), 'module input

pub enum MouseButton {
	left
	middle
	right
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import input

fn code(button ?input.MouseButton, release bool, mut out []u8) ! {
	mut button_code := u8(0)
	if button == none {
		button_code = 3
	} else if release {
		button_code = 4
	} else {
		b := button or { return }
		button_code = match b {
			.left { u8(0) }
			.middle { u8(1) }
			else { u8(2) }
		}
	}
	out << button_code
}

fn main() {
	mut out := []u8{}
	code(input.MouseButton.middle, false, mut out)!
	code(none, false, mut out)!
	code(.right, false, mut out)!
	code(.left, true, mut out)!
	println(out)
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
	assert run.output.trim_space() == '[1, 3, 2, 4]', run.output
}
