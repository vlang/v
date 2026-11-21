module fastc

import os
import v.cmdexec

// `[N]Enum{init: .member}` fills the array with that member of the element enum, also in a
// field default and for an enum of another module.
fn test_fastc_fixed_array_enum_init_shorthand() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_fixed_array_enum_init_shorthand_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'input')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'input', 'input.v'), 'module input

pub const button_max = 4

pub enum ButtonState {
	release
	press
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import input

struct Surface {
mut:
	click_state [input.button_max]input.ButtonState = [input.button_max]input.ButtonState{init: .release}
}

fn main() {
	mut s := Surface{}
	s.click_state[2] = .press
	println(s.click_state[0])
	println(s.click_state[2])
	local := [3]input.ButtonState{init: .press}
	println(local[1])
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
	assert run.output.trim_space() == 'release\npress\npress', run.output
}
