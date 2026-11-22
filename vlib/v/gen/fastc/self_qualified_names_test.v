module fastc

import os
import v.cmdexec

// A module may name its own constants with its own name (`term.tabstop_interval` inside
// `module term`), also in field defaults.
fn test_fastc_modules_name_their_own_declarations() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_modules_name_their_own_declarations_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'term')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'term', 'term.v'), 'module term

const tabstop_interval = 8

pub const colors_default = Colors{
	fg: 7
}

pub struct Colors {
pub:
	fg int
}

pub struct Terminal {
pub:
	colors Colors = term.colors_default
	tabs   int    = term.tabstop_interval * 2
}

pub fn interval() int {
	return term.tabstop_interval
}

pub fn make() Terminal {
	terminal := Terminal{}
	return terminal
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import term

fn main() {
	t := term.make()
	println(t.colors.fg)
	println(t.tabs)
	println(term.interval())
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
	assert run.output.trim_space() == '7\n16\n8', run.output
}
