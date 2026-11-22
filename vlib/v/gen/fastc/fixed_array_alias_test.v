module fastc

import os
import v.cmdexec

// An alias of a fixed array of structs (`type Table = [8]Row`) is declared after the fixed
// array, which follows the struct definitions.
fn test_fastc_aliases_of_fixed_arrays_of_structs() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_aliases_of_fixed_arrays_of_structs_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'pt')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'pt', 'pt.v'), 'module pt

pub struct Transition {
pub:
	state  u8
	action u8
}

pub struct TransitionRow {
pub mut:
	cols [4]Transition
}

pub type Table = [8]TransitionRow

fn gen_table() Table {
	mut result := Table{}
	result[2].cols[1] = Transition{3, 4}
	return result
}

pub const table = gen_table()

pub fn lookup(row int, col int) Transition {
	return table[row].cols[col]
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), "module main

import pt

fn main() {
	t := pt.lookup(2, 1)
	println('\${t.state} \${t.action}')
	println(pt.lookup(0, 0).state)
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
	assert run.output.trim_space() == '3 4\n0', run.output
}
