module main

import os
import v.pref

fn main() {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	os.chdir(vroot)
	os.setenv('VCOLORS', 'always', true)

	self_idx := os.args.index('self')
	args := os.args[1..self_idx]
	args_str := args.join(' ')
	options := if args.len > 0 { '($args_str)' } else { '' }
	println('V self compiling ${options}...')

	cmd := '$vexe -o v2 $args_str cmd/v'
	result := os.exec(cmd) or { panic(err) }
	if result.exit_code != 0 {
		mut err := 'Permission denied'
		if !result.output.contains('Permission denied') {
			err = '\n$result.output'
		}
		eprintln('cannot compile to `$vroot`: $err')
		exit(1)
	}
	if result.output.len > 0 {
		println(result.output.trim_space())
	}

	v_file := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	v2_file := if os.user_os() == 'windows' { 'v2.exe' } else { 'v2' }
	bak_file := if os.user_os() == 'windows' { 'v_old.exe' } else { 'v_old' }

	if os.exists(bak_file) {
		os.rm(bak_file)
	}
	os.mv(v_file, bak_file)
	os.mv(v2_file, v_file)
}
