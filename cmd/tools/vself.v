module main

import (
	os
	v.pref
)

fn main() {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	os.chdir(vroot)

	mut cmd := '$vexe -o v2 cmd/v'
	if os.args.len >= 3 && os.args[2] == '-prod' {
		cmd = '$vexe -o v2 -prod cmd/v'
		println('V self compiling (-prod mode)...')
	}
	else {
		println('V self compiling...')
	}

	result := os.exec(cmd) or { panic(err) }
	if result.exit_code != 0 {
		mut err := 'Permission denied'
		if !result.output.contains('Permission denied') {
			err = '\n$result.output'
		}
		eprintln('cannot compile to ‘$vroot: $err')
		exit(1)
	}
	if result.output.len > 0 {
		println(result.output)
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
