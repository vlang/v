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
		println('V Self Compiling (-prod mode)...')
	}
	else {
		println('V Self Compiling...')
	}

	s2 := os.exec(cmd) or { panic(err) }
	if s2.output.len > 0 {
		println(s2.output)
	}
	if s2.exit_code != 0 {
		exit(1)
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
