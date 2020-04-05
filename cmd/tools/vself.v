module main

import (
	os
	v.pref
)

fn main() {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	os.chdir(vroot)

	// check if have permission on the target directory
	tmp_perm_check := '$vroot/tmp_perm_check'
	os.open_file(tmp_perm_check, 'w+') or {
		eprintln('cannot compile tool to directory ‘$vroot: $err')
		exit(1)
	}
	os.rm(tmp_perm_check)

	mut cmd := '$vexe -o v2 cmd/v'
	if os.args.len >= 3 && os.args[2] == '-prod' {
		cmd = '$vexe -o v2 -prod cmd/v'
		println('V self compiling (-prod mode)...')
	}
	else {
		println('V self compiling...')
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
