module main

import (
	os
	v.pref
)

fn main() {
	vroot := os.dir(pref.vexe_path())
	os.chdir(vroot)

	// check if have permission on the target directory
	tmp_perm_check := '$vroot/tmp_perm_check'
	os.open_file(tmp_perm_check, 'w+') or {
		eprintln('cannot compile tool to directory ‘$vroot: $err')
		exit(1)
	}
	os.rm(tmp_perm_check)

	println('Updating V...')

	// git pull
	s := os.exec('git pull --rebase origin master') or {
		panic(err)
	}
	println(s.output)

	$if windows {
		v_backup_file := 'v_old.exe'
		if os.exists(v_backup_file) {
			os.rm(v_backup_file)
		}
		os.mv('v.exe', v_backup_file)

		s2 := os.exec('make.bat') or {
			panic(err)
		}
		println(s2.output)
	} $else {
		s2 := os.exec('make') or {
			panic(err)
		}
		println(s2.output)
	}
}
