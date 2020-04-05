module main

import (
	os
	v.pref
)

fn main() {
	vroot := os.dir(pref.vexe_path())
	os.chdir(vroot)

	println('Updating V...')

	// git pull
	git_result := os.exec('git pull --rebase origin master') or {
		panic(err)
	}

	if git_result.exit_code != 0 {
		mut err := 'Permission deined'
		if !git_result.output.contains('Permission denied') {
			err = '\n$git_result.output'
		}
		eprintln('cannot compile to ‘$vroot: $err')
		exit(1)
	}

	println(git_result.output)

	$if windows {
		v_backup_file := 'v_old.exe'
		if os.exists(v_backup_file) {
			os.rm(v_backup_file)
		}
		os.mv('v.exe', v_backup_file)

		make_result := os.exec('make.bat') or {
			panic(err)
		}
		println(make_result.output)
	} $else {
		make_result := os.exec('make') or {
			panic(err)
		}
		println(make_result.output)
	}
}
