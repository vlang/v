module main

import (
	os
	v.pref
	v.util
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
		if git_result.output.contains('Permission denied') {
			eprintln('have no access ‘$vroot: Permission denied')
		} else {
			eprintln(git_result.output)
		}
		exit(1)
	}

	println(git_result.output)
	v_hash := util.githash(false)
	current_hash := util.githash(true)
	if v_hash == current_hash { 
		return 
	}

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
