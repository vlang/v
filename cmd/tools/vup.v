module main

import os
import v.pref
import v.util

fn main() {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	os.chdir(vroot)

	println('Updating V...')

	// git pull
	git_result := os.exec('git pull --rebase origin master') or {
		panic(err)
	}

	if git_result.exit_code != 0 {
		if git_result.output.contains('Permission denied') {
			eprintln('have no access `$vroot`: Permission denied')
		} else {
			eprintln(git_result.output)
		}
		exit(1)
	}

	println(git_result.output)
	v_hash := util.githash(false)
	current_hash := util.githash(true)
	// println(v_hash)
	// println(current_hash)
	if v_hash == current_hash {
		show_current_v_version(vexe)
		return
	}

	$if windows {
		backup('v.exe')

		make_result := os.exec('make.bat') or {
			panic(err)
		}
		println(make_result.output)

		backup('cmd/tools/vup.exe')
	} $else {
		self_result := os.exec('./v self') or {
			panic(err)
		}
		println(self_result.output)
		if self_result.exit_code != 0 {
			// v self failed, have to use make
			println('v self failed, running make...')
			make_result := os.exec('make') or {
				panic(err)
			}
			println(make_result.output)
		}
	}

	_ := os.exec('v cmd/tools/vup.v') or {
		panic(err)
	}
	show_current_v_version(vexe)
}

fn show_current_v_version(vexe string){
	println('Current V version:')
	os.system('$vexe version')
}

fn backup(file string) {
	backup_file := '${file}_old.exe'
	if os.exists(backup_file) {
		os.rm(backup_file)
	}
	os.mv(file, backup_file)
}
