module main

import os
import v.pref
import v.util

fn main() {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	os.chdir(vroot)

	println('Updating V...')

	if !os.exists('.git') {
		// initialize as if it had been cloned
		git_command('init')
		git_command('remote add origin https://github.com/vlang/v')
		git_command('fetch')
		git_command('reset --hard origin/master')
		git_command('clean --quiet -xdf --exclude v.exe --exclude cmd/tools/vup.exe')
	} else {
		// pull latest
		git_command('pull origin master')
	}

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

fn git_command(command string) {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)

	git_result := os.exec('git $command') or {
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
}
