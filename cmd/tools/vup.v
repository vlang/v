module main

import os
import v.pref
import v.util

struct App {
	is_verbose bool
	vexe       string
	vroot      string
}

fn new_app() App {
	vexe := pref.vexe_path()
	return App{
		is_verbose: '-v' in os.args
		vexe: vexe
		vroot: os.dir(vexe)
	}
}

fn main() {
	app := new_app()
	os.chdir(app.vroot)
	println('Updating V...')
	if !os.exists('.git') {
		// initialize as if it had been cloned
		app.git_command('init')
		app.git_command('remote add origin https://github.com/vlang/v')
		app.git_command('fetch')
		app.git_command('reset --hard origin/master')
		app.git_command('clean --quiet -xdf --exclude v.exe --exclude cmd/tools/vup.exe')
	} else {
		// pull latest
		app.git_command('pull origin master')
	}
	v_hash := util.githash(false)
	current_hash := util.githash(true)
	// println(v_hash)
	// println(current_hash)
	if v_hash == current_hash {
		app.show_current_v_version()
		return
	}
	$if windows {
		app.backup('v.exe')
		make_result := os.exec('make.bat') or {
			panic(err)
		}
		println(make_result.output)
		app.backup('cmd/tools/vup.exe')
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
	os.exec('v cmd/tools/vup.v') or {
		panic(err)
	}
	app.show_current_v_version()
}

fn (app App) show_current_v_version() {
	println('Current V version:')
	os.system('$app.vexe version')
}

fn (app App) backup(file string) {
	backup_file := '${file}_old.exe'
	if os.exists(backup_file) {
		os.rm(backup_file)
	}
	os.mv(file, backup_file)
}

fn (app App) git_command(command string) {
	git_result := os.exec('git $command') or {
		panic(err)
	}
	if git_result.exit_code != 0 {
		if git_result.output.contains('Permission denied') {
			eprintln('have no access `$app.vroot`: Permission denied')
		} else {
			eprintln(git_result.output)
		}
		exit(1)
	} else {
		if app.is_verbose {
			println(git_result.output)
		}
	}
}
