module main

import os
import v.pref
import v.util
import v.util.recompilation

struct App {
	is_verbose bool
	is_prod    bool
	vexe       string
	vroot      string
}

fn new_app() App {
	vexe := os.real_path(pref.vexe_path())
	vroot := os.dir(vexe)
	return App{
		is_verbose: '-v' in os.args
		is_prod: '-prod' in os.args
		vexe: vexe
		vroot: vroot
	}
}

fn main() {
	app := new_app()
	recompilation.must_be_enabled(app.vroot, 'Please install V from source, to use `v up` .')
	os.chdir(app.vroot)
	println('Updating V...')
	app.update_from_master()
	v_hash := util.githash(false)
	current_hash := util.githash(true)
	// println(v_hash)
	// println(current_hash)
	if v_hash == current_hash {
		app.show_current_v_version()
		return
	}
	$if windows {
		app.backup('cmd/tools/vup.exe')
	}
	app.recompile_v()
	os.exec('"$app.vexe" cmd/tools/vup.v') or { panic(err) }
	app.show_current_v_version()
}

fn (app App) vprintln(s string) {
	if app.is_verbose {
		println(s)
	}
}

fn (app App) update_from_master() {
	app.vprintln('> updating from master ...')
	if !os.exists('.git') {
		// initialize as if it had been cloned
		app.git_command('init')
		app.git_command('remote add origin https://github.com/vlang/v')
		app.git_command('fetch')
		app.git_command('reset --hard origin/master')
		app.git_command('clean --quiet -xdf --exclude v.exe --exclude cmd/tools/vup.exe')
	} else {
		// pull latest
		app.git_command('pull https://github.com/vlang/v master')
	}
}

fn (app App) recompile_v() {
	// NB: app.vexe is more reliable than just v (which may be a symlink)
	opts := if app.is_prod { '-prod' } else { '' }
	vself := '"$app.vexe" $opts self'
	app.vprintln('> recompiling v itself with `$vself` ...')
	if self_result := os.exec(vself) {
		if self_result.exit_code == 0 {
			println(self_result.output.trim_space())
			return
		} else {
			app.vprintln('`$vself` failed, running `make`...')
			app.vprintln(self_result.output.trim_space())
		}
	}
	app.make(vself)
}

fn (app App) make(vself string) {
	mut make := 'make'
	$if windows {
		make = 'make.bat'
	}
	make_result := os.exec(make) or { panic(err) }
	app.vprintln(make_result.output)
}

fn (app App) show_current_v_version() {
	if vout := os.exec('"$app.vexe" version') {
		mut vversion := vout.output.trim_space()
		if vout.exit_code == 0 {
			latest_v_commit := vversion.split(' ').last().all_after('.')
			if latest_v_commit_time := os.exec('git show -s --format=%ci $latest_v_commit') {
				if latest_v_commit_time.exit_code == 0 {
					vversion += ', timestamp: ' + latest_v_commit_time.output.trim_space()
				}
			}
		}
		println('Current V version:')
		println(vversion)
	}
}

fn (app App) backup(file string) {
	backup_file := '${file}_old.exe'
	if os.exists(backup_file) {
		os.rm(backup_file) or { eprintln('failed removing $backup_file: $err') }
	}
	os.mv(file, backup_file) or { eprintln('failed moving $file: $err') }
}

fn (app App) git_command(command string) {
	app.vprintln('git_command: git $command')
	git_result := os.exec('git $command') or {
		app.get_git()
		// Try it again with (maybe) git installed
		os.exec('git $command') or { panic(err) }
	}
	if git_result.exit_code != 0 {
		eprintln(git_result.output)
		exit(1)
	}
	app.vprintln(git_result.output)
}

fn (app App) get_git() {
	$if windows {
		println('Downloading git 32 bit for Windows, please wait.')
		// We'll use 32 bit because maybe someone out there is using 32-bit windows
		os.exec('bitsadmin.exe /transfer "vgit" https://github.com/git-for-windows/git/releases/download/v2.30.0.windows.2/Git-2.30.0.2-32-bit.exe "$os.getwd()/git32.exe"') or {
			eprintln('Unable to install git automatically: please install git manually')
			panic(err)
		}
		os.exec('$os.getwd()/git32.exe') or {
			eprintln('Unable to install git automatically: please install git manually')
			panic(err)
		}
	} $else { // Probably some kind of *nix, usually need to get using a package manager.
		eprintln("error: Install `git` using your system's package manager")
	}
}
