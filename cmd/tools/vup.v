module main

import os
import v.util.version
import v.util.recompilation

const vexe = os.real_path(os.getenv_opt('VEXE') or { @VEXE })

const vroot = os.dir(vexe)

struct App {
	is_verbose bool
	is_prod    bool
	vexe       string
	vroot      string

	skip_v_self  bool // do not run `v self`, effectively enforcing the running of `make` or `make.bat`
	skip_current bool // skip the current hash check, enabling easier testing on the same commit, without using docker etc
}

const args = arguments()

fn new_app() App {
	return App{
		is_verbose:   '-v' in args
		is_prod:      '-prod' in args
		vexe:         vexe
		vroot:        vroot
		skip_v_self:  '-skip_v_self' in args
		skip_current: '-skip_current' in args
	}
}

fn main() {
	app := new_app()
	recompilation.must_be_enabled(app.vroot, 'Please install V from source, to use `v up` .')
	os.chdir(app.vroot)!
	println('Updating V...')
	app.update_from_master()
	hash_when_vup_was_compiled := @VCURRENTHASH
	current_hash_from_filesystem := version.githash(vroot) or { hash_when_vup_was_compiled }
	if !app.skip_current && hash_when_vup_was_compiled == current_hash_from_filesystem {
		println('V is already updated.')
		app.show_current_v_version()
		return
	}
	if os.user_os() == 'windows' {
		app.backup('cmd/tools/vup.exe')
	}
	if !app.recompile_v() {
		app.show_current_v_version()
		eprintln('Recompiling V *failed*.')
		eprintln('Try running `${get_make_cmd_name()}` .')
		exit(1)
	}
	app.recompile_vup()
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
		// initialize the folder, as if it had been cloned:
		app.git_command('git init')
		app.git_command('git remote add origin https://github.com/vlang/v')
		app.git_command('git fetch')
		app.git_command('git remote set-head origin master')
		app.git_command('git reset --hard origin/master')
		// Note 1: patterns starting with /, will match only against the root;
		//         `--exclude v` will match also vlib/v/ in addition to ./v; `--exclude /v` will only match ./v
		// Note 2: patterns ending with / are treated as folders.
		app.git_command('git clean -xfd --exclude /thirdparty/tcc/ --exclude /v --exclude /v.exe --exclude /cmd/tools/vup --exclude /cmd/tools/vup.exe')
	} else {
		// pull latest
		app.git_command('git pull https://github.com/vlang/v master')
	}
}

fn (app App) recompile_v() bool {
	// Note: app.vexe is more reliable than just v (which may be a symlink)
	opts := if app.is_prod { '-prod' } else { '' }
	vself := '${os.quoted_path(app.vexe)} ${opts} self'
	if app.skip_v_self {
		return app.make(vself)
	}

	self_result := os.execute(vself)
	if self_result.exit_code == 0 {
		println(self_result.output.trim_space())
		println('> Done recompiling.')
		return true
	} else {
		println('> `${vself}` failed, running `make`...')
		app.vprintln(self_result.output.trim_space())
	}
	return app.make(vself)
}

fn (app App) recompile_vup() bool {
	eprintln('> Recompiling vup.v ...')
	vup_result := os.execute('${os.quoted_path(app.vexe)} -g cmd/tools/vup.v')
	if vup_result.exit_code != 0 {
		eprintln('> Failed recompiling vup.v .')
		eprintln(vup_result.output)
		return false
	}
	return true
}

fn (app App) make(vself string) bool {
	println('> running make ...')
	make := get_make_cmd_name()
	make_result := os.execute(make)
	if make_result.exit_code != 0 {
		eprintln('> ${make} failed:')
		eprintln('> make output:')
		eprintln(make_result.output)
		return false
	}
	app.vprintln(make_result.output)
	println('> done running make.')
	return true
}

fn (app App) show_current_v_version() {
	vout := os.execute('${os.quoted_path(app.vexe)} version')
	if vout.exit_code >= 0 {
		mut vversion := vout.output.trim_space()
		if vout.exit_code == 0 {
			latest_v_commit := vversion.split(' ').last().all_after('.')
			latest_v_commit_time := os.execute('git show -s --format=%ci ${latest_v_commit}')
			if latest_v_commit_time.exit_code == 0 {
				vversion += ', timestamp: ' + latest_v_commit_time.output.trim_space()
			}
		}
		println('Current V version: ${vversion}')
	}
}

fn (app App) backup(file string) {
	backup_file := '${file}_old.exe'
	println('> backing up `${file}` to `${backup_file}` ...')
	if os.exists(backup_file) {
		os.rm(backup_file) or { eprintln('failed removing ${backup_file}: ${err.msg()}') }
	}
	os.mv(file, backup_file) or { eprintln('failed moving ${file}: ${err.msg()}') }
}

fn (app App) git_command(command string) {
	println('> git_command: ${command}')
	git_result := os.execute(command)
	if git_result.exit_code < 0 {
		app.install_git()
		// Try it again with (maybe) git installed
		os.execute_or_exit(command)
	}
	if git_result.exit_code != 0 {
		eprintln('Failed git command: ${command}')
		eprintln(git_result.output)
		exit(1)
	}
	app.vprintln(git_result.output)
}

fn (app App) install_git() {
	if os.user_os() != 'windows' {
		// Probably some kind of *nix, usually need to get using a package manager.
		eprintln("error: Install `git` using your system's package manager")
	}
	println('Downloading git 32 bit for Windows, please wait.')
	// We'll use 32 bit because maybe someone out there is using 32-bit windows
	res_download := os.execute('bitsadmin.exe /transfer "vgit" https://github.com/git-for-windows/git/releases/download/v2.30.0.windows.2/Git-2.30.0.2-32-bit.exe "${os.getwd()}/git32.exe"')
	if res_download.exit_code != 0 {
		eprintln('Unable to install git automatically: please install git manually')
		panic(res_download.output)
	}
	res_git32 := os.execute(os.quoted_path(os.join_path_single(os.getwd(), 'git32.exe')))
	if res_git32.exit_code != 0 {
		eprintln('Unable to install git automatically: please install git manually')
		panic(res_git32.output)
	}
}

fn get_make_cmd_name() string {
	if os.user_os() == 'windows' {
		return 'make.bat'
	}
	cmd := 'make'
	make_sure_cmd_is_available(cmd)
	cc := os.getenv_opt('CC') or { 'cc' }
	make_sure_cmd_is_available(cc)
	return cmd
}

fn make_sure_cmd_is_available(cmd string) {
	found_path := os.find_abs_path_of_executable(cmd) or {
		eprintln('Could not find `${cmd}` in PATH. Please install `${cmd}`, since `v up` needs it.')
		exit(1)
	}
	println('Found `${cmd}` as `${found_path}`.')
}
