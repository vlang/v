module main

import os
import v.skills
import v.util.version
import v.util.recompilation

const vexe = os.real_path(os.getenv_opt('VEXE') or { @VEXE })

const vroot = os.dir(vexe)
const v_upstream_url = 'https://github.com/vlang/v'
const v_upstream_branch = 'master'

struct App {
	is_verbose bool
	is_prod    bool
	vexe       string
	vroot      string

	skip_v_self   bool // do not run `v self`, effectively enforcing the running of `make` or `makev.bat`
	skip_current  bool // skip the current hash check, enabling easier testing on the same commit, without using docker etc
	update_skills bool // refresh the skills that fell behind, instead of only reporting them
}

const args = arguments()

// usage lists what `v up` accepts, for `-h`.
//
// Kept here rather than delegated to `v help up`, so that asking how to use the
// command does not depend on being able to start the main compiler: `v up` reads
// `VEXE`, which is allowed to be set, and a stale one must not turn `-h` into a
// failure. `v help up` remains the place with the prose; keep the options listed
// here in sync with vlib/v/help/installation/up.txt.
const usage = 'Usage: v up [options]\n' +
	'\n' +
	'Options:\n' +
	'  -v                 Print more details about the update.\n' +
	'  -prod              Compile the updated V with the -prod flag.\n' +
	'  -skills            Refresh the installed agent skills that fell behind.\n' +
	'  -skip_v_self       Rebuild with make or makev.bat instead of `v self`.\n' +
	'  -skip_current      Recompile even when the checkout is already at the\n' +
	'                     revision.\n' +
	'  -h, -help, --help  Show this help and exit.\n' +
	'\n' +
	'See `v help up` for what an update does.\n'

// known_options are the options `v up` acts on. Anything else stops it before
// the update starts, so a mistyped flag cannot pull and rebuild the compiler.
const known_options = ['-v', '-prod', '-skills', '-skip_v_self', '-skip_current']

const help_options = ['-h', '-help', '--help', 'help']

fn wants_help() bool {
	return args.any(it in help_options)
}

// unknown_options returns the arguments that are neither options of `v up` nor
// the `up` command name that the launcher passes along with them.
fn unknown_options() []string {
	return args[1..].filter(it != 'up' && it !in known_options)
}

fn new_app() App {
	return App{
		is_verbose:    '-v' in args
		is_prod:       '-prod' in args
		vexe:          vexe
		vroot:         vroot
		skip_v_self:   '-skip_v_self' in args
		skip_current:  '-skip_current' in args
		update_skills: '-skills' in args
	}
}

fn main() {
	if wants_help() {
		// Checked before anything else, because asking how to use the command
		// must not update the compiler.
		println(usage.trim_space())
		exit(0)
	}
	unknown := unknown_options()
	if unknown.len > 0 {
		eprintln('v up: unknown option: ${unknown.join(' ')}')
		eprintln(usage.trim_space())
		exit(1)
	}
	app := new_app()
	recompilation.must_be_enabled(app.vroot, 'Please install V from source, to use `v up` .')
	os.chdir(app.vroot)!
	println('Updating V...')
	app.update_from_master()
	if !app.update_tcc() {
		app.show_current_v_version()
		eprintln('Updating TCC *failed*.')
		eprintln('Try running `${get_tcc_update_cmd()}` .')
		exit(1)
	}
	current_v_hash := app.current_v_hash() or {
		// A fallback-built tool can restore a missing primary compiler at its own
		// revision. An existing compiler with an unknown revision must rebuild.
		if !os.exists(app.current_vexe_path()) { @VCURRENTHASH } else { '' }
	}
	current_hash_from_filesystem := version.githash(vroot) or { '' }
	if !app.skip_current && !app.is_prod && !app.skip_v_self
		&& current_v_hash != '' && current_hash_from_filesystem != ''
		&& current_v_hash == current_hash_from_filesystem {
		println('V is already updated.')
		current_vexe_path := app.current_vexe_path()
		if !os.exists(current_vexe_path) {
			eprintln('`${current_vexe_path}` is missing, trying `${get_make_cmd_name()}` to restore it...')
			if !app.make('') {
				app.show_current_v_version()
				eprintln('Recompiling V *failed*.')
				eprintln('Try running `${get_make_cmd_name()}` .')
				exit(1)
			}
		}
		app.show_current_v_version()
		app.report_skills()
		return
	}
	if os.user_os() == 'windows' {
		app.backup('cmd/tools/vup.exe')
	}
	if app.skip_current || app.is_prod || app.skip_v_self || current_v_hash == ''
		|| current_hash_from_filesystem == '' || !app.compiler_sources_unchanged() {
		if !app.recompile_v() {
			app.show_current_v_version()
			eprintln('Recompiling V *failed*.')
			eprintln('Try running `${get_make_cmd_name()}` .')
			exit(1)
		}
	} else {
		println('> compiler sources did not change, not recompiling V.')
	}
	if !app.recompile_vup() {
		app.show_current_v_version()
		eprintln('`v up` failed. Run `cd ${os.quoted_path(app.vroot)} && ${v_upstream_pull_command()} && ${get_make_cmd_name()}` to finish updating V.')
		exit(1)
	}
	app.show_current_v_version()
	app.report_skills()
}

// skills_refresh_hint is what the report tells the user to run.
//
// `--global` is not optional wording: the report reads the skills installed for
// the whole machine, while `v skills update` defaults to the project directory.
// Without the flag a user following the hint from a project would refresh a
// different installation than the one that was just reported.
fn skills_refresh_hint() string {
	return '`v up -skills` or `v skills update --global`'
}

// skills_lines is what the report says about the installed skills.
//
// Held-back skills are named whether or not anything can be refreshed, so an
// installation holding only skills this update must not touch is still
// reported rather than passing in silence. They are either edited locally or
// have no install record, and only `v skills update` tells the two apart, so
// the line names both and points there.
fn skills_lines(refreshable []string, held_back []string) []string {
	mut lines := []string{}
	if refreshable.len > 0 {
		lines << '> skills: can be refreshed: ${refreshable.join(', ')}'
		lines << '> skills: run ${skills_refresh_hint()} to refresh them'
	}
	if held_back.len > 0 {
		lines << '> skills: left alone because they were edited locally or have no install record: ${held_back.join(', ')}'
		lines << '> skills: run `v skills update --global --dry-run` to see why, or `v skills update --global --force` to overwrite them'
	}
	return lines
}

// report_skills tells the user about the skills that the pull left behind, and
// refreshes them when `-skills` was passed.
//
// It says nothing when every installed skill already matches its bundle, so a
// `v up` that changed no skills reads the same as it always did. Without
// `-skills` it writes nothing at all: the skills belong to the user, and
// updating the compiler is not consent to overwrite what they wrote.
//
// The skills installed for the whole machine are the ones that matter here, so
// this looks at the home directory rather than at the checkout it chdir'd into.
fn (app App) report_skills() {
	dir := skills.target_dir(.home_dir, app.vroot)
	refreshable, held_back := skills.refresh_candidates(app.vroot, dir)
	if refreshable.len == 0 && held_back.len == 0 {
		return
	}
	if app.update_skills {
		if refreshable.len > 0 {
			app.refresh_skills()
			// `v skills` has already named every skill it refreshed and every one
			// it held back, so saying it again here would only repeat it.
			return
		}
		// Nothing to refresh, so `v skills` was not run, and the held-back skills
		// have to be named from here or not at all.
		for line in skills_lines(refreshable, held_back) {
			println(line)
		}
		return
	}
	for line in skills_lines(refreshable, held_back) {
		println(line)
	}
}

// refresh_skills hands the refresh to `v skills`, which owns the rule for what
// may be overwritten, so the rule is stated and tested in one place.
//
// The child's output is forwarded rather than captured and dropped: a refusal
// the user cannot see is the same as one that did not happen.
//
// Its exit code does not fail `v up`. It reports a non-zero status exactly when
// it holds a skill back, which is expected here and is not a failure of the
// compiler update that just succeeded.
fn (app App) refresh_skills() {
	refresh := os.exec([app.current_vexe_path(), 'skills', 'update', '--global'])
	report := refresh.output.trim_space()
	for line in report.split_into_lines() {
		println(line)
	}
	if refresh.exit_code < 0 || report == '' {
		// The child could not be started (`os.exec` then reports that as its
		// output, with a negative status), or it said nothing, so the skills it
		// would have refreshed are not accounted for anywhere.
		eprintln('> skills: could not refresh them; run `v skills update --global`')
	}
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
		app.git_command('git remote add origin ${v_upstream_url}')
		app.git_command('git fetch')
		app.git_command('git remote set-head origin ${v_upstream_branch}')
		app.git_command('git reset --hard origin/${v_upstream_branch}')
		// Note 1: patterns starting with /, will match only against the root;
		//         `--exclude v` will match also vlib/v/ in addition to ./v; `--exclude /v` will only match ./v
		// Note 2: patterns ending with / are treated as folders.
		app.git_command('git clean -xfd --exclude /thirdparty/tcc/ --exclude /v --exclude /v.exe --exclude /v_old --exclude /v_old.exe --exclude /${app.current_vexe_name()} --exclude /${app.current_vbackup_name()} --exclude /.bin/ --exclude /cmd/tools/vup --exclude /cmd/tools/vup.exe')
	} else {
		// Rebase avoids merge failures when the local checkout has unrelated history.
		app.git_command(v_upstream_pull_command())
	}
}

fn v_upstream_pull_command() string {
	return 'git pull --rebase ${v_upstream_url} ${v_upstream_branch}'
}

fn (app App) update_tcc() bool {
	command := get_tcc_update_cmd()
	if os.user_os() != 'windows' {
		make_sure_cmd_is_available(get_tcc_make_cmd_name())
	}
	println('> updating TCC ...')
	result := os.exec(if os.user_os() == 'windows' && command == 'makev.bat' {
		['cmd.exe', '/d', '/c', 'makev.bat']
	} else {
		os.split_args(command) or { panic(err) }
	})
	if result.exit_code != 0 {
		eprintln('> `${command}` failed:')
		eprintln(result.output)
		return false
	}
	app.vprintln(result.output)
	println('> done updating TCC.')
	return true
}

// compiler_sources_unchanged checks whether nothing the compiler is built from
// differs from the revision the current `v` executable was built at.
fn (app App) compiler_sources_unchanged() bool {
	built_hash := app.current_v_hash() or { return false }
	// Compiler dependencies extend beyond the core modules (for example crypto.sha256,
	// runtime and sync). Conservatively include all vlib implementation sources.
	diff := os.exec(['git', 'diff', '--quiet', built_hash, '--', 'cmd/v/', 'vlib/', 'thirdparty/',
		'v.mod', 'GNUmakefile', 'Makefile', 'makev.bat', ':(exclude)*_test.v', ':(exclude)*.md'])
	return diff.exit_code == 0
}

fn (app App) recompile_v() bool {
	// Note: app.vexe is more reliable than just v (which may be a symlink)
	vexe_path := app.current_vexe_path()
	if !os.exists(vexe_path) {
		println('> `${app.vexe}` is missing, running `make`...')
		return app.make('')
	}
	opts := if app.is_prod { '-prod' } else { '' }
	vself := '${os.quoted_path(vexe_path)} ${opts} self'
	if app.skip_v_self {
		return app.make(vself)
	}

	// Let `v self` inherit stdio instead of buffering all of its output. Rebuilding
	// a V3-enabled compiler can take several seconds, and hiding the initial status
	// makes `v up` appear to hang after TCC. On Windows the default `os.Process`
	// launch does not wire inherited standard handles into STARTUPINFO, while
	// `os.system` does preserve redirected stdout and stderr through `_wsystem`.
	mut self_exit_code := -1
	$if windows {
		self_exit_code = os.system_args([vexe_path, ...(os.split_args(opts) or { panic(err) }),
			'self'])
	} $else {
		mut self_process := os.new_process(vexe_path)
		self_process.set_args(if app.is_prod { ['-prod', 'self'] } else { ['self'] })
		self_process.wait()
		self_exit_code = self_process.code
		self_process.close()
	}
	if self_exit_code == 0 {
		println('> Done recompiling.')
		return true
	}
	println('> `${vself}` failed, running `make`...')
	return app.make(vself)
}

fn (app App) recompile_vup() bool {
	eprintln('> Recompiling vup.v ...')
	vexe_path := app.current_vexe_path()
	if !os.exists(vexe_path) {
		eprintln('> Skipping recompiling vup.v, `${vexe_path}` is missing.')
		return false
	}
	// `-gc none` matches how `util.launch_tool` builds vup, so this self-rebuild
	// after a successful update does not overwrite the GC-free executable with a
	// libgc-linked one (which could fail to start in the dynamic loader). See #27148.
	vup_result := os.exec([vexe_path, '-g', '-gc', 'none', 'cmd/tools/vup.v'])
	if vup_result.exit_code != 0 {
		eprintln('> Failed recompiling vup.v .')
		eprintln(vup_result.output)
		return false
	}
	return true
}

fn (app App) make(_vself string) bool {
	println('> running make ...')
	make := get_make_cmd_name()
	make_result := os.exec(if os.user_os() == 'windows' {
		['cmd.exe', '/d', '/c', 'makev.bat']
	} else {
		[make]
	})
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
	vexe_path := app.current_vexe_path()
	if !os.exists(vexe_path) {
		println('Current V version: unavailable (`${vexe_path}` is missing).')
		return
	}
	vout := os.exec([vexe_path, 'version'])
	if vout.exit_code >= 0 {
		mut vversion := vout.output.trim_space()
		if vout.exit_code == 0 {
			latest_v_commit := vversion.split(' ').last().all_after('.')
			latest_v_commit_time := os.exec(['git', 'show', '-s', '--format=%ci', '${latest_v_commit}'])
			if latest_v_commit_time.exit_code == 0 {
				vversion += ', timestamp: ' + latest_v_commit_time.output.trim_space()
			}
		}
		println('Current V version: ${vversion}')
	}
}

fn (app App) current_v_hash() ?string {
	vexe_path := app.current_vexe_path()
	if !os.exists(vexe_path) {
		return none
	}
	vout := os.exec([vexe_path, 'version'])
	if vout.exit_code != 0 {
		return none
	}
	for line in vout.output.split_into_lines() {
		fields := line.trim_space().fields()
		if fields.len != 3 || fields[0] != 'V' {
			continue
		}
		hash := fields[2].all_after_last('.')
		if hash.len >= 7 && hash[..7].bytes().all(it.is_hex_digit()) {
			return hash[..7]
		}
	}
	return none
}

fn (app App) current_vexe_name() string {
	vexe_name := os.file_name(app.vexe)
	if vexe_name == '' {
		return if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	}
	return vexe_name
}

fn (app App) current_vbackup_name() string {
	vexe_name := app.current_vexe_name()
	short_v_name := vexe_name.all_before('.')
	return if os.user_os() == 'windows' { '${short_v_name}_old.exe' } else { '${short_v_name}_old' }
}

fn (app App) current_vexe_path() string {
	// The V3 dispatcher delegates building `vup` to `v1_fallback`. In that case
	// @VEXE identifies the fallback, but `v up` must inspect and rebuild the main
	// compiler next to it.
	if os.file_name(app.vexe) in ['v1_fallback', 'v1_fallback.exe'] {
		primary_vexe := os.join_path_single(app.vroot, if os.user_os() == 'windows' {
			'v.exe'
		} else {
			'v'
		})
		return primary_vexe
	}
	if os.exists(app.vexe) {
		return app.vexe
	}
	default_vexe := os.join_path_single(app.vroot, if os.user_os() == 'windows' {
		'v.exe'
	} else {
		'v'
	})
	if os.exists(default_vexe) {
		return default_vexe
	}
	configured_vexe := os.join_path_single(app.vroot, app.current_vexe_name())
	if os.exists(configured_vexe) {
		return configured_vexe
	}
	return app.vexe
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
	git_result := os.exec(os.split_args(command) or { panic(err) })
	if git_result.exit_code < 0 {
		app.install_git()
		// Try it again with (maybe) git installed
		os.exec_or_exit(os.split_args(command) or { panic(err) })
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
	res_download :=
		os.exec(['bitsadmin.exe', '/transfer', 'vgit',
			'https://github.com/git-for-windows/git/releases/download/v2.30.0.windows.2/Git-2.30.0.2-32-bit.exe',
			'${os.getwd()}' + '/git32.exe'])
	if res_download.exit_code != 0 {
		eprintln('Unable to install git automatically: please install git manually')
		panic(res_download.output)
	}
	res_git32 := os.exec([os.join_path_single(os.getwd(), 'git32.exe')])
	if res_git32.exit_code != 0 {
		eprintln('Unable to install git automatically: please install git manually')
		panic(res_git32.output)
	}
}

fn get_make_cmd_name() string {
	if os.user_os() == 'windows' {
		return 'makev.bat'
	}
	cmd := 'make'
	make_sure_cmd_is_available(cmd)
	cc := os.getenv_opt('CC') or { 'cc' }
	make_sure_cmd_is_available(cc)
	return cmd
}

fn get_tcc_update_cmd() string {
	return '${get_tcc_make_cmd_name()} latest_tcc'
}

fn get_tcc_make_cmd_name() string {
	return match os.user_os() {
		'windows' { 'makev.bat' }
		'freebsd', 'openbsd', 'netbsd', 'dragonfly', 'solaris' { 'gmake' }
		else { 'make' }
	}
}

fn make_sure_cmd_is_available(cmd string) {
	found_path := os.find_abs_path_of_executable(cmd) or {
		eprintln('Could not find `${cmd}` in PATH. Please install `${cmd}`, since `v up` needs it.')
		exit(1)
	}
	println('Found `${cmd}` as `${found_path}`.')
}
