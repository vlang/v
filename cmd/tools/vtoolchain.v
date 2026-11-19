module main

import os
import v.toolchain

const args = arguments()

// usage lists what `v toolchain` accepts for `-h`.
const usage = 'Usage: v toolchain <command> [options]\n' +
	'\n' +
	'Commands:\n' +
	'  install <version>  Install a V version into its own directory.\n' +
	'  list               List installed V versions.\n' +
	'  default [version]  Show or set the per-user default toolchain.\n' +
	'  use [version]      Show or set the toolchain pinned in this repository.\n' +
	'  run <version> [--] [args...]  Run a command with a specific toolchain.\n' +
	'\n' +
	'Options:\n' +
	'  -h, -help, --help  Show this help and exit.\n' +
	'\n' +
	'Version:\n' +
	'  master, stable, latest, nightly, or a tag like 0.4.11 / v0.4.11.\n' +
	'  Channels resolve to master in P0; tags keep their v prefix.\n' +
	'\n' +
	'Config:\n' +
	'  V_TOOLCHAIN selects a toolchain for one command.\n' +
	'  v-toolchain.toml / v.mod toolchain key pins a repository.\n' +
	'  ~/.v/settings.toml default_toolchain sets the per-user default.\n' +
	'\n' +
	'Examples:\n' +
	'  v toolchain install 0.4.11\n' +
	'  v toolchain default 0.4.11\n' +
	'  v toolchain use master\n' +
	'  v toolchain run 0.4.11 -- version\n' +
	'  v toolchain list\n'

const help_options = ['-h', '-help', '--help', 'help']

fn wants_help(rest []string) bool {
	for arg in rest {
		if arg == '--' {
			break
		}
		if arg in help_options {
			return true
		}
	}
	return false
}

// tool_args strips the launcher-passed `toolchain` command name.
fn tool_args() []string {
	if args.len > 1 && args[1] == 'toolchain' {
		return args[2..]
	}
	return args[1..]
}

fn unknown_options(rest []string) []string {
	mut unknown := []string{}
	for arg in rest {
		// Everything after `--` belongs to the command run by `toolchain run`.
		if arg == '--' {
			break
		}
		if arg.starts_with('-') && arg !in help_options {
			unknown << arg
		}
	}
	return unknown
}

// normalized_version validates user input, printing usage and exiting on
// failure. All commands that accept a version share this path.
fn normalized_version(raw string) string {
	name := toolchain.normalize_version(raw) or { '' }
	if name == '' {
		eprintln('invalid version `${raw}`: expected master, stable, latest, nightly, or a tag like 0.4.11')
		eprintln(usage.trim_space())
		exit(1)
	}
	return name
}

fn list_toolchains() {
	root := toolchain.configured_root()
	if !os.is_dir(root) {
		println('No toolchains installed in `${root}`.')
		println('Run `v toolchain install <version>`.')
		return
	}
	entries := os.ls(root) or {
		eprintln('could not list `${root}`: ${err.msg()}')
		exit(1)
		return
	}
	mut found := []string{}
	for e in entries.sorted() {
		full := os.join_path(root, e)
		if os.is_dir(full) {
			found << e
		}
	}
	if found.len == 0 {
		println('No toolchains installed in `${root}`.')
		println('Run `v toolchain install <version>`.')
		return
	}
	active := toolchain.select().version
	for f in found {
		marker := if f == active { '*' } else { ' ' }
		println('${marker} ${f}')
	}
}

fn install_version(raw string) {
	version := normalized_version(raw)
	root := toolchain.configured_root()
	os.mkdir_all(root) or {
		eprintln('could not create `${root}`: ${err.msg()}')
		exit(1)
		return
	}
	dest := toolchain.toolchain_dir(root, version)
	if os.is_dir(dest) && os.is_dir(os.join_path(dest, '.git')) {
		println('`${version}` is already installed in `${dest}`.')
		return
	}
	if os.exists(dest) && !os.is_dir(os.join_path(dest, '.git')) {
		eprintln('`${dest}` exists but is not a toolchain checkout; remove it first.')
		exit(1)
		return
	}
	branch := toolchain.branch_for(version)
	remote := toolchain.configured_upstream()
	println('Installing V `${version}` from `${remote}#${branch}` into `${dest}` ...')
	res := os.exec([
		'git',
		'clone',
		'--depth',
		'1',
		'--branch',
		branch,
		remote,
		dest,
	])
	if res.exit_code != 0 {
		os.rmdir_all(dest) or {}
		eprintln('git clone failed:')
		eprintln(res.output.trim_space())
		exit(1)
		return
	}
	if !os.is_dir(os.join_path(dest, 'vlib')) || !os.is_dir(os.join_path(dest, 'cmd')) {
		eprintln('`${dest}` does not look like a V checkout (missing vlib/ or cmd/).')
		exit(1)
		return
	}
	println('Installed `${version}` in `${dest}`.')
	$if windows {
		println('Build it with: cd ${dest} && makev.bat')
	} $else {
		println('Build it with: cd ${dest} && make')
	}
}

fn show_default() {
	path := toolchain.settings_path()
	if raw := toolchain.read_key(path, toolchain.settings_key) {
		println('Default toolchain: ${raw} (${path})')
		return
	}
	println('Default toolchain: ${toolchain.default_version} (${path})')
}

fn set_default(raw string) {
	version := normalized_version(raw)
	path := toolchain.settings_path()
	toolchain.set_key_in_file(path, toolchain.settings_key, version) or {
		eprintln('could not write `${path}`: ${err.msg()}')
		exit(1)
		return
	}
	println('Default toolchain set to `${version}` in `${path}`.')
}

fn show_use() {
	path := toolchain.pin_path()
	if raw := toolchain.read_key(path, toolchain.pin_key) {
		println('Repository toolchain: ${raw} (${path})')
		return
	}
	println('Repository toolchain: not pinned (${path})')
}

fn set_use(raw string) {
	version := normalized_version(raw)
	path := toolchain.pin_path()
	toolchain.set_key_in_file(path, toolchain.pin_key, version) or {
		eprintln('could not write `${path}`: ${err.msg()}')
		exit(1)
		return
	}
	println('Repository toolchain set to `${version}` in `${path}`.')
}

fn run_version(raw string, rest []string) {
	version := normalized_version(raw)
	root := toolchain.configured_root()
	dir := toolchain.toolchain_dir(root, version)
	if !os.is_dir(dir) {
		eprintln('`${version}` is not installed in `${root}`.')
		eprintln('Run `v toolchain install ${version}`.')
		exit(1)
		return
	}
	exe := toolchain.executable_in(dir)
	if exe == '' {
		eprintln('`${version}` is installed in `${dir}` but has not been built yet.')
		$if windows {
			eprintln('Build it with: cd ${dir} && makev.bat')
		} $else {
			eprintln('Build it with: cd ${dir} && make')
		}
		exit(1)
		return
	}
	mut child_args := rest.clone()
	if child_args.len > 0 && child_args[0] == '--' {
		child_args = child_args[1..]
	}
	mut env := os.environ()
	env['VEXE'] = exe
	env['VEXEROOT'] = dir
	env[toolchain.dispatch_env] = '1'
	mut child := os.new_process(exe)
	child.set_args(child_args)
	child.set_environment(env)
	child.wait()
	code := child.code
	child.close()
	exit(code)
}

fn main() {
	rest := tool_args()
	unknown := unknown_options(rest)
	if unknown.len > 0 {
		eprintln('v toolchain: unknown option: ${unknown.join(' ')}')
		eprintln(usage.trim_space())
		exit(1)
		return
	}
	if wants_help(rest) || rest.len == 0 || rest[0] == 'help' {
		println(usage.trim_space())
		return
	}
	match rest[0] {
		'list' {
			if rest.len != 1 {
				eprintln('v toolchain list takes no arguments.')
				eprintln(usage.trim_space())
				exit(1)
				return
			}
			list_toolchains()
		}
		'install' {
			if rest.len != 2 {
				eprintln('Usage: v toolchain install <version>')
				exit(1)
				return
			}
			install_version(rest[1])
		}
		'default' {
			if rest.len == 1 {
				show_default()
			} else if rest.len == 2 {
				set_default(rest[1])
			} else {
				eprintln('Usage: v toolchain default [version]')
				exit(1)
				return
			}
		}
		'use' {
			if rest.len == 1 {
				show_use()
			} else if rest.len == 2 {
				set_use(rest[1])
			} else {
				eprintln('Usage: v toolchain use [version]')
				exit(1)
				return
			}
		}
		'run' {
			if rest.len < 2 {
				eprintln('Usage: v toolchain run <version> [--] [args...]')
				exit(1)
				return
			}
			run_version(rest[1], rest[2..])
		}
		else {
			eprintln('unknown toolchain command `${rest[0]}`.')
			eprintln(usage.trim_space())
			exit(1)
			return
		}
	}
}
