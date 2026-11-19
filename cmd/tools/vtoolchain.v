module main

import os

const v_toolchain_upstream_url = 'https://github.com/vlang/v'

const args = arguments()

// usage lists what `v toolchain` accepts for `-h`.
const usage = 'Usage: v toolchain <command> [options]\n' +
	'\n' +
	'Commands:\n' +
	'  install <version>  Install a V version into its own directory.\n' +
	'  list               List installed V versions.\n' +
	'\n' +
	'Options:\n' +
	'  -h, -help, --help  Show this help and exit.\n' +
	'\n' +
	'Version:\n' +
	'  master, stable, latest, nightly, or a tag like 0.4.11 / v0.4.11.\n' +
	'  P0 installs from git; channels currently resolve to master.\n' +
	'\n' +
	'Examples:\n' +
	'  v toolchain install 0.4.11\n' +
	'  v toolchain list\n'

const help_options = ['-h', '-help', '--help', 'help']

fn wants_help() bool {
	return args.any(it in help_options)
}

// tool_args strips the launcher-passed `toolchain` command name.
fn tool_args() []string {
	if args.len > 1 && args[1] == 'toolchain' {
		return args[2..]
	}
	return args[1..]
}

fn unknown_options(rest []string) []string {
	return rest.filter(it.starts_with('-') && it !in help_options)
}

// toolchains_root returns where toolchains live.
// V_TOOLCHAINS_DIR overrides the default for tests.
fn toolchains_root() string {
	if e := os.getenv_opt('V_TOOLCHAINS_DIR') {
		if e.trim_space() != '' {
			return e
		}
	}
	return os.join_path(os.home_dir(), '.v', 'toolchains')
}

// upstream_url returns the git remote to clone from.
// V_TOOLCHAIN_UPSTREAM overrides it for offline tests.
fn upstream_url() string {
	if e := os.getenv_opt('V_TOOLCHAIN_UPSTREAM') {
		if e.trim_space() != '' {
			return e
		}
	}
	return v_toolchain_upstream_url
}

// is_version_number reports whether `s` looks like `0.4` or `0.4.11`.
fn is_version_number(s string) bool {
	parts := s.split('.')
	if parts.len < 2 || parts.len > 3 {
		return false
	}
	for p in parts {
		if p.len == 0 {
			return false
		}
		for c in p {
			if !c.is_digit() {
				return false
			}
		}
	}
	return true
}

// normalize_version maps user input to a git ref.
// Channels resolve to master in P0; tags keep their `v` prefix.
fn normalize_version(input string) ?string {
	s := input.trim_space().to_lower()
	if s == '' {
		return none
	}
	if s in ['master', 'stable', 'latest', 'nightly'] {
		return 'master'
	}
	mut v := s
	if v.starts_with('v') {
		v = v[1..]
	}
	if !is_version_number(v) {
		return none
	}
	return 'v' + v
}

// toolchain_dir returns the install directory for a normalized version.
fn toolchain_dir(root string, version string) string {
	return os.join_path(root, version)
}

// branch_for returns the git branch/tag to clone for a normalized version.
fn branch_for(version string) string {
	return version
}

fn list_toolchains() {
	root := toolchains_root()
	if !os.is_dir(root) {
		println('No toolchains installed in `${root}`.')
		println('Run `v toolchain install <version>`.')
		return
	}
	entries := os.ls(root) or {
		eprintln('could not list `${root}`: ${err.msg()}')
		exit(1)
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
	for f in found {
		println(f)
	}
}

fn install_version(raw string) {
	version := normalize_version(raw) or {
		eprintln('invalid version `${raw}`: expected master, stable, latest, nightly, or a tag like 0.4.11')
		eprintln(usage.trim_space())
		exit(1)
	}
	root := toolchains_root()
	os.mkdir_all(root) or {
		eprintln('could not create `${root}`: ${err.msg()}')
		exit(1)
	}
	dest := toolchain_dir(root, version)
	if os.is_dir(dest) && os.is_dir(os.join_path(dest, '.git')) {
		println('`${version}` is already installed in `${dest}`.')
		return
	}
	if os.exists(dest) && !os.is_dir(os.join_path(dest, '.git')) {
		eprintln('`${dest}` exists but is not a toolchain checkout; remove it first.')
		exit(1)
	}
	branch := branch_for(version)
	remote := upstream_url()
	println('Installing V `${version}` from `${remote}#${branch}` into `${dest}` ...')
	res := os.exec(['git', 'clone', '--depth', '1', '--branch', branch, remote, dest])
	if res.exit_code != 0 {
		os.rmdir_all(dest) or {}
		eprintln('git clone failed:')
		eprintln(res.output.trim_space())
		exit(1)
	}
	if !os.is_dir(os.join_path(dest, 'vlib')) || !os.is_dir(os.join_path(dest, 'cmd')) {
		eprintln('`${dest}` does not look like a V checkout (missing vlib/ or cmd/).')
		exit(1)
	}
	println('Installed `${version}` in `${dest}`.')
	$if windows {
		println('Build it with: cd ${dest} && makev.bat')
	} $else {
		println('Build it with: cd ${dest} && make')
	}
}

fn main() {
	rest := tool_args()
	unknown := unknown_options(rest)
	if unknown.len > 0 {
		eprintln('v toolchain: unknown option: ${unknown.join(' ')}')
		eprintln(usage.trim_space())
		exit(1)
	}
	if wants_help() || rest.len == 0 || rest[0] == 'help' {
		println(usage.trim_space())
		return
	}
	if rest[0] == 'list' {
		if rest.len != 1 {
			eprintln('v toolchain list takes no arguments.')
			eprintln(usage.trim_space())
			exit(1)
		}
		list_toolchains()
		return
	}
	if rest[0] == 'install' {
		if rest.len != 2 {
			eprintln('Usage: v toolchain install <version>')
			exit(1)
		}
		install_version(rest[1])
		return
	}
	eprintln('unknown toolchain command `${rest[0]}`.')
	eprintln(usage.trim_space())
	exit(1)
}
