module main

import os
import crypto.sha256
import encoding.hex
import json2
import net.http
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
	'  update [version]   Pull and rebuild a toolchain, keeping the old one on failure.\n' +
	'  remove <version>   Remove an installed toolchain.\n' +
	'  gc                 Remove stale update and removal leftovers.\n' +
	'\n' +
	'Options:\n' +
	'  -h, -help, --help  Show this help and exit.\n' +
	'\n' +
	'Version:\n' +
	'  master, stable, latest, nightly, or a tag like 0.4.11 / v0.4.11.\n' +
	'  stable/latest resolve to master; nightly has its own directory.\n' +
	'\n' +
	'Config:\n' +
	'  V_TOOLCHAIN selects a toolchain for one command.\n' +
	'  v-toolchain.toml / v.mod toolchain key pins a repository.\n' +
	'  ~/.v/settings.toml default_toolchain sets the per-user default.\n' +
	'  versions.json lists prebuilt bundles with url and sha256.\n' +
	'  V_TOOLCHAIN_CACHE_DIR reuses thirdparty/tcc and vc across installs.\n' +
	'\n' +
	'Examples:\n' +
	'  v toolchain install 0.4.11\n' +
	'  v toolchain default 0.4.11\n' +
	'  v toolchain use master\n' +
	'  v toolchain run 0.4.11 -- version\n' +
	'  v toolchain update 0.4.11\n' +
	'  v toolchain remove 0.4.7\n' +
	'  v toolchain gc\n' +
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
	forward_up_options := rest.len > 0 && rest[0] == 'update'
	for arg in rest {
		// Everything after `--` belongs to the command run by `toolchain run`.
		if arg == '--' {
			break
		}
		if arg.starts_with('-') && arg !in help_options
			&& !(forward_up_options && arg in toolchain.up_options) {
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

struct Release {
	url    string
	sha256 string
}

// load_releases reads the optional versions.json manifest. An absent or
// malformed manifest simply disables prebuilt installs.
fn load_releases() map[string]Release {
	path := toolchain.manifest_path()
	if !os.is_file(path) {
		return map[string]Release{}
	}
	text := os.read_file(path) or {
		return map[string]Release{}
	}
	return json2.decode[map[string]Release](text) or {
		return map[string]Release{}
	}
}

fn verify_sha256(path string, expected string) ! {
	data := os.read_file(path)!
	actual := hex.encode(sha256.sum(data.bytes()))
	if actual.to_lower() != expected.trim_space().to_lower() {
		return error('checksum mismatch: expected ${expected}, got ${actual}')
	}
}

// install_prebuilt installs a manifest-provided bundle. Local directories are
// copied directly; files and URLs are treated as tar archives.
fn install_prebuilt(version string, release Release, dest string) ! {
	if !os.is_dir(release.url) && release.sha256.trim_space() == '' {
		return error('manifest entry for `${version}` has no sha256 for a downloaded bundle')
	}
	staging := dest + '_stage'
	os.rmdir_all(staging) or {}
	if os.is_dir(release.url) {
		copy_dir(release.url, staging)!
	} else {
		archive := os.join_path(os.vtmp_dir(), 'vtoolchain_${os.getpid()}.archive')
		os.rm(archive) or {}
		if os.is_file(release.url) {
			os.cp(release.url, archive)!
		} else {
			http.download_file(release.url, archive)!
		}
		if release.sha256 != '' {
			verify_sha256(archive, release.sha256) or {
				os.rm(archive) or {}
				return err
			}
		}
		os.mkdir_all(staging)!
		res := os.exec(['tar', '-xf', archive, '-C', staging])
		os.rm(archive) or {}
		if res.exit_code != 0 {
			os.rmdir_all(staging) or {}
			return error('could not extract prebuilt archive: ${res.output.trim_space()}')
		}
	}
	if !os.is_dir(os.join_path(staging, 'vlib')) || !os.is_dir(os.join_path(staging, 'cmd')) {
		os.rmdir_all(staging) or {}
		return error('prebuilt bundle has no vlib/ or cmd/')
	}
	os.rename(staging, dest)!
	println('Installed prebuilt V `${version}` from `${release.url}` into `${dest}`.')
}

fn share_cache_from(src string, dst string) {
	if !os.is_dir(src) {
		return
	}
	os.rmdir_all(dst) or {}
	copy_dir(src, dst) or {
		eprintln('could not reuse shared cache `${src}` at `${dst}`: ${err.msg()}')
	}
}

// apply_shared_cache reuses an opt-in cache of TCC and vc instead of
// downloading them again into every checkout.
fn apply_shared_cache(dir string) {
	cache := toolchain.shared_cache_dir()
	if cache == '' {
		return
	}
	share_cache_from(os.join_path(cache, 'thirdparty', 'tcc'), os.join_path(dir, 'thirdparty', 'tcc'))
	share_cache_from(os.join_path(cache, 'vc'), os.join_path(dir, 'vc'))
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
	if os.is_dir(dest) && (os.is_dir(os.join_path(dest, '.git'))
		|| os.is_dir(os.join_path(dest, 'vlib'))) {
		println('`${version}` is already installed in `${dest}`.')
		return
	}
	if os.exists(dest) {
		eprintln('`${dest}` exists but is not a toolchain checkout; remove it first.')
		exit(1)
		return
	}
	releases := load_releases()
	if version in releases {
		install_prebuilt(version, releases[version], dest) or {
			eprintln('prebuilt install failed: ${err.msg()}')
			exit(1)
			return
		}
		apply_shared_cache(dest)
		if toolchain.executable_in(dest) != '' {
			println('`${version}` is ready to use.')
		} else {
			println('Build it with: cd ${dest} && make')
		}
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
	apply_shared_cache(dest)
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

struct Checkout {
	dir string
	exe string
}

// installed_checkout returns the paths of a checkout that is both installed
// and already built.
fn installed_checkout(version string) ?Checkout {
	dir := toolchain.toolchain_dir(toolchain.configured_root(), version)
	// Prebuilt installs have no .git; they are identifiable by their layout.
	if !os.is_dir(os.join_path(dir, '.git'))
		&& !(os.is_dir(os.join_path(dir, 'vlib')) && os.is_dir(os.join_path(dir, 'cmd'))) {
		return none
	}
	exe := toolchain.executable_in(dir)
	if exe == '' {
		return none
	}
	return Checkout{
		dir: dir
		exe: exe
	}
}

// git_head returns the HEAD revision of the checkout in `dir`, or '' when
// git is unavailable or `dir` is not a git checkout. It lets update verify
// that a failed run left the original checkout untouched.
fn git_head(dir string) string {
	res := os.exec(['git', '-C', dir, 'rev-parse', 'HEAD'])
	if res.exit_code != 0 {
		return ''
	}
	return res.output.trim_space()
}

// copy_dir copies a checkout into a staging directory. The original stays
// untouched while `v up` runs in the copy, so a failure cannot mutate it.
fn copy_dir(src string, dst string) ! {
	os.mkdir_all(dst)!
	for entry in os.ls(src)! {
		from := os.join_path(src, entry)
		to := os.join_path(dst, entry)
		// Preserve links instead of following them: an untrusted bundle could
		// otherwise contain a link to its own parent and recurse forever.
		if os.is_link(from) {
			os.symlink(os.readlink(from)!, to)!
		} else if os.is_dir(from) {
			copy_dir(from, to)!
		} else {
			os.cp(from, to)!
		}
	}
}

// update_version updates a separate staging checkout and flips it into place
// only on success. A failed update leaves the original checkout untouched.
fn update_version(raw string, rest []string) {
	version := normalized_version(raw)
	checkout := installed_checkout(version) or {
		eprintln('`${version}` is not an installed and built toolchain.')
		eprintln('Run `v toolchain install ${version}` and build it first.')
		exit(1)
		return
	}
	staging := checkout.dir + '_stage'
	old := checkout.dir + '_old'
	for stale in [staging, old] {
		if os.exists(stale) {
			os.rmdir_all(stale) or {
				eprintln('could not remove stale `${stale}`; run `v toolchain gc`: ${err.msg()}')
				exit(1)
				return
			}
		}
	}
	// A prebuilt checkout has no git history to pull. Reinstall it from the
	// manifest, keeping the old directory as the rollback point.
	if !os.is_dir(os.join_path(checkout.dir, '.git')) {
		releases := load_releases()
		if version !in releases {
			eprintln('`${version}` is a prebuilt toolchain with no manifest entry; reinstall it with `v toolchain install ${version}`.')
			exit(1)
			return
		}
		os.rename(checkout.dir, old) or {
			eprintln('could not move `${checkout.dir}` aside: ${err.msg()}')
			exit(1)
			return
		}
		install_prebuilt(version, releases[version], checkout.dir) or {
			os.rename(old, checkout.dir) or {}
			eprintln('prebuilt update failed: ${err.msg()}')
			exit(1)
			return
		}
		apply_shared_cache(checkout.dir)
		os.rmdir_all(old) or {}
		println('Updated prebuilt `${version}` in `${checkout.dir}`.')
		return
	}
	original_head := git_head(checkout.dir)
	copy_dir(checkout.dir, staging) or {
		eprintln('could not stage `${checkout.dir}` into `${staging}`: ${err.msg()}')
		exit(1)
		return
	}
	if original_head != '' && git_head(staging) != original_head {
		os.rmdir_all(staging) or {}
		eprintln('could not stage `${checkout.dir}`: staged HEAD differs; the original checkout is unchanged.')
		exit(1)
		return
	}
	exe := toolchain.executable_in(staging)
	if exe == '' {
		os.rmdir_all(staging) or {}
		eprintln('`${staging}` has no built compiler; the original checkout is unchanged.')
		exit(1)
		return
	}
	println('Updating `${version}` in `${staging}` ...')
	mut child_args := ['up']
	child_args << rest
	mut environment := os.environ()
	environment['VEXE'] = exe
	environment['VEXEROOT'] = staging
	environment[toolchain.dispatch_env] = '1'
	mut child := os.new_process(exe)
	child.set_args(child_args)
	child.set_environment(environment)
	child.wait()
	code := child.code
	child.close()
	if code != 0 {
		if original_head != '' && git_head(checkout.dir) != original_head {
			os.rmdir_all(staging) or {}
			eprintln('update of `${version}` failed and the original checkout changed unexpectedly.')
			exit(code)
			return
		}
		os.rmdir_all(staging) or {}
		eprintln('update of `${version}` failed; the original checkout in `${checkout.dir}` is unchanged.')
		exit(code)
		return
	}
	// Only now replace the original with the successfully updated staging tree.
	os.rename(checkout.dir, old) or {
		eprintln('could not move the original checkout aside: ${err.msg()}')
		exit(1)
		return
	}
	os.rename(staging, checkout.dir) or {
		os.rename(old, checkout.dir) or {}
		eprintln('could not activate `${checkout.dir}`; the original was restored')
		exit(1)
		return
	}
	// Windows may keep the old compiler locked; gc removes that leftover.
	os.rmdir_all(old) or {}
	println('Updated `${version}` in `${checkout.dir}`.')
}

// remove_version removes an installed checkout, using a rename so a failed
// removal leaves a recoverable `_removing` directory for `gc`.
fn remove_version(raw string) {
	version := normalized_version(raw)
	dir := toolchain.toolchain_dir(toolchain.configured_root(), version)
	if !os.is_dir(dir) {
		eprintln('`${version}` is not installed in `${dir}`.')
		exit(1)
		return
	}
	if toolchain.select().version == version {
		println('Note: `${version}` is the active toolchain; clear its pin or default too.')
	}
	temp := dir + '_removing'
	os.rmdir_all(temp) or {}
	mut moved := true
	os.rename(dir, temp) or {
		moved = false
	}
	if moved {
		os.rmdir_all(temp) or {
			eprintln('could not remove `${temp}`; run `v toolchain gc`: ${err.msg()}')
			exit(1)
			return
		}
	} else {
		os.rmdir_all(dir) or {
			eprintln('could not remove `${dir}`: ${err.msg()}')
			exit(1)
			return
		}
	}
	println('Removed `${version}` from `${dir}`.')
}

// gc_toolchains removes update and removal leftovers plus directories that
// are not complete checkouts.
fn gc_toolchains() {
	root := toolchain.configured_root()
	if !os.is_dir(root) {
		println('Nothing to clean in `${root}`.')
		return
	}
	entries := os.ls(root) or {
		eprintln('could not list `${root}`: ${err.msg()}')
		exit(1)
		return
	}
	mut removed := 0
	for e in entries {
		full := os.join_path(root, e)
		if !os.is_dir(full) {
			continue
		}
		// Prebuilt installs have no .git but are still real toolchains; only
		// remove directories that are neither a git checkout nor a V layout.
		complete := os.is_dir(os.join_path(full, '.git'))
			|| (os.is_dir(os.join_path(full, 'vlib')) && os.is_dir(os.join_path(full, 'cmd')))
		stale := e.ends_with('_old') || e.ends_with('_removing') || e.ends_with('_stage')
			|| e.ends_with('.stage') || !complete
		if !stale {
			continue
		}
		os.rmdir_all(full) or {
			eprintln('could not remove `${full}`: ${err.msg()}')
			continue
		}
		println('Removed `${full}`.')
		removed++
	}
	if removed == 0 {
		println('Nothing to clean in `${root}`.')
	}
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
		'update' {
			mut version := toolchain.select().version
			mut forwarded := []string{}
			if rest.len > 1 {
				if rest[1].starts_with('-') {
					forwarded = rest[1..]
				} else {
					version = rest[1]
					if rest.len > 2 {
						forwarded = rest[2..]
					}
				}
			}
			update_version(version, forwarded)
		}
		'remove' {
			if rest.len != 2 {
				eprintln('Usage: v toolchain remove <version>')
				exit(1)
				return
			}
			remove_version(rest[1])
		}
		'gc' {
			if rest.len != 1 {
				eprintln('v toolchain gc takes no arguments.')
				exit(1)
				return
			}
			gc_toolchains()
		}
		else {
			eprintln('unknown toolchain command `${rest[0]}`.')
			eprintln(usage.trim_space())
			exit(1)
			return
		}
	}
}
