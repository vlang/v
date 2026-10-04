module main

import os
import semver

// Supported version control system commands.
enum VCS {
	git
	hg
}

struct VCSInfo {
	dir  string @[required]
	args struct {
		install  []string @[required]
		version  string   @[required] // flag name; passed as one `--flag=<version>` element.
		path     string   @[required] // flag to specify a path. E.g., used to explicitly work on a path during multithreaded updating.
		update   string   @[required]
		outdated []string @[required]
	}
}

const vcs_info = init_vcs_info() or {
	vpm_error(err.msg())
	exit(1)
}

fn init_vcs_info() !map[VCS]VCSInfo {
	git_installed_raw_ver := parse_git_version(os.exec_opt(['git', '--version'])!.output) or { '' }
	git_installed_ver := semver.from(git_installed_raw_ver)!
	git_shallow_submod_ver := semver.from('2.36.0')!
	mut git_install_args := ['clone', '--recursive']
	if os.user_os() != 'windows' {
		// The variation of environment factors on windows is too high;
		// the following options are known to work well on != windows,
		// but can sometimes cause failures on windows for yet unknown reasons,
		// see https://discord.com/channels/592103645835821068/665558664949530644/1345422482974310440
		// for more details, about why this is now allowed only on != windows platforms.
		// Keep module blobs in the initial pack. Partial clones need another network
		// fetch during checkout, which can stall installations such as `v install ui2`.
		if git_installed_ver >= git_shallow_submod_ver {
			git_install_args << '--shallow-submodules'
		}
	}
	return {
		VCS.git: VCSInfo{
			dir:  '.git'
			args: struct {
				install:  git_install_args
				version:  '--branch'
				update:   'pull --recurse-submodules' // pulling with `--depth=1` leads to conflicts when the upstream has more than 1 new commits.
				path:     '-C'
				outdated: ['fetch', 'rev-parse @', 'rev-parse @{u}']
			}
		}
		VCS.hg:  VCSInfo{
			dir:  '.hg'
			args: struct {
				install:  ['clone']
				version:  '--rev'
				update:   'pull --update'
				path:     '-R'
				outdated: ['incoming']
			}
		}
	}
}

fn (vcs VCS) clone_args(url string, version string, path string) ![]string {
	if version.contains('\0') || version.contains('\r') || version.contains('\n') {
		return error('version contains NUL, CR, or LF')
	}
	// A source like `--upload-pack=...` would be taken for an option, not a url.
	if url.starts_with('-') {
		return error('refusing to clone from `${url}`, which looks like an option')
	}
	info := vcs_info[vcs]
	mut args := [vcs.str()]
	args << info.args.install
	if version != '' {
		if vcs == .git {
			args << '--single-branch'
		}
		args << '${info.args.version}=${version}'
	}
	args << [url, path]
	return args
}

fn (vcs VCS) clone(url string, version string, path string) ! {
	args := vcs.clone_args(url, version, path)!
	vpm_log(@FILE_LINE, @FN, 'cmd: ${args}')
	res := os.exec(args)
	if res.exit_code != 0 {
		return error(res.output)
	}
	vpm_log(@FILE_LINE, @FN, 'cmd output: ${res.output}')
}

fn (vcs &VCS) is_executable() ! {
	cmd := vcs.str()
	os.find_abs_path_of_executable(cmd) or {
		return error('VPM requires that `${cmd}` is executable.')
	}
}

fn vcs_used_in_dir(dir string) ?VCS {
	for vcs, info in vcs_info {
		vcs_path := os.real_path(os.join_path(dir, info.dir))
		if os.is_dir(vcs_path) || (vcs == .git && os.is_file(vcs_path)) {
			return vcs
		}
	}
	return none
}

fn vcs_from_str(str string) ?VCS {
	return match str {
		'git' { .git }
		'hg' { .hg }
		else { none }
	}
}

// head_revision returns the full SHA of the current HEAD of the git checkout
// in `dir`, trimmed, or '' when it cannot be determined, e.g. when `dir` is
// not a checkout at all. `hg` checkouts are reported as ''.
fn head_revision(dir string) string {
	head_vcs := vcs_used_in_dir(dir) or { return '' }
	if head_vcs != .git {
		return ''
	}
	res := os.exec_opt(['git', '-C', dir, 'rev-parse', 'HEAD']) or { return '' }
	if res.exit_code != 0 {
		return ''
	}
	return res.output.trim_space()
}

// head_commit_unix_ts returns the time of the current HEAD of the git checkout
// in `dir`, as a unix timestamp, or 0 when it cannot be determined. `hg`
// checkouts are reported as 0.
fn head_commit_unix_ts(dir string) i64 {
	head_vcs := vcs_used_in_dir(dir) or { return 0 }
	if head_vcs != .git {
		return 0
	}
	res := os.exec_opt(['git', '-C', dir, 'log', '-1', '--format=%ct']) or { return 0 }
	if res.exit_code != 0 {
		return 0
	}
	return res.output.trim_space().i64()
}

// head_is_detached reports whether the git checkout in `dir` is not on a
// branch, e.g. after it was checked out at a locked revision, or cloned at a
// tag. A detached HEAD cannot be pulled; it has to be moved by hand.
fn head_is_detached(dir string) bool {
	os.exec_opt(['git', '-C', dir, 'symbolic-ref', '-q', 'HEAD']) or { return true }
	return false
}

// checkout switches the git checkout in `dir` to the revision `rev`, e.g. the
// full SHA recorded for a module in the lockfile of a project, and moves its
// submodules along. A revision that the checkout does not hold yet, like one
// that a teammate locked after this clone was made, is fetched from the origin
// first. `hg` checkouts are left untouched, since a lockfile records git
// revisions only. A failed checkout is an error: installing whatever HEAD the
// clone happens to sit on instead would silently defeat the pinning the
// lockfile exists for.
fn (vcs VCS) checkout(dir string, rev string) ! {
	if vcs != .git {
		return
	}
	if rev == '' || rev.starts_with('-') || rev.contains_any(' \0\r\n') {
		return error('refusing to checkout the invalid revision `${rev}`.')
	}
	if !git_has_commit(dir, rev) {
		// Fetch the branches of the origin explicitly, since a clone made at a tag
		// with `--single-branch` would otherwise fetch only that tag.
		fetch_args := ['git', '-C', dir, 'fetch', '--quiet', 'origin',
			'+refs/heads/*:refs/remotes/origin/*']
		vpm_log(@FILE_LINE, @FN, 'cmd: ${fetch_args}')
		fetch_res := os.exec(fetch_args)
		if fetch_res.exit_code != 0 {
			return error('failed to fetch `${rev}` from the origin of `${fmt_mod_path(dir)}`: ${fetch_res.output.trim_space()}')
		}
	}
	args := ['git', '-C', dir, 'checkout', '--quiet', rev]
	vpm_log(@FILE_LINE, @FN, 'cmd: ${args}')
	res := os.exec_opt(args) or {
		return error('failed to checkout `${rev}` in `${fmt_mod_path(dir)}`: ${err.msg()}')
	}
	if res.exit_code != 0 {
		return error('failed to checkout `${rev}` in `${fmt_mod_path(dir)}`: ${res.output.trim_space()}')
	}
	update_git_submodules(dir)!
}

// update_git_submodules moves the submodules of the git checkout in `dir` to the
// commits its HEAD records. A plain `git checkout` leaves them where the
// previous HEAD had them, which shows up as local changes that later installs
// refuse to overwrite.
fn update_git_submodules(dir string) ! {
	args := ['git', '-C', dir, 'submodule', 'update', '--init', '--recursive']
	vpm_log(@FILE_LINE, @FN, 'cmd: ${args}')
	res := os.exec(args)
	if res.exit_code != 0 {
		return error('failed to update the submodules of `${fmt_mod_path(dir)}`: ${res.output.trim_space()}')
	}
}

// git_has_commit reports whether the git checkout in `dir` holds the commit `rev`.
fn git_has_commit(dir string, rev string) bool {
	res := os.exec(['git', '-C', dir, 'cat-file', '-e', '${rev}^{commit}'])
	return res.exit_code == 0
}

// checkout_origin_url returns the source url recorded in the VCS metadata of
// the checkout in `dir`, or '' when it cannot be determined.
fn checkout_origin_url(dir string) string {
	existing_vcs := vcs_used_in_dir(dir) or { return '' }
	match existing_vcs {
		.git {
			res := os.exec_opt(['git', '-C', dir, 'remote', 'get-url', 'origin']) or {
				return ''
			}
			return res.output.trim_space()
		}
		.hg {
			res := os.exec_opt(['hg', '-R', dir, 'paths', 'default']) or { return '' }
			return res.output.trim_space()
		}
	}
}

// parse_git_version retrieves only the stable version part of the output of `git version`.
// For example: parse_git_version('git version 2.39.3')! will return just '2.39.3'.
pub fn parse_git_version(version string) !string {
	git_version_start := 'git version '
	// The output from `git version` varies, depending on how git was compiled. Here are some examples:
	// `git version 2.44.0` when compiled from source, or from brew on macos.
	// `git version 2.39.3 (Apple Git-146)` on macos with XCode's cli tools.
	// `git version 2.44.0.windows.1` on windows's Git Bash shell.
	if !version.starts_with(git_version_start) {
		return error('should start with `${git_version_start}`')
	}
	suffixed := version.all_after(git_version_start).all_before(' ').trim_space()
	parts := suffixed.split('.')
	pure_version_parts := parts[0..3]
	spure := pure_version_parts.join('.')
	return spure
}
