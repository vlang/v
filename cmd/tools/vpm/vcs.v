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
		install  string   @[required]
		version  string   @[required] // flag to specify a version, added to install.
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
	git_installed_raw_ver := parse_git_version(os.execute_opt('git --version')!.output) or { '' }
	git_installed_ver := semver.from(git_installed_raw_ver)!
	git_submod_filter_ver := semver.from('2.36.0')!
	mut git_install_cmd := 'clone --recursive'
	if os.user_os() != 'windows' {
		// The variation of environment factors on windows is too high;
		// the following options are known to work well on != windows,
		// but can sometimes cause failures on windows for yet unknown reasons,
		// see https://discord.com/channels/592103645835821068/665558664949530644/1345422482974310440
		// for more details, about why this is now allowed only on != windows platforms.
		git_install_cmd += ' --filter=blob:none'
		if git_installed_ver >= git_submod_filter_ver {
			git_install_cmd += ' --shallow-submodules'
			git_install_cmd += ' --also-filter-submodules'
		}
	}
	return {
		VCS.git: VCSInfo{
			dir:  '.git'
			args: struct {
				install:  git_install_cmd
				version:  '--single-branch -b'
				update:   'pull --recurse-submodules' // pulling with `--depth=1` leads to conflicts when the upstream has more than 1 new commits.
				path:     '-C'
				outdated: ['fetch', 'rev-parse @', 'rev-parse @{u}']
			}
		}
		VCS.hg:  VCSInfo{
			dir:  '.hg'
			args: struct {
				install:  'clone'
				version:  '--rev'
				update:   'pull --update'
				path:     '-R'
				outdated: ['incoming']
			}
		}
	}
}

fn (vcs VCS) clone(url string, version string, path string) ! {
	args := vcs_info[vcs].args
	version_opt := if version != '' { '${args.version} ${version}' } else { '' }
	cmd := [vcs.str(), args.install, version_opt, url, os.quoted_path(path)].join(' ')
	vpm_log(@FILE_LINE, @FN, 'cmd: ${cmd}')
	res := os.execute_opt(cmd)!
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
		if os.is_dir(os.real_path(os.join_path(dir, info.dir))) {
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
	return version.all_after(git_version_start).all_before(' ').all_before('.windows').trim_space()
}
