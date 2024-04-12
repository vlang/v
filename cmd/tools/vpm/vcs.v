module main

import os
import semver

// Supported version control system commands.
enum VCS {
	git
	hg
}

struct VCSInfo {
	dir  string        @[required]
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
	git_installed_raw_ver := os.execute_opt('git --version')!.output.all_after_last(' ').all_before('.windows').trim_space()
	git_installed_ver := semver.from(git_installed_raw_ver)!
	git_submod_filter_ver := semver.from('2.36.0')!
	mut git_install_cmd := 'clone --depth=1 --recursive --shallow-submodules --filter=blob:none'
	if git_installed_ver >= git_submod_filter_ver {
		git_install_cmd += ' --also-filter-submodules'
	}
	return {
		VCS.git: VCSInfo{
			dir: '.git'
			args: struct {
				install: git_install_cmd
				version: '--single-branch -b'
				update: 'pull --recurse-submodules' // pulling with `--depth=1` leads to conflicts when the upstream has more than 1 new commits.
				path: '-C'
				outdated: ['fetch', 'rev-parse @', 'rev-parse @{u}']
			}
		}
		VCS.hg:  VCSInfo{
			dir: '.hg'
			args: struct {
				install: 'clone'
				version: '--rev'
				update: 'pull --update'
				path: '-R'
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
