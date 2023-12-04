module main

import os

struct VCS {
	cmd  string        @[required]
	dir  string        @[required]
	args struct {
		install  string   @[required]
		version  string   @[required] // flag to specify a version, added to install.
		path     string   @[required] // flag to specify a path. E.g., used to explicitly work on a path during multithreaded updating.
		update   string   @[required]
		outdated []string @[required]
	}
}

const supported_vcs = {
	'git': VCS{
		cmd: 'git'
		dir: '.git'
		args: struct {
			install: 'clone --depth=1 --recursive --shallow-submodules'
			version: '--single-branch -b'
			update: 'pull --recurse-submodules' // pulling with `--depth=1` leads to conflicts when the upstream has more than 1 new commits.
			path: '-C'
			outdated: ['fetch', 'rev-parse @', 'rev-parse @{u}']
		}
	}
	'hg':  VCS{
		cmd: 'hg'
		dir: '.hg'
		args: struct {
			install: 'clone'
			version: '' // not supported yet.
			update: 'pull --update'
			path: '-R'
			outdated: ['incoming']
		}
	}
}

fn (vcs &VCS) is_executable() ! {
	os.find_abs_path_of_executable(vcs.cmd) or {
		return error('VPM needs `${vcs.cmd}` to be installed.')
	}
}

fn vcs_used_in_dir(dir string) ?VCS {
	for vcs in supported_vcs.values() {
		if os.is_dir(os.real_path(os.join_path(dir, vcs.dir))) {
			return vcs
		}
	}
	return none
}
