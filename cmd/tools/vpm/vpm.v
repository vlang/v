// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import os.cmdline
import rand
import v.help
import v.vmod

struct VCS {
	dir  string
	cmd  string
	args struct {
		install  string
		path     string // the flag used to specify a path. E.g., used to explicitly work on a path during multithreaded updating.
		update   string
		outdated []string
	}
}

const (
	settings                = init_settings()
	default_vpm_server_urls = ['https://vpm.vlang.io', 'https://vpm.url4e.com']
	vpm_server_urls         = rand.shuffle_clone(default_vpm_server_urls) or { [] } // ensure that all queries are distributed fairly
	valid_vpm_commands      = ['help', 'search', 'install', 'update', 'upgrade', 'outdated', 'list',
		'remove', 'show']
	excluded_dirs           = ['cache', 'vlib']
	supported_vcs           = {
		'git': VCS{
			dir: '.git'
			cmd: 'git'
			args: struct {
				install: 'clone --depth=1 --recursive --shallow-submodules'
				update: 'pull --recurse-submodules' // pulling with `--depth=1` leads to conflicts, when the upstream is more than 1 commit newer
				path: '-C'
				outdated: ['fetch', 'rev-parse @', 'rev-parse @{u}']
			}
		}
		'hg':  VCS{
			dir: '.hg'
			cmd: 'hg'
			args: struct {
				install: 'clone'
				update: 'pull --update'
				path: '-R'
				outdated: ['incoming']
			}
		}
	}
)

fn main() {
	// This tool is intended to be launched by the v frontend,
	// which provides the path to V inside os.getenv('VEXE')
	// args are: vpm [options] SUBCOMMAND module names
	params := cmdline.only_non_options(os.args[1..])
	vpm_log(@FILE_LINE, @FN, 'params: ${params}')
	if params.len < 1 {
		help.print_and_exit('vpm', exit_code: 5)
	}
	vpm_command := params[0]
	mut requested_modules := params[1..].clone()
	ensure_vmodules_dir_exist()
	match vpm_command {
		'help' {
			help.print_and_exit('vpm')
		}
		'search' {
			vpm_search(requested_modules)
		}
		'install' {
			vpm_install(requested_modules)
		}
		'update' {
			vpm_update(requested_modules)
		}
		'upgrade' {
			vpm_upgrade()
		}
		'outdated' {
			vpm_outdated()
		}
		'list' {
			vpm_list()
		}
		'remove' {
			vpm_remove(requested_modules)
		}
		'show' {
			vpm_show(requested_modules)
		}
		else {
			// Unreachable in regular usage. V will catch unknown commands beforehand.
			vpm_error('unknown command "${vpm_command}"')
			help.print_and_exit('vpm', exit_code: 3)
		}
	}
	println('Completed.')
}

fn vpm_upgrade() {
	outdated := get_outdated() or { exit(1) }
	if outdated.len > 0 {
		vpm_update(outdated)
	} else {
		println('Modules are up to date.')
	}
}

fn vpm_outdated() {
	outdated := get_outdated() or { exit(1) }
	if outdated.len > 0 {
		println('Outdated modules:')
		for m in outdated {
			println('  ${m}')
		}
	} else {
		println('Modules are up to date.')
	}
}

fn vpm_list() {
	module_names := get_installed_modules()
	if module_names.len == 0 {
		println('You have no modules installed.')
		exit(0)
	}
	for mod in module_names {
		println(mod)
	}
}

fn vpm_remove(module_names []string) {
	if settings.is_help {
		help.print_and_exit('remove')
	}
	if module_names.len == 0 {
		vpm_error('specify at least one module name for removal.')
		exit(2)
	}
	for name in module_names {
		final_module_path := get_path_of_existing_module(name) or { continue }
		println('Removing module "${name}" ...')
		vpm_log(@FILE_LINE, @FN, 'removing: ${final_module_path}')
		os.rmdir_all(final_module_path) or { vpm_error(err.msg(), verbose: true) }
		// Delete author directory if it is empty.
		author := name.split('.')[0]
		author_dir := os.real_path(os.join_path(settings.vmodules_path, author))
		if !os.exists(author_dir) {
			continue
		}
		if os.is_dir_empty(author_dir) {
			verbose_println('Removing author folder ${author_dir}')
			os.rmdir(author_dir) or { vpm_error(err.msg(), verbose: true) }
		}
	}
}

fn vpm_show(modules []string) {
	installed_modules := get_installed_modules()
	for m in modules {
		if m !in installed_modules {
			info := get_mod_vpm_info(m) or { continue }
			print('Name: ${info.name}
Homepage: ${info.url}
Downloads: ${info.nr_downloads}
Installed: False
--------
')
			continue
		}
		path := os.join_path(os.vmodules_dir(), m.replace('.', os.path_separator))
		mod := vmod.from_file(os.join_path(path, 'v.mod')) or { continue }
		print('Name: ${mod.name}
Version: ${mod.version}
Description: ${mod.description}
Homepage: ${mod.repo_url}
Author: ${mod.author}
License: ${mod.license}
Location: ${path}
Requires: ${mod.dependencies.join(', ')}
--------
')
	}
}
