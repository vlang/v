module main

import os
import sync.pool
import v.help

pub struct UpdateResult {
mut:
	success bool
}

fn vpm_update(query []string) {
	if settings.is_help {
		help.print_and_exit('update')
	}
	mut p := Parser{}
	if query.len == 0 {
		p.parse_outdated()
	} else {
		p.parse_update_query(query)
	}
	// In case dependencies have changed, new modules may need to be installed.
	mut to_update, mut to_install := []Module{}, []Module{}
	for m in p.modules.values() {
		if m.is_installed {
			to_update << m
		} else {
			to_install << m
		}
	}
	if to_update.len == 0 {
		if p.errors > 0 {
			exit(1)
		} else {
			println('All modules are up to date.')
			exit(0)
		}
	}
	vpm_log(@FILE_LINE, @FN, 'Modules to update: ${to_update}')
	vpm_log(@FILE_LINE, @FN, 'Modules to install: ${to_install}')
	mut pp := pool.new_pool_processor(callback: update_module)
	pp.work_on_items(to_update)
	mut errors := 0
	for res in pp.get_results[UpdateResult]() {
		if !res.success {
			errors++
		}
	}
	if to_install.len != 0 {
		install_modules(to_install)
	}
	if p.errors > 0 || errors > 0 {
		exit(1)
	}
}

fn update_module(mut pp pool.PoolProcessor, idx int, wid int) &UpdateResult {
	m := pp.get_item[Module](idx)
	vcs := m.vcs or { settings.vcs }
	args := vcs_info[vcs].args
	cmd := [vcs.str(), args.path, os.quoted_path(m.install_path), args.update].join(' ')
	vpm_log(@FILE_LINE, @FN, '> cmd: ${cmd}')
	println('Updating module `${m.name}` in `${fmt_mod_path(m.install_path)}`...')
	res := os.execute_opt(cmd) or {
		vpm_error('failed to update module `${m.name}` in `${m.install_path}`.', details: err.msg())
		return &UpdateResult{}
	}
	vpm_log(@FILE_LINE, @FN, '>> output: ${res.output.trim_space()}')
	// Don't bail if the download count increment has failed.
	increment_module_download_count(m.name) or { vpm_error(err.msg(), verbose: true) }
	return &UpdateResult{true}
}
