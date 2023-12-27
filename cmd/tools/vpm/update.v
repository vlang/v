module main

import os
import sync.pool
import v.help

struct UpdateSession {
	idents []string
}

pub struct UpdateResult {
mut:
	success bool
}

fn vpm_update(query []string) {
	if settings.is_help {
		help.print_and_exit('update')
	}
	idents := if query.len == 0 {
		get_installed_modules()
	} else {
		query.clone()
	}
	mut pp := pool.new_pool_processor(callback: update_module)
	ctx := UpdateSession{idents}
	pp.set_shared_context(ctx)
	pp.work_on_items(idents)
	mut errors := 0
	for res in pp.get_results[UpdateResult]() {
		if !res.success {
			errors++
			continue
		}
	}
	if errors > 0 {
		exit(1)
	}
}

fn update_module(mut pp pool.PoolProcessor, idx int, wid int) &UpdateResult {
	ident := pp.get_item[string](idx)
	// Usually, the module `ident`ifier. `get_name_from_url` is only relevant for `v update <module_url>`.
	name := get_name_from_url(ident) or { ident }
	install_path := get_path_of_existing_module(ident) or {
		vpm_error('failed to find path for `${name}`.', verbose: true)
		return &UpdateResult{}
	}
	vcs := vcs_used_in_dir(install_path) or {
		vpm_error('failed to find version control system for `${name}`.', verbose: true)
		return &UpdateResult{}
	}
	vcs.is_executable() or {
		vpm_error(err.msg())
		return &UpdateResult{}
	}
	args := vcs_info[vcs].args
	cmd := [vcs.str(), args.path, os.quoted_path(install_path), args.update].join(' ')
	vpm_log(@FILE_LINE, @FN, 'cmd: ${cmd}')
	println('Updating module `${name}` in `${fmt_mod_path(install_path)}`...')
	res := os.execute_opt(cmd) or {
		vpm_error('failed to update module `${name}` in `${install_path}`.', details: err.msg())
		return &UpdateResult{}
	}
	vpm_log(@FILE_LINE, @FN, 'cmd output: ${res.output.trim_space()}')
	if res.output.contains('Already up to date.') {
		println('Skipped module `${ident}`. Already up to date.')
	} else {
		println('Updated module `${ident}`.')
	}
	// Don't bail if the download count increment has failed.
	increment_module_download_count(name) or { vpm_error(err.msg(), verbose: true) }
	ctx := unsafe { &UpdateSession(pp.get_shared_context()) }
	vpm_log(@FILE_LINE, @FN, 'ident: ${ident}; ctx: ${ctx}')
	resolve_dependencies(get_manifest(install_path), ctx.idents)
	return &UpdateResult{true}
}
