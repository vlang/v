module main

import os
import sync.pool
import v.help

struct UpdateSession {
	idents []string
}

pub struct UpdateResult {
mut:
	has_err bool
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
	if settings.is_verbose {
		vpm_update_verbose(idents)
		return
	}
	mut pp := pool.new_pool_processor(callback: update_module)
	ctx := UpdateSession{idents}
	pp.set_shared_context(ctx)
	pp.work_on_items(idents)
	mut errors := 0
	for res in pp.get_results[UpdateResult]() {
		if res.has_err {
			errors++
			continue
		}
	}
	if errors > 0 {
		exit(1)
	}
}

fn update_module(mut pp pool.PoolProcessor, idx int, wid int) &UpdateResult {
	mut result := &UpdateResult{}
	ident := pp.get_item[string](idx)
	name := get_name_from_url(ident) or { ident }
	install_path := get_path_of_existing_module(ident) or {
		vpm_error('failed to find path for `${name}`.', verbose: true)
		result.has_err = true
		return result
	}
	println('Updating module `${name}` in `${fmt_mod_path(install_path)}` ...')
	vcs := vcs_used_in_dir(install_path) or {
		vpm_error('failed to find version control system for `${name}`.', verbose: true)
		result.has_err = true
		return result
	}
	vcs.is_executable() or {
		result.has_err = true
		vpm_error(err.msg())
		return result
	}
	cmd := '${vcs.cmd} ${vcs.args.path} "${install_path}" ${vcs.args.update}'
	vpm_log(@FILE_LINE, @FN, 'cmd: ${cmd}')
	res := os.execute_opt(cmd) or {
		result.has_err = true
		vpm_error('failed to update module `${name}` in `${install_path}`.', details: err.msg())
		return result
	}
	vpm_log(@FILE_LINE, @FN, 'cmd output: ${res.output.trim_space()}')
	// Don't bail if the download count increment has failed.
	increment_module_download_count(name) or { vpm_error(err.msg(), verbose: true) }
	ctx := UpdateSession{pp.get_shared_context()}
	resolve_dependencies(get_manifest(install_path), ctx.idents)
	return result
}

fn vpm_update_verbose(idents []string) {
	mut errors := 0
	for mod in idents {
		name := get_name_from_url(mod) or { mod }
		install_path := get_path_of_existing_module(mod) or { continue }
		println('Updating module `${name}` in `${fmt_mod_path(install_path)}` ...')
		vcs := vcs_used_in_dir(install_path) or { continue }
		vcs.is_executable() or {
			errors++
			vpm_error(err.msg())
			continue
		}
		cmd := '${vcs.cmd} ${vcs.args.path} "${install_path}" ${vcs.args.update}'
		vpm_log(@FILE_LINE, @FN, 'cmd: ${cmd}')
		res := os.execute_opt(cmd) or {
			errors++
			vpm_error('failed to update module `${name}` in `${install_path}`.',
				details: err.msg()
			)
			continue
		}
		vpm_log(@FILE_LINE, @FN, 'cmd: ${res.output.trim_space()}')
		increment_module_download_count(name) or {
			errors++
			vpm_error(err.msg(), verbose: true)
		}
		resolve_dependencies(get_manifest(install_path), idents)
	}
	if errors > 0 {
		exit(1)
	}
}
