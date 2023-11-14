module main

import os
import sync.pool
import v.help

pub struct ModUpdateInfo {
mut:
	name       string
	final_path string
	has_err    bool
}

fn vpm_update(query []string) {
	if settings.is_help {
		help.print_and_exit('update')
	}
	modules := if query.len == 0 {
		get_installed_modules()
	} else {
		query.clone()
	}
	if settings.is_verbose {
		vpm_update_verbose(modules)
		return
	}
	mut pp := pool.new_pool_processor(callback: update_module)
	pp.work_on_items(modules)
	for res in pp.get_results[ModUpdateInfo]() {
		if res.has_err {
			exit(1)
		}
		resolve_dependencies(get_manifest(res.final_path), modules)
	}
}

fn update_module(mut pp pool.PoolProcessor, idx int, wid int) &ModUpdateInfo {
	mut result := &ModUpdateInfo{
		name: pp.get_item[string](idx)
	}
	name := get_name_from_url(result.name) or { result.name }
	result.final_path = get_path_of_existing_module(result.name) or { return result }
	println('Updating module `${name}` in `${fmt_mod_path(result.final_path)}` ...')
	vcs := vcs_used_in_dir(result.final_path) or { return result }
	vcs.is_executable() or {
		result.has_err = true
		vpm_error(err.msg())
		return result
	}
	cmd := '${vcs.cmd} ${vcs.args.path} "${result.final_path}" ${vcs.args.update}'
	vpm_log(@FILE_LINE, @FN, 'cmd: ${cmd}')
	res := os.execute_opt(cmd) or {
		result.has_err = true
		vpm_error('failed to update module `${name}` in `${result.final_path}`.', details: err.msg())
		return result
	}
	vpm_log(@FILE_LINE, @FN, 'cmd output: ${res.output.trim_space()}')
	increment_module_download_count(name) or {
		result.has_err = true
		vpm_error(err.msg(), verbose: true)
	}
	return result
}

fn vpm_update_verbose(modules []string) {
	mut errors := 0
	for mod in modules {
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
		resolve_dependencies(get_manifest(install_path), modules)
	}
	if errors > 0 {
		exit(1)
	}
}
