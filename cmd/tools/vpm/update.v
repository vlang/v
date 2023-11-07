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

fn update_module(mut pp pool.PoolProcessor, idx int, wid int) &ModUpdateInfo {
	mut result := &ModUpdateInfo{
		name: pp.get_item[string](idx)
	}
	zname := get_name_from_url(result.name) or { result.name }
	result.final_path = valid_final_path_of_existing_module(result.name) or { return result }
	println('Updating module `${zname}` in `${result.final_path}` ...')
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
		vpm_error('failed to update module `${zname}` in `${result.final_path}`.',
			details: err.msg()
		)
		return result
	}
	vpm_log(@FILE_LINE, @FN, 'cmd output: ${res.output.trim_space()}')
	increment_module_download_count(zname) or {
		result.has_err = true
		vpm_error(err.msg(),
			verbose: true
		)
	}
	return result
}

fn vpm_update(m []string) {
	mut module_names := m.clone()
	if settings.is_help {
		help.print_and_exit('update')
	}
	if module_names.len == 0 {
		module_names = get_installed_modules()
	}
	if settings.is_verbose {
		vpm_update_verbose(module_names)
		return
	}
	mut pp := pool.new_pool_processor(callback: update_module)
	pp.work_on_items(module_names)
	for res in pp.get_results[ModUpdateInfo]() {
		if res.has_err {
			exit(1)
		}
		resolve_dependencies(res.name, res.final_path, module_names)
	}
}

fn vpm_update_verbose(module_names []string) {
	mut errors := 0
	for name in module_names {
		zname := get_name_from_url(name) or { name }
		final_module_path := valid_final_path_of_existing_module(name) or { continue }
		println('Updating module `${zname}` in `${final_module_path}` ...')
		vcs := vcs_used_in_dir(final_module_path) or { continue }
		vcs.is_executable() or {
			errors++
			vpm_error(err.msg())
			continue
		}
		cmd := '${vcs.cmd} ${vcs.args.path} "${final_module_path}" ${vcs.args.update}'
		vpm_log(@FILE_LINE, @FN, 'cmd: ${cmd}')
		res := os.execute_opt(cmd) or {
			errors++
			vpm_error('failed to update module `${zname}` in `${final_module_path}`.',
				details: err.msg()
			)
			continue
		}
		vpm_log(@FILE_LINE, @FN, 'cmd: ${res.output.trim_space()}')
		increment_module_download_count(zname) or {
			errors++
			vpm_error(err.msg(),
				verbose: true
			)
		}
		resolve_dependencies(name, final_module_path, module_names)
	}
	if errors > 0 {
		exit(1)
	}
}
