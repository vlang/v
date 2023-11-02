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
	zname := url_to_module_name(result.name)
	result.final_path = valid_final_path_of_existing_module(result.name) or { return result }
	println('Updating module "${zname}" in "${result.final_path}" ...')
	vcs := vcs_used_in_dir(result.final_path) or { return result }[0]
	ensure_vcs_is_installed(vcs) or {
		result.has_err = true
		eprintln(err)
		return result
	}
	path_flag := if vcs == 'hg' { '-R' } else { '-C' }
	cmd := '${vcs} ${path_flag} "${result.final_path}" ${supported_vcs_update_cmds[vcs]}'
	verbose_println('    command: ${cmd}')
	res := os.execute_opt('${cmd}') or {
		result.has_err = true
		println('Failed updating module "${zname}" in "${result.final_path}".')
		verbose_println('      command output: ${err}')
		return result
	}
	verbose_println('    ${res.output.trim_space()}')
	increment_module_download_count(zname) or {
		result.has_err = true
		eprintln('Errors while incrementing the download count for ${zname}:')
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
		zname := url_to_module_name(name)
		final_module_path := valid_final_path_of_existing_module(name) or { continue }
		println('Updating module "${zname}" in "${final_module_path}" ...')
		vcs := vcs_used_in_dir(final_module_path) or { continue }[0]
		ensure_vcs_is_installed(vcs) or {
			errors++
			eprintln(err)
			continue
		}
		path_flag := if vcs == 'hg' { '-R' } else { '-C' }
		cmd := '${vcs} ${path_flag} "${final_module_path}" ${supported_vcs_update_cmds[vcs]}'
		verbose_println('    command: ${cmd}')
		res := os.execute_opt('${cmd}') or {
			errors++
			println('Failed updating module "${zname}" in "${final_module_path}" .')
			verbose_println('      command output: ${err}')
			continue
		}
		verbose_println('    ${res.output.trim_space()}')
		increment_module_download_count(zname) or {
			errors++
			eprintln('Errors while incrementing the download count for ${zname}:')
		}
		resolve_dependencies(name, final_module_path, module_names)
	}
	if errors > 0 {
		exit(1)
	}
}
