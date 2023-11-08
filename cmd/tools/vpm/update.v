module main

import os
import sync.pool
import v.help
import v.vmod

pub struct ModuleUpdateInfo {
mut:
	name         string
	install_path string
	has_err      bool
}

// TODO: parse query as in install.
fn vpm_update(modules []string) {
	vpm_log(@FILE_LINE, @FN, 'modules: ${modules}')
	if settings.is_help {
		help.print_and_exit('update')
	}
	mods := if modules.len == 0 {
		get_installed_modules()
	} else {
		modules.clone()
	}
	if settings.is_verbose {
		vpm_update_verbose(mods)
		return
	}
	mut pp := pool.new_pool_processor(callback: update_module)
	pp.work_on_items(mods)
	for res in pp.get_results[ModuleUpdateInfo]() {
		if res.has_err {
			exit(1)
		}
		manifest := vmod.from_file(os.join_path(res.install_path, 'v.mod')) or {
			vpm_error(err.msg())
			continue
		}
		resolve_dependencies(manifest.name, manifest.dependencies, modules)
	}
}

fn update_module(mut pp pool.PoolProcessor, idx int, wid int) &ModuleUpdateInfo {
	mut result := &ModuleUpdateInfo{
		name: pp.get_item[string](idx)
	}
	name := get_name_from_url(result.name) or { result.name }
	result.install_path = get_path_of_existing_module(result.name) or { return result }
	println('Updating `${name}` in `${result.install_path}`...')
	vcs := vcs_used_in_dir(result.install_path) or { return result }
	vcs.is_executable() or {
		result.has_err = true
		vpm_error(err.msg())
		return result
	}
	cmd := '${vcs.cmd} ${vcs.args.path} "${result.install_path}" ${vcs.args.update}'
	vpm_log(@FILE_LINE, @FN, 'cmd: ${cmd}')
	res := os.execute_opt(cmd) or {
		result.has_err = true
		vpm_error('failed to update module `${name}` in `${result.install_path}`.',
			details: err.msg()
		)
		return result
	}
	vpm_log(@FILE_LINE, @FN, 'cmd output: ${res.output.trim_space()}')
	increment_module_download_count(name) or {
		result.has_err = true
		vpm_error(err.msg(), verbose: true)
	}
	return result
}

// Updates modules with verbose output - runs single-threaded.
fn vpm_update_verbose(modules []string) {
	mut errors := 0
	for mod in modules {
		name := get_name_from_url(mod) or { mod }
		install_path := get_path_of_existing_module(mod) or { continue }
		println('Updating `${name}` in `${install_path}`...')
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
		manifest := vmod.from_file(os.join_path(install_path, 'v.mod')) or {
			errors++
			vpm_error(err.msg(), verbose: true)
			continue
		}
		resolve_dependencies(manifest.name, manifest.dependencies, modules)
	}
	if errors > 0 {
		exit(1)
	}
}
