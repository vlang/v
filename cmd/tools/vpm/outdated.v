module main

import os
import sync.pool

pub struct ModuleDateInfo {
	name string
mut:
	outdated bool
	exec_err bool
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

fn get_outdated() ![]string {
	installed := get_installed_modules()
	mut outdated := []string{}
	mut pp := pool.new_pool_processor(callback: get_mod_date_info)
	pp.work_on_items(installed)
	for res in pp.get_results[ModuleDateInfo]() {
		if res.exec_err {
			return error('failed to check the latest commits for `${res.name}`.')
		}
		if res.outdated {
			outdated << res.name
		}
	}
	return outdated
}

fn get_mod_date_info(mut pp pool.PoolProcessor, idx int, wid int) &ModuleDateInfo {
	mut result := &ModuleDateInfo{
		name: pp.get_item[string](idx)
	}
	path := get_path_of_existing_module(result.name) or { return result }
	vcs := vcs_used_in_dir(path) or { return result }
	args := vcs_info[vcs].args
	mut outputs := []string{}
	for step in args.outdated {
		cmd := [vcs.str(), args.path, os.quoted_path(path), step].join(' ')
		res := os.execute(cmd)
		if res.exit_code < 0 {
			verbose_println('Error command: ${cmd}')
			verbose_println('Error details:\n${res.output}')
			result.exec_err = true
			return result
		}
		if vcs == .hg && res.exit_code == 1 {
			result.outdated = true
			return result
		}
		outputs << res.output
	}
	if vcs == .git && outputs[1] != outputs[2] {
		result.outdated = true
	}
	return result
}
