module main

import os
import sync.pool

pub struct OutdatedResult {
	name string
mut:
	outdated bool
}

fn vpm_outdated() {
	outdated := get_outdated()
	if outdated.len > 0 {
		println('Outdated modules:')
		for m in outdated {
			println('  ${m}')
		}
	} else {
		println('Modules are up to date.')
	}
}

fn get_outdated() []string {
	installed := get_installed_modules()
	if installed.len == 0 {
		println('No modules installed.')
		exit(0)
	}
	mut pp := pool.new_pool_processor(
		callback: fn (mut pp pool.PoolProcessor, idx int, wid int) &OutdatedResult {
			mut result := &OutdatedResult{
				name: pp.get_item[string](idx)
			}
			path := get_path_of_existing_module(result.name) or { return result }
			result.outdated = is_outdated(path)
			return result
		}
	)
	pp.work_on_items(installed)
	mut outdated := []string{}
	for res in pp.get_results[OutdatedResult]() {
		if res.outdated {
			outdated << res.name
		}
	}
	return outdated
}

fn is_outdated(path string) bool {
	vcs := vcs_used_in_dir(path) or { return false }
	args := vcs_info[vcs].args
	mut outputs := []string{}
	for step in args.outdated {
		cmd := [vcs.str(), args.path, os.quoted_path(path), step].join(' ')
		vpm_log(@FILE_LINE, @FN, 'cmd: ${cmd}')
		res := os.execute(cmd)
		vpm_log(@FILE_LINE, @FN, 'output: ${res.output}')
		if res.exit_code != 0 {
			return false
		}
		if vcs == .hg {
			// HG uses only one outdated step. If it has not failed, the module is outdated.
			return true
		}
		outputs << res.output
	}
	// Compare the current and latest origin commit sha.
	return outputs[1] != outputs[2]
}
