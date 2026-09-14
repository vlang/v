module util

import os
import v.pref

// external_module_dependencies_for_tool lists the modules from outside vlib that a
// bundled tool needs before it can be compiled. `v build-tools` installs these up
// front, so that building the tools does not fail on a fresh checkout.
pub const external_module_dependencies_for_tool = {
	'vdoc': ['markdown']
}

// check_module_is_installed makes sure that `modulename` is present in ~/.vmodules,
// cloning it from https://github.com/vlang/<modulename> when it is not. With
// `need_update` set, an already installed module is refreshed with `v update`.
// A failed update is only a warning: updates fail on transient network problems,
// and the copy that is already there usually still works.
pub fn check_module_is_installed(modulename string, is_verbose bool, need_update bool) !bool {
	mpath := os.join_path_single(os.vmodules_dir(), modulename)
	mod_v_file := os.join_path_single(mpath, 'v.mod')
	murl := 'https://github.com/vlang/${modulename}'
	if is_verbose {
		eprintln('check_module_is_installed: mpath: ${mpath}')
		eprintln('check_module_is_installed: mod_v_file: ${mod_v_file}')
		eprintln('check_module_is_installed: murl: ${murl}')
	}
	vexe := pref.vexe_path()
	if os.exists(mod_v_file) {
		if need_update {
			update_cmd := "${os.quoted_path(vexe)} update '${modulename}'"
			if is_verbose {
				eprintln('check_module_is_installed: updating with ${update_cmd} ...')
			}
			update_res := os.execute(update_cmd)
			if update_res.exit_code < 0 {
				return error('can not start ${update_cmd}, error: ${update_res.output}')
			}
			if update_res.exit_code != 0 {
				eprintln('Warning: `${modulename}` exists, but is not updated.
V will continue, since updates can fail due to temporary network problems,
and the existing module `${modulename}` may still work.')
				if is_verbose {
					eprintln('Details:')
					eprintln(update_res.output)
				}
				eprintln('-'.repeat(50))
			}
		}
		return true
	}
	if is_verbose {
		eprintln('check_module_is_installed: cloning from ${murl} ...')
	}
	cloning_res := os.execute('${os.quoted_path(vexe)} retry -- git clone ${os.quoted_path(murl)} ${os.quoted_path(mpath)}')
	if cloning_res.exit_code != 0 {
		return error_with_code('cloning failed, details: ${cloning_res.output}', cloning_res.exit_code)
	}
	if !os.exists(mod_v_file) {
		return error('even after cloning, ${mod_v_file} is still missing')
	}
	if is_verbose {
		eprintln('check_module_is_installed: done')
	}
	return true
}

// ensure_modules_for_all_tools_are_installed installs every module named in
// external_module_dependencies_for_tool. It is called by `v build-tools` before it
// starts compiling, so a missing dependency is reported once, up front, instead of
// as a confusing "unknown module" error from the middle of a tool's build.
pub fn ensure_modules_for_all_tools_are_installed(is_verbose bool) {
	for tool_name, tool_modules in external_module_dependencies_for_tool {
		if is_verbose {
			eprintln('Installing modules for tool: ${tool_name} ...')
		}
		for emodule in tool_modules {
			check_module_is_installed(emodule, is_verbose, false) or { panic(err) }
		}
	}
}
