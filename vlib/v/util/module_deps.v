module util

import os
import v.pref

// external_module_dependencies_for_tool lists the modules from outside vlib that a
// bundled tool needs before it can be compiled. `v build-tools` installs these up
// front, and the `v` launcher installs them before it compiles a tool on demand, so
// that building the tools does not fail on a fresh checkout.
pub const external_module_dependencies_for_tool = {
	'vdoc': ['markdown']
}

// external_modules_for_tool returns the modules from outside vlib that the bundled
// tool `tool_name` (for example `vdoc`) needs before it can be compiled.
pub fn external_modules_for_tool(tool_name string) []string {
	return external_module_dependencies_for_tool[tool_name] or { []string{} }
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

// resolvable_module_dir returns the folder that `modulename` can already be imported from,
// searching every `VMODULES` root (not just the first one), and then `extra_search_roots`,
// like the expanded `-path` entries of a build. Like the compiler, it only accepts a folder
// that has `.v` files in it.
fn resolvable_module_dir(modulename string, extra_search_roots []string) ?string {
	mod_path := modulename.replace('.', os.path_separator)
	mut roots := os.vmodules_paths()
	roots << extra_search_roots
	for root in roots {
		if root.trim_space() == '' {
			continue
		}
		mod_dir := os.join_path_single(root, mod_path)
		entries := os.ls(mod_dir) or { continue }
		if entries.any(it.ends_with('.v')) {
			return mod_dir
		}
	}
	return none
}

// ensure_modules_for_tool_are_installed installs the modules from outside vlib that the
// bundled tool `tool_name` needs, before that tool is compiled. A module that the compiler
// can already resolve, from any `VMODULES` root or from `extra_search_roots` (the `-path`
// entries of the build), is left alone, so this does not touch the network in the common
// case, and works offline. The returned error names the module that could not be installed,
// and how to install it manually, instead of leaving the user with a `cannot import module`
// builder error.
pub fn ensure_modules_for_tool_are_installed(tool_name string, extra_search_roots []string, is_verbose bool) ! {
	for emodule in external_modules_for_tool(tool_name) {
		if mod_dir := resolvable_module_dir(emodule, extra_search_roots) {
			if is_verbose {
				eprintln('ensure_modules_for_tool_are_installed: `${emodule}` is available in ${mod_dir}')
			}
			continue
		}
		check_module_is_installed(emodule, is_verbose, false) or {
			return error('cannot install the `${emodule}` module, which the `${tool_name}` tool needs: ${err.msg().trim_space()}\nInstall it with `v install ${emodule}`, then try again.')
		}
	}
}

// ensure_modules_for_all_tools_are_installed installs every module named in
// external_module_dependencies_for_tool. It is called by `v build-tools` before it
// starts compiling, so a missing dependency is reported once, up front, instead of
// as a confusing "unknown module" error from the middle of a tool's build.
pub fn ensure_modules_for_all_tools_are_installed(is_verbose bool) {
	for tool_name, _ in external_module_dependencies_for_tool {
		if is_verbose {
			eprintln('Installing modules for tool: ${tool_name} ...')
		}
		ensure_modules_for_tool_are_installed(tool_name, []string{}, is_verbose) or { panic(err) }
	}
}
