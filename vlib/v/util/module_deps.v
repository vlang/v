module util

import os
import v.pref
import v.vmod

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

// resolvable_module_dir returns the folder that the compiler can already import `modulename`
// from, when it compiles the tool in `tool_source` (a `.v` file or a folder). It looks where the
// compiler does. First in the global roots: `search_paths`, the expanded `-path` roots of the
// build, or every `VMODULES` root (not just the first one) without a `-path`, since `-path`
// replaces the default roots. Then in the folder of `tool_source` and in each folder above it,
// which includes the tool's project root. Only a folder with `.v` files in it counts.
fn resolvable_module_dir(modulename string, tool_source string, search_paths []string) ?string {
	mod_path := modulename.replace('.', os.path_separator)
	roots := if search_paths.len > 0 { search_paths } else { os.vmodules_paths() }
	for root in roots {
		if root.trim_space() == '' {
			continue
		}
		mod_dir := os.join_path_single(root, mod_path)
		if dir_has_v_files(mod_dir) {
			return mod_dir
		}
	}
	if tool_source == '' {
		return none
	}
	source := os.real_path(tool_source)
	mut current := if os.is_dir(source) { source } else { os.dir(source) }
	project_root := nearest_vmod_root(current) or { '' }
	for {
		// The compiler passes by the retired `modules/` namespace of a project.
		if !pref.is_retired_modules_namespace(current, project_root) {
			mod_dir := os.join_path_single(current, mod_path)
			// Without a `-path`, the compiler's last fallback accepts any such folder. With one,
			// only its ancestor lookup is left, which skips folders of other projects.
			if dir_has_v_files(mod_dir) && (search_paths.len == 0
				|| !module_dir_belongs_to_other_project(mod_dir, project_root, modulename)) {
				return mod_dir
			}
		}
		parent := os.dir(current)
		if parent == current {
			break
		}
		current = parent
	}
	return none
}

fn dir_has_v_files(dir string) bool {
	entries := os.ls(dir) or { return false }
	return entries.any(it.ends_with('.v'))
}

// module_dir_belongs_to_other_project reports whether `mod_dir`, a folder named like the module
// `modulename`, is something else, like the compiler's ancestor lookup decides it: it is neither
// part of the importer's project in `project_root`, nor declared as that module by its own `v.mod`.
fn module_dir_belongs_to_other_project(mod_dir string, project_root string, modulename string) bool {
	if project_root == '' {
		return false
	}
	real_dir := os.real_path(mod_dir).replace('\\', '/')
	real_project := os.real_path(project_root).replace('\\', '/')
	if real_dir == real_project || real_dir.starts_with(real_project + '/') {
		return false
	}
	owner_root := nearest_vmod_root(mod_dir) or { return true }
	manifest := vmod.from_file(os.join_path_single(owner_root, 'v.mod')) or { return true }
	return manifest.name != modulename && !modulename.starts_with(manifest.name + '.')
}

// ensure_modules_for_tool_are_installed installs the modules from outside vlib that the
// bundled tool `tool_name`, with its sources in `tool_source`, needs before it is compiled.
// `search_paths` are the expanded `-path` roots of the tool's build, if it has any. A module
// that the compiler can already resolve, from those roots or else from any `VMODULES` root, or
// from the folder of `tool_source` or a folder above it (like the tool's project root), is left
// alone. So this does not touch the network in the common case, and works offline. An empty
// `tool_source` skips the folder lookup. The returned error names the module that could not be
// installed, and how to install it manually, instead of leaving the user with a
// `cannot import module` builder error.
pub fn ensure_modules_for_tool_are_installed(tool_name string, tool_source string, search_paths []string, is_verbose bool) ! {
	for emodule in external_modules_for_tool(tool_name) {
		if mod_dir := resolvable_module_dir(emodule, tool_source, search_paths) {
			if is_verbose {
				eprintln('ensure_modules_for_tool_are_installed: `${emodule}` is available in ${mod_dir}')
			}
			continue
		}
		check_module_is_installed(emodule, is_verbose, false) or {
			// Another `v` process can install the same module at the same time, and then its
			// clone makes this one fail. The module is installed all the same.
			if dir_has_v_files(os.join_path_single(os.vmodules_dir(), emodule)) {
				continue
			}
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
		tool_source := os.join_path(os.dir(pref.vexe_path()), 'cmd', 'tools', tool_name)
		ensure_modules_for_tool_are_installed(tool_name, tool_source, []string{}, is_verbose) or {
			panic(err)
		}
	}
}
