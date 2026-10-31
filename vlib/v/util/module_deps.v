module util

import os
import time
import v.pref
import v.vmod

// external_module_dependencies_for_tool is the legacy dependency snapshot.
// Installation reads each tool's current manifest through dev_dependencies_for_tool.
pub const external_module_dependencies_for_tool = {
	'vdoc': ['markdown']
}

// external_modules_for_tool returns the external dependencies in the tool's manifest.
pub fn external_modules_for_tool(tool_name string) []string {
	return dev_dependencies_for_tool(tool_name)
}

// tool_source_dir returns the folder holding a bundled tool's sources, from the
// name it is invoked under. It is anchored at the compiler's own `cmd/tools`, not
// at the working directory, so it does not move when vpm is run from a project.
fn tool_source_dir(tool_name string) string {
	if tool_name == '' || tool_name in ['.', '..'] || tool_name.contains_any('/\\') {
		return ''
	}
	return os.join_path_single(os.join_path(os.dir(pref.vexe_path()), 'cmd', 'tools'),
		tool_name)
}

// dev_dependencies_for_tool returns the modules from outside vlib that the bundled
// tool `tool_name` needs before it can be compiled, read from the tool's own
// `v.mod`. This replaces a table that the compiler carried, which meant a tool
// needing a module was invisible in the tool's own manifest and could not be
// declared without editing `vlib`.
pub fn dev_dependencies_for_tool(tool_name string) []string {
	source := tool_source_dir(tool_name)
	if source == '' {
		return []string{}
	}
	vmod_path := os.join_path_single(source, 'v.mod')
	if !os.is_file(vmod_path) {
		return []string{}
	}
	manifest := vmod.from_file(vmod_path) or { return []string{} }
	return manifest.unknown['dev_dependencies'] or { []string{} }
}

// tools_with_dev_dependencies lists the bundled tools whose `v.mod` declares a
// `dev_dependencies` entry, which is what `v build-tools` installs up front. It
// reads the folders rather than a table, so a new tool is picked up without a
// change here.
fn tools_with_dev_dependencies() []string {
	mut tools := []string{}
	tools_dir := os.join_path(os.dir(pref.vexe_path()), 'cmd', 'tools')
	for entry in os.ls(tools_dir) or { []string{} } {
		dir := os.join_path_single(tools_dir, entry)
		if !os.is_dir(dir) {
			continue
		}
		if dev_dependencies_for_tool(entry).len > 0 {
			tools << entry
		}
	}
	return tools.sorted()
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
			update_res := os.exec([vexe, 'update', modulename])
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
	cloning_res := os.exec([vexe, 'retry', '--', 'git', 'clone', '${murl}', mpath])
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

// module_dir_is_present reports whether `dir` looks like a module that the compiler can import,
// since it has `.v` files, or a `v.mod`, which can point to sources in other folders.
fn module_dir_is_present(dir string) bool {
	if os.is_file(os.join_path_single(dir, 'v.mod')) {
		return true
	}
	entries := os.ls(dir) or { return false }
	return entries.any(it.ends_with('.v'))
}

// resolvable_module_dir returns a folder that the compiler can likely import `modulename` from,
// when it compiles the tool in `tool_source` (a `.v` file or a folder) without a `-path`. It
// looks in `vlib`, and in every `VMODULES` root (not just the first one), and then, like the
// compiler's project-local and ancestor lookup, in the folder of `tool_source` and in each
// folder above it, which includes the tool's project root.
fn resolvable_module_dir(modulename string, tool_source string) ?string {
	mod_path := modulename.replace('.', os.path_separator)
	mut roots := [os.join_path_single(os.dir(pref.vexe_path()), 'vlib')]
	roots << os.vmodules_paths()
	for root in roots {
		if root.trim_space() == '' {
			continue
		}
		mod_dir := os.join_path_single(root, mod_path)
		if module_dir_is_present(mod_dir) {
			return mod_dir
		}
	}
	if tool_source == '' {
		return none
	}
	source := os.real_path(tool_source)
	mut current := if os.is_dir(source) { source } else { os.dir(source) }
	for {
		mod_dir := os.join_path_single(current, mod_path)
		if module_dir_is_present(mod_dir) {
			return mod_dir
		}
		parent := os.dir(current)
		if parent == current {
			break
		}
		current = parent
	}
	return none
}

// concurrent_install_timeout is how long ensure_modules_for_tool_are_installed waits for another
// `v` process to finish installing a module, when that makes its own install fail.
const concurrent_install_timeout = 120 * time.second

// concurrent_install_stale_time is how long such an install may go without changing anything in
// the module folder, before ensure_modules_for_tool_are_installed takes it as interrupted.
const concurrent_install_stale_time = 15 * time.second

// newest_change_in returns the newest modification time, in Unix seconds, of `dir` and of
// everything in it. A running `git clone` keeps changing some file below `.git`.
fn newest_change_in(dir string) i64 {
	mut newest := os.file_last_mod_unix(dir)
	mut pending := [dir]
	for pending.len > 0 {
		current := pending.pop()
		for entry in os.ls(current) or { []string{} } {
			path := os.join_path_single(current, entry)
			modified := os.file_last_mod_unix(path)
			if modified > newest {
				newest = modified
			}
			if os.is_dir(path) && !os.is_link(path) {
				pending << path
			}
		}
	}
	return newest
}

// wait_for_concurrent_install waits until another process finishes installing a module into
// `mod_dir`, which its `git clone` has started, since `mod_dir` has a `.git` folder. It reports
// whether the module is there, and returns false at once when no install is in progress there.
// It fails when nothing in `mod_dir` changes for `stale_time`, since then the install was
// interrupted, and when the install is not done after `timeout`.
fn wait_for_concurrent_install(mod_dir string, timeout time.Duration, stale_time time.Duration) !bool {
	deadline := time.now().add(timeout)
	mut announced := false
	for {
		if module_dir_is_present(mod_dir) {
			return true
		}
		if !os.is_dir(os.join_path_single(mod_dir, '.git')) {
			return false
		}
		idle_seconds := time.now().unix() - newest_change_in(mod_dir)
		if f64(idle_seconds) >= stale_time.seconds() {
			return error('${mod_dir} has a `.git` folder, but no module in it, and it has not changed for ${idle_seconds}s. It looks like an interrupted install: remove ${mod_dir}, and try again.')
		}
		if time.now() > deadline {
			return error('another process started installing it into ${mod_dir}, but did not finish in ${timeout.seconds():.0f}s')
		}
		if !announced {
			eprintln('Waiting for another process to finish installing the module in ${mod_dir} ...')
			announced = true
		}
		time.sleep(500 * time.millisecond)
	}
	return false
}

// ensure_modules_for_tool_are_installed installs the modules from outside vlib that the
// bundled tool `tool_name`, with its sources in `tool_source`, needs before it is compiled
// without a `-path`. It errs on the side of leaving a module alone: a folder named like the
// module, with `.v` files or a `v.mod` in it, in any `VMODULES` root, or in the folder of
// `tool_source` or a folder above it (like the tool's project root), counts as the module.
// If that folder is not a usable module after all, the compiler reports it, just like without
// this check. So a module is installed only when it is clearly missing, and this does not
// touch the network in the common case, and works offline. An empty `tool_source` skips the
// folder lookup. The returned error names the module that could not be installed, and how to
// install it manually, instead of leaving the user with a `cannot import module` builder error.
pub fn ensure_modules_for_tool_are_installed(tool_name string, tool_source string, is_verbose bool) ! {
	for emodule in dev_dependencies_for_tool(tool_name) {
		if mod_dir := resolvable_module_dir(emodule, tool_source) {
			if is_verbose {
				eprintln('ensure_modules_for_tool_are_installed: `${emodule}` is available in ${mod_dir}')
			}
			continue
		}
		check_module_is_installed(emodule, is_verbose, false) or {
			install_error := err.msg().trim_space()
			// Another `v` process can install the same module at the same time, and then its
			// clone makes this one fail. The module is installed all the same, once it is done.
			installed := wait_for_concurrent_install(os.join_path_single(os.vmodules_dir(),
				emodule), concurrent_install_timeout, concurrent_install_stale_time) or {
				return error('cannot install the `${emodule}` module, which the `${tool_name}` tool needs: ${err.msg()}')
			}
			if installed {
				continue
			}
			return error('cannot install the `${emodule}` module, which the `${tool_name}` tool needs: ${install_error}\nInstall it with `v install ${emodule}`, then try again.')
		}
	}
}

// ensure_modules_for_all_tools_are_installed installs every module named in
// a `dev_dependencies` entry in its own `v.mod`. It is called by `v build-tools` before it
// starts compiling, so a missing dependency is reported once, up front, instead of
// as a confusing "unknown module" error from the middle of a tool's build.
pub fn ensure_modules_for_all_tools_are_installed(is_verbose bool) {
	for tool_name in tools_with_dev_dependencies() {
		if is_verbose {
			eprintln('Installing modules for tool: ${tool_name} ...')
		}
		tool_source := os.join_path(os.dir(pref.vexe_path()), 'cmd', 'tools', tool_name)
		ensure_modules_for_tool_are_installed(tool_name, tool_source, is_verbose) or {
			panic(err)
		}
	}
}
