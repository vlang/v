// vtool.v runs a project tool by its module name.
//
// VPM installs modules, not binaries, so a CLI tool written in V has to be run
// by path today. `v tool NAME` resolves NAME the way an import would and runs
// the module, so the module search path is enough to say which tools a machine
// has.
module main

import os
import flag
import v.pref
import v.util

// vroot is the V source tree the running compiler belongs to. The launcher
// exports VEXE; a cached copy of this tool cannot find it from its own path.
fn vroot() string {
	return os.dir(os.real_path(os.getenv_opt('VEXE') or { os.executable() }))
}

// new_preferences builds the resolver the compiler uses for imports, so that
// `v tool` finds a module exactly the way a build would.
fn new_preferences() &pref.Preferences {
	mut p := pref.new_preferences()
	p.vroot = vroot()
	return p
}

// is_tool_module reports whether a module root holds a program to run. `main.v`
// at the root is the rule, rather than looking for a `main` function in the
// module's files: it is one stat instead of reading the module, and it is what
// a V project actually does.
fn is_tool_module(root string) bool {
	return os.is_file(os.join_path_single(root, 'main.v'))
}

// tool_modules returns the modules directly under `search_root` that hold a
// program, named the way an import would name them.
fn tool_modules(search_root string) ![]string {
	mut names := []string{}
	if !os.is_dir(search_root) {
		return names
	}
	for entry in os.ls(search_root)! {
		joined := os.join_path_single(search_root, entry)
		if os.is_dir(joined) && is_tool_module(joined) {
			names << entry
		}
	}
	names.sort()
	return names
}

// resolves_to_tool checks that a listed name resolves to the intended tool root.
fn resolves_to_tool(p &pref.Preferences, name string, root string) bool {
	resolved := p.get_module_path(name, os.getwd() + os.path_separator + '.')
	return resolved != '' && os.real_path(resolved) == os.real_path(root)
}

// list_tools prints the tool modules that can be named from here: the project
// itself, and the ones in the global module folders. It deliberately does not
// walk up from the project the way module resolution does. Resolution is
// looking for one named module, where a sibling checkout has to be found; a
// listing that walked up would report every module on the way to the filesystem
// root. Nested module folders are left out for the opposite reason: a module in
// `cmd/mytool` is not resolvable by name, so listing it would promise something
// `v tool NAME` cannot deliver.
fn list_tools(p &pref.Preferences) ! {
	mut names := []string{}
	if project := util.nearest_vmod_root('.') {
		name := os.file_name(os.real_path(project))
		if is_tool_module(project) && resolves_to_tool(p, name, project) {
			names << name
		}
	}
	for search_root in p.installed_module_roots() {
		for name in tool_modules(search_root)! {
			if name !in names && resolves_to_tool(p, name, os.join_path_single(search_root, name)) {
				names << name
			}
		}
	}
	if names.len == 0 {
		println('No tool modules found.')
		println('A module is a tool when its root holds a `main.v`.')
		return
	}
	for name in names {
		println(name)
	}
}

// run_tool resolves `name` and hands it to `v run`, so that a tool is built and
// run exactly the way any other V program is.
fn run_tool(p &pref.Preferences, name string) ! {
	root := p.get_module_path(name, os.getwd() + os.path_separator + '.')
	if root == '' || !os.is_dir(root) {
		eprintln('v tool: no module named `${name}` in the module search path.')
		eprintln('`v tool` with no arguments lists the tool modules it can see.')
		exit(1)
	}
	if !is_tool_module(root) {
		eprintln('v tool: module `${name}` holds no program to run.')
		eprintln('A tool module has a `main.v` at its root.')
		exit(1)
	}
	vexe := os.real_path(os.getenv_opt('VEXE') or { os.executable() })
	exit(os.system_args([vexe, 'run', root]))
}

fn print_help(fp &flag.FlagParser) {
	println(fp.usage())
	println('')
	println('Usage:')
	println('  v tool                List the tool modules that can be seen.')
	println('  v tool NAME           Run the tool module called NAME.')
	println('')
	println('A module is a tool when its root holds a `main.v`.')
	println('')
	println('Modules are searched for in the project first, then in vlib and in')
	println('the global module folders. Running one by name uses the same lookup as')
	println('an import, so a module checked out beside the project works too, even')
	println('though the listing above does not walk up to find it.')
}

fn main() {
	args := os.args[1..]
	// `v tool ...` reaches this tool with the `tool` word still in the arguments.
	passed := if args.len > 0 && args[0] == 'tool' { args[1..] } else { args }
	mut fp := flag.new_flag_parser(passed)
	fp.application('v tool')
	fp.version('0.0.1')
	fp.description('Run a tool module by name.')
	fp.arguments_description('[NAME]')
	show_help := fp.bool('help', `h`, false, 'Show this help.')
	rest := fp.finalize() or {
		eprintln('v tool: ${err.msg()}')
		print_help(fp)
		exit(1)
	}
	if show_help {
		print_help(fp)
		return
	}
	p := new_preferences()
	if rest.len == 0 {
		list_tools(p)!
		return
	}
	if rest.len > 1 {
		eprintln('v tool: expected at most one tool name, got ${rest.len}.')
		print_help(fp)
		exit(1)
	}
	run_tool(p, rest[0])!
}
