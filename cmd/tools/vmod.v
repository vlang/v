// vmod.v implements `v mod`, the questions a project can ask about the modules
// it depends on. It is a `cmd/tools/` program, and is not the `v.vmod` library
// module in `vlib/v/vmod/` that reads and writes the v.mod file itself.
module main

import os
import flag
import v.parser
import v.pref
import v.util
import v.vmod

// Graph is the import graph between modules, keyed by module name.
struct Graph {
mut:
	// roots maps an import name to its resolved directory, with or without v.mod.
	roots map[string]string
	// edges maps a module name to the modules its sources import, in the order
	// they were first seen and without repeats.
	edges map[string][]string
	// entry_of maps a module name to one of its source files, which is what
	// module resolution is relative to.
	entry_of map[string]string
}

// record notes a module and the file its imports should be resolved against.
fn (mut g Graph) record(name string, root string, entry string) {
	g.roots[name] or { g.roots[name] = root }
	g.entry_of[name] or { g.entry_of[name] = entry }
	g.edges[name] or { g.edges[name] = []string{} }
}

// link adds an edge, once. Duplicate edges would only repeat a module in a chain.
fn (mut g Graph) link(from string, to string) {
	mut known := g.edges[from]
	if to in known {
		return
	}
	known << to
	g.edges[from] = known
}

// vroot is the V source tree the running compiler belongs to. The launcher
// exports VEXE; a cached copy of this tool cannot find it from its own path.
fn vroot() string {
	vexe := os.real_path(os.getenv_opt('VEXE') or { os.executable() })
	return os.dir(vexe)
}

// new_preferences builds the resolver the compiler uses for imports, so that
// `v mod` resolves a module exactly the way a build of the project would. The
// vroot comes from VEXE rather than from this tool's own path, which is a cached
// copy when the launcher runs it.
fn new_preferences(project string) &pref.Preferences {
	mut p := pref.new_preferences()
	p.vroot = vroot()
	p.module_resolution_root = project
	return p
}

// module_name_of is the name a module is known by: the `name` field of its
// v.mod, or the directory path for a module folder that carries none.
fn module_name_of(dir string) string {
	manifest := vmod.from_file(os.join_path_single(dir, 'v.mod')) or {
		return dir.replace('\\', '/').replace('/', '.').trim('.')
	}
	if manifest.name.len > 0 {
		return manifest.name
	}
	return dir.replace('\\', '/').replace('/', '.').trim('.')
}

// is_module_dir reports whether `dir` starts its own module, which is where a
// scan for the enclosing module's files has to stop.
fn is_module_dir(dir string) bool {
	return os.is_file(os.join_path_single(dir, 'v.mod'))
}

// v_files_in collects the V sources of one module. A nested module is a module
// of its own, so the walk does not descend into it: its files belong to its
// edges, not to this module's.
fn v_files_in(dir string) ![]string {
	mut files := []string{}
	mut pending := [dir]
	for pending.len > 0 {
		current := pending[pending.len - 1]
		pending.delete(pending.len - 1)
		for entry in os.ls(current)! {
			joined := os.join_path_single(current, entry)
			if os.is_dir(joined) {
				if joined != dir && is_module_dir(joined) {
					continue
				}
				pending << joined
				continue
			}
			if joined.ends_with('.v') || joined.ends_with('.vsh') {
				files << joined
			}
		}
	}
	files.sort()
	return files
}

// imports_in_file returns declared import paths, including both comptime branches.
fn imports_in_file(path string) ![]string {
	mut prefs := pref.new_preferences()
	// Preserve source declarations without lowering scripts or selecting a target.
	prefs.is_fmt = true
	prefs.preserve_comptime_conditionals = true
	prefs.supports_inline_asm = true
	prefs.enable_globals = true
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(path)
	for diagnostic in p.diagnostics {
		if diagnostic.message.starts_with('error reading source:') {
			return error(diagnostic.message)
		}
	}
	mut result := []string{}
	for node in a.nodes {
		if node.kind == .import_decl {
			result << node.value
		}
	}
	return result
}

// build_graph walks the import graph outward from the project root, following
// each module to the modules it actually imports.
fn build_graph(project string) !&Graph {
	p := new_preferences(project)
	mut g := &Graph{
		roots:    map[string]string{}
		edges:    map[string][]string{}
		entry_of: map[string]string{}
	}
	root_name := module_name_of(project)
	g.record(root_name, project, os.join_path_single(project, 'v.mod'))
	mut queue := [root_name]
	mut seen := map[string]bool{}
	for queue.len > 0 {
		name := queue.pop()
		if name in seen {
			continue
		}
		seen[name] = true
		root := g.roots[name] or { continue }
		if root == '' || !os.is_dir(root) {
			continue
		}
		files := v_files_in(root)!
		if files.len == 0 {
			continue
		}
		g.record(name, root, files[0])
		for file in files {
			for module_path in imports_in_file(file)! {
				target := p.get_module_path(module_path, file)
				if target == '' {
					continue
				}
				target_name := module_path
				g.record(target_name, target, file)
				g.link(name, target_name)
				if target_name !in seen {
					queue << target_name
				}
			}
		}
	}
	return g
}

// chain_to returns the shortest path of module names from the project root down
// to `target`, the same question `go mod why` answers.
fn chain_to(g &Graph, root string, target string) []string {
	if target == root {
		return [root]
	}
	mut queue := [[root]]
	mut seen := map[string]bool{
		root: true
	}
	for queue.len > 0 {
		path := queue.pop()
		last := path[path.len - 1]
		for next in g.edges[last] or {
			[]string{}
		} {
			if next == target {
				mut found := path.clone()
				found << next
				return found
			}
			if next in seen {
				continue
			}
			seen[next] = true
			mut extended := path.clone()
			extended << next
			queue << extended
		}
	}
	return []
}

fn print_help(fp &flag.FlagParser) {
	println(fp.usage())
	println('')
	println('Subcommands:')
	println('  why MODULE   Print the chain of imports that brings MODULE into the build.')
}

// why prints the chain of imports that brings a module into the build, the same
// question `go mod why` answers. A module that is installed but that nothing
// imports is reported differently from one that is not installed at all, because
// those two answers point at different problems.
fn why(module_name string, project string) ! {
	g := build_graph(project)!
	root := module_name_of(project)
	if module_name in g.roots {
		for name in chain_to(g, root, module_name) {
			println(name)
		}
		return
	}
	p := new_preferences(project)
	if p.get_module_path(module_name, os.join_path_single(project, 'v.mod')) == '' {
		eprintln('v mod: no module named `${module_name}` could be found in the module')
		eprintln('search path. `v install ${module_name}` may be needed first.')
		exit(1)
	}
	println('(main module does not need module `${module_name}`)')
}

fn main() {
	args := os.args[1..]
	// `v mod ...` reaches this tool with the `mod` word still in the arguments.
	passed := if args.len > 0 && args[0] == 'mod' { args[1..] } else { args }
	mut fp := flag.new_flag_parser(passed)
	fp.application('v mod')
	fp.version('0.0.1')
	fp.description('Answer questions about the modules a project depends on.')
	fp.arguments_description('SUBCOMMAND [NAME]')
	rest := fp.finalize() or {
		eprintln('v mod: ${err.msg()}')
		print_help(fp)
		exit(1)
	}
	if rest.len == 0 {
		print_help(fp)
		return
	}
	project := util.nearest_vmod_root('.') or {
		eprintln('v mod: no v.mod found in this directory or any parent of it.')
		eprintln('`v mod` needs a project; run it from a project folder.')
		exit(1)
	}
	match rest[0] {
		'why' {
			if rest.len < 2 {
				eprintln('v mod why: expected a module name.')
				print_help(fp)
				exit(1)
			}
			why(rest[1], project)!
		}
		else {
			eprintln('v mod: unknown subcommand `${rest[0]}`.')
			print_help(fp)
			exit(1)
		}
	}
}
