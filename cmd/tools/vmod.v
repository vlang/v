// vmod.v implements `v mod`, the questions a project can ask about the modules
// it depends on. It is a `cmd/tools/` program, and is not the `v.vmod` library
// module in `vlib/v/vmod/` that reads and writes the v.mod file itself.
module main

import os
import flag
import v.pref
import v.scanner
import v.token
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
	source := os.read_file(path) or { return error('error reading source: ${err.msg()}') }
	prefs := pref.new_preferences()
	mut s := scanner.new_scanner(prefs, .skip_interpolation)
	s.init(token.File.unindexed(path, source.len), source)
	mut result := []string{}
	scan_import_scope(mut s, mut result)
	return result
}

// scan_import_scope reads declarations without constructing an AST. Full parsing
// reserves multi-megabyte node arrays for large dependency files, which can exhaust
// Boehm's large-allocation retries on macOS when free blocks are blacklisted.
// Ordinary bodies and signatures are opaque; only comptime declaration branches
// introduce further import scopes. The compiler scanner handles comments, strings,
// directives, tabs and keyword module names in the same way as a normal parse.
fn scan_import_scope(mut s scanner.Scanner, mut imports []string) {
	mut tok := s.scan()
	mut comptime_block := false
	mut comptime_else := false
	for tok != .eof {
		if tok !in [.semicolon, .dollar, .key_else] {
			comptime_else = false
		}
		match tok {
			.key_import {
				tok = s.scan()
				if tok != .name && !tok.is_keyword() {
					continue
				}
				mut name := s.lit
				tok = s.scan()
				for tok == .dot {
					tok = s.scan()
					if tok != .name && !tok.is_keyword() {
						break
					}
					name += '.' + s.lit
					tok = s.scan()
				}
				imports << name
				continue
			}
			.dollar {
				tok = s.scan()
				comptime_block = tok == .key_if
				continue
			}
			.key_else {
				comptime_block = comptime_else
			}
			.lcbr {
				if comptime_block {
					scan_import_scope(mut s, mut imports)
				} else {
					skip_import_group(mut s, .lcbr, .rcbr)
				}
				comptime_else = comptime_block
				comptime_block = false
			}
			.lpar {
				skip_import_group(mut s, .lpar, .rpar)
			}
			.lsbr, .attribute {
				skip_import_group(mut s, .lsbr, .rsbr)
			}
			.rcbr {
				return
			}
			else {}
		}
		tok = s.scan()
	}
}

fn skip_import_group(mut s scanner.Scanner, open token.Token, close token.Token) {
	mut depth := 1
	for depth > 0 {
		tok := s.scan()
		if tok == .eof {
			return
		}
		if tok == open || (open == .lsbr && tok == .attribute) {
			depth++
		} else if tok == close {
			depth--
		}
	}
}

// build_graph walks the import graph outward from the project root, following
// each module to the modules it actually imports. A root or direct dependency
// already gives `why` its shortest chain, so direct_target stops that search
// before parsing dependencies. Other queries keep the complete graph traversal.
fn build_graph(project string, direct_target string) !&Graph {
	p := new_preferences(project)
	mut g := &Graph{
		roots:    map[string]string{}
		edges:    map[string][]string{}
		entry_of: map[string]string{}
	}
	root_name := module_name_of(project)
	g.record(root_name, project, os.join_path_single(project, 'v.mod'))
	if direct_target != '' && direct_target == root_name {
		return g
	}
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
				if direct_target != '' && name == root_name && target_name == direct_target {
					return g
				}
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
		path := queue.pop_left()
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
	println('  graph        Print versioned v.mod dependency edges.')
	println('  why MODULE   Print the chain of imports that brings MODULE into the build.')
	println('  graph --imports  Print the tree of modules imported by the project.')
}

// why prints the chain of imports that brings a module into the build, the same
// question `go mod why` answers. A module that is installed but that nothing
// imports is reported differently from one that is not installed at all, because
// those two answers point at different problems.
fn why(module_name string, project string) ! {
	g := build_graph(project, module_name)!
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

// graph prints the dependency graph of the project, one module per line with the
// modules it imports indented beneath it. It is the picture `v mod why` answers one
// question about.
fn graph(project string) ! {
	g := build_graph(project, '')!
	root := module_name_of(project)
	println(root)
	mut seen := map[string]bool{
		root: true
	}
	print_graph(g, root, '', mut seen)
}

// print_graph prints a module and its dependencies, indented by `prefix`. Modules are
// printed once, at their first occurrence, so a diamond does not repeat a subtree.
fn print_graph(g &Graph, name string, prefix string, mut seen map[string]bool) {
	for dep in g.edges[name] or { []string{} } {
		if dep in seen {
			continue
		}
		seen[dep] = true
		println('${prefix}${dep}')
		print_graph(g, dep, '${prefix}  ', mut seen)
	}
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
	is_import_graph := fp.bool('imports', 0, false, 'Print the source import tree for graph.')
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
		'graph' {
			if rest.len != 1 {
				eprintln('v mod graph: expected no module arguments.')
				exit(1)
			}
			if is_import_graph {
				graph(project)!
				return
			}
			vexe := os.getenv_opt('VEXE') or { @VEXE }
			result := os.exec([vexe, 'why', '--graph'])
			print(result.output)
			exit(result.exit_code)
		}
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
