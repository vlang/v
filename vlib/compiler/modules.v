// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module compiler

import os

const (
	v_modules_path = os.home_dir() + '.vmodules'
)

// Holds import information scoped to the parsed file
struct ImportTable {
mut:
	imports        map[string]string // alias => module
	used_imports   []string          // alias
	import_tok_idx map[string]int    // module => idx
}

// Once we have a module format we can read from module file instead
// this is not optimal
fn (table &Table) qualify_module(mod string, file_path string) string {
	for m in table.imports {
		if m.contains('.') && m.contains(mod) {
			m_parts := m.split('.')
			m_path := m_parts.join(os.path_separator)
			if mod == m_parts[m_parts.len-1] && file_path.contains(m_path) {
				return m
			}
		}
	}
	return mod
}

fn new_import_table() ImportTable {
	return ImportTable{
		imports:   map[string]string
	}
}

fn (p mut Parser) register_import(mod string, tok_idx int) {
	p.register_import_alias(mod, mod, tok_idx)
}

fn (p mut Parser) register_import_alias(alias string, mod string, tok_idx int) {
	// NOTE: come back here
	// if alias in it.imports && it.imports[alias] == mod {}
	if alias in p.import_table.imports && p.import_table.imports[alias] != mod {
		p.error('cannot import $mod as $alias: import name $alias already in use"')
	}
	if mod.contains('.internal.') {
		mod_parts := mod.split('.')
		mut internal_mod_parts := []string
		for part in mod_parts {
			if part == 'internal' { break }
			internal_mod_parts << part
		}
		internal_parent := internal_mod_parts.join('.')
		if !p.mod.starts_with(internal_parent) {
			p.error('module $mod can only be imported internally by libs')
		}
	}
	p.import_table.imports[alias] = mod
	p.import_table.import_tok_idx[mod] = tok_idx
}

fn (it &ImportTable) get_import_tok_idx(mod string) int {
	return it.import_tok_idx[mod]
}

fn (it &ImportTable) known_import(mod string) bool {
	return mod in it.imports || it.is_aliased(mod)
}

fn (it &ImportTable) known_alias(alias string) bool {
	return alias in it.imports
}

fn (it &ImportTable) is_aliased(mod string) bool {
	for _, val in it.imports {
		if val == mod {
			return true
		}
	}
	return false
}

fn (it &ImportTable) resolve_alias(alias string) string {
	return it.imports[alias]
}

fn (it mut ImportTable) register_used_import(alias string) {
	if !(alias in it.used_imports) {
		it.used_imports << alias
	}
}

fn (it &ImportTable) is_used_import(alias string) bool {
	return alias in it.used_imports
}

// return resolved dep graph (order deps)
pub fn (v &V) resolve_deps() &DepGraph {
	graph := v.import_graph()
	deps_resolved := graph.resolve()
	if !deps_resolved.acyclic {
		verror('import cycle detected between the following modules: \n' + deps_resolved.display_cycles())
	}
	return deps_resolved
}

// graph of all imported modules
pub fn(v &V) import_graph() &DepGraph {
	mut graph := new_dep_graph()
	for p in v.parsers {
		mut deps := []string
		for _, m in p.import_table.imports {
			deps << m
		}
		graph.add(p.mod, deps)
	}
	return graph
}

// get ordered imports (module speficic dag method)
pub fn(graph &DepGraph) imports() []string {
	mut mods := []string
	for node in graph.nodes {
		mods << node.name
	}
	return mods
}

fn (v &V) module_path(mod string) string {
	// submodule support
	if mod.contains('.') {
		return mod.replace('.', os.path_separator)
		// return mod.replace('.', '/')
	}
	return mod
}

// 'strings' => 'VROOT/vlib/strings'
// 'installed_mod' => '~/.vmodules/installed_mod'
// 'local_mod' => '/path/to/current/dir/local_mod'
fn (v &V) find_module_path(mod string) ?string {
	mod_path := v.module_path(mod)
	// First check for local modules in the same directory
	mut import_path := os.getwd() + '${os.path_separator}$mod_path'
	// Now search in vlib/
	if mod == 'compiler' || !os.dir_exists(import_path) {
		import_path = '$v.lang_dir${os.path_separator}vlib${os.path_separator}$mod_path'
	}
	//println('ip=$import_path')
	// Finally try modules installed with vpm (~/.vmodules)
	if !os.dir_exists(import_path) {
		import_path = '$v_modules_path${os.path_separator}$mod_path'
		if !os.dir_exists(import_path){
			return error('module "$mod" not found')
		}
	}
	return import_path
}

[inline] fn mod_gen_name(mod string) string {
	return mod.replace('.', '_dot_')
}

[inline] fn mod_gen_name_rev(mod string) string {
	return mod.replace('_dot_', '.')
}
