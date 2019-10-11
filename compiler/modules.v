// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import os

const (
	v_modules_path = os.home_dir() + '.vmodules'
)

// add a module and its deps (module speficic dag method)
pub fn(graph mut DepGraph) from_import_tables(import_tables map[string]FileImportTable) {
	for _, fit in import_tables {
		mut deps := []string
		for _, m in fit.imports {
			deps << m
		}
		graph.add(fit.module_name, deps)
	}
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
		return mod.replace('.', os.PathSeparator)
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
	mut import_path := os.getwd() + '${os.PathSeparator}$mod_path'
	// Now search in vlib/
	if !os.dir_exists(import_path) {
		import_path = '$v.lang_dir${os.PathSeparator}vlib${os.PathSeparator}$mod_path'
	}
	//println('ip=$import_path')
	// Finally try modules installed with vpm (~/.vmodules)
	if !os.dir_exists(import_path) {
		import_path = '$v_modules_path${os.PathSeparator}$mod_path'
		if !os.dir_exists(import_path){
			return error('module "$mod" not found')
		}
	}
	return import_path
}

fn mod_gen_name(mod string) string {
	return mod.replace('.', '_dot_')
}

fn mod_gen_name_rev(mod string) string {
	return mod.replace('_dot_', '.')
}