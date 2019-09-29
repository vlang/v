// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import os

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
		if node.name == 'main' {
			continue
		}
		mods << node.name
	}
	return mods
}

// 'strings' => 'VROOT/vlib/strings'
// 'installed_mod' => '~/.vmodules/installed_mod'
// 'local_mod' => '/path/to/current/dir/local_mod'
fn (v &V) find_module_path(mod string) string {
	mod_path := v.module_path(mod)
	// First check for local modules in the same directory
	mut import_path := os.getwd() + '/$mod_path'
	// Now search in vlib/
	if !os.dir_exists(import_path) {
		import_path = '$v.lang_dir/vlib/$mod_path'
	}
	//println('ip=$import_path')
	// Finally try modules installed with vpm (~/.vmodules)
	if !os.dir_exists(import_path) {
		import_path = '$v_modules_path/$mod_path'
		if !os.dir_exists(import_path){
			verror('module "$mod" not found')
		}
	}
	return import_path
}
