// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import v2.ast
import v2.parser

fn (mut b Builder) parse_files(files []string) []ast.File {
	mut parser_reused := parser.Parser.new(b.pref)
	mut ast_files := []ast.File{}
	skip_builtin := b.pref.skip_builtin
	mut use_core_headers := false
	if !skip_builtin {
		use_core_headers = false
		// SSA/C and native backends need full core module bodies (not .vh summaries),
		// otherwise runtime helpers can be lowered to stubs.
		if b.pref.backend in [.c, .cleanc, .x64, .arm64] {
			use_core_headers = false
		}
		if use_core_headers {
			core_files := b.core_cached_parse_paths()
			parsed_core_files := parser_reused.parse_files(core_files, mut b.file_set)
			ast_files << parsed_core_files
		} else {
			for module_path in core_cached_module_paths {
				module_files := get_v_files_from_dir(b.pref.get_vlib_module_path(module_path),
					b.pref.user_defines)
				parsed_module_files := parser_reused.parse_files(module_files, mut b.file_set)
				ast_files << parsed_module_files
			}
		}
	}
	// For test files, include all non-test sibling module files from the same directory
	mut all_user_files := files.clone()
	for file in files {
		if file.contains('_test.') && file.ends_with('.v') {
			dir := os.dir(file)
			sibling_files := get_v_files_from_dir(dir, b.pref.user_defines)
			for sf in sibling_files {
				if sf !in all_user_files {
					all_user_files << sf
				}
			}
		}
	}
	// parse user files
	parsed_user_files := parser_reused.parse_files(all_user_files, mut b.file_set)
	ast_files << parsed_user_files
	skip_imports := b.pref.skip_imports
	if skip_imports {
		return ast_files
	}
	// parse imports
	mut parsed_imports := []string{}
	if !skip_builtin {
		parsed_imports << core_cached_module_paths
	}
	for afi := 0; afi < ast_files.len; afi++ {
		ast_file := ast_files[afi]
		for mod in ast_file.imports {
			if mod.name in parsed_imports {
				continue
			}
			mod_path := b.pref.get_module_path(mod.name, ast_file.name)
			module_files := get_v_files_from_dir(mod_path, b.pref.user_defines)
			parsed_module_files := parser_reused.parse_files(module_files, mut b.file_set)
			ast_files << parsed_module_files
			parsed_imports << mod.name
		}
	}
	return ast_files
}
