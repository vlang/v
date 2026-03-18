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
		// When a valid header cache exists, use lightweight .vh summaries
		// instead of fully parsing every core module source file.
		use_core_headers = b.can_use_cached_core_headers_for_parse()
		b.used_vh_for_parse = use_core_headers
		if use_core_headers {
			core_files := b.core_cached_parse_paths()
			parsed_core_files := parser_reused.parse_files(core_files, mut b.file_set)
			ast_files << parsed_core_files
		} else {
			for module_path in core_cached_module_paths {
				vlib_path := b.pref.get_vlib_module_path(module_path)
				module_files := get_v_files_from_dir(vlib_path, b.pref.user_defines)
				parsed_module_files := parser_reused.parse_files(module_files, mut b.file_set)
				ast_files << parsed_module_files
			}
		}
	}
	// Expand user input paths: allow compiling module directories (e.g. `v2 .`).
	mut expanded_user_files := []string{}
	mut seen_user_files := map[string]bool{}
	for input in files {
		if input == '' {
			continue
		}
		if os.is_dir(input) {
			dir_files := get_v_files_from_dir(input, b.pref.user_defines)
			for dir_file in dir_files {
				if dir_file != '' && dir_file !in seen_user_files {
					expanded_user_files << dir_file
					seen_user_files[dir_file] = true
				}
			}
			continue
		}
		if input !in seen_user_files {
			expanded_user_files << input
			seen_user_files[input] = true
		}
	}
	// For test files, include all non-test sibling module files from the same directory
	mut all_user_files := expanded_user_files.clone()
	for file in expanded_user_files {
		if file.contains('_test.') && file.ends_with('.v') {
			dir := os.dir(file)
			sibling_files := get_v_files_from_dir(dir, b.pref.user_defines)
			for sf in sibling_files {
				if sf != '' && sf !in all_user_files {
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
			if module_files.len == 0 {
				continue
			}
			parsed_module_files := parser_reused.parse_files(module_files, mut b.file_set)
			ast_files << parsed_module_files
			parsed_imports << mod.name
		}
	}
	return ast_files
}
