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
	if !skip_builtin {
		// Parse builtin
		ast_files << parser_reused.parse_files(get_v_files_from_dir(b.pref.get_vlib_module_path('builtin')), mut
			b.file_set)
		// Parse strconv (used by builtin for string formatting)
		ast_files << parser_reused.parse_files(get_v_files_from_dir(b.pref.get_vlib_module_path('strconv')), mut
			b.file_set)
		// Parse strings (used by builtin for string building)
		ast_files << parser_reused.parse_files(get_v_files_from_dir(b.pref.get_vlib_module_path('strings')), mut
			b.file_set)
		// Parse hash (used by maps for wyhash)
		ast_files << parser_reused.parse_files(get_v_files_from_dir(b.pref.get_vlib_module_path('hash')), mut
			b.file_set)
		// Parse math.bits (used by strconv for bit operations)
		ast_files << parser_reused.parse_files(get_v_files_from_dir(b.pref.get_vlib_module_path('math.bits')), mut
			b.file_set)
	}
	// For internal module tests (_test.v files with a non-main module declaration),
	// expand to include all .v files from the module directory.
	mut expanded_files := expand_test_files(files)
	// parse user files
	ast_files << parser_reused.parse_files(expanded_files, mut b.file_set)
	skip_imports := b.pref.skip_imports
	if skip_imports {
		return ast_files
	}
	// parse imports
	mut parsed_imports := []string{}
	for afi := 0; afi < ast_files.len; afi++ {
		ast_file := ast_files[afi]
		for mod in ast_file.imports {
			if mod.name in parsed_imports {
				continue
			}
			mod_path := b.pref.get_module_path(mod.name, ast_file.name)
			ast_files << parser_reused.parse_files(get_v_files_from_dir(mod_path), mut
				b.file_set)
			parsed_imports << mod.name
		}
	}
	return ast_files
}

// expand_test_files detects internal module tests and expands them to include
// all non-test .v files from the same module directory, matching v1 behavior.
fn expand_test_files(files []string) []string {
	mut expanded := []string{}
	mut added_dirs := []string{}
	for file in files {
		expanded << os.real_path(file)
		if !file.contains('_test.') {
			continue
		}
		// Read the file to check its module declaration
		content := os.read_file(file) or { continue }
		lines := content.split_into_lines()
		for line in lines {
			trimmed := line.trim_space()
			if trimmed.len == 0 || trimmed.starts_with('//') {
				continue
			}
			if trimmed.starts_with('module ') {
				mod_name := trimmed['module '.len..].trim_space()
				if mod_name != 'main' {
					// Internal module test: add all non-test .v files from the module dir
					dir := os.dir(os.real_path(file))
					if dir !in added_dirs {
						added_dirs << dir
						module_files := get_v_files_from_dir(dir)
						for mf in module_files {
							if mf !in expanded {
								expanded << mf
							}
						}
					}
				}
			}
			break
		}
	}
	return expanded
}
