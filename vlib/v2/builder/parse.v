// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import v2.ast
import v2.parser

fn should_expand_single_file_input(input string) bool {
	if os.file_name(input).ends_with('_test.v') {
		return true
	}
	module_name := file_module_name(input) or { return false }
	return module_name != 'main'
}

fn file_module_name(path string) ?string {
	content := os.read_file(path) or { return none }
	for raw_line in content.split_into_lines() {
		line := raw_line.trim_space()
		if line == '' || line.starts_with('//') {
			continue
		}
		if line.starts_with('module ') {
			return line['module '.len..].trim_space().all_before(' ')
		}
		break
	}
	return none
}

fn (mut b Builder) parse_files(files []string) []ast.File {
	mut parser_reused := parser.Parser.new(b.pref)
	mut ast_files := []ast.File{}
	skip_builtin := b.pref.skip_builtin
	mut use_core_headers := false
	if !skip_builtin {
		// TODO: restore .vh-based core parsing once the header summaries retain
		// enough const/global initializer detail for downstream codegen.
		// The current summaries lose information needed for builds like
		// `v2 vlib/builtin/string_test.v`, producing invalid C for values such as
		// `os.args` and `time.Duration` constants.
		use_core_headers = false
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
		if should_expand_single_file_input(input) {
			if input !in seen_user_files {
				expanded_user_files << input
				seen_user_files[input] = true
			}
			dir_files := get_v_files_from_dir(os.dir(input), b.pref.user_defines)
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
	// Directory inputs and non-main module files were expanded above. Single-file
	// `main` programs stay isolated, so `v2 hello.v` parses only `hello.v`,
	// while `v2 .` and `v2 vlib/math/math_test.v` still parse their module files.
	parsed_user_files := parser_reused.parse_files(expanded_user_files, mut b.file_set)
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
