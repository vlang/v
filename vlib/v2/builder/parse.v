// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import v2.ast
import v2.parser

fn (mut b Builder) parse_files(files []string) []ast.File {
	mut parser_reused := parser.Parser.new(b.pref)
	mut ast_files := []ast.File{}
	skip_builtin := b.pref.skip_builtin
	mut use_core_headers := false
	if !skip_builtin {
		use_core_headers = b.can_use_cached_core_headers()
		// SSA/C and native backends need full core module bodies (not .vh summaries),
		// otherwise runtime helpers can be lowered to stubs.
		if b.pref.backend in [.c, .cleanc, .x64, .arm64] {
			use_core_headers = false
		}
		if use_core_headers {
			ast_files << parser_reused.parse_files(b.core_cached_parse_paths(), mut b.file_set)
		} else {
			for module_path in core_cached_module_paths {
				ast_files << parser_reused.parse_files(get_v_files_from_dir(b.pref.get_vlib_module_path(module_path)), mut
					b.file_set)
			}
		}
	}
	// parse user files
	ast_files << parser_reused.parse_files(files, mut b.file_set)
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
			ast_files << parser_reused.parse_files(get_v_files_from_dir(mod_path), mut
				b.file_set)
			parsed_imports << mod.name
		}
	}
	return ast_files
}
