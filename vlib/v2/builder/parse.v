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

fn is_module_line_space(ch u8) bool {
	return ch == ` ` || ch == `\t` || ch == `\v` || ch == `\f`
}

fn file_module_name(path string) ?string {
	content := os.read_file(path) or { return none }
	mut line_start := 0
	for line_start <= content.len {
		mut line_end := line_start
		for line_end < content.len && content[line_end] != `\n` && content[line_end] != `\r` {
			line_end++
		}
		mut start := line_start
		for start < line_end && is_module_line_space(content[start]) {
			start++
		}
		mut end := line_end
		for end > start && is_module_line_space(content[end - 1]) {
			end--
		}
		if start == end || (end - start >= 2 && content[start] == `/` && content[start + 1] == `/`) {
			if line_end >= content.len {
				break
			}
			if content[line_end] == `\r` && line_end + 1 < content.len
				&& content[line_end + 1] == `\n` {
				line_end++
			}
			line_start = line_end + 1
			continue
		}
		if end - start >= 7 && content[start] == `m` && content[start + 1] == `o`
			&& content[start + 2] == `d` && content[start + 3] == `u` && content[start + 4] == `l`
			&& content[start + 5] == `e` && is_module_line_space(content[start + 6]) {
			mut name_start := start + 7
			for name_start < end && is_module_line_space(content[name_start]) {
				name_start++
			}
			mut name_end := name_start
			for name_end < end && !is_module_line_space(content[name_end]) {
				name_end++
			}
			if name_start < name_end {
				return content[name_start..name_end]
			}
			return none
		}
		break
	}
	return none
}

fn directory_primary_module(files []string) string {
	for file in files {
		if module_name := file_module_name(file) {
			return module_name
		}
	}
	return 'main'
}

fn collect_same_module_subdir_files(root string, dir string, module_name string, user_defines []string, target_os string, mut files []string, mut seen map[string]bool) {
	for entry in list_dir_entries(dir) {
		if entry == '' || entry.starts_with('.') {
			continue
		}
		path := os.join_path(dir, entry)
		if !os.is_dir(path) {
			continue
		}
		if path != root && os.exists(os.join_path(path, 'v.mod')) {
			continue
		}
		for file in get_v_files_from_dir(path, user_defines, target_os) {
			if file in seen {
				continue
			}
			if file_module_name(file) or { '' } != module_name {
				continue
			}
			files << file
			seen[file] = true
		}
		collect_same_module_subdir_files(root, path, module_name, user_defines, target_os, mut
			files, mut seen)
	}
}

fn get_user_v_files_from_dir(dir string, user_defines []string, target_os string) []string {
	mut files := get_v_files_from_dir(dir, user_defines, target_os)
	module_name := directory_primary_module(files)
	mut seen := map[string]bool{}
	for file in files {
		seen[file] = true
	}
	collect_same_module_subdir_files(dir, dir, module_name, user_defines, target_os, mut files, mut
		seen)
	return files
}

fn ast_comptime_flag_matches(name string, user_defines []string, target_os string) bool {
	lower_name := name.to_lower()
	if lower_name in user_defines {
		return true
	}
	return match lower_name {
		'macos', 'darwin', 'mac', 'linux', 'windows', 'bsd', 'freebsd', 'openbsd', 'netbsd',
		'dragonfly', 'android', 'ios', 'solaris', 'qnx', 'serenity', 'plan9', 'vinix', 'cross' {
			flag_os_matches(lower_name, target_os)
		}
		else {
			false
		}
	}
}

fn ast_comptime_cond_matches(cond ast.Expr, user_defines []string, target_os string) bool {
	match cond {
		ast.Ident {
			return ast_comptime_flag_matches(cond.name, user_defines, target_os)
		}
		ast.PrefixExpr {
			if cond.op == .not {
				return !ast_comptime_cond_matches(cond.expr, user_defines, target_os)
			}
		}
		ast.InfixExpr {
			if cond.op == .and {
				return ast_comptime_cond_matches(cond.lhs, user_defines, target_os)
					&& ast_comptime_cond_matches(cond.rhs, user_defines, target_os)
			}
			if cond.op == .logical_or {
				return ast_comptime_cond_matches(cond.lhs, user_defines, target_os)
					|| ast_comptime_cond_matches(cond.rhs, user_defines, target_os)
			}
		}
		ast.PostfixExpr {
			if cond.op == .question && cond.expr is ast.Ident {
				return cond.expr.name in user_defines
			}
		}
		ast.ParenExpr {
			return ast_comptime_cond_matches(cond.expr, user_defines, target_os)
		}
		else {}
	}

	return false
}

fn collect_active_imports_from_if_expr(node ast.IfExpr, user_defines []string, target_os string, mut imports []ast.ImportStmt) {
	if ast_comptime_cond_matches(node.cond, user_defines, target_os) {
		collect_active_imports_from_stmts(node.stmts, user_defines, target_os, mut imports)
		return
	}
	match node.else_expr {
		ast.IfExpr {
			if node.else_expr.cond is ast.EmptyExpr {
				collect_active_imports_from_stmts(node.else_expr.stmts, user_defines, target_os, mut
					imports)
			} else {
				collect_active_imports_from_if_expr(node.else_expr, user_defines, target_os, mut
					imports)
			}
		}
		else {}
	}
}

fn collect_active_imports_from_stmts(stmts []ast.Stmt, user_defines []string, target_os string, mut imports []ast.ImportStmt) {
	for stmt in stmts {
		match stmt {
			ast.ImportStmt {
				imports << stmt
			}
			ast.ExprStmt {
				if stmt.expr is ast.ComptimeExpr && stmt.expr.expr is ast.IfExpr {
					collect_active_imports_from_if_expr(stmt.expr.expr, user_defines, target_os, mut
						imports)
				}
			}
			else {}
		}
	}
}

fn active_file_imports(file ast.File, user_defines []string, target_os string) []ast.ImportStmt {
	mut imports := file.imports.clone()
	collect_active_imports_from_stmts(file.stmts, user_defines, target_os, mut imports)
	return imports
}

fn (mut b Builder) parse_files(files []string) []ast.File {
	mut parser_reused := parser.Parser.new(b.pref)
	mut ast_files := []ast.File{}
	skip_builtin := b.pref.skip_builtin
	target_os := b.pref.source_filter_target_os()
	mut use_core_headers := false
	if !skip_builtin {
		use_core_headers = b.can_use_cached_core_headers_for_parse()
		b.used_vh_for_parse = use_core_headers
		if use_core_headers {
			core_files := b.core_cached_parse_paths()
			parsed_core_files := parser_reused.parse_files(core_files, mut b.file_set)
			ast_files << parsed_core_files
		} else {
			for module_path in core_cached_module_paths {
				vlib_path := b.pref.get_vlib_module_path(module_path)
				module_files := get_v_files_from_dir(vlib_path, b.pref.user_defines, target_os)
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
			dir_files := get_user_v_files_from_dir(input, b.pref.user_defines, target_os)
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
			dir_files := get_v_files_from_dir(os.dir(input), b.pref.user_defines, target_os)
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
	virtual_main_modules := b.collect_virtual_main_modules_from_paths(expanded_user_files)
	if b.can_use_cached_virtual_headers_for_parse(virtual_main_modules) {
		expanded_user_files = b.replace_virtual_sources_with_headers(expanded_user_files,
			virtual_main_modules)
		b.used_virtual_vh_for_parse = true
	}
	parsed_user_files := parser_reused.parse_files(expanded_user_files, mut b.file_set)
	ast_files << parsed_user_files
	skip_imports := b.pref.skip_imports
	if skip_imports {
		return ast_files
	}
	// parse imports
	use_import_headers := b.can_use_cached_import_headers_for_parse()
	b.used_import_vh_for_parse = use_import_headers
	mut parsed_imports := []string{}
	if !skip_builtin {
		parsed_imports << core_cached_module_paths
	}
	for afi := 0; afi < ast_files.len; afi++ {
		ast_file := ast_files[afi]
		for mod in active_file_imports(ast_file, b.pref.user_defines, target_os) {
			if mod.name in parsed_imports {
				continue
			}
			if use_core_headers || use_import_headers {
				if cached_path := b.cached_import_parse_path(mod.name) {
					parsed_module_files := parser_reused.parse_files([cached_path], mut b.file_set)
					ast_files << parsed_module_files
					parsed_imports << mod.name
					continue
				}
			}
			mod_path := b.pref.get_module_path(mod.name, ast_file.name)
			module_files := get_v_files_from_dir(mod_path, b.pref.user_defines, target_os)
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
