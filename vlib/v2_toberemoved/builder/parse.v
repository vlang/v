// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token

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

fn is_module_source_space(ch u8) bool {
	return is_module_line_space(ch) || ch == `\n` || ch == `\r`
}

fn file_module_name(path string) ?string {
	content := os.read_file(path) or { return none }
	mut start := 0
	for start < content.len {
		if is_module_source_space(content[start]) {
			start++
			continue
		}
		if start + 1 < content.len && content[start] == `/` && content[start + 1] == `/` {
			start += 2
			for start < content.len && content[start] != `\n` && content[start] != `\r` {
				start++
			}
			continue
		}
		if start + 1 < content.len && content[start] == `/` && content[start + 1] == `*` {
			start += 2
			for start + 1 < content.len {
				if content[start] == `*` && content[start + 1] == `/` {
					start += 2
					break
				}
				start++
			}
			if start >= content.len {
				break
			}
			continue
		}
		break
	}
	if content.len - start >= 7 && content[start] == `m` && content[start + 1] == `o`
		&& content[start + 2] == `d` && content[start + 3] == `u` && content[start + 4] == `l`
		&& content[start + 5] == `e` && is_module_line_space(content[start + 6]) {
		mut name_start := start + 7
		for name_start < content.len && is_module_line_space(content[name_start]) {
			name_start++
		}
		mut name_end := name_start
		for name_end < content.len && !is_module_source_space(content[name_end]) {
			name_end++
		}
		if name_start < name_end {
			return content[name_start..name_end]
		}
		return none
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
			if file_module_name(file) or { 'main' } != module_name {
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
	match lower_name {
		'macos', 'darwin', 'mac', 'linux', 'windows', 'bsd', 'freebsd', 'openbsd', 'netbsd',
		'dragonfly', 'android', 'termux', 'ios', 'solaris', 'qnx', 'serenity', 'plan9', 'vinix',
		'none' {
			return flag_os_matches(lower_name, target_os)
		}
		'cross' {
			return flag_os_matches(lower_name, target_os) || lower_name in user_defines
		}
		'freestanding', 'bare' {
			return lower_name in user_defines
		}
		else {}
	}

	return lower_name in user_defines
}

fn ast_comptime_cond_matches(cond ast.Expr, user_defines []string, target_os string) bool {
	return ast_comptime_cond_matches_with_explicit(cond, user_defines, user_defines, target_os)
}

fn ast_comptime_cond_matches_with_explicit(cond ast.Expr, user_defines []string, explicit_user_defines []string, target_os string) bool {
	return ast_comptime_cond_matches_with_options(cond, user_defines, explicit_user_defines,
		target_os, true)
}

fn ast_comptime_cond_matches_with_options(cond ast.Expr, user_defines []string, explicit_user_defines []string, target_os string, allow_pkgconfig bool) bool {
	match cond {
		ast.Ident {
			return ast_comptime_flag_matches(cond.name, user_defines, target_os)
		}
		ast.ComptimeExpr {
			return ast_comptime_cond_matches_with_options(cond.expr, user_defines,
				explicit_user_defines, target_os, allow_pkgconfig)
		}
		ast.CallExpr {
			if pkg_name := ast_pkgconfig_call_name(cond) {
				if !allow_pkgconfig {
					return false
				}
				return vpref.comptime_pkgconfig_value(pkg_name)
			}
		}
		ast.CallOrCastExpr {
			if pkg_name := ast_pkgconfig_call_name(cond) {
				if !allow_pkgconfig {
					return false
				}
				return vpref.comptime_pkgconfig_value(pkg_name)
			}
		}
		ast.PrefixExpr {
			if cond.op == .not {
				return !ast_comptime_cond_matches_with_options(cond.expr, user_defines,
					explicit_user_defines, target_os, allow_pkgconfig)
			}
		}
		ast.InfixExpr {
			if cond.op == .and {
				return
					ast_comptime_cond_matches_with_options(cond.lhs, user_defines, explicit_user_defines, target_os, allow_pkgconfig)
					&& ast_comptime_cond_matches_with_options(cond.rhs, user_defines, explicit_user_defines, target_os, allow_pkgconfig)
			}
			if cond.op == .logical_or {
				return
					ast_comptime_cond_matches_with_options(cond.lhs, user_defines, explicit_user_defines, target_os, allow_pkgconfig)
					|| ast_comptime_cond_matches_with_options(cond.rhs, user_defines, explicit_user_defines, target_os, allow_pkgconfig)
			}
		}
		ast.PostfixExpr {
			if cond.op == .question && cond.expr is ast.Ident {
				return vpref.comptime_optional_define_value(cond.expr.name, user_defines,
					explicit_user_defines)
			}
		}
		ast.ParenExpr {
			return ast_comptime_cond_matches_with_options(cond.expr, user_defines,
				explicit_user_defines, target_os, allow_pkgconfig)
		}
		else {}
	}

	return false
}

fn ast_pkgconfig_call_name(expr ast.Expr) ?string {
	match expr {
		ast.CallExpr {
			if expr.lhs is ast.Ident && expr.lhs.name == 'pkgconfig' && expr.args.len == 1 {
				return ast_string_literal_value(expr.args[0])
			}
		}
		ast.CallOrCastExpr {
			if expr.lhs is ast.Ident && expr.lhs.name == 'pkgconfig' {
				return ast_string_literal_value(expr.expr)
			}
		}
		else {}
	}

	return none
}

fn ast_string_literal_value(expr ast.Expr) ?string {
	if expr is ast.StringLiteral {
		return unquote_ast_string_literal_value(expr.value)
	}
	return none
}

fn unquote_ast_string_literal_value(value string) string {
	if value.len >= 2 && ((value[0] == `"` && value[value.len - 1] == `"`)
		|| (value[0] == `'` && value[value.len - 1] == `'`)) {
		return value[1..value.len - 1]
	}
	return value
}

fn collect_active_imports_from_if_expr(node ast.IfExpr, user_defines []string, target_os string, mut imports []ast.ImportStmt) {
	collect_active_imports_from_if_expr_with_explicit(node, user_defines, user_defines, target_os, mut
		imports)
}

fn collect_active_imports_from_if_expr_with_explicit(node ast.IfExpr, user_defines []string, explicit_user_defines []string, target_os string, mut imports []ast.ImportStmt) {
	collect_active_imports_from_if_expr_with_options(node, user_defines, explicit_user_defines,
		target_os, true, mut imports)
}

fn collect_active_imports_from_if_expr_with_options(node ast.IfExpr, user_defines []string, explicit_user_defines []string, target_os string, allow_pkgconfig bool, mut imports []ast.ImportStmt) {
	if ast_comptime_cond_matches_with_options(node.cond, user_defines, explicit_user_defines,
		target_os, allow_pkgconfig)
	{
		collect_active_imports_from_stmts_with_options(node.stmts, user_defines,
			explicit_user_defines, target_os, allow_pkgconfig, mut imports)
		return
	}
	match node.else_expr {
		ast.IfExpr {
			if node.else_expr.cond is ast.EmptyExpr {
				collect_active_imports_from_stmts_with_options(node.else_expr.stmts, user_defines,
					explicit_user_defines, target_os, allow_pkgconfig, mut imports)
			} else {
				collect_active_imports_from_if_expr_with_options(node.else_expr, user_defines,
					explicit_user_defines, target_os, allow_pkgconfig, mut imports)
			}
		}
		else {}
	}
}

fn collect_active_imports_from_stmts(stmts []ast.Stmt, user_defines []string, target_os string, mut imports []ast.ImportStmt) {
	collect_active_imports_from_stmts_with_explicit(stmts, user_defines, user_defines, target_os, mut
		imports)
}

fn collect_active_imports_from_stmts_with_explicit(stmts []ast.Stmt, user_defines []string, explicit_user_defines []string, target_os string, mut imports []ast.ImportStmt) {
	collect_active_imports_from_stmts_with_options(stmts, user_defines, explicit_user_defines,
		target_os, true, mut imports)
}

fn collect_active_imports_from_stmts_with_options(stmts []ast.Stmt, user_defines []string, explicit_user_defines []string, target_os string, allow_pkgconfig bool, mut imports []ast.ImportStmt) {
	for stmt in stmts {
		match stmt {
			ast.ImportStmt {
				imports << stmt
			}
			ast.ExprStmt {
				if stmt.expr is ast.ComptimeExpr && stmt.expr.expr is ast.IfExpr {
					collect_active_imports_from_if_expr_with_options(stmt.expr.expr, user_defines,
						explicit_user_defines, target_os, allow_pkgconfig, mut imports)
				}
			}
			else {}
		}
	}
}

fn imports_contain(imports []ast.ImportStmt, name string) bool {
	for imp in imports {
		if imp.name == name {
			return true
		}
	}
	return false
}

fn add_implicit_sync_import_if_needed(mut imports []ast.ImportStmt, current_module string, uses_channel bool) {
	if !uses_channel || current_module == 'sync' || imports_contain(imports, 'sync') {
		return
	}
	imports << ast.ImportStmt{
		name: 'sync'
	}
}

fn field_decl_uses_channel(field ast.FieldDecl) bool {
	return type_expr_uses_channel(field.typ) || expr_type_slots_use_channel(field.value)
}

fn fields_use_channel(fields []ast.FieldDecl) bool {
	for field in fields {
		if field_decl_uses_channel(field) {
			return true
		}
	}
	return false
}

struct ChannelScanOptions {
	user_defines          []string
	explicit_user_defines []string
	target_os             string
	allow_pkgconfig       bool
	filter_comptime       bool
}

fn field_inits_use_channel(fields []ast.FieldInit) bool {
	return field_inits_use_channel_with_options(fields, ChannelScanOptions{
		allow_pkgconfig: true
	})
}

fn field_inits_use_channel_with_options(fields []ast.FieldInit, options ChannelScanOptions) bool {
	for field in fields {
		if expr_type_slots_use_channel_with_options(field.value, options) {
			return true
		}
	}
	return false
}

fn fn_type_uses_channel(typ ast.FnType) bool {
	for gp in typ.generic_params {
		if type_expr_uses_channel(gp) {
			return true
		}
	}
	for param in typ.params {
		if type_expr_uses_channel(param.typ) {
			return true
		}
	}
	return type_expr_uses_channel(typ.return_type)
}

fn stmts_use_channel(stmts []ast.Stmt) bool {
	return stmts_use_channel_with_options(stmts, ChannelScanOptions{
		allow_pkgconfig: true
	})
}

fn stmts_use_channel_with_options(stmts []ast.Stmt, options ChannelScanOptions) bool {
	for stmt in stmts {
		if stmt_uses_channel_with_options(stmt, options) {
			return true
		}
	}
	return false
}

fn stmt_uses_channel(stmt ast.Stmt) bool {
	return stmt_uses_channel_with_options(stmt, ChannelScanOptions{
		allow_pkgconfig: true
	})
}

fn fn_decl_body_is_active_for_channel_scan(decl ast.FnDecl, options ChannelScanOptions) bool {
	for attr in decl.attributes {
		if attr.comptime_cond is ast.EmptyExpr {
			continue
		}
		if !ast_comptime_cond_matches_with_options(attr.comptime_cond, options.user_defines,
			options.explicit_user_defines, options.target_os, options.allow_pkgconfig) {
			return false
		}
	}
	return true
}

fn stmt_uses_channel_with_options(stmt ast.Stmt, options ChannelScanOptions) bool {
	match stmt {
		ast.AssertStmt {
			return expr_type_slots_use_channel_with_options(stmt.expr, options)
				|| expr_type_slots_use_channel_with_options(stmt.extra, options)
		}
		ast.AssignStmt {
			return exprs_type_slots_use_channel_with_options(stmt.lhs, options)
				|| exprs_type_slots_use_channel_with_options(stmt.rhs, options)
		}
		ast.BlockStmt {
			return stmts_use_channel_with_options(stmt.stmts, options)
		}
		ast.ComptimeStmt {
			return stmt_uses_channel_with_options(stmt.stmt, options)
		}
		ast.ConstDecl {
			return field_inits_use_channel_with_options(stmt.fields, options)
		}
		ast.DeferStmt {
			return stmts_use_channel_with_options(stmt.stmts, options)
		}
		ast.EnumDecl {
			for field in stmt.fields {
				if expr_type_slots_use_channel_with_options(field.value, options) {
					return true
				}
			}
		}
		ast.ExprStmt {
			if options.filter_comptime && stmt.expr is ast.ComptimeExpr
				&& stmt.expr.expr is ast.IfExpr {
				return comptime_if_expr_uses_channel_with_options(stmt.expr.expr, options)
			}
			return expr_type_slots_use_channel_with_options(stmt.expr, options)
		}
		ast.FnDecl {
			signature_uses_channel := type_expr_uses_channel(stmt.receiver.typ)
				|| fn_type_uses_channel(stmt.typ)
			if !fn_decl_body_is_active_for_channel_scan(stmt, options) {
				return signature_uses_channel
			}
			return signature_uses_channel || stmts_use_channel_with_options(stmt.stmts, options)
		}
		ast.ForInStmt {
			return expr_type_slots_use_channel_with_options(stmt.expr, options)
		}
		ast.ForStmt {
			return stmt_uses_channel_with_options(stmt.init, options)
				|| expr_type_slots_use_channel_with_options(stmt.cond, options)
				|| stmt_uses_channel_with_options(stmt.post, options)
				|| stmts_use_channel_with_options(stmt.stmts, options)
		}
		ast.GlobalDecl {
			return fields_use_channel(stmt.fields)
		}
		ast.InterfaceDecl {
			return fields_use_channel(stmt.fields) || type_exprs_use_channel(stmt.embedded)
		}
		ast.LabelStmt {
			return stmt_uses_channel_with_options(stmt.stmt, options)
		}
		ast.ReturnStmt {
			return exprs_type_slots_use_channel_with_options(stmt.exprs, options)
		}
		ast.StructDecl {
			return type_exprs_use_channel(stmt.implements) || type_exprs_use_channel(stmt.embedded)
				|| type_exprs_use_channel(stmt.generic_params) || fields_use_channel(stmt.fields)
		}
		ast.TypeDecl {
			return type_expr_uses_channel(stmt.base_type) || type_exprs_use_channel(stmt.variants)
				|| type_exprs_use_channel(stmt.generic_params)
		}
		else {}
	}

	return false
}

fn type_exprs_use_channel(exprs []ast.Expr) bool {
	for expr in exprs {
		if type_expr_uses_channel(expr) {
			return true
		}
	}
	return false
}

fn exprs_type_slots_use_channel(exprs []ast.Expr) bool {
	return exprs_type_slots_use_channel_with_options(exprs, ChannelScanOptions{
		allow_pkgconfig: true
	})
}

fn exprs_type_slots_use_channel_with_options(exprs []ast.Expr, options ChannelScanOptions) bool {
	for expr in exprs {
		if expr_type_slots_use_channel_with_options(expr, options) {
			return true
		}
	}
	return false
}

fn string_inters_use_channel(inters []ast.StringInter) bool {
	return string_inters_use_channel_with_options(inters, ChannelScanOptions{
		allow_pkgconfig: true
	})
}

fn string_inters_use_channel_with_options(inters []ast.StringInter, options ChannelScanOptions) bool {
	for inter in inters {
		if expr_type_slots_use_channel_with_options(inter.expr, options)
			|| expr_type_slots_use_channel_with_options(inter.format_expr, options) {
			return true
		}
	}
	return false
}

fn comptime_if_expr_uses_channel_with_options(node ast.IfExpr, options ChannelScanOptions) bool {
	if ast_comptime_cond_matches_with_options(node.cond, options.user_defines,
		options.explicit_user_defines, options.target_os, options.allow_pkgconfig)
	{
		return stmts_use_channel_with_options(node.stmts, options)
	}
	match node.else_expr {
		ast.IfExpr {
			if node.else_expr.cond is ast.EmptyExpr {
				return stmts_use_channel_with_options(node.else_expr.stmts, options)
			}
			return comptime_if_expr_uses_channel_with_options(node.else_expr, options)
		}
		else {
			return expr_type_slots_use_channel_with_options(node.else_expr, options)
		}
	}
}

fn expr_type_slots_use_channel(expr ast.Expr) bool {
	return expr_type_slots_use_channel_with_options(expr, ChannelScanOptions{
		allow_pkgconfig: true
	})
}

fn expr_type_slots_use_channel_with_options(expr ast.Expr, options ChannelScanOptions) bool {
	match expr {
		ast.ArrayInitExpr {
			return type_expr_uses_channel(expr.typ)
				|| expr_type_slots_use_channel_with_options(expr.init, options)
				|| exprs_type_slots_use_channel_with_options(expr.exprs, options)
				|| expr_type_slots_use_channel_with_options(expr.cap, options)
				|| expr_type_slots_use_channel_with_options(expr.len, options)
				|| expr_type_slots_use_channel_with_options(expr.update_expr, options)
		}
		ast.AsCastExpr {
			return type_expr_uses_channel(expr.typ)
				|| expr_type_slots_use_channel_with_options(expr.expr, options)
		}
		ast.AssocExpr {
			return type_expr_uses_channel(expr.typ)
				|| expr_type_slots_use_channel_with_options(expr.expr, options)
				|| field_inits_use_channel_with_options(expr.fields, options)
		}
		ast.CallExpr {
			return expr_type_slots_use_channel_with_options(expr.lhs, options)
				|| exprs_type_slots_use_channel_with_options(expr.args, options)
		}
		ast.CallOrCastExpr {
			return type_expr_uses_channel(expr.lhs)
				|| expr_type_slots_use_channel_with_options(expr.expr, options)
		}
		ast.CastExpr {
			return type_expr_uses_channel(expr.typ)
				|| expr_type_slots_use_channel_with_options(expr.expr, options)
		}
		ast.ComptimeExpr {
			if options.filter_comptime && expr.expr is ast.IfExpr {
				return comptime_if_expr_uses_channel_with_options(expr.expr, options)
			}
			return expr_type_slots_use_channel_with_options(expr.expr, options)
		}
		ast.FieldInit {
			return expr_type_slots_use_channel_with_options(expr.value, options)
		}
		ast.FnLiteral {
			return fn_type_uses_channel(expr.typ)
				|| stmts_use_channel_with_options(expr.stmts, options)
		}
		ast.GenericArgOrIndexExpr {
			return type_expr_uses_channel(expr.lhs) || type_expr_uses_channel(expr.expr)
		}
		ast.GenericArgs {
			return type_expr_uses_channel(expr.lhs) || type_exprs_use_channel(expr.args)
		}
		ast.IfExpr {
			return expr_type_slots_use_channel_with_options(expr.cond, options)
				|| stmts_use_channel_with_options(expr.stmts, options)
				|| expr_type_slots_use_channel_with_options(expr.else_expr, options)
		}
		ast.IfGuardExpr {
			return stmt_uses_channel_with_options(ast.Stmt(expr.stmt), options)
		}
		ast.IndexExpr {
			return expr_type_slots_use_channel_with_options(expr.lhs, options)
				|| expr_type_slots_use_channel_with_options(expr.expr, options)
		}
		ast.InfixExpr {
			return expr_type_slots_use_channel_with_options(expr.lhs, options)
				|| expr_type_slots_use_channel_with_options(expr.rhs, options)
		}
		ast.InitExpr {
			return type_expr_uses_channel(expr.typ)
				|| field_inits_use_channel_with_options(expr.fields, options)
		}
		ast.KeywordOperator {
			return exprs_type_slots_use_channel_with_options(expr.exprs, options)
		}
		ast.LambdaExpr {
			return expr_type_slots_use_channel_with_options(expr.expr, options)
		}
		ast.ModifierExpr {
			return expr_type_slots_use_channel_with_options(expr.expr, options)
		}
		ast.LockExpr {
			return exprs_type_slots_use_channel_with_options(expr.lock_exprs, options)
				|| exprs_type_slots_use_channel_with_options(expr.rlock_exprs, options)
				|| stmts_use_channel_with_options(expr.stmts, options)
		}
		ast.MapInitExpr {
			return type_expr_uses_channel(expr.typ)
				|| exprs_type_slots_use_channel_with_options(expr.keys, options)
				|| exprs_type_slots_use_channel_with_options(expr.vals, options)
		}
		ast.MatchExpr {
			if expr_type_slots_use_channel_with_options(expr.expr, options) {
				return true
			}
			for branch in expr.branches {
				if exprs_type_slots_use_channel_with_options(branch.cond, options)
					|| stmts_use_channel_with_options(branch.stmts, options) {
					return true
				}
			}
		}
		ast.OrExpr {
			return expr_type_slots_use_channel_with_options(expr.expr, options)
				|| stmts_use_channel_with_options(expr.stmts, options)
		}
		ast.ParenExpr {
			return expr_type_slots_use_channel_with_options(expr.expr, options)
		}
		ast.PostfixExpr {
			return expr_type_slots_use_channel_with_options(expr.expr, options)
		}
		ast.PrefixExpr {
			return expr_type_slots_use_channel_with_options(expr.expr, options)
		}
		ast.RangeExpr {
			return expr_type_slots_use_channel_with_options(expr.start, options)
				|| expr_type_slots_use_channel_with_options(expr.end, options)
		}
		ast.SelectExpr {
			return stmt_uses_channel_with_options(expr.stmt, options)
				|| stmts_use_channel_with_options(expr.stmts, options)
				|| expr_type_slots_use_channel_with_options(expr.next, options)
		}
		ast.SelectorExpr {
			return expr_type_slots_use_channel_with_options(expr.lhs, options)
		}
		ast.SqlExpr {
			return expr_type_slots_use_channel_with_options(expr.expr, options)
		}
		ast.StringInterLiteral {
			return string_inters_use_channel_with_options(expr.inters, options)
		}
		ast.Tuple {
			return exprs_type_slots_use_channel_with_options(expr.exprs, options)
		}
		ast.UnsafeExpr {
			return stmts_use_channel_with_options(expr.stmts, options)
		}
		ast.Type {
			return type_expr_uses_channel(expr)
		}
		else {}
	}

	return false
}

fn type_expr_uses_channel(expr ast.Expr) bool {
	match expr {
		ast.Type {
			match expr {
				ast.ChannelType {
					return true
				}
				ast.ArrayType {
					return type_expr_uses_channel(expr.elem_type)
				}
				ast.ArrayFixedType {
					return type_expr_uses_channel(expr.elem_type)
				}
				ast.FnType {
					return fn_type_uses_channel(expr)
				}
				ast.GenericType {
					return type_expr_uses_channel(expr.name) || type_exprs_use_channel(expr.params)
				}
				ast.MapType {
					return type_expr_uses_channel(expr.key_type)
						|| type_expr_uses_channel(expr.value_type)
				}
				ast.OptionType {
					return type_expr_uses_channel(expr.base_type)
				}
				ast.PointerType {
					return type_expr_uses_channel(expr.base_type)
				}
				ast.ResultType {
					return type_expr_uses_channel(expr.base_type)
				}
				ast.ThreadType {
					return type_expr_uses_channel(expr.elem_type)
				}
				ast.TupleType {
					return type_exprs_use_channel(expr.types)
				}
				ast.AnonStructType {
					return type_exprs_use_channel(expr.generic_params)
						|| type_exprs_use_channel(expr.embedded) || fields_use_channel(expr.fields)
				}
				else {}
			}
		}
		else {}
	}

	return false
}

fn active_file_imports(file ast.File, user_defines []string, target_os string) []ast.ImportStmt {
	return active_file_imports_with_explicit(file, user_defines, user_defines, target_os)
}

fn active_file_imports_with_explicit(file ast.File, user_defines []string, explicit_user_defines []string, target_os string) []ast.ImportStmt {
	return active_file_imports_with_options(file, user_defines, explicit_user_defines, target_os,
		true)
}

fn active_file_imports_with_options(file ast.File, user_defines []string, explicit_user_defines []string, target_os string, allow_pkgconfig bool) []ast.ImportStmt {
	mut imports := file.imports.clone()
	collect_active_imports_from_stmts_with_options(file.stmts, user_defines, explicit_user_defines,
		target_os, allow_pkgconfig, mut imports)
	add_implicit_sync_import_if_needed(mut imports, file.mod, stmts_use_channel_with_options(file.stmts, ChannelScanOptions{
		user_defines:          user_defines
		explicit_user_defines: explicit_user_defines
		target_os:             target_os
		allow_pkgconfig:       allow_pkgconfig
		filter_comptime:       true
	}))
	return imports
}

// active_file_imports_from_flat mirrors active_file_imports but reads the
// import list and top-level stmts straight from the FlatAst, so import
// discovery can run without rehydrating an ast.File.
fn active_file_imports_from_flat(flat &ast.FlatAst, ff ast.FlatFile, user_defines []string, explicit_user_defines []string, target_os string) []ast.ImportStmt {
	return active_file_imports_from_flat_with_options(flat, ff, user_defines,
		explicit_user_defines, target_os, true)
}

fn active_file_imports_from_flat_with_options(flat &ast.FlatAst, ff ast.FlatFile, user_defines []string, explicit_user_defines []string, target_os string, allow_pkgconfig bool) []ast.ImportStmt {
	// s253: walk the FlatAst via cursors instead of rehydrating the whole file to
	// legacy ast.Stmt with a top-level decode. The decode-then-walk path (a) costs a
	// full legacy-AST materialisation per file just to answer "does this file use a
	// channel / which comptime imports are active?", and (b) crashes on the arm64
	// self-host — the legacy `*_use_channel` walkers pass `[]Expr`/sum types by
	// value through deep recursion over decoded nodes, hitting the arm64
	// chained-access bug. Cursor reads are plain int-array lookups, so the walk is
	// cheap and arm64-safe; only the tiny comptime-condition sub-exprs are decoded
	// (reusing the exact legacy evaluator) so semantics match the AST path.
	mut imports := ast.Cursor{
		flat: unsafe { flat }
		id:   ff.file_id
	}.list_at(1).import_stmts()
	options := ChannelScanOptions{
		user_defines:          user_defines
		explicit_user_defines: explicit_user_defines
		target_os:             target_os
		allow_pkgconfig:       allow_pkgconfig
		filter_comptime:       true
	}
	file_node := ast.Cursor{
		flat: unsafe { flat }
		id:   ff.file_id
	}
	stmts := file_node.list_at(2) // file node edge 2 = top-level statement list
	flat_collect_active_imports(stmts, options, mut imports)
	module_name := if ff.mod_idx >= 0 && ff.mod_idx < flat.strings.len {
		flat.strings[ff.mod_idx]
	} else {
		'main'
	}
	add_implicit_sync_import_if_needed(mut imports, module_name, flat_stmts_use_channel(stmts,
		options))
	return imports
}

// flat_collect_active_imports is the cursor-native mirror of
// `collect_active_imports_from_stmts_with_options`: it appends top-level
// `import` statements and the imports of active comptime `$if` branches.
fn flat_collect_active_imports(stmts ast.CursorList, options ChannelScanOptions, mut imports []ast.ImportStmt) {
	for i in 0 .. stmts.len() {
		flat_collect_active_imports_stmt(stmts.at(i), options, mut imports)
	}
}

fn flat_collect_active_imports_stmt(s ast.Cursor, options ChannelScanOptions, mut imports []ast.ImportStmt) {
	match s.kind() {
		.stmt_import {
			imports << s.import_stmt()
		}
		.stmt_expr {
			inner := s.edge(0)
			if inner.kind() == .expr_comptime {
				cif := inner.edge(0)
				if cif.kind() == .expr_if {
					flat_collect_active_imports_from_if(cif, options, mut imports)
				}
			}
		}
		else {}
	}
}

// flat_collect_active_imports_from_if mirrors
// `collect_active_imports_from_if_expr_with_options`. expr_if layout:
// edge0 = cond, edge1 = else_expr, edge2.. = then-branch statements.
fn flat_collect_active_imports_from_if(if_c ast.Cursor, options ChannelScanOptions, mut imports []ast.ImportStmt) {
	if flat_comptime_cond_matches(if_c.edge(0), options) {
		for i in 2 .. if_c.edge_count() {
			flat_collect_active_imports_stmt(if_c.edge(i), options, mut imports)
		}
		return
	}
	else_c := if_c.edge(1)
	if else_c.kind() == .expr_if {
		if else_c.edge(0).kind() == .expr_empty {
			for i in 2 .. else_c.edge_count() {
				flat_collect_active_imports_stmt(else_c.edge(i), options, mut imports)
			}
		} else {
			flat_collect_active_imports_from_if(else_c, options, mut imports)
		}
	}
}

// flat_stmts_use_channel is the cursor-native mirror of
// `stmts_use_channel_with_options`.
fn flat_stmts_use_channel(stmts ast.CursorList, options ChannelScanOptions) bool {
	for i in 0 .. stmts.len() {
		if flat_node_uses_channel(stmts.at(i), options) {
			return true
		}
	}
	return false
}

// flat_node_uses_channel returns true if the active subtree rooted at `c`
// references a channel type. Channel types are always encoded as `.typ_channel`
// nodes, so a generic structural recursion over every edge finds them without
// per-kind edge knowledge (aux_list nodes are transparent — their edges are the
// list items). Two arms must prune to match the legacy scan's comptime
// filtering: a FnDecl body is scanned only when its comptime attributes keep it
// active, and a comptime `$if` (ComptimeExpr wrapping an IfExpr) scans only the
// branch selected by the condition.
fn flat_node_uses_channel(c ast.Cursor, options ChannelScanOptions) bool {
	if !c.is_valid() {
		return false
	}
	k := c.kind()
	if k == .typ_channel {
		return true
	}
	if k == .stmt_fn_decl {
		// stmt_fn_decl layout: edge0 = receiver, edge1 = fn type, edge2 = attrs,
		// edge3 = body. Signature (receiver + fn type) is always active; the body
		// only when comptime-active. Attributes never hold a channel type (matches
		// the legacy FnDecl arm, which scans only receiver.typ + fn_type + body).
		if flat_node_uses_channel(c.edge(0), options) {
			return true
		}
		if flat_node_uses_channel(c.edge(1), options) {
			return true
		}
		if flat_fn_decl_body_active_for_channel_scan(c, options) {
			return flat_node_uses_channel(c.edge(3), options)
		}
		return false
	}
	if k == .expr_comptime && options.filter_comptime {
		inner := c.edge(0)
		if inner.kind() == .expr_if {
			return flat_comptime_if_uses_channel(inner, options)
		}
		return flat_node_uses_channel(inner, options)
	}
	for i in 0 .. c.edge_count() {
		if flat_node_uses_channel(c.edge(i), options) {
			return true
		}
	}
	return false
}

// flat_comptime_if_uses_channel mirrors
// `comptime_if_expr_uses_channel_with_options`: only the branch selected by the
// comptime condition is scanned for channel usage.
fn flat_comptime_if_uses_channel(if_c ast.Cursor, options ChannelScanOptions) bool {
	if flat_comptime_cond_matches(if_c.edge(0), options) {
		for i in 2 .. if_c.edge_count() {
			if flat_node_uses_channel(if_c.edge(i), options) {
				return true
			}
		}
		return false
	}
	else_c := if_c.edge(1)
	if else_c.kind() == .expr_if {
		if else_c.edge(0).kind() == .expr_empty {
			for i in 2 .. else_c.edge_count() {
				if flat_node_uses_channel(else_c.edge(i), options) {
					return true
				}
			}
			return false
		}
		return flat_comptime_if_uses_channel(else_c, options)
	}
	return flat_node_uses_channel(else_c, options)
}

// flat_fn_decl_body_active_for_channel_scan mirrors
// `fn_decl_body_is_active_for_channel_scan`: a FnDecl whose comptime attributes
// (`@[if ...]`) don't match the current target is inactive, so its body is
// skipped. aux_attribute layout: edge0 = value, edge1 = comptime_cond.
fn flat_fn_decl_body_active_for_channel_scan(fn_c ast.Cursor, options ChannelScanOptions) bool {
	attrs := fn_c.list_at(2)
	for i in 0 .. attrs.len() {
		cond := attrs.at(i).edge(1)
		if !cond.is_valid() || cond.kind() == .expr_empty {
			continue
		}
		if !flat_comptime_cond_matches(cond, options) {
			return false
		}
	}
	return true
}

// flat_comptime_cond_matches evaluates a comptime condition from FlatAst
// cursors. Call/pkgconfig conditions still fall back to the legacy expression
// evaluator until call argument cursors are ported.
fn flat_comptime_cond_matches(cond ast.Cursor, options ChannelScanOptions) bool {
	if !cond.is_valid() {
		return false
	}
	match cond.kind() {
		.expr_ident {
			return ast_comptime_flag_matches(cond.name(), options.user_defines, options.target_os)
		}
		.expr_comptime, .expr_paren {
			return flat_comptime_cond_matches(cond.edge(0), options)
		}
		.expr_prefix {
			op := unsafe { token.Token(int(cond.aux())) }
			if op == .not {
				return !flat_comptime_cond_matches(cond.edge(0), options)
			}
		}
		.expr_infix {
			op := unsafe { token.Token(int(cond.aux())) }
			if op == .and {
				return flat_comptime_cond_matches(cond.edge(0), options)
					&& flat_comptime_cond_matches(cond.edge(1), options)
			}
			if op == .logical_or {
				return flat_comptime_cond_matches(cond.edge(0), options)
					|| flat_comptime_cond_matches(cond.edge(1), options)
			}
		}
		.expr_postfix {
			op := unsafe { token.Token(int(cond.aux())) }
			inner := cond.edge(0)
			if op == .question && inner.kind() == .expr_ident {
				return vpref.comptime_optional_define_value(inner.name(), options.user_defines,
					options.explicit_user_defines)
			}
		}
		.expr_call, .expr_call_or_cast {
			if pkg_name := flat_pkgconfig_call_name(cond) {
				if !options.allow_pkgconfig {
					return false
				}
				return vpref.comptime_pkgconfig_value(pkg_name)
			}
		}
		else {}
	}

	return false
}

fn flat_pkgconfig_call_name(expr ast.Cursor) ?string {
	match expr.kind() {
		.expr_call {
			lhs := expr.edge(0)
			if lhs.is_valid() && lhs.kind() == .expr_ident && lhs.name() == 'pkgconfig'
				&& expr.edge_count() == 2 {
				return flat_string_literal_value(expr.edge(1))
			}
		}
		.expr_call_or_cast {
			lhs := expr.edge(0)
			if lhs.is_valid() && lhs.kind() == .expr_ident && lhs.name() == 'pkgconfig' {
				return flat_string_literal_value(expr.edge(1))
			}
		}
		else {}
	}

	return none
}

fn flat_string_literal_value(expr ast.Cursor) ?string {
	if !expr.is_valid() || expr.kind() != .expr_string {
		return none
	}
	return unquote_ast_string_literal_value(expr.name())
}

// parse_batch routes normal parsing through the streaming-into-shared-
// FlatBuilder path.
fn (mut b Builder) parse_batch(mut parser_reused parser.Parser, files []string) {
	b.ensure_flat_builder_inited()
	parser_reused.parse_files_into_flat(files, mut b.file_set, mut b.flat_builder)
}

// ensure_flat_builder_inited lazily seeds b.flat_builder on first use.
// Use init_flat_builder_for_paths upfront when the input set is known to
// avoid arena reallocs; this fallback only kicks in when the streaming
// path is entered without that pre-sizing pass.
fn (mut b Builder) ensure_flat_builder_inited() {
	if b.flat_builder_inited {
		return
	}
	b.flat_builder = ast.new_flat_builder()
	b.flat_builder_inited = true
}

// init_flat_builder_for_paths sizes the persistent flat_builder arenas
// from the total source bytes of the supplied paths. Used by parse_files
// to pre-size before any parse_batch call so the streaming path avoids
// the geometric realloc churn the one-shot flatten_files() side-steps.
// The 2x scale factor accounts for imports, which the caller does not
// yet know — empirically core+user is ~half of the final byte total.
fn (mut b Builder) init_flat_builder_for_paths(paths []string) {
	if b.flat_builder_inited {
		return
	}
	mut total_bytes := i64(0)
	for path in paths {
		if path == '' {
			continue
		}
		total_bytes += os.file_size(path)
	}
	nodes_cap, edges_cap, strings_cap := ast.arena_caps_for_bytes(total_bytes * 2)
	b.flat_builder = ast.new_flat_builder_with_capacity(nodes_cap, edges_cap, strings_cap)
	b.flat_builder_inited = true
}

fn (mut b Builder) parse_files(files []string) []ast.File {
	mut parser_reused := parser.Parser.new(b.pref)
	skip_builtin := b.pref.skip_builtin
	target_os := b.pref.source_filter_target_os()
	allow_pkgconfig_imports := !b.pref.is_cross_target()
	mut use_core_headers := false
	// Resolve core-source paths upfront so the pre-size pass below can
	// stat them alongside user files. We still parse in the same batch
	// shape as before so module-init ordering is unchanged.
	mut core_module_files := [][]string{}
	mut cached_core_files := []string{}
	if !skip_builtin {
		use_core_headers = b.can_use_cached_core_headers_for_parse()
		b.used_vh_for_parse = use_core_headers
		if use_core_headers {
			cached_core_files = b.core_cached_parse_paths()
		} else {
			for module_path in core_cached_module_paths {
				vlib_path := b.pref.get_vlib_module_path(module_path)
				core_module_files << get_v_files_from_dir(vlib_path, b.pref.user_defines, target_os)
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
	mut pre_size_paths := []string{}
	pre_size_paths << cached_core_files
	for module_files in core_module_files {
		pre_size_paths << module_files
	}
	pre_size_paths << expanded_user_files
	b.init_flat_builder_for_paths(pre_size_paths)
	if !skip_builtin {
		if use_core_headers {
			b.parse_batch(mut parser_reused, cached_core_files)
		} else {
			for module_files in core_module_files {
				b.parse_batch(mut parser_reused, module_files)
			}
		}
	}
	b.parse_batch(mut parser_reused, expanded_user_files)
	skip_imports := b.pref.skip_imports
	if skip_imports {
		// Flat path: b.flat_builder is the canonical store; build() derives
		// b.flat from it after parse completes.
		return []ast.File{}
	}
	// parse imports
	use_import_headers := b.can_use_cached_import_headers_for_parse()
	b.used_import_vh_for_parse = use_import_headers
	mut parsed_imports := []string{}
	if !skip_builtin {
		parsed_imports << core_cached_module_paths
	}
	// Walk the flat store directly. parse_batch may append new files to
	// b.flat_builder, so re-read the length each iteration.
	for afi := 0; afi < b.flat_builder.flat.files.len; afi++ {
		ff := b.flat_builder.flat.files[afi]
		ast_file_name := b.flat_builder.flat.file_name(ff)
		for mod in active_file_imports_from_flat_with_options(&b.flat_builder.flat, ff,
			b.pref.user_defines, b.pref.explicit_user_defines, target_os, allow_pkgconfig_imports) {
			if mod.name in parsed_imports {
				continue
			}
			if use_core_headers || use_import_headers {
				if cached_path := b.cached_import_parse_path(mod.name) {
					b.parse_batch(mut parser_reused, [cached_path])
					parsed_imports << mod.name
					continue
				}
			}
			mod_path := b.pref.get_module_path(mod.name, ast_file_name)
			module_files := get_v_files_from_dir(mod_path, b.pref.user_defines, target_os)
			if module_files.len == 0 {
				continue
			}
			b.parse_batch(mut parser_reused, module_files)
			parsed_imports << mod.name
		}
	}
	return []ast.File{}
}
