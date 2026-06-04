// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import v2.ast
import v2.pref

fn (b &Builder) validate_freestanding_cleanc_contract() bool {
	if b.pref == unsafe { nil } || !b.pref.is_freestanding() {
		return true
	}
	mut diagnostics := []string{}
	mut seen := map[string]bool{}
	target_os := b.pref.target_os_or_host()
	if b.pref.prealloc {
		msg := 'error: freestanding target cannot use -prealloc because it requires hosted allocation support'
		diagnostics << msg
		seen[msg] = true
	}
	if b.pref.is_shared_lib {
		msg := 'error: freestanding target cannot use -shared because dynamic library output requires hosted loader support'
		if !seen[msg] {
			diagnostics << msg
			seen[msg] = true
		}
	}
	if b.pref.hot_fn.len > 0 {
		msg := 'error: freestanding target cannot use -hot-fn because live code reload needs hosted OS services'
		if !seen[msg] {
			diagnostics << msg
			seen[msg] = true
		}
	}
	if b.pref.has_freestanding_hooks() && (!b.pref.skip_builtin || !b.pref.skip_type_check) {
		msg := 'error: freestanding target platform hooks currently require --skip-builtin and --skip-type-check until cleanc builtin hook signatures are implemented'
		if !seen[msg] {
			diagnostics << msg
			seen[msg] = true
		}
	}
	scan_ctx := freestanding_scan_context(b.pref, target_os)
	allow_pkgconfig_imports := !b.pref.is_cross_target()
	for file in b.freestanding_diagnostic_files() {
		for imported in active_file_imports_with_options(file, b.pref.user_defines,
			b.pref.explicit_user_defines, target_os, allow_pkgconfig_imports) {
			if msg := freestanding_restricted_import_diagnostic(imported.name) {
				if !seen[msg] {
					diagnostics << msg
					seen[msg] = true
				}
			}
		}
		call_name := freestanding_restricted_call_in_stmts(file.stmts, scan_ctx)
		if call_name != '' {
			msg := freestanding_restricted_call_diagnostic(call_name)
			if !seen[msg] {
				diagnostics << msg
				seen[msg] = true
			}
		}
	}
	for msg in diagnostics {
		eprintln(msg)
	}
	if diagnostics.len > 0 {
		eprintln('hint: provide platform bindings or build without -freestanding')
		return false
	}
	return true
}

fn (b &Builder) freestanding_diagnostic_files() []ast.File {
	if b.flat_check_enabled {
		mut files := []ast.File{}
		for i, ff in b.flat.files {
			if !b.should_scan_freestanding_diagnostic_file(b.flat.file_name(ff)) {
				continue
			}
			files << b.flat.to_files_range(i, i + 1)
		}
		return files
	}
	mut files := []ast.File{}
	for file in b.files {
		if !b.should_scan_freestanding_diagnostic_file(file.name) {
			continue
		}
		files << file
	}
	return files
}

fn (b &Builder) should_scan_freestanding_diagnostic_file(file_name string) bool {
	if file_name == '' || b.pref == unsafe { nil } || b.pref.vroot == '' {
		return true
	}
	path := file_name.replace('\\', '/')
	vroot := b.pref.vroot.replace('\\', '/').trim_right('/')
	return !path.starts_with('${vroot}/vlib/')
}

fn freestanding_restricted_import_diagnostic(import_name string) ?string {
	root := import_name.all_before('.')
	return match root {
		'os' {
			'error: freestanding target cannot use module os without a platform implementation'
		}
		'time' {
			'error: freestanding target cannot use module time without a platform time source'
		}
		'term' {
			'error: freestanding target cannot use module term without terminal support'
		}
		'net' {
			'error: freestanding target cannot use module net without platform networking support'
		}
		'sync' {
			'error: freestanding target cannot use module sync without threading/locking support'
		}
		else {
			none
		}
	}
}

fn freestanding_restricted_call_diagnostic(call_name string) string {
	return match call_name {
		'panic' {
			'error: freestanding target cannot use builtin panic without panic platform hook'
		}
		'spawn' {
			'error: freestanding target cannot use spawn because threading support is not provided'
		}
		'go' {
			'error: freestanding target cannot use go because threading support is not provided'
		}
		'lock' {
			'error: freestanding target cannot use lock/rlock because locking support is not provided'
		}
		'shared' {
			'error: freestanding target cannot use shared data because locking support is not provided'
		}
		'live' {
			'error: freestanding target cannot use @[live] because live reload needs hosted OS services'
		}
		'assert' {
			'error: freestanding target cannot use assert because failed assertions need hosted output/exit support'
		}
		else {
			'error: freestanding target cannot use builtin ${call_name} without output platform hook'
		}
	}
}

struct FreestandingScanContext {
	user_defines       []string
	explicit_defines   []string
	target_os          string
	freestanding_hooks []string
}

fn freestanding_scan_context(p &pref.Preferences, target_os string) FreestandingScanContext {
	return FreestandingScanContext{
		user_defines:       p.user_defines
		explicit_defines:   p.explicit_user_defines
		target_os:          target_os
		freestanding_hooks: p.freestanding_hook_list()
	}
}

fn (ctx FreestandingScanContext) has_hook(hook string) bool {
	return hook in ctx.freestanding_hooks
}

fn freestanding_output_builtin_call_name(name string, ctx FreestandingScanContext) string {
	if name in ['print', 'println', 'eprint', 'eprintln'] && !ctx.has_hook('output') {
		return name
	}
	if name == 'panic' && !ctx.has_hook('panic') {
		return name
	}
	return ''
}

fn freestanding_attributes_are_inactive(attributes []ast.Attribute, ctx FreestandingScanContext) bool {
	for attr in attributes {
		if attr.comptime_cond !is ast.EmptyExpr
			&& !ast_comptime_cond_matches_with_explicit(attr.comptime_cond, ctx.user_defines, ctx.explicit_defines, ctx.target_os) {
			return true
		}
	}
	return false
}

fn freestanding_restricted_call_in_stmts(stmts []ast.Stmt, ctx FreestandingScanContext) string {
	for stmt in stmts {
		call_name := freestanding_restricted_call_in_stmt(stmt, ctx)
		if call_name != '' {
			return call_name
		}
	}
	return ''
}

fn freestanding_restricted_call_in_stmt(stmt ast.Stmt, ctx FreestandingScanContext) string {
	return match stmt {
		ast.AssertStmt {
			expr_call := freestanding_restricted_call_in_exprs([stmt.expr, stmt.extra], ctx)
			if expr_call != '' {
				expr_call
			} else {
				'assert'
			}
		}
		ast.AssignStmt {
			lhs_call := freestanding_restricted_call_in_exprs(stmt.lhs, ctx)
			if lhs_call != '' {
				lhs_call
			} else {
				freestanding_restricted_call_in_exprs(stmt.rhs, ctx)
			}
		}
		ast.BlockStmt {
			freestanding_restricted_call_in_stmts(stmt.stmts, ctx)
		}
		ast.ComptimeStmt {
			freestanding_restricted_call_in_stmt(stmt.stmt, ctx)
		}
		ast.ConstDecl {
			freestanding_restricted_call_in_field_inits(stmt.fields, ctx)
		}
		ast.DeferStmt {
			freestanding_restricted_call_in_stmts(stmt.stmts, ctx)
		}
		ast.ExprStmt {
			freestanding_restricted_call_in_expr(stmt.expr, ctx)
		}
		ast.FnDecl {
			if freestanding_attributes_are_inactive(stmt.attributes, ctx) {
				''
			} else if stmt.attributes.has('live') {
				'live'
			} else {
				freestanding_restricted_call_in_stmts(stmt.stmts, ctx)
			}
		}
		ast.ForInStmt {
			freestanding_restricted_call_in_exprs([stmt.key, stmt.value, stmt.expr], ctx)
		}
		ast.ForStmt {
			init_call := freestanding_restricted_call_in_stmt(stmt.init, ctx)
			if init_call != '' {
				init_call
			} else {
				cond_call := freestanding_restricted_call_in_expr(stmt.cond, ctx)
				if cond_call != '' {
					cond_call
				} else {
					post_call := freestanding_restricted_call_in_stmt(stmt.post, ctx)
					if post_call != '' {
						post_call
					} else {
						freestanding_restricted_call_in_stmts(stmt.stmts, ctx)
					}
				}
			}
		}
		ast.GlobalDecl {
			freestanding_restricted_call_in_field_decls(stmt.fields, ctx)
		}
		ast.InterfaceDecl {
			freestanding_restricted_call_in_field_decls(stmt.fields, ctx)
		}
		ast.LabelStmt {
			freestanding_restricted_call_in_stmt(stmt.stmt, ctx)
		}
		ast.ReturnStmt {
			freestanding_restricted_call_in_exprs(stmt.exprs, ctx)
		}
		ast.StructDecl {
			freestanding_restricted_call_in_field_decls(stmt.fields, ctx)
		}
		else {
			''
		}
	}
}

fn freestanding_restricted_call_in_exprs(exprs []ast.Expr, ctx FreestandingScanContext) string {
	for expr in exprs {
		call_name := freestanding_restricted_call_in_expr(expr, ctx)
		if call_name != '' {
			return call_name
		}
	}
	return ''
}

fn freestanding_restricted_call_in_field_inits(fields []ast.FieldInit, ctx FreestandingScanContext) string {
	for field in fields {
		call_name := freestanding_restricted_call_in_expr(field.value, ctx)
		if call_name != '' {
			return call_name
		}
	}
	return ''
}

fn freestanding_restricted_call_in_field_decls(fields []ast.FieldDecl, ctx FreestandingScanContext) string {
	for field in fields {
		typ_call := freestanding_restricted_call_in_expr(field.typ, ctx)
		if typ_call != '' {
			return typ_call
		}
		value_call := freestanding_restricted_call_in_expr(field.value, ctx)
		if value_call != '' {
			return value_call
		}
	}
	return ''
}

fn freestanding_restricted_call_in_expr(expr ast.Expr, ctx FreestandingScanContext) string {
	return match expr {
		ast.ArrayInitExpr {
			meta_call := freestanding_restricted_call_in_exprs([expr.typ, expr.init, expr.cap,
				expr.len, expr.update_expr], ctx)
			if meta_call != '' {
				meta_call
			} else {
				freestanding_restricted_call_in_exprs(expr.exprs, ctx)
			}
		}
		ast.AsCastExpr {
			freestanding_restricted_call_in_expr(expr.expr, ctx)
		}
		ast.AssocExpr {
			base_call := freestanding_restricted_call_in_expr(expr.expr, ctx)
			if base_call != '' {
				base_call
			} else {
				freestanding_restricted_call_in_field_inits(expr.fields, ctx)
			}
		}
		ast.CallExpr {
			call_name := freestanding_restricted_builtin_call_name(expr.lhs, ctx)
			if call_name != '' {
				call_name
			} else {
				lhs_call := freestanding_restricted_call_in_expr(expr.lhs, ctx)
				if lhs_call != '' {
					lhs_call
				} else {
					freestanding_restricted_call_in_exprs(expr.args, ctx)
				}
			}
		}
		ast.CallOrCastExpr {
			call_name := freestanding_restricted_builtin_call_name(expr.lhs, ctx)
			if call_name != '' {
				call_name
			} else {
				lhs_call := freestanding_restricted_call_in_expr(expr.lhs, ctx)
				if lhs_call != '' {
					lhs_call
				} else {
					freestanding_restricted_call_in_expr(expr.expr, ctx)
				}
			}
		}
		ast.CastExpr {
			freestanding_restricted_call_in_expr(expr.expr, ctx)
		}
		ast.ComptimeExpr {
			freestanding_restricted_call_in_comptime_expr(expr, ctx)
		}
		ast.FieldInit {
			freestanding_restricted_call_in_expr(expr.value, ctx)
		}
		ast.FnLiteral {
			capture_call := freestanding_restricted_call_in_exprs(expr.captured_vars, ctx)
			if capture_call != '' {
				capture_call
			} else {
				freestanding_restricted_call_in_stmts(expr.stmts, ctx)
			}
		}
		ast.GenericArgOrIndexExpr {
			lhs_call := freestanding_restricted_call_in_expr(expr.lhs, ctx)
			if lhs_call != '' {
				lhs_call
			} else {
				freestanding_restricted_call_in_expr(expr.expr, ctx)
			}
		}
		ast.GenericArgs {
			lhs_call := freestanding_restricted_call_in_expr(expr.lhs, ctx)
			if lhs_call != '' {
				lhs_call
			} else {
				freestanding_restricted_call_in_exprs(expr.args, ctx)
			}
		}
		ast.IfExpr {
			cond_call := freestanding_restricted_call_in_expr(expr.cond, ctx)
			if cond_call != '' {
				cond_call
			} else {
				stmts_call := freestanding_restricted_call_in_stmts(expr.stmts, ctx)
				if stmts_call != '' {
					stmts_call
				} else {
					freestanding_restricted_call_in_expr(expr.else_expr, ctx)
				}
			}
		}
		ast.IfGuardExpr {
			freestanding_restricted_call_in_stmt(ast.Stmt(expr.stmt), ctx)
		}
		ast.IndexExpr {
			lhs_call := freestanding_restricted_call_in_expr(expr.lhs, ctx)
			if lhs_call != '' {
				lhs_call
			} else {
				freestanding_restricted_call_in_expr(expr.expr, ctx)
			}
		}
		ast.InfixExpr {
			lhs_call := freestanding_restricted_call_in_expr(expr.lhs, ctx)
			if lhs_call != '' {
				lhs_call
			} else {
				freestanding_restricted_call_in_expr(expr.rhs, ctx)
			}
		}
		ast.InitExpr {
			typ_call := freestanding_restricted_call_in_expr(expr.typ, ctx)
			if typ_call != '' {
				typ_call
			} else {
				freestanding_restricted_call_in_field_inits(expr.fields, ctx)
			}
		}
		ast.KeywordOperator {
			if expr.op in [.key_go, .key_spawn] {
				expr.op.str()
			} else {
				freestanding_restricted_call_in_exprs(expr.exprs, ctx)
			}
		}
		ast.LambdaExpr {
			freestanding_restricted_call_in_expr(expr.expr, ctx)
		}
		ast.LockExpr {
			'lock'
		}
		ast.MapInitExpr {
			key_call := freestanding_restricted_call_in_exprs(expr.keys, ctx)
			if key_call != '' {
				key_call
			} else {
				freestanding_restricted_call_in_exprs(expr.vals, ctx)
			}
		}
		ast.MatchExpr {
			expr_call := freestanding_restricted_call_in_expr(expr.expr, ctx)
			if expr_call != '' {
				expr_call
			} else {
				freestanding_restricted_call_in_match_branches(expr.branches, ctx)
			}
		}
		ast.ModifierExpr {
			if expr.kind == .key_shared {
				'shared'
			} else {
				freestanding_restricted_call_in_expr(expr.expr, ctx)
			}
		}
		ast.OrExpr {
			expr_call := freestanding_restricted_call_in_expr(expr.expr, ctx)
			if expr_call != '' {
				expr_call
			} else {
				freestanding_restricted_call_in_stmts(expr.stmts, ctx)
			}
		}
		ast.ParenExpr {
			freestanding_restricted_call_in_expr(expr.expr, ctx)
		}
		ast.PostfixExpr {
			freestanding_restricted_call_in_expr(expr.expr, ctx)
		}
		ast.PrefixExpr {
			freestanding_restricted_call_in_expr(expr.expr, ctx)
		}
		ast.RangeExpr {
			start_call := freestanding_restricted_call_in_expr(expr.start, ctx)
			if start_call != '' {
				start_call
			} else {
				freestanding_restricted_call_in_expr(expr.end, ctx)
			}
		}
		ast.SelectExpr {
			stmt_call := freestanding_restricted_call_in_stmt(expr.stmt, ctx)
			if stmt_call != '' {
				stmt_call
			} else {
				freestanding_restricted_call_in_stmts(expr.stmts, ctx)
			}
		}
		ast.SelectorExpr {
			freestanding_restricted_call_in_expr(expr.lhs, ctx)
		}
		ast.StringInterLiteral {
			freestanding_restricted_call_in_string_inters(expr.inters, ctx)
		}
		ast.Tuple {
			freestanding_restricted_call_in_exprs(expr.exprs, ctx)
		}
		ast.UnsafeExpr {
			freestanding_restricted_call_in_stmts(expr.stmts, ctx)
		}
		else {
			''
		}
	}
}

fn freestanding_restricted_call_in_comptime_expr(expr ast.ComptimeExpr, ctx FreestandingScanContext) string {
	if expr.expr is ast.IfExpr {
		return freestanding_restricted_call_in_active_comptime_if(expr.expr, ctx)
	}
	return freestanding_restricted_call_in_expr(expr.expr, ctx)
}

fn freestanding_restricted_call_in_active_comptime_if(node ast.IfExpr, ctx FreestandingScanContext) string {
	if ast_comptime_cond_matches_with_explicit(node.cond, ctx.user_defines, ctx.explicit_defines,
		ctx.target_os)
	{
		return freestanding_restricted_call_in_stmts(node.stmts, ctx)
	}
	if node.else_expr is ast.IfExpr {
		if node.else_expr.cond is ast.EmptyExpr {
			return freestanding_restricted_call_in_stmts(node.else_expr.stmts, ctx)
		}
		return freestanding_restricted_call_in_active_comptime_if(node.else_expr, ctx)
	}
	return ''
}

fn freestanding_restricted_call_in_match_branches(branches []ast.MatchBranch, ctx FreestandingScanContext) string {
	for branch in branches {
		cond_call := freestanding_restricted_call_in_exprs(branch.cond, ctx)
		if cond_call != '' {
			return cond_call
		}
		stmts_call := freestanding_restricted_call_in_stmts(branch.stmts, ctx)
		if stmts_call != '' {
			return stmts_call
		}
	}
	return ''
}

fn freestanding_restricted_call_in_string_inters(inters []ast.StringInter, ctx FreestandingScanContext) string {
	for inter in inters {
		expr_call := freestanding_restricted_call_in_expr(inter.expr, ctx)
		if expr_call != '' {
			return expr_call
		}
		format_call := freestanding_restricted_call_in_expr(inter.format_expr, ctx)
		if format_call != '' {
			return format_call
		}
	}
	return ''
}

fn freestanding_restricted_builtin_call_name(lhs ast.Expr, ctx FreestandingScanContext) string {
	if lhs is ast.Ident {
		return freestanding_output_builtin_call_name(lhs.name, ctx)
	}
	return ''
}
