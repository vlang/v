// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import os
import v2.ast
import v2.parser
import v2.pref
import v2.token
import v2.types

// Integration test: run the full pipeline (parse → type check → transform) on
// cmd/v2/v2.v and verify that every expression with a valid position carries a
// type in the environment after transformation.

struct ExprTypeChecker {
	env &types.Environment
mut:
	total         int
	missing       int
	details       []string
	by_kind       map[string]int
	in_generic_fn bool
	generic_miss  int
	cur_fn_name   string
	fn_miss       map[string]int
}

fn test_v2_transformer_all_exprs_have_types() {
	vroot := detect_vroot()
	v2_dir := os.join_path(vroot, 'cmd', 'v2')
	assert os.is_dir(v2_dir), 'cmd/v2 directory not found at ${v2_dir}'

	prefs := &pref.Preferences{
		backend:     .cleanc
		vroot:       vroot
		no_parallel: true
	}

	// --- Parse ---
	mut p := parser.Parser.new(prefs)
	mut file_set := token.FileSet.new()

	// Parse core modules (same order as builder)
	core_module_paths := [
		'builtin',
		'strconv',
		'strings',
		'hash',
		'math.bits',
		'os',
		'time',
		'term',
		'term.termios',
		'os.cmdline',
		'encoding.binary',
		'crypto.sha256',
		'strings.textscanner',
	]
	mut ast_files := []ast.File{}
	for mod_path in core_module_paths {
		module_dir := prefs.get_vlib_module_path(mod_path)
		module_files := get_v_files_from_dir(module_dir)
		parsed := p.parse_files(module_files, mut file_set)
		ast_files << parsed
	}

	// Parse user files (only v2.v, not test files in the same directory)
	user_files := [os.join_path(v2_dir, 'v2.v')]
	parsed_user := p.parse_files(user_files, mut file_set)
	ast_files << parsed_user

	// Parse imports
	mut parsed_imports := []string{}
	parsed_imports << core_module_paths
	for afi := 0; afi < ast_files.len; afi++ {
		ast_file := ast_files[afi]
		for mod in ast_file.imports {
			if mod.name in parsed_imports {
				continue
			}
			mod_dir := prefs.get_module_path(mod.name, ast_file.name)
			module_files := get_v_files_from_dir(mod_dir)
			parsed := p.parse_files(module_files, mut file_set)
			ast_files << parsed
			parsed_imports << mod.name
		}
	}

	assert ast_files.len > 0, 'no files parsed'

	// --- Type Check ---
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(ast_files)

	// --- Transform ---
	mut trans := Transformer.new_with_pref(ast_files, env, prefs)
	transformed := trans.transform_files(ast_files)

	// --- Verify: every expression with a valid pos must have a type ---
	mut etc := ExprTypeChecker{
		env: env
	}

	for file in transformed {
		for stmt in file.stmts {
			etc.check_stmt(stmt)
		}
	}

	// Allow a small number of missing types from transformer-generated synthetic
	// expressions (temp variables, lowered operator calls, etc.) that don't go
	// through the checker. Track this threshold and reduce it as coverage improves.
	max_missing := 385
	if etc.missing > max_missing {
		mut msg := '${etc.missing} of ${etc.total} expressions missing types (max allowed: ${max_missing}).\n'
		msg += 'breakdown by kind:\n'
		for kind, count in etc.by_kind {
			msg += '  ${kind}: ${count}\n'
		}
		msg += 'by function (${etc.fn_miss.len} fns):\n'
		mut fn_counts := []int{}
		mut fn_names := []string{}
		for fn_name, count in etc.fn_miss {
			fn_counts << count
			fn_names << fn_name
		}
		for i := 0; i < fn_counts.len; i++ {
			for j := i + 1; j < fn_counts.len; j++ {
				if fn_counts[j] > fn_counts[i] {
					fn_counts[i], fn_counts[j] = fn_counts[j], fn_counts[i]
					fn_names[i], fn_names[j] = fn_names[j], fn_names[i]
				}
			}
		}
		fn_limit := if fn_counts.len < 50 { fn_counts.len } else { 50 }
		for i := 0; i < fn_limit; i++ {
			msg += '  ${fn_names[i]}: ${fn_counts[i]}\n'
		}
		limit := if etc.details.len < 100 { etc.details.len } else { 100 }
		msg += 'first ${limit} missing:\n'
		for detail in etc.details[..limit] {
			msg += '  ${detail}\n'
		}
		assert false, msg
	}

	assert etc.total > 0, 'no expressions found in transformed AST'
}

// --- Helpers ---

fn detect_vroot() string {
	mut dir := os.getwd()
	for _ in 0 .. 8 {
		if os.is_dir(os.join_path(dir, 'vlib', 'builtin')) {
			return dir
		}
		dir = os.dir(dir)
	}
	home_vroot := os.join_path(os.home_dir(), 'code', 'v')
	if os.is_dir(os.join_path(home_vroot, 'vlib', 'builtin')) {
		return home_vroot
	}
	panic('cannot detect vroot')
}

fn get_v_files_from_dir(dir string) []string {
	entries := os.ls(dir) or { []string{} }
	mut v_files := []string{}
	for file in entries {
		if !file.ends_with('.v') || file.ends_with('.js.v') || file.contains('_test.') {
			continue
		}
		if file.contains('.arm64.') || file.contains('.arm32.') || file.contains('.amd64.') {
			continue
		}
		$if macos {
			if file.contains('_windows.') || file.contains('_linux.') || file.contains('_android') {
				continue
			}
		} $else $if linux {
			if file.contains('_windows.') || file.contains('_macos.') || file.contains('_darwin.')
				|| file.contains('_android') {
				continue
			}
		} $else $if windows {
			if file.contains('_linux.') || file.contains('_macos.') || file.contains('_nix.')
				|| file.contains('_android') {
				continue
			}
		}
		if file.contains('_d_') {
			continue
		}
		v_files << os.join_path(dir, file)
	}
	return v_files
}

// --- AST walkers ---

// has_type checks whether the environment has a type set for the given expression ID.
// This checks directly against the Void(1) sentinel (meaning "unset") rather than
// filtering all Void types, so expressions explicitly typed as void (Void(0)) are
// correctly recognized as having a type.
fn (c &ExprTypeChecker) has_type(id int) bool {
	if id > 0 && id < c.env.expr_type_values.len {
		typ := c.env.expr_type_values[id]
		if typ is types.Void {
			return u8(typ) != 1
		}
		return true
	} else if id < 0 {
		idx := -id
		if idx < c.env.expr_type_neg_values.len {
			typ := c.env.expr_type_neg_values[idx]
			if typ is types.Void {
				return u8(typ) != 1
			}
			return true
		}
	}
	return false
}

fn (mut c ExprTypeChecker) check_expr(expr ast.Expr) {
	pos := expr.pos()
	if pos.is_valid() {
		c.total++
		if c.has_type(pos.id) {
			// ok
		} else {
			if c.in_generic_fn {
				c.generic_miss++
			}
			c.missing++
			c.fn_miss[c.cur_fn_name] = c.fn_miss[c.cur_fn_name] + 1
			kind := expr.type_name()
			c.by_kind[kind] = c.by_kind[kind] + 1
			if c.details.len < 100 {
				extra := match expr {
					ast.Ident { ' name="${expr.name}"' }
					ast.BasicLiteral { ' val="${expr.value}"' }
					ast.StringLiteral { ' val="${expr.value}"' }
					ast.SelectorExpr { ' .sel' }
					ast.CallExpr { ' call' }
					ast.InfixExpr { ' op=${expr.op}' }
					ast.IndexExpr { ' idx' }
					ast.CastExpr { ' cast' }
					ast.PrefixExpr { ' op=${expr.op}' }
					ast.ParenExpr { ' paren' }
					ast.ModifierExpr { ' mod=${expr.kind}' }
					ast.KeywordOperator { ' kw' }
					ast.PostfixExpr { ' op=${expr.op}' }
					ast.IfExpr { ' if' }
					else { '' }
				}
				c.details << 'id=${pos.id} kind=${kind}${extra}'
			}
		}
	}

	// Recurse into sub-expressions
	match expr {
		ast.ArrayInitExpr {
			c.check_expr(expr.typ)
			for e in expr.exprs {
				c.check_expr(e)
			}
			c.check_expr(expr.init)
			c.check_expr(expr.cap)
			c.check_expr(expr.len)
		}
		ast.AsCastExpr {
			c.check_expr(expr.expr)
			c.check_expr(expr.typ)
		}
		ast.AssocExpr {
			c.check_expr(expr.typ)
			c.check_expr(expr.expr)
			for f in expr.fields {
				c.check_expr(f.value)
			}
		}
		ast.BasicLiteral {}
		ast.CallExpr {
			c.check_expr(expr.lhs)
			for arg in expr.args {
				c.check_expr(arg)
			}
		}
		ast.CallOrCastExpr {
			c.check_expr(expr.lhs)
			c.check_expr(expr.expr)
		}
		ast.CastExpr {
			c.check_expr(expr.typ)
			c.check_expr(expr.expr)
		}
		ast.ComptimeExpr {
			c.check_expr(expr.expr)
		}
		ast.FnLiteral {
			for cv in expr.captured_vars {
				c.check_expr(cv)
			}
			for s in expr.stmts {
				c.check_stmt(s)
			}
		}
		ast.GenericArgs {
			c.check_expr(expr.lhs)
			for arg in expr.args {
				c.check_expr(arg)
			}
		}
		ast.GenericArgOrIndexExpr {
			c.check_expr(expr.lhs)
			c.check_expr(expr.expr)
		}
		ast.Ident {}
		ast.IfExpr {
			c.check_expr(expr.cond)
			for s in expr.stmts {
				c.check_stmt(s)
			}
			c.check_expr(expr.else_expr)
		}
		ast.IfGuardExpr {
			c.check_stmt(ast.Stmt(expr.stmt))
		}
		ast.InfixExpr {
			c.check_expr(expr.lhs)
			c.check_expr(expr.rhs)
		}
		ast.IndexExpr {
			c.check_expr(expr.lhs)
			c.check_expr(expr.expr)
		}
		ast.InitExpr {
			c.check_expr(expr.typ)
			for f in expr.fields {
				c.check_expr(f.value)
			}
		}
		ast.KeywordOperator {
			for e in expr.exprs {
				c.check_expr(e)
			}
		}
		ast.LambdaExpr {
			c.check_expr(expr.expr)
		}
		ast.LockExpr {
			for e in expr.lock_exprs {
				c.check_expr(e)
			}
			for e in expr.rlock_exprs {
				c.check_expr(e)
			}
			for s in expr.stmts {
				c.check_stmt(s)
			}
		}
		ast.MapInitExpr {
			c.check_expr(expr.typ)
			for k in expr.keys {
				c.check_expr(k)
			}
			for v in expr.vals {
				c.check_expr(v)
			}
		}
		ast.MatchExpr {
			c.check_expr(expr.expr)
			for br in expr.branches {
				for cond in br.cond {
					c.check_expr(cond)
				}
				for s in br.stmts {
					c.check_stmt(s)
				}
			}
		}
		ast.ModifierExpr {
			c.check_expr(expr.expr)
		}
		ast.OrExpr {
			c.check_expr(expr.expr)
			for s in expr.stmts {
				c.check_stmt(s)
			}
		}
		ast.ParenExpr {
			c.check_expr(expr.expr)
		}
		ast.PostfixExpr {
			c.check_expr(expr.expr)
		}
		ast.PrefixExpr {
			c.check_expr(expr.expr)
		}
		ast.RangeExpr {
			c.check_expr(expr.start)
			c.check_expr(expr.end)
		}
		ast.SelectExpr {
			c.check_stmt(expr.stmt)
			for s in expr.stmts {
				c.check_stmt(s)
			}
			c.check_expr(expr.next)
		}
		ast.SelectorExpr {
			c.check_expr(expr.lhs)
		}
		ast.SqlExpr {
			c.check_expr(expr.expr)
		}
		ast.StringInterLiteral {
			for inter in expr.inters {
				c.check_expr(inter.expr)
				c.check_expr(inter.format_expr)
			}
		}
		ast.StringLiteral {}
		ast.Tuple {
			for e in expr.exprs {
				c.check_expr(e)
			}
		}
		ast.UnsafeExpr {
			for s in expr.stmts {
				c.check_stmt(s)
			}
		}
		else {}
	}
}

fn (mut c ExprTypeChecker) check_stmt(stmt ast.Stmt) {
	match stmt {
		ast.AssertStmt {
			c.check_expr(stmt.expr)
			c.check_expr(stmt.extra)
		}
		ast.AssignStmt {
			for e in stmt.lhs {
				c.check_expr(e)
			}
			for e in stmt.rhs {
				c.check_expr(e)
			}
		}
		ast.BlockStmt {
			for s in stmt.stmts {
				c.check_stmt(s)
			}
		}
		ast.ComptimeStmt {
			c.check_stmt(stmt.stmt)
		}
		ast.ConstDecl {
			for f in stmt.fields {
				c.check_expr(f.value)
			}
		}
		ast.DeferStmt {
			for s in stmt.stmts {
				c.check_stmt(s)
			}
		}
		ast.ExprStmt {
			c.check_expr(stmt.expr)
		}
		ast.FnDecl {
			prev_generic := c.in_generic_fn
			prev_fn := c.cur_fn_name
			c.cur_fn_name = stmt.name
			if stmt.typ.generic_params.len > 0 {
				c.in_generic_fn = true
			}
			for s in stmt.stmts {
				c.check_stmt(s)
			}
			c.in_generic_fn = prev_generic
			c.cur_fn_name = prev_fn
		}
		ast.ForStmt {
			c.check_stmt(stmt.init)
			c.check_expr(stmt.cond)
			c.check_stmt(stmt.post)
			for s in stmt.stmts {
				c.check_stmt(s)
			}
		}
		ast.ForInStmt {
			c.check_expr(stmt.key)
			c.check_expr(stmt.value)
			c.check_expr(stmt.expr)
		}
		ast.LabelStmt {
			c.check_stmt(stmt.stmt)
		}
		ast.ReturnStmt {
			for e in stmt.exprs {
				c.check_expr(e)
			}
		}
		ast.EnumDecl {
			for f in stmt.fields {
				c.check_expr(f.value)
			}
		}
		ast.GlobalDecl {
			for f in stmt.fields {
				c.check_expr(f.value)
			}
		}
		ast.InterfaceDecl {}
		ast.StructDecl {}
		ast.TypeDecl {}
		else {}
	}
}
