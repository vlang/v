// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
module transformer

import os
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token
import v2.types

// Helper to create a minimal transformer for testing
fn create_test_transformer() &Transformer {
	env := &types.Environment{}
	return &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		local_decl_types:            map[string]types.Type{}
	}
}

// Helper to create a transformer with a scope containing variable types
fn create_transformer_with_vars(vars map[string]types.Type) &Transformer {
	env := &types.Environment{}
	mut scope := types.new_scope(unsafe { nil })
	for name, typ in vars {
		scope.insert(name, value_object_from_type(typ))
	}
	return &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		scope:                       scope
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		local_decl_types:            map[string]types.Type{}
	}
}

fn test_value_object_from_type_stores_value_symbol_type() {
	obj := value_object_from_type(types.Type(types.int_))
	assert obj is types.TypeObject
	assert obj.typ() is types.Primitive
}

fn transform_code_for_test(code string) []ast.File {
	tmp_file := '/tmp/v2_transformer_test_${os.getpid()}.v'
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut transformer := Transformer.new_with_pref(files, env, prefs)
	return transformer.transform_files(files)
}

fn transform_code_with_env_for_test(code string) (&types.Environment, []ast.File) {
	tmp_file := '/tmp/v2_transformer_env_test_${os.getpid()}.v'
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut transformer := Transformer.new_with_pref(files, env, prefs)
	transformed := transformer.transform_files(files)
	return env, transformed
}

struct TestSource {
	rel  string
	code string
}

fn transform_sources_for_test(sources []TestSource) []ast.File {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_transformer_sources_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	mut paths := []string{cap: sources.len}
	for source in sources {
		path := os.join_path(tmp_dir, source.rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, source.code) or { panic('failed to write ${path}') }
		paths << path
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files(paths, mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut transformer := Transformer.new_with_pref(files, env, prefs)
	return transformer.transform_files(files)
}

fn ident_name_from_expr_for_test(expr ast.Expr) ?string {
	if expr is ast.Ident {
		return expr.name
	}
	if expr is ast.ModifierExpr && expr.expr is ast.Ident {
		return expr.expr.name
	}
	return none
}

fn test_sort_selector_path_after_root_keeps_nested_fields() {
	t := create_test_transformer()
	expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'a'
			})
			rhs: ast.Ident{
				name: 'path'
			}
		})
		rhs: ast.Ident{
			name: 'len'
		}
	})
	path := t.selector_path_after_root(expr, 'a') or { panic('missing selector path') }

	assert path.len == 2
	assert path[0] == 'path'
	assert path[1] == 'len'
}

fn collect_call_names_from_stmt(stmt ast.Stmt, mut names []string) {
	match stmt {
		ast.AssignStmt {
			for expr in stmt.lhs {
				collect_call_names_from_expr(expr, mut names)
			}
			for expr in stmt.rhs {
				collect_call_names_from_expr(expr, mut names)
			}
		}
		ast.AssertStmt {
			collect_call_names_from_expr(stmt.expr, mut names)
			collect_call_names_from_expr(stmt.extra, mut names)
		}
		ast.BlockStmt {
			for nested in stmt.stmts {
				collect_call_names_from_stmt(nested, mut names)
			}
		}
		ast.ComptimeStmt {
			collect_call_names_from_stmt(stmt.stmt, mut names)
		}
		ast.DeferStmt {
			for nested in stmt.stmts {
				collect_call_names_from_stmt(nested, mut names)
			}
		}
		ast.ExprStmt {
			collect_call_names_from_expr(stmt.expr, mut names)
		}
		ast.ForInStmt {
			collect_call_names_from_expr(stmt.key, mut names)
			collect_call_names_from_expr(stmt.value, mut names)
			collect_call_names_from_expr(stmt.expr, mut names)
		}
		ast.ForStmt {
			collect_call_names_from_stmt(stmt.init, mut names)
			collect_call_names_from_expr(stmt.cond, mut names)
			collect_call_names_from_stmt(stmt.post, mut names)
			for nested in stmt.stmts {
				collect_call_names_from_stmt(nested, mut names)
			}
		}
		ast.LabelStmt {
			collect_call_names_from_stmt(stmt.stmt, mut names)
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				collect_call_names_from_expr(expr, mut names)
			}
		}
		else {}
	}
}

fn collect_call_names_from_expr(expr ast.Expr, mut names []string) {
	match expr {
		ast.ArrayInitExpr {
			collect_call_names_from_expr(expr.init, mut names)
			collect_call_names_from_expr(expr.cap, mut names)
			collect_call_names_from_expr(expr.len, mut names)
			for item in expr.exprs {
				collect_call_names_from_expr(item, mut names)
			}
		}
		ast.AsCastExpr {
			collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.AssocExpr {
			collect_call_names_from_expr(expr.expr, mut names)
			for field in expr.fields {
				collect_call_names_from_expr(field.value, mut names)
			}
		}
		ast.CallExpr {
			if expr.lhs is ast.Ident {
				names << expr.lhs.name
			}
			collect_call_names_from_expr(expr.lhs, mut names)
			for arg in expr.args {
				collect_call_names_from_expr(arg, mut names)
			}
		}
		ast.CallOrCastExpr {
			collect_call_names_from_expr(expr.lhs, mut names)
			collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.CastExpr {
			collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.FieldInit {
			collect_call_names_from_expr(expr.value, mut names)
		}
		ast.FnLiteral {
			for nested in expr.stmts {
				collect_call_names_from_stmt(nested, mut names)
			}
		}
		ast.GenericArgOrIndexExpr {
			collect_call_names_from_expr(expr.lhs, mut names)
			collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.GenericArgs {
			collect_call_names_from_expr(expr.lhs, mut names)
			for arg in expr.args {
				collect_call_names_from_expr(arg, mut names)
			}
		}
		ast.IfExpr {
			collect_call_names_from_expr(expr.cond, mut names)
			for nested in expr.stmts {
				collect_call_names_from_stmt(nested, mut names)
			}
			collect_call_names_from_expr(expr.else_expr, mut names)
		}
		ast.IndexExpr {
			collect_call_names_from_expr(expr.lhs, mut names)
			collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.InfixExpr {
			collect_call_names_from_expr(expr.lhs, mut names)
			collect_call_names_from_expr(expr.rhs, mut names)
		}
		ast.InitExpr {
			for field in expr.fields {
				collect_call_names_from_expr(field.value, mut names)
			}
		}
		ast.LambdaExpr {
			collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.LockExpr {
			for nested in expr.stmts {
				collect_call_names_from_stmt(nested, mut names)
			}
		}
		ast.MapInitExpr {
			for key in expr.keys {
				collect_call_names_from_expr(key, mut names)
			}
			for val in expr.vals {
				collect_call_names_from_expr(val, mut names)
			}
		}
		ast.MatchExpr {
			collect_call_names_from_expr(expr.expr, mut names)
			for branch in expr.branches {
				for cond in branch.cond {
					collect_call_names_from_expr(cond, mut names)
				}
				for nested in branch.stmts {
					collect_call_names_from_stmt(nested, mut names)
				}
			}
		}
		ast.ModifierExpr {
			collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.OrExpr {
			collect_call_names_from_expr(expr.expr, mut names)
			for nested in expr.stmts {
				collect_call_names_from_stmt(nested, mut names)
			}
		}
		ast.ParenExpr {
			collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.PostfixExpr {
			collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.PrefixExpr {
			collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.RangeExpr {
			collect_call_names_from_expr(expr.start, mut names)
			collect_call_names_from_expr(expr.end, mut names)
		}
		ast.SelectExpr {
			collect_call_names_from_stmt(expr.stmt, mut names)
			for nested in expr.stmts {
				collect_call_names_from_stmt(nested, mut names)
			}
			collect_call_names_from_expr(expr.next, mut names)
		}
		ast.SelectorExpr {
			collect_call_names_from_expr(expr.lhs, mut names)
		}
		ast.StringInterLiteral {
			for inter in expr.inters {
				collect_call_names_from_expr(inter.expr, mut names)
				collect_call_names_from_expr(inter.format_expr, mut names)
			}
		}
		ast.Tuple {
			for item in expr.exprs {
				collect_call_names_from_expr(item, mut names)
			}
		}
		ast.UnsafeExpr {
			for nested in expr.stmts {
				collect_call_names_from_stmt(nested, mut names)
			}
		}
		else {}
	}
}

fn call_names_for_fn(files []ast.File, fn_name string) []string {
	mut names := []string{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == fn_name {
				for nested in stmt.stmts {
					collect_call_names_from_stmt(nested, mut names)
				}
			}
		}
	}
	return names
}

fn stmt_has_assign_op(stmt ast.Stmt, op token.Token) bool {
	match stmt {
		ast.AssignStmt {
			return stmt.op == op
		}
		ast.BlockStmt {
			for nested in stmt.stmts {
				if stmt_has_assign_op(nested, op) {
					return true
				}
			}
		}
		ast.ExprStmt {
			return expr_has_assign_op(stmt.expr, op)
		}
		ast.ForStmt {
			if stmt_has_assign_op(stmt.init, op) || stmt_has_assign_op(stmt.post, op) {
				return true
			}
			for nested in stmt.stmts {
				if stmt_has_assign_op(nested, op) {
					return true
				}
			}
		}
		ast.LabelStmt {
			return stmt_has_assign_op(stmt.stmt, op)
		}
		else {}
	}

	return false
}

fn expr_has_assign_op(expr ast.Expr, op token.Token) bool {
	match expr {
		ast.IfExpr {
			for stmt in expr.stmts {
				if stmt_has_assign_op(stmt, op) {
					return true
				}
			}
			return expr_has_assign_op(expr.else_expr, op)
		}
		ast.MatchExpr {
			for branch in expr.branches {
				for stmt in branch.stmts {
					if stmt_has_assign_op(stmt, op) {
						return true
					}
				}
			}
		}
		ast.UnsafeExpr {
			for stmt in expr.stmts {
				if stmt_has_assign_op(stmt, op) {
					return true
				}
			}
		}
		else {}
	}

	return false
}

fn fn_has_assign_op(files []ast.File, fn_name string, op token.Token) bool {
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == fn_name {
				for nested in stmt.stmts {
					if stmt_has_assign_op(nested, op) {
						return true
					}
				}
			}
		}
	}
	return false
}

fn count_label_name_in_files(files []ast.File, fn_name string, label string) int {
	mut count := 0
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == fn_name {
				count += count_label_name_in_stmts(stmt.stmts, label)
			}
		}
	}
	return count
}

fn count_label_name_in_stmts(stmts []ast.Stmt, label string) int {
	mut count := 0
	for stmt in stmts {
		count += count_label_name_in_stmt(stmt, label)
	}
	return count
}

fn count_label_name_in_stmt(stmt ast.Stmt, label string) int {
	match stmt {
		ast.BlockStmt {
			return count_label_name_in_stmts(stmt.stmts, label)
		}
		ast.ComptimeStmt {
			return count_label_name_in_stmt(stmt.stmt, label)
		}
		ast.DeferStmt {
			return count_label_name_in_stmts(stmt.stmts, label)
		}
		ast.ExprStmt {
			return count_label_name_in_expr(stmt.expr, label)
		}
		ast.ForStmt {
			return count_label_name_in_stmt(stmt.init, label) +
				count_label_name_in_stmt(stmt.post, label) +
				count_label_name_in_expr(stmt.cond, label) +
				count_label_name_in_stmts(stmt.stmts, label)
		}
		ast.LabelStmt {
			mut count := if stmt.name == label { 1 } else { 0 }
			count += count_label_name_in_stmt(stmt.stmt, label)
			return count
		}
		ast.ReturnStmt {
			mut count := 0
			for expr in stmt.exprs {
				count += count_label_name_in_expr(expr, label)
			}
			return count
		}
		else {
			return 0
		}
	}
}

fn count_label_name_in_expr(expr ast.Expr, label string) int {
	match expr {
		ast.IfExpr {
			return count_label_name_in_expr(expr.cond, label) +
				count_label_name_in_stmts(expr.stmts, label) +
				count_label_name_in_expr(expr.else_expr, label)
		}
		ast.MatchExpr {
			mut count := 0
			for branch in expr.branches {
				count += count_label_name_in_stmts(branch.stmts, label)
			}
			return count
		}
		ast.UnsafeExpr {
			return count_label_name_in_stmts(expr.stmts, label)
		}
		else {
			return 0
		}
	}
}

fn find_call_with_lhs_suffix_in_stmts(stmts []ast.Stmt, suffix string) ?ast.CallExpr {
	for stmt in stmts {
		if call := find_call_with_lhs_suffix_in_stmt(stmt, suffix) {
			return call
		}
	}
	return none
}

fn find_call_with_lhs_suffix_in_stmt(stmt ast.Stmt, suffix string) ?ast.CallExpr {
	match stmt {
		ast.AssignStmt {
			for expr in stmt.lhs {
				if call := find_call_with_lhs_suffix_in_expr(expr, suffix) {
					return call
				}
			}
			for expr in stmt.rhs {
				if call := find_call_with_lhs_suffix_in_expr(expr, suffix) {
					return call
				}
			}
		}
		ast.BlockStmt {
			return find_call_with_lhs_suffix_in_stmts(stmt.stmts, suffix)
		}
		ast.ExprStmt {
			return find_call_with_lhs_suffix_in_expr(stmt.expr, suffix)
		}
		ast.ForInStmt {
			if call := find_call_with_lhs_suffix_in_expr(stmt.expr, suffix) {
				return call
			}
		}
		ast.ForStmt {
			if call := find_call_with_lhs_suffix_in_stmt(stmt.init, suffix) {
				return call
			}
			if call := find_call_with_lhs_suffix_in_expr(stmt.cond, suffix) {
				return call
			}
			if call := find_call_with_lhs_suffix_in_stmt(stmt.post, suffix) {
				return call
			}
			return find_call_with_lhs_suffix_in_stmts(stmt.stmts, suffix)
		}
		ast.FnDecl {
			return find_call_with_lhs_suffix_in_stmts(stmt.stmts, suffix)
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				if call := find_call_with_lhs_suffix_in_expr(expr, suffix) {
					return call
				}
			}
		}
		else {}
	}

	return none
}

fn find_call_with_lhs_suffix_in_expr(expr ast.Expr, suffix string) ?ast.CallExpr {
	match expr {
		ast.ArrayInitExpr {
			for item in expr.exprs {
				if call := find_call_with_lhs_suffix_in_expr(item, suffix) {
					return call
				}
			}
		}
		ast.CallExpr {
			if expr.lhs is ast.Ident && (expr.lhs as ast.Ident).name.ends_with(suffix) {
				return expr
			}
			if call := find_call_with_lhs_suffix_in_expr(expr.lhs, suffix) {
				return call
			}
			for arg in expr.args {
				if call := find_call_with_lhs_suffix_in_expr(arg, suffix) {
					return call
				}
			}
		}
		ast.CastExpr {
			return find_call_with_lhs_suffix_in_expr(expr.expr, suffix)
		}
		ast.FieldInit {
			return find_call_with_lhs_suffix_in_expr(expr.value, suffix)
		}
		ast.IfExpr {
			if call := find_call_with_lhs_suffix_in_expr(expr.cond, suffix) {
				return call
			}
			if call := find_call_with_lhs_suffix_in_stmts(expr.stmts, suffix) {
				return call
			}
			return find_call_with_lhs_suffix_in_expr(expr.else_expr, suffix)
		}
		ast.InfixExpr {
			if call := find_call_with_lhs_suffix_in_expr(expr.lhs, suffix) {
				return call
			}
			return find_call_with_lhs_suffix_in_expr(expr.rhs, suffix)
		}
		ast.InitExpr {
			for field in expr.fields {
				if call := find_call_with_lhs_suffix_in_expr(field.value, suffix) {
					return call
				}
			}
		}
		ast.MatchExpr {
			if call := find_call_with_lhs_suffix_in_expr(expr.expr, suffix) {
				return call
			}
			for branch in expr.branches {
				if call := find_call_with_lhs_suffix_in_stmts(branch.stmts, suffix) {
					return call
				}
			}
		}
		ast.ModifierExpr {
			return find_call_with_lhs_suffix_in_expr(expr.expr, suffix)
		}
		ast.ParenExpr {
			return find_call_with_lhs_suffix_in_expr(expr.expr, suffix)
		}
		ast.PostfixExpr {
			return find_call_with_lhs_suffix_in_expr(expr.expr, suffix)
		}
		ast.PrefixExpr {
			return find_call_with_lhs_suffix_in_expr(expr.expr, suffix)
		}
		ast.SelectorExpr {
			return find_call_with_lhs_suffix_in_expr(expr.lhs, suffix)
		}
		ast.UnsafeExpr {
			return find_call_with_lhs_suffix_in_stmts(expr.stmts, suffix)
		}
		else {}
	}

	return none
}

// string_type returns the builtin v2 string type.
fn string_type() types.Type {
	return types.string_
}

fn test_transform_decl_assign_keeps_explicit_as_cast_type_with_stale_lhs_name_scope() {
	env := &types.Environment{}
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('inner', types.Type(types.Struct{
		name: 'ast__Expr'
	}))
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		scope:                       scope
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
	}
	result := t.transform_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [
			ast.Expr(ast.Ident{
				name: 'inner'
			}),
		]
		rhs: [
			ast.Expr(ast.AsCastExpr{
				expr: ast.Expr(ast.Ident{
					name: 'node'
				})
				typ:  ast.Expr(ast.SelectorExpr{
					lhs: ast.Expr(ast.Ident{
						name: 'ast'
					})
					rhs: ast.Ident{
						name: 'PrefixExpr'
					}
				})
			}),
		]
	})
	assert result.rhs[0] is ast.AsCastExpr
}

fn test_transform_decl_assign_does_not_wrap_rhs_with_stale_lhs_sumtype_scope() {
	env := &types.Environment{}
	type_sum := types.Type(types.SumType{
		name:     'types__Type'
		variants: [string_type()]
	})
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('receiver_type', type_sum)
	mut types_scope := types.new_scope(unsafe { nil })
	types_scope.insert('Type', type_sum)
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		scope:                       scope
		cur_module:                  'transformer'
		cached_scopes:               {
			'types': types_scope
		}
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
	}
	result := t.transform_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [
			ast.Expr(ast.Ident{
				name: 'receiver_type'
			}),
		]
		rhs: [
			ast.Expr(ast.StringLiteral{
				value: 'flag'
				kind:  .v
			}),
		]
	})
	assert result.rhs[0] is ast.StringLiteral
}

fn test_transform_decl_assign_registers_smartcast_selector_type() {
	env := &types.Environment{}
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('Interface', types.Type(types.Struct{
		name: 'ast__Interface'
	}))
	mut fn_scope := types.new_scope(unsafe { nil })
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		scope:                       fn_scope
		fn_root_scope:               fn_scope
		cur_module:                  'checker'
		cached_scopes:               {
			'ast': ast_scope
		}
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
	}
	t.push_smartcast_full('parent_sym.info', 'ast__Interface', 'ast__Interface', 'ast__TypeInfo')
	_ := t.transform_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'generic_info'
		})]
		rhs: [
			ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'parent_sym'
				})
				rhs: ast.Ident{
					name: 'info'
				}
			}),
		]
	})
	typ := fn_scope.lookup_var_type('generic_info') or {
		assert false, 'generic_info type was not registered'
		return
	}
	assert typ is types.Struct
	assert (typ as types.Struct).name == 'ast__Interface'
}

fn test_smartcast_method_call_prefers_variant_method_over_sumtype_method() {
	files := transform_code_for_test('
struct Primitive {}

struct Enum {
	name string
}

type Type = Primitive | Enum

fn (t Type) name() string {
	_ = t
	return "type"
}

fn (t Primitive) name() string {
	_ = t
	return "primitive"
}

fn (t Enum) name() string {
	return t.name
}

fn type_name(t Type) string {
	return match t {
		Primitive, Enum { t.name() }
	}
}
')
	call_names := call_names_for_fn(files, 'type_name')
	assert 'Primitive__name' in call_names
	assert 'Enum__name' in call_names
	assert 'Type__name' !in call_names
}

fn test_flag_enum_set_on_local_variable_lowers_to_assignment() {
	files := transform_code_for_test('
@[flag]
enum Flags {
	empty
	enabled
}

fn set_flag() Flags {
	mut attrs := Flags.empty
	attrs.set(.enabled)
	return attrs
}
')
	call_names := call_names_for_fn(files, 'set_flag')
	assert 'Flags__set' !in call_names
	assert fn_has_assign_op(files, 'set_flag', .or_assign)
}

fn test_flag_enum_set_inside_statement_match_lowers_to_assignment() {
	files := transform_code_for_test('
@[flag]
enum Flags {
	empty
	enabled
}

fn set_flag(names []string) Flags {
	mut attrs := Flags.empty
	for name in names {
		match name {
			"enabled" { attrs.set(.enabled) }
			else {}
		}
	}
	return attrs
}
')
	call_names := call_names_for_fn(files, 'set_flag')
	assert 'Flags__set' !in call_names
	assert fn_has_assign_op(files, 'set_flag', .or_assign)
}

fn test_embedded_struct_selector_uses_embedded_method_owner() {
	files := transform_code_for_test('
struct Request {}
struct Response {}

struct SilentStreamingDownloader {}

fn (mut d SilentStreamingDownloader) on_finish(request &Request, response &Response) ! {
	_ = request
	_ = response
}

struct TerminalStreamingDownloader {
	SilentStreamingDownloader
}

fn finish(mut d TerminalStreamingDownloader, request &Request, response &Response) ! {
	d.SilentStreamingDownloader.on_finish(request, response)!
}
')
	call_names := call_names_for_fn(files, 'finish')
	assert 'SilentStreamingDownloader__on_finish' in call_names
	assert 'int__on_finish' !in call_names
}

fn test_alias_receiver_method_is_resolved_before_base_container_method() {
	files := transform_code_for_test('
type Builder = []u8

fn (mut b Builder) str() string {
	_ = b
	return ""
}

fn builder_str() string {
	mut b := Builder([]u8{})
	return b.str()
}
')
	call_names := call_names_for_fn(files, 'builder_str')
	assert 'Builder__str' in call_names
	assert 'array__str' !in call_names
}

fn test_qualified_alias_receiver_uses_concrete_base_method_with_same_short_name() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'base/base.v'
			code: 'module base

pub struct SSLConn {}

pub fn (mut s SSLConn) connect() {
	_ = s
}
'
		},
		TestSource{
			rel:  'ssl/ssl.v'
			code: 'module ssl

import base

pub type SSLConn = base.SSLConn
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module main

import ssl

fn call(mut s ssl.SSLConn) {
	s.connect()
}
'
		},
	])
	call_names := call_names_for_fn(files, 'call')
	assert 'base__SSLConn__connect' in call_names, 'expected base method owner, got ${call_names}'
	assert 'ssl__SSLConn__connect' !in call_names
}

fn test_imported_global_receiver_method_uses_global_type() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'ast/ast.v'
			code: 'module ast

pub type Type = u32

pub struct Table {}

__global global_table = &Table(unsafe { nil })

pub fn (t &Table) type_to_str(typ Type) string {
	_ = t
	_ = typ
	return ""
}
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module transformer

import ast

struct Param {
	typ ast.Type
}

struct FnDecl {
	receiver Param
}

fn call(node &FnDecl) string {
	return global_table.type_to_str(node.receiver.typ)
}
'
		},
	])
	call_names := call_names_for_fn(files, 'call')
	assert 'ast__Table__type_to_str' in call_names, 'expected ast.Table method owner, got ${call_names}'
	assert 'int__type_to_str' !in call_names
}

fn test_sumtype_type_name_on_imported_struct_field_is_lowered() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'astx/astx.v'
			code: 'module astx

pub type Expr = Ident | Number

pub struct Ident {}

pub struct Number {}

pub struct CallExpr {
pub:
	lhs Expr
}
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module main

import astx

fn call_target_kind(expr astx.CallExpr) string {
	return expr.lhs.type_name()
}
'
		},
	])
	call_names := call_names_for_fn(files, 'call_target_kind')
	assert 'astx__Expr__type_name' !in call_names
	assert 'Expr__type_name' !in call_names
}

fn test_map_alias_field_clone_lowers_to_builtin_map_clone() {
	files := transform_code_for_test('
type TypeID = int

struct TypeStore {
	cache map[string]TypeID
}

struct Module {
	type_store TypeStore
}

fn clone_cache(m Module) map[string]TypeID {
	return m.type_store.cache.clone()
}
')
	call_names := call_names_for_fn(files, 'clone_cache')
	assert 'map__clone' in call_names
	assert 'Map_string_TypeID__clone' !in call_names
	assert 'Map_string_main__TypeID__clone' !in call_names
}

fn test_string_index_byte_methods_on_nested_receiver_fields_use_u8_receiver() {
	files := transform_code_for_test('
struct Scanner {
	src string
	offset int
}

struct Parser {
	scanner &Scanner
}

fn (c u8) is_space() bool {
	return true
}

fn (c u8) is_letter() bool {
	return true
}

fn (p &Parser) peek_dollar_keyword() string {
	if p.scanner.offset >= p.scanner.src.len {
		return ""
	}
	mut idx := p.scanner.offset
	for idx < p.scanner.src.len && p.scanner.src[idx].is_space() {
		idx++
	}
	start := idx
	for idx < p.scanner.src.len && p.scanner.src[idx].is_letter() {
		idx++
	}
	return p.scanner.src[start..idx]
}
')
	call_names := call_names_for_fn(files, 'peek_dollar_keyword')
	assert 'u8__is_space' in call_names
	assert 'u8__is_letter' in call_names
	assert 'int__is_space' !in call_names
	assert 'int__is_letter' !in call_names
}

fn test_smartcast_call_arg_keeps_original_sumtype_when_param_expects_sumtype() {
	files := transform_code_for_test('
struct Table {}

struct Checker {
	table Table
}

struct GlobalField {}
struct Var {}

type ScopeObject = GlobalField | Var

fn (t Table) is_interface_var(obj ScopeObject) bool {
	_ = t
	_ = obj
	return true
}

fn uses_scope_object(mut c Checker, mut obj ScopeObject) bool {
	match mut obj {
		Var {
			return c.table.is_interface_var(obj)
		}
		else {}
	}
	return false
}
')
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'uses_scope_object' {
				call := find_call_with_lhs_suffix_in_stmts(stmt.stmts, '__is_interface_var') or {
					assert false, 'expected transformed is_interface_var call'
					return
				}
				assert call.args.len > 0
				arg := call.args[call.args.len - 1]
				assert arg is ast.Ident, 'sumtype call arg was wrapped as ${arg.type_name()}'
				assert (arg as ast.Ident).name == 'obj'
				return
			}
		}
	}
	assert false, 'uses_scope_object was not found'
}

fn test_smartcast_method_call_arg_keeps_original_sumtype_when_param_expects_sumtype() {
	files := transform_code_for_test('
struct Holder {}
struct Gen {}

type Type = Holder | int

fn (mut g Gen) use_type(value Type, holder Holder) {
	_ = g
	_ = value
	_ = holder
}

fn use_smartcasted_type(mut g Gen, concrete Type) {
	if concrete is Holder {
		g.use_type(concrete, concrete as Holder)
	}
}
')
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'use_smartcasted_type' {
				call := find_call_with_lhs_suffix_in_stmts(stmt.stmts, 'Gen__use_type') or {
					assert false, 'expected transformed Gen__use_type call'
					return
				}
				assert call.args.len == 3
				arg := call.args[1]
				assert arg is ast.Ident, 'sumtype method call arg was wrapped as ${arg.type_name()}'
				assert (arg as ast.Ident).name == 'concrete'
				return
			}
		}
	}
	assert false, 'use_smartcasted_type was not found'
}

fn test_map_index_or_decl_keeps_declared_sumtype_for_later_smartcast_call_arg() {
	files := transform_code_for_test('
struct Holder {}
struct Gen {}

type Type = Holder | int

fn (mut g Gen) use_type(value Type, holder Holder) {
	_ = g
	_ = value
	_ = holder
}

fn use_map_or_smartcasted_type(mut g Gen, active map[string]Type, name string) {
	concrete := active[name] or { return }
	if concrete !is Holder {
		return
	}
	g.use_type(concrete, concrete as Holder)
}
')
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'use_map_or_smartcasted_type' {
				call := find_call_with_lhs_suffix_in_stmts(stmt.stmts, 'Gen__use_type') or {
					assert false, 'expected transformed Gen__use_type call'
					return
				}
				assert call.args.len == 3
				arg := call.args[1]
				assert arg is ast.Ident, 'sumtype method call arg was wrapped as ${arg.type_name()}'
				assert (arg as ast.Ident).name == 'concrete'
				return
			}
		}
	}
	assert false, 'use_map_or_smartcasted_type was not found'
}

fn test_sumtype_call_arg_uses_declared_local_type_when_current_type_is_narrowed() {
	global_field_type := types.Type(types.Struct{
		name: 'GlobalField'
	})
	var_type := types.Type(types.Struct{
		name: 'Var'
	})
	scope_object_type := types.Type(types.SumType{
		name:     'ScopeObject'
		variants: [global_field_type, var_type]
	})
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('GlobalField', global_field_type)
	scope.insert('Var', var_type)
	scope.insert('ScopeObject', scope_object_type)
	scope.insert_or_update('obj', var_type)
	mut env := types.Environment.new()
	obj_pos := token.Pos{
		id: 8901
	}
	env.set_expr_type(obj_pos.id, var_type)
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		scope:                       scope
		fn_root_scope:               scope
		cached_scopes:               {
			'main': scope
		}
		local_decl_types:            {
			'obj': scope_object_type
		}
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		smartcast_expr_counts:       map[string]int{}
	}
	arg := ast.Expr(ast.Ident{
		name: 'obj'
		pos:  obj_pos
	})
	out := t.transform_call_arg_with_sumtype_check(arg, CallFnInfo{
		param_types: [scope_object_type]
	}, 0)
	assert out is ast.Ident, 'declared sumtype local was wrapped as ${out.type_name()}'
	assert (out as ast.Ident).name == 'obj'
}

fn test_is_pointer_type_handles_unresolved_alias_base_type() {
	t := create_test_transformer()
	assert !t.is_pointer_type(types.Type(types.Alias{
		name: 'UnresolvedAlias'
	}))
}

fn test_expr_to_string_handles_no_arg_selector_call_for_smartcasts() {
	t := create_test_transformer()
	call := ast.Expr(ast.CallExpr{
		lhs: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'branch'
				})
				rhs: ast.Ident{
					name: 'stmts'
				}
			})
			rhs: ast.Ident{
				name: 'last'
			}
		})
	})
	assert t.expr_to_string(call) == 'branch.stmts.last()'
}

fn test_expr_to_string_handles_indexed_selector_for_smartcasts() {
	t := create_test_transformer()
	expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.IndexExpr{
			lhs:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'node'
				})
				rhs: ast.Ident{
					name: 'branches'
				}
			})
			expr: ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})
		})
		rhs: ast.Ident{
			name: 'cond'
		}
	})
	assert t.expr_to_string(expr) == 'node.branches[0].cond'
}

fn test_smartcast_context_from_lowered_tag_check_uses_sumtype_metadata() {
	variants := [
		types.Type(types.Struct{
			name: 'ast__Ident'
		}),
		types.Type(types.Struct{
			name: 'ast__BasicLiteral'
		}),
	]
	sum_type := types.Type(types.SumType{
		name:     'ast__Expr'
		variants: variants
	})
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('Expr', sum_type)
	ast_scope.insert('Ident', variants[0])
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('node', sum_type)
	env := &types.Environment{}
	t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		scope:                       fn_scope
		cur_module:                  'checker'
		cached_scopes:               {
			'ast': ast_scope
		}
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
	}
	ctx := t.smartcast_context_from_condition_term(ast.InfixExpr{
		op:  .eq
		lhs: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'node'
			})
			rhs: ast.Ident{
				name: '_tag'
			}
		})
		rhs: ast.Expr(ast.BasicLiteral{
			kind:  .number
			value: '0'
		})
	}) or {
		assert false, 'lowered tag check did not recover smartcast context'
		return
	}
	assert ctx.expr == 'node'
	assert ctx.variant == 'ast__Ident'
	assert ctx.variant_full == 'ast__Ident'
	assert ctx.sumtype == 'ast__Expr'
}

fn test_if_mut_selector_smartcast_rewrites_body_selector() {
	expr_sum_type := types.Type(types.SumType{
		name:     'ast__Expr'
		variants: [
			types.Type(types.Struct{
				name:   'ast__Ident'
				fields: [
					types.Field{
						name: 'language'
						typ:  types.Type(types.Enum{
							name: 'ast__Language'
						})
					},
				]
			}),
			types.Type(types.Struct{
				name: 'ast__BasicLiteral'
			}),
		]
	})
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('Expr', expr_sum_type)
	ast_scope.insert('Ident', types.Type(types.Struct{
		name:   'ast__Ident'
		fields: [
			types.Field{
				name: 'language'
				typ:  types.Type(types.Enum{
					name: 'ast__Language'
				})
			},
		]
	}))
	ast_scope.insert('EnumField', types.Type(types.Struct{
		name:   'ast__EnumField'
		fields: [
			types.Field{
				name: 'expr'
				typ:  expr_sum_type
			},
		]
	}))
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('field', types.Type(types.Pointer{
		base_type: types.Type(types.Struct{
			name: 'ast__EnumField'
		})
	}))
	env := &types.Environment{}
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		scope:                       fn_scope
		cur_module:                  'checker'
		cached_scopes:               {
			'ast': ast_scope
		}
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		synth_types:                 map[int]types.Type{}
		smartcast_expr_counts:       map[string]int{}
	}
	field_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'field'
		})
		rhs: ast.Ident{
			name: 'expr'
		}
	})
	result := t.transform_expr(ast.Expr(ast.IfExpr{
		cond:  ast.Expr(ast.InfixExpr{
			op:  .key_is
			lhs: ast.Expr(ast.ModifierExpr{
				kind: .key_mut
				expr: field_expr
			})
			rhs: ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'ast'
				})
				rhs: ast.Ident{
					name: 'Ident'
				}
			})
		})
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.SelectorExpr{
					lhs: field_expr
					rhs: ast.Ident{
						name: 'language'
					}
				})
			}),
		]
	}))
	assert result is ast.IfExpr
	if_expr := result as ast.IfExpr
	assert if_expr.stmts.len == 1
	assert if_expr.stmts[0] is ast.ExprStmt
	stmt := if_expr.stmts[0] as ast.ExprStmt
	assert stmt.expr is ast.SelectorExpr
	selector := stmt.expr as ast.SelectorExpr
	assert selector.lhs is ast.CastExpr, 'selector lhs was not narrowed: ${selector.lhs.type_name()}'
	cast := selector.lhs as ast.CastExpr
	assert cast.typ.name() == 'ast__Ident*'
}

fn test_for_mut_selector_smartcast_rewrites_assignment_rhs_selector() {
	type_info_type := types.Type(types.SumType{
		name:     'ast__TypeInfo'
		variants: [
			types.Type(types.Struct{
				name:   'ast__Array'
				fields: [
					types.Field{
						name: 'elem_type'
						typ:  types.Type(types.Struct{
							name: 'ast__Type'
						})
					},
				]
			}),
			types.Type(types.Struct{
				name: 'ast__Struct'
			}),
		]
	})
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('TypeInfo', type_info_type)
	ast_scope.insert('Array', types.Type(types.Struct{
		name:   'ast__Array'
		fields: [
			types.Field{
				name: 'elem_type'
				typ:  types.Type(types.Struct{
					name: 'ast__Type'
				})
			},
		]
	}))
	ast_scope.insert('TypeSymbol', types.Type(types.Struct{
		name:   'ast__TypeSymbol'
		fields: [
			types.Field{
				name: 'info'
				typ:  type_info_type
			},
		]
	}))
	type_type := types.Type(types.Struct{
		name: 'ast__Type'
	})
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('elem_sym', types.Type(types.Pointer{
		base_type: types.Type(types.Struct{
			name: 'ast__TypeSymbol'
		})
	}))
	fn_scope.insert('elem_type', type_type)
	env := &types.Environment{}
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		scope:                       fn_scope
		cur_module:                  'ast'
		cached_scopes:               {
			'ast': ast_scope
		}
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		synth_types:                 map[int]types.Type{}
		smartcast_expr_counts:       map[string]int{}
	}
	info_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'elem_sym'
		})
		rhs: ast.Ident{
			name: 'info'
		}
	})
	result := t.transform_stmt(ast.Stmt(ast.ForStmt{
		cond:  ast.Expr(ast.InfixExpr{
			op:  .key_is
			lhs: ast.Expr(ast.ModifierExpr{
				kind: .key_mut
				expr: info_expr
			})
			rhs: ast.Expr(ast.Ident{
				name: 'Array'
			})
		})
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .assign
				lhs: [
					ast.Expr(ast.Ident{
						name: 'elem_type'
					}),
				]
				rhs: [
					ast.Expr(ast.SelectorExpr{
						lhs: info_expr
						rhs: ast.Ident{
							name: 'elem_type'
						}
					}),
				]
			}),
		]
	}))
	assert result is ast.ForStmt
	for_stmt := result as ast.ForStmt
	assert for_stmt.stmts.len == 1
	assert for_stmt.stmts[0] is ast.AssignStmt
	assign_stmt := for_stmt.stmts[0] as ast.AssignStmt
	assert assign_stmt.rhs[0] is ast.SelectorExpr
	selector := assign_stmt.rhs[0] as ast.SelectorExpr
	assert selector.lhs is ast.CastExpr, 'selector lhs was not narrowed: ${selector.lhs.type_name()}'
	cast := selector.lhs as ast.CastExpr
	assert cast.typ.name() == 'ast__Array*'
}

fn test_checked_for_mut_selector_smartcast_rewrites_assignment_rhs_selector() {
	files := transform_code_for_test('
module main

struct Type {}

struct Array {
	elem_type Type
}

struct Struct {}

type TypeInfo = Array | Struct

struct TypeSymbol {
mut:
	info TypeInfo
}

fn f(mut elem_sym TypeSymbol) {
	mut elem_type := Type{}
	for mut elem_sym.info is Array {
		elem_type = elem_sym.info.elem_type
	}
}
')
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'f' {
				for nested in stmt.stmts {
					if nested is ast.ForStmt {
						assert nested.stmts.len == 1
						assert nested.stmts[0] is ast.AssignStmt
						assign_stmt := nested.stmts[0] as ast.AssignStmt
						assert assign_stmt.rhs[0] is ast.SelectorExpr
						selector := assign_stmt.rhs[0] as ast.SelectorExpr
						assert selector.lhs is ast.CastExpr, 'selector lhs was not narrowed: ${selector.lhs.type_name()}'
						cast := selector.lhs as ast.CastExpr
						assert cast.typ.name() == 'Array*'
						return
					}
				}
			}
		}
	}
	assert false, 'function f loop was not found'
}

fn test_label_detection_ignores_empty_ast_slots() {
	assert !transformer_expr_contains_label_stmt(ast.empty_expr)
	assert !transformer_stmt_contains_label_stmt(ast.empty_stmt)
	assert !transformer_expr_contains_label_stmt(ast.IfExpr{
		cond:  ast.BasicLiteral{
			kind:  .key_true
			value: 'true'
		}
		stmts: []
	})
}

fn test_lowered_tag_check_selector_smartcast_rewrites_body_selector() {
	expr_sum_type := types.Type(types.SumType{
		name:     'ast__Expr'
		variants: [
			types.Type(types.Struct{
				name:   'ast__Ident'
				fields: [
					types.Field{
						name: 'language'
						typ:  types.Type(types.Enum{
							name: 'ast__Language'
						})
					},
				]
			}),
			types.Type(types.Struct{
				name: 'ast__BasicLiteral'
			}),
		]
	})
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('Expr', expr_sum_type)
	ast_scope.insert('Ident', types.Type(types.Struct{
		name:   'ast__Ident'
		fields: [
			types.Field{
				name: 'language'
				typ:  types.Type(types.Enum{
					name: 'ast__Language'
				})
			},
		]
	}))
	ast_scope.insert('EnumField', types.Type(types.Struct{
		name:   'ast__EnumField'
		fields: [
			types.Field{
				name: 'expr'
				typ:  expr_sum_type
			},
		]
	}))
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('field', types.Type(types.Pointer{
		base_type: types.Type(types.Struct{
			name: 'ast__EnumField'
		})
	}))
	env := &types.Environment{}
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		scope:                       fn_scope
		cur_module:                  'checker'
		cached_scopes:               {
			'ast': ast_scope
		}
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		synth_types:                 map[int]types.Type{}
		smartcast_expr_counts:       map[string]int{}
	}
	field_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'field'
		})
		rhs: ast.Ident{
			name: 'expr'
		}
	})
	result := t.transform_expr(ast.Expr(ast.IfExpr{
		cond:  ast.Expr(ast.InfixExpr{
			op:  .eq
			lhs: ast.Expr(ast.SelectorExpr{
				lhs: field_expr
				rhs: ast.Ident{
					name: '_tag'
				}
			})
			rhs: ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})
		})
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.SelectorExpr{
					lhs: field_expr
					rhs: ast.Ident{
						name: 'language'
					}
				})
			}),
		]
	}))
	assert result is ast.IfExpr
	if_expr := result as ast.IfExpr
	assert if_expr.stmts.len == 1
	assert if_expr.stmts[0] is ast.ExprStmt
	stmt := if_expr.stmts[0] as ast.ExprStmt
	assert stmt.expr is ast.SelectorExpr
	selector := stmt.expr as ast.SelectorExpr
	assert selector.lhs is ast.CastExpr, 'selector lhs was not narrowed: ${selector.lhs.type_name()}'
	cast := selector.lhs as ast.CastExpr
	assert cast.typ.name() == 'ast__Ident*'
}

fn test_assignment_rhs_call_or_cast_lhs_preserves_nested_smartcast() {
	stmt_sum_type := types.Type(types.SumType{
		name:     'ast__Stmt'
		variants: [
			types.Type(types.Struct{
				name: 'ast__ExprStmt'
			}),
		]
	})
	expr_sum_type := types.Type(types.SumType{
		name:     'ast__Expr'
		variants: [
			types.Type(types.Struct{
				name: 'ast__CallExpr'
			}),
			types.Type(types.Struct{
				name: 'ast__InfixExpr'
			}),
		]
	})
	or_expr_type := types.Type(types.Struct{
		name:   'ast__OrExpr'
		fields: [
			types.Field{
				name: 'scope'
				typ:  types.Type(types.Struct{
					name: 'ast__Scope'
				})
			},
			types.Field{
				name: 'err_used'
				typ:  types.Type(types.bool_)
			},
		]
	})
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('Stmt', stmt_sum_type)
	ast_scope.insert('Expr', expr_sum_type)
	ast_scope.insert('ExprStmt', types.Type(types.Struct{
		name:   'ast__ExprStmt'
		fields: [
			types.Field{
				name: 'expr'
				typ:  expr_sum_type
			},
		]
	}))
	ast_scope.insert('InfixExpr', types.Type(types.Struct{
		name:   'ast__InfixExpr'
		fields: [
			types.Field{
				name: 'or_block'
				typ:  or_expr_type
			},
			types.Field{
				name: 'right'
				typ:  expr_sum_type
			},
		]
	}))
	ast_scope.insert('CallExpr', types.Type(types.Struct{
		name: 'ast__CallExpr'
	}))
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('node', stmt_sum_type)
	env := &types.Environment{}
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		scope:                       fn_scope
		cur_module:                  'checker'
		cached_scopes:               {
			'ast': ast_scope
		}
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		synth_types:                 map[int]types.Type{}
		smartcast_expr_counts:       map[string]int{}
	}
	t.push_smartcast_full('node', 'ast__ExprStmt', 'ast__ExprStmt', 'ast__Stmt')
	t.push_smartcast_full('node.expr', 'ast__InfixExpr', 'ast__InfixExpr', 'ast__Expr')
	t.push_smartcast_full('node.expr.right', 'ast__CallExpr', 'ast__CallExpr', 'ast__Expr')
	node_ident := ast.Expr(ast.Ident{
		name: 'node'
	})
	node_expr := ast.Expr(ast.SelectorExpr{
		lhs: node_ident
		rhs: ast.Ident{
			name: 'expr'
		}
	})
	node_or_block := ast.Expr(ast.SelectorExpr{
		lhs: node_expr
		rhs: ast.Ident{
			name: 'or_block'
		}
	})
	node_scope := ast.Expr(ast.SelectorExpr{
		lhs: node_or_block
		rhs: ast.Ident{
			name: 'scope'
		}
	})
	transformed := t.transform_assign_stmt(ast.AssignStmt{
		op:  .assign
		lhs: [
			ast.Expr(ast.SelectorExpr{
				lhs: node_or_block
				rhs: ast.Ident{
					name: 'err_used'
				}
			}),
		]
		rhs: [
			ast.Expr(ast.CallOrCastExpr{
				lhs:  ast.Expr(ast.SelectorExpr{
					lhs: node_scope
					rhs: ast.Ident{
						name: 'known_var'
					}
				})
				expr: ast.Expr(ast.StringLiteral{
					kind:  .v
					value: "'err'"
				})
			}),
		]
	})
	assert transformed.rhs[0] is ast.CallExpr
	call := transformed.rhs[0] as ast.CallExpr
	assert call.lhs is ast.SelectorExpr
	call_lhs := call.lhs as ast.SelectorExpr
	assert call_lhs.lhs is ast.SelectorExpr
	scope_sel := call_lhs.lhs as ast.SelectorExpr
	assert scope_sel.lhs is ast.SelectorExpr
	or_block_sel := scope_sel.lhs as ast.SelectorExpr
	assert or_block_sel.lhs is ast.CastExpr, 'rhs call lhs lost node.expr smartcast: ${or_block_sel.lhs.type_name()}'
}

fn test_transform_folds_string_literal_concat() {
	mut t := create_test_transformer()
	result := t.transform_expr(ast.InfixExpr{
		op:  .plus
		lhs: ast.Expr(ast.StringLiteral{
			kind:  .v
			value: "'left-'"
		})
		rhs: ast.Expr(ast.StringLiteral{
			kind:  .v
			value: "'right'"
		})
	})

	assert result is ast.StringLiteral, 'expected StringLiteral, got ${result.type_name()}'
	lit := result as ast.StringLiteral
	assert lit.kind == .v
	assert lit.value == 'left-right'
}

fn test_transform_keeps_generic_array_elem_equality_as_infix() {
	mut t := create_transformer_with_vars({
		'a': types.Type(types.Array{
			elem_type: types.Type(types.NamedType('T'))
		})
		'e': types.Type(types.NamedType('T'))
	})
	t.cur_fn_generic_params = ['T']
	t.generic_var_type_params = {
		'a': 'T'
	}
	t.env.set_expr_type(101, types.string_)
	t.env.set_expr_type(102, types.string_)

	result := t.transform_infix_expr(ast.InfixExpr{
		op:  .eq
		lhs: ast.Expr(ast.IndexExpr{
			lhs:  ast.Expr(ast.Ident{
				name: 'a'
			})
			expr: ast.Expr(ast.Ident{
				name: 'idx'
			})
			pos:  token.Pos{
				id: 101
			}
		})
		rhs: ast.Expr(ast.Ident{
			pos:  token.Pos{
				id: 102
			}
			name: 'e'
		})
	})

	assert result is ast.InfixExpr, 'generic equality should stay for specialized codegen'
}

fn test_transform_generic_module_call_uses_specialized_name() {
	mut t := create_test_transformer()
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('json', types.Module{
		name: 'x.json2'
	})
	t.scope = scope

	result := t.transform_expr(ast.CallExpr{
		lhs:  ast.Expr(ast.GenericArgOrIndexExpr{
			lhs:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'json'
				})
				rhs: ast.Ident{
					name: 'decode'
				}
			})
			expr: ast.Expr(ast.Ident{
				name: 'GitHubRepoInfo'
			})
		})
		args: [
			ast.Expr(ast.Ident{
				name: 'body'
			}),
		]
	})

	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'json2__decode_T_GitHubRepoInfo'
	assert call.args.len == 1
}

fn test_transform_generic_module_call_or_cast_uses_specialized_name() {
	mut t := create_test_transformer()
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('json', types.Module{
		name: 'x.json2'
	})
	t.scope = scope

	result := t.transform_expr(ast.CallOrCastExpr{
		lhs:  ast.Expr(ast.GenericArgOrIndexExpr{
			lhs:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'json'
				})
				rhs: ast.Ident{
					name: 'decode'
				}
			})
			expr: ast.Expr(ast.Ident{
				name: 'GitHubRepoInfo'
			})
		})
		expr: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'resp'
			})
			rhs: ast.Ident{
				name: 'body'
			}
		})
	})

	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'json2__decode_T_GitHubRepoInfo'
	assert call.args.len == 1
}

fn test_transform_generic_module_call_or_cast_uses_array_specialized_name() {
	mut t := create_test_transformer()
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('json', types.Module{
		name: 'x.json2'
	})
	t.scope = scope

	result := t.transform_expr(ast.CallOrCastExpr{
		lhs:  ast.Expr(ast.GenericArgOrIndexExpr{
			lhs:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'json'
				})
				rhs: ast.Ident{
					name: 'decode'
				}
			})
			expr: ast.Expr(ast.Type(ast.ArrayType{
				elem_type: ast.Expr(ast.Ident{
					name: 'GitHubContributor'
				})
			}))
		})
		expr: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'resp'
			})
			rhs: ast.Ident{
				name: 'body'
			}
		})
	})

	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'json2__decode_T_Array_GitHubContributor'
	assert call.args.len == 1
}

fn test_transform_nested_module_selector_respects_local_shadow() {
	mut t := create_test_transformer()
	mut checker_scope := types.new_scope(unsafe { nil })
	checker_scope.insert('checker', types.Module{
		name: 'checker'
	})
	mut fn_scope := types.new_scope(checker_scope)
	fn_scope.insert('checker', types.Type(types.Struct{
		name: 'checker__Checker'
	}))
	t.cur_module = 'checker'
	t.scope = fn_scope
	t.cached_scopes = {
		'checker':       checker_scope
		'type_resolver': types.new_scope(unsafe { nil })
	}

	result := t.transform_expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'checker'
			})
			rhs: ast.Ident{
				name: 'type_resolver'
			}
		})
		rhs: ast.Ident{
			name: 'info'
		}
	})

	assert result is ast.SelectorExpr, 'local field chain must not become a module symbol'
	outer := result as ast.SelectorExpr
	assert outer.lhs is ast.SelectorExpr
	inner := outer.lhs as ast.SelectorExpr
	assert inner.lhs is ast.Ident
	assert (inner.lhs as ast.Ident).name == 'checker'
	assert inner.rhs.name == 'type_resolver'
	assert outer.rhs.name == 'info'
}

fn test_expr_to_string_keeps_as_cast_selector_paths_distinct() {
	t := create_test_transformer()
	node_right := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'node'
		})
		rhs: ast.Ident{
			name: 'right'
		}
	})
	ast_as_cast := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'ast'
		})
		rhs: ast.Ident{
			name: 'AsCast'
		}
	})
	ast_par_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'ast'
		})
		rhs: ast.Ident{
			name: 'ParExpr'
		}
	})
	as_cast_payload_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.AsCastExpr{
			expr: ast.Expr(ast.SelectorExpr{
				lhs: node_right
				rhs: ast.Ident{
					name: 'expr'
				}
			})
			typ:  ast_as_cast
		})
		rhs: ast.Ident{
			name: 'expr'
		}
	})
	par_expr_payload_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.AsCastExpr{
			expr: node_right
			typ:  ast_par_expr
		})
		rhs: ast.Ident{
			name: 'expr'
		}
	})

	assert t.expr_to_string(as_cast_payload_expr) == '(node.right.expr as ast__AsCast).expr'
	assert t.expr_to_string(par_expr_payload_expr) == '(node.right as ast__ParExpr).expr'
	assert t.expr_to_string(as_cast_payload_expr) != t.expr_to_string(par_expr_payload_expr)
}

fn test_transform_bare_generic_call_uses_specialized_name() {
	mut t := create_test_transformer()
	result := t.transform_expr(ast.CallExpr{
		lhs:  ast.Expr(ast.GenericArgs{
			lhs:  ast.Expr(ast.Ident{
				name: 'run_new'
			})
			args: [
				ast.Expr(ast.Ident{
					name: 'A'
				}),
				ast.Expr(ast.Ident{
					name: 'X'
				}),
			]
		})
		args: [
			ast.Expr(ast.Ident{
				name: 'app'
			}),
			ast.Expr(ast.Ident{
				name: 'params'
			}),
		]
	})

	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'run_new_T_A_X'
	assert call.args.len == 2
}

fn stmts_have_defer(stmts []ast.Stmt) bool {
	for stmt in stmts {
		match stmt {
			ast.BlockStmt {
				if stmts_have_defer(stmt.stmts) {
					return true
				}
			}
			ast.ComptimeStmt {
				if stmts_have_defer([stmt.stmt]) {
					return true
				}
			}
			ast.DeferStmt {
				return true
			}
			ast.ExprStmt {
				if stmt.expr is ast.IfExpr {
					if stmts_have_defer(stmt.expr.stmts) {
						return true
					}
				}
			}
			ast.ForStmt {
				if stmts_have_defer(stmt.stmts) {
					return true
				}
			}
			else {}
		}
	}
	return false
}

fn test_lower_defer_stmts_lowers_defer_inside_comptime_stmt() {
	mut t := create_test_transformer()
	cleanup_stmt := ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(ast.Ident{
			name: 'cleanup'
		})
	})
	body_stmt := ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(ast.Ident{
			name: 'body'
		})
	})
	lowered := t.lower_defer_stmts([
		ast.Stmt(ast.ComptimeStmt{
			stmt: ast.Stmt(ast.ForStmt{
				stmts: [
					body_stmt,
					ast.Stmt(ast.DeferStmt{
						stmts: [cleanup_stmt]
					}),
				]
			})
		}),
	], false, types.Type(types.void_))

	assert !stmts_have_defer(lowered)
	assert lowered.len == 1
	assert lowered[0] is ast.ComptimeStmt
	comptime_stmt := lowered[0] as ast.ComptimeStmt
	assert comptime_stmt.stmt is ast.ForStmt
	for_stmt := comptime_stmt.stmt as ast.ForStmt
	assert for_stmt.stmts.len == 2
	assert for_stmt.stmts[1] is ast.ExprStmt
	assert (for_stmt.stmts[1] as ast.ExprStmt).expr.name() == 'cleanup'
}

fn test_lower_defer_fn_inside_loop_runs_on_function_exit() {
	mut t := create_test_transformer()
	cleanup_stmt := ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(ast.Ident{
			name: 'cleanup'
		})
	})
	body_stmt := ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(ast.Ident{
			name: 'body'
		})
	})
	lowered := t.lower_defer_stmts([
		ast.Stmt(ast.ForStmt{
			stmts: [
				ast.Stmt(ast.DeferStmt{
					mode:  .function
					stmts: [cleanup_stmt]
				}),
				body_stmt,
			]
		}),
		ast.Stmt(ast.ReturnStmt{
			exprs: [
				ast.Expr(ast.Ident{
					name: 'x'
				}),
			]
		}),
	], true, types.Type(types.int_))

	assert !stmts_have_defer(lowered)
	assert lowered.len == 5
	assert lowered[0] is ast.AssignStmt
	flag_decl := lowered[0] as ast.AssignStmt
	assert flag_decl.op == .decl_assign
	assert flag_decl.lhs[0] is ast.Ident
	flag_name := (flag_decl.lhs[0] as ast.Ident).name

	assert lowered[1] is ast.ForStmt
	for_stmt := lowered[1] as ast.ForStmt
	assert for_stmt.stmts.len == 2
	assert for_stmt.stmts[0] is ast.AssignStmt
	flag_assign := for_stmt.stmts[0] as ast.AssignStmt
	assert flag_assign.op == .assign
	assert flag_assign.lhs[0] is ast.Ident
	assert (flag_assign.lhs[0] as ast.Ident).name == flag_name
	assert for_stmt.stmts[1] is ast.ExprStmt
	assert (for_stmt.stmts[1] as ast.ExprStmt).expr.name() == 'body'

	assert lowered[3] is ast.ExprStmt
	guard_stmt := lowered[3] as ast.ExprStmt
	assert guard_stmt.expr is ast.IfExpr
	guard := guard_stmt.expr as ast.IfExpr
	assert guard.cond is ast.Ident
	assert (guard.cond as ast.Ident).name == flag_name
	assert guard.stmts.len == 1
	assert guard.stmts[0] is ast.ExprStmt
	assert (guard.stmts[0] as ast.ExprStmt).expr.name() == 'cleanup'
}

fn test_defer_return_temp_uses_value_type_for_pointer_expr_returning_value() {
	mut scope := types.new_scope(unsafe { nil })
	mut env := types.Environment.new()
	stmt_type := types.Type(types.SumType{
		name: 'ast__Stmt'
	})
	ptr_stmt_type := types.Type(types.Pointer{
		base_type: stmt_type
	})
	pos := token.Pos{
		id: 99101
	}
	env.set_expr_type(pos.id, ptr_stmt_type)
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		scope:                       scope
		fn_root_scope:               scope
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		local_decl_types:            map[string]types.Type{}
	}
	t.register_defer_return_temp('_defer_t1', ast.Expr(ast.Ident{
		name: 'node'
		pos:  pos
	}), stmt_type, map[string]types.Type{})
	obj := scope.lookup_parent('_defer_t1', 0) or { panic('missing defer temp') }
	assert obj.typ().name() == 'ast__Stmt'
}

fn test_defer_return_temp_uses_sum_return_type_for_smartcast_variant() {
	temp_type := defer_return_temp_type(types.Type(types.Struct{
		name: 'ast__ForStmt'
	}), types.Type(types.SumType{
		name:     'ast__Stmt'
		variants: [
			types.Type(types.Struct{
				name: 'ast__ForCStmt'
			}),
			types.Type(types.Struct{
				name: 'ast__ForInStmt'
			}),
			types.Type(types.Struct{
				name: 'ast__ForStmt'
			}),
		]
	}))
	assert temp_type.name() == 'ast__Stmt'
}

fn test_transform_persists_defer_return_temp_scope_for_codegen() {
	env, _ := transform_code_with_env_for_test('
type Stmt = EmptyStmt | ForInStmt

struct EmptyStmt {}

struct ForInStmt {
	val int
}

struct G {
mut:
	values map[string]int
}

fn cleanup() {}

fn (mut g G) stmt(node Stmt) Stmt {
	if node is ForInStmt {
		defer(fn) {
			cleanup()
		}
		mut new_node := ForInStmt{
			...node
		}
		return Stmt(new_node)
	}
	return node
}
')
	fn_scope := env.get_fn_scope('main', 'G__stmt') or { panic('missing transformed fn scope') }
	mut found_defer_temp := false
	for name, obj in fn_scope.objects {
		if !name.starts_with('_defer_t') {
			continue
		}
		found_defer_temp = true
		assert obj.typ() !is types.Pointer
		assert obj.typ().name().ends_with('Stmt')
	}
	assert found_defer_temp
}

fn test_scoped_defer_return_err_temp_uses_ierror_type() {
	mut scope := types.new_scope(unsafe { nil })
	mut t := create_test_transformer()
	t.scope = scope
	t.fn_root_scope = scope
	t.set_synth_pos_counter(-1)
	err_type := types.Type(types.Struct{
		name: 'IError'
	})
	t.register_defer_return_temp('_defer_t_err', ast.Expr(ast.Ident{
		name: 'err'
	}), types.Type(types.ResultType{
		base_type: types.Type(types.SumType{
			name: 'ast__Expr'
		})
	}), {
		'err': err_type
	})
	mut found_ierror_temp := false
	mut temp_types := []string{}
	for name, obj in scope.objects {
		if !name.starts_with('_defer_t') {
			continue
		}
		temp_types << '${name}:${obj.typ().name()}'
		if obj.typ().name() == 'IError' {
			found_ierror_temp = true
		}
	}
	assert found_ierror_temp, temp_types.str()
}

fn test_defer_decl_tracking_prefers_rhs_type_for_err_decl() {
	mut scope := types.new_scope(unsafe { nil })
	scope.insert_or_update('err', types.Type(types.Struct{
		name: 'ast__NodeError'
	}))
	mut t := create_test_transformer()
	t.scope = scope
	t.set_synth_pos_counter(-1)
	t.synth_types = map[int]types.Type{}
	or_tmp := ast.Expr(ast.Ident{
		name: '_or_t1'
	})
	mut decls := map[string]types.Type{}
	t.add_decl_types_from_stmt(mut decls, ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'err'
		})]
		rhs: [
			t.synth_selector(or_tmp, 'err', types.Type(types.Struct{
				name: 'IError'
			})),
		]
	}))
	err_type := decls['err'] or {
		assert false, 'err declaration type was not tracked'
		return
	}
	assert err_type.name() == 'IError'
}

fn test_transform_string_inter_smartcast_temp_uses_variant_type() {
	env, _ := transform_code_with_env_for_test('
type Value = int | string

struct Entry {
	key Value
}

fn entry_string(entry Entry) string {
	key_text := if entry.key is string {
		"\${entry.key}"
	} else {
		"other"
	}
	return key_text
}
')
	fn_scope := env.get_fn_scope('main', 'entry_string') or {
		panic('missing transformed fn scope')
	}
	mut found_string_temp := false
	for name, obj in fn_scope.objects {
		if !name.starts_with('_or_t') {
			continue
		}
		if obj.typ().name() == 'string' {
			found_string_temp = true
		}
		assert obj.typ().name() != 'Value'
	}
	assert found_string_temp
}

fn test_lower_defer_fn_captures_block_local_values() {
	mut t := create_transformer_with_vars({
		'old': types.Type(types.bool_)
	})
	old_decl := ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'old'
		})]
		rhs: [ast.Expr(ast.BasicLiteral{
			kind:  .key_false
			value: 'false'
		})]
	})
	defer_stmt := ast.Stmt(ast.DeferStmt{
		mode:  .function
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .assign
				lhs: [
					ast.Expr(ast.SelectorExpr{
						lhs: ast.Ident{
							name: 'g'
						}
						rhs: ast.Ident{
							name: 'inside_smartcast'
						}
					}),
				]
				rhs: [
					ast.Expr(ast.Ident{
						name: 'old'
					}),
				]
			}),
		]
	})
	lowered := t.lower_defer_stmts([
		ast.Stmt(ast.BlockStmt{
			stmts: [old_decl, defer_stmt]
		}),
	], false, types.Type(types.void_))

	assert lowered.len == 4
	assert lowered[1] is ast.AssignStmt
	capture_decl := lowered[1] as ast.AssignStmt
	assert capture_decl.lhs[0] is ast.Ident
	capture_name := (capture_decl.lhs[0] as ast.Ident).name
	assert capture_name.starts_with('_defer_cap')

	assert lowered[2] is ast.BlockStmt
	block_stmt := lowered[2] as ast.BlockStmt
	assert block_stmt.stmts.len == 3
	assert block_stmt.stmts[1] is ast.AssignStmt
	capture_assign := block_stmt.stmts[1] as ast.AssignStmt
	assert capture_assign.lhs[0] is ast.Ident
	assert (capture_assign.lhs[0] as ast.Ident).name == capture_name

	assert lowered[3] is ast.ExprStmt
	guard := (lowered[3] as ast.ExprStmt).expr as ast.IfExpr
	restore := guard.stmts[0] as ast.AssignStmt
	assert restore.rhs[0] is ast.Ident
	assert (restore.rhs[0] as ast.Ident).name == capture_name
}

fn test_lower_defer_fn_captures_block_local_values_in_string_interpolation() {
	mut t := create_transformer_with_vars({
		'old': types.Type(types.string_)
	})
	old_decl := ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'old'
		})]
		rhs: [ast.Expr(ast.StringLiteral{
			kind:  .v
			value: 'old'
		})]
	})
	defer_stmt := ast.Stmt(ast.DeferStmt{
		mode:  .function
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.CallExpr{
					lhs:  ast.Ident{
						name: 'cleanup'
					}
					args: [
						ast.Expr(ast.StringInterLiteral{
							kind:   .v
							values: ['value=', '']
							inters: [ast.StringInter{
								expr: ast.Expr(ast.Ident{
									name: 'old'
								})
							}]
						}),
					]
				})
			}),
		]
	})
	lowered := t.lower_defer_stmts([
		ast.Stmt(ast.BlockStmt{
			stmts: [old_decl, defer_stmt]
		}),
	], false, types.Type(types.void_))

	assert lowered.len == 4
	capture_decl := lowered[1] as ast.AssignStmt
	capture_name := (capture_decl.lhs[0] as ast.Ident).name
	assert capture_name.starts_with('_defer_cap')

	block_stmt := lowered[2] as ast.BlockStmt
	capture_assign := block_stmt.stmts[1] as ast.AssignStmt
	assert (capture_assign.lhs[0] as ast.Ident).name == capture_name

	guard := (lowered[3] as ast.ExprStmt).expr as ast.IfExpr
	cleanup := (guard.stmts[0] as ast.ExprStmt).expr as ast.CallExpr
	inter_lit := cleanup.args[0] as ast.StringInterLiteral
	assert inter_lit.inters[0].expr is ast.Ident
	assert (inter_lit.inters[0].expr as ast.Ident).name == capture_name
}

fn test_lower_defer_fn_captures_parent_block_values_from_nested_if() {
	mut t := create_test_transformer()
	old_decl := ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'old'
		})]
		rhs: [ast.Expr(ast.BasicLiteral{
			kind:  .key_false
			value: 'false'
		})]
	})
	defer_stmt := ast.Stmt(ast.DeferStmt{
		mode:  .function
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .assign
				lhs: [
					ast.Expr(ast.SelectorExpr{
						lhs: ast.Ident{
							name: 'g'
						}
						rhs: ast.Ident{
							name: 'inside_smartcast'
						}
					}),
				]
				rhs: [
					ast.Expr(ast.Ident{
						name: 'old'
					}),
				]
			}),
		]
	})
	lowered := t.lower_defer_stmts([
		old_decl,
		ast.Stmt(ast.ExprStmt{
			expr: ast.Expr(ast.IfExpr{
				cond:  ast.Expr(ast.BasicLiteral{
					kind:  .key_true
					value: 'true'
				})
				stmts: [defer_stmt]
			})
		}),
	], false, types.Type(types.void_))

	assert lowered.len == 5
	capture_decl := lowered[1] as ast.AssignStmt
	capture_name := (capture_decl.lhs[0] as ast.Ident).name
	assert capture_name.starts_with('_defer_cap')

	if_stmt := lowered[3] as ast.ExprStmt
	if_expr := if_stmt.expr as ast.IfExpr
	assert if_expr.stmts.len == 2
	capture_assign := if_expr.stmts[0] as ast.AssignStmt
	assert (capture_assign.lhs[0] as ast.Ident).name == capture_name

	guard := (lowered[4] as ast.ExprStmt).expr as ast.IfExpr
	restore := guard.stmts[0] as ast.AssignStmt
	assert restore.rhs[0] is ast.Ident
	assert (restore.rhs[0] as ast.Ident).name == capture_name
}

fn test_transform_defer_fn_in_range_for_captures_loop_block_literals() {
	files := transform_code_for_test("
fn cleanup(s string) {}

fn main() {
	for i in 0 .. 2 {
		mut str_add_rhs_tmp := ''
		mut str_add_rhs_needs_free := false
		if i == 1 {
			str_add_rhs_tmp = 'x'
			str_add_rhs_needs_free = true
			defer(fn) {
				if str_add_rhs_needs_free {
					cleanup('\${str_add_rhs_tmp}')
				}
			}
		}
	}
}
")
	mut main_fn := ast.FnDecl{}
	mut found_main := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'main' {
				main_fn = stmt
				found_main = true
			}
		}
	}
	assert found_main
	mut capture_decl_count := 0
	for stmt in main_fn.stmts {
		if stmt is ast.AssignStmt && stmt.op == .decl_assign && stmt.lhs.len > 0
			&& stmt.lhs[0] is ast.Ident && (stmt.lhs[0] as ast.Ident).name.starts_with('_defer_cap') {
			capture_decl_count++
		}
	}
	assert capture_decl_count == 2
}

fn test_transform_lowers_static_method_struct_shorthand_args() {
	files := transform_code_for_test('
struct Walker {
	table int
	count int
}

fn Walker.new(params Walker) &Walker {
	_ = params
	return &Walker{}
}

fn main() {
	table := 1
	_ = Walker.new(table: table, count: 2)
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'main' {
				for body_stmt in stmt.stmts {
					if body_stmt is ast.AssignStmt && body_stmt.rhs.len == 1
						&& body_stmt.rhs[0] is ast.CallExpr {
						call := body_stmt.rhs[0] as ast.CallExpr
						if call.lhs is ast.Ident && (call.lhs as ast.Ident).name == 'Walker__new' {
							found = true
							assert call.args.len == 1
							assert call.args[0] is ast.InitExpr
						}
					}
				}
			}
		}
	}
	assert found
}

fn test_explicit_as_cast_survives_smartcasted_selector_call_expr() {
	files := transform_code_for_test('
type Expr = ArrayDecompose | Ident

struct ArrayDecompose {}

struct Ident {}

struct CallArg {
	expr Expr
}

fn last(args []CallArg) CallArg {
	return args[0]
}

fn f(args []CallArg) {
	if last(args).expr is ArrayDecompose {
		array_decompose := last(args).expr as ArrayDecompose
		_ = array_decompose
	}
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'f' {
				for body_stmt in stmt.stmts {
					if body_stmt is ast.ExprStmt && body_stmt.expr is ast.IfExpr {
						if_expr := body_stmt.expr as ast.IfExpr
						for inner in if_expr.stmts {
							if inner is ast.AssignStmt && inner.op == .decl_assign
								&& inner.lhs.len == 1 && inner.lhs[0] is ast.Ident
								&& (inner.lhs[0] as ast.Ident).name == 'array_decompose' {
								found = true
								assert inner.rhs.len == 1
								assert inner.rhs[0] is ast.AsCastExpr
							}
						}
					}
				}
			}
		}
	}
	assert found
}

fn test_transform_lowers_index_parsed_generic_method_call() {
	files := transform_code_for_test('
struct File {}

struct PoolProcessor {}

fn (p &PoolProcessor) get_item[T](idx int) T {
	_ = p
	_ = idx
	return T(0)
}

fn main() {
	p := &PoolProcessor{}
	file := p.get_item[&File](0)
	_ = file
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'main' {
				for body_stmt in stmt.stmts {
					if body_stmt is ast.AssignStmt && body_stmt.rhs.len == 1
						&& body_stmt.rhs[0] is ast.CallExpr {
						call := body_stmt.rhs[0] as ast.CallExpr
						if call.lhs is ast.Ident
							&& (call.lhs as ast.Ident).name == 'PoolProcessor__get_item_T_Fileptr' {
							found = true
							assert call.args.len == 2
							assert call.args[0].name() == 'p'
							assert call.args[1].name() == '0'
						}
					}
				}
			}
		}
	}
	assert found
}

// Create a rune-like type that returns 'rune' from name()
fn rune_type() types.Type {
	return types.Alias{
		name: 'rune'
	}
}

fn test_transform_comptime_embed_file_call_or_cast_expr_to_init_expr() {
	raw_dir := os.join_path(os.temp_dir(), 'v2_transformer_embed_file_${os.getpid()}')
	os.mkdir_all(raw_dir) or { panic(err) }
	tmp_dir := os.real_path(raw_dir)
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'main.v')
	asset_path := os.join_path(tmp_dir, 'asset.txt')
	os.write_file(source_path, 'fn main() {}') or { panic(err) }
	os.write_file(asset_path, 'hello') or { panic(err) }

	mut t := create_test_transformer()
	t.cur_file_name = source_path

	result := t.transform_comptime_expr(ast.ComptimeExpr{
		expr: ast.Expr(ast.CallOrCastExpr{
			lhs:  ast.Expr(ast.Ident{
				name: 'embed_file'
			})
			expr: ast.Expr(ast.StringLiteral{
				kind:  .v
				value: "'asset.txt'"
			})
		})
	})

	assert t.needed_embed_file_helper
	assert result is ast.InitExpr, 'expected InitExpr, got ${result.type_name()}'
	init := result as ast.InitExpr
	assert init.typ is ast.Ident
	assert (init.typ as ast.Ident).name == embed_file_helper_type_name
	assert init.fields.len == 4
	assert init.fields[0].name == '_data'
	assert init.fields[0].value is ast.StringLiteral
	assert (init.fields[0].value as ast.StringLiteral).value == "'hello'"
	assert init.fields[1].name == 'len'
	assert init.fields[1].value is ast.BasicLiteral
	assert (init.fields[1].value as ast.BasicLiteral).value == '5'
	assert init.fields[2].name == 'path'
	assert init.fields[3].name == 'apath'
	assert init.fields[3].value is ast.StringLiteral
	assert (init.fields[3].value as ast.StringLiteral).value == quote_v_string_literal(asset_path)
}

fn test_transform_comptime_embed_file_chained_method_call() {
	raw_dir := os.join_path(os.temp_dir(), 'v2_transformer_embed_file_chain_${os.getpid()}')
	os.mkdir_all(raw_dir) or { panic(err) }
	tmp_dir := os.real_path(raw_dir)
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'main.v')
	asset_path := os.join_path(tmp_dir, 'asset.txt')
	os.write_file(source_path, 'fn main() {}') or { panic(err) }
	os.write_file(asset_path, 'hello') or { panic(err) }

	mut t := create_test_transformer()
	t.cur_file_name = source_path

	result := t.transform_comptime_expr(ast.ComptimeExpr{
		expr: ast.Expr(ast.CallExpr{
			lhs:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.CallOrCastExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'embed_file'
					})
					expr: ast.Expr(ast.StringLiteral{
						kind:  .v
						value: "'asset.txt'"
					})
				})
				rhs: ast.Ident{
					name: 'to_bytes'
				}
			})
			args: []
		})
	})

	assert t.needed_embed_file_helper
	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.SelectorExpr
	sel := call.lhs as ast.SelectorExpr
	assert sel.lhs is ast.InitExpr
	init := sel.lhs as ast.InitExpr
	assert init.typ is ast.Ident
	assert (init.typ as ast.Ident).name == embed_file_helper_type_name
	assert sel.rhs.name == 'to_bytes'
	assert init.fields.len == 4
	assert init.fields[0].value is ast.StringLiteral
	assert (init.fields[0].value as ast.StringLiteral).value == "'hello'"
	assert init.fields[3].value is ast.StringLiteral
	assert (init.fields[3].value as ast.StringLiteral).value == quote_v_string_literal(asset_path)
}

fn test_inject_embed_file_helper_adds_builtin_helper_once() {
	mut t := create_test_transformer()
	mut files := [
		ast.File{
			mod:   'builtin'
			name:  'builtin.v'
			stmts: [
				ast.Stmt(ast.StructDecl{
					name: 'Existing'
				}),
			]
		},
		ast.File{
			mod:  'main'
			name: 'main.v'
		},
	]

	t.inject_embed_file_helper(mut files)
	t.inject_embed_file_helper(mut files)

	assert files[0].stmts.len == 7
	assert files[0].stmts[1] is ast.StructDecl
	assert (files[0].stmts[1] as ast.StructDecl).name == embed_file_helper_type_name
	mut helper_count := 0
	for stmt in files[0].stmts {
		if stmt is ast.StructDecl && stmt.name == embed_file_helper_type_name {
			helper_count++
		}
	}
	assert helper_count == 1
}

fn test_transform_ident_vmodroot_to_string_literal() {
	mut t := create_test_transformer()
	t.comptime_vmodroot = '/tmp/v'
	result := t.transform_expr(ast.Ident{
		name: '@VMODROOT'
	})
	assert result is ast.StringLiteral, 'expected StringLiteral, got ${result.type_name()}'
	lit := result as ast.StringLiteral
	assert lit.kind == .v
	assert lit.value == "'/tmp/v'"
}

fn test_transform_ident_vmodroot_empty_root() {
	mut t := create_test_transformer()
	t.comptime_vmodroot = ''
	result := t.transform_expr(ast.Ident{
		name: '@VMODROOT'
	})
	assert result is ast.StringLiteral, 'expected StringLiteral, got ${result.type_name()}'
	lit := result as ast.StringLiteral
	assert lit.kind == .v
	assert lit.value == "''"
}

fn test_transform_autogenerates_clone_helpers_for_iclone_structs() {
	files := transform_code_for_test('
interface IClone {}

struct Inner implements IClone {
	name string
}

struct Outer implements IClone {
	inner Inner
	nums []int
}

fn use_clone(value Outer) Outer {
	return value.clone()
}
')
	assert files.len == 1
	file := files[0]
	mut saw_inner_clone := false
	mut saw_outer_clone := false
	mut saw_lowered_call := false
	for stmt in file.stmts {
		if stmt is ast.FnDecl && stmt.name == 'use_clone' {
			assert stmt.stmts.len == 1
			ret := stmt.stmts[0] as ast.ReturnStmt
			assert ret.exprs.len == 1
			assert ret.exprs[0] is ast.CallExpr
			call := ret.exprs[0] as ast.CallExpr
			assert call.lhs is ast.Ident
			assert (call.lhs as ast.Ident).name == 'Outer__clone'
			saw_lowered_call = true
		}
		if stmt is ast.FnDecl && stmt.name == 'Inner__clone' {
			saw_inner_clone = true
		}
		if stmt is ast.FnDecl && stmt.name == 'Outer__clone' {
			saw_outer_clone = true
			assert stmt.stmts.len == 1
			ret := stmt.stmts[0] as ast.ReturnStmt
			assert ret.exprs.len == 1
			assert ret.exprs[0] is ast.InitExpr
			init := ret.exprs[0] as ast.InitExpr
			assert init.fields.len == 2
			assert init.fields[0].name == 'inner'
			assert init.fields[0].value is ast.CallExpr
			inner_call := init.fields[0].value as ast.CallExpr
			assert inner_call.lhs is ast.Ident
			assert (inner_call.lhs as ast.Ident).name == 'Inner__clone'
			assert init.fields[1].name == 'nums'
			assert init.fields[1].value is ast.CallExpr
			nums_call := init.fields[1].value as ast.CallExpr
			assert nums_call.lhs is ast.Ident
			assert (nums_call.lhs as ast.Ident).name == 'array__clone'
		}
	}
	assert saw_lowered_call
	assert saw_inner_clone
	assert saw_outer_clone
}

fn test_array_comparison_eq() {
	// Set up variable types so get_array_type_str can detect them
	mut t := create_transformer_with_vars({
		'arr1': types.Type(types.Array{ elem_type: types.int_ })
		'arr2': types.Type(types.Array{
			elem_type: types.int_
		})
	})

	// Create: arr1 == arr2
	expr := ast.InfixExpr{
		op:  .eq
		lhs: ast.Ident{
			name: 'arr1'
		}
		rhs: ast.Ident{
			name: 'arr2'
		}
	}

	result := t.transform_infix_expr(expr)

	// Should be transformed to: array__eq(arr1, arr2)
	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	call_name := (call.lhs as ast.Ident).name
	assert call_name == 'array__eq', 'expected array__eq, got ${call_name}'
	assert call.args.len == 2
}

fn test_array_comparison_ne() {
	mut t := create_transformer_with_vars({
		'arr1': types.Type(types.Array{ elem_type: types.int_ })
		'arr2': types.Type(types.Array{
			elem_type: types.int_
		})
	})

	// Create: arr1 != arr2
	expr := ast.InfixExpr{
		op:  .ne
		lhs: ast.Ident{
			name: 'arr1'
		}
		rhs: ast.Ident{
			name: 'arr2'
		}
	}

	result := t.transform_infix_expr(expr)

	// Should be transformed to: !array__eq(arr1, arr2)
	assert result is ast.PrefixExpr, 'expected PrefixExpr, got ${result.type_name()}'
	prefix := result as ast.PrefixExpr
	assert prefix.op == .not
	assert prefix.expr is ast.CallExpr
	call := prefix.expr as ast.CallExpr
	call_name := (call.lhs as ast.Ident).name
	assert call_name == 'array__eq', 'expected array__eq, got ${call_name}'
}

fn test_array_comparison_non_array_passthrough() {
	// Variables with non-array types
	mut t := create_transformer_with_vars({
		'x': types.Type(types.int_)
		'y': types.Type(types.int_)
	})

	// Create: x == y (non-array comparison)
	expr := ast.InfixExpr{
		op:  .eq
		lhs: ast.Ident{
			name: 'x'
		}
		rhs: ast.Ident{
			name: 'y'
		}
	}

	result := t.transform_infix_expr(expr)

	// Should remain as InfixExpr (not transformed)
	assert result is ast.InfixExpr, 'expected InfixExpr for non-array comparison'
}

fn test_transform_index_expr_string_slice_lowered() {
	mut t := create_transformer_with_vars({
		's': types.Type(string_type())
	})

	expr := ast.IndexExpr{
		lhs:  ast.Ident{
			name: 's'
		}
		expr: ast.RangeExpr{
			op:    .dotdot
			start: ast.BasicLiteral{
				kind:  .number
				value: '1'
			}
			end:   ast.BasicLiteral{
				kind:  .number
				value: '3'
			}
		}
	}

	result := t.transform_expr(expr)
	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'string__substr'
	assert call.args.len == 3
}

fn test_transform_index_expr_nested_selector_string_slice_lowered() {
	scanner_type := types.Type(types.Struct{
		name:   'scanner__Scanner'
		fields: [
			types.Field{
				name: 'src'
				typ:  types.Type(string_type())
			},
		]
	})
	parser_type := types.Type(types.Struct{
		name:   'parser__Parser'
		fields: [
			types.Field{
				name: 'scanner'
				typ:  types.Type(types.Pointer{
					base_type: scanner_type
				})
			},
		]
	})
	mut t := create_transformer_with_vars({
		'p': types.Type(types.Pointer{
			base_type: parser_type
		})
	})

	expr := ast.IndexExpr{
		lhs:  ast.SelectorExpr{
			lhs: ast.SelectorExpr{
				lhs: ast.Ident{
					name: 'p'
				}
				rhs: ast.Ident{
					name: 'scanner'
				}
			}
			rhs: ast.Ident{
				name: 'src'
			}
		}
		expr: ast.RangeExpr{
			op:    .dotdot
			start: ast.BasicLiteral{
				kind:  .number
				value: '1'
			}
			end:   ast.BasicLiteral{
				kind:  .number
				value: '3'
			}
		}
	}

	result := t.transform_expr(expr)
	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'string__substr'
	assert call.args.len == 3
}

fn test_transform_index_expr_imported_nested_selector_string_slice_lowered() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'v2/scanner/scanner.v'
			code: 'module scanner

pub struct Scanner {
pub mut:
	src string
	pos int
}
'
		},
		TestSource{
			rel:  'v2/parser/parser.v'
			code: 'module parser

import v2.scanner

pub struct Parser {
mut:
	scanner &scanner.Scanner
}

fn (mut p Parser) directive() string {
	start := 0
	end := p.scanner.src.len
	return p.scanner.src[start..end]
}
'
		},
	])
	mut found_directive := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'directive' {
				found_directive = true
				assert find_call_with_lhs_suffix_in_stmts(stmt.stmts, 'string__substr') != none
				assert find_call_with_lhs_suffix_in_stmts(stmt.stmts, 'array__slice') == none
			}
		}
	}
	assert found_directive
}

fn test_transform_selector_expr_on_nested_selector_string_slice_lowers_slice_lhs() {
	scanner_type := types.Type(types.Struct{
		name:   'scanner__Scanner'
		fields: [
			types.Field{
				name: 'src'
				typ:  types.Type(string_type())
			},
		]
	})
	parser_type := types.Type(types.Struct{
		name:   'parser__Parser'
		fields: [
			types.Field{
				name: 'scanner'
				typ:  types.Type(types.Pointer{
					base_type: scanner_type
				})
			},
		]
	})
	mut t := create_transformer_with_vars({
		'p': types.Type(types.Pointer{
			base_type: parser_type
		})
	})

	expr := ast.SelectorExpr{
		lhs: ast.IndexExpr{
			lhs:  ast.SelectorExpr{
				lhs: ast.SelectorExpr{
					lhs: ast.Ident{
						name: 'p'
					}
					rhs: ast.Ident{
						name: 'scanner'
					}
				}
				rhs: ast.Ident{
					name: 'src'
				}
			}
			expr: ast.RangeExpr{
				op:    .dotdot
				start: ast.BasicLiteral{
					kind:  .number
					value: '1'
				}
				end:   ast.BasicLiteral{
					kind:  .number
					value: '3'
				}
			}
		}
		rhs: ast.Ident{
			name: 'trim_space'
		}
	}

	result := t.transform_expr(expr)
	assert result is ast.SelectorExpr, 'expected SelectorExpr, got ${result.type_name()}'
	sel := result as ast.SelectorExpr
	assert sel.lhs is ast.CallExpr, 'expected transformed slice CallExpr, got ${sel.lhs.type_name()}'
	call := sel.lhs as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'string__substr'
}

fn test_transform_selector_expr_string_slice_uses_fn_root_scope_fallback() {
	full_scanner_type := types.Type(types.Struct{
		name:   'scanner__Scanner'
		fields: [
			types.Field{
				name: 'src'
				typ:  types.Type(string_type())
			},
		]
	})
	stale_scanner_type := types.Type(types.Struct{
		name: 'scanner__Scanner'
	})
	parser_type := types.Type(types.Struct{
		name:   'parser__Parser'
		fields: [
			types.Field{
				name: 'scanner'
				typ:  types.Type(types.Pointer{
					base_type: stale_scanner_type
				})
			},
		]
	})
	mut root_scope := types.new_scope(unsafe { nil })
	root_scope.insert('p', types.Type(types.Pointer{
		base_type: parser_type
	}))
	mut scanner_scope := types.new_scope(unsafe { nil })
	scanner_scope.insert('Scanner', types.TypeObject{
		typ: full_scanner_type
	})
	mut t := create_test_transformer()
	t.fn_root_scope = root_scope
	t.scope = types.new_scope(unsafe { nil })
	t.cached_scopes = {
		'scanner': scanner_scope
	}

	expr := ast.SelectorExpr{
		lhs: ast.IndexExpr{
			lhs:  ast.SelectorExpr{
				lhs: ast.SelectorExpr{
					lhs: ast.Ident{
						name: 'p'
					}
					rhs: ast.Ident{
						name: 'scanner'
					}
				}
				rhs: ast.Ident{
					name: 'src'
				}
			}
			expr: ast.RangeExpr{
				op:    .dotdot
				start: ast.BasicLiteral{
					kind:  .number
					value: '1'
				}
				end:   ast.BasicLiteral{
					kind:  .number
					value: '3'
				}
			}
		}
		rhs: ast.Ident{
			name: 'trim_space'
		}
	}

	result := t.transform_expr(expr)
	assert result is ast.SelectorExpr, 'expected SelectorExpr, got ${result.type_name()}'
	sel := result as ast.SelectorExpr
	assert sel.lhs is ast.CallExpr, 'expected transformed slice CallExpr, got ${sel.lhs.type_name()}'
	call := sel.lhs as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'string__substr'
}

fn test_transform_index_expr_string_call_slice_open_ended_uses_max_int() {
	mut env := &types.Environment{}
	env.set_expr_type(1, types.string_)
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
	}

	expr := ast.IndexExpr{
		lhs:  ast.CallExpr{
			lhs: ast.Ident{
				name: 'get_type'
			}
			pos: token.Pos{
				id: 1
			}
		}
		expr: ast.RangeExpr{
			op:    .dotdot
			start: ast.BasicLiteral{
				kind:  .number
				value: '2'
			}
			end:   ast.empty_expr
		}
	}

	result := t.transform_expr(expr)
	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'string__substr'
	assert call.args.len == 3
	assert call.args[0] is ast.CallExpr
	assert call.args[2] is ast.BasicLiteral
	end := call.args[2] as ast.BasicLiteral
	assert end.kind == .number
	assert end.value == '2147483647'
}

fn test_transform_index_expr_array_slice_lowered() {
	mut t := create_transformer_with_vars({
		'arr': types.Type(types.Array{ elem_type: types.int_ })
	})

	expr := ast.IndexExpr{
		lhs:  ast.Ident{
			name: 'arr'
		}
		expr: ast.RangeExpr{
			op:    .ellipsis
			start: ast.BasicLiteral{
				kind:  .number
				value: '0'
			}
			end:   ast.BasicLiteral{
				kind:  .number
				value: '4'
			}
		}
	}

	result := t.transform_expr(expr)
	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'array__slice'
	assert call.args.len == 3
	// Inclusive range `...` should become end + 1.
	assert call.args[2] is ast.InfixExpr
}

fn test_transform_index_expr_open_ended_array_slice_uses_slice_ni() {
	mut t := create_transformer_with_vars({
		'arr': types.Type(types.Array{ elem_type: types.string_ })
	})

	expr := ast.IndexExpr{
		lhs:  ast.Ident{
			name: 'arr'
		}
		expr: ast.RangeExpr{
			op:    .dotdot
			start: ast.BasicLiteral{
				kind:  .number
				value: '1'
			}
			end:   ast.empty_expr
		}
	}

	result := t.transform_expr(expr)
	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'array__slice_ni'
	assert call.args.len == 3
	assert call.args[2] is ast.SelectorExpr
}

fn test_transform_call_or_cast_expr_array_contains_fixed_array() {
	mut t := create_transformer_with_vars({
		'a': types.Type(types.ArrayFixed{
			len:       3
			elem_type: types.int_
		})
	})
	expr := ast.CallOrCastExpr{
		lhs:  ast.SelectorExpr{
			lhs: ast.Ident{
				name: 'a'
			}
			rhs: ast.Ident{
				name: 'contains'
			}
		}
		expr: ast.BasicLiteral{
			kind:  .number
			value: '2'
		}
	}
	result := t.transform_call_or_cast_expr(expr)
	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'Array_fixed_int_3_contains'
	assert call.args.len == 2
	assert call.args[0] is ast.Ident
	assert (call.args[0] as ast.Ident).name == 'a'
	assert 'Array_fixed_int_3_contains' in t.needed_array_contains_fns
}

fn test_transform_prefers_declared_array_receiver_contains_method() {
	files := transform_code_for_test('
struct Attr {
	name string
}

fn (attrs []Attr) contains(str string) bool {
	_ = attrs
	_ = str
	return true
}

fn has_typedef(attrs []Attr) bool {
	return attrs.contains("typedef")
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'has_typedef' {
				assert stmt.stmts.len == 1
				assert stmt.stmts[0] is ast.ReturnStmt
				ret := stmt.stmts[0] as ast.ReturnStmt
				assert ret.exprs.len == 1
				assert ret.exprs[0] is ast.CallExpr
				call := ret.exprs[0] as ast.CallExpr
				assert call.lhs is ast.Ident
				assert (call.lhs as ast.Ident).name == 'Array_Attr__contains'
				assert call.args.len == 2
				found = true
			}
		}
	}
	assert found
}

fn test_transform_call_expr_array_contains_fixed_array() {
	mut t := create_transformer_with_vars({
		'a': types.Type(types.ArrayFixed{
			len:       3
			elem_type: types.int_
		})
	})
	expr := ast.CallExpr{
		lhs:  ast.Ident{
			name: 'array__contains'
		}
		args: [
			ast.Expr(ast.Ident{
				name: 'a'
			}),
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: ast.BasicLiteral{
					kind:  .number
					value: '2'
				}
			}),
		]
	}
	result := t.transform_call_expr(expr)
	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'Array_fixed_int_3_contains'
	assert call.args.len == 2
	assert call.args[1] is ast.BasicLiteral
	assert 'Array_fixed_int_3_contains' in t.needed_array_contains_fns
}

fn test_generate_array_method_elem_expr_registers_elem_type() {
	mut t := create_test_transformer()
	elem_expr := t.generate_array_method_elem_expr(ArrayMethodInfo{
		array_type: 'Array_string'
		elem_type:  'string'
	}, ast.Expr(ast.Ident{
		name: 'i'
	}))
	assert elem_expr is ast.IndexExpr
	assert elem_expr.pos().id < 0
	elem_type := t.synth_types[elem_expr.pos().id] or {
		assert false, 'expected synthesized element type for array helper index expr'
		return
	}
	assert t.type_to_c_name(elem_type) == 'string'
}

fn test_resolve_expr_with_expected_type_resolves_enum_shorthand() {
	mut t := create_test_transformer()
	resolved := t.resolve_expr_with_expected_type(ast.Expr(ast.SelectorExpr{
		lhs: ast.empty_expr
		rhs: ast.Ident{
			name: 'v'
		}
	}), types.Type(types.Enum{
		name: 'ast__StringLiteralKind'
	}))
	assert resolved is ast.Ident
	assert (resolved as ast.Ident).name == 'ast__StringLiteralKind__v'
}

fn test_transform_selector_enum_uses_declared_parent_owner() {
	enum_typ := types.Type(types.Enum{
		name: 'StrIntpType'
	})
	mut builtin_scope := types.new_scope(unsafe { nil })
	builtin_scope.insert('StrIntpType', enum_typ)
	mut c_scope := types.new_scope(builtin_scope)
	mut t := create_test_transformer()
	t.cur_module = 'c'
	t.cached_scopes = {
		'builtin': builtin_scope
		'c':       c_scope
	}
	result := t.transform_expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'StrIntpType'
		})
		rhs: ast.Ident{
			name: 'si_no_str'
		}
	})
	assert result is ast.Ident
	assert (result as ast.Ident).name == 'StrIntpType__si_no_str'
}

fn test_resolve_enum_shorthand_uses_declared_parent_owner() {
	enum_typ := types.Type(types.Enum{
		name: 'ChanState'
	})
	mut builtin_scope := types.new_scope(unsafe { nil })
	builtin_scope.insert('ChanState', enum_typ)
	mut sync_scope := types.new_scope(builtin_scope)
	mut t := create_test_transformer()
	t.cur_module = 'sync'
	t.cached_scopes = {
		'builtin': builtin_scope
		'sync':    sync_scope
	}
	resolved := t.resolve_enum_shorthand(ast.Expr(ast.SelectorExpr{
		lhs: ast.empty_expr
		rhs: ast.Ident{
			name: 'closed'
		}
	}), 'sync__ChanState')
	assert resolved is ast.Ident
	assert (resolved as ast.Ident).name == 'ChanState__closed'
}

fn test_transform_selector_enum_keeps_direct_module_owner() {
	enum_typ := types.Type(types.Enum{
		name: 'Token'
	})
	mut token_scope := types.new_scope(unsafe { nil })
	token_scope.insert('Token', enum_typ)
	mut t := create_test_transformer()
	t.cur_module = 'token'
	t.cached_scopes = {
		'token': token_scope
	}
	result := t.transform_expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'Token'
		})
		rhs: ast.Ident{
			name: 'name'
		}
	})
	assert result is ast.Ident
	assert (result as ast.Ident).name == 'token__Token__name'
}

fn test_transform_init_expr_resolves_imported_enum_shorthand() {
	env := &types.Environment{}
	mut ast_scope := types.new_scope(unsafe { nil })
	enum_typ := types.Type(types.Enum{
		name: 'ast__StringLiteralKind'
	})
	ast_scope.insert('StringLiteralKind', enum_typ)
	ast_scope.insert('StringLiteral', types.Type(types.Struct{
		name:   'ast__StringLiteral'
		fields: [
			types.Field{
				name: 'kind'
				typ:  enum_typ
			},
			types.Field{
				name: 'value'
				typ:  types.string_
			},
		]
	}))
	lock env.scopes {
		env.scopes['ast'] = ast_scope
	}
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		cur_module:                  'main'
		cached_scopes:               {
			'ast': ast_scope
		}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
	}
	result := t.transform_init_expr(ast.InitExpr{
		typ:    ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'ast'
			})
			rhs: ast.Ident{
				name: 'StringLiteral'
			}
		})
		fields: [
			ast.FieldInit{
				name:  'kind'
				value: ast.Expr(ast.SelectorExpr{
					lhs: ast.empty_expr
					rhs: ast.Ident{
						name: 'v'
					}
				})
			},
		]
	})
	assert result is ast.InitExpr
	init := result as ast.InitExpr
	mut found_kind := false
	for field in init.fields {
		if field.name == 'kind' {
			found_kind = true
			assert field.value is ast.Ident
			assert (field.value as ast.Ident).name == 'ast__StringLiteralKind__v'
			break
		}
	}
	assert found_kind
}

fn test_transform_map_init_expr_non_empty_lowers_to_runtime_ctor() {
	mut t := create_test_transformer()

	expr := ast.MapInitExpr{
		keys: [
			ast.Expr(ast.StringLiteral{
				value: 'foo'
			}),
			ast.Expr(ast.StringLiteral{
				value: 'bar'
			}),
		]
		vals: [
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '1'
			}),
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '2'
			}),
		]
	}

	result := t.transform_map_init_expr(expr)

	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'new_map_init_noscan_value'
	assert call.args.len == 9, 'expected 9 args for map constructor, got ${call.args.len}'
	assert call.args[7] is ast.ArrayInitExpr, 'expected key array arg'
	assert call.args[8] is ast.ArrayInitExpr, 'expected value array arg'
}

fn test_transform_map_init_expr_empty_lowers_to_new_map() {
	mut t := create_test_transformer()

	expr := ast.MapInitExpr{
		typ: ast.Expr(ast.Type(ast.MapType{
			key_type:   ast.Ident{
				name: 'string'
			}
			value_type: ast.Ident{
				name: 'int'
			}
		}))
	}

	result := t.transform_map_init_expr(expr)
	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'new_map'
	assert call.args.len == 6
	assert call.args[2] is ast.PrefixExpr
	assert (call.args[2] as ast.PrefixExpr).op == .amp
	assert (call.args[2] as ast.PrefixExpr).expr is ast.Ident
	assert ((call.args[2] as ast.PrefixExpr).expr as ast.Ident).name == 'map_hash_string'
}

fn test_transform_struct_init_pointer_field_keeps_pointer_value() {
	files := transform_code_for_test('
type BytePtr = &u8

struct Dense {
	keys BytePtr = unsafe { nil }
}

fn make_ptr() BytePtr {
	return unsafe { nil }
}

fn new_dense() Dense {
	return Dense{
		keys: make_ptr()
	}
}
')
	mut found_field := false
	for stmt in files[0].stmts {
		if stmt is ast.FnDecl && stmt.name == 'new_dense' {
			for inner in stmt.stmts {
				if inner is ast.ReturnStmt && inner.exprs.len == 1 && inner.exprs[0] is ast.InitExpr {
					init := inner.exprs[0] as ast.InitExpr
					for field in init.fields {
						if field.name == 'keys' {
							found_field = true
							assert field.value !is ast.PrefixExpr
						}
					}
				}
			}
		}
	}
	assert found_field
}

fn test_transform_index_expr_map_read_lowers_to_map_get() {
	mut t := create_transformer_with_vars({
		'm': types.Type(types.Map{
			key_type:   string_type()
			value_type: types.int_
		})
	})

	expr := ast.IndexExpr{
		lhs:  ast.Ident{
			name: 'm'
		}
		expr: ast.StringLiteral{
			kind:  .v
			value: "'foo'"
		}
	}

	result := t.transform_index_expr(expr)
	assert result is ast.UnsafeExpr, 'expected UnsafeExpr, got ${result.type_name()}'
	unsafe_expr := result as ast.UnsafeExpr
	assert unsafe_expr.stmts.len > 0
	last := unsafe_expr.stmts[unsafe_expr.stmts.len - 1]
	assert last is ast.ExprStmt
	last_expr := (last as ast.ExprStmt).expr
	assert last_expr is ast.ParenExpr
	paren := last_expr as ast.ParenExpr
	assert paren.expr is ast.PrefixExpr
	pref := paren.expr as ast.PrefixExpr
	assert pref.op == .mul
	assert pref.expr is ast.CastExpr
	cast := pref.expr as ast.CastExpr
	assert cast.expr is ast.CallExpr
	call := cast.expr as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'map__get'
	assert call.args.len == 3
}

fn test_transform_map_index_push_lowers_to_map_get_and_set() {
	mut t := create_transformer_with_vars({
		'lists': types.Type(types.Map{
			key_type:   types.int_
			value_type: types.Type(types.Array{
				elem_type: types.int_
			})
		})
	})

	result := t.try_transform_map_index_push(ast.ExprStmt{
		expr: ast.InfixExpr{
			op:  .left_shift
			lhs: ast.IndexExpr{
				lhs:  ast.Ident{
					name: 'lists'
				}
				expr: ast.BasicLiteral{
					kind:  .number
					value: '2'
				}
			}
			rhs: ast.BasicLiteral{
				kind:  .number
				value: '1'
			}
		}
	}) or {
		assert false, 'expected map index push to be transformed'
		return
	}

	assert result is ast.ExprStmt
	expr_stmt := result as ast.ExprStmt
	assert expr_stmt.expr is ast.CallExpr
	push_call := expr_stmt.expr as ast.CallExpr
	assert push_call.lhs is ast.Ident
	assert (push_call.lhs as ast.Ident).name == 'array__push_noscan'
	assert push_call.args.len == 2
	assert push_call.args[0] is ast.CastExpr
	arr_ptr := push_call.args[0] as ast.CastExpr
	assert arr_ptr.expr is ast.CallExpr
	map_call := arr_ptr.expr as ast.CallExpr
	assert map_call.lhs is ast.Ident
	assert (map_call.lhs as ast.Ident).name == 'map__get_and_set'
	assert map_call.args.len == 3
}

fn test_transform_map_index_selector_postfix_lowers_to_map_get_and_set() {
	fn_type := types.Type(types.Struct{
		name:   'Fn'
		fields: [
			types.Field{
				name: 'usages'
				typ:  types.int_
			},
		]
	})
	mut t := create_transformer_with_vars({
		'fns': types.Type(types.Map{
			key_type:   string_type()
			value_type: fn_type
		})
	})

	result := t.try_transform_map_index_postfix(ast.ExprStmt{
		expr: ast.PostfixExpr{
			op:   .inc
			expr: ast.SelectorExpr{
				lhs: ast.Expr(ast.IndexExpr{
					lhs:  ast.Ident{
						name: 'fns'
					}
					expr: ast.StringLiteral{
						kind:  .v
						value: "'main'"
					}
				})
				rhs: ast.Ident{
					name: 'usages'
				}
			}
		}
	}) or {
		assert false, 'expected map index selector postfix to be transformed'
		return
	}

	assert result is ast.AssignStmt
	assign_stmt := result as ast.AssignStmt
	assert assign_stmt.op == .plus_assign
	assert assign_stmt.lhs.len == 1
	assert assign_stmt.lhs[0] is ast.SelectorExpr
	lhs := assign_stmt.lhs[0] as ast.SelectorExpr
	assert lhs.rhs.name == 'usages'
	mut call_names := []string{}
	collect_call_names_from_expr(lhs.lhs, mut call_names)
	assert 'map__get_and_set' in call_names
}

fn test_transform_map_index_assign_uses_temp_for_const_key() {
	files := transform_code_for_test('
const int_type_idx = 8

fn set_print_type(mut print_types map[int]bool) {
	print_types[int_type_idx] = true
}
')
	call := find_call_with_lhs_suffix_in_stmts(files[0].stmts, 'map__set') or {
		assert false, 'expected map__set call'
		return
	}
	assert call.args.len == 3
	assert call.args[1] is ast.CastExpr
	key_cast := call.args[1] as ast.CastExpr
	assert key_cast.expr is ast.PrefixExpr
	key_addr := key_cast.expr as ast.PrefixExpr
	assert key_addr.expr is ast.Ident
	assert (key_addr.expr as ast.Ident).name != 'int_type_idx'
}

fn test_transform_map_index_assign_uses_temp_for_module_const_key() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'ast/types.v'
			code: 'module ast
pub const int_type_idx = 8
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module main
import ast

fn set_print_type(mut print_types map[int]bool) {
	print_types[ast.int_type_idx] = true
}
'
		},
	])
	mut found := false
	for file in files {
		if file.mod != 'main' {
			continue
		}
		call := find_call_with_lhs_suffix_in_stmts(file.stmts, 'map__set') or { continue }
		assert call.args.len == 3
		assert call.args[1] is ast.CastExpr
		key_cast := call.args[1] as ast.CastExpr
		assert key_cast.expr is ast.PrefixExpr
		key_addr := key_cast.expr as ast.PrefixExpr
		assert key_addr.expr is ast.Ident
		assert (key_addr.expr as ast.Ident).name != 'ast__int_type_idx'
		found = true
	}
	assert found
}

fn test_addr_of_prefix_temp_materializes_selector_from_call_result() {
	mut t := create_test_transformer()
	mut prefix_stmts := []ast.Stmt{}
	addr := t.addr_of_with_prefix_temp(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.CallExpr{
			lhs: ast.Ident{
				name: 'pos'
			}
		})
		rhs: ast.Ident{
			name: 'pos'
		}
	}), types.int_, mut prefix_stmts)
	assert prefix_stmts.len == 1
	assert prefix_stmts[0] is ast.AssignStmt
	tmp_decl := prefix_stmts[0] as ast.AssignStmt
	assert tmp_decl.lhs[0] is ast.Ident
	assert addr is ast.PrefixExpr
	addr_expr := addr as ast.PrefixExpr
	assert addr_expr.expr is ast.Ident
	assert (addr_expr.expr as ast.Ident).name == (tmp_decl.lhs[0] as ast.Ident).name
}

fn test_transform_map_index_assign_with_or_rhs_lowers_to_map_set() {
	files := transform_code_for_test('
fn maybe_int() ?int {
	return 1
}

fn set_value(mut values map[string]int) {
	values["c"] = maybe_int() or { 0 }
}
')
	call := find_call_with_lhs_suffix_in_stmts(files[0].stmts, 'map__set') or {
		assert false, 'expected map__set call'
		return
	}
	assert call.args.len == 3
}

fn test_transform_map_index_push_resolves_named_array_alias_value() {
	mut t := create_transformer_with_vars({
		'cleanups': types.Type(types.Map{
			key_type:   types.string_
			value_type: types.Type(types.NamedType('strings.Builder'))
		})
		'cleanup':  types.Type(types.NamedType('strings.Builder'))
	})
	mut strings_scope := types.new_scope(unsafe { nil })
	strings_scope.insert_type('Builder', types.Type(types.Alias{
		name:      'strings.Builder'
		base_type: types.Type(types.Array{
			elem_type: types.int_
		})
	}))
	t.cached_scopes = {
		'strings': strings_scope
	}

	result := t.try_transform_map_index_push(ast.ExprStmt{
		expr: ast.InfixExpr{
			op:  .left_shift
			lhs: ast.IndexExpr{
				lhs:  ast.Ident{
					name: 'cleanups'
				}
				expr: ast.StringLiteral{
					kind:  .v
					value: "'main'"
				}
			}
			rhs: ast.Ident{
				name: 'cleanup'
			}
		}
	}) or {
		assert false, 'expected named alias map index push to be transformed'
		return
	}

	assert result is ast.ExprStmt
	expr_stmt := result as ast.ExprStmt
	assert expr_stmt.expr is ast.CallExpr
	push_call := expr_stmt.expr as ast.CallExpr
	assert push_call.lhs is ast.Ident
	assert (push_call.lhs as ast.Ident).name == 'array__push_many'
	assert push_call.args[0] is ast.CastExpr
	arr_ptr := push_call.args[0] as ast.CastExpr
	assert arr_ptr.expr is ast.CallExpr
	map_call := arr_ptr.expr as ast.CallExpr
	assert map_call.lhs is ast.Ident
	assert (map_call.lhs as ast.Ident).name == 'map__get_and_set'
}

fn test_transform_array_append_selector_array_field_uses_push_many() {
	ast_type := types.Type(types.NamedType('ast.Type'))
	array_ast_type := types.Type(types.Array{
		elem_type: ast_type
	})
	mut t := create_transformer_with_vars({
		'smartcasts': array_ast_type
		'field':      types.Type(types.Pointer{
			base_type: types.Type(types.Struct{
				name:   'ast.ScopeStructField'
				fields: [
					types.Field{
						name: 'smartcasts'
						typ:  array_ast_type
					},
				]
			})
		})
	})

	result := t.transform_expr(ast.InfixExpr{
		op:  .left_shift
		lhs: ast.Ident{
			name: 'smartcasts'
		}
		rhs: ast.SelectorExpr{
			lhs: ast.Ident{
				name: 'field'
			}
			rhs: ast.Ident{
				name: 'smartcasts'
			}
		}
	})

	assert result is ast.CallExpr
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'array__push_many'
	assert call.args.len == 3
}

fn test_transform_array_append_after_array_decl_uses_declared_array_type() {
	array_int_type := types.Type(types.Array{
		elem_type: types.int_
	})
	mut t := create_transformer_with_vars({
		'field': types.Type(types.Pointer{
			base_type: types.Type(types.Struct{
				name:   'Field'
				fields: [
					types.Field{
						name: 'values'
						typ:  array_int_type
					},
				]
			})
		})
	})
	_ = t.transform_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'values'
		})]
		rhs: [
			ast.Expr(ast.ArrayInitExpr{
				typ: ast.Expr(ast.Type(ast.ArrayType{
					elem_type: ast.Ident{
						name: 'int'
					}
				}))
			}),
		]
	})

	result := t.transform_expr(ast.InfixExpr{
		op:  .left_shift
		lhs: ast.Ident{
			name: 'values'
		}
		rhs: ast.SelectorExpr{
			lhs: ast.Ident{
				name: 'field'
			}
			rhs: ast.Ident{
				name: 'values'
			}
		}
	})

	assert result is ast.CallExpr
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'array__push_many'
}

fn test_transform_array_alias_mut_receiver_append_uses_receiver_pointer() {
	files := transform_code_for_test('
type Builder = []u8

fn (mut b Builder) add_zero() {
	b << u8(0)
}

fn main() {
	mut b := Builder([]u8{})
	b.add_zero()
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'add_zero' {
				assert stmt.stmts.len == 1
				assert stmt.stmts[0] is ast.ExprStmt
				expr_stmt := stmt.stmts[0] as ast.ExprStmt
				assert expr_stmt.expr is ast.CallExpr
				call := expr_stmt.expr as ast.CallExpr
				assert call.lhs is ast.Ident
				assert (call.lhs as ast.Ident).name == 'builtin__array_push_noscan'
				assert call.args.len == 2
				assert call.args[0] is ast.CastExpr
				arr_ptr := call.args[0] as ast.CastExpr
				assert arr_ptr.expr is ast.Ident
				assert (arr_ptr.expr as ast.Ident).name == 'b'
				found = true
			}
		}
	}
	assert found
}

fn test_array_elem_type_uses_checker_ident_metadata() {
	env := types.Environment.new()
	mut scope := types.new_scope(unsafe { nil })
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         env
		scope:                       scope
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
	}
	mut env_mut := unsafe { env }
	env_mut.set_expr_type(42, types.Type(types.Array{
		elem_type: types.Type(types.NamedType('ast.Type'))
	}))

	elem_type := t.get_array_elem_type_str(ast.Expr(ast.Ident{
		name: 'smartcasts'
		pos:  token.Pos{
			id: 42
		}
	})) or {
		assert false, 'expected array element type from checker metadata'
		return
	}
	assert elem_type == 'ast__Type'
}

fn test_transform_array_append_preserves_alias_element_type_for_push_many() {
	files := transform_code_for_test('
type Type = u32

struct ScopeStructField {
	smartcasts []Type
}

fn f(field &ScopeStructField) {
	mut smartcasts := []Type{}
	smartcasts << field.smartcasts
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'f' {
				assert stmt.stmts.len == 2
				assert stmt.stmts[1] is ast.ExprStmt
				expr_stmt := stmt.stmts[1] as ast.ExprStmt
				assert expr_stmt.expr is ast.CallExpr
				call := expr_stmt.expr as ast.CallExpr
				assert call.lhs is ast.Ident
				assert (call.lhs as ast.Ident).name == 'array__push_many'
				found = true
			}
		}
	}
	assert found
}

fn test_transform_nested_array_append_literal_pushes_single_array_value() {
	files := transform_code_for_test('
fn f(column_name string) {
	mut groups := [][]string{}
	groups << [column_name]
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'f' {
				assert stmt.stmts.len == 2
				assert stmt.stmts[1] is ast.ExprStmt
				expr_stmt := stmt.stmts[1] as ast.ExprStmt
				assert expr_stmt.expr is ast.CallExpr
				call := expr_stmt.expr as ast.CallExpr
				assert call.lhs is ast.Ident
				assert (call.lhs as ast.Ident).name == 'builtin__array_push_noscan'
				assert call.args.len == 2
				assert call.args[1] is ast.ArrayInitExpr
				outer_arr := call.args[1] as ast.ArrayInitExpr
				assert outer_arr.typ is ast.Type
				assert outer_arr.typ as ast.Type is ast.ArrayType
				outer_type := outer_arr.typ as ast.Type
				outer_array_type := outer_type as ast.ArrayType
				assert outer_array_type.elem_type is ast.Ident
				assert (outer_array_type.elem_type as ast.Ident).name == 'Array_string'
				assert outer_arr.exprs.len == 1
				assert outer_arr.exprs[0] is ast.CallExpr
				inner_call := outer_arr.exprs[0] as ast.CallExpr
				assert inner_call.lhs is ast.Ident
				assert (inner_call.lhs as ast.Ident).name == 'builtin__new_array_from_c_array_noscan'
				assert inner_call.args.len == 4
				assert inner_call.args[2] is ast.KeywordOperator
				sizeof_arg := inner_call.args[2] as ast.KeywordOperator
				assert sizeof_arg.exprs.len == 1
				assert sizeof_arg.exprs[0] is ast.Ident
				assert (sizeof_arg.exprs[0] as ast.Ident).name == 'string'
				assert inner_call.args[3] is ast.ArrayInitExpr
				inner_arr := inner_call.args[3] as ast.ArrayInitExpr
				assert inner_arr.typ is ast.Type
				assert inner_arr.typ as ast.Type is ast.ArrayType
				inner_type := inner_arr.typ as ast.Type
				inner_array_type := inner_type as ast.ArrayType
				assert inner_array_type.elem_type is ast.Ident
				assert (inner_array_type.elem_type as ast.Ident).name == 'string'
				found = true
			}
		}
	}
	assert found
}

fn test_transform_selector_field_index_method_receiver_stays_index_expr() {
	files := transform_code_for_test('
struct EmbeddedFile {
	bytes []u8
}

fn (b u8) hex() string {
	return "x"
}

fn f(emfile EmbeddedFile) string {
	return emfile.bytes[0].hex()
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'f' {
				call := find_call_with_lhs_suffix_in_stmts(stmt.stmts, 'u8__hex') or {
					assert false, 'missing transformed u8.hex call'
					return
				}
				assert call.args.len == 1
				assert call.args[0] is ast.IndexExpr, 'method receiver should stay an index expression, got ${call.args[0].type_name()}'
				found = true
			}
		}
	}
	assert found
}

fn test_map_index_or_assign_uses_selector_field_map_type() {
	map_type := types.Type(types.Map{
		key_type:   string_type()
		value_type: string_type()
	})
	handler_type := types.Type(types.Struct{
		name:   'Handler'
		fields: [
			types.Field{
				name: 'files'
				typ:  map_type
			},
		]
	})
	mut module_scope := types.new_scope(unsafe { nil })
	module_scope.insert('Handler', handler_type)
	mut t := create_transformer_with_vars({
		'handler': handler_type
	})
	t.cur_module = 'main'
	t.cached_scopes = {
		'main': module_scope
	}

	or_expr := ast.OrExpr{
		expr:  ast.IndexExpr{
			lhs:  ast.SelectorExpr{
				lhs: ast.Ident{
					name: 'handler'
				}
				rhs: ast.Ident{
					name: 'files'
				}
			}
			expr: ast.StringLiteral{
				kind:  .v
				value: "'index.html'"
			}
		}
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.StringLiteral{
						kind:  .v
						value: "''"
					}),
				]
			}),
		]
	}
	result := t.try_expand_map_index_or_assign(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'file'
		})]
		rhs: [ast.Expr(or_expr)]
	}, or_expr) or {
		assert false, 'expected selector-field map or assignment to be expanded'
		return
	}

	assert result.len == 3
	temp_assign := result[0]
	assert temp_assign is ast.AssignStmt
	temp_rhs := (temp_assign as ast.AssignStmt).rhs[0]
	assert temp_rhs is ast.CallExpr
	temp_call := temp_rhs as ast.CallExpr
	assert temp_call.lhs is ast.Ident
	assert (temp_call.lhs as ast.Ident).name == 'map__get_check'
}

fn test_transform_init_expr_empty_typed_map_lowers_to_new_map() {
	mut t := create_test_transformer()

	expr := ast.InitExpr{
		typ: ast.Expr(ast.Type(ast.MapType{
			key_type:   ast.Ident{
				name: 'string'
			}
			value_type: ast.Ident{
				name: 'int'
			}
		}))
	}

	result := t.transform_init_expr(expr)
	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'new_map'
	assert call.args.len == 6
}

fn test_transform_string_index_or_assign_expands_to_bounds_check() {
	mut t := create_transformer_with_vars({
		's': types.Type(types.string_)
		'i': types.Type(types.int_)
	})
	or_expr := ast.OrExpr{
		expr:  ast.IndexExpr{
			lhs:  ast.Ident{
				name: 's'
			}
			expr: ast.Ident{
				name: 'i'
			}
		}
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.BasicLiteral{
						kind:  .number
						value: '0'
					}),
				]
			}),
		]
	}
	result := t.try_expand_array_index_or_assign(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'ch'
		})]
		rhs: [ast.Expr(or_expr)]
	}, or_expr) or {
		assert false, 'expected string index or assignment to be expanded'
		return
	}
	assert result.len == 2
	assert result[0] is ast.AssignStmt
	if_stmt := (result[1] as ast.ExprStmt).expr as ast.IfExpr
	assert if_stmt.cond is ast.InfixExpr
	cond := if_stmt.cond as ast.InfixExpr
	assert cond.op == .lt
	assert cond.rhs is ast.SelectorExpr
	assert (cond.rhs as ast.SelectorExpr).rhs.name == 'len'
}

fn test_array_index_or_assign_with_void_fallback_does_not_assign_void() {
	mut t := create_transformer_with_vars({
		'items': types.Type(types.Array{
			elem_type: types.string_
		})
		'i':     types.Type(types.int_)
		'path':  types.Type(types.string_)
	})
	or_expr := ast.OrExpr{
		expr:  ast.IndexExpr{
			lhs:  ast.Ident{
				name: 'items'
			}
			expr: ast.Ident{
				name: 'i'
			}
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.CallExpr{
					lhs: ast.Ident{
						name: 'eprintln_exit'
					}
				}
			}),
		]
	}
	result := t.try_expand_array_index_or_assign(ast.AssignStmt{
		op:  .assign
		lhs: [ast.Expr(ast.Ident{
			name: 'path'
		})]
		rhs: [ast.Expr(or_expr)]
	}, or_expr) or {
		assert false, 'expected array index or assignment to be expanded'
		return
	}
	if_stmt := (result[0] as ast.ExprStmt).expr as ast.IfExpr
	else_if := if_stmt.else_expr as ast.IfExpr
	assert else_if.stmts.len == 1
	assert else_if.stmts[0] is ast.ExprStmt
	assert else_if.stmts[0] !is ast.AssignStmt
}

fn test_is_string_expr_string_literal() {
	mut t := create_test_transformer()

	expr := ast.StringLiteral{
		value: 'hello'
	}

	assert t.is_string_expr(expr), 'StringLiteral should be detected as string'
}

fn test_is_string_expr_basic_literal_string() {
	mut t := create_test_transformer()

	expr := ast.BasicLiteral{
		value: 'hello'
		kind:  .string
	}

	assert t.is_string_expr(expr), 'BasicLiteral with .string kind should be detected as string'
}

fn test_is_string_expr_cast_to_string() {
	mut t := create_test_transformer()

	// Create: (string){...}
	expr := ast.CastExpr{
		typ:  ast.Ident{
			name: 'string'
		}
		expr: ast.BasicLiteral{
			value: 'test'
			kind:  .string
		}
	}

	assert t.is_string_expr(expr), 'CastExpr to string should be detected as string'
}

fn test_is_string_expr_method_call() {
	mut t := create_test_transformer()

	// Create: s.to_upper()
	expr := ast.CallExpr{
		lhs:  ast.SelectorExpr{
			lhs: ast.Ident{
				name: 's'
			}
			rhs: ast.Ident{
				name: 'to_upper'
			}
		}
		args: []
	}

	assert t.is_string_expr(expr), 'method call to_upper() should be detected as string'
}

fn test_is_string_returning_method() {
	t := create_test_transformer()

	// Test various string-returning methods
	assert t.is_string_returning_method('str')
	assert t.is_string_returning_method('string')
	assert t.is_string_returning_method('to_upper')
	assert t.is_string_returning_method('to_lower')
	assert t.is_string_returning_method('substr')
	assert t.is_string_returning_method('hex')
	assert t.is_string_returning_method('join')

	// Non-string methods
	assert !t.is_string_returning_method('len')
	assert !t.is_string_returning_method('push')
}

// --- OrExpr expansion tests ---

fn test_expand_single_or_expr_defaults_to_result() {
	// When type lookup fails (empty environment), expand_single_or_expr
	// should default to Result expansion instead of returning OrExpr unchanged.
	mut t := create_test_transformer()

	// Build: some_call() or { 0 }
	or_expr := ast.OrExpr{
		expr:  ast.CallExpr{
			lhs:  ast.Ident{
				name: 'some_call'
			}
			args: []
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.BasicLiteral{
					kind:  .number
					value: '0'
				}
			}),
		]
	}

	mut prefix_stmts := []ast.Stmt{}
	result := t.expand_single_or_expr(or_expr, mut prefix_stmts)

	// Should generate prefix statements (temp assign + if-check)
	assert prefix_stmts.len == 2, 'expected 2 prefix stmts (assign + if), got ${prefix_stmts.len}'

	// First prefix stmt: _or_t1 := some_call()
	assert prefix_stmts[0] is ast.AssignStmt
	assign := prefix_stmts[0] as ast.AssignStmt
	assert assign.op == .decl_assign
	assert assign.lhs.len == 1
	temp_ident := assign.lhs[0]
	assert temp_ident is ast.Ident
	temp_name := (temp_ident as ast.Ident).name
	assert temp_name.starts_with('_or_t'), 'expected temp name starting with _or_t, got ${temp_name}'

	// Second prefix stmt: if _or_t1.is_error { ... } (Result pattern, not Option pattern)
	assert prefix_stmts[1] is ast.ExprStmt
	if_stmt := (prefix_stmts[1] as ast.ExprStmt).expr
	assert if_stmt is ast.IfExpr
	if_expr := if_stmt as ast.IfExpr
	// For Result: condition is _or_t1.is_error (SelectorExpr)
	assert if_expr.cond is ast.SelectorExpr, 'expected SelectorExpr (Result pattern), got ${if_expr.cond.type_name()}'
	sel := if_expr.cond as ast.SelectorExpr
	assert sel.rhs.name == 'is_error', 'expected is_error selector for Result, got ${sel.rhs.name}'

	// base_type is unknown (empty env), defaults to 'int' => not void => returns data access
	assert result is ast.SelectorExpr, 'expected SelectorExpr (.data access) for default base type, got ${result.type_name()}'
}

fn test_expand_single_or_expr_with_return_in_or_block() {
	// Or-block with return statement (control flow pattern):
	// some_call() or { return }
	mut t := create_test_transformer()

	or_expr := ast.OrExpr{
		expr:  ast.CallExpr{
			lhs:  ast.Ident{
				name: 'some_call'
			}
			args: []
		}
		stmts: [ast.Stmt(ast.FlowControlStmt{
			op: .key_return
		})]
	}

	mut prefix_stmts := []ast.Stmt{}
	result := t.expand_single_or_expr(or_expr, mut prefix_stmts)

	// Should still generate prefix statements
	assert prefix_stmts.len == 2, 'expected 2 prefix stmts, got ${prefix_stmts.len}'

	// The if-block body should contain only the return statement (err not used, so no err assign)
	if_stmt := (prefix_stmts[1] as ast.ExprStmt).expr as ast.IfExpr
	assert if_stmt.stmts.len == 2, 'expected 2 stmt in if body (return only, err not used), got ${if_stmt.stmts.len}'

	// base_type is unknown => defaults to 'int' => not void => returns data access
	assert result is ast.SelectorExpr, 'expected SelectorExpr (.data access) for default base type'
}

fn test_transform_expr_or_expr_wraps_in_unsafe() {
	// transform_expr with OrExpr should wrap in UnsafeExpr (compound expression)
	mut t := create_test_transformer()

	or_expr := ast.OrExpr{
		expr:  ast.CallExpr{
			lhs:  ast.Ident{
				name: 'get_value'
			}
			args: []
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.BasicLiteral{
					kind:  .number
					value: '42'
				}
			}),
		]
	}

	result := t.transform_expr(or_expr)

	// Should be wrapped in UnsafeExpr (GCC compound expression)
	assert result is ast.UnsafeExpr, 'expected UnsafeExpr wrapper, got ${result.type_name()}'
	unsafe_expr := result as ast.UnsafeExpr
	// Stmts: temp assign, if-check, and the result ExprStmt
	assert unsafe_expr.stmts.len == 3, 'expected 3 stmts in UnsafeExpr, got ${unsafe_expr.stmts.len}'

	// First stmt: temp assign
	assert unsafe_expr.stmts[0] is ast.AssignStmt

	// Second stmt: if-check with is_error (Result pattern)
	assert unsafe_expr.stmts[1] is ast.ExprStmt
	if_check := (unsafe_expr.stmts[1] as ast.ExprStmt).expr
	assert if_check is ast.IfExpr

	// Last stmt should be ExprStmt with the result expression
	last := unsafe_expr.stmts[2]
	assert last is ast.ExprStmt
}

fn test_get_or_block_stmts_and_value_ignores_trailing_empty_stmt() {
	mut t := create_test_transformer()

	stmts := [
		ast.Stmt(ast.ExprStmt{
			expr: ast.Tuple{
				exprs: [
					ast.Expr(ast.BasicLiteral{
						kind:  .number
						value: '1'
					}),
					ast.Expr(ast.BasicLiteral{
						kind:  .number
						value: '2'
					}),
				]
			}
		}),
		ast.empty_stmt,
	]

	side_effects, value := t.get_or_block_stmts_and_value(stmts)

	assert side_effects.len == 0
	assert value is ast.Tuple
	assert (value as ast.Tuple).exprs.len == 2
}

// --- IfGuardExpr expansion tests ---

fn test_transform_expr_if_guard_standalone_evaluates_rhs() {
	// Standalone IfGuardExpr in transform_expr should just evaluate RHS
	mut t := create_test_transformer()

	guard := ast.IfGuardExpr{
		stmt: ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(ast.Ident{
				name: 'x'
			})]
			rhs: [
				ast.Expr(ast.CallExpr{
					lhs:  ast.Ident{
						name: 'some_func'
					}
					args: []
				}),
			]
		}
	}

	result := t.transform_expr(guard)

	// Should evaluate to the RHS (the call expression)
	assert result is ast.CallExpr, 'expected CallExpr, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'some_func'
}

fn test_transform_expr_if_guard_empty_rhs() {
	// IfGuardExpr with empty RHS should pass through
	mut t := create_test_transformer()

	guard := ast.IfGuardExpr{
		stmt: ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(ast.Ident{
				name: 'x'
			})]
			rhs: []
		}
	}

	result := t.transform_expr(guard)

	// With empty RHS, should return the guard as-is
	assert result is ast.IfGuardExpr
}

fn test_transform_if_expr_with_if_guard_result_uses_temp_var() {
	// IfExpr with IfGuardExpr condition for Result type should use temp var pattern
	// Since env is empty, type lookups fail, so this hits the non-option path
	// which does map/array/simple transformation
	mut t := create_test_transformer()

	// Build: if x := result_call() { body } else { else_body }
	if_expr := ast.IfExpr{
		cond:      ast.IfGuardExpr{
			stmt: ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(ast.Ident{
					name: 'x'
				})]
				rhs: [
					ast.Expr(ast.CallExpr{
						lhs:  ast.Ident{
							name: 'result_call'
						}
						args: []
					}),
				]
			}
		}
		stmts:     [ast.Stmt(ast.ExprStmt{
			expr: ast.Ident{
				name: 'x'
			}
		})]
		else_expr: ast.BasicLiteral{
			kind:  .number
			value: '0'
		}
	}

	result := t.transform_if_expr(if_expr)

	// The IfGuardExpr should be expanded - result should NOT contain IfGuardExpr
	// It should be a regular IfExpr with a non-guard condition
	assert result is ast.IfExpr, 'expected IfExpr, got ${result.type_name()}'
	result_if := result as ast.IfExpr
	assert result_if.cond !is ast.IfGuardExpr, 'IfGuardExpr should have been expanded'
}

fn test_transform_if_expr_with_if_guard_blank_lhs() {
	// if _ := some_call() { body }  — blank LHS should skip variable assignment
	mut t := create_test_transformer()

	if_expr := ast.IfExpr{
		cond:      ast.IfGuardExpr{
			stmt: ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(ast.Ident{
					name: '_'
				})]
				rhs: [
					ast.Expr(ast.CallExpr{
						lhs:  ast.Ident{
							name: 'some_call'
						}
						args: []
					}),
				]
			}
		}
		stmts:     [
			ast.Stmt(ast.ExprStmt{
				expr: ast.BasicLiteral{
					kind:  .number
					value: '1'
				}
			}),
		]
		else_expr: ast.BasicLiteral{
			kind:  .number
			value: '0'
		}
	}

	result := t.transform_if_expr(if_expr)

	assert result is ast.IfExpr, 'expected IfExpr, got ${result.type_name()}'
	result_if := result as ast.IfExpr
	assert result_if.cond !is ast.IfGuardExpr, 'IfGuardExpr should have been expanded'
	// With blank LHS, the body should NOT have a guard assignment prepended
	// Body should just have the original statement (transformed)
	assert result_if.stmts.len == 1, 'expected 1 stmt in body (no guard assign for blank), got ${result_if.stmts.len}'
}

fn test_if_guard_option_payload_assignment_uses_unwrapped_lhs_type() {
	opt_string := types.Type(types.OptionType{
		base_type: types.string_
	})
	mut env := types.Environment.new()
	call_pos := token.Pos{
		id: 1001
	}
	guard_pos := token.Pos{
		id: 1002
	}
	if_pos := token.Pos{
		id: 1003
	}
	env.set_expr_type(call_pos.id, opt_string)
	env.set_expr_type(guard_pos.id, opt_string)
	env.set_expr_type(if_pos.id, types.Type(types.Array{
		elem_type: types.string_
	}))
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('resolved_mod', opt_string)
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		scope:                       scope
		fn_root_scope:               scope
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		synth_types:                 map[int]types.Type{}
	}
	stmt := ast.ExprStmt{
		expr: ast.Expr(ast.IfExpr{
			pos:   if_pos
			cond:  ast.Expr(ast.IfGuardExpr{
				stmt: ast.AssignStmt{
					op:  .decl_assign
					lhs: [
						ast.Expr(ast.Ident{
							name: 'resolved_mod'
							pos:  guard_pos
						}),
					]
					rhs: [
						ast.Expr(ast.CallExpr{
							lhs: ast.Expr(ast.Ident{
								name: 'resolve_mod'
							})
							pos: call_pos
						}),
					]
				}
			})
			stmts: [
				ast.Stmt(ast.ExprStmt{
					expr: ast.Expr(ast.Ident{
						name: 'resolved_mod'
					})
				}),
			]
		})
	}
	expanded := t.try_expand_if_guard_stmt(stmt) or {
		assert false, 'if-guard did not expand'
		return
	}
	assert expanded.len == 2
	assert expanded[0] is ast.AssignStmt
	temp_assign := expanded[0] as ast.AssignStmt
	assert temp_assign.lhs[0] is ast.Ident
	temp_ident := temp_assign.lhs[0] as ast.Ident
	temp_type := t.synth_types[temp_ident.pos.id] or {
		assert false, 'guard temp type was not registered'
		return
	}
	assert temp_type.name() == '?string'
	assert expanded[1] is ast.ExprStmt
	if_stmt := (expanded[1] as ast.ExprStmt).expr as ast.IfExpr
	assert if_stmt.stmts[0] is ast.AssignStmt
	guard_assign := if_stmt.stmts[0] as ast.AssignStmt
	assert guard_assign.rhs[0] is ast.SelectorExpr, 'payload assignment was wrapped in ${guard_assign.rhs[0].type_name()}'
	payload := guard_assign.rhs[0] as ast.SelectorExpr
	assert payload.rhs.name == 'data'
	lhs_type := t.lookup_var_type('resolved_mod') or {
		assert false, 'guard payload variable type was not registered'
		return
	}
	assert lhs_type.name() == 'string'
}

fn test_if_guard_map_lookup_temp_keeps_pointer_type() {
	map_type := types.Type(types.Map{
		key_type:   types.Type(types.string_)
		value_type: types.Type(types.string_)
	})
	mut t := create_transformer_with_vars({
		'import_aliases': map_type
		'key':            types.Type(types.string_)
	})
	t.synth_types = map[int]types.Type{}
	if_pos := token.Pos{
		id: 9101
	}
	t.env.set_expr_type(if_pos.id, types.Type(types.string_))
	stmt := ast.ExprStmt{
		expr: ast.Expr(ast.IfExpr{
			pos:       if_pos
			cond:      ast.Expr(ast.IfGuardExpr{
				stmt: ast.AssignStmt{
					op:  .decl_assign
					lhs: [
						ast.Expr(ast.Ident{
							name: 'alias'
						}),
					]
					rhs: [
						ast.Expr(ast.IndexExpr{
							lhs:  ast.Expr(ast.Ident{
								name: 'import_aliases'
							})
							expr: ast.Expr(ast.Ident{
								name: 'key'
							})
						}),
					]
				}
			})
			stmts:     [
				ast.Stmt(ast.ExprStmt{
					expr: ast.Expr(ast.Ident{
						name: 'alias'
					})
				}),
			]
			else_expr: ast.Expr(ast.StringLiteral{
				kind:  .v
				value: 'missing'
			})
		})
	}
	expanded := t.try_expand_if_guard_stmt(stmt) or {
		assert false, 'map if-guard did not expand'
		return
	}
	assert expanded.len == 2
	assert expanded[0] is ast.AssignStmt
	temp_assign := expanded[0] as ast.AssignStmt
	assert temp_assign.lhs[0] is ast.Ident
	temp_ident := temp_assign.lhs[0] as ast.Ident
	temp_type := t.synth_types[temp_ident.pos.id] or {
		assert false, 'map guard temp type was not registered'
		return
	}
	assert temp_type is types.Pointer
	temp_pointer := temp_type as types.Pointer
	assert temp_pointer.base_type.name() == 'string'
	assert expanded[1] is ast.ExprStmt
	if_stmt := (expanded[1] as ast.ExprStmt).expr as ast.IfExpr
	assert if_stmt.pos.id != temp_ident.pos.id
	if_type := t.synth_types[if_stmt.pos.id] or {
		assert false, 'rewritten if expression type was not registered'
		return
	}
	assert if_type.name() == 'string'
}

fn test_native_smartcast_alias_variant_uses_direct_data_cast() {
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('EmptyExpr', types.Type(types.Alias{
		name:      'EmptyExpr'
		base_type: types.Type(types.Primitive{
			props: .integer | .unsigned
			size:  8
		})
	}))
	mut t := create_test_transformer()
	t.pref.backend = .arm64
	t.cached_scopes['ast'] = ast_scope
	ctx := SmartcastContext{
		expr:         'else_if.cond'
		variant:      'ast__EmptyExpr'
		variant_full: 'ast__EmptyExpr'
		sumtype:      'ast__Expr'
	}
	out := t.apply_smartcast_direct_ctx(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'else_if'
		})
		rhs: ast.Ident{
			name: 'cond'
		}
	}), ctx)
	assert out is ast.ParenExpr
	cast := (out as ast.ParenExpr).expr
	assert cast is ast.CastExpr, 'direct-data alias smartcast must not dereference _data'
	assert (cast as ast.CastExpr).typ is ast.Ident
	assert ((cast as ast.CastExpr).typ as ast.Ident).name == 'ast__EmptyExpr'
}

fn test_sumtype_alias_variant_init_uses_direct_data() {
	empty_expr_type := types.Type(types.Alias{
		name:      'EmptyExpr'
		base_type: types.Type(types.Primitive{
			props: .integer | .unsigned
			size:  8
		})
	})
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('EmptyExpr', empty_expr_type)
	ast_scope.insert('FnType', types.Type(types.Struct{
		name: 'FnType'
	}))
	ast_scope.insert('Expr', types.Type(types.SumType{
		name:     'Expr'
		variants: [empty_expr_type]
	}))
	mut t := create_test_transformer()
	t.cur_module = 'ast'
	t.cached_scopes['ast'] = ast_scope
	assert t.sumtype_variant_init_data_is_direct('EmptyExpr')
	assert !t.sumtype_variant_init_data_is_direct('FnType')
	wrapped := t.build_sumtype_init(ast.Expr(ast.CastExpr{
		typ:  ast.Expr(ast.Ident{
			name: 'EmptyExpr'
		})
		expr: ast.Expr(ast.BasicLiteral{
			kind:  .number
			value: '0'
		})
	}), 'EmptyExpr', 'Expr') or {
		assert false, 'EmptyExpr should be wrapped as Expr'
		return
	}
	assert wrapped is ast.InitExpr
	init := wrapped as ast.InitExpr
	assert init.fields.len == 2
	data_value := init.fields[1].value
	assert data_value is ast.CastExpr
	data_cast := data_value as ast.CastExpr
	assert data_cast.expr is ast.CastExpr, 'alias-to-scalar variants should be stored inline'
	assert data_cast.expr !is ast.CallExpr, 'alias-to-scalar variants should not be copied with memdup'
}

fn test_transform_if_expr_preserves_else() {
	// Ensure else branch is preserved during IfGuardExpr expansion
	mut t := create_test_transformer()

	if_expr := ast.IfExpr{
		cond:      ast.IfGuardExpr{
			stmt: ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(ast.Ident{
					name: 'val'
				})]
				rhs: [
					ast.Expr(ast.CallExpr{
						lhs:  ast.Ident{
							name: 'try_get'
						}
						args: []
					}),
				]
			}
		}
		stmts:     [ast.Stmt(ast.ExprStmt{
			expr: ast.Ident{
				name: 'val'
			}
		})]
		else_expr: ast.BasicLiteral{
			kind:  .number
			value: '-1'
		}
	}

	result := t.transform_if_expr(if_expr)

	// Result should have an else branch
	if result is ast.IfExpr {
		assert result.else_expr !is ast.EmptyExpr, 'else branch should be preserved'
	} else if result is ast.UnsafeExpr {
		// Result expansion wraps in UnsafeExpr; find the inner IfExpr
		mut found_if := false
		for s in result.stmts {
			if s is ast.ExprStmt {
				if s.expr is ast.IfExpr {
					assert s.expr.else_expr !is ast.EmptyExpr, 'else branch should be preserved in UnsafeExpr'
					found_if = true
				}
			}
		}
		assert found_if, 'expected IfExpr inside UnsafeExpr'
	} else {
		assert false, 'expected IfExpr or UnsafeExpr, got ${result.type_name()}'
	}
}

fn test_transform_for_in_stmt_lowers_to_for_stmt() {
	mut t := create_test_transformer()
	result := t.transform_for_in_stmt(ast.ForInStmt{
		value: ast.Ident{
			name: 'v'
		}
		expr:  ast.Ident{
			name: 'items'
		}
	})

	// NOTE: transform_for_in_stmt returns `ast.ForStmt` directly.
	assert result.init is ast.AssignStmt, 'expected lowered init AssignStmt, got ${result.init.type_name()}'
}

// --- Inline array `in` optimization tests ---

fn test_transform_in_inline_array_expands_to_eq_chain() {
	// x in [1, 2, 3] => (x == 1 || x == 2 || x == 3)
	mut t := create_transformer_with_vars({
		'x': types.Type(types.int_)
	})

	expr := ast.InfixExpr{
		op:  .key_in
		lhs: ast.Ident{
			name: 'x'
		}
		rhs: ast.ArrayInitExpr{
			exprs: [
				ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '1'
				}),
				ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '2'
				}),
				ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '3'
				}),
			]
		}
	}

	result := t.transform_infix_expr(expr)

	// Should be: (x == 1 || x == 2 || x == 3)
	// Top level: (x == 1 || x == 2) || (x == 3)
	assert result is ast.InfixExpr, 'expected InfixExpr (||), got ${result.type_name()}'
	top := result as ast.InfixExpr
	assert top.op == .logical_or, 'expected || at top, got ${top.op}'
	// RHS should be x == 3
	assert top.rhs is ast.InfixExpr
	rhs := top.rhs as ast.InfixExpr
	assert rhs.op == .eq
	assert rhs.lhs is ast.Ident
	assert (rhs.lhs as ast.Ident).name == 'x'
	assert rhs.rhs is ast.BasicLiteral
	assert (rhs.rhs as ast.BasicLiteral).value == '3'
}

fn test_transform_in_inline_array_single_element() {
	// x in [5] => x == 5
	mut t := create_transformer_with_vars({
		'x': types.Type(types.int_)
	})

	expr := ast.InfixExpr{
		op:  .key_in
		lhs: ast.Ident{
			name: 'x'
		}
		rhs: ast.ArrayInitExpr{
			exprs: [
				ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '5'
				}),
			]
		}
	}

	result := t.transform_infix_expr(expr)

	// Single element: x == 5
	assert result is ast.InfixExpr, 'expected InfixExpr (==), got ${result.type_name()}'
	eq := result as ast.InfixExpr
	assert eq.op == .eq
	assert eq.lhs is ast.Ident
	assert (eq.lhs as ast.Ident).name == 'x'
	assert eq.rhs is ast.BasicLiteral
	assert (eq.rhs as ast.BasicLiteral).value == '5'
}

fn test_transform_not_in_inline_array_wraps_with_not() {
	// x !in [1, 2] => !(x == 1 || x == 2)
	mut t := create_transformer_with_vars({
		'x': types.Type(types.int_)
	})

	expr := ast.InfixExpr{
		op:  .not_in
		lhs: ast.Ident{
			name: 'x'
		}
		rhs: ast.ArrayInitExpr{
			exprs: [
				ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '1'
				}),
				ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '2'
				}),
			]
		}
	}

	result := t.transform_infix_expr(expr)

	// Should be: !(x == 1 || x == 2)
	assert result is ast.PrefixExpr, 'expected PrefixExpr (!), got ${result.type_name()}'
	prefix := result as ast.PrefixExpr
	assert prefix.op == .not
	assert prefix.expr is ast.InfixExpr
	inner := prefix.expr as ast.InfixExpr
	assert inner.op == .logical_or
}

fn test_transformer_preserves_pointer_lifetime_in_v_syntax_but_not_c_names() {
	mut t := create_test_transformer()
	ptr_type := types.Type(types.Pointer{
		base_type: types.Type(types.NamedType('Foo'))
		lifetime:  'a'
	})

	ast_expr := t.type_to_ast_type_expr(ptr_type)
	assert ast_expr is ast.Type
	assert (ast_expr as ast.Type) is ast.PointerType
	ptr_ast := (ast_expr as ast.Type) as ast.PointerType
	assert ptr_ast.lifetime == 'a'
	assert ptr_ast.base_type is ast.Ident
	assert (ptr_ast.base_type as ast.Ident).name == 'Foo'

	assert t.types_type_to_v(ptr_type) == '&^a Foo'
	assert t.type_to_c_name(ptr_type) == 'Fooptr'
}

fn test_transformer_uses_pointer_type_receiver_name_for_scope_key() {
	t := create_test_transformer()
	receiver := ast.Expr(ast.Type(ast.PointerType{
		base_type: ast.Expr(ast.Ident{
			name: 'Ignore'
		})
		lifetime:  'a'
	}))
	assert t.get_receiver_type_name(receiver) == 'Ignore'
}

fn test_transformer_uses_pointer_type_for_generic_specialization_token() {
	t := create_test_transformer()
	foo_ptr := ast.Expr(ast.Type(ast.PointerType{
		base_type: ast.Expr(ast.Ident{
			name: 'Foo'
		})
	}))
	bar_ptr := ast.Expr(ast.Type(ast.PointerType{
		base_type: ast.Expr(ast.Ident{
			name: 'Bar'
		})
	}))
	assert t.generic_specialization_token(foo_ptr) == 'Fooptr'
	assert t.generic_specialization_token(bar_ptr) == 'Barptr'
	assert t.generic_specialization_suffix([foo_ptr]) == '_T_Fooptr'
	assert t.generic_specialization_suffix([bar_ptr]) == '_T_Barptr'
}

fn test_expr_to_type_name_handles_array_pointer_type() {
	t := create_test_transformer()
	array_ptr := ast.Expr(ast.Type(ast.ArrayType{
		elem_type: ast.Expr(ast.Type(ast.PointerType{
			base_type: ast.Expr(ast.Ident{
				name: 'char'
			})
		}))
	}))
	assert t.expr_to_type_name(array_ptr) == 'Array_charptr'
}

fn test_c_name_to_type_resolves_builtin_pointer_aliases_without_scope_lookup() {
	t := create_test_transformer()
	voidptr_type := t.c_name_to_type('voidptr') or {
		assert false, 'voidptr should resolve as a builtin type'
		return
	}
	assert voidptr_type is types.Alias
	assert (voidptr_type as types.Alias).name == 'voidptr'

	array_type := t.c_name_to_type('Array_voidptr') or {
		assert false, 'Array_voidptr should resolve as a builtin array type'
		return
	}
	assert array_type is types.Array
	array_elem := (array_type as types.Array).elem_type
	assert array_elem is types.Alias
	assert (array_elem as types.Alias).name == 'voidptr'
}

fn test_get_array_init_expr_type_resolves_voidptr_element_type() {
	t := create_test_transformer()
	array_init := ast.ArrayInitExpr{
		typ: ast.Expr(ast.Type(ast.ArrayType{
			elem_type: ast.Expr(ast.Ident{
				name: 'voidptr'
			})
		}))
	}
	array_type := t.get_array_init_expr_type(array_init) or {
		assert false, '[]voidptr{} should produce an array type'
		return
	}
	assert array_type is types.Array
	array_elem := (array_type as types.Array).elem_type
	assert array_elem is types.Alias
	assert (array_elem as types.Alias).name == 'voidptr'
}

fn test_match_variant_resolves_fixed_array_c_name() {
	t := create_test_transformer()
	constructor_variant := t.match_variant('Array_fixed_u32_64', ['ArrayFixed']) or { '' }
	assert constructor_variant == 'ArrayFixed'
	fixed_array_variant := t.match_variant('Array_fixed_u32_64', ['[64]u32']) or { '' }
	assert fixed_array_variant == '[64]u32'
}

fn test_sumtype_return_wrap_prefers_init_constructor_over_contextual_type() {
	sum_type := types.Type(types.SumType{
		name:     'types__Type'
		variants: [
			types.Type(types.Struct{
				name: 'ArrayFixed'
			}),
		]
	})
	mut types_scope := types.new_scope(unsafe { nil })
	types_scope.insert('Type', sum_type)
	mut env := types.Environment.new()
	pos := token.Pos{
		id: 9101
	}
	env.set_expr_type(pos.id, sum_type)
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		cached_scopes:               {
			'types': types_scope
		}
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
	}
	value := ast.Expr(ast.InitExpr{
		typ: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'types'
			})
			rhs: ast.Ident{
				name: 'ArrayFixed'
			}
		})
		pos: pos
	})
	variants := t.get_sum_type_variants('types__Type')
	assert variants == ['ArrayFixed']
	assert t.init_expr_sumtype_variant_name(value as ast.InitExpr, variants, 'types__Type') == 'ArrayFixed'
	wrapped := t.wrap_sumtype_value_transformed(value, 'types__Type') or {
		assert false, 'ArrayFixed constructor should be wrapped as types.Type'
		return
	}
	assert wrapped is ast.InitExpr
	wrapped_init := wrapped as ast.InitExpr
	assert wrapped_init.typ is ast.Ident
	assert (wrapped_init.typ as ast.Ident).name == 'types__Type'

	option_pos := token.Pos{
		id: 9102
	}
	env.set_expr_type(option_pos.id, types.Type(types.OptionType{
		base_type: sum_type
	}))
	option_context_value := ast.Expr(ast.InitExpr{
		typ: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'types'
			})
			rhs: ast.Ident{
				name: 'ArrayFixed'
			}
		})
		pos: option_pos
	})
	t.cur_fn_ret_type_name = 'types__Type'
	t.cur_fn_returns_option = true
	return_stmt := t.transform_return_stmt(ast.ReturnStmt{
		exprs: [option_context_value]
	})
	assert return_stmt.exprs.len == 1
	assert return_stmt.exprs[0] is ast.InitExpr
	return_init := return_stmt.exprs[0] as ast.InitExpr
	assert return_init.typ is ast.Ident
	assert (return_init.typ as ast.Ident).name == 'types__Type'

	or_context_value := ast.Expr(ast.InitExpr{
		typ:    ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'types'
			})
			rhs: ast.Ident{
				name: 'ArrayFixed'
			}
		})
		fields: [
			ast.FieldInit{
				name:  'len'
				value: ast.Expr(ast.OrExpr{
					expr:  ast.Expr(ast.CallExpr{
						lhs: ast.Expr(ast.Ident{
							name: 'parse_len'
						})
					})
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.Expr(ast.BasicLiteral{
								kind:  .number
								value: '0'
							})
						}),
					]
				})
			},
		]
	})
	expanded := t.try_expand_or_expr_return(ast.ReturnStmt{
		exprs: [or_context_value]
	}) or {
		assert false, 'return with nested or should expand'
		return
	}
	assert expanded.len > 0
	last_stmt := expanded[expanded.len - 1]
	assert last_stmt is ast.ReturnStmt
	expanded_return := last_stmt as ast.ReturnStmt
	assert expanded_return.exprs.len == 1
	assert expanded_return.exprs[0] is ast.InitExpr
	expanded_return_init := expanded_return.exprs[0] as ast.InitExpr
	assert expanded_return_init.typ is ast.Ident
	assert (expanded_return_init.typ as ast.Ident).name == 'types__Type'
}

fn test_return_match_expr_expands_to_branch_returns() {
	mut t := create_test_transformer()
	t.cur_fn_returns_option = true
	match_expr := ast.MatchExpr{
		expr:     ast.Expr(ast.Ident{
			name: 'name'
		})
		branches: [
			ast.MatchBranch{
				cond:  [
					ast.Expr(ast.StringLiteral{
						kind:  .v
						value: 'int'
					}),
				]
				stmts: [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.Ident{
							name: 'int_type'
						})
					}),
				]
			},
			ast.MatchBranch{
				stmts: [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.Ident{
							name: 'lookup_type'
						})
					}),
				]
			},
		]
	}
	expanded := t.try_expand_return_match_expr(ast.ReturnStmt{
		exprs: [ast.Expr(match_expr)]
	}) or {
		assert false, 'return match should expand'
		return
	}
	assert expanded.len == 1
	assert expanded[0] is ast.ExprStmt
	if_stmt := (expanded[0] as ast.ExprStmt).expr
	assert if_stmt is ast.IfExpr
	if_expr := if_stmt as ast.IfExpr
	assert if_expr.stmts.len == 1
	assert if_expr.stmts[0] is ast.ReturnStmt
	assert if_expr.else_expr is ast.IfExpr
	else_expr := if_expr.else_expr as ast.IfExpr
	assert else_expr.cond is ast.EmptyExpr
	assert else_expr.stmts.len == 1
	assert else_expr.stmts[0] is ast.ReturnStmt
}

fn test_return_match_void_branch_stays_side_effect() {
	mut t := create_test_transformer()
	match_expr := ast.MatchExpr{
		expr:     ast.Expr(ast.Ident{
			name: 'name'
		})
		branches: [
			ast.MatchBranch{
				cond:  [
					ast.Expr(ast.StringLiteral{
						kind:  .v
						value: 'bad'
					}),
				]
				stmts: [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.CallExpr{
							lhs: ast.Expr(ast.Ident{
								name: 'eprintln_exit'
							})
						})
					}),
				]
			},
			ast.MatchBranch{
				stmts: [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.Ident{
							name: 'fallback'
						})
					}),
				]
			},
		]
	}
	expanded := t.try_expand_return_match_expr(ast.ReturnStmt{
		exprs: [ast.Expr(match_expr)]
	}) or {
		assert false, 'return match should expand'
		return
	}
	if_stmt := (expanded[0] as ast.ExprStmt).expr as ast.IfExpr
	assert if_stmt.stmts.len == 1
	assert if_stmt.stmts[0] is ast.ExprStmt
	assert if_stmt.stmts[0] !is ast.ReturnStmt
}

fn test_return_match_or_branch_preserves_success_payload_return() {
	mut t := create_test_transformer()
	t.cur_fn_returns_option = true
	match_expr := ast.MatchExpr{
		expr:     ast.Expr(ast.Ident{
			name: 'name'
		})
		branches: [
			ast.MatchBranch{
				cond:  [
					ast.Expr(ast.StringLiteral{
						kind:  .v
						value: 'rune'
					}),
				]
				stmts: [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.OrExpr{
							expr:  ast.Expr(ast.CallExpr{
								lhs: ast.Expr(ast.Ident{
									name: 'lookup_type'
								})
							})
							stmts: [
								ast.Stmt(ast.ReturnStmt{
									exprs: [
										ast.Expr(ast.Ident{
											name: 'none'
										}),
									]
								}),
							]
						})
					}),
				]
			},
			ast.MatchBranch{
				stmts: [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.Ident{
							name: 'none'
						})
					}),
				]
			},
		]
	}
	expanded := t.try_expand_return_match_expr(ast.ReturnStmt{
		exprs: [ast.Expr(match_expr)]
	}) or {
		assert false, 'return match should expand'
		return
	}
	assert expanded.len == 1
	if_stmt := (expanded[0] as ast.ExprStmt).expr as ast.IfExpr
	assert if_stmt.stmts.len == 1
	assert if_stmt.stmts[0] is ast.ReturnStmt
	ret := if_stmt.stmts[0] as ast.ReturnStmt
	assert ret.exprs.len == 1
	assert ret.exprs[0] is ast.UnsafeExpr, 'or payload should stay in value context, got ${ret.exprs[0].type_name()}'
	unsafe_expr := ret.exprs[0] as ast.UnsafeExpr
	assert unsafe_expr.stmts.len >= 3
	last_stmt := unsafe_expr.stmts[unsafe_expr.stmts.len - 1]
	assert last_stmt is ast.ExprStmt
	payload_expr := (last_stmt as ast.ExprStmt).expr
	assert payload_expr is ast.SelectorExpr, 'expected final or expression to return payload selector, got ${payload_expr.type_name()}'
	payload_selector := payload_expr as ast.SelectorExpr
	assert payload_selector.rhs.name == 'data'
}

fn test_transform_return_match_reprocesses_nested_return_if_expr() {
	mut t := create_test_transformer()
	t.cur_fn_returns_option = true
	match_expr := ast.MatchExpr{
		expr:     ast.Expr(ast.Ident{
			name: 'expr'
		})
		branches: [
			ast.MatchBranch{
				cond:  [
					ast.Expr(ast.Ident{
						name: 'CallOrCastExpr'
					}),
				]
				stmts: [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.IfExpr{
							cond:      ast.Expr(ast.Ident{
								name: 'is_type_expr'
							})
							stmts:     [
								ast.Stmt(ast.ExprStmt{
									expr: ast.Expr(ast.StringLiteral{
										kind:  .v
										value: 'type-name'
									})
								}),
							]
							else_expr: ast.Expr(ast.IfExpr{
								cond:  ast.empty_expr
								stmts: [
									ast.Stmt(ast.ExprStmt{
										expr: ast.Expr(ast.Ident{
											name: 'none'
										})
									}),
								]
							})
						})
					}),
				]
			},
			ast.MatchBranch{
				stmts: [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.Ident{
							name: 'none'
						})
					}),
				]
			},
		]
	}
	transformed := t.transform_stmts([
		ast.Stmt(ast.ReturnStmt{
			exprs: [ast.Expr(match_expr)]
		}),
	])
	assert transformed.len == 1
	assert transformed[0] is ast.ExprStmt
	top_if := (transformed[0] as ast.ExprStmt).expr as ast.IfExpr
	assert top_if.stmts.len == 1
	assert top_if.stmts[0] is ast.ExprStmt
	nested_if := (top_if.stmts[0] as ast.ExprStmt).expr as ast.IfExpr
	assert nested_if.stmts.len == 1
	assert nested_if.stmts[0] is ast.ReturnStmt
	assert nested_if.else_expr is ast.IfExpr
	nested_else := nested_if.else_expr as ast.IfExpr
	assert nested_else.cond is ast.EmptyExpr
	assert nested_else.stmts.len == 1
	assert nested_else.stmts[0] is ast.ReturnStmt
}

fn test_return_match_expands_if_guard_unsafe_branch_value_to_statement_returns() {
	mut t := create_test_transformer()
	nested_guard_value := ast.Expr(ast.UnsafeExpr{
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(ast.Ident{
					name: '_or_t2'
				})]
				rhs: [
					ast.Expr(ast.CallExpr{
						lhs: ast.Expr(ast.Ident{
							name: 'lookup_builtin'
						})
					}),
				]
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.IfExpr{
					cond:      ast.Expr(ast.Ident{
						name: '_or_t2_ok'
					})
					stmts:     [
						ast.Stmt(ast.ExprStmt{
							expr: ast.Expr(ast.Ident{
								name: 'const_expr2'
							})
						}),
					]
					else_expr: ast.Expr(ast.IfExpr{
						cond:  ast.empty_expr
						stmts: [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Expr(ast.Ident{
									name: 'none'
								})
							}),
						]
					})
				})
			}),
		]
	})
	outer_guard_value := ast.Expr(ast.UnsafeExpr{
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(ast.Ident{
					name: '_or_t1'
				})]
				rhs: [
					ast.Expr(ast.CallExpr{
						lhs: ast.Expr(ast.Ident{
							name: 'lookup_current'
						})
					}),
				]
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.IfExpr{
					cond:      ast.Expr(ast.Ident{
						name: '_or_t1_ok'
					})
					stmts:     [
						ast.Stmt(ast.ExprStmt{
							expr: ast.Expr(ast.Ident{
								name: 'const_expr'
							})
						}),
					]
					else_expr: nested_guard_value
				})
			}),
		]
	})
	stmts := t.return_stmts_for_branch_expr(outer_guard_value, true)
	assert stmts.len == 2
	assert stmts[0] is ast.AssignStmt
	assert stmts[1] is ast.ExprStmt
	outer_if := (stmts[1] as ast.ExprStmt).expr as ast.IfExpr
	assert outer_if.stmts.len == 1
	assert outer_if.stmts[0] is ast.ReturnStmt
	assert outer_if.else_expr is ast.IfExpr
	outer_else := outer_if.else_expr as ast.IfExpr
	assert outer_else.cond is ast.EmptyExpr
	assert outer_else.stmts.len == 2
	assert outer_else.stmts[0] is ast.AssignStmt
	assert outer_else.stmts[1] is ast.ExprStmt
	inner_if := (outer_else.stmts[1] as ast.ExprStmt).expr as ast.IfExpr
	assert inner_if.stmts.len == 1
	assert inner_if.stmts[0] is ast.ReturnStmt
	assert inner_if.else_expr is ast.IfExpr
	inner_else := inner_if.else_expr as ast.IfExpr
	assert inner_else.cond is ast.EmptyExpr
	assert inner_else.stmts.len == 1
	assert inner_else.stmts[0] is ast.ReturnStmt
}

fn test_return_match_expr_without_else_keeps_empty_terminal_branch() {
	mut t := create_test_transformer()
	match_expr := ast.MatchExpr{
		expr:     ast.Expr(ast.Ident{
			name: 'kind'
		})
		branches: [
			ast.MatchBranch{
				cond:  [
					ast.Expr(ast.StringLiteral{
						kind:  .v
						value: 'a'
					}),
				]
				stmts: [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.StringLiteral{
							kind:  .v
							value: 'alpha'
						})
					}),
				]
			},
			ast.MatchBranch{
				cond:  [
					ast.Expr(ast.StringLiteral{
						kind:  .v
						value: 'b'
					}),
				]
				stmts: [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.StringLiteral{
							kind:  .v
							value: 'beta'
						})
					}),
				]
			},
		]
	}
	expanded := t.try_expand_return_match_expr(ast.ReturnStmt{
		exprs: [ast.Expr(match_expr)]
	}) or {
		assert false, 'return match should expand'
		return
	}
	assert expanded.len == 1
	assert expanded[0] is ast.ExprStmt
	if_stmt := (expanded[0] as ast.ExprStmt).expr
	assert if_stmt is ast.IfExpr
	if_expr := if_stmt as ast.IfExpr
	assert if_expr.stmts.len == 1
	assert if_expr.stmts[0] is ast.ReturnStmt
	assert if_expr.else_expr is ast.IfExpr
	else_if := if_expr.else_expr as ast.IfExpr
	assert else_if.cond !is ast.EmptyExpr
	assert else_if.stmts.len == 1
	assert else_if.stmts[0] is ast.ReturnStmt
	assert else_if.else_expr is ast.EmptyExpr
}

fn test_transformer_preserves_lifetime_method_signature_and_nested_generic_return_type() {
	files := transform_code_for_test('
struct Ignore {}

struct DirEntry {}

struct Match[T] {
	value T
}

struct IgnoreMatch[^a] {
	ig &^a Ignore
}

fn (ig &^a Ignore) matched_dir_entry[^a](dent &DirEntry) Match[IgnoreMatch[^a]] {
	return Match[IgnoreMatch[^a]]{
		value: IgnoreMatch[^a]{
			ig: ig
		}
	}
}

fn main() {
	ig := Ignore{}
	dent := DirEntry{}
	ig.matched_dir_entry(&dent)
}
')
	assert files.len == 1
	file := files[0]
	mut saw_method := false
	mut saw_call := false
	for stmt in file.stmts {
		if stmt is ast.FnDecl && stmt.name == 'matched_dir_entry' {
			saw_method = true
			assert stmt.is_method
			assert stmt.stmts.len > 0
			assert stmt.receiver.typ is ast.Type
			receiver_type := stmt.receiver.typ as ast.Type
			assert receiver_type is ast.PointerType
			receiver_ptr := receiver_type as ast.PointerType
			assert receiver_ptr.lifetime == 'a'
			assert stmt.typ.generic_params.len == 1
			assert stmt.typ.generic_params[0] is ast.LifetimeExpr
			assert (stmt.typ.generic_params[0] as ast.LifetimeExpr).name == 'a'
			assert stmt.typ.return_type is ast.Type
			return_type := stmt.typ.return_type as ast.Type
			assert return_type is ast.GenericType
			outer_generic := return_type as ast.GenericType
			assert outer_generic.name is ast.Ident
			assert (outer_generic.name as ast.Ident).name == 'Match'
			assert outer_generic.params.len == 1
			assert outer_generic.params[0] is ast.Type
			inner_type := outer_generic.params[0] as ast.Type
			assert inner_type is ast.GenericType
			inner_generic := inner_type as ast.GenericType
			assert inner_generic.name is ast.Ident
			assert (inner_generic.name as ast.Ident).name == 'IgnoreMatch'
			assert inner_generic.params.len == 1
			assert inner_generic.params[0] is ast.LifetimeExpr
			assert (inner_generic.params[0] as ast.LifetimeExpr).name == 'a'
		}
		if stmt is ast.FnDecl && stmt.name == 'main' {
			assert stmt.stmts.len == 3
			assert stmt.stmts[2] is ast.ExprStmt
			expr_stmt := stmt.stmts[2] as ast.ExprStmt
			assert expr_stmt.expr is ast.CallExpr
			call := expr_stmt.expr as ast.CallExpr
			assert call.lhs is ast.Ident
			assert (call.lhs as ast.Ident).name == 'Ignore__matched_dir_entry'
			assert call.args.len == 2
			saw_call = true
		}
	}
	assert saw_method
	assert saw_call
}

fn test_nested_filter_map_filter_receiver_cache_names_are_unique() {
	files := transform_code_for_test('
fn nested_filter_map_filter(xs []int, keep []string) []string {
	unknown := xs.filter(it > 0).map(it.str()).filter(it !in keep)
	return unknown
}
')
	assert files.len == 1
	mut recv_names := []string{}
	for stmt in files[0].stmts {
		if stmt is ast.FnDecl && stmt.name == 'nested_filter_map_filter' {
			for inner in stmt.stmts {
				if inner is ast.AssignStmt {
					for lhs in inner.lhs {
						if lhs is ast.Ident && lhs.name.starts_with('_filter_recv') {
							recv_names << lhs.name
						}
					}
				}
			}
		}
	}
	assert recv_names.len >= 2, 'expected receiver caches for nested map/filter, got ${recv_names}'
	mut seen := map[string]bool{}
	for name in recv_names {
		assert name !in seen, 'duplicate receiver cache name: ${name}'
		seen[name] = true
	}
}

fn test_for_in_filter_receiver_temp_is_hoisted_before_loop() {
	files := transform_code_for_test('
struct ImportSymbol {
	name string
}

struct Import {
	syms []ImportSymbol
}

fn use_filtered_imports(imports []Import, owner string) int {
	mut count := 0
	for import_sym in imports.filter(it.syms.any(it.name == owner)) {
		_ = import_sym
		count++
	}
	return count
}
')
	assert files.len == 1
	mut found_fn := false
	for stmt in files[0].stmts {
		if stmt is ast.FnDecl && stmt.name == 'use_filtered_imports' {
			found_fn = true
			mut saw_filter_temp_decl := false
			mut found_outer_loop := false
			for inner in stmt.stmts {
				if inner is ast.AssignStmt && inner.op == .decl_assign && inner.lhs.len > 0 {
					if lhs := ident_name_from_expr_for_test(inner.lhs[0]) {
						if lhs.starts_with('_filter_t') {
							saw_filter_temp_decl = true
						}
					}
				}
				if inner is ast.ForStmt && inner.stmts.len > 0 {
					first_body := inner.stmts[0]
					if first_body is ast.AssignStmt && first_body.lhs.len > 0 {
						if lhs := ident_name_from_expr_for_test(first_body.lhs[0]) {
							if lhs == 'import_sym' {
								found_outer_loop = true
								assert saw_filter_temp_decl
								for loop_stmt in inner.stmts {
									if loop_stmt is ast.AssignStmt && loop_stmt.op == .decl_assign
										&& loop_stmt.lhs.len > 0 {
										if loop_lhs := ident_name_from_expr_for_test(loop_stmt.lhs[0]) {
											assert !loop_lhs.starts_with('_filter_t')
										}
									}
								}
							}
						}
					}
				}
			}
			assert found_outer_loop
		}
	}
	assert found_fn
}

fn test_replace_it_ident_keeps_nested_any_body_scope() {
	mut t := create_test_transformer()
	nested_any := ast.Expr(ast.CallOrCastExpr{
		lhs:  ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'it'
				})
				rhs: ast.Ident{
					name: 'exprs'
				}
			})
			rhs: ast.Ident{
				name: 'any'
			}
		})
		expr: ast.Expr(ast.CallExpr{
			lhs:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'g'
				})
				rhs: ast.Ident{
					name: 'match_must_reset_if'
				}
			})
			args: [
				ast.Expr(ast.Ident{
					name: 'it'
				}),
			]
		})
	})
	replaced := t.replace_it_ident(nested_any, '_outer_it')
	assert replaced is ast.CallOrCastExpr
	call := replaced as ast.CallOrCastExpr
	assert call.lhs is ast.SelectorExpr
	outer_sel := call.lhs as ast.SelectorExpr
	assert outer_sel.lhs is ast.SelectorExpr
	exprs_sel := outer_sel.lhs as ast.SelectorExpr
	assert exprs_sel.lhs is ast.Ident
	assert (exprs_sel.lhs as ast.Ident).name == '_outer_it'
	assert call.expr is ast.CallExpr
	inner_call := call.expr as ast.CallExpr
	assert inner_call.args.len == 1
	assert inner_call.args[0] is ast.Ident
	assert (inner_call.args[0] as ast.Ident).name == 'it'
}

fn test_smartcast_and_all_condition_hoists_temp_before_inner_if() {
	files := transform_code_for_test('
struct Aggregate {
	types []int
}

struct Other {}

type Info = Aggregate | Other

struct Sym {
	info Info
}

fn use_all(sym Sym) bool {
	if sym.info is Aggregate && sym.info.types.all(it > 0) {
		return true
	}
	return false
}
')
	assert files.len == 1
	mut found := false
	for stmt in files[0].stmts {
		if stmt is ast.FnDecl && stmt.name == 'use_all' {
			found = true
			assert stmt.stmts.len >= 2
			assert stmt.stmts[0] is ast.ExprStmt
			outer_expr_stmt := stmt.stmts[0] as ast.ExprStmt
			assert outer_expr_stmt.expr is ast.IfExpr
			outer_if := outer_expr_stmt.expr as ast.IfExpr
			assert outer_if.stmts.len >= 3, 'expected condition expansion before inner if, got ${outer_if.stmts.len} stmts'
			assert outer_if.stmts[0] is ast.AssignStmt
			init_stmt := outer_if.stmts[0] as ast.AssignStmt
			assert init_stmt.lhs.len == 1
			assert init_stmt.lhs[0] is ast.ModifierExpr
			init_lhs := init_stmt.lhs[0] as ast.ModifierExpr
			assert init_lhs.expr is ast.Ident
			assert (init_lhs.expr as ast.Ident).name.starts_with('_filter_t')
			assert outer_if.stmts[1] is ast.ForStmt
			assert outer_if.stmts[2] is ast.ExprStmt
			inner_expr_stmt := outer_if.stmts[2] as ast.ExprStmt
			assert inner_expr_stmt.expr is ast.IfExpr
			inner_if := inner_expr_stmt.expr as ast.IfExpr
			assert inner_if.cond is ast.Ident
			assert (inner_if.cond as ast.Ident).name.starts_with('_filter_t')
		}
	}
	assert found
}

fn test_smartcast_and_else_if_label_branch_is_not_duplicated() {
	files := transform_code_for_test('
struct A {}
struct B {}
struct D {}

type Node = A | B | D

fn labeled_else_branch(left Node, right Node) {
	if left is A && right is B {
		return
	} else if right is D {
		out: for i := 0; i < 1; i++ {
			continue out
		}
	}
}
')
	assert count_label_name_in_files(files, 'labeled_else_branch', 'out') == 1
}

fn test_smartcasted_field_method_receiver_after_explicit_parent_cast() {
	files := transform_code_for_test('
type Expr = Ident | IndexExpr

struct Ident {
	flag bool
}

fn (i &Ident) is_mut() bool {
	return i.flag
}

struct IndexExpr {
	left  Expr
	index Expr
}

fn uses_smartcasted_field_method(init Expr) bool {
	return init is IndexExpr && (init as IndexExpr).left is Ident
		&& (init as IndexExpr).left.is_mut()
}
')
	assert files.len == 1
	mut call_names := []string{}
	for stmt in files[0].stmts {
		if stmt is ast.FnDecl && stmt.name == 'uses_smartcasted_field_method' {
			for inner in stmt.stmts {
				collect_call_names_from_stmt(inner, mut call_names)
			}
		}
	}
	assert 'Ident__is_mut' in call_names, 'expected smartcasted method call, got ${call_names}'
}
