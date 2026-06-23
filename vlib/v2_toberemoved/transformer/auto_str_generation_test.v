module transformer

import os
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token
import v2.types

fn auto_str_parse_files_for_test(label string, source string) ([]ast.File, ast.FlatAst, &types.Environment, &vpref.Preferences) {
	tmp_file := os.join_path(os.vtmp_dir(), 'v2_auto_str_generation_${label}_${os.getpid()}.v')
	os.write_file(tmp_file, source) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	mut prefs := &vpref.Preferences{
		backend:     .x64
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	flat := ast.flatten_files(files)
	return files, flat, env, prefs
}

fn auto_str_legacy_generated_files(label string, source string) []ast.File {
	files, _, env, prefs := auto_str_parse_files_for_test(label, source)
	mut trans := Transformer.new_with_pref(env, prefs)
	return trans.transform_files(files)
}

fn auto_str_flat_generated_files(label string, source string) []ast.File {
	files, flat, env, prefs := auto_str_parse_files_for_test(label, source)
	mut trans := Transformer.new_with_pref(env, prefs)
	transformed_flat, _ := trans.transform_files_to_flat_via_driver(&flat, files)
	return transformed_flat.to_files()
}

fn auto_str_generated_fn_names(files []ast.File) []string {
	mut names := []string{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name.ends_with('__str') {
				names << stmt.name
			}
		}
	}
	names.sort()
	return names
}

fn assert_auto_str_names(files []ast.File, expected []string) {
	names := auto_str_generated_fn_names(files)
	for name in expected {
		assert name in names, '${name} missing from generated names ${names}'
	}
}

fn auto_str_generated_fn(files []ast.File, name string) ?ast.FnDecl {
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == name {
				return stmt
			}
		}
	}
	return none
}

fn auto_str_collect_stmt_string_literals(stmt ast.Stmt, mut values []string) {
	match stmt {
		ast.AssignStmt {
			for expr in stmt.lhs {
				auto_str_collect_expr_string_literals(expr, mut values)
			}
			for expr in stmt.rhs {
				auto_str_collect_expr_string_literals(expr, mut values)
			}
		}
		ast.ExprStmt {
			auto_str_collect_expr_string_literals(stmt.expr, mut values)
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				auto_str_collect_expr_string_literals(expr, mut values)
			}
		}
		else {}
	}
}

fn auto_str_collect_expr_string_literals(expr ast.Expr, mut values []string) {
	match expr {
		ast.AsCastExpr {
			auto_str_collect_expr_string_literals(expr.expr, mut values)
			auto_str_collect_expr_string_literals(expr.typ, mut values)
		}
		ast.CallExpr {
			for arg in expr.args {
				auto_str_collect_expr_string_literals(arg, mut values)
			}
		}
		ast.CastExpr {
			auto_str_collect_expr_string_literals(expr.expr, mut values)
			auto_str_collect_expr_string_literals(expr.typ, mut values)
		}
		ast.IfExpr {
			auto_str_collect_expr_string_literals(expr.cond, mut values)
			for nested in expr.stmts {
				auto_str_collect_stmt_string_literals(nested, mut values)
			}
			auto_str_collect_expr_string_literals(expr.else_expr, mut values)
		}
		ast.InfixExpr {
			auto_str_collect_expr_string_literals(expr.lhs, mut values)
			auto_str_collect_expr_string_literals(expr.rhs, mut values)
		}
		ast.ModifierExpr {
			auto_str_collect_expr_string_literals(expr.expr, mut values)
		}
		ast.ParenExpr {
			auto_str_collect_expr_string_literals(expr.expr, mut values)
		}
		ast.PrefixExpr {
			auto_str_collect_expr_string_literals(expr.expr, mut values)
		}
		ast.SelectorExpr {
			auto_str_collect_expr_string_literals(expr.lhs, mut values)
		}
		ast.StringLiteral {
			values << expr.value
		}
		else {}
	}
}

fn auto_str_fn_string_literals(files []ast.File, fn_name string) []string {
	fn_decl := auto_str_generated_fn(files, fn_name) or {
		assert false, 'missing generated function ${fn_name}'
		return []string{}
	}
	mut values := []string{}
	for stmt in fn_decl.stmts {
		auto_str_collect_stmt_string_literals(stmt, mut values)
	}
	return values
}

fn auto_str_has_literal_prefix(values []string, prefix string) bool {
	for value in values {
		if value.starts_with(prefix) {
			return true
		}
	}
	return false
}

fn auto_str_collect_stmt_call_names(stmt ast.Stmt, mut names []string) {
	match stmt {
		ast.AssignStmt {
			for expr in stmt.lhs {
				auto_str_collect_expr_call_names(expr, mut names)
			}
			for expr in stmt.rhs {
				auto_str_collect_expr_call_names(expr, mut names)
			}
		}
		ast.ExprStmt {
			auto_str_collect_expr_call_names(stmt.expr, mut names)
		}
		ast.ForStmt {
			auto_str_collect_stmt_call_names(stmt.init, mut names)
			auto_str_collect_expr_call_names(stmt.cond, mut names)
			auto_str_collect_stmt_call_names(stmt.post, mut names)
			for nested in stmt.stmts {
				auto_str_collect_stmt_call_names(nested, mut names)
			}
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				auto_str_collect_expr_call_names(expr, mut names)
			}
		}
		else {}
	}
}

fn auto_str_collect_expr_call_names(expr ast.Expr, mut names []string) {
	match expr {
		ast.AsCastExpr {
			auto_str_collect_expr_call_names(expr.expr, mut names)
			auto_str_collect_expr_call_names(expr.typ, mut names)
		}
		ast.CallExpr {
			if expr.lhs is ast.Ident {
				lhs := expr.lhs as ast.Ident
				names << lhs.name
			}
			for arg in expr.args {
				auto_str_collect_expr_call_names(arg, mut names)
			}
		}
		ast.CastExpr {
			auto_str_collect_expr_call_names(expr.expr, mut names)
			auto_str_collect_expr_call_names(expr.typ, mut names)
		}
		ast.IfExpr {
			auto_str_collect_expr_call_names(expr.cond, mut names)
			for nested in expr.stmts {
				auto_str_collect_stmt_call_names(nested, mut names)
			}
			auto_str_collect_expr_call_names(expr.else_expr, mut names)
		}
		ast.InfixExpr {
			auto_str_collect_expr_call_names(expr.lhs, mut names)
			auto_str_collect_expr_call_names(expr.rhs, mut names)
		}
		ast.ModifierExpr {
			auto_str_collect_expr_call_names(expr.expr, mut names)
		}
		ast.ParenExpr {
			auto_str_collect_expr_call_names(expr.expr, mut names)
		}
		ast.PrefixExpr {
			auto_str_collect_expr_call_names(expr.expr, mut names)
		}
		ast.SelectorExpr {
			auto_str_collect_expr_call_names(expr.lhs, mut names)
		}
		ast.StringInterLiteral {
			for inter in expr.inters {
				auto_str_collect_expr_call_names(inter.expr, mut names)
				auto_str_collect_expr_call_names(inter.format_expr, mut names)
			}
		}
		else {}
	}
}

fn auto_str_fn_call_names(files []ast.File, fn_name string) []string {
	fn_decl := auto_str_generated_fn(files, fn_name) or {
		assert false, 'missing generated function ${fn_name}'
		return []string{}
	}
	mut names := []string{}
	for stmt in fn_decl.stmts {
		auto_str_collect_stmt_call_names(stmt, mut names)
	}
	return names
}

fn test_array_str_generation_uses_helper_name_for_elem_type_when_metadata_is_stale() {
	env := types.Environment.new()
	mut trans := Transformer.new_with_pref(env, &vpref.Preferences{})
	stmt := trans.generate_array_str_fn('Array_int_str', 'ast__KeywordOperator__str')
	if stmt is ast.FnDecl {
		mut calls := []string{}
		for body_stmt in stmt.stmts {
			auto_str_collect_stmt_call_names(body_stmt, mut calls)
		}
		assert 'int__str' in calls, calls.str()
		assert 'ast__KeywordOperator__str__str' !in calls, calls.str()
		return
	}
	assert false, 'expected generated Array_int_str function'
}

fn auto_str_collect_stmt_selector_names(stmt ast.Stmt, mut names []string) {
	match stmt {
		ast.AssignStmt {
			for expr in stmt.lhs {
				auto_str_collect_expr_selector_names(expr, mut names)
			}
			for expr in stmt.rhs {
				auto_str_collect_expr_selector_names(expr, mut names)
			}
		}
		ast.ExprStmt {
			auto_str_collect_expr_selector_names(stmt.expr, mut names)
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				auto_str_collect_expr_selector_names(expr, mut names)
			}
		}
		else {}
	}
}

fn auto_str_collect_expr_selector_names(expr ast.Expr, mut names []string) {
	match expr {
		ast.AsCastExpr {
			auto_str_collect_expr_selector_names(expr.expr, mut names)
			auto_str_collect_expr_selector_names(expr.typ, mut names)
		}
		ast.CallExpr {
			for arg in expr.args {
				auto_str_collect_expr_selector_names(arg, mut names)
			}
		}
		ast.CastExpr {
			auto_str_collect_expr_selector_names(expr.expr, mut names)
			auto_str_collect_expr_selector_names(expr.typ, mut names)
		}
		ast.IfExpr {
			auto_str_collect_expr_selector_names(expr.cond, mut names)
			for nested in expr.stmts {
				auto_str_collect_stmt_selector_names(nested, mut names)
			}
			auto_str_collect_expr_selector_names(expr.else_expr, mut names)
		}
		ast.InfixExpr {
			auto_str_collect_expr_selector_names(expr.lhs, mut names)
			auto_str_collect_expr_selector_names(expr.rhs, mut names)
		}
		ast.ModifierExpr {
			auto_str_collect_expr_selector_names(expr.expr, mut names)
		}
		ast.ParenExpr {
			auto_str_collect_expr_selector_names(expr.expr, mut names)
		}
		ast.PrefixExpr {
			auto_str_collect_expr_selector_names(expr.expr, mut names)
		}
		ast.SelectorExpr {
			names << expr.rhs.name
			auto_str_collect_expr_selector_names(expr.lhs, mut names)
		}
		ast.StringInterLiteral {
			for inter in expr.inters {
				auto_str_collect_expr_selector_names(inter.expr, mut names)
				auto_str_collect_expr_selector_names(inter.format_expr, mut names)
			}
		}
		else {}
	}
}

fn auto_str_fn_selector_names(files []ast.File, fn_name string) []string {
	fn_decl := auto_str_generated_fn(files, fn_name) or {
		assert false, 'missing generated function ${fn_name}'
		return []string{}
	}
	mut names := []string{}
	for stmt in fn_decl.stmts {
		auto_str_collect_stmt_selector_names(stmt, mut names)
	}
	return names
}

fn assert_auto_str_simple_point_body(files []ast.File) {
	literals := auto_str_fn_string_literals(files, 'Point__str')
	calls := auto_str_fn_call_names(files, 'Point__str')
	assert 'Point{}' !in literals
	assert auto_str_has_literal_prefix(literals, 'Point{')
	assert '    x: ' in literals
	assert '    y: ' in literals
	assert 'strings__Builder__write_string' in calls
	assert 'strings__Builder__str' in calls
}

fn assert_auto_str_recursive_node_body(files []ast.File) {
	literals := auto_str_fn_string_literals(files, 'Node__str')
	calls := auto_str_fn_call_names(files, 'Node__str')
	assert 'Node{}' !in literals
	assert auto_str_has_literal_prefix(literals, 'Node{')
	assert '    value: ' in literals
	assert '    left: ' in literals
	assert '    right: ' in literals
	assert 'Tree__str' in calls
	assert 'string__replace' in calls
}

fn assert_auto_str_tree_sumtype_body(files []ast.File) {
	literals := auto_str_fn_string_literals(files, 'Tree__str')
	calls := auto_str_fn_call_names(files, 'Tree__str')
	selectors := auto_str_fn_selector_names(files, 'Tree__str')
	assert 'Tree(' in literals
	assert ')' in literals
	assert 'Tree{}' in literals
	assert 'Empty__str' in calls
	assert 'Node__str' in calls
	assert '_tag' in selectors
}

fn assert_auto_str_main_uses_node_str(files []ast.File) {
	calls := auto_str_fn_call_names(files, 'main')
	assert 'Node__str' in calls
}

fn test_auto_struct_str_generates_field_struct_helper_from_legacy_and_flat() {
	source := '
module main

struct Point {
	x int
	y int
}

fn show(point Point) string {
	return "\${point}"
}
'
	legacy := auto_str_legacy_generated_files('simple_struct_legacy', source)
	flat := auto_str_flat_generated_files('simple_struct_flat', source)
	assert_auto_str_names(legacy, ['Point__str'])
	assert_auto_str_names(flat, ['Point__str'])
	assert_auto_str_simple_point_body(legacy)
	assert_auto_str_simple_point_body(flat)
}

fn test_auto_sumtype_str_generates_wrapper_and_variant_helpers_from_legacy_and_flat() {
	source := '
module main

type Tree = Empty | Node

struct Empty {}

struct Node {
	value int
}

fn show(tree Tree) string {
	return "\${tree}"
}
'
	legacy := auto_str_legacy_generated_files('sumtype_legacy', source)
	flat := auto_str_flat_generated_files('sumtype_flat', source)
	assert_auto_str_names(legacy, ['Empty__str', 'Node__str', 'Tree__str'])
	assert_auto_str_names(flat, ['Empty__str', 'Node__str', 'Tree__str'])
	assert_auto_str_tree_sumtype_body(legacy)
	assert_auto_str_tree_sumtype_body(flat)
}

fn test_auto_recursive_struct_sumtype_str_generates_helpers_from_legacy_and_flat() {
	source := '
module main

type Tree = Empty | Node

struct Empty {}

struct Node {
	value int
	left  Tree
	right Tree
}

fn show(node Node) string {
	return "\${node}"
}
'
	legacy := auto_str_legacy_generated_files('recursive_legacy', source)
	flat := auto_str_flat_generated_files('recursive_flat', source)
	assert_auto_str_names(legacy, ['Empty__str', 'Node__str', 'Tree__str'])
	assert_auto_str_names(flat, ['Empty__str', 'Node__str', 'Tree__str'])
	assert_auto_str_recursive_node_body(legacy)
	assert_auto_str_recursive_node_body(flat)
	assert_auto_str_tree_sumtype_body(legacy)
	assert_auto_str_tree_sumtype_body(flat)
}

fn test_auto_recursive_struct_sumtype_str_from_call_arg_interpolation_legacy_and_flat() {
	source := '
module main

type Tree = Empty | Node

struct Empty {}

struct Node {
	value int
	left  Tree
	right Tree
}

fn sink(s string) {
	_ = s
}

fn main() {
	leaf := Node{7, Empty{}, Empty{}}
	root := Node{9, leaf, Empty{}}
	sink("\${root}")
}
'
	legacy := auto_str_legacy_generated_files('recursive_call_arg_legacy', source)
	flat := auto_str_flat_generated_files('recursive_call_arg_flat', source)
	assert_auto_str_names(legacy, ['Empty__str', 'Node__str', 'Tree__str'])
	assert_auto_str_names(flat, ['Empty__str', 'Node__str', 'Tree__str'])
	assert_auto_str_main_uses_node_str(legacy)
	assert_auto_str_main_uses_node_str(flat)
	assert_auto_str_recursive_node_body(legacy)
	assert_auto_str_recursive_node_body(flat)
	assert_auto_str_tree_sumtype_body(legacy)
	assert_auto_str_tree_sumtype_body(flat)
}

fn test_auto_recursive_struct_sumtype_str_from_implicit_main_call_arg_legacy_and_flat() {
	source := '
type Tree = Empty | Node

struct Empty {}

struct Node {
	value int
	left  Tree
	right Tree
}

fn sink(s string) {
	_ = s
}

fn main() {
	node1 := Node{30, Empty{}, Empty{}}
	node2 := Node{20, Empty{}, Empty{}}
	tree := Node{10, node1, node2}
	sink("\${tree}")
}
'
	legacy := auto_str_legacy_generated_files('recursive_implicit_main_call_arg_legacy', source)
	flat := auto_str_flat_generated_files('recursive_implicit_main_call_arg_flat', source)
	assert_auto_str_names(legacy, ['Empty__str', 'Node__str', 'Tree__str'])
	assert_auto_str_names(flat, ['Empty__str', 'Node__str', 'Tree__str'])
	assert_auto_str_main_uses_node_str(legacy)
	assert_auto_str_main_uses_node_str(flat)
	assert_auto_str_recursive_node_body(legacy)
	assert_auto_str_recursive_node_body(flat)
	assert_auto_str_tree_sumtype_body(legacy)
	assert_auto_str_tree_sumtype_body(flat)
}
