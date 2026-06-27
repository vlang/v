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

fn test_worker_clone_owns_mutable_maps() {
	prefs := &vpref.Preferences{}
	env := types.Environment.new()
	mut transformer := Transformer.new_with_pref(env, prefs)
	transformer.synth_pos_counter = -73
	scope := types.new_scope(unsafe { nil })
	transformer.elided_fns = {
		'main__skipped': true
	}
	transformer.cached_scopes = {
		'main': scope
	}
	transformer.cached_fn_scopes = {
		'main__known': scope
	}
	transformer.cached_method_keys = ['main__Foo']
	transformer.generic_fn_value_names = {
		'handler': true
	}

	mut worker := transformer.new_worker_clone(1)
	assert worker.next_synth_pos().id == -100073
	worker.elided_fns['main__worker_skipped'] = true
	worker.cached_scopes['worker'] = scope
	worker.cached_fn_scopes['main__worker'] = scope
	worker.cached_method_keys << 'main__Bar'
	worker.generic_fn_value_names['worker_handler'] = true

	assert 'main__worker_skipped' !in transformer.elided_fns
	assert 'worker' !in transformer.cached_scopes
	assert 'main__worker' !in transformer.cached_fn_scopes
	assert transformer.cached_method_keys == ['main__Foo']
	assert 'worker_handler' !in transformer.generic_fn_value_names

	transformer.merge_worker(worker)
	assert transformer.elided_fns['main__worker_skipped']
	assert transformer.cached_fn_scopes['main__worker'] == scope
}

fn transform_code_for_test(code string) []ast.File {
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	return transform_code_with_prefs_for_test(code, prefs)
}

fn transform_code_with_prefs_for_test(code string, prefs &vpref.Preferences) []ast.File {
	tmp_file := '/tmp/v2_transformer_test_${os.getpid()}.v'
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut transformer := Transformer.new_with_pref(env, prefs)
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
	mut transformer := Transformer.new_with_pref(env, prefs)
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
	mut transformer := Transformer.new_with_pref(env, prefs)
	return transformer.transform_files(files)
}

fn transform_sources_with_env_for_test(sources []TestSource) (&types.Environment, []ast.File) {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_transformer_sources_env_${os.getpid()}')
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
	mut transformer := Transformer.new_with_pref(env, prefs)
	transformed := transformer.transform_files(files)
	return env, transformed
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

fn test_get_expr_type_selector_prefers_struct_field_over_stale_pos_type() {
	mut env := types.Environment.new()
	env.set_expr_type(91, types.string_)
	value_kind_type := types.Type(types.Enum{
		name: 'json2__ValueKind'
	})
	value_info_type := types.Type(types.Struct{
		name:   'json2__ValueInfo'
		fields: [
			types.Field{
				name: 'value_kind'
				typ:  value_kind_type
			},
		]
	})
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('struct_info', value_object_from_type(value_info_type))
	mut t := Transformer{
		pref:             &vpref.Preferences{}
		env:              unsafe { env }
		scope:            scope
		local_decl_types: map[string]types.Type{}
	}
	typ := t.get_expr_type(ast.SelectorExpr{
		lhs: ast.Ident{
			name: 'struct_info'
		}
		rhs: ast.Ident{
			name: 'value_kind'
		}
		pos: token.Pos{
			id: 91
		}
	}) or { panic('missing selector type') }
	assert typ is types.Enum
	assert (typ as types.Enum).name == 'json2__ValueKind'
}

fn test_get_expr_type_selector_prefers_struct_field_over_stale_synth_type() {
	value_kind_type := types.Type(types.Enum{
		name: 'json2__ValueKind'
	})
	value_info_type := types.Type(types.Struct{
		name:   'json2__ValueInfo'
		fields: [
			types.Field{
				name: 'value_kind'
				typ:  value_kind_type
			},
		]
	})
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('struct_info', value_object_from_type(value_info_type))
	mut t := Transformer{
		pref:             &vpref.Preferences{}
		env:              unsafe { types.Environment.new() }
		scope:            scope
		local_decl_types: map[string]types.Type{}
	}
	pos := token.Pos{
		id: 92
	}
	t.register_synth_type(pos, types.Type(types.string_))
	typ := t.get_expr_type(ast.SelectorExpr{
		lhs: ast.Ident{
			name: 'struct_info'
		}
		rhs: ast.Ident{
			name: 'value_kind'
		}
		pos: pos
	}) or { panic('missing selector type') }
	assert typ is types.Enum
	assert (typ as types.Enum).name == 'json2__ValueKind'
}

fn test_string_interpolation_prefers_declared_selector_type_over_stale_string_pos() {
	value_kind_type := types.Type(types.Enum{
		name: 'json2__ValueKind'
	})
	value_info_type := types.Type(types.Struct{
		name:   'json2__ValueInfo'
		fields: [
			types.Field{
				name: 'value_kind'
				typ:  value_kind_type
			},
		]
	})
	mut env := types.Environment.new()
	env.set_expr_type(93, types.Type(types.string_))
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('struct_info', value_object_from_type(value_info_type))
	mut t := Transformer{
		pref:                &vpref.Preferences{}
		env:                 unsafe { env }
		scope:               scope
		local_decl_types:    map[string]types.Type{}
		needed_str_fns:      map[string]string{}
		needed_enum_str_fns: map[string]types.Enum{}
		synth_types:         map[int]types.Type{}
	}
	inter_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Ident{
			name: 'struct_info'
		}
		rhs: ast.Ident{
			name: 'value_kind'
		}
		pos: token.Pos{
			id: 93
		}
	})
	inter := ast.StringInter{
		expr: inter_expr
	}
	assert t.resolve_sprintf_format(inter) == '%s'
	arg := t.transform_sprintf_arg(inter)
	assert arg is ast.SelectorExpr
	call := (arg as ast.SelectorExpr).lhs
	assert call is ast.CallExpr
	lhs := (call as ast.CallExpr).lhs
	assert lhs is ast.Ident
	assert (lhs as ast.Ident).name == 'json2__ValueKind__str'

	stale_arg := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.CallExpr{
			lhs:  ast.Ident{
				name: 'string__str'
			}
			args: [inter_expr]
			pos:  token.Pos{
				id: 94
			}
		})
		rhs: ast.Ident{
			name: 'str'
		}
		pos: token.Pos{
			id: 95
		}
	})
	lit := t.transform_string_inter_literal(ast.StringInterLiteral{
		values: ["'Expected object, but got ", "'"]
		inters: [
			ast.StringInter{
				expr: stale_arg
			},
		]
	})
	assert lit is ast.StringInterLiteral
	repaired := (lit as ast.StringInterLiteral).inters[0].expr
	assert repaired is ast.SelectorExpr
	repaired_call := (repaired as ast.SelectorExpr).lhs
	assert repaired_call is ast.CallExpr
	repaired_lhs := (repaired_call as ast.CallExpr).lhs
	assert repaired_lhs is ast.Ident
	assert (repaired_lhs as ast.Ident).name == 'json2__ValueKind__str'

	cloned := t.clone_expr_with_bindings_and_fields(ast.Expr(ast.StringInterLiteral{
		values: ["'Expected object, but got ", "'"]
		inters: [
			ast.StringInter{
				expr: stale_arg
			},
		]
	}), map[string]types.Type{}, []CloneComptimeFieldCtx{})
	assert cloned is ast.StringInterLiteral
	cloned_arg := (cloned as ast.StringInterLiteral).inters[0].expr
	assert cloned_arg is ast.SelectorExpr
	cloned_call := (cloned_arg as ast.SelectorExpr).lhs
	assert cloned_call is ast.CallExpr
	cloned_lhs := (cloned_call as ast.CallExpr).lhs
	assert cloned_lhs is ast.Ident
	assert (cloned_lhs as ast.Ident).name == 'json2__ValueKind__str'
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

fn test_println_rune_lowers_to_rune_str_call() {
	mut t := create_transformer_with_vars({
		'r': types.Type(types.rune_)
	})
	out := t.transform_call_expr(ast.CallExpr{
		lhs:  ast.Ident{
			name: 'println'
		}
		args: [ast.Expr(ast.Ident{
			name: 'r'
		})]
	})
	assert out is ast.CallExpr
	call := out as ast.CallExpr
	assert call.args.len == 1
	assert call.args[0] is ast.CallExpr
	str_call := call.args[0] as ast.CallExpr
	assert str_call.lhs is ast.Ident
	assert (str_call.lhs as ast.Ident).name == 'rune__str'
	assert 'rune__str' in t.needed_str_fns
}

fn test_println_i64_call_or_cast_lowers_to_i64_str_call() {
	mut t := create_test_transformer()
	out := t.transform_call_expr(ast.CallExpr{
		lhs:  ast.Ident{
			name: 'println'
		}
		args: [
			ast.Expr(ast.CallOrCastExpr{
				lhs:  ast.Ident{
					name: 'i64'
				}
				expr: ast.BasicLiteral{
					kind:  .number
					value: '4294967296'
				}
			}),
		]
	})
	assert out is ast.CallExpr
	call := out as ast.CallExpr
	assert call.args.len == 1
	assert call.args[0] is ast.CallExpr
	str_call := call.args[0] as ast.CallExpr
	assert str_call.lhs is ast.Ident
	assert (str_call.lhs as ast.Ident).name == 'i64__str'
	assert 'i64__str' in t.needed_str_fns
}

fn call_names_for_fn_suffix(files []ast.File, fn_suffix string) []string {
	mut names := []string{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name.ends_with(fn_suffix) {
				for nested in stmt.stmts {
					collect_call_names_from_stmt(nested, mut names)
				}
			}
		}
	}
	return names
}

fn test_transform_generic_call_uses_sumtype_match_smartcast_for_inference() {
	files := transform_code_for_test('
struct Point {
	x int
}

type Primitive = []int | []string | []Point | bool | int | string | Point

fn wrap_first[T](values []T) Primitive {
	return Primitive(values[0])
}

fn check(value Primitive) {
	match value {
		[]int {
			_ := wrap_first(value)
		}
		[]string {
			_ := wrap_first(value)
		}
		[]Point {
			_ := wrap_first(value)
		}
		else {}
	}
}
')
	names := call_names_for_fn(files, 'check')
	assert 'wrap_first_T_int' in names
	assert 'wrap_first_T_string' in names
	assert 'wrap_first_T_Point' in names
	assert 'wrap_first' !in names
}

fn test_transform_generic_receiver_methods_use_concrete_specializations() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'x/json2/decode.v'
			code: '
module json2

struct ValueInfo {
	x int
}

struct Node[T] {
mut:
	value T
	next  &Node[T] = unsafe { nil }
}

struct Decoder {
mut:
	values_info LinkedList[ValueInfo]
}

struct LinkedList[T] {
mut:
	head &Node[T] = unsafe { nil }
	tail &Node[T] = unsafe { nil }
	len  int
}

fn (mut list LinkedList[T]) push(value T) {
	_ = value
}

fn (list &LinkedList[T]) last() &T {
	return &list.tail.value
}
'
		},
		TestSource{
			rel:  'x/json2/check.v'
			code: '
module json2

fn (mut checker Decoder) check() {
	mut actual_value_info_pointer := unsafe { nil }
	checker.values_info.push(ValueInfo{})
	actual_value_info_pointer = checker.values_info.last()
	_ = actual_value_info_pointer
}
'
		},
	])
	names := call_names_for_fn(files, 'check')
	assert names.any(it.contains('json2__LinkedList__push_T_') && it.contains('json2_ValueInfo')), 'expected specialized push call, got ${names}'

	assert names.any(it.contains('json2__LinkedList__last_T_') && it.contains('json2_ValueInfo')), 'expected specialized last call, got ${names}'

	assert 'LinkedList__push' !in names
	assert 'LinkedList__last' !in names
	assert 'json2__LinkedList__push' !in names
	assert 'json2__LinkedList__last' !in names
}

fn test_transform_nested_generic_receiver_method_call_in_monomorphized_clone() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'x/json2/encode.v'
			code: '
module json2

pub struct EncoderOptions {}

pub struct Any {}

struct Encoder {}

pub fn encode[T](val T, config EncoderOptions) string {
	_ = config
	mut encoder := Encoder{}
	encoder.encode_value[T](val)
	return ""
}

fn (mut encoder Encoder) encode_value[T](val T) {
	_ = encoder
	_ = val
}
'
		},
		TestSource{
			rel:  'main.v'
			code: '
module main

import json2

fn use() {
	_ = json2.encode(json2.Any{}, json2.EncoderOptions{})
}
'
		},
	])
	mut fn_names := []string{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				fn_names << stmt.name
			}
		}
	}
	assert 'json2__encode_T_json2_Any' in fn_names
	assert 'encode_value_T_json2_Any' in fn_names
	call_names := call_names_for_fn(files, 'json2__encode_T_json2_Any')
	assert 'json2__Encoder__encode_value_T_json2_Any' in call_names
	assert 'encode_value_T_json2_Any' !in call_names
}

fn test_transform_eventbus_receiver_generic_method_call_is_monomorphized() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'eventbus/eventbus.v'
			code: '
module eventbus

pub type EventHandlerFn = fn (receiver voidptr, args voidptr, sender voidptr)

pub struct Subscriber[T] {}

pub fn new_subscriber[T]() Subscriber[T] {
	return Subscriber[T]{}
}

pub fn (mut s Subscriber[T]) subscribe_method(name T, handler EventHandlerFn, receiver voidptr) {
	_ = s
	_ = name
	_ = handler
	_ = receiver
}
'
		},
		TestSource{
			rel:  'main.v'
			code: "
module main

import eventbus

fn handler(receiver voidptr, args voidptr, sender voidptr) {
	_ = receiver
	_ = args
	_ = sender
}

fn use() {
	mut subscriber := eventbus.new_subscriber[string]()
	subscriber.subscribe_method('ready', handler, unsafe { nil })
}
"
		},
	])
	mut fn_names := []string{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				fn_names << stmt.name
			}
		}
	}
	assert 'subscribe_method_T_string' in fn_names

	call_names := call_names_for_fn(files, 'use')
	assert 'eventbus__Subscriber__subscribe_method_T_string' in call_names
	assert 'eventbus__Subscriber__subscribe_method' !in call_names
}

fn test_transform_embedded_method_promotion_keeps_owner_method_precedence() {
	mut t := create_test_transformer()
	t.collect_declared_method_fns([
		ast.File{
			mod:   'main'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:      'redirect_to_index'
					is_method: true
					receiver:  ast.Parameter{
						name: 'ctx'
						typ:  ast.Expr(ast.Ident{
							name: 'Context'
						})
					}
				}),
			]
		},
	])
	recv_type := types.Type(types.Struct{
		name: 'Context'
	})
	resolved := t.resolve_cached_method_fn_name_for_type(recv_type, 'redirect_to_index', 'Context') or {
		panic('missing declared owner method')
	}
	assert resolved == 'Context__redirect_to_index'
	assert 'Context__redirect_to_index' in t.declared_method_fns
}

fn test_generic_receiver_bindings_prefer_declared_selector_over_stale_wrapper_type() {
	value_info_type := types.Type(types.Struct{
		name: 'json2__ValueInfo'
	})
	template_type := types.Type(types.Struct{
		name:           'json2__LinkedList'
		generic_params: ['T']
		fields:         [
			types.Field{
				name: 'item'
				typ:  types.Type(types.NamedType('T'))
			},
		]
	})
	concrete_type := types.Type(types.Struct{
		name:   'json2__LinkedList'
		fields: [
			types.Field{
				name: 'item'
				typ:  value_info_type
			},
		]
	})
	decoder_type := types.Type(types.Struct{
		name:   'json2__Decoder'
		fields: [
			types.Field{
				name: 'values_info'
				typ:  concrete_type
			},
		]
	})
	mut env := types.Environment.new()
	env.set_expr_type(701, template_type)
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('checker', value_object_from_type(decoder_type))
	scope.insert_type('LinkedList', template_type)
	scope.insert_type('json2__LinkedList', template_type)
	mut t := Transformer{
		pref:             &vpref.Preferences{}
		env:              unsafe { env }
		scope:            scope
		cur_module:       'json2'
		cached_scopes:    {
			'json2': scope
		}
		local_decl_types: map[string]types.Type{}
	}
	receiver := ast.Expr(ast.ParenExpr{
		expr: ast.Expr(ast.SelectorExpr{
			lhs: ast.Ident{
				name: 'checker'
			}
			rhs: ast.Ident{
				name: 'values_info'
			}
		})
		pos:  token.Pos{
			id: 701
		}
	})
	decl := ast.FnDecl{
		name:      'last'
		is_method: true
		receiver:  ast.Parameter{
			name: 'list'
			typ:  ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: ast.Expr(ast.GenericArgOrIndexExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'LinkedList'
					})
					expr: ast.Expr(ast.Ident{
						name: 'T'
					})
				})
			})
		}
	}
	bindings := t.generic_bindings_from_method_receiver(decl, receiver, 'json2__LinkedList__last') or {
		panic('missing receiver bindings')
	}
	binding := bindings['T'] or { panic('missing T binding') }
	assert binding is types.Struct
	assert (binding as types.Struct).name == 'json2__ValueInfo'
}

fn test_decl_assign_rune_arithmetic_keeps_rune_storage_type() {
	mut t := create_transformer_with_vars({
		'unicode_point':  types.builtin_type('rune') or { rune_type() }
		'unicode_point2': types.builtin_type('rune') or { rune_type() }
	})
	mut env := types.Environment.new()
	env.set_expr_type(93, types.Type(types.int_))
	env.set_expr_type(94, types.Type(types.int_))
	env.set_expr_type(95, types.Type(types.int_))
	t.env = unsafe { env }
	lhs := ast.Expr(ast.Ident{
		name: 'final_unicode_point'
		pos:  token.Pos{
			id: 93
		}
	})
	cast_rhs := ast.Expr(ast.CastExpr{
		typ:  ast.Expr(ast.Ident{
			name: 'rune'
		})
		expr: ast.Expr(ast.BasicLiteral{
			kind:  .number
			value: '0'
		})
	})
	cast_typ := t.decl_assign_storage_type(lhs, cast_rhs) or { panic('missing cast type') }
	assert cast_typ is types.Rune
	rhs := ast.Expr(ast.InfixExpr{
		op:  .plus
		lhs: ast.Expr(ast.InfixExpr{
			op:  .amp
			lhs: ast.Expr(ast.Ident{
				name: 'unicode_point2'
				pos:  token.Pos{
					id: 94
				}
			})
			rhs: ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0x3FF'
			})
		})
		rhs: ast.Expr(ast.InfixExpr{
			op:  .left_shift
			lhs: ast.Expr(ast.InfixExpr{
				op:  .amp
				lhs: ast.Expr(ast.Ident{
					name: 'unicode_point'
					pos:  token.Pos{
						id: 95
					}
				})
				rhs: ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '0x3FF'
				})
			})
			rhs: ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '10'
			})
		})
	})
	typ := t.decl_assign_storage_type(lhs, rhs) or { panic('missing rune arithmetic type') }
	assert typ is types.Rune
}

fn test_runtime_const_init_main_calls_run_main_consts_after_module_inits() {
	mut t := create_test_transformer()
	t.runtime_const_modules << 'main'
	t.runtime_const_init_fn_name['main'] = '__v_init_consts_main'
	files := [
		ast.File{
			mod:   'main'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'main'
				}),
			]
		},
		ast.File{
			mod:   'rand'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'init'
				}),
			]
		},
	]
	calls := t.runtime_const_init_main_calls_parts(files)
	mut names := []string{}
	for call_stmt in calls {
		collect_call_names_from_stmt(call_stmt, mut names)
	}
	assert names == ['rand__init', '__v_init_consts_main']
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

fn test_promoted_embedded_generic_method_call_lowers_to_concrete_method() {
	files := transform_code_for_test('
struct Options[T] {
	handler fn (mut ctx T) bool
}

struct Middleware[T] {}

fn (mut m Middleware[T]) use(options Options[T]) {
	_ = options
}

struct Context {}

struct App {
	Middleware[Context]
}

fn (mut app App) before_request(mut ctx Context) bool {
	_ = ctx
	return true
}

fn main() {
	mut app := App{}
	app.use(handler: app.before_request)
}
')
	mut fn_names := []string{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				fn_names << stmt.name
			}
		}
	}
	call_names := call_names_for_fn(files, 'main')
	assert 'use_T_Context' in fn_names
	assert 'Middleware__use_T_Context' in call_names
	assert 'app.use' !in call_names
}

fn test_promoted_embedded_generic_method_uses_live_struct_metadata() {
	files := transform_code_for_test('
struct Result {}

struct BaseContext {}

fn (mut ctx BaseContext) json[T](j T) Result {
	_ = j
	return Result{}
}

struct Context {
	BaseContext
}

struct App {}

fn (mut app App) index(mut ctx Context) Result {
	_ = app
	return ctx.json("ok")
}

fn main() {
	mut app := App{}
	mut ctx := Context{}
	_ = app.index(mut ctx)
}
')
	call_names := call_names_for_fn(files, 'index')
	assert 'BaseContext__json_T_string' in call_names
	assert 'ctx.json' !in call_names
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

fn test_string_interpolation_does_not_generate_default_over_explicit_str_method() {
	files := transform_code_for_test('
module globset

pub struct ErrorKind {}

pub fn (kind ErrorKind) str() string {
	_ = kind
	return "custom"
}

pub fn message(kind ErrorKind) string {
	return "\${kind}"
}
')
	mut explicit_methods := 0
	mut generated_defaults := 0
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				if stmt.is_method && stmt.name == 'str' {
					explicit_methods++
				}
				if !stmt.is_method && stmt.name == 'globset__ErrorKind__str' {
					generated_defaults++
				}
			}
		}
	}
	call_names := call_names_for_fn(files, 'message')
	assert explicit_methods == 1
	assert generated_defaults == 0
	assert 'globset__ErrorKind__str' in call_names
}

fn test_array_str_call_uses_element_specific_helper() {
	files := transform_code_for_test('
module globset

pub struct Token {}

pub fn (tok Token) str() string {
	_ = tok
	return "Token"
}

pub fn show(tokens []Token) string {
	return tokens.str()
}
')
	call_names := call_names_for_fn(files, 'show')
	assert 'Array_globset__Token_str' in call_names
	assert 'array__str' !in call_names
}

fn test_string_array_str_call_keeps_builtin_method() {
	files := transform_code_for_test('
module builtin

fn (a []string) str() string {
	_ = a
	return ""
}

fn show(values []string) string {
	return values.str()
}
')
	call_names := call_names_for_fn(files, 'show')
	assert 'Array_string__str' in call_names
	assert 'Array_string_str' !in call_names
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

pub __global global_table = &Table(unsafe { nil })

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
	return ast.global_table.type_to_str(node.receiver.typ)
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

fn test_transform_monomorphizes_imported_generic_module_call() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'x/json2/decode.v'
			code: '
module json2

struct Decoder {}

fn (mut decoder Decoder) decode_value[T](mut value T) ! {
	_ = value
}

pub fn decode[T]() !T {
	mut decoder := Decoder{}
	mut result := T{}
	decoder.decode_value(mut result)!
	return result
}
'
		},
		TestSource{
			rel:  'main.v'
			code: '
module main

import x.json2

struct Payload {
	value int
}

fn main() {
	_ := json2.decode[Payload]() or { Payload{} }
}
'
		},
	])
	call_names := call_names_for_fn(files, 'main')
	assert 'json2__decode_T_Payload' in call_names
	assert 'decode_T_Payload' !in call_names

	mut found_prefixed_clone := false
	mut found_unprefixed_clone := false
	mut found_method_clone := false
	mut found_generic_decl := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				if stmt.name == 'json2__decode_T_Payload' {
					found_prefixed_clone = true
					assert stmt.typ.generic_params.len == 0
				}
				if stmt.name == 'decode_T_Payload' {
					found_unprefixed_clone = true
				}
				if stmt.name == 'decode_value_T_Payload' {
					found_method_clone = true
					assert stmt.typ.generic_params.len == 0
				}
				if stmt.name == 'decode' && decl_generic_param_names(stmt).len > 0 {
					found_generic_decl = true
				}
			}
		}
	}
	assert found_prefixed_clone
	assert !found_unprefixed_clone
	assert found_method_clone
	assert !found_generic_decl
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

fn test_transform_transitive_generic_call_in_clone_emits_callee_clone() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'webx/webx.v'
			code: 'module webx

pub fn run_at[A, X]() ! {
	run_new[A, X]()!
}

pub fn run_new[A, X]() ! {}
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module main

import webx

struct App {}
struct Context {}

fn boot() {
	webx.run_at[App, Context]() or {}
}
'
		},
	])
	mut fn_names := []string{}
	mut webx_fn_names := []string{}
	mut main_fn_names := []string{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				fn_names << stmt.name
				if file.mod == 'webx' {
					webx_fn_names << stmt.name
				}
				if file.mod == 'main' {
					main_fn_names << stmt.name
				}
			}
		}
	}
	assert 'webx__run_at_T_App_Context' in fn_names
	assert 'webx__run_new_T_App_Context' in fn_names
	assert 'webx__run_at_T_App_Context' in main_fn_names
	assert 'webx__run_new_T_App_Context' in main_fn_names
	assert 'webx__run_at_T_App_Context' !in webx_fn_names
	assert 'webx__run_new_T_App_Context' !in webx_fn_names
}

fn test_transform_transitive_generic_call_with_mut_arg_in_clone_emits_callee_clone() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'webx/webx.v'
			code: 'module webx

pub struct RunParams {}

pub fn run_at[A, X](mut global_app A, params RunParams) ! {
	run_new[A, X](mut global_app, params)!
}

pub fn run_new[A, X](mut global_app A, params RunParams) ! {}
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module main

import webx

struct App {}
struct Context {}

fn boot() {
	mut app := App{}
	webx.run_at[App, Context](mut app, webx.RunParams{}) or {}
}
'
		},
	])
	mut fn_names := []string{}
	mut webx_fn_names := []string{}
	mut main_fn_names := []string{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				fn_names << stmt.name
				if file.mod == 'webx' {
					webx_fn_names << stmt.name
				}
				if file.mod == 'main' {
					main_fn_names << stmt.name
				}
			}
		}
	}
	assert 'webx__run_at_T_App_Context' in fn_names
	assert 'webx__run_new_T_App_Context' in fn_names
	assert 'webx__run_at_T_App_Context' in main_fn_names
	assert 'webx__run_new_T_App_Context' in main_fn_names
	assert 'webx__run_at_T_App_Context' !in webx_fn_names
	assert 'webx__run_new_T_App_Context' !in webx_fn_names
}

fn test_transform_transitive_generic_function_value_in_clone_emits_full_callee_clone() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'webx/webx.v'
			code: 'module webx

pub struct ServerConfig {
	handler fn (req int) int
}

pub struct Context {}

pub fn run_at[A, X]() {
	run_new[A, X]()
}

pub fn run_new[A, X]() {
	_ := ServerConfig{
		handler: parallel_request_handler[A, X]
	}
}

fn parallel_request_handler[A, X](req int) int {
	return route[A, X](req)
}

fn route[A, X](req int) int {
	return req
}
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module main

import webx

struct App {}
struct Context {}

fn boot() {
	webx.run_at[App, Context]()
}
'
		},
	])
	mut main_fn_names := []string{}
	mut handler_name := ''
	for file in files {
		if file.mod != 'main' {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				main_fn_names << stmt.name
				if stmt.name == 'webx__run_new_T_App_Context' {
					assign := stmt.stmts[0] as ast.AssignStmt
					init := assign.rhs[0] as ast.InitExpr
					if init.fields[0].value is ast.Ident {
						handler_name = (init.fields[0].value as ast.Ident).name
					} else {
						handler_name = init.fields[0].value.type_name()
					}
				}
			}
		}
	}
	assert 'webx__run_new_T_App_Context' in main_fn_names
	assert handler_name == 'webx__parallel_request_handler_T_App_Context'
	assert 'webx__parallel_request_handler_T_App_Context' in main_fn_names
	assert 'webx__route_T_App_Context' in main_fn_names
}

fn test_transform_imported_clone_substituted_init_type_keeps_declaring_module_context() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'webx/webx.v'
			code: 'module webx

pub struct Context {}

pub fn make[A, X]() {
	ctx := &Context{}
	mut user_context := X{
		Context: ctx
	}
	route[A, X](mut user_context)
}

fn route[A, X](mut user_context X) {
	_ = user_context
}
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module main

import webx

struct App {}

struct Context {
	webx.Context
}

fn boot() {
	webx.make[App, Context]()
}
'
		},
	])
	mut init_type_name := ''
	mut main_fn_names := []string{}
	for file in files {
		if file.mod != 'main' {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'webx__make_T_App_Context' {
				main_fn_names << stmt.name
				assign := stmt.stmts[1] as ast.AssignStmt
				init := assign.rhs[0] as ast.InitExpr
				init_type := init.typ as ast.Ident
				init_type_name = init_type.name
			} else if stmt is ast.FnDecl {
				main_fn_names << stmt.name
			}
		}
	}
	assert init_type_name == 'Context'
	assert 'webx__route_T_App_Context' in main_fn_names
}

fn test_transform_generic_struct_clone_qualifies_module_type_arg_in_fn_field() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'veb/veb.v'
			code: 'module veb

pub struct Context {}

pub struct MiddlewareOptions[T] {
	handler fn (mut ctx T) bool
}

pub fn make[T]() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{}
}
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module main

import veb

struct Context {}

fn boot() {
	_ := veb.make[veb.Context]()
}
'
		},
	])
	mut clone_name := ''
	mut handler_param_name := ''
	for file in files {
		if file.mod != 'veb' {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.StructDecl && stmt.name == 'MiddlewareOptions_T_veb_Context' {
				clone_name = stmt.name
				field_typ := stmt.fields[0].typ as ast.Type
				fn_typ := field_typ as ast.FnType
				param_ident := fn_typ.params[0].typ as ast.Ident
				handler_param_name = param_ident.name
			}
		}
	}
	assert clone_name == 'MiddlewareOptions_T_veb_Context'
	assert handler_param_name == 'veb__Context'
}

fn test_transform_embedded_generic_method_options_do_not_emit_bare_import_context() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'veb/veb.v'
			code: 'module veb

pub struct Context {}

pub struct Middleware[T] {}

@[params]
pub struct MiddlewareOptions[T] {
	handler fn (mut ctx T) bool
}

pub fn (mut m Middleware[T]) use(options MiddlewareOptions[T]) {
	_ = options
}
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module main

import veb

struct App {
	veb.Middleware[Context]
}

struct Context {
	veb.Context
}

fn (app &App) before_request(mut ctx Context) bool {
	return true
}

fn boot() {
	mut app := App{}
	app.use(handler: app.before_request)
}
'
		},
	])
	mut import_handler_param := ''
	mut main_handler_param := ''
	for file in files {
		for stmt in file.stmts {
			match stmt {
				ast.StructDecl {
					if !stmt.name.contains('MiddlewareOptions_T') {
						continue
					}
					field_typ := stmt.fields[0].typ as ast.Type
					fn_typ := field_typ as ast.FnType
					param_ident := fn_typ.params[0].typ as ast.Ident
					if file.mod == 'veb' && stmt.name == 'MiddlewareOptions_T_veb_Context' {
						import_handler_param = param_ident.name
					}
					if file.mod == 'main' && stmt.name == 'veb__MiddlewareOptions_T_Context' {
						main_handler_param = param_ident.name
					}
				}
				else {}
			}
		}
	}
	assert import_handler_param == '' || import_handler_param == 'veb__Context', 'import_handler_param=${import_handler_param}, main_handler_param=${main_handler_param}'

	assert main_handler_param == '' || main_handler_param == 'Context', 'import_handler_param=${import_handler_param}, main_handler_param=${main_handler_param}'
}

fn test_transform_imported_generic_struct_with_local_type_stays_in_call_file() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'json2/json2.v'
			code: 'module json2

pub struct StructKeyDecodeResult[T] {
	value T
}

pub fn decode[T]() StructKeyDecodeResult[T] {
	return StructKeyDecodeResult[T]{}
}
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module main

import json2

struct LocalResponse {}

fn boot() {
	_ := json2.decode[LocalResponse]()
}
'
		},
	])
	mut import_struct_found := false
	mut import_fn_found := false
	mut main_struct_found := false
	mut main_fn_found := false
	mut main_field_typ := ''
	for file in files {
		for stmt in file.stmts {
			match stmt {
				ast.StructDecl {
					if stmt.name == 'json2__StructKeyDecodeResult_T_LocalResponse'
						|| stmt.name == 'StructKeyDecodeResult_T_LocalResponse' {
						if file.mod == 'json2' {
							import_struct_found = true
						}
						if file.mod == 'main'
							&& stmt.name == 'json2__StructKeyDecodeResult_T_LocalResponse' {
							main_struct_found = true
							field_typ := stmt.fields[0].typ as ast.Ident
							main_field_typ = field_typ.name
						}
					}
				}
				ast.FnDecl {
					if stmt.name == 'json2__decode_T_LocalResponse'
						|| stmt.name == 'decode_T_LocalResponse' {
						if file.mod == 'json2' {
							import_fn_found = true
						}
						if file.mod == 'main' && stmt.name == 'json2__decode_T_LocalResponse' {
							main_fn_found = true
						}
					}
				}
				else {}
			}
		}
	}
	assert !import_struct_found
	assert !import_fn_found
	assert main_struct_found
	assert main_fn_found
	assert main_field_typ == 'LocalResponse'
}

fn test_transform_generic_struct_emits_module_clone_after_file_local_clone() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'other/other.v'
			code: 'module other

pub struct Type {}
'
		},
		TestSource{
			rel:  'm/m.v'
			code: 'module m

import other

pub struct Box[T] {
	value T
}

pub fn use_external() Box[other.Type] {
	return Box[other.Type]{}
}

pub fn use_int() Box[int] {
	return Box[int]{}
}
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module main

import m

fn boot() {
	_ := m.use_external()
	_ := m.use_int()
}
'
		},
	])
	mut m_structs := []string{}
	for file in files {
		if file.mod != 'm' {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				m_structs << stmt.name
			}
		}
	}
	assert 'Box_T_other_Type' in m_structs
	assert 'Box_T_int' in m_structs
}

fn test_transform_moved_generic_struct_qualifies_source_module_field_types() {
	files := transform_sources_for_test([
		TestSource{
			rel:  'boxlib/boxlib.v'
			code: 'module boxlib

pub struct Helper {}

pub struct Box[T] {
	Helper
	helper Helper
	values []Helper
	value T
}

pub fn make[T]() Box[T] {
	return Box[T]{}
}
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module main

import boxlib

struct LocalResponse {}

fn boot() {
	_ := boxlib.make[LocalResponse]()
}
'
		},
	])
	mut moved_struct_found := false
	mut embedded_typ := ''
	mut helper_typ := ''
	mut values_elem_typ := ''
	mut value_typ := ''
	for file in files {
		if file.mod != 'main' {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.StructDecl && stmt.name == 'boxlib__Box_T_LocalResponse' {
				moved_struct_found = true
				if stmt.embedded.len > 0 {
					embedded_ident := stmt.embedded[0] as ast.Ident
					embedded_typ = embedded_ident.name
				}
				for field in stmt.fields {
					match field.name {
						'helper' {
							helper_ident := field.typ as ast.Ident
							helper_typ = helper_ident.name
						}
						'values' {
							array_typ := field.typ as ast.Type
							values_array := array_typ as ast.ArrayType
							values_elem_ident := values_array.elem_type as ast.Ident
							values_elem_typ = values_elem_ident.name
						}
						'value' {
							value_ident := field.typ as ast.Ident
							value_typ = value_ident.name
						}
						else {}
					}
				}
			}
		}
	}
	assert moved_struct_found
	assert embedded_typ == 'boxlib__Helper'
	assert helper_typ == 'boxlib__Helper'
	assert values_elem_typ == 'boxlib__Helper'
	assert value_typ == 'LocalResponse'
}

fn test_transform_transitive_imported_clone_substitutes_nested_generic_route_context() {
	env, files := transform_sources_with_env_for_test([
		TestSource{
			rel:  'webx/webx.v'
			code: 'module webx

pub struct Request {
	route int
}

pub struct ServerConfig {
	handler fn (req Request) &Context
}

pub struct Context {
	current_path string
}

pub fn run_new[A, X](mut global_app A) {
	_ := ServerConfig{
		handler: parallel_request_handler[A, X]
	}
	_ = global_app
}

fn parallel_request_handler[A, X](req Request) &Context {
	mut app := A{}
	return handle_request_and_route[A, X](mut app, req)
}

fn handle_request_and_route[A, X](mut app A, req Request) &Context {
	mut ctx := &Context{}
	mut user_context := X{
		Context: ctx
	}
	handle_route[A, X](mut app, mut user_context, req.route)
	return ctx
}

fn handle_route[A, X](mut app A, mut user_context X, route int) {
	_ = app
	_ = user_context
	_ = route
}
'
		},
		TestSource{
			rel:  'main.v'
			code: 'module main

import webx

struct App {}

struct Context {
	webx.Context
}

fn boot() {
	mut app := App{}
	webx.run_new[App, Context](mut app)
}
'
		},
	])
	mut main_fn_names := []string{}
	mut init_type_name := ''
	mut route_call_name := ''
	for file in files {
		if file.mod != 'main' {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				main_fn_names << stmt.name
				if stmt.name == 'webx__handle_request_and_route_T_App_Context' {
					for inner in stmt.stmts {
						if inner is ast.AssignStmt && inner.lhs.len == 1 && inner.rhs.len == 1 {
							lhs_name := ident_name_from_expr_for_test(inner.lhs[0]) or { '' }
							if lhs_name == 'user_context' && inner.rhs[0] is ast.InitExpr {
								init := inner.rhs[0] as ast.InitExpr
								init_type := init.typ as ast.Ident
								init_type_name = init_type.name
							}
						}
						if inner is ast.ExprStmt && inner.expr is ast.CallExpr {
							call := inner.expr as ast.CallExpr
							if call.lhs is ast.Ident {
								route_call_name = (call.lhs as ast.Ident).name
							} else {
								route_call_name = call.lhs.type_name()
							}
						}
					}
				}
			}
		}
	}
	assert 'webx__parallel_request_handler_T_App_Context' in main_fn_names
	assert 'webx__handle_request_and_route_T_App_Context' in main_fn_names
	assert 'webx__handle_route_T_App_Context' in main_fn_names
	assert init_type_name == 'Context'
	assert route_call_name == 'webx__handle_route_T_App_Context'
	route_scope := env.get_fn_scope('main', 'webx__handle_route_T_App_Context') or {
		panic('missing imported route clone scope')
	}
	user_context_type := route_scope.lookup_var_type('user_context') or {
		panic('missing user_context type')
	}
	assert user_context_type.name() == 'Context'
}

fn test_transform_inferred_generic_method_call_uses_specialized_name() {
	files := transform_code_for_test('
module main

struct Job {}

struct Item {
	value int
}

struct Out {
mut:
	value int
}

fn fill(item Item, mut out Out) {
	out.value = item.value
}

fn (job &Job) apply[K, D, F](items []K, mut out []D, f F) {
	_ = job
	if items.len > 0 && out.len > 0 {
		f(items[0], mut out[0])
	}
}

fn use_apply() int {
	job := Job{}
	items := [Item{
		value: 7
	}]
	mut out := []Out{len: 1}
	job.apply(items, mut out, fill)
	return out[0].value
}
')
	call_names := call_names_for_fn(files, 'use_apply')
	assert 'Job__apply_T_Item_Out_fn_item_Item_out_Out_void' in call_names
	assert 'Job__apply' !in call_names

	mut found_clone := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'apply_T_Item_Out_fn_item_Item_out_Out_void' {
				found_clone = true
				assert stmt.typ.generic_params.len == 0
				assert stmt.typ.params.len == 3
				assert stmt.typ.params[2].typ is ast.Type
				fn_param_type := stmt.typ.params[2].typ as ast.Type
				assert fn_param_type is ast.FnType
			}
		}
	}
	assert found_clone
}

fn test_transform_non_generic_method_does_not_match_generic_method_short_name() {
	files := transform_code_for_test('
module main

struct Vec3[T] {
	x T
	y T
	z T
}

fn (v Vec3[T]) div(u Vec3[T]) Vec3[T] {
	_ = u
	return v
}

struct Float3 {
	x f32
	y f32
	z f32
}

fn (a Float3) div(s f32) Float3 {
	_ = s
	return a
}

fn use_float(a Float3) {
	_ = a.div(f32(2))
}
')
	call_names := call_names_for_fn(files, 'use_float')
	assert 'Float3__div' in call_names
	assert 'Float3__div_T_f32' !in call_names
}

fn test_transform_inferred_generic_fn_call_uses_specialized_fn_arg_name() {
	files := transform_code_for_test('
module main

struct Alpha {}

fn use_alpha(mut a Alpha) {
	_ = a
}

fn call_cb[T, F](mut value T, cb F) {
	cb(mut value)
}

fn main() {
	mut a := Alpha{}
	call_cb(mut a, use_alpha)
}
')
	call_names := call_names_for_fn(files, 'main')
	assert 'call_cb_T_Alpha_fn_a_Alpha_void' in call_names
	assert 'call_cb' !in call_names
	assert 'call_cb_T_Alpha_voidptr' !in call_names
}

fn test_transform_records_inferred_generic_binding_before_monomorphize() {
	files := transform_code_for_test('
module main

fn clamp[T](a T, x T, b T) T {
	mut min := T(0)
	if x < b {
		min = x
	} else {
		min = b
	}
	return if min < a { a } else { min }
}

fn main() {
	ratio := f32(0.5)
	_ = clamp(f32(0), ratio, 1.0)
}
')
	call_names := call_names_for_fn(files, 'main')
	assert 'clamp_T_f32' in call_names
	mut found_clone := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'clamp_T_f32' {
				found_clone = true
				assert stmt.typ.generic_params.len == 0
				assert stmt.stmts.len > 0
			}
		}
	}
	assert found_clone
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

fn test_transform_autogenerates_chained_clone_helpers_from_snapshot() {
	files := transform_code_for_test('
interface IClone {}

struct Leaf implements IClone {
	name string
}

struct Branch implements IClone {
	leaf Leaf
}

struct Root implements IClone {
	branch Branch
}

fn use_clone(value Root) Root {
	return value.clone()
}
')
	assert files.len == 1
	file := files[0]
	mut clone_names := map[string]bool{}
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			clone_names[stmt.name] = true
		}
	}
	assert clone_names['Root__clone']
	assert clone_names['Branch__clone']
	assert clone_names['Leaf__clone']
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

fn test_transform_index_expr_fixed_array_slice_uses_start_address_and_length() {
	mut t := create_transformer_with_vars({
		'f': types.Type(types.ArrayFixed{
			len:       5
			elem_type: types.int_
		})
	})

	expr := ast.IndexExpr{
		lhs:  ast.Ident{
			name: 'f'
		}
		expr: ast.RangeExpr{
			op:    .dotdot
			start: ast.BasicLiteral{
				kind:  .number
				value: '1'
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
	assert (call.lhs as ast.Ident).name == 'new_array_from_c_array'
	assert call.args.len == 4
	assert call.args[0] is ast.InfixExpr
	len_expr := call.args[0] as ast.InfixExpr
	assert len_expr.op == .minus
	assert len_expr.lhs is ast.BasicLiteral
	assert (len_expr.lhs as ast.BasicLiteral).value == '4'
	assert len_expr.rhs is ast.BasicLiteral
	assert (len_expr.rhs as ast.BasicLiteral).value == '1'
	assert call.args[1] is ast.InfixExpr
	assert call.args[3] is ast.PrefixExpr
	ptr_expr := call.args[3] as ast.PrefixExpr
	assert ptr_expr.op == .amp
	assert ptr_expr.expr is ast.IndexExpr
	index_expr := ptr_expr.expr as ast.IndexExpr
	assert index_expr.lhs is ast.Ident
	assert (index_expr.lhs as ast.Ident).name == 'f'
	assert index_expr.expr is ast.BasicLiteral
	assert (index_expr.expr as ast.BasicLiteral).value == '1'
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

fn test_type_to_c_decl_name_preserves_float_primitives() {
	t := create_test_transformer()
	assert t.type_to_c_decl_name(types.Type(types.Primitive{
		props: .float
		size:  32
	})) == 'f32'
	assert t.type_to_c_decl_name(types.Type(types.f64_)) == 'f64'
	assert t.type_to_c_decl_name(types.Type(types.Pointer{
		base_type: types.Type(types.f64_)
	})) == 'f64*'
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

fn test_resolve_expr_with_expected_type_casts_none_to_option() {
	mut t := create_test_transformer()
	expected := types.Type(types.OptionType{
		base_type: types.Type(types.Array{
			elem_type: types.Type(types.u8_)
		})
	})
	resolved := t.resolve_expr_with_expected_type(ast.Expr(ast.Type(ast.NoneType{})), expected)
	assert resolved is ast.CastExpr
	cast := resolved as ast.CastExpr
	assert cast.expr is ast.Type
	assert cast.typ is ast.Type
	assert cast.typ as ast.Type is ast.OptionType
}

fn test_transform_return_match_branch_resolves_enum_shorthand_to_return_type() {
	enum_typ := types.Type(types.Enum{
		name: 'CompletionType'
	})
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('CompletionType', enum_typ)
	mut t := create_test_transformer()
	t.cur_module = 'main'
	t.cur_fn_ret_type_name = 'CompletionType'
	t.preserve_match_branch_value = true
	t.cached_scopes = {
		'main': scope
	}
	stmts := [
		ast.Stmt(ast.ExprStmt{
			expr: ast.Expr(ast.SelectorExpr{
				lhs: ast.empty_expr
				rhs: ast.Ident{
					name: 'encoding'
				}
			})
		}),
	]
	transformed := t.transform_match_branch_stmts(stmts)
	assert transformed.len == 1
	assert transformed[0] is ast.ExprStmt
	expr := (transformed[0] as ast.ExprStmt).expr
	assert expr is ast.Ident
	assert (expr as ast.Ident).name == 'CompletionType__encoding'
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

fn test_transform_map_index_push_generic_arg_or_index_lowers_to_map_get_and_set() {
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
			lhs: ast.GenericArgOrIndexExpr{
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
		assert false, 'expected ambiguous map index push to be transformed'
		return
	}

	assert result is ast.ExprStmt
	expr_stmt := result as ast.ExprStmt
	assert expr_stmt.expr is ast.CallExpr
	mut call_names := []string{}
	collect_call_names_from_expr(expr_stmt.expr, mut call_names)
	assert 'map__get_and_set' in call_names
}

fn test_transform_map_index_push_generic_args_lowers_to_map_get_and_set() {
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
			lhs: ast.GenericArgs{
				lhs:  ast.Ident{
					name: 'lists'
				}
				args: [
					ast.Expr(ast.BasicLiteral{
						kind:  .number
						value: '2'
					}),
				]
			}
			rhs: ast.BasicLiteral{
				kind:  .number
				value: '1'
			}
		}
	}) or {
		assert false, 'expected generic-args map index push to be transformed'
		return
	}

	assert result is ast.ExprStmt
	expr_stmt := result as ast.ExprStmt
	assert expr_stmt.expr is ast.CallExpr
	mut call_names := []string{}
	collect_call_names_from_expr(expr_stmt.expr, mut call_names)
	assert 'map__get_and_set' in call_names
}

fn test_transform_map_index_push_refuses_pointer_map_value() {
	array_int_type := types.Type(types.Array{
		elem_type: types.int_
	})
	mut t := create_transformer_with_vars({
		'lists': types.Type(types.Map{
			key_type:   types.int_
			value_type: types.Type(types.Pointer{
				base_type: array_int_type
			})
		})
	})

	if _ := t.try_transform_map_index_push(ast.ExprStmt{
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
	})
	{
		assert false, 'expected pointer map value push not to be transformed'
	}
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

fn test_transform_addr_of_call_or_cast_selector_lowers_to_pointer_cast_selector() {
	mut t := create_test_transformer()
	result := t.transform_expr(ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.CallOrCastExpr{
				lhs:  ast.Expr(ast.Ident{
					name: 'int'
				})
				expr: ast.Expr(ast.Ident{
					name: 'raw'
				})
			})
			rhs: ast.Ident{
				name: 'field'
			}
		})
	}))
	assert result is ast.SelectorExpr, 'expected SelectorExpr, got ${result.type_name()}'
	selector := result as ast.SelectorExpr
	assert selector.lhs is ast.CastExpr, 'selector lhs was not reduced: ${selector.lhs.type_name()}'
	cast := selector.lhs as ast.CastExpr
	assert cast.typ is ast.PrefixExpr
	cast_typ := cast.typ as ast.PrefixExpr
	assert cast_typ.op == .amp
	assert cast_typ.expr is ast.Ident
	assert (cast_typ.expr as ast.Ident).name == 'int'
	assert cast.expr is ast.Ident
	assert (cast.expr as ast.Ident).name == 'raw'
	assert selector.rhs.name == 'field'
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

fn test_map_value_temp_expr_keeps_existing_sumtype_value() {
	sum_type := types.Type(types.SumType{
		name:     'types.Type'
		variants: [types.Type(types.string_), types.Type(types.int_)]
	})
	mut t := create_transformer_with_vars({
		'v': sum_type
	})
	expr := t.map_value_temp_expr(ast.Expr(ast.Ident{
		name: 'v'
	}), sum_type)
	assert expr is ast.Ident
	assert (expr as ast.Ident).name == 'v'
}

fn test_map_value_temp_expr_keeps_short_named_sumtype_value() {
	sum_type := types.Type(types.SumType{
		name:     'types.Type'
		variants: [types.Type(types.string_), types.Type(types.int_)]
	})
	mut t := create_transformer_with_vars({
		'v': types.Type(types.NamedType('Type'))
	})
	expr := t.map_value_temp_expr(ast.Expr(ast.Ident{
		name: 'v'
	}), sum_type)
	assert expr is ast.Ident
	assert (expr as ast.Ident).name == 'v'
}

fn test_for_in_var_registration_replaces_reused_loop_value_type() {
	sum_type := types.Type(types.SumType{
		name:     'types.Type'
		variants: [types.Type(types.string_), types.Type(types.int_)]
	})
	mut t := create_transformer_with_vars({})
	t.register_for_in_var_type('v', types.Type(types.string_))
	t.register_for_in_var_type('v', sum_type)
	expr := t.map_value_temp_expr(ast.Expr(ast.Ident{
		name: 'v'
	}), sum_type)
	assert expr is ast.Ident
	assert (expr as ast.Ident).name == 'v'
}

fn test_decl_assign_index_expr_records_lhs_type() {
	mut t := create_transformer_with_vars({
		'values': types.Type(types.Array{
			elem_type: types.int_
		})
	})
	lhs_pos := token.Pos{
		id: 4242
	}
	index_pos := token.Pos{
		id: 4243
	}
	t.env.set_expr_type(index_pos.id, types.Type(types.f64_))
	transformed := t.transform_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'first'
			pos:  lhs_pos
		})]
		rhs: [
			ast.Expr(ast.IndexExpr{
				lhs:  ast.Expr(ast.Ident{
					name: 'values'
				})
				expr: ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '0'
				})
				pos:  index_pos
			}),
		]
	})
	assert transformed.op == .decl_assign
	local_type := t.lookup_local_decl_type('first') or {
		assert false, 'missing local declaration type for index expression'
		return
	}
	assert t.type_to_c_name(local_type) == 'int'
	synth_type := t.get_synth_type(lhs_pos) or {
		assert false, 'missing synth type for index declaration lhs'
		return
	}
	assert t.type_to_c_name(synth_type) == 'int'
}

fn test_unsafe_cast_deref_prefers_cast_target_over_stale_env_type() {
	mut t := create_test_transformer()
	unsafe_pos := token.Pos{
		id: 4250
	}
	deref_pos := token.Pos{
		id: 4251
	}
	t.env.set_expr_type(unsafe_pos.id, types.Type(types.f64_))
	t.env.set_expr_type(deref_pos.id, types.Type(types.f64_))
	typ := t.get_expr_type(ast.UnsafeExpr{
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.PrefixExpr{
					op:   .mul
					expr: ast.Expr(ast.PrefixExpr{
						op:   .amp
						expr: ast.Expr(ast.CallOrCastExpr{
							lhs:  ast.Expr(ast.Ident{
								name: 'int'
							})
							expr: ast.Expr(ast.Ident{
								name: 'raw'
							})
						})
					})
					pos:  deref_pos
				})
			}),
		]
		pos:   unsafe_pos
	}) or {
		assert false, 'missing unsafe cast deref type'
		return
	}
	assert t.type_to_c_name(typ) == 'int'
}

fn test_decl_assign_unsafe_cast_deref_records_lhs_type() {
	mut t := create_test_transformer()
	lhs_pos := token.Pos{
		id: 4260
	}
	unsafe_pos := token.Pos{
		id: 4261
	}
	deref_pos := token.Pos{
		id: 4262
	}
	t.env.set_expr_type(unsafe_pos.id, types.Type(types.f64_))
	t.env.set_expr_type(deref_pos.id, types.Type(types.f64_))
	transformed := t.transform_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'x'
			pos:  lhs_pos
		})]
		rhs: [
			ast.Expr(ast.UnsafeExpr{
				stmts: [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.PrefixExpr{
							op:   .mul
							expr: ast.Expr(ast.PrefixExpr{
								op:   .amp
								expr: ast.Expr(ast.CallOrCastExpr{
									lhs:  ast.Expr(ast.Ident{
										name: 'int'
									})
									expr: ast.Expr(ast.Ident{
										name: 'raw'
									})
								})
							})
							pos:  deref_pos
						})
					}),
				]
				pos:   unsafe_pos
			}),
		]
	})
	assert transformed.op == .decl_assign
	local_type := t.lookup_local_decl_type('x') or {
		assert false, 'missing local declaration type for unsafe cast deref'
		return
	}
	assert t.type_to_c_name(local_type) == 'int'
	synth_type := t.get_synth_type(lhs_pos) or {
		assert false, 'missing synth type for unsafe cast deref declaration lhs'
		return
	}
	assert t.type_to_c_name(synth_type) == 'int'
}

fn test_is_enum_rvalue_stops_on_unresolved_alias() {
	t := create_test_transformer()
	unresolved_alias := types.Type(types.Alias{
		name: 'Unresolved'
	})
	assert !t.is_enum_rvalue(ast.Expr(ast.Ident{
		name: 'value'
	}), unresolved_alias)
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
				assert stmt.stmts.len == 3
				assert stmt.stmts[1] is ast.AssignStmt
				tmp_assign := stmt.stmts[1] as ast.AssignStmt
				assert tmp_assign.op == .decl_assign
				assert tmp_assign.lhs.len == 1
				assert tmp_assign.lhs[0] is ast.Ident
				tmp_name := (tmp_assign.lhs[0] as ast.Ident).name
				assert tmp_name.starts_with('_ap_t')
				assert tmp_assign.rhs.len == 1
				assert tmp_assign.rhs[0] is ast.ArrayInitExpr
				assert stmt.stmts[2] is ast.ExprStmt
				expr_stmt := stmt.stmts[2] as ast.ExprStmt
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
				assert outer_arr.exprs[0] is ast.Ident
				assert (outer_arr.exprs[0] as ast.Ident).name == tmp_name
				found = true
			}
		}
	}
	assert found
}

fn test_transform_nested_array_append_call_result_uses_temp() {
	files := transform_code_for_test('
fn make_items() []string {
	return ["x"]
}

fn f() {
	mut groups := [][]string{}
	groups << make_items()
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'f' {
				assert stmt.stmts.len == 3
				assert stmt.stmts[1] is ast.AssignStmt
				tmp_assign := stmt.stmts[1] as ast.AssignStmt
				assert tmp_assign.op == .decl_assign
				assert tmp_assign.lhs.len == 1
				assert tmp_assign.lhs[0] is ast.Ident
				tmp_name := (tmp_assign.lhs[0] as ast.Ident).name
				assert tmp_name.starts_with('_ap_t')
				assert tmp_assign.rhs.len == 1
				assert tmp_assign.rhs[0] is ast.CallExpr
				rhs_call := tmp_assign.rhs[0] as ast.CallExpr
				assert rhs_call.lhs is ast.Ident
				assert (rhs_call.lhs as ast.Ident).name == 'make_items'
				assert stmt.stmts[2] is ast.ExprStmt
				expr_stmt := stmt.stmts[2] as ast.ExprStmt
				assert expr_stmt.expr is ast.CallExpr
				call := expr_stmt.expr as ast.CallExpr
				assert call.lhs is ast.Ident
				assert (call.lhs as ast.Ident).name == 'builtin__array_push_noscan'
				assert call.args.len == 2
				assert call.args[1] is ast.ArrayInitExpr
				outer_arr := call.args[1] as ast.ArrayInitExpr
				assert outer_arr.exprs.len == 1
				assert outer_arr.exprs[0] is ast.Ident
				assert (outer_arr.exprs[0] as ast.Ident).name == tmp_name
				found = true
			}
		}
	}
	assert found
}

fn assert_array_append_arg_is_addressed_ident(arg ast.Expr, name string) {
	assert arg is ast.CastExpr
	arr_ptr := arg as ast.CastExpr
	assert arr_ptr.expr is ast.PrefixExpr
	prefix := arr_ptr.expr as ast.PrefixExpr
	assert prefix.op == .amp
	assert prefix.expr is ast.Ident
	assert (prefix.expr as ast.Ident).name == name
}

fn test_transform_array_append_mut_array_param_uses_local_storage_address() {
	files := transform_code_for_test('
fn add_seen(mut seen []string, name string) {
	seen << name
}

fn push_path(mut patterns [][]string, path []string) {
	patterns << path
}

fn push_many_paths(mut patterns [][]string, paths [][]string) {
	patterns << paths
}
')
	mut found_seen := false
	mut found_path := false
	mut found_many := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name in ['add_seen', 'push_path', 'push_many_paths'] {
				assert stmt.stmts.len == 1
				assert stmt.stmts[0] is ast.ExprStmt
				expr_stmt := stmt.stmts[0] as ast.ExprStmt
				assert expr_stmt.expr is ast.CallExpr
				call := expr_stmt.expr as ast.CallExpr
				assert call.lhs is ast.Ident
				call_name := (call.lhs as ast.Ident).name
				assert call.args.len >= 2
				if stmt.name == 'add_seen' {
					assert call_name == 'builtin__array_push_noscan'
					assert_array_append_arg_is_addressed_ident(call.args[0], 'seen')
					found_seen = true
				} else if stmt.name == 'push_path' {
					assert call_name == 'builtin__array_push_noscan'
					assert_array_append_arg_is_addressed_ident(call.args[0], 'patterns')
					found_path = true
				} else if stmt.name == 'push_many_paths' {
					assert call_name == 'array__push_many'
					assert_array_append_arg_is_addressed_ident(call.args[0], 'patterns')
					found_many = true
				}
			}
		}
	}
	assert found_seen
	assert found_path
	assert found_many
}

fn test_transform_nested_array_literal_single_push_uses_temp_and_param_storage_address() {
	files := transform_code_for_test('
fn push_literal_path(mut patterns [][]string, path []string) {
	patterns << [path]
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'push_literal_path' {
				assert stmt.stmts.len == 1
				assert stmt.stmts[0] is ast.ExprStmt
				expr_stmt := stmt.stmts[0] as ast.ExprStmt
				assert expr_stmt.expr is ast.CallExpr
				call := expr_stmt.expr as ast.CallExpr
				assert call.lhs is ast.Ident
				assert (call.lhs as ast.Ident).name == 'builtin__array_push_noscan'
				assert call.args.len == 2
				assert_array_append_arg_is_addressed_ident(call.args[0], 'patterns')
				assert call.args[1] is ast.ArrayInitExpr
				outer_arr := call.args[1] as ast.ArrayInitExpr
				assert outer_arr.exprs.len == 1
				assert outer_arr.exprs[0] is ast.Ident
				assert (outer_arr.exprs[0] as ast.Ident).name == 'path'
				found = true
			}
		}
	}
	assert found
}

fn test_transform_for_in_nested_array_value_uses_array_index() {
	files := transform_code_for_test('
fn f(groups [][]string) []int {
	mut counts := []int{}
	for group in groups {
		counts << group.len
	}
	return counts
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'f' {
				assert stmt.stmts.len >= 2
				assert stmt.stmts[1] is ast.ForStmt
				for_stmt := stmt.stmts[1] as ast.ForStmt
				assert for_stmt.stmts.len > 0
				assert for_stmt.stmts[0] is ast.AssignStmt
				value_assign := for_stmt.stmts[0] as ast.AssignStmt
				assert value_assign.lhs.len == 1
				assert value_assign.lhs[0] is ast.Ident
				assert (value_assign.lhs[0] as ast.Ident).name == 'group'
				assert value_assign.rhs.len == 1
				assert value_assign.rhs[0] is ast.IndexExpr
				index_expr := value_assign.rhs[0] as ast.IndexExpr
				assert index_expr.lhs !is ast.CastExpr
				found = true
			}
		}
	}
	assert found
}

fn test_transform_for_in_prefers_scope_iter_type_over_position_type() {
	mut env := types.Environment.new()
	iter_pos := token.Pos{
		id: 42
	}
	env.set_expr_type(iter_pos.id, types.Type(types.Array{
		elem_type: types.Type(types.int_)
	}))
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('s', value_object_from_type(types.Type(types.string_)))
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         env
		scope:                       scope
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		local_decl_types:            map[string]types.Type{}
	}
	result := t.transform_for_stmt(ast.ForStmt{
		init: ast.Stmt(ast.ForInStmt{
			value: ast.Expr(ast.Ident{
				name: 'ch'
			})
			expr:  ast.Expr(ast.ParenExpr{
				expr: ast.Expr(ast.Ident{
					name: 's'
					pos:  iter_pos
				})
			})
		})
	})
	assert result.stmts.len > 0
	assert result.stmts[0] is ast.AssignStmt
	value_assign := result.stmts[0] as ast.AssignStmt
	assert value_assign.rhs.len == 1
	assert value_assign.rhs[0] is ast.IndexExpr
	index_expr := value_assign.rhs[0] as ast.IndexExpr
	mut index_lhs := index_expr.lhs
	for _ in 0 .. 4 {
		if index_lhs is ast.ParenExpr {
			index_lhs = index_lhs.expr
			continue
		}
		break
	}
	assert index_lhs is ast.Ident
	assert (index_lhs as ast.Ident).name == 's'
}

fn test_transform_for_in_fixed_array_value_uses_array_index() {
	files := transform_code_for_test('
fn f(pairs [][2]int) []int {
	mut out := []int{}
	for pair in pairs {
		out << pair[1]
	}
	return out
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'f' {
				assert stmt.stmts.len >= 2
				assert stmt.stmts[1] is ast.ForStmt
				for_stmt := stmt.stmts[1] as ast.ForStmt
				assert for_stmt.stmts.len > 0
				assert for_stmt.stmts[0] is ast.AssignStmt
				value_assign := for_stmt.stmts[0] as ast.AssignStmt
				assert value_assign.lhs.len == 1
				assert value_assign.lhs[0] is ast.Ident
				assert (value_assign.lhs[0] as ast.Ident).name == 'pair'
				assert value_assign.rhs.len == 1
				assert value_assign.rhs[0] is ast.IndexExpr
				index_expr := value_assign.rhs[0] as ast.IndexExpr
				assert index_expr.lhs !is ast.CastExpr
				found = true
			}
		}
	}
	assert found
}

fn test_transform_for_in_struct_value_uses_typed_array_index() {
	files := transform_code_for_test('
struct Field {
	typ int
}

fn f(fields []Field) int {
	mut out := 0
	for field in fields {
		out += field.typ
	}
	return out
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'f' {
				assert stmt.stmts.len >= 2
				assert stmt.stmts[1] is ast.ForStmt
				for_stmt := stmt.stmts[1] as ast.ForStmt
				assert for_stmt.stmts.len > 0
				assert for_stmt.stmts[0] is ast.AssignStmt
				value_assign := for_stmt.stmts[0] as ast.AssignStmt
				assert value_assign.lhs.len == 1
				assert value_assign.lhs[0] is ast.Ident
				assert (value_assign.lhs[0] as ast.Ident).name == 'field'
				assert value_assign.rhs.len == 1
				assert value_assign.rhs[0] is ast.IndexExpr
				index_expr := value_assign.rhs[0] as ast.IndexExpr
				assert index_expr.lhs !is ast.CastExpr
				found = true
			}
		}
	}
	assert found
}

fn test_transform_for_in_smartcast_variant_array_field_keeps_element_type() {
	field_type := types.Type(types.Struct{
		name:   'Field'
		fields: [
			types.Field{
				name: 'name'
				typ:  types.Type(types.string_)
			},
		]
	})
	enum_type := types.Type(types.Struct{
		name:   'Enum'
		fields: [
			types.Field{
				name: 'fields'
				typ:  types.Type(types.Array{
					elem_type: field_type
				})
			},
		]
	})
	sum_type := types.Type(types.SumType{
		name:     'Type'
		variants: [enum_type]
	})
	mut local_scope := types.new_scope(unsafe { nil })
	local_scope.insert('typ', value_object_from_type(sum_type))
	mut module_scope := types.new_scope(unsafe { nil })
	module_scope.insert_type('Enum', enum_type)
	module_scope.insert_type('Type', sum_type)
	mut t := create_test_transformer()
	t.scope = local_scope
	t.cur_module = 'main'
	t.cached_scopes = {
		'main': module_scope
	}
	t.push_smartcast_full('typ', 'Enum', 'Enum', 'Type')
	result := t.transform_for_stmt(ast.ForStmt{
		init: ast.Stmt(ast.ForInStmt{
			value: ast.Expr(ast.Ident{
				name: 'field'
			})
			expr:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'typ'
				})
				rhs: ast.Ident{
					name: 'fields'
				}
			})
		})
	})
	assert result.stmts.len > 0
	assert result.stmts[0] is ast.AssignStmt
	value_assign := result.stmts[0] as ast.AssignStmt
	assert value_assign.lhs.len == 1
	assert value_assign.lhs[0] is ast.Ident
	value_ident := value_assign.lhs[0] as ast.Ident
	value_typ := t.get_synth_type(value_ident.pos) or {
		panic('missing smartcast for-in value type')
	}
	assert value_typ.name() == 'Field'
	assert value_assign.rhs.len == 1
	assert value_assign.rhs[0] is ast.IndexExpr
	index_expr := value_assign.rhs[0] as ast.IndexExpr
	index_typ := t.get_synth_type(index_expr.pos) or {
		panic('missing smartcast for-in index type')
	}
	assert index_typ.name() == 'Field'
}

fn test_transform_for_in_nested_fixed_array_value_registers_decl_type() {
	elem_type := types.Type(types.ArrayFixed{
		len:       3
		elem_type: types.Type(types.f32_)
	})
	iter_type := types.Type(types.ArrayFixed{
		len:       4
		elem_type: elem_type
	})
	mut t := create_transformer_with_vars({
		'corners': iter_type
	})
	result := t.transform_for_stmt(ast.ForStmt{
		init:  ast.Stmt(ast.ForInStmt{
			value: ast.Expr(ast.Ident{
				name: 'c'
			})
			expr:  ast.Expr(ast.Ident{
				name: 'corners'
			})
		})
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.IndexExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'c'
					})
					expr: ast.Expr(ast.BasicLiteral{
						kind:  .number
						value: '0'
					})
				})
			}),
		]
	})
	assert result.stmts.len > 0
	assert result.stmts[0] is ast.AssignStmt
	value_assign := result.stmts[0] as ast.AssignStmt
	assert value_assign.lhs.len == 1
	assert value_assign.lhs[0] is ast.Ident
	value_ident := value_assign.lhs[0] as ast.Ident
	assert value_ident.name == 'c'
	assert value_ident.pos.id != 0
	value_decl_type := t.get_synth_type(value_ident.pos) or {
		assert false, 'missing for-in value declaration type'
		return
	}
	assert value_decl_type == elem_type
}

fn test_transform_fixed_array_of_array_literals_keeps_inner_arrays_dynamic() {
	env, files := transform_code_with_env_for_test('
fn f() {
	corners := [[f32(1), 2, 3], [f32(4), 5, 6]]!
	for c in corners {
		_ = c[0]
	}
}
')
	mut found_decl := false
	mut found_loop := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'f' {
				assert stmt.stmts.len >= 2
				assert stmt.stmts[0] is ast.AssignStmt
				corners_decl := stmt.stmts[0] as ast.AssignStmt
				assert corners_decl.rhs.len == 1
				assert corners_decl.rhs[0] is ast.ArrayInitExpr
				corners_init := corners_decl.rhs[0] as ast.ArrayInitExpr
				assert corners_init.typ is ast.Type
				assert corners_init.typ as ast.Type is ast.ArrayFixedType
				corners_type := corners_init.typ as ast.Type
				fixed_type := corners_type as ast.ArrayFixedType
				assert fixed_type.elem_type is ast.Type
				assert fixed_type.elem_type as ast.Type is ast.ArrayType
				assert corners_init.exprs.len == 2
				assert corners_init.exprs[0] is ast.CallExpr
				assert corners_init.exprs[1] is ast.CallExpr
				found_decl = true

				assert stmt.stmts[1] is ast.ForStmt
				for_stmt := stmt.stmts[1] as ast.ForStmt
				assert for_stmt.stmts.len > 0
				assert for_stmt.stmts[0] is ast.AssignStmt
				value_assign := for_stmt.stmts[0] as ast.AssignStmt
				assert value_assign.lhs.len == 1
				assert value_assign.lhs[0] is ast.Ident
				value_ident := value_assign.lhs[0] as ast.Ident
				value_type := env.get_expr_type(value_ident.pos.id) or {
					assert false, 'missing loop value type'
					return
				}
				assert value_type is types.Array
				found_loop = true
			}
		}
	}
	assert found_decl
	assert found_loop
}

fn test_transform_for_in_mixed_float_array_uses_float_data_cast() {
	files := transform_code_for_test('
fn f() {
	for _, i in [8.0, 1000, 175_616] {
		_ = i
	}
}
')
	mut found := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'f' {
				assert stmt.stmts.len == 1
				assert stmt.stmts[0] is ast.ForStmt
				for_stmt := stmt.stmts[0] as ast.ForStmt
				assert for_stmt.stmts.len > 0
				assert for_stmt.stmts[0] is ast.AssignStmt
				value_assign := for_stmt.stmts[0] as ast.AssignStmt
				assert value_assign.rhs.len == 1
				assert value_assign.rhs[0] is ast.IndexExpr
				index_expr := value_assign.rhs[0] as ast.IndexExpr
				assert index_expr.lhs is ast.CastExpr
				cast_expr := index_expr.lhs as ast.CastExpr
				assert cast_expr.typ is ast.Ident
				assert (cast_expr.typ as ast.Ident).name == 'f64*'
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

fn test_static_method_call_treats_type_object_receiver_as_static() {
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('LineBufferReader', types.TypeObject{
		typ: types.Type(types.Struct{
			name: 'LineBufferReader'
		})
	})
	mut t := create_test_transformer()
	t.scope = scope
	assert t.is_static_method_call(ast.Expr(ast.Ident{
		name: 'LineBufferReader'
	}))
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

fn test_filter_map_receiver_pending_stmts_precede_receiver_cache() {
	files := transform_code_for_test('
fn mapped_positive(xs []int) []string {
	parts := xs.filter(it > 0).map(it.str())
	return parts
}
')
	assert files.len == 1
	mut filter_pos := -1
	mut recv_pos := -1
	for stmt in files[0].stmts {
		if stmt is ast.FnDecl && stmt.name == 'mapped_positive' {
			for i, inner in stmt.stmts {
				if inner is ast.AssignStmt && inner.op == .decl_assign && inner.lhs.len > 0 {
					if lhs := ident_name_from_expr_for_test(inner.lhs[0]) {
						if lhs.starts_with('_filter_t') && filter_pos == -1 {
							filter_pos = i
						}
						if lhs.starts_with('_filter_recv') && recv_pos == -1 {
							recv_pos = i
						}
					}
				}
			}
		}
	}
	assert filter_pos >= 0
	assert recv_pos >= 0
	assert filter_pos < recv_pos
}

fn test_direct_or_assign_fn_pointer_result_declares_final_lhs() {
	files := transform_code_for_test('
fn use_sql(sql_from_v fn (int) !string) !string {
	mut col_typ := sql_from_v(1) or {
		sql_from_v(2)!
	}
	if col_typ == "" {
		return ""
	}
	return col_typ
}
')
	assert files.len == 1
	mut saw_fn := false
	mut col_decl_count := 0
	for stmt in files[0].stmts {
		if stmt is ast.FnDecl && stmt.name == 'use_sql' {
			saw_fn = true
			for inner in stmt.stmts {
				if inner is ast.AssignStmt && inner.op == .decl_assign && inner.lhs.len > 0 {
					if lhs := ident_name_from_expr_for_test(inner.lhs[0]) {
						if lhs == 'col_typ' {
							col_decl_count++
						}
					}
				}
			}
		}
	}
	assert saw_fn
	assert col_decl_count == 1
}

fn test_sql_orm_vattribute_array_keeps_builtin_element_type() {
	attribute_kind_type := types.Type(types.Enum{
		name: 'AttributeKind'
	})
	vattribute_type := types.Type(types.Struct{
		name:   'VAttribute'
		fields: [
			types.Field{
				name: 'name'
				typ:  types.string_
			},
			types.Field{
				name: 'has_arg'
				typ:  types.Type(types.bool_)
			},
			types.Field{
				name: 'arg'
				typ:  types.string_
			},
			types.Field{
				name: 'kind'
				typ:  attribute_kind_type
			},
		]
	})
	table_field_type := types.Type(types.Struct{
		name:   'orm__TableField'
		fields: [
			types.Field{
				name: 'name'
				typ:  types.string_
			},
			types.Field{
				name: 'typ'
				typ:  types.Type(types.int_)
			},
			types.Field{
				name: 'nullable'
				typ:  types.Type(types.bool_)
			},
			types.Field{
				name: 'default_val'
				typ:  types.string_
			},
			types.Field{
				name: 'attrs'
				typ:  types.Type(types.Array{
					elem_type: vattribute_type
				})
			},
			types.Field{
				name: 'is_arr'
				typ:  types.Type(types.bool_)
			},
		]
	})
	mut builtin_scope := types.new_scope(unsafe { nil })
	builtin_scope.insert('AttributeKind', attribute_kind_type)
	builtin_scope.insert('VAttribute', vattribute_type)
	mut orm_scope := types.new_scope(unsafe { nil })
	orm_scope.insert('TableField', table_field_type)
	mut t := create_test_transformer()
	t.cur_module = 'gitly'
	t.cached_scopes = {
		'builtin': builtin_scope
		'orm':     orm_scope
	}
	pos := token.Pos{
		id: 8123
	}
	attr := ast.Attribute{
		name:  'sql'
		value: ast.StringLiteral{
			kind:  .v
			value: "'serial'"
			pos:   pos
		}
	}
	expr := t.sql_orm_table_field_expr(types.Field{
		name:       'id'
		typ:        types.Type(types.int_)
		attributes: [attr]
	}, pos) or { panic('expected orm table field expression') }
	result := t.transform_expr(expr)
	assert result is ast.InitExpr
	init := result as ast.InitExpr
	mut saw_attrs := false
	for field in init.fields {
		if field.name != 'attrs' {
			continue
		}
		saw_attrs = true
		assert field.value is ast.CallExpr
		call := field.value as ast.CallExpr
		assert call.args.len == 4
		assert call.args[2] is ast.KeywordOperator
		sizeof_arg := call.args[2] as ast.KeywordOperator
		assert sizeof_arg.exprs.len == 1
		assert sizeof_arg.exprs[0] is ast.Ident
		assert (sizeof_arg.exprs[0] as ast.Ident).name == 'VAttribute'
		assert call.args[3] is ast.ArrayInitExpr
		inner := call.args[3] as ast.ArrayInitExpr
		assert inner.typ is ast.Type
		inner_typ := inner.typ as ast.Type
		assert inner_typ is ast.ArrayType
		inner_arr_typ := inner_typ as ast.ArrayType
		assert inner_arr_typ.elem_type is ast.Ident
		assert (inner_arr_typ.elem_type as ast.Ident).name == 'VAttribute'
		assert inner.exprs.len == 1
		assert inner.exprs[0] is ast.InitExpr
		attr_init := inner.exprs[0] as ast.InitExpr
		assert attr_init.typ is ast.Ident
		assert (attr_init.typ as ast.Ident).name == 'VAttribute'
	}
	assert saw_attrs
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

fn struct_field_names(file ast.File, struct_name string) []string {
	for stmt in file.stmts {
		if stmt is ast.StructDecl && stmt.name == struct_name {
			mut names := []string{cap: stmt.fields.len}
			for field in stmt.fields {
				names << field.name
			}
			return names
		}
	}
	return []string{}
}

fn parse_code_with_defines_for_test(code string, defines []string) []ast.File {
	return parse_code_with_prefs_for_test(code, .cleanc, defines)
}

fn parse_code_with_prefs_for_test(code string, backend vpref.Backend, defines []string) []ast.File {
	prefs := &vpref.Preferences{
		backend:      backend
		no_parallel:  true
		user_defines: defines
	}
	return parse_code_with_custom_prefs_for_test(code, prefs)
}

fn parse_code_with_custom_prefs_for_test(code string, prefs &vpref.Preferences) []ast.File {
	tmp_file := '/tmp/v2_parser_cond_field_test_${os.getpid()}.v'
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	return par.parse_files([tmp_file], mut file_set)
}

fn test_struct_comptime_if_field_block_default_branch() {
	files := parse_code_with_defines_for_test('
module main

struct Container {
$if my_feature ? {
	feature_val string = "on"
} $else {
	feature_val string = "off"
}
	always_present int
}
', [])
	assert files.len == 1
	assert struct_field_names(files[0], 'Container') == ['feature_val', 'always_present']
}

fn test_struct_comptime_if_field_block_selected_branch() {
	files := parse_code_with_defines_for_test('
module main

struct Container {
$if my_feature ? {
	feature_val string = "on"
} $else {
	feature_val string = "off"
}
	always_present int
}
', [
		'my_feature',
	])
	assert files.len == 1
	assert struct_field_names(files[0], 'Container') == ['feature_val', 'always_present']
}

fn test_struct_comptime_if_field_block_omits_when_unset() {
	files := parse_code_with_defines_for_test('
module main

struct Container {
$if optional ? {
	opt_field int
}
	always int
}
', [])
	assert files.len == 1
	assert struct_field_names(files[0], 'Container') == ['always']
}

fn test_struct_comptime_else_if_chain_picks_first_match() {
	files := parse_code_with_defines_for_test('
module main

struct Container {
$if feat_a ? {
	tag string = "A"
} $else $if feat_b ? {
	tag string = "B"
} $else {
	tag string = "default"
}
}
', [
		'feat_b',
	])
	assert files.len == 1
	names := struct_field_names(files[0], 'Container')
	assert names == ['tag']
}

fn test_struct_field_if_attribute_elides_when_false() {
	files := parse_code_with_defines_for_test('
module main

struct Container {
	name   string
	debug_only int @[if absent_flag ?]
}
', [])
	assert files.len == 1
	assert struct_field_names(files[0], 'Container') == ['name']
}

fn test_struct_field_if_attribute_keeps_when_true() {
	files := parse_code_with_defines_for_test('
module main

struct Container {
	name   string
	debug_only int @[if present_flag ?]
}
', [
		'present_flag',
	])
	assert files.len == 1
	assert struct_field_names(files[0], 'Container') == ['name', 'debug_only']
}

fn test_struct_pkgconfig_fields_inactive_for_cross_target() {
	if !vpref.comptime_pkgconfig_value('sqlite3') {
		return
	}
	prefs := &vpref.Preferences{
		backend:        .cleanc
		no_parallel:    true
		target_os:      'cross'
		output_cross_c: true
	}
	files := parse_code_with_custom_prefs_for_test('
module main

struct Container {
$if $pkgconfig("sqlite3") {
	host_field HostSqliteField
} $else {
	cross_field int
}
attr_field int @[if $pkgconfig("sqlite3")]
	always int
}
',
		prefs)
	assert files.len == 1
	assert struct_field_names(files[0], 'Container') == ['cross_field', 'always']
}

fn test_transform_pkgconfig_branch_inactive_for_cross_target() {
	if !vpref.comptime_pkgconfig_value('sqlite3') {
		return
	}
	prefs := &vpref.Preferences{
		backend:        .cleanc
		no_parallel:    true
		target_os:      'cross'
		output_cross_c: true
	}
	files := transform_code_with_prefs_for_test('
module main

fn pick() int {
	$if $pkgconfig("sqlite3") {
		return 1
	} $else {
		return 2
	}
}
',
		prefs)
	assert files.len == 1
	for stmt in files[0].stmts {
		if stmt is ast.FnDecl && stmt.name == 'pick' {
			assert stmt.stmts.len == 1
			assert stmt.stmts[0] is ast.ReturnStmt
			ret := stmt.stmts[0] as ast.ReturnStmt
			assert ret.exprs.len == 1
			assert ret.exprs[0] is ast.BasicLiteral
			lit := ret.exprs[0] as ast.BasicLiteral
			assert lit.value == '2'
			return
		}
	}
	assert false
}

// Regression: `$else` starts on the line after `}`. The auto-inserted `;`
// between `}` and `$` must not hide the `$else` branch.
fn test_struct_comptime_else_on_next_line() {
	files := parse_code_with_defines_for_test('
module main

struct Container {
$if my_feature ? {
	val string = "on"
}
$else {
	val string = "off"
}
	always int
}
', [])
	assert files.len == 1
	assert struct_field_names(files[0], 'Container') == ['val', 'always']
}

// Regression: `{` starts on the line after `$if cond ?`. The scanner inserts
// a `;` after `?`, which must not block the opening brace.
fn test_struct_comptime_if_lcbr_on_next_line() {
	files := parse_code_with_defines_for_test('
module main

struct Container {
$if my_feature ?
{
	val string = "on"
}
$else
{
	val string = "off"
}
	always int
}
', [
		'my_feature',
	])
	assert files.len == 1
	assert struct_field_names(files[0], 'Container') == ['val', 'always']
}

// Regression: `$else $if` chain where each `$else` starts on a new line.
fn test_struct_comptime_else_if_on_next_line() {
	files := parse_code_with_defines_for_test('
module main

struct Container {
$if feat_a ? {
	tag string = "A"
}
$else $if feat_b ? {
	tag string = "B"
}
$else {
	tag string = "default"
}
}
', [
		'feat_b',
	])
	assert files.len == 1
	assert struct_field_names(files[0], 'Container') == ['tag']
}

// Native backend flags are recognized by the shared `pref.comptime_flag_value`.
// Confirms the parser sees the same flag set the transformer does.
fn test_struct_comptime_native_backend_flags() {
	files := parse_code_with_prefs_for_test('
module main

struct Container {
$if tinyc ? {
	tinyc_field string
}
$if no_backtrace ? {
	no_backtrace_field string
}
$if builtin_write_buf_to_fd_should_use_c_write ? {
	write_field string
}
$if new_int ? {
	never_field string
}
	always int
}
',
		.x64, [])
	assert files.len == 1
	assert struct_field_names(files[0], 'Container') == ['tinyc_field', 'no_backtrace_field',
		'write_field', 'always']
}
