// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: !windows
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
		runtime_const_inits_by_mod:  map[string][]RuntimeConstInit{}
		runtime_const_init_fn_name:  map[string]string{}
		static_local_globals_by_mod: map[string][]ast.FieldDecl{}
		static_local_global_seen:    map[string]bool{}
		static_local_renames:        map[string]string{}
		decl_type_overrides:         map[string]types.Type{}
		synth_types:                 map[int]types.Type{}
	}
}

// Helper to create a transformer with a scope containing variable types
fn create_transformer_with_vars(vars map[string]types.Type) &Transformer {
	env := &types.Environment{}
	mut scope := types.new_scope(unsafe { nil })
	for name, typ in vars {
		// Insert Type directly as Object (Type is part of Object sum type)
		scope.insert(name, typ)
	}
	return &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		scope:                       scope
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		runtime_const_inits_by_mod:  map[string][]RuntimeConstInit{}
		runtime_const_init_fn_name:  map[string]string{}
		static_local_globals_by_mod: map[string][]ast.FieldDecl{}
		static_local_global_seen:    map[string]bool{}
		static_local_renames:        map[string]string{}
		decl_type_overrides:         map[string]types.Type{}
		synth_types:                 map[int]types.Type{}
	}
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
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut transformer := Transformer.new_with_pref(files, env, prefs)
	return transformer.transform_files(files)
}

fn stmts_have_call_name(stmts []ast.Stmt, name string) bool {
	for stmt in stmts {
		if stmt_has_call_name(stmt, name) {
			return true
		}
	}
	return false
}

fn stmt_has_call_name(stmt ast.Stmt, name string) bool {
	return match stmt {
		ast.ExprStmt {
			expr_has_call_name(stmt.expr, name)
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				if expr_has_call_name(expr, name) {
					return true
				}
			}
			false
		}
		ast.ForStmt {
			stmts_have_call_name(stmt.stmts, name)
		}
		ast.ForInStmt {
			expr_has_call_name(stmt.expr, name)
		}
		ast.BlockStmt {
			stmts_have_call_name(stmt.stmts, name)
		}
		else {
			false
		}
	}
}

fn expr_has_call_name(expr ast.Expr, name string) bool {
	return match expr {
		ast.CallExpr {
			if expr.lhs is ast.Ident && expr.lhs.name == name {
				return true
			}
			if expr_has_call_name(expr.lhs, name) {
				return true
			}
			for arg in expr.args {
				if expr_has_call_name(arg, name) {
					return true
				}
			}
			false
		}
		ast.CallOrCastExpr {
			expr_has_call_name(expr.lhs, name) || expr_has_call_name(expr.expr, name)
		}
		ast.CastExpr {
			expr_has_call_name(expr.expr, name)
		}
		ast.IfExpr {
			expr_has_call_name(expr.cond, name) || stmts_have_call_name(expr.stmts, name)
				|| expr_has_call_name(expr.else_expr, name)
		}
		ast.InfixExpr {
			expr_has_call_name(expr.lhs, name) || expr_has_call_name(expr.rhs, name)
		}
		ast.ParenExpr {
			expr_has_call_name(expr.expr, name)
		}
		ast.PrefixExpr {
			expr_has_call_name(expr.expr, name)
		}
		ast.SelectorExpr {
			expr_has_call_name(expr.lhs, name)
		}
		else {
			false
		}
	}
}

fn selector_cast_type_name(expr ast.Expr) string {
	if expr is ast.SelectorExpr {
		if expr.lhs is ast.ParenExpr {
			paren := expr.lhs as ast.ParenExpr
			if paren.expr is ast.CastExpr {
				cast := paren.expr as ast.CastExpr
				if cast.typ is ast.Ident {
					return (cast.typ as ast.Ident).name
				}
			}
		}
	}
	return ''
}

// string_type returns the builtin v2 string type.
fn string_type() types.Type {
	return types.string_
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

fn test_transform_ident_vexeroot_to_string_literal() {
	mut t := create_test_transformer()
	t.comptime_vexeroot = '/tmp/v'
	result := t.transform_expr(ast.Ident{
		name: '@VEXEROOT'
	})
	assert result is ast.StringLiteral, 'expected StringLiteral, got ${result.type_name()}'
	lit := result as ast.StringLiteral
	assert lit.kind == .v
	assert lit.value == "'/tmp/v'"
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

fn test_addr_of_interface_cast_preserves_heap_interface_cast() {
	mut t := create_test_transformer()
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('PRNG', types.Type(types.Interface{
		name: 'PRNG'
	}))
	t.cur_module = 'rand'
	t.cached_scopes = {
		'rand': scope
	}
	expr := ast.PrefixExpr{
		op:   .amp
		expr: ast.CallOrCastExpr{
			lhs:  ast.Ident{
				name: 'PRNG'
			}
			expr: ast.Ident{
				name: 'rng'
			}
		}
	}

	result := t.transform_expr(expr)
	assert result is ast.PrefixExpr, 'expected PrefixExpr, got ${result.type_name()}'
	prefix := result as ast.PrefixExpr
	assert prefix.op == .amp
	assert prefix.expr is ast.CastExpr, 'expected inner CastExpr, got ${prefix.expr.type_name()}'
	cast := prefix.expr as ast.CastExpr
	assert cast.typ is ast.Ident
	assert (cast.typ as ast.Ident).name == 'PRNG'
	assert cast.expr is ast.Ident
	assert (cast.expr as ast.Ident).name == 'rng'
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

fn test_transform_flag_enum_zero_call_to_typed_zero() {
	mut t := create_test_transformer()
	mut scope := types.new_scope(unsafe { nil })
	enum_typ := types.Type(types.Enum{
		name:    'Show'
		is_flag: true
	})
	scope.insert('Show', enum_typ)
	t.cur_module = 'main'
	t.cached_scopes = {
		'main': scope
	}
	result := t.transform_call_expr(ast.CallExpr{
		lhs:  ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'Show'
			})
			rhs: ast.Ident{
				name: 'zero'
			}
		})
		args: []ast.Expr{}
	})
	assert result is ast.CastExpr, 'Enum.zero() should lower to a typed zero cast'
	cast := result as ast.CastExpr
	assert cast.typ is ast.Ident
	assert (cast.typ as ast.Ident).name == 'Show'
	assert cast.expr is ast.BasicLiteral
	assert (cast.expr as ast.BasicLiteral).value == '0'
}

fn test_transform_static_enum_from_string_call_to_generated_helper() {
	mut t := create_test_transformer()
	mut scope := types.new_scope(unsafe { nil })
	enum_typ := types.Type(types.Enum{
		name: 'Subsystem'
	})
	scope.insert('Subsystem', enum_typ)
	t.cur_module = 'pref'
	t.cached_scopes = {
		'pref': scope
	}
	result := t.transform_call_expr(ast.CallExpr{
		lhs:  ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'Subsystem'
			})
			rhs: ast.Ident{
				name: 'from_string'
			}
		})
		args: [
			ast.Expr(ast.Ident{
				name: 'subsystem'
			}),
		]
	})
	assert result is ast.CallExpr, 'Subsystem.from_string should lower to a helper call'
	call := result as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'pref__Subsystem__from_string'
	assert call.args.len == 1
}

fn test_smartcast_method_call_prefers_variant_method() {
	files := transform_code_for_test('
struct Text {}

type Value = Text | int

fn (t Text) value() int {
	return 7
}

fn (v Value) value() int {
	match v {
		Text {
			return v.value()
		}
		int {
			return v
		}
	}
	return 0
}
')
	mut found_value_method := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'value' && stmt.is_method
				&& stmt.receiver.typ is ast.Ident
				&& (stmt.receiver.typ as ast.Ident).name == 'Value' {
				found_value_method = true
				assert stmts_have_call_name(stmt.stmts, 'Text__value')
				assert !stmts_have_call_name(stmt.stmts, 'Value__value')
			}
		}
	}
	assert found_value_method
}

fn test_smartcast_method_call_through_as_cast_selector() {
	files := transform_code_for_test('
struct Ident {
	is_mut bool
}

fn (i Ident) is_mut() bool {
	return i.is_mut
}

struct IndexExpr {
	left Expr
}

type Expr = Ident | IndexExpr

fn f(expr Expr, field_is_mut bool) bool {
	return expr is IndexExpr && expr.left is Ident && ((expr as IndexExpr).left.is_mut() || field_is_mut)
}
')
	mut found_f := false
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == 'f' {
				found_f = true
				assert stmts_have_call_name(stmt.stmts, 'Ident__is_mut')
			}
		}
	}
	assert found_f
}

fn test_return_enum_shorthand_in_result_fn_uses_base_enum_type() {
	files := transform_code_for_test('
module pref

pub enum OS {
	_auto
	linux
}

pub fn os_from_string(os_str string) !OS {
	match os_str {
		"" { return ._auto }
		else { return .linux }
	}
}
')
	assert files.len == 1
	mut found := false
	for stmt in files[0].stmts {
		if stmt is ast.FnDecl && stmt.name == 'os_from_string' {
			found = true
			assert stmt.stmts.len == 1
			assert stmt.stmts[0] is ast.ExprStmt
			expr_stmt := stmt.stmts[0] as ast.ExprStmt
			assert expr_stmt.expr is ast.IfExpr
			if_expr := expr_stmt.expr as ast.IfExpr
			assert if_expr.else_expr is ast.IfExpr
			else_if := if_expr.else_expr as ast.IfExpr
			first_return := if_expr.stmts[0] as ast.ReturnStmt
			second_return := else_if.stmts[0] as ast.ReturnStmt
			assert first_return.exprs[0] is ast.Ident
			assert second_return.exprs[0] is ast.Ident
			assert (first_return.exprs[0] as ast.Ident).name == 'pref__OS___auto'
			assert (second_return.exprs[0] as ast.Ident).name == 'pref__OS___linux'
		}
	}
	assert found
}

fn test_has_defer_stmt_finds_deep_else_if_branch() {
	mut t := create_test_transformer()
	stmts := [
		ast.Stmt(ast.ExprStmt{
			expr: ast.Expr(ast.IfExpr{
				stmts:     []ast.Stmt{}
				else_expr: ast.Expr(ast.IfExpr{
					stmts:     []ast.Stmt{}
					else_expr: ast.Expr(ast.IfExpr{
						stmts: [
							ast.Stmt(ast.DeferStmt{
								stmts: []ast.Stmt{}
							}),
						]
					})
				})
			})
		}),
	]
	assert t.has_defer_stmt(stmts)
}

fn test_function_defer_in_if_branch_is_guarded_for_later_returns() {
	mut t := create_test_transformer()
	close_stmt := ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(ast.CallExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'close_scope'
			})
		})
	})
	stmts := [
		ast.Stmt(ast.ExprStmt{
			expr: ast.Expr(ast.IfExpr{
				cond:  ast.Expr(ast.Ident{
					name: 'cond'
				})
				stmts: [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.CallExpr{
							lhs: ast.Expr(ast.Ident{
								name: 'open_scope'
							})
						})
					}),
					ast.Stmt(ast.DeferStmt{
						mode:  .function
						stmts: [close_stmt]
					}),
				]
			})
		}),
		ast.Stmt(ast.ReturnStmt{}),
	]

	lowered := t.lower_defer_stmts(stmts, false, types.Type(types.void_))
	assert lowered.len == 4
	assert lowered[0] is ast.AssignStmt
	guard_decl := lowered[0] as ast.AssignStmt
	assert guard_decl.op == .decl_assign
	assert guard_decl.lhs[0] is ast.Ident
	guard_name := (guard_decl.lhs[0] as ast.Ident).name
	assert guard_decl.rhs[0] is ast.BasicLiteral
	assert (guard_decl.rhs[0] as ast.BasicLiteral).value == 'false'

	assert lowered[1] is ast.ExprStmt
	outer_if := (lowered[1] as ast.ExprStmt).expr as ast.IfExpr
	assert outer_if.stmts.len == 2
	assert outer_if.stmts[1] is ast.AssignStmt
	activate_stmt := outer_if.stmts[1] as ast.AssignStmt
	assert activate_stmt.op == .assign
	assert (activate_stmt.lhs[0] as ast.Ident).name == guard_name
	assert (activate_stmt.rhs[0] as ast.BasicLiteral).value == 'true'

	assert lowered[2] is ast.ExprStmt
	guarded_defer := (lowered[2] as ast.ExprStmt).expr as ast.IfExpr
	assert guarded_defer.cond is ast.Ident
	assert (guarded_defer.cond as ast.Ident).name == guard_name
	assert guarded_defer.stmts.len == 1
	assert lowered[3] is ast.ReturnStmt
}

fn test_function_defer_hoists_branch_local_captures() {
	mut t := create_transformer_with_vars({
		'value': types.Type(types.int_)
		'old':   types.Type(types.int_)
	})
	defer_stmt := ast.Stmt(ast.AssignStmt{
		op:  .assign
		lhs: [
			ast.Expr(ast.Ident{
				name: 'value'
			}),
		]
		rhs: [
			ast.Expr(ast.Ident{
				name: 'old'
			}),
		]
	})
	stmts := [
		ast.Stmt(ast.ExprStmt{
			expr: ast.Expr(ast.IfExpr{
				cond:  ast.Expr(ast.Ident{
					name: 'cond'
				})
				stmts: [
					ast.Stmt(ast.AssignStmt{
						op:  .decl_assign
						lhs: [
							ast.Expr(ast.Ident{
								name: 'old'
							}),
						]
						rhs: [
							ast.Expr(ast.Ident{
								name: 'value'
							}),
						]
					}),
					ast.Stmt(ast.DeferStmt{
						mode:  .function
						stmts: [
							defer_stmt,
						]
					}),
				]
			})
		}),
		ast.Stmt(ast.ReturnStmt{
			exprs: [
				ast.Expr(ast.Ident{
					name: 'value'
				}),
			]
		}),
	]

	lowered := t.lower_defer_stmts(stmts, true, types.Type(types.int_))
	assert lowered.len == 6
	assert lowered[0] is ast.AssignStmt
	hoisted_decl := lowered[0] as ast.AssignStmt
	assert hoisted_decl.op == .decl_assign
	assert (hoisted_decl.lhs[0] as ast.Ident).name == 'old'
	assert hoisted_decl.rhs[0] is ast.BasicLiteral
	assert (hoisted_decl.rhs[0] as ast.BasicLiteral).value == '0'

	assert lowered[2] is ast.ExprStmt
	outer_if := (lowered[2] as ast.ExprStmt).expr as ast.IfExpr
	assert outer_if.stmts.len == 2
	assert outer_if.stmts[0] is ast.AssignStmt
	branch_assign := outer_if.stmts[0] as ast.AssignStmt
	assert branch_assign.op == .assign
	assert (branch_assign.lhs[0] as ast.Ident).name == 'old'
	assert outer_if.stmts[1] is ast.AssignStmt
	activate_stmt := outer_if.stmts[1] as ast.AssignStmt
	assert activate_stmt.op == .assign
}

fn test_function_defer_hoists_enclosing_block_captures() {
	mut t := create_transformer_with_vars({
		'tmp':   types.Type(types.string_)
		'needs': types.Type(types.bool_)
	})
	defer_stmt := ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(ast.IfExpr{
			cond:  ast.Expr(ast.Ident{
				name: 'needs'
			})
			stmts: [
				ast.Stmt(ast.ExprStmt{
					expr: ast.Expr(ast.CallExpr{
						lhs:  ast.Expr(ast.Ident{
							name: 'free_tmp'
						})
						args: [
							ast.Expr(ast.Ident{
								name: 'tmp'
							}),
						]
					})
				}),
			]
		})
	})
	stmts := [
		ast.Stmt(ast.AssignStmt{
			op:  .decl_assign
			lhs: [
				ast.Expr(ast.Ident{
					name: 'tmp'
				}),
			]
			rhs: [
				ast.Expr(ast.StringLiteral{
					kind:  .v
					value: "''"
				}),
			]
		}),
		ast.Stmt(ast.AssignStmt{
			op:  .decl_assign
			lhs: [
				ast.Expr(ast.Ident{
					name: 'needs'
				}),
			]
			rhs: [
				ast.Expr(ast.BasicLiteral{
					kind:  .key_false
					value: 'false'
				}),
			]
		}),
		ast.Stmt(ast.ExprStmt{
			expr: ast.Expr(ast.IfExpr{
				cond:  ast.Expr(ast.Ident{
					name: 'cond'
				})
				stmts: [
					ast.Stmt(ast.DeferStmt{
						mode:  .function
						stmts: [defer_stmt]
					}),
				]
			})
		}),
		ast.Stmt(ast.ReturnStmt{}),
	]

	lowered := t.lower_defer_stmts(stmts, false, types.Type(types.void_))
	assert lowered.len == 8
	assert lowered[0] is ast.AssignStmt
	assert lowered[1] is ast.AssignStmt
	assert ((lowered[1] as ast.AssignStmt).lhs[0] as ast.Ident).name == 'tmp'
	assert lowered[2] is ast.AssignStmt
	assert ((lowered[2] as ast.AssignStmt).lhs[0] as ast.Ident).name == 'needs'
	assert lowered[3] is ast.AssignStmt
	tmp_assign := lowered[3] as ast.AssignStmt
	assert tmp_assign.op == .assign
	assert (tmp_assign.lhs[0] as ast.Ident).name == 'tmp'
	assert lowered[4] is ast.AssignStmt
	needs_assign := lowered[4] as ast.AssignStmt
	assert needs_assign.op == .assign
	assert (needs_assign.lhs[0] as ast.Ident).name == 'needs'
}

fn test_defer_multi_return_saves_tuple_temp() {
	files := transform_code_for_test('
fn touch() {}

fn f() (int, int, int) {
	a := 1
	b := 2
	c := 3
	defer {
		touch()
	}
	return a, b, c
}
')
	mut found_fn := false
	for stmt in files[0].stmts {
		if stmt is ast.FnDecl && stmt.name == 'f' {
			found_fn = true
			mut temp_names := []string{}
			mut found_return := false
			for fn_stmt in stmt.stmts {
				if fn_stmt is ast.AssignStmt && fn_stmt.lhs.len == 1 && fn_stmt.rhs.len == 1
					&& fn_stmt.lhs[0] is ast.Ident {
					name := (fn_stmt.lhs[0] as ast.Ident).name
					if name.starts_with('_defer_t') {
						temp_names << name
						assert fn_stmt.rhs[0] is ast.Ident
					}
				}
				if fn_stmt is ast.ReturnStmt {
					found_return = true
					assert fn_stmt.exprs.len == 3
					for i, expr in fn_stmt.exprs {
						assert expr is ast.Ident
						assert (expr as ast.Ident).name == temp_names[i]
					}
				}
			}
			assert temp_names.len == 3
			assert found_return
		}
	}
	assert found_fn
}

fn test_normalize_blank_fn_parameters_makes_unique_c_names() {
	decl := ast.FnDecl{
		name: 'f'
		typ:  ast.FnType{
			params: [
				ast.Parameter{
					name: '_'
					typ:  ast.Expr(ast.Ident{
						name: 'int'
					})
				},
				ast.Parameter{
					name: '_'
					typ:  ast.Expr(ast.Ident{
						name: 'string'
					})
				},
			]
		}
	}
	normalized := normalize_blank_fn_parameters(decl)
	assert normalized.typ.params[0].name == '_v_blank_param_1'
	assert normalized.typ.params[1].name == '_v_blank_param_2'
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

fn test_transform_init_expr_adds_nested_struct_defaults() {
	inner_type := types.Type(types.Struct{
		name:   'Inner'
		fields: [
			types.Field{
				name:         'min_len'
				typ:          types.Type(types.int_)
				default_expr: ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '999999'
				})
			},
		]
	})
	outer_type := types.Type(types.Struct{
		name:   'Outer'
		fields: [
			types.Field{
				name: 'inner'
				typ:  inner_type
			},
			types.Field{
				name: 'inner_ptr'
				typ:  types.Type(types.Pointer{
					base_type: inner_type
				})
			},
		]
	})
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('Inner', inner_type)
	scope.insert('Outer', outer_type)
	mut t := create_test_transformer()
	t.cur_module = 'main'
	t.scope = scope
	t.cached_scopes = {
		'main': scope
	}
	result := t.transform_init_expr(ast.InitExpr{
		typ: ast.Expr(ast.Ident{
			name: 'Outer'
		})
	})
	assert result is ast.InitExpr
	outer_init := result as ast.InitExpr
	assert outer_init.fields.len == 1
	assert outer_init.fields[0].name == 'inner'
	assert outer_init.fields[0].value is ast.InitExpr
	inner_init := outer_init.fields[0].value as ast.InitExpr
	assert inner_init.fields.len == 1
	assert inner_init.fields[0].name == 'min_len'
	assert inner_init.fields[0].value is ast.BasicLiteral
	assert (inner_init.fields[0].value as ast.BasicLiteral).value == '999999'
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

fn test_map_key_type_for_expr_prefers_string_expr_over_wrapper_key() {
	mut t := create_transformer_with_vars({
		'mod': string_type()
	})
	bad_map_type := types.Map{
		key_type:   types.OptionType{
			base_type: types.int_
		}
		value_type: string_type()
	}
	key_expr := ast.InfixExpr{
		op:  .plus
		lhs: ast.StringLiteral{
			kind:  .v
			value: "'v.'"
		}
		rhs: ast.Ident{
			name: 'mod'
		}
	}

	resolved := t.map_key_type_for_expr(key_expr, bad_map_type)

	assert resolved is types.String, 'expected string key fallback, got ${resolved.type_name()}'
}

fn test_map_key_type_for_expr_prefers_string_expr_over_wrapper_alias_key() {
	mut t := create_transformer_with_vars({
		'mod': string_type()
	})
	bad_map_type := types.Map{
		key_type:   types.Alias{
			name:      '_option_int'
			base_type: types.int_
		}
		value_type: string_type()
	}
	key_expr := ast.InfixExpr{
		op:  .plus
		lhs: ast.StringLiteral{
			kind:  .v
			value: "'v.'"
		}
		rhs: ast.Ident{
			name: 'mod'
		}
	}

	resolved := t.map_key_type_for_expr(key_expr, bad_map_type)

	assert resolved is types.String, 'expected string key fallback, got ${resolved.type_name()}'
}

fn test_addr_of_expr_with_temp_registers_synth_type() {
	mut t := create_test_transformer()

	result := t.addr_of_expr_with_temp(ast.CallExpr{
		lhs: ast.Ident{
			name: 'make_string'
		}
	}, string_type())

	assert result is ast.UnsafeExpr, 'expected UnsafeExpr, got ${result.type_name()}'
	unsafe_expr := result as ast.UnsafeExpr
	assert unsafe_expr.stmts.len == 2
	assign := unsafe_expr.stmts[0] as ast.AssignStmt
	tmp_ident := assign.lhs[0] as ast.Ident
	assert tmp_ident.pos.id < 0, 'expected synthesized temp position'
	if typ := t.synth_types[tmp_ident.pos.id] {
		assert typ is types.String
	} else {
		assert false, 'expected synthesized type registration'
	}
}

fn test_addr_of_expr_with_temp_prefers_transformed_type_over_wrapper_type() {
	mut t := create_test_transformer()
	bad_typ := types.Alias{
		name:      '_option_int'
		base_type: types.int_
	}

	result := t.addr_of_expr_with_temp(ast.CallExpr{
		lhs:  ast.Ident{
			name: 'string__plus'
		}
		args: [
			ast.Expr(ast.StringLiteral{
				kind:  .v
				value: "'v.'"
			}),
			ast.Expr(ast.StringLiteral{
				kind:  .v
				value: "'mod'"
			}),
		]
	}, bad_typ)

	assert result is ast.UnsafeExpr, 'expected UnsafeExpr, got ${result.type_name()}'
	unsafe_expr := result as ast.UnsafeExpr
	assign := unsafe_expr.stmts[0] as ast.AssignStmt
	tmp_ident := assign.lhs[0] as ast.Ident
	if typ := t.synth_types[tmp_ident.pos.id] {
		assert typ is types.String
	} else {
		assert false, 'expected synthesized type registration'
	}
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

fn test_runtime_const_init_detects_or_expr_initializer() {
	t := create_test_transformer()

	expr := ast.Expr(ast.OrExpr{
		expr:  ast.CallExpr{
			lhs:  ast.Ident{
				name: 'maybe_string'
			}
			args: []
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.StringLiteral{
					kind:  .v
					value: "'cc'"
				}
			}),
		]
	})

	assert t.needs_runtime_const_init(expr, false)
	assert t.contains_call_expr(expr)
}

fn test_runtime_const_init_detects_string_interpolation() {
	t := create_test_transformer()

	expr := ast.Expr(ast.StringInterLiteral{
		kind:   .v
		values: ['prefix ', ' suffix']
		inters: [
			ast.StringInter{
				expr: ast.Expr(ast.Ident{
					name: 'value'
				})
			},
		]
	})

	assert t.needs_runtime_const_init(expr, false)
}

fn test_runtime_const_init_tracks_dependents_of_string_consts() {
	mut t := create_test_transformer()

	files := [
		ast.File{
			mod:   'ast'
			stmts: [
				ast.Stmt(ast.ConstDecl{
					fields: [
						ast.FieldInit{
							name:  'option_name'
							value: ast.Expr(ast.StringLiteral{
								kind:  .v
								value: "'_option'"
							})
						},
						ast.FieldInit{
							name:  'builtins'
							value: ast.Expr(ast.ArrayInitExpr{
								exprs: [
									ast.Expr(ast.StringLiteral{
										kind:  .v
										value: "'string'"
									}),
									ast.Expr(ast.Ident{
										name: 'option_name'
									}),
								]
							})
						},
					]
				}),
			]
		},
		ast.File{
			mod:   'generics'
			stmts: [
				ast.Stmt(ast.ConstDecl{
					fields: [
						ast.FieldInit{
							name:  'option_name'
							value: ast.Expr(ast.SelectorExpr{
								lhs: ast.Expr(ast.Ident{
									name: 'ast'
								})
								rhs: ast.Ident{
									name: 'option_name'
								}
							})
						},
					]
				}),
			]
		},
	]
	t.collect_runtime_const_inits(files)

	ast_inits := t.runtime_const_inits_by_mod['ast'] or { []RuntimeConstInit{} }
	generics_inits := t.runtime_const_inits_by_mod['generics'] or { []RuntimeConstInit{} }
	assert ast_inits.len == 1
	assert ast_inits[0].name == 'builtins'
	assert generics_inits.len == 1
	assert generics_inits[0].name == 'option_name'
}

fn test_static_local_decl_lowers_to_global_and_renames_uses() {
	mut t := create_test_transformer()
	t.cur_module = 'main'
	t.cur_fn_name_str = 'timers_pointer'
	main_scope := types.new_scope(unsafe { nil })
	lock t.env.scopes {
		t.env.scopes['main'] = main_scope
	}

	stmts := t.transform_stmts([
		ast.Stmt(ast.AssignStmt{
			op:  .decl_assign
			lhs: [
				ast.Expr(ast.ModifierExpr{
					kind: .key_mut
					expr: ast.Expr(ast.ModifierExpr{
						kind: .key_static
						expr: ast.Expr(ast.Ident{
							name: 'ptimers'
						})
					})
				}),
			]
			rhs: [
				ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '0'
				}),
			]
		}),
		ast.Stmt(ast.AssignStmt{
			op:  .assign
			lhs: [
				ast.Expr(ast.Ident{
					name: 'ptimers'
				}),
			]
			rhs: [
				ast.Expr(ast.Ident{
					name: 'p'
				}),
			]
		}),
	])

	assert stmts.len == 1
	fields := t.static_local_globals_by_mod['main'] or { []ast.FieldDecl{} }
	assert fields.len == 1
	assert fields[0].name == '_static_timers_pointer_ptimers'
	assign := stmts[0] as ast.AssignStmt
	assert assign.op == .assign
	assert assign.lhs[0] is ast.Ident
	assert (assign.lhs[0] as ast.Ident).name == '_static_timers_pointer_ptimers'
	mut files := [
		ast.File{
			mod:   'main'
			stmts: stmts
		},
	]
	t.inject_static_local_globals(mut files)
	assert files[0].stmts[0] is ast.GlobalDecl
	obj := main_scope.lookup('_static_timers_pointer_ptimers') or {
		panic('missing static local global')
	}
	assert obj is types.Global
}

fn test_static_local_assignment_inside_if_renames_to_global() {
	mut t := create_test_transformer()
	t.cur_module = 'main'
	t.cur_fn_name_str = 'timers_pointer'

	stmts := t.transform_stmts([
		ast.Stmt(ast.AssignStmt{
			op:  .decl_assign
			lhs: [
				ast.Expr(ast.ModifierExpr{
					kind: .key_mut
					expr: ast.Expr(ast.ModifierExpr{
						kind: .key_static
						expr: ast.Expr(ast.Ident{
							name: 'ptimers'
						})
					})
				}),
			]
			rhs: [
				ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '0'
				}),
			]
		}),
		ast.Stmt(ast.ExprStmt{
			expr: ast.Expr(ast.IfExpr{
				cond:  ast.Expr(ast.InfixExpr{
					op:  .ne
					lhs: ast.Expr(ast.Ident{
						name: 'p'
					})
					rhs: ast.Expr(ast.Ident{
						name: 'nil'
					})
				})
				stmts: [
					ast.Stmt(ast.AssignStmt{
						op:  .assign
						lhs: [
							ast.Expr(ast.Ident{
								name: 'ptimers'
							}),
						]
						rhs: [
							ast.Expr(ast.Ident{
								name: 'p'
							}),
						]
					}),
				]
			})
		}),
	])

	assert stmts.len == 1
	assert stmts[0] is ast.ExprStmt
	if_stmt := stmts[0] as ast.ExprStmt
	assert if_stmt.expr is ast.IfExpr
	if_expr := if_stmt.expr as ast.IfExpr
	assert if_expr.stmts.len == 1
	assert if_expr.stmts[0] is ast.AssignStmt
	assign := if_expr.stmts[0] as ast.AssignStmt
	assert assign.op == .assign
	assert assign.lhs[0] is ast.Ident
	assert (assign.lhs[0] as ast.Ident).name == '_static_timers_pointer_ptimers'
}

fn test_merge_worker_keeps_runtime_global_inits() {
	mut t := create_test_transformer()
	mut w := create_test_transformer()
	w.record_runtime_global_init('util', 'g_timers', ast.Expr(ast.CallExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'new_timers'
		})
	}))

	t.merge_worker(w)

	inits := t.runtime_const_inits_by_mod['util'] or { []RuntimeConstInit{} }
	assert inits.len == 1
	assert inits[0].name == 'g_timers'
	assert 'util' in t.runtime_const_modules
}

fn test_fixed_array_zero_global_init_stays_static() {
	mut t := create_test_transformer()
	t.cur_module = 'builtin'
	init := ast.Expr(ast.ArrayInitExpr{
		typ: ast.Expr(ast.Type(ast.ArrayFixedType{
			len:       ast.Expr(ast.Ident{
				name: 'autostr_type_stack_max_depth'
			})
			elem_type: ast.Expr(ast.Ident{
				name: 'int'
			})
		}))
	})
	decl := t.transform_global_decl(ast.GlobalDecl{
		fields: [
			ast.FieldDecl{
				name:  'g_autostr_type_stack'
				value: init
			},
		]
	})

	assert decl.fields.len == 1
	assert decl.fields[0].value is ast.ArrayInitExpr
	assert 'builtin' !in t.runtime_const_inits_by_mod
}

fn test_addr_of_assoc_expr_lowers_to_heap_pointer() {
	mut t := create_test_transformer()
	item_type := types.Type(types.Struct{
		name: 'TypeSymbol'
	})
	assoc_pos := token.Pos{
		id: 10
	}
	base_pos := token.Pos{
		id: 11
	}
	t.env.set_expr_type(assoc_pos.id, item_type)
	t.env.set_expr_type(base_pos.id, item_type)

	result := t.transform_expr(ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: ast.Expr(ast.AssocExpr{
			typ:  ast.Expr(ast.Ident{
				name: 'TypeSymbol'
			})
			expr: ast.Expr(ast.Ident{
				name: 'sym'
				pos:  base_pos
			})
			pos:  assoc_pos
		})
	}))

	assert result is ast.Ident
	ptr_name := (result as ast.Ident).name
	assert ptr_name.starts_with('_assoc_ptr_t')
	ptr_type := t.lookup_var_type(ptr_name) or {
		assert false, 'missing assoc pointer temp type'
		return
	}
	assert ptr_type is types.Pointer
	assert t.pending_stmts.len == 3

	ptr_decl := t.pending_stmts[1] as ast.AssignStmt
	assert ptr_decl.op == .decl_assign
	assert ptr_decl.lhs[0] is ast.Ident
	assert (ptr_decl.lhs[0] as ast.Ident).name == ptr_name
	assert ptr_decl.rhs[0] is ast.CastExpr
	malloc_cast := ptr_decl.rhs[0] as ast.CastExpr
	assert malloc_cast.typ is ast.PrefixExpr
	assert (malloc_cast.typ as ast.PrefixExpr).op == .amp
	assert malloc_cast.expr is ast.CallExpr
	malloc_call := malloc_cast.expr as ast.CallExpr
	assert malloc_call.lhs is ast.SelectorExpr
	malloc_lhs := malloc_call.lhs as ast.SelectorExpr
	assert malloc_lhs.lhs is ast.Ident
	assert (malloc_lhs.lhs as ast.Ident).name == 'C'
	assert malloc_lhs.rhs.name == 'malloc'
	assert malloc_call.args.len == 1
	assert malloc_call.args[0] is ast.KeywordOperator
	assert (malloc_call.args[0] as ast.KeywordOperator).op == .key_sizeof

	copy_stmt := t.pending_stmts[2] as ast.AssignStmt
	assert copy_stmt.op == .assign
	assert copy_stmt.lhs[0] is ast.PrefixExpr
	copy_lhs := copy_stmt.lhs[0] as ast.PrefixExpr
	assert copy_lhs.op == .mul
	assert copy_lhs.expr is ast.Ident
	assert (copy_lhs.expr as ast.Ident).name == ptr_name
	assert copy_stmt.rhs[0] is ast.Ident
	assert (copy_stmt.rhs[0] as ast.Ident).name.starts_with('_assoc_t')
}

fn test_array_index_or_decl_uses_element_zero_value() {
	mut t := create_transformer_with_vars({
		'args': types.Type(types.Array{
			elem_type: types.string_
		})
	})
	or_expr := ast.OrExpr{
		expr:  ast.Expr(ast.IndexExpr{
			lhs:  ast.Expr(ast.Ident{
				name: 'args'
			})
			expr: ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})
		})
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.StringLiteral{
						kind:  .v
						value: "''"
					}),
					ast.Expr(ast.BasicLiteral{
						kind:  .key_false
						value: 'false'
					}),
				]
			}),
		]
	}
	stmt := ast.AssignStmt{
		op:  .decl_assign
		lhs: [
			ast.Expr(ast.Ident{
				name: 'next'
			}),
		]
		rhs: [ast.Expr(or_expr)]
	}
	result := t.try_expand_array_index_or_assign(stmt, or_expr) or {
		assert false, 'expected array index or-assignment to expand'
		return
	}
	assert result[0] is ast.AssignStmt
	decl := result[0] as ast.AssignStmt
	assert decl.rhs[0] is ast.StringLiteral
	assert (decl.rhs[0] as ast.StringLiteral).value == "''"
}

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

fn test_error_propagation_in_plain_function_lowers_to_panic() {
	mut t := create_test_transformer()
	t.cur_fn_returns_result = false

	mut prefix_stmts := []ast.Stmt{}
	t.extract_or_expr(ast.Expr(ast.PostfixExpr{
		op:   .not
		expr: ast.Expr(ast.CallExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'may_fail'
			})
		})
	}), mut prefix_stmts)

	assert prefix_stmts.len == 2
	if_stmt := (prefix_stmts[1] as ast.ExprStmt).expr as ast.IfExpr
	assert if_stmt.stmts.len == 2
	panic_stmt := if_stmt.stmts[1] as ast.ExprStmt
	assert panic_stmt.expr is ast.CallExpr
	panic_call := panic_stmt.expr as ast.CallExpr
	assert panic_call.lhs is ast.Ident
	assert (panic_call.lhs as ast.Ident).name == 'panic'
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

fn test_if_guard_else_expr_wraps_expanded_else_if_guard() {
	mut t := create_test_transformer()
	unsafe_else := ast.UnsafeExpr{
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(ast.Ident{
					name: '_or_t1'
				})]
				rhs: [ast.Expr(ast.CallExpr{
					lhs: ast.Ident{
						name: 'second'
					}
				})]
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.IfExpr{
					cond:  ast.Ident{
						name: '_or_t1_ok'
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [ast.Expr(ast.Ident{
								name: 'value'
							})]
						}),
					]
				}
			}),
		]
	}

	result := t.transform_if_guard_else_expr(unsafe_else)

	assert result is ast.IfExpr, 'expected else block IfExpr, got ${result.type_name()}'
	result_if := result as ast.IfExpr
	assert result_if.cond is ast.EmptyExpr
	assert result_if.stmts.len == 2
	assert result_if.stmts[1] is ast.ExprStmt
	assert (result_if.stmts[1] as ast.ExprStmt).expr is ast.IfExpr
}

fn test_transform_if_expr_map_guard_uses_map_path() {
	mut t := create_transformer_with_vars({
		'm':      types.Type(types.Map{
			key_type:   string_type()
			value_type: string_type()
		})
		'prefix': string_type()
		'mod':    string_type()
	})

	if_expr := ast.IfExpr{
		cond:  ast.IfGuardExpr{
			stmt: ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(ast.Ident{
					name: 'alias'
				})]
				rhs: [
					ast.Expr(ast.GenericArgOrIndexExpr{
						lhs:  ast.Ident{
							name: 'm'
						}
						expr: ast.InfixExpr{
							op:  .plus
							lhs: ast.Ident{
								name: 'prefix'
							}
							rhs: ast.Ident{
								name: 'mod'
							}
						}
					}),
				]
			}
		}
		stmts: [ast.Stmt(ast.ExprStmt{
			expr: ast.Ident{
				name: 'alias'
			}
		})]
	}

	result := t.transform_if_expr(if_expr)

	assert result is ast.IfExpr, 'expected map guard IfExpr, got ${result.type_name()}'
	result_if := result as ast.IfExpr
	assert result_if.cond !is ast.IfGuardExpr
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

fn create_transformer_with_ast_expr_sumtype() &Transformer {
	mut t := create_test_transformer()
	expr_type := types.Type(types.NamedType('ast__Expr'))
	array_decompose_type := types.Type(types.Struct{
		name:   'ArrayDecompose'
		fields: [
			types.Field{
				name: 'expr'
				typ:  expr_type
			},
		]
	})
	ident_type := types.Type(types.Struct{
		name: 'Ident'
	})
	selector_type := types.Type(types.Struct{
		name: 'SelectorExpr'
	})
	infix_type := types.Type(types.Struct{
		name:   'InfixExpr'
		fields: [
			types.Field{
				name: 'left'
				typ:  expr_type
			},
		]
	})
	postfix_type := types.Type(types.Struct{
		name:   'PostfixExpr'
		fields: [
			types.Field{
				name: 'expr'
				typ:  expr_type
			},
		]
	})
	sum_type := types.Type(types.SumType{
		name:     'Expr'
		variants: [
			array_decompose_type,
			ident_type,
			selector_type,
			infix_type,
			postfix_type,
		]
	})
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('Expr', sum_type)
	ast_scope.insert('ArrayDecompose', array_decompose_type)
	ast_scope.insert('Ident', ident_type)
	ast_scope.insert('SelectorExpr', selector_type)
	ast_scope.insert('InfixExpr', infix_type)
	ast_scope.insert('PostfixExpr', postfix_type)
	t.cached_scopes = {
		'ast': ast_scope
	}
	t.cur_module = 'parser'
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('cond', sum_type)
	t.scope = scope
	return t
}

fn create_transformer_with_scope_object_sumtype() &Transformer {
	name_type := string_type()
	empty_scope_object_type := types.Type(types.Struct{
		name:   'EmptyScopeObject'
		fields: [
			types.Field{
				name: 'name'
				typ:  name_type
			},
		]
	})
	const_field_type := types.Type(types.Struct{
		name:   'ConstField'
		fields: [
			types.Field{
				name: 'mod'
				typ:  name_type
			},
			types.Field{
				name: 'name'
				typ:  name_type
			},
		]
	})
	scope_object_type := types.Type(types.SumType{
		name:     'ScopeObject'
		variants: [
			empty_scope_object_type,
			const_field_type,
		]
	})
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('EmptyScopeObject', empty_scope_object_type)
	ast_scope.insert('ConstField', const_field_type)
	ast_scope.insert('ScopeObject', scope_object_type)
	mut t := create_test_transformer()
	t.cur_module = 'ast'
	t.cached_scopes = {
		'ast': ast_scope
	}
	t.scope = types.new_scope(unsafe { nil })
	t.scope.insert('obj', scope_object_type)
	return t
}

fn test_sumtype_common_field_access_lowers_to_tag_dispatch() {
	mut t := create_transformer_with_scope_object_sumtype()

	result := t.transform_selector_expr(ast.SelectorExpr{
		lhs: ast.Ident{
			name: 'obj'
		}
		rhs: ast.Ident{
			name: 'name'
		}
	})

	assert result is ast.IfExpr, 'expected tag-dispatched IfExpr, got ${result.type_name()}'
	first_if := result as ast.IfExpr
	assert first_if.cond is ast.InfixExpr
	first_cond := first_if.cond as ast.InfixExpr
	assert first_cond.rhs is ast.BasicLiteral
	assert (first_cond.rhs as ast.BasicLiteral).value == '0'
	assert first_if.stmts.len == 1
	assert first_if.stmts[0] is ast.ExprStmt
	first_expr := (first_if.stmts[0] as ast.ExprStmt).expr
	assert selector_cast_type_name(first_expr) == 'ast__EmptyScopeObject*'
	assert first_if.else_expr is ast.IfExpr
	else_if := first_if.else_expr as ast.IfExpr
	assert else_if.cond is ast.EmptyExpr
	assert else_if.stmts.len == 1
	assert else_if.stmts[0] is ast.ExprStmt
	else_expr := (else_if.stmts[0] as ast.ExprStmt).expr
	assert selector_cast_type_name(else_expr) == 'ast__ConstField*'
}

fn test_sumtype_common_field_assignment_lowers_to_tag_dispatch() {
	mut t := create_transformer_with_scope_object_sumtype()

	expanded := t.try_expand_sumtype_common_field_assign_stmts(ast.AssignStmt{
		op:  .assign
		lhs: [
			ast.Expr(ast.SelectorExpr{
				lhs: ast.Ident{
					name: 'obj'
				}
				rhs: ast.Ident{
					name: 'name'
				}
			}),
		]
		rhs: [
			ast.Expr(ast.Ident{
				name: 'new_name'
			}),
		]
	}) or {
		assert false, 'expected common-field assignment expansion'
		return
	}

	assert expanded.len == 1
	assert expanded[0] is ast.ExprStmt
	if_expr := (expanded[0] as ast.ExprStmt).expr
	assert if_expr is ast.IfExpr
	first_if := if_expr as ast.IfExpr
	assert first_if.stmts.len == 1
	assert first_if.stmts[0] is ast.AssignStmt
	first_assign := first_if.stmts[0] as ast.AssignStmt
	assert selector_cast_type_name(first_assign.lhs[0]) == 'ast__EmptyScopeObject*'
	assert first_if.else_expr is ast.IfExpr
	else_if := first_if.else_expr as ast.IfExpr
	assert else_if.stmts.len == 1
	assert else_if.stmts[0] is ast.AssignStmt
	else_assign := else_if.stmts[0] as ast.AssignStmt
	assert selector_cast_type_name(else_assign.lhs[0]) == 'ast__ConstField*'
}

fn test_smartcast_variant_arg_to_pointer_param_uses_variant_address() {
	array_fixed_type := types.Type(types.Struct{
		name: 'ArrayFixed'
	})
	other_type := types.Type(types.Struct{
		name: 'Other'
	})
	sum_type := types.Type(types.SumType{
		name:     'TypeInfo'
		variants: [array_fixed_type, other_type]
	})
	mut t := create_test_transformer()
	t.scope = types.new_scope(unsafe { nil })
	t.scope.insert('info', sum_type)
	t.push_smartcast_full('info', 'ArrayFixed', 'ArrayFixed', 'TypeInfo')

	result := t.transform_call_arg_with_sumtype_check(ast.Ident{
		name: 'info'
	}, CallFnInfo{
		param_types: [
			types.Type(types.Pointer{
				base_type: array_fixed_type
			}),
		]
	}, 0)

	assert result is ast.PrefixExpr, 'expected address-of smartcasted variant, got ${result.type_name()}'
	prefix := result as ast.PrefixExpr
	assert prefix.op == .amp
	assert prefix.expr is ast.ParenExpr
	deref := (prefix.expr as ast.ParenExpr).expr
	assert deref is ast.PrefixExpr
	deref_prefix := deref as ast.PrefixExpr
	assert deref_prefix.op == .mul
	assert deref_prefix.expr is ast.CastExpr
	cast := deref_prefix.expr as ast.CastExpr
	assert cast.typ is ast.Ident
	assert (cast.typ as ast.Ident).name == 'ArrayFixed*'
}

fn test_or_block_unknown_fallback_call_is_kept_as_value() {
	mut t := create_test_transformer()
	mut prefix_stmts := []ast.Stmt{}
	t.expand_single_or_expr(ast.OrExpr{
		expr:  ast.CallExpr{
			lhs: ast.Ident{
				name: 'maybe_value'
			}
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.CallExpr{
					lhs: ast.SelectorExpr{
						lhs: ast.Ident{
							name: 'expr'
						}
						rhs: ast.Ident{
							name: 'str'
						}
					}
				}
			}),
		]
	}, mut prefix_stmts)

	assert prefix_stmts.len == 2
	assert prefix_stmts[1] is ast.ExprStmt
	if_stmt := (prefix_stmts[1] as ast.ExprStmt).expr
	assert if_stmt is ast.IfExpr
	if_expr := if_stmt as ast.IfExpr
	assert if_expr.stmts.len >= 2
	last_stmt := if_expr.stmts[if_expr.stmts.len - 1]
	assert last_stmt is ast.AssignStmt
	assign := last_stmt as ast.AssignStmt
	assert assign.lhs.len == 1
	assert assign.lhs[0] is ast.SelectorExpr
	assert (assign.lhs[0] as ast.SelectorExpr).rhs.name == 'data'
	assert assign.rhs.len == 1
	assert assign.rhs[0] is ast.CallExpr
}

fn test_or_block_nested_or_with_flow_fallback_is_kept_as_value() {
	mut t := create_test_transformer()
	mut prefix_stmts := []ast.Stmt{}
	t.expand_single_or_expr(ast.OrExpr{
		expr:  ast.CallExpr{
			lhs: ast.Ident{
				name: 'primary'
			}
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.OrExpr{
					expr:  ast.CallExpr{
						lhs: ast.Ident{
							name: 'secondary'
						}
					}
					stmts: [ast.Stmt(ast.FlowControlStmt{
						op: .key_continue
					})]
				}
			}),
		]
	}, mut prefix_stmts)

	assert prefix_stmts.len == 2
	assert prefix_stmts[1] is ast.ExprStmt
	if_stmt := (prefix_stmts[1] as ast.ExprStmt).expr
	assert if_stmt is ast.IfExpr
	if_expr := if_stmt as ast.IfExpr
	assert if_expr.stmts.len >= 2
	last_stmt := if_expr.stmts[if_expr.stmts.len - 1]
	assert last_stmt is ast.AssignStmt
	assign := last_stmt as ast.AssignStmt
	assert assign.lhs.len == 1
	assert assign.lhs[0] is ast.SelectorExpr
	assert (assign.lhs[0] as ast.SelectorExpr).rhs.name == 'data'
}

fn test_or_block_sumtype_fallback_value_is_wrapped_for_data() {
	mut t := create_transformer_with_ast_expr_sumtype()
	pos := token.Pos{
		id: 789
	}
	fallback := ast.CallExpr{
		lhs: ast.Ident{
			name: 'make_ident'
		}
		pos: pos
	}
	t.env.set_expr_type(pos.id, types.Type(types.Struct{
		name: 'Ident'
	}))
	data_type := t.lookup_var_type('cond') or {
		assert false, 'expected sumtype data type'
		return
	}

	result := t.or_fallback_value_for_data(fallback, data_type)

	assert result is ast.InitExpr, 'expected sumtype InitExpr, got ${result.type_name()}'
	init := result as ast.InitExpr
	assert init.typ is ast.Ident
	assert (init.typ as ast.Ident).name == 'ast__Expr'
	assert init.fields.len == 2
	assert init.fields[0].name == '_tag'
	assert init.fields[0].value is ast.BasicLiteral
	assert (init.fields[0].value as ast.BasicLiteral).value == '1'
	assert init.fields[1].name == '_data._Ident'
}

fn test_string_interpolation_named_sumtype_field_uses_str() {
	mut t := create_transformer_with_ast_expr_sumtype()
	arg_type := t.lookup_type('ast__ArrayDecompose') or {
		assert false, 'expected ArrayDecompose type'
		return
	}
	t.scope.insert('arg0', arg_type)
	selector_pos := token.Pos{
		id: 790
	}
	t.env.set_expr_type(selector_pos.id, types.Type(types.NamedType('ast__Expr')))

	result := t.transform_string_inter_literal(ast.StringInterLiteral{
		kind:   .v
		values: ['', '']
		inters: [
			ast.StringInter{
				format: .unformatted
				expr:   ast.SelectorExpr{
					lhs: ast.Ident{
						name: 'arg0'
					}
					rhs: ast.Ident{
						name: 'expr'
					}
					pos: selector_pos
				}
			},
		]
	})

	assert result is ast.StringInterLiteral
	lit := result as ast.StringInterLiteral
	assert lit.inters.len == 1
	assert lit.inters[0].resolved_fmt == '%s'
	assert lit.inters[0].expr is ast.SelectorExpr
	str_selector := lit.inters[0].expr as ast.SelectorExpr
	assert str_selector.rhs.name == 'str'
	assert str_selector.lhs is ast.CallExpr
	str_call := str_selector.lhs as ast.CallExpr
	assert str_call.lhs is ast.Ident
	assert (str_call.lhs as ast.Ident).name == 'ast__Expr__str'
}

fn test_generic_named_type_does_not_request_str_fn() {
	mut t := create_test_transformer()
	t.scope = types.new_scope(unsafe { nil })
	t.scope.insert('T', types.Type(types.NamedType('T')))

	if _ := t.get_str_fn_name_for_type(types.Type(types.NamedType('T'))) {
		assert false, 'generic T should not resolve to a str function'
	} else {
		assert true
	}
}

fn test_field_init_call_args_lower_to_struct_when_param_names_missing() {
	mut t := create_test_transformer()
	walker_type := types.Type(types.Struct{
		name:   'Walker'
		fields: [
			types.Field{
				name: 'table'
				typ:  types.Type(types.Pointer{
					base_type: types.Type(types.Struct{
						name: 'Table'
					})
				})
			},
			types.Field{
				name: 'trace_enabled'
				typ:  types.Type(types.bool_)
			},
		]
	})

	result := t.lower_field_init_call_args([
		ast.Expr(ast.FieldInit{
			name:  'table'
			value: ast.Ident{
				name: 'table'
			}
		}),
		ast.Expr(ast.FieldInit{
			name:  'trace_enabled'
			value: ast.BasicLiteral{
				kind: .key_true
			}
		}),
	], []string{}, [walker_type])

	assert result.len == 1
	assert result[0] is ast.InitExpr
	init := result[0] as ast.InitExpr
	assert init.fields.len == 2
	assert init.fields[0].name == 'table'
	assert init.fields[1].name == 'trace_enabled'
}

fn test_static_method_field_init_call_args_lower_to_struct() {
	files := transform_code_for_test('
struct Table {}

struct Walker {
	table &Table
	trace_enabled bool
}

fn Walker.new(params Walker) &Walker {
	return &Walker{}
}

fn make_walker(table &Table) &Walker {
	return Walker.new(
		table: table
		trace_enabled: true
	)
}
')
	assert files.len == 1
	file := files[0]
	for stmt in file.stmts {
		if stmt is ast.FnDecl && stmt.name == 'make_walker' {
			assert stmt.stmts.len == 1
			ret := stmt.stmts[0] as ast.ReturnStmt
			assert ret.exprs.len == 1
			assert ret.exprs[0] is ast.CallExpr
			call := ret.exprs[0] as ast.CallExpr
			assert call.lhs is ast.Ident
			assert (call.lhs as ast.Ident).name == 'Walker__new'
			assert call.args.len == 1
			assert call.args[0] is ast.InitExpr
			init := call.args[0] as ast.InitExpr
			assert init.fields.len == 2
			assert init.fields[0].name == 'table'
			assert init.fields[1].name == 'trace_enabled'
			return
		}
	}
	assert false, 'make_walker not found'
}

fn test_sumtype_string_interpolation_rewraps_variant_typed_arg() {
	mut t := create_transformer_with_ast_expr_sumtype()
	ident_type := t.lookup_type('ast__Ident') or {
		assert false, 'expected Ident type'
		return
	}
	t.scope.insert('variant', ident_type)
	variant_pos := token.Pos{
		id: 791
	}
	t.env.set_expr_type(variant_pos.id, types.Type(types.NamedType('ast__Expr')))

	result := t.transform_sprintf_arg(ast.StringInter{
		format: .unformatted
		expr:   ast.Ident{
			name: 'variant'
			pos:  variant_pos
		}
	})

	assert result is ast.SelectorExpr
	str_selector := result as ast.SelectorExpr
	assert str_selector.lhs is ast.CallExpr
	str_call := str_selector.lhs as ast.CallExpr
	assert str_call.args.len == 1
	assert str_call.args[0] is ast.InitExpr
	init := str_call.args[0] as ast.InitExpr
	assert init.typ is ast.Ident
	assert (init.typ as ast.Ident).name == 'ast__Expr'
	assert init.fields[0].name == '_tag'
	assert (init.fields[0].value as ast.BasicLiteral).value == '1'
	assert init.fields[1].name == '_data._Ident'
}

fn test_smartcast_sumtype_sprintf_arg_rewraps_variant() {
	mut t := create_transformer_with_ast_expr_sumtype()
	cond_pos := token.Pos{
		id: 792
	}
	t.env.set_expr_type(cond_pos.id, types.Type(types.NamedType('ast__Expr')))
	t.push_smartcast_full('cond', 'Ident', 'ast__Ident', 'ast__Expr')

	result := t.transform_sprintf_arg(ast.StringInter{
		format: .unformatted
		expr:   ast.Ident{
			name: 'cond'
			pos:  cond_pos
		}
	})

	assert result is ast.SelectorExpr
	str_selector := result as ast.SelectorExpr
	assert str_selector.lhs is ast.CallExpr
	str_call := str_selector.lhs as ast.CallExpr
	assert str_call.args[0] is ast.InitExpr
	init := str_call.args[0] as ast.InitExpr
	assert init.typ is ast.Ident
	assert (init.typ as ast.Ident).name == 'ast__Expr'
	assert (init.fields[0].value as ast.BasicLiteral).value == '1'
	assert init.fields[1].name == '_data._Ident'
}

fn test_smartcast_modifier_selector_sprintf_arg_rewraps_variant() {
	mut t := create_transformer_with_ast_expr_sumtype()
	cond_type := t.lookup_type('ast__InfixExpr') or {
		assert false, 'expected InfixExpr type'
		return
	}
	t.scope.insert('cond', cond_type)
	left_pos := token.Pos{
		id: 793
	}
	t.env.set_expr_type(left_pos.id, types.Type(types.NamedType('ast__Expr')))
	t.push_smartcast_full('cond.left', 'SelectorExpr', 'ast__SelectorExpr', 'ast__Expr')

	result := t.transform_sprintf_arg(ast.StringInter{
		format: .unformatted
		expr:   ast.ModifierExpr{
			kind: .key_mut
			expr: ast.SelectorExpr{
				lhs: ast.Ident{
					name: 'cond'
				}
				rhs: ast.Ident{
					name: 'left'
				}
				pos: left_pos
			}
			pos:  left_pos
		}
	})

	assert result is ast.SelectorExpr
	str_selector := result as ast.SelectorExpr
	assert str_selector.lhs is ast.CallExpr
	str_call := str_selector.lhs as ast.CallExpr
	assert str_call.args[0] is ast.InitExpr
	init := str_call.args[0] as ast.InitExpr
	assert init.typ is ast.Ident
	assert (init.typ as ast.Ident).name == 'ast__Expr'
	assert (init.fields[0].value as ast.BasicLiteral).value == '2'
	assert init.fields[1].name == '_data._SelectorExpr'
}

fn test_transform_not_is_selector_uses_outer_match_smartcast_for_tag_lhs() {
	mut t := create_transformer_with_ast_expr_sumtype()
	t.push_smartcast_full('cond', 'PostfixExpr', 'ast__PostfixExpr', 'ast__Expr')

	result := t.transform_infix_expr(ast.InfixExpr{
		op:  .not_is
		lhs: ast.SelectorExpr{
			lhs: ast.Ident{
				name: 'cond'
			}
			rhs: ast.Ident{
				name: 'expr'
			}
		}
		rhs: ast.SelectorExpr{
			lhs: ast.Ident{
				name: 'ast'
			}
			rhs: ast.Ident{
				name: 'Ident'
			}
		}
	})

	assert result is ast.InfixExpr, 'expected InfixExpr, got ${result.type_name()}'
	ne := result as ast.InfixExpr
	assert ne.op == .ne
	assert ne.lhs is ast.SelectorExpr
	tag_sel := ne.lhs as ast.SelectorExpr
	assert tag_sel.rhs.name == '_tag'
	assert tag_sel.lhs is ast.SelectorExpr
	field_sel := tag_sel.lhs as ast.SelectorExpr
	assert field_sel.rhs.name == 'expr'
	assert field_sel.lhs is ast.ParenExpr
	deref := (field_sel.lhs as ast.ParenExpr).expr
	assert deref is ast.PrefixExpr
	prefix := deref as ast.PrefixExpr
	assert prefix.expr is ast.CastExpr
	cast := prefix.expr as ast.CastExpr
	assert cast.typ is ast.Ident
	assert (cast.typ as ast.Ident).name == 'ast__PostfixExpr*'
	assert cast.expr is ast.SelectorExpr
	variant_sel := cast.expr as ast.SelectorExpr
	assert variant_sel.rhs.name == '_PostfixExpr'
}

fn test_transform_and_keeps_filter_pending_inside_short_circuit_branch() {
	mut t := create_transformer_with_vars({
		'ok':    types.Type(types.Primitive{
			props: types.Properties.boolean
		})
		'items': types.Type(types.Array{
			elem_type: types.Type(types.int_)
		})
	})

	result := t.transform_infix_expr(ast.InfixExpr{
		op:  .and
		lhs: ast.Ident{
			name: 'ok'
		}
		rhs: ast.InfixExpr{
			op:  .eq
			lhs: ast.SelectorExpr{
				lhs: ast.CallExpr{
					lhs:  ast.SelectorExpr{
						lhs: ast.Ident{
							name: 'items'
						}
						rhs: ast.Ident{
							name: 'filter'
						}
					}
					args: [
						ast.Expr(ast.InfixExpr{
							op:  .gt
							lhs: ast.Ident{
								name: 'it'
							}
							rhs: ast.BasicLiteral{
								kind:  .number
								value: '0'
							}
						}),
					]
				}
				rhs: ast.Ident{
					name: 'len'
				}
			}
			rhs: ast.BasicLiteral{
				kind:  .number
				value: '0'
			}
		}
	})

	assert result is ast.Ident, 'expected short-circuit temp ident, got ${result.type_name()}'
	temp_name := (result as ast.Ident).name
	assert temp_name.starts_with('_and_t')
	assert t.pending_stmts.len == 2
	assert t.pending_stmts[0] is ast.AssignStmt
	init_stmt := t.pending_stmts[0] as ast.AssignStmt
	assert init_stmt.op == .decl_assign
	assert init_stmt.rhs[0] is ast.BasicLiteral
	assert (init_stmt.rhs[0] as ast.BasicLiteral).kind == .key_false
	assert t.pending_stmts[1] is ast.ExprStmt
	outer_stmt := t.pending_stmts[1] as ast.ExprStmt
	assert outer_stmt.expr is ast.IfExpr
	if_expr := outer_stmt.expr as ast.IfExpr
	assert if_expr.cond is ast.Ident
	assert (if_expr.cond as ast.Ident).name == 'ok'
	assert if_expr.stmts.len >= 3, 'expected filter init, loop, and temp assignment inside then branch'
	last_stmt := if_expr.stmts[if_expr.stmts.len - 1]
	assert last_stmt is ast.AssignStmt
	assign_stmt := last_stmt as ast.AssignStmt
	assert assign_stmt.lhs[0] is ast.Ident
	assert (assign_stmt.lhs[0] as ast.Ident).name == temp_name
}

fn test_transform_if_is_and_all_keeps_all_pending_inside_tag_branch() {
	aggregate_type := types.Type(types.Struct{
		name:   'ast__Aggregate'
		fields: [
			types.Field{
				name: 'types'
				typ:  types.Type(types.Array{
					elem_type: types.Type(types.int_)
				})
			},
		]
	})
	other_type := types.Type(types.Struct{
		name: 'ast__Other'
	})
	info_type := types.Type(types.SumType{
		name:     'ast__Info'
		variants: [aggregate_type, other_type]
	})
	sym_type := types.Type(types.Struct{
		name:   'Sym'
		fields: [
			types.Field{
				name: 'info'
				typ:  info_type
			},
		]
	})
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('Aggregate', aggregate_type)
	ast_scope.insert('Other', other_type)
	ast_scope.insert('Info', info_type)
	mut t := create_test_transformer()
	t.cached_scopes = {
		'ast': ast_scope
	}
	t.scope = types.new_scope(unsafe { nil })
	t.scope.insert('sym', sym_type)

	sym_info := ast.Expr(ast.SelectorExpr{
		lhs: ast.Ident{
			name: 'sym'
		}
		rhs: ast.Ident{
			name: 'info'
		}
	})
	result := t.transform_if_expr(ast.IfExpr{
		cond:  ast.InfixExpr{
			op:  .and
			lhs: ast.InfixExpr{
				op:  .key_is
				lhs: sym_info
				rhs: ast.SelectorExpr{
					lhs: ast.Ident{
						name: 'ast'
					}
					rhs: ast.Ident{
						name: 'Aggregate'
					}
				}
			}
			rhs: ast.CallExpr{
				lhs:  ast.SelectorExpr{
					lhs: ast.SelectorExpr{
						lhs: sym_info
						rhs: ast.Ident{
							name: 'types'
						}
					}
					rhs: ast.Ident{
						name: 'all'
					}
				}
				args: [
					ast.Expr(ast.InfixExpr{
						op:  .gt
						lhs: ast.Ident{
							name: 'it'
						}
						rhs: ast.BasicLiteral{
							kind:  .number
							value: '0'
						}
					}),
				]
			}
		}
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.BasicLiteral{
						kind:  .key_true
						value: 'true'
					}),
				]
			}),
		]
	})

	assert t.pending_stmts.len == 0, 'rest pending statements must not escape the tag branch'
	assert result is ast.IfExpr, 'expected outer tag IfExpr, got ${result.type_name()}'
	outer_if := result as ast.IfExpr
	assert outer_if.stmts.len >= 3, 'expected all() init, loop, and inner if inside tag branch'
	assert outer_if.stmts[0] is ast.AssignStmt
	assert outer_if.stmts[1] is ast.ForStmt
	assert outer_if.stmts.last() is ast.ExprStmt
	inner_stmt := outer_if.stmts.last() as ast.ExprStmt
	assert inner_stmt.expr is ast.IfExpr
	inner_if := inner_stmt.expr as ast.IfExpr
	assert inner_if.cond is ast.Ident
	assert (inner_if.cond as ast.Ident).name.starts_with('_filter_t')
}

fn test_filter_pointer_array_push_uses_typed_slot_literal() {
	node_type := types.Type(types.Struct{
		name: 'Node'
	})
	mut t := create_transformer_with_vars({
		'items': types.Type(types.Array{
			elem_type: types.Type(types.Pointer{
				base_type: node_type
			})
		})
	})

	_ := t.try_expand_filter_or_map_expr(ast.CallExpr{
		lhs:  ast.SelectorExpr{
			lhs: ast.Ident{
				name: 'items'
			}
			rhs: ast.Ident{
				name: 'filter'
			}
		}
		args: [
			ast.Expr(ast.InfixExpr{
				op:  .ne
				lhs: ast.Ident{
					name: 'it'
				}
				rhs: ast.Ident{
					name: 'nil'
				}
			}),
		]
	}) or {
		assert false, 'expected pointer array filter to be expanded'
		return
	}

	assert t.pending_stmts.len >= 2
	assert t.pending_stmts.last() is ast.ForStmt
	for_stmt := t.pending_stmts.last() as ast.ForStmt
	mut push_arg := ast.Expr(ast.empty_expr)
	for stmt in for_stmt.stmts {
		if stmt is ast.ExprStmt && stmt.expr is ast.IfExpr {
			if_expr := stmt.expr as ast.IfExpr
			for inner_stmt in if_expr.stmts {
				if inner_stmt is ast.ExprStmt && inner_stmt.expr is ast.CallExpr {
					call := inner_stmt.expr as ast.CallExpr
					if call.lhs is ast.Ident && (call.lhs as ast.Ident).name == 'array__push' {
						assert call.args.len == 2
						push_arg = call.args[1]
					}
				}
			}
		}
	}
	assert push_arg !is ast.EmptyExpr
	assert push_arg is ast.PrefixExpr
	arg := push_arg as ast.PrefixExpr
	assert arg.op == .amp
	assert arg.expr is ast.ArrayInitExpr
	slot := arg.expr as ast.ArrayInitExpr
	assert slot.exprs.len == 1
	assert slot.typ is ast.Type
	slot_type := slot.typ as ast.Type
	assert slot_type is ast.ArrayFixedType
	fixed := slot_type as ast.ArrayFixedType
	assert fixed.elem_type is ast.Ident
	assert (fixed.elem_type as ast.Ident).name == 'Nodeptr'
}

fn test_return_addr_of_local_lowers_to_heap_copy() {
	pool_type := types.Type(types.Struct{
		name: 'PoolProcessor'
	})
	mut t := create_transformer_with_vars({
		'pool': pool_type
	})
	t.fn_root_scope = t.scope
	t.cur_fn_has_return_type_override = true
	t.cur_fn_return_type_override = types.Type(types.Pointer{
		base_type: pool_type
	})

	result := t.transform_stmts([
		ast.Stmt(ast.ReturnStmt{
			exprs: [
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: ast.Ident{
						name: 'pool'
					}
				}),
			]
		}),
	])

	assert result.len == 3
	assert result[0] is ast.AssignStmt
	decl := result[0] as ast.AssignStmt
	assert decl.op == .decl_assign
	assert decl.rhs[0] is ast.PrefixExpr
	heap_addr := decl.rhs[0] as ast.PrefixExpr
	assert heap_addr.op == .amp
	assert heap_addr.expr is ast.InitExpr
	assert result[1] is ast.AssignStmt
	copy_assign := result[1] as ast.AssignStmt
	assert copy_assign.lhs[0] is ast.PrefixExpr
	deref := copy_assign.lhs[0] as ast.PrefixExpr
	assert deref.op == .mul
	assert result[2] is ast.ReturnStmt
	ret := result[2] as ast.ReturnStmt
	assert ret.exprs[0] is ast.Ident
	assert (ret.exprs[0] as ast.Ident).name.starts_with('_ret_heap_t')
}

fn test_transform_drops_redundant_as_cast_after_narrowed_array_last() {
	mut t := create_test_transformer()
	t.array_elem_type_overrides['stmts'] = 'ast__ExprStmt'

	result := t.transform_expr(ast.AsCastExpr{
		expr: ast.CallExpr{
			lhs:  ast.SelectorExpr{
				lhs: ast.Ident{
					name: 'stmts'
				}
				rhs: ast.Ident{
					name: 'last'
				}
			}
			args: []
		}
		typ:  ast.SelectorExpr{
			lhs: ast.Ident{
				name: 'ast'
			}
			rhs: ast.Ident{
				name: 'ExprStmt'
			}
		}
	})

	assert result is ast.CallExpr, 'expected redundant as cast to be removed, got ${result.type_name()}'
	call := result as ast.CallExpr
	assert call.lhs is ast.SelectorExpr
	sel := call.lhs as ast.SelectorExpr
	assert sel.lhs is ast.ParenExpr
	typed_receiver := sel.lhs as ast.ParenExpr
	receiver_type := t.synth_types[typed_receiver.pos.id] or {
		assert false, 'expected narrowed array receiver type'
		types.Type(types.void_)
	}
	assert receiver_type is types.Array
	array_type := receiver_type as types.Array
	assert t.type_to_c_name(array_type.elem_type) == 'ast__ExprStmt'
}

fn test_transform_filter_then_array_last_as_uses_narrowed_receiver() {
	expr_stmt_type := types.Type(types.Struct{
		name: 'ExprStmt'
	})
	stmt_type := types.Type(types.SumType{
		name:     'Stmt'
		variants: [expr_stmt_type]
	})
	branch_type := types.Type(types.Struct{
		name:   'IfBranch'
		fields: [
			types.Field{
				name: 'stmts'
				typ:  types.Type(types.Array{
					elem_type: stmt_type
				})
			},
		]
	})
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('Stmt', stmt_type)
	ast_scope.insert('ExprStmt', expr_stmt_type)
	mut t := create_test_transformer()
	t.cached_scopes = {
		'ast': ast_scope
	}
	t.scope = types.new_scope(unsafe { nil })
	t.scope.insert('branch', branch_type)

	result := t.transform_stmts([
		ast.Stmt(ast.AssignStmt{
			op:  .decl_assign
			lhs: [
				ast.Expr(ast.Ident{
					name: 'stmts'
				}),
			]
			rhs: [
				ast.Expr(ast.CallExpr{
					lhs:  ast.SelectorExpr{
						lhs: ast.SelectorExpr{
							lhs: ast.Ident{
								name: 'branch'
							}
							rhs: ast.Ident{
								name: 'stmts'
							}
						}
						rhs: ast.Ident{
							name: 'filter'
						}
					}
					args: [
						ast.Expr(ast.InfixExpr{
							op:  .key_is
							lhs: ast.Ident{
								name: 'it'
							}
							rhs: ast.SelectorExpr{
								lhs: ast.Ident{
									name: 'ast'
								}
								rhs: ast.Ident{
									name: 'ExprStmt'
								}
							}
						}),
					]
				}),
			]
		}),
		ast.Stmt(ast.ExprStmt{
			expr: ast.IfExpr{
				cond:  ast.InfixExpr{
					op:  .gt
					lhs: ast.SelectorExpr{
						lhs: ast.Ident{
							name: 'stmts'
						}
						rhs: ast.Ident{
							name: 'len'
						}
					}
					rhs: ast.BasicLiteral{
						kind:  .number
						value: '0'
					}
				}
				stmts: [
					ast.Stmt(ast.AssignStmt{
						op:  .decl_assign
						lhs: [
							ast.Expr(ast.Ident{
								name: 'last_expr'
							}),
						]
						rhs: [
							ast.Expr(ast.AsCastExpr{
								expr: ast.CallExpr{
									lhs:  ast.SelectorExpr{
										lhs: ast.Ident{
											name: 'stmts'
										}
										rhs: ast.Ident{
											name: 'last'
										}
									}
									args: []
								}
								typ:  ast.SelectorExpr{
									lhs: ast.Ident{
										name: 'ast'
									}
									rhs: ast.Ident{
										name: 'ExprStmt'
									}
								}
							}),
						]
					}),
				]
			}
		}),
	])

	stmts_type := t.lookup_var_type('stmts') or {
		assert false, 'expected narrowed stmts local type'
		types.Type(types.void_)
	}
	assert stmts_type is types.Array
	stmts_array_type := stmts_type as types.Array
	assert t.type_to_c_name(stmts_array_type.elem_type) == 'ast__ExprStmt'
	assert result.last() is ast.ExprStmt
	if_stmt := result.last() as ast.ExprStmt
	assert if_stmt.expr is ast.IfExpr
	if_expr := if_stmt.expr as ast.IfExpr
	assert if_expr.stmts[0] is ast.AssignStmt
	assign := if_expr.stmts[0] as ast.AssignStmt
	assert assign.rhs[0] is ast.CallExpr
	call := assign.rhs[0] as ast.CallExpr
	assert call.lhs is ast.SelectorExpr
	sel := call.lhs as ast.SelectorExpr
	assert sel.lhs is ast.ParenExpr
	typed_receiver := sel.lhs as ast.ParenExpr
	receiver_type := t.synth_types[typed_receiver.pos.id] or {
		assert false, 'expected narrowed receiver type after filter'
		types.Type(types.void_)
	}
	assert receiver_type is types.Array
	array_type := receiver_type as types.Array
	assert t.type_to_c_name(array_type.elem_type) == 'ast__ExprStmt'
}

fn test_typed_array_method_receiver_handles_resolved_array_helper_call() {
	mut t := create_test_transformer()
	result := t.with_typed_array_method_receiver(ast.Expr(ast.CallExpr{
		lhs:  ast.Ident{
			name: 'array__last'
		}
		args: [
			ast.Expr(ast.Ident{
				name: 'stmts'
				pos:  token.Pos{
					id: 123
				}
			}),
		]
	}), types.Type(types.Struct{
		name: 'ast__ExprStmt'
	}))

	assert result is ast.CallExpr
	call := result as ast.CallExpr
	assert call.args[0] is ast.ParenExpr
	typed_arg := call.args[0] as ast.ParenExpr
	assert typed_arg.pos.id == 123
	receiver_type := t.synth_types[typed_arg.pos.id] or {
		assert false, 'expected narrowed array helper argument type'
		types.Type(types.void_)
	}
	assert receiver_type is types.Array
	array_type := receiver_type as types.Array
	assert t.type_to_c_name(array_type.elem_type) == 'ast__ExprStmt'
}

fn test_smartcast_string_interpolation_hoists_transformed_field_type() {
	call_expr_type := types.Type(types.Struct{
		name: 'CallExpr'
	})
	go_expr_type := types.Type(types.Struct{
		name:   'GoExpr'
		fields: [
			types.Field{
				name: 'call_expr'
				typ:  call_expr_type
			},
		]
	})
	expr_type := types.Type(types.SumType{
		name:     'Expr'
		variants: [go_expr_type]
	})
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('CallExpr', call_expr_type)
	ast_scope.insert('GoExpr', go_expr_type)
	ast_scope.insert('Expr', expr_type)
	mut t := create_test_transformer()
	t.cur_module = 'ast'
	t.cached_scopes = {
		'ast': ast_scope
	}
	t.scope = types.new_scope(unsafe { nil })
	t.scope.insert('x', expr_type)
	selector_pos := token.Pos{
		id: 456
	}
	t.env.set_expr_type(selector_pos.id, expr_type)
	t.push_smartcast_full('x', 'GoExpr', 'ast__GoExpr', 'ast__Expr')

	t.transform_string_inter_literal(ast.StringInterLiteral{
		kind:   .v
		values: ['go ', '']
		inters: [
			ast.StringInter{
				format: .unformatted
				expr:   ast.SelectorExpr{
					lhs: ast.Ident{
						name: 'x'
					}
					rhs: ast.Ident{
						name: 'call_expr'
					}
					pos: selector_pos
				}
			},
		]
	})

	assert t.pending_stmts.len == 1
	assign := t.pending_stmts[0] as ast.AssignStmt
	assert assign.lhs[0] is ast.Ident
	tmp_name := (assign.lhs[0] as ast.Ident).name
	tmp_type := t.lookup_var_type(tmp_name) or {
		assert false, 'expected interpolation temp type'
		types.Type(types.void_)
	}
	assert t.type_to_c_name(tmp_type) == 'ast__CallExpr'
}

fn test_addr_of_sumtype_constructor_heap_allocates_wrapper_and_payload() {
	hash_stmt_type := types.Type(types.Struct{
		name: 'ast__HashStmt'
	})
	hash_stmt_node_type := types.Type(types.SumType{
		name:     'ast__HashStmtNode'
		variants: [hash_stmt_type]
	})
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('HashStmt', hash_stmt_type)
	ast_scope.insert('HashStmtNode', hash_stmt_node_type)
	mut local_scope := types.new_scope(unsafe { nil })
	local_scope.insert('node', hash_stmt_type)
	mut t := create_test_transformer()
	t.cur_module = 'ast'
	t.cached_scopes = {
		'ast': ast_scope
	}
	t.scope = local_scope
	t.fn_root_scope = local_scope

	result := t.transform_expr(ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: ast.Expr(ast.CallOrCastExpr{
			lhs:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'ast'
				})
				rhs: ast.Ident{
					name: 'HashStmtNode'
				}
			})
			expr: ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: ast.Expr(ast.Ident{
					name: 'node'
				})
			})
		})
	}))

	assert result is ast.UnsafeExpr
	unsafe_expr := result as ast.UnsafeExpr
	assert unsafe_expr.stmts.len == 5
	payload_assign := unsafe_expr.stmts[1] as ast.AssignStmt
	assert payload_assign.lhs[0] is ast.PrefixExpr
	assert payload_assign.rhs[0] is ast.Ident
	assert (payload_assign.rhs[0] as ast.Ident).name == 'node'

	sum_assign := unsafe_expr.stmts[3] as ast.AssignStmt
	assert sum_assign.rhs[0] is ast.InitExpr
	sum_init := sum_assign.rhs[0] as ast.InitExpr
	assert sum_init.fields[1].name == '_data._HashStmt'
	assert sum_init.fields[1].value is ast.CastExpr
	data_cast := sum_init.fields[1].value as ast.CastExpr
	assert data_cast.expr is ast.PrefixExpr
	data_addr := data_cast.expr as ast.PrefixExpr
	assert data_addr.expr is ast.PrefixExpr
	data_deref := data_addr.expr as ast.PrefixExpr
	assert data_deref.expr is ast.Ident
	assert (data_deref.expr as ast.Ident).name != 'node'
}

fn test_addr_of_sumtype_constructor_derefs_mut_param_payload() {
	alias_decl_type := types.Type(types.Struct{
		name: 'ast__AliasTypeDecl'
	})
	type_decl_type := types.Type(types.SumType{
		name:     'ast__TypeDecl'
		variants: [alias_decl_type]
	})
	mut ast_scope := types.new_scope(unsafe { nil })
	ast_scope.insert('AliasTypeDecl', alias_decl_type)
	ast_scope.insert('TypeDecl', type_decl_type)
	mut local_scope := types.new_scope(unsafe { nil })
	local_scope.insert('node', alias_decl_type)
	mut t := create_test_transformer()
	t.cur_module = 'ast'
	t.cached_scopes = {
		'ast': ast_scope
	}
	t.scope = local_scope
	t.fn_root_scope = local_scope
	t.cur_fn_mut_params = ['node']

	result := t.transform_expr(ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: ast.Expr(ast.CallOrCastExpr{
			lhs:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'ast'
				})
				rhs: ast.Ident{
					name: 'TypeDecl'
				}
			})
			expr: ast.Expr(ast.Ident{
				name: 'node'
			})
		})
	}))

	assert result is ast.UnsafeExpr
	unsafe_expr := result as ast.UnsafeExpr
	assert unsafe_expr.stmts.len == 5
	payload_assign := unsafe_expr.stmts[1] as ast.AssignStmt
	assert payload_assign.rhs[0] is ast.PrefixExpr
	payload_rhs := payload_assign.rhs[0] as ast.PrefixExpr
	assert payload_rhs.op == .mul
	assert payload_rhs.expr is ast.Ident
	assert (payload_rhs.expr as ast.Ident).name == 'node'
}

fn test_else_if_result_guard_keeps_lowered_guard_in_else_branch() {
	mut t := create_test_transformer()
	t.scope = types.new_scope(unsafe { nil })
	call_pos := token.Pos{
		id: 900
	}
	ct_value_type := types.Type(types.SumType{
		name:     'ast__ComptTimeConstValue'
		variants: [types.Type(types.voidptr_)]
	})
	t.env.set_expr_type(call_pos.id, types.Type(types.ResultType{
		base_type: ct_value_type
	}))

	result := t.transform_if_expr(ast.IfExpr{
		cond:      ast.Ident{
			name: 'ok'
		}
		stmts:     [
			ast.Stmt(ast.AssignStmt{
				op:  .assign
				lhs: [
					ast.Expr(ast.Ident{
						name: 'seen'
					}),
				]
				rhs: [
					ast.Expr(ast.BasicLiteral{
						kind:  .key_true
						value: 'true'
					}),
				]
			}),
		]
		else_expr: ast.Expr(ast.IfExpr{
			cond:  ast.IfGuardExpr{
				stmt: ast.AssignStmt{
					op:  .decl_assign
					lhs: [
						ast.Expr(ast.Ident{
							name: 'value'
						}),
					]
					rhs: [
						ast.Expr(ast.CallExpr{
							lhs: ast.Ident{
								name: 'maybe_value'
							}
							pos: call_pos
						}),
					]
				}
			}
			stmts: [
				ast.Stmt(ast.AssignStmt{
					op:  .assign
					lhs: [
						ast.Expr(ast.Ident{
							name: 'seen'
						}),
					]
					rhs: [
						ast.Expr(ast.BasicLiteral{
							kind:  .key_true
							value: 'true'
						}),
					]
				}),
			]
		})
	})

	assert result is ast.IfExpr
	outer_if := result as ast.IfExpr
	assert outer_if.else_expr is ast.IfExpr
	else_block := outer_if.else_expr as ast.IfExpr
	assert else_block.cond is ast.EmptyExpr
	assert else_block.stmts.len == 2
	assert else_block.stmts[0] is ast.AssignStmt
	assert else_block.stmts[1] is ast.ExprStmt
	inner_stmt := else_block.stmts[1] as ast.ExprStmt
	assert inner_stmt.expr is ast.IfExpr
	inner_if := inner_stmt.expr as ast.IfExpr
	assert inner_if.stmts[0] is ast.AssignStmt
	guard_assign := inner_if.stmts[0] as ast.AssignStmt
	assert guard_assign.rhs[0] is ast.SelectorExpr
	value_type := t.lookup_var_type('value') or {
		assert false, 'expected if-guard variable to use result payload type'
		types.Type(types.voidptr_)
	}
	assert t.type_to_c_name(value_type) == 'ast__ComptTimeConstValue'
}
