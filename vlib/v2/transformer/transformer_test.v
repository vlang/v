// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v2.ast
import v2.pref as vpref
import v2.types

// Helper to create a minimal transformer for testing
fn create_test_transformer() &Transformer {
	env := &types.Environment{}
	return &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
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
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
	}
}

// string_type returns the builtin v2 string type.
fn string_type() types.Type {
	return types.string_
}

// Create a rune-like type that returns 'rune' from name()
fn rune_type() types.Type {
	return types.Alias{
		name: 'rune'
	}
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
