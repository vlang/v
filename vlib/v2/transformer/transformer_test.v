// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v2.ast
import v2.types

// Helper to create a minimal transformer for testing
fn create_test_transformer() &Transformer {
	env := &types.Environment{}
	return &Transformer{
		env: unsafe { env }
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
		env:   unsafe { env }
		scope: scope
	}
}

// Create a string-like type that returns 'string' from name()
fn string_type() types.Type {
	return types.Alias{
		name: 'string'
	}
}

// Create a rune-like type that returns 'rune' from name()
fn rune_type() types.Type {
	return types.Alias{
		name: 'rune'
	}
}

fn test_array_comparison_eq() {
	// Set up variable types so infer_array_type can detect them
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

fn test_infer_array_type_from_var() {
	mut t := create_transformer_with_vars({
		'my_arr': types.Type(types.Array{ elem_type: string_type() })
	})

	expr := ast.Ident{
		name: 'my_arr'
	}

	result := t.infer_array_type(expr) or {
		assert false, 'expected array type to be inferred'
		return
	}
	assert result == 'Array_string', 'expected Array_string, got ${result}'
}

fn test_infer_array_type_from_slice() {
	mut t := create_transformer_with_vars({
		'arr': types.Type(types.Array{ elem_type: rune_type() })
	})

	// Create: arr[0..5] (slice expression)
	expr := ast.IndexExpr{
		lhs:  ast.Ident{
			name: 'arr'
		}
		expr: ast.RangeExpr{
			op:    .dotdot
			start: ast.BasicLiteral{
				value: '0'
				kind:  .number
			}
			end:   ast.BasicLiteral{
				value: '5'
				kind:  .number
			}
		}
	}

	result := t.infer_array_type(expr) or {
		assert false, 'expected array type to be inferred for slice'
		return
	}
	assert result == 'Array_rune', 'expected Array_rune, got ${result}'
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
