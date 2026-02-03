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
		env:               unsafe { env }
		flag_enum_names:   map[string]bool{}
		var_enum_types:    map[string]string{}
		param_types:       map[string]string{}
		var_types:         map[string]string{}
		fn_return_types:   map[string]string{}
		fn_returns_result: map[string]string{}
		fn_returns_option: map[string]string{}
	}
}

fn test_array_comparison_eq() {
	mut t := create_test_transformer()
	// Set up variable types so infer_array_type can detect them
	t.var_types['arr1'] = 'Array_int'
	t.var_types['arr2'] = 'Array_int'

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
	mut t := create_test_transformer()
	t.var_types['arr1'] = 'Array_int'
	t.var_types['arr2'] = 'Array_int'

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
	mut t := create_test_transformer()
	// Variables not in var_types, so not detected as arrays
	t.var_types['x'] = 'int'
	t.var_types['y'] = 'int'

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
	mut t := create_test_transformer()
	t.var_types['my_arr'] = 'Array_string'

	expr := ast.Ident{
		name: 'my_arr'
	}

	result := t.infer_array_type(expr) or {
		assert false, 'expected array type to be inferred'
		return
	}
	assert result == 'Array_string'
}

fn test_infer_array_type_from_slice() {
	mut t := create_test_transformer()
	t.var_types['arr'] = 'Array_rune'

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
