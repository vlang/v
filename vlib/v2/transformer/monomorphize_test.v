// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: !windows
module transformer

import v2.ast
import v2.token
import v2.types
import v2.pref as vpref

fn mono_test_transformer() &Transformer {
	env := &types.Environment{}
	return &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
	}
}

// substitute_type: primitives

fn test_substitute_type_returns_same_type_when_bindings_empty() {
	original := types.Type(types.NamedType('T'))
	out := substitute_type(original, map[string]types.Type{})
	assert out.name() == 'T'
}

fn test_substitute_type_replaces_placeholder_named_type() {
	bindings := {
		'T': types.Type(types.int_)
	}
	out := substitute_type(types.Type(types.NamedType('T')), bindings)
	assert out.name() == 'int'
}

fn test_substitute_type_leaves_non_matching_named_type_alone() {
	bindings := {
		'T': types.Type(types.int_)
	}
	out := substitute_type(types.Type(types.NamedType('U')), bindings)
	assert out.name() == 'U'
}

// substitute_type: compound recursion

fn test_substitute_type_recurses_into_pointer_base() {
	bindings := {
		'T': types.Type(types.int_)
	}
	original := types.Type(types.Pointer{
		base_type: types.Type(types.NamedType('T'))
	})
	out := substitute_type(original, bindings)
	if out is types.Pointer {
		assert out.base_type.name() == 'int'
	} else {
		assert false, 'expected Pointer, got ${out.type_name()}'
	}
}

fn test_substitute_type_recurses_into_array_elem() {
	bindings := {
		'T': types.Type(types.int_)
	}
	original := types.Type(types.Array{
		elem_type: types.Type(types.NamedType('T'))
	})
	out := substitute_type(original, bindings)
	if out is types.Array {
		assert out.elem_type.name() == 'int'
	} else {
		assert false, 'expected Array, got ${out.type_name()}'
	}
}

fn test_substitute_type_recurses_into_map_key_and_value() {
	bindings := {
		'K': types.Type(types.string_)
		'V': types.Type(types.int_)
	}
	original := types.Type(types.Map{
		key_type:   types.Type(types.NamedType('K'))
		value_type: types.Type(types.NamedType('V'))
	})
	out := substitute_type(original, bindings)
	if out is types.Map {
		assert out.key_type.name() == 'string'
		assert out.value_type.name() == 'int'
	} else {
		assert false, 'expected Map, got ${out.type_name()}'
	}
}

fn test_substitute_type_recurses_into_option_base() {
	bindings := {
		'T': types.Type(types.int_)
	}
	original := types.Type(types.OptionType{
		base_type: types.Type(types.NamedType('T'))
	})
	out := substitute_type(original, bindings)
	if out is types.OptionType {
		assert out.base_type.name() == 'int'
	} else {
		assert false, 'expected OptionType, got ${out.type_name()}'
	}
}

fn test_substitute_type_recurses_into_nested_array_pointer() {
	bindings := {
		'T': types.Type(types.f64_)
	}
	original := types.Type(types.Array{
		elem_type: types.Type(types.Pointer{
			base_type: types.Type(types.NamedType('T'))
		})
	})
	out := substitute_type(original, bindings)
	if out is types.Array {
		elem := out.elem_type
		if elem is types.Pointer {
			assert elem.base_type.name() == 'f64'
		} else {
			assert false, 'expected nested Pointer, got ${elem.type_name()}'
		}
	} else {
		assert false, 'expected outer Array, got ${out.type_name()}'
	}
}

// substitute_type_in_expr: AST-level

fn test_substitute_type_in_expr_replaces_placeholder_ident() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	expr := ast.Expr(ast.Ident{
		name: 'T'
	})
	out := t.substitute_type_in_expr(expr, bindings)
	if out is ast.Ident {
		assert out.name == 'int'
	} else {
		assert false, 'expected Ident, got ${out.type_name()}'
	}
}

fn test_substitute_type_in_expr_recurses_into_array_type() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	expr := ast.Expr(ast.Type(ast.ArrayType{
		elem_type: ast.Expr(ast.Ident{
			name: 'T'
		})
	}))
	out := t.substitute_type_in_expr(expr, bindings)
	out_ty := out as ast.Type
	at := out_ty as ast.ArrayType
	elem := at.elem_type as ast.Ident
	assert elem.name == 'int'
}

fn test_substitute_type_in_expr_recurses_into_map_type() {
	mut t := mono_test_transformer()
	bindings := {
		'K': types.Type(types.string_)
		'V': types.Type(types.int_)
	}
	expr := ast.Expr(ast.Type(ast.MapType{
		key_type:   ast.Expr(ast.Ident{
			name: 'K'
		})
		value_type: ast.Expr(ast.Ident{
			name: 'V'
		})
	}))
	out := t.substitute_type_in_expr(expr, bindings)
	out_ty := out as ast.Type
	mt := out_ty as ast.MapType
	key := mt.key_type as ast.Ident
	val := mt.value_type as ast.Ident
	assert key.name == 'string'
	assert val.name == 'int'
}

// specialized_fn_name: matches cleanc's naming

fn test_specialized_fn_name_uses_all_placeholders_suffix_when_concrete_is_self() {
	t := mono_test_transformer()
	decl := ast.FnDecl{
		name: 'foo'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
	}
	bindings := {
		'T': types.Type(types.NamedType('T'))
	}
	assert t.specialized_fn_name(decl, bindings) == 'foo_T'
}

fn test_specialized_fn_name_uses_concrete_token_suffix() {
	t := mono_test_transformer()
	decl := ast.FnDecl{
		name: 'foo'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
	}
	bindings := {
		'T': types.Type(types.int_)
	}
	assert t.specialized_fn_name(decl, bindings) == 'foo_T_int'
}

fn test_specialized_fn_name_handles_multiple_params() {
	t := mono_test_transformer()
	decl := ast.FnDecl{
		name: 'pair'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'K'
				}),
				ast.Expr(ast.Ident{
					name: 'V'
				}),
			]
		}
	}
	bindings := {
		'K': types.Type(types.string_)
		'V': types.Type(types.int_)
	}
	assert t.specialized_fn_name(decl, bindings) == 'pair_T_string_int'
}

// clone_fn_decl_with_substitutions: end-to-end on a trivial generic fn

fn test_clone_fn_decl_renames_and_substitutes_param_and_return_types() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	body_pos := token.Pos{
		id: 1
	}
	decl := ast.FnDecl{
		name:  'identity'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'x'
					typ:  ast.Expr(ast.Ident{
						name: 'T'
					})
				},
			]
			return_type:    ast.Expr(ast.Ident{
				name: 'T'
			})
		}
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.Ident{
						name: 'x'
						pos:  body_pos
					}),
				]
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, bindings, 'identity_T_int')
	assert cloned.name == 'identity_T_int'
	assert cloned.typ.generic_params.len == 0
	assert cloned.typ.params.len == 1
	param_typ := cloned.typ.params[0].typ as ast.Ident
	assert param_typ.name == 'int'
	ret_typ := cloned.typ.return_type as ast.Ident
	assert ret_typ.name == 'int'
	// Body is preserved structurally (x is a value identifier, not a type — stays 'x')
	assert cloned.stmts.len == 1
	if cloned.stmts[0] is ast.ReturnStmt {
		ret_stmt := cloned.stmts[0] as ast.ReturnStmt
		ret_ident := ret_stmt.exprs[0] as ast.Ident
		assert ret_ident.name == 'x'
	} else {
		assert false, 'expected ReturnStmt'
	}
}

fn test_clone_fn_decl_substitutes_inside_array_param_type() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	decl := ast.FnDecl{
		name: 'first'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'arr'
					typ:  ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Ident{
							name: 'T'
						})
					}))
				},
			]
			return_type:    ast.Expr(ast.Ident{
				name: 'T'
			})
		}
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, bindings, 'first_T_int')
	assert cloned.name == 'first_T_int'
	param_typ := cloned.typ.params[0].typ as ast.Type
	arr_typ := param_typ as ast.ArrayType
	elem := arr_typ.elem_type as ast.Ident
	assert elem.name == 'int'
}

fn test_clone_fn_decl_preserves_unrelated_stmts() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	decl := ast.FnDecl{
		name:  'noop'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '42'
				})
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, bindings, 'noop_T_int')
	assert cloned.stmts.len == 1
	es := cloned.stmts[0] as ast.ExprStmt
	lit := es.expr as ast.BasicLiteral
	assert lit.value == '42'
}
