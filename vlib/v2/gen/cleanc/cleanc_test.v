module cleanc

import v2.ast
import v2.token
import v2.types

fn test_c_string_literal_content_to_c_single_line() {
	out := c_string_literal_content_to_c('hello')
	assert out == '"hello"'
}

fn test_c_string_literal_content_to_c_multiline() {
	out := c_string_literal_content_to_c('hello\nworld')
	assert out == '"hello\\n"\n"world"'
}

fn test_c_string_literal_content_to_c_trailing_newline() {
	out := c_string_literal_content_to_c('hello\n')
	assert out == '"hello\\n"\n""'
}

fn test_c_string_literal_content_to_c_escapes_quote() {
	out := c_string_literal_content_to_c('say "hello"')
	assert out == '"say \\"hello\\""'
}

fn test_c_string_literal_content_to_c_preserves_percent_placeholders() {
	out := c_string_literal_content_to_c('"%s"')
	assert out == '"\\"%s\\""'
}

fn test_c_string_literal_content_to_c_splits_hex_escape_before_hex_digit() {
	out := c_string_literal_content_to_c(r'\x0c8')
	assert out == '"\\x0c""8"'
}

fn test_fixed_array_elem_type_ready_accepts_primitive_alias() {
	mut g := Gen.new([])
	g.primitive_type_aliases['sha3__Lane'] = true
	assert g.fixed_array_elem_type_ready('sha3__Lane')
}

fn test_fixed_array_elem_type_ready_waits_for_alias_base() {
	mut g := Gen.new([])
	g.alias_base_types['foo__Alias'] = 'foo__Thing'
	assert !g.fixed_array_elem_type_ready('foo__Alias')
	g.emitted_types['body_foo__Thing'] = true
	assert g.fixed_array_elem_type_ready('foo__Alias')
}

fn test_types_type_to_c_prefixes_declared_c_structs() {
	mut g := Gen.new([])
	g.c_struct_types['kevent'] = true
	assert g.types_type_to_c(types.Type(types.Struct{
		name: 'kevent'
	})) == 'struct kevent'

	g.typedef_c_types['kevent'] = true
	assert g.types_type_to_c(types.Type(types.Struct{
		name: 'kevent'
	})) == 'kevent'
}

fn test_generic_struct_instance_lookup_mangles_c_struct_type_keys() {
	mut g := Gen.new([])
	g.generic_struct_instances['Box'] = [
		GenericStructInstance{
			params_key: 'int'
			c_name:     'Box'
		},
		GenericStructInstance{
			params_key: 'struct_cJSON'
			c_name:     'Box_T_struct_cJSON'
		},
	]

	resolved := g.resolve_generic_struct_c_name('Box', [
		ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'C'
			})
			rhs: ast.Ident{
				name: 'cJSON'
			}
		}),
	])
	assert resolved == 'Box_T_struct_cJSON'
}

fn test_generic_index_equality_uses_active_concrete_type() {
	mut env := types.Environment.new()
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('a', types.Type(types.Array{
		elem_type: types.Type(types.NamedType('T'))
	}))
	fn_scope.insert('e', types.Type(types.NamedType('T')))
	env.set_expr_type(1, types.string_)
	env.set_expr_type(2, types.int_)
	env.set_expr_type(3, types.string_)
	env.set_expr_type(4, types.string_)

	mut g := Gen.new_with_env([], env)
	g.cur_fn_scope = fn_scope
	g.active_generic_types['T'] = types.Type(types.Struct{
		name: 'Item'
	})
	node := ast.InfixExpr{
		op:  .eq
		lhs: ast.Expr(ast.IndexExpr{
			lhs:  ast.Expr(ast.Ident{
				pos:  token.Pos{
					id: 1
				}
				name: 'a'
			})
			expr: ast.Expr(ast.Ident{
				pos:  token.Pos{
					id: 2
				}
				name: 'idx'
			})
			pos:  token.Pos{
				id: 3
			}
		})
		rhs: ast.Expr(ast.Ident{
			pos:  token.Pos{
				id: 4
			}
			name: 'e'
		})
	}
	g.gen_infix_expr(&node)
	out := g.sb.str()
	assert out.contains('memcmp')
	assert out.contains('Item')
	assert !out.contains('string__eq')

	mut g2 := Gen.new_with_env([], env)
	g2.runtime_local_types['a'] = 'Array_Item'
	g2.runtime_local_types['e'] = 'Item'
	g2.gen_infix_expr(&node)
	out2 := g2.sb.str()
	assert out2.contains('memcmp')
	assert out2.contains('Item')
	assert !out2.contains('string__eq')
}

fn test_map_equality_uses_map_eq_function() {
	mut g := Gen.new([])
	g.runtime_local_types['a'] = 'Map_string_string'
	g.runtime_local_types['b'] = 'Map_string_string'
	node := ast.InfixExpr{
		op:  .eq
		lhs: ast.Expr(ast.Ident{
			name: 'a'
		})
		rhs: ast.Expr(ast.Ident{
			name: 'b'
		})
	}
	g.gen_infix_expr(&node)
	assert g.sb.str() == 'Map_string_string_map_eq(a, b)'
}

fn test_mut_receiver_operator_assignment_dereferences_receiver() {
	mut g := Gen.new([])
	g.runtime_local_types['result'] = 'Big*'
	g.runtime_local_types['ten'] = 'Big'
	g.cur_fn_mut_params['result'] = true
	g.fn_return_types['Big__mul'] = 'Big'
	node := ast.AssignStmt{
		op:  .assign
		lhs: [ast.Expr(ast.Ident{
			name: 'result'
		})]
		rhs: [
			ast.Expr(ast.InfixExpr{
				op:  .mul
				lhs: ast.Expr(ast.Ident{
					name: 'result'
				})
				rhs: ast.Expr(ast.Ident{
					name: 'ten'
				})
			}),
		]
	}
	g.gen_assign_stmt(node)
	out := g.sb.str().trim_space()
	assert out == '*result = Big__mul((*result), ten);'
}

fn test_decl_from_mut_receiver_operator_uses_value_type() {
	mut g := Gen.new([])
	g.runtime_local_types['result'] = 'Big*'
	g.runtime_local_types['ten'] = 'Big'
	g.cur_fn_mut_params['result'] = true
	g.fn_return_types['Big__mul'] = 'Big'
	node := ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'next'
		})]
		rhs: [
			ast.Expr(ast.InfixExpr{
				op:  .mul
				lhs: ast.Expr(ast.Ident{
					name: 'result'
				})
				rhs: ast.Expr(ast.Ident{
					name: 'ten'
				})
			}),
		]
	}
	g.gen_assign_stmt(node)
	out := g.sb.str().trim_space()
	assert out == 'Big next = Big__mul((*result), ten);'
	assert !out.contains('Big* next')
}

fn test_nested_mut_receiver_operator_does_not_deref_value_result_with_pointer_env() {
	mut env := types.Environment.new()
	env.set_expr_type(25, types.Type(types.Pointer{
		base_type: types.Struct{
			name: 'Big'
		}
	}))
	mut g := Gen.new_with_env([], env)
	g.runtime_local_types['result'] = 'Big*'
	g.runtime_local_types['ten'] = 'Big'
	g.runtime_local_types['one'] = 'Big'
	g.cur_fn_mut_params['result'] = true
	g.fn_return_types['Big__mul'] = 'Big'
	g.fn_return_types['Big__plus'] = 'Big'
	node := ast.InfixExpr{
		op:  .plus
		lhs: ast.Expr(ast.CallExpr{
			lhs:  ast.Expr(ast.Ident{
				name: 'Big__mul'
			})
			args: [ast.Expr(ast.Ident{
				name: 'result'
			}),
				ast.Expr(ast.Ident{
					name: 'ten'
				})]
			pos:  token.Pos{
				id: 25
			}
		})
		rhs: ast.Expr(ast.Ident{
			name: 'one'
		})
	}
	g.gen_infix_expr(&node)
	out := g.sb.str()
	assert out == 'Big__plus(Big__mul(result, ten), one)'
	assert !out.contains('*(Big__mul')
}

fn test_preamble_overrides_cpu_relax_for_tinyc_arm() {
	mut g := Gen.new([])
	g.write_preamble()
	out := g.sb.str()
	assert out.contains('#undef cpu_relax')
	assert out.contains('#define cpu_relax() ((void)0)')
}

fn test_struct_equality_resolves_alias_field_base_type() {
	mut g := Gen.new([])
	db_type := types.Struct{
		name:   'pg__DB'
		fields: [
			types.Field{
				name: 'conninfo'
				typ:  types.string_
			},
		]
	}
	app_type := types.Struct{
		name:   'App'
		fields: [
			types.Field{
				name: 'db'
				typ:  types.Alias{
					name:      'GitlyDb'
					base_type: types.Type(db_type)
				}
			},
		]
	}

	assert g.struct_has_ref_fields(app_type)
	out := g.gen_struct_field_eq_expr(app_type, 'left', 'right')
	assert out == 'string__eq(left.db.conninfo, right.db.conninfo)'
	assert !out.contains('left.db == right.db')
}

fn test_fixed_array_alias_index_uses_element_type_before_env() {
	mut env := types.Environment.new()
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('b', types.Type(types.Alias{
		name:      'fixed_u8_2'
		base_type: types.Type(types.ArrayFixed{
			len:       2
			elem_type: types.Type(types.Primitive{
				props: .integer | .unsigned
				size:  8
			})
		})
	}))
	env.set_expr_type(5, types.Type(types.Alias{
		name: 'fixed_u8_2'
	}))

	mut g := Gen.new_with_env([], env)
	g.cur_fn_scope = fn_scope
	elem_type := g.get_expr_type(ast.Expr(ast.IndexExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'b'
		})
		expr: ast.Expr(ast.BasicLiteral{
			value: '1'
			kind:  .number
		})
		pos:  token.Pos{
			id: 5
		}
	}))
	assert elem_type == 'u8'
}
