module cleanc

import v2.ast

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

fn test_selector_field_type_prefers_explicit_smartcast_deref() {
	mut g := Gen.new([])
	g.struct_field_types['ast__GoExpr.call_expr'] = 'ast__CallExpr'
	sel := ast.SelectorExpr{
		lhs: ast.ParenExpr{
			expr: ast.PrefixExpr{
				op:   .mul
				expr: ast.CastExpr{
					typ:  ast.Ident{
						name: 'ast__GoExpr*'
					}
					expr: ast.Ident{
						name: 'x'
					}
				}
			}
		}
		rhs: ast.Ident{
			name: 'call_expr'
		}
	}
	assert g.selector_field_type(sel) == 'ast__CallExpr'
	assert g.get_expr_type(ast.Expr(sel)) == 'ast__CallExpr'
}

fn test_selector_field_type_resolves_array_len_before_stale_scope_type() {
	mut g := Gen.new([])
	g.runtime_local_types['params'] = 'Array_ast__Param'
	sel := ast.SelectorExpr{
		lhs: ast.Ident{
			name: 'params'
		}
		rhs: ast.Ident{
			name: 'len'
		}
	}
	assert g.selector_field_type(sel) == 'int'
	assert g.get_expr_type(ast.Expr(sel)) == 'int'
}

fn test_addr_temp_compound_type_prefers_concrete_rhs_over_stale_option() {
	assert addr_temp_compound_type('_option_int', 'string') == 'string'
	assert addr_temp_compound_type('_result_int', 'Array_string') == 'Array_string'
	assert addr_temp_compound_type('_option_int', '_option_string') == '_option_int'
	assert addr_temp_compound_type('string', 'int') == 'string'
}

fn test_addr_of_sumtype_variant_arg_boxes_concrete_variant() {
	mut g := Gen.new([])
	g.sum_type_variants['ast__Expr'] = ['OrExpr']
	g.runtime_local_types['or_block'] = 'ast__OrExpr'
	assert g.gen_addr_of_sumtype_variant_arg('ast__Expr', ast.Ident{
		name: 'or_block'
	})
	out := g.sb.str()
	assert out.starts_with('&((ast__Expr[]){')
	assert out.contains('._data._OrExpr')
	assert out.contains('memdup')
	assert out.ends_with('})[0]')

	mut g_ptr := Gen.new([])
	g_ptr.sum_type_variants['ast__Expr'] = ['OrExpr']
	g_ptr.runtime_local_types['or_block'] = 'ast__OrExpr*'
	assert g_ptr.gen_addr_of_sumtype_variant_arg('ast__Expr', ast.Ident{
		name: 'or_block'
	})
	out_ptr := g_ptr.sb.str()
	assert out_ptr.contains('*or_block')
}

fn test_new_call_keeps_same_typed_struct_arg() {
	mut g := Gen.new([])
	g.fn_param_types['markused__Walker__new'] = ['markused__Walker']
	g.call_expr(ast.Ident{
		name: 'markused__Walker__new'
	}, [
		ast.Expr(ast.InitExpr{
			typ:    ast.Ident{
				name: 'markused__Walker'
			}
			fields: [
				ast.FieldInit{
					name:  'table'
					value: ast.Ident{
						name: 'table'
					}
				},
				ast.FieldInit{
					name:  'trace_enabled'
					value: ast.BasicLiteral{
						kind: .key_true
					}
				},
			]
		}),
	])
	out := g.sb.str()
	assert out.contains('markused__Walker__new(((markused__Walker){')
	assert out.contains('.table = table')
	assert out.contains('.trace_enabled = true')
	assert !out.contains('markused__Walker__new(((markused__Walker){0})')
}
