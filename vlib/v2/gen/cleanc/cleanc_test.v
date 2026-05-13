// vtest build: !linux
module cleanc

import v2.ast
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

fn test_struct_generic_params_need_bindings_returns_false_for_lifetime_only_params() {
	params := [
		ast.Expr(ast.LifetimeExpr{
			name: 'a'
		}),
		ast.Expr(ast.LifetimeExpr{
			name: 'b'
		}),
	]
	assert !struct_generic_params_need_bindings(params)
}

fn test_struct_generic_params_need_bindings_returns_true_for_runtime_generic_params() {
	params := [
		ast.Expr(ast.LifetimeExpr{
			name: 'a'
		}),
		ast.Expr(ast.Ident{
			name: 'T'
		}),
	]
	assert struct_generic_params_need_bindings(params)
}

fn test_runtime_generic_params_filter_lifetime_params() {
	params := [
		ast.Expr(ast.LifetimeExpr{
			name: 'a'
		}),
		ast.Expr(ast.Ident{
			name: 'T'
		}),
	]
	args := [
		ast.Expr(ast.LifetimeExpr{
			name: 'a'
		}),
		ast.Expr(ast.Ident{
			name: 'Value'
		}),
	]
	filtered_args := runtime_generic_args(args)
	assert runtime_generic_param_names(['^a', 'T']) == ['T']
	assert generic_param_names(params) == ['T']
	assert filtered_args.len == 1
	assert filtered_args[0] is ast.Ident
	assert (filtered_args[0] as ast.Ident).name == 'Value'
}

fn test_record_generic_struct_bindings_filters_lifetime_params() {
	mut env := types.Environment.new()
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('Ref', types.Object(types.Type(types.Struct{
		name:           'Ref'
		generic_params: ['^a', 'T']
	})))
	scope.insert('Value', types.Object(types.Type(types.Struct{
		name: 'Value'
	})))
	lock env.scopes {
		env.scopes['main'] = scope
	}
	mut g := Gen.new_with_env([], env)
	g.record_generic_struct_bindings('Ref', 'Ref', [
		ast.Expr(ast.LifetimeExpr{
			name: 'a'
		}),
		ast.Expr(ast.Ident{
			name: 'Value'
		}),
	])
	bindings := (g.generic_struct_bindings['Ref'] or { panic('missing Ref binding') }).clone()
	value_type := bindings['T'] or { panic('missing T binding') }
	assert value_type.name() == 'Value'
	instances := g.generic_struct_instances['Ref']
	assert instances.len == 1
	assert instances[0].params_key == 'Value'
}

fn test_expr_type_to_c_lowers_pointer_type() {
	mut g := Gen.new([])
	pointer_type := ast.Expr(ast.Type(ast.PointerType{
		base_type: ast.Expr(ast.Ident{
			name: 'Foo'
		})
	}))
	assert g.expr_type_to_c(pointer_type) == 'Foo*'
	assert g.is_pointer_type(pointer_type)
	assert g.receiver_type_to_scope_name(pointer_type) == 'Foo'
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
