module transform

import v.flat
import v.types

fn test_comptime_canonical_type_is_not_rebound_by_another_module_import() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['config.Cfg'] = []types.StructField{}
	tc.imports['config'] = 'rand.config'
	tc.imports['cfg'] = 'config'
	t := Transformer{
		a:          &a
		tc:         &tc
		cur_file:   'generic.v'
		cur_module: 'json2'
	}
	assert t.comptime_resolve_selective_import_type('config.Cfg') == 'config.Cfg'
	assert t.comptime_resolve_selective_import_type('cfg.Cfg') == 'config.Cfg'
}

fn test_comptime_explicit_file_alias_precedes_a_same_spelled_canonical_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['config.Cfg'] = []types.StructField{}
	tc.structs['alternate.Cfg'] = []types.StructField{}
	tc.file_imports[file_import_key('alias.v', 'config')] = 'alternate'
	t := Transformer{
		a:          &a
		tc:         &tc
		cur_file:   'alias.v'
		cur_module: 'main'
	}
	assert t.comptime_resolve_selective_import_type('config.Cfg') == 'alternate.Cfg'
}

fn test_comptime_field_function_type_keeps_declaring_module() {
	mut a := flat.FlatAst.new()
	t := Transformer{
		a: &a
	}
	qualified := t.comptime_field_type_id_key('?fn (mut SSLListener, string) !&SSLCerts', 'mbedtls')
	assert qualified == '?fn(mut mbedtls.SSLListener, string) !&mbedtls.SSLCerts'
	assert t.comptime_field_type_id_key('Registry[string]', 'eventbus') == 'eventbus.Registry[string]'
	assert t.comptime_field_type_id_key('Container[T]', 'eventbus') == 'eventbus.Container[T]'
	assert t.comptime_field_type_id_key('!(Item, []u8)', 'main') == '!(Item, []u8)'
}

fn test_comptime_field_type_id_keeps_custom_types_above_builtin_range() {
	mut a := flat.FlatAst.new()
	t := Transformer{
		a: &a
	}
	assert comptime_type_id_hash('T207') & ~(0xff << 16) < 65536
	type_id := t.comptime_field_type_id('T207', '')
	assert type_id > 65535
	assert type_id != comptime_builtin_type_idx('isize')
	assert type_id & (0xff << 16) == 0
}

fn test_comptime_field_type_id_keeps_specialized_main_type_provenance() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{
		a: &a
	}
	t.active_specialization_main_types['MyParams'] = true
	assert t.comptime_field_type_id_key('MyParams', 'reflection') == 'main.MyParams'
	assert t.comptime_field_type_id('MyParams', 'reflection') == comptime_type_id_hash('main.MyParams') & ~(0xff << 16)
}

fn test_comptime_for_base_type_unwraps_storage_indirections() {
	mut a := flat.FlatAst.new()
	t := Transformer{
		a: &a
	}
	assert t.comptime_for_base_type('&websocket.Server') == 'websocket.Server'
	assert t.comptime_for_base_type('shared websocket.ClientState') == 'websocket.ClientState'
}

fn test_comptime_method_receiver_name_normalizes_main_qualification() {
	assert comptime_method_receiver_name('main.App', 'veb') == 'App'
	assert comptime_method_receiver_matches('App', 'main.App', 'main.App', 'main', 'veb')
}

fn substitute_reflected_method_receiver(mut t Transformer, receiver flat.NodeId, method MethodMeta) flat.NodeId {
	name := t.make_ident('method')
	start := t.a.children.len
	t.a.children << [receiver, name]
	selector := t.a.add_node(flat.Node{
		kind:           .selector
		value:          '\$'
		children_start: start
		children_count: 2
	})
	return t.clone_method_subst_scoped(selector, 'method', method, []string{}) or {
		panic('missing reflected method selector')
	}
}

fn test_comptime_method_selector_keeps_unregistered_local_receivers() {
	for binding in ['unregistered', 'checked', 'registered', 'builtin_shadow', 'checked_builtin_shadow',
		'checked_generated_shadow'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.structs['Dummy'] = []types.StructField{}
		tc.structs['generated'] = []types.StructField{}
		tc.fn_param_types['Dummy.sample'] = [
			types.Type(types.Struct{ name: 'Dummy' }),
			types.Type(types.String{}),
		]
		tc.fn_ret_types['Dummy.sample'] = types.Type(types.int_)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.cur_module = 'main'
		name := match binding {
			'builtin_shadow' { 'string' }
			'checked_builtin_shadow' { 'uint' }
			'checked_generated_shadow' { 'generated' }
			else { 'd' }
		}
		receiver := a.add_val(.ident, name)
		if binding in ['checked', 'checked_builtin_shadow', 'checked_generated_shadow'] {
			tc.register_synth_type(receiver, types.Type(types.Struct{ name: 'Dummy' }))
		} else if binding in ['registered', 'builtin_shadow'] {
			t.set_var_type(name, 'Dummy')
		}
		selector := substitute_reflected_method_receiver(mut t, receiver, MethodMeta{
			name:        'sample'
			receiver:    'Dummy'
			module_name: 'main'
			return_type: 'int'
			params:      [ParamMeta{ name: 'value', typ: 'string' }]
		})
		node := a.node(selector)
		assert node.kind == .selector, binding
		assert a.child_node(node, 0).value == name
		assert comptime_method_selector_marker in node.generic_params()
	}
}

fn test_comptime_method_selector_preserves_type_namespace_function_values() {
	for name in ['Dummy', 'Alias', 'T', 'string', 'generated', 'dep.Dummy', 'receiver'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.structs['Dummy'] = []types.StructField{}
		tc.structs['generated'] = []types.StructField{}
		tc.structs['dep.Dummy'] = []types.StructField{}
		tc.structs['dep.receiver'] = []types.StructField{}
		tc.type_aliases['Alias'] = 'Dummy'
		tc.generated_files['generated.v'] = true
		tc.file_selective_imports[file_import_key('generated.v', 'receiver')] = ['dep.receiver']
		receiver_type := match name {
			'Alias' { 'Dummy' }
			'receiver' { 'dep.receiver' }
			else { name }
		}
		method_key := '${receiver_type}.sample'
		param_type := if name == 'string' {
			types.Type(types.String{})
		} else {
			types.Type(types.Struct{ name: receiver_type })
		}
		tc.fn_param_types[method_key] = [param_type, types.Type(types.String{})]
		tc.fn_ret_types[method_key] = types.Type(types.int_)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.cur_module = 'main'
		t.cur_file = 'generated.v'
		receiver := a.add_val(.ident, name)
		if name != 'T' {
			tc.register_synth_type(receiver, param_type)
		}
		value := substitute_reflected_method_receiver(mut t, receiver, MethodMeta{
			name:        'sample'
			receiver:    receiver_type
			module_name: 'main'
			return_type: 'int'
			params:      [ParamMeta{ name: 'value', typ: 'string' }]
		})
		node := a.node(value)
		assert node.kind == .ident, name
		assert node.value == method_key
		fn_type := tc.expr_type(value) or { panic('missing reflected function type') }
		assert fn_type is types.FnType
		if fn_type is types.FnType {
			assert fn_type.params.len == 2
			assert fn_type.params[0] == param_type
		}
	}
}

fn test_comptime_generated_type_namespace_respects_unlowered_same_type_local() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['receiver'] = []types.StructField{}
	tc.fn_param_types['receiver.sample'] = [types.Type(types.Struct{ name: 'receiver' })]
	tc.fn_ret_types['receiver.sample'] = types.Type(types.int_)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	value := a.add_node(flat.Node{ kind: .struct_init, typ: 'receiver' })
	decl := t.make_decl_assign('receiver', value)
	receiver := a.add_val(.ident, 'receiver')
	tc.register_synth_type(receiver, types.Type(types.Struct{ name: 'receiver' }))
	body := t.make_block([decl, receiver])
	start := a.children.len
	a.children << body
	a.add_node(flat.Node{ kind: .fn_decl, value: 'run', children_start: start, children_count: 1 })
	t.build_source_parent_index()
	selector := substitute_reflected_method_receiver(mut t, receiver, MethodMeta{
		name:        'sample'
		receiver:    'receiver'
		module_name: 'main'
		return_type: 'int'
	})
	node := a.node(selector)
	assert node.kind == .selector
	assert a.child_node(node, 0).value == 'receiver'
}

fn test_comptime_method_call_arity_allows_omitted_optional_args_and_ctx() {
	mut a := flat.FlatAst.new()
	callee := a.add_node(flat.Node{
		kind:  .ident
		value: 'call'
	})
	ctx := a.add_node(flat.Node{
		kind:   .ident
		value:  'ctx'
		typ:    '&Context'
		is_mut: true
	})
	route_arg := a.add_node(flat.Node{
		kind:  .string_literal
		value: 'item'
		typ:   'string'
	})
	children_start := a.children.len
	a.children << callee
	a.children << ctx
	call := flat.Node{
		kind:           .call
		children_start: i32(children_start)
		children_count: 2
	}
	omitted_ctx_children_start := a.children.len
	a.children << callee
	call_without_ctx := flat.Node{
		kind:           .call
		children_start: i32(omitted_ctx_children_start)
		children_count: 1
	}
	route_arg_children_start := a.children.len
	a.children << callee
	a.children << route_arg
	call_with_route_arg := flat.Node{
		kind:           .call
		children_start: i32(route_arg_children_start)
		children_count: 2
	}
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Context'] = []types.StructField{}
	tc.interface_names['IError'] = true
	tc.interface_abstract_methods['IError'] = ['msg']
	tc.fn_implicit_veb_ctx['App.show'] = true
	tc.fn_param_types['App.show'] = [types.Type(types.Struct{ name: 'App' }),
		tc.parse_type('mut Context')]
	tc.params_structs['RouteParams'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	base_method := MethodMeta{
		name:        'show'
		receiver:    'App'
		module_name: 'main'
	}
	assert t.comptime_method_call_arity_matches(call_without_ctx, base_method)
	assert t.comptime_method_call_arity_matches(call, base_method)
	assert t.comptime_method_call_matches(call, base_method)
	optional_method := MethodMeta{
		...base_method
		params: [ParamMeta{ typ: '?string' }]
	}
	assert t.comptime_method_call_arity_matches(call, optional_method)
	assert t.comptime_method_call_matches(call, optional_method)
	params_method := MethodMeta{
		...base_method
		params: [ParamMeta{ typ: 'RouteParams' }]
	}
	assert t.comptime_method_call_arity_matches(call, params_method)
	assert t.comptime_method_call_matches(call, params_method)
	variadic_method := MethodMeta{
		...base_method
		params: [ParamMeta{ typ: '...bool' }]
	}
	assert t.comptime_method_call_arity_matches(call, variadic_method)
	assert t.comptime_method_call_matches(call, variadic_method)
	assert !t.comptime_method_call_arity_matches(call_without_ctx, MethodMeta{
		...base_method
		params: [ParamMeta{ typ: 'string' }]
	})
	string_method := MethodMeta{
		...base_method
		params: [ParamMeta{ typ: 'string' }]
	}
	assert t.comptime_method_call_arity_matches(call, string_method)
	assert !t.comptime_method_call_matches(call, string_method)
	route_method := MethodMeta{
		...base_method
		params: [ParamMeta{ typ: 'string' }]
	}
	assert t.comptime_method_call_arity_matches(call_with_route_arg, route_method)
	assert t.comptime_method_call_matches(call_with_route_arg, route_method)
	assert !t.comptime_method_call_arity_matches(call_without_ctx, MethodMeta{
		...base_method
		params: [ParamMeta{ typ: '!fn ()' }]
	})
	result_callback_method := MethodMeta{
		...base_method
		params: [ParamMeta{ typ: '!fn ()' }]
	}
	assert t.comptime_method_call_arity_matches(call, result_callback_method)
	assert !t.comptime_method_call_matches(call, result_callback_method)
}

fn test_comptime_method_call_arity_distinguishes_mut_route_arg_from_ctx() {
	mut a := flat.FlatAst.new()
	callee := a.add_node(flat.Node{
		kind:  .ident
		value: 'call'
	})
	item := a.add_node(flat.Node{
		kind:   .ident
		value:  'item'
		typ:    'main.Item'
		is_mut: true
	})
	children_start := a.children.len
	a.children << callee
	a.children << item
	call := flat.Node{
		kind:           .call
		children_start: i32(children_start)
		children_count: 2
	}
	mut tc := types.TypeChecker.new(&a)
	tc.fn_implicit_veb_ctx['App.update'] = true
	tc.fn_param_types['App.update'] = [types.Type(types.Struct{ name: 'App' }),
		tc.parse_type('mut Context'), tc.parse_type('mut Item')]
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	method := MethodMeta{
		name:        'update'
		receiver:    'App'
		module_name: 'main'
		params:      [ParamMeta{ typ: '&Item' }]
	}
	assert t.comptime_method_call_arity_matches(call, method)
	assert t.comptime_method_call_matches(call, method)
}

fn test_comptime_method_call_arity_binds_context_value_to_declared_interface() {
	mut a := flat.FlatAst.new()
	callee := a.add_node(flat.Node{
		kind:  .ident
		value: 'call'
	})
	ctx := a.add_node(flat.Node{
		kind:  .ident
		value: 'ctx'
		typ:   '&main.Context'
	})
	children_start := a.children.len
	a.children << callee
	a.children << ctx
	call := flat.Node{
		kind:           .call
		children_start: i32(children_start)
		children_count: 2
	}
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Context'] = []types.StructField{}
	tc.interface_names['RouteArg'] = true
	tc.fn_implicit_veb_ctx['App.show'] = true
	tc.fn_param_types['App.show'] = [types.Type(types.Struct{ name: 'App' }),
		tc.parse_type('mut Context'), tc.parse_type('RouteArg')]
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	method := MethodMeta{
		name:        'show'
		receiver:    'App'
		module_name: 'main'
		params:      [ParamMeta{ typ: 'RouteArg' }]
	}
	assert t.comptime_method_call_arity_matches(call, method)
	assert t.comptime_method_call_matches(call, method)
}

fn test_comptime_method_call_matches_params_struct_fields() {
	mut a := flat.FlatAst.new()
	callee := a.add_node(flat.Node{
		kind:  .ident
		value: 'call'
	})
	ctx := a.add_node(flat.Node{
		kind:   .ident
		value:  'ctx'
		typ:    '&Context'
		is_mut: true
	})
	first_field := a.add_node(flat.Node{
		kind:  .field_init
		value: 'a'
	})
	second_field := a.add_node(flat.Node{
		kind:  .field_init
		value: 'b'
	})
	third_field := a.add_node(flat.Node{
		kind:  .field_init
		value: 'c'
	})
	children_start := a.children.len
	a.children << callee
	a.children << ctx
	a.children << first_field
	a.children << second_field
	call := flat.Node{
		kind:           .call
		children_start: i32(children_start)
		children_count: 4
	}
	required_children_start := a.children.len
	a.children << callee
	a.children << ctx
	a.children << first_field
	a.children << second_field
	a.children << third_field
	call_with_required := flat.Node{
		kind:           .call
		children_start: i32(required_children_start)
		children_count: 5
	}
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Context'] = []types.StructField{}
	tc.structs['RouteParams'] = [
		types.StructField{
			name: 'a'
			typ:  tc.parse_type('int')
		},
		types.StructField{
			name: 'b'
			typ:  tc.parse_type('int')
		},
	]
	tc.structs['OtherParams'] = [types.StructField{
		name: 'a'
		typ:  tc.parse_type('int')
	}]
	tc.structs['RequiredParams'] = [
		types.StructField{
			name: 'a'
			typ:  tc.parse_type('int')
		},
		types.StructField{
			name: 'b'
			typ:  tc.parse_type('int')
		},
		types.StructField{
			name: 'c'
			typ:  tc.parse_type('int')
		},
	]
	tc.fn_implicit_veb_ctx['App.configure'] = true
	tc.fn_param_types['App.configure'] = [types.Type(types.Struct{ name: 'App' }),
		tc.parse_type('mut Context'), tc.parse_type('RouteParams')]
	tc.params_structs['RouteParams'] = true
	tc.params_structs['OtherParams'] = true
	tc.params_structs['RequiredParams'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['RouteParams'] = StructInfo{
		name:      'RouteParams'
		is_params: true
		fields:    [
			FieldInfo{
				name:    'a'
				typ:     'int'
				raw_typ: 'int'
			},
			FieldInfo{
				name:    'b'
				typ:     'int'
				raw_typ: 'int'
			},
		]
	}
	t.structs['OtherParams'] = StructInfo{
		name:      'OtherParams'
		is_params: true
		fields:    [
			FieldInfo{
				name:    'a'
				typ:     'int'
				raw_typ: 'int'
			},
		]
	}
	t.structs['RequiredParams'] = StructInfo{
		name:      'RequiredParams'
		is_params: true
		fields:    [
			FieldInfo{
				name:    'a'
				typ:     'int'
				raw_typ: 'int'
			},
			FieldInfo{
				name:    'b'
				typ:     'int'
				raw_typ: 'int'
			},
			FieldInfo{
				name:    'c'
				typ:     'int'
				raw_typ: 'int'
			},
		]
	}
	t.struct_field_decl_metas_cache['RequiredParams'] = {
		'c': FieldDeclMeta{
			attrs: ['required']
		}
	}
	method := MethodMeta{
		name:        'configure'
		receiver:    'App'
		module_name: 'main'
		params:      [ParamMeta{ typ: 'RouteParams' }]
	}
	assert t.comptime_method_call_arity_matches(call, method)
	assert t.comptime_method_call_matches(call, method)
	assert !t.comptime_method_call_matches(call, MethodMeta{
		...method
		params: [ParamMeta{ typ: 'int' }]
	})
	assert !t.comptime_method_call_matches(call, MethodMeta{
		...method
		params: [ParamMeta{ typ: 'OtherParams' }]
	})
	required_method := MethodMeta{
		...method
		params: [ParamMeta{ typ: 'RequiredParams' }]
	}
	assert !t.comptime_method_call_matches(call, required_method)
	assert t.comptime_method_call_matches(call_with_required, required_method)
}

fn test_comptime_method_attrs_index_cond_keeps_member_access_attached() {
	method := MethodMeta{
		name:  'one'
		attrs: ['GET /a', 'flag']
	}
	// `==` follows the index without a space in the serialized guard
	assert subst_method_attrs_access_cond("method.attrs[0]== 'GET /a'", 'method', method) == "'GET /a' == 'GET /a'"
	// a method call or a closing paren must stay attached to the literal
	assert subst_method_attrs_access_cond("method.attrs[0].starts_with ( 'GET' )", 'method',
		method) == "'GET /a'.starts_with ( 'GET' )"
	assert subst_method_attrs_access_cond('method.attrs[2].len == 0', 'method', method) == "''.len == 0"
	assert subst_method_attrs_access_cond("(method.attrs[1]) == 'flag'", 'method', method) == "('flag') == 'flag'"
}

fn test_comptime_sum_variants_normalize_main_specialization_lock() {
	mut a := flat.FlatAst.new()
	t := Transformer{
		a:         &a
		sum_types: {
			'Sum': ['int', 'string']
		}
	}
	variants := t.comptime_sum_variants('main.Sum')
	assert variants.len == 2
	assert variants[0].typ == 'int'
	assert variants[1].typ == 'string'
}

fn test_comptime_condition_distinguishes_pointer_depth_from_logical_and() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{
		a: &a
	}
	is_array := t.eval_field_cond('&&char is $array_dynamic') or {
		assert false, 'double-pointer type condition should be decidable'
		return
	}
	is_pointer := t.eval_field_cond('&&char is $pointer') or {
		assert false, 'double-pointer type condition should be decidable'
		return
	}
	pointer_and_true := t.eval_field_cond('&&char is $pointer && true') or {
		assert false, 'logical AND after a double-pointer type should be decidable'
		return
	}
	assert !is_array
	assert is_pointer
	assert pointer_and_true
}

fn test_comptime_condition_ignores_brackets_and_operators_in_string_literals() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{
		a: &a
	}
	assert comptime_condition_strip_outer_parens("('a)b' == 'a)b')") == "'a)b' == 'a)b'"
	assert comptime_condition_top_level_index("'a(b' == 'x'", ' == ') == 5
	assert comptime_condition_top_level_index("'x || y' == 'x'", '||') == -1
	conds := {
		"('a)b' == 'a)b')":                            true
		"'a(b' == 'a(b'":                              true
		"'x || y' == 'x'":                             false
		"'a,b' in ['a,b', 'c']":                       true
		"'a' in ['a,b', 'c']":                         false
		"'c' !in ['a,b', 'c']":                        false
		"'get_user' !in ['x]', 'get_user']":           false
		"'get_user' in ['x]', 'get_user']":            true
		"'list_users' !in ['x]', 'get_user']":         true
		"'get_user' in ['a)b', 'get_user']":           true
		"'a,b' in ['a,b'.to_upper().to_lower(), 'c']": true
		"'anything' in []":                            false
		"('a]' == 'a]') && true":                      true
	}
	for cond, want in conds {
		got := t.eval_field_cond(cond) or {
			assert false, 'condition `${cond}` should be decidable'
			return
		}
		assert got == want, cond
	}
}

fn test_comptime_condition_string_literal_members() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{
		a: &a
	}
	members := {
		"'name'.len":                    '4'
		"'name'.starts_with ( 'na' )":   'true'
		"('name'.ends_with ( 'x' ))":    'false'
		"'a)b'.contains ( ')' )":        'true'
		'"it\'s".contains ( "\'" )':     'true'
		"'name'.contains ( 'a' + 'b' )": ''
		"'name'.to_upper ( )":           "'NAME'"
		"'name'.to_upper ( ).len":       '4'
		"'name'.starts_with ( prefix )": ''
		"name.starts_with ( 'na' )":     ''
	}
	for expr, want in members {
		got := comptime_cond_string_member(expr) or { '' }
		assert got == want, expr
	}
	conds := {
		"!'name'.starts_with ( 'x' ) && 'name'.len == 4": true
		"'name'.len > 4 || 'name'.ends_with ( 'me' )":    true
		"'name'.len in [3, 5]":                           false
	}
	for cond, want in conds {
		got := t.eval_field_cond(cond) or {
			assert false, 'condition `${cond}` should be decidable'
			return
		}
		assert got == want, cond
	}
	assert t.eval_field_cond("'name'.to_upper ( ).len > 3") or { false }
}

fn test_comptime_condition_does_not_compare_expressions_as_text() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{
		a: &a
	}
	for cond in ["'name'.unsupported ( ) == 'NAME'", "'name'[0] != `n`", "'name'.split ( 'a' ) in ['n']",
		"'name'.index ( 'a' ) < 2"] {
		if value := t.eval_field_cond(cond) {
			assert false, 'condition `${cond}` should stay undecided, got ${value}'
		}
	}
	conds := {
		"('name') == 'name'":            true
		'2 != 3':                        true
		'.plain == .string':             false
		"'x' == x":                      true
		"`n` == `n` && 'name'.len == 4": true
	}
	for cond, want in conds {
		got := t.eval_field_cond(cond) or {
			assert false, 'condition `${cond}` should be decidable'
			return
		}
		assert got == want, cond
	}
}

fn test_mangled_generic_struct_field_metadata_resolves_declaration() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'sample')
	mut field := flat.Node{
		kind:  .field_decl
		value: 'value'
		typ:   'T'
	}
	field.set_generic_params(['mp', 'skip'])
	field_id := a.add_node(field)
	children_start := a.children.len
	a.children << field_id
	mut generic_struct := flat.Node{
		kind:           .struct_decl
		value:          'Box'
		children_start: i32(children_start)
		children_count: 1
	}
	generic_struct.set_generic_params(['T'])
	a.add_node(generic_struct)

	mut t := Transformer{
		a: &a
	}
	t.build_struct_field_decl_metas_cache()
	for name in ['Box_int', 'sample.Box_int', 'sample__Box_int'] {
		metas := t.struct_field_decl_metas(name)
		meta := metas['value'] or {
			assert false, 'missing field metadata for `${name}`'
			continue
		}
		assert meta.is_mut
		assert meta.is_pub
		assert meta.attrs == ['skip']
	}
}

fn test_comptime_field_metadata_cache_uses_resolved_module() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{
		a:                             &a
		comptime_field_metas_cache:    map[string][]FieldMeta{}
		struct_field_decl_metas_cache: {
			'first.Config':  {
				'first': FieldDeclMeta{
					is_pub: true
					attrs:  ['first_attr']
				}
			}
			'second.Config': {
				'second': FieldDeclMeta{
					is_mut: true
					attrs:  ['second_attr']
				}
			}
		}
		structs:                       {
			'first.Config':  StructInfo{
				name:   'Config'
				module: 'first'
				fields: [
					FieldInfo{
						name:    'first'
						typ:     'int'
						raw_typ: 'int'
					},
				]
			}
			'second.Config': StructInfo{
				name:   'Config'
				module: 'second'
				fields: [
					FieldInfo{
						name:    'second'
						typ:     'string'
						raw_typ: 'string'
					},
				]
			}
		}
	}
	t.cur_module = 'first'
	first := t.comptime_field_metas('Config')
	assert first.len == 1
	assert first[0].name == 'first'
	assert first[0].attrs == ['first_attr']
	assert first[0].is_pub

	t.cur_module = 'second'
	second := t.comptime_field_metas('Config')
	assert second.len == 1
	assert second[0].name == 'second'
	assert second[0].attrs == ['second_attr']
	assert second[0].is_mut
}

fn test_comptime_field_metadata_cache_normalizes_main_qualified_name() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{
		a:                             &a
		comptime_field_metas_cache:    map[string][]FieldMeta{}
		struct_field_decl_metas_cache: {
			'Config': {
				'skipped': FieldDeclMeta{
					attrs: ['skip']
				}
			}
		}
		structs:                       {
			'main.Config': StructInfo{
				name:   'main.Config'
				module: 'main'
				fields: [
					FieldInfo{
						name:    'skipped'
						typ:     '&App'
						raw_typ: '&App'
					},
				]
			}
		}
	}
	metas := t.comptime_field_metas('main.Config')
	assert metas.len == 1
	assert metas[0].attrs == ['skip']
}

fn test_comptime_field_metadata_cache_keeps_main_and_builtin_names_distinct() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'builtin')
	mut builtin_field := flat.Node{
		kind:  .field_decl
		value: 'builtin_value'
		typ:   'int'
	}
	builtin_field.set_generic_params(['p', 'builtin_attr'])
	builtin_field_id := a.add_node(builtin_field)
	builtin_children_start := a.children.len
	a.children << builtin_field_id
	a.add_node(flat.Node{
		kind:           .struct_decl
		value:          'FieldData'
		children_start: i32(builtin_children_start)
		children_count: 1
	})

	a.add_val(.module_decl, 'main')
	mut main_field := flat.Node{
		kind:  .field_decl
		value: 'main_value'
		typ:   'string'
	}
	main_field.set_generic_params(['mp', 'main_attr'])
	main_field_id := a.add_node(main_field)
	main_children_start := a.children.len
	a.children << main_field_id
	a.add_node(flat.Node{
		kind:           .struct_decl
		value:          'FieldData'
		children_start: i32(main_children_start)
		children_count: 1
	})

	mut t := Transformer{
		a: &a
	}
	t.build_struct_field_decl_metas_cache()
	main_metas := t.struct_field_decl_metas_in_module('FieldData', 'main')
	main_meta := main_metas['main_value'] or {
		assert false, 'missing main FieldData metadata'
		return
	}
	assert main_meta.is_mut
	assert main_meta.is_pub
	assert main_meta.attrs == ['main_attr']
	assert 'builtin_value' !in main_metas

	builtin_metas := t.struct_field_decl_metas_in_module('FieldData', 'builtin')
	builtin_meta := builtin_metas['builtin_value'] or {
		assert false, 'missing builtin FieldData metadata'
		return
	}
	assert !builtin_meta.is_mut
	assert builtin_meta.is_pub
	assert builtin_meta.attrs == ['builtin_attr']
	assert 'main_value' !in builtin_metas

	bare_metas := t.struct_field_decl_metas('FieldData')
	assert 'main_value' in bare_metas
	assert 'builtin_value' !in bare_metas
}

fn add_comptime_test_method(mut a flat.FlatAst, receiver string, name string, return_type string, param_type string) flat.NodeId {
	receiver_id := a.add_node(flat.Node{
		kind:  .param
		op:    .dot
		value: 'self'
		typ:   receiver
	})
	param_id := if param_type.len > 0 {
		a.add_node(flat.Node{ kind: .param, value: 'value', typ: param_type })
	} else {
		flat.NodeId(-1)
	}
	children_start := a.children.len
	a.children << receiver_id
	if int(param_id) >= 0 {
		a.children << param_id
	}
	return a.add_node(flat.Node{
		kind:           .fn_decl
		value:          '${receiver}.${name}'
		typ:            return_type
		children_start: i32(children_start)
		children_count: if int(param_id) >= 0 { 2 } else { 1 }
	})
}

fn test_comptime_method_metadata_keeps_resolved_module_and_file_context() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'first')
	add_comptime_test_method(mut a, 'App', 'show', 'int', '')
	add_comptime_test_method(mut a, 'Alias', 'extra', 'int', '')
	a.add_val(.module_decl, 'second')
	add_comptime_test_method(mut a, 'App', 'show', 'string', '')
	add_comptime_test_method(mut a, 'Alias', 'extra', 'string', '')
	mut tc := types.TypeChecker.new(&a)
	tc.structs['first.App'] = []types.StructField{}
	tc.structs['second.App'] = []types.StructField{}
	tc.type_aliases['first.Alias'] = 'first.App'
	tc.type_aliases['second.Alias'] = 'second.App'
	tc.file_imports[file_import_key('first.v', 'dep')] = 'first'
	tc.file_imports[file_import_key('second.v', 'dep')] = 'second'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for module_name in ['first', 'second', 'first'] {
		t.cur_module = module_name
		for name in ['App', 'Alias', '${module_name}.App'] {
			methods := t.comptime_method_metas(name)
			assert methods.len == if name == 'Alias' { 2 } else { 1 }
			assert methods[0].module_name == module_name
			assert methods[0].return_type == if module_name == 'first' { 'int' } else { 'string' }
			if name == 'Alias' {
				assert methods[1].name == 'extra'
				assert methods[1].module_name == module_name
			}
		}
	}
	t.cur_module = 'main'
	for module_name in ['first', 'second', 'first'] {
		t.cur_file = '${module_name}.v'
		methods := t.comptime_method_metas('dep.App')
		assert methods.len == 1
		assert methods[0].module_name == module_name
	}
}

fn test_comptime_method_metadata_keeps_generic_receiver_specializations() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'main')
	add_comptime_test_method(mut a, 'Box[T]', 'replace', 'T', 'T')
	mut tc := types.TypeChecker.new(&a)
	tc.struct_generic_params['Box'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	for typ in ['int', 'string', 'int'] {
		for name in ['Box[${typ}]', 'Box_${typ}'] {
			methods := t.comptime_method_metas(name)
			assert methods.len == 1
			assert methods[0].return_type == typ
			assert methods[0].params.len == 1
			assert methods[0].params[0].typ == typ
		}
	}
}

fn test_comptime_method_metadata_keeps_specialization_main_type_provenance() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'main')
	add_comptime_test_method(mut a, 'App', 'show', 'int', '')
	a.add_val(.module_decl, 'reflection')
	add_comptime_test_method(mut a, 'App', 'show', 'string', '')
	mut tc := types.TypeChecker.new(&a)
	tc.structs['App'] = []types.StructField{}
	tc.structs['reflection.App'] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'reflection'
	for main_type in [false, true, false] {
		t.active_specialization_main_types['App'] = main_type
		methods := t.comptime_method_metas('App')
		assert methods.len == 1
		assert methods[0].module_name == if main_type { 'main' } else { 'reflection' }
		assert methods[0].return_type == if main_type { 'int' } else { 'string' }
	}
}

fn test_comptime_method_metadata_keeps_module_sensitive_generic_arguments() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'main')
	add_comptime_test_method(mut a, 'Box[T]', 'replace', 'T', 'T')
	mut tc := types.TypeChecker.new(&a)
	tc.structs['first.A'] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for module_name in ['first', 'second', 'first'] {
		t.cur_module = module_name
		methods := t.comptime_method_metas('main.Box[A]')
		assert methods.len == 1
		assert methods[0].return_type == if module_name == 'first' { 'A' } else { 'T' }
		assert methods[0].params[0].typ == methods[0].return_type
	}
}

fn test_comptime_method_metadata_observes_appended_methods_and_attributes() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'main')
	method_id := add_comptime_test_method(mut a, 'App', 'first', 'int', '')
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	initial := t.comptime_method_metas('App')
	assert initial.len == 1
	assert initial[0].attrs.len == 0
	a.add_node(flat.Node{
		kind:    .directive
		value:   '@attributes:${int(method_id)}'
		typ:     '1,2,3,0'
		payload: flat.node_payload(["route: '/first'", 'priority: 2', 'enabled: true', 'inline'])
	})
	attributed := t.comptime_method_metas('App')
	assert attributed.len == 1
	assert attributed[0].attrs == ["route: '/first'", 'priority: 2', 'enabled: true', 'inline']
	assert attributed[0].attributes == [
		AttributeMeta{ name: 'route', arg: '/first', has_arg: true, kind: 1 },
		AttributeMeta{ name: 'priority', arg: '2', has_arg: true, kind: 2 },
		AttributeMeta{ name: 'enabled', arg: 'true', has_arg: true, kind: 3 },
		AttributeMeta{ name: 'inline', kind: 0 },
	]
	add_comptime_test_method(mut a, 'App', 'second', 'string', '')
	expanded := t.comptime_method_metas('App')
	assert expanded.len == 2
	assert expanded[0].name == 'first'
	assert expanded[0].attrs == attributed[0].attrs
	assert expanded[1].name == 'second'
	assert expanded[1].return_type == 'string'
	assert expanded[1].attrs.len == 0
}

fn test_comptime_method_metadata_observes_erased_generic_methods() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'main')
	template_id := add_comptime_test_method(mut a, 'Box[T]', 'replace', 'T', 'T')
	add_comptime_test_method(mut a, 'Box[int]', 'keep', 'bool', '')
	clone_id := add_comptime_test_method(mut a, 'Box[int]', 'replace', 'int', 'int')
	a.add_node(flat.Node{
		kind:    .directive
		value:   '@attributes:${int(template_id)}'
		payload: flat.node_payload(['generic'])
	})
	a.add_node(flat.Node{
		kind:    .directive
		value:   '@attributes:${int(clone_id)}'
		payload: flat.node_payload(['concrete'])
	})
	mut tc := types.TypeChecker.new(&a)
	tc.struct_generic_params['Box'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	initial := t.comptime_method_metas('Box[int]')
	assert initial.map(it.name) == ['replace', 'keep']
	assert initial[0].receiver == 'Box[T]'
	assert initial[0].attrs == ['generic']
	decl := GenericFnDecl{
		id:     template_id
		node:   a.nodes[int(template_id)]
		file:   'main.v'
		module: 'main'
		key:    'Box.replace'
	}
	node_count := a.nodes.len
	t.erase_generic_fn_decls({
		decl.key: decl
	})
	assert a.nodes.len == node_count
	assert a.node(template_id).kind == .empty
	after := t.comptime_method_metas('Box[int]')
	assert after.map(it.name) == ['keep', 'replace']
	assert after[1].receiver == 'Box[int]'
	assert after[1].return_type == 'int'
	assert after[1].params.len == 1
	assert after[1].params[0].typ == 'int'
	assert after[1].attrs == ['concrete']
}

fn test_comptime_method_scan_does_not_allocate_per_ast_node() {
	$if gcboehm ? {
		mut a := flat.FlatAst.new()
		a.add_val(.module_decl, 'main')
		method_id := add_comptime_test_method(mut a, 'App', 'show', 'int', 'string')
		for _ in 0 .. 8192 {
			a.add_node(flat.Node{ kind: .int_literal, value: '0', typ: 'int' })
		}
		a.add_node(flat.Node{
			kind:    .directive
			value:   '@attributes:${int(method_id)}'
			typ:     '1'
			payload: flat.node_payload(["route: '/show'"])
		})
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.cur_module = 'main'
		before := gc_heap_usage().total_bytes
		for _ in 0 .. 16 {
			assert t.comptime_method_metas('int').len == 0
			methods := t.comptime_method_metas('App')
			assert methods.len == 1
			assert methods[0].attrs == ["route: '/show'"]
			assert methods[0].attributes[0].arg == '/show'
		}
		allocated := gc_heap_usage().total_bytes - before
		// Method and attribute lookups allocate metadata independently of unrelated AST nodes.
		assert allocated < 1024 * 1024, 'method scans allocated ${allocated} bytes'
	}
}

fn test_comptime_method_metadata_reuses_repeated_receiver_lookups() {
	$if gcboehm ? {
		mut a := flat.FlatAst.new()
		a.add_val(.module_decl, 'main')
		for i in 0 .. 128 {
			method_id := add_comptime_test_method(mut a, 'App', 'method_${i}', 'string', 'int')
			a.add_node(flat.Node{
				kind:    .directive
				value:   '@attributes:${int(method_id)}'
				typ:     '1'
				payload: flat.node_payload(["route: '/show'"])
			})
		}
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.cur_module = 'main'
		assert t.comptime_method_metas('App').len == 128
		before := gc_heap_usage().total_bytes
		for _ in 0 .. 64 {
			methods := t.comptime_method_metas('App')
			assert methods.len == 128
			assert methods[127].params[0].typ == 'int'
			assert methods[127].attributes[0].arg == '/show'
		}
		allocated := gc_heap_usage().total_bytes - before
		assert allocated < 1024 * 1024, 'repeated method lookups allocated ${allocated} bytes'
	}
}

fn test_comptime_string_method_chains_match_builtin_methods() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{ a: &a }
	for cond in ["'GET /users/:id'.all_after(' ').trim_left('/').starts_with('users')",
		"'GET /users'.all_before(' ').to_lower() == 'get'", "'a/b/c'.count('/') == 2",
		"'  x  '.trim_space().to_upper() == 'X'",
		"'prefix-value'.trim_string_left('prefix-') == 'value'",
		"'value-suffix'.trim_string_right('-suffix') == 'value'",
		"'a,b,c'.replace(',', '/').all_after_last('/') == 'c'", "'a/b/c'.all_before_last('/') == 'a/b'",
		"'name'.trim_right('e').trim_left('n') == 'am'", "'aa'.replace('a', 'x').count('x') == 2"] {
		assert t.eval_field_cond(cond) or { false }, cond
	}
}

fn test_folded_condition_string_keeps_its_own_quotes() {
	value := "'QUOTED'"
	text := comptime_cond_string_literal(value)
	expr := text + '.to_lower()'
	assert comptime_cond_operand(expr) or { '' } == value.to_lower()
	mut a := flat.FlatAst.new()
	mut t := Transformer{ a: &a }
	assert t.eval_field_cond(expr + ' == ' + comptime_cond_string_literal(value.to_lower())) or { false }
}

fn test_unresolved_type_guard_does_not_hide_concrete_string_conditions() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.active_generic_params = ['T']
	assert t.comptime_condition_has_unresolved_type_test('T is \$struct')
	assert t.comptime_condition_has_unresolved_type_test('T !is int && runtime.contains("x")')
	assert !t.comptime_condition_has_unresolved_type_test("int is int && 'abc'.repeat(1) == 'abc'")
	assert !t.comptime_condition_has_unresolved_type_test("' is '.contains('is')")
}

fn test_scalar_constant_lookup_respects_global_owners_and_import_namespaces() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.string_literal, 'a b')
	mut tc := types.TypeChecker.new(&a)
	for key, owner in {
		'registry.value': 'registry'
		'registry.route': 'registry'
		'route':          'main'
	} {
		tc.const_types[key] = types.string_
		tc.const_exprs[key] = value
		tc.const_modules[key] = owner
	}
	tc.file_imports[file_import_key('main.v', 'registry')] = 'registry'
	tc.file_imports[file_import_key('main.v', 'r')] = 'registry'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.globals['registry'] = 'registry.Record'
	t.globals['registry.registry'] = 'registry.Record'
	t.globals['route'] = 'api.Record'
	t.globals['api.route'] = 'api.Record'
	assert t.comptime_scalar_named_const('registry.value', 0, 'registry', 'registry.v') == none
	assert (t.comptime_scalar_named_const('registry.route', 0, 'main', 'main.v') or { panic('import') }).value == 'a b'
	assert (t.comptime_scalar_named_const('r.route', 0, 'main', 'main.v') or { panic('alias') }).value == 'a b'
	assert (t.comptime_scalar_named_const('route', 0, 'main', 'main.v') or { panic('owner') }).value == 'a b'
	assert (t.comptime_scalar_named_const('route', 0, '', 'main.v') or { panic('implicit main owner') }).value == 'a b'
	assert t.subst_comptime_scalar_locals('registry.value == 7') == 'registry.value == 7'
}
