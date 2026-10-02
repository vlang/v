module c

import v.flat
import v.types

fn owned_result_error_clone_code(clone_kind string, pointer_receiver bool, needs_drop bool, tail_kind string) string {
	mut a := flat.FlatAst.new()
	default_value := a.add_node(flat.Node{ kind: .int_literal, value: '7', typ: 'int' })
	field_start := a.children.len
	a.children << default_value
	field := a.add_node(flat.Node{
		kind:           .field_decl
		value:          'limit'
		typ:            'int'
		children_start: field_start
		children_count: 1
	})
	struct_start := a.children.len
	a.children << field
	params_decl := flat.Node{
		kind:           .struct_decl
		value:          'CloneParams'
		children_start: struct_start
		children_count: 1
	}
	params_id := a.add_node(params_decl)
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Fault'] = [types.StructField{
		name: 'id'
		typ:  types.Type(types.int_)
	}]
	tc.structs['OtherFault'] = tc.structs['Fault'].clone()
	tc.params_structs['CloneParams'] = true
	tc.interface_names['IError'] = true
	fault := tc.parse_type('Fault')
	fault_pointer := types.Type(types.Pointer{ base_type: fault })
	tc.type_aliases['FaultRef'] = '&Fault'
	tc.type_aliases['FaultRefAlias'] = 'FaultRef'
	tc.type_aliases['FaultValueAlias'] = 'Fault'
	if needs_drop {
		tc.fn_param_types['Fault.drop'] = [fault_pointer]
		tc.fn_ret_types['Fault.drop'] = types.Type(types.void_)
	}
	if clone_kind != 'none' {
		tc.fn_param_types['Fault.clone'] = [if pointer_receiver { fault_pointer } else { fault }]
		tc.fn_ret_types['Fault.clone'] = match clone_kind {
			'value', 'extra_argument' { fault }
			'pointer' { fault_pointer }
			'pointer_alias' { tc.parse_type('FaultRef') }
			'pointer_alias_chain' { tc.parse_type('FaultRefAlias') }
			'value_alias' { tc.parse_type('FaultValueAlias') }
			'wrong_pointer' { types.Type(types.Pointer{ base_type: types.Type(types.int_) }) }
			'other_pointer' { types.Type(types.Pointer{ base_type: tc.parse_type('OtherFault') }) }
			else { types.Type(types.int_) }
		}
		if clone_kind == 'extra_argument' {
			tc.fn_param_types['Fault.clone'] << types.Type(types.int_)
		}
		match tail_kind {
			'optional' {
				tc.fn_param_types['Fault.clone'] << types.Type(types.OptionType{ base_type: types.Type(types.int_) })
			}
			'variadic', 'native_variadic' {
				tc.fn_param_types['Fault.clone'] << types.Type(types.Array{
					elem_type: if tail_kind == 'variadic' {
						types.Type(types.bool_)
					} else {
						types.Type(types.void_)
					}
				})
				tc.fn_variadic['Fault.clone'] = true
			}
			'params' {
				tc.fn_param_types['Fault.clone'] << tc.parse_type('CloneParams')
			}
			'params_pointer' {
				tc.fn_param_types['Fault.clone'] << types.Type(types.Pointer{ base_type: tc.parse_type('CloneParams') })
			}
			else {}
		}
	}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.register_struct_decl_info_at(int(params_id), 'CloneParams', 'CloneParams', 'main',
		'main.v', params_decl)
	g.iface_impls['IError'] = ['Fault']
	g.iface_type_ids['IError::Fault'] = 1
	assert g.ownership_type_requires_destruction(fault, 0) == needs_drop
	compatible := clone_kind in ['value', 'value_alias', 'pointer', 'pointer_alias',
		'pointer_alias_chain']
	assert (tc.ownership_type_has_clone_method(fault)
		|| tc.ownership_type_has_clone_method(fault_pointer)) == compatible, '${clone_kind}, pointer receiver: ${pointer_receiver}'
	source := a.add_node(flat.Node{
		kind:  .ident
		value: 'borrowed_error'
		typ:   'IError'
	})
	g.gen_ownership_clone_ierror(source)
	return g.sb.str()
}

fn test_owned_result_errors_use_compatible_value_and_pointer_clones() {
	for clone_kind in ['value', 'value_alias', 'pointer', 'pointer_alias', 'pointer_alias_chain'] {
		for pointer_receiver in [false, true] {
			code := owned_result_error_clone_code(clone_kind, pointer_receiver, true, '')
			assert code.count('Fault__clone(') == 1, code
			assert code.contains('_clone_ierror_result0._object_is_boxed = true;'), code
			assert !code.contains('v_panic('), code
			assert !code.contains('memdup(_clone_ierror_source0._object,'), code
			if pointer_receiver {
				assert code.contains('Fault__clone(((Fault*)_clone_ierror_source0._object))'), code
			} else {
				assert code.contains('Fault__clone(*((Fault*)_clone_ierror_source0._object))'), code
			}
			if clone_kind in ['value', 'value_alias'] {
				assert code.contains('Fault _clone_ierror_value0 = Fault__clone('), code
				assert code.contains('memdup(&_clone_ierror_value0, sizeof(Fault))'), code
			} else {
				assert code.contains('_clone_ierror_result0._object = Fault__clone('), code
				assert !code.contains('memdup('), code
			}
		}
	}
}

fn test_owned_result_errors_reject_missing_or_incompatible_destructible_clones() {
	for clone_kind in ['none', 'incompatible', 'wrong_pointer', 'other_pointer', 'extra_argument'] {
		code := owned_result_error_clone_code(clone_kind, true, true, '')
		assert code.contains('v_panic('), code
		assert code.contains('requires ownership destruction but has no compatible `clone()` method'), code
		assert !code.contains('Fault__clone('), code
		assert !code.contains('memdup('), code
		assert !code.contains('_clone_ierror_result0._object_is_boxed = true;'), code
	}
}

fn test_owned_result_error_clone_keeps_sentinels_and_trivial_payloads_safe() {
	code := owned_result_error_clone_code('none', false, false, '')
	assert code.count('borrowed_error') == 1, code
	assert code.contains('_clone_ierror_result0 = _clone_ierror_source0;'), code
	assert code.contains('if (_clone_ierror_source0._object != NULL'), code
	assert code.contains('_clone_ierror_source0._object != builtin__none__._object'), code
	assert code.contains('_clone_ierror_source0._object != builtin__error_sentinel._object'), code
	assert code.contains('memdup(_clone_ierror_source0._object, sizeof(Fault))'), code
	assert code.contains('_clone_ierror_result0._object_is_boxed = true;'), code
	assert !code.contains('v_panic('), code
	assert !code.contains('Fault__clone('), code
}

fn test_owned_result_error_clone_supplies_supported_omitted_arguments() {
	for clone_kind in ['value', 'pointer_alias_chain'] {
		for tail_kind in ['optional', 'variadic', 'native_variadic', 'params', 'params_pointer'] {
			code := owned_result_error_clone_code(clone_kind, true, true, tail_kind)
			assert code.count('Fault__clone(') == 1, code
			assert code.contains('_clone_ierror_result0._object_is_boxed = true;'), code
			assert !code.contains('v_panic('), code
			match tail_kind {
				'optional' {
					assert code.contains(', (__v_option_i64){.ok = false})'), code
				}
				'variadic' {
					assert code.contains(', new_array_from_c_array(0, 0, sizeof(bool), (bool[]){0}))'), code
				}
				'native_variadic' {
					assert code.contains('Fault__clone(((Fault*)_clone_ierror_source0._object))'), code
				}
				'params' {
					assert code.contains(', (main__CloneParams){.limit = 7})'), code
				}
				'params_pointer' {
					assert code.contains(', &(main__CloneParams[]){(main__CloneParams){.limit = 7}})'), code
				}
				else {}
			}
		}
	}
}
