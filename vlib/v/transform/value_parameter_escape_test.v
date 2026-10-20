module transform

import v.flat
import v.types

fn test_address_into_separate_backing_does_not_escape_its_value_header() {
	for container in ['[]u8', 'string', 'map[int]u8', '[2]u8', '&[2]u8'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		value := t.make_ident('value')
		tc.register_synth_type(value, types.Type(types.Struct{ name_id: flat.source_type_id('Request') }))
		field := t.make_selector(value, 'buffer', flat.source_type_id(container))
		tc.register_synth_type(field, tc.parse_type(flat.source_type_id(container)))
		item := t.make_index(field, t.make_int_literal(0), flat.source_type_id('u8'))
		tc.register_synth_type(item, types.Type(types.u8_))
		address := t.make_prefix(.amp, item)
		assert t.escape_aggregate_address_sources(address, map[string][]string{}, map[string]string{}) == if container == '[2]u8' {
			['value']
		} else {
			[]string{}
		}, container
		// Separate storage can still hold a recorded reference into this frame.
		assert t.escape_aggregate_address_sources(address, {
			'value': ['local']
		}, map[string]string{}) == if container == '[2]u8' {
			['local', 'value']
		} else {
			['local']
		}
	}
}

fn test_interface_dispatch_keeps_object_aliases_without_retaining_the_header() {
	for is_interface in [false, true] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		owner := t.make_ident('owner')
		tc.register_synth_type(owner, types.Type(types.Struct{ name_id: flat.source_type_id('UseCase') }))
		receiver_type := if is_interface {
			types.Type(types.Interface{ name_id: flat.source_type_id('Repo') })
		} else {
			types.Type(types.Struct{ name_id: flat.source_type_id('Repo') })
		}
		tc.fn_param_types['Repo.count'] = [types.Type(types.Pointer{ base_type: receiver_type })]
		result_type := types.Type(types.ResultType{ base_type: types.Type(types.int_) })
		tc.fn_ret_types['Repo.count'] = result_type
		receiver := t.make_selector(owner, 'repo', flat.source_type_id('Repo'))
		tc.register_synth_type(receiver, receiver_type)
		method := t.make_selector(receiver, 'count', 0)
		start := a.children.len
		a.children << method
		call := a.add_node(flat.Node{ kind: .call, children_start: start, children_count: 1 })
		tc.register_synth_type(call, result_type)
		t.set_resolved_call_entry(int(call), 'Repo.count')
		assert t.escape_aggregate_address_sources(call, map[string][]string{}, map[string]string{}) == if is_interface {
			[]string{}
		} else {
			['owner']
		}
		assert t.escape_aggregate_address_sources(call, {
			'owner': ['local']
		}, map[string]string{}) == ['local']
	}
}

fn test_direct_dereference_and_pointer_cast_keep_the_inline_address_owner() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	values := t.make_ident('values')
	fixed_type := tc.parse_type(flat.source_type_id('[2]int'))
	tc.register_synth_type(values, fixed_type)
	address := t.make_prefix(.amp, values)
	tc.register_synth_type(address, tc.parse_type(flat.source_type_id('&[2]int')))
	dereferenced := t.make_prefix(.mul, address)
	tc.register_synth_type(dereferenced, fixed_type)
	item := t.make_index(values, t.make_int_literal(0), flat.source_type_int)
	tc.register_synth_type(item, types.Type(types.int_))
	cast := t.make_cast(flat.source_type_id('&u64'), t.make_prefix(.amp, item), flat.source_type_id('&u64'))
	for expression in [dereferenced, cast] {
		assert t.escape_address_sources(expression, map[string][]string{}, map[string]string{}) == ['values']
	}
}

fn test_scalar_cast_and_result_unwrap_consume_retained_addresses() {
	for wrapper in ['cast', 'or'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		value := t.make_ident('value')
		tc.register_synth_type(value, types.Type(types.int_))
		address := t.make_prefix(.amp, value)
		tc.register_synth_type(address, types.Type(types.Pointer{ base_type: types.Type(types.int_) }))
		call := t.make_call_typed('read', [address], flat.source_type_id('!bool'))
		tc.register_synth_type(call, types.Type(types.ResultType{ base_type: types.Type(types.bool_) }))
		assert t.escape_aggregate_address_sources(call, map[string][]string{}, map[string]string{}) == ['value']
		mut unwrapped := t.make_cast(flat.source_type_id('bool'), call, flat.source_type_id('bool'))
		if wrapper == 'or' {
			fallback := t.make_block([]flat.NodeId{})
			start := a.children.len
			a.children << [call, fallback]
			unwrapped = a.add_node(flat.Node{ kind: .or_expr, children_start: start, children_count: 2 })
		}
		tc.register_synth_type(unwrapped, types.Type(types.bool_))
		assert t.escape_aggregate_address_sources(unwrapped, map[string][]string{}, map[string]string{}) == []string{}
		mut returned := map[string]bool{}
		t.collect_return_escape_idents(unwrapped, mut returned)
		assert returned.len == 0
	}
}

fn test_scalar_success_keeps_addresses_that_can_escape_in_a_result_error() {
	for mode in ['propagate', 'return_error', 'retain_error', 'panic_error', 'shadowed_panic'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		if mode == 'shadowed_panic' {
			t.set_var_type('panic', flat.source_type_id('fn (IError)'))
		}
		value := t.make_ident('value')
		tc.register_synth_type(value, types.Type(types.int_))
		address := t.make_prefix(.amp, value)
		tc.register_synth_type(address, types.Type(types.Pointer{ base_type: types.Type(types.int_) }))
		call := t.make_call_typed('read', [address], flat.source_type_id('!bool'))
		tc.register_synth_type(call, types.Type(types.ResultType{ base_type: types.Type(types.bool_) }))
		mut children := [call]
		if mode != 'propagate' {
			error_value := t.make_ident('err')
			statement := if mode == 'return_error' {
				t.make_return(error_value, flat.source_type_id('!bool'))
			} else {
				t.make_expr_stmt(t.make_call_typed(if mode in ['panic_error', 'shadowed_panic'] {
					'panic'
				} else {
					'retain'
				},
					[error_value], flat.source_type_id('void')))
			}
			children << t.make_block([statement])
		}
		start := a.children.len
		a.children << children
		unwrapped := a.add_node(flat.Node{
			kind:           if mode == 'propagate' {
				flat.NodeKind.postfix
			} else {
				flat.NodeKind.or_expr
			}
			op:             .not
			children_start: start
			children_count: children.len
		})
		tc.register_synth_type(unwrapped, types.Type(types.bool_))
		cast := t.make_cast(flat.source_type_id('bool'), unwrapped, flat.source_type_id('bool'))
		tc.register_synth_type(cast, types.Type(types.bool_))
		assert t.escape_aggregate_address_sources(cast, map[string][]string{}, map[string]string{}) == if mode == 'panic_error' {
			[]string{}
		} else {
			['value']
		}, mode
	}
}

fn test_pointer_to_integer_casts_keep_addresses_through_aliases_and_nested_casts() {
	for target in ['usize', 'isize', 'u64'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		value := t.make_ident('value')
		tc.register_synth_type(value, types.Type(types.int_))
		address := t.make_prefix(.amp, value)
		tc.register_synth_type(address, tc.parse_type(flat.source_type_id('&int')))
		encoded := t.make_cast(flat.source_type_id(target), address, flat.source_type_id(target))
		tc.register_synth_type(encoded, tc.parse_type(flat.source_type_id(target)))
		nested := t.make_cast(flat.source_type_id('u64'), encoded, flat.source_type_id('u64'))
		tc.register_synth_type(nested, tc.parse_type(flat.source_type_id('u64')))
		alias := t.make_ident('alias')
		tc.register_synth_type(alias, tc.parse_type(flat.source_type_id(target)))
		converted_alias := t.make_cast(flat.source_type_id('u64'), alias, flat.source_type_id('u64'))
		tc.register_synth_type(converted_alias, tc.parse_type(flat.source_type_id('u64')))
		for expression in [encoded, nested, converted_alias] {
			assert t.escape_aggregate_address_sources(expression, {
				'alias': ['value']
			}, map[string]string{}) == ['value'], target
			mut returned := map[string]bool{}
			t.collect_return_escape_idents(expression, mut returned)
			assert returned.len > 0, target
		}
	}
}

fn test_scalar_or_fallback_preserves_its_own_encoded_address() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	value := t.make_ident('value')
	tc.register_synth_type(value, types.Type(types.int_))
	address := t.make_prefix(.amp, value)
	tc.register_synth_type(address, tc.parse_type(flat.source_type_id('&int')))
	encoded := t.make_cast(flat.source_type_id('i16'), address, flat.source_type_id('i16'))
	tc.register_synth_type(encoded, tc.parse_type(flat.source_type_id('i16')))
	call := t.make_call_typed('read', []flat.NodeId{}, flat.source_type_id('!i16'))
	tc.register_synth_type(call, tc.parse_type(flat.source_type_id('!i16')))
	fallback := t.make_block([t.make_expr_stmt(encoded)])
	start := a.children.len
	a.children << [call, fallback]
	unwrapped := a.add_node(flat.Node{ kind: .or_expr, children_start: start, children_count: 2 })
	tc.register_synth_type(unwrapped, tc.parse_type(flat.source_type_id('i16')))
	assert t.escape_aggregate_address_sources(unwrapped, map[string][]string{}, map[string]string{}) == ['value']
	mut returned := map[string]bool{}
	t.collect_return_escape_idents(unwrapped, mut returned)
	assert returned['value']
}

fn test_sum_type_scalar_extraction_preserves_encoded_address_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	tagged := t.make_ident('tagged')
	start := a.children.len
	a.children << tagged
	unwrapped := a.add_node(flat.Node{ kind: .as_expr, children_start: start, children_count: 1, value: 'usize' })
	tc.register_synth_type(unwrapped, tc.parse_type(flat.source_type_id('usize')))
	assert t.escape_aggregate_address_sources(unwrapped, {
		'tagged': ['value']
	}, map[string]string{}) == ['value']
	mut returned := map[string]bool{}
	t.collect_return_escape_idents(unwrapped, mut returned)
	assert returned['tagged']
}

fn test_scalar_optional_alias_preserves_its_encoded_address() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	alias := t.make_ident('alias')
	tc.register_synth_type(alias, tc.parse_type(flat.source_type_id('?usize')))
	fallback := t.make_block([t.make_expr_stmt(t.make_int_literal(0))])
	start := a.children.len
	a.children << [alias, fallback]
	unwrapped := a.add_node(flat.Node{ kind: .or_expr, children_start: start, children_count: 2 })
	tc.register_synth_type(unwrapped, tc.parse_type(flat.source_type_id('usize')))
	assert t.escape_aggregate_address_sources(unwrapped, {
		'alias': ['value']
	}, map[string]string{}) == ['value']
	mut returned := map[string]bool{}
	t.collect_return_escape_idents(unwrapped, mut returned)
	assert returned['alias']
}

fn test_address_sized_result_success_preserves_argument_addresses() {
	for target in ['usize', 'isize', 'u64', 'i64', 'int', 'f64', 'f32'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		value := t.make_ident('value')
		tc.register_synth_type(value, types.Type(types.int_))
		address := t.make_prefix(.amp, value)
		tc.register_synth_type(address, tc.parse_type(flat.source_type_id('&int')))
		call := t.make_call_typed('read', [address], flat.source_type_id('!${target}'))
		tc.register_synth_type(call, tc.parse_type(flat.source_type_id('!${target}')))
		fallback := t.make_block([t.make_expr_stmt(t.make_int_literal(0))])
		start := a.children.len
		a.children << [call, fallback]
		unwrapped := a.add_node(flat.Node{ kind: .or_expr, children_start: start, children_count: 2 })
		tc.register_synth_type(unwrapped, tc.parse_type(flat.source_type_id(target)))
		assert t.escape_aggregate_address_sources(unwrapped, map[string][]string{}, map[string]string{}) == ['value'], target
		mut returned := map[string]bool{}
		t.collect_return_escape_idents(unwrapped, mut returned)
		assert returned['value'], target
	}
}

fn test_scalar_result_address_capacity_uses_the_compilation_target_width() {
	previous_bits := types.platform_int_bits()
	previous_spelling := types.platform_int_c_spelling()
	defer {
		types.set_platform_int_bits(previous_bits)
		if previous_spelling == types.portable_int_c_type {
			types.set_portable_platform_int()
		}
	}
	for bits in [32, 64] {
		types.set_platform_int_bits(bits)
		assert !escape_scalar_result_can_encode_address(types.Type(types.bool_))
		assert !escape_scalar_result_can_encode_address(types.Type(types.i16_))
		assert escape_scalar_result_can_encode_address(types.Type(types.f64_))
		assert escape_scalar_result_can_encode_address(types.Type(types.f32_))
		assert escape_scalar_result_can_encode_address(types.Type(types.i32_)) == (bits == 32)
		assert escape_scalar_result_can_encode_address(types.Type(types.int_))
		assert escape_scalar_result_can_encode_address(types.Type(types.u64_))
		assert escape_scalar_result_can_encode_address(types.Type(types.usize_))
		assert escape_scalar_result_can_encode_address(types.Type(types.Alias{
			name_id:   flat.source_type_id('EncodedWord')
			base_type: types.Type(types.u32_)
		})) == (bits == 32)
		assert escape_scalar_result_can_encode_address(types.Type(types.MultiReturn{
			types: [types.Type(types.i16_), types.Type(types.i32_)]
		}))
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		value := t.make_ident('value')
		tc.register_synth_type(value, types.Type(types.int_))
		address := t.make_prefix(.amp, value)
		tc.register_synth_type(address, tc.parse_type(flat.source_type_id('&int')))
		call := t.make_call_typed('read', [address], flat.source_type_id('!i32'))
		tc.register_synth_type(call, tc.parse_type(flat.source_type_id('!i32')))
		fallback := t.make_block([t.make_expr_stmt(t.make_int_literal(0))])
		start := a.children.len
		a.children << [call, fallback]
		unwrapped := a.add_node(flat.Node{ kind: .or_expr, children_start: start, children_count: 2 })
		tc.register_synth_type(unwrapped, types.Type(types.i32_))
		assert t.escape_aggregate_address_sources(unwrapped, map[string][]string{}, map[string]string{}) == if bits == 32 {
			['value']
		} else {
			[]string{}
		}, bits.str()
	}
}

fn test_scalar_multi_return_result_preserves_split_address_arguments() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	value := t.make_ident('value')
	tc.register_synth_type(value, types.Type(types.int_))
	address := t.make_prefix(.amp, value)
	tc.register_synth_type(address, tc.parse_type(flat.source_type_id('&int')))
	tuple := types.Type(types.MultiReturn{ types: [types.Type(types.u32_), types.Type(types.u32_)] })
	call := t.make_call_typed('read', [address], flat.source_type_id('!(u32,u32)'))
	tc.register_synth_type(call, types.Type(types.ResultType{ base_type: tuple }))
	fallback := t.make_block([]flat.NodeId{})
	start := a.children.len
	a.children << [call, fallback]
	unwrapped := a.add_node(flat.Node{ kind: .or_expr, children_start: start, children_count: 2 })
	tc.register_synth_type(unwrapped, tuple)
	assert t.escape_aggregate_address_sources(unwrapped, map[string][]string{}, map[string]string{}) == ['value']
	mut returned := map[string]bool{}
	t.collect_return_escape_idents(unwrapped, mut returned)
	assert returned['value']
}
