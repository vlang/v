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
		call := t.make_call_typed('read', [address], flat.source_type_id('!int'))
		tc.register_synth_type(call, types.Type(types.ResultType{ base_type: types.Type(types.int_) }))
		assert t.escape_aggregate_address_sources(call, map[string][]string{}, map[string]string{}) == ['value']
		mut unwrapped := t.make_cast(flat.source_type_int, call, flat.source_type_int)
		if wrapper == 'or' {
			start := a.children.len
			a.children << [call, t.make_block([]flat.NodeId{})]
			unwrapped = a.add_node(flat.Node{ kind: .or_expr, children_start: start, children_count: 2 })
		}
		tc.register_synth_type(unwrapped, types.Type(types.int_))
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
		call := t.make_call_typed('read', [address], flat.source_type_id('!int'))
		tc.register_synth_type(call, types.Type(types.ResultType{ base_type: types.Type(types.int_) }))
		mut children := [call]
		if mode != 'propagate' {
			error_value := t.make_ident('err')
			statement := if mode == 'return_error' {
				t.make_return(error_value, flat.source_type_id('!int'))
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
		tc.register_synth_type(unwrapped, types.Type(types.int_))
		cast := t.make_cast(flat.source_type_int, unwrapped, flat.source_type_int)
		tc.register_synth_type(cast, types.Type(types.int_))
		assert t.escape_aggregate_address_sources(cast, map[string][]string{}, map[string]string{}) == if mode == 'panic_error' {
			[]string{}
		} else {
			['value']
		}, mode
	}
}
