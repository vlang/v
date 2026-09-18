module c

import v.flat
import v.types

fn fn_field_call_test_output(pointer_storage bool, pointer_type bool, pointer_field bool) string {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.push_scope()

	value_type := types.Type(types.Struct{ name: 'FnFieldHolder' })
	base_type := if pointer_type {
		types.Type(types.Pointer{ base_type: value_type })
	} else {
		value_type
	}
	callback_type := types.Type(types.FnType{
		params:      [types.Type(types.int_)]
		return_type: types.Type(types.int_)
	})
	field_type := if pointer_field {
		types.Type(types.Pointer{ base_type: callback_type })
	} else {
		callback_type
	}
	tc.structs['FnFieldHolder'] = [types.StructField{
		name: 'callback'
		typ:  field_type
	}]
	owner := tc.cur_scope.insert_with_owner('receiver', base_type)
	g.declare_local_pointer_storage(owner, pointer_storage)
	g.declare_local_c_type(owner, if pointer_storage { 'FnFieldHolder*' } else { 'FnFieldHolder' })

	base_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'receiver'
	})
	tc.register_synth_type(base_id, base_type)
	selector_start := a.children.len
	a.children << base_id
	mut selector := flat.Node{
		kind:           .selector
		value:          'callback'
		children_start: i32(selector_start)
		children_count: 1
	}
	if pointer_storage {
		selector.op = .arrow
	}
	selector_id := a.add_node(selector)
	tc.register_synth_type(selector_id, field_type)
	arg_id := a.add_node(flat.Node{
		kind:  .int_literal
		value: '7'
	})
	tc.register_synth_type(arg_id, types.Type(types.int_))
	call_start := a.children.len
	a.children << [selector_id, arg_id]
	call := flat.Node{
		kind:           .call
		children_start: i32(call_start)
		children_count: 2
	}
	assert g.gen_fn_field_call(call, &a.nodes[int(selector_id)], base_type)
	return g.sb.str()
}

fn test_fn_field_call_uses_pointer_backed_value_storage() {
	out := fn_field_call_test_output(true, false, false)
	assert out.contains('receiver->callback('), out
	assert !out.contains('receiver.callback('), out
}

fn test_fn_field_call_preserves_value_and_explicit_pointer_access() {
	value_out := fn_field_call_test_output(false, false, false)
	assert value_out.contains('receiver.callback('), value_out
	pointer_out := fn_field_call_test_output(true, true, false)
	assert pointer_out.contains('receiver->callback('), pointer_out
}

fn test_fn_field_call_preserves_function_pointer_dereference() {
	out := fn_field_call_test_output(true, false, true)
	assert out.contains('(*receiver->callback)('), out
}
