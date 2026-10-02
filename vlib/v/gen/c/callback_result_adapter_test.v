module c

import v.flat
import v.types

fn test_void_callback_result_adapter_returns_success() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	actual := types.FnType{
		params:      [types.Type(types.int_)]
		return_type: types.Type(types.void_)
	}
	expected := types.FnType{
		params:      actual.params
		return_type: types.Type(types.ResultType{ base_type: types.Type(types.void_) })
	}
	adapter := g.ensure_callback_userdata_wrapper('record', actual, expected, '') or {
		assert false, 'a void callback needs a successful Result adapter'
		return
	}
	assert adapter.len > 0
	assert g.callback_wrapper_defs.len == 1
	body := g.callback_wrapper_defs[0]
	assert body.contains('record(arg0); return '), body
	assert body.contains('{.ok = true}'), body
	assert g.ensure_callback_userdata_wrapper('record', actual, expected, '')? == adapter
	assert g.callback_wrapper_defs.len == 1
}

fn test_void_callback_does_not_supply_nonvoid_result() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	actual := types.FnType{ return_type: types.Type(types.void_) }
	expected := types.FnType{
		return_type: types.Type(types.ResultType{ base_type: types.Type(types.int_) })
	}
	assert g.ensure_callback_userdata_wrapper('record', actual, expected, '') == none
}

fn test_pointer_cast_mut_argument_materializes_pointer_slot() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	value := types.Type(types.Struct{ name: 'CallbackContext' })
	pointer := types.Type(types.Pointer{ base_type: value })
	slot := types.Type(types.Pointer{ base_type: pointer })
	context := a.add_node(flat.Node{ kind: .ident, value: 'ctx' })
	tc.register_synth_type(context, types.Type(types.Pointer{ base_type: types.Type(types.void_) }))
	start := a.children.len
	a.children << context
	cast := a.add_node(flat.Node{
		kind:           .cast_expr
		value:          '&CallbackContext'
		is_mut:         true
		children_start: i32(start)
		children_count: 1
	})
	tc.register_synth_type(cast, pointer)
	assert g.gen_mut_pointer_slot_arg(cast, a.nodes[int(cast)], slot)
	output := g.sb.str()
	assert output.starts_with('&((CallbackContext*[]){'), output
	assert output.ends_with('})[0]'), output
}

fn test_untyped_null_argument_does_not_materialize_pointer_slot() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	void_pointer := types.Type(types.Pointer{ base_type: types.Type(types.void_) })
	slot := types.Type(types.Pointer{ base_type: void_pointer })
	nil_id := a.add_node(flat.Node{ kind: .nil_literal })
	tc.register_synth_type(nil_id, void_pointer)
	start := a.children.len
	a.children << nil_id
	argument := a.add_node(flat.Node{
		kind:           .block
		value:          'unsafe'
		children_start: i32(start)
		children_count: 1
	})
	tc.register_synth_type(argument, void_pointer)
	assert !g.gen_mut_pointer_slot_arg(argument, a.nodes[int(argument)], slot)
	assert g.sb.len == 0
	mutable_argument := flat.Node{ ...a.nodes[int(argument)], is_mut: true }
	assert g.gen_mut_pointer_slot_arg(argument, mutable_argument, slot)
	assert g.sb.str().starts_with('&((void*[]){')
}
