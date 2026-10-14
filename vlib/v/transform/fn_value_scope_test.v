module transform

import v.flat
import v.types

fn test_monomorphization_callback_inference_uses_checked_binding() {
	mut a := flat.FlatAst.new()
	argument := a.add_node(flat.Node{ kind: .ident, value: 'initial', typ: 'fn (int) int' })
	mut tc := types.TypeChecker.new(&a)
	tc.register_synth_type(argument, types.Type(types.FnType{
		params:      [types.Type(types.int_)]
		return_type: types.Type(types.int_)
	}))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('initial', 'fn (string) string')
	t.in_monomorphize_scan = true
	callback_type := t.fn_value_type_name(argument) or { '' }
	assert callback_type.replace(' ', '') == 'fn(int)int'
}

fn test_specialization_callback_inference_uses_live_parameter_binding() {
	mut a := flat.FlatAst.new()
	argument := a.add_node(flat.Node{ kind: .ident, value: 'initial', typ: 'T' })
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.in_monomorphize_scan = true
	t.cloning_generic_fn_depth = 1
	t.set_var_type('initial', 'fn (string) string')
	assert t.fn_value_type_name(argument) or { '' } == 'fn (string) string'
	t.set_var_type('initial', 'int')
	tc.fn_param_types['initial'] = [types.Type(types.int_)]
	tc.fn_ret_types['initial'] = types.int_
	assert t.fn_value_type_name(argument) == none
}

fn test_raw_generic_callback_specialization_ignores_previous_function_locals() {
	mut a := flat.FlatAst.new()
	argument := a.add_node(flat.Node{ kind: .ident, value: 'initial', typ: 'fn (int) int' })
	parameter := a.add_node(flat.Node{ kind: .param, value: 'value', typ: 'T' })
	declaration_start := a.children.len
	a.children << parameter
	mut function := flat.Node{
		kind:           .fn_decl
		value:          'duplicate'
		typ:            'T'
		children_start: declaration_start
		children_count: 1
	}
	function.set_generic_params(['T'])
	declaration := GenericFnDecl{ node: function, module: 'main', key: 'duplicate' }
	callee := a.add_val(.ident, 'duplicate')
	call_start := a.children.len
	a.children << callee
	a.children << argument
	call := flat.Node{ kind: .call, children_start: call_start, children_count: 2 }
	mut tc := types.TypeChecker.new(&a)
	tc.register_synth_type(argument, tc.parse_type('fn (int) int'))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('initial', 'fn (string) string')
	t.in_monomorphize_scan = true
	arguments := t.infer_generic_call_args_from_raw_node_types(declaration, call) or {
		assert false, 'checked callback type must infer T'
		return
	}
	assert arguments.len == 1
	assert arguments[0].replace(' ', '') == 'fn(int)int'
}
