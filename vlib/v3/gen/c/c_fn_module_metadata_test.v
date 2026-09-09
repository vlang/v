module c

import v3.flat
import v3.types

fn test_c_fn_call_metadata_is_module_scoped() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed_params := [types.Type(types.int_), types.Type(types.int_), types.Type(types.int_)]
	variadic_params := [types.Type(types.int_), types.Type(types.int_), types.Type(types.u64_)]
	tc.c_fn_module_ret_types['fixed\x01C.probe'] = types.Type(types.int_)
	tc.c_fn_module_param_types['fixed\x01C.probe'] = fixed_params
	tc.c_fn_module_variadic['fixed\x01C.probe'] = false
	tc.c_fn_module_ret_types['variadic\x01C.probe'] = types.Type(types.int_)
	tc.c_fn_module_param_types['variadic\x01C.probe'] = variadic_params
	tc.c_fn_module_variadic['variadic\x01C.probe'] = true
	tc.c_fn_abi_variadic_prefixes['C.probe'] = 2
	// Simulate the program-wide compatibility metadata having retained the other view.
	tc.fn_param_types['C.probe'] = variadic_params
	tc.c_variadic_fns['C.probe'] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc

	tc.cur_module = 'fixed'
	assert g.param_types_for('C.probe', 'probe') == fixed_params
	assert !(g.module_c_fn_variadic('C.probe', 'probe') or { true })
	assert g.c_fn_abi_variadic_prefix('C.probe', 'probe', fixed_params) == 2

	tc.fn_param_types['C.probe'] = fixed_params
	tc.c_variadic_fns['C.probe'] = false
	tc.cur_module = 'variadic'
	assert g.param_types_for('C.probe', 'probe') == variadic_params
	assert g.module_c_fn_variadic('C.probe', 'probe') or { false }
	assert g.c_fn_abi_variadic_prefix('C.probe', 'probe', variadic_params) == 2
}
