module c

import v.flat
import v.types

fn test_function_value_reconstruction_keeps_selected_declaration_metadata() {
	for variadic in [false, true] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		params := [types.Type(types.int_),
			types.Type(types.Array{ elem_type: types.Type(types.string_) })]
		g.fn_decl_param_types['callback'] = params
		g.fn_decl_ret_types['callback'] = types.Type(types.int_)
		g.fn_decl_variadic['callback'] = variadic
		// Both registries exist, but identifier reconstruction selects the declaration.
		tc.fn_param_types['callback'] = params
		tc.fn_ret_types['callback'] = types.Type(types.int_)
		tc.fn_variadic['callback'] = !variadic
		declared := g.fn_value_type_for_ident('callback') or { panic('missing declaration') }
		assert declared is types.FnType
		assert (declared as types.FnType).is_variadic == variadic
		// Callback reconstruction selects the checker signature instead.
		checked_callback := g.callback_fn_value_type('callback') or { panic('missing callback') }
		assert checked_callback.is_variadic == !variadic
		tc.fn_param_types.delete('callback')
		declared_callback := g.callback_fn_value_type('callback') or { panic('missing callback') }
		assert declared_callback.is_variadic == variadic
	}
}

fn test_function_value_reconstruction_keeps_checker_variadic_and_fixed_array_types() {
	for variadic in [false, true] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		tc.fn_param_types['callback'] = [types.Type(types.int_),
			types.Type(types.Array{ elem_type: types.Type(types.string_) })]
		tc.fn_ret_types['callback'] = types.Type(types.int_)
		tc.fn_variadic['callback'] = variadic
		checked := g.fn_value_type_for_ident('callback') or { panic('missing checker signature') }
		assert checked is types.FnType
		assert (checked as types.FnType).is_variadic == variadic
		assert checked.name() == if variadic {
			'fn(int, ...string) int'
		} else {
			'fn(int, []string) int'
		}
	}
}
