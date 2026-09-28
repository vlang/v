module c

import v.flat
import v.types

fn test_c_selector_calls_cast_byte_compatible_fixed_arrays() {
	for nested in [false, true] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		actual_elem := if nested {
			types.Type(types.ArrayFixed{ elem_type: types.Type(types.Char{}), len: 2 })
		} else {
			types.Type(types.Char{})
		}
		expected_elem := if nested {
			types.Type(types.ArrayFixed{ elem_type: types.Type(types.u8_), len: 2 })
		} else {
			types.Type(types.u8_)
		}
		tc.fn_param_types['C.consume'] = [types.Type(types.Pointer{ base_type: expected_elem })]
		tc.fn_ret_types['C.consume'] = types.Type(types.u8_)
		base := a.add_val(.ident, 'C')
		selector_start := a.begin_children()
		a.add_child(base)
		selector := a.add_node(flat.Node{
			kind:           .selector
			value:          'consume'
			children_start: selector_start
			children_count: 1
		})
		argument := a.add_val(.ident, 'chars')
		tc.register_synth_type(argument, types.Type(types.ArrayFixed{ elem_type: actual_elem, len: 2 }))
		call_start := a.begin_children()
		a.add_child(selector)
		a.add_child(argument)
		call := a.add_node(flat.Node{
			kind:           .call
			children_start: call_start
			children_count: 2
		})
		generated := g.expr_to_string(call)
		expected_cast := if nested { '(Array_fixed_u8_2*)chars' } else { '(u8*)chars' }
		assert generated == 'consume(${expected_cast})'
	}
}
