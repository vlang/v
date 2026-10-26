module c

import v.flat
import v.types

fn argument_rvalue_node(mut a flat.FlatAst, kind flat.NodeKind, value string, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		value:          value
		children_start: start
		children_count: children.len
	})
}

fn test_argument_address_lowering_preserves_values_locals_consts_calls_and_map_elements() {
	for argument_kind in ['local', 'const', 'call', 'map_index'] {
		for pointer_parameter in [false, true] {
			for full_call in [false, true] {
				mut a := flat.FlatAst.new()
				callee := a.add_val(.ident, 'consume')
				value := a.add_val(.int_literal, '7')
				map_var := a.add_val(.ident, 'table')
				map_key := a.add_val(.int_literal, '0')
				inner_callee := a.add_val(.ident, 'make_value')
				arg := match argument_kind {
					'local' { a.add_val(.ident, 'local') }
					'const' { a.add_val(.ident, 'limit') }
					'call' { argument_rvalue_node(mut a, .call, '', [inner_callee]) }
					else { argument_rvalue_node(mut a, .index, '', [map_var, map_key]) }
				}
				call := argument_rvalue_node(mut a, .call, '', [callee, arg])
				mut tc := types.TypeChecker.new(&a)
				tc.cur_module = 'main'
				int_type := types.Type(types.int_)
				param_type := if pointer_parameter {
					types.Type(types.Pointer{ base_type: &int_type })
				} else {
					int_type
				}
				tc.fn_param_types['consume'] = [param_type]
				tc.fn_ret_types['consume'] = types.Type(types.void_)
				tc.fn_param_types['make_value'] = []types.Type{}
				tc.fn_ret_types['make_value'] = int_type
				tc.cur_scope.insert('local', int_type)
				tc.const_types['limit'] = int_type
				tc.register_synth_type(arg, int_type)
				tc.register_synth_type(value, int_type)
				tc.register_synth_type(map_key, int_type)
				tc.register_synth_type(map_var, types.Type(types.Map{ key_type: &int_type, value_type: &int_type }))
				mut g := FlatGen.new()
				g.a = &a
				g.tc = &tc
				g.const_vals['limit'] = value
				// Keep expression emission independent of the argument's storage shape.
				g.assert_expr_overrides[int(arg)] = 'ARG'
				if full_call {
					g.gen_call(call, a.node(call))
				} else {
					g.gen_call_args('consume', a.node(call), 1)
				}
				ct := tc.c_type(int_type)
				expected_arg := if !pointer_parameter {
					'ARG'
				} else if argument_kind == 'const' {
					'(${ct}[]){ARG}'
				} else if argument_kind == 'call' || (argument_kind == 'map_index' && !full_call) {
					'&((${ct}[]){ARG})[0]'
				} else {
					'&ARG'
				}
				expected := if full_call { 'consume(${expected_arg})' } else { expected_arg }
				assert g.sb.str() == expected, '${argument_kind}, pointer=${pointer_parameter}, full=${full_call}'
			}
		}
	}
}
