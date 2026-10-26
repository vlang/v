module c

import v.flat
import v.types

fn test_unused_callback_preseeding_keeps_module_scoped_signatures() {
	// The same raw parameter spelling can denote different callback ABIs in
	// different modules. An empty annotation still has a checked signature.
	for annotation in [u32(0), flat.source_type_id('Callback')] {
		mut a := flat.FlatAst.new()
		param := a.add_node(flat.Node{
			kind:  .param
			value: 'callback'
			typ:   annotation
		})
		a.children << param
		decl := flat.Node{
			kind:           .fn_decl
			value:          'unused'
			children_start: 0
			children_count: 1
		}
		mut tc := types.TypeChecker.new(&a)
		integer := tc.intern_type_reference(types.Type(types.int_))
		text := tc.intern_type_reference(types.Type(types.string_))
		integer_callback := types.FnType{
			params:      [types.FnParam{ typ: integer }]
			return_type: integer
		}
		string_callback := types.FnType{
			params:      [types.FnParam{ typ: text }]
			return_type: text
		}
		tc.fn_param_types['one.unused'] = [types.Type(integer_callback)]
		tc.fn_param_types['two.unused'] = [types.Type(string_callback)]
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.unused_param_seen = &UnusedParamSeen{}
		integer_key := g.fn_ptr_type_key(integer_callback)
		string_key := g.fn_ptr_type_key(string_callback)
		assert integer_key != string_key

		g.preseed_unused_fn_ptr_param_types(decl, 'one', 'one.v')
		assert integer_key in g.fn_ptr_types
		assert string_key !in g.fn_ptr_types

		g.preseed_unused_fn_ptr_param_types(decl, 'two', 'two.v')
		assert integer_key in g.fn_ptr_types
		assert string_key in g.fn_ptr_types
	}
}
