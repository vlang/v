module transform

import v.flat

fn test_known_struct_selector_does_not_inherit_unrelated_field_type() {
	for receiver_type in ['Model', '&Model', 'Unregistered'] {
		mut a := flat.FlatAst.new()
		receiver := a.add_val(.ident, 'm')
		start := a.children.len
		a.children << receiver
		selector := a.add_node(flat.Node{
			kind:           .selector
			value:          'on_done'
			children_start: start
			children_count: 1
		})
		t := Transformer{
			a:             &a
			cur_module:    'main'
			var_types:     [
				VarTypeBinding{
					name: 'm'
					typ:  receiver_type
				},
			]
			structs:       {
				'Model': StructInfo{}
			}
			unique_fields: {
				'on_done': '?fn ()'
			}
		}
		// Model has no on_done field: a method selector must not acquire the
		// optional callback type belonging to another struct. Keep the fallback
		// for receivers whose struct information is unavailable.
		expected := if receiver_type == 'Unregistered' { '?fn ()' } else { '' }
		assert t.resolve_selector_type(a.nodes[int(selector)]) == expected
	}
}
