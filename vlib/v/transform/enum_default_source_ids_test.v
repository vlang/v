module transform

import v.flat
import v.types

fn test_enum_default_keeps_type_id_and_emitted_member_spelling() {
	for module_name in ['main', 'palette'] {
		mut a := flat.FlatAst.new()
		a.add_val(.module_decl, module_name)
		initial := a.add_val(.int_literal, '7')
		field_start := a.begin_children()
		a.add_child(initial)
		field := a.add_node(flat.Node{
			kind:           .enum_field
			value:          'first'
			children_start: field_start
			children_count: 1
		})
		decl_start := a.begin_children()
		a.add_child(field)
		a.add_node(flat.Node{
			kind:           .enum_decl
			value:          'Mode'
			children_start: decl_start
			children_count: 1
		})
		qualified := '${module_name}.Mode'
		expected := if module_name == 'main' { 'Mode' } else { qualified }
		expected_id := flat.source_type_id(expected)
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = module_name
		tc.enum_names[qualified] = true
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.enum_types[expected] = ['first']
		t.enum_explicit_first_values[expected] = true
		type_id, member := t.enum_nonzero_default_member(flat.source_type_id(qualified)) or {
			assert false
			return
		}
		assert type_id == expected_id
		assert member == 'first'
		value := t.enum_nonzero_default_value(flat.source_type_id(qualified)) or {
			assert false
			return
		}
		assert a.nodes[int(value)].typ == expected_id
		assert a.nodes[int(value)].value == '${expected}.first'
	}
}
