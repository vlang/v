module transform

import v.flat
import v.types

fn test_inferred_globals_use_checked_pointer_and_array_types() {
	for type_name in ['&int', 'GlobalValues'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.type_aliases['GlobalValues'] = '[3]int'
		tc.file_scope.insert('values', tc.parse_type(type_name))
		initializer := a.add_val(.int_literal, '0')
		field_start := a.begin_children()
		a.add_child(initializer)
		field := a.add_node(flat.Node{
			kind:           .field_decl
			value:          'values'
			children_start: field_start
			children_count: 1
		})
		global_start := a.begin_children()
		a.add_child(field)
		a.add_node(flat.Node{
			kind:           .global_decl
			children_start: global_start
			children_count: 1
		})
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.collect_types()
		// Transform stores normalized aliases, while keeping pointer indirection.
		expected := if type_name == 'GlobalValues' { '[3]int' } else { '&int' }
		assert t.globals['values'] == expected
	}
}
