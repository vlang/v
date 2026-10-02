module transform

import v.flat
import v.types

fn test_promoted_sizeof_strips_only_heap_storage_indirection() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for value_type in ['Record', 'RecordAlias', 'u32', '[3]u32', '&Record'] {
		t.set_var_type('value', '&${value_type}')
		t.heaped_amp_locals['value'] = true
		sizeof_name := flat.Node{ kind: .sizeof_expr, value: 'value' }
		assert t.promoted_sizeof_value_type(sizeof_name)? == value_type
		value := t.make_ident('value')
		paren_start := t.a.children.len
		t.a.children << value
		paren := t.a.add_node(flat.Node{ kind: .paren, children_start: paren_start, children_count: 1 })
		sizeof_start := t.a.children.len
		t.a.children << paren
		sizeof_expr := flat.Node{ kind: .sizeof_expr, children_start: sizeof_start, children_count: 1 }
		assert t.promoted_sizeof_value_type(sizeof_expr)? == value_type
	}
	t.heaped_amp_locals.clear()
	assert t.promoted_sizeof_value_type(flat.Node{ kind: .sizeof_expr, value: 'value' }) == none
}
