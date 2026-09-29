module transform

import v.flat
import v.types

fn test_for_in_pointer_storage_looks_through_parentheses() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, {
		'main': true
	})
	ident := a.add_node(flat.Node{ kind: .ident, value: 'entries' })
	mut wrapped := ident
	for _ in 0 .. 2 {
		start := a.children.len
		a.children << wrapped
		wrapped = a.add_node(flat.Node{
			kind:           .paren
			children_start: i32(start)
			children_count: 1
		})
	}
	assert !t.for_in_container_is_pointer_storage(wrapped)
	t.mut_param_values['entries'] = true
	assert t.for_in_container_is_pointer_storage(ident)
	assert t.for_in_container_is_pointer_storage(wrapped)
}
