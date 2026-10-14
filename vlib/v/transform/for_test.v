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

fn test_for_in_slice_type_uses_current_container_before_checker_metadata() {
	for container_type, expected in {
		'[]string':  '[]string'
		'[2]string': '[]string'
		'string':    'string'
	} {
		mut a := flat.FlatAst.new()
		container := a.add_node(flat.Node{ kind: .ident, value: 'route_words' })
		end := a.add_val(.int_literal, '2')
		range_start := a.children.len
		a.children << [flat.empty_node, end]
		range_id := a.add_node(flat.Node{
			kind:           .range
			children_start: range_start
			children_count: 2
		})
		slice_start := a.children.len
		a.children << [container, range_id]
		slice := a.add_node(flat.Node{
			kind:           .index
			value:          'range'
			children_start: slice_start
			children_count: 2
		})
		binding := a.add_val(.ident, 'route_word')
		loop_start := a.children.len
		a.children << [binding, flat.empty_node, slice]
		loop := a.add_node(flat.Node{
			kind:           .for_in_stmt
			value:          '3'
			children_start: loop_start
			children_count: 3
		})
		mut tc := types.TypeChecker.new(&a)
		stale_type := if expected == 'string' { '[]u8' } else { 'string' }
		tc.register_synth_type(slice, tc.parse_type(stale_type))
		mut t := new_transformer(mut a, &tc, {
			'main': true
		})
		t.set_var_type('route_words', container_type)
		assert t.detect_for_in_type(a.nodes[int(loop)]) == expected
	}
}
