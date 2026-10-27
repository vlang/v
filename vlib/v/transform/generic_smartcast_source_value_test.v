module transform

import v.flat
import v.types

fn test_generic_typeof_smartcast_consumes_source_backed_is_and_typeof_values() {
	mut a := flat.FlatAst.new()
	sum_id := flat.source_type_id('Choice')
	child := a.add_node(flat.Node{ kind: .ident, value: 'subject', typ: sum_id })
	children_start := a.children.len
	a.children << child
	is_id := a.add_node(flat.Node{
		kind:           .is_expr
		source_value:   flat.source_type_id('T')
		children_start: children_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types.set_ids(flat.source_type_id('Choice'), [flat.source_type_int, flat.source_type_string])
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types.set_ids(flat.source_type_id('Choice'), [flat.source_type_int, flat.source_type_string])
	t.active_generic_params = ['T']
	assert a.nodes[int(is_id)].value == ''
	t.record_generic_typeof_is_smartcast(is_id, [flat.source_type_int], true)
	assert t.generic_typeof_smartcasts.len == 1
	assert t.generic_typeof_smartcasts[0].variant_name == flat.source_type_int
	assert t.generic_typeof_smartcasts[0].sum_type_name == sum_id
	plain_typeof := flat.Node{
		kind:           .typeof_expr
		children_start: children_start
		children_count: 1
	}
	assert t.generic_typeof_smartcast(plain_typeof)? == flat.source_type_int
	// A source-backed explicit type keeps its own typeof resolution, even when
	// its child names a narrowed binding and its raw value field is empty.
	explicit_typeof := flat.Node{
		...plain_typeof
		source_value: flat.source_type_id('Box[T]')
	}
	assert explicit_typeof.value == ''
	assert t.generic_typeof_smartcast(explicit_typeof) == none
	t.generic_typeof_smartcasts = [SmartcastContext{
		expr_name:     'subject'
		variant_name:  flat.source_type_bool
		sum_type_name: option_unwrap_marker_id
	}]
	assert t.generic_typeof_smartcast(plain_typeof)? == flat.source_type_bool
}
