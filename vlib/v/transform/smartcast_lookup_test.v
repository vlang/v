module transform

import v.flat

fn smartcast_lookup_test_expr(mut a flat.FlatAst) flat.NodeId {
	base := a.add_val(.ident, 'object')
	start := a.children.len
	a.children << base
	field := a.add_node(flat.Node{
		kind:           .selector
		value:          'member'
		children_start: i32(start)
		children_count: 1
	})
	index := a.add_val(.int_literal, '7')
	index_start := a.children.len
	a.children << field
	a.children << index
	return a.add_node(flat.Node{
		kind:           .index
		children_start: i32(index_start)
		children_count: 2
	})
}

fn test_empty_smartcast_lookup_does_not_allocate_expression_keys() {
	$if prealloc && prealloc_stats ? {
		mut a := flat.FlatAst.new()
		id := smartcast_lookup_test_expr(mut a)
		t := Transformer{ a: &a }
		before := prealloc_stats_snapshot()
		assert before.enabled
		for _ in 0 .. 4096 {
			if _ := t.find_smartcast_for_expr(id) {
				assert false, 'empty context matched an expression'
			}
			assert t.smartcasts_for_expr(id).len == 0
		}
		after := prealloc_stats_snapshot()
		assert after.allocation_count == before.allocation_count
		assert after.allocated_bytes == before.allocated_bytes
	}
}

fn test_smartcast_context_miss_does_not_allocate_a_result_array() {
	$if prealloc && prealloc_stats ? {
		t := Transformer{
			smartcast_stack: [SmartcastContext{ expr_name: 'other' }]
		}
		before := prealloc_stats_snapshot()
		assert before.enabled
		for _ in 0 .. 4096 {
			assert t.smartcasts_for('object.member[7]').len == 0
		}
		after := prealloc_stats_snapshot()
		assert after.allocation_count == before.allocation_count
		assert after.allocated_bytes == before.allocated_bytes
	}
}

fn test_expression_smartcast_contexts_keep_order_and_remove_duplicates() {
	mut a := flat.FlatAst.new()
	id := smartcast_lookup_test_expr(mut a)
	first := SmartcastContext{ expr_name: 'object.member[7]', variant_name: flat.source_type_int }
	second := SmartcastContext{ expr_name: 'object.member[7]', variant_name: flat.source_type_bool }
	t := Transformer{
		a:               &a
		smartcast_stack: [first, SmartcastContext{ expr_name: 'different' }, second, first]
	}
	assert t.smartcasts_for_expr(id) == [first, second]
	literal := a.add_val(.int_literal, '1')
	assert t.smartcasts_for_expr(literal).len == 0
}

fn test_expression_smartcast_lookup_preserves_shadowing_and_unnamed_keys() {
	mut a := flat.FlatAst.new()
	id := smartcast_lookup_test_expr(mut a)
	mut t := Transformer{ a: &a }
	assert t.find_smartcast_for_expr(id) == none
	t.smartcast_stack = [
		SmartcastContext{ expr_name: 'object.member[7]', variant_name: flat.source_type_int },
		SmartcastContext{ expr_name: 'different', variant_name: flat.source_type_string },
		SmartcastContext{ expr_name: 'object.member[7]', variant_name: flat.source_type_bool },
	]
	assert (t.find_smartcast_for_expr(id) or { panic('missing inner context') }).variant_name == flat.source_type_bool
	t.smartcast_stack.delete_last()
	assert (t.find_smartcast_for_expr(id) or { panic('missing outer context') }).variant_name == flat.source_type_int
	t.smartcast_stack.clear()
	assert t.find_smartcast_for_expr(id) == none
	// A nonempty stack retains find_smartcast's handling of an unnamed key.
	literal := a.add_val(.int_literal, '1')
	t.smartcast_stack << SmartcastContext{ expr_name: '', variant_name: flat.source_type_bool }
	assert (t.find_smartcast_for_expr(literal) or { panic('missing unnamed context') }).variant_name == flat.source_type_bool
}

fn test_selector_type_keeps_field_and_receiver_smartcasts() {
	mut a := flat.FlatAst.new()
	object := a.add_val(.ident, 'object')
	start := a.begin_children()
	a.add_child(object)
	member := a.add_node(flat.Node{
		kind:           .selector
		value:          'member'
		children_start: i32(start)
		children_count: 1
	})
	mut t := Transformer{
		a:       &a
		structs: {
			flat.source_type_id('Box'):   StructInfo{
				name:   flat.source_type_id('Box')
				fields: [FieldInfo{ name: 'member', typ: flat.source_type_int }]
			}
			flat.source_type_id('Other'): StructInfo{
				name:   flat.source_type_id('Other')
				fields: [FieldInfo{ name: 'member', typ: flat.source_type_string }]
			}
		}
	}
	t.set_var_type('object', flat.source_type_id('Box'))
	assert t.resolve_selector_type(a.nodes[int(member)]) == flat.source_type_int
	t.push_smartcast('object', flat.source_type_id('Other'), flat.source_type_id(option_unwrap_marker))
	assert t.resolve_selector_type(a.nodes[int(member)]) == flat.source_type_string
	t.push_smartcast('object.member', flat.source_type_bool, flat.source_type_id(option_unwrap_marker))
	assert t.resolve_selector_type(a.nodes[int(member)]) == flat.source_type_bool
	t.pop_smartcast()
	assert t.resolve_selector_type(a.nodes[int(member)]) == flat.source_type_string
	t.pop_smartcast()
	assert t.resolve_selector_type(a.nodes[int(member)]) == flat.source_type_int
}
