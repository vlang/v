module types

import v.flat
import v.token

fn lazy_rhs_node(mut a flat.FlatAst, kind flat.NodeKind, children []flat.NodeId) flat.NodeId {
	start := a.begin_children()
	for child in children {
		a.add_child(child)
	}
	return a.add_node(flat.Node{
		kind:           kind
		children_start: start
		children_count: i32(children.len)
	})
}

fn lazy_rhs_decl(mut a flat.FlatAst, name string, offset int, value string) (flat.NodeId, flat.NodeId) {
	lhs := a.add_node(flat.Node{
		kind:  .ident
		value: name
		pos:   token.new_pos(0, offset)
	})
	rhs := a.add_val(.int_literal, value)
	return lazy_rhs_node(mut a, .decl_assign, [lhs, rhs]), rhs
}

fn test_local_decl_rhs_lazy_index_keeps_range_and_lookup_semantics() {
	mut a := flat.FlatAst.new()
	first, first_rhs := lazy_rhs_decl(mut a, 'value', 10, '1')
	second, second_rhs := lazy_rhs_decl(mut a, 'value', 30, '2')
	inner, _ := lazy_rhs_decl(mut a, 'value', 50, '3')
	closure := lazy_rhs_node(mut a, .fn_literal, [inner])
	use := a.add_node(flat.Node{ kind: .ident, value: 'value', pos: token.new_pos(0, 60) })
	fn_id := lazy_rhs_node(mut a, .fn_decl, [first, second, closure, use])
	for range_mode in [false, true] {
		mut tc := TypeChecker.new(&a)
		tc.build_direct_parent_index(&a)
		tc.fn_context.node_id = int(fn_id)
		if range_mode {
			tc.check_range_lo = 0
			tc.check_range_hi = int(fn_id)
		}
		tc.prepare_local_decl_rhs_index(fn_id)
		assert tc.fn_context.local_decl_rhs_pending
		assert !tc.fn_context.local_decl_rhs_indexed
		assert tc.fn_context.local_decl_rhs_by_name.len == 0
		// The active work-item range can change while an outer context is saved.
		tc.check_range_lo = -1
		tc.check_range_hi = -1
		assert tc.local_decl_rhs_before('value', use) or { flat.empty_node } == second_rhs
		assert tc.fn_context.local_decl_rhs_indexed
		assert !tc.fn_context.local_decl_rhs_pending
		assert tc.fn_context.local_decl_rhs_by_name['value'].len == 2
		assert tc.fn_context.local_decl_rhs_by_name['value'].any(it.rhs == first_rhs)
		assert tc.local_decl_rhs_before('missing', use) == none
	}
}

fn test_local_decl_rhs_lazy_context_copy_restore_and_sibling_reset() {
	mut a := flat.FlatAst.new()
	decl, rhs := lazy_rhs_decl(mut a, 'value', 10, '1')
	use := a.add_node(flat.Node{ kind: .ident, value: 'value', pos: token.new_pos(0, 20) })
	fn_id := lazy_rhs_node(mut a, .fn_decl, [decl, use])
	mut tc := TypeChecker.new(&a)
	tc.fn_context.node_id = int(fn_id)
	tc.check_range_lo = 0
	tc.check_range_hi = int(fn_id)
	tc.prepare_local_decl_rhs_index(fn_id)
	saved := tc.fn_context
	copy := clone_function_check_context(tc.fn_context)
	tc.fn_context = new_function_check_context()
	tc.fn_context.node_id = int(fn_id)
	assert !tc.fn_context.local_decl_rhs_pending
	assert tc.local_decl_rhs_before('value', use) or { flat.empty_node } == rhs
	tc.fn_context = copy
	assert tc.fn_context.local_decl_rhs_pending
	assert tc.local_decl_rhs_before('value', use) or { flat.empty_node } == rhs
	assert !tc.fn_context.local_decl_rhs_pending
	assert saved.local_decl_rhs_pending
	assert saved.local_decl_rhs_by_name.len == 0
	tc.fn_context = saved
	assert tc.local_decl_rhs_before('value', use) or { flat.empty_node } == rhs
	tc.fn_context.reset_for_sibling_check()
	assert tc.fn_context == new_function_check_context()
}

fn test_local_decl_rhs_lazy_preparation_preserves_eager_goto_flags() {
	mut a := flat.FlatAst.new()
	inner_goto := a.add_node(flat.Node{ kind: .goto_stmt })
	closure := lazy_rhs_node(mut a, .fn_literal, [inner_goto])
	inner_fn := lazy_rhs_node(mut a, .fn_decl, [closure])
	outer_goto := a.add_node(flat.Node{ kind: .goto_stmt })
	outer_fn := lazy_rhs_node(mut a, .fn_decl, [outer_goto])
	for fn_id in [inner_fn, outer_fn] {
		for range_mode in [false, true] {
			mut eager := TypeChecker.new(&a)
			mut lazy := TypeChecker.new(&a)
			eager.fn_context.node_id = int(fn_id)
			lazy.fn_context.node_id = int(fn_id)
			if range_mode {
				eager.check_range_lo = if fn_id == inner_fn { 0 } else { int(inner_fn) + 1 }
				eager.check_range_hi = int(fn_id)
				lazy.check_range_lo = eager.check_range_lo
				lazy.check_range_hi = int(fn_id)
			}
			eager.index_local_decl_rhs(fn_id)
			lazy.prepare_local_decl_rhs_index(fn_id)
			assert lazy.fn_context.has_goto_nodes == eager.fn_context.has_goto_nodes
			assert lazy.fn_context.local_decl_rhs_by_name.len == 0
			assert lazy.fn_context.local_decl_rhs_pending
			assert lazy.local_decl_rhs_before('', fn_id) == none
			assert lazy.local_decl_rhs_before('missing', flat.empty_node) == none
			assert lazy.fn_context.local_decl_rhs_pending
		}
	}
}
