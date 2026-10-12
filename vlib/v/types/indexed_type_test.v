module types

import v.flat

fn test_checked_type_ids_follow_retyping_rewrites_and_cache_collisions() {
	mut a := flat.FlatAst.new()
	for _ in 0 .. 4097 {
		a.add_val(.ident, 'value')
	}
	mut tc := TypeChecker.new(&a)
	first := flat.NodeId(0)
	collision := flat.NodeId(4096)
	tc.register_synth_type(first, Type(Array{ elem_type: Type(int_) }))
	tc.register_synth_type(collision, Type(Array{ elem_type: Type(u8_) }))
	ints := tc.checked_expr_type_id(first) or { panic('missing array type') }
	bytes := tc.checked_expr_type_id(collision) or { panic('missing colliding array type') }
	assert ints != bytes
	assert (tc.checked_expr_type_id(first) or { panic('missing checked type') }) == ints
	assert tc.type_name_by_id(ints) == '[]int'
	assert tc.type_name_by_id(bytes) == '[]u8'
	assert (tc.type_by_id(ints) or { panic('missing canonical type') }) is Array
	tc.register_synth_type(first, Type(Array{ elem_type: Type(string_) }))
	strings := (tc.checked_expr_type_id(first) or { panic('missing checked type') })
	assert strings != ints && strings != bytes
	assert tc.type_name_by_id(strings) == '[]string'
	tc.invalidate_checked_expr_type(int(first))
	assert tc.checked_expr_type_id(first) == none
	tc.register_synth_type(first, Type(Array{ elem_type: Type(int_) }))
	assert (tc.checked_expr_type_id(first) or { panic('missing checked type') }) == ints
}

fn test_checked_type_ids_are_private_to_worker_interners() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'value')
	mut tc := TypeChecker.new(&a)
	tc.register_synth_type(id, Type(Array{ elem_type: Type(int_) }))
	parent_id := (tc.checked_expr_type_id(id) or { panic('missing checked type') })
	assert tc.type_name_by_id(parent_id) == '[]int'
	mut worker := tc.fork_for_parallel_transform(&a)
	// Deliberately occupy the parent's numeric identity with a different type.
	other_id, _ := worker.intern_type(Type(Array{ elem_type: Type(u8_) }))
	assert other_id == parent_id
	worker_id := (worker.checked_expr_type_id(id) or { panic('missing worker type') })
	assert worker_id != parent_id
	assert worker.type_name_by_id(worker_id) == '[]int'
	assert worker.type_name_by_id(other_id) == '[]u8'
	assert tc.type_name_by_id(parent_id) == '[]int'
	tc.reset_type_interners()
	new_id := (tc.checked_expr_type_id(id) or { panic('missing checked type') })
	assert tc.type_name_by_id(new_id) == '[]int'
}

fn test_indexed_type_names_do_not_cache_missing_future_ids() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'value')
	mut tc := TypeChecker.new(&a)
	future := TypeId(tc.type_count())
	assert tc.type_by_id(future) == none
	assert tc.type_name_by_id(future) == 'unknown'
	tc.register_synth_type(id, Type(Array{ elem_type: Type(bool_) }))
	actual := (tc.checked_expr_type_id(id) or { panic('missing checked type') })
	assert actual == future
	assert tc.type_name_by_id(actual) == '[]bool'
}

fn test_checked_type_ids_keep_same_spelling_different_layouts_separate() {
	mut a := flat.FlatAst.new()
	first := a.add_val(.ident, 'first')
	second := a.add_val(.ident, 'second')
	mut tc := TypeChecker.new(&a)
	tc.register_synth_type(first, Type(ArrayFixed{ elem_type: Type(int_), len: 4, len_expr: 'limit' }))
	tc.register_synth_type(second, Type(ArrayFixed{ elem_type: Type(int_), len: 8, len_expr: 'limit' }))
	first_id := tc.checked_expr_type_id(first) or { panic('missing first layout') }
	second_id := tc.checked_expr_type_id(second) or { panic('missing second layout') }
	assert first_id != second_id
	assert tc.type_name_by_id(first_id) == tc.type_name_by_id(second_id)
	first_type := tc.type_by_id(first_id) or { panic('missing first type') }
	second_type := tc.type_by_id(second_id) or { panic('missing second type') }
	assert first_type is ArrayFixed && first_type.len == 4
	assert second_type is ArrayFixed && second_type.len == 8
}
