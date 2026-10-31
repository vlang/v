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
	assert flat.source_type_name(tc.type_name_by_id(ints)) == '[]int'
	assert flat.source_type_name(tc.type_name_by_id(bytes)) == '[]u8'
	assert (tc.type_by_id(ints) or { panic('missing canonical type') }) is Array
	tc.register_synth_type(first, Type(Array{ elem_type: Type(string_) }))
	strings := (tc.checked_expr_type_id(first) or { panic('missing checked type') })
	assert strings != ints && strings != bytes
	assert flat.source_type_name(tc.type_name_by_id(strings)) == '[]string'
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
	assert flat.source_type_name(tc.type_name_by_id(parent_id)) == '[]int'
	assert tc.c_type_by_id(parent_id) or { panic('missing parent C type') } == 'Array'
	mut worker := tc.fork_for_parallel_transform(&a)
	// Deliberately occupy the parent's numeric identity with a different type.
	other_id, _ := worker.intern_type(Type(Array{ elem_type: Type(u8_) }))
	assert other_id == parent_id
	worker_id := (worker.checked_expr_type_id(id) or { panic('missing worker type') })
	assert worker_id != parent_id
	assert flat.source_type_name(worker.type_name_by_id(worker_id)) == '[]int'
	assert flat.source_type_name(worker.type_name_by_id(other_id)) == '[]u8'
	assert worker.c_type_by_id(other_id) or { panic('missing worker C type') } == 'Array'
	assert flat.source_type_name(tc.type_name_by_id(parent_id)) == '[]int'
	tc.reset_type_interners()
	new_id := (tc.checked_expr_type_id(id) or { panic('missing checked type') })
	assert flat.source_type_name(tc.type_name_by_id(new_id)) == '[]int'
}

fn test_indexed_type_names_do_not_cache_missing_future_ids() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'value')
	mut tc := TypeChecker.new(&a)
	tc.intern_type(Type(bool_))
	future := TypeId(tc.type_count())
	assert tc.type_by_id(future) == none
	assert tc.c_type_by_id(future) == none
	assert tc.type_name_by_id(future) == flat.source_type_unknown
	tc.register_synth_type(id, Type(Array{ elem_type: Type(bool_) }))
	actual := (tc.checked_expr_type_id(id) or { panic('missing checked type') })
	assert actual == future
	assert flat.source_type_name(tc.type_name_by_id(actual)) == '[]bool'
	assert tc.c_type_by_id(actual) or { panic('missing C type') } == 'Array'
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
	first_c := tc.c_type_by_id(first_id) or { panic('missing first C layout') }
	second_c := tc.c_type_by_id(second_id) or { panic('missing second C layout') }
	assert first_c != second_c
	assert first_c == tc.c_type(first_type)
	assert second_c == tc.c_type(second_type)
	assert tc.c_type_by_id(first_id) or { panic('missing cached C layout') } == first_c
	tc.clear_c_type_cache()
	assert tc.c_type_by_id(first_id) or { panic('missing cleared C layout') } == first_c
}

fn test_indexed_c_types_survive_recent_slot_collisions_and_interner_reset() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	mut ids := []TypeId{}
	mut expected := []string{}
	for length in 1 .. 2050 {
		id, _ := tc.intern_type(Type(ArrayFixed{ elem_type: Type(int_), len: length }))
		ids << id
		expected << tc.c_type_by_id(id) or { panic('missing C layout') }
	}
	for i, id in ids {
		assert tc.c_type_by_id(id) or { panic('missing evicted C layout') } == expected[i]
	}
	tc.reset_type_interners()
	assert tc.c_type_by_id(ids[0]) == none
	reused, _ := tc.intern_type(Type(Pointer{ base_type: Type(bool_) }))
	assert reused == ids[0]
	assert tc.c_type_by_id(reused) or { panic('missing replacement C layout') } == 'bool*'
}

fn test_parsed_ids_follow_generic_recursive_alias_and_worker_precedence() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.set_fresh_type_cache(true)
	integer := tc.parse_type_id(flat.source_type_u64) or { panic('missing integer identity') }
	integer_reference := tc.parse_type(flat.source_type_u64)
	assert voidptr(tc.parse_type(flat.source_type_u64)) == voidptr(integer_reference)
	tc.cur_module = 'other'
	tc.cur_file = 'other.v'
	assert tc.parse_type_id(flat.source_type_u64) or { panic('missing contextual identity') } == integer
	tc.type_param_texts[flat.source_type_u64] = flat.source_type_string
	bound := tc.parse_type_id(flat.source_type_u64) or { panic('missing bound identity') }
	assert bound != integer
	assert tc.type_name_by_id(bound) == flat.source_type_string
	tc.type_param_texts.clear()
	tc.type_cache.alias_parse_stack << flat.source_type_u64
	recursive := tc.parse_type_id(flat.source_type_u64) or { panic('missing recursive identity') }
	assert recursive != integer
	assert tc.type_by_id(recursive) or { panic('missing recursive alias') } is Alias
	tc.type_cache.alias_parse_stack.clear()
	assert tc.parse_type_id(flat.source_type_u64) or { panic('missing restored identity') } == integer
	mut worker := tc.fork_for_parallel_transform(&a)
	worker.set_fresh_type_cache(true)
	boolean := worker.parse_type_id(flat.source_type_bool) or { panic('missing worker boolean') }
	assert boolean == integer
	worker_integer := worker.parse_type_id(flat.source_type_u64) or { panic('missing worker integer') }
	assert worker_integer != integer
	assert worker.type_name_by_id(boolean) == flat.source_type_bool
	assert worker.type_name_by_id(worker_integer) == flat.source_type_u64
	tc.reset_type_interners()
	replacement := tc.parse_type_id(flat.source_type_bool) or { panic('missing replacement identity') }
	assert replacement == integer
	assert tc.type_name_by_id(replacement) == flat.source_type_bool
	assert tc.parse_type(flat.source_type_u64) is Primitive
	assert voidptr(tc.parse_type(flat.source_type_u64)) != voidptr(integer_reference)
}
