module mir

import v.ssa

fn test_type_size_and_align_follow_ssa_type_layout() {
	mut sm := ssa.Module.new()
	i8_t := sm.type_store.get_int(8)
	i32_t := sm.type_store.get_int(32)
	ptr_t := sm.type_store.get_ptr(i32_t)
	pair_t := sm.type_store.get_tuple([i8_t, ptr_t])
	m := lower_from_ssa(sm)
	assert m.type_size(i32_t) == 4
	assert m.type_align(i32_t) == 4
	assert m.type_size(ptr_t) == 8
	assert m.type_align(ptr_t) == 8
	assert m.type_size(pair_t) == 16
	assert m.type_align(pair_t) == 8
}
