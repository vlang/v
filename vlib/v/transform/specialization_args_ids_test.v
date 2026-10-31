module transform

import v.flat
import v.types

fn test_specialization_args_source_and_symbol_slots_preserve_first_write() {
	mut bindings := SpecializationArgsBindings{}
	type_id := flat.source_type_id('Box[int]')
	bindings.set_source_ids(type_id, [flat.source_type_int])
	assert bindings.contains('Box[int]')
	assert bindings.source_ids(type_id) or { []u32{} } == [flat.source_type_int]
	assert bindings.type_ids('Box[int]') or { []u32{} } == [flat.source_type_int]
	assert !bindings.contains(' Box[int] ')
	// Exact-byte aliases update the existing slot rather than adding a second key.
	bindings.set_ids('Box[int]', [flat.source_type_string])
	assert bindings.len == 1
	assert bindings.keys()[0].type_id == type_id
	assert bindings.source_ids(type_id) or { []u32{} } == [flat.source_type_string]
	mut reverse := SpecializationArgsBindings{}
	reverse.set_ids('Box[int]', [flat.source_type_string])
	reverse.set_source_ids(type_id, [flat.source_type_int])
	assert reverse.len == 1
	assert reverse.keys()[0].name == 'Box[int]'
	assert reverse.source_ids(type_id) or { []u32{} } == [flat.source_type_int]
}

fn test_specialization_args_source_bucket_collision_and_owned_clone() {
	mut bindings := SpecializationArgsBindings{}
	first := flat.source_type_id('Box[int]')
	second := flat.source_type_id('Box[string]')
	mut ids := [flat.source_type_int]
	bindings.set_source_ids(first, ids)
	ids[0] = flat.source_type_string
	bindings.set_source_ids(second, [flat.source_type_string])
	// Force an unrelated source ID into a lookup bucket; equality remains exact.
	bindings.type_text_buckets[specialization_args_text_hash('Box[int]')] = [second, first]
	assert bindings.type_ids('Box[int]') or { []u32{} } == [flat.source_type_int]
	mut copied := bindings.clone()
	copied.set_source_ids(first, [flat.source_type_string])
	assert bindings.source_ids(first) or { []u32{} } == [flat.source_type_int]
	assert copied.keys().len == 2
}

fn test_recorded_source_specialization_args_keep_overlay_and_pointer_order() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.spec_args_misses = &SpecArgsMissCache{}
	type_id := flat.source_type_id('Box[int]')
	assert t.recorded_type_specialization_args(type_id) == none
	t.generic_specialization_args.set_source_ids(type_id, [flat.source_type_int])
	assert t.recorded_type_specialization_args(flat.source_type_id(' &&Box[int] ')) or { []u32{} } == [flat.source_type_int]
	assert t.recorded_generic_specialization_args('Box[int]') or { []u32{} } == [flat.source_type_int]
	mut parent := SpecializationArgsBindings{}
	parent.set_ids('Box[int]', [flat.source_type_string])
	t.generic_specialization_args_parent = &parent
	assert t.recorded_type_specialization_args(type_id) or { []u32{} } == [flat.source_type_int]
}
