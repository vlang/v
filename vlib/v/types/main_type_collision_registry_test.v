module types

import v.flat

fn test_main_type_collision_ids_terminate_alias_cycles_and_follow_wrapped_targets() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	first_id := flat.source_type_id('First')
	second_id := flat.source_type_id('Second')
	leaf_id := flat.source_type_id('Leaf')
	tc.type_aliases.set_id(first_id, flat.source_type_id('[]Second'))
	tc.type_aliases.set_id(second_id, flat.source_type_id('map[string]First'))
	tc.structs[leaf_id] = []StructField{}
	tc.structs[flat.source_type_id('dep.Leaf')] = []StructField{}
	mut seen := map[u32]bool{}
	assert !tc.main_type_name_has_non_main_collision_inner(first_id, mut seen)
	assert seen[first_id]
	assert seen[second_id]
	assert seen.len == 2

	// A repeated alias must not prevent a later wrapped target from colliding.
	tc.type_aliases.set_id(second_id, flat.source_type_id('map[First]Leaf'))
	assert tc.main_type_name_has_non_main_collision(first_id)
	assert tc.qualify_type_name_at(first_id, flat.empty_node, 'main') == flat.source_type_id('main.First')
	tc.struct_modules[leaf_id] = 'dep'
	assert !tc.main_type_name_has_non_main_collision(first_id)
}

fn test_main_type_collision_ids_preserve_qualifier_exclusions() {
	name_id := flat.source_type_id('Item')
	assert type_name_collides_with_main(flat.source_type_id('dep.Item'), name_id)
	for candidate in ['Item', 'main.Item', 'builtin.Item', 'dep.Other'] {
		assert !type_name_collides_with_main(flat.source_type_id(candidate), name_id)
	}
}

fn test_short_type_name_ids_match_cached_and_scan_lookup_without_normalization() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_aliases.set_id(flat.source_type_id('alpha.Alias'), flat.source_type_int)
	tc.structs[flat.source_type_id('alpha.Item')] = []StructField{}
	tc.interface_names[flat.source_type_id('alpha.Contract')] = true
	tc.enum_names[flat.source_type_id('alpha.Kind')] = true
	tc.sum_types.set_ids(flat.source_type_id('alpha.Choice'), [flat.source_type_int])
	for name in ['Alias', 'Item', 'Contract', 'Kind', 'Choice'] {
		name_id := flat.source_type_id(name)
		expected_id := flat.source_type_id('alpha.${name}')
		assert tc.unique_qualified_type_name(name_id)? == expected_id
		assert tc.unique_qualified_type_name_scan(name_id)? == expected_id
	}
	for name_id in [u32(0), flat.source_type_id('Missing'), flat.source_type_id(' Item '),
		flat.source_type_id('alpha.Item')] {
		assert tc.unique_qualified_type_name(name_id) == none
		assert tc.unique_qualified_type_name_scan(name_id) == none
	}
	tc.structs[flat.source_type_id('beta.Item')] = []StructField{}
	tc.invalidate_short_type_name_index()
	assert tc.unique_qualified_type_name(flat.source_type_id('Item')) == none
	assert tc.unique_qualified_type_name_scan(flat.source_type_id('Item')) == none
}

fn test_short_type_name_id_registration_detaches_base_and_preserves_ambiguous_entries() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	first_id := flat.source_type_id('alpha.Item')
	second_id := flat.source_type_id('beta.Item')
	short_id := flat.source_type_id('Item')
	tc.structs[first_id] = []StructField{}
	assert tc.unique_qualified_type_name(short_id)? == first_id
	base := tc.type_cache
	tc.type_cache = new_type_cache_with_base(false, base)
	tc.register_short_type_name(second_id)
	assert base.short_type_name_index.type_id(short_id)? == first_id
	assert tc.type_cache.short_type_name_index.type_id(short_id)? == 0
	tc.unregister_short_type_name(second_id)
	assert tc.type_cache.short_type_name_index.type_id(short_id)? == 0
	assert tc.unique_qualified_type_name(short_id) == none

	unique_id := flat.source_type_id('alpha.Unique')
	unique_short_id := flat.source_type_id('Unique')
	tc.register_short_type_name(unique_id)
	tc.unregister_short_type_name(flat.source_type_id('beta.Unique'))
	assert tc.unique_qualified_type_name(unique_short_id)? == unique_id
	tc.unregister_short_type_name(unique_id)
	assert tc.unique_qualified_type_name(unique_short_id) == none
	assert !base.short_type_name_index.contains(unique_short_id)
}
