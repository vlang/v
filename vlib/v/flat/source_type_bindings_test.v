module flat

fn test_source_type_bindings_keep_empty_keys_values_and_lookup_order() {
	first := source_type_id('numeric_registry.First')
	second := source_type_id('numeric_registry.Second')
	mut bindings := SourceTypeBindings{}
	bindings.set_id(0, 0)
	bindings.set_id(first, second)
	bindings.set_id(second, 0)
	assert bindings.contains(0)
	assert bindings.type_id(0) or { panic('missing empty key') } == 0
	assert bindings.type_id(second) or { panic('missing empty value') } == 0
	assert bindings.get(second) or { panic('missing empty spelling') } == ''
	assert bindings.keys() == [u32(0), first, second]
	assert bindings.value_id(first) == second
	absent := source_type_id('numeric_registry.Absent')
	assert !bindings.contains(absent)
	if _ := bindings.type_id(absent) {
		assert false
	}
	mut owned := bindings.clone_owned()
	owned.delete(first)
	owned.set_id(second, first)
	assert bindings.value_id(first) == second
	assert bindings.value_id(second) == 0
	assert !owned.contains(first)
	assert owned.value_id(second) == first
}

fn test_source_type_list_bindings_own_lists_across_input_and_fork_mutation() {
	owner := source_type_id('numeric_registry.Owner')
	first := source_type_id('numeric_registry.Element')
	mut input := [first, u32(0), first]
	mut parent := SourceTypeListBindings{}
	parent.set_ids(owner, input)
	parent.set_ids(0, [])
	input[0] = 0
	assert parent.type_ids(owner) or { panic('missing list') } == [first, u32(0), first]
	assert parent.contains(0)
	assert parent.type_ids(0) or { panic('missing empty list') } == []u32{}
	mut owned := parent.clone_owned()
	owned.ids[owner][0] = 0
	assert (parent.type_ids(owner) or { panic('missing parent') })[0] == first
	assert (owned.type_ids(owner) or { panic('missing owned') })[0] == 0
	owned.append_ids(owner, [u32(0)])
	owned.delete(0)
	assert (parent.type_ids(owner) or { panic('missing parent') }).len == 3
	assert (owned.type_ids(owner) or { panic('missing owned') }).len == 4
	assert parent.contains(0)
	assert !owned.contains(0)
	assert parent.keys() == [owner, u32(0)]
}
