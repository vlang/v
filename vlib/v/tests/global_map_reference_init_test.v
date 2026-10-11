@[has_globals]
module main

__global empty_map_reference = &map[string]bool{}
__global populated_map_reference = &{
	'k0': true
	'k1': false
}

__global (
	block_empty_map_reference     = &map[int]string{}
	block_populated_map_reference = &map[int]string{
		1: 'one'
		2: 'two'
	}
)

fn test_global_map_references_are_initialized() {
	assert empty_map_reference != unsafe { nil }
	assert populated_map_reference != unsafe { nil }
	assert empty_map_reference.len == 0
	assert populated_map_reference.len == 2
	assert (*populated_map_reference)['k0']
	assert !(*populated_map_reference)['k1']
	assert 'k1' in populated_map_reference
	// Explicitly dereference the map pointers to mutate the initialized maps.
	unsafe {
		(*empty_map_reference)['added'] = true
		(*populated_map_reference)['k1'] = true
	}
	assert (*empty_map_reference)['added']
	assert (*populated_map_reference)['k1']
}

fn test_global_block_map_references_are_initialized() {
	assert block_empty_map_reference != unsafe { nil }
	assert block_populated_map_reference != unsafe { nil }
	assert block_empty_map_reference.len == 0
	assert block_populated_map_reference.len == 2
	assert (*block_populated_map_reference)[1] == 'one'
	assert (*block_populated_map_reference)[2] == 'two'
	// Explicitly dereference the map pointers to mutate the initialized maps.
	unsafe {
		(*block_empty_map_reference)[3] = 'three'
		(*block_populated_map_reference)[2] = 'updated'
	}
	assert (*block_empty_map_reference)[3] == 'three'
	assert (*block_populated_map_reference)[2] == 'updated'
}
