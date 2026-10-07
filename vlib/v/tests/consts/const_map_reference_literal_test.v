module main

const const_bool_map_reference = &{
	'k0': true
	'k1': false
}

const const_int_map_reference = &{
	'first':  17
	'second': 29
}

const const_empty_map_reference = &map[string]int{}

fn read_const_map_reference() int {
	mut scratch := [0].repeat(4096)
	scratch[0] = 11
	assert scratch[0] == 11
	return (*const_int_map_reference)['second']
}

fn test_constant_map_literal_reference_keeps_its_entries() {
	assert const_empty_map_reference.len == 0
	assert const_bool_map_reference.len == 2
	assert (*const_bool_map_reference)['k0']
	assert !(*const_bool_map_reference)['k1']
	assert const_int_map_reference.len == 2
	assert (*const_int_map_reference)['first'] == 17
	assert read_const_map_reference() == 29
}

fn test_local_map_literal_reference_control() {
	local := &{
		'answer': 42
	}
	assert local.len == 1
	assert (*local)['answer'] == 42
}
