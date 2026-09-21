module types

fn test_array_receiver_candidates_keep_element_module_identity() {
	value := Type(SumType{
		name: 'one.Any'
	})
	names := exact_array_receiver_method_candidates(Array{
		elem_type: value
	}, 'str', 'two')
	assert '[]one.Any.str' in names
	assert 'one.[]Any.str' in names
	assert 'two.[]one.Any.str' in names
	assert '[]Any.str' !in names
	assert 'two.[]Any.str' !in names
}

fn test_map_receiver_candidates_shorten_only_the_declaring_modules_types() {
	key := Type(Struct{
		name: 'one.Key'
	})
	value := Type(SumType{
		name: 'two.Any'
	})
	mut names := []string{}
	append_map_receiver_method_candidates(mut names, Map{
		key_type:   key
		value_type: value
	}, 'str', 'three')
	assert 'map[one.Key]two.Any.str' in names
	assert 'one.map[Key]two.Any.str' in names
	assert 'two.map[one.Key]Any.str' in names
	assert 'three.map[one.Key]two.Any.str' in names
	assert 'map[Key]Any.str' !in names
	assert 'one.map[Key]Any.str' !in names
	assert 'two.map[Key]Any.str' !in names
	assert 'three.map[Key]Any.str' !in names
}
