import datatypes

fn test_datatypes_set_elements_is_public() {
	mut set := datatypes.Set[string]{}
	set.add_all(['a', 'b', 'c'])
	assert set.elements.len == 3
}
