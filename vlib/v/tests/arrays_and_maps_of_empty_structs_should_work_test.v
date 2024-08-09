struct EmptyStruct {}

fn test_array_append_empty_struct() {
	size_of_empty_struct := dump(sizeof(EmptyStruct))
	assert size_of_empty_struct <= 1
	mut names := []EmptyStruct{cap: 2}
	names << EmptyStruct{}
	dump(names)
	assert (EmptyStruct{} in names) == true

	mut fa := [EmptyStruct{}, EmptyStruct{}]!
	assert fa.len == 2
	dump(fa[0])
	fa[0] = EmptyStruct{}
	dump(fa[0])
	assert fa[0] == EmptyStruct{}
	assert fa[1] == EmptyStruct{}
}

fn test_map_of_int_set_empty_struct() {
	mut names := map[int]EmptyStruct{}
	names[10] = EmptyStruct{}
	names[99] = EmptyStruct{}
	dump(names)
	assert 10 in names
	assert 99 in names
	assert names[10] == EmptyStruct{}
	assert names[99] == EmptyStruct{}
}

fn test_map_of_string_set_empty_struct() {
	mut names := map[string]EmptyStruct{}
	names['ab'] = EmptyStruct{}
	names['cd'] = EmptyStruct{}
	dump(names)
	assert 'ab' in names
	assert 'cd' in names
	assert names['ab'] == EmptyStruct{}
	assert names['cd'] == EmptyStruct{}
}
