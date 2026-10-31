module flat

fn test_type_list_keeps_repeated_and_empty_spellings() {
	values := TypeTextList.from_names(['pkg.T', '', 'pkg.T'])
	copy := values.clone()
	assert values.ids[0] == values.ids[2]
	assert values.ids[1] == 0
	assert copy.names() == ['pkg.T', '', 'pkg.T']
	assert copy.ids.data != values.ids.data
}
