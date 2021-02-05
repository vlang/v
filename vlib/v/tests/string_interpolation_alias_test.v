type Literal = string

fn test_array_alias_string() {
	a := [Literal('aaa'), Literal('bbb')]
	assert '$a' == "['aaa', 'bbb']"
}

fn test_fixed_array_alias_string() {
	a := [Literal('aaa'), Literal('bbb')]!
	assert '$a' == "['aaa', 'bbb']"
}

fn test_map_alias_string() {
	m := {'one': Literal('1'), 'two': Literal('2')}
	assert '$m'.contains("'one': '1'")
	assert '$m'.contains("'two': '2'")
}
