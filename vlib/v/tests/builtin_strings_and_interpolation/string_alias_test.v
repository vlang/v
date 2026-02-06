type StrAlias = string

fn test_alias_left_eq() {
	assert StrAlias('test') == 'test'
}

fn test_alias_right_eq() {
	assert 'test' == StrAlias('test')
}

fn test_alias_left_ne() {
	assert StrAlias('test') != 'test2'
}

fn test_alias_right_ne() {
	assert 'test' != StrAlias('test2')
}
