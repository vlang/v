@[_allow_multiple_values]
enum Kind {
	placeholder
	i32
	int = 9
	i64 = 9
	isize
	u8
}

fn classify_with_aliases(x Kind) string {
	return match x {
		.placeholder { 'placeholder' }
		.i32 { 'i32' }
		.int { 'int' }
		.i64 { 'i64' }
		.isize { 'isize' }
		.u8 { 'u8' }
	}
}

fn classify_without_alias(x Kind) string {
	return match x {
		.placeholder { 'placeholder' }
		.i32 { 'i32' }
		.i64 { 'nine' }
		.isize { 'isize' }
		.u8 { 'u8' }
	}
}

fn test_match_with_allow_multiple_values_enum() {
	assert classify_with_aliases(.placeholder) == 'placeholder'
	assert classify_with_aliases(.int) == 'int'
	assert classify_with_aliases(.i64) == 'int'
	assert classify_without_alias(.int) == 'nine'
	assert classify_without_alias(.i64) == 'nine'
}
