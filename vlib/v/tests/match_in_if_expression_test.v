fn test_match_in_if_expression() {
	num := 3
	str := 'a'
	res := if num in [1, 3] {
		match str {
			'a' { 'A' }
			else { 'ODD' }
		}
	} else {
		'NONE'
	}
	assert res == 'A'
}
