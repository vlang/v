type SumType = int | string

fn s2s(s SumType) SumType {
	return s
}

fn test_match_expression_on_sumtype_ordinary_branch() {
	// tests whether an ordinary branch supports multiple statements,
	// followed by a default expression
	mut c := 0
	s := s2s('abc')
	res := match s {
		string {
			c = 1
			eprintln('hi')
			'a string'
		}
		else {
			'unknown'
		}
	}
	assert res == 'a string'
	assert c == 1
}

fn test_match_expression_on_sumtype_else() {
	// tests whether else branches support multiple statements,
	// when the other branches are simple default expressions
	mut c := 0
	s := s2s(int(123))
	res := match s {
		string {
			'a string'
		}
		else {
			c = 3
			eprintln('hi')
			'unknown'
		}
	}
	assert res == 'unknown'
	assert c == 3
}

fn test_match_expression_on_sumtype_full() {
	// tests whether all branches can have multiple statements,
	// followed by a default expression
	mut c := 0
	s := s2s(int(123))
	res := match s {
		int {
			c = 1
			eprintln('hi')
			'an integer'
		}
		string {
			c = 2
			eprintln('hi')
			'a string'
		}
	}
	assert res == 'an integer'
	assert c == 1
}
