// vtest vflags: -w

fn match() int {
	return 7
}

fn parenthesized_match_expr(value int) int {
	return match value {
		1 { 11 }
		else { 22 }
	}
}

fn test_parenthesized_match_header() {
	mut result := 0
	match (2) {
		1 { result = 11 }
		2 { result = 22 }
		else { result = 33 }
	}
	assert result == 22
	assert parenthesized_match_expr(1) == 11
	assert parenthesized_match_expr(2) == 22
}

fn test_keyword_named_function_call_remains_unambiguous() {
	assert match() == 7
}
