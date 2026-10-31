import singleletter

fn test_single_letter_struct_result_return() {
	value := singleletter.result(false)!
	assert value.x == 7
	fallback := singleletter.result(true) or { singleletter.M{ x: 9 } }
	assert fallback.x == 9
}

fn test_single_letter_struct_optional_return() {
	value := singleletter.optional(true) or { panic('expected a value') }
	assert value.x == 8
	fallback := singleletter.optional(false) or { singleletter.M{ x: 10 } }
	assert fallback.x == 10
}
