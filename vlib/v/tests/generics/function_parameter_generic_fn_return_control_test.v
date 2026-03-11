struct ControlParseResult[T] {
	result T
	rest   string
}

type ControlParseFunction[T] = fn (string) !ControlParseResult[T]

fn control_literal(l string) ControlParseFunction[string] {
	return fn [l] (input string) !ControlParseResult[string] {
		if !input.starts_with(l) {
			return error(input)
		}
		return ControlParseResult[string]{
			result: l
			rest:   input.all_after_first(l)
		}
	}
}

fn pass_through[O](parser ControlParseFunction[O]) ControlParseFunction[O] {
	return parser
}

fn test_generic_function_returning_generic_function_parameter() {
	parser := control_literal('ab')
	mapped := pass_through[string](parser)
	result := mapped('abcd')!
	assert result.result == 'ab'
	assert result.rest == 'cd'
}
