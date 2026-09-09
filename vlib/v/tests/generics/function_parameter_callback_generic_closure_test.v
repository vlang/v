struct ParseResult[T] {
	result T
	rest   string
}

type ParseFunction[T] = fn (string) !ParseResult[T]

fn literal(l string) ParseFunction[string] {
	return fn [l] (input string) !ParseResult[string] {
		if !input.starts_with(l) {
			return error(input)
		}
		return ParseResult[string]{
			result: l
			rest:   input.all_after_first(l)
		}
	}
}

fn wrap_parser[T](parser ParseFunction[T]) ParseFunction[T] {
	return fn [parser] [T](input string) !ParseResult[T] {
		return parser[T](input)
	}
}

fn test_function_parameter_inside_generic_closure() {
	parser := literal('ab')
	wrapped := wrap_parser[string](parser)
	result := wrapped('abcd')!
	assert result.result == 'ab'
	assert result.rest == 'cd'
}
