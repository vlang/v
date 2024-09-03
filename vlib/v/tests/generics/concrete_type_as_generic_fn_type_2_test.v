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

fn test_concrete_function_type_as_generic_type() {
	l_func := literal('start')
	val1 := l_func('start test') or { ParseResult[string]{} }
	val2 := l_func('test') or { ParseResult[string]{} }

	println(val1)
	assert val1.result == 'start'
	assert val1.rest == ' test'

	println(val2)
	assert val2.result == ''
	assert val2.rest == ''
}
