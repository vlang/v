struct ParseResult[T] {
	result T
	rest   string
}

type ParseFunction[T] = fn (string) !ParseResult[T]

fn (f ParseFunction[T]) parse[T](input string) !ParseResult[T] {
	return f(input)
}

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

fn test_generic_fn_type_generic_method() {
	l_func := literal('start')
	val1 := l_func.parse('start test') or { ParseResult[string]{} }
	val2 := l_func.parse('test') or { ParseResult[string]{} }
	assert val1.result == 'start'
	assert val1.rest == ' test'
	assert val2.result == ''
	assert val2.rest == ''
}
