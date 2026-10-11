interface Named {
	name() string
}

struct Country {
	code string
}

fn (c Country) name() string { return c.code }

fn inserted(codes []Country) []Named {
	mut values := []Named{}
	values.insert(0, ...codes)
	return values
}

fn prepended(codes []Country) []Named {
	mut values := []Named{}
	values.prepend(...codes)
	return values
}

fn consume(values []Named) {
	assert values.len == 2
	assert values[0] is Country
	assert values[1] is Country
	assert values[0].name() == 'IT'
	assert values[1].name() == 'US'
	assert '${values}' == "[Named(Country{
    code: 'IT'
}), Named(Country{
    code: 'US'
})]"
}

fn test_insert() { consume(inserted([Country{'IT'}, Country{'US'}])) }

fn test_prepend() { consume(prepended([Country{'IT'}, Country{'US'}])) }
