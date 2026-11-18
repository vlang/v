module mail

fn test_parse_address() {
	a := parse_address('John Doe <john@example.com>')!
	assert a.name == 'John Doe'
	assert a.address == 'john@example.com'
	b := parse_address('jane@example.com')!
	assert b.address == 'jane@example.com'
}

fn test_parse_address_quoted_gt() {
	a := parse_address('"A > B" <a@example.com>')!
	assert a.name == 'A > B'
	assert a.address == 'a@example.com'
}

fn test_address_string_escapes_quotes() {
	a := Address{ name: 'Jane "JJ" Doe', address: 'jane@example.com' }
	assert a.string() == '"Jane \\"JJ\\" Doe" <jane@example.com>'
	b := parse_address(a.string())!
	assert b.name == 'Jane "JJ" Doe'
	assert b.address == 'jane@example.com'
}
