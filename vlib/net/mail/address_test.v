module mail

fn test_parse_address() {
	a := parse_address('John Doe <john@example.com>')!
	assert a.name == 'John Doe'
	assert a.address == 'john@example.com'
	b := parse_address('jane@example.com')!
	assert b.address == 'jane@example.com'
}
