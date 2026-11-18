module mail

fn test_parse_address_list() {
	list := parse_address_list('a@b.com, c@d.com')!
	assert list.len == 2
}

fn test_parse_address_list_quoted_comma() {
	list := parse_address_list('"Doe, John" <john@example.com>, jane@example.com')!
	assert list.len == 2
	assert list[0].name == 'Doe, John'
	assert list[0].address == 'john@example.com'
	assert list[1].address == 'jane@example.com'
}

fn test_parse_address_list_quoted_local_part() {
	list := parse_address_list('"john,doe"@example.com, jane@example.com')!
	assert list.len == 2
	assert list[0].address == '"john,doe"@example.com'
	assert list[1].address == 'jane@example.com'
}
