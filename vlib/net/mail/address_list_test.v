module mail

fn test_parse_address_list() {
	list := parse_address_list('a@b.com, c@d.com')!
	assert list.len == 2
}
