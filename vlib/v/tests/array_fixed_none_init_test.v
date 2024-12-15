fn test_none_init() {
	a := ?[3]u8(none)
	assert a == none

	b := ?[3]u8{}
	assert b == none

	c := ?[3]u8{}
	assert c == none
}

fn foo() u8 {
	return 123
}

fn test_non_none_init() {
	c := ?[3]u8{init: 2}
	assert c? == [u8(2), 2, 2]!
	assert c != none

	d := ?[3]u8{init: foo()}
	assert d? == [u8(123), 123, 123]!
}
