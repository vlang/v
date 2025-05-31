type Any = ?int | string | ?string

fn test_main() {
	b := ?Any(?string('bar'))
	assert b?.str() == "Any(Option('bar'))"

	c := ?Any(string('baz'))
	assert c?.str() == "Any('baz')"

	d := ?Any('baz')
	assert d?.str() == "Any('baz')"
}
