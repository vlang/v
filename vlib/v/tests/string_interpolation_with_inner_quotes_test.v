fn f(x int, s string) string {
	return 'label ${s}: ${x}'
}

// vfmt off
fn test_string_interp_with_inner_quotes() {
	x := 'hi'
	println('abc ${f(123, 'def')} xyz')
	assert 'abc ${f(123, 'def')} xyz' == 'abc label def: 123 xyz'

	println('abc ${f(123, "def")} xyz')
	assert 'abc ${f(123, "def")} xyz' == 'abc label def: 123 xyz'

	println("abc ${f(123, 'def')} xyz")
	assert "abc ${f(123, 'def')} xyz" == 'abc label def: 123 xyz'

	println("abc ${f(123, "def")} xyz")
	assert "abc ${f(123, "def")} xyz" == 'abc label def: 123 xyz'
	
	println("abc ${f(123, "$x $x")} xyz")
	assert "abc ${f(123, "$x $x")} xyz" == 'abc label hi hi: 123 xyz'

	println('abc ${f(123, '$x $x')} xyz')
	assert 'abc ${f(123, '$x $x')} xyz' == 'abc label hi hi: 123 xyz'

	println('abc ${f(123, "$x $x")} xyz')
	assert 'abc ${f(123, "$x $x")} xyz' == 'abc label hi hi: 123 xyz'

	println("abc ${f(123, '$x $x')} xyz")
	assert "abc ${f(123, '$x $x')} xyz" == 'abc label hi hi: 123 xyz'

	println("abc ${f(123, "${x} ${x}")} xyz")
	assert "abc ${f(123, "${x} ${x}")} xyz" == 'abc label hi hi: 123 xyz'

	println("abc ${f(123, '${x} ${x}')} xyz")
	assert "abc ${f(123, '${x} ${x}')} xyz" == 'abc label hi hi: 123 xyz'

	println('abc ${f(123, '${x} ${x}')} xyz')
	assert 'abc ${f(123, '${x} ${x}')} xyz' == 'abc label hi hi: 123 xyz'

	println('abc ${f(123, "${x} ${x}")} xyz')
	assert 'abc ${f(123, "${x} ${x}")} xyz' == 'abc label hi hi: 123 xyz'
}
// vfmt on
