module main

fn test_cast_to_anon_sumtype() {
	x := string|none(none)
	println(x)
	assert '${x}' == 'string|none(none)'
}
