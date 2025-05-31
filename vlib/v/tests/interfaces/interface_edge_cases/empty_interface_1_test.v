interface IEmpty {}

struct Abc {
	x string
}

fn test_empty_interface_string_interpolation() {
	a := IEmpty(u64(1))
	b := IEmpty(f32(1))
	c := IEmpty(Abc{'abc'})
	assert '${a}' == 'IEmpty(1)'
	assert '${b}' == 'IEmpty(1.0)'
	assert '${c}'.starts_with('IEmpty(Abc{')
	assert '${c}'.contains("x: 'abc'")
	assert '${c}'.ends_with('})')
}
