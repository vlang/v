// An arithmetic operation between an f32 and an untyped float constant is an f32, also when
// the constant is signed, parenthesized or literal arithmetic. `typeof` and string
// interpolation used to treat `c + -1.2` as an f64 and print `-2.4000000953674316`.

const scale = 1.2

fn test_typeof_names_f32() {
	c := f32(-1.2)
	assert typeof(c + -1.2).name == 'f32'
	assert typeof(-1.2 + c).name == 'f32'
	assert typeof(c * (2.0)).name == 'f32'
	assert typeof(c + 1.2 * 2).name == 'f32'
	assert typeof(c * -scale).name == 'f32'
	assert typeof(c * 2.0).name == 'f32'
}

fn test_interpolation_formats_f32() {
	c := f32(-1.2)
	assert '${c + -1.2}' == '-2.4'
	assert '${c * -2.0}' == '2.4'
	assert '${-2.0 * c}' == '2.4'
	assert '${c * (2.0)}' == '-2.4'
	assert '${c + 1.2 * 2}' == '1.2'
	assert '${c * -scale}' == '1.44'
}
