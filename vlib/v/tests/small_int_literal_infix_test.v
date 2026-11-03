// An untyped integer literal next to a smaller integer takes that integer's type, so
// `i8(127) + 1` is an `i8` that wraps. The value and the type must not depend on whether
// the expression is bound to a variable first.

fn test_i8_plus_literal() {
	x := i8(127)
	assert x + 1 == -128
	assert typeof(x + 1).name == 'i8'
	y := x + 1
	assert y == -128
	assert typeof(y).name == 'i8'
	assert '${x + 1}' == '-128'
	assert '${y}' == '-128'
}

fn test_i16_plus_literal() {
	x := i16(32767)
	assert x + 1 == -32768
	assert typeof(x + 1).name == 'i16'
	y := x + 1
	assert y == -32768
	assert typeof(y).name == 'i16'
	assert '${x + 1}' == '-32768'
}

fn test_u8_plus_literal() {
	x := u8(255)
	assert x + 1 == 0
	assert typeof(x + 1).name == 'u8'
	y := x + 1
	assert y == 0
	assert typeof(y).name == 'u8'
	assert '${x + 1}' == '0'
}

fn test_u16_plus_literal() {
	x := u16(65535)
	assert x + 1 == 0
	assert typeof(x + 1).name == 'u16'
	y := x + 1
	assert y == 0
	assert typeof(y).name == 'u16'
	assert '${x + 1}' == '0'
}

fn test_literal_on_the_left_and_other_operators() {
	x := i8(127)
	assert 1 + x == -128
	assert typeof(1 + x).name == 'i8'
	assert x * 2 == -2
	assert i8(-128) - 1 == 127
	assert typeof(i8(-128) - 1).name == 'i8'
	assert u8(0) - 1 == 255
	assert typeof(u8(0) - 1).name == 'u8'
}

fn test_literal_that_does_not_fit_widens() {
	x := i8(100)
	assert x + 1000 == 1100
	assert typeof(x + 1000).name == 'int'
}
