enum Color1 {
	unknown
	red
	green
	blue
}

enum Color2 as i64 {
	unknown
	red
	blue = 123456789012345
	green
}

fn test_enum_static_from_string() {
	color11 := Color1.from_string('red')?
	println(color11)
	assert color11 == Color1.red

	color12 := Color1.from_string('blue')?
	println(color12)
	assert color12 == Color1.blue

	color13 := Color1.from_string('aaaaa') or { Color1.unknown }
	println(color13)
	assert color13 == Color1.unknown

	color21 := Color2.from_string('red')?
	println(color21)
	assert color21 == Color2.red

	color22 := Color2.from_string('green')?
	println(color22)
	assert color22 == Color2.green

	color23 := Color2.from_string('bbbbb') or { Color2.unknown }
	println(color23)
	assert color23 == Color2.unknown
}
