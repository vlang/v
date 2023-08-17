enum Color1 {
	red
	green
	blue
}

enum Color2 {
	red
	blue = 11
	green
}

fn test_enum_static_from_string() {
	color1 := Color1.from_string('blue')?
	println(color1)
	assert color1 == Color1.blue

	color2 := Color2.from_string('green')?
	println(color2)
	assert color2 == Color2.green
}
