enum Color {
	red
	green
	blue
}

fn Color.from_string(x string) Color {
	return Color.red
}

fn test_enum_custom_static_from_string() {
	ret := Color.from_string('abc')
	assert ret == Color.red
}
