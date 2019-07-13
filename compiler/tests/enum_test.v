enum Color {
	red
	blue
	green
}

fn test_enum() {
	assert Color.red == Color.red
	assert Color.blue == Color.blue
	assert Color.green == Color.green

	assert Color.red == .red
	assert Color.blue == .blue
	assert Color.green == .green

	assert Color.red != Color.blue
	assert Color.red != Color.green
	assert Color.blue != Color.green

	mut color := Color.red
	assert color == Color.red
	color = .green
	assert color == Color.green
}
