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

fn test_in() {
	color := Color.red
	num := 3 // used to be an expr bug before `in`
	assert color in [.red, .green]
	assert num == 3
}	

fn test_match() {
	color := Color.red
	num := 3
	match color {
		.red { assert true }
		.green { assert false }
		else { assert false }
	}	
	assert num == 3
}	
