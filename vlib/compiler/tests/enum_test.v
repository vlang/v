enum Color {
	red
	blue
	green
}

fn test_enum() {
	assert Color.red == .red
	assert Color.blue == .blue
	assert Color.green == .green

	assert Color.red != .blue
	assert Color.red != .green
	assert Color.blue != .green

	mut color := Color.red
	assert color == .red
	color = .green
	assert color == .green
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
