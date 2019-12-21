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
	println(color)
	assert true
}

fn test_match() {
	color := Color.green
	num := 3
	match color {
		.red { assert false }
		.green { assert true }
		else { assert false }
	}
	println(color)
	assert num == 3
}

enum Foo {
	a = 1
	b = 2
	c = 3
	d = -10
}

fn test_nums() {
	foo := Foo.a
	assert foo == 1
	assert Foo.c == 3
	d := Foo.d
	assert d == -10
}
