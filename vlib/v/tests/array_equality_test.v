struct Tester {
	b bool
}

enum Color {
	red green blue
}

fn test_array_equality() {
	strlist := ["a", "b", "c"]
	assert strlist == ["a", "b", "c"]
	assert strlist != ["a", "c", "b"]
	assert strlist != ["b", "c", "a"]
	assert strlist != ["b", "a", "c"]
	assert strlist != ["c", "b", "a"]
	assert strlist != ["c", "a", "b"]
	boollist := [true, true, false]
	assert boollist == [true, true, false]
	assert boollist != [true, false, false]
	assert boollist != [false, true, true]
	assert boollist != [false, false, true]
	assert boollist != [false, false, false]
	assert boollist != [false, true, false]
	intlist := [1, 2, 3]
	assert intlist == [1, 2, 3]
	assert intlist != [1, 3, 2]
	assert intlist != [2, 3, 1]
	assert intlist != [2, 1, 3]
	assert intlist != [3, 2, 1]
	assert intlist != [3, 1, 2]
	t := Tester{true}
	f := Tester{false}
	testerlist := [t, f]
	assert testerlist == [t, f]
	assert testerlist != [t, t]
	assert testerlist != [f, f]
	assert testerlist != [f, t]
	colorlist := [Color.red, Color.green, Color.blue]
	assert colorlist == [Color.red, Color.green, Color.blue]
	assert colorlist != [Color.red, Color.blue, Color.green]
	assert colorlist != [Color.green, Color.blue, Color.red]
	assert colorlist != [Color.green, Color.red, Color.blue]
	assert colorlist != [Color.blue, Color.green, Color.red]
	assert colorlist != [Color.blue, Color.red, Color.green]
}
