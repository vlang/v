struct Tester {
	b bool
	i int
}

enum Color {
	red green blue
}

fn test_array_equality() {
	strs := ['a', 'b', 'c']
	assert strs == ['a', 'b', 'c']
	assert strs != ['a', 'c', 'b']
	assert strs != ['b', 'c', 'a']
	assert strs != ['b', 'a', 'c']
	assert strs != ['c', 'b', 'a']
	assert strs != ['c', 'a', 'b']
	bools := [true, true, false]
	assert bools == [true, true, false]
	assert bools != [true, false, false]
	assert bools != [false, true, true]
	assert bools != [false, false, true]
	assert bools != [false, false, false]
	assert bools != [false, true, false]
	ints := [1, 2, 3]
	assert ints == [1, 2, 3]
	assert ints != [1, 3, 2]
	assert ints != [2, 3, 1]
	assert ints != [2, 1, 3]
	assert ints != [3, 2, 1]
	assert ints != [3, 1, 2]
	a := Tester{true, 100}
	b := Tester{false, 200}
	testers := [a, b]
	assert testers == [a, b]
	assert testers != [a, a]
	assert testers != [b, b]
	assert testers != [b, a]
	colors := [Color.red, Color.green, Color.blue]
	assert colors == [Color.red, Color.green, Color.blue]
	assert colors != [Color.red, Color.blue, Color.green]
	assert colors != [Color.green, Color.blue, Color.red]
	assert colors != [Color.green, Color.red, Color.blue]
	assert colors != [Color.blue, Color.green, Color.red]
	assert colors != [Color.blue, Color.red, Color.green]
}
