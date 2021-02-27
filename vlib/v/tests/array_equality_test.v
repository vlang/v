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

fn test_nested_array_equality() {
	a1 := [[1]]
	assert a1 == [[1]]
	assert a1 != [[2]]
	a2 := [[[[1]]]]
	assert a2 == [[[[1]]]]
	assert a2 != [[[[2]]]]
	a3 := [[[1,2,3]]]
	assert a3 == [[[1,2,3]]]
	assert a3 != [[[1,0,3]]]
	a4 := [[1.1], [2.2]]
	assert a4 == [[1.1], [2.2]]
	assert a4 != [[2.1], [3.2]]
	a5 := [[[[1,2], [2,3], [3,4]]]]
	assert a5 == [[[[1,2], [2,3], [3,4]]]]
	assert a5 != [[[[2,2], [2,4], [3,4]]]]
	a6 := [[['aa', 'bb'], ['cc', 'dd']]]
	assert a6 == [[['aa', 'bb'], ['cc', 'dd']]]
	assert a6 != [[['a', 'b'], ['cc', 'dd']]]
	a7 := [[[true], [false]]]
	assert a7 == [[[true], [false]]]
	assert a7 != [[[false], [true]]]
	a8 := [[[[`a`, `b`], [`c`, `d`]]]]
	assert a8 == [[[[`a`, `b`], [`c`, `d`]]]]
	assert a8 != [[[[`c`, `a`], [`e`, `d`]]]]
	a9 := [[[u16(22), 11]]]
	assert a9 == [[[u16(22), 11]]]
	assert a9 != [[[u16(20), 10]]]
}

type Literal = string
type Literals = []Literal

fn (l1 Literal) concat(l2 Literal) Literals {
	return Literals([l1, l2])
}

fn test_array_of_alias_equality() {
	mut literals := Literals([]Literal{})
	literals = Literal('hello').concat(Literal('World'))
	println(literals)
	assert literals == Literals([Literal('hello'), Literal('World')])
}
