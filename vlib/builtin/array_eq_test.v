struct User {
	age  int
	name string
}

fn test_eq() {
	assert [5, 6, 7] != [6, 7]
	assert [`a`, `b`] == [`a`, `b`]
	assert [User{age: 22, name: 'bob'}] == [User{age: 22, name: 'bob'}]
	assert [{'bob': 22}, {'tom': 33}] == [{'bob': 22}, {'tom': 33}]
	assert [[1, 2, 3], [4]] == [[1, 2, 3], [4]]
}

fn test_fixed_array_eq() {
	a1 := [1, 2, 3]!
	assert a1 == [1, 2, 3]!
	assert a1 != [2, 3, 4]!

	a2 := [[1, 2]!, [3, 4]!]!
	assert a2 == [[1, 2]!, [3, 4]!]!
	assert a2 != [[3, 4]!, [1, 2]!]!

	a3 := [[1, 2], [3, 4]]!
	assert a3 == [[1, 2], [3, 4]]!
	assert a3 != [[1, 1], [2, 2]]!

	a4 := [[`a`, `b`], [`c`, `d`]]!
	assert a4 == [[`a`, `b`], [`c`, `d`]]!
	assert a4 != [[`c`, `a`], [`a`, `b`]]!

	a5 := [['aaa', 'bbb'], ['ccc', 'ddd']]!
	assert a5 == [['aaa', 'bbb'], ['ccc', 'ddd']]!
	assert a5 != [['abc', 'def'], ['ccc', 'ddd']]!

	a6 := [['aaa', 'bbb']!, ['ccc', 'ddd']!]!
	assert a6 == [['aaa', 'bbb']!, ['ccc', 'ddd']!]!
	assert a6 != [['aaa', 'bbb']!, ['aaa', 'ddd']!]!

	a7 := [[1, 2]!, [3, 4]!]
	assert a7 == [[1, 2]!, [3, 4]!]
	assert a7 != [[2, 3]!, [1, 2]!]

	a8 := [['aaa', 'bbb']!, ['ccc', 'ddd']!]
	assert a8 == [['aaa', 'bbb']!, ['ccc', 'ddd']!]
	assert a8 != [['bbb', 'aaa']!, ['cccc', 'dddd']!]
}

fn test_fixed_array_literal_eq() {
	assert [1, 2, 3]! == [1, 2, 3]!
	assert [1, 1, 1]! != [1, 2, 3]!

	assert [[1, 2], [3, 4]]! == [[1, 2], [3, 4]]!
	assert [[1, 1], [2, 2]]! != [[1, 2], [3, 4]]!

	assert [[1, 1]!, [2, 2]!]! == [[1, 1]!, [2, 2]!]!
	assert [[1, 1]!, [2, 2]!]! != [[1, 2]!, [2, 3]!]!

	assert [[1, 1]!, [2, 2]!] == [[1, 1]!, [2, 2]!]
	assert [[1, 1]!, [2, 2]!] != [[1, 2]!, [2, 3]!]
}
