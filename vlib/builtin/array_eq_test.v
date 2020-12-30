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
