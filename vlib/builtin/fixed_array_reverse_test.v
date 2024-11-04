struct User {
	age  int
	name string
}

fn test_fixed_array_reverse_in_place() {
	mut a := ['hi', '1', '5', '3']!
	a.reverse_in_place()
	assert a == ['3', '5', '1', 'hi']!

	mut nums := [67, -3, 108, 42, 7]!
	nums.reverse_in_place()
	assert nums == [7, 42, 108, -3, 67]!

	mut users := [User{22, 'Peter'}, User{20, 'Bob'}, User{25, 'Alice'}]!
	users.reverse_in_place()
	assert users[0].age == 25
	assert users[1].age == 20
	assert users[2].age == 22
	assert users[0].name == 'Alice'
	assert users[1].name == 'Bob'
	assert users[2].name == 'Peter'
}

fn test_fixed_array_reverse() {
	a := ['hi', '1', '5', '3']!
	b := a.reverse()
	assert a == ['hi', '1', '5', '3']!
	assert b == ['3', '5', '1', 'hi']!
	assert ['hi', '1', '5', '3']!.reverse() == ['3', '5', '1', 'hi']!

	mut nums := [67, -3, 108, 42, 7]!
	n := nums.reverse()
	assert nums == [67, -3, 108, 42, 7]!
	assert n == [7, 42, 108, -3, 67]!
	assert [67, -3, 108, 42, 7]!.reverse() == [7, 42, 108, -3, 67]!

	mut users := [User{22, 'Peter'}, User{20, 'Bob'}, User{25, 'Alice'}]!
	u := users.reverse()

	assert users[0].age == 22
	assert users[1].age == 20
	assert users[2].age == 25
	assert users[0].name == 'Peter'
	assert users[1].name == 'Bob'
	assert users[2].name == 'Alice'

	assert u[0].age == 25
	assert u[1].age == 20
	assert u[2].age == 22
	assert u[0].name == 'Alice'
	assert u[1].name == 'Bob'
	assert u[2].name == 'Peter'

	u2 := [User{22, 'Peter'}, User{20, 'Bob'}, User{25, 'Alice'}]!.reverse()
	assert u2[0].age == 25
	assert u2[1].age == 20
	assert u2[2].age == 22
	assert u2[0].name == 'Alice'
	assert u2[1].name == 'Bob'
	assert u2[2].name == 'Peter'
}
