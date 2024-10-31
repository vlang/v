struct User {
	age  int
	name string
}

fn test_fixed_array_sort() {
	mut a := ['hi', '1', '5', '3']!
	a.sort()
	assert a == ['1', '3', '5', 'hi']!

	mut nums := [67, -3, 108, 42, 7]!
	nums.sort()
	assert nums == [-3, 7, 42, 67, 108]!

	nums.sort(a < b)
	assert nums == [-3, 7, 42, 67, 108]!

	nums.sort(b < a)
	assert nums == [108, 67, 42, 7, -3]!

	mut users := [User{22, 'Peter'}, User{20, 'Bob'}, User{25, 'Alice'}]!
	users.sort(a.age < b.age)
	assert users[0].age == 20
	assert users[1].age == 22
	assert users[2].age == 25
	assert users[0].name == 'Bob'
	assert users[1].name == 'Peter'
	assert users[2].name == 'Alice'

	users.sort(a.age > b.age)
	assert users[0].age == 25
	assert users[1].age == 22
	assert users[2].age == 20

	users.sort(b.age > a.age)
	assert users[0].age == 20
	assert users[1].age == 22
	assert users[2].age == 25

	users.sort(a.name < b.name)
	assert users[0].name == 'Alice'
	assert users[1].name == 'Bob'
	assert users[2].name == 'Peter'
}

fn test_sorted_immutable_original_should_not_change() {
	a := ['hi', '1', '5', '3']!
	b := a.sorted()
	assert a == ['hi', '1', '5', '3']!
	assert b == ['1', '3', '5', 'hi']!
}

fn test_sorted_mutable_original_should_not_change() {
	mut a := ['hi', '1', '5', '3']!
	b := a.sorted()
	assert a == ['hi', '1', '5', '3']!
	assert b == ['1', '3', '5', 'hi']!
}

fn test_sorted_reversed() {
	aa := ['hi', '1', '5', '3']!
	bb := aa.sorted(a > b)
	assert aa == ['hi', '1', '5', '3']!
	assert bb == ['hi', '5', '3', '1']!
}

fn test_sorted_by_len() {
	a := ['hi', 'abc', 'a', 'zzzzz']!
	c := a.sorted(a.len < b.len)
	assert c == ['a', 'hi', 'abc', 'zzzzz']!
}

fn test_sort_with_compare_1() {
	mut a := ['hi', '1', '5', '3']!
	a.sort_with_compare(fn (a &string, b &string) int {
		if a < b {
			return -1
		}
		if a > b {
			return 1
		}
		return 0
	})
	assert a == ['1', '3', '5', 'hi']!
}

fn test_sorted_with_compare_1() {
	a := ['hi', '1', '5', '3']!
	b := a.sorted_with_compare(fn (a &string, b &string) int {
		if a < b {
			return -1
		}
		if a > b {
			return 1
		}
		return 0
	})
	assert a == ['hi', '1', '5', '3']!
	assert b == ['1', '3', '5', 'hi']!
}

struct Ka {
	s string
	i int
}

fn test_sort_with_compare_2() {
	mut arr := [
		Ka{
			s: 'bbb'
			i: 100
		},
		Ka{
			s: 'aaa'
			i: 101
		},
		Ka{
			s: 'ccc'
			i: 102
		},
	]!
	cmp := fn (a &Ka, b &Ka) int {
		return compare_strings(a.s, b.s)
	}
	arr.sort_with_compare(cmp)
	assert arr[0].s == 'aaa'
	assert arr[0].i == 101
	assert arr[1].s == 'bbb'
	assert arr[1].i == 100
	assert arr[2].s == 'ccc'
	assert arr[2].i == 102
}

fn test_sorted_with_compare_2() {
	arr := [
		Ka{
			s: 'bbb'
			i: 100
		},
		Ka{
			s: 'aaa'
			i: 101
		},
		Ka{
			s: 'ccc'
			i: 102
		},
	]!
	cmp := fn (a &Ka, b &Ka) int {
		return compare_strings(a.s, b.s)
	}
	b := arr.sorted_with_compare(cmp)
	assert arr[0].s == 'bbb'
	assert arr[0].i == 100
	assert arr[1].s == 'aaa'
	assert arr[1].i == 101
	assert arr[2].s == 'ccc'
	assert arr[2].i == 102

	assert b[0].s == 'aaa'
	assert b[0].i == 101
	assert b[1].s == 'bbb'
	assert b[1].i == 100
	assert b[2].s == 'ccc'
	assert b[2].i == 102
}
