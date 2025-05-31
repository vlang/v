fn a() {}

fn b() {}

fn c() {}

fn v() {}

fn test_any_all_of_ints() {
	ia := [1, 2, 3]!

	assert ia.any(it > 2)
	assert ia.any(|x| x > 2)
	assert [1, 2, 3]!.any(it > 2)
	assert [1, 2, 3]!.any(|x| x > 2)

	assert !ia.all(it > 1)
	assert !ia.all(|x| x > 1)
	assert ![1, 2, 3]!.all(it > 1)
	assert ![1, 2, 3]!.all(|x| x > 1)

	assert ia.any(it == 2)
	assert ia.any(|x| x == 2)
	assert [1, 2, 3]!.any(it == 2)
	assert [1, 2, 3]!.any(|x| x == 2)

	assert !ia.all(it == 3)
	assert !ia.all(|x| x == 3)
	assert ![1, 2, 3]!.all(it == 3)
	assert ![1, 2, 3]!.all(|x| x == 3)
}

fn test_any_all_of_strings() {
	sa := ['a', 'b', 'c']!

	assert sa.any(it == 'b')
	assert sa.any(|x| x == 'b')
	assert ['a', 'b', 'c']!.any(it == 'b')
	assert ['a', 'b', 'c']!.any(|x| x == 'b')

	assert !sa.all(it == 'c')
	assert !sa.all(|x| x == 'c')
	assert !['a', 'b', 'c']!.all(it == 'c')
	assert !['a', 'b', 'c']!.all(|x| x == 'c')
}

fn test_any_all_of_voidptrs() {
	pa := [voidptr(123), voidptr(45), voidptr(99)]!

	assert pa.any(it == voidptr(45))
	assert pa.any(|x| x == voidptr(45))
	assert [voidptr(123), voidptr(45), voidptr(99)]!.any(it == voidptr(45))
	assert [voidptr(123), voidptr(45), voidptr(99)]!.any(|x| x == voidptr(45))

	assert !pa.all(it == voidptr(123))
	assert !pa.all(|x| x == voidptr(123))
	assert ![voidptr(123), voidptr(45), voidptr(99)]!.all(it == voidptr(123))
	assert ![voidptr(123), voidptr(45), voidptr(99)]!.all(|x| x == voidptr(123))
}

fn test_any_all_of_fns() {
	fa := [a, b, c]!

	assert fa.any(it == b)
	assert fa.any(|x| x == b)
	assert [a, b, c]!.any(it == b)
	assert [a, b, c]!.any(|x| x == b)

	assert !fa.all(it == v)
	assert !fa.all(|x| x == v)
	assert ![a, b, c]!.all(it == v)
	assert ![a, b, c]!.all(|x| x == v)
}

fn test_contains_of_ints() {
	ia := [1, 2, 3]!
	mut ii := ia.contains(2)
	dump(ii)
	assert ii
	assert [1, 2, 3]!.contains(2)

	ii = ia.contains(5)
	dump(ii)
	assert !ii
	assert ![1, 2, 3]!.contains(5)
}

fn test_contains_of_strings() {
	sa := ['a', 'b', 'c']!
	mut si := sa.contains('b')
	dump(si)
	assert si
	assert ['a', 'b', 'c']!.contains('b')

	si = sa.contains('v')
	dump(si)
	assert !si
	assert !['a', 'b', 'c']!.contains('v')
}

fn test_contains_of_voidptrs() {
	pa := [voidptr(123), voidptr(45), voidptr(99)]!
	mut pi := pa.contains(voidptr(45))
	dump(pi)
	assert pi
	assert [voidptr(123), voidptr(45), voidptr(99)]!.contains(voidptr(45))

	pi = pa.contains(unsafe { nil })
	dump(pi)
	assert !pi
	assert ![voidptr(123), voidptr(45), voidptr(99)]!.contains(unsafe { nil })
}

fn test_contains_of_fns() {
	fa := [a, b, c]!
	mut fi := fa.contains(b)
	dump(fi)
	assert fi
	assert [a, b, c]!.contains(b)

	fi = fa.contains(v)
	dump(fi)
	assert !fi
	assert ![a, b, c]!.contains(v)
}

fn test_index_of_ints() {
	ia := [1, 2, 3]!
	mut ii := ia.index(2)
	dump(ii)
	assert ii == 1
	assert [1, 2, 3]!.index(2) == 1

	ii = ia.index(5)
	dump(ii)
	assert ii == -1
	assert [1, 2, 3]!.index(5) == -1
}

fn test_index_of_strings() {
	sa := ['a', 'b', 'c']!
	mut si := sa.index('b')
	dump(si)
	assert si == 1
	assert ['a', 'b', 'c']!.index('b') == 1

	si = sa.index('v')
	dump(si)
	assert si == -1
	assert ['a', 'b', 'c']!.index('v') == -1
}

fn test_index_of_voidptrs() {
	pa := [voidptr(123), voidptr(45), voidptr(99)]!
	mut pi := pa.index(voidptr(45))
	dump(pi)
	assert pi == 1
	assert [voidptr(123), voidptr(45), voidptr(99)]!.index(voidptr(45)) == 1

	pi = pa.index(unsafe { nil })
	dump(pi)
	assert pi == -1
	assert [voidptr(123), voidptr(45), voidptr(99)]!.index(unsafe { nil }) == -1
}

fn test_index_of_fns() {
	fa := [a, b, c]!
	mut fi := fa.index(b)
	dump(fi)
	assert fi == 1
	assert [a, b, c]!.index(b) == 1

	fi = fa.index(v)
	dump(fi)
	assert fi == -1
	assert [a, b, c]!.index(v) == -1
}

fn test_fixed_array_map() {
	a := [1, 2, 3]!

	b1 := a.map(it * 2)
	println(b1)
	assert b1 == [2, 4, 6]!
	b10 := [1, 2, 3]!.map(it * 2)
	assert b10 == [2, 4, 6]!
	assert [1, 2, 3]!.map(it * 2) == [2, 4, 6]!

	b11 := a.map(|x| x * 2)
	println(b11)
	assert b11 == [2, 4, 6]!
	b110 := [1, 2, 3]!.map(|x| x * 2)
	assert b110 == [2, 4, 6]!
	assert [1, 2, 3]!.map(|x| x * 2) == [2, 4, 6]!

	b2 := a.map('${it}')
	println(b2)
	assert b2 == ['1', '2', '3']!
	b20 := [1, 2, 3]!.map('${it}')
	assert b20 == ['1', '2', '3']!
	assert [1, 2, 3]!.map('${it}') == ['1', '2', '3']!

	b22 := a.map(|x| '${x}')
	println(b22)
	assert b22 == ['1', '2', '3']!
	b220 := [1, 2, 3]!.map(|x| '${x}')
	assert b220 == ['1', '2', '3']!
	assert [1, 2, 3]!.map(|x| '${x}') == ['1', '2', '3']!

	b3 := a.map(it + 2)
	println(b3)
	assert b3 == [3, 4, 5]!
	b30 := [1, 2, 3]!.map(it + 2)
	assert b30 == [3, 4, 5]!
	assert [1, 2, 3]!.map(it + 2) == [3, 4, 5]!

	b33 := a.map(|x| x + 2)
	println(b33)
	assert b33 == [3, 4, 5]!
	b330 := [1, 2, 3]!.map(|x| x + 2)
	assert b330 == [3, 4, 5]!
	assert [1, 2, 3]!.map(|x| x + 2) == [3, 4, 5]!
}

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
	assert ['hi', '1', '5', '3']!.sorted() == ['1', '3', '5', 'hi']!
}

fn test_sorted_mutable_original_should_not_change() {
	mut a := ['hi', '1', '5', '3']!
	b := a.sorted()
	assert a == ['hi', '1', '5', '3']!
	assert b == ['1', '3', '5', 'hi']!
	assert ['hi', '1', '5', '3']!.sorted() == ['1', '3', '5', 'hi']!
}

fn test_sorted_reversed() {
	aa := ['hi', '1', '5', '3']!
	bb := aa.sorted(a > b)
	assert aa == ['hi', '1', '5', '3']!
	assert bb == ['hi', '5', '3', '1']!
	assert ['hi', '1', '5', '3']!.sorted(a > b) == ['hi', '5', '3', '1']!
}

fn test_sorted_by_len() {
	a := ['hi', 'abc', 'a', 'zzzzz']!
	c := a.sorted(a.len < b.len)
	assert c == ['a', 'hi', 'abc', 'zzzzz']!
	assert ['hi', 'abc', 'a', 'zzzzz']!.sorted(a.len < b.len) == ['a', 'hi', 'abc', 'zzzzz']!
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
	assert ['hi', '1', '5', '3']!.sorted_with_compare(fn (a &string, b &string) int {
		if a < b {
			return -1
		}
		if a > b {
			return 1
		}
		return 0
	}) == ['1', '3', '5', 'hi']!
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

	b2 := [
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
	]!.sorted_with_compare(cmp)

	assert b2[0].s == 'aaa'
	assert b2[0].i == 101
	assert b2[1].s == 'bbb'
	assert b2[1].i == 100
	assert b2[2].s == 'ccc'
	assert b2[2].i == 102
}
