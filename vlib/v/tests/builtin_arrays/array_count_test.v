fn test_main() {
	a := []int{len: 10, init: index}
	assert a.count(it % 2) == 5

	b := [10]int{init: index}
	assert a.count(it % 2) == 5
}

fn test_zero() {
	a := []int{len: 10, init: index}
	assert a.count(it == 1000) == 0

	b := [10]int{init: index}
	assert a.count(it == 1000) == 0
}

fn test_struct() {
	struct Abc {
		x int
		y int
		z string
	}

	a := [Abc{}, Abc{1, 2, 'abc'}, Abc{100, 2, 'def'}, Abc{0, 0, 'a'}]

	assert dump(a.count(it.z.starts_with('a'))) == 2
	assert dump(a.count(it.y == 2)) == 2
	assert dump(a.count(it.z.len == 1)) == 1
	assert dump(a.count(it.z.len < 3)) == 2

	sa := ['aa', 'bb', 'ccc']
	dump(sa)
	assert dump(sa.count(it.len < 3)) == 2
	assert dump(sa.count(it == 'aa')) == 1
	assert dump(sa.count(it.len == 3)) == 1
}
