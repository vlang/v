struct Box[T] {
mut:
	values []T
}

fn (b Box[T]) [] (i int) T {
	return b.values[i]
}

fn (mut b Box[T]) []= (i int, value T) {
	b.values[i] = value
}

fn test_generic_index_operator_assign() {
	mut b := Box[int]{
		values: [1, 2]
	}
	b[1] = 9
	assert b[1] == 9
	b[0] += 5
	assert b[0] == 6
	mut s := Box[string]{
		values: ['a', 'b']
	}
	s[0] = 'x'
	s[1] += 'y'
	assert s[0] == 'x'
	assert s[1] == 'by'
}
