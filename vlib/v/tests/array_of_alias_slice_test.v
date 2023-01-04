type Ints = []int

fn (i Ints) slice(to int) Ints {
	return i[0..to]
}

fn test_array_of_alias_slice() {
	values := Ints([5, 7, 9])

	println(values.slice(2))
	assert values.slice(2) == Ints([5, 7])
}
