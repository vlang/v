struct Doubler {
mut:
	val int
	until int
}

fn (mut it Doubler) next() ?int {
	v := it.val
	if v > it.until {
		return none
	}
	it.val *= 2
	return v
}

fn doubler(start int, until int) Doubler {
	return Doubler{start, until}
}

fn test_for_in_iterator() {
	mut d := doubler(5, 30)
	mut vals := []int{}
	for val in d {
		vals << val
	}
	assert vals == [5, 10, 20]
}

fn test_for_in_empty_iterator() {
	mut d := doubler(5, 2)
	mut vals := []int{}
	for val in d {
		vals << val
	}
	assert vals == []
}

fn test_for_in_iterator_with_tmp_expr() {
	mut vals := []int{}
	for val in doubler(2, 64) {
		vals << val
	}
	assert vals == [2, 4, 8, 16, 32, 64]
}
