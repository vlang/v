import arrays

struct Compound {
mut:
	s string
	i int
	u u64
	m map[string]i16
}

fn check[T](original []T) {
	mut result := []T{cap: original.len}
	for x in arrays.reverse_iterator(original) {
		result << x
	}
	assert result.len == original.len
	assert result.first() == original.last()
	assert result.reverse() == original
	eprintln('> original: ${original}')
	eprintln('>   result: ${result}')
}

fn test_reverse_iterator_basic() {
	check(['abc', 'def', 'ghi', 'jkl'])
	check([10, 20, 30, 40])
	check([
		Compound{'abc', 123, 444, {
			'aa': i16(12)
			'bb': 31
		}},
		Compound{'def', 456, 555, {
			'bb': i16(22)
			'cc': 32
		}},
		Compound{'xyz', 789, 666, {
			'cc': i16(32)
			'dd': 33
		}},
	])
}

fn test_reverse_iterator_with_mut() {
	mut original := [10, 20]
	mut before := []int{cap: original.len}
	mut after := []int{cap: original.len}
	for mut x in arrays.reverse_iterator(original) {
		before << *x
		(**x)++
		after << *x
	}
	assert before == [20, 10]
	assert after == [21, 11]
	assert original == [11, 21]
}

fn test_reverse_iterator_with_mut_compound() {
	mut original := [Compound{
		s: 'abc'
		i: 123
	}, Compound{
		s: 'xyz'
		i: 987
	}]
	mut before := []Compound{cap: original.len}
	mut after := []Compound{cap: original.len}
	for mut x in arrays.reverse_iterator(original) {
		before << *x
		x.i++
		x.s += ' tail'
		x.u = 99
		x.m['modified'] = 1
		after << *x
	}
	assert after[0] == Compound{
		s: 'xyz tail'
		i: 988
		u: 99
		m: {
			'modified': i16(1)
		}
	}
	assert before[0] == Compound{
		s: 'xyz'
		i: 987
		u: 0
		m: {}
	}
	assert after.reverse() == original
}
