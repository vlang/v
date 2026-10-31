type LiteralValue = int | string

struct LiteralItem {
	value int
}

fn literal_pair[T](first T, second T) []T {
	mut values := []T{}
	for value in [first, second] {
		values << value
	}
	return values
}

fn record_literal(mut order []int, value int) int {
	order << value
	return value
}

fn test_literal_iteration_values_and_evaluation() {
	mut source := 3
	mut values := []int{}
	for index, value in [source, source] {
		source += index + 1
		values << value
	}
	assert values == [3, 3]
	assert source == 6
	assert literal_pair('first', 'second') == ['first', 'second']
	first := LiteralValue(12)
	second := LiteralValue('second')
	assert literal_pair(first, second) == [first, second]
	assert literal_pair([1, 2], [3]) == [[1, 2], [3]]
	mut order := []int{}
	for value in [record_literal(mut order, 1), record_literal(mut order, 2)] {
		order << value + 10
	}
	assert order == [1, 2, 11, 12]
}

fn test_literal_iteration_keeps_escaping_values() {
	mut values := []&LiteralItem{}
	for value in [11, 22, 33] {
		values << &LiteralItem{ value: value }
	}
	assert values[0].value == 11
	assert values[1].value == 22
	assert values[2].value == 33
	mut total := 0
	for value in [1, 2, 3] {
		if value == 2 {
			continue
		}
		total += value
	}
	assert total == 4
}
