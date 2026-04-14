module main

struct IntList {
mut:
	values []int
}

fn (l IntList) [] (index int) int {
	return l.values[index]
}

fn (mut l IntList) []= (index int, value int) {
	l.values[index] = value
}

fn test_operator_overloading_index() {
	mut list := IntList{
		values: [3, 5, 8]
	}
	assert list[1] == 5
	list[1] = 33
	assert list[1] == 33
	list[1] += 7
	assert list[1] == 40
	list[0] *= 4
	assert list[0] == 12
}
