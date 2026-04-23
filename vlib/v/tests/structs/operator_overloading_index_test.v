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

struct Tensor {
mut:
	last_set       int
	last_signature int
}

fn (t Tensor) [] (parts []SliceIndex) int {
	mut total := 0
	for part in parts {
		if part.is_range {
			total += 100
			if part.has_low {
				total += part.low * 10
			}
			if part.has_high {
				total += part.high
			}
		} else {
			total += part.value
		}
	}
	return total
}

fn (mut t Tensor) []= (parts []SliceIndex, value int) {
	mut total := 0
	for part in parts {
		if part.is_range {
			total += 100
			if part.has_low {
				total += part.low * 10
			}
			if part.has_high {
				total += part.high
			}
		} else {
			total += part.value
		}
	}
	t.last_signature = total
	t.last_set = value
}

struct Axis {}

fn (a Axis) [] (part SliceIndex) int {
	if part.is_range {
		mut total := 100
		if part.has_low {
			total += part.low * 10
		}
		if part.has_high {
			total += part.high
		}
		return total
	}
	return part.value
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

fn test_operator_overloading_multi_index() {
	mut tensor := Tensor{}
	assert tensor[1, 2] == 3
	assert tensor[1..3, .., 4..] == 353
	tensor[2, ..3] = 7
	assert tensor.last_signature == 105
	assert tensor.last_set == 7
	tensor[1, 3..] += 1
	assert tensor.last_signature == 131
	assert tensor.last_set == 132

	axis := Axis{}
	assert axis[2] == 2
	assert axis[..3] == 103
	assert axis[4..] == 140
}
