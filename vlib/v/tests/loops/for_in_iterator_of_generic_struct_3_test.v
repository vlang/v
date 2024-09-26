struct MyGenericArray[T] {
	arr []T
mut:
	idx int
}

fn (mut self MyGenericArray[T]) next() ?T {
	if self.arr.len <= self.idx {
		return none
	} else {
		defer {
			self.idx++
		}
		return self.arr[self.idx]
	}
}

fn test_for_in_iterator_of_generic_struct() {
	mut a := MyGenericArray{[4, 5, 6], 1}
	mut ret := []int{}
	for i in a {
		println(i)
		ret << i
	}
	assert ret.len == 2
	assert ret[0] == 5
	assert ret[1] == 6
}
