struct Num {
	value int
}

fn (a Num) < (b Num) bool {
	return a.value < b.value
}

fn test_sort_lt_overloaded_struct_array() {
	mut arr := []Num{}
	arr << Num{
		value: 10
	}
	arr << Num{
		value: 5
	}
	arr << Num{
		value: 7
	}
	arr.sort()
	assert arr[0].value == 5
}
