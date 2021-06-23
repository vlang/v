struct Child {
	f f64
}

struct Parent {
	child Child
	name  string
}

fn (p Parent) < (p1 Parent) bool {
	return p.name < p1.name
}

fn test_sorting_by_different_criteria_in_same_function() {
	mut arr := [
		Parent{Child{0.2}, 'def'},
		Parent{Child{0.1}, 'xyz'},
		Parent{Child{0.3}, 'abc'},
	]
	assert arr[0].name == 'def'
	arr.sort(a.name < b.name)
	// println(arr)
	assert arr[0].name == 'abc'
	// println(arr)
	arr.sort(a.child.f < b.child.f)
	assert arr[0].name == 'xyz'
	arr.sort(a < b)
	assert arr[0].name == 'abc'
	arr.sort(a > b)
	assert arr[0].name == 'xyz'
}
