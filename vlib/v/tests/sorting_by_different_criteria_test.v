struct Child {
	f f64
}

struct Parent {
	child Child
	name  string
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
}
