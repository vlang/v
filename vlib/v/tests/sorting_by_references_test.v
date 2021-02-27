struct Container {
mut:
	name string
}

fn test_array_sort_by_references() {
	mut a := []&Container{}

	a << &Container{ name: 'a' }
	a << &Container{ name: 'b' }
	a << &Container{ name: 'c' }
	a << &Container{ name: 'd' }
	a << &Container{ name: 'e' }

	a.sort(a.name > b.name)
	println(a)
	assert a[0].name == 'e'
	assert a[1].name == 'd'
	assert a[2].name == 'c'
	assert a[3].name == 'b'
	assert a[4].name == 'a'
}
