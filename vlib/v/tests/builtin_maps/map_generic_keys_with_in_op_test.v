import datatypes

fn x() {
	mut set1 := datatypes.Set[string]{}
	set1.add('')
	assert set1.exists('')
}

fn y() {
	mut set2 := datatypes.Set[int]{}
	set2.add(1)
	assert set2.exists(1)
}

fn test_main() {
	x()
	y()
}
