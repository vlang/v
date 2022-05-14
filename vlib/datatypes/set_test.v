import datatypes as dt

fn test_is_empty() {
	mut set := dt.Set<int>{}
	assert set.is_empty() == true
	set.add(1)
	assert set.is_empty() == false
}
