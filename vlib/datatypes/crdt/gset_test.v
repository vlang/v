module crdt

fn test_lookup() {
	mut gset := new_gset[string]()
	element := 'some-test-element'
	assert gset.lookup(element) == false
}

fn test_add() {
	mut gset := new_gset[string]()
	element := 'some-test-element'
	assert gset.lookup(element) == false
	gset.add(element)
	assert gset.lookup(element)
}

fn test_compare() {
	mut first_gset := new_gset[string]()
	element := 'some-test-element'
	assert first_gset.lookup(element) == false
	first_gset.add(element)
	assert first_gset.lookup(element) == true
	mut second_gset := new_gset[string]()
	assert first_gset.compare(second_gset) == false
	second_gset.add(element)
	assert second_gset.compare(first_gset)
	assert first_gset.compare(second_gset)
}

fn test_merge() {
	mut first_gset := new_gset[string]()
	element := 'some-test-element'
	assert first_gset.lookup(element) == false
	first_gset.add(element)
	assert first_gset.lookup(element) == true
	mut second_gset := new_gset[string]()
	assert first_gset.compare(second_gset) == false
	second_gset.merge(first_gset)
	assert second_gset.compare(first_gset)
	assert first_gset.compare(second_gset)
}
