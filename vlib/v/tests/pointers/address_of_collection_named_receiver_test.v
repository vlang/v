struct CollectionNamedReceiver {
	object int
}

fn collection_named_receiver() ?CollectionNamedReceiver {
	return CollectionNamedReceiver{42}
}

fn test_address_of_collection_named_if_guard_receiver() {
	mut handles := map[voidptr]int{}
	if array := collection_named_receiver() {
		handles[voidptr(&array.object)] = array.object
		assert handles[voidptr(&array.object)] == 42
	} else {
		assert false
	}
	assert handles.len == 1
}
