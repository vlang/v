struct GenericMapIterator[T] {
mut:
	values []T
	index  int
}

fn (mut iter GenericMapIterator[T]) next() ?T {
	if iter.index >= iter.values.len {
		return none
	}
	value := iter.values[iter.index]
	iter.index++
	return value
}

fn test_generic_optional_map_return() {
	mut iter := GenericMapIterator[map[string]int]{
		values: [map[string]int{
			'one': 1
		}]
	}
	value := iter.next() or {
		assert false
		return
	}
	assert value == {
		'one': 1
	}
	assert iter.next() == none
}
