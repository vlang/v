struct InsertList[T] {
mut:
	items []T
}

fn (mut list InsertList[T]) add(value T) {
	list.items << value
}

fn (mut list InsertList[T]) insert(index int, value T) {
	list.items.insert(index, value)
}

fn test_generic_array_insert_keeps_integer_and_string_instantiations() {
	mut numbers := &InsertList[i64]{}
	numbers.add(1)
	numbers.insert(0, 7)
	mut strings := &InsertList[string]{}
	strings.add('x')
	strings.insert(0, 'y')
	assert numbers.items == [i64(7), 1]
	assert strings.items == ['y', 'x']
	numbers.insert(1, 8)
	strings.insert(1, 'z')
	assert numbers.items == [i64(7), 8, 1]
	assert strings.items == ['y', 'z', 'x']
}

fn test_generic_array_insert_keeps_reversed_instantiation_order() {
	mut strings := InsertList[string]{}
	strings.insert(0, 'first')
	mut numbers := InsertList[i64]{}
	numbers.insert(0, 42)
	assert strings.items == ['first']
	assert numbers.items == [i64(42)]
}
