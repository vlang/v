struct Item[T] {
	value T
}

fn (i Item[T]) unwrap() T {
	return i.value
}

fn process[T](i Item[T]) {
	n := i.unwrap()
	println(n)
	assert n == 5
}

fn test_generic_fn_infer_nested_struct() {
	item := Item[int]{
		value: 5
	}
	process(item)
}
