struct GenericMethodOrdering {
	value int
}

fn (receiver GenericMethodOrdering) read[T](value T) int {
	return receiver.value + int(value)
}

fn replace_generic_method_receiver(mut receiver GenericMethodOrdering) int {
	receiver = GenericMethodOrdering{ value: 20 }
	return 3
}

fn test_explicit_generic_method_reads_receiver_before_argument_changes_it() {
	mut receiver := GenericMethodOrdering{ value: 2 }
	assert receiver.read[int](replace_generic_method_receiver(mut receiver)) == 5
	assert receiver.value == 20
}
