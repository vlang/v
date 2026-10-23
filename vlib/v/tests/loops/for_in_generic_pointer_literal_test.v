@[heap]
struct LoopBox[T] {
	value T
}

fn test_for_in_generic_pointer_literals() {
	first := &LoopBox[string]{ value: 'first' }
	second := &LoopBox[string]{ value: 'second' }
	mut seen := []string{}
	for i, item in [first, second] {
		assert item == [first, second][i]
		seen << item.value
	}
	assert seen == ['first', 'second']
}
