module main

fn test_fixed_array_unsafe_elements_keep_prior_temporaries() {
	mut pointers := unsafe { [3]&u8{} }
	pointers = [c'first', unsafe { nil }, unsafe { nil }]!
	assert unsafe { pointers[0].vstring() } == 'first'
	assert pointers[1] == unsafe { nil }
	assert pointers[2] == unsafe { nil }
	pointers = [c'next', c'last', unsafe { nil }]!
	assert unsafe { pointers[0].vstring() } == 'next'
	assert unsafe { pointers[1].vstring() } == 'last'
	assert pointers[2] == unsafe { nil }
}

struct ArrayElementCounter {
mut:
	value int
}

fn next_array_value(mut count ArrayElementCounter) int {
	count.value++
	return count.value
}

fn test_fixed_array_block_elements_preserve_evaluation_order() {
	mut count := ArrayElementCounter{}
	values := [next_array_value(mut count), unsafe { next_array_value(mut count) },
		unsafe { next_array_value(mut count) }]!
	assert values == [1, 2, 3]!
	assert count.value == 3
}
