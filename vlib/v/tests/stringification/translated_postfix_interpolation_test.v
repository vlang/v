@[translated]
module main

struct InterpolationCounter {
mut:
	value int
}

fn test_translated_postfix_interpolation_keeps_value_and_side_effects() {
	mut number := 40
	assert '${number++}' == '40'
	assert number == 41
	assert 'value=${number--}!' == 'value=41!'
	assert number == 40
	mut counter := InterpolationCounter{ value: 7 }
	assert '${counter.value++}=${counter.value++};${counter.value}' == '7=8;9'
	assert counter.value == 9
	assert '${counter.value++:04}' == '0009'
	assert counter.value == 10
}
