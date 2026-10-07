// `?T` with an option `T` is that option, as `??T` is not a V type (#29418).
struct OptBox[T] {
	v ?T
}

struct OptMany[T] {
	arr []?T
	m   map[string]?T
}

fn (b OptBox[T]) get() ?T {
	return b.v
}

fn test_generic_struct_option_field_of_option_arg() {
	empty := OptBox[?int]{}
	assert empty.v == none
	assert '${empty}'.contains('v: Option(none)')
	assert empty.get() == none
	full := OptBox[?int]{
		v: 5
	}
	assert (full.v or { -1 }) == 5
	assert '${full}'.contains('v: Option(5)')
	assert (full.get() or { -1 }) == 5
	if v := full.v {
		assert v == 5
	} else {
		assert false
	}
}

fn test_generic_struct_option_containers_of_option_arg() {
	many := OptMany[?int]{
		arr: [?int(1), none]
		m:   {
			'a': ?int(2)
		}
	}
	assert '${many.arr}' == '[Option(1), Option(none)]'
	assert (many.arr[0] or { -1 }) == 1
	assert many.arr[1] == none
	assert (many.m['a'] or { -1 }) == 2
}
