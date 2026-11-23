// `T{}` with `T = &S` allocates a zero `S`; with `T = &&S`, `&&&S`, ... it allocates
// a chain of heap slots, each pointing to the next, that ends in that zero `S`.
// vlib/v/gen/c/multi_pointer_struct_init_test.v checks the C types of this file.
struct Point {
	name string = 'p'
mut:
	x int
}

type Shape = Point | int

fn new_value[T]() T {
	return T{}
}

fn new_ref[T]() &T {
	return &T{}
}

fn test_multi_pointer_struct_init_is_a_chain_of_heap_slots() {
	p2 := new_value[&&Point]()
	assert voidptr(p2) != unsafe { nil }
	assert voidptr(*p2) != unsafe { nil }
	assert (**p2).x == 0
	assert (**p2).name == 'p'
	p3 := new_value[&&&Point]()
	assert voidptr(*p3) != unsafe { nil }
	assert voidptr(**p3) != unsafe { nil }
	assert (***p3).x == 0
	assert (***p3).name == 'p'
	mut other := new_value[&&&Point]()
	(***other).x = 5
	assert (***p3).x == 0
	assert (***other).x == 5
}

fn test_address_of_pointer_struct_init() {
	r := new_ref[&Point]()
	assert voidptr(*r) != unsafe { nil }
	assert (**r).name == 'p'
}

fn test_multi_pointer_to_scalar_ends_in_nil() {
	i1 := new_value[&int]()
	assert voidptr(i1) == unsafe { nil }
	i2 := new_value[&&int]()
	assert voidptr(i2) != unsafe { nil }
	assert voidptr(*i2) == unsafe { nil }
	i3 := new_value[&&&int]()
	assert voidptr(*i3) != unsafe { nil }
	assert voidptr(**i3) == unsafe { nil }
}

fn test_multi_pointer_to_sum_and_fixed_array() {
	s := new_value[&&Shape]()
	assert voidptr(*s) != unsafe { nil }
	f := new_value[&&[2]int]()
	assert voidptr(*f) != unsafe { nil }
	assert (**f)[1] == 0
}
