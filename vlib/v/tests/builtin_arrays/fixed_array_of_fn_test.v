fn foo(a string) int {
	return 10 + a.len
}

fn test_fixed_array_fn_index() {
	a := [foo]!
	println(a[0]('hello'))
	assert a[0]('hello') == 15
}

type Handler = fn () int

type HandlerList = []Handler

fn one() int {
	return 1
}

fn two() int {
	return 2
}

fn test_fixed_callback_array_keeps_inline_storage() {
	choose_one := one() == 1
	a := [2]Handler[if choose_one { one } else { two }, two]
	assert sizeof(a) == 2 * sizeof(Handler)
	assert sizeof(typeof(a)) == 2 * sizeof(Handler)
	assert a[0]() == 1
	assert a[1]() == 2
	b := [if !choose_one { one } else { two }, one]!
	assert sizeof(b) == 2 * sizeof(Handler)
	assert sizeof(typeof(b)) == 2 * sizeof(Handler)
	assert b[0]() == 2
	assert b[1]() == 1
}

fn test_callback_array_keeps_declared_alias() {
	choose_one := one() == 1
	a := HandlerList([if choose_one { one } else { two }, two])
	assert typeof(a).name == 'HandlerList'
	assert a[0]() == 1
	assert a[1]() == 2
}

fn check_generic_callback_arrays[T](value T) {
	choose_one := value == value
	a := [2]Handler[if choose_one { one } else { two }, two]
	assert sizeof(a) == 2 * sizeof(Handler)
	assert a[0]() == 1
	assert a[1]() == 2
	b := HandlerList([if choose_one { one } else { two }, two])
	assert typeof(b).name == 'HandlerList'
	assert b[0]() == 1
	assert b[1]() == 2
}

fn test_generic_callback_arrays_keep_declared_types() {
	check_generic_callback_arrays[int](1)
}
