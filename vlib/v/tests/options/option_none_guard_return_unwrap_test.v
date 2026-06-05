fn guarded_name_len(name ?string) int {
	if name == none {
		return 0
	}
	return name.len
}

fn test_option_none_guard_return_unwrap() {
	assert guarded_name_len(none) == 0
	assert guarded_name_len('guarded') == 7
}

struct NoneGuardCat {
	state int
}

fn maybe_none_guard_cat() ?NoneGuardCat {
	return NoneGuardCat{
		state: 1
	}
}

fn none_guard_cat_state(cat NoneGuardCat) int {
	return cat.state
}

fn guarded_cat_arg_state() int {
	x := maybe_none_guard_cat()
	if x == none {
		return 0
	}
	return none_guard_cat_state(x)
}

fn guarded_cat_cast_state() int {
	x := maybe_none_guard_cat()
	if x == none {
		return 0
	}
	cat := NoneGuardCat(x)
	return cat.state
}

fn guarded_cat_closure_arg_state() int {
	x := maybe_none_guard_cat()
	if x == none {
		return 0
	}
	callback := fn [x] () int {
		return none_guard_cat_state(x)
	}
	return callback()
}

fn generic_none_guard_identity[T](value T) T {
	return value
}

fn guarded_generic_closure_unwrap[T](value ?T) ?T {
	if value == none {
		return none
	}
	callback := fn [value] [T]() T {
		return generic_none_guard_identity[T](value)
	}
	return callback[T]()
}

fn guarded_cat_generic_closure_arg_state() int {
	cat := guarded_generic_closure_unwrap[NoneGuardCat](maybe_none_guard_cat()) or { return 0 }
	return cat.state
}

fn test_option_none_guard_return_unwrap_for_args_and_casts() {
	assert guarded_cat_arg_state() == 1
	assert guarded_cat_cast_state() == 1
	assert guarded_cat_closure_arg_state() == 1
	assert guarded_cat_generic_closure_arg_state() == 1
}
