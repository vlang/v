fn test_simple_opt_ptr() {
	val := 123
	mut var := unsafe { ?&int(&val) }
	assert var? == unsafe { &int(&val) }
}

fn test_simple_writing() {
	val := 123
	mut var := unsafe { ?&int(&val) }
	unsafe {
		*var? = 321
	}
	assert val == 321
}

fn test_simple_deref() ? {
	val := 123
	mut var := unsafe { ?&int(&val) }
	assert *var? == val
	mut r := var?
	unsafe {
		*r = 321
	}
	assert val == 321
}

fn f_ref(val ?&int) ? {
	t := 123
	assert *val? == t
}

fn test_simple_writing2() {
	val := 123
	mut var := unsafe { ?&int(&val) }
	unsafe {
		*var? = 321
	}
	x := *var?
	assert val == x
}

fn test_simple_fn() {
	var := 123
	f_ref(&var)
}

fn test_unset_opt_ptr() {
	val := 123
	mut var := unsafe { ?&int(&val) }
	unsafe {
		*var? = 1
	}
	assert var != none
	var = none
	assert var == none
}
