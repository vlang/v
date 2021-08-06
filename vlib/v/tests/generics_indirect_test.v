const zzz = &Local{}

struct Local {
	aborted bool
}

pub fn current() &Local {
	return zzz
}

pub fn store<T>(var &T, value T) {
	eprintln('store ${voidptr(var)} <- ${value}')
	unsafe {
		*var = value
	}
}

fn test_generic_over_a_local_boolean_address() {
	eprintln('-'.repeat(40))
	mut mybool := false
	println(mybool)
	store(mybool, true)
	println(mybool)
	eprintln('-'.repeat(40))
}

fn test_generic_over_a_const_returned_by_a_fn() {
	println(current().aborted)
	store(current().aborted, true)
	println(current().aborted)
	eprintln('-'.repeat(40))
}
