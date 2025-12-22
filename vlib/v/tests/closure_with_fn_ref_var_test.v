module main

fn f_a() int {
	println('a')
	return 1
}

fn test_closure_with_fn_ref_var() {
	f := f_a
	ref := &f
	println('ref=0x${ptr_str(ref)}')

	handler := fn [ref] () int { // ref_fn is captured as a direct function pointer (*fn) instead of a pointer to a function (**fn)
		println('in closure: ref=0x${ptr_str(ref)}')
		deref_fn := *ref
		println('in closure: deref=0x${ptr_str(deref_fn)}')
		return deref_fn()
	}

	f()
	deref := *ref
	println('deref=0x${ptr_str(deref)}')
	assert deref() == 1
	assert handler() == 1
}
