// `$zero` of a pointer type is a typed `nil`, so a generic call infers the pointer
// type from it (not `voidptr`), and a generic can recurse over `pointee_type`.

fn new_pointer_to[T](value T) &T {
	mut ptr := unsafe { &T(vcalloc(sizeof(T))) }
	unsafe {
		*ptr = value
	}
	return ptr
}

fn make_pointer[P](_ P) P {
	$if P.indirections == 1 {
		mut inner := $new(P.pointee_type)
		unsafe {
			*inner = 42
		}
		return inner
	} $else {
		return new_pointer_to(make_pointer($zero(P.pointee_type)))
	}
}

fn test_generic_recursion_over_pointer_depth() {
	two := make_pointer(&&int(unsafe { nil }))
	assert **two == 42
	five := make_pointer(&&&&&int(unsafe { nil }))
	assert *****five == 42
}
