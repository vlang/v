@[translated]
module main

struct Module {
	find fn (int, &fn (voidptr, int) int, &voidptr) int
}

fn plus_one(_ voidptr, x int) int {
	return x + 1
}

// C: `int find(int n, int (**pxFunc)(void *, int), void **ppArg)`. c2v declares the
// `pxFunc` parameter as `&voidptr` in the definition and as `&fn (...)` in the struct
// field (SQLite's `xFindFunction`).
fn find_impl(n int, px_func &voidptr, pp_arg &voidptr) int {
	unsafe {
		*px_func = voidptr(plus_one)
		*pp_arg = voidptr(n)
	}
	return n
}

fn test_a_callee_stores_a_function_through_the_address_of_a_local() {
	m := Module{
		find: find_impl
	}
	mut f := fn (_ voidptr, x int) int {
		return x
	}
	mut arg := unsafe { nil }
	assert m.find(41, &f, &arg) == 41
	assert f(unsafe { nil }, 41) == 42
	assert arg == voidptr(41)
	// Preserve parentheses around addressed operands as regression inputs.
	// vfmt off
	assert m.find(51, &(f), &arg) == 51
	assert f(unsafe { nil }, 51) == 52
	assert arg == voidptr(51)
	assert m.find(61, &((f)), &arg) == 61
	assert f(unsafe { nil }, 61) == 62
	assert arg == voidptr(61)
	// vfmt on
}
