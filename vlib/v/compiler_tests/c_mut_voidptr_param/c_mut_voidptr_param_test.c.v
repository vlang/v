module main

#include "@VMODROOT/c_id.c"

fn C.c_mut_voidptr_id(p voidptr) voidptr

// A `mut` parameter read by value is one dereference deep: the value is `*o`,
// not the address of the parameter slot. Passing it to a C function that takes
// that value must therefore emit `C.func(*o)`; emitting `C.func(o)` hands the
// C side the address of the parameter slot, which then reads an unrelated
// pointer.
fn c_mut_voidptr_id_thunk(mut o voidptr) voidptr {
	return C.c_mut_voidptr_id(o)
}

fn test_mut_voidptr_param_passed_to_c_fn() {
	mut o := voidptr(u64(0x1234))
	got := c_mut_voidptr_id_thunk(mut o)
	assert got == voidptr(u64(0x1234))
}

fn test_mut_voidptr_param_value_identity_simple() {
	// The same value read inside plain V code must still resolve through the
	// mutable parameter.
	a := 7
	mut p := unsafe { &a }
	assert p == unsafe { &a }
}
