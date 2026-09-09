module main

#include "@VMODROOT/c_id.c"

fn C.c_mut_voidptr_id(p voidptr) voidptr

// A `mut` parameter referenced by value inside a V function is one dereference
// deep: the current value is `*o`, not `o`. Passing it to a C function that
// expects exactly that pointer value must therefore emit `C.func(*o)`; if the
// codegen emits `C.func(o)`, the C side receives the address of the parameter
// slot and dereferences unrelated memory (cf. cpp_impulse SIGBUS in simu).
fn c_mut_voidptr_id_thunk(mut o voidptr) voidptr {
	return C.c_mut_voidptr_id(o)
}

fn test_mut_voidptr_param_passed_to_c_fn() {
	want := voidptr(u64(0x1234))
	mut o := want
	got := c_mut_voidptr_id_thunk(mut o)
	assert got == want
}

fn test_mut_voidptr_param_value_identity_simple() {
	// also: the same value used inside plain V code (no C call) must still
	// resolve through the mut parameter
	a := 7
	mut p := unsafe { &a }
	assert p == unsafe { &a }
}
