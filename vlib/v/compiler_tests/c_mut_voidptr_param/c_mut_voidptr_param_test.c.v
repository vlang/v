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

type Builder = []u8

fn (mut b Builder) push_byte(c u8) {
	b << c
}

// A mutable parameter of a value type is not one dereference deep. Its C variable
// already holds the address of the caller's storage, so forwarding it to a callee
// that asks for a pointer must emit the slot itself. Emitting `*value` there passes
// a value where a pointer is required, which both tcc and clang reject outright.
fn build_with_mut_value_param(mut sb Builder) int {
	sb.push_byte(u8(65))
	return sb.len
}

fn test_mut_value_param_forwarded_by_reference_to_mut_callee() {
	mut b := Builder([]u8{})
	assert build_with_mut_value_param(mut b) == 1
	assert b.len == 1
}
