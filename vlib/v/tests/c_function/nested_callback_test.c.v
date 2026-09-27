module main

#include "@VMODROOT/nested_callback.c"

// A C callback whose own parameter is a multi-parameter callback, see issue #28935.
fn C.v_nested_call_outer(outer fn (int, fn (int, int) int, voidptr) int, inner fn (int, int) int) int

fn nested_add(a int, b int) int {
	return a + b
}

fn nested_mul(a int, b int) int {
	return a * b
}

fn nested_outer(x int, cb fn (int, int) int, p voidptr) int {
	extra := if p == unsafe { nil } { 0 } else { 1000 }
	return cb(x, 10) + extra
}

fn test_c_callback_with_nested_callback_param() {
	assert C.v_nested_call_outer(nested_outer, nested_add) == 13
	assert C.v_nested_call_outer(nested_outer, nested_mul) == 30
}
