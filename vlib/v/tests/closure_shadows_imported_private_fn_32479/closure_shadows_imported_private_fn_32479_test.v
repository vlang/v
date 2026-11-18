module main

import ablank

// A call of a local closure must resolve to the closure, even when an
// imported module declares a private function with the same name.
// Regression test for https://github.com/vlang/v/issues/32479: the call
// below used to be checked and generated against `ablank.blank`, which made
// the generated C uncompilable.
fn test_local_closure_shadows_private_fn_of_imported_module() {
	assert ablank.use_blank() == 'x:2'
	mut out := []u8{}
	blank := fn (from int, to int, mut out []u8) {
		out << u8(from + to)
	}
	blank(1, 2, mut out)
	assert out.len == 1
	assert out[0] == 3
}

fn helper(a int, b int) int {
	return a - b
}

// A local closure also keeps shadowing a same-module function.
fn test_local_closure_shadows_same_module_fn() {
	helper := fn (a int, b int) int {
		return a + b
	}
	assert helper(2, 3) == 5
}
