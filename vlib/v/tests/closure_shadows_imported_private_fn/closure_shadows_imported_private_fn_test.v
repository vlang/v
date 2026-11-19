module main

import shadowed

// https://github.com/vlang/v/issues/32479
// A call of a local closure must resolve to the closure, not to a
// same-named private function of an imported module (which is not even
// visible unqualified from here).
fn test_local_closure_shadows_imported_private_fn() {
	assert shadowed.use_shadowed() == 'x0'
	mut out := []u8{}
	blank := fn (from int, to int, mut out []u8) {
		out << u8(from + to)
	}
	blank(1, 2, mut out)
	assert out.len == 1
	assert out[0] == 3
}
