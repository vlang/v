module main

import fieldglobals

struct Color {
	r f64
	g f64
}

fn take(x f64) f64 {
	return x
}

// `c.g` is the field, not the `__global g` of the imported module.
fn test_field_named_like_module_global_keeps_field_type() {
	c := Color{1, 2}
	assert take(c.g + 1) == 3.0
	assert take((c.g) + 1) == 3.0
	assert take(c.g * 2) == 4.0
	assert take(c.r + 1) == 2.0
	assert fieldglobals.next() == 43
}
