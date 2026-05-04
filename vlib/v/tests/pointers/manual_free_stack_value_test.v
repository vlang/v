module main

struct Hc256 {}

@[unsafe]
fn (mut h Hc256) free() {
	unsafe { free(h) }
}

fn test_manual_free_stack_value() {
	mut h := Hc256{}
	unsafe { h.free() }
	assert true
}
