module main

@[soa]
struct Vec2 {
	x f32
	y f32
}

fn main() {
	// Test that the SOA struct definition is generated
	// The companion type Vec2_SOA should be available
	println('soa_struct test: ok')
}
