// Test that unused functions returning fixed arrays of custom structs
// don't cause C compilation errors when skip_unused is enabled.
// Related to: functions returning [N]CustomStruct where the function is never called.

struct QuadVertex {
	pos [2]f32
	uv  [2]f32
}

// This function is intentionally never called - it tests that skip_unused
// correctly handles fn_ret ArrayFixed types with custom struct elements.
pub fn make_quad_vertices(x f32, y f32, w f32, h f32) [4]QuadVertex {
	return [
		QuadVertex{
			pos: [x, y]!
			uv:  [f32(0), 0]!
		},
		QuadVertex{
			pos: [x + w, y]!
			uv:  [f32(1), 0]!
		},
		QuadVertex{
			pos: [x + w, y + h]!
			uv:  [f32(1), 1]!
		},
		QuadVertex{
			pos: [x, y + h]!
			uv:  [f32(0), 1]!
		},
	]!
}

fn test_unused_fn_fixed_array_ret() {
	// The test passes if compilation succeeds.
	// make_quad_vertices is intentionally not called.
	assert true
}
