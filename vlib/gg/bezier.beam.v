// BEAM Backend: Graphics/GPU modules are not applicable on the BEAM VM.
// The BEAM backend focuses on server-side, networking, and concurrent workloads.
// All functions return safe defaults (empty structs, 0, false).
// For GUI on BEAM, consider using wx (Erlang's native GUI toolkit).
//
// Bezier curve drawing functions are no-ops on BEAM.
module gg

@[direct_array_access]
pub fn (ctx &Context) draw_cubic_bezier_recursive(points []f32, c Color) {
}

pub fn (ctx &Context) draw_cubic_bezier_recursive_scalar(x1 f32, y1 f32, x2 f32, y2 f32, x3 f32, y3 f32,
	x4 f32, y4 f32, c Color) {
}

fn (ctx &Context) cubic_bezier_rec(x1 f32, y1 f32, x2 f32, y2 f32, x3 f32, y3 f32, x4 f32, y4 f32, level int) {
}
