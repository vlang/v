// BEAM Backend: Graphics/GPU modules are not applicable on the BEAM VM.
// The BEAM backend focuses on server-side, networking, and concurrent workloads.
// All functions return safe defaults (empty structs, 0, false).
// For GUI on BEAM, consider using wx (Erlang's native GUI toolkit).
//
// All draw_* functions are no-ops that accept parameters for API compatibility.
module gg

@[params]
pub struct DrawPixelConfig {
pub mut:
	size f32 = 1.0
}

pub fn (ctx &Context) draw_pixel(x f32, y f32, c Color, params DrawPixelConfig) {}

@[direct_array_access]
pub fn (ctx &Context) draw_pixels(points []f32, c Color, params DrawPixelConfig) {}

pub fn (ctx &Context) draw_line(x f32, y f32, x2 f32, y2 f32, c Color) {}

pub fn (ctx &Context) draw_line_with_config(x f32, y f32, x2 f32, y2 f32, config PenConfig) {}

pub fn (ctx &Context) draw_poly_empty(points []f32, c Color) {}

pub fn (ctx &Context) draw_convex_poly(points []f32, c Color) {}

pub fn (ctx &Context) draw_rect_empty(x f32, y f32, w f32, h f32, c Color) {}

pub fn (ctx &Context) draw_rect_empty_no_context(x f32, y f32, w f32, h f32, c Color) {}

pub fn (ctx &Context) draw_rect_filled(x f32, y f32, w f32, h f32, c Color) {}

pub fn (ctx &Context) draw_rect_filled_no_context(x f32, y f32, w f32, h f32, c Color) {}

pub enum PaintStyle {
	fill
	stroke
}

@[params]
pub struct DrawRectParams {
pub:
	x          f32
	y          f32
	w          f32
	h          f32
	color      Color      = black
	style      PaintStyle = .fill
	is_rounded bool
	radius     f32
}

pub fn (ctx &Context) draw_rect(p DrawRectParams) {}

pub fn (ctx &Context) draw_rounded_rect_empty(x f32, y f32, w f32, h f32, radius f32, c Color) {}

pub fn (ctx &Context) draw_rounded_rect_filled(x f32, y f32, w f32, h f32, radius f32, c Color) {}

pub fn (ctx &Context) draw_triangle_empty(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c Color) {}

pub fn (ctx &Context) draw_triangle_filled(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c Color) {}

pub fn (ctx &Context) draw_square_empty(x f32, y f32, s f32, c Color) {}

pub fn (ctx &Context) draw_square_filled(x f32, y f32, s f32, c Color) {}

pub fn (ctx &Context) draw_circle_empty(x f32, y f32, radius f32, c Color) {}

pub fn (ctx &Context) draw_circle_filled(x f32, y f32, radius f32, c Color) {}

pub fn (ctx &Context) draw_polygon_filled(x f32, y f32, size f32, edges int, rotation f32, c Color) {}

pub fn (ctx &Context) draw_circle_with_segments(x f32, y f32, radius f32, segments int, c Color) {}

pub fn (ctx &Context) draw_circle_line(x f32, y f32, radius int, segments int, c Color) {}

pub fn (ctx &Context) draw_slice_empty(x f32, y f32, radius f32, start_angle f32, end_angle f32, segments int, c Color) {}

pub fn (ctx &Context) draw_slice_filled(x f32, y f32, radius f32, start_angle f32, end_angle f32, segments int, c Color) {}

pub fn (ctx Context) draw_arc_line(x f32, y f32, radius f32, start_angle f32, end_angle f32, segments int, c Color) {}

pub fn (ctx &Context) draw_arc_empty(x f32, y f32, inner_radius f32, thickness f32, start_angle f32, end_angle f32, segments int, c Color) {}

pub fn (ctx &Context) draw_arc_filled(x f32, y f32, inner_radius f32, thickness f32, start_angle f32, end_angle f32, segments int, c Color) {}

pub fn (ctx &Context) draw_ellipse_empty(x f32, y f32, rw f32, rh f32, c Color) {}

pub fn (ctx &Context) draw_ellipse_thick(x f32, y f32, rw f32, rh f32, th f32, c Color) {}

pub fn (ctx &Context) draw_ellipse_filled(x f32, y f32, rw f32, rh f32, c Color) {}

pub fn (ctx &Context) draw_ellipse_empty_rotate(x f32, y f32, rw f32, rh f32, rota f32, c Color) {}

pub fn (ctx &Context) draw_ellipse_thick_rotate(x f32, y f32, rw f32, rh f32, th f32, rota f32, c Color) {}

pub fn (ctx &Context) draw_ellipse_filled_rotate(x f32, y f32, rw f32, rh f32, rota f32, c Color) {}

pub fn (ctx &Context) draw_cubic_bezier(points []f32, c Color) {}

pub fn (ctx &Context) draw_cubic_bezier_in_steps(points []f32, steps u32, c Color) {}
