// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

// begin_triangles starts a triangle batch using gg's render backend, allowing
// custom geometry with per-vertex colour and UV. Close it with ctx.end_batch().
pub fn (ctx &Context) begin_triangles() {
	ctx.r.begin_triangles()
}

// begin_lines starts a line batch. Close it with ctx.end_batch().
pub fn (ctx &Context) begin_lines() {
	ctx.r.begin_lines()
}

// begin_points starts a point batch. Close it with ctx.end_batch().
pub fn (ctx &Context) begin_points() {
	ctx.r.begin_points()
}

// vertex3 records a world-space vertex with a colour in the active batch.
pub fn (ctx &Context) vertex3(x f32, y f32, z f32, color Color) {
	ctx.r.v3f_c4b(x, y, z, color.r, color.g, color.b, color.a)
}

// vertex3_uv records a textured world-space vertex in the active batch.
pub fn (ctx &Context) vertex3_uv(x f32, y f32, z f32, u f32, v f32, color Color) {
	ctx.r.v3f_t2f_c4b(x, y, z, u, v, color.r, color.g, color.b, color.a)
}

// end_batch closes the active batch opened by begin_triangles/lines/points.
pub fn (ctx &Context) end_batch() {
	ctx.r.end()
}

// draw_triangle_strip records three or more vertices as a triangle strip.
pub fn (ctx &Context) draw_triangle_strip(points []Vec3, color Color) {
	ctx.r.begin_triangle_strip()
	for p in points {
		ctx.r.v3f_c4b(p.x, p.y, p.z, color.r, color.g, color.b, color.a)
	}
	ctx.r.end()
}
