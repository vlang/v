// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

// draw_cube draws a solid cube centred at pos with the given size and color.
// It must be called inside a ctx.begin_3d()/ctx.end_3d() scope.
pub fn (ctx &Context) draw_cube(pos Vec3, size Vec3, color Color) {
	corners := cube_corners(pos, size)
	ctx.r.begin_triangles()
	for i in cube_triangles {
		v := corners[i]
		ctx.r.v3f_c4b(v.x, v.y, v.z, color.r, color.g, color.b, color.a)
	}
	ctx.r.end()
}

// draw_cube_wires draws the twelve edges of a cube centred at pos.
pub fn (ctx &Context) draw_cube_wires(pos Vec3, size Vec3, color Color) {
	corners := cube_corners(pos, size)
	ctx.r.begin_lines()
	for i in cube_edges {
		v := corners[i]
		ctx.r.v3f_c4b(v.x, v.y, v.z, color.r, color.g, color.b, color.a)
	}
	ctx.r.end()
}

// emit_triangles records a list of vertices as a single triangle batch.
fn emit_triangles(ctx &Context, verts []Vec3, color Color) {
	ctx.r.begin_triangles()
	for v in verts {
		ctx.r.v3f_c4b(v.x, v.y, v.z, color.r, color.g, color.b, color.a)
	}
	ctx.r.end()
}

// draw_plane draws a flat XZ rectangle centred at pos, facing upwards.
pub fn (ctx &Context) draw_plane(pos Vec3, width f32, depth f32, color Color) {
	hw := width / 2.0
	hd := depth / 2.0
	p0 := Vec3{pos.x - hw, pos.y, pos.z - hd}
	p1 := Vec3{pos.x + hw, pos.y, pos.z - hd}
	p2 := Vec3{pos.x + hw, pos.y, pos.z + hd}
	p3 := Vec3{pos.x - hw, pos.y, pos.z + hd}
	emit_triangles(ctx, [p0, p2, p1, p0, p3, p2], color)
}

// draw_grid draws a grid of lines on the XZ plane centred at the origin.
pub fn (ctx &Context) draw_grid(slices int, spacing f32, color Color) {
	if slices <= 0 || spacing <= 0 {
		return
	}
	half := f32(slices) * spacing / 2.0
	ctx.r.begin_lines()
	for i in 0 .. slices + 1 {
		p := -half + f32(i) * spacing
		ctx.r.v3f_c4b(-half, 0, p, color.r, color.g, color.b, color.a)
		ctx.r.v3f_c4b(half, 0, p, color.r, color.g, color.b, color.a)
		ctx.r.v3f_c4b(p, 0, -half, color.r, color.g, color.b, color.a)
		ctx.r.v3f_c4b(p, 0, half, color.r, color.g, color.b, color.a)
	}
	ctx.r.end()
}

// draw_line_3d draws a single line between two points in 3D.
pub fn (ctx &Context) draw_line_3d(start Vec3, end Vec3, color Color) {
	ctx.r.begin_lines()
	ctx.r.v3f_c4b(start.x, start.y, start.z, color.r, color.g, color.b, color.a)
	ctx.r.v3f_c4b(end.x, end.y, end.z, color.r, color.g, color.b, color.a)
	ctx.r.end()
}

// draw_sphere draws a solid sphere with the given number of rings and slices.
pub fn (ctx &Context) draw_sphere(center Vec3, radius f32, rings int, slices int, color Color) {
	emit_triangles(ctx, sphere_triangles(center, radius, rings, slices), color)
}

// draw_cylinder draws a solid cylinder between two points.
pub fn (ctx &Context) draw_cylinder(start Vec3, end Vec3, radius f32, slices int, color Color) {
	emit_triangles(ctx, cylinder_triangles(start, end, radius, slices), color)
}
