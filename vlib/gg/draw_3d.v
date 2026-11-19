// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

// cube_corners returns the eight corners of a cube centred at pos.
fn cube_corners(pos Vec3, size Vec3) [8]Vec3 {
	hx := size.x / 2.0
	hy := size.y / 2.0
	hz := size.z / 2.0
	return [
		Vec3{pos.x - hx, pos.y - hy, pos.z - hz}
		Vec3{pos.x + hx, pos.y - hy, pos.z - hz}
		Vec3{pos.x + hx, pos.y + hy, pos.z - hz}
		Vec3{pos.x - hx, pos.y + hy, pos.z - hz}
		Vec3{pos.x - hx, pos.y - hy, pos.z + hz}
		Vec3{pos.x + hx, pos.y - hy, pos.z + hz}
		Vec3{pos.x + hx, pos.y + hy, pos.z + hz}
		Vec3{pos.x - hx, pos.y + hy, pos.z + hz},
	]!
}

// cube_triangles lists the twelve triangles of a cube with counter-clockwise
// winding on every outward face, matching the default backface culling.
const cube_triangles = [u8(4), 5, 6, 4, 6, 7, 1, 0, 3, 1, 3, 2, 7, 6, 2, 7, 2, 3, 0, 1, 5, 0, 5,
	4, 5, 1, 2, 5, 2, 6, 0, 4, 7, 0, 7, 3]!

// cube_edges lists the twelve edges of a cube.
const cube_edges = [u8(0), 1, 1, 2, 2, 3, 3, 0, 4, 5, 5, 6, 6, 7, 7, 4, 0, 4, 1, 5, 2, 6, 3, 7]!

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
