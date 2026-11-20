// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

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

// sphere_triangles builds the vertices of a UV sphere with outward winding.
fn sphere_triangles(center Vec3, radius f32, rings int, slices int) []Vec3 {
	if rings < 1 || slices < 3 {
		return []
	}
	pi := f32(math.pi)
	mut verts := []Vec3{cap: rings * slices * 6}
	for i in 0 .. rings {
		theta0 := pi * f32(i) / f32(rings)
		theta1 := pi * f32(i + 1) / f32(rings)
		st0 := math.sinf(theta0)
		ct0 := math.cosf(theta0)
		st1 := math.sinf(theta1)
		ct1 := math.cosf(theta1)
		for j in 0 .. slices {
			phi0 := 2.0 * pi * f32(j) / f32(slices)
			phi1 := 2.0 * pi * f32(j + 1) / f32(slices)
			p00 := Vec3{center.x + radius * st0 * math.cosf(phi0), center.y + radius * ct0, center.z + radius * st0 * math.sinf(phi0)}
			p01 := Vec3{center.x + radius * st0 * math.cosf(phi1), center.y + radius * ct0, center.z + radius * st0 * math.sinf(phi1)}
			p10 := Vec3{center.x + radius * st1 * math.cosf(phi0), center.y + radius * ct1, center.z + radius * st1 * math.sinf(phi0)}
			p11 := Vec3{center.x + radius * st1 * math.cosf(phi1), center.y + radius * ct1, center.z + radius * st1 * math.sinf(phi1)}
			verts << p00
			verts << p11
			verts << p10
			verts << p00
			verts << p01
			verts << p11
		}
	}
	return verts
}

// cylinder_triangles builds the vertices of a capped cylinder.
fn cylinder_triangles(start Vec3, end Vec3, radius f32, slices int) []Vec3 {
	if slices < 3 {
		return []
	}
	axis := (end - start).normalized()
	if axis.x == 0 && axis.y == 0 && axis.z == 0 {
		return []
	}
	helper := if math.abs(axis.y) < 0.9 { Vec3{0, 1, 0} } else { Vec3{1, 0, 0} }
	u := helper.cross(axis).normalized()
	v := axis.cross(u)
	pi := f32(math.pi)
	mut verts := []Vec3{cap: slices * 12}
	for j in 0 .. slices {
		phi0 := 2.0 * pi * f32(j) / f32(slices)
		phi1 := 2.0 * pi * f32(j + 1) / f32(slices)
		c0 := radius * math.cosf(phi0)
		s0sin := radius * math.sinf(phi0)
		c1 := radius * math.cosf(phi1)
		s1sin := radius * math.sinf(phi1)
		s0 := start + u.scale(c0) + v.scale(s0sin)
		s1 := start + u.scale(c1) + v.scale(s1sin)
		e0 := end + u.scale(c0) + v.scale(s0sin)
		e1 := end + u.scale(c1) + v.scale(s1sin)
		verts << s0
		verts << e1
		verts << e0
		verts << s0
		verts << s1
		verts << e1
		verts << start
		verts << s1
		verts << s0
		verts << end
		verts << e0
		verts << e1
	}
	return verts
}
