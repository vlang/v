// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

// sequential_indices returns the trivial 0..n-1 index buffer used by the
// triangle-soup generators in this file. Keeping indices sequential means
// every three consecutive vertices form one triangle, just like the sphere
// and cylinder generators in mesh_3d.v.
fn sequential_indices(n int) []u32 {
	mut indices := []u32{len: n}
	for i in 0 .. n {
		indices[i] = u32(i)
	}
	return indices
}

// from_triangles wraps parallel vertex attribute arrays into an indexed
// Mesh3D. Degenerate or mismatched inputs yield an empty mesh so callers can
// use the result without extra validation.
fn from_triangles(vertices []Vec3, normals []Vec3, uvs []f32) Mesh3D {
	if vertices.len == 0 || vertices.len % 3 != 0 {
		return Mesh3D{}
	}
	if normals.len != vertices.len || uvs.len != vertices.len * 2 {
		return Mesh3D{}
	}
	return Mesh3D{
		vertices: vertices
		normals:  normals
		uvs:      uvs
		indices:  sequential_indices(vertices.len)
	}
}

// torus_point returns the surface position of a torus around center with its
// axis along Y. theta walks the major circle, phi walks the minor circle.
fn torus_point(center Vec3, major f32, minor f32, theta f32, phi f32) Vec3 {
	r := major + minor * math.cosf(phi)
	return Vec3{center.x + r * math.cosf(theta), center.y + minor * math.sinf(phi), center.z + r * math.sinf(theta)}
}

// torus_normal returns the outward surface normal at the given angles.
fn torus_normal(theta f32, phi f32) Vec3 {
	cp := math.cosf(phi)
	n := Vec3{cp * math.cosf(theta), math.sinf(phi), cp * math.sinf(theta)}
	return n.normalized()
}

// gen_mesh_torus creates a torus centred at center, lying in the XZ plane.
// major is the radius of the central circle and minor is the tube radius;
// rings and slices are the subdivisions of those two circles. Degenerate
// sizes or fewer than three subdivisions produce an empty Mesh3D.
pub fn gen_mesh_torus(center Vec3, major f32, minor f32, rings int, slices int) Mesh3D {
	if rings < 3 || slices < 3 || major <= 0 || minor <= 0 {
		return Mesh3D{}
	}
	turn := 2.0 * f32(math.pi)
	mut vertices := []Vec3{cap: rings * slices * 6}
	mut normals := []Vec3{cap: rings * slices * 6}
	mut uvs := []f32{cap: rings * slices * 12}
	for i in 0 .. rings {
		u0 := f32(i) / f32(rings)
		u1 := f32(i + 1) / f32(rings)
		theta0 := turn * u0
		theta1 := turn * u1
		for j in 0 .. slices {
			v0 := f32(j) / f32(slices)
			v1 := f32(j + 1) / f32(slices)
			phi0 := turn * v0
			phi1 := turn * v1
			p00 := torus_point(center, major, minor, theta0, phi0)
			p01 := torus_point(center, major, minor, theta0, phi1)
			p10 := torus_point(center, major, minor, theta1, phi0)
			p11 := torus_point(center, major, minor, theta1, phi1)
			// Winding matches the UV sphere: (p00,p11,p10) and (p00,p01,p11).
			vertices << p00
			vertices << p11
			vertices << p10
			vertices << p00
			vertices << p01
			vertices << p11
			n00 := torus_normal(theta0, phi0)
			n01 := torus_normal(theta0, phi1)
			n10 := torus_normal(theta1, phi0)
			n11 := torus_normal(theta1, phi1)
			normals << n00
			normals << n11
			normals << n10
			normals << n00
			normals << n01
			normals << n11
			uvs << u0
			uvs << v0
			uvs << u1
			uvs << v1
			uvs << u1
			uvs << v0
			uvs << u0
			uvs << v0
			uvs << u0
			uvs << v1
			uvs << u1
			uvs << v1
		}
	}
	return from_triangles(vertices, normals, uvs)
}

// gen_mesh_cone creates a capped cone from a circular base around base to a
// single point at tip. slices is the base subdivision. A zero-length axis, a
// non-positive radius or fewer than three slices produce an empty Mesh3D.
pub fn gen_mesh_cone(base Vec3, tip Vec3, radius f32, slices int) Mesh3D {
	if slices < 3 || radius <= 0 {
		return Mesh3D{}
	}
	d := tip - base
	height := f32(math.sqrt(f64(d.x * d.x + d.y * d.y + d.z * d.z)))
	if height == 0 {
		return Mesh3D{}
	}
	axis := d.normalized()
	helper := if math.abs(axis.y) < 0.9 { Vec3{0, 1, 0} } else { Vec3{1, 0, 0} }
	u := helper.cross(axis).normalized()
	v := axis.cross(u)
	turn := 2.0 * f32(math.pi)
	cap_normal := axis.scale(-1)
	mut vertices := []Vec3{cap: slices * 6}
	mut normals := []Vec3{cap: slices * 6}
	mut uvs := []f32{cap: slices * 12}
	for j in 0 .. slices {
		u0 := f32(j) / f32(slices)
		u1 := f32(j + 1) / f32(slices)
		phi0 := turn * u0
		phi1 := turn * u1
		c0 := math.cosf(phi0)
		s0 := math.sinf(phi0)
		c1 := math.cosf(phi1)
		s1 := math.sinf(phi1)
		p0 := base + u.scale(radius * c0) + v.scale(radius * s0)
		p1 := base + u.scale(radius * c1) + v.scale(radius * s1)
		// A cone's side normal tilts outward and along the axis.
		radial0 := u.scale(c0) + v.scale(s0)
		radial1 := u.scale(c1) + v.scale(s1)
		n0 := (radial0.scale(height) + axis.scale(radius)).normalized()
		n1 := (radial1.scale(height) + axis.scale(radius)).normalized()
		ntip := (n0 + n1).normalized()
		mu := (u0 + u1) * 0.5
		// Side triangle, wound (p0,p1,tip) so its normal points outward.
		vertices << p0
		vertices << p1
		vertices << tip
		normals << n0
		normals << n1
		normals << ntip
		uvs << u0
		uvs << 0
		uvs << u1
		uvs << 0
		uvs << mu
		uvs << 1
		// Base cap, wound (base,p1,p0) so its normal is -axis.
		vertices << base
		vertices << p1
		vertices << p0
		normals << cap_normal
		normals << cap_normal
		normals << cap_normal
		uvs << 0.5
		uvs << 0.5
		uvs << 0.5 + 0.5 * c1
		uvs << 0.5 + 0.5 * s1
		uvs << 0.5 + 0.5 * c0
		uvs << 0.5 + 0.5 * s0
	}
	return from_triangles(vertices, normals, uvs)
}

// gen_mesh_polygon creates a regular prism with sides edges around center.
// The polygon lies in the XZ plane with radius radius and is extruded along
// Y by thickness (half above and half below center). Invalid sizes or fewer
// than three sides produce an empty Mesh3D.
pub fn gen_mesh_polygon(center Vec3, radius f32, sides int, thickness f32) Mesh3D {
	if sides < 3 || radius <= 0 || thickness <= 0 {
		return Mesh3D{}
	}
	half := thickness * 0.5
	turn := 2.0 * f32(math.pi)
	up := Vec3{0, 1, 0}
	down := Vec3{0, -1, 0}
	mut vertices := []Vec3{cap: sides * 12}
	mut normals := []Vec3{cap: sides * 12}
	mut uvs := []f32{cap: sides * 24}
	for j in 0 .. sides {
		u0 := f32(j) / f32(sides)
		u1 := f32(j + 1) / f32(sides)
		a0 := turn * u0
		a1 := turn * u1
		c0 := math.cosf(a0)
		s0 := math.sinf(a0)
		c1 := math.cosf(a1)
		s1 := math.sinf(a1)
		p0 := Vec3{center.x + radius * c0, center.y - half, center.z + radius * s0}
		p1 := Vec3{center.x + radius * c1, center.y - half, center.z + radius * s1}
		q0 := Vec3{p0.x, center.y + half, p0.z}
		q1 := Vec3{p1.x, center.y + half, p1.z}
		ct := Vec3{center.x, center.y + half, center.z}
		cb := Vec3{center.x, center.y - half, center.z}
		mid := (a0 + a1) * 0.5
		side_normal := Vec3{math.cosf(mid), 0, math.sinf(mid)}
		// Two side triangles, wound outwards.
		vertices << p0
		vertices << q1
		vertices << p1
		vertices << p0
		vertices << q0
		vertices << q1
		normals << side_normal
		normals << side_normal
		normals << side_normal
		normals << side_normal
		normals << side_normal
		normals << side_normal
		uvs << u0
		uvs << 0
		uvs << u1
		uvs << 1
		uvs << u1
		uvs << 0
		uvs << u0
		uvs << 0
		uvs << u0
		uvs << 1
		uvs << u1
		uvs << 1
		// Top and bottom fan triangles.
		vertices << ct
		vertices << q1
		vertices << q0
		vertices << cb
		vertices << q0
		vertices << q1
		normals << up
		normals << up
		normals << up
		normals << down
		normals << down
		normals << down
		uvs << 0.5
		uvs << 0.5
		uvs << 0.5 + 0.5 * c1
		uvs << 0.5 + 0.5 * s1
		uvs << 0.5 + 0.5 * c0
		uvs << 0.5 + 0.5 * s0
		uvs << 0.5
		uvs << 0.5
		uvs << 0.5 + 0.5 * c0
		uvs << 0.5 + 0.5 * s0
		uvs << 0.5 + 0.5 * c1
		uvs << 0.5 + 0.5 * s1
	}
	return from_triangles(vertices, normals, uvs)
}

// heightmap_vertex maps grid coordinate (ix,iz) into object space, using the
// height array value as Y and centring the grid on the origin.
fn heightmap_vertex(heights []f32, cols int, rows int, ix int, iz int, spacing f32) Vec3 {
	return Vec3{spacing * (f32(ix) - f32(cols - 1) * 0.5), heights[iz * cols + ix], spacing * (f32(iz) - f32(rows - 1) * 0.5)}
}

// heightmap_normal estimates a smooth normal from the neighbouring heights,
// using one-sided differences at the grid border.
fn heightmap_normal(heights []f32, cols int, rows int, ix int, iz int, spacing f32) Vec3 {
	h := heights[iz * cols + ix]
	hl := if ix > 0 { heights[iz * cols + ix - 1] } else { h }
	hr := if ix < cols - 1 { heights[iz * cols + ix + 1] } else { h }
	hd := if iz > 0 { heights[(iz - 1) * cols + ix] } else { h }
	hu := if iz < rows - 1 { heights[(iz + 1) * cols + ix] } else { h }
	mut xs := f32(1)
	if ix > 0 && ix < cols - 1 {
		xs = 2
	}
	mut zs := f32(1)
	if iz > 0 && iz < rows - 1 {
		zs = 2
	}
	gx := (hr - hl) / (spacing * xs)
	gz := (hu - hd) / (spacing * zs)
	n := Vec3{-gx, 1, -gz}
	return n.normalized()
}

// gen_mesh_heightmap creates a smoothly shaded surface from a row-major grid
// of heights. cols and rows describe the grid, spacing is the XZ distance
// between neighbours. A grid smaller than 2x2, a non-positive spacing or a
// height array shorter than cols*rows produce an empty Mesh3D.
pub fn gen_mesh_heightmap(heights []f32, cols int, rows int, spacing f32) Mesh3D {
	if cols < 2 || rows < 2 || spacing <= 0 || heights.len < cols * rows {
		return Mesh3D{}
	}
	mut vertices := []Vec3{cap: (cols - 1) * (rows - 1) * 6}
	mut normals := []Vec3{cap: (cols - 1) * (rows - 1) * 6}
	mut uvs := []f32{cap: (cols - 1) * (rows - 1) * 12}
	for iz in 0 .. rows - 1 {
		for ix in 0 .. cols - 1 {
			a := heightmap_vertex(heights, cols, rows, ix, iz, spacing)
			b := heightmap_vertex(heights, cols, rows, ix + 1, iz, spacing)
			c := heightmap_vertex(heights, cols, rows, ix, iz + 1, spacing)
			d := heightmap_vertex(heights, cols, rows, ix + 1, iz + 1, spacing)
			na := heightmap_normal(heights, cols, rows, ix, iz, spacing)
			nb := heightmap_normal(heights, cols, rows, ix + 1, iz, spacing)
			nc := heightmap_normal(heights, cols, rows, ix, iz + 1, spacing)
			nd := heightmap_normal(heights, cols, rows, ix + 1, iz + 1, spacing)
			u0 := f32(ix) / f32(cols - 1)
			u1 := f32(ix + 1) / f32(cols - 1)
			v0 := f32(iz) / f32(rows - 1)
			v1 := f32(iz + 1) / f32(rows - 1)
			// Same upward winding as gen_mesh_plane: (a,d,b) and (a,c,d).
			vertices << a
			vertices << d
			vertices << b
			vertices << a
			vertices << c
			vertices << d
			normals << na
			normals << nd
			normals << nb
			normals << na
			normals << nc
			normals << nd
			uvs << u0
			uvs << v0
			uvs << u1
			uvs << v1
			uvs << u1
			uvs << v0
			uvs << u0
			uvs << v0
			uvs << u0
			uvs << v1
			uvs << u1
			uvs << v1
		}
	}
	return from_triangles(vertices, normals, uvs)
}

// gen_mesh_cubicmap creates the union of unit cubes for every non-zero cell
// in a row-major occupancy grid. cols and rows describe the grid and cube_size
// is the edge length of each cube; the grid is centred on the origin. Invalid
// dimensions, non-positive sizes or an empty grid produce an empty Mesh3D.
pub fn gen_mesh_cubicmap(cells []u8, cols int, rows int, cube_size f32) Mesh3D {
	if cols < 1 || rows < 1 || cube_size <= 0 || cells.len < cols * rows {
		return Mesh3D{}
	}
	mut vertices := []Vec3{}
	mut normals := []Vec3{}
	mut uvs := []f32{}
	for iz in 0 .. rows {
		for ix in 0 .. cols {
			idx := iz * cols + ix
			if cells[idx] == 0 {
				continue
			}
			cx := cube_size * (f32(ix) - f32(cols - 1) * 0.5)
			cz := cube_size * (f32(iz) - f32(rows - 1) * 0.5)
			offset := Vec3{cx, 0, cz}
			cube := gen_mesh_cube(Vec3{cube_size, cube_size, cube_size})
			// Expand the cube's indexed faces into triangle soup so the
			// sequential indices in from_triangles keep the correct winding.
			for k in 0 .. cube.indices.len {
				i := int(cube.indices[k])
				vertices << cube.vertices[i] + offset
				normals << cube.normals[i]
				uvs << cube.uvs[i * 2]
				uvs << cube.uvs[i * 2 + 1]
			}
		}
	}
	return from_triangles(vertices, normals, uvs)
}
