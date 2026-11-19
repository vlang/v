// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

// Mesh3D holds indexed triangle geometry in object space.
pub struct Mesh3D {
pub mut:
	vertices []Vec3
	indices  []u32
}

// valid reports whether every index refers to a vertex.
pub fn (mesh &Mesh3D) valid() bool {
	if mesh.indices.len % 3 != 0 {
		return false
	}
	for i in mesh.indices {
		if i >= mesh.vertices.len {
			return false
		}
	}
	return true
}

// gen_mesh_cube creates an indexed cube centred at the origin.
pub fn gen_mesh_cube(size Vec3) Mesh3D {
	corners := cube_corners(Vec3{}, size)
	mut indices := []u32{len: cube_triangles.len}
	for i, index in cube_triangles {
		indices[i] = u32(index)
	}
	mut vertices := []Vec3{len: corners.len}
	for i, corner in corners {
		vertices[i] = corner
	}
	return Mesh3D{ vertices: vertices, indices: indices }
}

// gen_mesh_plane creates a segmented XZ plane facing upwards.
pub fn gen_mesh_plane(width f32, depth f32, segments int) Mesh3D {
	n := if segments < 1 { 1 } else { segments }
	mut vertices := []Vec3{cap: (n + 1) * (n + 1)}
	for iz in 0 .. n + 1 {
		z := depth * (f32(iz) / f32(n) - 0.5)
		for ix in 0 .. n + 1 {
			vertices << Vec3{width * (f32(ix) / f32(n) - 0.5), 0, z}
		}
	}
	mut indices := []u32{cap: n * n * 6}
	for iz in 0 .. n {
		for ix in 0 .. n {
			a := u32(iz * (n + 1) + ix)
			b := a + 1
			c := a + u32(n + 1)
			d := c + 1
			indices << a
			indices << d
			indices << b
			indices << a
			indices << c
			indices << d
		}
	}
	return Mesh3D{ vertices: vertices, indices: indices }
}

// gen_mesh_sphere creates a UV sphere centred at the origin.
pub fn gen_mesh_sphere(radius f32, rings int, slices int) Mesh3D {
	verts := sphere_triangles(Vec3{}, radius, rings, slices)
	mut indices := []u32{len: verts.len}
	for i in 0 .. verts.len {
		indices[i] = u32(i)
	}
	return Mesh3D{ vertices: verts, indices: indices }
}

// gen_mesh_cylinder creates a capped cylinder between two points.
pub fn gen_mesh_cylinder(start Vec3, end Vec3, radius f32, slices int) Mesh3D {
	verts := cylinder_triangles(start, end, radius, slices)
	mut indices := []u32{len: verts.len}
	for i in 0 .. verts.len {
		indices[i] = u32(i)
	}
	return Mesh3D{ vertices: verts, indices: indices }
}

fn rotate_3d(v Vec3, rot Vec3) Vec3 {
	pi := f32(math.pi)
	rx := rot.x * pi / 180.0
	cx := f32(math.cosf(rx))
	sx := f32(math.sinf(rx))
	p := Vec3{v.x, v.y * cx - v.z * sx, v.y * sx + v.z * cx}
	ry := rot.y * pi / 180.0
	cy := f32(math.cosf(ry))
	sy := f32(math.sinf(ry))
	q := Vec3{p.x * cy + p.z * sy, p.y, -p.x * sy + p.z * cy}
	rz := rot.z * pi / 180.0
	cz := f32(math.cosf(rz))
	sz := f32(math.sinf(rz))
	return Vec3{q.x * cz - q.y * sz, q.x * sz + q.y * cz, q.z}
}

// draw_mesh records a mesh transformed by pos, rotation (degrees) and scale.
// It must be called inside a ctx.begin_3d()/ctx.end_3d() scope.
pub fn (ctx &Context) draw_mesh(mesh &Mesh3D, pos Vec3, rotation Vec3, size f32, color Color) {
	if !mesh.valid() {
		return
	}
	mut verts := []Vec3{cap: mesh.indices.len}
	for index in mesh.indices {
		v := rotate_3d(mesh.vertices[index], rotation).scale(size)
		verts << Vec3{pos.x + v.x, pos.y + v.y, pos.z + v.z}
	}
	emit_triangles(ctx, verts, color)
}
