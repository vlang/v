// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

// Mesh3D holds indexed triangle geometry in object space.
// Normals and UVs are parallel to vertices; they may be empty when unused.
pub struct Mesh3D {
pub mut:
	vertices []Vec3
	normals  []Vec3
	uvs      []f32
	indices  []u32
}

// valid reports whether every index refers to a vertex and attributes match.
pub fn (mesh &Mesh3D) valid() bool {
	if mesh.indices.len % 3 != 0 || mesh.vertices.len == 0 {
		return false
	}
	if mesh.normals.len != 0 && mesh.normals.len != mesh.vertices.len {
		return false
	}
	if mesh.uvs.len != 0 && mesh.uvs.len != mesh.vertices.len * 2 {
		return false
	}
	for i in mesh.indices {
		if i >= mesh.vertices.len {
			return false
		}
	}
	return true
}

const cube_faces = [
	u8(4),
	5,
	6,
	7,
	1,
	0,
	3,
	2,
	7,
	6,
	2,
	3,
	0,
	1,
	5,
	4,
	5,
	1,
	2,
	6,
	0,
	4,
	7,
	3,
]!

const cube_normals = [
	Vec3{0, 0, 1},
	Vec3{0, 0, -1},
	Vec3{0, 1, 0},
	Vec3{0, -1, 0},
	Vec3{1, 0, 0},
	Vec3{-1, 0, 0},
]!

// gen_mesh_cube creates an indexed cube with per-face normals and UVs.
pub fn gen_mesh_cube(size Vec3) Mesh3D {
	corners := cube_corners(Vec3{}, size)
	mut vertices := []Vec3{cap: 24}
	mut normals := []Vec3{cap: 24}
	mut uvs := []f32{cap: 48}
	mut indices := []u32{cap: 36}
	face_uvs := [f32(0), 0, 1, 0, 1, 1, 0, 1]!
	for f in 0 .. 6 {
		base := u32(f * 4)
		n := cube_normals[f]
		for k in 0 .. 4 {
			vertices << corners[cube_faces[f * 4 + k]]
			normals << n
			uvs << face_uvs[k * 2]
			uvs << face_uvs[k * 2 + 1]
		}
		indices << base
		indices << base + 1
		indices << base + 2
		indices << base
		indices << base + 2
		indices << base + 3
	}
	return Mesh3D{ vertices: vertices, normals: normals, uvs: uvs, indices: indices }
}

// gen_mesh_plane creates a segmented XZ plane facing upwards.
pub fn gen_mesh_plane(width f32, depth f32, segments int) Mesh3D {
	n := if segments < 1 { 1 } else { segments }
	mut vertices := []Vec3{cap: (n + 1) * (n + 1)}
	mut normals := []Vec3{cap: (n + 1) * (n + 1)}
	mut uvs := []f32{cap: (n + 1) * (n + 1) * 2}
	for iz in 0 .. n + 1 {
		vz := f32(iz) / f32(n)
		z := depth * (vz - 0.5)
		for ix in 0 .. n + 1 {
			ux := f32(ix) / f32(n)
			vertices << Vec3{width * (ux - 0.5), 0, z}
			normals << Vec3{0, 1, 0}
			uvs << ux
			uvs << vz
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
	return Mesh3D{ vertices: vertices, normals: normals, uvs: uvs, indices: indices }
}

// gen_mesh_sphere creates a UV sphere centred at the origin.
pub fn gen_mesh_sphere(radius f32, rings int, slices int) Mesh3D {
	if radius <= 0 {
		// A non-positive radius would divide by zero when computing UVs.
		return Mesh3D{}
	}
	verts := sphere_triangles(Vec3{}, radius, rings, slices)
	mut normals := []Vec3{cap: verts.len}
	mut uvs := []f32{cap: verts.len * 2}
	for v in verts {
		normals << v.normalized()
		mut yy := v.y / radius
		if yy > 1 {
			yy = 1
		}
		if yy < -1 {
			yy = -1
		}
		uvs << 0.5 + f32(math.atan2(f64(v.z), f64(v.x)) / (2.0 * math.pi))
		uvs << 0.5 - f32(math.asin(f64(yy)) / math.pi)
	}
	mut indices := []u32{len: verts.len}
	for i in 0 .. verts.len {
		indices[i] = u32(i)
	}
	return Mesh3D{ vertices: verts, normals: normals, uvs: uvs, indices: indices }
}

// gen_mesh_cylinder creates a capped cylinder between two points.
pub fn gen_mesh_cylinder(start Vec3, end Vec3, radius f32, slices int) Mesh3D {
	verts := cylinder_triangles(start, end, radius, slices)
	axis := (end - start).normalized()
	mut uvs := []f32{len: verts.len * 2}
	if verts.len == slices * 12 {
		for j in 0 .. slices {
			u0 := f32(j) / f32(slices)
			u1 := f32(j + 1) / f32(slices)
			phi0 := 2.0 * f32(math.pi) * u0
			phi1 := 2.0 * f32(math.pi) * u1
			cap0x := 0.5 + 0.5 * f32(math.cosf(phi0))
			cap0y := 0.5 + 0.5 * f32(math.sinf(phi0))
			cap1x := 0.5 + 0.5 * f32(math.cosf(phi1))
			cap1y := 0.5 + 0.5 * f32(math.sinf(phi1))
			base := j * 24
			side_uv := [f32(u0), 0, u1, 1, u0, 1, u0, 0, u1, 0, u1, 1]!
			cap_uv := [f32(0.5), 0.5, cap1x, cap1y, cap0x, cap0y, 0.5, 0.5, cap0x, cap0y, cap1x,
				cap1y]!
			for k, value in side_uv {
				uvs[base + k] = value
			}
			for k, value in cap_uv {
				uvs[base + 12 + k] = value
			}
		}
	}
	mut normals := []Vec3{cap: verts.len}
	for i, v in verts {
		// Each slice emits 6 side vertices, 3 start-cap vertices (normal
		// -axis) and 3 end-cap vertices (normal +axis).
		part := i % 12
		if verts.len == slices * 12 && part >= 6 {
			normals << if part <= 8 { axis.scale(-1) } else { axis }
			continue
		}
		// Work relative to the axis origin: subtracting the axial component
		// from the absolute position makes normals change under translation.
		d := v - start
		radial := d - axis.scale(axis.dot(d))
		n := radial.normalized()
		normals << if n.dot(n) < 0.5 { axis } else { n }
	}
	mut indices := []u32{len: verts.len}
	for i in 0 .. verts.len {
		indices[i] = u32(i)
	}
	return Mesh3D{ vertices: verts, normals: normals, uvs: uvs, indices: indices }
}

// RotationBasis holds the images of the three unit axes under a rotation.
// Computing it once per draw avoids evaluating sin/cos for every vertex.
pub struct RotationBasis {
pub mut:
	x Vec3
	y Vec3
	z Vec3
}

// rotation_basis builds a reusable basis for rot (degrees around each axis).
pub fn rotation_basis(rot Vec3) RotationBasis {
	return RotationBasis{
		x: rotate_3d(Vec3{1, 0, 0}, rot)
		y: rotate_3d(Vec3{0, 1, 0}, rot)
		z: rotate_3d(Vec3{0, 0, 1}, rot)
	}
}

// apply_basis rotates v using a precomputed basis.
pub fn apply_basis(v Vec3, basis RotationBasis) Vec3 {
	return Vec3{
		x: basis.x.x * v.x + basis.y.x * v.y + basis.z.x * v.z
		y: basis.x.y * v.x + basis.y.y * v.y + basis.z.y * v.z
		z: basis.x.z * v.x + basis.y.z * v.y + basis.z.z * v.z
	}
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
