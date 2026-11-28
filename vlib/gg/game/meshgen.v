module game

import gg
import math

fn finish_mesh(mut vertices []gg.Vec3, mut indices []u32, mut uvs []f32) gg.Mesh3D {
	mut normals := []gg.Vec3{len: vertices.len}
	for i in 0 .. indices.len / 3 {
		a := indices[i * 3]
		b := indices[i * 3 + 1]
		c := indices[i * 3 + 2]
		ab := vertices[b] - vertices[a]
		ac := vertices[c] - vertices[a]
		n := ab.cross(ac).normalized()
		for idx in [a, b, c] {
			normals[idx] = normals[idx] + n
		}
	}
	for i in 0 .. normals.len {
		normals[i] = normals[i].normalized()
	}
	return gg.Mesh3D{ vertices: vertices, normals: normals, uvs: uvs, indices: indices }
}

// gen_mesh_lathe revolves a 2D profile (x radius, y height) around the Y axis.
// profile needs at least two points and segments at least three.
pub fn gen_mesh_lathe(profile []gg.Vec2, segments int) gg.Mesh3D {
	if profile.len < 2 || segments < 3 {
		return gg.Mesh3D{}
	}
	mut vertices := []gg.Vec3{}
	mut uvs := []f32{}
	mut indices := []u32{}
	for s in 0 .. segments {
		angle := 2 * math.pi * f64(s) / f64(segments)
		ca := f32(math.cos(angle))
		sa := f32(math.sin(angle))
		for p, point in profile {
			vertices << gg.Vec3{point.x * ca, point.y, point.x * sa}
			uvs << f32(s) / f32(segments)
			uvs << f32(p) / f32(profile.len - 1)
		}
	}
	for s in 0 .. segments {
		next := (s + 1) % segments
		for p in 0 .. profile.len - 1 {
			a := u32(s * profile.len + p)
			b := u32(next * profile.len + p)
			c := u32(next * profile.len + p + 1)
			d := u32(s * profile.len + p + 1)
			indices << a
			indices << b
			indices << c
			indices << a
			indices << c
			indices << d
		}
	}
	return finish_mesh(mut vertices, mut indices, mut uvs)
}

// gen_mesh_extrude builds a prism from a 2D polygon and a depth along Z.
// The polygon is triangulated as a fan from its first vertex.
pub fn gen_mesh_extrude(shape []gg.Vec2, depth f32) gg.Mesh3D {
	if shape.len < 3 {
		return gg.Mesh3D{}
	}
	mut vertices := []gg.Vec3{}
	mut uvs := []f32{}
	mut indices := []u32{}
	for p in shape {
		vertices << gg.Vec3{p.x, p.y, 0}
		uvs << p.x
		uvs << p.y
	}
	for p in shape {
		vertices << gg.Vec3{p.x, p.y, depth}
		uvs << p.x
		uvs << p.y + depth
	}
	for i in 1 .. shape.len - 1 {
		indices << 0
		indices << u32(i)
		indices << u32(i + 1)
		base := u32(shape.len)
		indices << base
		indices << base + u32(i + 1)
		indices << base + u32(i)
	}
	for i in 0 .. shape.len {
		next := (i + 1) % shape.len
		n := u32(shape.len)
		a := u32(i)
		b := u32(next)
		indices << a
		indices << b
		indices << n + b
		indices << a
		indices << n + b
		indices << n + a
	}
	return finish_mesh(mut vertices, mut indices, mut uvs)
}

// gen_mesh_tube builds a tube of radius around a polyline of points using
// sides segments per ring. Rings use a simple upright frame.
pub fn gen_mesh_tube(points []gg.Vec3, radius f32, sides int) gg.Mesh3D {
	if points.len < 2 || sides < 3 || radius <= 0 {
		return gg.Mesh3D{}
	}
	mut vertices := []gg.Vec3{}
	mut uvs := []f32{}
	mut indices := []u32{}
	for p, center in points {
		for s in 0 .. sides {
			angle := 2 * math.pi * f64(s) / f64(sides)
			vertices << gg.Vec3{center.x + f32(math.cos(angle)) * radius, center.y + f32(math.sin(angle)) * radius, center.z}
			uvs << f32(s) / f32(sides)
			uvs << f32(p) / f32(points.len - 1)
		}
	}
	for p in 0 .. points.len - 1 {
		for s in 0 .. sides {
			next := (s + 1) % sides
			a := u32(p * sides + s)
			b := u32(p * sides + next)
			c := u32((p + 1) * sides + next)
			d := u32((p + 1) * sides + s)
			indices << a
			indices << b
			indices << c
			indices << a
			indices << c
			indices << d
		}
	}
	return finish_mesh(mut vertices, mut indices, mut uvs)
}
