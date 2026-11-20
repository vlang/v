// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

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

fn edge_key(p Vec3) string {
	return '${int(p.x * 1000.0 + 0.5)},${int(p.y * 1000.0 + 0.5)},${int(p.z * 1000.0 + 0.5)}'
}

// mesh_edges returns each undirected triangle edge once, comparing vertex
// positions so meshes with duplicated face vertices still produce clean wires.
// Edges shared by two coplanar triangles are skipped: they are quad diagonals.
fn mesh_edges(mesh &Mesh3D) []u32 {
	mut faces := map[string][]Vec3{}
	mut first := map[string][2]u32{}
	for t in 0 .. mesh.indices.len / 3 {
		ids := [mesh.indices[t * 3], mesh.indices[t * 3 + 1], mesh.indices[t * 3 + 2]]
		mut n := Vec3{0, 1, 0}
		if mesh.normals.len == mesh.vertices.len {
			n = mesh.normals[ids[0]]
		}
		for k in 0 .. 3 {
			a := ids[k]
			b := ids[(k + 1) % 3]
			ka := edge_key(mesh.vertices[a])
			kb := edge_key(mesh.vertices[b])
			key := if ka < kb { '${ka}|${kb}' } else { '${kb}|${ka}' }
			if key !in first {
				first[key] = [a, b]!
			}
			faces[key] << n
		}
	}
	mut edges := []u32{}
	for key, normals in faces {
		mut interior := false
		if normals.len == 2 && normals[0].dot(normals[1]) > 0.99 {
			interior = true
		}
		if !interior {
			edges << first[key][0]
			edges << first[key][1]
		}
	}
	return edges
}

// draw_mesh_wires draws each mesh edge once, skipping coplanar quad diagonals.
pub fn (ctx &Context) draw_mesh_wires(mesh &Mesh3D, pos Vec3, rotation Vec3, size f32, color Color) {
	if !mesh.valid() {
		return
	}
	edges := mesh_edges(mesh)
	ctx.r.begin_lines()
	for index in edges {
		v := rotate_3d(mesh.vertices[index], rotation).scale(size)
		ctx.r.v3f_c4b(pos.x + v.x, pos.y + v.y, pos.z + v.z, color.r, color.g, color.b, color.a)
	}
	ctx.r.end()
}

// draw_sphere_wires draws a sphere as line segments.
pub fn (ctx &Context) draw_sphere_wires(center Vec3, radius f32, rings int, slices int, color Color) {
	mesh := gen_mesh_sphere(radius, rings, slices)
	ctx.draw_mesh_wires(&mesh, center, Vec3{}, 1.0, color)
}

// draw_cylinder_wires draws a cylinder as line segments.
pub fn (ctx &Context) draw_cylinder_wires(start Vec3, end Vec3, radius f32, slices int, color Color) {
	mesh := gen_mesh_cylinder(start, end, radius, slices)
	ctx.draw_mesh_wires(&mesh, Vec3{}, Vec3{}, 1.0, color)
}
