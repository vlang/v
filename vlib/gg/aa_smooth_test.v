module gg

import gg.render

fn test_cpu_aa_downsample_size() {
	mut ctx := new_context_cpu()
	ctx.r.begin_triangles()
	ctx.r.v3f_c4b(-1, -1, 0, 255, 0, 0, 255)
	ctx.r.v3f_c4b(1, -1, 0, 255, 0, 0, 255)
	ctx.r.v3f_c4b(0, 1, 0, 255, 0, 0, 255)
	ctx.r.end()
	cam := Camera3D{ position: Vec3{0, 0, 5}, target: Vec3{} }
	frame := ctx.render_cpu_3d_aa(32, 24, black, cam, 2)
	assert frame.width == 32 && frame.height == 24
	assert frame.pixels.len == 32 * 24
	assert frame.depth.len == 32 * 24
}

fn test_cpu_aa_fallback_factor() {
	mut ctx := new_context_cpu()
	frame := ctx.render_cpu_3d_aa(8, 8, black, Camera3D{}, 0)
	assert frame.width == 8 && frame.height == 8
}

fn test_smooth_normals_average() {
	mesh := Mesh3D{
		vertices: [Vec3{0, 0, 0}, Vec3{0, 0, 0}, Vec3{1, 0, 0}]!
		normals:  [Vec3{0, 1, 0}, Vec3{1, 0, 0}, Vec3{0, 0, 1}]!
		indices:  [u32(0), 1, 2]!
	}
	out := smooth_normals(&mesh, 1e-3)
	assert out.len == 3
	// The first two vertices share a position and get the same averaged normal.
	assert (out[0] - out[1]).dot(out[0] - out[1]) < 1e-6
}

fn test_smooth_mesh_preserves_geometry() {
	sphere := gen_mesh_sphere(1, 4, 8)
	sm := smooth_mesh(&sphere)
	assert sm.vertices.len == sphere.vertices.len
	assert sm.indices.len == sphere.indices.len
	assert sm.normals.len == sphere.normals.len
}
