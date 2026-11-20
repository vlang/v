module gg

import math

fn test_generated_meshes_are_indexed() {
	cube := gen_mesh_cube(Vec3{2, 2, 2})
	assert cube.valid()
	assert cube.vertices.len == 24
	assert cube.normals.len == 24
	assert cube.uvs.len == 48
	assert cube.indices.len == 36
	plane := gen_mesh_plane(4, 4, 2)
	assert plane.valid()
	assert plane.vertices.len == 9
	assert plane.indices.len == 24
	sphere := gen_mesh_sphere(1, 4, 8)
	assert sphere.valid()
	assert sphere.indices.len == sphere.vertices.len
	cylinder := gen_mesh_cylinder(Vec3{0, 0, 0}, Vec3{0, 1, 0}, 1, 8)
	assert cylinder.valid()
}

fn test_cylinder_normals_are_translation_invariant() {
	a := gen_mesh_cylinder(Vec3{0, 0, 0}, Vec3{0, 1, 0}, 1, 8)
	b := gen_mesh_cylinder(Vec3{10, 0, 0}, Vec3{10, 1, 0}, 1, 8)
	assert a.normals.len == b.normals.len
	for i in 0 .. a.normals.len {
		assert math.abs(a.normals[i].x - b.normals[i].x) < 1e-5
		assert math.abs(a.normals[i].y - b.normals[i].y) < 1e-5
		assert math.abs(a.normals[i].z - b.normals[i].z) < 1e-5
	}
	assert math.abs(a.normals[0].z - 1.0) < 1e-5
}

fn test_mesh_rejects_out_of_range_indices() {
	mut bad := Mesh3D{ vertices: [Vec3{0, 0, 0}], indices: [u32(3)]! }
	assert !bad.valid()
	bad = Mesh3D{ vertices: [Vec3{0, 0, 0}], indices: [u32(0), u32(0)]! }
	assert !bad.valid()
}

fn test_wire_generators_are_valid() {
	sphere := gen_mesh_sphere(1, 4, 8)
	cylinder := gen_mesh_cylinder(Vec3{}, Vec3{0, 1, 0}, 1, 8)
	assert sphere.valid()
	assert cylinder.valid()
	assert sphere.indices.len > 0
	assert cylinder.indices.len > 0
}

fn test_cylinder_cap_normals_point_along_axis() {
	mesh := gen_mesh_cylinder(Vec3{0, 0, 0}, Vec3{0, 10, 0}, 1, 8)
	assert mesh.normals.len == mesh.vertices.len
	for j in 0 .. 8 {
		base := j * 12
		for k in base + 6 .. base + 9 {
			assert math.abs(mesh.normals[k].y + 1) < 1e-5
		}
		for k in base + 9 .. base + 12 {
			assert math.abs(mesh.normals[k].y - 1) < 1e-5
		}
	}
}

fn test_rotate_3d_turns_90_degrees() {
	v := rotate_3d(Vec3{1, 0, 0}, Vec3{0, 90, 0})
	assert math.abs(v.x) < 1e-4
	assert v.z < -0.999
}
