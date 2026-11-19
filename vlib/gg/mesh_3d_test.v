module gg

import math

fn test_generated_meshes_are_indexed() {
	cube := gen_mesh_cube(Vec3{2, 2, 2})
	assert cube.valid()
	assert cube.vertices.len == 8
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

fn test_mesh_rejects_out_of_range_indices() {
	mut bad := Mesh3D{ vertices: [Vec3{0, 0, 0}], indices: [u32(3)]! }
	assert !bad.valid()
	bad = Mesh3D{ vertices: [Vec3{0, 0, 0}], indices: [u32(0), u32(0)]! }
	assert !bad.valid()
}

fn test_rotate_3d_turns_90_degrees() {
	v := rotate_3d(Vec3{1, 0, 0}, Vec3{0, 90, 0})
	assert math.abs(v.x) < 1e-4
	assert v.z < -0.999
}
