module gg

fn test_shadow_capture_marks_shadowed_point() {
	mut ctx := new_context_cpu()
	cam := Camera3D{ position: Vec3{0, 4, 9}, target: Vec3{} }
	mat := Material3D{ color: white, ambient: 0.3, light: Light3D{ direction: Vec3{-0.5, -1, -0.3} } }
	plane := gen_mesh_plane(8, 8, 4)
	cube := gen_mesh_cube(Vec3{1, 2, 1})
	ctx.draw_material_mesh(&plane, Vec3{0, -1, 0}, Vec3{}, 1.0, &mat, cam)
	ctx.draw_material_mesh(&cube, Vec3{0, 0, 2}, Vec3{}, 1.0, &mat, cam)
	mut shadow := new_shadow_map3d(64, Vec3{-0.5, -1, -0.3})!
	shadow.strength = 1.0
	shadow.capture_depth(ctx, Vec3{}, 8, 0.1, 40)
	// A point directly beneath the cube along the light direction should
	// have visibility < 1.
	vis := shadow.sample(Vec3{0, -1, 0})
	assert vis < 1.0
}
