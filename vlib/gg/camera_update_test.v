module gg

import math

fn test_camera_update_orbital_uses_input_state() {
	mut ctx := &gg.Context{}
	ctx.mouse_dx = 10
	ctx.mouse_dy = 0
	ctx.scroll_y = 1
	mut cam := gg.Camera3D{}
	cam.update(.orbital, ctx, 0.016)
	assert math.abs(cam.yaw - 27.0) < 0.1
	assert math.abs(cam.distance - 6.0) < 0.1
}

fn test_camera_update_first_person_moves_on_key() {
	mut ctx := &gg.Context{}
	ctx.pressed_keys[int(.w) % key_code_max] = true
	mut cam := gg.Camera3D{
		position: gg.Vec3{0, 0, 0}
		target:   gg.Vec3{0, 0, -1}
	}
	cam.update(.first_person, ctx, 1.0)
	assert cam.position.z < -5.0
}

fn test_camera_update_custom_does_nothing() {
	mut ctx := &gg.Context{}
	ctx.mouse_dx = 100
	mut cam := gg.Camera3D{}
	before := cam.position
	cam.update(.custom, ctx, 0.016)
	assert cam.position == before
}

fn test_mesh_edges_are_unique() {
	mesh := gen_mesh_cube(gg.Vec3{1, 1, 1})
	edges := mesh_edges(&mesh)
	assert edges.len == 24 // a cube has twelve unique edges
}
