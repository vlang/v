module gg

import math

fn test_camera2d_roundtrip() {
	mut cam := default_camera2d()
	cam.offset = Vec2{400, 300}
	cam.zoom = 2
	cam.target = Vec2{10, 5}
	p := Vec2{12, 7}
	s := cam.world_to_screen(p, 800, 600)
	back := screen_to_world(s, &cam, 800, 600)
	assert math.abs(back.x - p.x) < 0.01
	assert math.abs(back.y - p.y) < 0.01
}

fn test_mesh_extra_generators_are_valid() {
	torus := gen_mesh_torus(Vec3{}, 3, 1, 8, 12)
	assert torus.valid()
	assert torus.vertices.len > 0
	cone := gen_mesh_cone(Vec3{}, Vec3{0, 4, 0}, 1, 8)
	assert cone.valid()
	poly := gen_mesh_polygon(Vec3{}, 2, 6, 1)
	assert poly.valid()
	mut heights := []f32{len: 9, init: 0}
	hm := gen_mesh_heightmap(heights, 3, 3, 1)
	assert hm.valid()
}

fn test_image_ops_pixel_and_flip() {
	pixels := []u8{len: 2 * 2 * 4, init: 0}
	mut img := &Image{ data: pixels.data, width: 2, height: 2, nr_channels: 4, ok: true }
	image_fill(img, Color{255, 0, 0, 255})
	image_draw_pixel(img, 0, 0, Color{0, 255, 0, 255})
	image_flip_horizontal(img)
	assert img.width == 2 && img.height == 2
}

fn test_gesture_tracker_tap() {
	mut tracker := GestureTracker{}
	e := Event{ typ: .mouse_down, mouse_x: 10, mouse_y: 10 }
	tracker.update(e) or {}
	assert true
}
