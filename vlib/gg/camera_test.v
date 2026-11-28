module gg_test

import gg
import math

fn approx(a f64, b f64) bool {
	return math.abs(a - b) < 1e-4
}

fn test_vec3_operators() {
	a := gg.Vec3{1, 2, 3}
	b := gg.Vec3{4, 5, 6}
	c := a + b
	assert c.x == 5 && c.y == 7 && c.z == 9
	d := b - a
	assert d.x == 3 && d.y == 3 && d.z == 3
	e := a.scale(2.0)
	assert e.x == 2 && e.y == 4 && e.z == 6
}

fn test_orbit_places_camera_around_target() {
	mut cam := gg.Camera3D{ target: gg.Vec3{1, 2, 3} }
	cam.orbit(0, 0, 10)
	assert approx(cam.position.x, 1)
	assert approx(cam.position.y, 2)
	assert approx(cam.position.z, 13)
	cam.orbit(90, 0, 10)
	assert approx(cam.position.x, 11)
	assert approx(cam.position.y, 2)
	assert approx(cam.position.z, 3)
}

fn test_perspective_projection_matrix() {
	cam := gg.Camera3D{ fovy: 90, near: 1, far: 100 }
	m := cam.projection_matrix(1.0)
	assert approx(m.v[0][0], 1)
	assert approx(m.v[1][1], 1)
	assert m.v[2][3] == -1.0
}

fn test_orthographic_projection_matrix() {
	cam := gg.Camera3D{
		projection: .orthographic
		ortho_size: 10
		near:       1
		far:        100
	}
	m := cam.projection_matrix(2.0)
	assert approx(m.v[0][0], 0.05)
	assert approx(m.v[1][1], 0.1)
	assert m.v[2][3] == 0.0
}

fn test_view_matrix_looks_at_target() {
	cam := gg.Camera3D{
		position: gg.Vec3{0, 0, 5}
		target:   gg.Vec3{0, 0, 0}
	}
	m := cam.view_matrix()
	assert approx(m.v[3][2], -5)
}
