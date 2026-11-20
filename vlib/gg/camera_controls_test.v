module gg_test

import gg
import math

fn close(a f64, b f64) bool {
	return math.abs(a - b) < 1e-3
}

fn test_orbit_tracks_state() {
	mut cam := gg.Camera3D{}
	cam.orbit(90, 0, 10)
	assert cam.yaw == 90
	assert cam.pitch == 0
	assert cam.distance == 10
	cam.orbit_by(10, -10, 2)
	assert close(cam.yaw, 87)
	assert close(cam.pitch, -3)
	assert close(cam.distance, 8)
}

fn test_orbit_clamps_pitch_and_distance() {
	mut cam := gg.Camera3D{}
	cam.orbit(0, 0, 1)
	// pitch += dy*0.3, distance -= zoom: both deltas push past their limits.
	cam.orbit_by(0, -2000, 2000)
	assert close(cam.pitch, -89)
	assert cam.pitch >= -89
	assert close(cam.distance, 0.1)
	assert cam.distance >= 0.1
	cam.orbit_by(0, 2000, 0)
	assert close(cam.pitch, 89)
	assert cam.pitch <= 89
}

fn test_move_forward_preserves_view_direction() {
	mut cam := gg.Camera3D{
		position: gg.Vec3{0, 0, 5}
		target:   gg.Vec3{0, 0, 0}
	}
	cam.move_forward(2)
	assert close(cam.position.z, 3)
	assert close(cam.target.z, -2)
	assert close((cam.target - cam.position).normalized().z, -1)
}

fn test_fly_turns_and_moves() {
	mut cam := gg.Camera3D{
		position: gg.Vec3{0, 0, 0}
		target:   gg.Vec3{0, 0, -1}
	}
	cam.fly(1, 0, 0, 90, 0)
	assert close(cam.position.x, -1)
}

fn test_fly_pitches_after_turning_to_x_axis() {
	mut cam := gg.Camera3D{
		position: gg.Vec3{0, 0, 0}
		target:   gg.Vec3{1, 0, 0}
	}
	cam.fly(0, 0, 0, 0, 30)
	fwd := cam.forward()
	assert close(f64(fwd.x), math.cos(math.pi / 6))
	assert close(f64(fwd.y), math.sin(math.pi / 6))
	assert math.abs(fwd.z) < 1e-3
}
