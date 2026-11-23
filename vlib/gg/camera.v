// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

// Vec3 is a three-component vector used by the 3D helpers.
pub struct Vec3 {
pub mut:
	x f32
	y f32
	z f32
}

// add returns the component-wise sum of two vectors.
pub fn (a Vec3) + (b Vec3) Vec3 {
	return Vec3{a.x + b.x, a.y + b.y, a.z + b.z}
}

// sub returns the component-wise difference of two vectors.
pub fn (a Vec3) - (b Vec3) Vec3 {
	return Vec3{a.x - b.x, a.y - b.y, a.z - b.z}
}

// scale returns the vector multiplied by a scalar.
pub fn (a Vec3) scale(s f32) Vec3 {
	return Vec3{a.x * s, a.y * s, a.z * s}
}

// dot returns the dot product of two vectors.
pub fn (a Vec3) dot(b Vec3) f32 {
	return a.x * b.x + a.y * b.y + a.z * b.z
}

// cross returns the cross product of two vectors.
pub fn (a Vec3) cross(b Vec3) Vec3 {
	return Vec3{
		x: a.y * b.z - a.z * b.y
		y: a.z * b.x - a.x * b.z
		z: a.x * b.y - a.y * b.x
	}
}

// normalized returns a unit-length copy, or a zero vector for a zero input.
pub fn (a Vec3) normalized() Vec3 {
	len := math.sqrtf(a.x * a.x + a.y * a.y + a.z * a.z)
	if len == 0 {
		return Vec3{}
	}
	return a.scale(1.0 / len)
}

// Projection selects how a Camera3D maps the scene onto the screen.
pub enum Projection {
	perspective
	orthographic
}

// Camera3D describes a view of a 3D scene. It mirrors the small struct used by
// raylib and macroquad: a position, a target, an up vector, and a projection.
pub struct Camera3D {
pub mut:
	position   Vec3       = Vec3{3.2889, 2.3941, 5.6954} // orbit(30, 20, 7)
	target     Vec3       = Vec3{0, 0, 0}
	up         Vec3       = Vec3{0, 1, 0}
	fovy       f32        = 60 // vertical field of view in degrees (perspective)
	near       f32        = 0.1
	far        f32        = 1000.0
	ortho_size f32        = 10 // half-height of the view in orthographic mode
	projection Projection = .perspective
	// Orbit state. orbit() and orbit_by() keep these in sync with position.
	yaw      f32 = 30
	pitch    f32 = 20
	distance f32 = 7
}

// default_camera3d returns a ready-to-use perspective camera.
pub fn default_camera3d() Camera3D {
	return Camera3D{}
}

// new_camera3d creates a camera at pos looking at target with the default up vector.
pub fn new_camera3d(pos Vec3, target Vec3) Camera3D {
	return Camera3D{ position: pos, target: target }
}

// look_at points the camera at target. An empty up vector means world up.
pub fn (mut cam Camera3D) look_at(target Vec3, up Vec3) {
	cam.target = target
	cam.up = if up.x == 0 && up.y == 0 && up.z == 0 { Vec3{0, 1, 0} } else { up }
}

// orbit places the camera on a sphere around its target using degrees.
pub fn (mut cam Camera3D) orbit(yaw f32, pitch f32, distance f32) {
	cam.yaw = yaw
	cam.pitch = pitch
	cam.distance = distance
	yaw_rad := yaw * math.pi / 180.0
	pitch_rad := pitch * math.pi / 180.0
	cp := f32(math.cos(pitch_rad))
	cam.position = Vec3{
		x: cam.target.x + distance * cp * f32(math.sin(yaw_rad))
		y: cam.target.y + distance * f32(math.sin(pitch_rad))
		z: cam.target.z + distance * cp * f32(math.cos(yaw_rad))
	}
}

// orbit_by adjusts the orbit angles and distance, then repositions the camera.
// dx/dy are mouse deltas in pixels; zoom is a scroll amount.
pub fn (mut cam Camera3D) orbit_by(dx f32, dy f32, zoom f32) {
	// Recover the orbit angles from the current position so earlier movement
	// or direct position edits are not discarded on the next orbit step.
	d := cam.position - cam.target
	dist := math.sqrtf(d.x * d.x + d.y * d.y + d.z * d.z)
	if dist > 1e-4 {
		mut sy := d.y / dist
		if sy > 1 {
			sy = 1
		}
		if sy < -1 {
			sy = -1
		}
		cam.distance = dist
		cam.pitch = f32(math.asin(f64(sy))) * 180.0 / f32(math.pi)
		cam.yaw = f32(math.atan2(f64(d.x), f64(d.z))) * 180.0 / f32(math.pi)
	}
	cam.yaw -= dx * 0.3
	cam.pitch += dy * 0.3
	if cam.pitch > 89 {
		cam.pitch = 89
	}
	if cam.pitch < -89 {
		cam.pitch = -89
	}
	cam.distance -= zoom
	if cam.distance < 0.1 {
		cam.distance = 0.1
	}
	cam.orbit(cam.yaw, cam.pitch, cam.distance)
}

// forward returns the normalized view direction of the camera.
pub fn (cam Camera3D) forward() Vec3 {
	return (cam.target - cam.position).normalized()
}

// camera_right returns a unit vector perpendicular to fwd, using up when it
// is not parallel to fwd and a fixed axis otherwise. Without the fallback a
// top-down camera (forward parallel to up) gets a zero right vector.
fn camera_right(fwd Vec3, up Vec3) Vec3 {
	mut right := fwd.cross(up)
	if right.dot(right) < 1e-8 {
		mut alt := Vec3{1, 0, 0}
		if math.abs(fwd.x) > 0.9 {
			alt = Vec3{0, 0, 1}
		}
		right = fwd.cross(alt)
	}
	return right.normalized()
}

// right returns the normalized right-hand direction of the camera.
pub fn (cam Camera3D) right() Vec3 {
	return camera_right(cam.forward(), cam.up)
}

// move_forward translates the camera along its view direction.
pub fn (mut cam Camera3D) move_forward(amount f32) {
	fwd := cam.forward()
	cam.position = cam.position + fwd.scale(amount)
	cam.target = cam.target + fwd.scale(amount)
}

// move_right translates the camera along its right-hand direction.
pub fn (mut cam Camera3D) move_right(amount f32) {
	r := cam.right()
	cam.position = cam.position + r.scale(amount)
	cam.target = cam.target + r.scale(amount)
}

// move_up translates the camera along its up direction.
pub fn (mut cam Camera3D) move_up(amount f32) {
	cam.position = cam.position + cam.up.scale(amount)
	cam.target = cam.target + cam.up.scale(amount)
}

// fly applies a frame of first-person movement and mouse look.
// The deltas are already scaled by the caller (usually by dt).
pub fn (mut cam Camera3D) fly(fwd f32, side f32, rise f32, yaw_delta f32, pitch_delta f32) {
	mut look := cam.target - cam.position
	if yaw_delta != 0 || pitch_delta != 0 {
		yaw_rad := yaw_delta * math.pi / 180.0
		cy := f32(math.cos(yaw_rad))
		sy := f32(math.sin(yaw_rad))
		look = Vec3{look.x * cy + look.z * sy, look.y, -look.x * sy + look.z * cy}
		if pitch_delta != 0 {
			// Pitch must rotate around the camera's own right axis. Rotating
			// around world X has no effect once the view points along X.
			// camera_right() supplies a fallback axis for vertical views so
			// pitch is not silently dropped there.
			k := camera_right(look.normalized(), cam.up)
			pitch_rad := pitch_delta * math.pi / 180.0
			cp := f32(math.cos(pitch_rad))
			sp := f32(math.sin(pitch_rad))
			// Rodrigues' rotation formula around k.
			look = look.scale(cp) + k.cross(look).scale(sp) + k.scale(k.dot(look) * (1.0 - cp))
		}
	}
	dir := look.normalized()
	cam.target = cam.position + dir
	cam.move_forward(fwd)
	cam.move_right(side)
	cam.move_up(rise)
}
