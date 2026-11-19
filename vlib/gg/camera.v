// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math
import sokol.sapp
import gg.render

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
	len := f32(math.sqrt(a.x * a.x + a.y * a.y + a.z * a.z))
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
	position   Vec3       = Vec3{0, 2, 6}
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

// view_matrix returns the camera's world-to-view matrix.
pub fn (cam Camera3D) view_matrix() render.Mat4 {
	mut m := render.mat4_identity()
	m.lookat(cam.position.x, cam.position.y, cam.position.z, cam.target.x, cam.target.y,
		cam.target.z, cam.up.x, cam.up.y, cam.up.z)
	return m
}

// projection_matrix returns the camera's view-to-clip matrix for aspect.
pub fn (cam Camera3D) projection_matrix(aspect f32) render.Mat4 {
	mut m := render.mat4_identity()
	match cam.projection {
		.perspective {
			m.perspective(cam.fovy * math.pi / 180.0, aspect, cam.near, cam.far)
		}
		.orthographic {
			m.ortho(-cam.ortho_size * aspect, cam.ortho_size * aspect, -cam.ortho_size,
				cam.ortho_size, cam.near, cam.far)
		}
	}
	return m
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

// right returns the normalized right-hand direction of the camera.
pub fn (cam Camera3D) right() Vec3 {
	return cam.forward().cross(cam.up).normalized()
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
	mut look := Vec3{cam.target.x - cam.position.x, cam.target.y - cam.position.y, cam.target.z - cam.position.z}
	if yaw_delta != 0 || pitch_delta != 0 {
		yaw_rad := yaw_delta * math.pi / 180.0
		pitch_rad := pitch_delta * math.pi / 180.0
		cy := f32(math.cos(yaw_rad))
		sy := f32(math.sin(yaw_rad))
		rot_y := Vec3{look.x * cy + look.z * sy, look.y, -look.x * sy + look.z * cy}
		if pitch_delta != 0 {
			right := rot_y.normalized().cross(cam.up).normalized()
			if right.dot(right) > 1e-6 {
				cp := f32(math.cos(pitch_rad))
				sp := f32(math.sin(pitch_rad))
				cross := right.cross(rot_y)
				dot := right.dot(rot_y)
				inv := 1.0 - cp
				look = Vec3{
					x: rot_y.x * cp + cross.x * sp + right.x * dot * inv
					y: rot_y.y * cp + cross.y * sp + right.y * dot * inv
					z: rot_y.z * cp + cross.z * sp + right.z * dot * inv
				}
			} else {
				look = rot_y
			}
		} else {
			look = rot_y
		}
	}
	dir := look.normalized()
	cam.target = cam.position + dir
	cam.move_forward(fwd)
	cam.move_right(side)
	cam.move_up(rise)
}

// begin_3d switches the context to depth-tested 3D drawing with cam.
// Call it after ctx.begin() and close it with ctx.end_3d() before ctx.end().
// 3D drawing requires Config.depth = true.
pub fn (ctx &Context) begin_3d(cam Camera3D) {
	if !ctx.config.depth {
		eprintln('gg: begin_3d() needs Config.depth = true')
		return
	}
	ctx.r.set_depth_test(true)
	mut r := ctx.r
	fb_w := f32(sapp.width())
	fb_h := f32(sapp.height())
	aspect := if fb_h > 0 { fb_w / fb_h } else { 1.0 }
	r.matrix_mode_projection()
	r.load_identity()
	r.mult_mat4(cam.projection_matrix(aspect))
	r.matrix_mode_modelview()
	r.load_identity()
	r.mult_mat4(cam.view_matrix())
	r.matrix_mode_texture()
	r.load_identity()
}

// end_3d restores the 2D projection and painter-order pipelines.
pub fn (ctx &Context) end_3d() {
	ctx.r.set_depth_test(false)
	mut r := ctx.r
	r.matrix_mode_projection()
	r.load_identity()
	r.ortho(0.0, f32(sapp.width()), f32(sapp.height()), 0.0, -1.0, 1.0)
	r.matrix_mode_modelview()
	r.load_identity()
	r.matrix_mode_texture()
	r.load_identity()
}
