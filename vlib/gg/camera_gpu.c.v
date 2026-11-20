// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math
import sokol.sapp
import gg.render

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

// CameraMode selects how update() drives a camera from gg input state.
pub enum CameraMode {
	custom       // update() does nothing
	orbital      // mouse drag orbits, scroll zooms
	first_person // mouse look + WASD/QE movement
	free         // same as first_person, intended for fly-throughs
}

fn camera_key_down(ctx &Context, code KeyCode) bool {
	return ctx.pressed_keys[int(code) % key_code_max]
}

// update advances a camera from the context's current input state.
// Call it once per frame, typically from the frame or update callback.
pub fn (mut cam Camera3D) update(mode CameraMode, ctx &Context, dt f32) {
	match mode {
		.custom {}
		.orbital {
			cam.orbit_by(f32(ctx.mouse_dx), f32(ctx.mouse_dy), f32(ctx.scroll_y))
		}
		.first_person, .free {
			if ctx.mouse_dx != 0 || ctx.mouse_dy != 0 {
				cam.fly(0, 0, 0, -f32(ctx.mouse_dx) * 0.3, f32(ctx.mouse_dy) * 0.3)
			}
			step := 6.0 * dt
			mut forward := f32(0)
			mut side := f32(0)
			mut rise := f32(0)
			if camera_key_down(ctx, .w) {
				forward += step
			}
			if camera_key_down(ctx, .s) {
				forward -= step
			}
			if camera_key_down(ctx, .d) {
				side += step
			}
			if camera_key_down(ctx, .a) {
				side -= step
			}
			if camera_key_down(ctx, .e) {
				rise += step
			}
			if camera_key_down(ctx, .q) {
				rise -= step
			}
			cam.move_forward(forward)
			cam.move_right(side)
			cam.move_up(rise)
			if ctx.scroll_y != 0 {
				cam.fovy -= f32(ctx.scroll_y)
				if cam.fovy < 10 {
					cam.fovy = 10
				}
				if cam.fovy > 120 {
					cam.fovy = 120
				}
			}
		}
	}
}

@[params]
pub struct Begin3DOptions {
pub:
	cam ?Camera3D // overrides the camera stored with set_camera()
}

// set_camera stores cam on the context so 3D draws need no camera argument.
pub fn (ctx &Context) set_camera(cam Camera3D) {
	mut ctx_mut := unsafe { &Context(ctx) }
	ctx_mut.camera = cam
}

// view_size returns the framebuffer size used for 3D projection and picking.
pub fn (ctx &Context) view_size() (int, int) {
	return sapp.width(), sapp.height()
}

// begin_3d switches the context to depth-tested 3D drawing.
// With no argument it uses the camera stored by set_camera(); pass cam: to
// override it for a single scope. Call after begin() and close with end_3d().
pub fn (ctx &Context) begin_3d(options Begin3DOptions) ! {
	if !ctx.config.depth {
		return error('gg: begin_3d requires Config.depth = true; set depth: true in gg.new_context')
	}
	cam := options.cam or { ctx.camera }
	ctx.r.set_depth_test(true)
	mut r := ctx.r
	fb_w, fb_h := ctx.view_size()
	aspect := if fb_h > 0 { f32(fb_w) / f32(fb_h) } else { 1.0 }
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
	w, h := ctx.view_size()
	r.ortho(0.0, f32(w), f32(h), 0.0, -1.0, 1.0)
	r.matrix_mode_modelview()
	r.load_identity()
	r.matrix_mode_texture()
	r.load_identity()
}
