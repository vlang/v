// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

// Vec2 is a two-component vector used by the 2D camera helpers.
pub struct Vec2 {
pub mut:
	x f32
	y f32
}

// add returns the component-wise sum of two vectors.
pub fn (a Vec2) + (b Vec2) Vec2 {
	return Vec2{a.x + b.x, a.y + b.y}
}

// sub returns the component-wise difference of two vectors.
pub fn (a Vec2) - (b Vec2) Vec2 {
	return Vec2{a.x - b.x, a.y - b.y}
}

// scale returns the vector multiplied by a scalar.
pub fn (a Vec2) scale(s f32) Vec2 {
	return Vec2{a.x * s, a.y * s}
}

// Camera2D describes a 2D view, mirroring the small struct used by raylib.
// offset is the screen-space position of target, rotation is in degrees, and
// zoom scales the world around target.
pub struct Camera2D {
pub mut:
	offset   Vec2
	target   Vec2
	zoom     f32 = 1
	rotation f32 = 0
}

// cam_zoom returns the effective zoom, treating a zero zoom as one like raylib.
fn cam_zoom(cam &Camera2D) f32 {
	if cam.zoom == 0 {
		return 1
	}
	return cam.zoom
}

// default_camera2d returns an identity 2D camera with zoom 1.
pub fn default_camera2d() Camera2D {
	return Camera2D{ zoom: 1 }
}

// world_to_screen maps a world-space point to screen pixels for cam.
// width and height are accepted for symmetry with the 3D projection helpers;
// the transform itself depends only on offset, target, zoom and rotation.
pub fn (cam &Camera2D) world_to_screen(p Vec2, width f32, height f32) Vec2 {
	_ = width
	_ = height
	z := cam_zoom(cam)
	rad := cam.rotation * math.pi / 180.0
	c := math.cosf(rad)
	s := math.sinf(rad)
	dx := (p.x - cam.target.x) * z
	dy := (p.y - cam.target.y) * z
	return Vec2{
		x: cam.offset.x + dx * c - dy * s
		y: cam.offset.y + dx * s + dy * c
	}
}

// screen_to_world maps a screen-space point to world coordinates for cam.
// It is the exact inverse of world_to_screen. width and height are accepted
// for symmetry with the 3D picking helpers.
pub fn screen_to_world(p Vec2, cam &Camera2D, width f32, height f32) Vec2 {
	_ = width
	_ = height
	z := cam_zoom(cam)
	rad := cam.rotation * math.pi / 180.0
	c := math.cosf(rad)
	s := math.sinf(rad)
	dx := (p.x - cam.offset.x) / z
	dy := (p.y - cam.offset.y) / z
	return Vec2{
		x: cam.target.x + dx * c + dy * s
		y: cam.target.y - dx * s + dy * c
	}
}

// begin_2d switches the context to camera-controlled 2D drawing.
// The camera transform is folded into the projection matrix so that every
// following draw uses cam; close the scope with end_2d().
pub fn (ctx &Context) begin_2d(cam Camera2D) {
	mut r := ctx.r
	w, h := ctx.view_size()
	z := cam_zoom(&cam)
	rad := cam.rotation * math.pi / 180.0
	r.matrix_mode_projection()
	r.load_identity()
	r.ortho(0, f32(w), f32(h), 0, -1, 1)
	r.translate(cam.offset.x, cam.offset.y, 0)
	r.rotate(rad, 0, 0, 1)
	r.scale(z, z, 1)
	r.translate(-cam.target.x, -cam.target.y, 0)
	r.matrix_mode_modelview()
	r.load_identity()
	r.matrix_mode_texture()
	r.load_identity()
}

// end_2d restores the default 2D projection and identity model matrix.
pub fn (ctx &Context) end_2d() {
	mut r := ctx.r
	r.matrix_mode_projection()
	r.load_identity()
	w, h := ctx.view_size()
	r.ortho(0, f32(w), f32(h), 0, -1, 1)
	r.matrix_mode_modelview()
	r.load_identity()
	r.matrix_mode_texture()
	r.load_identity()
}

// set_viewport sets the render viewport in pixels with a top-left origin.
pub fn (ctx &Context) set_viewport(x int, y int, w int, h int) {
	ctx.r.viewport(x, y, w, h, true)
}
