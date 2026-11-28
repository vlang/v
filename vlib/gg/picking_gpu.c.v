// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import gg.render

// draw_ray draws a ray as a 3D line of unit length.
pub fn (ctx &Context) draw_ray(ray Ray, length f32, color Color) {
	end := ray.origin + ray.direction.scale(length)
	ctx.draw_line_3d(ray.origin, end, color)
}

// ray_from_mouse builds a world-space ray from a gg mouse event using the
// camera stored with set_camera() and the current logical window size.
pub fn (ctx &Context) ray_from_mouse(e &Event) Ray {
	// Mouse coordinates are in logical pixels (gg divides them by ctx.scale),
	// so use the logical window size rather than physical framebuffer pixels.
	sz := ctx.window_size()
	return screen_to_world_ray(e.mouse_x, e.mouse_y, ctx.camera, f32(sz.width), f32(sz.height))
}

// pick_at returns the id of the nearest collider under the mouse cursor.
pub fn (ctx &Context) pick_at(e &Event, colliders []Collider) ?int {
	return pick_nearest(ctx.ray_from_mouse(e), colliders)
}

// ScreenPoint is a position in framebuffer pixels, y growing downward.
// visible is false when the point is behind the camera or the viewport is empty.
pub struct ScreenPoint {
pub:
	x       f32
	y       f32
	visible bool
}

// world_to_screen projects a world-space point into screen pixels.
pub fn world_to_screen(p Vec3, cam Camera3D, width int, height int) ScreenPoint {
	if width <= 0 || height <= 0 {
		return ScreenPoint{}
	}
	// Orthographic projection has w == 1 for every point, so the clip-w test
	// below cannot detect points behind the camera. Reject those explicitly
	// using view-space depth, which works for both projection modes.
	to_point := p - cam.position
	if to_point.dot(cam.forward()) <= 0 {
		return ScreenPoint{}
	}
	aspect := f32(width) / f32(height)
	mvp := render.mat4_mul(cam.projection_matrix(aspect), cam.view_matrix())
	a := mvp.to_array() // column-major
	x := a[0] * p.x + a[4] * p.y + a[8] * p.z + a[12]
	y := a[1] * p.x + a[5] * p.y + a[9] * p.z + a[13]
	w := a[3] * p.x + a[7] * p.y + a[11] * p.z + a[15]
	// A non-positive w means the point is on or behind the camera plane;
	// projecting it would mirror it into the framebuffer.
	if w <= 0 {
		return ScreenPoint{}
	}
	return ScreenPoint{
		x:       (x / w + 1) * 0.5 * f32(width)
		y:       (1 - y / w) * 0.5 * f32(height)
		visible: true
	}
}

// draw_text_3d draws text anchored at a world-space point. It projects the
// point to screen coordinates and uses gg's regular 2D text renderer, so the
// label is always readable and drawn on top of the 3D scene.
pub fn (ctx &Context) draw_text_3d(text string, pos Vec3, cam Camera3D, cfg TextCfg) {
	// draw_text() takes logical pixel coordinates, matching the mouse event
	// coordinates, so project into the logical window size here too.
	sz := ctx.window_size()
	sp := world_to_screen(pos, cam, sz.width, sz.height)
	if !sp.visible {
		return
	}
	ctx.draw_text(int(sp.x), int(sp.y), text, cfg)
}
