// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

// draw_ray draws a ray as a 3D line of unit length.
pub fn (ctx &Context) draw_ray(ray Ray, length f32, color Color) {
	end := ray.origin + ray.direction.scale(length)
	ctx.draw_line_3d(ray.origin, end, color)
}
