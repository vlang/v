// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import gg.render
import math

// ShadowMap3D is a depth texture rendered from a light's point of view.
// It stores one depth value per shadow-map texel; gg compares a fragment's
// light-space depth against it while baking vertex colours, so shadows work
// on both GPU and CPU backends without a custom shader.
pub struct ShadowMap3D {
pub mut:
	size         int
	depth        []f32
	view_proj    render.Mat4
	light_dir    Vec3 // direction the light travels
	bias         f32 = 0.0015
	strength     f32 = 0.85 // 0 = no shadow, 1 = fully dark
	soft_samples int = 0    // 0 = single tap, 1 = 4-tap PCF
}

// new_shadow_map3d creates an empty shadow map of size x size texels.
pub fn new_shadow_map3d(size int, light_dir Vec3) !&ShadowMap3D {
	if size < 4 || size > 4096 {
		return error('gg.E0032: shadow map size must be between 4 and 4096')
	}
	dir := light_dir.normalized()
	if dir.dot(dir) < 0.5 {
		return error('gg.E0033: shadow map light direction must not be zero')
	}
	return &ShadowMap3D{
		size:      size
		depth:     []f32{len: size * size, init: 1.0}
		light_dir: dir
	}
}

// light_view_proj builds the orthographic projection used to render the
// shadow depth from the light's point of view around target with extent.
pub fn (s &ShadowMap3D) light_view_proj(target Vec3, extent f32, near f32, far f32) render.Mat4 {
	eye := target - s.light_dir.scale(extent * 1.5)
	mut view := render.mat4_identity()
	view.lookat(eye.x, eye.y, eye.z, target.x, target.y, target.z, 0, 1, 0)
	mut proj := render.mat4_identity()
	proj.ortho(-extent, extent, -extent, extent, near, far)
	return render.mat4_mul(proj, view)
}

// capture_depth records the scene's depth from the light using the CPU
// rasterizer. Call it after recording the same geometry you will draw in the
// main pass: ctx.r must hold the transformed world-space vertices. The shadow
// map can then be attached with ctx.set_shadow_map.
pub fn (mut s ShadowMap3D) capture_depth(ctx &Context, target Vec3, extent f32, near f32, far f32) {
	s.view_proj = s.light_view_proj(target, extent, near, far)
	frame := ctx.r.render_cpu_depth(s.size, s.size, 0xffffffff, 1.0)
	// Re-render with the light's MVP by temporarily overriding the uniform.
	frame_light := ctx.r.render_cpu_depth_with_mvp(s.size, s.size, 0xffffffff, 1.0, s.view_proj)
	s.depth = frame_light.depth.clone()
	_ = frame
}

// sample returns the visibility factor (1 = lit, 1-strength = shadowed) for
// a world-space point. It compares the point's depth in light space with the
// captured shadow map, using an optional 4-tap PCF filter.
pub fn (s &ShadowMap3D) sample(world Vec3) f32 {
	if s.depth.len != s.size * s.size || s.strength <= 0 {
		return 1.0
	}
	m := s.view_proj
	a := m.to_array()
	x := a[0] * world.x + a[4] * world.y + a[8] * world.z + a[12]
	y := a[1] * world.x + a[5] * world.y + a[9] * world.z + a[13]
	z := a[2] * world.x + a[6] * world.y + a[10] * world.z + a[14]
	w := a[3] * world.x + a[7] * world.y + a[11] * world.z + a[15]
	if w <= 0 {
		return 1.0
	}
	u := (x / w + 1.0) * 0.5
	v := (1.0 - y / w) * 0.5
	depth := z / w
	if u < 0 || u >= 1 || v < 0 || v >= 1 || depth < -1 || depth > 1 {
		return 1.0
	}
	if s.soft_samples > 0 {
		mut vis := f32(0)
		for dy in -1 .. 1 {
			for dx in -1 .. 1 {
				vis += s.tap(u + f32(dx) / f32(s.size), v + f32(dy) / f32(s.size), depth)
			}
		}
		return vis / 9.0
	}
	return s.tap(u, v, depth)
}

fn (s &ShadowMap3D) tap(u f32, v f32, depth f32) f32 {
	if u < 0 || u >= 1 || v < 0 || v >= 1 {
		return 1.0
	}
	px := int(u * f32(s.size))
	py := int(v * f32(s.size))
	stored := s.depth[py * s.size + px]
	if depth - s.bias > stored {
		return 1.0 - s.strength
	}
	return 1.0
}

// shadow_color applies a shadow map's visibility to an already-lit colour.
pub fn shadow_color(c Color, visibility f32) Color {
	return Color{
		r: u8(f32(c.r) * visibility)
		g: u8(f32(c.g) * visibility)
		b: u8(f32(c.b) * visibility)
		a: c.a
	}
}
