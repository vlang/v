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
	inv_size     f32  // 1 / size, precomputed for PCF tap stepping
	half_size    f32  // size / 2, precomputed for uv -> texel conversion
	last_target  Vec3 // last transform, so capture_if_dirty can spot changes
	last_extent  f32
	last_near    f32
	last_far     f32
	has_capture  bool
	bias         f32  = 0.0015
	strength     f32  = 0.85 // 0 = no shadow, 1 = fully dark
	soft_samples int  = 0    // 0 = single tap, 1 = 9-tap (3x3) PCF
	auto_update  bool = true // recompute view_proj inside capture_depth (legacy behaviour)
	dirty        bool = true // set by set_transform/mark_dirty, cleared after a capture
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
	size_f := f32(size)
	return &ShadowMap3D{
		size:      size
		depth:     []f32{len: size * size, init: 1.0}
		light_dir: dir
		inv_size:  1.0 / size_f
		half_size: size_f * 0.5
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

// set_transform updates only the light's view-projection matrix; it does not
// rasterise. Set auto_update = false so capture_depth keeps this matrix, then
// call capture_if_dirty to skip captures when nothing has changed.
pub fn (mut s ShadowMap3D) set_transform(target Vec3, extent f32, near f32, far f32) {
	s.view_proj = s.light_view_proj(target, extent, near, far)
	s.last_target = target
	s.last_extent = extent
	s.last_near = near
	s.last_far = far
	s.has_capture = true
	s.dirty = true
}

// mark_dirty forces the next capture_if_dirty call to recapture, for example
// after moving or adding scene geometry (which cannot be detected here).
pub fn (mut s ShadowMap3D) mark_dirty() {
	s.dirty = true
}

// capture_depth records the scene's depth from the light using the CPU
// rasterizer. Call it after recording the same geometry you will draw in the
// main pass: ctx.r must hold the transformed world-space vertices. The shadow
// map can then be attached with ctx.set_shadow_map.
//
// When auto_update is true (the default) the view-projection matrix is
// rebuilt from the arguments, preserving the original behaviour. When it is
// false the matrix installed by set_transform is reused, avoiding a rebuild.
pub fn (mut s ShadowMap3D) capture_depth(ctx &Context, target Vec3, extent f32, near f32, far f32) {
	if s.auto_update {
		s.view_proj = s.light_view_proj(target, extent, near, far)
	}
	s.last_target = target
	s.last_extent = extent
	s.last_near = near
	s.last_far = far
	s.has_capture = true
	// Rasterize once with the light's MVP overriding the recorded uniform.
	frame_light := ctx.r.render_cpu_depth_with_mvp(s.size, s.size, 0xffffffff, 1.0, s.view_proj)
	// The rasterizer's depth buffer is freshly allocated and no longer used by
	// the frame, so adopt it directly instead of cloning the whole size*size
	// buffer (V slices are reference counted, so this is just a refcount bump).
	s.depth = frame_light.depth
	s.dirty = false
}

// capture_if_dirty rasterises only when the map has been invalidated by
// set_transform or mark_dirty. Repeated frames with an unchanged light and
// scene therefore skip the expensive full-scene depth pass entirely.
pub fn (mut s ShadowMap3D) capture_if_dirty(ctx &Context, target Vec3, extent f32, near f32, far f32) {
	// With the legacy auto_update path the transform comes from the arguments,
	// so detect a changed light here. With auto_update = false the caller
	// signals changes through set_transform / mark_dirty instead.
	if s.auto_update && s.has_capture {
		t := s.last_target
		if t.x != target.x || t.y != target.y || t.z != target.z || s.last_extent != extent || s.last_near != near || s.last_far != far {
			s.dirty = true
		}
	}
	if !s.dirty {
		return
	}
	s.capture_depth(ctx, target, extent, near, far)
}

// resize changes the shadow map resolution, reusing the existing depth
// buffer whenever the new size fits in its current capacity.
pub fn (mut s ShadowMap3D) resize(new_size int) ! {
	if new_size < 4 || new_size > 4096 {
		return error('gg.E0032: shadow map size must be between 4 and 4096')
	}
	if new_size == s.size && s.depth.len == new_size * new_size {
		return
	}
	old_len := s.depth.len
	new_len := new_size * new_size
	if new_len <= old_len {
		// Shrinking keeps the backing allocation; the live prefix is already
		// initialised, so no per-texel work is needed.
		s.depth = s.depth[..new_len]
	} else {
		// grow_len grows in place while there is spare capacity and only
		// reallocates when the current buffer is too small.
		unsafe { s.depth.grow_len(new_len - old_len) }
		for i in old_len .. new_len {
			s.depth[i] = 1.0
		}
	}
	size_f := f32(new_size)
	s.size = new_size
	s.inv_size = 1.0 / size_f
	s.half_size = size_f * 0.5
	s.dirty = true
}

// sample returns the visibility factor (1 = lit, 1-strength = shadowed) for
// a world-space point. It compares the point's depth in light space with the
// captured shadow map, using an optional 3x3 PCF filter.
pub fn (s &ShadowMap3D) sample(world Vec3) f32 {
	if s.depth.len != s.size * s.size || s.strength <= 0 {
		return 1.0
	}
	// Read the matrix once; all sixteen coefficients stay in local variables.
	a := s.view_proj.to_array()
	x := a[0] * world.x + a[4] * world.y + a[8] * world.z + a[12]
	y := a[1] * world.x + a[5] * world.y + a[9] * world.z + a[13]
	z := a[2] * world.x + a[6] * world.y + a[10] * world.z + a[14]
	w := a[3] * world.x + a[7] * world.y + a[11] * world.z + a[15]
	if w <= 0 {
		return 1.0
	}
	// One reciprocal shared by all three projected coordinates instead of
	// three separate divisions.
	inv_w := 1.0 / w
	u := (x * inv_w + 1.0) * 0.5
	v := (1.0 - y * inv_w) * 0.5
	depth := z * inv_w
	if u < 0 || u >= 1 || v < 0 || v >= 1 || depth < -1 || depth > 1 {
		return 1.0
	}
	if s.soft_samples > 0 {
		mut vis := f32(0)
		step := s.inv_size
		u0 := u - step
		v0 := v - step
		for dy in 0 .. 3 {
			// The row offset does not depend on dx, so compute it once per row.
			vy := v0 + f32(dy) * step
			for dx in 0 .. 3 {
				vis += s.tap(u0 + f32(dx) * step, vy, depth)
			}
		}
		return vis / 9.0
	}
	return s.tap(u, v, depth)
}

// sample_fast is a bounds-check-free, single-tap version of sample for the
// common case. The caller must ensure the world point is in the light's
// frustum (w > 0); out-of-range texel indices are clamped rather than
// rejected, and no PCF filtering is performed.
@[direct_array_access]
pub fn (s &ShadowMap3D) sample_fast(world Vec3) f32 {
	if s.strength <= 0 || s.depth.len != s.size * s.size {
		return 1.0
	}
	a := s.view_proj.to_array()
	x := a[0] * world.x + a[4] * world.y + a[8] * world.z + a[12]
	y := a[1] * world.x + a[5] * world.y + a[9] * world.z + a[13]
	z := a[2] * world.x + a[6] * world.y + a[10] * world.z + a[14]
	w := a[3] * world.x + a[7] * world.y + a[11] * world.z + a[15]
	if w <= 0 {
		return 1.0
	}
	inv_w := 1.0 / w
	u := (x * inv_w + 1.0) * 0.5
	v := (1.0 - y * inv_w) * 0.5
	size_f := s.half_size * 2.0
	mut px := int(u * size_f)
	mut py := int(v * size_f)
	if px < 0 {
		px = 0
	} else if px >= s.size {
		px = s.size - 1
	}
	if py < 0 {
		py = 0
	} else if py >= s.size {
		py = s.size - 1
	}
	stored := s.depth[py * s.size + px]
	if z * inv_w - s.bias > stored {
		return 1.0 - s.strength
	}
	return 1.0
}

@[direct_array_access]
fn (s &ShadowMap3D) tap(u f32, v f32, depth f32) f32 {
	if u < 0 || u >= 1 || v < 0 || v >= 1 {
		return 1.0
	}
	size_f := s.half_size * 2.0
	mut px := int(u * size_f)
	mut py := int(v * size_f)
	stored := s.depth[py * s.size + px]
	if depth - s.bias > stored {
		return 1.0 - s.strength
	}
	return 1.0
}

// shadow_color applies a shadow map's visibility to an already-lit colour.
pub fn shadow_color(c Color, visibility f32) Color {
	v := if visibility < 0 {
		f32(0)
	} else if visibility > 1 {
		f32(1)
	} else {
		visibility
	}
	return Color{
		r: u8(f32(c.r) * v)
		g: u8(f32(c.g) * v)
		b: u8(f32(c.b) * v)
		a: c.a
	}
}
