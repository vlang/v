// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

// Light3D is a directional light. `direction` is the way the light travels.
pub struct Light3D {
pub mut:
	direction Vec3  = Vec3{0, -1, 0}
	color     Color = white
	intensity f32   = 1
}

// Material3D describes how an indexed mesh is shaded.
// Lighting and fog are baked into per-vertex colors, so they work on every
// sokol backend without a custom shader.
pub struct Material3D {
pub mut:
	color     Color  = white
	texture   &Image = unsafe { nil }
	ambient   f32    = 0.25 // 0 = unlit shadow, 1 = fully ambient
	light     ?Light3D
	fog_color Color = Color{0, 0, 0, 255}
	fog_near  f32   = 0
	fog_far   f32   = 0 // 0 disables fog
}

// shade_color multiplies a color's RGB channels by factor and clamps them.
pub fn shade_color(c Color, factor f32) Color {
	mut f := factor
	if f < 0 {
		f = 0
	}
	if f > 1 {
		f = 1
	}
	return Color{
		r: u8(f32(c.r) * f)
		g: u8(f32(c.g) * f)
		b: u8(f32(c.b) * f)
		a: c.a
	}
}

// mix_color linearly interpolates between two colors.
pub fn mix_color(a Color, b Color, t f32) Color {
	mut w := t
	if w < 0 {
		w = 0
	}
	if w > 1 {
		w = 1
	}
	return Color{
		r: u8(f32(a.r) + (f32(b.r) - f32(a.r)) * w)
		g: u8(f32(a.g) + (f32(b.g) - f32(a.g)) * w)
		b: u8(f32(a.b) + (f32(b.b) - f32(a.b)) * w)
		a: u8(f32(a.a) + (f32(b.a) - f32(a.a)) * w)
	}
}

fn material_color_impl(mat &Material3D, world Vec3, normal Vec3, cam Camera3D, light_unit bool) Color {
	mut c := mat.color
	if light := mat.light {
		dir := if light_unit { light.direction } else { light.direction.normalized() }
		mut ndl := -normal.normalized().dot(dir)
		if ndl < 0 {
			ndl = 0
		}
		factor := mat.ambient + (1.0 - mat.ambient) * ndl * light.intensity
		c = shade_color(mix_color(c, light.color, ndl * 0.25), factor)
	}
	if mat.fog_far > mat.fog_near {
		dx := world.x - cam.position.x
		dy := world.y - cam.position.y
		dz := world.z - cam.position.z
		distance := f32(math.sqrt(f64(dx * dx + dy * dy + dz * dz)))
		t := (distance - mat.fog_near) / (mat.fog_far - mat.fog_near)
		c = mix_color(c, mat.fog_color, t)
	}
	return c
}

// material_color computes the final color of a world-space vertex.
pub fn material_color(mat &Material3D, world Vec3, normal Vec3, cam Camera3D) Color {
	return material_color_impl(mat, world, normal, cam, false)
}

// material_color_unit_light is for callers that already normalised the light
// direction, avoiding a redundant sqrt for every vertex of a mesh.
fn material_color_unit_light(mat &Material3D, world Vec3, normal Vec3, cam Camera3D) Color {
	return material_color_impl(mat, world, normal, cam, true)
}
