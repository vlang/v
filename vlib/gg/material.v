// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math
import gg.render

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

// material_color computes the final color of a world-space vertex.
pub fn material_color(mat &Material3D, world Vec3, normal Vec3, cam Camera3D) Color {
	mut c := mat.color
	if light := mat.light {
		mut ndl := -normal.normalized().dot(light.direction.normalized())
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

// draw_material_mesh records a mesh with the given material and camera.
// Fog distances are measured from the camera, so begin_3d(cam) should use it.
pub fn (ctx &Context) draw_material_mesh(mesh &Mesh3D, pos Vec3, rotation Vec3, size f32, mat &Material3D, cam Camera3D) {
	if !mesh.valid() || !ctx.config.depth {
		return
	}
	mut verts := []Vec3{cap: mesh.indices.len}
	mut colors := []Color{cap: mesh.indices.len}
	mut uvs := []f32{cap: mesh.indices.len * 2}
	for index in mesh.indices {
		local := mesh.vertices[index]
		v := rotate_3d(local, rotation).scale(size)
		world := Vec3{pos.x + v.x, pos.y + v.y, pos.z + v.z}
		mut n := Vec3{0, 1, 0}
		if mesh.normals.len == mesh.vertices.len {
			n = rotate_3d(mesh.normals[index], rotation).normalized()
		}
		verts << world
		colors << material_color(mat, world, n, cam)
		if mesh.uvs.len == mesh.vertices.len * 2 {
			uvs << mesh.uvs[index * 2]
			uvs << mesh.uvs[index * 2 + 1]
		} else {
			uvs << 0
			uvs << 0
		}
	}
	if !isnil(mat.texture) && (mat.texture.simg_ok || mat.texture.data != unsafe { nil }) {
		img := mat.texture
		register_cpu_image(ctx.r, img)
		if ctx.pipeline != unsafe { nil } {
			ctx.r.load_pipeline(ctx.pipeline.depth_alpha)
		}
		ctx.r.enable_texture()
		ctx.r.texture(img.simg, img.ssmp)
		ctx.r.begin_triangles()
		for i, v in verts {
			c := colors[i]
			ctx.r.v3f_t2f_c4b(v.x, v.y, v.z, uvs[i * 2], uvs[i * 2 + 1], c.r, c.g, c.b, c.a)
		}
		ctx.r.end()
		ctx.r.disable_texture()
		if ctx.pipeline != unsafe { nil } {
			ctx.r.load_default_pipeline()
		}
		return
	}
	ctx.r.begin_triangles()
	for i, v in verts {
		c := colors[i]
		ctx.r.v3f_c4b(v.x, v.y, v.z, c.r, c.g, c.b, c.a)
	}
	ctx.r.end()
}

// draw_billboard draws a camera-facing textured quad at pos.
pub fn (ctx &Context) draw_billboard(cam Camera3D, img &Image, pos Vec3, width f32, height f32, color Color) {
	if !ctx.config.depth || (!img.simg_ok && img.data == unsafe { nil }) {
		return
	}
	register_cpu_image(ctx.r, img)
	forward := cam.forward()
	right := cam.right()
	up := right.cross(forward).normalized()
	hw := width / 2.0
	hh := height / 2.0
	p0 := pos - right.scale(hw) - up.scale(hh)
	p1 := pos + right.scale(hw) - up.scale(hh)
	p2 := pos + right.scale(hw) + up.scale(hh)
	p3 := pos - right.scale(hw) + up.scale(hh)
	if ctx.pipeline != unsafe { nil } {
		ctx.r.load_pipeline(ctx.pipeline.depth_alpha)
	}
	ctx.r.enable_texture()
	ctx.r.texture(img.simg, img.ssmp)
	ctx.r.begin_triangles()
	points := [p0, p1, p2, p0, p2, p3]!
	uv := [f32(0), 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1]!
	for i, p in points {
		ctx.r.v3f_t2f_c4b(p.x, p.y, p.z, uv[i * 2], uv[i * 2 + 1], color.r, color.g, color.b, color.a)
	}
	ctx.r.end()
	ctx.r.disable_texture()
	if ctx.pipeline != unsafe { nil } {
		ctx.r.load_default_pipeline()
	}
}

// register_cpu_image copies an image's pixels into the CPU rasterizer once,
// so textured 3D draws work without a GPU.
fn register_cpu_image(r &render.Context, img &Image) {
	if img.data == unsafe { nil } || img.width <= 0 || img.height <= 0 {
		return
	}
	id := int(img.simg.id)
	if r.is_image_registered(id) {
		return
	}
	filter := if img.texture_filter == .linear { 1 } else { 0 }
	r.register_image(id, img.data, img.width, img.height, 4, filter)
}
