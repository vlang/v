// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import gg.render

// draw_material_mesh records a mesh with the given material and camera.
// Fog distances are measured from the camera, so begin_3d(cam) should use it.
pub fn (ctx &Context) draw_material_mesh(mesh &Mesh3D, pos Vec3, rotation Vec3, size f32, mat &Material3D, cam Camera3D) {
	if !mesh.valid() || !ctx.config.depth {
		return
	}
	basis := rotation_basis(rotation)
	mut shade := *mat
	if l := shade.light {
		mut normalized := l
		normalized.direction = normalized.direction.normalized()
		shade.light = normalized
	}
	// Transform and shade each unique vertex once, then expand the indices.
	// Indexed meshes reuse vertices up to six times, so this removes most of
	// the per-fragment setup work on the CPU path and the per-vertex work on
	// the GPU path.
	mut world_verts := []Vec3{len: mesh.vertices.len}
	mut vertex_colors := []Color{len: mesh.vertices.len}
	for i, local in mesh.vertices {
		v := apply_basis(local, basis).scale(size)
		world := Vec3{pos.x + v.x, pos.y + v.y, pos.z + v.z}
		mut n := Vec3{0, 1, 0}
		if mesh.normals.len == mesh.vertices.len {
			n = apply_basis(mesh.normals[i], basis)
		}
		world_verts[i] = world
		vertex_colors[i] = material_color(&shade, world, n, cam)
	}
	mut verts := []Vec3{cap: mesh.indices.len}
	mut colors := []Color{cap: mesh.indices.len}
	mut uvs := []f32{cap: mesh.indices.len * 2}
	for index in mesh.indices {
		verts << world_verts[index]
		colors << vertex_colors[index]
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
