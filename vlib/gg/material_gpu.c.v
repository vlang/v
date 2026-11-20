// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

// draw_material_mesh records a mesh with the given material and camera.
// Fog distances are measured from the camera, so begin_3d(cam) should use it.
pub fn (ctx &Context) draw_material_mesh(mesh &Mesh3D, pos Vec3, rotation Vec3, size f32, mat &Material3D, cam Camera3D) bool {
	return ctx.draw_material_mesh_s(mesh, pos, rotation, Vec3{size, size, size}, mat, cam)
}

// draw_material_mesh_s draws a shaded mesh with per-axis scale.
// It returns false when the mesh is invalid or the context has no depth buffer.
pub fn (ctx &Context) draw_material_mesh_s(mesh &Mesh3D, pos Vec3, rotation Vec3, size Vec3, mat &Material3D, cam Camera3D) bool {
	if !mesh.valid() || !ctx.config.depth {
		return false
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
		v := apply_basis(local, basis)
		world := Vec3{pos.x + v.x * size.x, pos.y + v.y * size.y, pos.z + v.z * size.z}
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
		cpu_id := register_cpu_image(ctx, img)
		if ctx.pipeline != unsafe { nil } {
			ctx.r.load_pipeline(ctx.pipeline.depth_alpha)
		}
		if img.simg_ok && ctx.pipeline != unsafe { nil } {
			ctx.r.texture(img.simg, img.ssmp)
		} else {
			ctx.r.set_image_id(u32(cpu_id))
		}
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
		return true
	}
	ctx.r.begin_triangles()
	for i, v in verts {
		c := colors[i]
		ctx.r.v3f_c4b(v.x, v.y, v.z, c.r, c.g, c.b, c.a)
	}
	ctx.r.end()
	return true
}

// draw_cube_tex draws a textured cube without constructing a Mesh3D or
// Material3D. The camera stored with set_camera() is used.
pub fn (ctx &Context) draw_cube_tex(pos Vec3, size Vec3, img &Image, color Color) bool {
	mesh := gen_mesh_cube(size)
	mat := Material3D{ color: color, texture: img }
	return ctx.draw_material_mesh(&mesh, pos, Vec3{}, 1.0, &mat, ctx.camera)
}

// draw_sphere_tex draws a textured sphere using the stored camera.
pub fn (ctx &Context) draw_sphere_tex(center Vec3, radius f32, rings int, slices int, img &Image, color Color) bool {
	mesh := gen_mesh_sphere(radius, rings, slices)
	mat := Material3D{ color: color, texture: img }
	return ctx.draw_material_mesh(&mesh, center, Vec3{}, 1.0, &mat, ctx.camera)
}

// draw_billboard draws a camera-facing textured quad at pos.
pub fn (ctx &Context) draw_billboard(cam Camera3D, img &Image, pos Vec3, width f32, height f32, color Color) {
	if !ctx.config.depth || (!img.simg_ok && img.data == unsafe { nil }) {
		return
	}
	cpu_id := register_cpu_image(ctx, img)
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
	if img.simg_ok && ctx.pipeline != unsafe { nil } {
		ctx.r.texture(img.simg, img.ssmp)
	} else {
		ctx.r.set_image_id(u32(cpu_id))
	}
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

// register_cpu_image copies an image's pixels into the CPU rasterizer once
// and returns a stable id. Headless images have no sokol handle, so identity
// is the image pointer plus its buffer signature; replacing or resizing the
// pixel buffer selects a new id instead of reusing stale pixels.
fn register_cpu_image(ctx &Context, img &Image) int {
	channels := if img.nr_channels > 0 && img.nr_channels <= 4 { img.nr_channels } else { 4 }
	key := '${voidptr(img)}|${voidptr(img.data)}|${img.width}|${img.height}|${channels}'
	if id := ctx.cpu_image_ids[key] {
		return id
	}
	id := ctx.cpu_image_next
	mut ctx_mut := unsafe { &Context(ctx) }
	ctx_mut.cpu_image_next++
	ctx_mut.cpu_image_ids[key] = id
	if img.data != unsafe { nil } && img.width > 0 && img.height > 0 {
		filter := if img.texture_filter == .linear { 1 } else { 0 }
		ctx.r.register_image(id, img.data, img.width, img.height, channels, filter)
	}
	return id
}
