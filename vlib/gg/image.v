// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

// import gx
// import sokol.sapp
// import sokol.gfx
import os
import sokol
import sokol.sgl
import stbi

pub struct Image {
pub mut:
	id          int
	width       int
	height      int
	nr_channels int
	ok          bool
	data        voidptr
	ext         string
	simg_ok     bool
	simg        C.sg_image
	path        string
}

pub struct Rect {
pub:
	x      f32
	y      f32
	width  f32
	height f32
}

fn C.sg_isvalid() bool

// TODO return ?Image
pub fn (mut ctx Context) create_image(file string) Image {
	// println('\ncreate_image("$file")')
	if !os.exists(file) {
		return Image{}
	}
	$if macos {
		if ctx.native_rendering {
			// return C.darwin_create_image(file)
			mut img := C.darwin_create_image(file)
			// println('created macos image: $img.path w=$img.width')
			// C.printf('p = %p\n', img.data)
			img.id = ctx.image_cache.len
			ctx.image_cache << img
			return img
		}
	}
	if !C.sg_isvalid() {
		// Sokol is not initialized yet, add stbi object to a queue/cache
		// ctx.image_queue << file
		stb_img := stbi.load(file) or { return Image{} }
		img := Image{
			width: stb_img.width
			height: stb_img.height
			nr_channels: stb_img.nr_channels
			ok: false
			data: stb_img.data
			ext: stb_img.ext
			path: file
			id: ctx.image_cache.len
		}
		ctx.image_cache << img
		return img
	}
	mut img := create_image(file)
	img.id = ctx.image_cache.len
	ctx.image_cache << img
	return img
}

// TODO copypasta
pub fn (mut ctx Context) create_image_with_size(file string, width int, height int) Image {
	if !C.sg_isvalid() {
		// Sokol is not initialized yet, add stbi object to a queue/cache
		// ctx.image_queue << file
		stb_img := stbi.load(file) or { return Image{} }
		img := Image{
			width: width
			height: height
			nr_channels: stb_img.nr_channels
			ok: false
			data: stb_img.data
			ext: stb_img.ext
			path: file
			id: ctx.image_cache.len
		}
		ctx.image_cache << img
		return img
	}
	mut img := create_image(file)
	img.id = ctx.image_cache.len
	ctx.image_cache << img
	return img
}

// TODO remove this
fn create_image(file string) Image {
	if !os.exists(file) {
		println('gg.create_image(): file not found: $file')
		return Image{} // none
	}
	stb_img := stbi.load(file) or { return Image{} }
	mut img := Image{
		width: stb_img.width
		height: stb_img.height
		nr_channels: stb_img.nr_channels
		ok: stb_img.ok
		data: stb_img.data
		ext: stb_img.ext
		path: file
	}
	img.init_sokol_image()
	return img
}

pub fn (mut ctx Context) create_image_from_memory(buf &byte, bufsize int) Image {
	stb_img := stbi.load_from_memory(buf, bufsize) or { return Image{} }
	mut img := Image{
		width: stb_img.width
		height: stb_img.height
		nr_channels: stb_img.nr_channels
		ok: stb_img.ok
		data: stb_img.data
		ext: stb_img.ext
		id: ctx.image_cache.len
	}
	ctx.image_cache << img
	return img
}

pub fn (mut ctx Context) create_image_from_byte_array(b []byte) Image {
	return ctx.create_image_from_memory(b.data, b.len)
}

pub fn (mut img Image) init_sokol_image() &Image {
	// println('\n init sokol image $img.path ok=$img.simg_ok')
	mut img_desc := C.sg_image_desc{
		width: img.width
		height: img.height
		num_mipmaps: 0
		wrap_u: .clamp_to_edge
		wrap_v: .clamp_to_edge
		label: &char(0)
		d3d11_texture: 0
	}
	img_desc.data.subimage[0][0] = C.sg_range{
		ptr: img.data
		size: size_t(img.nr_channels * img.width * img.height)
	}
	img.simg = C.sg_make_image(&img_desc)
	img.simg_ok = true
	img.ok = true
	return img
}

// Draw part of an image using uv coordinates
// img_rect is the size and position (in pixels on screen) of the displayed rectangle (ie the draw_image args)
// part_rect is the size and position (in absolute pixels in the image) of the wanted part
// eg. On a 600*600 context, to display only the first 400*400 pixels of a 2000*2000 image
// on the entire context surface, call :
// draw_image_part(Rect{0, 0, 600, 600}, Rect{0, 0, 400, 400}, img)
pub fn (ctx &Context) draw_image_part(img_rect Rect, part_rect Rect, img_ &Image) {
	if img_.id >= ctx.image_cache.len {
		eprintln('gg: draw_image() bad img id $img_.id (img cache len = $ctx.image_cache.len)')
		return
	}
	img := ctx.image_cache[img_.id] // fetch the image from cache

	if !img.simg_ok {
		return
	}
	u0 := part_rect.x / img.width
	v0 := part_rect.y / img.height
	u1 := (part_rect.x + part_rect.width) / img.width
	v1 := (part_rect.y + part_rect.height) / img.height
	x0 := img_rect.x * ctx.scale
	y0 := img_rect.y * ctx.scale
	x1 := (img_rect.x + img_rect.width) * ctx.scale
	mut y1 := (img_rect.y + img_rect.height) * ctx.scale
	if img_rect.height == 0 {
		scale := f32(img.width) / f32(img_rect.width)
		y1 = f32(img_rect.y + int(f32(img.height) / scale)) * ctx.scale
	}
	//
	sgl.load_pipeline(ctx.timage_pip)
	sgl.enable_texture()
	sgl.texture(img.simg)
	sgl.begin_quads()
	sgl.c4b(255, 255, 255, 255)
	sgl.v2f_t2f(x0, y0, u0, v0)
	sgl.v2f_t2f(x1, y0, u1, v0)
	sgl.v2f_t2f(x1, y1, u1, v1)
	sgl.v2f_t2f(x0, y1, u0, v1)
	sgl.end()
	sgl.disable_texture()
}

pub fn (ctx &Context) draw_image(x f32, y f32, width f32, height f32, img_ &Image) {
	$if macos {
		if img_.id >= ctx.image_cache.len {
			eprintln('gg: draw_image() bad img id $img_.id (img cache len = $ctx.image_cache.len)')
			return
		}
		if ctx.native_rendering {
			if img_.width == 0 {
				return
			}
			if !os.exists(img_.path) {
				return
			}
			C.darwin_draw_image(x, ctx.height - (y + height), width, height, img_)
			return
		}
	}

	ctx.draw_image_part(Rect{x, y, width, height}, Rect{0, 0, img_.width, img_.height},
		img_)
}

// TODO remove copy pasta, merge the functions
pub fn (ctx &Context) draw_image_flipped(x f32, y f32, width f32, height f32, img_ &Image) {
	if img_.id >= ctx.image_cache.len {
		eprintln('gg: draw_image_flipped() bad img id $img_.id (img cache len = $ctx.image_cache.len)')
		return
	}
	img := ctx.image_cache[img_.id] // fetch the image from cache
	if !img.simg_ok {
		return
	}
	u0 := f32(0.0)
	v0 := f32(0.0)
	u1 := f32(1.0)
	v1 := f32(1.0)
	x0 := f32(x) * ctx.scale
	y0 := f32(y) * ctx.scale
	x1 := f32(x + width) * ctx.scale
	y1 := f32(y + height) * ctx.scale
	//
	sgl.load_pipeline(ctx.timage_pip)
	sgl.enable_texture()
	sgl.texture(img.simg)
	sgl.begin_quads()
	sgl.c4b(255, 255, 255, 255)
	sgl.v2f_t2f(x0, y0, u1, v0)
	sgl.v2f_t2f(x1, y0, u0, v0)
	sgl.v2f_t2f(x1, y1, u0, v1)
	sgl.v2f_t2f(x0, y1, u1, v1)
	sgl.end()
	sgl.disable_texture()
}

pub fn (ctx &Context) draw_image_by_id(x f32, y f32, width f32, height f32, id int) {
	img := ctx.image_cache[id]
	ctx.draw_image(x, y, width, height, img)
}

pub fn (ctx &Context) draw_image_3d(x f32, y f32, z f32, width f32, height f32, img_ &Image) {
	if img_.id >= ctx.image_cache.len {
		eprintln('gg: draw_image_3d() bad img id $img_.id (img cache len = $ctx.image_cache.len)')
		return
	}
	img := ctx.image_cache[img_.id] // fetch the image from cache
	if !img.simg_ok {
		return
	}
	u0 := f32(0.0)
	v0 := f32(0.0)
	u1 := f32(1.0)
	v1 := f32(1.0)
	x0 := f32(x) * ctx.scale
	y0 := f32(y) * ctx.scale
	x1 := f32(x + width) * ctx.scale
	y1 := f32(y + height) * ctx.scale
	//
	sgl.load_pipeline(ctx.timage_pip)
	sgl.enable_texture()
	sgl.texture(img.simg)
	sgl.begin_quads()
	sgl.c4b(255, 255, 255, 255)
	sgl.v3f_t2f(x0, y0, z, u0, v0)
	sgl.v3f_t2f(x1, y0, z, u1, v0)
	sgl.v3f_t2f(x1, y1, z, u1, v1)
	sgl.v3f_t2f(x0, y1, z, u0, v1)
	sgl.end()
	sgl.disable_texture()
}
