// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import os
import stbi
import sokol.gfx
import sokol.sgl

// Image holds the fileds and data needed to
// represent a bitmap/pixel based image in memory.
[heap]
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
	simg        gfx.Image
	path        string
}

// create_image creates an `Image` from `file`.
// TODO return !Image
pub fn (ctx &Context) create_image(file string) Image {
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
			unsafe {
				ctx.image_cache << img
			}
			return img
		}
	}
	if !gfx.is_valid() {
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
		unsafe {
			ctx.image_cache << img
		}
		return img
	}
	mut img := create_image(file)
	img.id = ctx.image_cache.len
	unsafe {
		ctx.image_cache << img
	}
	return img
}

// init_sokol_image initializes this `Image` for use with the
// sokol graphical backend system.
pub fn (mut img Image) init_sokol_image() &Image {
	// println('\n init sokol image $img.path ok=$img.simg_ok')
	mut img_desc := gfx.ImageDesc{
		width: img.width
		height: img.height
		num_mipmaps: 0
		wrap_u: .clamp_to_edge
		wrap_v: .clamp_to_edge
		label: img.path.str
		d3d11_texture: 0
	}
	img_desc.data.subimage[0][0] = gfx.Range{
		ptr: img.data
		size: usize(img.nr_channels * img.width * img.height)
	}
	img.simg = gfx.make_image(&img_desc)
	img.simg_ok = true
	img.ok = true
	return img
}

// draw_image draws the provided image onto the screen.
pub fn (ctx &Context) draw_image(x f32, y f32, width f32, height f32, img_ &Image) {
	$if macos {
		if img_.id >= ctx.image_cache.len {
			eprintln('gg: draw_image() bad img id ${img_.id} (img cache len = ${ctx.image_cache.len})')
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

	ctx.draw_image_with_config(
		img: img_
		img_rect: Rect{x, y, width, height}
		part_rect: Rect{0, 0, img_.width, img_.height}
	)
}

// new_streaming_image returns a cached `image_idx` of a special image, that
// can be updated *each frame* by calling:  gg.update_pixel_data(image_idx, buf)
// ... where buf is a pointer to the actual pixel data for the image.
// Note: you still need to call app.gg.draw_image after that, to actually draw it.
pub fn (mut ctx Context) new_streaming_image(w int, h int, channels int, sicfg StreamingImageConfig) int {
	mut img := Image{}
	img.width = w
	img.height = h
	img.nr_channels = channels // 4 bytes per pixel for .rgba8, see pixel_format
	mut img_desc := gfx.ImageDesc{
		width: img.width
		height: img.height
		pixel_format: sicfg.pixel_format
		num_slices: 1
		num_mipmaps: 1
		usage: .stream
		wrap_u: sicfg.wrap_u
		wrap_v: sicfg.wrap_v
		min_filter: sicfg.min_filter
		mag_filter: sicfg.mag_filter
		label: img.path.str
	}
	// Sokol requires that streamed images have NO .ptr/.size initially:
	img_desc.data.subimage[0][0] = gfx.Range{
		ptr: 0
		size: usize(0)
	}
	img.simg = gfx.make_image(&img_desc)
	img.simg_ok = true
	img.ok = true
	img_idx := ctx.cache_image(img)
	return img_idx
}

// update_pixel_data is a helper for working with image streams (i.e. images,
// that are updated dynamically by the CPU on each frame)
pub fn (mut ctx Context) update_pixel_data(cached_image_idx int, buf &u8) {
	mut image := ctx.get_cached_image_by_idx(cached_image_idx)
	image.update_pixel_data(buf)
}

// update_pixel_data updates the sokol specific pixel data associated
// with this `Image`.
pub fn (mut img Image) update_pixel_data(buf &u8) {
	mut data := gfx.ImageData{}
	data.subimage[0][0].ptr = buf
	data.subimage[0][0].size = usize(img.width * img.height * img.nr_channels)
	gfx.update_image(img.simg, &data)
}

// create_image_with_size creates an `Image` from `file` in the given
// `width` x `height` dimension.
//
// TODO copypasta
pub fn (mut ctx Context) create_image_with_size(file string, width int, height int) Image {
	if !gfx.is_valid() {
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

// create_image creates an `Image` from `file`.
//
// TODO remove this
fn create_image(file string) Image {
	if !os.exists(file) {
		println('gg.create_image(): file not found: ${file}')
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

// create_image_from_memory creates an `Image` from the
// memory buffer `buf` of size `bufsize`.
//
// See also: create_image_from_byte_array
pub fn (mut ctx Context) create_image_from_memory(buf &u8, bufsize int) Image {
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

// create_image_from_byte_array creates an `Image` from the
// byte array `b`.
//
// See also: create_image_from_memory
pub fn (mut ctx Context) create_image_from_byte_array(b []u8) Image {
	return ctx.create_image_from_memory(b.data, b.len)
}

pub struct StreamingImageConfig {
	pixel_format gfx.PixelFormat = .rgba8
	wrap_u       gfx.Wrap        = .clamp_to_edge
	wrap_v       gfx.Wrap        = .clamp_to_edge
	min_filter   gfx.Filter      = .linear
	mag_filter   gfx.Filter      = .linear
	num_mipmaps  int = 1
	num_slices   int = 1
}

// draw_image_with_config takes in a config that details how the
// provided image should be drawn onto the screen
pub fn (ctx &Context) draw_image_with_config(config DrawImageConfig) {
	id := if !isnil(config.img) { config.img.id } else { config.img_id }
	if id >= ctx.image_cache.len {
		eprintln('gg: draw_image() bad img id ${id} (img cache len = ${ctx.image_cache.len})')
		return
	}

	img := &ctx.image_cache[id]
	if !img.simg_ok {
		return
	}

	mut img_rect := config.img_rect
	if img_rect.width == 0 && img_rect.height == 0 {
		img_rect = Rect{img_rect.x, img_rect.y, img.width, img.height}
	}

	mut part_rect := config.part_rect
	if part_rect.width == 0 && part_rect.height == 0 {
		part_rect = Rect{part_rect.x, part_rect.y, img.width, img.height}
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

	flip_x := config.flip_x
	flip_y := config.flip_y

	mut u0f := if !flip_x { u0 } else { u1 }
	mut u1f := if !flip_x { u1 } else { u0 }
	mut v0f := if !flip_y { v0 } else { v1 }
	mut v1f := if !flip_y { v1 } else { v0 }

	// FIXME: is this okay?
	match config.effect {
		.alpha { sgl.load_pipeline(ctx.pipeline.alpha) }
		.add { sgl.load_pipeline(ctx.pipeline.add) }
	}

	sgl.enable_texture()
	sgl.texture(img.simg)

	if config.rotate != 0 {
		width := img_rect.width * ctx.scale
		height := (if img_rect.height > 0 { img_rect.height } else { img.height }) * ctx.scale

		sgl.push_matrix()
		sgl.translate(x0 + (width / 2), y0 + (height / 2), 0)
		sgl.rotate(sgl.rad(-config.rotate), 0, 0, 1)
		sgl.translate(-x0 - (width / 2), -y0 - (height / 2), 0)
	}

	sgl.begin_quads()
	sgl.c4b(config.color.r, config.color.g, config.color.b, config.color.a)
	sgl.v3f_t2f(x0, y0, config.z, u0f, v0f)
	sgl.v3f_t2f(x1, y0, config.z, u1f, v0f)
	sgl.v3f_t2f(x1, y1, config.z, u1f, v1f)
	sgl.v3f_t2f(x0, y1, config.z, u0f, v1f)
	sgl.end()

	if config.rotate != 0 {
		sgl.pop_matrix()
	}

	sgl.disable_texture()
}
