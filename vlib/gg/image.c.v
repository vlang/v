// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import os
import stbi
import sokol.gfx

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
		ctx.image_cache << img
		return img
	}
	mut img := create_image(file)
	img.id = ctx.image_cache.len
	ctx.image_cache << img
	return img
}

pub fn (mut img Image) init_sokol_image() &Image {
	// println('\n init sokol image $img.path ok=$img.simg_ok')
	mut img_desc := C.sg_image_desc{
		width: img.width
		height: img.height
		num_mipmaps: 0
		wrap_u: .clamp_to_edge
		wrap_v: .clamp_to_edge
		label: img.path.str
		d3d11_texture: 0
	}
	img_desc.data.subimage[0][0] = C.sg_range{
		ptr: img.data
		size: usize(img.nr_channels * img.width * img.height)
	}
	img.simg = C.sg_make_image(&img_desc)
	img.simg_ok = true
	img.ok = true
	return img
}

// draw_image draws the provided image onto the screen
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

	ctx.draw_image_with_config(
		img: img_
		img_rect: Rect{x, y, width, height}
		part_rect: Rect{0, 0, img_.width, img_.height}
	)
}

// new_streaming_image returns a cached `image_idx` of a special image, that
// can be updated *each frame* by calling:  gg.update_pixel_data(image_idx, buf)
// ... where buf is a pointer to the actual pixel data for the image.
// NB: you still need to call app.gg.draw_image after that, to actually draw it.
pub fn (mut ctx Context) new_streaming_image(w int, h int, channels int, sicfg StreamingImageConfig) int {
	mut img := Image{}
	img.width = w
	img.height = h
	img.nr_channels = channels // 4 bytes per pixel for .rgba8, see pixel_format
	mut img_desc := C.sg_image_desc{
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
	img_desc.data.subimage[0][0] = C.sg_range{
		ptr: 0
		size: usize(0)
	}
	img.simg = C.sg_make_image(&img_desc)
	img.simg_ok = true
	img.ok = true
	img_idx := ctx.cache_image(img)
	return img_idx
}

// update_pixel_data is a helper for working with image streams (i.e. images,
// that are updated dynamically by the CPU on each frame)
pub fn (mut ctx Context) update_pixel_data(cached_image_idx int, buf &byte) {
	mut image := ctx.get_cached_image_by_idx(cached_image_idx)
	image.update_pixel_data(buf)
}

pub fn (mut img Image) update_pixel_data(buf &byte) {
	mut data := C.sg_image_data{}
	data.subimage[0][0].ptr = buf
	data.subimage[0][0].size = usize(img.width * img.height * img.nr_channels)
	gfx.update_image(img.simg, &data)
}

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
