// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import os
import sokol
import sokol.sgl
import stbi

pub struct Image {
pub mut:
	width       int
	height      int
	nr_channels int
	ok          bool
	data        voidptr
	ext         string
	path string
	cache_slot  &ImageCacheSlot = 0
}
//
struct ImageCacheSlot {
	id         int
mut:
	is_simg_ok bool
	simg       C.sg_image
}
fn (mut cs ImageCacheSlot) set_sokol_image(simg C.sg_image) {
	cs.simg = simg
	cs.is_simg_ok = true
}
//
[ref_only]
struct ImageCache {
mut:
	current_image_id int = -1
	slots []ImageCacheSlot
}
fn (ic &ImageCache) get_new_cache_slot() &ImageCacheSlot {
	mut mic := &ImageCache(0)
	unsafe {
		mic = ic
	}
	mic.current_image_id++
	mic.slots << ImageCacheSlot {
		id: mic.current_image_id
		is_simg_ok: false
	}
	return &mic.slots[mic.slots.len-1]
}
const (
	image_cache = &ImageCache{}
)
//
pub fn  create_image(file string) Image {
	if !os.exists(file) {
		println('gg.create_image(): file not found: $file')
		return Image{} // none
	}
	stb_img := stbi.load(file)
	mut img := Image{
		width: stb_img.width
		height: stb_img.height
		nr_channels: stb_img.nr_channels
		ok: stb_img.ok
		data: stb_img.data
		ext: stb_img.ext
		path: file
		cache_slot: image_cache.get_new_cache_slot()
	}
	return img
}

pub fn create_image_from_memory(buf byteptr, bufsize int) Image {
	stb_img := stbi.load_from_memory(buf, bufsize)
	mut img := Image{
		width: stb_img.width
		height: stb_img.height
		nr_channels: stb_img.nr_channels
		ok: stb_img.ok
		data: stb_img.data
		ext: stb_img.ext
		cache_slot: image_cache.get_new_cache_slot()
	}
	return img
}

pub fn create_image_from_byte_array(b []byte) Image {
	return create_image_from_memory(b.data, b.len)
}

pub fn (img &Image) new_sokol_image() C.sg_image {
	//eprintln('> new_sokol_image from img: $img')
	mut img_desc := C.sg_image_desc{
		width: img.width
		height: img.height
		num_mipmaps: 0
		wrap_u: .clamp_to_edge
		wrap_v: .clamp_to_edge
		label: &byte(0)
		d3d11_texture: 0
	}
	img_desc.content.subimage[0][0] = C.sg_subimage_content{
		ptr: img.data
		size: img.nr_channels * img.width * img.height
	}
	simg := C.sg_make_image(&img_desc)
	return simg
}

pub fn (img &Image) get_sokol_image() C.sg_image {
	slot := img.cache_slot
	if slot.is_simg_ok {
		return slot.simg
	}
	simg := img.new_sokol_image()
	unsafe {
		mut cs := slot
		cs.set_sokol_image(simg)
	}
	return simg
}

pub fn (ctx &Context) draw_image(x, y, width, height f32, img &Image) {
	simg := img.get_sokol_image()
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
	sgl.texture(simg)
	sgl.begin_quads()
	sgl.c4b(255, 255, 255, 255)
	sgl.v2f_t2f(x0, y0,	  u0, v0)
	sgl.v2f_t2f(x1, y0,	  u1, v0)
	sgl.v2f_t2f(x1, y1,	  u1, v1)
	sgl.v2f_t2f(x0, y1,	  u0, v1)
	sgl.end()
	sgl.disable_texture()
}
