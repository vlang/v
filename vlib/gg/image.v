// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import gx
import os
import sokol
import sokol.sapp
import sokol.sgl
import sokol.gfx
import stbi

pub struct Image {
pub mut:
	//id int
	width       int
	height      int
	nr_channels int
	ok          bool
	data        voidptr
	ext         string
	simg_ok     bool
	simg        C.sg_image
	path string
}

/*
struct ImageCache {
	id int
mut:
	simg C.sg_image
	ok bool
}
*/

//pub fn (mut ctx Context) create_image(file string) Image {
pub fn  create_image(file string) Image {
	if !os.exists(file) {
		println('gg.create_image(): file not found: $file')
		return Image{} // none
	}
	//id := ctx.img_buf.len
	stb_img := stbi.load(file)
	mut img := Image{
		width: stb_img.width
		height: stb_img.height
		nr_channels: stb_img.nr_channels
		ok: stb_img.ok
		data: stb_img.data
		ext: stb_img.ext
		path: file
		//id: id
	}
	img.init_sokol_image()
	//ctx.img_buf << ImageCache {
		//id: id
	//}
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
	}
	return img
}

pub fn create_image_from_byte_array(b []byte) Image {
	return create_image_from_memory(b.data, b.len)
}

pub fn (mut img Image) init_sokol_image() &Image {
	println('\n init sokol image $img.path ok=$img.simg_ok')
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
	img.simg = C.sg_make_image(&img_desc)
	img.simg_ok = true
	return img
}

fn (mut ctx Context) cache_sokol_image(img &Image) {
	//println('\ncache sokol image $img.path ok=$img.simg_ok')
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
	//ctx.img_cache[img.id].simg = C.sg_make_image(&img_desc)
	//ctx.img_cache[img.id].ok = true
}

pub fn (ctx &Context) draw_image(x, y, width, height f32, img &Image) {
	if !img.simg_ok {
		return
		//ctx.cache_sokol_image(img)
		/*
		unsafe {
			mut image := img
			image.init_sokol_image()
		}
		*/
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
	sgl.v2f_t2f(x0, y0,	  u0, v0)
	sgl.v2f_t2f(x1, y0,	  u1, v0)
	sgl.v2f_t2f(x1, y1,	  u1, v1)
	sgl.v2f_t2f(x0, y1,	  u0, v1)
	sgl.end()
	sgl.disable_texture()
}


