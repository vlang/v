// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

// import sokol.sapp
import gx
import sokol.gfx
import os
import sokol
import sokol.sgl
import stbi

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
	simg        C.sg_image
	path        string
}

// DrawImageConfig struct defines the various options
// that can be used to draw an image onto the screen
pub struct DrawImageConfig {
pub:
	flip_x    bool
	flip_y    bool
	img       &Image = voidptr(0)
	img_id    int
	img_rect  Rect // defines the size and position on image when rendering to the screen
	part_rect Rect // defines the size and position of part of the image to use when rendering
	rotate    int  // amount to rotate the image in degrees
	z         f32
	color     gx.Color = gx.white
}

pub struct Rect {
pub:
	x      f32
	y      f32
	width  f32
	height f32
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

pub fn (mut ctx Context) cache_image(img Image) int {
	ctx.image_cache << img
	image_idx := ctx.image_cache.len - 1
	ctx.image_cache[image_idx].id = image_idx
	return image_idx
}

pub fn (mut ctx Context) get_cached_image_by_idx(image_idx int) &Image {
	return &ctx.image_cache[image_idx]
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
		eprintln('gg: draw_image() bad img id $id (img cache len = $ctx.image_cache.len)')
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

	sgl.load_pipeline(ctx.timage_pip)
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

// Draw part of an image using uv coordinates
// img_rect is the size and position (in pixels on screen) of the displayed rectangle (ie the draw_image args)
// part_rect is the size and position (in absolute pixels in the image) of the wanted part
// eg. On a 600*600 context, to display only the first 400*400 pixels of a 2000*2000 image
// on the entire context surface, call :
// draw_image_part(Rect{0, 0, 600, 600}, Rect{0, 0, 400, 400}, img)
pub fn (ctx &Context) draw_image_part(img_rect Rect, part_rect Rect, img_ &Image) {
	ctx.draw_image_with_config(
		img: img_
		img_rect: img_rect
		part_rect: part_rect
	)
}

// draw_image_flipped draws the provided image flipped horizontally (use `draw_image_with_config` to flip vertically)
pub fn (ctx &Context) draw_image_flipped(x f32, y f32, width f32, height f32, img_ &Image) {
	ctx.draw_image_with_config(
		flip_x: true
		img: img_
		img_rect: Rect{x, y, width, height}
	)
}

// draw_image_by_id draws an image by its id
pub fn (ctx &Context) draw_image_by_id(x f32, y f32, width f32, height f32, id int) {
	ctx.draw_image_with_config(
		img_id: id
		img_rect: Rect{x, y, width, height}
	)
}

// draw_image_3d draws an image with a z depth
pub fn (ctx &Context) draw_image_3d(x f32, y f32, z f32, width f32, height f32, img_ &Image) {
	ctx.draw_image_with_config(
		img: img_
		img_rect: Rect{x, y, width, height}
		z: z
	)
}
