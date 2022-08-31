// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import gx

// DrawImageConfig struct defines the various options
// that can be used to draw an image onto the screen
pub struct DrawImageConfig {
pub:
	flip_x    bool
	flip_y    bool
	img       &Image = unsafe { nil }
	img_id    int
	img_rect  Rect // defines the size and position on image when rendering to the screen
	part_rect Rect // defines the size and position of part of the image to use when rendering
	rotate    int  // amount to rotate the image in degrees
	z         f32
	color     gx.Color = gx.white
}

// Rect represents a rectangular shape in `gg`.
pub struct Rect {
pub:
	x      f32
	y      f32
	width  f32
	height f32
}

// cache_image caches the image `img` in memory for later reuse.
// cache_image returns the cache index of the cached image.
//
// See also: get_cached_image_by_idx
// See also: remove_cached_image_by_idx
pub fn (mut ctx Context) cache_image(img Image) int {
	ctx.image_cache << img
	image_idx := ctx.image_cache.len - 1
	ctx.image_cache[image_idx].id = image_idx
	return image_idx
}

// get_cached_image_by_idx returns a cached `Image` identified by `image_idx`.
//
// See also: cache_image
// See also: remove_cached_image_by_idx
pub fn (mut ctx Context) get_cached_image_by_idx(image_idx int) &Image {
	return &ctx.image_cache[image_idx]
}

// remove_cached_image_by_idx removes an `Image` identified by `image_idx` from the
// image cache.
//
// See also: cache_image
// See also: get_cached_image_by_idx
pub fn (mut ctx Context) remove_cached_image_by_idx(image_idx int) {
	ctx.image_cache.delete(image_idx)
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
