// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module stbi

#flag -I @VEXEROOT/thirdparty/stb_image
#include "stb_image.h"
#include "stb_image_write.h"
#include "stb_v_header.h"
#flag @VEXEROOT/thirdparty/stb_image/stbi.o

pub struct Image {
pub mut:
	width       int
	height      int
	nr_channels int
	ok          bool
	data        voidptr
	ext         string
}

//-----------------------------------------------------------------------------
//
// Configuration functions
//
//-----------------------------------------------------------------------------
fn C.stbi_set_flip_vertically_on_load(should_flip int)
fn C.stbi_flip_vertically_on_write(flag int)
fn C.set_png_compression_level(level int)
fn C.write_force_png_filter(level int)
fn C.write_tga_with_rle(level int)

pub fn set_flip_vertically_on_load(val bool) {
	C.stbi_set_flip_vertically_on_load(val)
}

pub fn set_flip_vertically_on_write(val bool) {
	C.stbi_flip_vertically_on_write(val)
}

// set_png_compression_level set the PNG compression level during the writing process
// defaults to 8; set to higher for more compression
pub fn set_png_compression_level(level int) {
	C.set_png_compression_level(level)
}

// write_force_png_filter defaults to -1; set to 0..5 to force a filter mode
// the filter algorithms that can be applied before compression. The purpose of these filters is to prepare the image data for optimum compression.
// Type    Name
//
// 0       None
// 1       Sub
// 2       Up
// 3       Average
// 4       Paeth
pub fn write_force_png_filter(level int) {
	C.write_force_png_filter(level)
}

// stbi_write_tga_with_rle enable/disable the TGA RLE during the writing process
// defaults to true; set to false to disable RLE in tga
pub fn write_tga_with_rle(flag bool) {
	C.write_tga_with_rle(if flag { 1 } else { 0 })
}

//-----------------------------------------------------------------------------
//
// Utility functions
//
//-----------------------------------------------------------------------------
fn C.stbi_image_free(retval_from_stbi_load &u8)

pub fn (img &Image) free() {
	C.stbi_image_free(img.data)
}

//-----------------------------------------------------------------------------
//
// Load functions
//
//-----------------------------------------------------------------------------
fn C.stbi_load(filename &char, x &int, y &int, channels_in_file &int, desired_channels int) &u8
fn C.stbi_load_from_file(f voidptr, x &int, y &int, channels_in_file &int, desired_channels int) &u8
fn C.stbi_load_from_memory(buffer &u8, len int, x &int, y &int, channels_in_file &int, desired_channels int) &u8

// load load an image from a path
pub fn load(path string) ?Image {
	ext := path.all_after_last('.')
	mut res := Image{
		ok: true
		ext: ext
		data: 0
	}
	// flag := if ext == 'png' { C.STBI_rgb_alpha } else { 0 }
	desired_channels := if ext in ['png', 'jpg', 'jpeg'] { 4 } else { 0 }
	res.data = C.stbi_load(&char(path.str), &res.width, &res.height, &res.nr_channels,
		desired_channels)
	if desired_channels == 4 && res.nr_channels == 3 {
		// Fix an alpha png bug
		res.nr_channels = 4
	}
	if isnil(res.data) {
		return error('stbi_image failed to load from "$path"')
	}
	return res
}

// load_from_memory load an image from a memory buffer
pub fn load_from_memory(buf &u8, bufsize int) ?Image {
	mut res := Image{
		ok: true
		data: 0
	}
	flag := C.STBI_rgb_alpha
	res.data = C.stbi_load_from_memory(buf, bufsize, &res.width, &res.height, &res.nr_channels,
		flag)
	if isnil(res.data) {
		return error('stbi_image failed to load from memory')
	}
	return res
}

//-----------------------------------------------------------------------------
//
// Write functions
//
//-----------------------------------------------------------------------------
fn C.stbi_write_png(filename &char, w int, h int, comp int, buffer &u8, stride_in_bytes int) int
fn C.stbi_write_bmp(filename &char, w int, h int, comp int, buffer &u8) int
fn C.stbi_write_tga(filename &char, w int, h int, comp int, buffer &u8) int
fn C.stbi_write_jpg(filename &char, w int, h int, comp int, buffer &u8, quality int) int

// fn C.stbi_write_hdr(filename &char, w int, h int, comp int, buffer &byte) int // buffer &byte => buffer &f32

// stbi_write_png write on path a PNG file
// row_stride_in_bytes is usually equal to: w * comp
pub fn stbi_write_png(path string, w int, h int, comp int, buf &u8, row_stride_in_bytes int) ? {
	if 0 == C.stbi_write_png(&char(path.str), w, h, comp, buf, row_stride_in_bytes) {
		return error('stbi_image failed to write png file to "$path"')
	}
}

// stbi_write_png write on path a BMP file
pub fn stbi_write_bmp(path string, w int, h int, comp int, buf &u8) ? {
	if 0 == C.stbi_write_bmp(&char(path.str), w, h, comp, buf) {
		return error('stbi_image failed to write bmp file to "$path"')
	}
}

// stbi_write_png write on path a TGA file
pub fn stbi_write_tga(path string, w int, h int, comp int, buf &u8) ? {
	if 0 == C.stbi_write_tga(&char(path.str), w, h, comp, buf) {
		return error('stbi_image failed to write tga file to "$path"')
	}
}

// stbi_write_png write on path a JPG file
// quality select teh compression quality of the JPG
// quality is between 1 and 100. Higher quality looks better but results in a bigger image.
pub fn stbi_write_jpg(path string, w int, h int, comp int, buf &u8, quality int) ? {
	if 0 == C.stbi_write_jpg(&char(path.str), w, h, comp, buf, quality) {
		return error('stbi_image failed to write jpg file to "$path"')
	}
}

/*
pub fn stbi_write_hdr(path string, w int, h int, comp int, buf &byte) ? {
	if 0 == C.stbi_write_hdr(&char(path.str), w , h , comp , buf){
		return error('stbi_image failed to write hdr file to "$path"')
	}
}
*/
