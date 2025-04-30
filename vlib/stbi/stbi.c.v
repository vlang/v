// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module stbi

@[if trace_stbi_allocations ?]
fn trace_allocation(message string) {
	eprintln(message)
}

@[export: 'stbi__callback_malloc']
fn cb_malloc(s usize) voidptr {
	res := unsafe { malloc(isize(s)) }
	trace_allocation('> stbi__callback_malloc: ${s} => ${ptr_str(res)}')
	return res
}

@[export: 'stbi__callback_realloc']
fn cb_realloc(p voidptr, s usize) voidptr {
	res := unsafe { v_realloc(p, isize(s)) }
	trace_allocation('> stbi__callback_realloc: ${ptr_str(p)} , ${s} => ${ptr_str(res)}')
	return res
}

@[export: 'stbi__callback_free']
fn cb_free(p voidptr) {
	trace_allocation('> stbi__callback_free: ${ptr_str(p)}')
	unsafe { free(p) }
}

#flag -I @VEXEROOT/thirdparty/stb_image
#include "stb_image.h"
#include "stb_image_write.h"
#include "stb_image_resize2.h"
#include "stb_v_header.h"
#flag @VEXEROOT/thirdparty/stb_image/stbi.o

// Image represents an image loaded from file or memory, or an image, produced after resizing
pub struct Image {
pub mut:
	width       int  // the width in pixels in the .data
	height      int  // the height in pixels in the .data
	nr_channels int  // the number of color channels in the .data
	ok          bool // if the image was loaded successfully
	data        &u8 = unsafe { nil } // the actual data/pixels in the image, after reading and potentially converting the image
	ext         string // the extension of the file, from which the image was loaded
	//
	original_nr_channels int // when loaded from memory/disk, this field will contain the original number of channels, based on the data, prior to any conversions. Use only as metadata, not for further conversions.
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

@[params]
pub struct LoadParams {
pub:
	desired_channels int = C.STBI_rgb_alpha // 4 by default (RGBA); desired_channels is the number of color channels, that will be used for representing the image in memory. If set to 0, stbi will figure out the number of channels, based on the original image data.
}

// load loads an image from `path`
// If you do not pass desired_channels: explicitly, it will default to 4 (RGBA),
// The image, will get converted into that internal format, no matter what it was on disk.
// Use desired_channels:0, if you need to keep the channels of the image on disk.
//    Note that displaying such an image, with gg/sokol later, can be a problem.
//    Converting/resizing it, should work fine though.
pub fn load(path string, params LoadParams) !Image {
	ext := path.all_after_last('.')
	mut res := Image{
		ok:          true
		ext:         ext
		nr_channels: params.desired_channels
	}
	res.data = C.stbi_load(&char(path.str), &res.width, &res.height, &res.original_nr_channels,
		params.desired_channels)
	if params.desired_channels == 0 {
		res.nr_channels = res.original_nr_channels
	}
	if isnil(res.data) {
		return error('stbi_image failed to load from "${path}"')
	}
	return res
}

// load_from_memory load an image from a memory buffer
// If you do not pass desired_channels: explicitly, it will default to 4 (RGBA),
// and the image will get converted into that internal format, no matter what it was originally.
// Use desired_channels:0, if you need to keep the channels of the image as they were.
//    Note that displaying such an image, with gg/sokol later, can be a problem.
//    Converting/resizing it, should work fine though.
pub fn load_from_memory(buf &u8, bufsize int, params LoadParams) !Image {
	mut res := Image{
		ok:          true
		nr_channels: params.desired_channels
	}
	res.data = C.stbi_load_from_memory(buf, bufsize, &res.width, &res.height, &res.original_nr_channels,
		params.desired_channels)
	if params.desired_channels == 0 {
		res.nr_channels = res.original_nr_channels
	}
	if isnil(res.data) {
		return error('stbi_image failed to load from memory')
	}
	return res
}

//-----------------------------------------------------------------------------
//
// Resize functions
//
//-----------------------------------------------------------------------------
fn C.stbir_resize_uint8_linear(input_pixels &u8, input_w int, input_h int, input_stride_in_bytes int, output_pixels &u8,
	output_w int, output_h int, output_stride_in_bytes int, num_channels int) int

// resize_uint8 resizes `img` to dimensions of `output_w` and `output_h`
pub fn resize_uint8(img &Image, output_w int, output_h int) !Image {
	mut res := Image{
		ok:                   true
		ext:                  img.ext
		width:                output_w
		height:               output_h
		nr_channels:          img.nr_channels
		original_nr_channels: img.original_nr_channels // preserve the metadata of the original, during resizes
	}

	res.data = cb_malloc(usize(output_w * output_h * img.nr_channels))
	if res.data == 0 {
		return error('stbi_image failed to resize file')
	}

	if 0 == C.stbir_resize_uint8_linear(img.data, img.width, img.height, 0, res.data,
		output_w, output_h, 0, img.nr_channels) {
		return error('stbi_image failed to resize file')
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

// fn C.stbi_write_hdr(filename &char, w int, h int, comp int, buffer &u8) int // buffer &u8 => buffer &f32

// stbi_write_png write on path a PNG file
// row_stride_in_bytes is usually equal to: w * comp
// comp is the number of channels
pub fn stbi_write_png(path string, w int, h int, comp int, buf &u8, row_stride_in_bytes int) ! {
	if 0 == C.stbi_write_png(&char(path.str), w, h, comp, buf, row_stride_in_bytes) {
		return error('stbi_image failed to write png file to "${path}"')
	}
}

// stbi_write_png write on path a BMP file
// comp is the number of channels
pub fn stbi_write_bmp(path string, w int, h int, comp int, buf &u8) ! {
	if 0 == C.stbi_write_bmp(&char(path.str), w, h, comp, buf) {
		return error('stbi_image failed to write bmp file to "${path}"')
	}
}

// stbi_write_png write on path a TGA file
// comp is the number of channels
pub fn stbi_write_tga(path string, w int, h int, comp int, buf &u8) ! {
	if 0 == C.stbi_write_tga(&char(path.str), w, h, comp, buf) {
		return error('stbi_image failed to write tga file to "${path}"')
	}
}

// stbi_write_png write on path a JPG file
// quality select the compression quality of the JPG
// quality is between 1 and 100. Higher quality looks better but results in a bigger image.
// comp is the number of channels
pub fn stbi_write_jpg(path string, w int, h int, comp int, buf &u8, quality int) ! {
	if 0 == C.stbi_write_jpg(&char(path.str), w, h, comp, buf, quality) {
		return error('stbi_image failed to write jpg file to "${path}"')
	}
}

/*
pub fn stbi_write_hdr(path string, w int, h int, comp int, buf &u8) ! {
	if 0 == C.stbi_write_hdr(&char(path.str), w , h , comp , buf){
		return error('stbi_image failed to write hdr file to "$path"')
	}
}
*/
