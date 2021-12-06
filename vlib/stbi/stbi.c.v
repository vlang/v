// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module stbi

#flag -I @VEXEROOT/thirdparty/stb_image
#include "stb_image.h"
#include "stb_image_write.h"
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
// Load functions
//
//-----------------------------------------------------------------------------
fn C.stbi_load(filename &char, x &int, y &int, channels_in_file &int, desired_channels int) &byte
fn C.stbi_load_from_file(f voidptr, x &int, y &int, channels_in_file &int, desired_channels int) &byte
fn C.stbi_load_from_memory(buffer &byte, len int, x &int, y &int, channels_in_file &int, desired_channels int) &byte

fn C.stbi_image_free(retval_from_stbi_load &byte)
fn C.stbi_set_flip_vertically_on_load(should_flip int)

fn init() {
	set_flip_vertically_on_load(false)
}

pub fn load(path string) ?Image {
	ext := path.all_after_last('.')
	mut res := Image{
		ok: true
		ext: ext
		data: 0
	}
	// flag := if ext == 'png' { C.STBI_rgb_alpha } else { 0 }
	desired_channels := if ext == 'png' { 4 } else { 0 }
	res.data = C.stbi_load(&char(path.str), &res.width, &res.height, &res.nr_channels,
		desired_channels)
	if desired_channels == 4 && res.nr_channels == 3 {
		// Fix an alpha png bug
		res.nr_channels = 4
	}
	if isnil(res.data) {
		return error('stbi image failed to load from "$path"')
	}
	return res
}

pub fn load_from_memory(buf &byte, bufsize int) ?Image {
	mut res := Image{
		ok: true
		data: 0
	}
	flag := C.STBI_rgb_alpha
	res.data = C.stbi_load_from_memory(buf, bufsize, &res.width, &res.height, &res.nr_channels,
		flag)
	if isnil(res.data) {
		return error('stbi image failed to load from memory')
	}
	return res
}

pub fn (img &Image) free() {
	C.stbi_image_free(img.data)
}

/*
pub fn (img Image) tex_image_2d() {
	mut rgb_flag := C.GL_RGB
	if img.ext == 'png' {
		rgb_flag = C.GL_RGBA
	}
	C.glTexImage2D(C.GL_TEXTURE_2D, 0, rgb_flag, img.width, img.height, 0,
		rgb_flag, C.GL_UNSIGNED_BYTE,	img.data)
}
*/

pub fn set_flip_vertically_on_load(val bool) {
	C.stbi_set_flip_vertically_on_load(val)
}

//-----------------------------------------------------------------------------
//
// Write functions
//
//-----------------------------------------------------------------------------

/*
    int stbi_write_png(char const *filename, int w, int h, int comp, const void *data, int stride_in_bytes);
     int stbi_write_bmp(char const *filename, int w, int h, int comp, const void *data);
     int stbi_write_tga(char const *filename, int w, int h, int comp, const void *data);
     int stbi_write_jpg(char const *filename, int w, int h, int comp, const void *data, int quality);
     int stbi_write_hdr(char const *filename, int w, int h, int comp, const float *data);
*/

fn C.stbi_flip_vertically_on_write(flag int)
fn C.stbi_write_png(filename &char, w int, h int, comp int, buffer &byte, stride_in_bytes int)
fn C.stbi_write_bmp(filename &char, w int, h int, comp int, buffer &byte)
fn C.stbi_write_tga(filename &char, w int, h int, comp int, buffer &byte)
fn C.stbi_write_jpg(filename &char, w int, h int, comp int, buffer &byte, quality int)
fn C.stbi_write_hdr(filename &char, w int, h int, comp int, buffer &byte)

pub fn set_flip_vertically_on_write(val bool) {
	C.stbi_flip_vertically_on_write(val)
}

pub fn stbi_write_png(path string, w int, h int, comp int, buf &byte, stride_in_bytes int) ?bool {
	C.stbi_write_png(&char(path.str), w , h , comp , buf, stride_in_bytes)
	return true
}

pub fn stbi_write_bmp(path string, w int, h int, comp int, buf &byte) ?bool {
	C.stbi_write_bmp(&char(path.str), w , h , comp , buf)
	return true
}

pub fn stbi_write_tga(path string, w int, h int, comp int, buf &byte) ?bool {
	C.stbi_write_tga(&char(path.str), w , h , comp , buf)
	return true
}

pub fn stbi_write_jpg(path string, w int, h int, comp int, buf &byte, quality int) ?bool {
	C.stbi_write_jpg(&char(path.str), w , h , comp , buf, quality)
	return true
}

pub fn stbi_write_hdr(path string, w int, h int, comp int, buf &byte) ?bool {
	C.stbi_write_hdr(&char(path.str), w , h , comp , buf)
	return true
}