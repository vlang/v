// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module stbi

// note we might need special case for this
import gl

#flag   -I @VROOT/thirdparty/stb_image

#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image.h"
#include "stb_image_write.h"

pub struct Image {
pub:
mut:
	width       int
	height      int
	channels 	int
	data        byteptr
}

pub fn load(path string) Image {
	ext := path.all_after('.')
	mut res := Image{}
	flag := if ext == 'png' { C.STBI_rgb_alpha } else { 0 }
	res.data = C.stbi_load(path.str, &res.width, &res.height,	&res.channels, flag)
	if isnil(res.data) {
		println('stbi image failed to load')
		exit(1)
	}
	return res
}

pub fn (img Image) size() int {
	return img.width * img.height * img.channels
}

pub fn (img Image) gray() Image {
	size := img.size()
	gray_channels := if img.channels == 4 { 2 } else { 1 }
	gray_size := img.width * img.height * gray_channels

	gray_data := malloc(gray_size)
	if isnil(gray_data) {
		println('Unable to allocate memory for the gray image.')
		exit(1)
	}
	mut p := img.data
	mut pg := gray_data
	for  p != img.data + size {
		*pg = (*p + *(p + 1) + *(p + 2))/3
		if img.channels == 4 {
			pg++
			*pg = *(p + 3)
		}
		p = p + img.channels
		pg = pg + gray_channels 
	}
	return Image {
		width	: img.width
		height  : img.height
		channels: gray_channels
		data    : gray_data
	}

}

pub fn (img Image) write_png(file_name string) {
	C.stbi_write_png(file_name.str, img.width, img.height, img.channels, img.data, img.width * img.channels)
}

pub fn (img Image) write_bmp(file_name string) {
	C.stbi_write_bmp(file_name.str, img.width, img.height, img.channels, img.data)
}

pub fn (img Image) write_jpg(file_name string, quality int) {
	C.stbi_write_jpg(file_name.str, img.width, img.height, img.channels, img.data, quality)
}

pub fn (img Image) free() {
	C.stbi_image_free(img.data)
}

pub fn (img Image) tex_image_2d() {
	mut rgb_flag := C.GL_RGB
	if img.ext == 'png' {
		rgb_flag = C.GL_RGBA
	}
	C.glTexImage2D(C.GL_TEXTURE_2D, 0, rgb_flag, img.width, img.height, 0,
		rgb_flag, C.GL_UNSIGNED_BYTE,	img.data)
}

pub fn set_flip_vertically_on_load(val bool) {
	C.stbi_set_flip_vertically_on_load(val)
}

