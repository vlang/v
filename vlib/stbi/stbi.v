// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module stbi

// note we might need special case for this
// import gl

#flag   -I @VROOT/thirdparty/stb_image

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"
struct Image {
mut:
	width       int
	height      int
	nr_channels int
	ok          bool
	data        voidptr
	ext         string
}

pub fn load(path string) Image {
	ext := path.all_after('.')
	mut res := Image {
		ok: true
		ext: ext
		data: 0
	}
	flag := if ext == 'png' { C.STBI_rgb_alpha } else { 0 }
	res.data = C.stbi_load(path.str, &res.width, &res.height,	&res.nr_channels, flag)
	if isnil(res.data) {
		println('stbi image failed to load')
		exit(1)
	}
	return res
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

