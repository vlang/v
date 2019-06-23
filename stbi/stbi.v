// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module stbi

import gl

#flag linux -I$HOME/code/v/thirdparty/stb_image
#flag darwin -I$HOME/code/v/thirdparty/stb_image

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

import const (
	GL_RGBA
	GL_RGB
	GL_UNSIGNED_BYTE
	GL_TEXTURE_2D
	STBI_rgb_alpha
)

fn load(path string) Image {
	ext := path.all_after('.')
	mut res := Image {
		ok: true
		ext: ext
		data: 0
	}
	if ext == 'png' {
		res.data = C.stbi_load(path.str, &res.width, &res.height, &res.nr_channels, STBI_rgb_alpha)
	}
	else {
		res.data = C.stbi_load(path.str, &res.width, &res.height, &res.nr_channels, 0)
	}
	if isnil(res.data) {
		println('stbi cant load')
		exit(1)
	}
	return res
}

fn (img Image) free() {
	C.stbi_image_free(img.data)
}

fn (img Image) tex_image_2d() {
	mut rgb_flag := GL_RGB
	if img.ext == 'png' {
		rgb_flag = GL_RGBA
	}
	C.glTexImage2D(GL_TEXTURE_2D, 0, rgb_flag, img.width, img.height, 0, rgb_flag, GL_UNSIGNED_BYTE,
	img.data)
}

fn set_flip_vertically_on_load(val bool) {
	C.stbi_set_flip_vertically_on_load(val)
}

