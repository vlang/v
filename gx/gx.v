// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module gx

struct Color {
pub:
	r int
	g int
	b int
}

const (
	BLUE      = Color { r: 60, g: 126, b: 197 }
	Blue      = Color { r: 60, g: 126, b: 197 }
	BlueLite  = Color { r: 226, g: 233, b: 241 }
	BLACK     = Color { r: 0, g: 0, b: 0 }
	WHITE     = Color { r: 255, g: 255, b: 255 }
	GRAY      = Color { r: 223, g: 223, b: 223 }
	GRAY_DARK = Color { r: 150, g: 150, b: 150 }
	GRAY_LITE = Color { r: 245, g: 245, b: 245 }
	BLUE_LITE = Color { r: 226, g: 233, b: 241 }
	ORANGE    = Color { r: 255, g: 140, b: 0 }
	GREEN     = Color { r: 0, g: 140, b: 0 }
	RED       = Color { r: 140, g: 0, b: 0 }
	YELLOW    = Color { r: 255, g: 255, b: 0 }
)

const (
	ALIGN_LEFT  = 1
	ALIGN_RIGHT = 4
)

struct TextCfg {
pub:
	color     Color
	size      int
	align     int
	max_width int
	family    string
	bold      bool
	mono      bool
}

struct Image {
mut:
	obj    voidptr
pub:
	id     int
	width  int
	height int
}

pub fn (img Image) is_empty() bool {
	return isnil(img.obj)
}

pub fn (c Color) str() string {
	return '{$c.r, $c.g, $c.b}'
}

pub fn (a Color) eq(b Color) bool {
	return a.r == b.r &&
	a.g == b.g &&
	a.b == b.b
}

pub fn rgb(r, g, b int) Color {
	res := Color {
		r: r,
		g: g,
		b: b,
	}
	return res
}

// fn text_width_char(c char) int {
// return text_width(char2string(c))
// // return C.text_width_char(c)
// }
