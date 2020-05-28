// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module gx

pub struct Color {
pub:
	r int
	g int
	b int
}

pub const (
	blue   = Color { r: 0, g: 0, b: 255 }
	red    = Color { r: 255, g: 0, b: 0 }
	green  = Color { r: 0, g: 255, b: 0 }
	yellow = Color { r: 255, g: 255, b: 0 }

	orange = Color { r: 255, g: 165, b: 0 }
	purple = Color { r: 128, g: 0, b: 128 }

	black  = Color { r: 0, g: 0, b: 0 }
	gray   = Color { r: 128, g: 128, b: 128 }
	indigo = Color { r: 75, g: 0, b: 130 }
	pink   = Color { r: 255, g: 192, b: 203 }
	violet = Color { r: 238, g: 130, b: 238 }
	white  = Color { r: 255, g: 255, b: 255 }

	// Shades
	dark_blue   = Color { r: 0, g: 0, b: 139 }
	dark_gray   = Color { r: 169, g: 169, b: 169 }
	dark_green  = Color { r: 0, g: 100, b: 0 }
	dark_red    = Color { r: 139, g: 0, b: 0 }
	light_blue  = Color { r: 173, g: 216, b: 230 }
	light_gray  = Color { r: 211, g: 211, b: 211 }
	light_green = Color { r: 144, g: 238, b: 144 }
	light_red   = Color { r: 255, g: 204, b: 203 }
)

pub const (
	align_left  = 1
	align_right = 4
)

pub struct TextCfg {
pub:
	color     Color
	size      int
	align     int
	max_width int
	family    string
	bold      bool
	mono      bool
}

pub struct Image {
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
	return a.r == b.r &&	a.g == b.g &&	a.b == b.b
}

pub fn rgb(r, g, b int) Color {
	res := Color {
		r: r
		g: g
		b: b
	}
	return res
}

pub fn hex(color int) Color {
  res := Color {
    r: (color >> 16) & 0xFF
    g: (color >> 8) & 0xFF
    b: color & 0xFF
  }
  return res
}

// fn text_width_char(c char) int {
// return text_width(char2string(c))
// // return C.text_width_char(c)
// }
