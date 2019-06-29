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
	// Primary colors
	Blue   = Color { r: 0, g: 0, b: 255 }
	Red    = Color { r: 255, g: 0, b: 0 }
	Yellow = Color { r: 255, g: 255, b: 0 }
	
	// Secondary colors
	Green  = Color { r: 0, g: 255, b: 0 }
	Orange = Color { r: 255, g: 165, b: 0 }
	Purple = Color { r: 128, g: 0, b: 128 }
	
	// Other
	Black  = Color { r: 0, g: 0, b: 0 }
	Gray   = Color { r: 128, g: 128, b: 128 }
	Indigo = Color { r: 75, g: 0, b: 130 }
	Pink   = Color { r: 255, g: 192, b: 203 }
	Violet = Color { r: 238, g: 130, b: 238 }
	White  = Color { r: 255, g: 255, b: 255 }
	
	// Shades
	DarkBlue   = Color { r: 0, g: 0, b: 139 }
	DarkGray   = Color { r: 169, g: 169, b: 169 }
	DarkGreen  = Color { r: 0, g: 100, b: 0 }
	DarkRed    = Color { r: 139, g: 0, b: 0 }
	LightBlue  = Color { r: 173, g: 216, b: 230 }
	LightGray  = Color { r: 211, g: 211, b: 211 }
	LightGreen = Color { r: 144, g: 238, b: 144 }
	LightRed   = Color { r: 255, g: 204, b: 203 }
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
