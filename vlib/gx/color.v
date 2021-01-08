module gx

pub const (
	blue        = Color{
		r: 0
		g: 0
		b: 255
	}
	red         = Color{
		r: 255
		g: 0
		b: 0
	}
	green       = Color{
		r: 0
		g: 255
		b: 0
	}
	yellow      = Color{
		r: 255
		g: 255
		b: 0
	}
	orange      = Color{
		r: 255
		g: 165
		b: 0
	}
	purple      = Color{
		r: 128
		g: 0
		b: 128
	}
	black       = Color{
		r: 0
		g: 0
		b: 0
	}
	gray        = Color{
		r: 128
		g: 128
		b: 128
	}
	indigo      = Color{
		r: 75
		g: 0
		b: 130
	}
	pink        = Color{
		r: 255
		g: 192
		b: 203
	}
	violet      = Color{
		r: 238
		g: 130
		b: 238
	}
	white       = Color{
		r: 255
		g: 255
		b: 255
	}
	dark_blue   = Color{
		r: 0
		g: 0
		b: 139
	}
	dark_gray   = Color{
		r: 169
		g: 169
		b: 169
	}
	dark_green  = Color{
		r: 0
		g: 100
		b: 0
	}
	dark_red    = Color{
		r: 139
		g: 0
		b: 0
	}
	light_blue  = Color{
		r: 173
		g: 216
		b: 230
	}
	light_gray  = Color{
		r: 211
		g: 211
		b: 211
	}
	light_green = Color{
		r: 144
		g: 238
		b: 144
	}
	light_red   = Color{
		r: 255
		g: 204
		b: 203
	}
)

// Color represents a 32 bit color value in sRGB format
pub struct Color {
pub mut:
	r byte
	g byte
	b byte
	a byte = 255
}

// hex takes in a 32 bit integer and splits it into 4 byte values
pub fn hex(color int) Color {
	return Color{
		r: byte((color >> 24) & 0xFF)
		g: byte((color >> 16) & 0xFF)
		b: byte((color >> 8) & 0xFF)
		a: byte((color) & 0xFF)
	}
}

pub fn rgb(r byte, g byte, b byte) Color {
	return Color{
		r: r
		g: g
		b: b
	}
}

pub fn rgba(r byte, g byte, b byte, a byte) Color {
	return Color{
		r: r
		g: g
		b: b
		a: a
	}
}

pub fn (c Color) + (c2 Color) Color {
	return Color{
		r: c.r + c2.r
		g: c.g + c2.g
		b: c.b + c2.b
		a: c.b + c2.a
	}
}

pub fn (c Color) - (c2 Color) Color {
	return Color{
		r: c.r - c2.r
		g: c.g - c2.g
		b: c.b - c2.b
		a: c.b - c2.a
	}
}

pub fn (c Color) * (c2 Color) Color {
	return Color{
		r: c.r * c2.r
		g: c.g * c2.g
		b: c.b * c2.b
		a: c.b * c2.a
	}
}

pub fn (c Color) / (c2 Color) Color {
	return Color{
		r: c.r / c2.r
		g: c.g / c2.g
		b: c.b / c2.b
		a: c.b / c2.a
	}
}

pub fn (c Color) eq(c2 Color) bool {
	return c.r == c2.r && c.g == c2.g && c.b == c2.b && c.a == c2.a
}

pub fn (c Color) str() string {
	return 'Color{$c.r, $c.g, $c.b, $c.a}'
}

// rgba8 - convert a color value to an int in the RGBA8 order.
// see https://developer.apple.com/documentation/coreimage/ciformat
[inline]
pub fn (c Color) rgba8() int {
	return (int(c.r) << 24) + (int(c.g) << 16) + (int(c.b) << 8) + int(c.a)
}

// bgra8 - convert a color value to an int in the BGRA8 order.
// see https://developer.apple.com/documentation/coreimage/ciformat
[inline]
pub fn (c Color) bgra8() int {
	return (int(c.b) << 24) + (int(c.g) << 16) + (int(c.r) << 8) + int(c.a)
}

// abgr8 - convert a color value to an int in the ABGR8 order. 
// see https://developer.apple.com/documentation/coreimage/ciformat
[inline]
pub fn (c Color) abgr8() int {
	return (int(c.a) << 24) + (int(c.b) << 16) + (int(c.g) << 8) + int(c.r)
}

const (
	string_colors = {
		'black': black
		'blue':  blue
		'red':   red
	}
)

pub fn color_from_string(s string) Color {
	return string_colors[s]
}
