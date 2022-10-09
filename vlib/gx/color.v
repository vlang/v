module gx

pub const (
	black = Color{
		r: 0
		g: 0
		b: 0
	}
	gray = Color{
		r: 128
		g: 128
		b: 128
	}
	white = Color{
		r: 255
		g: 255
		b: 255
	}
	red = Color{
		r: 255
		g: 0
		b: 0
	}
	green = Color{
		r: 0
		g: 255
		b: 0
	}
	blue = Color{
		r: 0
		g: 0
		b: 255
	}
	yellow = Color{
		r: 255
		g: 255
		b: 0
	}
	magenta = Color{
		r: 255
		g: 0
		b: 255
	}
	cyan = Color{
		r: 0
		g: 255
		b: 255
	}
	orange = Color{
		r: 255
		g: 165
		b: 0
	}
	purple = Color{
		r: 128
		g: 0
		b: 128
	}
	indigo = Color{
		r: 75
		g: 0
		b: 130
	}
	pink = Color{
		r: 255
		g: 192
		b: 203
	}
	violet = Color{
		r: 238
		g: 130
		b: 238
	}
	dark_blue = Color{
		r: 0
		g: 0
		b: 139
	}
	dark_gray = Color{
		r: 169
		g: 169
		b: 169
	}
	dark_green = Color{
		r: 0
		g: 100
		b: 0
	}
	dark_red = Color{
		r: 139
		g: 0
		b: 0
	}
	light_blue = Color{
		r: 173
		g: 216
		b: 230
	}
	light_gray = Color{
		r: 211
		g: 211
		b: 211
	}
	light_green = Color{
		r: 144
		g: 238
		b: 144
	}
	light_red = Color{
		r: 255
		g: 204
		b: 203
	}
)

// Color represents a 32 bit color value in sRGB format
pub struct Color {
pub mut:
	r u8
	g u8
	b u8
	a u8 = 255
}

// hex takes in a 32 bit integer and splits it into 4 byte values
pub fn hex(color int) Color {
	return Color{
		r: u8((color >> 24) & 0xFF)
		g: u8((color >> 16) & 0xFF)
		b: u8((color >> 8) & 0xFF)
		a: u8(color & 0xFF)
	}
}

// rgb builds a Color instance from given r, g, b values
pub fn rgb(r u8, g u8, b u8) Color {
	return Color{
		r: r
		g: g
		b: b
	}
}

// rgba builds a Color instance from given r, g, b, a values
pub fn rgba(r u8, g u8, b u8, a u8) Color {
	return Color{
		r: r
		g: g
		b: b
		a: a
	}
}

// + adds `b` to `a`, with a maximum value of 255 for each channel
pub fn (a Color) + (b Color) Color {
	mut na := int(a.a) + b.a
	mut nr := int(a.r) + b.r
	mut ng := int(a.g) + b.g
	mut nb := int(a.b) + b.b
	if na > 255 {
		na = 255
	}
	if nr > 255 {
		nr = 255
	}
	if ng > 255 {
		ng = 255
	}
	if nb > 255 {
		nb = 255
	}
	return Color{
		r: u8(nr)
		g: u8(ng)
		b: u8(nb)
		a: u8(na)
	}
}

// - subtracts `b` from `a`, with a minimum value of 0 for each channel
pub fn (a Color) - (b Color) Color {
	mut na := if a.a > b.a { a.a } else { b.a }
	mut nr := int(a.r) - b.r
	mut ng := int(a.g) - b.g
	mut nb := int(a.b) - b.b
	if na < 0 {
		na = 0
	}
	if nr < 0 {
		nr = 0
	}
	if ng < 0 {
		ng = 0
	}
	if nb < 0 {
		nb = 0
	}
	return Color{
		r: u8(nr)
		g: u8(ng)
		b: u8(nb)
		a: u8(na)
	}
}

// * multiplies Color `c` and `c2` keeping channel values in [0, 255] range
pub fn (c Color) * (c2 Color) Color {
	return Color{
		r: c.r * c2.r
		g: c.g * c2.g
		b: c.b * c2.b
		a: c.a * c2.a
	}
}

// / divides `c` by `c2` and converts each channel's value to u8(int)
pub fn (c Color) / (c2 Color) Color {
	return Color{
		r: c.r / c2.r
		g: c.g / c2.g
		b: c.b / c2.b
		a: c.a / c2.a
	}
}

// over - implements an `a` over `b` operation.
// see https://keithp.com/~keithp/porterduff/p253-porter.pdf
pub fn (a Color) over(b Color) Color {
	aa := f32(a.a) / 255
	ab := f32(b.a) / 255
	ar := aa + ab * (1 - aa)
	//
	rr := (f32(a.r) * aa + f32(b.r) * ab * (1 - aa)) / ar
	gr := (f32(a.g) * aa + f32(b.g) * ab * (1 - aa)) / ar
	br := (f32(a.b) * aa + f32(b.b) * ab * (1 - aa)) / ar
	return Color{
		r: u8(rr)
		g: u8(gr)
		b: u8(br)
		a: u8(ar * 255)
	}
}

// eq checks if color `c` and `c2` are equal in every channel
pub fn (c Color) eq(c2 Color) bool {
	return c.r == c2.r && c.g == c2.g && c.b == c2.b && c.a == c2.a
}

// str returns a string representation of the Color `c`
pub fn (c Color) str() string {
	return 'Color{$c.r, $c.g, $c.b, $c.a}'
}

// rgba8 - convert a color value to an int in the RGBA8 order.
// see https://developer.apple.com/documentation/coreimage/ciformat
[inline]
pub fn (c Color) rgba8() int {
	return int(u32(c.r) << 24 | u32(c.g) << 16 | u32(c.b) << 8 | u32(c.a))
}

// bgra8 - convert a color value to an int in the BGRA8 order.
// see https://developer.apple.com/documentation/coreimage/ciformat
[inline]
pub fn (c Color) bgra8() int {
	return int(u32(c.b) << 24 | u32(c.g) << 16 | u32(c.r) << 8 | u32(c.a))
}

// abgr8 - convert a color value to an int in the ABGR8 order.
// see https://developer.apple.com/documentation/coreimage/ciformat
[inline]
pub fn (c Color) abgr8() int {
	return int(u32(c.a) << 24 | u32(c.b) << 16 | u32(c.g) << 8 | u32(c.r))
}

const (
	string_colors = {
		'blue':        blue
		'red':         red
		'green':       green
		'yellow':      yellow
		'orange':      orange
		'purple':      purple
		'black':       black
		'gray':        gray
		'indigo':      indigo
		'pink':        pink
		'violet':      violet
		'white':       white
		'dark_blue':   dark_blue
		'dark_gray':   dark_gray
		'dark_green':  dark_green
		'dark_red':    dark_red
		'light_blue':  light_blue
		'light_gray':  light_gray
		'light_green': light_green
		'light_red':   light_red
	}
)

// color_from_string returns a Color, corresponding to the given string
// or black Color if string is not found in lookup table
pub fn color_from_string(s string) Color {
	return gx.string_colors[s]
}

// to_css_string returns a CSS compatible string e.g. `rgba(10,11,12,13)` of the color `c`.
pub fn (c Color) to_css_string() string {
	return 'rgba($c.r,$c.g,$c.b,$c.a)'
}
