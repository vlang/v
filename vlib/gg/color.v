module gg

pub const black = Color{
	r: 0
	g: 0
	b: 0
}
pub const gray = Color{
	r: 128
	g: 128
	b: 128
}
pub const white = Color{
	r: 255
	g: 255
	b: 255
}
pub const red = Color{
	r: 255
	g: 0
	b: 0
}
pub const green = Color{
	r: 0
	g: 255
	b: 0
}
pub const blue = Color{
	r: 0
	g: 0
	b: 255
}
pub const yellow = Color{
	r: 255
	g: 255
	b: 0
}
pub const magenta = Color{
	r: 255
	g: 0
	b: 255
}
pub const cyan = Color{
	r: 0
	g: 255
	b: 255
}
pub const orange = Color{
	r: 255
	g: 165
	b: 0
}
pub const purple = Color{
	r: 128
	g: 0
	b: 128
}
pub const indigo = Color{
	r: 75
	g: 0
	b: 130
}
pub const pink = Color{
	r: 255
	g: 192
	b: 203
}
pub const violet = Color{
	r: 238
	g: 130
	b: 238
}
pub const dark_blue = Color{
	r: 0
	g: 0
	b: 139
}
pub const dark_gray = Color{
	r: 169
	g: 169
	b: 169
}
pub const dark_green = Color{
	r: 0
	g: 100
	b: 0
}
pub const dark_red = Color{
	r: 139
	g: 0
	b: 0
}
pub const light_blue = Color{
	r: 173
	g: 216
	b: 230
}
pub const light_gray = Color{
	r: 211
	g: 211
	b: 211
}
pub const light_green = Color{
	r: 144
	g: 238
	b: 144
}
pub const light_red = Color{
	r: 255
	g: 204
	b: 203
}

// Color represents a 32 bit color value in sRGB format
@[markused]
pub struct Color {
pub mut:
	r u8
	g u8
	b u8
	a u8 = 255
}

// hex takes in a 32 bit integer and splits its lower 24 bits into 3 byte values r, g, b.
// The top byte is ignored, so an ARGB value like 0xAARRGGBB loses its alpha;
// the resulting alpha is always 255 (opaque). Use rgba() or color_from_string()
// with the #RRGGBBAA form when an explicit alpha is needed.
pub fn hex(color u32) Color {
	return Color{
		r: u8((color >> 16) & 0xFF)
		g: u8((color >> 8) & 0xFF)
		b: u8((color >> 0) & 0xFF)
	}
}

// frgb builds a Color instance from the given floating point values (between 0.0 and 1.0) r, g, b
@[inline]
pub fn frgb[T](r T, g T, b T) Color {
	return frgba(r, g, b, 1.0)
}

// clamp01 clamps the floating point value `x` to the [0.0, 1.0] range.
// NaN maps to 0.0, values below 0.0 map to 0.0, values above 1.0 map to 1.0.
@[inline]
fn clamp01[T](x T) f64 {
	v := f64(x)
	if v != v {
		return 0.0
	}
	if v < 0.0 {
		return 0.0
	}
	if v > 1.0 {
		return 1.0
	}
	return v
}

// frgba builds a Color instance from the given floating point values (between 0.0 and 1.0) r, g, b, a
// Inputs outside [0.0, 1.0] are clamped to the nearest bound, and NaN is treated as 0.0.
@[inline]
pub fn frgba[T](r T, g T, b T, a T) Color {
	return Color{
		r: u8(clamp01(r) * 255.0)
		g: u8(clamp01(g) * 255.0)
		b: u8(clamp01(b) * 255.0)
		a: u8(clamp01(a) * 255.0)
	}
}

// rgb builds a Color instance from the given r, g, b u8 values
@[inline]
pub fn rgb(r u8, g u8, b u8) Color {
	return Color{
		r: r
		g: g
		b: b
	}
}

// rgba builds a Color instance from the given r, g, b, a u8 values
@[inline]
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
	mut na := i32(a.a) + b.a
	mut nr := i32(a.r) + b.r
	mut ng := i32(a.g) + b.g
	mut nb := i32(a.b) + b.b
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
// the alpha channel will be set as the minimum between ``a.a`` and ``b.a``
pub fn (a Color) - (b Color) Color {
	na := if a.a < b.a { a.a } else { b.a }
	mut nr := i32(a.r) - b.r
	mut ng := i32(a.g) - b.g
	mut nb := i32(a.b) - b.b
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

// * multiplies Color `c` and `c2` channel-wise in normalized form: each channel
// is computed as `(c1 * c2 + 127) / 255` with rounding, keeping values in [0, 255].
// White (255) is the neutral element, so `c * white == c`.
pub fn (c Color) * (c2 Color) Color {
	return Color{
		r: u8((i32(c.r) * c2.r + 127) / 255)
		g: u8((i32(c.g) * c2.g + 127) / 255)
		b: u8((i32(c.b) * c2.b + 127) / 255)
		a: u8((i32(c.a) * c2.a + 127) / 255)
	}
}

// div_channel divides the channel value `n` by `d`, saturating instead of
// panicking on a zero divisor: 0/0 gives 0, while x/0 with x != 0 gives 255
// (the closest representable value to +infinity).
@[inline]
fn div_channel(n u8, d u8) u8 {
	if d == 0 {
		if n == 0 {
			return 0
		}
		return 255
	}
	return n / d
}

// / divides `c` by `c2` channel-wise and converts each channel's value to u8(int).
// A zero divisor channel saturates (0/0 -> 0, x/0 -> 255) instead of panicking.
pub fn (c Color) / (c2 Color) Color {
	return Color{
		r: div_channel(c.r, c2.r)
		g: div_channel(c.g, c2.g)
		b: div_channel(c.b, c2.b)
		a: div_channel(c.a, c2.a)
	}
}

// over implements an `a` over `b` operation.
// see https://keithp.com/~keithp/porterduff/p253-porter.pdf
// Compositing two fully transparent colors yields a fully transparent color.
pub fn (a Color) over(b Color) Color {
	aa := f32(a.a) / 255
	ab := f32(b.a) / 255
	ar := aa + ab * (1 - aa)
	if ar == 0 {
		return Color{
			r: 0
			g: 0
			b: 0
			a: 0
		}
	}

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
	return 'Color{${c.r}, ${c.g}, ${c.b}, ${c.a}}'
}

// rgba8 converts a color value to an 32bit int in the RGBA8 order.
// see https://developer.apple.com/documentation/coreimage/ciformat
// Note: the result keeps the raw bit pattern in a signed i32, so colors with
// the high bit set (e.g. white) come back negative; convert with u32(x)
// to recover the unsigned value.
@[inline]
pub fn (c Color) rgba8() i32 {
	return i32(u32(c.r) << 24 | u32(c.g) << 16 | u32(c.b) << 8 | u32(c.a))
}

// bgra8 converts a color value to an 32bit int in the BGRA8 order.
// see https://developer.apple.com/documentation/coreimage/ciformat
// Note: the result keeps the raw bit pattern in a signed i32, so colors with
// the high bit set come back negative; convert with u32(x) to recover
// the unsigned value.
@[inline]
pub fn (c Color) bgra8() i32 {
	return i32(u32(c.b) << 24 | u32(c.g) << 16 | u32(c.r) << 8 | u32(c.a))
}

// abgr8 converts a color value to an 32bit int in the ABGR8 order.
// see https://developer.apple.com/documentation/coreimage/ciformat
// Note: the result keeps the raw bit pattern in a signed i32, so colors with
// the high bit set come back negative; convert with u32(x) to recover
// the unsigned value.
@[inline]
pub fn (c Color) abgr8() i32 {
	return i32(u32(c.a) << 24 | u32(c.b) << 16 | u32(c.g) << 8 | u32(c.r))
}

const string_colors = {
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

// color_from_string returns a Color, corresponding to the given string,
// or the opaque black Color if the string is neither a known color name,
// nor a valid hex color, nor a supported length.
// The hex form accepts the CSS shorthand notations: #RGB and #RGBA, where every digit
// is doubled, as well as the full #RRGGBB and #RRGGBBAA forms, where the last
// two digits (when present) give the alpha channel.
// Invalid hex digits or lengths return black.
pub fn color_from_string(s string) Color {
	if s.starts_with('#') {
		mut digits := s[1..]
		for digit in digits {
			if !digit.is_hex_digit() {
				return black
			}
		}
		if digits.len == 3 || digits.len == 4 {
			// CSS hex shorthand: #RGB -> #RRGGBB and #RGBA -> #RRGGBBAA
			mut expanded := []u8{cap: digits.len * 2}
			for d in digits {
				expanded << d
				expanded << d
			}
			digits = expanded.bytestr()
		}
		if digits.len == 6 {
			return hex(('0x' + digits).u32())
		}
		if digits.len == 8 {
			value := ('0x' + digits).u32()
			return Color{
				r: u8((value >> 24) & 0xFF)
				g: u8((value >> 16) & 0xFF)
				b: u8((value >> 8) & 0xFF)
				a: u8((value >> 0) & 0xFF)
			}
		}
		return black
	} else {
		return string_colors[s]
	}
}

// to_css_string returns a CSS compatible string e.g. `rgba(10,11,12,13)` of the color `c`.
pub fn (c Color) to_css_string() string {
	return 'rgba(${c.r},${c.g},${c.b},${c.a})'
}
