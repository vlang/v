// Copyright (c) 2026 The V Authors. All rights reserved.
// Portions derived from Go's image/color package.
// Copyright 2011 The Go Authors. All rights reserved.
// Use of the original Go source is governed by Go's BSD-style license.
module color

// RGBA represents a 32-bit alpha-premultiplied color.
pub struct RGBA {
pub mut:
	r u8
	g u8
	b u8
	a u8
}

// rgba returns the alpha-premultiplied red, green, blue, and alpha channels as
// 16-bit values stored in u32s.
pub fn (c RGBA) rgba() (u32, u32, u32, u32) {
	mut r := u32(c.r)
	r |= r << 8
	mut g := u32(c.g)
	g |= g << 8
	mut b := u32(c.b)
	b |= b << 8
	mut a := u32(c.a)
	a |= a << 8
	return r, g, b, a
}

// RGBA64 represents a 64-bit alpha-premultiplied color.
pub struct RGBA64 {
pub mut:
	r u16
	g u16
	b u16
	a u16
}

// rgba returns the alpha-premultiplied red, green, blue, and alpha channels as
// 16-bit values stored in u32s.
pub fn (c RGBA64) rgba() (u32, u32, u32, u32) {
	return u32(c.r), u32(c.g), u32(c.b), u32(c.a)
}

// NRGBA represents a non-alpha-premultiplied 32-bit color.
pub struct NRGBA {
pub mut:
	r u8
	g u8
	b u8
	a u8
}

// rgba returns the alpha-premultiplied red, green, blue, and alpha channels as
// 16-bit values stored in u32s.
pub fn (c NRGBA) rgba() (u32, u32, u32, u32) {
	mut r := u32(c.r)
	r |= r << 8
	r *= u32(c.a)
	r /= 0xff
	mut g := u32(c.g)
	g |= g << 8
	g *= u32(c.a)
	g /= 0xff
	mut b := u32(c.b)
	b |= b << 8
	b *= u32(c.a)
	b /= 0xff
	mut a := u32(c.a)
	a |= a << 8
	return r, g, b, a
}

// NRGBA64 represents a non-alpha-premultiplied 64-bit color.
pub struct NRGBA64 {
pub mut:
	r u16
	g u16
	b u16
	a u16
}

// rgba returns the alpha-premultiplied red, green, blue, and alpha channels as
// 16-bit values stored in u32s.
pub fn (c NRGBA64) rgba() (u32, u32, u32, u32) {
	mut r := u32(c.r)
	r *= u32(c.a)
	r /= 0xffff
	mut g := u32(c.g)
	g *= u32(c.a)
	g /= 0xffff
	mut b := u32(c.b)
	b *= u32(c.a)
	b /= 0xffff
	return r, g, b, u32(c.a)
}

// Alpha represents an 8-bit alpha color.
pub struct Alpha {
pub mut:
	a u8
}

// rgba returns the alpha channel copied into each alpha-premultiplied channel.
pub fn (c Alpha) rgba() (u32, u32, u32, u32) {
	mut a := u32(c.a)
	a |= a << 8
	return a, a, a, a
}

// Alpha16 represents a 16-bit alpha color.
pub struct Alpha16 {
pub mut:
	a u16
}

// rgba returns the alpha channel copied into each alpha-premultiplied channel.
pub fn (c Alpha16) rgba() (u32, u32, u32, u32) {
	a := u32(c.a)
	return a, a, a, a
}

// Gray represents an 8-bit grayscale color.
pub struct Gray {
pub mut:
	y u8
}

// rgba returns the grayscale channel as opaque red, green, and blue channels.
pub fn (c Gray) rgba() (u32, u32, u32, u32) {
	mut y := u32(c.y)
	y |= y << 8
	return y, y, y, 0xffff
}

// Gray16 represents a 16-bit grayscale color.
pub struct Gray16 {
pub mut:
	y u16
}

// rgba returns the grayscale channel as opaque red, green, and blue channels.
pub fn (c Gray16) rgba() (u32, u32, u32, u32) {
	y := u32(c.y)
	return y, y, y, 0xffff
}

// YCbCr represents an opaque JFIF Y'CbCr color.
pub struct YCbCr {
pub mut:
	y  u8
	cb u8
	cr u8
}

// rgba converts c to alpha-premultiplied 16-bit RGBA channels.
pub fn (c YCbCr) rgba() (u32, u32, u32, u32) {
	yy1 := i32(c.y) * 0x10101
	cb1 := i32(c.cb) - 128
	cr1 := i32(c.cr) - 128
	r := clamp_u16_from_i32(yy1 + 91881 * cr1, 8)
	g := clamp_u16_from_i32(yy1 - 22554 * cb1 - 46802 * cr1, 8)
	b := clamp_u16_from_i32(yy1 + 116130 * cb1, 8)
	return u32(r), u32(g), u32(b), 0xffff
}

// NYCbCrA represents a non-alpha-premultiplied JFIF Y'CbCr-with-alpha color.
pub struct NYCbCrA {
pub mut:
	ycbcr YCbCr
	a     u8
}

// rgba converts c to alpha-premultiplied 16-bit RGBA channels.
pub fn (c NYCbCrA) rgba() (u32, u32, u32, u32) {
	r0, g0, b0, _ := c.ycbcr.rgba()
	a := u32(c.a) * 0x101
	return r0 * a / 0xffff, g0 * a / 0xffff, b0 * a / 0xffff, a
}

// CMYK represents an opaque CMYK color.
pub struct CMYK {
pub mut:
	c u8
	m u8
	y u8
	k u8
}

// rgba converts c to alpha-premultiplied 16-bit RGBA channels.
pub fn (c CMYK) rgba() (u32, u32, u32, u32) {
	w := 0xffff - u32(c.k) * 0x101
	r := (0xffff - u32(c.c) * 0x101) * w / 0xffff
	g := (0xffff - u32(c.m) * 0x101) * w / 0xffff
	b := (0xffff - u32(c.y) * 0x101) * w / 0xffff
	return r, g, b, 0xffff
}

// Color is the set of color types provided by this module.
pub type Color = Alpha
	| Alpha16
	| CMYK
	| Gray
	| Gray16
	| NRGBA
	| NRGBA64
	| NYCbCrA
	| RGBA
	| RGBA64
	| YCbCr

// rgba returns the alpha-premultiplied red, green, blue, and alpha channels as
// 16-bit values stored in u32s.
pub fn (c Color) rgba() (u32, u32, u32, u32) {
	match c {
		Alpha {
			return c.rgba()
		}
		Alpha16 {
			return c.rgba()
		}
		CMYK {
			return c.rgba()
		}
		Gray {
			return c.rgba()
		}
		Gray16 {
			return c.rgba()
		}
		NRGBA {
			return c.rgba()
		}
		NRGBA64 {
			return c.rgba()
		}
		NYCbCrA {
			return c.rgba()
		}
		RGBA {
			return c.rgba()
		}
		RGBA64 {
			return c.rgba()
		}
		YCbCr {
			return c.rgba()
		}
	}
}

// ModelKind identifies one of the standard color conversion models.
pub enum ModelKind {
	rgba
	rgba64
	nrgba
	nrgba64
	alpha
	alpha16
	gray
	gray16
	ycbcr
	nycbcra
	cmyk
}

// Palette is an ordered list of colors.
pub struct Palette {
pub mut:
	colors []Color
}

// ConstantModel converts every input color to c.
pub struct ConstantModel {
pub mut:
	c Color
}

// Model converts colors into a color model.
pub type Model = ConstantModel | ModelKind | Palette

// convert converts c into the receiver's color model.
pub fn (m Model) convert(c Color) !Color {
	match m {
		ConstantModel {
			return m.convert(c)
		}
		ModelKind {
			return m.convert(c)
		}
		Palette {
			return m.convert(c)!
		}
	}
}

// convert converts c into the receiver's constant color.
pub fn (m ConstantModel) convert(c Color) Color {
	return m.c
}

// constant_model returns a model that converts every input color to c.
pub fn constant_model(c Color) Model {
	return Model(ConstantModel{
		c: c
	})
}

// convert converts c into the receiver's standard color model.
pub fn (m ModelKind) convert(c Color) Color {
	match m {
		.rgba {
			return Color(to_rgba(c))
		}
		.rgba64 {
			return Color(to_rgba64(c))
		}
		.nrgba {
			return Color(to_nrgba(c))
		}
		.nrgba64 {
			return Color(to_nrgba64(c))
		}
		.alpha {
			return Color(to_alpha(c))
		}
		.alpha16 {
			return Color(to_alpha16(c))
		}
		.gray {
			return Color(to_gray(c))
		}
		.gray16 {
			return Color(to_gray16(c))
		}
		.ycbcr {
			return Color(to_ycbcr(c))
		}
		.nycbcra {
			return Color(to_nycbcra(c))
		}
		.cmyk {
			return Color(to_cmyk(c))
		}
	}
}

pub const rgba_model = Model(ModelKind.rgba)
pub const rgba64_model = Model(ModelKind.rgba64)
pub const nrgba_model = Model(ModelKind.nrgba)
pub const nrgba64_model = Model(ModelKind.nrgba64)
pub const alpha_model = Model(ModelKind.alpha)
pub const alpha16_model = Model(ModelKind.alpha16)
pub const gray_model = Model(ModelKind.gray)
pub const gray16_model = Model(ModelKind.gray16)
pub const ycbcr_model = Model(ModelKind.ycbcr)
pub const nycbcra_model = Model(ModelKind.nycbcra)
pub const cmyk_model = Model(ModelKind.cmyk)

pub const black = Color(Gray16{})
pub const white = Color(Gray16{
	y: 0xffff
})
pub const transparent = Color(Alpha16{})
pub const opaque = Color(Alpha16{
	a: 0xffff
})

// to_rgba converts c to RGBA.
pub fn to_rgba(c Color) RGBA {
	if c is RGBA {
		return c
	}
	r, g, b, a := c.rgba()
	return RGBA{u8(r >> 8), u8(g >> 8), u8(b >> 8), u8(a >> 8)}
}

// to_rgba64 converts c to RGBA64.
pub fn to_rgba64(c Color) RGBA64 {
	if c is RGBA64 {
		return c
	}
	r, g, b, a := c.rgba()
	return RGBA64{u16(r), u16(g), u16(b), u16(a)}
}

// to_nrgba converts c to NRGBA.
pub fn to_nrgba(c Color) NRGBA {
	if c is NRGBA {
		return c
	}
	mut r, mut g, mut b, a := c.rgba()
	if a == 0xffff {
		return NRGBA{u8(r >> 8), u8(g >> 8), u8(b >> 8), 0xff}
	}
	if a == 0 {
		return NRGBA{}
	}
	r = (r * 0xffff) / a
	g = (g * 0xffff) / a
	b = (b * 0xffff) / a
	return NRGBA{u8(r >> 8), u8(g >> 8), u8(b >> 8), u8(a >> 8)}
}

// to_nrgba64 converts c to NRGBA64.
pub fn to_nrgba64(c Color) NRGBA64 {
	if c is NRGBA64 {
		return c
	}
	mut r, mut g, mut b, a := c.rgba()
	if a == 0xffff {
		return NRGBA64{u16(r), u16(g), u16(b), 0xffff}
	}
	if a == 0 {
		return NRGBA64{}
	}
	r = (r * 0xffff) / a
	g = (g * 0xffff) / a
	b = (b * 0xffff) / a
	return NRGBA64{u16(r), u16(g), u16(b), u16(a)}
}

// to_alpha converts c to Alpha.
pub fn to_alpha(c Color) Alpha {
	if c is Alpha {
		return c
	}
	_, _, _, a := c.rgba()
	return Alpha{
		a: u8(a >> 8)
	}
}

// to_alpha16 converts c to Alpha16.
pub fn to_alpha16(c Color) Alpha16 {
	if c is Alpha16 {
		return c
	}
	_, _, _, a := c.rgba()
	return Alpha16{
		a: u16(a)
	}
}

// to_gray converts c to Gray.
pub fn to_gray(c Color) Gray {
	if c is Gray {
		return c
	}
	r, g, b, _ := c.rgba()
	y := (19595 * r + 38470 * g + 7471 * b + (1 << 15)) >> 24
	return Gray{
		y: u8(y)
	}
}

// to_gray16 converts c to Gray16.
pub fn to_gray16(c Color) Gray16 {
	if c is Gray16 {
		return c
	}
	r, g, b, _ := c.rgba()
	y := (19595 * r + 38470 * g + 7471 * b + (1 << 15)) >> 16
	return Gray16{
		y: u16(y)
	}
}

// to_ycbcr converts c to YCbCr.
pub fn to_ycbcr(c Color) YCbCr {
	if c is YCbCr {
		return c
	}
	r, g, b, _ := c.rgba()
	y, cb, cr := rgb_to_ycbcr(u8(r >> 8), u8(g >> 8), u8(b >> 8))
	return YCbCr{
		y:  y
		cb: cb
		cr: cr
	}
}

// to_nycbcra converts c to NYCbCrA.
pub fn to_nycbcra(c Color) NYCbCrA {
	if c is NYCbCrA {
		return c
	}
	if c is YCbCr {
		return NYCbCrA{
			ycbcr: c
			a:     0xff
		}
	}
	mut r, mut g, mut b, a := c.rgba()
	if a != 0 {
		r = (r * 0xffff) / a
		g = (g * 0xffff) / a
		b = (b * 0xffff) / a
	}
	y, cb, cr := rgb_to_ycbcr(u8(r >> 8), u8(g >> 8), u8(b >> 8))
	return NYCbCrA{
		ycbcr: YCbCr{
			y:  y
			cb: cb
			cr: cr
		}
		a:     u8(a >> 8)
	}
}

// to_cmyk converts c to CMYK.
pub fn to_cmyk(c Color) CMYK {
	if c is CMYK {
		return c
	}
	r, g, b, _ := c.rgba()
	cc, mm, yy, kk := rgb_to_cmyk(u8(r >> 8), u8(g >> 8), u8(b >> 8))
	return CMYK{
		c: cc
		m: mm
		y: yy
		k: kk
	}
}

// convert returns the palette color closest to c.
pub fn (p Palette) convert(c Color) !Color {
	if p.colors.len == 0 {
		return error('color: empty palette')
	}
	return p.colors[p.index(c)]
}

// index returns the index of the palette color closest to c in RGBA space.
pub fn (p Palette) index(c Color) int {
	cr, cg, cb, ca := c.rgba()
	mut ret := 0
	mut best_sum := u32(max_u32)
	for i, v in p.colors {
		vr, vg, vb, va := v.rgba()
		sum := sq_diff(cr, vr) + sq_diff(cg, vg) + sq_diff(cb, vb) + sq_diff(ca, va)
		if sum < best_sum {
			if sum == 0 {
				return i
			}
			ret = i
			best_sum = sum
		}
	}
	return ret
}

// rgb_to_ycbcr converts an RGB triple to a JFIF Y'CbCr triple.
pub fn rgb_to_ycbcr(r u8, g u8, b u8) (u8, u8, u8) {
	r1 := i32(r)
	g1 := i32(g)
	b1 := i32(b)
	yy := (19595 * r1 + 38470 * g1 + 7471 * b1 + (1 << 15)) >> 16
	cb := (-11056 * r1 - 21712 * g1 + 32768 * b1 + (257 << 15)) >> 16
	cr := (32768 * r1 - 27440 * g1 - 5328 * b1 + (257 << 15)) >> 16
	return u8(yy), clamp_u8(cb), clamp_u8(cr)
}

// ycbcr_to_rgb converts a JFIF Y'CbCr triple to an RGB triple.
pub fn ycbcr_to_rgb(y u8, cb u8, cr u8) (u8, u8, u8) {
	yy1 := i32(y) * 0x10101
	cb1 := i32(cb) - 128
	cr1 := i32(cr) - 128
	r := (yy1 + 91881 * cr1) >> 16
	g := (yy1 - 22554 * cb1 - 46802 * cr1) >> 16
	b := (yy1 + 116130 * cb1) >> 16
	return clamp_u8(r), clamp_u8(g), clamp_u8(b)
}

// rgb_to_cmyk converts an RGB triple to a CMYK quadruple.
pub fn rgb_to_cmyk(r u8, g u8, b u8) (u8, u8, u8, u8) {
	rr := u32(r)
	gg := u32(g)
	bb := u32(b)
	mut w := rr
	if w < gg {
		w = gg
	}
	if w < bb {
		w = bb
	}
	if w == 0 {
		return 0, 0, 0, 0xff
	}
	c := (w - rr) * 0xff / w
	m := (w - gg) * 0xff / w
	y := (w - bb) * 0xff / w
	return u8(c), u8(m), u8(y), u8(0xff - w)
}

// cmyk_to_rgb converts a CMYK quadruple to an RGB triple.
pub fn cmyk_to_rgb(c u8, m u8, y u8, k u8) (u8, u8, u8) {
	w := 0xffff - u32(k) * 0x101
	r := (0xffff - u32(c) * 0x101) * w / 0xffff
	g := (0xffff - u32(m) * 0x101) * w / 0xffff
	b := (0xffff - u32(y) * 0x101) * w / 0xffff
	return u8(r >> 8), u8(g >> 8), u8(b >> 8)
}

fn sq_diff(x u32, y u32) u32 {
	d := if x > y { x - y } else { y - x }
	return (d * d) >> 2
}

fn clamp_u8(v i32) u8 {
	if v < 0 {
		return 0
	}
	if v > 0xff {
		return 0xff
	}
	return u8(v)
}

fn clamp_u16_from_i32(v i32, shift int) u16 {
	x := v >> shift
	if x < 0 {
		return 0
	}
	if x > 0xffff {
		return 0xffff
	}
	return u16(x)
}
