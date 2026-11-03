module gg

import math

// The pure colour constructors and string forms, which color_test.v does not
// cover: the generic float builders, the 4-byte form of hex(), and the two
// string renderings.

fn test_hex_uses_the_lower_three_bytes_and_ignores_the_top_one() {
	c := hex(0x11223344)
	// only the low 24 bits are split into r,g,b; the top byte is discarded
	assert c.r == 0x22
	assert c.g == 0x33
	assert c.b == 0x44
	// and alpha is left at the struct default rather than taken from the top byte
	assert c.a == 255
	assert hex(0xff000000) == black
	assert hex(0x00ff0000) == red
}

fn test_frgb_and_frgba_truncate_toward_zero() {
	assert frgb(1.0, 1.0, 1.0) == white
	assert frgb(0.0, 0.0, 0.0) == Color{
		r: 0
		g: 0
		b: 0
		a: 255
	}
	// 0.5 * 255 is 127.5, and the u8 conversion truncates
	assert frgba(0.5, 0.5, 0.5, 1.0) == Color{
		r: 127
		g: 127
		b: 127
		a: 255
	}
	// frgb always forces alpha to fully opaque
	assert frgb(0.25, 0.5, 0.75).a == 255
}

fn test_frgba_clamps_out_of_range_inputs() {
	// inputs above 1.0 clamp to 255, inputs below 0.0 clamp to 0
	assert frgba(2.0, -1.0, 0.5, 1.5) == rgba(255, 0, 127, 255)
	assert frgba(-0.5, 2.5, 0.0, 1.0) == rgba(0, 255, 0, 255)
	assert frgb(1.5, -1.5, 0.5) == rgb(255, 0, 127)
	// NaN is treated as 0.0
	assert frgba(math.nan(), 0.5, 0.5, 1.0).r == 0
	// clamping works for f32 inputs too
	assert frgba(f32(2.0), f32(-1.0), f32(0.5), f32(1.0)) == rgba(255, 0, 127, 255)
}

fn test_the_generic_float_builders_accept_both_float_widths() {
	// 0.5 * 255 is exactly representable in both widths, so the f32 and f64
	// instantiations land on the same bytes
	narrow := frgb(f32(0.5), f32(0.5), f32(0.5))
	wide := frgb(0.5, 0.5, 0.5)
	assert narrow == wide
	assert narrow.r == 127
	// frgba keeps the alpha the caller asked for, so it differs from frgb there
	half := frgba(f64(0.5), f64(0.5), f64(0.5), f64(0.5))
	assert half.r == narrow.r
	assert half.a == 127
	assert narrow.a == 255
}

fn test_color_str_is_the_struct_shape() {
	assert red.str() == 'Color{255, 0, 0, 255}'
	assert Color{}.str() == 'Color{0, 0, 0, 255}'
	assert Color{
		r: 1
		g: 2
		b: 3
		a: 4
	}.str() == 'Color{1, 2, 3, 4}'
}

fn test_to_css_string_keeps_the_channel_order() {
	assert red.to_css_string() == 'rgba(255,0,0,255)'
	assert Color{
		r: 1
		g: 2
		b: 3
		a: 4
	}.to_css_string() == 'rgba(1,2,3,4)'
	// the alpha channel is written even when it is the default
	assert white.to_css_string() == 'rgba(255,255,255,255)'
}
