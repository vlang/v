// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module anim

import gg
import math

// HSL is a color in hue/saturation/lightness space. All components are
// normalized to [0, 1].
pub struct HSL {
pub mut:
	h f32
	s f32
	l f32
}

// HSV is a color in hue/saturation/value space. All components are
// normalized to [0, 1].
pub struct HSV {
pub mut:
	h f32
	s f32
	v f32
}

fn color_max3(a f32, b f32, c f32) f32 {
	m := if a > b { a } else { b }
	return if m > c { m } else { c }
}

fn color_min3(a f32, b f32, c f32) f32 {
	m := if a < b { a } else { b }
	return if m < c { m } else { c }
}

fn color_hue(r f32, g f32, b f32, mx f32, d f32) f32 {
	if d == 0 {
		return 0
	}
	mut h := f32(0)
	if mx == r {
		h = (g - b) / d
		if g < b {
			h += 6
		}
	} else if mx == g {
		h = (b - r) / d + 2
	} else {
		h = (r - g) / d + 4
	}
	return h / 6
}

// color_to_hsl converts a gg.Color to HSL with all components in [0, 1].
pub fn color_to_hsl(c gg.Color) HSL {
	r := f32(c.r) / 255
	g := f32(c.g) / 255
	b := f32(c.b) / 255
	mx := color_max3(r, g, b)
	mn := color_min3(r, g, b)
	d := mx - mn
	l := (mx + mn) / 2
	s := if d == 0 {
		0.0
	} else if l > 0.5 {
		d / (2 - mx - mn)
	} else {
		d / (mx + mn)
	}
	return HSL{color_hue(r, g, b, mx, d), s, l}
}

fn hue_to_rgb(p f32, q f32, t f32) f32 {
	mut x := t
	if x < 0 {
		x += 1
	}
	if x > 1 {
		x -= 1
	}
	if x < 1.0 / 6 {
		return p + (q - p) * 6 * x
	}
	if x < 0.5 {
		return q
	}
	if x < 2.0 / 3 {
		return p + (q - p) * (2.0 / 3 - x) * 6
	}
	return p
}

// hsl_to_color converts HSL to an opaque gg.Color. Out-of-range components are
// clamped before conversion.
pub fn hsl_to_color(hsl HSL) gg.Color {
	h := math.clip(hsl.h, 0.0, 1.0)
	s := math.clip(hsl.s, 0.0, 1.0)
	l := math.clip(hsl.l, 0.0, 1.0)
	if s == 0 {
		v := u8(l * 255)
		return gg.Color{ r: v, g: v, b: v, a: 255 }
	}
	q := if l < 0.5 { l * (1 + s) } else { l + s - l * s }
	p := 2 * l - q
	return gg.Color{
		r: u8(hue_to_rgb(p, q, h + 1.0 / 3) * 255)
		g: u8(hue_to_rgb(p, q, h) * 255)
		b: u8(hue_to_rgb(p, q, h - 1.0 / 3) * 255)
		a: 255
	}
}

// color_to_hsv converts a gg.Color to HSV with all components in [0, 1].
pub fn color_to_hsv(c gg.Color) HSV {
	r := f32(c.r) / 255
	g := f32(c.g) / 255
	b := f32(c.b) / 255
	mx := color_max3(r, g, b)
	mn := color_min3(r, g, b)
	d := mx - mn
	s := if mx == 0 { 0.0 } else { d / mx }
	return HSV{color_hue(r, g, b, mx, d), s, mx}
}

// hsv_to_color converts HSV to an opaque gg.Color. Out-of-range components are
// clamped before conversion.
pub fn hsv_to_color(hsv HSV) gg.Color {
	h := math.clip(hsv.h, 0.0, 1.0) * 6
	s := math.clip(hsv.s, 0.0, 1.0)
	v := math.clip(hsv.v, 0.0, 1.0)
	i := int(math.floor(f64(h)))
	f := h - f32(i)
	p := v * (1 - s)
	q := v * (1 - f * s)
	t := v * (1 - (1 - f) * s)
	make := fn (r f32, g f32, b f32) gg.Color {
		return gg.Color{ r: u8(r * 255), g: u8(g * 255), b: u8(b * 255), a: 255 }
	}
	match i % 6 {
		0 {
			return make(v, t, p)
		}
		1 {
			return make(q, v, p)
		}
		2 {
			return make(p, v, t)
		}
		3 {
			return make(p, q, v)
		}
		4 {
			return make(t, p, v)
		}
		else {
			return make(v, p, q)
		}
	}
}

// palette_gradient samples a list of colors as an evenly spaced gradient.
// An empty palette returns black; a single color is returned unchanged.
pub fn palette_gradient(stops []gg.Color, t f32) gg.Color {
	if stops.len == 0 {
		return gg.Color{}
	}
	if stops.len == 1 {
		return stops[0]
	}
	k := math.clip(t, 0.0, 1.0)
	scaled := k * f32(stops.len - 1)
	i := int(scaled)
	if i >= stops.len - 1 {
		return stops[stops.len - 1]
	}
	return lerp_color(stops[i], stops[i + 1], scaled - f32(i))
}

// palette returns one of four small starter palettes useful for plots,
// heightmaps and debug colorization: sunset, ocean, forest or grayscale.
pub fn palette(name string) []gg.Color {
	match name {
		'sunset' {
			return [gg.Color{255, 94, 77, 255}, gg.Color{255, 153, 102, 255},
				gg.Color{255, 210, 111, 255}, gg.Color{255, 255, 199, 255}]
		}
		'ocean' {
			return [gg.Color{12, 32, 66, 255}, gg.Color{22, 96, 140, 255}, gg.Color{54, 166, 178, 255},
				gg.Color{183, 230, 220, 255}]
		}
		'forest' {
			return [gg.Color{20, 48, 32, 255}, gg.Color{44, 102, 60, 255}, gg.Color{112, 160, 86, 255},
				gg.Color{210, 220, 150, 255}]
		}
		'grayscale' {
			return [gg.Color{0, 0, 0, 255}, gg.Color{85, 85, 85, 255}, gg.Color{170, 170, 170, 255},
				gg.Color{255, 255, 255, 255}]
		}
		else {
			return []
		}
	}
}
