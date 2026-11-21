module hdr

import math

// ColorSpace identifies a display primaries set. Conversion goes through
// XYZ using the corresponding primaries, with D65 as the reference white.
pub enum ColorSpace {
	srgb
	p3
	rec2020
}

struct XYZ {
	x f32
	y f32
	z f32
}

fn rgb_to_xyz(c ColorHDR, cs ColorSpace) XYZ {
	// Fixed matrices keep this backend independent and exact enough.
	if cs == .srgb {
		return XYZ{0.4124 * c.r + 0.3576 * c.g + 0.1805 * c.b, 0.2126 * c.r + 0.7152 * c.g + 0.0722 * c.b, 0.0193 * c.r + 0.1192 * c.g + 0.9505 * c.b}
	}
	if cs == .p3 {
		return XYZ{0.4866 * c.r + 0.2657 * c.g + 0.1982 * c.b, 0.2290 * c.r + 0.6917 * c.g + 0.0793 * c.b, 0.0054 * c.r + 0.0441 * c.g + 0.9944 * c.b}
	}
	return XYZ{0.636958 * c.r + 0.144617 * c.g + 0.168881 * c.b, 0.2627 * c.r + 0.6780 * c.g + 0.0593 * c.b, 0 + 0.0281 * c.g + 1.0614 * c.b}
}

fn xyz_to_rgb(v XYZ, cs ColorSpace) ColorHDR {
	if cs == .srgb {
		return ColorHDR{3.240970 * v.x - 1.537383 * v.y - 0.498611 * v.z, -0.969244 * v.x + 1.875968 * v.y + 0.041555 * v.z, 0.055630 * v.x - 0.203977 * v.y + 1.056971 * v.z, 1}
	}
	if cs == .p3 {
		return ColorHDR{2.493497 * v.x - 0.931384 * v.y - 0.402711 * v.z, -0.829489 * v.x + 1.622665 * v.y + 0.035586 * v.z, 0.035829 * v.x - 0.058345 * v.y + 0.927186 * v.z, 1}
	}
	return ColorHDR{1.716651 * v.x - 0.355671 * v.y - 0.253366 * v.z, -0.666684 * v.x + 1.616481 * v.y + 0.015769 * v.z, 0.017640 * v.x - 0.042771 * v.y + 0.942103 * v.z, 1}
}

// convert_color_space converts linear-light RGB between primaries.
pub fn convert_color_space(c ColorHDR, from ColorSpace, to ColorSpace) ColorHDR {
	if from == to {
		return c
	}
	return xyz_to_rgb(rgb_to_xyz(c, from), to)
}

// LUT3D is a cubic lookup table with size^3 RGB samples.
pub struct LUT3D {
pub mut:
	size int
	data []f32
}

pub fn identity_lut(size int) !LUT3D {
	if size < 2 {
		return error('hdr: LUT size must be at least 2')
	}
	mut d := []f32{len: size * size * size * 3}
	for b in 0 .. size {
		for g in 0 .. size {
			for r in 0 .. size {
				at := ((b * size + g) * size + r) * 3
				d[at] = f32(r) / f32(size - 1)
				d[at + 1] = f32(g) / f32(size - 1)
				d[at + 2] = f32(b) / f32(size - 1)
			}
		}
	}
	return LUT3D{size, d}
}

fn lut_sample(l LUT3D, r f32, g f32, b f32) ColorHDR {
	scale := f32(l.size - 1)
	fr := math.clip(r, 0, 1) * scale
	fg := math.clip(g, 0, 1) * scale
	fb := math.clip(b, 0, 1) * scale
	r0 := int(fr)
	g0 := int(fg)
	b0 := int(fb)
	r1 := if r0 + 1 < l.size { r0 + 1 } else { r0 }
	g1 := if g0 + 1 < l.size { g0 + 1 } else { g0 }
	b1 := if b0 + 1 < l.size { b0 + 1 } else { b0 }
	tx := fr - f32(r0)
	ty := fg - f32(g0)
	tz := fb - f32(b0)
	mut out := ColorHDR{}
	for c in 0 .. 3 {
		c000 := l.data[((b0 * l.size + g0) * l.size + r0) * 3 + c]
		c100 := l.data[((b0 * l.size + g0) * l.size + r1) * 3 + c]
		c010 := l.data[((b0 * l.size + g1) * l.size + r0) * 3 + c]
		c110 := l.data[((b0 * l.size + g1) * l.size + r1) * 3 + c]
		c001 := l.data[((b1 * l.size + g0) * l.size + r0) * 3 + c]
		c101 := l.data[((b1 * l.size + g0) * l.size + r1) * 3 + c]
		c011 := l.data[((b1 * l.size + g1) * l.size + r0) * 3 + c]
		c111 := l.data[((b1 * l.size + g1) * l.size + r1) * 3 + c]
		c00 := c000 + (c100 - c000) * tx
		c10 := c010 + (c110 - c010) * tx
		c01 := c001 + (c101 - c001) * tx
		c11 := c011 + (c111 - c011) * tx
		c0 := c00 + (c10 - c00) * ty
		c1 := c01 + (c11 - c01) * ty
		value := c0 + (c1 - c0) * tz
		if c == 0 {
			out.r = value
		} else if c == 1 {
			out.g = value
		} else {
			out.b = value
		}
	}
	out.a = 1
	return out
}

// apply maps a display-referred color through the LUT.
pub fn (l &LUT3D) apply(c ColorHDR) ColorHDR {
	return lut_sample(l, c.r, c.g, c.b)
}

// AutoExposure adapts a running exposure value toward measured luminance.
pub struct AutoExposure {
pub mut:
	value   f32 = 1
	target  f32 = 1
	key     f32 = 0.18
	speed   f32 = 2
	minimum f32 = 0.05
	maximum f32 = 16
}

// update measures average luminance and adapts exponentially by dt seconds.
pub fn (mut a AutoExposure) update(average_luminance f32, dt f32) {
	if average_luminance > 0 {
		a.target = a.key / average_luminance
	}
	a.target = math.clip(a.target, a.minimum, a.maximum)
	factor := 1 - f32(math.exp(f64(-a.speed * dt)))
	a.value += (a.target - a.value) * factor
	a.value = math.clip(a.value, a.minimum, a.maximum)
}
