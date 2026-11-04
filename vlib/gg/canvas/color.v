module canvas

// Color4f is an sRGB color with float components, not premultiplied unless
// stated otherwise.
pub struct Color4f {
pub:
	r f32
	g f32
	b f32
	a f32
}

// inv_255 is `1 / 255.0f`, the factor Skia multiplies 8-bit components by.
const inv_255 = f32(1.0) / f32(255.0)

// Color4f.from_rgba8 converts 8-bit components exactly like SkColor4f::FromColor().
pub fn Color4f.from_rgba8(r u8, g u8, b u8, a u8) Color4f {
	return Color4f{f32(r) * inv_255, f32(g) * inv_255, f32(b) * inv_255, f32(a) * inv_255}
}

// premul returns the premultiplied color, `{r * a, g * a, b * a, a}`.
pub fn (c Color4f) premul() Color4f {
	return Color4f{c.r * c.a, c.g * c.a, c.b * c.a, c.a}
}

pub fn (c Color4f) is_opaque() bool {
	return c.a == 1
}

// to_rgba8 converts to 8-bit components like Sk4f_toL32: scale, add 0.5, pin
// and truncate.
pub fn (c Color4f) to_rgba8() [4]u8 {
	return [unit_to_u8(c.r), unit_to_u8(c.g), unit_to_u8(c.b), unit_to_u8(c.a)]!
}

fn unit_to_u8(v f32) u8 {
	x := v * f32(255) + f32(0.5)
	pinned := if x < 0 {
		f32(0)
	} else if x > 255 {
		f32(255)
	} else {
		x
	}
	return u8(pinned)
}
