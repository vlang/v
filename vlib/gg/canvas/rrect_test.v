module canvas

import math

// The expected values were produced by SkRRect::setRectRadii() of the pinned
// Chromium Skia revision; floats are compared bit for bit.

struct RRectCase {
	input [4]u32
	radii [8]u32
	typ   RRectType
	rect  [4]u32
	out   [8]u32
}

const rrect_cases = [
	RRectCase{
		input: [u32(0x40a80000), 0x40900000, 0x42370000, 0x420b0000]!
		radii: [u32(0x40c00000), 0x40c00000, 0x40c00000, 0x40c00000, 0x40c00000, 0x40c00000,
			0x40c00000, 0x40c00000]!
		typ:   .simple
		rect:  [u32(0x40a80000), 0x40900000, 0x42370000, 0x420b0000]!
		out:   [u32(0x40c00000), 0x40c00000, 0x40c00000, 0x40c00000, 0x40c00000, 0x40c00000,
			0x40c00000, 0x40c00000]!
	},
	RRectCase{
		input: [u32(0x00000000), 0x00000000, 0x41200000, 0x41200000]!
		radii: [u32(0x41a00000), 0x41a00000, 0x41a00000, 0x41a00000, 0x41a00000, 0x41a00000,
			0x41a00000, 0x41a00000]!
		typ:   .oval
		rect:  [u32(0x00000000), 0x00000000, 0x41200000, 0x41200000]!
		out:   [u32(0x40a00000), 0x40a00000, 0x40a00000, 0x40a00000, 0x40a00000, 0x40a00000,
			0x40a00000, 0x40a00000]!
	},
	RRectCase{
		input: [u32(0x00000000), 0x00000000, 0x42c80000, 0x41200000]!
		radii: [u32(0x42a00000), 0x41000000, 0x42700000, 0x40400000, 0x00000000, 0x00000000,
			0x3f800000, 0x41100000]!
		typ:   .complex
		rect:  [u32(0x00000000), 0x00000000, 0x42c80000, 0x41200000]!
		out:   [u32(0x423c3c3c), 0x40969697, 0x420d2d2d, 0x3fe1e1e2, 0x00000000, 0x00000000,
			0x3f169697, 0x40a96969]!
	},
	RRectCase{
		input: [u32(0x00000000), 0x00000000, 0x41200000, 0x41200000]!
		radii: [u32(0xbf800000), 0x40400000, 0x40800000, 0x40800000, 0x40800000, 0x40800000,
			0x40800000, 0x40800000]!
		typ:   .complex
		rect:  [u32(0x00000000), 0x00000000, 0x41200000, 0x41200000]!
		out:   [u32(0x00000000), 0x00000000, 0x40800000, 0x40800000, 0x40800000, 0x40800000,
			0x40800000, 0x40800000]!
	},
	RRectCase{
		input: [u32(0x41200000), 0x41200000, 0x00000000, 0x00000000]!
		radii: [u32(0x40000000), 0x40000000, 0x40000000, 0x40000000, 0x40000000, 0x40000000,
			0x40000000, 0x40000000]!
		typ:   .simple
		rect:  [u32(0x00000000), 0x00000000, 0x41200000, 0x41200000]!
		out:   [u32(0x40000000), 0x40000000, 0x40000000, 0x40000000, 0x40000000, 0x40000000,
			0x40000000, 0x40000000]!
	},
	RRectCase{
		input: [u32(0x00000000), 0x00000000, 0x41200000, 0x00000000]!
		radii: [u32(0x40000000), 0x40000000, 0x40000000, 0x40000000, 0x40000000, 0x40000000,
			0x40000000, 0x40000000]!
		typ:   .empty
		rect:  [u32(0x00000000), 0x00000000, 0x41200000, 0x00000000]!
		out:   [u32(0x00000000), 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x00000000, 0x00000000]!
	},
	RRectCase{
		input: [u32(0x00000000), 0x00000000, 0x42480000, 0x41f00000]!
		radii: [u32(0x41200000), 0x40a00000, 0x41a00000, 0x40a00000, 0x41a00000, 0x40a00000,
			0x41200000, 0x40a00000]!
		typ:   .nine_patch
		rect:  [u32(0x00000000), 0x00000000, 0x42480000, 0x41f00000]!
		out:   [u32(0x41200000), 0x40a00000, 0x41a00000, 0x40a00000, 0x41a00000, 0x40a00000,
			0x41200000, 0x40a00000]!
	},
	RRectCase{
		input: [u32(0x3dcccccd), 0x3e4ccccd, 0x49742408, 0x40e80000]!
		radii: [u32(0x48f42400), 0x40800000, 0x48f42420, 0x40800000, 0x40400000, 0x40400000,
			0x3f800000, 0x3f800000]!
		typ:   .complex
		rect:  [u32(0x3dcccccd), 0x3e4ccccd, 0x49742408, 0x40e80000]!
		out:   [u32(0x48f423f6), 0x407ffff6, 0x48f42416, 0x407ffff6, 0x403ffff8, 0x403ffff8,
			0x3f7ffff6, 0x3f7ffff6]!
	},
	RRectCase{
		input: [u32(0x4228fad9), 0xc0dacf8d, 0x431f3eb6, 0x41ec71fe]!
		radii: [u32(0x43846cd9), 0x43769533, 0x43ab7971, 0x43ad9cea, 0x438d30ff, 0x43807e34,
			0x43366bda, 0x433a45d8]!
		typ:   .complex
		rect:  [u32(0x4228fad9), 0xc0dacf8d, 0x431f3eb6, 0x41ec71fe]!
		out:   [u32(0x417f3e66), 0x416da387, 0x41a54133, 0x41a750cf, 0x418811f3, 0x4177aa23,
			0x412fcdf5, 0x41338430]!
	},
	RRectCase{
		input: [u32(0x00000000), 0x00000000, 0x7149f2ca, 0x7149f2ca]!
		radii: [u32(0x7149f2ca), 0x7149f2ca, 0x7149f2ca, 0x7149f2ca, 0x7149f2ca, 0x7149f2ca,
			0x7149f2ca, 0x7149f2ca]!
		typ:   .oval
		rect:  [u32(0x00000000), 0x00000000, 0x7149f2ca, 0x7149f2ca]!
		out:   [u32(0x70c9f2ca), 0x70c9f2ca, 0x70c9f2ca, 0x70c9f2ca, 0x70c9f2ca, 0x70c9f2ca,
			0x70c9f2ca, 0x70c9f2ca]!
	},
	RRectCase{
		input: [u32(0x40400000), 0x40800000, 0x41100000, 0x40e00000]!
		radii: [u32(0x40400000), 0x3fc00000, 0x40400000, 0x3fc00000, 0x40400000, 0x3fc00000,
			0x40400000, 0x3fc00000]!
		typ:   .oval
		rect:  [u32(0x40400000), 0x40800000, 0x41100000, 0x40e00000]!
		out:   [u32(0x40400000), 0x3fc00000, 0x40400000, 0x3fc00000, 0x40400000, 0x3fc00000,
			0x40400000, 0x3fc00000]!
	},
]

fn test_rect_radii_matches_skrrect() {
	for i, c in rrect_cases {
		f := fn (bits u32) f32 {
			return math.f32_from_bits(bits)
		}
		rr := RRect.rect_radii(Rect.ltrb(f(c.input[0]), f(c.input[1]), f(c.input[2]), f(c.input[3])),
			[Point{f(c.radii[0]), f(c.radii[1])}, Point{f(c.radii[2]), f(c.radii[3])},
				Point{f(c.radii[4]), f(c.radii[5])}, Point{f(c.radii[6]), f(c.radii[7])}]!)
		assert rr.typ == c.typ, 'case ${i}'
		assert [math.f32_bits(rr.rect.left()), math.f32_bits(rr.rect.top()),
			math.f32_bits(rr.rect.right()), math.f32_bits(rr.rect.bottom())] == c.rect[..], 'case ${i}'
		mut out := []u32{}
		for p in rr.radii {
			out << math.f32_bits(p.x)
			out << math.f32_bits(p.y)
		}
		assert out == c.out[..], 'case ${i}'
	}
}

fn test_oval_and_inner_bounds() {
	oval := RRect.oval(Rect.ltrb(10.5, 8.25, 50, 40))
	assert oval.typ == .oval
	assert oval.radii[0] == Point{19.75, 15.875}
	flat := RRect.oval(Rect.ltrb(1, 1, 1, 9))
	assert flat.typ == .empty
	inner := rrect_inner_bounds(RRect.rect_radii(Rect.ltrb(0, 0, 100, 50), [
		Point{10, 10},
		Point{10, 10},
		Point{10, 10},
		Point{10, 10},
	]!))
	// SkRRectPriv::InnerBounds() insets all sides by the scaled radii here.
	assert [math.f32_bits(inner.left()), math.f32_bits(inner.top()), math.f32_bits(inner.right()),
		math.f32_bits(inner.bottom())] == [u32(0x403b7544), 0x403b7544, 0x42c22456, 0x423c48ac]
}
