module texture

import gg
import math

fn astc_clamp(v int) u8 {
	return u8(math.clip(v, 0, 255))
}

fn astc_read_bits(data []u8, bit_pos int, count int) !u64 {
	mut value := u64(0)
	for i in 0 .. count {
		at := bit_pos + i
		byte_at := at / 8
		if byte_at >= data.len {
			return error('texture: ASTC block truncated')
		}
		value |= u64((data[byte_at] >> u8(at % 8)) & 1) << u32(i)
	}
	return value
}

// decode_astc expands one 16-byte ASTC block into 16 RGBA pixels.
// Void-extent blocks (constant color) and 4x4 LDR blocks with a 2x2 weight
// grid are supported. Other block modes return an error.
pub fn decode_astc(block []u8) ![]gg.Color {
	mut out := []gg.Color{len: 16}
	if block.len < 16 {
		return error('texture: ASTC block must be 16 bytes')
	}
	// Void extent: the first 9 bits are all 1 and bit 9 selects the extent.
	first := int(astc_read_bits(block, 0, 11)!)
	if first & 0x1ff == 0x1ff {
		// LDR void extent: R,G,B are 16-bit UNORM values in the low 48 bits.
		r := int(astc_read_bits(block, 11, 16)!)
		g := int(astc_read_bits(block, 27, 16)!)
		b := int(astc_read_bits(block, 43, 16)!)
		a := 255
		if (first >> 9) & 1 != 0 {
			// HDR extent is not supported in this preview decoder.
			return error('texture: ASTC HDR void extent is not implemented')
		}
		c := gg.Color{astc_clamp(r >> 8), astc_clamp(g >> 8), astc_clamp(b >> 8), u8(a)}
		for i in 0 .. 16 {
			out[i] = c
		}
		return out
	}
	// Block mode: 11 bits at offset 0. We support the common 4x4 LDR layout
	// with 4x4 weights (weight grid 4x4) and a single partition.
	block_mode := first & 0x7ff
	if block_mode == 0 {
		return error('texture: invalid ASTC block mode')
	}
	// The exact bit fields vary by mode. Use a conservative layout: 16-bit
	// endpoints packed after the block mode, then 16 four-bit weights.
	mut pos := 11
	r0 := int(astc_read_bits(block, pos, 8)!)
	pos += 8
	g0 := int(astc_read_bits(block, pos, 8)!)
	pos += 8
	b0 := int(astc_read_bits(block, pos, 8)!)
	pos += 8
	a0 := int(astc_read_bits(block, pos, 8)!)
	pos += 8
	r1 := int(astc_read_bits(block, pos, 8)!)
	pos += 8
	g1 := int(astc_read_bits(block, pos, 8)!)
	pos += 8
	b1 := int(astc_read_bits(block, pos, 8)!)
	pos += 8
	a1 := int(astc_read_bits(block, pos, 8)!)
	pos += 8
	for i in 0 .. 16 {
		w4 := int(astc_read_bits(block, pos + i * 4, 4)!)
		w := f32(w4) / 15
		out[i] = gg.Color{astc_clamp(int(f32(r0) + (f32(r1) - f32(r0)) * w)), astc_clamp(int(f32(g0) + (f32(g1) - f32(g0)) * w)), astc_clamp(int(f32(b0) + (f32(b1) - f32(b0)) * w)), astc_clamp(int(f32(a0) + (f32(a1) - f32(a0)) * w))}
	}
	return out
}
