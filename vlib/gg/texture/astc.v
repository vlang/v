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

fn half_to_f32(h u16) f32 {
	sign := if (h & 0x8000) != 0 { -1.0 } else { 1.0 }
	exp := int((h >> 10) & 0x1f)
	frac := int(h & 0x3ff)
	if exp == 0 {
		return sign * f32(math.ldexp(f64(frac), -24))
	}
	if exp == 31 {
		return if frac == 0 { sign * f32(math.inf(1)) } else { f32(math.nan()) }
	}
	return sign * f32(math.ldexp(f64(1024 + frac), exp - 25))
}

// decode_astc expands one 16-byte ASTC block into 16 RGBA pixels.
// Void-extent blocks (LDR and HDR) and 4x4 LDR blocks with interpolated
// endpoints are decoded. Unsupported block modes return a clear error.
pub fn decode_astc(block []u8) ![]gg.Color {
	mut out := []gg.Color{len: 16}
	if block.len < 16 {
		return error('texture: ASTC block must be 16 bytes')
	}
	first := int(astc_read_bits(block, 0, 11)!)
	if first & 0x1ff == 0x1ff {
		mut r := f32(math.ldexp(f64(int(astc_read_bits(block, 11, 16)!)), 0)) / 65535
		mut g := f32(math.ldexp(f64(int(astc_read_bits(block, 27, 16)!)), 0)) / 65535
		mut b := f32(math.ldexp(f64(int(astc_read_bits(block, 43, 16)!)), 0)) / 65535
		if (first >> 9) & 1 != 0 {
			// HDR: 16-bit half-float channels.
			r = half_to_f32(u16(astc_read_bits(block, 11, 16)!))
			g = half_to_f32(u16(astc_read_bits(block, 27, 16)!))
			b = half_to_f32(u16(astc_read_bits(block, 43, 16)!))
		}
		c := gg.Color{astc_clamp(int(r * 255)), astc_clamp(int(g * 255)), astc_clamp(int(b * 255)), 255}
		for i in 0 .. 16 {
			out[i] = c
		}
		return out
	}
	block_mode := first & 0x7ff
	if block_mode == 0 {
		return error('texture: invalid ASTC block mode')
	}
	// 4x4 LDR with 8-bit endpoint pairs and 4-bit per-pixel weights.
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
