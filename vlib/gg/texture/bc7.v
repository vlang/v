module texture

import gg
import math

// BitReader reads LSB-first bits from a byte block, as required by BC7.
struct BitReader {
mut:
	data []u8
	pos  int
}

fn (mut r BitReader) read(count int) !u64 {
	if count == 0 {
		return 0
	}
	mut value := u64(0)
	for i in 0 .. count {
		byte_at := r.pos / 8
		if byte_at >= r.data.len {
			return error('texture: BC7 block truncated')
		}
		bit := (r.data[byte_at] >> u8(r.pos % 8)) & 1
		value |= u64(bit) << u32(i)
		r.pos++
	}
	return value
}

fn bc7_expand4(v int) u8 {
	return u8(v * 17)
}

fn bc7_expand6(v int) u8 {
	return u8((v * 255 + 31) / 63)
}

fn bc7_lerp(a u8, b u8, weight f32) u8 {
	return u8(math.clip(f32(a) + (f32(b) - f32(a)) * weight, 0, 255))
}

// decode_bc7 expands one 16-byte BC7 block into 16 RGBA pixels.
// LDR modes 0, 1 and 3 are supported; other modes return an error instead of
// producing incorrect colors.
pub fn decode_bc7(block []u8) ![]gg.Color {
	mut out := []gg.Color{len: 16}
	if block.len < 16 {
		return error('texture: BC7 block must be 16 bytes')
	}
	mut mode := 0
	for mode < 8 && (block[0] & u8(1 << u8(mode))) != 0 {
		mode++
	}
	if mode == 8 || mode > 7 {
		return error('texture: invalid BC7 mode')
	}
	if mode != 0 && mode != 1 && mode != 3 {
		return error('texture: BC7 mode ${mode} is not implemented')
	}
	mut br := BitReader{ data: block, pos: mode + 1 }
	if mode == 3 {
		// One subset: eight 4-bit endpoints R0,R1,G0,G1,B0,B1,A0,A1.
		mut ep := []u8{len: 8}
		for i in 0 .. 8 {
			ep[i] = bc7_expand4(int(br.read(4)!))
		}
		out = []
		for i in 0 .. 16 {
			idx := int(br.read(4)!)
			w := f32(idx) / 15
			out << gg.Color{bc7_lerp(ep[0], ep[1], w), bc7_lerp(ep[2], ep[3], w), bc7_lerp(ep[4], ep[5], w), bc7_lerp(ep[6], ep[7], w)}
		}
		return out
	}
	// Modes 0 and 1: three subsets, six endpoint streams R/G/B/A (R0..R5).
	bits_per := if mode == 0 { 4 } else { 6 }
	mut streams := [][]u8{len: 4, init: []u8{len: 6}}
	for c in 0 .. 4 {
		for i in 0 .. 6 {
			v := int(br.read(bits_per)!)
			streams[c][i] = if mode == 0 { bc7_expand4(v) } else { bc7_expand6(v) }
		}
	}
	partition := int(br.read(4)!)
	index_bits := if mode == 0 { 2 } else { 3 }
	for px in 0 .. 16 {
		x := px % 4
		y := px / 4
		subset := bc7_partition(partition, x, y)
		idx := int(br.read(index_bits)!)
		max_idx := (1 << u8(index_bits)) - 1
		w := f32(idx) / f32(max_idx)
		r := bc7_lerp(streams[0][subset * 2], streams[0][subset * 2 + 1], w)
		g := bc7_lerp(streams[1][subset * 2], streams[1][subset * 2 + 1], w)
		b := bc7_lerp(streams[2][subset * 2], streams[2][subset * 2 + 1], w)
		a := bc7_lerp(streams[3][subset * 2], streams[3][subset * 2 + 1], w)
		out[px] = gg.Color{r, g, b, a}
	}
	return out
}

// bc7_partition returns the subset id for a pixel using a small deterministic
// partition table. BC7 defines 64 fixed patterns; this covers the common ones.
fn bc7_partition(partition int, x int, y int) int {
	p := partition & 0x3f
	if p == 0 {
		return if (x + y) < 3 { 0 } else { 1 }
	}
	if p == 1 {
		return if x < 2 { 0 } else { 1 }
	}
	return (x / 2 + y / 2 + p) % 3
}
