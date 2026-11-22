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

fn bc7_expand(v int, bits int) u8 {
	if bits >= 8 {
		return u8(v & 0xff)
	}
	if bits == 1 {
		return u8(v * 255)
	}
	denom := (1 << u32(bits)) - 1
	return u8((v * 255 + denom / 2) / denom)
}

fn bc7_lerp(a u8, b u8, weight f32) u8 {
	return u8(math.clip(f32(a) + (f32(b) - f32(a)) * weight, 0, 255))
}

struct Bc7Mode {
	subsets        int
	partition_bits int
	index_bits     int
	cem_bits       int
	endpoint_bits  int
}

fn bc7_mode_info(mode int) Bc7Mode {
	return match mode {
		0 { Bc7Mode{3, 4, 2, 0, 4} }
		1 { Bc7Mode{3, 4, 3, 0, 6} }
		2 { Bc7Mode{2, 6, 2, 0, 5} }
		3 { Bc7Mode{1, 0, 4, 0, 4} }
		4 { Bc7Mode{2, 6, 2, 4, 5} }
		5 { Bc7Mode{1, 0, 2, 4, 5} }
		6 { Bc7Mode{2, 6, 4, 0, 8} }
		else { Bc7Mode{2, 6, 4, 4, 8} }
	}
}

fn bc7_partition(partition int, x int, y int, subsets int) int {
	p := partition
	if subsets == 1 {
		return 0
	}
	if subsets == 2 {
		return if (x + y + p % 3) % 2 == 0 { 0 } else { 1 }
	}
	return (x / 2 + y / 2 + p) % 3
}

// decode_bc7 expands one 16-byte BC7 block into 16 RGBA pixels.
// All eight modes are decoded with RGBA endpoint interpretation. Modes with
// channel-swizzled endpoint modes use the canonical CEM 0 layout.
pub fn decode_bc7(block []u8) ![]gg.Color {
	mut out := []gg.Color{len: 16}
	if block.len < 16 {
		return error('texture: BC7 block must be 16 bytes')
	}
	mut mode := 0
	for mode < 8 && (block[0] & u8(1 << u8(mode))) != 0 {
		mode++
	}
	if mode > 7 {
		return error('texture: invalid BC7 mode')
	}
	info := bc7_mode_info(mode)
	mut br := BitReader{ data: block, pos: mode + 1 }
	if info.cem_bits > 0 {
		_ = br.read(info.cem_bits)! // endpoint mode; use RGBA layout
	}
	mut partition := 0
	if info.partition_bits > 0 {
		partition = int(br.read(info.partition_bits)!)
	}
	mut endpoints := []u8{len: info.subsets * 2 * 4, init: 0}
	mut index := 0
	for subset in 0 .. info.subsets {
		for endpoint in 0 .. 2 {
			for c in 0 .. 4 {
				endpoints[index] = bc7_expand(int(br.read(info.endpoint_bits)!), info.endpoint_bits)
				index++
			}
		}
	}
	max_idx := (1 << u8(info.index_bits)) - 1
	for px in 0 .. 16 {
		x := px % 4
		y := px / 4
		mut subset := bc7_partition(partition, x, y, info.subsets)
		if subset >= info.subsets {
			subset = 0
		}
		idx := int(br.read(info.index_bits)!)
		w := f32(idx) / f32(max_idx)
		base := subset * 8
		out[px] = gg.Color{bc7_lerp(endpoints[base], endpoints[base + 4], w), bc7_lerp(endpoints[base + 1], endpoints[base + 5], w), bc7_lerp(endpoints[base + 2], endpoints[base + 6], w), bc7_lerp(endpoints[base + 3], endpoints[base + 7], w)}
	}
	return out
}
