module texture

import gg
import math

// ETC1 intensity modifier tables. Each entry is a signed value multiplied by
// 8 and added to the 8-bit base color before clipping.
const etc1_tables = [
	[2, 5, 8, 11, 13, 16, 19, 22]!
	[3, 6, 9, 12, 15, 18, 21, 24]!
	[2, 6, 9, 13, 16, 19, 23, 26]!
	[3, 7, 10, 14, 17, 21, 24, 28]!,
]

fn etc1_clamp(v int) u8 {
	return u8(math.clip(v, 0, 255))
}

fn etc1_sign4(v int) int {
	return if (v & 8) != 0 { v - 16 } else { v }
}

// decode_etc1 expands one 8-byte ETC1 block into 16 RGBA pixels. Both
// individual and differential color modes, the flip bit, per-pixel endpoint
// selection and intensity modifiers are applied.
pub fn decode_etc1(block []u8) []gg.Color {
	mut out := []gg.Color{len: 16}
	if block.len < 8 {
		return out
	}
	mut bits := u64(0)
	for i in 0 .. 8 {
		bits = bits << 8 | u64(block[i])
	}
	get := fn (b u64, shift int, count int) int {
		return int((b >> u32(shift)) & u64((1 << u32(count)) - 1))
	}
	r1 := get(bits, 60, 4)
	r2raw := get(bits, 56, 4)
	g1 := get(bits, 52, 4)
	g2raw := get(bits, 48, 4)
	b1 := get(bits, 44, 4)
	b2raw := get(bits, 40, 4)
	table1 := get(bits, 38, 2)
	table2 := get(bits, 36, 2)
	flip := get(bits, 35, 1) != 0
	individual := get(bits, 34, 1) != 0
	base1 := [etc1_clamp(r1 * 17), etc1_clamp(g1 * 17), etc1_clamp(b1 * 17)]
	mut base2 := [u8(0), 0, 0]
	if individual {
		base2 = [etc1_clamp(r2raw * 17), etc1_clamp(g2raw * 17), etc1_clamp(b2raw * 17)]
	} else {
		for c in 0 .. 3 {
			delta := etc1_sign4([r2raw, g2raw, b2raw][c]) * 8
			base2[c] = etc1_clamp(int(base1[c]) + delta)
		}
	}
	pixel_bits := bits & 0xFFFFFFFF
	for i in 0 .. 16 {
		mut sx := i % 4
		mut sy := i / 4
		if flip {
			tmp := sx
			sx = sy
			sy = tmp
		}
		index := sy * 4 + sx
		selector := int((pixel_bits >> u32(index)) & 1)
		// The intensity modifier indexes an 8-entry table, so read all
		// three bits of the field instead of a single bit.
		modifier := int((pixel_bits >> u32(16 + index)) & 0x7)
		base := if selector == 0 { base1 } else { base2 }
		table := if selector == 0 { table1 } else { table2 }
		mag := etc1_tables[table][modifier & 0x7]
		mut color := [u8(0), 0, 0]
		for c in 0 .. 3 {
			// The low bit keeps the original on/off sign convention while the
			// full 3-bit index selects the magnitude from the 8-entry table.
			adjust := if (modifier & 1) != 0 { mag * 8 } else { -mag * 8 }
			color[c] = etc1_clamp(int(base[c]) + adjust)
		}
		out[i] = gg.Color{color[0], color[1], color[2], 255}
	}
	return out
}

// generate_mipmaps builds a chain of box-filtered RGBA levels.
pub fn generate_mipmaps(src []u8, width int, height int) [][]u8 {
	mut levels := [][]u8{}
	if width <= 0 || height <= 0 {
		return levels
	}
	// Guard the byte count in i64 so huge dimensions cannot overflow the
	// multiplication before it is compared with the source length.
	pixels := i64(width) * i64(height)
	if pixels <= 0 || pixels > i64(src.len) / 4 {
		return levels
	}
	levels << src.clone()
	mut w := width
	mut h := height
	mut prev := src
	for w > 1 || h > 1 {
		nw := if w > 1 { w / 2 } else { 1 }
		nh := if h > 1 { h / 2 } else { 1 }
		mut next := []u8{len: nw * nh * 4}
		for y in 0 .. nh {
			for x in 0 .. nw {
				for c in 0 .. 4 {
					mut sum := 0
					mut count := 0
					for dy in 0 .. 2 {
						for dx in 0 .. 2 {
							sx := x * 2 + dx
							sy := y * 2 + dy
							if sx >= w || sy >= h {
								continue
							}
							sum += int(prev[(sy * w + sx) * 4 + c])
							count++
						}
					}
					next[(y * nw + x) * 4 + c] = u8(sum / count)
				}
			}
		}
		levels << next
		// `next` is allocated fresh each level and never mutated after this
		// point, so transferring ownership avoids a full buffer copy.
		unsafe { prev = next }
		w = nw
		h = nh
	}
	return levels
}
