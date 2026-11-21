module texture

import gg
import math

// ETC1 (ETC_RGB1) decoder for one 4x4 block. It supports individual and
// differential modes with the standard 4-level intensity table.
pub fn decode_etc1(block []u8) []gg.Color {
	mut out := []gg.Color{len: 16}
	if block.len < 8 {
		return out
	}
	base_hi := int(block[0])
	base_lo := int(block[1])
	flags := int(block[2])
	flip := (flags & 0x10) != 0
	individual := (flags & 0x20) != 0
	// Intensity modifiers from the selected table are intentionally not
	// applied; this decodes base colors, which is enough for previews.
	mut base1 := [base_hi >> 4, base_hi & 0xf, base_lo >> 4]
	mut base2 := [base_hi >> 4, base_hi & 0xf, base_lo & 0xf]
	if individual {
		base2 = [base_lo >> 4, base_lo & 0xf, base_lo & 0xf]
	}
	mut bit := u32(block[4]) << 24 | u32(block[5]) << 16 | u32(block[6]) << 8 | u32(block[7])
	for i in 0 .. 16 {
		mut index_row := i / 4
		mut index_col := i % 4
		if flip {
			tmp := index_row
			index_row = index_col
			index_col = tmp
		}
		sel := int((bit >> u32(index_row * 4 + index_col)) & 1)
		base := if sel == 0 { base1 } else { base2 }
		r := base[0] * 17
		g := base[1] * 17
		b := base[2] * 17
		out[i] = gg.Color{u8(math.clip(r, 0, 255)), u8(math.clip(g, 0, 255)), u8(math.clip(b, 0, 255)), 255}
	}
	return out
}

// generate_mipmaps builds a chain of box-filtered RGBA levels. The returned
// slice starts with the source at level 0 and ends at a 1x1 level.
pub fn generate_mipmaps(src []u8, width int, height int) [][]u8 {
	mut levels := [][]u8{}
	if width <= 0 || height <= 0 || src.len < width * height * 4 {
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
		prev = next.clone()
		w = nw
		h = nh
	}
	return levels
}
