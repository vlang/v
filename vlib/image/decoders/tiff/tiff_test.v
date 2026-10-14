/**********************************************************************
*
* Tests for the TIFF image decoder
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module tiff

import os

fn golden_path(name string, ext string) string {
	root := @DIR
	return '${root}/testdata/${name}.${ext}'
}

fn read_golden(name string, ext string) ![]u8 {
	return os.read_bytes(golden_path(name, ext)) or {
		return error('missing ${name}.${ext}')
	}
}

fn read_meta(name string) (int, int, int) {
	txt := os.read_file(golden_path(name, 'txt')) or { return 0, 0, 0 }
	parts := txt.trim_space().split(' ')
	return parts[0].int(), parts[1].int(), parts[2].int()
}

fn cmp_rgba(got []u8, want []u8, label string) ! {
	if got.len != want.len {
		return error('${label}: len mismatch got ${got.len} want ${want.len}')
	}
	for i in 0 .. want.len {
		if got[i] != want[i] {
			return error('${label}: byte ${i} got ${got[i]} want ${want[i]}')
		}
	}
}

fn check_golden(name string) ! {
	tif := read_golden(name, 'tif')!
	want := read_golden(name, 'rgba')!
	w, h, _ := read_meta(name)
	got := decode(tif) or {
		return error('decode failed for ${name}: ${err}')
	}
	if got.width != w || got.height != h {
		return error('${name}: size mismatch got ${got.width}x${got.height} want ${w}x${h}')
	}
	cmp_rgba(got.rgba, want, name)!
}

fn check_pages(name string) ! {
	tif := read_golden(name, 'tif')!
	_, _, npages := read_meta(name)
	anim := decode_pages(tif) or {
		return error('decode_pages failed for ${name}: ${err}')
	}
	if anim.frames.len != npages {
		return error('${name}: page count got ${anim.frames.len} want ${npages}')
	}
	for i in 0 .. npages {
		want := read_golden('${name}_page_${i:03d}', 'rgba')!
		cmp_rgba(anim.frames[i].image.rgba, want, '${name} page ${i}')!
	}
}

// ============================================================================
// Pillow fixtures: photometrics, bit depths, compressions
// ============================================================================

fn test_rgb_8_none() ! { check_golden('rgb_8_none')! }

fn test_rgb_8_packbits() ! { check_golden('rgb_8_packbits')! }

fn test_rgb_8_lzw() ! { check_golden('rgb_8_lzw')! }

fn test_gray_8_none() ! { check_golden('gray_8_none')! }

fn test_gray_8_lzw() ! { check_golden('gray_8_lzw')! }

fn test_bilevel_1_none() ! { check_golden('bilevel_1_none')! }

fn test_palette_8_none() ! { check_golden('palette_8_none')! }

fn test_gray_16_none() ! { check_golden('gray_16_none')! }

fn test_rgba_8_none() ! { check_golden('rgba_8_none')! }

// ============================================================================
// Tiled + separate-plane fixtures
// ============================================================================

fn test_tiled_chunky_none() ! { check_golden('tiled_chunky_rgb_8_none')! }

fn test_tiled_chunky_lzw() ! { check_golden('tiled_chunky_rgb_8_lzw')! }

fn test_tiled_chunky_packbits() ! { check_golden('tiled_chunky_rgb_8_packbits')! }

fn test_tiled_edge_clip() ! { check_golden('tiled_edge_clip_rgb_8_none')! }

fn test_separate_strips_none() ! { check_golden('separate_strips_rgb_8_none')! }

fn test_separate_strips_lzw() ! { check_golden('separate_strips_rgb_8_lzw')! }

fn test_separate_tiles_none() ! { check_golden('separate_tiles_rgb_8_none')! }

// ============================================================================
// Sub-byte + palette fixtures
// ============================================================================

fn test_subbyte_1_biz() ! { check_golden('subbyte_1_black_is_zero')! }

fn test_subbyte_1_wiz() ! { check_golden('subbyte_1_white_is_zero')! }

fn test_subbyte_2_biz() ! { check_golden('subbyte_2_black_is_zero')! }

fn test_subbyte_2_wiz() ! { check_golden('subbyte_2_white_is_zero')! }

fn test_subbyte_4_biz() ! { check_golden('subbyte_4_black_is_zero')! }

fn test_subbyte_4_wiz() ! { check_golden('subbyte_4_white_is_zero')! }

fn test_palette_4_none() ! { check_golden('palette_4_none')! }

// ============================================================================
// Byte order + multi-page
// ============================================================================

fn test_big_endian() ! { check_golden('big_endian_rgb_8_none')! }

fn test_multipage() ! { check_pages('multipage_2')! }

// ============================================================================
// Malformed-input tests: the decoder must return an error, not panic.
// ============================================================================

fn le16(v int) []u8 {
	return [u8(v & 0xff), u8((v >> 8) & 0xff)]
}

fn le32(v int) []u8 {
	return [u8(v & 0xff), u8((v >> 8) & 0xff), u8((v >> 16) & 0xff), u8((v >> 24) & 0xff)]
}

fn entry_short(tag int, val int) []u8 {
	mut e := le16(tag)
	e << le16(3) // SHORT
	e << le32(1)
	e << le16(val)
	e << [u8(0), 0]
	return e
}

fn build_ifd(entries [][]u8, next int) []u8 {
	mut out := le16(entries.len)
	for e in entries {
		out << e
	}
	out << le32(next)
	return out
}

fn expect_fail(name string, data []u8) ! {
	got := decode(data) or { return }
	return error('${name}: expected decode to fail, but it succeeded (${got.width}x${got.height})')
}

fn test_malformed_bad_byte_order() ! {
	mut data := [u8(0x00), 0x00]
	data << le16(42)
	data << le32(8)
	expect_fail('bad_byte_order', data)!
}

fn test_malformed_bad_magic() ! {
	mut data := [u8(0x49), u8(0x49)]
	data << le16(99)
	data << le32(8)
	expect_fail('bad_magic', data)!
}

fn test_malformed_bigtiff() ! {
	mut data := [u8(0x49), u8(0x49)]
	data << le16(43)
	data << le32(8)
	expect_fail('bigtiff', data)!
}

fn test_malformed_ifd_out_of_range() ! {
	mut data := [u8(0x49), u8(0x49)]
	data << le16(42)
	data << le32(4)
	expect_fail('ifd_out_of_range', data)!
}

fn test_malformed_missing_width() ! {
	mut data := [u8(0x49), u8(0x49)]
	data << le16(42)
	data << le32(8)
	// IFD with only a Compression tag, no ImageWidth/ImageLength
	data << build_ifd([entry_short(259, 1)], 0)
	expect_fail('missing_width', data)!
}

fn test_malformed_ifd_loop() ! {
	mut data := [u8(0x49), u8(0x49)]
	data << le16(42)
	data << le32(8)
	// IFD whose next pointer points back to itself (offset 8)
	data << build_ifd([entry_short(256, 4)], 8)
	expect_fail('ifd_loop', data)!
}

fn test_malformed_bad_bit_depth() ! {
	mut data := [u8(0x49), u8(0x49)]
	data << le16(42)
	data << le32(8)
	// width=4, length=4, bps=3 (invalid), spp=1, photometric=1
	mut entries := [][]u8{}
	entries << entry_short(256, 4)
	entries << entry_short(257, 4)
	entries << entry_short(258, 3)
	entries << entry_short(277, 1)
	entries << entry_short(262, 1)
	data << build_ifd(entries, 0)
	expect_fail('bad_bit_depth', data)!
}

// Use one-pixel blocks so invalid vector lengths fail before pixel allocation.
fn test_malformed_block_vectors() ! {
	for tiled in [false, true] {
		for planar in [1, 2] {
			for counts in [0, 1] {
				mut entries := [entry_short(256, 2), entry_short(257, 2), entry_short(258, 8),
					entry_short(277, 1), entry_short(262, 1), entry_short(284, planar)]
				offset_tag := if tiled { 324 } else { 273 }
				count_tag := if tiled { 325 } else { 279 }
				entries << entry_short(offset_tag, 0)
				mut count_entry := entry_short(count_tag, 1)
				count_entry[4] = u8(counts)
				entries << count_entry
				if tiled {
					entries << entry_short(322, 1)
					entries << entry_short(323, 1)
				} else {
					entries << entry_short(278, 1)
				}
				mut data := [u8(0x49), 0x49]
				data << le16(42)
				data << le32(8)
				data << build_ifd(entries, 0)
				decode(data) or {
					assert err.msg().contains('arrays do not match'), err.msg()
					continue
				}
				assert false, 'incomplete block vectors must return a decode error'
			}
		}
	}
}

fn test_dimensions_do_not_overflow_decode_buffers() ! {
	mut entries := [entry_short(256, 1), entry_short(257, 1), entry_short(262, 1)]
	for i in 0 .. 2 {
		entries[i][2] = 4 // LONG dimensions, stored inline
		for b in 0 .. 4 {
			entries[i][8 + b] = u8(u32(0x7fffffff) >> (b * 8))
		}
	}
	mut data := [u8(0x49), 0x49]
	data << le16(42)
	data << le32(8)
	data << build_ifd(entries, 0)
	decode(data) or {
		assert err.msg().contains('supported buffer size'), err.msg()
		return
	}
	assert false, 'oversized dimensions must return a decode error'
}
