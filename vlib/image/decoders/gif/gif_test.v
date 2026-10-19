/**********************************************************************
*
* Tests for the GIF image decoder
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module gif

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

fn read_meta(name string) (int, int, int, int) {
	txt := os.read_file(golden_path(name, 'txt')) or { return 0, 0, 0, 0 }
	parts := txt.trim_space().split(' ')
	return parts[0].int(), parts[1].int(), parts[2].int(), parts[3].int()
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
	data := read_golden(name, 'gif')!
	want := read_golden(name, 'rgba')!
	w, h, _, _ := read_meta(name)
	got := decode(data) or {
		return error('decode failed for ${name}: ${err}')
	}
	if got.width != w || got.height != h {
		return error('${name}: size mismatch got ${got.width}x${got.height} want ${w}x${h}')
	}
	cmp_rgba(got.rgba, want, name)!
}

fn check_animation(name string) ! {
	data := read_golden(name, 'gif')!
	_, _, nframes, loop := read_meta(name)
	anim := decode_animation(data) or {
		return error('decode_animation failed for ${name}: ${err}')
	}
	if anim.frames.len != nframes {
		return error('${name}: frame count got ${anim.frames.len} want ${nframes}')
	}
	if anim.loop_count != loop {
		return error('${name}: loop got ${anim.loop_count} want ${loop}')
	}
	for i in 0 .. nframes {
		want := read_golden('${name}_frame_${i:03d}', 'rgba')!
		cmp_rgba(anim.frames[i].image.rgba, want, '${name} frame ${i}')!
	}
}

// ============================================================================
// Pillow fixtures
// ============================================================================

fn test_indexed_gray_8() ! { check_golden('indexed_gray_8')! }

fn test_indexed_odd_8() ! { check_golden('indexed_odd_8')! }

fn test_indexed_rgb_8() ! { check_golden('indexed_rgb_8')! }

fn test_transparent_8() ! { check_golden('transparent_8')! }

fn test_animated_2frame() ! { check_animation('animated_2frame')! }

// ============================================================================
// Built fixtures: interlace, offset, transparency, local color table
// ============================================================================

fn test_interlace_8x8() ! { check_golden('interlace_8x8')! }

fn test_offset_6x6() ! { check_golden('offset_6x6')! }

fn test_transparent_idx() ! { check_golden('transparent_idx_4x4')! }

fn test_local_color_table() ! { check_golden('local_color_table_4x4')! }

// ============================================================================
// Built fixtures: multi-frame disposal + loop
// ============================================================================

fn test_multi_keep() ! { check_animation('multi_keep')! }

fn test_multi_background() ! { check_animation('multi_background')! }

fn test_multi_previous() ! { check_animation('multi_previous')! }

fn test_loop_count_3() ! { check_animation('loop_count_3')! }

// ============================================================================
// Malformed-input tests: the decoder must return an error, not panic.
// ============================================================================

fn le16(v int) []u8 {
	return [u8(v & 0xff), u8((v >> 8) & 0xff)]
}

// gif_base emits a valid GIF89a header + LSD + 2-entry global color table.
fn gif_base(width int, height int) []u8 {
	mut d := [u8(0x47), u8(0x49), u8(0x46), u8(0x38), u8(0x39), u8(0x61)]
	d << le16(width)
	d << le16(height)
	d << [u8(0x80)] // GCT present, size 0 -> 2 entries
	d << [u8(0)] // bg index
	d << [u8(0)] // aspect
	d << [u8(0), u8(0), u8(0), u8(0), u8(0), u8(0)] // 2-entry GCT
	return d
}

// image_desc_1x1 emits a 1x1 image descriptor with the given min code size
// and LZW sub-block payload (already framed with count + terminator).
fn image_desc_1x1(min_code_size int, lzw []u8) []u8 {
	mut d := [u8(0x2c)]
	d << le16(0) // left
	d << le16(0) // top
	d << le16(1) // width
	d << le16(1) // height
	d << [u8(0x00)] // no LCT, no interlace
	d << [u8(min_code_size)]
	d << lzw
	return d
}

fn expect_fail(name string, data []u8) ! {
	got := decode(data) or { return }
	return error('${name}: expected decode to fail, but it succeeded (${got.width}x${got.height})')
}

fn test_malformed_bad_signature() ! {
	mut d := [u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0x00), u8(0x00)]
	d << gif_base(2, 2)[6..]
	expect_fail('bad_signature', d)!
}

fn test_malformed_truncated_header() ! {
	expect_fail('truncated_header', [u8(0x47), u8(0x49), u8(0x46), u8(0x38)])!
}

fn test_malformed_no_image() ! {
	mut d := gif_base(2, 2)
	d << [u8(0x3b)] // trailer, no image descriptor
	expect_fail('no_image', d)!
}

fn test_malformed_unknown_introducer() ! {
	mut d := gif_base(2, 2)
	d << [u8(0x12)] // unknown block introducer
	expect_fail('unknown_introducer', d)!
}

fn test_malformed_bad_min_code_size() ! {
	mut d := gif_base(1, 1)
	d << image_desc_1x1(1, [u8(0x00)]) // min code size 1 is invalid
	d << [u8(0x3b)]
	expect_fail('bad_min_code_size', d)!
}

fn test_malformed_lzw_truncated() ! {
	mut d := gif_base(1, 1)
	d << image_desc_1x1(2, [u8(0x00)]) // empty LZW payload -> EOF
	d << [u8(0x3b)]
	expect_fail('lzw_truncated', d)!
}

fn test_malformed_palette_index_out_of_range() ! {
	// LZW codes [3, EOI] at 3 bits LSB-first pack to 0x2B; index 3 is
	// beyond the 2-entry global color table.
	mut d := gif_base(1, 1)
	d << image_desc_1x1(2, [u8(0x01), u8(0x2b), u8(0x00)])
	d << [u8(0x3b)]
	expect_fail('palette_index_out_of_range', d)!
}
