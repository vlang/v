/**********************************************************************
*
* Tests for the PNG image decoder
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module png

import os
import hash.crc32

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
	png_data := read_golden(name, 'png')!
	want := read_golden(name, 'rgba')!
	w, h, _ := read_meta(name)
	got := decode(png_data) or {
		return error('decode failed for ${name}: ${err}')
	}
	if got.width != w || got.height != h {
		return error('${name}: size mismatch got ${got.width}x${got.height} want ${w}x${h}')
	}
	cmp_rgba(got.rgba, want, name)!
}

fn check_animation(name string) ! {
	png_data := read_golden(name, 'png')!
	_, _, nframes := read_meta(name)
	anim := decode_animation(png_data) or {
		return error('decode_animation failed for ${name}: ${err}')
	}
	if anim.frames.len != nframes {
		return error('${name}: frame count got ${anim.frames.len} want ${nframes}')
	}
	for i in 0 .. nframes {
		want := read_golden('${name}_frame_${i:03d}', 'rgba')!
		cmp_rgba(anim.frames[i].image.rgba, want, '${name} frame ${i}')!
	}
}

fn check_color_corrected(name string) ! {
	png_data := read_golden('${name}_cc', 'png')!
	want := read_golden('${name}_cc', 'rgba')!
	got := decode_color_corrected(png_data, 2.2) or {
		return error('decode_color_corrected failed for ${name}: ${err}')
	}
	cmp_rgba(got.rgba, want, '${name}_cc')!
}

// ============================================================================
// Static fixtures: color types, bit depths, tRNS
// ============================================================================

fn test_gray_1() ! { check_golden('test_gray_1')! }

fn test_gray_2() ! { check_golden('test_gray_2')! }

fn test_gray_4() ! { check_golden('test_gray_4')! }

fn test_gray_8() ! { check_golden('test_gray_8')! }

fn test_gray_alpha_8() ! { check_golden('test_gray_alpha_8')! }

fn test_palette_8() ! { check_golden('test_palette_8')! }

fn test_rgb_8() ! { check_golden('test_rgb_8')! }

fn test_rgb_16() ! { check_golden('test_rgb_16')! }

fn test_rgba_8() ! { check_golden('test_rgba_8')! }

fn test_rgba_16() ! { check_golden('test_rgba_16')! }

fn test_trns_gray() ! { check_golden('test_trns_gray')! }

fn test_trns_rgb() ! { check_golden('test_trns_rgb')! }

fn test_trns_palette() ! { check_golden('test_trns_palette')! }

// ============================================================================
// Adam7 interlaced fixtures
// ============================================================================

fn test_interlace_gray_1() ! { check_golden('test_interlace_gray_1')! }

fn test_interlace_gray_8() ! { check_golden('test_interlace_gray_8')! }

fn test_interlace_gray_alpha_8() ! { check_golden('test_interlace_gray_alpha_8')! }

fn test_interlace_palette_8() ! { check_golden('test_interlace_palette_8')! }

fn test_interlace_rgb_8() ! { check_golden('test_interlace_rgb_8')! }

fn test_interlace_rgb_16() ! { check_golden('test_interlace_rgb_16')! }

fn test_interlace_rgba_8() ! { check_golden('test_interlace_rgba_8')! }

fn test_interlace_rgba_16() ! { check_golden('test_interlace_rgba_16')! }

fn test_interlace_trns_rgb() ! { check_golden('test_interlace_trns_rgb')! }

// ============================================================================
// Real-world-like fixtures
// ============================================================================

fn test_real_checkerboard() ! { check_golden('real_checkerboard_rgba_8')! }

fn test_real_gray_gradient() ! { check_golden('real_gray_gradient_16x16')! }

fn test_real_gray_4() ! { check_golden('real_gray_4_16x16')! }

fn test_real_gray_16() ! { check_golden('real_gray_16_32x32')! }

fn test_real_palette() ! { check_golden('real_palette_16x16')! }

fn test_real_rgb_gradient() ! { check_golden('real_rgb_gradient_16x16')! }

fn test_real_rgb_16() ! { check_golden('real_rgb_16_16x16')! }

fn test_real_rgba_noise() ! { check_golden('real_rgba_noise_16x16')! }

fn test_real_multi_idat() ! { check_golden('real_multi_idat_rgba_8')! }

fn test_real_interlace_gray_gradient() ! { check_golden('real_interlace_gray_gradient_16x16')! }

fn test_real_interlace_gray_4() ! { check_golden('real_interlace_gray_4_16x16')! }

fn test_real_interlace_gray_16() ! { check_golden('real_interlace_gray_16_32x32')! }

fn test_real_interlace_palette() ! { check_golden('real_interlace_palette_16x16')! }

fn test_real_interlace_rgb_gradient() ! { check_golden('real_interlace_rgb_gradient_16x16')! }

fn test_real_interlace_rgb_16() ! { check_golden('real_interlace_rgb_16_16x16')! }

fn test_real_interlace_rgba_noise() ! { check_golden('real_interlace_rgba_noise_16x16')! }

// ============================================================================
// APNG fixtures
// ============================================================================

fn test_apng_single() ! { check_animation('apng_single_frame_rgba_8')! }

fn test_apng_two_frames() ! { check_animation('apng_two_frames_rgba_8')! }

fn test_apng_dispose_background() ! { check_animation('apng_dispose_background_rgba_8')! }

fn test_apng_dispose_previous() ! { check_animation('apng_dispose_previous_rgba_8')! }

fn test_apng_partial_over() ! { check_animation('apng_partial_over_rgba_8')! }

fn test_apng_multi_fdat() ! { check_animation('apng_multi_fdat_rgba_8')! }

fn test_apng_gamma_over() ! { check_animation('apng_gamma_over_rgba_8')! }

// ============================================================================
// Color correction (gAMA / cHRM)
// ============================================================================

fn test_cc_gamma_gray() ! { check_color_corrected('real_gamma_1_0_gray_8')! }

fn test_cc_gamma_rgb() ! { check_color_corrected('real_gamma_2_2_rgb_8')! }

fn test_cc_wide_gamut() ! { check_color_corrected('real_wide_gamut_rgb_8')! }

fn test_cc_apng_gamma() ! { check_color_corrected('apng_gamma_over_rgba_8')! }

// ============================================================================
// Malformed-input tests: the decoder must return an error, not panic.
// ============================================================================

fn make_chunk(ctype string, payload []u8) []u8 {
	mut out := []u8{}
	n := payload.len
	out << u8((n >> 24) & 0xff)
	out << u8((n >> 16) & 0xff)
	out << u8((n >> 8) & 0xff)
	out << u8(n & 0xff)
	mut crcbuf := ctype.bytes()
	for b in payload {
		crcbuf << b
	}
	crc := crc32.sum(crcbuf)
	for b in ctype.bytes() {
		out << b
	}
	for b in payload {
		out << b
	}
	out << u8((int(crc) >> 24) & 0xff)
	out << u8((int(crc) >> 16) & 0xff)
	out << u8((int(crc) >> 8) & 0xff)
	out << u8(int(crc) & 0xff)
	return out
}

fn png_sig() []u8 {
	return [u8(0x89), 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]
}

fn ihdr_payload(w int, h int, bd int, ct int, interlace int) []u8 {
	mut p := []u8{}
	p << u8((w >> 24) & 0xff)
	p << u8((w >> 16) & 0xff)
	p << u8((w >> 8) & 0xff)
	p << u8(w & 0xff)
	p << u8((h >> 24) & 0xff)
	p << u8((h >> 16) & 0xff)
	p << u8((h >> 8) & 0xff)
	p << u8(h & 0xff)
	p << u8(bd)
	p << u8(ct)
	p << u8(0)
	p << u8(0)
	p << u8(interlace)
	return p
}

fn expect_fail(name string, data []u8) ! {
	got := decode(data) or { return }
	return error('${name}: expected decode to fail, but it succeeded (${got.width}x${got.height})')
}

fn test_malformed_bad_signature() ! {
	expect_fail('bad_signature', [u8(0), 0, 0, 0, 0, 0, 0, 0])!
}

fn test_malformed_truncated_header() ! {
	mut data := png_sig()
	data << u8(0x00)
	data << u8(0x00)
	expect_fail('truncated_header', data)!
}

fn test_malformed_bad_crc() ! {
	mut data := png_sig()
	mut chunk := make_chunk('IHDR', ihdr_payload(1, 1, 8, 0, 0))
	// corrupt the last byte (part of the CRC)
	chunk[chunk.len - 1] = chunk[chunk.len - 1] ^ 0xff
	for b in chunk {
		data << b
	}
	expect_fail('bad_crc', data)!
}

fn test_malformed_ihdr_wrong_length() ! {
	mut data := png_sig()
	for b in make_chunk('IHDR', [u8(0x00)]) {
		data << b
	}
	for b in make_chunk('IEND', []u8{}) {
		data << b
	}
	expect_fail('ihdr_wrong_length', data)!
}

fn test_malformed_bad_bit_depth() ! {
	mut data := png_sig()
	// bit depth 3 is invalid for color type 2 (RGB)
	for b in make_chunk('IHDR', ihdr_payload(1, 1, 3, 2, 0)) {
		data << b
	}
	for b in make_chunk('IEND', []u8{}) {
		data << b
	}
	expect_fail('bad_bit_depth', data)!
}

fn test_malformed_missing_iend() ! {
	mut data := png_sig()
	for b in make_chunk('IHDR', ihdr_payload(1, 1, 8, 0, 0)) {
		data << b
	}
	expect_fail('missing_iend', data)!
}

fn test_malformed_missing_idat() ! {
	mut data := png_sig()
	for b in make_chunk('IHDR', ihdr_payload(1, 1, 8, 0, 0)) {
		data << b
	}
	for b in make_chunk('IEND', []u8{}) {
		data << b
	}
	expect_fail('missing_idat', data)!
}

fn test_malformed_dimensions_do_not_overflow_decode_buffers() ! {
	mut data := png_sig()
	data << make_chunk('IHDR', ihdr_payload(0x7fffffff, 0x7fffffff, 8, 6, 0))
	data << make_chunk('IDAT', [u8(0x78), 0x9c, 0x03, 0x00, 0x00, 0x00, 0x00, 0x01])
	data << make_chunk('IEND', []u8{})
	decode(data) or {
		assert err.msg().contains('supported buffer size'), err.msg()
		return
	}
	assert false, 'oversized dimensions must return a decode error'
}
