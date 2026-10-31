/**********************************************************************
*
* Tests for the WEBP image decoder
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module webp

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

fn read_meta(name string) (int, int) {
	txt := os.read_file(golden_path(name, 'txt')) or { return 0, 0 }
	parts := txt.trim_space().split(' ')
	return parts[0].int(), parts[1].int()
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
	data := read_golden(name, 'webp')!
	want := read_golden(name, 'rgba')!
	w, h := read_meta(name)
	got := decode(data) or {
		return error('decode failed for ${name}: ${err}')
	}
	if got.width != w || got.height != h {
		return error('${name}: size mismatch got ${got.width}x${got.height} want ${w}x${h}')
	}
	cmp_rgba(got.rgba, want, name)!
}

// ============================================================================
// Lossless (VP8L) fixtures
// ============================================================================

fn test_lossless_rgb() ! {
	check_golden('lossless_rgb_8')!
}

fn test_lossless_rgba() ! {
	check_golden('lossless_rgba_8')!
}

fn test_lossless_solid() ! {
	check_golden('lossless_solid_8')!
}

fn test_lossless_noise() ! {
	check_golden('lossless_noise_8')!
}

fn test_lossless_palette() ! {
	check_golden('lossless_palette_8')!
}

fn test_lossless_transparent() ! {
	check_golden('lossless_transparent_8')!
}

fn test_lossless_odd_dims() ! {
	check_golden('lossless_odd_dims_8')!
}

fn test_lossless_odd_rgba() ! {
	check_golden('lossless_odd_rgba_8')!
}

fn test_lossless_wide() ! {
	check_golden('lossless_wide_8')!
}

fn test_lossless_tall() ! {
	check_golden('lossless_tall_8')!
}

fn test_lossless_large() ! {
	check_golden('lossless_large_8')!
}

// ============================================================================
// Lossy (VP8) fixtures
// ============================================================================

fn test_lossy_rgb() ! {
	check_golden('lossy_rgb_8')!
}

fn test_lossy_alpha() ! {
	check_golden('lossy_alpha_8')!
}

fn test_lossy_alpha_16() ! {
	check_golden('lossy_alpha_16')!
}

fn test_lossy_alpha_odd() ! {
	check_golden('lossy_alpha_odd_8')!
}

fn test_lossy_wide() ! {
	check_golden('lossy_wide_8')!
}

fn test_lossy_tall() ! {
	check_golden('lossy_tall_8')!
}

fn test_lossy_real() ! {
	check_golden('test')!
}

// ============================================================================
// Geometry (crop + scale) goldens.
// Meta: out_w out_h scale_w scale_h crop cx cy cw ch.
// ============================================================================

fn read_geom_meta(name string) (int, int, Options) {
	txt := os.read_file(golden_path(name, 'txt')) or { return 0, 0, Options{} }
	p := txt.trim_space().split(' ')
	if p.len < 9 {
		return 0, 0, Options{}
	}
	opts := Options{
		scale_w: p[2].int()
		scale_h: p[3].int()
		crop:    p[4].int() != 0
		crop_x:  p[5].int()
		crop_y:  p[6].int()
		crop_w:  p[7].int()
		crop_h:  p[8].int()
	}
	return p[0].int(), p[1].int(), opts
}

fn check_geom_golden(name string, base string) ! {
	data := read_golden(base, 'webp')!
	want := read_golden(name, 'rgba')!
	w, h, opts := read_geom_meta(name)
	got := decode_with(data, opts) or {
		return error('decode_with failed for ${name}: ${err}')
	}
	if got.width != w || got.height != h {
		return error('${name}: size mismatch got ${got.width}x${got.height} want ${w}x${h}')
	}
	cmp_rgba(got.rgba, want, name)!
}

fn test_geom_rgb_shrink() ! {
	check_geom_golden('geom_rgb_shrink', 'lossless_rgb_8')!
}

fn test_geom_rgb_expand() ! {
	check_geom_golden('geom_rgb_expand', 'lossless_rgb_8')!
}

fn test_geom_rgb_aspect() ! {
	check_geom_golden('geom_rgb_aspect', 'lossless_rgb_8')!
}

fn test_geom_rgb_crop() ! {
	check_geom_golden('geom_rgb_crop', 'lossless_rgb_8')!
}

fn test_geom_rgb_crop_scale() ! {
	check_geom_golden('geom_rgb_crop_scale', 'lossless_rgb_8')!
}

fn test_geom_rgb_identity() ! {
	check_geom_golden('geom_rgb_identity', 'lossless_rgb_8')!
}

fn test_geom_rgb_tiny() ! {
	check_geom_golden('geom_rgb_tiny', 'lossless_rgb_8')!
}

fn test_geom_rgb_w2() ! {
	check_geom_golden('geom_rgb_w2', 'lossless_rgb_8')!
}

fn test_geom_odd_shrink() ! {
	check_geom_golden('geom_odd_shrink', 'lossless_odd_dims_8')!
}

fn test_geom_odd_expand() ! {
	check_geom_golden('geom_odd_expand', 'lossless_odd_dims_8')!
}

fn test_geom_alpha_scale() ! {
	check_geom_golden('geom_alpha_scale', 'lossy_alpha_8')!
}

fn expect_geom_fail(data []u8, opts Options, label string) ! {
	got := decode_with(data, opts) or { return }
	return error('${label}: expected error, got ${got.width}x${got.height}')
}

fn test_geom_crop_out_of_bounds() ! {
	data := read_golden('lossless_rgb_8', 'webp')!
	expect_geom_fail(data, Options{ crop: true, crop_w: 99, crop_h: 99 }, 'crop oob')!
}

fn test_geom_crop_negative() ! {
	data := read_golden('lossless_rgb_8', 'webp')!
	expect_geom_fail(data, Options{ crop: true, crop_x: -1, crop_w: 4, crop_h: 4 }, 'crop negative')!
}

fn test_geom_crop_zero_size() ! {
	data := read_golden('lossless_rgb_8', 'webp')!
	expect_geom_fail(data, Options{ crop: true, crop_w: 0, crop_h: 4 }, 'crop zero')!
}

fn test_geom_scale_zero_is_noop() ! {
	data := read_golden('lossless_rgb_8', 'webp')!
	plain := decode(data)!
	scaled := decode_with(data, Options{ scale_w: 0, scale_h: 0 })!
	if scaled.width != plain.width || scaled.height != plain.height {
		return error('scale zero: size mismatch ${scaled.width}x${scaled.height} vs ${plain.width}x${plain.height}')
	}
	cmp_rgba(scaled.rgba, plain.rgba, 'scale zero noop')!
}

fn test_geom_scale_negative() ! {
	data := read_golden('lossless_rgb_8', 'webp')!
	expect_geom_fail(data, Options{ scale_w: -4, scale_h: 8 }, 'scale negative')!
}

// ============================================================================
// Animation (W12)
// ============================================================================

struct AnimMeta {
mut:
	nframes int
	loop    int
	w       int
	h       int
	bg_r    int
	bg_g    int
	bg_b    int
	bg_a    int
	durs    []int
}

fn read_anim_meta(name string) !AnimMeta {
	txt := os.read_file(golden_path(name, 'txt')) or {
		return error('missing ${name}.txt')
	}
	parts := txt.trim_space().split(' ')
	if parts.len < 8 {
		return error('bad anim meta for ${name}')
	}
	mut m := AnimMeta{
		nframes: parts[0].int()
		loop:    parts[1].int()
		w:       parts[2].int()
		h:       parts[3].int()
		bg_r:    parts[4].int()
		bg_g:    parts[5].int()
		bg_b:    parts[6].int()
		bg_a:    parts[7].int()
	}
	for i in 8 .. parts.len {
		m.durs << parts[i].int()
	}
	return m
}

fn free_anim(mut a WebPAnimation) {
	for i in 0 .. a.frames.len {
		unsafe { a.frames[i].rgba.free() }
	}
	unsafe { a.frames.free() }
}

fn check_anim_golden(name string) ! {
	data := read_golden(name, 'webp')!
	meta := read_anim_meta(name)!
	mut anim := decode_animation(data) or {
		return error('decode_animation failed for ${name}: ${err}')
	}
	if anim.frame_count() != meta.nframes {
		free_anim(mut anim)
		return error('${name}: frames got ${anim.frame_count()} want ${meta.nframes}')
	}
	if anim.loop_count != meta.loop {
		free_anim(mut anim)
		return error('${name}: loop got ${anim.loop_count} want ${meta.loop}')
	}
	if anim.width != meta.w || anim.height != meta.h {
		free_anim(mut anim)
		return error('${name}: size mismatch')
	}
	if anim.bg_r != meta.bg_r || anim.bg_g != meta.bg_g
		|| anim.bg_b != meta.bg_b || anim.bg_a != meta.bg_a {
		free_anim(mut anim)
		return error('${name}: background mismatch')
	}
	for i in 0 .. anim.frames.len {
		f := anim.frames[i]
		if f.duration_ms != meta.durs[i] {
			free_anim(mut anim)
			return error('${name} frame ${i}: duration got ${f.duration_ms} want ${meta.durs[i]}')
		}
		want := read_golden('${name}_f${i}', 'rgba')!
		cmp_rgba(f.rgba, want, '${name} frame ${i}') or {
			free_anim(mut anim)
			return err
		}
	}
	free_anim(mut anim)
	// Poster (frame 0) via the plain decode path.
	poster := decode(data) or {
		return error('poster decode failed for ${name}: ${err}')
	}
	want0 := read_golden(name, 'rgba')!
	cmp_rgba(poster.rgba, want0, '${name} poster') or {
		unsafe { poster.rgba.free() }
		return err
	}
	unsafe { poster.rgba.free() }
}

fn test_anim_2frame() ! {
	check_anim_golden('animated_2frame')!
}

fn test_anim_3frame_loop2() ! {
	check_anim_golden('animated_3frame_loop2')!
}

fn test_anim_dispose_bg() ! {
	check_anim_golden('animated_dispose_bg')!
}

fn test_anim_offset_keep() ! {
	check_anim_golden('animated_offset_keep')!
}

fn test_anim_offset_dispose() ! {
	check_anim_golden('animated_offset_dispose')!
}

fn test_anim_blend() ! {
	check_anim_golden('animated_blend')!
}

fn test_anim_no_blend() ! {
	check_anim_golden('animated_no_blend')!
}

fn test_anim_mixed_codecs() ! {
	check_anim_golden('animated_mixed_codecs')!
}

fn test_anim_lossy_alpha_offset() ! {
	check_anim_golden('animated_lossy_alpha_offset')!
}

// fnv1a64_from extends a running 64-bit FNV-1a hash over a byte slice.
fn fnv1a64_from(h0 u64, b []u8) u64 {
	mut h := h0
	for v in b {
		h ^= u64(v)
		h *= u64(0x100000001b3)
	}
	return h
}

// fnv1a64 computes the 64-bit FNV-1a hash of a byte slice.
fn fnv1a64(b []u8) u64 {
	return fnv1a64_from(0xcbf29ce484222325, b)
}

// test_real_4webp checks a resized (64x48) real-world 100-frame animated
// WEBP. Frame contents are pinned with FNV-1a checksums from the
// reference implementation.
fn test_real_4webp() ! {
	data := read_golden('4', 'webp')!
	mut anim := decode_animation(data) or {
		return error('decode_animation failed for 4.webp: ${err}')
	}
	if anim.frame_count() != 100 {
		free_anim(mut anim)
		return error('4.webp: frames got ${anim.frame_count()} want 100')
	}
	if anim.loop_count != 0 {
		free_anim(mut anim)
		return error('4.webp: loop got ${anim.loop_count} want 0')
	}
	if anim.total_duration_ms() != 10000 {
		free_anim(mut anim)
		return error('4.webp: total duration got ${anim.total_duration_ms()} want 10000')
	}
	if anim.width != 64 || anim.height != 48 {
		free_anim(mut anim)
		return error('4.webp: canvas got ${anim.width}x${anim.height} want 64x48')
	}
	mut all := u64(0xcbf29ce484222325)
	for i in 0 .. anim.frames.len {
		f := anim.frames[i]
		if f.width != 64 || f.height != 48 {
			free_anim(mut anim)
			return error('4.webp frame ${i}: size mismatch')
		}
		h := fnv1a64(f.rgba)
		all = fnv1a64_from(all, f.rgba)
		match i {
			0 {
				if h != u64(12416567360893434606) {
					free_anim(mut anim)
					return error('4.webp frame 0 fnv got ${h}')
				}
			}
			33 {
				if h != u64(5734615764875973948) {
					free_anim(mut anim)
					return error('4.webp frame 33 fnv got ${h}')
				}
			}
			99 {
				if h != u64(8193788520280428117) {
					free_anim(mut anim)
					return error('4.webp frame 99 fnv got ${h}')
				}
			}
			else {}
		}
	}
	if all != u64(5598144098817850334) {
		free_anim(mut anim)
		return error('4.webp all-frame fnv got ${all}')
	}
	free_anim(mut anim)
}

// test_webp_player_4webp steps the viewer-facing player through every
// frame of 4.webp and checks each step matches decode_animation.
fn test_webp_player_4webp() ! {
	data := read_golden('4', 'webp')!
	mut anim := decode_animation(data)!
	mut player := new_webp_player(data)!
	if player.frame_count() != 100 || player.canvas_width() != 64
		|| player.canvas_height() != 48 || player.loop_count() != 0 {
		return error('4.webp player: header mismatch')
	}
	if player.delay_ms(0) != 100 {
		return error('4.webp player: delay_ms(0) got ${player.delay_ms(0)} want 100')
	}
	player.reset()
	for i in 0 .. 100 {
		rgba := player.step() or {
			return error('4.webp player: step ${i} failed: ${err}')
		}
		if rgba.len != anim.frames[i].rgba.len {
			return error('4.webp player: step ${i} size mismatch')
		}
		if fnv1a64(rgba) != fnv1a64(anim.frames[i].rgba) {
			return error('4.webp player: step ${i} content mismatch')
		}
	}
	// Stepping past the last frame errors until reset.
	mut stepped_past := false
	_ = player.step() or {
		stepped_past = true
		[]u8{}
	}
	if !stepped_past {
		return error('4.webp player: expected error when stepping past the last frame')
	}
	player.reset()
	first := player.step() or {
		return error('4.webp player: step after reset failed')
	}
	if fnv1a64(first) != fnv1a64(anim.frames[0].rgba) {
		return error('4.webp player: loop did not rewind to frame 0')
	}
	player.free()
	free_anim(mut anim)
}

fn test_anim_single() ! {
	check_anim_golden('animated_single')!
}

fn test_anim_frame_index_at() ! {
	mut anim := decode_animation(read_golden('animated_2frame', 'webp')!)!
	mut ok := anim.frame_index_at(0) == 0 && anim.frame_index_at(99) == 0
		&& anim.frame_index_at(100) == 1 && anim.frame_index_at(299) == 1
		&& anim.frame_index_at(300) == 0
	free_anim(mut anim)
	if !ok {
		return error('frame_index_at (loop 0) mismatch')
	}
	mut anim2 := decode_animation(read_golden('animated_3frame_loop2', 'webp')!)!
	ok = anim2.total_duration_ms() == 180 && anim2.frame_index_at(359) == 2
		&& anim2.frame_index_at(360) == 2 && anim2.is_animated()
	free_anim(mut anim2)
	if !ok {
		return error('frame_index_at (loop 2) mismatch')
	}
}

fn test_anim_static_rejected() ! {
	_ = decode_animation(read_golden('lossless_rgb_8', 'webp')!) or {
		return
	}
	return error('expected not-animated error')
}

// --- malformed animation framing ---------------------------------------------

fn chunk_bytes(fourcc string, payload []u8) []u8 {
	mut c := []u8{}
	c << fourcc.bytes()
	c << le32(payload.len)
	c << payload
	return c
}

fn vp8x_bytes(flags int, w int, h int) []u8 {
	wm := w - 1
	hm := h - 1
	return [u8(flags), 0, 0, 0, u8(wm & 0xff), u8((wm >> 8) & 0xff), u8((wm >> 16) & 0xff),
		u8(hm & 0xff), u8((hm >> 8) & 0xff), u8((hm >> 16) & 0xff)]
}

fn anim_payload(bg int, loop int) []u8 {
	mut p := le32(bg)
	p << u8(loop & 0xff)
	p << u8((loop >> 8) & 0xff)
	return p
}

fn anmf_payload(x int, y int, w int, h int, dur int, flags int, inner []u8) []u8 {
	xh := x / 2
	yh := y / 2
	wm := w - 1
	hm := h - 1
	mut p := []u8{}
	p << u8(xh & 0xff) << u8((xh >> 8) & 0xff) << u8((xh >> 16) & 0xff)
	p << u8(yh & 0xff) << u8((yh >> 8) & 0xff) << u8((yh >> 16) & 0xff)
	p << u8(wm & 0xff) << u8((wm >> 8) & 0xff) << u8((wm >> 16) & 0xff)
	p << u8(hm & 0xff) << u8((hm >> 8) & 0xff) << u8((hm >> 16) & 0xff)
	p << u8(dur & 0xff) << u8((dur >> 8) & 0xff) << u8((dur >> 16) & 0xff)
	p << u8(flags)
	p << inner
	return p
}

fn read_le32(b []u8) int {
	return int(b[0]) | (int(b[1]) << 8) | (int(b[2]) << 16) | (int(b[3]) << 24)
}

// vp8l_chunk_of extracts the VP8L chunk (header + payload) from a golden.
fn vp8l_chunk_of(name string) ![]u8 {
	data := read_golden(name, 'webp')!
	mut pos := 12
	for pos + 8 <= data.len {
		size := read_le32(data[pos + 4..pos + 8])
		if data[pos..pos + 4] == 'VP8L'.bytes() {
			return data[pos..pos + 8 + size]
		}
		pos += 8 + size + (size % 2)
	}
	return error('no VP8L chunk in ${name}')
}

fn test_malformed_anim_anmf_without_anim() ! {
	data := riff_wrap('WEBP', [
		chunk_bytes('VP8X', vp8x_bytes(0x02, 8, 8)),
		chunk_bytes('ANMF', anmf_payload(0, 0, 8, 8, 100, 0, []u8{})),
	])
	expect_fail(data, 'anmf without anim')!
}

fn test_malformed_anim_duplicate_anim() ! {
	data := riff_wrap('WEBP', [
		chunk_bytes('VP8X', vp8x_bytes(0x02, 8, 8)),
		chunk_bytes('ANIM', anim_payload(0xFFFFFFFF, 0)),
		chunk_bytes('ANIM', anim_payload(0xFFFFFFFF, 0)),
		chunk_bytes('ANMF', anmf_payload(0, 0, 8, 8, 100, 0, vp8l_chunk_of('lossless_solid_8')!)),
	])
	expect_fail(data, 'duplicate anim')!
}

fn test_malformed_anim_short_anim() ! {
	data := riff_wrap('WEBP', [
		chunk_bytes('VP8X', vp8x_bytes(0x02, 8, 8)),
		chunk_bytes('ANIM', [u8(0), 0, 0, 0, 0]),
	])
	expect_fail(data, 'short anim')!
}

fn test_malformed_anim_short_anmf() ! {
	data := riff_wrap('WEBP', [
		chunk_bytes('VP8X', vp8x_bytes(0x02, 8, 8)),
		chunk_bytes('ANIM', anim_payload(0xFFFFFFFF, 0)),
		chunk_bytes('ANMF', [u8(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]),
	])
	expect_fail(data, 'short anmf')!
}

fn test_malformed_anim_anmf_no_image() ! {
	data := riff_wrap('WEBP', [
		chunk_bytes('VP8X', vp8x_bytes(0x02, 8, 8)),
		chunk_bytes('ANIM', anim_payload(0xFFFFFFFF, 0)),
		chunk_bytes('ANMF', anmf_payload(0, 0, 8, 8, 100, 0, []u8{})),
	])
	expect_fail(data, 'anmf no image')!
}

fn test_malformed_anim_frame_outside_canvas() ! {
	data := riff_wrap('WEBP', [
		chunk_bytes('VP8X', vp8x_bytes(0x02, 8, 8)),
		chunk_bytes('ANIM', anim_payload(0xFFFFFFFF, 0)),
		chunk_bytes('ANMF', anmf_payload(4, 4, 8, 8, 100, 0, []u8{})),
	])
	expect_fail(data, 'frame outside canvas')!
}

fn test_malformed_anim_image_outside_anmf() ! {
	data := riff_wrap('WEBP', [
		chunk_bytes('VP8X', vp8x_bytes(0x02, 8, 8)),
		chunk_bytes('ANIM', anim_payload(0xFFFFFFFF, 0)),
		vp8l_chunk_of('lossless_solid_8')!,
	])
	expect_fail(data, 'image outside anmf')!
}

fn test_malformed_anim_flag_no_frames() ! {
	data := riff_wrap('WEBP', [
		chunk_bytes('VP8X', vp8x_bytes(0x02, 8, 8)),
		chunk_bytes('ANIM', anim_payload(0xFFFFFFFF, 0)),
	])
	expect_fail(data, 'anim flag no frames')!
}

fn test_malformed_anim_no_vp8x() ! {
	data := riff_wrap('WEBP', [
		chunk_bytes('ANIM', anim_payload(0xFFFFFFFF, 0)),
		chunk_bytes('ANMF', anmf_payload(0, 0, 8, 8, 100, 0, []u8{})),
	])
	expect_fail(data, 'anim without vp8x')!
}

fn test_malformed_anim_flag_not_set() ! {
	data := riff_wrap('WEBP', [
		chunk_bytes('VP8X', vp8x_bytes(0x00, 8, 8)),
		chunk_bytes('ANIM', anim_payload(0xFFFFFFFF, 0)),
		chunk_bytes('ANMF', anmf_payload(0, 0, 8, 8, 100, 0, []u8{})),
	])
	expect_fail(data, 'anmf without anim flag')!
}

fn test_malformed_anim_size_mismatch() ! {
	data := riff_wrap('WEBP', [
		chunk_bytes('VP8X', vp8x_bytes(0x02, 8, 8)),
		chunk_bytes('ANIM', anim_payload(0xFFFFFFFF, 0)),
		chunk_bytes('ANMF', anmf_payload(0, 0, 4, 4, 100, 0, vp8l_chunk_of('lossless_solid_8')!)),
	])
	expect_fail(data, 'anmf size mismatch')!
}

fn test_malformed_anim_alph_with_vp8l() ! {
	mut inner := chunk_bytes('ALPH', [u8(0), 0, 0, 0])
	inner << vp8l_chunk_of('lossless_solid_8')!
	data := riff_wrap('WEBP', [
		chunk_bytes('VP8X', vp8x_bytes(0x02, 8, 8)),
		chunk_bytes('ANIM', anim_payload(0xFFFFFFFF, 0)),
		chunk_bytes('ANMF', anmf_payload(0, 0, 8, 8, 100, 0, inner)),
	])
	expect_fail(data, 'alph with vp8l')!
}

// ============================================================================
// Malformed-input tests: the decoder must return an error, not panic.
// ============================================================================

fn le32(v int) []u8 {
	return [u8(v & 0xff), u8((v >> 8) & 0xff), u8((v >> 16) & 0xff), u8((v >> 24) & 0xff)]
}

fn riff_wrap(fourcc string, chunks [][]u8) []u8 {
	mut body := []u8{}
	body << fourcc.bytes()
	for c in chunks {
		body << c
		if c.len % 2 == 1 {
			body << u8(0)
		}
	}
	mut out := []u8{}
	out << 'RIFF'.bytes()
	out << le32(4 + body.len)
	out << body
	return out
}

fn expect_fail(data []u8, label string) ! {
	got := decode(data) or { return }
	return error('${label}: expected error, got ${got.width}x${got.height}')
}

fn test_malformed_bad_riff_magic() ! {
	mut d := []u8{}
	d << 'NSIF'.bytes()
	d << le32(4)
	d << 'WEBP'.bytes()
	expect_fail(d, 'bad riff')!
}

fn test_malformed_bad_webp_fourcc() ! {
	mut d := []u8{}
	d << 'RIFF'.bytes()
	d << le32(4)
	d << 'XXXX'.bytes()
	expect_fail(d, 'bad fourcc')!
}

fn test_malformed_too_short() ! {
	expect_fail([u8(0x52), u8(49), u8(70), u8(70)], 'too short')!
}

fn test_malformed_riff_size_exceeds() ! {
	mut d := []u8{}
	d << 'RIFF'.bytes()
	d << le32(1000)
	d << 'WEBP'.bytes()
	expect_fail(d, 'riff size exceeds')!
}

fn test_malformed_no_image_chunk() ! {
	// VP8X only, no image chunk.
	mut vp8x := []u8{}
	vp8x << 'VP8X'.bytes()
	vp8x << le32(10)
	vp8x << [u8(0), 0, 0, 0, 9, 0, 0, 9, 0, 0]
	expect_fail(riff_wrap('WEBP', [vp8x]), 'no image chunk')!
}

fn test_malformed_vp8l_bad_signature() ! {
	mut vp8l := []u8{}
	vp8l << 'VP8L'.bytes()
	vp8l << le32(4)
	vp8l << [u8(0x00), 0, 0, 0]
	expect_fail(riff_wrap('WEBP', [vp8l]), 'vp8l bad sig')!
}

fn test_malformed_vp8l_bad_version() ! {
	// signature 0x2f, width=1(14b), height=1(14b), alpha=0, version=1
	mut bits := 0
	bits |= (0) << 0 // width-1 = 0
	bits |= (0) << 14 // height-1 = 0
	bits |= (0) << 28 // alpha
	bits |= (1) << 29 // version = 1
	mut vp8l := []u8{}
	vp8l << 'VP8L'.bytes()
	vp8l << le32(5)
	vp8l << [u8(0x2f), u8(bits & 0xff), u8((bits >> 8) & 0xff), u8((bits >> 16) & 0xff),
		u8((bits >> 24) & 0xff)]
	expect_fail(riff_wrap('WEBP', [vp8l]), 'vp8l bad version')!
}

fn test_malformed_vp8_non_keyframe() ! {
	// frame header with keyframe bit set (bit0=1 => not keyframe)
	mut vp8 := []u8{}
	vp8 << 'VP8 '.bytes()
	vp8 << le32(4)
	vp8 << [u8(0x01), 0, 0, 0]
	expect_fail(riff_wrap('WEBP', [vp8]), 'vp8 non-keyframe')!
}

fn test_malformed_vp8_bad_codeword() ! {
	// keyframe (bit0=0), show=1, but wrong code word
	mut body := []u8{}
	body << [u8(0x10), 0, 0] // keyframe=0, show=1
	body << [u8(0x00), 0x00, 0x00, 0x00, 0x00, 0x00] // wrong code word + dims
	mut vp8 := []u8{}
	vp8 << 'VP8 '.bytes()
	vp8 << le32(body.len)
	vp8 << body
	expect_fail(riff_wrap('WEBP', [vp8]), 'vp8 bad codeword')!
}

fn test_malformed_vp8x_reserved_bytes() ! {
	mut vp8x := []u8{}
	vp8x << 'VP8X'.bytes()
	vp8x << le32(10)
	vp8x << [u8(0), 1, 0, 0, 9, 0, 0, 9, 0, 0] // reserved byte nonzero
	mut vp8l := []u8{}
	vp8l << 'VP8L'.bytes()
	vp8l << le32(4)
	vp8l << [u8(0x2f), 0, 0, 0]
	expect_fail(riff_wrap('WEBP', [vp8x, vp8l]), 'vp8x reserved')!
}

fn test_malformed_vp8x_reserved_flags() ! {
	mut vp8x := []u8{}
	vp8x << 'VP8X'.bytes()
	vp8x << le32(10)
	vp8x << [u8(0xC1), 0, 0, 0, 9, 0, 0, 9, 0, 0] // reserved flag bits set
	mut vp8l := []u8{}
	vp8l << 'VP8L'.bytes()
	vp8l << le32(4)
	vp8l << [u8(0x2f), 0, 0, 0]
	expect_fail(riff_wrap('WEBP', [vp8x, vp8l]), 'vp8x reserved flags')!
}

fn test_malformed_alph_without_vp8x() ! {
	mut vp8 := []u8{}
	vp8 << 'VP8 '.bytes()
	vp8 << le32(4)
	vp8 << [u8(0x10), 0, 0, 0]
	mut alph := []u8{}
	alph << 'ALPH'.bytes()
	alph << le32(1)
	alph << [u8(0)]
	expect_fail(riff_wrap('WEBP', [vp8, alph]), 'alph without vp8x')!
}

fn test_malformed_animation_rejected() ! {
	mut anmf := []u8{}
	anmf << 'ANMF'.bytes()
	anmf << le32(4)
	anmf << [u8(0), 0, 0, 0]
	mut vp8l := []u8{}
	vp8l << 'VP8L'.bytes()
	vp8l << le32(4)
	vp8l << [u8(0x2f), 0, 0, 0]
	expect_fail(riff_wrap('WEBP', [anmf, vp8l]), 'animation rejected')!
}

fn test_output_options_reject_unrepresentable_buffers() ! {
	data := os.read_bytes(golden_path('test', 'webp'))!
	for opts in [Options{ scale_w: max_int, scale_h: 1 }, Options{ scale_h: max_int },
		Options{ crop: true, crop_x: 1, crop_w: max_int, crop_h: 1 }] {
		decode_with(data, opts) or { continue }
		assert false, 'unrepresentable output options must return an error'
	}
}

fn test_arena_keeps_owned_headers_across_growth() {
	mut arena := Arena{}
	for i in 0 .. 1025 {
		values := [i]
		arena.track(values)
		assert arena.blocks[i].data == values.data
		assert arena.blocks[i].element_size == sizeof(int)
	}
	arena.free_all()
	arena.free_all()
	assert arena.n == 0
	assert arena.blocks.len == 0
}
