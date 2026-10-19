/**********************************************************************
*
* Tests for the JPEG image decoder
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module jpeg

import os

fn golden_path(name string, ext string) string {
	root := @DIR
	return '${root}/testdata/${name}.${ext}'
}

fn check_golden(name string) ! {
	jpg := os.read_bytes(golden_path(name, 'jpg')) or {
		return error('missing fixture ${name}.jpg')
	}
	want := os.read_bytes(golden_path(name, 'rgba')) or {
		return error('missing golden ${name}.rgba')
	}
	txt := os.read_file(golden_path(name, 'txt')) or {
		return error('missing meta ${name}.txt')
	}
	parts := txt.trim_space().split(' ')
	w := parts[0].int()
	h := parts[1].int()
	got := decode(jpg) or {
		return error('decode failed for ${name}: ${err}')
	}
	if got.width != w || got.height != h {
		return error('${name}: size mismatch got ${got.width}x${got.height} want ${w}x${h}')
	}
	if got.rgba.len != want.len {
		return error('${name}: len mismatch got ${got.rgba.len} want ${want.len}')
	}
	for i in 0 .. want.len {
		if got.rgba[i] != want[i] {
			return error('${name}: byte ${i} got ${got.rgba[i]} want ${want[i]}')
		}
	}
}

fn test_base() ! {
	check_golden('base')!
}

fn test_testorig() ! {
	check_golden('testorig')!
}

fn test_gray() ! {
	check_golden('gray')!
}

fn test_fox410() ! {
	check_golden('fox410')!
}

fn test_mjpeg() ! {
	check_golden('mjpeg')!
}

fn test_prog_gray() ! {
	check_golden('prog_gray')!
}

fn test_arith_seq() ! {
	check_golden('arith_seq')!
}

fn test_arith_prog() ! {
	check_golden('arith_prog')!
}

fn test_cmyk() ! {
	check_golden('cmyk')!
}

fn test_ycck() ! {
	check_golden('ycck')!
}

fn test_monkey12() ! {
	check_golden('monkey12')!
}

fn test_422_12() ! {
	check_golden('test422_12')!
}

fn test_444_12() ! {
	check_golden('test444_12')!
}

fn test_orig12() ! {
	check_golden('testorig12')!
}

fn test_restart_12() ! {
	check_golden('testrestart_12')!
}

fn test_prog12() ! {
	check_golden('testprog12')!
}

fn test_restart_prog_12() ! {
	check_golden('testrestart_prog_12')!
}

fn test_arith12() ! {
	check_golden('testarith12')!
}

fn test_prog_arith_12() ! {
	check_golden('testprog_arith_12')!
}

fn test_prog_arith_full_12() ! {
	check_golden('testprog_arith_full_12')!
}

// ============================================================================
// M1: the parallel render bands must be byte-identical to the serial
// path. The small fixtures fall under the auto size gate, so force the
// worker count through the JPG_THREADS escape hatch and re-check the
// goldens (covers the grayscale / CMYK / YCCK parallel branches).
// ============================================================================

fn check_golden_forced_threads(name string, threads string) ! {
	_ = os.setenv('JPG_THREADS', threads, true)
	defer {
		_ = os.unsetenv('JPG_THREADS')
	}
	check_golden(name)!
}

fn test_threads_gray() ! {
	check_golden_forced_threads('gray', '4')!
}

fn test_threads_cmyk() ! {
	check_golden_forced_threads('cmyk', '3')!
}

fn test_threads_ycck() ! {
	check_golden_forced_threads('ycck', '3')!
}

fn test_threads_base_serial_vs_parallel() ! {
	check_golden_forced_threads('base', '1')!
	check_golden_forced_threads('base', '4')!
}

// ============================================================================
// M2: the fused h2v2 chroma path must yield the same samples as the
// materialized upsample_plane path. This replays the fused pair loop
// (shared upsample_2x2_left/right helpers) against upsample_plane's
// 2x2 output on a synthetic plane.
// ============================================================================

fn test_upsample_2x2_fused_matches_materialized() ! {
	mut plane := []i16{len: 12}
	for i in 0 .. 12 {
		plane[i] = i16((i * 37) % 251)
	}
	cw := 6
	ch := 2
	mut cs := []i16{len: cw}
	mut up := []i16{len: cw * 2 * ch * 2}
	upsample_plane(plane, cw, ch, mut up, cw * 2, ch * 2, 2, 2, mut cs)
	for y in 0 .. ch * 2 {
		cy := y >> 1
		mut adj := if y & 1 == 0 { cy - 1 } else { cy + 1 }
		if adj < 0 {
			adj = 0
		}
		if adj > ch - 1 {
			adj = ch - 1
		}
		mut prev := 0
		mut cur := 3 * int(plane[cy * cw]) + int(plane[adj * cw])
		for c in 0 .. cw {
			mut next := cur
			if c + 1 < cw {
				next = 3 * int(plane[cy * cw + c + 1]) + int(plane[adj * cw + c + 1])
			}
			left := upsample_2x2_left(cur, prev, c == 0)
			right := upsample_2x2_right(cur, next, c == cw - 1)
			if int(up[y * cw * 2 + 2 * c]) != left {
				return error('row ${y} col ${c}: fused left ${left} != mat ${int(up[y * cw * 2 + 2 * c])}')
			}
			if int(up[y * cw * 2 + 2 * c + 1]) != right {
				return error('row ${y} col ${c}: fused right ${right} != mat ${int(up[y * cw * 2 + 2 * c + 1])}')
			}
			prev = cur
			cur = next
		}
	}
}

// ============================================================================
// Malformed-input tests (J9): the decoder must return an error, not panic.
// ============================================================================

fn expect_fail(name string, data []u8) ! {
	got := decode(data) or { return }
	return error('${name}: expected decode to fail, but it succeeded (${got.width}x${got.height})')
}

fn test_malformed_no_sos() ! {
	expect_fail('no_sos', [u8(0xFF), u8(0xD8), u8(0xFF), u8(0xD9)])!
}

fn test_malformed_bad_precision() ! {
	// SOF0 with precision 10 (unsupported)
	expect_fail('bad_precision', [
		u8(0xFF),
		u8(0xD8),
		u8(0xFF),
		u8(0xC0),
		u8(0x00),
		u8(0x0D),
		u8(0x0A),
		u8(0x00),
		u8(0x10),
		u8(0x00),
		u8(0x10),
		u8(0x01),
		u8(0x01),
		u8(0x11),
		u8(0x00),
	])!
}

fn test_malformed_bad_channels() ! {
	// SOF0 with 5 components (unsupported)
	expect_fail('bad_channels', [
		u8(0xFF),
		u8(0xD8),
		u8(0xFF),
		u8(0xC0),
		u8(0x00),
		u8(0x17),
		u8(0x08),
		u8(0x00),
		u8(0x10),
		u8(0x00),
		u8(0x10),
		u8(0x05),
		u8(0x01),
		u8(0x11),
		u8(0x00),
		u8(0x02),
		u8(0x11),
		u8(0x01),
		u8(0x03),
		u8(0x11),
		u8(0x01),
		u8(0x04),
		u8(0x11),
		u8(0x01),
		u8(0x05),
		u8(0x11),
		u8(0x01),
	])!
}

fn test_malformed_bad_dqt_id() ! {
	// DQT declaring table id 4 (out of range)
	mut data := [u8(0xFF), u8(0xD8), u8(0xFF), u8(0xDB), u8(0x00), u8(0x43), u8(0x04)]
	for _ in 0 .. 64 {
		data << u8(0x01)
	}
	expect_fail('bad_dqt_id', data)!
}

fn test_malformed_bad_dac_index() ! {
	// DAC with an out-of-range index (>= 32)
	expect_fail('bad_dac_index', [
		u8(0xFF),
		u8(0xD8),
		u8(0xFF),
		u8(0xCC),
		u8(0x00),
		u8(0x04),
		u8(0x20),
		u8(0x11),
		u8(0x21),
		u8(0x11),
	])!
}

fn test_malformed_truncated_sof() ! {
	// SOF0 truncated mid-header
	expect_fail('truncated_sof', [
		u8(0xFF),
		u8(0xD8),
		u8(0xFF),
		u8(0xC0),
		u8(0x00),
		u8(0x0D),
		u8(0x08),
	])!
}
