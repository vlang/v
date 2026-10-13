/**********************************************************************
*
* Tests for the BMP image decoder
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module bmp

import os

fn fixture_dir() string {
	return @DIR + '/testdata'
}

fn check_fixture(name string) !void {
	dir := fixture_dir()
	data := os.read_bytes(os.join_path(dir, name + '.bmp')) or { return error('missing bmp ${name}') }
	golden := os.read_bytes(os.join_path(dir, name + '.rgba')) or { return error('missing rgba ${name}') }
	txt := os.read_file(os.join_path(dir, name + '.txt')) or { return error('missing txt ${name}') }
	parts := txt.trim_space().split(' ')
	w := parts[0].int()
	h := parts[1].int()
	img := decode(data) or { return error('decode failed ${name}: ${err}') }
	if img.width != w || img.height != h {
		return error('${name}: size ${img.width}x${img.height} != ${w}x${h}')
	}
	if img.rgba.len != golden.len {
		return error('${name}: rgba len ${img.rgba.len} != ${golden.len}')
	}
	for i in 0 .. golden.len {
		if img.rgba[i] != golden[i] {
			return error('${name}: byte ${i} got ${img.rgba[i]} want ${golden[i]}')
		}
	}
}

fn test_bmp_goldens() {
	names := [
		'rgb24',
		'rgb24_topdown',
		'rgb32',
		'rgb16_555',
		'bitfields_565',
		'bitfields_32_rgba',
		'idx8',
		'idx4',
		'idx1',
		'core12_idx8',
		'rle8',
		'rle4',
	]
	for n in names {
		check_fixture(n) or {
			assert false, '${err}'
		}
	}
}

fn test_dimensions_do_not_overflow_decode_buffers() ! {
	for name in ['rgb24', 'rle8'] {
		mut data := os.read_bytes(os.join_path(fixture_dir(), name + '.bmp'))!
		for start in [18, 22] {
			for i in 0 .. 4 {
				data[start + i] = u8(u32(0x7fffffff) >> (i * 8))
			}
		}
		decode(data) or {
			assert err.msg().contains('supported buffer size'), err.msg()
			continue
		}
		assert false, 'oversized dimensions must return a decode error'
	}
}
