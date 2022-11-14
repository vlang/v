import x.ttf
import os
import strings

/**********************************************************************
*
* BMP render module utility functions
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* Note:
* use `v -d create_data vlib/x/ttf/ttf_test.v` to generate binary data for this test file
*
* TODO:
* - manage text directions R to L
**********************************************************************/
const font_path = 'Qarmic_sans_Abridged.ttf'

const font_bytes = $embed_file('ttf_test_data.bin')

const test_data = '
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
00bf bfbf bfbf bfbf bfbf bfbf bf00 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
00bf bfbf bfbf bfbf bfbf bfbf bf00 0000
bfff ffff ffff ffff ffff ffff ffbf 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
bfff ffff ffff ffff ffff ffff ffbf 0000
00bf ffff ffbf ffff bfff ffff bf00 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
00bf ffff ffbf ffff bfff ffff bf00 0000
0000 0000 00bf ffff bf00 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
bf00 0000 0000 0000 0000 0000 0000 0000
0000 0000 00bf ffff bf00 0000 0000 0000
0000 0000 00bf ffff bf00 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 00bf
ffbf 0000 0000 0000 0000 0000 0000 0000
0000 0000 00bf ffff bf00 0000 0000 0000
0000 0000 00bf ffff bf00 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 00bf
ffbf 0000 0000 0000 0000 0000 0000 0000
0000 0000 00bf ffff bf00 0000 0000 0000
0000 0000 00bf ffff bf00 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 bfbf
ffbf bfbf bf00 0000 0000 0000 0000 0000
0000 0000 00bf ffff bf00 0000 0000 0000
0000 0000 00bf ffff bf00 0000 0032 72bf
bfbf 0000 0000 bfbf bfbf 5400 00bf ffff
ffff ffff ffbf 0000 0000 0000 0000 0000
0000 0000 00bf ffff bf00 0000 0032 72bf
0000 0000 00bf ffff bf00 0065 9999 ffff
ffff bf00 00bf ffff ffff ff7f 0000 bfff
bfff bfff bf00 0000 0000 0000 0000 0000
0000 0000 00bf ffff bf00 0065 9999 6500
0000 0000 00bf ffff bf00 bfff ffff ffbf
ffff ffbf bfff bfff bfbf ffff bf00 bfff
bf00 0000 0000 0000 0000 0000 0000 0000
0000 0000 00bf ffff bf00 bf72 3300 7fbf
0000 0000 00bf ffff bf7f 5fff ffbf 3f7f
8fbf ffbf ffbf 5500 0000 5fbf 0000 bfff
bf00 0000 0000 0000 0000 0000 0000 0000
0000 0000 00bf ffff bf7f 5fff ffbf 3f7f
0000 0000 00bf ffff bfbf ffbf bfbf ffff
ffff ffbf ffff ff7f 0000 0000 0000 bfff
bf00 0000 0000 0000 0000 0000 0000 0000
0000 0000 00bf ffff bfbf 00bf bfbf 8f5f
0000 0000 00bf ffff 7f5f ffff ffff ffff
ffff ffbf 5fbf ffff bfbf bfbf 0000 bfff
bf00 0000 0000 0000 0000 0000 0000 0000
0000 0000 00bf ffff 7f5f 0000 0000 0000
0000 0000 00bf ffff bfff bfff ffbf ffff
ffff ffbf 0000 5fbf ffff ffff bf00 bfff
bf00 0000 0000 0000 0000 0000 0000 0000
0000 0000 00bf ffff bfff bfff ffbf ffff
0000 0000 00bf ffff bfff bf00 0000 0000
0000 0000 0000 0000 7f7f ffff bf00 bfff
bf00 0000 bf00 0000 0000 0000 0000 0000
0000 0000 00bf ffff bfff bf00 0000 0000
0000 0000 00bf ffff bfff bf00 0000 0000
0000 bf00 bf00 0000 0055 bfff ffbf bfff
ff7f 00bf ff5f 0000 0000 0000 0000 0000
0000 0000 00bf ffff bfff bf00 0000 0000
0000 0000 00bf ffff bfbf ffbf 0000 0055
7fbf ffbf ffbf 7f55 00bf ffff bf00 7f5f
ff7f 7f5f ffbf 0000 0000 0000 0000 0000
0000 0000 00bf ffff bfbf ffbf 0000 0055
0000 0000 00bf ffff bfbf ffff bfbf bfff
ffff bfbf ffff ffff ffff ffff bf00 00bf
ffff ffff ffbf 0000 0000 0000 0000 0000
0000 0000 00bf ffff bfbf 0000 bfbf bf7f
0000 0000 00bf ffff bf00 bfff ffff ffff
ffbf 0000 bfbf ffff ffff bfbf 0000 00bf
ffbf ffff bf00 0000 0000 0000 0000 0000
0000 0000 00bf ffff bf00 bf00 0000 3f7f
0000 0000 0000 5fbf 0000 00bf ffbf 8f5f
3f00 0000 0000 5fbf bf5f 0000 0000 0000
0000 bf5f 0000 0000 0000 0000 0000 0000
0000 0000 0000 5fbf 0000 00bf ffbf 8f5f
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
0000 0000 0000 0000 0000 0000 0000 0000
'

fn save_raw_data_as_array(buf_bin []u8, file_name string) {
	mut buf := strings.new_builder(buf_bin.len * 5)
	for x in buf_bin {
		buf.write_string('0x${x:02x},')
	}
	os.write_file_array(file_name, buf) or { panic(err) }
}

fn test_main() {
	mut tf := ttf.TTF_File{}
	$if create_data ? {
		tf.buf = os.read_bytes(font_path) or { panic(err) }
		println('TrueTypeFont file [${font_path}] len: ${tf.buf.len}')
		save_raw_data_as_array(tf.buf, 'test_ttf_Font_arr.bin')
	} $else {
		mut mut_font_bytes := font_bytes
		tf.buf = unsafe { mut_font_bytes.data().vbytes(font_bytes.len) }
	}
	tf.init()
	// println("Unit per EM: $tf.units_per_em")

	w := 64
	h := 32
	bp := 4
	sz := w * h * bp

	font_size := 20
	device_dpi := 72
	scale := f32(font_size * device_dpi) / f32(72 * tf.units_per_em)

	mut bmp := ttf.BitMap{
		tf: &tf
		buf: unsafe { malloc(sz) }
		buf_size: sz
		scale: scale
		width: w
		height: h
	}

	y_base := int((tf.y_max - tf.y_min) * bmp.scale)
	bmp.clear()
	bmp.set_pos(0, y_base)
	bmp.init_filler()
	bmp.draw_text('Test Text')

	mut test_buf := get_raw_data(test_data)
	$if create_data ? {
		bmp.save_as_ppm('test_ttf.ppm')
		bmp.save_raw_data('test_ttf.bin')
		test_buf = os.read_bytes('test_ttf.bin') or { panic(err) }
	}

	ram_buf := bmp.get_raw_bytes()
	assert ram_buf.len == test_buf.len
	for i in 0 .. ram_buf.len {
		if test_buf[i] != ram_buf[i] {
			assert false
		}
	}
}

fn get_raw_data(data string) []u8 {
	mut buf := []u8{}
	mut c := 0
	mut b := u32(0)
	for ch in data {
		if ch >= `0` && ch <= `9` {
			b = b << 4
			b += u32(ch - `0`)
			c++
		} else if ch >= `a` && ch <= `f` {
			b = b << 4
			b += u32(ch - `a` + 10)
			c++
		}

		if c == 2 {
			buf << u8(b)
			b = 0
			c = 0
		}
	}
	return buf
}
