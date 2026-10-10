// Coverage for `x/ttf`'s renderer and its colour helpers.
//
// `ttf_test.v` exercises `draw_text` once, against a golden bitmap, and asserts
// nothing about the drawing primitives themselves. This file covers the rest:
// `BitMap`'s transforms, the filler (scanline) buffer, the four draw styles,
// the text layout entry points, and the pure colour functions in `common.v`.
//
// The bitmap is small and hand sized so a pixel can be named directly. Colours
// are written as `0xRRGGBBAA`; `plot` stores only the low byte, so a plotted
// colour `0x000000AB` leaves `0xAB` in the buffer. Every expected byte below
// was read off the running library.
import math
import os
import x.ttf

const font_bytes = $embed_file('ttf_test_data.bin')

fn load_font() ttf.TTF_File {
	mut tf := ttf.TTF_File{}
	mut bytes := font_bytes
	tf.buf = unsafe { bytes.data().vbytes(font_bytes.len) }
	tf.init()
	return tf
}

fn new_bmp(tf &ttf.TTF_File, w int, h int) ttf.BitMap {
	font_size := 20
	device_dpi := 72
	// 20 pt at 72 dpi against a 2048 unit em: 20/2048 exactly.
	scale := f32(font_size * device_dpi) / f32(72 * int(tf.units_per_em))
	return ttf.BitMap{
		tf:       unsafe { tf }
		buf:      unsafe { malloc(w * h * 4) }
		buf_size: w * h * 4
		scale:    scale
		width:    w
		height:   h
		style:    .filled
	}
}

// px returns the single byte `plot` writes for the pixel at `x`, `y`.
fn px(bmp ttf.BitMap, x int, y int) u8 {
	return unsafe { *(bmp.buf + (x + y * bmp.width) * bmp.bp) }
}

fn inked_pixels(bmp ttf.BitMap) int {
	mut n := 0
	for i in 0 .. bmp.buf_size / bmp.bp {
		if unsafe { *(bmp.buf + i * bmp.bp) } > 0 {
			n++
		}
	}
	return n
}

fn ink_extent(bmp ttf.BitMap) (int, int) {
	mut lo := -1
	mut hi := -1
	for y in 0 .. bmp.height {
		for x in 0 .. bmp.width {
			if px(bmp, x, y) > 0 {
				if lo == -1 {
					lo = x
				}
				hi = x
			}
		}
	}
	return lo, hi
}

fn ink_rows(bmp ttf.BitMap) int {
	mut n := 0
	for y in 0 .. bmp.height {
		for x in 0 .. bmp.width {
			if px(bmp, x, y) > 0 {
				n++
				break
			}
		}
	}
	return n
}

// ---------------------------------------------------------------------
// BitMap defaults
// ---------------------------------------------------------------------

fn test_bit_map_defaults() {
	bmp := ttf.BitMap{}
	assert bmp.width == 1
	assert bmp.height == 1
	assert bmp.bp == 4
	assert bmp.bg_color == 0xFFFFFF00
	assert bmp.color == 0x000000FF
	assert bmp.scale == 1.0
	assert bmp.scale_x == 1.0
	assert bmp.scale_y == 1.0
	assert bmp.angle == 0.0
	assert bmp.space_cw == 1.0
	assert bmp.space_mult == 0.0
	assert bmp.style == .filled
	assert bmp.align == .left
	assert bmp.justify == false
	assert bmp.justify_fill_ratio == 0.5
	assert bmp.use_font_metrics == false
	assert bmp.filler.len == 0
	// The transform matrices start as the identity in 3x2 layout.
	assert bmp.tr_matrix == [f32(1), 0, 0, 0, 1, 0, 0, 0, 0]
	assert bmp.ch_matrix == [f32(1), 0, 0, 0, 1, 0, 0, 0, 0]
}

// ---------------------------------------------------------------------
// clear and plot
// ---------------------------------------------------------------------

fn test_clear_zeroes_the_whole_buffer() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.plot(0, 0, u32(0xFF))
	bmp.plot(7, 3, u32(0xFF))
	bmp.clear()
	for i in 0 .. bmp.buf_size {
		assert unsafe { *(bmp.buf + i) } == 0, 'clear must zero byte ${i}'
	}
}

fn test_plot_writes_only_the_alpha_byte() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	assert bmp.plot(0, 0, u32(0xFFFFFFFF)) == true
	// `plot` writes `u8(c & 0xFF)`, so the colour's other channels are lost.
	assert px(bmp, 0, 0) == 0xFF
	bmp.plot(1, 0, u32(0x123456AB))
	assert px(bmp, 1, 0) == 0xAB
}

fn test_plot_rejects_coordinates_outside_the_buffer() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	assert bmp.plot(-1, 0, u32(0xFF)) == false
	assert bmp.plot(8, 0, u32(0xFF)) == false
	assert bmp.plot(0, -1, u32(0xFF)) == false
	assert bmp.plot(0, 4, u32(0xFF)) == false
	assert bmp.plot(7, 3, u32(0xFF)) == true
	assert inked_pixels(bmp) == 1
}

// ---------------------------------------------------------------------
// Transforms
// ---------------------------------------------------------------------

fn test_trf_txt_and_trf_ch_are_the_identity_by_default() {
	tf := load_font()
	bmp := new_bmp(&tf, 8, 4)
	p := ttf.Point{
		x: 3
		y: 5
	}
	x1, y1 := bmp.trf_txt(&p)
	assert x1 == 3
	assert y1 == 5
	x2, y2 := bmp.trf_ch(&p)
	assert x2 == 3
	assert y2 == 5
}

fn test_set_pos_translates_the_text_transform() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.set_pos(10, 20)
	// `set_pos` writes the translation into elements 6 and 7 only.
	assert bmp.tr_matrix[6] == 10.0
	assert bmp.tr_matrix[7] == 20.0
	x, y := bmp.trf_txt(&ttf.Point{
		x: 3
		y: 5
	})
	assert x == 13
	assert y == 25
}

fn test_set_rotation_writes_the_rotation_block() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.set_rotation(f32(0))
	assert bmp.tr_matrix[0] == 1.0
	assert bmp.tr_matrix[4] == 1.0
	assert bmp.tr_matrix[1] == 0.0
	assert bmp.tr_matrix[3] == 0.0
	// A quarter turn puts -sin on element 1 and sin on element 3. `cos` and
	// `sin` of pi/2 are not exactly 0 in f32, so compare with a tolerance.
	bmp.set_rotation(f32(3.14159265 / 2))
	assert math.abs(bmp.tr_matrix[0]) < 1e-6
	assert bmp.tr_matrix[1] == -1.0
	assert bmp.tr_matrix[3] == 1.0
	assert math.abs(bmp.tr_matrix[4]) < 1e-6
}

// ---------------------------------------------------------------------
// The filler buffer
// ---------------------------------------------------------------------

fn test_init_filler_grows_to_the_bitmap_height() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.init_filler()
	assert bmp.filler.len == 4
	// Each row starts as four zeroed ints, which is why the drawing code
	// calls `clear_filler` before recording anything.
	assert bmp.filler[0] == [0, 0, 0, 0]
	// Idempotent: the growth is computed against what is already there.
	bmp.init_filler()
	bmp.init_filler()
	assert bmp.filler.len == 4
}

// NOTE: `clear_filler` and `exec_filler` both index `filler` by row without
// checking its length, so either one panics on a bitmap that has not had
// `init_filler` called on it yet. Every test below calls `init_filler` first.

fn test_fline_records_a_horizontal_span() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.init_filler()
	bmp.clear_filler()
	bmp.fline(1, 0, 5, 0, u32(0xFF))
	assert bmp.filler[0] == [1, 5]
	// A span that runs off the bottom still stops at the bitmap height.
	bmp.fline(1, 10, 1, 40, u32(0xFF))
	assert bmp.filler.len == 4
}

fn test_fline_ignores_a_span_beyond_the_right_edge() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.init_filler()
	bmp.clear_filler()
	// Both endpoints are past the width, so the bounds check returns early.
	bmp.fline(100, 0, 200, 0, u32(0xFF))
	assert bmp.filler[0].len == 0
}

fn test_fline_keeps_a_span_that_starts_off_screen() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.init_filler()
	bmp.clear_filler()
	// Only one endpoint is off-screen, so the span is still recorded and the
	// clipping is left to `plot`.
	bmp.fline(-5, 0, 3, 0, u32(0xFF))
	assert bmp.filler[0] == [-5, 3]
}

fn test_exec_filler_fills_from_start_plus_one_to_end() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.init_filler()
	bmp.clear_filler()
	bmp.fline(1, 0, 5, 0, u32(0xFF))
	bmp.fline(2, 1, 6, 1, u32(0xFF))
	bmp.exec_filler()
	// The span [1,5] fills x = 2,3,4 and the span [2,6] fills 3,4,5.
	assert px(bmp, 0, 0) == 0
	assert px(bmp, 1, 0) == 0
	assert px(bmp, 2, 0) == 0xFF
	assert px(bmp, 4, 0) == 0xFF
	assert px(bmp, 5, 0) == 0
	assert px(bmp, 3, 1) == 0xFF
	assert px(bmp, 5, 1) == 0xFF
	assert px(bmp, 6, 1) == 0
	assert inked_pixels(bmp) == 6
}

fn test_exec_filler_sorts_the_span_before_filling() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.init_filler()
	bmp.clear_filler()
	bmp.filler[0] << [5, 1]
	bmp.exec_filler()
	// `exec_filler` sorts the row first, so a recorded pair in either order
	// produces the same fill.
	assert px(bmp, 2, 0) == 0xFF
	assert px(bmp, 3, 0) == 0xFF
	assert px(bmp, 4, 0) == 0xFF
	assert px(bmp, 1, 0) == 0
	assert px(bmp, 5, 0) == 0
}

fn test_exec_filler_skips_a_row_with_an_odd_number_of_edges() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.init_filler()
	bmp.clear_filler()
	bmp.filler[0] << [0, 3, 99]
	bmp.exec_filler()
	assert inked_pixels(bmp) == 0
}

fn test_exec_filler_skips_a_span_whose_start_is_past_its_end() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.init_filler()
	bmp.clear_filler()
	bmp.filler[0] << [5, 5]
	bmp.exec_filler()
	// startx is one past the recorded value and endx is the recorded value,
	// so the pair is skipped rather than filled backwards.
	assert inked_pixels(bmp) == 0
}

// ---------------------------------------------------------------------
// line, box and quadratic under each style
// ---------------------------------------------------------------------

fn test_line_outline_writes_the_colour_byte_verbatim() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.style = .outline
	bmp.line(1, 0, 4, 0, u32(0x7F))
	assert px(bmp, 1, 0) == 0x7F
	assert px(bmp, 4, 0) == 0x7F
	assert px(bmp, 0, 0) == 0
	assert px(bmp, 5, 0) == 0
	bmp.line(1, 1, 1, 3, u32(0x55))
	assert px(bmp, 1, 1) == 0x55
	assert px(bmp, 1, 2) == 0x55
	assert px(bmp, 1, 3) == 0x55
	assert px(bmp, 2, 2) == 0
}

fn test_line_outline_is_symmetric_in_its_endpoints() {
	tf := load_font()
	mut fwd := new_bmp(&tf, 8, 4)
	fwd.style = .outline
	fwd.line(0, 0, 3, 3, u32(0x55))
	mut rev := new_bmp(&tf, 8, 4)
	rev.style = .outline
	rev.line(3, 3, 0, 0, u32(0x55))
	for y in 0 .. 4 {
		for x in 0 .. 8 {
			assert px(fwd, x, y) == px(rev, x, y)
		}
	}
	assert px(fwd, 0, 0) == 0x55
	assert px(fwd, 3, 3) == 0x55
}

fn test_line_filled_antialiases_the_same_segment() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	// `.filled` is the default style: the aliased edge is drawn, then the
	// span is recorded for the filler.
	bmp.line(1, 0, 4, 0, u32(0x7F))
	// The alias factor for a horizontal line is 0.75, so 0x7F * 0.75.
	assert px(bmp, 1, 0) == 0x5F
	assert px(bmp, 4, 0) == 0x5F
	// The span is recorded but not filled: `exec_filler` is a separate call.
	assert inked_pixels(bmp) == 4
}

fn test_line_raw_only_records_the_filler_span() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.style = .raw
	bmp.init_filler()
	bmp.clear_filler()
	bmp.line(0, 0, 3, 0, u32(0x44))
	// Nothing is plotted directly; the span only reaches the buffer if
	// `exec_filler` runs.
	assert inked_pixels(bmp) == 0
	bmp.exec_filler()
	assert inked_pixels(bmp) > 0
}

fn test_box_draws_four_edges() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.style = .outline
	bmp.box(1, 1, 4, 2, u32(0x33))
	for x in 1 .. 5 {
		assert px(bmp, x, 1) == 0x33
		assert px(bmp, x, 2) == 0x33
	}
	assert px(bmp, 1, 1) == 0x33
	assert px(bmp, 4, 1) == 0x33
	assert px(bmp, 0, 1) == 0
	assert px(bmp, 5, 1) == 0
}

fn test_quadratic_falls_back_to_a_line_for_a_flat_curve() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.style = .outline
	// The bounding box of this curve is 20 by 0 pixels, and the code draws a
	// straight line when either extent is 2 or less.
	bmp.quadratic(0, 0, 7, 0, 3, 5, u32(0x66))
	for x in 0 .. 8 {
		assert px(bmp, x, 0) == 0x66
	}
	assert inked_pixels(bmp) == 8
}

fn test_quadratic_with_equal_endpoints_draws_one_pixel() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.style = .outline
	bmp.quadratic(5, 1, 5, 1, 5, 1, u32(0x77))
	assert inked_pixels(bmp) == 1
	assert px(bmp, 5, 1) == 0x77
}

// ---------------------------------------------------------------------
// common.v: colour helpers
// ---------------------------------------------------------------------

fn test_color_multiply_alpha_scales_the_alpha_byte() {
	// Only the low byte is read, and the result is a plain integer product.
	assert ttf.color_multiply_alpha(u32(0xFF0000FF), f32(0.5)) == 0x7F
	assert ttf.color_multiply_alpha(u32(0x00000080), f32(0.5)) == 0x40
	assert ttf.color_multiply_alpha(u32(0x00000000), f32(2.0)) == 0
}

fn test_color_multiply_scales_every_channel_and_clamps() {
	// 0xFF * 0.5 is 0x7F on each channel.
	assert ttf.color_multiply(u32(0xFF0000FF), f32(0.5)) == 0x7F00007F
	assert ttf.color_multiply(u32(0x00000000), f32(0.5)) == 0
	// A level above 1 saturates rather than overflowing.
	assert ttf.color_multiply(u32(0x80808080), f32(2.0)) == 0xFFFFFFFF
}

// ---------------------------------------------------------------------
// format_texture, get_raw_bytes and the file writers
// ---------------------------------------------------------------------

fn test_format_texture_expands_coverage_into_rgba() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.plot(1, 1, u32(0x80))
	bmp.plot(2, 1, u32(0x00))
	bmp.color = u32(0x11223344)
	bmp.bg_color = u32(0xAABBCCDD)
	bmp.format_texture()
	// A covered pixel takes the foreground colour and scales its alpha by
	// the coverage byte: 0x44 * 0x80 / 256 == 0x22.
	assert px(bmp, 1, 1) == 0x11
	assert unsafe { *(bmp.buf + (1 + 1 * 8) * 4 + 1) } == 0x22
	assert unsafe { *(bmp.buf + (1 + 1 * 8) * 4 + 2) } == 0x33
	assert unsafe { *(bmp.buf + (1 + 1 * 8) * 4 + 3) } == 0x22
	// An uncovered pixel takes the background colour unchanged.
	assert px(bmp, 2, 1) == 0xAA
	assert unsafe { *(bmp.buf + (2 + 1 * 8) * 4 + 3) } == 0xDD
}

fn test_get_raw_bytes_takes_one_byte_per_pixel() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.plot(3, 2, u32(0x9A))
	raw := bmp.get_raw_bytes()
	assert raw.len == bmp.buf_size / 4
	assert raw.len == 8 * 4
	assert raw[3 + 2 * 8] == 0x9A
	assert raw[0] == 0
}

fn test_save_as_ppm_writes_a_p3_header_and_every_pixel() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.clear()
	bmp.color = u32(0x01020304)
	bmp.bg_color = u32(0x0A0B0C0D)
	path := os.join_path(os.temp_dir(), 'x_ttf_render_test_ppm.ppm')
	bmp.save_as_ppm(path)
	text := os.read_file(path) or { panic('could not read the ppm back: ${err}') }
	lines := text.split('\n')
	assert lines[0] == 'P3'
	assert lines[1] == '8 4'
	assert lines[2] == '255'
	// One triple of decimals per pixel, all on the fourth line.
	values := lines[3].trim_space().split(' ')
	assert values.len == 8 * 4 * 3
	// A cleared bitmap is all background, so every triple is the background
	// colour: 0x0A, 0x0B, 0x0C.
	assert values[0..6] == ['10', '11', '12', '10', '11', '12']
	assert lines.len == 4
	// `save_as_ppm` formats a private copy and restores the coverage buffer,
	// so the bitmap is untouched by the write.
	for i in 0 .. bmp.buf_size {
		assert unsafe { *(bmp.buf + i) } == 0, 'the bitmap must survive the write'
	}
	os.rm(path) or {}
}

fn test_save_raw_data_writes_the_same_bytes_as_get_raw_bytes() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 8, 4)
	bmp.plot(0, 0, u32(0x5A))
	bmp.plot(7, 3, u32(0x11))
	path := os.join_path(os.temp_dir(), 'x_ttf_render_test_raw.bin')
	bmp.save_raw_data(path)
	saved := os.read_bytes(path) or { panic('could not read the raw back: ${err}') }
	assert saved == bmp.get_raw_bytes()
	assert saved.len == 32
	os.rm(path) or {}
}

// ---------------------------------------------------------------------
// Layout: get_bbox, get_chars_bbox, draw_text, draw_glyph
// ---------------------------------------------------------------------

fn test_get_bbox_of_an_empty_string_is_only_the_line_height() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 40)
	w, h := bmp.get_bbox('')
	assert w == 0
	// int(|2086 - (-579)| * 20/2048)
	assert h == 26
}

fn test_get_bbox_advances_by_the_glyph_width() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 40)
	one_w, one_h := bmp.get_bbox('A')
	assert one_w == 16
	assert one_h == 26
	// Two glyphs advance by exactly twice one.
	two_w, two_h := bmp.get_bbox('AA')
	assert two_w == 32
	assert two_h == 26
}

fn test_get_bbox_charges_a_space_for_a_missing_glyph() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 40)
	space_w, _ := bmp.get_bbox(' ')
	// U+00E9 and U+263A are both absent from this font, and an unmapped code
	// point costs the same advance as a space.
	accent_w, _ := bmp.get_bbox('\u00E9')
	assert accent_w == space_w
	umbrella_w, _ := bmp.get_bbox('\u263A')
	assert umbrella_w == space_w
}

fn test_get_chars_bbox_returns_width_and_height_per_character() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 40)
	// Two entries per character: the running width and the line height.
	assert bmp.get_chars_bbox('AB') == [16, 26, 32, 26]
	assert bmp.get_chars_bbox('').len == 0
}

fn test_draw_text_returns_the_same_box_as_get_bbox() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 40)
	bmp.init_filler()
	tw, th := bmp.draw_text('A')
	bw, bh := bmp.get_bbox('A')
	assert tw == bw
	assert th == bh
	assert inked_pixels(bmp) > 0
}

fn test_draw_text_of_an_empty_string_draws_nothing() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 40)
	bmp.init_filler()
	w, h := bmp.draw_text('')
	assert w == 0
	assert h == 26
	assert inked_pixels(bmp) == 0
}

fn test_draw_glyph_returns_the_glyph_box() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 60)
	bmp.init_filler()
	// Glyph 36 is `A`, whose box the table reader reports as (69, 1634).
	x_min, x_max := bmp.draw_glyph(36)
	assert x_min == 69
	assert x_max == 1634
	assert inked_pixels(bmp) > 0
}

fn test_draw_glyph_returns_zero_for_an_invalid_glyph() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 60)
	bmp.init_filler()
	// Glyph 200 is past the end of the table, so `read_glyph` hands back the
	// zero Glyph and nothing is plotted.
	x_min, x_max := bmp.draw_glyph(200)
	assert x_min == 0
	assert x_max == 0
	assert inked_pixels(bmp) == 0
}

// ---------------------------------------------------------------------
// text_block.v
// ---------------------------------------------------------------------

fn test_get_justify_space_cw_spreads_the_shortfall() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 40)
	// (20 - 10) / 1 space / 5 px == 2.0
	assert bmp.get_justify_space_cw('a b', 10, 20, 5) == 2.0
	// No spaces in the text, so there is nothing to spread.
	assert bmp.get_justify_space_cw('ab', 10, 20, 5) == 0.0
	// A shortfall of zero spreads to zero.
	assert bmp.get_justify_space_cw('a b', 20, 20, 5) == 0.0
}

fn test_draw_text_block_restores_the_space_width() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 60)
	bmp.init_filler()
	bmp.space_cw = 3.0
	bmp.draw_text_block('A B', ttf.Text_block{
		x: 0
		y: 0
		w: 100
		h: 30
	})
	// `draw_text_block` saves `space_cw` on entry and writes it back on exit.
	assert bmp.space_cw == 3.0
}

fn test_draw_text_block_left_is_drawn_from_the_left_edge() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 60)
	bmp.align = .left
	bmp.init_filler()
	bmp.draw_text_block('A', ttf.Text_block{
		x: 0
		y: 0
		w: 100
		h: 30
	})
	lo, hi := ink_extent(bmp)
	// The glyph `A` does not reach the left edge of its own em box, so the
	// inked run starts 6 px in and is 16 px wide overall.
	assert lo == 6
	assert hi == 15
}

fn test_draw_text_block_center_and_right_offset_the_same_ink() {
	tf := load_font()
	mut left := new_bmp(&tf, 200, 60)
	left.align = .left
	left.init_filler()
	left.draw_text_block('A', ttf.Text_block{
		x: 0
		y: 0
		w: 100
		h: 30
	})
	left_lo, _ := ink_extent(left)

	mut centre := new_bmp(&tf, 200, 60)
	centre.align = .center
	centre.init_filler()
	centre.draw_text_block('A', ttf.Text_block{
		x: 0
		y: 0
		w: 100
		h: 30
	})
	centre_lo, centre_hi := ink_extent(centre)
	// The advance is 16 px and the block is 100, so the leftover 84 splits in
	// half: int(84 * 0.5) == 42.
	assert centre_lo == 48
	assert centre_hi == 57

	mut right := new_bmp(&tf, 200, 60)
	right.align = .right
	right.init_filler()
	right.draw_text_block('A', ttf.Text_block{
		x: 0
		y: 0
		w: 100
		h: 30
	})
	right_lo, right_hi := ink_extent(right)
	assert right_lo == 90
	assert right_hi == 99

	// All three offsets draw the same pixels, shifted.
	assert inked_pixels(left) == inked_pixels(centre)
	assert inked_pixels(left) == inked_pixels(right)
	assert centre_lo - left_lo == 42
	assert right_lo - centre_lo == 42
}

fn test_draw_text_block_justify_aligns_from_the_left() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 60)
	// `.justify` is a text alignment value with no offset of its own, so it
	// lays out exactly like `.left`.
	bmp.align = .justify
	bmp.justify = true
	bmp.init_filler()
	bmp.draw_text_block('A', ttf.Text_block{
		x: 0
		y: 0
		w: 100
		h: 30
	})
	lo, hi := ink_extent(bmp)
	assert lo == 6
	assert hi == 15
}

fn test_draw_text_block_justify_spreads_spaces_to_fill_the_block() {
	tf := load_font()
	mut plain := new_bmp(&tf, 200, 60)
	plain.init_filler()
	plain.draw_text_block('A A A', ttf.Text_block{
		x: 0
		y: 0
		w: 100
		h: 30
	})
	plain_w, _ := plain.get_bbox('A A A')
	_, plain_hi := ink_extent(plain)

	mut justified := new_bmp(&tf, 200, 60)
	justified.justify = true
	justified.init_filler()
	justified.draw_text_block('A A A', ttf.Text_block{
		x: 0
		y: 0
		w: 100
		h: 30
	})
	justified_w, _ := justified.get_bbox('A A A')
	_, justified_hi := ink_extent(justified)
	assert justified_w == plain_w
	// Justifying pushes the last glyph out to the right edge of the block,
	// and draws the same glyphs, only further apart.
	assert justified_hi > plain_hi
	assert justified_hi == 99
	assert inked_pixels(justified) == inked_pixels(plain)
}

fn test_draw_text_block_wraps_only_when_cut_lines_is_set() {
	tf := load_font()
	line := 'AA AA AA AA AA AA AA AA AA AA AA AA AA AA'
	mut cutting := new_bmp(&tf, 200, 60)
	cutting.init_filler()
	cutting.draw_text_block(line, ttf.Text_block{
		x:         0
		y:         0
		w:         300
		h:         200
		cut_lines: true
	})
	// Two lines of roughly the font's 26 px line height.
	assert ink_rows(cutting) == 40

	mut overflowing := new_bmp(&tf, 200, 60)
	overflowing.init_filler()
	overflowing.draw_text_block(line, ttf.Text_block{
		x:         0
		y:         0
		w:         300
		h:         200
		cut_lines: false
	})
	assert ink_rows(overflowing) == 20
	assert inked_pixels(overflowing) * 2 == inked_pixels(cutting)
}

fn test_draw_text_block_splits_on_newlines() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 60)
	bmp.init_filler()
	// Both lines fit the block, so they are drawn one under the other rather
	// than wrapped.
	bmp.draw_text_block('A\nB', ttf.Text_block{
		x: 0
		y: 0
		w: 100
		h: 30
	})
	assert ink_rows(bmp) == 40
}

fn test_draw_text_block_of_an_empty_string_draws_nothing() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 60)
	bmp.init_filler()
	bmp.draw_text_block('', ttf.Text_block{
		x: 0
		y: 0
		w: 100
		h: 30
	})
	assert inked_pixels(bmp) == 0
}

fn test_draw_text_block_drops_a_line_when_its_first_word_overflows() {
	tf := load_font()
	mut bmp := new_bmp(&tf, 200, 60)
	bmp.init_filler()
	// `AA` is 32 px wide on its own, which does not fit a 30 px block, so the
	// cut loop runs down to c == 0 and draws nothing at all.
	bmp.draw_text_block('AA AA AA', ttf.Text_block{
		x:         0
		y:         0
		w:         30
		h:         30
		cut_lines: true
	})
	// NOTE: this is current behaviour, not an intended contract: a block
	// narrower than its first word silently produces no output.
	assert inked_pixels(bmp) == 0
}
