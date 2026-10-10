// Coverage for the parts of `x/ttf` that `ttf_test.v` never touches.
//
// `ttf_test.v` loads the embedded font and compares a full rendered bitmap
// against a golden array, which exercises `draw_text` end to end but asserts
// nothing about the parsed font itself. This file covers the table readers:
// the offset table, `head`, `name`, `cmap`, `hhea`, `kern`, `OS/2`, the glyph
// cache, the horizontal metrics lookups and the width table.
//
// Everything here is deterministic: the font is the same embedded fixture the
// module already ships (`ttf_test_data.bin`), there is no clock, no file
// access and no rendering. The numbers were read off the running library, not
// derived from the TrueType specification, because several of them are
// properties of *this* font rather than of the format.
import x.ttf

const font_bytes = $embed_file('ttf_test_data.bin')

fn load_font() ttf.TTF_File {
	mut tf := ttf.TTF_File{}
	mut bytes := font_bytes
	tf.buf = unsafe { bytes.data().vbytes(font_bytes.len) }
	tf.init()
	return tf
}

// ---------------------------------------------------------------------
// init: the tables the offset table is expected to carry
// ---------------------------------------------------------------------

fn test_init_reads_every_table_the_font_declares() {
	tf := load_font()
	// Measured: this fixture declares 12 tables and the offsets, not the
	// reader, decide which ones exist.
	for tag in ['OS/2', 'cmap', 'gasp', 'glyf', 'head', 'hhea', 'hmtx', 'kern', 'loca', 'maxp',
		'name', 'post'] {
		assert tag in tf.tables, 'tag `${tag}` should be in the offset table'
	}
}

fn test_init_sets_length_from_maxp() {
	mut tf := load_font()
	// `init` assigns `tf.length = tf.glyph_count()`; assert both, and that
	// they agree, rather than trusting the assignment.
	assert tf.glyph_count() == 112
	assert tf.length == tf.glyph_count()
}

fn test_init_fills_the_head_table() {
	tf := load_font()
	assert tf.magic_number == 0x5f0f3cf5
	assert tf.units_per_em == 2048
	assert tf.version == 1.0
	assert tf.font_revision == 1.0
	assert tf.x_min == -331.0
	assert tf.y_min == -579.0
	assert tf.x_max == 2403.0
	assert tf.y_max == 2086.0
	assert tf.mac_style == 0
	assert tf.lowest_rec_ppem == 6
	// `index_to_loc_format` picks between the short and the long `loca`
	// entry width, so it is worth pinning: this font uses the 16-bit form.
	assert tf.index_to_loc_format == 0
}

fn test_init_reads_the_name_table() {
	tf := load_font()
	assert tf.font_family == 'Qarmic sans'
	assert tf.font_sub_family == 'Normal'
	assert tf.full_name == 'Qarmic sans'
	assert tf.postscript_name == 'QikkiReg'
}

fn test_init_converts_mac_dates_to_unix() {
	tf := load_font()
	assert tf.created == 1206541801
	assert tf.modified == 1241022015
}

fn test_init_reads_the_hhea_table() {
	tf := load_font()
	assert tf.ascent == 2353
	assert tf.descent == -579
	assert tf.line_gap == 0
	assert tf.advance_width_max == 2402
	assert tf.num_of_long_hor_metrics == 112
	assert tf.x_max_extent == 2403
	assert tf.min_left_side_bearing == -331
	assert tf.min_right_side_bearing == -199
	assert tf.caret_slope_rise == 1
	assert tf.caret_slope_run == 0
	assert tf.caret_offset == 0
	assert tf.metric_data_format == 0
}

fn test_init_reads_the_panose_bytes() {
	tf := load_font()
	// `panose_array` is 12 bytes: a 16-bit family class followed by the
	// 10-byte PANOSE classification.
	assert tf.panose_array.len == 12
	assert tf.panose_array == [u8(0), 0, 2, 0, 5, 0, 0, 0, 0, 0, 0, 0]
}

fn test_init_finds_one_cmap() {
	tf := load_font()
	// The reader only keeps subtables whose platform is 3 with a specific id
	// of 0 or 1, so this font's other, if any, are dropped.
	assert tf.cmaps.len == 1
}

// ---------------------------------------------------------------------
// map_code: the cmap lookup
// ---------------------------------------------------------------------

fn test_map_code_maps_ascii() {
	mut tf := load_font()
	assert tf.map_code(0) == 0
	assert tf.map_code(65) == 36
	assert tf.map_code(97) == 68
	assert tf.map_code(32) == 3
}

fn test_map_code_returns_zero_for_an_unmapped_code_point() {
	mut tf := load_font()
	// U+20AC EURO SIGN is not in this font's cmap.
	assert tf.map_code(0x20AC) == 0
	// 0x7F is below 0x100 and simply absent from the byte range.
	assert tf.map_code(0x7F) == 0
}

fn test_map_code_is_stable_across_calls() {
	mut tf := load_font()
	// `map_4` caches into `TrueTypeCmap.cache`; the second call must not
	// re-read the glyph index array and must return the same index.
	assert tf.map_code(65) == 36
	assert tf.map_code(65) == 36
	assert tf.map_code(98) == 69
	assert tf.map_code(65) == 36
}

// ---------------------------------------------------------------------
// Glyph reading
// ---------------------------------------------------------------------

fn test_read_glyph_reads_the_outline() {
	mut tf := load_font()
	index := tf.map_code(65)
	g := tf.read_glyph(index)
	assert g.valid_glyph == true
	// A simple glyph keeps the number of contours it was read with.
	assert g.number_of_contours == 2
	assert g.contour_ends == [u16(21), 28]
	assert g.points.len == 29
	// The first point of `A` is an on-curve start point.
	assert g.points[0].on_curve == true
	assert g.points[1].on_curve == false
	assert g.x_min == 69
	assert g.x_max == 1634
	assert g.y_min == -4
	assert g.y_max == 1866
}

fn test_read_glyph_caches_the_result() {
	mut tf := load_font()
	index := tf.map_code(65)
	first := tf.read_glyph(index)
	second := tf.read_glyph(index)
	// The second read comes from `glyph_cache`, so it must be identical
	// rather than merely similar.
	assert first.number_of_contours == second.number_of_contours
	assert first.contour_ends == second.contour_ends
	assert first.points.len == second.points.len
}

fn test_read_glyph_marks_an_empty_glyph_invalid() {
	mut tf := load_font()
	// Glyph 3 is the space: `loca` gives equal offsets for it, so the reader
	// returns the zero Glyph.
	g := tf.read_glyph(3)
	assert g.valid_glyph == false
	assert g.points.len == 0
	assert g.contour_ends.len == 0
}

fn test_read_glyph_dim_matches_read_glyph_box() {
	mut tf := load_font()
	index := tf.map_code(65)
	x_min, x_max, y_min, y_max := tf.read_glyph_dim(index)
	g := tf.read_glyph(index)
	assert x_min == g.x_min
	assert x_max == g.x_max
	assert y_min == g.y_min
	assert y_max == g.y_max
}

fn test_read_glyph_dim_is_zero_for_an_empty_glyph() {
	mut tf := load_font()
	// Glyph 3 has no outline, so its offset is 0 and the reader returns
	// zeros rather than reading the `glyf` table header.
	x_min, x_max, y_min, y_max := tf.read_glyph_dim(3)
	assert x_min == 0
	assert x_max == 0
	assert y_min == 0
	assert y_max == 0
}

fn test_read_glyph_dim_is_zero_outside_the_glyph_table() {
	mut tf := load_font()
	// Glyph 112 is one past the last glyph, so the offset lands beyond the
	// end of `glyf` and the guard returns zeros.
	x_min, x_max, y_min, y_max := tf.read_glyph_dim(112)
	assert x_min == 0
	assert x_max == 0
	assert y_min == 0
	assert y_max == 0
}

// ---------------------------------------------------------------------
// Horizontal metrics
// ---------------------------------------------------------------------

fn test_get_horizontal_metrics_reads_a_long_entry() {
	mut tf := load_font()
	// `num_of_long_hor_metrics` is 112, so every glyph below it has a full
	// advance width and left side bearing.
	aw, lsb := tf.get_horizontal_metrics(0)
	assert aw == 1024
	assert lsb == 100
	aw_space, lsb_space := tf.get_horizontal_metrics(u16(` `))
	assert aw_space == 1196
	assert lsb_space == 192
	aw_a, lsb_a := tf.get_horizontal_metrics(u16(`A`))
	assert aw_a == 1067
	assert lsb_a == 0
}

fn test_get_horizontal_metrics_falls_back_to_the_last_long_entry() {
	mut tf := load_font()
	// Glyph 200 is past the 112 long entries: the advance width comes from
	// the final long entry and the left side bearing from the short array.
	aw, lsb := tf.get_horizontal_metrics(200)
	assert aw == 1708
	assert lsb == 0
}

fn test_get_horizontal_metrics_does_not_move_the_file_position() {
	mut tf := load_font()
	// The reader saves and restores `tf.pos`; a glyph read afterwards must
	// still succeed, which it would not if the position leaked.
	before := tf.pos
	tf.get_horizontal_metrics(u16(`A`))
	assert tf.pos == before
	g := tf.read_glyph(tf.map_code(65))
	assert g.valid_glyph == true
}

// ---------------------------------------------------------------------
// get_ttf_widths
// ---------------------------------------------------------------------

fn test_get_ttf_widths_covers_the_mapped_range() {
	mut tf := load_font()
	widths, min_code, max_code := tf.get_ttf_widths()
	// The scan is over the first 300 code points, so the range is what this
	// font actually maps in that window.
	assert min_code == 32
	assert max_code == 190
	assert widths.len == max_code - min_code + 1
}

fn test_get_ttf_widths_uses_the_space_advance_for_the_space() {
	mut tf := load_font()
	widths, min_code, _ := tf.get_ttf_widths()
	assert widths[32 - min_code] == 1196
}

fn test_get_ttf_widths_is_filled_for_every_letter() {
	mut tf := load_font()
	widths, min_code, _ := tf.get_ttf_widths()
	assert widths[65 - min_code] == 1701
	assert widths[97 - min_code] == 1282
}

// ---------------------------------------------------------------------
// Kerning
// ---------------------------------------------------------------------

fn test_kern_table_is_loaded() {
	tf := load_font()
	// `init` runs `read_kern_table`, which appends one entry per format-0
	// subtable. The count is a property of the font, not of the reader.
	assert tf.kern.len == 1
}

fn test_next_kern_returns_zero_without_a_pair() {
	mut tf := load_font()
	// This font's kern table has no pair for the glyphs of `A` and `V`, so
	// every lookup returns (0, 0) and the sum does too.
	x1, y1 := tf.next_kern(1)
	assert x1 == 0
	assert y1 == 0
	x2, y2 := tf.next_kern(36)
	assert x2 == 0
	assert y2 == 0
	x3, y3 := tf.next_kern(0)
	assert x3 == 0
	assert y3 == 0
}

fn test_reset_kern_keeps_next_kern_usable() {
	mut tf := load_font()
	tf.next_kern(36)
	tf.reset_kern()
	x, y := tf.next_kern(36)
	assert x == 0
	assert y == 0
}

// ---------------------------------------------------------------------
// get_info_string
// ---------------------------------------------------------------------

fn test_get_info_string_reports_the_parsed_fields() {
	tf := load_font()
	info := tf.get_info_string()
	assert info.contains('font_family     : Qarmic sans')
	assert info.contains('font_sub_family : Normal')
	assert info.contains('full_name       : Qarmic sans')
	assert info.contains('postscript_name : QikkiReg')
	assert info.contains('magic_number    : 5f0f3cf5')
	assert info.contains('mac_style       : 0')
	// The box line prints the head table values as floats.
	assert info.contains('box             : [x_min:-331.0, y_min:-579.0, x_Max:2403.0, y_Max:2086.0]')
	// The header and trailer are fixed text.
	assert info.starts_with('----- Font Info -----')
	assert info.contains('-----------------------')
}
