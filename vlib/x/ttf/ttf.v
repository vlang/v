module ttf

/**********************************************************************
*
* TrueTypeFont reader V implementation
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* Note:
* - inspired by: http://stevehanov.ca/blog/?id=143
*
* TODO:
* - check for unicode > 0xFFFF if supported
* - evaluate use a buffer for the points in the glyph
**********************************************************************/
import strings

/******************************************************************************
*
* CMAP structs
*
******************************************************************************/
struct Segment {
mut:
	id_range_offset u32
	start_code      u16
	end_code        u16
	id_delta        u16
}

struct TrueTypeCmap {
mut:
	format   int
	cache    []int = []int{len: 65536, init: -1} // for now we allocate 2^16 charcode
	segments []Segment
	arr      []int
}

/******************************************************************************
*
* TTF_File structs
*
******************************************************************************/
pub struct TTF_File {
pub mut:
	buf                     []u8
	pos                     u32
	length                  u16
	scalar_type             u32
	search_range            u16
	entry_selector          u16
	range_shift             u16
	tables                  map[string]Offset_Table
	version                 f32
	font_revision           f32
	checksum_adjustment     u32
	magic_number            u32
	flags                   u16
	units_per_em            u16
	created                 u64
	modified                u64
	x_min                   f32
	y_min                   f32
	x_max                   f32
	y_max                   f32
	mac_style               u16
	lowest_rec_ppem         u16
	font_direction_hint     i16
	index_to_loc_format     i16
	glyph_data_format       i16
	font_family             string
	font_sub_family         string
	full_name               string
	postscript_name         string
	cmaps                   []TrueTypeCmap
	ascent                  i16
	descent                 i16
	line_gap                i16
	advance_width_max       u16
	min_left_side_bearing   i16
	min_right_side_bearing  i16
	x_max_extent            i16
	caret_slope_rise        i16
	caret_slope_run         i16
	caret_offset            i16
	metric_data_format      i16
	num_of_long_hor_metrics u16
	kern                    []Kern0Table
	// cache
	glyph_cache map[int]Glyph
	// font widths array scale for PDF export
	width_scale f32 = 1.0
}

pub fn (mut tf TTF_File) init() {
	tf.read_offset_tables()
	tf.read_head_table()
	// dprintln(tf.get_info_string())
	tf.read_name_table()
	tf.read_cmap_table()
	tf.read_hhea_table()
	tf.read_kern_table()
	tf.length = tf.glyph_count()
	dprintln('Number of symbols: $tf.length')
	dprintln('*****************************')
	dprintln('Unit per em: $tf.units_per_em')
	dprintln('advance_width_max: $tf.advance_width_max')
}

/******************************************************************************
*
* TTF_File Glyph Structs
*
******************************************************************************/
pub struct Point {
pub mut:
	x        int
	y        int
	on_curve bool
}

struct Gylph_Component {
mut:
	points []Point
}

// type of glyph
const (
	g_type_simple  = u16(1) // simple type
	g_type_complex = u16(2) // compound type
)

pub struct Glyph {
pub mut:
	g_type             u16 = ttf.g_type_simple
	contour_ends       []u16
	number_of_contours i16
	points             []Point
	x_min              i16
	x_max              i16
	y_min              i16
	y_max              i16
	valid_glyph        bool
	components         []Component
}

/******************************************************************************
*
* TTF_File metrics and glyph
*
******************************************************************************/
pub fn (mut tf TTF_File) get_horizontal_metrics(glyph_index u16) (int, int) {
	assert 'hmtx' in tf.tables
	old_pos := tf.pos
	mut offset := tf.tables['hmtx'].offset

	mut advance_width := 0
	mut left_side_bearing := 0
	if glyph_index < tf.num_of_long_hor_metrics {
		offset += glyph_index * 4
		tf.pos = offset
		advance_width = tf.get_u16()
		left_side_bearing = tf.get_i16()
		// dprintln("${glyph_index} aw:${advance_width} lsb:${left_side_bearing}")
	} else {
		// read the last entry of the hMetrics array
		tf.pos = offset + (tf.num_of_long_hor_metrics - 1) * 4
		advance_width = tf.get_u16()
		tf.pos = offset + tf.num_of_long_hor_metrics * 4 +
			2 * (glyph_index - tf.num_of_long_hor_metrics)
		left_side_bearing = tf.get_fword()
	}
	tf.pos = old_pos
	return advance_width, left_side_bearing
}

fn (mut tf TTF_File) get_glyph_offset(index u32) u32 {
	// check if needed tables exists
	assert 'loca' in tf.tables
	assert 'glyf' in tf.tables
	mut old_pos := tf.pos

	table := tf.tables['loca']
	mut offset := u32(0)
	mut next := u32(0)
	if tf.index_to_loc_format == 1 {
		tf.pos = table.offset + (index << 2)
		offset = tf.get_u32()
		next = tf.get_u32()
	} else {
		tf.pos = table.offset + (index << 1)
		offset = tf.get_u16() << 1
		next = tf.get_u16() << 1
	}

	if offset == next {
		// indicates glyph has no outline( eg space)
		return 0
	}
	// dprintln("Offset for glyph index $index is $offset")
	tf.pos = old_pos
	return offset + tf.tables['glyf'].offset
}

pub fn (mut tf TTF_File) glyph_count() u16 {
	assert 'maxp' in tf.tables
	old_pos := tf.pos
	tf.pos = tf.tables['maxp'].offset + 4
	count := tf.get_u16()
	tf.pos = old_pos
	return count
}

pub fn (mut tf TTF_File) read_glyph_dim(index u16) (int, int, int, int) {
	offset := tf.get_glyph_offset(index)
	// dprintln("offset: $offset")
	if offset == 0 || offset >= tf.tables['glyf'].offset + tf.tables['glyf'].length {
		dprintln('No glyph found!')
		return 0, 0, 0, 0
	}

	assert offset >= tf.tables['glyf'].offset
	assert offset < tf.tables['glyf'].offset + tf.tables['glyf'].length

	tf.pos = offset
	// dprintln("file seek read_glyph: $tf.pos")

	// number_of_contours
	_ := tf.get_i16()
	x_min := tf.get_fword()
	y_min := tf.get_fword()
	x_max := tf.get_fword()
	y_max := tf.get_fword()

	return x_min, x_max, y_min, y_max
}

pub fn (mut tf TTF_File) get_ttf_widths() ([]int, int, int) {
	mut space_cw, _ := tf.get_horizontal_metrics(u16(` `))
	// div_space_cw := int((f32(space_cw) * 0.3))

	// count := int(tf.glyph_count())
	mut min_code := 0xFFFF + 1
	mut max_code := 0
	for i in 0 .. 300 {
		glyph_index := tf.map_code(i)
		if glyph_index == 0 {
			continue
		}
		// dprintln("$i = glyph_index: $glyph_index ${i:c}")
		if i > max_code {
			max_code = i
		}
		if i < min_code {
			min_code = i
		}
	}
	// dprintln("min_code: $min_code max_code: $max_code")
	mut widths := []int{len: max_code - min_code + 1, init: 0}

	for i in min_code .. max_code {
		pos := i - min_code
		glyph_index := tf.map_code(i)

		if glyph_index == 0 || i == 32 {
			widths[pos] = space_cw
			continue
		}

		x_min, x_max, _, _ := tf.read_glyph_dim(glyph_index)
		aw, lsb := tf.get_horizontal_metrics(u16(glyph_index))
		w := x_max - x_min
		rsb := aw - (lsb + w)

		// pp1 := x_min - lsb
		// pp2 := pp1 + aw

		w1 := w + lsb + rsb

		widths[pos] = int(w1 / tf.width_scale)
		// if i >= int(`A`) && i <= int(`Z`) {
		//	dprintln("${i:c}|$glyph_index [$pos] =>  width:${x_max-x_min} aw:${aw}|w1:${w1} lsb:${lsb} rsb:${rsb} pp1:${pp1} pp2:${pp2}")
		//}
	}

	// dprintln("Widths: ${widths.len}")
	return widths, min_code, max_code
}

pub fn (mut tf TTF_File) read_glyph(index u16) Glyph {
	index_int := int(index) // index.str()
	if index_int in tf.glyph_cache {
		// dprintln("Found glyp: ${index}")
		return tf.glyph_cache[index_int]
	}
	// dprintln("Create glyp: ${index}")

	offset := tf.get_glyph_offset(index)
	// dprintln("offset: $offset")
	if offset == 0 || offset >= tf.tables['glyf'].offset + tf.tables['glyf'].length {
		dprintln('No glyph found!')
		return Glyph{}
	}

	assert offset >= tf.tables['glyf'].offset
	assert offset < tf.tables['glyf'].offset + tf.tables['glyf'].length

	tf.pos = offset
	// dprintln("file seek read_glyph: $tf.pos")

	/*
	---- BUG TO SOLVE -----
	--- Order of the data if printed in the main is shuffled!! Very Strange
	mut tmp_glyph := Glyph{
		number_of_contours : tf.get_i16()
		x_min : tf.get_fword()
		y_min : tf.get_fword()
		x_max : tf.get_fword()
		y_max : tf.get_fword()
	}
	*/

	mut tmp_glyph := Glyph{}
	tmp_glyph.number_of_contours = tf.get_i16()
	tmp_glyph.x_min = tf.get_fword()
	tmp_glyph.y_min = tf.get_fword()
	tmp_glyph.x_max = tf.get_fword()
	tmp_glyph.y_max = tf.get_fword()

	// dprintln("file seek after read_glyph: $tf.pos")

	assert tmp_glyph.number_of_contours >= -1

	if tmp_glyph.number_of_contours == -1 {
		// dprintln("read_compound_glyph")
		tf.read_compound_glyph(mut tmp_glyph)
	} else {
		// dprintln("read_simple_glyph")
		tf.read_simple_glyph(mut tmp_glyph)
	}

	tf.glyph_cache[index_int] = tmp_glyph
	return tmp_glyph
}

const (
	tfk_on_curve  = 1
	tfk_x_is_byte = 2
	tfk_y_is_byte = 4
	tfk_repeat    = 8
	tfk_x_delta   = 16
	tfk_y_delta   = 32
)

fn (mut tf TTF_File) read_simple_glyph(mut in_glyph Glyph) {
	if in_glyph.number_of_contours == 0 {
		return
	}

	for _ in 0 .. in_glyph.number_of_contours {
		in_glyph.contour_ends << tf.get_u16()
	}

	// skip over intructions
	tf.pos = tf.get_u16() + tf.pos

	mut num_points := 0
	for ce in in_glyph.contour_ends {
		if ce > num_points {
			num_points = ce
		}
	}
	num_points++

	mut i := 0
	mut flags := []u8{}
	for i < num_points {
		flag := tf.get_u8()
		flags << flag
		in_glyph.points << Point{
			x: 0
			y: 0
			on_curve: (flag & ttf.tfk_on_curve) > 0
		}
		if (flag & ttf.tfk_repeat) > 0 {
			mut repeat_count := tf.get_u8()
			assert repeat_count > 0
			i += repeat_count
			for repeat_count > 0 {
				flags << flag
				in_glyph.points << Point{
					x: 0
					y: 0
					on_curve: (flag & ttf.tfk_on_curve) > 0
				}
				repeat_count--
			}
		}
		i++
	}

	// read coords x
	mut value := 0
	for i_x in 0 .. num_points {
		flag_x := flags[i_x]
		if (flag_x & ttf.tfk_x_is_byte) > 0 {
			if (flag_x & ttf.tfk_x_delta) > 0 {
				value += tf.get_u8()
			} else {
				value -= tf.get_u8()
			}
		} else if (~flag_x & ttf.tfk_x_delta) > 0 {
			value += tf.get_i16()
		} else {
			// value is unchanged
		}
		// dprintln("$i_x x: $value")
		in_glyph.points[i_x].x = value
	}

	// read coords y
	value = 0
	for i_y in 0 .. num_points {
		flag_y := flags[i_y]
		if (flag_y & ttf.tfk_y_is_byte) > 0 {
			if (flag_y & ttf.tfk_y_delta) > 0 {
				value += tf.get_u8()
			} else {
				value -= tf.get_u8()
			}
		} else if (~flag_y & ttf.tfk_y_delta) > 0 {
			value += tf.get_i16()
		} else {
			// value is unchanged
		}
		// dprintln("$i_y y: $value")
		in_glyph.points[i_y].y = value
	}

	// ok we have a valid glyph
	in_glyph.valid_glyph = true
}

const (
	tfkc_arg_1_and_2_are_words    = 1
	tfkc_args_are_xy_values       = 2
	tfkc_round_xy_to_grid         = 4
	tfkc_we_have_a_scale          = 8
	// reserved                   = 16
	tfkc_more_components          = 32
	tfkc_we_have_an_x_and_y_scale = 64
	tfkc_we_have_a_two_by_two     = 128
	tfkc_we_have_instructions     = 256
	tfkc_use_my_metrics           = 512
	tfkc_overlap_component        = 1024
)

struct Component {
mut:
	glyph_index      u16
	dest_point_index i16
	src_point_index  i16
	matrix           []f32 = [f32(1.0), 0, 0, 1.0, 0, 0]
}

fn (mut tf TTF_File) read_compound_glyph(mut in_glyph Glyph) {
	in_glyph.g_type = ttf.g_type_complex
	mut component := Component{}
	mut flags := ttf.tfkc_more_components
	for (flags & ttf.tfkc_more_components) > 0 {
		mut arg1 := i16(0)
		mut arg2 := i16(0)

		flags = tf.get_u16()

		component.glyph_index = tf.get_u16()

		if (flags & ttf.tfkc_arg_1_and_2_are_words) > 0 {
			arg1 = tf.get_i16()
			arg2 = tf.get_i16()
		} else {
			arg1 = tf.get_u8()
			arg2 = tf.get_u8()
		}

		if (flags & ttf.tfkc_args_are_xy_values) > 0 {
			component.matrix[4] = arg1
			component.matrix[5] = arg2
		} else {
			component.dest_point_index = arg1
			component.src_point_index = arg2
		}

		if (flags & ttf.tfkc_we_have_a_scale) > 0 {
			component.matrix[0] = tf.get_2dot14()
			component.matrix[3] = component.matrix[0]
		} else if (flags & ttf.tfkc_we_have_an_x_and_y_scale) > 0 {
			component.matrix[0] = tf.get_2dot14()
			component.matrix[3] = tf.get_2dot14()
		} else if (flags & ttf.tfkc_we_have_a_two_by_two) > 0 {
			component.matrix[0] = tf.get_2dot14()
			component.matrix[1] = tf.get_2dot14()
			component.matrix[2] = tf.get_2dot14()
			component.matrix[3] = tf.get_2dot14()
		}
		// dprintln("Read component glyph index ${component.glyph_index}")
		// dprintln("Transform: ${component.matrix}")

		old_pos := tf.pos

		simple_glyph := tf.read_glyph(component.glyph_index)
		if simple_glyph.valid_glyph {
			point_offset := in_glyph.points.len
			for i in 0 .. simple_glyph.contour_ends.len {
				in_glyph.contour_ends << u16(simple_glyph.contour_ends[i] + point_offset)
			}

			for p in simple_glyph.points {
				mut x := f32(p.x)
				mut y := f32(p.y)
				x = component.matrix[0] * x + component.matrix[1] * y + component.matrix[4]
				y = component.matrix[2] * x + component.matrix[3] * y + component.matrix[5]
				in_glyph.points << Point{
					x: int(x)
					y: int(y)
					on_curve: p.on_curve
				}
			}
		}
		tf.pos = old_pos
	}

	in_glyph.number_of_contours = i16(in_glyph.contour_ends.len)

	if (flags & ttf.tfkc_we_have_instructions) > 0 {
		tf.pos = tf.get_u16() + tf.pos
	}
	// ok we have a valid glyph
	in_glyph.valid_glyph = true
}

/******************************************************************************
*
* TTF_File get functions
*
******************************************************************************/
fn (mut tf TTF_File) get_u8() u8 {
	x := tf.buf[tf.pos]
	tf.pos++
	return u8(x)
}

fn (mut tf TTF_File) get_i8() i8 {
	return i8(tf.get_u8())
}

fn (mut tf TTF_File) get_u16() u16 {
	x := u16(tf.buf[tf.pos]) << 8 | u16(tf.buf[tf.pos + 1])
	tf.pos += 2
	return x
}

fn (mut tf TTF_File) get_ufword() u16 {
	return tf.get_u16()
}

fn (mut tf TTF_File) get_i16() i16 {
	// return i16(tf.get_u16())
	mut res := u32(tf.get_u16())
	if (res & 0x8000) > 0 {
		res -= (u32(1) << 16)
	}
	return i16(res)
}

fn (mut tf TTF_File) get_fword() i16 {
	return tf.get_i16()
}

fn (mut tf TTF_File) get_u32() u32 {
	x := (u32(tf.buf[tf.pos]) << u32(24)) | (u32(tf.buf[tf.pos + 1]) << u32(16)) | (u32(tf.buf[
		tf.pos + 2]) << u32(8)) | u32(tf.buf[tf.pos + 3])
	tf.pos += 4
	return x
}

fn (mut tf TTF_File) get_i32() int {
	mut res := u64(tf.get_u32())
	if (res & 0x8000_0000) > 0 {
		res -= (u64(1) << 32)
	}
	return int(res)
}

fn (mut tf TTF_File) get_2dot14() f32 {
	return f32(tf.get_i16()) / f32(i16(1 << 14))
}

fn (mut tf TTF_File) get_fixed() f32 {
	return f32(tf.get_i32() / f32(1 << 16))
}

fn (mut tf TTF_File) get_string(length int) string {
	tmp_pos := u64(tf.pos)
	tf.pos += u32(length)
	return unsafe { tos(&u8(u64(tf.buf.data) + tmp_pos), length) }
}

fn (mut tf TTF_File) get_unicode_string(length int) string {
	mut tmp_txt := strings.new_builder(length)
	mut real_len := 0

	for _ in 0 .. (length >> 1) {
		c := tf.get_u16()
		c_len := ((0xe5000000 >> ((c >> 3) & 0x1e)) & 3) + 1
		real_len += c_len
		if c_len == 1 {
			tmp_txt.write_u8(u8(c & 0xff))
		} else {
			tmp_txt.write_u8(u8((c >> 8) & 0xff))
			tmp_txt.write_u8(u8(c & 0xff))
		}
		// dprintln("c: ${c:c}|${ u8(c &0xff) :c} c_len: ${c_len} str_len: ${real_len} in_len: ${length}")
	}
	tf.pos += u32(real_len)
	res_txt := tmp_txt.str()
	// dprintln("get_unicode_string: ${res_txt}")
	return res_txt
}

fn (mut tf TTF_File) get_date() u64 {
	// get mac time and covert it to unix timestamp
	mac_time := (u64(tf.get_u32()) << 32) + u64(tf.get_u32())
	utc_time := mac_time - u64(2082844800)
	return utc_time
}

fn (mut tf TTF_File) calc_checksum(offset u32, length u32) u32 {
	old_index := tf.pos
	mut sum := u64(0)
	mut nlongs := int((length + 3) >> 2)
	tf.pos = offset
	// dprintln("offs: $offset nlongs: $nlongs")
	for nlongs > 0 {
		sum = sum + u64(tf.get_u32())
		nlongs--
	}
	tf.pos = old_index
	return u32(sum & u64(0xffff_ffff))
}

/******************************************************************************
*
* Offset_Table
*
******************************************************************************/
struct Offset_Table {
mut:
	checksum u32
	offset   u32
	length   u32
}

fn (mut tf TTF_File) read_offset_tables() {
	dprintln('*** READ TABLES OFFSET ***')
	tf.pos = 0
	tf.scalar_type = tf.get_u32()
	num_tables := tf.get_u16()
	tf.search_range = tf.get_u16()
	tf.entry_selector = tf.get_u16()
	tf.range_shift = tf.get_u16()

	dprintln('scalar_type   : [0x$tf.scalar_type.hex()]')
	dprintln('num tables    : [$num_tables]')
	dprintln('search_range  : [0x$tf.search_range.hex()]')
	dprintln('entry_selector: [0x$tf.entry_selector.hex()]')
	dprintln('range_shift   : [0x$tf.range_shift.hex()]')

	mut i := 0
	for i < num_tables {
		tag := tf.get_string(4)
		tf.tables[tag] = Offset_Table{
			checksum: tf.get_u32()
			offset: tf.get_u32()
			length: tf.get_u32()
		}
		dprintln('Table: [$tag]')
		// dprintln("${tf.tables[tag]}")

		if tag != 'head' {
			assert tf.calc_checksum(tf.tables[tag].offset, tf.tables[tag].length) == tf.tables[tag].checksum
		}
		i++
	}
	dprintln('*** END READ TABLES OFFSET ***')
}

/******************************************************************************
*
* Head_Table
*
******************************************************************************/
fn (mut tf TTF_File) read_head_table() {
	dprintln('*** READ HEAD TABLE ***')
	tf.pos = tf.tables['head'].offset
	dprintln('Offset: $tf.pos')

	tf.version = tf.get_fixed()
	tf.font_revision = tf.get_fixed()
	tf.checksum_adjustment = tf.get_u32()
	tf.magic_number = tf.get_u32()
	assert tf.magic_number == 0x5f0f3cf5
	tf.flags = tf.get_u16()
	tf.units_per_em = tf.get_u16()
	tf.created = tf.get_date()
	tf.modified = tf.get_date()
	tf.x_min = tf.get_i16()
	tf.y_min = tf.get_i16()
	tf.x_max = tf.get_i16()
	tf.y_max = tf.get_i16()
	tf.mac_style = tf.get_u16()
	tf.lowest_rec_ppem = tf.get_u16()
	tf.font_direction_hint = tf.get_i16()
	tf.index_to_loc_format = tf.get_i16()
	tf.glyph_data_format = tf.get_i16()
}

/******************************************************************************
*
* Name_Table
*
******************************************************************************/
fn (mut tf TTF_File) read_name_table() {
	dprintln('*** READ NAME TABLE ***')
	assert 'name' in tf.tables
	table_offset := tf.tables['name'].offset
	tf.pos = tf.tables['name'].offset

	format := tf.get_u16() // must be 0
	assert format == 0
	count := tf.get_u16()
	string_offset := tf.get_u16()

	for _ in 0 .. count {
		platform_id := tf.get_u16()
		// platform_specific_id :=
		tf.get_u16()
		// language_id          :=
		tf.get_u16()
		name_id := tf.get_u16()
		length := tf.get_u16()
		offset := tf.get_u16()

		old_pos := tf.pos
		tf.pos = table_offset + string_offset + offset

		mut name := ''
		if platform_id == 0 || platform_id == 3 {
			name = tf.get_unicode_string(length)
		} else {
			name = tf.get_string(length)
		}
		// dprintln("Name [${platform_id} / ${platform_specific_id}] id:[$name_id] language:[$language_id] [$name]")
		tf.pos = old_pos

		match name_id {
			1 { tf.font_family = name }
			2 { tf.font_sub_family = name }
			4 { tf.full_name = name }
			6 { tf.postscript_name = name }
			else {}
		}
	}
}

/******************************************************************************
*
* Cmap_Table
*
******************************************************************************/
fn (mut tf TTF_File) read_cmap_table() {
	dprintln('*** READ CMAP TABLE ***')
	assert 'cmap' in tf.tables
	table_offset := tf.tables['cmap'].offset
	tf.pos = table_offset

	version := tf.get_u16() // must be 0
	assert version == 0
	number_sub_tables := tf.get_u16()

	// tables must be sorted by platform id and then platform specific
	// encoding.
	for _ in 0 .. number_sub_tables {
		// platforms are:
		// 0 - Unicode -- use specific id 6 for full coverage. 0/4 common.
		// 1 - Macintosh (Discouraged)
		// 2 - reserved
		// 3 - Microsoft
		platform_id := tf.get_u16()
		platform_specific_id := tf.get_u16()
		offset := tf.get_u32()
		dprintln('CMap platform_id=$platform_id specific_id=$platform_specific_id offset=$offset')
		if platform_id == 3 && platform_specific_id <= 1 {
			tf.read_cmap(table_offset + offset)
		}
	}
}

fn (mut tf TTF_File) read_cmap(offset u32) {
	old_pos := tf.pos
	tf.pos = offset
	format := tf.get_u16()
	length := tf.get_u16()
	language := tf.get_u16()

	dprintln('  Cmap format: $format length: $length language: $language')
	if format == 0 {
		dprintln('  Cmap 0 Init...')
		mut cmap := TrueTypeCmap{}
		cmap.init_0(mut tf)
		tf.cmaps << cmap
	} else if format == 4 {
		dprintln('  Cmap 4 Init...')
		mut cmap := TrueTypeCmap{}
		cmap.init_4(mut tf)
		tf.cmaps << cmap
	}

	tf.pos = old_pos
}

/******************************************************************************
*
* CMAPS 0/4
*
******************************************************************************/
pub fn (mut tf TTF_File) map_code(char_code int) u16 {
	mut index := 0
	for i in 0 .. tf.cmaps.len {
		mut cmap := tf.cmaps[i]
		if cmap.format == 0 {
			// dprintln("format 0")
			index = cmap.map_0(char_code)
		} else if cmap.format == 4 {
			// dprintln("format 4")
			index = cmap.map_4(char_code, mut tf)
		}
	}
	return u16(index)
}

fn (mut tm TrueTypeCmap) init_0(mut tf TTF_File) {
	tm.format = 0
	for i in 0 .. 256 {
		glyph_index := tf.get_u8()
		dprintln('   Glyph[$i] = %glyph_index')
		tm.arr << glyph_index
	}
}

fn (mut tm TrueTypeCmap) map_0(char_code int) int {
	if char_code >= 0 && char_code <= 255 {
		// dprintln("charCode $char_code maps to ${tm.arr[char_code]}")
		return tm.arr[char_code]
	}
	return 0
}

fn (mut tm TrueTypeCmap) init_4(mut tf TTF_File) {
	tm.format = 4

	// 2x segcount
	seg_count := tf.get_u16() >> 1
	// search_range   :=
	tf.get_u16()
	// entry_selector :=
	tf.get_u16()
	// range_shift    :=
	tf.get_u16()

	// Ending character code for each segment, last is 0xffff
	for _ in 0 .. seg_count {
		tm.segments << Segment{0, 0, tf.get_u16(), 0}
	}

	// reservePAD
	tf.get_u16()

	// starting character code for each segment
	for i in 0 .. seg_count {
		tm.segments[i].start_code = tf.get_u16()
	}

	// Delta for all character codes in segment
	for i in 0 .. seg_count {
		tm.segments[i].id_delta = tf.get_u16()
	}

	// offset in bytes to glyph indexArray, or 0
	for i in 0 .. seg_count {
		ro := u32(tf.get_u16())
		if ro != 0 {
			tm.segments[i].id_range_offset = tf.pos - 2 + ro
		} else {
			tm.segments[i].id_range_offset = 0
		}
	}
	/*
	// DEBUG LOG
	for i in 0..seg_count {
	seg := tm.segments[i]
	dprintln("    segments[$i] = $seg.start_code $seg.end_code $seg.id_delta $seg.id_range_offset")
	}
	*/
}

fn (mut tm TrueTypeCmap) map_4(char_code int, mut tf TTF_File) int {
	// dprintln("HERE map_4 for char [$char_code]")
	old_pos := tf.pos
	if tm.cache[char_code] == -1 {
		// dprintln("Not found, search for it!")
		mut found := false
		for segment in tm.segments {
			if segment.start_code <= char_code && segment.end_code >= char_code {
				mut index := (segment.id_delta + char_code) & 0xffff
				if segment.id_range_offset > 0 {
					glyph_index_address := segment.id_range_offset +
						2 * u32(char_code - segment.start_code)
					tf.pos = glyph_index_address
					index = tf.get_u16()
				}

				tm.cache[char_code] = index
				found = true
				break
			}
		}
		if !found {
			tm.cache[char_code] = 0
		}
	}
	tf.pos = old_pos
	return tm.cache[char_code]
}

/******************************************************************************
*
* Hhea table
*
******************************************************************************/
fn (mut tf TTF_File) read_hhea_table() {
	dprintln('*** READ HHEA TABLE ***')
	assert 'hhea' in tf.tables
	table_offset := tf.tables['hhea'].offset
	tf.pos = table_offset

	// version :=
	tf.get_fixed() // 0x00010000

	tf.ascent = tf.get_fword()
	tf.descent = tf.get_fword()
	tf.line_gap = tf.get_fword()
	tf.advance_width_max = tf.get_ufword()
	tf.min_left_side_bearing = tf.get_fword()
	tf.min_right_side_bearing = tf.get_fword()
	tf.x_max_extent = tf.get_fword()
	tf.caret_slope_rise = tf.get_i16()
	tf.caret_slope_run = tf.get_i16()
	tf.caret_offset = tf.get_fword()
	tf.get_i16() // reserved
	tf.get_i16() // reserved
	tf.get_i16() // reserved
	tf.get_i16() // reserved
	tf.metric_data_format = tf.get_i16()
	tf.num_of_long_hor_metrics = tf.get_u16()
}

/******************************************************************************
*
* Kern table
*
******************************************************************************/
struct Kern0Table {
mut:
	swap      bool
	offset    u32
	n_pairs   int
	kmap      map[u32]i16
	old_index int = -1
}

fn (mut kt Kern0Table) reset() {
	kt.old_index = -1
}

fn (mut kt Kern0Table) get(glyph_index int) (int, int) {
	mut x := 0

	if kt.old_index >= 0 {
		ch := ((u32(kt.old_index & 0xFFFF) << 16) | u32(glyph_index & 0xFFFF))
		// dprintln("kern_get: $ch")
		if ch in kt.kmap {
			x = int(kt.kmap[ch])
		}
	}
	kt.old_index = glyph_index
	if kt.swap {
		return 0, x
	}
	return x, 0
}

fn (mut tf TTF_File) create_kern_table0(vertical bool, cross bool) Kern0Table {
	offset := tf.pos
	n_pairs := tf.get_u16()
	search_range := tf.get_u16()
	entry_selector := tf.get_u16()
	range_shift := tf.get_u16()
	dprintln('n_pairs: $n_pairs search_range: $search_range entry_selector: $entry_selector range_shift: $range_shift')

	mut kt0 := Kern0Table{
		swap: (vertical && !cross) || (!vertical && cross)
		offset: offset
		n_pairs: n_pairs
	}

	for _ in 0 .. n_pairs {
		left := tf.get_u16()
		right := tf.get_u16()
		value := tf.get_fword()
		tmp_index := (u32(left) << 16) | u32(right)
		kt0.kmap[tmp_index] = value
		// dprintln("index: ${tmp_index.hex()} val: ${value.hex()}")
	}
	kt0.old_index = -1
	return kt0
}

fn (mut tf TTF_File) read_kern_table() {
	dprintln('*** READ KERN TABLE ***')
	if 'kern' !in tf.tables {
		return
	}
	table_offset := tf.tables['kern'].offset
	tf.pos = table_offset

	version := tf.get_u16() // must be 0
	assert version == 0 // must be 0
	n_tables := tf.get_u16()

	dprintln('Kern Table version: $version Kern nTables: $n_tables')

	for _ in 0 .. n_tables {
		st_version := tf.get_u16() // sub table version
		length := tf.get_u16()
		coverage := tf.get_u16()
		format := coverage >> 8
		cross := coverage & 4
		vertical := (coverage & 0x1) == 0
		dprintln('Kerning subtable version [$st_version] format [$format] length [$length] coverage: [$coverage.hex()]')
		if format == 0 {
			dprintln('kern format: 0')
			kern := tf.create_kern_table0(vertical, cross != 0)
			tf.kern << kern
		} else {
			dprintln('Unknown format -- skip')
			tf.pos = tf.pos + length
		}
	}
}

pub fn (mut tf TTF_File) reset_kern() {
	for i in 0 .. tf.kern.len {
		tf.kern[i].reset()
	}
}

pub fn (mut tf TTF_File) next_kern(glyph_index int) (int, int) {
	mut x := 0
	mut y := 0
	for i in 0 .. tf.kern.len {
		tmp_x, tmp_y := tf.kern[i].get(glyph_index)
		x = x + tmp_x
		y = y + tmp_y
	}
	return x, y
}

/******************************************************************************
*
* TTF_File Utility
*
******************************************************************************/
pub fn (tf TTF_File) get_info_string() string {
	txt := '----- Font Info -----
font_family     : $tf.font_family
font_sub_family : $tf.font_sub_family
full_name       : $tf.full_name
postscript_name : $tf.postscript_name
version         : $tf.version
font_revision   : $tf.font_revision
magic_number    : $tf.magic_number.hex()
flags           : $tf.flags.hex()
created  unixTS : $tf.created
modified unixTS : $tf.modified
box             : [x_min:$tf.x_min, y_min:$tf.y_min, x_Max:$tf.x_max, y_Max:$tf.y_max]
mac_style       : $tf.mac_style
-----------------------
'
	return txt
}

/******************************************************************************
*
* TTF_File test
*
******************************************************************************/
fn tst() {
	mut tf := TTF_File{}

	tf.buf = [
		u8(0xFF), /* 8  bit */
		0xF1,
		0xF2, /* 16 bit */
		0x81,
		0x23,
		0x45,
		0x67, /* 32 bit */
		0x12,
		0x34,
		0x12,
		0x34, /* get_2dot14 16 bit */
		0x12,
		0x34,
		0x12,
		0x34 /* get_fixed 32 bit int */,
	]
	assert tf.get_u8().hex() == 'ff'
	assert tf.get_u16().hex() == 'f1f2'
	assert tf.get_u32().hex() == '81234567'

	dprintln('buf len: $tf.buf.len')
	// dprintln( tf.get_u8().hex() )
	// dprintln( tf.get_u16().hex() )
	// dprintln( tf.get_u32().hex() )
	// dprintln( tf.get_2dot14() )
	// dprintln( tf.get_fixed() )
}
