module ttf

/**********************************************************************
*
* BMP render module utility functions
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* Note:
*
* TODO:
* - manage text directions R to L
**********************************************************************/
import encoding.utf8
import math

pub struct BitMap {
pub mut:
	tf       &TTF_File
	buf      &u8 = unsafe { 0 } // pointer to the memory buffer
	buf_size int // allocated buf size in bytes
	width    int = 1 // width of the buffer
	height   int = 1 // height of the buffer
	bp       int = 4 // byte per pixel of the buffer
	bg_color u32 = 0xFFFFFF_00 // background RGBA format
	color    u32 = 0x000000_FF // RGBA format
	scale    f32 = 1.0 // internal usage!!
	scale_x  f32 = 1.0 // X scale of the single glyph
	scale_y  f32 = 1.0 // Y scale of the single glyph
	angle    f32 = 0.0 // angle of rotation of the bitmap
	// spaces
	space_cw   f32 = 1.0 // width of the space glyph internal usage!!
	space_mult f32 = f32(0.0) // 1.0/16.0  // space between letter, is a multiplier for a standrd space ax
	// used only by internal text rendering!!
	tr_matrix          []f32      = [f32(1), 0, 0, 0, 1, 0, 0, 0, 0] // transformation matrix
	ch_matrix          []f32      = [f32(1), 0, 0, 0, 1, 0, 0, 0, 0] // character matrix
	style              Style      = .filled // default syle
	align              Text_align = .left // default text align
	justify            bool // justify text flag, default deactivated
	justify_fill_ratio f32 = 0.5 // justify fill ratio, if the ratio of the filled row is >= of this then justify the text
	filler             [][]int    // filler buffer for the renderer
	// flag to force font embedded metrics
	use_font_metrics bool
}

/******************************************************************************
*
* Utility
*
******************************************************************************/
// clear clear the bitmap with 0 bytes
pub fn (mut bmp BitMap) clear() {
	mut sz := bmp.width * bmp.height * bmp.bp
	unsafe {
		C.memset(bmp.buf, 0x00, sz)
	}
}

// transform matrix applied to the text
pub fn (bmp &BitMap) trf_txt(p &Point) (int, int) {
	return int(p.x * bmp.tr_matrix[0] + p.y * bmp.tr_matrix[3] + bmp.tr_matrix[6]), int(
		p.x * bmp.tr_matrix[1] + p.y * bmp.tr_matrix[4] + bmp.tr_matrix[7])
}

// transform matrix applied to the char
pub fn (bmp &BitMap) trf_ch(p &Point) (int, int) {
	return int(p.x * bmp.ch_matrix[0] + p.y * bmp.ch_matrix[3] + bmp.ch_matrix[6]), int(
		p.x * bmp.ch_matrix[1] + p.y * bmp.ch_matrix[4] + bmp.ch_matrix[7])
}

// set draw postion in the buffer
pub fn (mut bmp BitMap) set_pos(x f32, y f32) {
	bmp.tr_matrix[6] = x
	bmp.tr_matrix[7] = y
}

// set the rotation angle in radiants
pub fn (mut bmp BitMap) set_rotation(a f32) {
	bmp.tr_matrix[0] = f32(math.cos(a)) // 1
	bmp.tr_matrix[1] = f32(-math.sin(a)) // 0
	bmp.tr_matrix[3] = f32(math.sin(a)) // 0
	bmp.tr_matrix[4] = f32(math.cos(a)) // 1
}

/******************************************************************************
*
* Filler functions
*
******************************************************************************/
pub fn (mut bmp BitMap) init_filler() {
	h := bmp.height - bmp.filler.len
	if h < 1 {
		return
	}
	for _ in 0 .. h {
		bmp.filler << []int{len: 4}
	}
	// dprintln("Init filler: ${bmp.filler.len} rows")
}

pub fn (mut bmp BitMap) clear_filler() {
	for i in 0 .. bmp.height {
		bmp.filler[i].clear()
	}
}

pub fn (mut bmp BitMap) exec_filler() {
	for y in 0 .. bmp.height {
		if bmp.filler[y].len > 0 {
			bmp.filler[y].sort()
			if bmp.filler[y].len & 1 != 0 {
				// dprintln("even line!! $y => ${bmp.filler[y]}")
				continue
			}
			mut index := 0
			for index < bmp.filler[y].len {
				startx := bmp.filler[y][index] + 1
				endx := bmp.filler[y][index + 1]
				if startx >= endx {
					index += 2
					continue
				}
				for x in startx .. endx {
					bmp.plot(x, y, bmp.color)
				}
				index += 2
			}
		}
	}
}

pub fn (mut bmp BitMap) fline(in_x0 int, in_y0 int, in_x1 int, in_y1 int, c u32) {
	mut x0 := f32(in_x0)
	mut x1 := f32(in_x1)
	mut y0 := f32(in_y0)
	mut y1 := f32(in_y1)
	mut tmp := f32(0)

	// check bounds
	if (in_x0 < 0 && in_x1 < 0) || (in_x0 > bmp.width && in_x1 > bmp.width) {
		return
	}

	if y1 < y0 {
		tmp = x0
		x0 = x1
		x1 = tmp

		tmp = y0
		y0 = y1
		y1 = tmp
	}

	mut dx := x1 - x0
	mut dy := y1 - y0

	if dy == 0 {
		if in_y0 >= 0 && in_y0 < bmp.filler.len {
			if in_x0 <= in_x1 {
				bmp.filler[in_y0] << in_x0
				bmp.filler[in_y0] << in_x1
			} else {
				bmp.filler[in_y0] << in_x1
				bmp.filler[in_y0] << in_x0
			}
		}
		return
	}
	mut n := dx / dy
	for y in 0 .. int(dy + 0.5) {
		yd := int(y + y0)
		x := n * y + x0
		if x > bmp.width || yd >= bmp.filler.len {
			break
		}
		if yd >= 0 && yd < bmp.filler.len {
			bmp.filler[yd] << int(x + 0.5)
			// bmp.plot(int(x+0.5), yd, bmp.color)
		}
	}
}

/******************************************************************************
*
* Draw functions
*
******************************************************************************/
[inline]
pub fn (mut bmp BitMap) plot(x int, y int, c u32) bool {
	if x < 0 || x >= bmp.width || y < 0 || y >= bmp.height {
		return false
	}
	mut index := (x + y * bmp.width) * bmp.bp
	unsafe {
		// bmp.buf[index]=0xFF
		bmp.buf[index] = u8(c & 0xFF) // write only the alpha
	}
	/*
	for count in 0..(bmp.bp) {
		unsafe{
			bmp.buf[index + count] = u8((c >> (bmp.bp - count - 1) * 8) & 0x0000_00FF)
		}
	}
	*/
	return true
}

/******************************************************************************
*
* smooth draw functions
*
******************************************************************************/
// aline draw an aliased line on the bitmap
pub fn (mut bmp BitMap) aline(in_x0 int, in_y0 int, in_x1 int, in_y1 int, c u32) {
	// mut c1 := c
	mut x0 := f32(in_x0)
	mut x1 := f32(in_x1)
	mut y0 := f32(in_y0)
	mut y1 := f32(in_y1)
	mut tmp := f32(0)

	mut dx := x1 - x0
	mut dy := y1 - y0

	dist := f32(0.4)

	if math.abs(dx) > math.abs(dy) {
		if x1 < x0 {
			tmp = x0
			x0 = x1
			x1 = tmp

			tmp = y0
			y0 = y1
			y1 = tmp
		}
		dx = x1 - x0
		dy = y1 - y0

		x0 += 0.5
		y0 += 0.5

		m := dy / dx
		mut x := x0
		for x <= x1 + 0.5 {
			y := m * (x - x0) + y0
			e := 1 - math.abs(y - 0.5 - int(y))
			bmp.plot(int(x), int(y), color_multiply_alpha(c, e * 0.75))

			ys1 := y + dist
			if int(ys1) != int(y) {
				v1 := math.abs(ys1 - y) / dist * (1 - e)
				bmp.plot(int(x), int(ys1), color_multiply_alpha(c, v1))
			}

			ys2 := y - dist
			if int(ys2) != int(y) {
				v2 := math.abs(y - ys2) / dist * (1 - e)
				bmp.plot(int(x), int(ys2), color_multiply_alpha(c, v2))
			}

			x += 1.0
		}
	} else {
		if y1 < y0 {
			tmp = x0
			x0 = x1
			x1 = tmp

			tmp = y0
			y0 = y1
			y1 = tmp
		}
		dx = x1 - x0
		dy = y1 - y0

		x0 += 0.5
		y0 += 0.5

		n := dx / dy
		mut y := y0
		for y <= y1 + 0.5 {
			x := n * (y - y0) + x0
			e := f32(1 - math.abs(x - 0.5 - int(x)))
			bmp.plot(int(x), int(y), color_multiply_alpha(c, f32(e * 0.75)))

			xs1 := x + dist
			if int(xs1) != int(x) {
				v1 := math.abs(xs1 - x) / dist * (1 - e)
				bmp.plot(int(xs1), int(y), color_multiply_alpha(c, f32(v1)))
			}

			xs2 := x - dist
			if int(xs2) != int(x) {
				v2 := math.abs(x - xs1) / dist * (1 - e)
				bmp.plot(int(xs2), int(y), color_multiply_alpha(c, f32(v2)))
			}
			y += 1.0
		}
	}
}

/******************************************************************************
*
* draw functions
*
******************************************************************************/
pub fn (mut bmp BitMap) line(in_x0 int, in_y0 int, in_x1 int, in_y1 int, c u32) {
	// outline with aliased borders
	if bmp.style == .outline_aliased {
		bmp.aline(in_x0, in_y0, in_x1, in_y1, c)
		return
	}
	// filled with aliased borders
	else if bmp.style == .filled {
		bmp.aline(in_x0, in_y0, in_x1, in_y1, c)
		bmp.fline(in_x0, in_y0, in_x1, in_y1, c)
		return
	}
	// only the filler is drawn
	else if bmp.style == .raw {
		bmp.fline(in_x0, in_y0, in_x1, in_y1, c)
		return
	}
	// if we are here we are drawing an outlined border

	x0 := int(in_x0)
	x1 := int(in_x1)
	y0 := int(in_y0)
	y1 := int(in_y1)
	// dprintln("line[$x0,$y0,$x1,$y1]")

	mut x := x0
	mut y := y0

	dx := math.abs(x1 - x0)
	sx := if x0 < x1 { 1 } else { -1 }
	dy := -math.abs(y1 - y0)
	sy := if y0 < y1 { 1 } else { -1 }

	// verical line
	if dx == 0 {
		if y0 < y1 {
			for yt in y0 .. y1 + 1 {
				bmp.plot(x0, yt, c)
			}
			return
		}
		for yt in y1 .. y0 + 1 {
			bmp.plot(x0, yt, c)
		}
		// horizontal line
		return
	} else if dy == 0 {
		if x0 < x1 {
			for xt in x0 .. x1 + 1 {
				bmp.plot(xt, y0, c)
			}
			return
		}
		for xt in x1 .. x0 + 1 {
			bmp.plot(xt, y0, c)
		}
		return
	}

	mut err := dx + dy // error value e_xy
	for {
		// bmp.plot(x, y, u32(0xFF00))
		bmp.plot(x, y, c)

		// dprintln("$x $y [$x0,$y0,$x1,$y1]")
		if x == x1 && y == y1 {
			break
		}
		e2 := 2 * err
		if e2 >= dy { // e_xy+e_x > 0
			err += dy
			x += sx
		}
		if e2 <= dx { // e_xy+e_y < 0
			err += dx
			y += sy
		}
	}
}

pub fn (mut bmp BitMap) box(in_x0 int, in_y0 int, in_x1 int, in_y1 int, c u32) {
	bmp.line(in_x0, in_y0, in_x1, in_y0, c)
	bmp.line(in_x1, in_y0, in_x1, in_y1, c)
	bmp.line(in_x0, in_y1, in_x1, in_y1, c)
	bmp.line(in_x0, in_y0, in_x0, in_y1, c)
}

pub fn (mut bmp BitMap) quadratic(in_x0 int, in_y0 int, in_x1 int, in_y1 int, in_cx int, in_cy int, c u32) {
	/*
	x0 := int(in_x0 * bmp.scale)
	x1 := int(in_x1 * bmp.scale)
	y0 := int(in_y0 * bmp.scale)
	y1 := int(in_y1 * bmp.scale)
	cx := int(in_cx * bmp.scale)
	cy := int(in_cy * bmp.scale)
	*/
	x0 := int(in_x0)
	x1 := int(in_x1)
	y0 := int(in_y0)
	y1 := int(in_y1)
	cx := int(in_cx)
	cy := int(in_cy)

	mut division := f64(1.0)
	dx := math.abs(x0 - x1)
	dy := math.abs(y0 - y1)

	// if few pixel draw a simple line
	// if dx == 0 && dy == 0 {
	if dx <= 2 || dy <= 2 {
		// bmp.plot(x0, y0, c)
		bmp.line(x0, y0, x1, y1, c)
		return
	}

	division = 1.0 / (f64(if dx > dy { dx } else { dy }))

	// division = 0.1   // 10 division
	// division = 0.25  // 4 division

	// dprintln("div: $division")

	/*
	----- Bezier quadratic form -----
	t = 0.5; // given example value, half length of the curve
	x = (1 - t) * (1 - t) * p[0].x + 2 * (1 - t) * t * p[1].x + t * t * p[2].x;
	y = (1 - t) * (1 - t) * p[0].y + 2 * (1 - t) * t * p[1].y + t * t * p[2].y;
	---------------------------------
	*/

	mut x_old := x0
	mut y_old := y0
	mut t := 0.0

	for t <= (1.0 + division / 2.0) {
		s := 1.0 - t
		x := s * s * x0 + 2.0 * s * t * cx + t * t * x1
		y := s * s * y0 + 2.0 * s * t * cy + t * t * y1
		xi := int(x + 0.5)
		yi := int(y + 0.5)
		// bmp.plot(xi, yi, c)
		bmp.line(x_old, y_old, xi, yi, c)
		x_old = xi
		y_old = yi
		t += division
	}
}

/******************************************************************************
*
* TTF Query functions
*
******************************************************************************/
pub fn (mut bmp BitMap) get_chars_bbox(in_string string) []int {
	mut res := []int{}
	mut w := 0

	mut space_cw, _ := bmp.tf.get_horizontal_metrics(u16(` `))
	div_space_cw := int((f32(space_cw) * bmp.space_mult) * bmp.scale)
	space_cw = int(space_cw * bmp.scale)

	bmp.tf.reset_kern()

	mut i := 0
	for i < in_string.len {
		mut chr := u16(in_string[i])

		// draw the space
		if int(chr) == 32 {
			w += int(space_cw * bmp.space_cw)
			i++
			continue
		}
		// manage unicode chars like latin greek etc
		c_len := ((0xe5000000 >> ((chr >> 3) & 0x1e)) & 3) + 1
		if c_len > 1 {
			tmp_char := utf8.get_uchar(in_string, i)
			// dprintln("tmp_char: ${tmp_char.hex()}")
			chr = u16(tmp_char)
		}

		c_index := bmp.tf.map_code(int(chr))
		// Glyph not found
		if c_index == 0 {
			w += int(space_cw * bmp.space_cw)
			i += c_len
			continue
		}

		ax, ay := bmp.tf.next_kern(c_index)
		// dprintln("char_index: $c_index ax: $ax ay: $ay")

		// cw, lsb := bmp.tf.get_horizontal_metrics(u16(chr))
		// dprintln("metrics: [${u16(chr):c}] cw:$cw lsb:$lsb")

		//----- Calc Glyph transformations -----
		mut x0 := w + int(ax * bmp.scale)
		mut y0 := 0 + int(ay * bmp.scale)

		p := Point{x0, y0, false}
		x1, y1 := bmp.trf_txt(p)
		// init ch_matrix
		bmp.ch_matrix[0] = bmp.tr_matrix[0] * bmp.scale * bmp.scale_x
		bmp.ch_matrix[1] = bmp.tr_matrix[1] * bmp.scale * bmp.scale_x
		bmp.ch_matrix[3] = bmp.tr_matrix[3] * -bmp.scale * bmp.scale_y
		bmp.ch_matrix[4] = bmp.tr_matrix[4] * -bmp.scale * bmp.scale_y
		bmp.ch_matrix[6] = int(x1)
		bmp.ch_matrix[7] = int(y1)

		// x_min, x_max, y_min, y_max := bmp.tf.read_glyph_dim(c_index)
		x_min, x_max, _, _ := bmp.tf.read_glyph_dim(c_index)
		//-----------------

		width := int((math.abs(x_max + x_min) + ax) * bmp.scale)
		// width := int((cw+ax) * bmp.scale)
		w += width + div_space_cw
		h := int(math.abs(int(bmp.tf.y_max - bmp.tf.y_min)) * bmp.scale)
		res << w
		res << h

		i += c_len
	}
	return res
}

pub fn (mut bmp BitMap) get_bbox(in_string string) (int, int) {
	mut w := 0

	mut space_cw, _ := bmp.tf.get_horizontal_metrics(u16(` `))
	div_space_cw := int((f32(space_cw) * bmp.space_mult) * bmp.scale)
	space_cw = int(space_cw * bmp.scale)

	bmp.tf.reset_kern()

	mut i := 0
	for i < in_string.len {
		mut chr := u16(in_string[i])

		// draw the space
		if int(chr) == 32 {
			w += int(space_cw * bmp.space_cw)
			i++
			continue
		}
		// manage unicode chars like latin greek etc
		c_len := ((0xe5000000 >> ((chr >> 3) & 0x1e)) & 3) + 1
		if c_len > 1 {
			tmp_char := utf8.get_uchar(in_string, i)
			// dprintln("tmp_char: ${tmp_char.hex()}")
			chr = u16(tmp_char)
		}

		c_index := bmp.tf.map_code(int(chr))
		// Glyph not found
		if c_index == 0 {
			w += int(space_cw * bmp.space_cw)
			i += c_len
			continue
		}
		ax, ay := bmp.tf.next_kern(c_index)
		// dprintln("char_index: $c_index ax: $ax ay: $ay")

		// cw, lsb := bmp.tf.get_horizontal_metrics(u16(chr))
		// dprintln("metrics: [${u16(chr):c}] cw:$cw lsb:$lsb")

		//----- Calc Glyph transformations -----
		mut x0 := w + int(ax * bmp.scale)
		mut y0 := 0 + int(ay * bmp.scale)

		p := Point{x0, y0, false}
		x1, y1 := bmp.trf_txt(p)
		// init ch_matrix
		bmp.ch_matrix[0] = bmp.tr_matrix[0] * bmp.scale * bmp.scale_x
		bmp.ch_matrix[1] = bmp.tr_matrix[1] * bmp.scale * bmp.scale_x
		bmp.ch_matrix[3] = bmp.tr_matrix[3] * -bmp.scale * bmp.scale_y
		bmp.ch_matrix[4] = bmp.tr_matrix[4] * -bmp.scale * bmp.scale_y
		bmp.ch_matrix[6] = int(x1)
		bmp.ch_matrix[7] = int(y1)

		x_min, x_max, _, _ := bmp.tf.read_glyph_dim(c_index)
		// x_min := 1
		// x_max := 2
		//-----------------

		width := int((math.abs(x_max + x_min) + ax) * bmp.scale)
		// width := int((cw+ax) * bmp.scale)
		w += width + div_space_cw

		i += c_len
	}

	// dprintln("y_min: $bmp.tf.y_min y_max: $bmp.tf.y_max res: ${int((bmp.tf.y_max - bmp.tf.y_min)*buf.scale)} width: ${int( (cw) * buf.scale)}")
	// buf.box(0,y_base - int((bmp.tf.y_min)*buf.scale), int( (x_max) * buf.scale), y_base-int((bmp.tf.y_max)*buf.scale), u32(0xFF00_0000) )
	return w, int(math.abs(int(bmp.tf.y_max - bmp.tf.y_min)) * bmp.scale)
}

/******************************************************************************
*
* TTF draw glyph
*
******************************************************************************/
fn (mut bmp BitMap) draw_notdef_glyph(in_x int, in_w int) {
	mut p := Point{in_x, 0, false}
	x1, y1 := bmp.trf_txt(p)
	// init ch_matrix
	bmp.ch_matrix[0] = bmp.tr_matrix[0] * bmp.scale * bmp.scale_x
	bmp.ch_matrix[1] = bmp.tr_matrix[1] * bmp.scale * bmp.scale_x
	bmp.ch_matrix[3] = bmp.tr_matrix[3] * -bmp.scale * bmp.scale_y
	bmp.ch_matrix[4] = bmp.tr_matrix[4] * -bmp.scale * bmp.scale_y
	bmp.ch_matrix[6] = int(x1)
	bmp.ch_matrix[7] = int(y1)
	x, y := bmp.trf_ch(p)

	y_h := math.abs(bmp.tf.y_max - bmp.tf.y_min) * bmp.scale * 0.5

	bmp.box(int(x), int(y), int(x - in_w), int(y - y_h), bmp.color)
	bmp.line(int(x), int(y), int(x - in_w), int(y - y_h), bmp.color)
	bmp.line(int(x - in_w), int(y), int(x), int(y - y_h), bmp.color)
}

pub fn (mut bmp BitMap) draw_text(in_string string) (int, int) {
	mut w := 0

	mut space_cw, _ := bmp.tf.get_horizontal_metrics(u16(` `))
	div_space_cw := int((f32(space_cw) * bmp.space_mult) * bmp.scale)
	space_cw = int(space_cw * bmp.scale)

	bmp.tf.reset_kern()

	mut i := 0
	for i < in_string.len {
		mut chr := u16(in_string[i])

		// draw the space
		if int(chr) == 32 {
			w += int(space_cw * bmp.space_cw)
			i++
			continue
		}
		// manage unicode chars like latin greek etc
		c_len := ((0xe5000000 >> ((chr >> 3) & 0x1e)) & 3) + 1
		if c_len > 1 {
			tmp_char := utf8.get_uchar(in_string, i)
			// dprintln("tmp_char: ${tmp_char.hex()}")
			chr = u16(tmp_char)
		}

		c_index := bmp.tf.map_code(int(chr))
		// Glyph not found
		if c_index == 0 {
			bmp.draw_notdef_glyph(w, int(space_cw * bmp.space_cw))
			w += int(space_cw * bmp.space_cw)
			i += c_len
			continue
		}

		ax, ay := bmp.tf.next_kern(c_index)
		// dprintln("char_index: $c_index ax: $ax ay: $ay")

		cw, _ := bmp.tf.get_horizontal_metrics(u16(chr))
		// cw, lsb := bmp.tf.get_horizontal_metrics(u16(chr))
		// dprintln("metrics: [${u16(chr):c}] cw:$cw lsb:$lsb")

		//----- Draw_Glyph transformations -----
		mut x0 := w + int(ax * bmp.scale)
		mut y0 := 0 + int(ay * bmp.scale)

		p := Point{x0, y0, false}
		x1, y1 := bmp.trf_txt(p)
		// init ch_matrix
		bmp.ch_matrix[0] = bmp.tr_matrix[0] * bmp.scale * bmp.scale_x
		bmp.ch_matrix[1] = bmp.tr_matrix[1] * bmp.scale * bmp.scale_x
		bmp.ch_matrix[3] = bmp.tr_matrix[3] * -bmp.scale * bmp.scale_y
		bmp.ch_matrix[4] = bmp.tr_matrix[4] * -bmp.scale * bmp.scale_y
		bmp.ch_matrix[6] = int(x1)
		bmp.ch_matrix[7] = int(y1)

		x_min, x_max := bmp.draw_glyph(c_index)
		// x_min := 1
		// x_max := 2
		//-----------------

		mut width := int((math.abs(x_max + x_min) + ax) * bmp.scale)
		if bmp.use_font_metrics {
			width = int((cw + ax) * bmp.scale)
		}
		w += width + div_space_cw
		i += c_len
	}

	// dprintln("y_min: $bmp.tf.y_min y_max: $bmp.tf.y_max res: ${int((bmp.tf.y_max - bmp.tf.y_min)*buf.scale)} width: ${int( (cw) * buf.scale)}")
	// buf.box(0,y_base - int((bmp.tf.y_min)*buf.scale), int( (x_max) * buf.scale), y_base-int((bmp.tf.y_max)*buf.scale), u32(0xFF00_0000) )
	return w, int(math.abs(int(bmp.tf.y_max - bmp.tf.y_min)) * bmp.scale)
}

pub fn (mut bmp BitMap) draw_glyph(index u16) (int, int) {
	glyph := bmp.tf.read_glyph(index)

	if !glyph.valid_glyph {
		return 0, 0
	}

	if bmp.style == .filled || bmp.style == .raw {
		bmp.clear_filler()
	}

	mut s := 0 // status
	mut c := 0 // contours count
	mut contour_start := 0
	mut x0 := 0
	mut y0 := 0
	color := bmp.color // u32(0xFFFF_FF00) // RGBA white
	// color1            := u32(0xFF00_0000) // RGBA red
	// color2            := u32(0x00FF_0000) // RGBA green

	mut sp_x := 0
	mut sp_y := 0
	mut point := Point{}

	for count, point_raw in glyph.points {
		// dprintln("count: $count, state: $s pl:$glyph.points.len")
		point.x = point_raw.x
		point.y = point_raw.y

		point.x, point.y = bmp.trf_ch(point)
		point.on_curve = point_raw.on_curve

		if s == 0 {
			x0 = point.x
			y0 = point.y
			sp_x = x0
			sp_y = y0
			s = 1 // next state
			continue
		} else if s == 1 {
			if point.on_curve {
				bmp.line(x0, y0, point.x, point.y, color)
				// bmp.aline(x0, y0, point.x, point.y, u32(0xFFFF0000))
				x0 = point.x
				y0 = point.y
			} else {
				s = 2
			}
		} else {
			// dprintln("s==2")
			mut prev := glyph.points[count - 1]
			prev.x, prev.y = bmp.trf_ch(prev)
			if point.on_curve {
				// dprintln("HERE1")
				// ctx.quadraticCurveTo(prev.x + x, prev.y + y,point.x + x, point.y + y);
				// bmp.line(x0, y0, point.x + in_x, point.y + in_y, color1)
				// bmp.quadratic(x0, y0, point.x + in_x, point.y + in_y, prev.x + in_x, prev.y + in_y, u32(0xa0a00000))
				bmp.quadratic(x0, y0, point.x, point.y, prev.x, prev.y, color)
				x0 = point.x
				y0 = point.y
				s = 1
			} else {
				// dprintln("HERE2")
				// ctx.quadraticCurveTo(prev.x + x, prev.y + y,
				//            (prev.x + point.x) / 2 + x,
				//            (prev.y + point.y) / 2 + y);

				// bmp.line(x0, y0, (prev.x + point.x)/2, (prev.y + point.y)/2, color2)
				// bmp.quadratic(x0, y0, (prev.x + point.x)/2, (prev.y + point.y)/2, prev.x, prev.y, color2)
				bmp.quadratic(x0, y0, (prev.x + point.x) / 2, (prev.y + point.y) / 2,
					prev.x, prev.y, color)
				x0 = (prev.x + point.x) / 2
				y0 = (prev.y + point.y) / 2
			}
		}

		if count == glyph.contour_ends[c] {
			// dprintln("count == glyph.contour_ends[count]")
			if s == 2 { // final point was off-curve. connect to start

				mut start_point := glyph.points[contour_start]
				start_point.x, start_point.y = bmp.trf_ch(start_point)
				if point.on_curve {
					// ctx.quadraticCurveTo(prev.x + x, prev.y + y,
					// point.x + x, point.y + y);
					// bmp.line(x0, y0, start_point.x + in_x, start_point.y + in_y, u32(0x00FF0000))

					//	start_point.x + in_x, start_point.y + in_y, u32(0xFF00FF00))
					bmp.quadratic(x0, y0, start_point.x, start_point.y, start_point.x,
						start_point.y, color)
				} else {
					// ctx.quadraticCurveTo(prev.x + x, prev.y + y,
					//        (prev.x + point.x) / 2 + x,
					//        (prev.y + point.y) / 2 + y);

					// bmp.line(x0, y0, start_point.x, start_point.y, u32(0x00FF0000)
					// u32(0xFF000000))
					bmp.quadratic(x0, y0, start_point.x, start_point.y, (point.x + start_point.x) / 2,
						(point.y + start_point.y) / 2, color)
				}
			} else {
				// last point not in a curve
				// bmp.line(point.x, point.y, sp_x, sp_y, u32(0x00FF0000))
				bmp.line(point.x, point.y, sp_x, sp_y, color)
			}
			contour_start = count + 1
			s = 0
			c++
		}
	}

	if bmp.style == .filled || bmp.style == .raw {
		bmp.exec_filler()
	}
	x_min := glyph.x_min
	x_max := glyph.x_max
	return x_min, x_max

	// return glyph.x_min, glyph.x_max
}
