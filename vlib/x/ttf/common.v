module ttf

/**********************************************************************
*
* Common data for the module
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* Note:
*
* TODO:
**********************************************************************/
import os
import math

// text align
pub enum Text_align {
	left
	center
	right
	justify
}

// draw style
pub enum Style {
	outline
	outline_aliased
	filled
	raw
}

/******************************************************************************
*
* DEBUG Utility
*
******************************************************************************/
const debug_flag = false

fn dprintln(txt string) {
	if debug_flag {
		println(txt)
	}
}

/******************************************************************************
*
* Utility
*
******************************************************************************/
// transform the bitmap from one layer to color layers
fn (mut bmp BitMap) format_texture() {
	r := u8(bmp.color >> 24)
	g := u8((bmp.color >> 16) & 0xFF)
	b := u8((bmp.color >> 8) & 0xFF)
	a := u8(bmp.color & 0xFF)

	b_r := u8(bmp.bg_color >> 24)
	b_g := u8((bmp.bg_color >> 16) & 0xFF)
	b_b := u8((bmp.bg_color >> 8) & 0xFF)
	b_a := u8(bmp.bg_color & 0xFF)

	// transform buffer in a texture
	x := bmp.buf
	unsafe {
		mut i := 0
		for i < bmp.buf_size {
			data := x[i]
			if data > 0 {
				x[i + 0] = r
				x[i + 1] = g
				x[i + 2] = b
				// alpha
				x[i + 3] = u8(u16(u16(a) * data) >> 8)
			} else {
				x[i + 0] = b_r
				x[i + 1] = b_g
				x[i + 2] = b_b
				x[i + 3] = b_a
			}
			i += 4
		}
	}
}

// write out a .ppm file
pub fn (mut bmp BitMap) save_as_ppm(file_name string) {
	tmp_buf := bmp.buf
	mut buf := unsafe { malloc_noscan(bmp.buf_size) }
	unsafe { vmemcpy(buf, tmp_buf, bmp.buf_size) }
	bmp.buf = buf

	bmp.format_texture()
	npixels := bmp.width * bmp.height
	mut f_out := os.create(file_name) or { panic(err) }
	f_out.writeln('P3') or { panic(err) }
	f_out.writeln('${bmp.width} ${bmp.height}') or { panic(err) }
	f_out.writeln('255') or { panic(err) }
	for i in 0 .. npixels {
		pos := i * bmp.bp
		unsafe {
			c_r := bmp.buf[pos]
			c_g := bmp.buf[pos + 1]
			c_b := bmp.buf[pos + 2]
			f_out.write_string('${c_r} ${c_g} ${c_b} ') or { panic(err) }
		}
	}
	f_out.close()

	unsafe {
		free(buf)
	}
	bmp.buf = tmp_buf
}

pub fn (mut bmp BitMap) get_raw_bytes() []u8 {
	mut f_buf := []u8{len: bmp.buf_size / 4}
	mut i := 0
	for i < bmp.buf_size {
		unsafe {
			f_buf[i >> 2] = *(bmp.buf + i)
		}
		i += 4
	}
	return f_buf
}

pub fn (mut bmp BitMap) save_raw_data(file_name string) {
	os.write_file_array(file_name, bmp.get_raw_bytes()) or { panic(err) }
}

//
// Math functions
//
// integer part of x
@[inline]
fn ipart(x f32) f32 {
	return f32(math.floor(x))
}

@[inline]
fn round(x f32) f32 {
	return ipart(x + 0.5)
}

// fractional part of x
@[inline]
fn fpart(x f32) f32 {
	return x - f32(math.floor(x))
}

@[inline]
fn rfpart(x f32) f32 {
	return 1 - fpart(x)
}

/******************************************************************************
*
* Colors
*
******************************************************************************/
/*
@[inline]
pub fn (mut dev BitMap) get_color(x int, y int) (int, int, int, int){
	if x < 0 || x >= dev.width || y < 0 || y >= dev.height {
		return 0,0,0,0
	}
	mut i := (x + y * dev.width)*dev.bp
	unsafe{
		return dev.buf[i], dev.buf[i+1], dev.buf[i+2], dev.buf[i+3]
	}
}

@[inline]
pub fn (mut dev BitMap) get_color_u32(x int, y int) u32{
	r, g, b, a := dev.get_color(x, y)
	unsafe{
		return u32(r<<24) | u32(g<<16) | u32(b<<8) | u32(a)
	}
}
*/
/******************************************************************************
*
* Drawing
*
******************************************************************************/
@[inline]
pub fn color_multiply_alpha(c u32, level f32) u32 {
	return u32(f32(c & 0xFF) * level)
}

@[inline]
pub fn color_multiply(c u32, level f32) u32 {
	mut r := (f32((c >> 24) & 0xFF) / 255.0) * level
	mut g := (f32((c >> 16) & 0xFF) / 255.0) * level
	mut b := (f32((c >> 8) & 0xFF) / 255.0) * level
	mut a := (f32(c & 0xFF) / 255.0) * level
	r = if r > 1.0 { 1.0 } else { r }
	g = if g > 1.0 { 1.0 } else { g }
	b = if b > 1.0 { 1.0 } else { b }
	a = if a > 1.0 { 1.0 } else { a }

	return (u32(r * 255) << 24) | (u32(g * 255) << 16) | (u32(b * 255) << 8) | u32(a * 255)
}
