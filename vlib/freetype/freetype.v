// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module freetype

import os
import gx
import gg
import glm
import gl

/*
TODO
!!!!!!
Use a font atlas
!!!!!!
*/
#flag windows -I @VROOT/thirdparty/freetype/include
#flag windows -L @VROOT/thirdparty/freetype/win64
#flag solaris -I/opt/local/include/freetype2
#flag solaris -L/opt/local/lib
#flag darwin -I/usr/local/include/freetype2
// MacPorts
#flag darwin -I/opt/local/include/freetype2
#flag darwin -L/opt/local/lib
#flag freebsd -I/usr/local/include/freetype2
#flag freebsd -Wl -L/usr/local/lib
#flag -lfreetype
// #flag -I @VROOT/thirdparty/freetype
// #flag @VROOT/thirdparty/freetype/libfreetype.a
#flag darwin -lpng -lbz2 -lz
#flag linux 	-I/usr/include/freetype2
#flag linux -I.
#include "ft2build.h"
#include FT_FREETYPE_H
fn C.FT_Init_FreeType() voidptr

fn C.FT_New_Face() voidptr

fn C.FT_Set_Pixel_Sizes()

pub const (
	default_font_size = 12
)

struct Character {
	code                  i64
	texture_id            u32
	size                  gg.Vec2
	horizontal_bearing_px gg.Vec2
	horizontal_advance_px u32
	vertical_bearing_px   gg.Vec2
	vertical_advance_px   u32
}

[typedef]
struct C.FT_Library {
}

pub struct FreeType {
	shader    gl.Shader
	width     int
	height    int
	vao       u32
	rect_vao  u32
	rect_vbo  u32
	line_vao  u32
	line_vbo  u32
	vbo       u32
	chars     []Character
	face      &C.FT_FaceRec
	scale     int // retina = 2 , normal = 1
mut:
	utf_runes []string
	utf_chars []Character
}

struct C.Bitmap {
	width  int
	rows   int
	buffer int
}

struct C.Advance {
	x int
	y int
}

[typedef]
struct C.FT_Glyph_Metrics {
	width        int
	height       int
	horiBearingX int
	horiBearingY int
	horiAdvance  int
	vertBearingX int
	vertBearingY int
	vertAdvance  int
}

struct C.Glyph {
	bitmap      C.Bitmap
	bitmap_left int
	bitmap_top  int
	advance     Advance
	metrics     C.FT_Glyph_Metrics
}

[typedef]
struct C.FT_FaceRec {
	glyph       &C.Glyph
	family_name charptr
	style_name  charptr
}

// /type FT_Face &C.FT_FaceRec
fn C.FT_Load_Char(voidptr, i64, int) int

fn ft_load_char(face &C.FT_FaceRec, code i64) Character {
	// println('\nftload_char( code=$code)')
	// C.printf('face=%p\n', face)
	// C.printf('cobj=%p\n', _face.cobj)
	ret := C.FT_Load_Char(face, code, C.FT_LOAD_RENDER | C.FT_LOAD_FORCE_AUTOHINT)
	// println('ret=$ret')
	if ret != 0 {
		println('freetype: failed to load glyph (utf32 code=$code, ' + 'error code=$ret)')
		return Character{
			code: code
	}
	}
	// Generate texture
	mut texture := 0
	C.glGenTextures(1, &texture)
	C.glBindTexture(C.GL_TEXTURE_2D, texture)
	fgwidth := (*face).glyph.bitmap.width
	fgrows := (*face).glyph.bitmap.rows
	C.glTexImage2D(C.GL_TEXTURE_2D, 0, C.GL_RED, fgwidth, fgrows, 0, C.GL_RED, C.GL_UNSIGNED_BYTE,
		(*face).glyph.bitmap.buffer)
	// Set texture options
	C.glTexParameteri(C.GL_TEXTURE_2D, C.GL_TEXTURE_WRAP_S, C.GL_CLAMP_TO_EDGE)
	C.glTexParameteri(C.GL_TEXTURE_2D, C.GL_TEXTURE_WRAP_T, C.GL_CLAMP_TO_EDGE)
	C.glTexParameteri(C.GL_TEXTURE_2D, C.GL_TEXTURE_MIN_FILTER, C.GL_LINEAR)
	C.glTexParameteri(C.GL_TEXTURE_2D, C.GL_TEXTURE_MAG_FILTER, C.GL_LINEAR)
	// Create the character
	return Character {
		code: code
		texture_id: u32(texture)
		size:    gg.vec2(fgwidth, fgrows)

		// Note: advance is number of 1/64 pixels
		// Bitshift by 6 to get value in pixels (2^6 = 64 (divide amount of 1/64th pixels by 64 to get amount of pixels))
		horizontal_bearing_px:  gg.vec2((*face).glyph.metrics.horiBearingX >> 6, (*face).glyph.metrics.horiBearingY >> 6)
		vertical_bearing_px:    gg.vec2((*face).glyph.metrics.vertBearingX >> 6, (*face).glyph.metrics.vertBearingY >> 6) // not used for now

		horizontal_advance_px:  u32((*face).glyph.metrics.horiAdvance) >> 6
		vertical_advance_px:    u32((*face).glyph.metrics.vertAdvance) >> 6
	}
}

pub fn new_context(cfg gg.Cfg) &FreeType {
	scale := cfg.scale
	// Can only have text in ortho mode
	if !cfg.use_ortho {
		return &FreeType{
			face: 0
	}
	}
	width := cfg.width * scale
	height := cfg.height * scale
	font_size := cfg.font_size * scale
	// exit('fs=$font_size')
	// if false {
	// retina
	// width = width * 2// scale// 2
	// height = height * 2// scale// 2
	// font_size *= scale// 2
	// }
	/*
	gl.viewport(0, 0, width, height)
*/
	// gl.enable(GL_CULL_FACE) // TODO NEED CULL?
	gl.enable(C.GL_BLEND)
	C.glBlendFunc(C.GL_SRC_ALPHA, C.GL_ONE_MINUS_SRC_ALPHA)
	shader := gl.new_shader('text')
	shader.use()
	projection := glm.ortho(0, width, 0, height) // 0 at BOT
	shader.set_mat4('projection', projection)
	// FREETYPE
	ft := C.FT_Library{}
	// All functions return a value different than 0 whenever
	// an error occurred
	mut ret := C.FT_Init_FreeType(&ft)
	if ret != 0 {
		panic('freetype: Could not init FreeType Library')
	}
	// Load font as face
	mut font_path := cfg.font_path
	if font_path == '' {
		font_path = 'RobotoMono-Regular.ttf'
	}
	if !os.exists(font_path) {
		font_path = os.resource_abs_path(font_path)
	}
	if !os.exists(font_path) {
		eprintln('freetype: font "$font_path" does not exist')
		return voidptr(0)
	}
	face := &C.FT_FaceRec{
		glyph: 0
	}
	ret = int(C.FT_New_Face(ft, font_path.str, 0, &face))
	if ret != 0 {
		eprintln('freetype: failed to load font (error=$ret) from path: $font_path')
		exit(1)
	}
	// Set size to load glyphs as
	C.FT_Set_Pixel_Sizes(face, 0, font_size)
	// Disable byte-alignment restriction
	C.glPixelStorei(C.GL_UNPACK_ALIGNMENT, 1)
	// Gen texture
	// Load first 128 characters of ASCII set
	mut chars := []Character{}
	for c in 0 .. 128 {
		ch := ft_load_char(face, i64(c))
		// s := utf32_to_str(uint(0x043f))
		// s := 'п'
		// ch = ft_load_char(f, s.utf32_code())
		// # unsigned long c = FT_Get_Char_Index(face,              0x043f );
		// # printf("!!!!!!!!! %lu\n", c);
		// # c = FT_Get_Char_Index(face,              0xd0bf );
		// # printf("!!!!!!!!! %lu\n", c);
		// # ch = gg__ft_load_char(f, 0xd0bf) ;  // UTF 8
		chars << ch
	}
	// ch := Character{}
	// Configure VAO
	vao := gl.gen_vertex_array()
	// println('new gg text context vao=$vao')
	vbo := gl.gen_buffer()
	gl.bind_vao(vao)
	gl.bind_buffer(C.GL_ARRAY_BUFFER, vbo)
	// # glBufferData(GL_ARRAY_BUFFER, sizeof(GLf32) * 6 * 4, NULL, GL_DYNAMIC_DRAW);
	gl.enable_vertex_attrib_array(0)
	gl.vertex_attrib_pointer(0, 4, C.GL_FLOAT, false, 4, 0)
	// # glVertexAttribPointer(0, 4, GL_FLOAT,false, 4 * sizeof(GLf32), 0);
	// gl.bind_buffer(GL_ARRAY_BUFFER, uint(0))
	// # glBindVertexArray(0);
	ctx := &FreeType{
		shader: shader
		width: width
		height: height
		scale: scale
		vao: vao
		vbo: vbo
		chars: chars
		face: face
	}
	// ctx.init_utf8_runes()
	return ctx
}

pub fn (mut ctx FreeType) draw_text(_x, _y int, text string, cfg gx.TextCfg) {
	// utext := text.ustring_tmp()
	utext := text.ustring()
	ctx.private_draw_text(_x, _y, utext, cfg)
}

fn (mut ctx FreeType) draw_text_fast(_x, _y int, text ustring, cfg gx.TextCfg) {
	ctx.private_draw_text(_x, _y, text, cfg)
}

fn (mut ctx FreeType) private_draw_text(_x, _y int, utext ustring, cfg gx.TextCfg) {
	/*
	if utext.s.contains('on_seg') {
		println('\nat(0)')
		println(utext.runes)
		firstc := utext.at(0)
		println('drawtext "$utext.s" len=$utext.s.len ulen=$utext.len x=$_x firstc=$firstc')
		if firstc != ' ' {
			exit(1)
		}
	}
*/
	mut x := f32(_x)
	mut y := f32(_y)
	wx, wy := ctx.text_size(utext.s)
	yoffset := if ctx.scale > 1 { 5 } else { -1 } // 5 hidpi, -1 lowdpi
	// println('scale=$ctx.scale size=$cfg.size')
	if cfg.align == gx.align_right {
		// width := utext.len * 7
		width := wx
		x -= f32(width + 10)
	}
	x *= f32(ctx.scale)
	y *= f32(ctx.scale)
	y += f32(yoffset)
	y = f32(ctx.height) - y // invert y direction
	color := cfg.color
	// Activate corresponding render state
	ctx.shader.use()
	ctx.shader.set_color('textColor', color)
	C.glActiveTexture(C.GL_TEXTURE0)
	gl.bind_vao(ctx.vao)
	// Iterate through all characters
	// utext := text.ustring()
	for i in 0 .. utext.len {
		rune_ := utext.at(i)
		// println('$i => $rune_')
		mut ch := Character{}
		mut found := false
		if rune_.len == 1 {
			idx := rune_[0]
			if idx < 0 || idx >= ctx.chars.len {
				println('BADE RUNE $rune_')
				continue
			}
			found = true
			ch = ctx.chars[rune_[0]]
		} else if rune_.len > 1 {
			// TODO O(1) use map
			for j in 0 .. ctx.utf_runes.len {
				rune_j := ctx.utf_runes[j]
				if rune_j == rune_ {
					ch = ctx.utf_chars[j]
					found = true
					break
				}
			}
		}
		// A new Unicode character. Load it and cache it.
		if !found && rune_.len > 0 && rune_[0] > 32 {
			// c := rune_[0]
			// println('cant draw rune "$rune_" code=$c, loading')
			// continue
			ch = ft_load_char(ctx.face, rune_.utf32_code())
			// println('done loading')
			ctx.utf_runes << rune_
			ctx.utf_chars << ch
			// exit(1)
			// continue
		}
		xpos := x + f32(ch.horizontal_bearing_px.x) * 1
		ypos := y - f32(ch.size.y + wy - ch.horizontal_bearing_px.y) * 1
		// ypos := y - wy
		w := f32(ch.size.x) * 1
		h := f32(ch.size.y) * 1
		// Update VBO for each character
		vertices := [
			xpos, ypos + h, 0.0, 0.0,
			xpos, ypos, 0.0, 1.0,
			xpos + w, ypos, 1.0, 1.0,
			xpos, ypos + h, 0.0, 0.0,
			xpos + w, ypos, 1.0, 1.0,
			xpos + w, ypos + h, 1.0, 0.0]
		// Render glyph texture over quad
		C.glBindTexture(C.GL_TEXTURE_2D, ch.texture_id)
		// Update content of VBO memory
		gl.bind_buffer(C.GL_ARRAY_BUFFER, ctx.vbo)
		// glBufferSubData(..)
		C.glBufferData(C.GL_ARRAY_BUFFER, 96, vertices.data, C.GL_DYNAMIC_DRAW)
		// Render quad
		gl.draw_arrays(C.GL_TRIANGLES, 0, 6)
		x += f32(ch.horizontal_advance_px)
		// Stop drawing if the limit is reached
		if cfg.max_width > 0 {
			if x >= cfg.max_width {
				// break
			}
		}
	}
	gl.bind_vao(u32(0))
	C.glBindTexture(C.GL_TEXTURE_2D, 0)
}

pub fn (mut ctx FreeType) draw_text_def(x, y int, text string) {
	cfg := gx.TextCfg{
		color: gx.black
		size: default_font_size
		align: gx.align_left
	}
	ctx.draw_text(x, y, text, cfg)
}

pub fn (mut ctx FreeType) text_width(s string) int {
	x, _ := ctx.text_size(s)
	return x
}

pub fn (mut ctx FreeType) text_height(s string) int {
	_, y := ctx.text_size(s)
	return y
}

pub fn (mut ctx FreeType) text_size(s string) (int, int) {
	// t := time.ticks()
	utext := s.ustring()
	mut x := u32(0)
	mut maxy := u32(0)
	mut rune_ := ''
	mut ch := Character{}
	for i in 0 .. utext.len {
		rune_ = utext.at(i)
		ch = Character{}
		mut found := false
		if rune_.len == 1 {
			idx := rune_[0]
			if idx < 0 || idx >= ctx.chars.len {
				println('BADE RUNE $rune_')
				continue
			}
			found = true
			ch = ctx.chars[rune_[0]]
		} else if rune_.len > 1 {
			// TODO O(1) use map
			for j in 0 .. ctx.utf_runes.len {
				rune_j := ctx.utf_runes[j]
				if rune_j == rune_ {
					ch = ctx.utf_chars[j]
					found = true
					break
				}
			}
		}
		if !found && rune_.len > 0 && rune_[0] > 32 {
			ch = ft_load_char(ctx.face, rune_.utf32_code())
			ctx.utf_runes << rune_
			ctx.utf_chars << ch
		}
		x += ch.horizontal_advance_px
		if maxy < ch.vertical_advance_px {
			maxy = ch.vertical_advance_px
		}
	}
	// println('text width "$s" = ${time.ticks() - t} ms')
	// scaled_x := x
	// scaled_y := maxy
	scaled_x := int(f64(x) / ctx.scale)
	scaled_y := int(f64(maxy) / ctx.scale)
	// println('text_size of "${s}" | x,y: $x,$maxy | scaled_x: ${scaled_x:3d} | scaled_y: ${scaled_y:3d} ')
	return scaled_x, scaled_y
}

/*
pub fn (f FT_Face) str() string {
	return 'FT_Face{ style_name: ${ptr_str(f.style_name)} family_name: ${ptr_str(f.family_name)} }'
}
*/
pub fn (ac []Character) str() string {
	mut res := []string{}
	for c in ac {
		res << '  Character{ code: $c.code , texture_id: $c.texture_id }'
	}
	return '[\n' + res.join(',\n') + ']'
}
