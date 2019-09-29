// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module freetype

import (
	os
	gx
	gg
	glm
	gl
)

#flag windows -I @VROOT/thirdparty/freetype/include
#flag windows -L @VROOT/thirdparty/freetype/win64

#flag darwin -I/usr/local/include/freetype2
#flag darwin -I/opt/local/include/freetype2
#flag freebsd -I/usr/local/include/freetype2
#flag freebsd -Wl,-L/usr/local/lib
#flag -lfreetype

//#flag -I @VROOT/thirdparty/freetype

//#flag @VROOT/thirdparty/freetype/libfreetype.a
#flag darwin -lpng -lbz2 -lz


#flag linux 	-I/usr/include/freetype2
#flag linux -I.


#include "ft2build.h"
#include FT_FREETYPE_H



const (
	DEFAULT_FONT_SIZE = 12
)

struct Character {
	texture_id u32
	size       gg.Vec2
	bearing    gg.Vec2
	advance    u32
}

[typedef]
struct C.FT_Library {
	_z int
}

struct Context {
	shader    gl.Shader
	// use_ortho bool
	width     int
	height    int
	vao       u32
	rect_vao  u32
	rect_vbo  u32
	line_vao  u32
	line_vbo  u32
	vbo       u32
	chars     []Character
	face      C.FT_Face
	scale     int // retina = 2 , normal = 1
mut:
	utf_runes []string
	utf_chars []Character
}

struct C.Bitmap {
	width int
	rows int
	buffer int
}

struct C.Advance {
	x int
}
	
struct C.Glyph {
	bitmap Bitmap
	bitmap_left int
	bitmap_top int
	advance Advance
}

[typedef]
struct C.FT_Face {
	glyph &Glyph
}

fn C.FT_Load_Char(voidptr, i64, int) int

fn ft_load_char(face C.FT_Face, code i64) Character {
	//println('\nftload_char( code=$code)')
	//C.printf('face=%p\n', face)
	//C.printf('cobj=%p\n', _face.cobj)
	ret := C.FT_Load_Char(face, code, C.FT_LOAD_RENDER)
	//println('ret=$ret')
	if ret != 0 {
		println('freetype: failed to load glyph (utf32 code=$code, ' +
			'error code=$ret)')
		return Character{}
	}
	// Generate texture
	mut texture := 0
	C.glGenTextures(1, &texture)
	C.glBindTexture(C.GL_TEXTURE_2D, texture)
	fgwidth := face.glyph.bitmap.width
	fgrows  := face.glyph.bitmap.rows
	C.glTexImage2D(C.GL_TEXTURE_2D, 0, C.GL_RED, fgwidth,  fgrows,
		0, C.GL_RED, C.GL_UNSIGNED_BYTE, face.glyph.bitmap.buffer)
	// Set texture options
	C.glTexParameteri(C.GL_TEXTURE_2D, C.GL_TEXTURE_WRAP_S, C.GL_CLAMP_TO_EDGE)
	C.glTexParameteri(C.GL_TEXTURE_2D, C.GL_TEXTURE_WRAP_T, C.GL_CLAMP_TO_EDGE)
	C.glTexParameteri(C.GL_TEXTURE_2D, C.GL_TEXTURE_MIN_FILTER, C.GL_LINEAR)
	C.glTexParameteri(C.GL_TEXTURE_2D, C.GL_TEXTURE_MAG_FILTER, C.GL_LINEAR)
	fgleft := face.glyph.bitmap_left
	fgtop := face.glyph.bitmap_top
	// Create the character
	return Character {
		texture_id: u32(texture)
		size:    gg.vec2(int(u32(fgwidth)), int(u32(fgrows)))
		bearing: gg.vec2(int(u32(fgleft)), int(u32(fgtop)))
		advance: (u32(face.glyph.advance.x))
	}
}

pub fn new_context(cfg gg.Cfg) &Context {
	scale := cfg.scale
	// Can only have text in ortho mode
	if !cfg.use_ortho {
		return &Context{}
	}
	mut width := cfg.width * scale
	mut height := cfg.height * scale
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
	projection := glm.ortho(0, width, 0, height)// 0 at BOT
	shader.set_mat4('projection', projection)
	// FREETYPE
	ft := FT_Library{0}
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
	if !os.file_exists(font_path) {
		exe_path := os.executable()
		exe_dir := os.basedir(exe_path)
		font_path = '$exe_dir/$font_path'
	}
	if !os.file_exists(font_path) {
		println('failed to load $font_path')
		return 0
	}
	println('Trying to load font from $font_path')
	face := C.FT_Face{}
	ret = int(C.FT_New_Face(ft, font_path.str, 0, &face))
	if ret != 0	{
		println('freetype: failed to load the font (error=$ret)')
		exit(1)
	}
	// Set size to load glyphs as
	C.FT_Set_Pixel_Sizes(face, 0, font_size)
	// Disable byte-alignment restriction
	C.glPixelStorei(C.GL_UNPACK_ALIGNMENT, 1)
	// Gen texture
	// Load first 128 characters of ASCII set
	mut chars := []Character{}
	for c := 0; c < 128; c++ {
		mut ch := ft_load_char(face, i64(c))
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
	ch := Character{}
	// Configure VAO
	vao := gl.gen_vertex_array()
	println('new gg text context vao=$vao')
	vbo := gl.gen_buffer()
	gl.bind_vao(vao)
	gl.bind_buffer(C.GL_ARRAY_BUFFER, vbo)
	// # glBufferData(GL_ARRAY_BUFFER, sizeof(GLf32) * 6 * 4, NULL, GL_DYNAMIC_DRAW);
	gl.enable_vertex_attrib_array(0)
	gl.vertex_attrib_pointer(0, 4, C.GL_FLOAT, false, 4, 0)
	// # glVertexAttribPointer(0, 4, GL_FLOAT,false, 4 * sizeof(GLf32), 0);
	// gl.bind_buffer(GL_ARRAY_BUFFER, uint(0))
	// # glBindVertexArray(0);
	mut ctx := &Context {
		shader: shader
		width: width
		height: height
		scale: scale
		vao: vao
		vbo: vbo
		chars: chars
		face: face
	}
	//ctx.init_utf8_runes()
	return ctx
}

/*
// A dirty hack to implement rendering of cyrillic letters.
// All UTF-8 must be supported. update: no longer needed
fn (ctx mut Context) init_utf8_runes() {
	s := '≈≠⩽⩾йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ'
	print('init utf8 runes: ')
	//println(s)
	us := s.ustring()
	for i := 0; i < us.len; i++ {
		_rune := us.at(i)
		ch := ft_load_char(ctx.face, _rune.utf32_code())
		// ctx.utf_rune_map.set(rune, ch)
		ctx.utf_runes << _rune
		ctx.utf_chars << ch
	}
}
*/

pub fn (ctx mut Context) draw_text(_x, _y int, text string, cfg gx.TextCfg) {
	//utext := text.ustring_tmp()
	utext := text.ustring()
	ctx._draw_text(_x, _y, utext, cfg)
}

fn (ctx mut Context) draw_text_fast(_x, _y int, text ustring, cfg gx.TextCfg) {
	ctx._draw_text(_x, _y, text, cfg)
}

fn (ctx mut Context) _draw_text(_x, _y int, utext ustring, cfg gx.TextCfg) {
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
	// println('scale=$ctx.scale size=$cfg.size')
	if cfg.align == gx.ALIGN_RIGHT {
		width := utext.len * 7
		x -= width + 10
	}
	x *= ctx.scale// f32(2)
	// println('y=$_y height=$ctx.height')
	// _y = _y * int(ctx.scale) //+ 26
	y = y * int(ctx.scale) + ((cfg.size * ctx.scale) / 2) + 5 * ctx.scale
	y = f32(ctx.height) - y
	color := cfg.color
	// Activate corresponding render state
	ctx.shader.use()
	ctx.shader.set_color('textColor', color)
	C.glActiveTexture(C.GL_TEXTURE0)
	gl.bind_vao(ctx.vao)
	// Iterate through all characters
	// utext := text.ustring()
	for i := 0; i < utext.len; i++ {
		_rune := utext.at(i)
		// println('$i => $_rune')
		mut ch := Character{}
		mut found := false
		if _rune.len == 1 {
			idx := _rune[0]
			if idx < 0 || idx >= ctx.chars.len {
				println('BADE RUNE $_rune')
				continue
			}
			found = true
			ch = ctx.chars[_rune[0]]
		}
		else if _rune.len > 1 {
			// TODO O(1) use map
			for j := 0; j < ctx.utf_runes.len; j++ {
				rune_j := ctx.utf_runes[j]
				if rune_j==_rune {
					ch = ctx.utf_chars[j]
					found = true
					break
				}
			}
		}
		// A new Unicode character. Load it and cache it.
		if !found && _rune.len > 0 && _rune[0] > 32 {
			c := _rune[0]
			//println('cant draw rune "$_rune" code=$c, loading')
			//continue
			ch = ft_load_char(ctx.face, _rune.utf32_code())
			//println('done loading')
			ctx.utf_runes << _rune
			ctx.utf_chars << ch
			//exit(1)
			// continue
		}
		xpos := x + f32(ch.bearing.x) * 1
		ypos := y - f32(ch.size.y - ch.bearing.y) * 1
		w := f32(ch.size.x) * 1
		h := f32(ch.size.y) * 1
		// Update VBO for each character
		vertices :=	[
		 xpos,     ypos + h,   0.0, 0.0 ,
		 xpos,     ypos,       0.0, 1.0 ,
		 xpos + w, ypos,       1.0, 1.0 ,
		 xpos,     ypos + h,   0.0, 0.0 ,
		 xpos + w, ypos,       1.0, 1.0 ,
		 xpos + w, ypos + h,   1.0, 0.0
		]
		// Render glyph texture over quad
		C.glBindTexture(C.GL_TEXTURE_2D, ch.texture_id)
		// Update content of VBO memory
		gl.bind_buffer(C.GL_ARRAY_BUFFER, ctx.vbo)
		// glBufferSubData(..)
		C.glBufferData(C.GL_ARRAY_BUFFER, 96, vertices.data, C.GL_DYNAMIC_DRAW)
		// Render quad
		gl.draw_arrays(C.GL_TRIANGLES, 0, 6)
		// Now advance cursors for next glyph (note that advance is number of 1/64 pixels)
		// Bitshift by 6 to get value in pixels (2^6 = 64 (divide amount of 1/64th pixels by 64 to get amount of pixels))
		x += ch.advance >> u32(6)
	}
	gl.bind_vao(u32(0))
	C.glBindTexture(C.GL_TEXTURE_2D, 0)
}

pub fn (ctx mut Context) draw_text_def(x, y int, text string) {
	cfg := gx.TextCfg {
		color: gx.Black,
		size: DEFAULT_FONT_SIZE,
		align: gx.ALIGN_LEFT,
	}
	ctx.draw_text(x, y, text, cfg)
}
