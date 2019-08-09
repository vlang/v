// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module freetype 

import (
	os 
	gx 
	gg 
	stbi
	glm
	gl
) 

#flag darwin -I/usr/local/include/freetype2
#flag darwin -I/opt/local/include/freetype2
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

struct Face {
	cobj voidptr
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
	utf_runes []string
	utf_chars []Character
	face      Face
	scale     int // retina = 2 , normal = 1
}

/* 
struct Cfg {
	width     int
	height    int
	use_ortho bool 
	retina    bool
	scale int 
	font_size int
	create_window bool 
	window_user_ptr voidptr 
	window_title string 
	always_on_top bool 
}
*/ 


// jfn ft_load_char(face FT_Face, code FT_ULong) Character {
// fn ft_load_char(_face voidptr, _code voidptr) Character {
fn ft_load_char(_face Face, code i64) Character {
	// #FT_Face face = *(FT_Face*)(_face); FT_ULong code = *(FT_ULong*)(code);
	# FT_Face face = *((FT_Face*)_face.cobj);
	# if (FT_Load_Char(face, code, FT_LOAD_RENDER))
	{
		println('freetype: Failed to load Glyph')
		exit(1)
	}
	// Generate texture
	# GLuint texture;
	# glGenTextures(1, &texture);
	# glBindTexture(GL_TEXTURE_2D, texture);
	# glTexImage2D(
	# GL_TEXTURE_2D,
	# 0,
	# GL_RED,
	# face->glyph->bitmap.width,
	# face->glyph->bitmap.rows,
	# 0,
	# GL_RED,
	# GL_UNSIGNED_BYTE,
	# face->glyph->bitmap.buffer
	# );
	// Set texture options
	# glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	# glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	# glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	# glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	// Now store character for later use
	ch := Character{}
	# ch.texture_id=texture ;
	# ch.size  = gg__vec2(face->glyph->bitmap.width, face->glyph->bitmap.rows);
	# ch.bearing = gg__vec2(face->glyph->bitmap_left, face->glyph->bitmap_top),
	# ch.advance = face->glyph->advance.x;
	return ch
}

pub fn new_context(cfg gg.Cfg) *Context {
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
	gl.enable(GL_BLEND)
//return &GG{} 
	// return &GG{}
	# glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	shader := gl.new_shader('text')
	shader.use()
	projection := glm.ortho(0, width, 0, height)// 0 at BOT
	// projection_new := ortho(0, width, 0, height)// 0 at BOT
	// projection := gl.ortho(0, width,height,0)  // 0 at TOP
	shader.set_mat4('projection', projection)
	// FREETYPE
	# FT_Library ft;
	// All functions return a value different than 0 whenever an error occurred
	# if (FT_Init_FreeType(&ft))
	println('ERROR::FREETYPE: Could not init FreeType Library')
	// Load font as face
	// face := FT_Face{}
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
	# FT_Face face;
	# if (FT_New_Face(ft, font_path.str, 0, &face))
	// # if (FT_New_Face(ft, "/Library/Fonts/Courier New.ttf", 0, &face))
	// # if (FT_New_Face(ft, "/System/Library/Fonts/Apple Color Emoji.ttc", 0, &face))
	{
		println('freetyp: Failed to load font')
		exit(1)
	}
	// Set size to load glyphs as
	# FT_Set_Pixel_Sizes(face, 0, font_size) ;
	// Disable byte-alignment restriction
	# glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	// Gen texture
	// Load first 128 characters of ASCII set
	mut chars := []Character{}
	f := Face {
		cobj: 0
	}
	# f.cobj = &face;
	// # for (GLubyte c = 0; c < 128; c++)
	for c := 0; c < 128; c++ {
		// ch := Character{}
		// ch:=ft_load_char(face, c)
		// # ch =gg__ft_load_char(&face, &c);
		// ////////////////////////////////
		mut ch := ft_load_char(f, i64(c))
		// s := utf32_to_str(uint(0x043f))
		// s := 'п'
		// ch = ft_load_char(f, s.utf32_code())
		// # ch = gg__ft_load_char(f, 0x043f); // RUS P
		// # unsigned long c = FT_Get_Char_Index(face,              0x043f );
		// # printf("!!!!!!!!! %lu\n", c);
		// # c = FT_Get_Char_Index(face,              0xd0bf );
		// # printf("!!!!!!!!! %lu\n", c);
		// # ch = gg__ft_load_char(f, 0xd0bf) ;  // UTF 8
		chars << ch
	}
	ch := Character{}
	// # ch = gg__ft_load_char(f, 0x0000043f);
	// # ch = gg__ft_load_char(f, 128169);
	// chars.push(ch)
	// Configure VAO
	vao := gl.gen_vertex_array()
	println('new gg text context vao=$vao')
	vbo := gl.gen_buffer()
	gl.bind_vao(vao)
	gl.bind_buffer(GL_ARRAY_BUFFER, vbo)
	// # glBufferData(GL_ARRAY_BUFFER, sizeof(GLf32) * 6 * 4, NULL, GL_DYNAMIC_DRAW);
	gl.enable_vertex_attrib_array(0)
	gl.vertex_attrib_pointer(0, 4, GL_FLOAT, false, 4, 0)
	// # glVertexAttribPointer(0, 4, GL_FLOAT,false, 4 * sizeof(GLf32), 0);
	// gl.bind_buffer(GL_ARRAY_BUFFER, uint(0))
	// # glBindVertexArray(0);
	mut ctx := &Context {
		shader: shader,
		width: width,
		height: height,
		scale: scale
		vao: vao,
		vbo: vbo,
		chars: chars,
		face: f
	}
	ctx.init_utf8_runes()
	return ctx
}

// A dirty hack to implement rendering of cyrillic letters.
// All UTF-8 must be supported. 
fn (ctx mut Context) init_utf8_runes() {
	s := '≈йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ'
	println(s)
	us := s.ustring()
	for i := 0; i < us.len; i++ {
		_rune := us.at(i)
		ch := ft_load_char(ctx.face, _rune.utf32_code())
		// ctx.utf_rune_map.set(rune, ch)
		ctx.utf_runes << _rune
		ctx.utf_chars << ch
	}
}

// fn (ctx &GG) render_text(text string, x, y, scale f32, color gx.Color) {
pub fn (ctx &Context) draw_text(_x, _y int, text string, cfg gx.TextCfg) {
	utext := text.ustring_tmp()
	// utext := text.ustring()
	ctx._draw_text(_x, _y, utext, cfg)
	// utext.free()
	// # glScissor(0,0,ctx->width*2,ctx->height*2);
	// gl.disable(GL_SCISSOR_TEST)// TODO
	// #free(text.str);
}

fn (ctx &Context) draw_text_fast(_x, _y int, text ustring, cfg gx.TextCfg) {
	ctx._draw_text(_x, _y, text, cfg)
}

// TODO  HACK with second text context
// fn (ctx &GG) _draw_text(_x, _y int, text string, cfg gx.TextCfg) {
fn (ctx &Context) _draw_text(_x, _y int, utext ustring, cfg gx.TextCfg) {
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
	# glActiveTexture(GL_TEXTURE0);
	gl.bind_vao(ctx.vao)
	// Iterate through all characters
	// utext := text.ustring()
	for i := 0; i < utext.len; i++ {
		_rune := utext.at(i)
		// println('$i => $_rune')
		mut ch := Character{}
		if _rune.len == 1 {
			idx := _rune[0]
			if idx < 0 || idx >= ctx.chars.len {
				println('BADE RUNE $_rune')
				continue
			}
			ch = ctx.chars[_rune[0]]
		}
		else if _rune.len > 1 {
			// TODO O(1) use map
			for j := 0; j < ctx.utf_runes.len; j++ {
				rune_j := ctx.utf_runes[j]
				// if string_eq(ctx.utf_runes[j], rune) {
				if rune_j==_rune {
					ch = ctx.utf_chars[j]
					break
				}
			}
		}
		if ch.size.x == 0 {
			// continue
		}
		// mut c := int(text[i])
		// c = 128
		// s := 'A'
		// c := int(s[0])
		// ch := ctx.chars[c]
		xpos := x + f32(ch.bearing.x) * 1
		ypos := y - f32(ch.size.y - ch.bearing.y) * 1
		w := f32(ch.size.x) * 1
		h := f32(ch.size.y) * 1
		// Update VBO for each character
		# GLfloat vertices[6][4] = {
		# { xpos,     ypos + h,   0.0, 0.0 },
		# { xpos,     ypos,       0.0, 1.0 },
		# { xpos + w, ypos,       1.0, 1.0 },
		# { xpos,     ypos + h,   0.0, 0.0 },
		# { xpos + w, ypos,       1.0, 1.0 },
		# { xpos + w, ypos + h,   1.0, 0.0 }
		# };
		// t := glfw.get_time()
		// Render glyph texture over quad
		// t1 := glfw.get_time()
		# glBindTexture(GL_TEXTURE_2D, ch.texture_id);
		// Update content of VBO memory
		gl.bind_buffer(GL_ARRAY_BUFFER, ctx.vbo)
		// t2 := glfw.get_time()
		// # glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(vertices), vertices); // Be sure to use glBufferSubData and not glBufferData
		# glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_DYNAMIC_DRAW);
		// t3 := glfw.get_time()
		// gl.bind_buffer(GL_ARRAY_BUFFER, uint(0))
		// t4 := glfw.get_time()
		// Render quad
		gl.draw_arrays(GL_TRIANGLES, 0, 6)
		// t5 := glfw.get_time()
		// # if (glfw__get_time() - t > 0.001)
		// {
		// # printf("do_text = %f '%s' \n", glfw__get_time() - t, text.str);
		// # printf("t1=%f, t2=%f, t3=%f, t4=%f, t5=%f\n\n\n", t1-t, t2-t1, t3-t2, t4-t3, t5-t4);
		// }
		// Now advance cursors for next glyph (note that advance is number of 1/64 pixels)
		// Bitshift by 6 to get value in pixels (2^6 = 64 (divide amount of 1/64th pixels by 64 to get amount of pixels))
		# x += (ch.advance >> 6) * 1;
	}
	gl.bind_vao(u32(0))
	# glBindTexture(GL_TEXTURE_2D, 0);
	// runes.free()
	// #free(runes.data);
}

pub fn (ctx &Context) draw_text_def(x, y int, text string) {
	cfg := gx.TextCfg {
		color: gx.Black,
		size: DEFAULT_FONT_SIZE,
		align: gx.ALIGN_LEFT,
	}
	ctx.draw_text(x, y, text, cfg)
}
