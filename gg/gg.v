// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module gg

import stbi
import glm
import gl

#flag darwin -I/usr/local/Cellar/freetype/2.10.0/include/freetype2
#flag -lfreetype
#flag linux 	-I/usr/include/freetype2
#flag linux -I.
#include "ft2build.h"
#include FT_FREETYPE_H
#include "glad.h"
struct Vec2 {
	x int
	y int
}

import const (
	GL_STATIC_DRAW
	GL_FLOAT
	GL_FALSE
	GL_UNSIGNED_INT
	GL_INT
)

const (
	DEFAULT_FONT_SIZE = 12
)

pub fn vec2(x, y int) Vec2 {
	res := Vec2 {
		x: x,
		y: y,
	}
	return res
}

struct Character {
	texture_id u32
	size       Vec2
	bearing    Vec2
	advance    u32
}

fn init() {
	println(gl.TEXT_VERT)
	gl.init_glad()
}

struct Face {
	cobj voidptr
	kek  int
}

struct Cfg {
	width     int
	height    int
	use_ortho int
	retina    bool
	font_size int
}

struct GG {
	shader    gl.Shader
	// use_ortho bool
	width     int
	height    int
	VAO       u32
	rect_vao  u32
	rect_vbo  u32
	line_vao  u32
	line_vbo  u32
	VBO       u32
	chars     []gg.Character
	utf_runes []string
	utf_chars []gg.Character
	text_ctx  *GG
	face      Face
	scale     int // retina = 2 , normal = 1
}

// fn new_context(width, height int, use_ortho bool, font_size int) *GG {
pub fn new_context(cfg Cfg) *GG {
	// println('new context orhto=$cfg.use_ortho')
	// # glScissor(0,0,300,300);
	shader := gl.new_shader('simple')
	shader.use()
	if cfg.use_ortho > 0 {
		projection := glm.ortho(0, cfg.width, cfg.height, 0)
		/* 
		// for debugging broken tetris in gg.o
		# projection.data[0]=0.010000;
		# projection.data[1]=0.000000;
		# projection.data[2]=0.000000;
		# projection.data[3]=0.000000;
		# projection.data[4]=0.000000;
		# projection.data[5]=-0.005000;
		# projection.data[6]=0.000000;
		# projection.data[7]=0.000000;
		# projection.data[8]=0.000000;
		# projection.data[9]=0.000000;
		# projection.data[10]=1.000000;
		# projection.data[11]=0.000000;
		# projection.data[12]=-1.000000;
		# projection.data[13]=1.000000;
		# projection.data[14]=0.000000;
		# projection.data[15]=1.000000;
*/
		// projection_new := ortho(0, width, height, 0)
		// println('\nORTHO OLD=')
		# for (int i=0;i<16;i++) printf("%d=%f ",i, projection.data[i]);
		// println('\n\n!ORTHO NEW=')
		// # for (int i=0;i<16;i++) printf("%d=%f ",i, projection_new[i]);
		// println('\n\n')
		println('setting o')
		shader.set_mat4('projection', projection)
	}
	else {
		// TODO move to function (allow volt functions to return arrrays without allocations)
		// i := glm.identity3()
		shader.set_mat4('projection', glm.identity())
	}
	VAO := gl.gen_vertex_array()
	println('new gg context VAO=$VAO')
	VBO := gl.gen_buffer()
	mut scale := 1
	if cfg.retina {
		scale = 2
	}
	mut ctx := &GG {
		shader: shader,
		width: cfg.width,
		height: cfg.height,
		VAO: VAO,
		VBO: VBO,
		// /line_vao: gl.gen_vertex_array()
		// /line_vbo: gl.gen_buffer()
		text_ctx: new_context_text(cfg, scale),
		scale: scale
		// use_ortho: use_ortho
	}
	// ctx.init_rect_vao()
	return ctx
}

pub fn (ctx &GG) draw_triangle(x1, y1, x2, y2, x3, y3 float, c gx.Color) {
	// println('draw_triangle $x1,$y1 $x2,$y2 $x3,$y3')
	ctx.shader.use()
	ctx.shader.set_color('color', c)
	vertices := [
	x1, y1, 0,
	x2, y2, 0,
	x3, y3, 0,
	] !
	// bind the Vertex Array Object first, then bind and set vertex buffer(s),
	// and then configure vertex attributes(s).
	gl.bind_vao(ctx.VAO)
	gl.set_vbo(ctx.VBO, vertices, GL_STATIC_DRAW)
	gl.vertex_attrib_pointer(0, 3, GL_FLOAT, false, 3, 0)
	gl.enable_vertex_attrib_array(0)
	// gl.bind_buffer(GL_ARRAY_BUFFER, uint(0))
	// You can unbind the VAO afterwards so other VAO calls won't accidentally modify this VAO,
	// but this rarely happens. Modifying other
	// VAOs requires a call to glBindVertexArray anyways so we generally don't unbind VAOs
	// (nor VBOs) when it's not directly necessary.
	// gl.bind_vertex_array(uint(0))
	// gl.bind_vertex_array(ctx.VAO)
	gl.draw_arrays(GL_TRIANGLES, 0, 3)
}

pub fn (ctx &GG) draw_triangle_tex(x1, y1, x2, y2, x3, y3 float, c gx.Color) {
	ctx.shader.use()
	ctx.shader.set_color('color', c)
	ctx.shader.set_int('has_texture', 1)
	vertices := [
	x1, y1, 0, 0, 0, 0, 1, 1,
	x2, y2, 0, 0, 0, 0, 1, 0,
	x3, y3, 0, 0, 0, 0, 0, 0,
	] !
	gl.bind_vao(ctx.VAO)
	gl.set_vbo(ctx.VBO, vertices, GL_STATIC_DRAW)
	// position attribute
	gl.vertex_attrib_pointer(0, 3, GL_FLOAT, false, 3, 0)
	gl.enable_vertex_attrib_array(0)
	// color attribute
	gl.vertex_attrib_pointer(1, 3, GL_FLOAT, false, 8, 3)
	gl.enable_vertex_attrib_array(1)
	// texture attribute
	gl.vertex_attrib_pointer(2, 2, GL_FLOAT, false, 8, 6)
	gl.enable_vertex_attrib_array(2)
	// /
	// gl.draw_arrays(GL_TRIANGLES, 0, 3)
	gl.draw_elements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0)
}

fn (ctx &GG) draw_rect(x, y, w, h float, c gx.Color) {
	// println('gg.draw_rect($x,$y,$w,$h)')
	// wrong order
	// // ctx.draw_triangle(x, y, x + w, y, x + w, y + h, c)
	// // ctx.draw_triangle(x, y, x, y + h, x + w, y + h, c)
	// good order. counter clock wise
	// ctx.draw_triangle(x, y, x, y + h, x + w, y + h, c)
	// ctx.draw_triangle(x, y, x + w, y + h, x + w, y, c)
	ctx.draw_rect2(x, y, w, h, c)
}

/* 
fn (ctx mut GG) init_rect_vao() {
 
	ctx.rect_vao = gl.gen_vertex_array()
	ctx.rect_vbo = gl.gen_buffer()
	vertices := [
	x + w, y, 0,
	x + w, y + h, 0,
	x, y + h, 0,
	x, y, 0,
	] !
	indices := [
	0, 1, 3,// first triangle
	1, 2, 3// second triangle
	] !
	gl.bind_vao(ctx.rect_vao)
	gl.set_vbo(ctx.rect_vbo, vertices, GL_STATIC_DRAW)
	ebo := gl.gen_buffer()
	// ///////
	gl.set_ebo(ebo, indices, GL_STATIC_DRAW)
} 
*/
fn (ctx &GG) draw_rect2(x, y, w, h float, c gx.Color) {
	C.glDeleteBuffers(1, &ctx.VAO)
	C.glDeleteBuffers(1, &ctx.VBO)
	ctx.shader.use()
	ctx.shader.set_color('color', c)
	ctx.shader.set_int('has_texture', 0)
	// 4--1
	// 3--2
	$if linux { 
	// y += h
	} 
	vertices := [
	x + w, y, 0,
	x + w, y + h, 0,
	x, y + h, 0,
	x, y, 0,
	] !
	indices := [
	0, 1, 3,// first triangle
	1, 2, 3// second triangle
	] !
	gl.bind_vao(ctx.VAO)
	gl.set_vbo(ctx.VBO, vertices, GL_STATIC_DRAW)
	ebo := gl.gen_buffer()
	// ///////
	gl.set_ebo(ebo, indices, GL_STATIC_DRAW)// !!! LEAKS
	// /////
	gl.vertex_attrib_pointer(0, 3, GL_FLOAT, false, 3, 0)
	gl.enable_vertex_attrib_array(0)
	// gl.bind_vao(ctx.rect_vao)
	gl.bind_vao(ctx.VAO)
	gl.draw_elements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0)
	C.glDeleteBuffers(1, &ebo)
}

// jfn ft_load_char(face FT_Face, code FT_ULong) Character {
// fn ft_load_char(_face voidptr, _code voidptr) Character {
fn ft_load_char(_face Face, code long) Character {
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

fn new_context_text(cfg Cfg, scale int) *GG {
	// Can only have text in ortho mode
	if !cfg.use_ortho {
		return &GG{text_ctx: 0}
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
	// gl.enable(GL_CULL_FACE) // TODO NEED CULL? MEANS SHIT IS BROKEN?
	gl.enable(GL_BLEND)
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
	mut font_path := 'RobotoMono-Regular.ttf'
	if !os.file_exists(font_path) {
		font_path = '/var/tmp/RobotoMono-Regular.ttf'
	}
	if !os.file_exists(font_path) {
		println('failed to load RobotoMono-Regular.ttf')
		exit(1)
	}
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
	mut chars := []gg.Character{}
	f := Face {
		cobj: 0
		kek: 0
	}
	# f.cobj = &face;
	// # for (GLubyte c = 0; c < 128; c++)
	for c := 0; c < 128; c++ {
		// ch := Character{}
		// ch:=ft_load_char(face, c)
		// # ch =gg__ft_load_char(&face, &c);
		// ////////////////////////////////
		mut ch := ft_load_char(f, long(c))
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
	VAO := gl.gen_vertex_array()
	println('new gg text context VAO=$VAO')
	VBO := gl.gen_buffer()
	gl.bind_vao(VAO)
	gl.bind_buffer(GL_ARRAY_BUFFER, VBO)
	// # glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * 6 * 4, NULL, GL_DYNAMIC_DRAW);
	gl.enable_vertex_attrib_array(0)
	gl.vertex_attrib_pointer(0, 4, GL_FLOAT, false, 4, 0)
	// # glVertexAttribPointer(0, 4, GL_FLOAT,false, 4 * sizeof(GLfloat), 0);
	// gl.bind_buffer(GL_ARRAY_BUFFER, uint(0))
	// # glBindVertexArray(0);
	mut ctx := &GG {
		shader: shader,
		width: width,
		height: height,
		scale: scale
		VAO: VAO,
		VBO: VBO,
		chars: chars,
		face: f
		text_ctx: 0
	}
	ctx.init_utf8_runes()
	return ctx
}

// A dirty hack to implement rendering of cyrillic letters.
// All UTF-8 must be supported. 
fn (ctx mut GG) init_utf8_runes() {
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

// fn (ctx &GG) render_text(text string, x, y, scale float, color gx.Color) {
pub fn (ctx &GG) draw_text(_x, _y int, text string, cfg gx.TextCfg) {
	// dont draw non ascii for now
	/* 
	for i := 0; i < text.len; i++ {
		c := text[i]
		if int(c) > 128 {
			// ctx.text_ctx._draw_text(_x, _y, '[NON ASCII]', cfg)
			// return
		}
	}
*/
	// # glScissor(0,0,300,300);
	utext := text.ustring_tmp()
	// utext := text.ustring()
	ctx.text_ctx._draw_text(_x, _y, utext, cfg)
	// utext.free()
	// # glScissor(0,0,ctx->width*2,ctx->height*2);
	// gl.disable(GL_SCISSOR_TEST)// TODO
	// #free(text.str);
}

fn (ctx &GG) draw_text_fast(_x, _y int, text ustring, cfg gx.TextCfg) {
	ctx.text_ctx._draw_text(_x, _y, text, cfg)
}

// TODO  HACK with second text context
// fn (ctx &GG) _draw_text(_x, _y int, text string, cfg gx.TextCfg) {
fn (ctx &GG) _draw_text(_x, _y int, utext ustring, cfg gx.TextCfg) {
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
	// println('scale=$ctx.scale size=$cfg.size')
	if cfg.align == gx.ALIGN_RIGHT {
		width := utext.len * 7
		_x -= width + 10
	}
	x := float(_x) * ctx.scale// float(2)
	// println('y=$_y height=$ctx.height')
	// _y = _y * int(ctx.scale) //+ 26
	_y = _y * int(ctx.scale) + ((cfg.size * ctx.scale) / 2) + 5 * ctx.scale
	y := float(ctx.height - _y)
	color := cfg.color
	// Activate corresponding render state
	ctx.shader.use()
	ctx.shader.set_color('textColor', color)
	# glActiveTexture(GL_TEXTURE0);
	gl.bind_vao(ctx.VAO)
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
				if rune_j.eq(_rune) {
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
		xpos := x + float(ch.bearing.x) * 1
		ypos := y - float(ch.size.y - ch.bearing.y) * 1
		w := float(ch.size.x) * 1
		h := float(ch.size.y) * 1
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
		gl.bind_buffer(GL_ARRAY_BUFFER, ctx.VBO)
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

fn (ctx &GG) draw_text_def(x, y int, text string) {
	cfg := gx.TextCfg {
		color: gx.BLACK,
		size: DEFAULT_FONT_SIZE,
		align: gx.ALIGN_LEFT,
	}
	ctx.draw_text(x, y, text, cfg)
}

fn update() {
	// # ui__post_empty_event();
}

pub fn (c GG) circle(x, y, r int) {
}

fn (c GG) fill_color(color gx.Color) {
}

fn (c GG) fill() {
}

fn (c GG) move_to(x, y int) {
}

fn (c GG) line_to(x, y int) {
}

fn (c GG) stroke_width(size int) {
}

fn (c GG) stroke_color(color gx.Color) {
}

fn (c GG) stroke() {
}

fn (c GG) save() {
}

fn (c GG) restore() {
}

fn (c GG) intersect_scissor(x, y, w, h int) {
}

fn (c GG) translate(x, y int) {
}

fn (c GG) create_font(name, file string) int {
	return 0
}

fn (c GG) text(x, y int, text string) {
}

fn (c GG) text_box(x, y, max int, text string) {
}

fn (c GG) font_face(f string) {
}

fn (c GG) font_size(size int) {
}

fn (c GG) text_align(a int) {
}

pub fn create_image(file string) u32 {
	println('gg create image "$file"')
	if file.contains('twitch') {
		return u32(0)// TODO
	}
	if !os.file_exists(file) {
		println('gg create image no such file "$file"')
		return u32(0)
	}
	texture := gl.gen_texture()
	img := stbi.load(file)
	gl.bind_2d_texture(texture)
	img.tex_image_2d()
	gl.generate_mipmap(GL_TEXTURE_2D)
	img.free()
	// println('gg end')
	return texture
}

pub fn (ctx &GG) draw_line_c(x, y, x2, y2 int, color gx.Color) {
	C.glDeleteBuffers(1, &ctx.VAO)
	C.glDeleteBuffers(1, &ctx.VBO)
	ctx.shader.use()
	ctx.shader.set_color('color', color)
	vertices := [float(x), float(y), float(x2), float(y2)] !
	gl.bind_vao(ctx.VAO)
	gl.set_vbo(ctx.VBO, vertices, GL_STATIC_DRAW)
	gl.vertex_attrib_pointer(0, 2, GL_FLOAT, false, 2, 0)
	gl.enable_vertex_attrib_array(0)
	gl.bind_vao(ctx.VAO)
	gl.draw_arrays(GL_LINES, 0, 2)
}

pub fn (c &GG) draw_line(x, y, x2, y2 int) {
	c.draw_line_c(x, y, x2, y2, gx.GRAY)
}

pub fn (c &GG) draw_vertical(x, y, height int) {
	c.draw_line(x, y, x, y + height)
}

// fn (ctx &GG) draw_image(x, y, w, h float, img stbi.Image) {
pub fn (ctx &GG) draw_image(x, y, w, h float, tex_id u32) {
	// println('DRAW IMAGE $x $y $w $h $tex_id')
	ctx.shader.use()
	// ctx.shader.set_color('color', c)
	ctx.shader.set_int('has_texture', 1)
	// 4--1
	// |  |
	// 3--2
	vertices := [
	x + w, y, 0, 1, 0, 0, 1, 1,
	x + w, y + h, 0, 0, 1, 0, 1, 0,
	x, y + h, 0, 0, 0, 1, 0, 0,
	x, y, 0, 1, 1, 0, 0, 1,
	] !
	indices := [
	0, 1, 3,// first triangle
	1, 2, 3// second triangle
	] !
	// VAO := gl.gen_vertex_array()
	// VBO := gl.gen_buffer()
	gl.bind_vao(ctx.VAO)
	gl.set_vbo(ctx.VBO, vertices, GL_STATIC_DRAW)
	ebo := gl.gen_buffer()
	gl.set_ebo(ebo, indices, GL_STATIC_DRAW)
	gl.vertex_attrib_pointer(0, 3, GL_FLOAT, false, 8, 0)
	gl.enable_vertex_attrib_array(0)
	gl.vertex_attrib_pointer(1, 3, GL_FLOAT, false, 8, 3)
	gl.enable_vertex_attrib_array(1)
	gl.vertex_attrib_pointer(2, 2, GL_FLOAT, false, 8, 6)
	gl.enable_vertex_attrib_array(2)
	gl.bind_2d_texture(u32(tex_id))
	gl.bind_vao(ctx.VAO)
	gl.draw_elements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0)
}

pub fn (c &GG) draw_empty_rect(x, y, w, h int, color gx.Color) {
	c.draw_line_c(x, y, x + w, y, color)
	c.draw_line_c(x, y, x, y + h, color)
	c.draw_line_c(x, y + h, x + w, y + h, color)
	c.draw_line_c(x + w, y, x + w, y + h, color)
}

