// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module gg

import stbi
import glm
import gl
import gx
import os
import glfw

struct Vec2 {
pub:
	x int
	y int
}

pub fn vec2(x, y int) Vec2 {
	res := Vec2 {
		x: x
		y: y
	}
	return res
}

pub fn init() {
	glfw.init()
	println(gl.TEXT_VERT)
	gl.init_glad()
}


struct Cfg {
pub:
	width     int
	height    int
	use_ortho bool
	retina    bool
	
	font_size int
	font_path string
	create_window bool
	window_user_ptr voidptr
	window_title string
	always_on_top bool
	scale int
}

struct GG {
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
	scale     int // retina = 2 , normal = 1
pub mut:
	window &glfw.Window
	render_fn fn()
}


// fn new_context(width, height int, use_ortho bool, font_size int) *GG {
pub fn new_context(cfg Cfg) &GG {
	mut window := &glfw.Window{!}
	if cfg.create_window {
		window = glfw.create_window(glfw.WinCfg{
			title: cfg.window_title
			width: cfg.width
			height: cfg.height
			ptr: cfg.window_user_ptr
			always_on_top: cfg.always_on_top
		})
		window.make_context_current()
		init()
	}
	shader := gl.new_shader('simple')
	shader.use()
	if cfg.use_ortho {
		projection := glm.ortho(0, cfg.width, cfg.height, 0)
		shader.set_mat4('projection', projection)
	}
	else {
		// TODO move to function (allow volt functions to return arrays without allocations)
		// i := glm.identity3()
		shader.set_mat4('projection', glm.identity())
	}
	vao := gl.gen_vertex_array()
	//println('new gg context VAO=$VAO')
	vbo := gl.gen_buffer()
	mut scale := 1
	if cfg.retina {
		scale = 2
	}
	//gl.enable(GL_BLEND)
	//# glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	//println('new gg text context VAO=$VAO')
	//gl.bind_vao(VAO)
	//gl.bind_buffer(GL_ARRAY_BUFFER, VBO)
	//gl.enable_vertex_attrib_array(0)
	//gl.vertex_attrib_pointer(0, 4, GL_FLOAT, false, 4, 0)
	todo_remove_me(cfg, scale)
	return &GG {
		shader: shader
		width: cfg.width
		height: cfg.height
		vao: vao
		vbo: vbo
		window: window
	
		// /line_vao: gl.gen_vertex_array()
		// /line_vbo: gl.gen_buffer()
		//text_ctx: new_context_text(cfg, scale),
		scale: scale
		// use_ortho: use_ortho
	}

	// ctx.init_rect_vao()
	//return ctx
}

/*
pub fn (gg &GG) render_loop() bool {
	for !gg.window.show_close() {
		gg.render_fn()
		gg.window.swap_buffers()
		glfw.wait_events()
	}
}
*/

pub fn clear(color gx.Color) {
	gl.clear()
	gl.clear_color(255, 255, 255, 255)
}

pub fn (gg &GG) render() {
	gg.window.swap_buffers()
	glfw.wait_events()
}

pub fn (ctx &GG) draw_triangle(x1, y1, x2, y2, x3, y3 f32, c gx.Color) {
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
	gl.bind_vao(ctx.vao)
	gl.set_vbo(ctx.vbo, vertices, C.GL_STATIC_DRAW)
	gl.vertex_attrib_pointer(0, 3, C.GL_FLOAT, false, 3, 0)
	gl.enable_vertex_attrib_array(0)
	// gl.bind_buffer(GL_ARRAY_BUFFER, uint(0))
	// You can unbind the VAO afterwards so other VAO calls won't accidentally modify this VAO,
	// but this rarely happens. Modifying other
	// VAOs requires a call to glBindVertexArray anyways so we generally don't unbind VAOs
	// (nor VBOs) when it's not directly necessary.
	// gl.bind_vertex_array(uint(0))
	// gl.bind_vertex_array(ctx.VAO)
	gl.draw_arrays(C.GL_TRIANGLES, 0, 3)
}

pub fn (ctx &GG) draw_triangle_tex(x1, y1, x2, y2, x3, y3 f32, c gx.Color) {
	ctx.shader.use()
	ctx.shader.set_color('color', c)
	ctx.shader.set_int('has_texture', 1)
	vertices := [
	x1, y1, 0, 0, 0, 0, 1, 1,
	x2, y2, 0, 0, 0, 0, 1, 0,
	x3, y3, 0, 0, 0, 0, 0, 0,
	] !
	gl.bind_vao(ctx.vao)
	gl.set_vbo(ctx.vbo, vertices, C.GL_STATIC_DRAW)
	// position attribute
	gl.vertex_attrib_pointer(0, 3, C.GL_FLOAT, false, 3, 0)
	gl.enable_vertex_attrib_array(0)
	// color attribute
	gl.vertex_attrib_pointer(1, 3, C.GL_FLOAT, false, 8, 3)
	gl.enable_vertex_attrib_array(1)
	// texture attribute
	gl.vertex_attrib_pointer(2, 2, C.GL_FLOAT, false, 8, 6)
	gl.enable_vertex_attrib_array(2)
	// /
	// gl.draw_arrays(GL_TRIANGLES, 0, 3)
	gl.draw_elements(C.GL_TRIANGLES, 6, C.GL_UNSIGNED_INT, 0)
}

pub fn (ctx &GG) draw_rect(x, y, w, h f32, c gx.Color) {
	// println('gg.draw_rect($x,$y,$w,$h)')
	// wrong order
	// // ctx.draw_triangle(x, y, x + w, y, x + w, y + h, c)
	// // ctx.draw_triangle(x, y, x, y + h, x + w, y + h, c)
	// good order. counter clockwise
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
	gl.set_vbo(ctx.rect_vbo, vertices, C.GL_STATIC_DRAW)
	ebo := gl.gen_buffer()
	// ///////
	gl.set_ebo(ebo, indices, C.GL_STATIC_DRAW)
}
*/
pub fn (ctx &GG) draw_rect2(x, y, w, h f32, c gx.Color) {
	C.glDeleteBuffers(1, &ctx.vao)
	C.glDeleteBuffers(1, &ctx.vbo)
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
	gl.bind_vao(ctx.vao)
	gl.set_vbo(ctx.vbo, vertices, C.GL_STATIC_DRAW)
	ebo := gl.gen_buffer()
	// ///////
	gl.set_ebo(ebo, indices, C.GL_STATIC_DRAW)// !!! LEAKS
	// /////
	gl.vertex_attrib_pointer(0, 3, C.GL_FLOAT, false, 3, 0)
	gl.enable_vertex_attrib_array(0)
	// gl.bind_vao(ctx.rect_vao)
	gl.bind_vao(ctx.vao)
	gl.draw_elements(C.GL_TRIANGLES, 6, C.GL_UNSIGNED_INT, 0)
	C.glDeleteBuffers(1, &ebo)
}

fn todo_remove_me(cfg Cfg, scale int) {
	// Can only have text in ortho mode
	if !cfg.use_ortho {
		return
	}
	mut width := cfg.width * scale
	mut height := cfg.height * scale
	font_size := cfg.font_size * scale
	gl.enable(C.GL_BLEND)
	//# glBlendFunc(C.GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	shader := gl.new_shader('text')
	shader.use()
	projection := glm.ortho(0, width, 0, height)// 0 at BOT
	// projection_new := ortho(0, width, 0, height)// 0 at BOT
	// projection := gl.ortho(0, width,height,0)  // 0 at TOP
	shader.set_mat4('projection', projection)
	vao := gl.gen_vertex_array()
	//println('new gg text context VAO=$VAO')
	vbo := gl.gen_buffer()
	gl.bind_vao(vao)
	gl.bind_buffer(C.GL_ARRAY_BUFFER, vbo)
	gl.enable_vertex_attrib_array(0)
	gl.vertex_attrib_pointer(0, 4, C.GL_FLOAT, false, 4, 0)
}

fn update() {
	// # ui__post_empty_event();
}

pub fn post_empty_event() {
	glfw.post_empty_event()
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
	gl.generate_mipmap(C.GL_TEXTURE_2D)
	img.free()
	// println('gg end')
	return texture
}

pub fn (ctx &GG) draw_line_c(x, y, x2, y2 f32, color gx.Color) {
	C.glDeleteBuffers(1, &ctx.vao)
	C.glDeleteBuffers(1, &ctx.vbo)
	ctx.shader.use()
	ctx.shader.set_color('color', color)
	vertices := [f32(x), f32(y), f32(x2), f32(y2)] !
	gl.bind_vao(ctx.vao)
	gl.set_vbo(ctx.vbo, vertices, C.GL_STATIC_DRAW)
	gl.vertex_attrib_pointer(0, 2, C.GL_FLOAT, false, 2, 0)
	gl.enable_vertex_attrib_array(0)
	gl.bind_vao(ctx.vao)
	gl.draw_arrays(C.GL_LINES, 0, 2)
}

pub fn (c &GG) draw_line(x, y, x2, y2 f32) {
	c.draw_line_c(x, y, x2, y2, gx.Gray)
}

pub fn (c &GG) draw_vertical(x, y, height int) {
	c.draw_line(x, y, x, y + height)
}


//ctx.gg.draw_line(center + prev_x, center+prev_y, center + x*10.0, center+y)

// fn (ctx &GG) draw_image(x, y, w, h f32, img stbi.Image) {
pub fn (ctx &GG) draw_image(x, y, w, h f32, tex_id u32) {
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
	gl.bind_vao(ctx.vao)
	gl.set_vbo(ctx.vbo, vertices, C.GL_STATIC_DRAW)
	ebo := gl.gen_buffer()
	gl.set_ebo(ebo, indices, C.GL_STATIC_DRAW)
	gl.vertex_attrib_pointer(0, 3, C.GL_FLOAT, false, 8, 0)
	gl.enable_vertex_attrib_array(0)
	gl.vertex_attrib_pointer(1, 3, C.GL_FLOAT, false, 8, 3)
	gl.enable_vertex_attrib_array(1)
	gl.vertex_attrib_pointer(2, 2, C.GL_FLOAT, false, 8, 6)
	gl.enable_vertex_attrib_array(2)
	gl.bind_2d_texture(u32(tex_id))
	gl.bind_vao(ctx.vao)
	gl.draw_elements(C.GL_TRIANGLES, 6, C.GL_UNSIGNED_INT, 0)
}

pub fn (c &GG) draw_empty_rect(x, y, w, h int, color gx.Color) {
	c.draw_line_c(x, y, x + w, y, color)
	c.draw_line_c(x, y, x, y + h, color)
	c.draw_line_c(x, y + h, x + w, y + h, color)
	c.draw_line_c(x + w, y, x + w, y + h, color)
}

