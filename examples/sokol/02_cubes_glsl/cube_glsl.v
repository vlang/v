/**********************************************************************
*
* Sokol 3d cube demo
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* HOW TO COMPILE SHADERS:
* Run `v shader .` in this directory to compile the shaders.
* For more info and help with shader compilation see `docs.md` and `v help shader`.
*
* TODO:
* - add instancing
**********************************************************************/
import gg
import gx
// import math
import sokol.sapp
import sokol.gfx
import sokol.sgl
import time
import gg.m4

// GLSL Include and functions
#flag -I @VMODROOT/.
#include "cube_glsl.h" # Should be generated with `v shader .` (see the instructions at the top of this file)

fn C.cube_shader_desc(gfx.Backend) &gfx.ShaderDesc

const (
	win_width  = 800
	win_height = 800
	bg_color   = gx.white
)

struct App {
mut:
	gg          &gg.Context
	pip_3d      sgl.Pipeline
	texture     gfx.Image
	init_flag   bool
	frame_count int
	mouse_x     int = -1
	mouse_y     int = -1
	// glsl
	cube_pip_glsl gfx.Pipeline
	cube_bind     gfx.Bindings
	// time
	ticks i64
}

/******************************************************************************
*
* Texture functions
*
******************************************************************************/
fn create_texture(w int, h int, buf &byte) gfx.Image {
	sz := w * h * 4
	mut img_desc := gfx.ImageDesc{
		width: w
		height: h
		num_mipmaps: 0
		min_filter: .linear
		mag_filter: .linear
		// usage: .dynamic
		wrap_u: .clamp_to_edge
		wrap_v: .clamp_to_edge
		label: &u8(0)
		d3d11_texture: 0
	}
	// comment if .dynamic is enabled
	img_desc.data.subimage[0][0] = gfx.Range{
		ptr: buf
		size: usize(sz)
	}

	sg_img := gfx.make_image(&img_desc)
	return sg_img
}

fn destroy_texture(sg_img gfx.Image) {
	gfx.destroy_image(sg_img)
}

// Use only if usage: .dynamic is enabled
fn update_text_texture(sg_img gfx.Image, w int, h int, buf &byte) {
	sz := w * h * 4
	mut tmp_sbc := gfx.ImageData{}
	tmp_sbc.subimage[0][0] = gfx.Range{
		ptr: buf
		size: usize(sz)
	}
	gfx.update_image(sg_img, &tmp_sbc)
}

/******************************************************************************
*
* Draw functions
*
******************************************************************************/
fn draw_triangle() {
	sgl.defaults()
	sgl.begin_triangles()
	sgl.v2f_c3b( 0.0,  0.5, 255, 0  , 0  )
	sgl.v2f_c3b(-0.5, -0.5,   0, 0  , 255)
	sgl.v2f_c3b( 0.5, -0.5,   0, 255, 0  )
	sgl.end()
}

// vertex specification for a cube with colored sides and texture coords
fn cube() {
	sgl.begin_quads()
	// edge color
	sgl.c3f(1.0, 0.0, 0.0)
	// edge coord
	// x,y,z, texture cord: u,v
	sgl.v3f_t2f(-1.0,  1.0, -1.0, -1.0,  1.0)
	sgl.v3f_t2f( 1.0,  1.0, -1.0,  1.0,  1.0)
	sgl.v3f_t2f( 1.0, -1.0, -1.0,  1.0, -1.0)
	sgl.v3f_t2f(-1.0, -1.0, -1.0, -1.0, -1.0)
	sgl.c3f(0.0, 1.0, 0.0)
	sgl.v3f_t2f(-1.0, -1.0,  1.0, -1.0,  1.0)
	sgl.v3f_t2f( 1.0, -1.0,  1.0,  1.0,  1.0)
	sgl.v3f_t2f( 1.0,  1.0,  1.0,  1.0, -1.0)
	sgl.v3f_t2f(-1.0,  1.0,  1.0, -1.0, -1.0)
	sgl.c3f(0.0, 0.0, 1.0)
	sgl.v3f_t2f(-1.0, -1.0,  1.0, -1.0,  1.0)
	sgl.v3f_t2f(-1.0,  1.0,  1.0,  1.0,  1.0)
	sgl.v3f_t2f(-1.0,  1.0, -1.0,  1.0, -1.0)
	sgl.v3f_t2f(-1.0, -1.0, -1.0, -1.0, -1.0)
	sgl.c3f(1.0, 0.5, 0.0)
	sgl.v3f_t2f(1.0, -1.0,  1.0, -1.0,   1.0)
	sgl.v3f_t2f(1.0, -1.0, -1.0,  1.0,   1.0)
	sgl.v3f_t2f(1.0,  1.0, -1.0,  1.0,  -1.0)
	sgl.v3f_t2f(1.0,  1.0,  1.0, -1.0,  -1.0)
	sgl.c3f(0.0, 0.5, 1.0)
	sgl.v3f_t2f( 1.0, -1.0, -1.0, -1.0,  1.0)
	sgl.v3f_t2f( 1.0, -1.0,  1.0,  1.0,  1.0)
	sgl.v3f_t2f(-1.0, -1.0,  1.0,  1.0, -1.0)
	sgl.v3f_t2f(-1.0, -1.0, -1.0, -1.0, -1.0)
	sgl.c3f(1.0, 0.0, 0.5)
	sgl.v3f_t2f(-1.0,  1.0, -1.0, -1.0,  1.0)
	sgl.v3f_t2f(-1.0,  1.0,  1.0,  1.0,  1.0)
	sgl.v3f_t2f( 1.0,  1.0,  1.0,  1.0, -1.0)
	sgl.v3f_t2f( 1.0,  1.0, -1.0, -1.0, -1.0)
	sgl.end()
}

fn draw_cubes(app App) {
	rot := [f32(1.0) * (app.frame_count % 360), 0.5 * f32(app.frame_count % 360)]
	// rot := [f32(app.mouse_x), f32(app.mouse_y)]

	sgl.defaults()
	sgl.load_pipeline(app.pip_3d)

	sgl.matrix_mode_projection()
	sgl.perspective(sgl.rad(45.0), 1.0, 0.1, 100.0)

	sgl.matrix_mode_modelview()
	sgl.translate(0.0, 0.0, -12.0)
	sgl.rotate(sgl.rad(rot[0]), 1.0, 0.0, 0.0)
	sgl.rotate(sgl.rad(rot[1]), 0.0, 1.0, 0.0)
	cube()
	sgl.push_matrix()
	sgl.translate(0.0, 0.0, 3.0)
	sgl.scale(0.5, 0.5, 0.5)
	sgl.rotate(-2.0 * sgl.rad(rot[0]), 1.0, 0.0, 0.0)
	sgl.rotate(-2.0 * sgl.rad(rot[1]), 0.0, 1.0, 0.0)
	cube()
	sgl.push_matrix()
	sgl.translate(0.0, 0.0, 3.0)
	sgl.scale(0.5, 0.5, 0.5)
	sgl.rotate(-3.0 * sgl.rad(2 * rot[0]), 1.0, 0.0, 0.0)
	sgl.rotate( 3.0 * sgl.rad(2 * rot[1]), 0.0, 0.0, 1.0)
	cube()
	sgl.pop_matrix()
	sgl.pop_matrix()
}

fn cube_texture(r f32, g f32, b f32) {
	sgl.begin_quads()
	// edge color
	sgl.c3f(r, g, b)
	// edge coord
	// x,y,z, texture cord: u,v
	sgl.v3f_t2f(-1.0,  1.0, -1.0,  0.0 , 0.25)
	sgl.v3f_t2f( 1.0,  1.0, -1.0,  0.25, 0.25)
	sgl.v3f_t2f( 1.0, -1.0, -1.0,  0.25, 0.0 )
	sgl.v3f_t2f(-1.0, -1.0, -1.0,  0.0 , 0.0 )
	sgl.c3f(r, g, b)
	sgl.v3f_t2f(-1.0, -1.0,  1.0,  0.0 , 0.25)
	sgl.v3f_t2f( 1.0, -1.0,  1.0,  0.25, 0.25)
	sgl.v3f_t2f( 1.0,  1.0,  1.0,  0.25, 0.0 )
	sgl.v3f_t2f(-1.0,  1.0,  1.0,  0.0 , 0.0 )
	sgl.c3f(r, g, b)
	sgl.v3f_t2f(-1.0, -1.0,  1.0,  0.0 , 0.25)
	sgl.v3f_t2f(-1.0,  1.0,  1.0,  0.25, 0.25)
	sgl.v3f_t2f(-1.0,  1.0, -1.0,  0.25, 0.0 )
	sgl.v3f_t2f(-1.0, -1.0, -1.0,  0.0 , 0.0 )
	sgl.c3f(r, g, b)
	sgl.v3f_t2f(1.0, -1.0,  1.0,  0.0 , 0.25)
	sgl.v3f_t2f(1.0, -1.0, -1.0,  0.25, 0.25)
	sgl.v3f_t2f(1.0,  1.0, -1.0,  0.25, 0.0 )
	sgl.v3f_t2f(1.0,  1.0,  1.0,  0.0 , 0.0 )
	sgl.c3f(r, g, b)
	sgl.v3f_t2f( 1.0, -1.0, -1.0, 0.0 , 0.25)
	sgl.v3f_t2f( 1.0, -1.0,  1.0, 0.25, 0.25)
	sgl.v3f_t2f(-1.0, -1.0,  1.0, 0.25, 0.0 )
	sgl.v3f_t2f(-1.0, -1.0, -1.0, 0.0 , 0.0 )
	sgl.c3f(r, g, b)
	sgl.v3f_t2f(-1.0,  1.0, -1.0,  0.0 , 0.25)
	sgl.v3f_t2f(-1.0,  1.0,  1.0,  0.25, 0.25)
	sgl.v3f_t2f( 1.0,  1.0,  1.0,  0.25, 0.0 )
	sgl.v3f_t2f( 1.0,  1.0, -1.0,  0.0 , 0.0 )
	sgl.end()
}

/*
Cube vertex buffer with packed vertex formats for color and texture coords.
		Note that a vertex format which must be portable across all
		backends must only use the normalized integer formats
		(BYTE4N, UBYTE4N, SHORT2N, SHORT4N), which can be converted
		to floating point formats in the vertex shader inputs.
		The reason is that D3D11 cannot convert from non-normalized
		formats to floating point inputs (only to integer inputs),
		and WebGL2 / GLES2 don't support integer vertex shader inputs.
*/

struct Vertex_t {
	x     f32
	y     f32
	z     f32
	color u32
	// u u16
	// v u16
	u     f32
	v     f32
}

fn init_cube_glsl(mut app App) {
	// cube vertex buffer
	// d := u16(32767/8)       // for compatibility with D3D11, 32767 stand for 1
	d := f32(1.0) // 0.05)
	c := u32(0xFFFFFF_FF) // color RGBA8
	vertices := [
		// Face 0
		Vertex_t{-1.0, -1.0, -1.0, c,  0, 0},
		Vertex_t{ 1.0, -1.0, -1.0, c,  d, 0},
		Vertex_t{ 1.0,  1.0, -1.0, c,  d, d},
		Vertex_t{-1.0,  1.0, -1.0, c,  0, d},
		// Face 1
		Vertex_t{-1.0, -1.0,  1.0, c,  0, 0},
		Vertex_t{ 1.0, -1.0,  1.0, c,  d, 0},
		Vertex_t{ 1.0,  1.0,  1.0, c,  d, d},
		Vertex_t{-1.0,  1.0,  1.0, c,  0, d},
		// Face 2
		Vertex_t{-1.0, -1.0, -1.0, c,  0, 0},
		Vertex_t{-1.0,  1.0, -1.0, c,  d, 0},
		Vertex_t{-1.0,  1.0,  1.0, c,  d, d},
		Vertex_t{-1.0, -1.0,  1.0, c,  0, d},
		// Face 3
		Vertex_t{ 1.0, -1.0, -1.0, c,  0, 0},
		Vertex_t{ 1.0,  1.0, -1.0, c,  d, 0},
		Vertex_t{ 1.0,  1.0,  1.0, c,  d, d},
		Vertex_t{ 1.0, -1.0,  1.0, c,  0, d},
		// Face 4
		Vertex_t{-1.0, -1.0, -1.0, c,  0, 0},
		Vertex_t{-1.0, -1.0,  1.0, c,  d, 0},
		Vertex_t{ 1.0, -1.0,  1.0, c,  d, d},
		Vertex_t{ 1.0, -1.0, -1.0, c,  0, d},
		// Face 5
		Vertex_t{-1.0,  1.0, -1.0, c,  0, 0},
		Vertex_t{-1.0,  1.0,  1.0, c,  d, 0},
		Vertex_t{ 1.0,  1.0,  1.0, c,  d, d},
		Vertex_t{ 1.0,  1.0, -1.0, c,  0, d},
	]

	mut vert_buffer_desc := gfx.BufferDesc{label: c'cube-vertices'}
	unsafe { vmemset(&vert_buffer_desc, 0, int(sizeof(vert_buffer_desc))) }

	vert_buffer_desc.size = usize(vertices.len * int(sizeof(Vertex_t)))
	vert_buffer_desc.data = gfx.Range{
		ptr: vertices.data
		size: usize(vertices.len * int(sizeof(Vertex_t)))
	}

	vert_buffer_desc.@type = .vertexbuffer
	// vert_buffer_desc.usage   = .immutable
	vbuf := gfx.make_buffer(&vert_buffer_desc)

	/* create an index buffer for the cube */
	indices := [
		u16(0), 1, 2,  0, 2, 3,
		6, 5, 4,       7, 6, 4,
		8, 9, 10,      8, 10, 11,
		14, 13, 12,    15, 14, 12,
		16, 17, 18,    16, 18, 19,
		22, 21, 20,    23, 22, 20
	]

	mut index_buffer_desc := gfx.BufferDesc{label: c'cube-indices'}
	unsafe { vmemset(&index_buffer_desc, 0, int(sizeof(index_buffer_desc))) }

	index_buffer_desc.size = usize(indices.len * int(sizeof(u16)))
	index_buffer_desc.data = gfx.Range{
		ptr: indices.data
		size: usize(indices.len * int(sizeof(u16)))
	}

	index_buffer_desc.@type = .indexbuffer
	ibuf := gfx.make_buffer(&index_buffer_desc)

	// create shader
	shader := gfx.make_shader(C.cube_shader_desc(C.sg_query_backend()))

	mut pipdesc := gfx.PipelineDesc{}
	unsafe { vmemset(&pipdesc, 0, int(sizeof(pipdesc))) }

	pipdesc.layout.buffers[0].stride = int(sizeof(Vertex_t))
	// the constants [C.ATTR_vs_pos, C.ATTR_vs_color0, C.ATTR_vs_texcoord0] are generated bysokol-shdc
	pipdesc.layout.attrs[C.ATTR_vs_pos      ].format = .float3  // x,y,z as f32
	pipdesc.layout.attrs[C.ATTR_vs_color0   ].format = .ubyte4n // color as u32
	pipdesc.layout.attrs[C.ATTR_vs_texcoord0].format = .float2  // u,v as f32
	// pipdesc.layout.attrs[C.ATTR_vs_texcoord0].format  = .short2n  // u,v as u16

	pipdesc.shader = shader
	pipdesc.index_type = .uint16

	pipdesc.depth = gfx.DepthState{
		write_enabled: true
		compare: .less_equal
	}
	pipdesc.cull_mode = .back

	pipdesc.label = 'glsl_shader pipeline'.str

	app.cube_bind.vertex_buffers[0] = vbuf
	app.cube_bind.index_buffer = ibuf
	app.cube_bind.fs_images[C.SLOT_tex] = app.texture
	app.cube_pip_glsl = gfx.make_pipeline(&pipdesc)
	println('GLSL init DONE!')
}

fn draw_cube_glsl(app App) {
	if app.init_flag == false {
		return
	}

	rot := [f32(app.mouse_y), f32(app.mouse_x)]

	ws := gg.window_size_real_pixels()
	// ratio := f32(ws.width)/ws.height
	dw := f32(ws.width / 2)
	dh := f32(ws.height / 2)

	tr_matrix := m4.calc_tr_matrices(dw, dh, rot[0], rot[1], 2.0)
	gfx.apply_viewport(ws.width / 2, 0, ws.width / 2, ws.height / 2, true)

	// apply the pipline and bindings
	gfx.apply_pipeline(app.cube_pip_glsl)
	gfx.apply_bindings(app.cube_bind)

	//***************
	// Uniforms
	//***************
	// passing the view matrix as uniform
	// res is a 4x4 matrix of f32 thus: 4*16 byte of size
	vs_uniforms_range := gfx.Range{
		ptr: &tr_matrix
		size: usize(4 * 16)
	}
	gfx.apply_uniforms(.vs, C.SLOT_vs_params, &vs_uniforms_range)

	// fs uniforms
	time_ticks := f32(time.ticks() - app.ticks) / 1000
	mut text_res := [
		f32(512),
		512, /* x,y resolution to pass to FS */
		time_ticks, /* time as f32 */
		0 /* padding 4 Bytes == 1 f32 */,
	]!
	fs_uniforms_range := gfx.Range{
		ptr: unsafe { &text_res }
		size: usize(4 * 4)
	}
	gfx.apply_uniforms(.fs, C.SLOT_fs_params, &fs_uniforms_range)

	gfx.draw(0, (3 * 2) * 6, 1)
	gfx.end_pass()
	gfx.commit()
}

fn draw_texture_cubes(app App) {
	rot := [f32(app.mouse_x), f32(app.mouse_y)]
	sgl.defaults()
	sgl.load_pipeline(app.pip_3d)

	sgl.enable_texture()
	sgl.texture(app.texture)

	sgl.matrix_mode_projection()
	sgl.perspective(sgl.rad(45.0), 1.0, 0.1, 100.0)

	sgl.matrix_mode_modelview()
	sgl.translate(0.0, 0.0, -12.0)
	sgl.rotate(sgl.rad(rot[0]), 1.0, 0.0, 0.0)
	sgl.rotate(sgl.rad(rot[1]), 0.0, 1.0, 0.0)
	cube_texture(1, 1, 1)
	sgl.push_matrix()
		sgl.translate(0.0, 0.0, 3.0)
		sgl.scale(0.5, 0.5, 0.5)
		sgl.rotate(-2.0 * sgl.rad(rot[0]), 1.0, 0.0, 0.0)
		sgl.rotate(-2.0 * sgl.rad(rot[1]), 0.0, 1.0, 0.0)
		cube_texture(1,1,1)
		sgl.push_matrix()
			sgl.translate(0.0, 0.0, 3.0)
			sgl.scale(0.5, 0.5, 0.5)
			sgl.rotate(-3.0 * sgl.rad(2*rot[0]), 1.0, 0.0, 0.0)
			sgl.rotate(3.0 * sgl.rad(2*rot[1]), 0.0, 0.0, 1.0)
			cube_texture(1,1,1)
		sgl.pop_matrix()
	sgl.pop_matrix()

	sgl.disable_texture()
}

fn frame(mut app App) {
	ws := gg.window_size_real_pixels()
	ratio := f32(ws.width) / ws.height
	dw := ws.width
	dh := ws.height
	ww := int(dh / 3) // not a bug
	hh := int(dh / 3)
	x0 := int(f32(dw) * 0.05)
	// x1 := dw/2
	y0 := 0
	y1 := int(f32(dh) * 0.5)

	// app.gg.begin()

	app.gg.begin()
	sgl.defaults()

	// 2d triangle
	sgl.viewport(x0, y0, ww, hh, true)
	draw_triangle()

	// colored cubes with viewport
	sgl.viewport(x0, y1, ww, hh, true)
	draw_cubes(app)

	// textured cubed with viewport
	sgl.viewport(0, int(dh / 5), dw, int(dh * ratio), true)
	draw_texture_cubes(app)

	app.gg.end()

	// clear
	mut color_action := gfx.ColorAttachmentAction{
		action: gfx.Action(C.SG_ACTION_DONTCARE) // C.SG_ACTION_CLEAR)
		value: gfx.Color{
			r: 1.0
			g: 1.0
			b: 1.0
			a: 1.0
		}
	}
	mut pass_action := gfx.PassAction{}
	pass_action.colors[0] = color_action
	gfx.begin_default_pass(&pass_action, ws.width, ws.height)

	// glsl cube
	draw_cube_glsl(app)

	app.frame_count++
}

/******************************************************************************
*
* Init / Cleanup
*
******************************************************************************/
fn my_init(mut app App) {
	// set max vertices,
	// for a large number of the same type of object it is better use the instances!!
	desc := sapp.create_desc()
	gfx.setup(&desc)
	sgl_desc := sgl.Desc{
		max_vertices: 50 * 65536
	}
	sgl.setup(&sgl_desc)

	// 3d pipeline
	mut pipdesc := gfx.PipelineDesc{}
	unsafe { vmemset(&pipdesc, 0, int(sizeof(pipdesc))) }

	color_state := gfx.ColorState{
		blend: gfx.BlendState{
			enabled: true
			src_factor_rgb: .src_alpha
			dst_factor_rgb: .one_minus_src_alpha
		}
	}
	pipdesc.colors[0] = color_state

	pipdesc.depth = gfx.DepthState{
		write_enabled: true
		compare: .less_equal
	}
	pipdesc.cull_mode = .back

	app.pip_3d = sgl.make_pipeline(&pipdesc)

	// create chessboard texture 256*256 RGBA
	w := 256
	h := 256
	sz := w * h * 4
	tmp_txt := unsafe { malloc(sz) }
	mut i := 0
	for i < sz {
		unsafe {
			y := (i >> 0x8) >> 5 // 8 cell
			x := (i & 0xFF) >> 5 // 8 cell
			// upper left corner
			if x == 0 && y == 0 {
				tmp_txt[i] = u8(0xFF)
				tmp_txt[i + 1] = u8(0)
				tmp_txt[i + 2] = u8(0)
				tmp_txt[i + 3] = u8(0xFF)
			}
			// low right corner
			else if x == 7 && y == 7 {
				tmp_txt[i + 0] = u8(0)
				tmp_txt[i + 1] = u8(0xFF)
				tmp_txt[i + 2] = u8(0)
				tmp_txt[i + 3] = u8(0xFF)
			} else {
				col := if ((x + y) & 1) == 1 { 0xFF } else { 128 }
				tmp_txt[i + 0] = u8(col)  // red
				tmp_txt[i + 1] = u8(col)  // green
				tmp_txt[i + 2] = u8(col)  // blue
				tmp_txt[i + 3] = u8(0xFF) // alpha
			}
			i += 4
		}
	}
	app.texture = create_texture(w, h, tmp_txt)
	unsafe { free(tmp_txt) }

	// glsl
	init_cube_glsl(mut app)
	app.init_flag = true
}

/******************************************************************************
*
* event
*
******************************************************************************/
fn my_event_manager(mut ev gg.Event, mut app App) {
	if ev.typ == .mouse_move {
		app.mouse_x = int(ev.mouse_x)
		app.mouse_y = int(ev.mouse_y)
	}
	if ev.typ == .touches_began || ev.typ == .touches_moved {
		if ev.num_touches > 0 {
			touch_point := ev.touches[0]
			app.mouse_x = int(touch_point.pos_x)
			app.mouse_y = int(touch_point.pos_y)
		}
	}
}

/******************************************************************************
*
* Main
*
******************************************************************************/
[console] // is needed for easier diagnostics on windows
fn main() {
	// App init
	mut app := &App{
		gg: 0
	}

	mut a := [5]int{}
	a[0] = 2
	println(a)

	app.gg = gg.new_context(
		width: win_width
		height: win_height
		create_window: true
		window_title: '3D Cube Demo'
		user_data: app
		bg_color: bg_color
		frame_fn: frame
		init_fn: my_init
		event_fn: my_event_manager
	)

	app.ticks = time.ticks()
	app.gg.run()
}
