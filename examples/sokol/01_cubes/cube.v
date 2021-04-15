/**********************************************************************
*
* Sokol 3d cube demo
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* TODO:
* - add instancing
* - add an exampel with shaders
**********************************************************************/
import gg
import gx
import math
import sokol.sapp
import sokol.gfx
import sokol.sgl

const (
	win_width  = 800
	win_height = 800
	bg_color   = gx.white
)

struct App {
mut:
	gg          &gg.Context
	pip_3d      C.sgl_pipeline
	texture     C.sg_image
	init_flag   bool
	frame_count int
	mouse_x     int = -1
	mouse_y     int = -1
}

/******************************************************************************
*
* Texture functions
*
******************************************************************************/
fn create_texture(w int, h int, buf &u8) C.sg_image {
	sz := w * h * 4
	mut img_desc := C.sg_image_desc{
		width: w
		height: h
		num_mipmaps: 0
		min_filter: .linear
		mag_filter: .linear
		// usage: .dynamic
		wrap_u: .clamp_to_edge
		wrap_v: .clamp_to_edge
		label: &byte(0)
		d3d11_texture: 0
	}
	// commen if .dynamic is enabled
	img_desc.data.subimage[0][0] = C.sg_range{
		ptr: buf
		size: size_t(sz)
	}

	sg_img := C.sg_make_image(&img_desc)
	return sg_img
}

fn destroy_texture(sg_img C.sg_image) {
	C.sg_destroy_image(sg_img)
}

// Use only if usage: .dynamic is enabled
fn update_text_texture(sg_img C.sg_image, w int, h int, buf &byte) {
	sz := w * h * 4
	mut tmp_sbc := C.sg_image_data{}
	tmp_sbc.subimage[0][0] = C.sg_range{
		ptr: buf
		size: size_t(sz)
	}
	C.sg_update_image(sg_img, &tmp_sbc)
}

/******************************************************************************
*
* Draw functions
*
******************************************************************************/
fn draw_triangle() {
	sgl.defaults()
	sgl.begin_triangles()
	sgl.v2f_c3b(0.0, 0.5, 255, 0, 0)
	sgl.v2f_c3b(-0.5, -0.5, 0, 0, 255)
	sgl.v2f_c3b(0.5, -0.5, 0, 255, 0)
	sgl.end()
}

// vertex specification for a cube with colored sides and texture coords
fn cube() {
	sgl.begin_quads()
	// edge color
	sgl.c3f(1.0, 0.0, 0.0)
	// edge coord
	// x,y,z, texture cord: u,v
	sgl.v3f_t2f(-1.0, 1.0, -1.0, -1.0, 1.0)
	sgl.v3f_t2f(1.0, 1.0, -1.0, 1.0, 1.0)
	sgl.v3f_t2f(1.0, -1.0, -1.0, 1.0, -1.0)
	sgl.v3f_t2f(-1.0, -1.0, -1.0, -1.0, -1.0)
	sgl.c3f(0.0, 1.0, 0.0)
	sgl.v3f_t2f(-1.0, -1.0, 1.0, -1.0, 1.0)
	sgl.v3f_t2f(1.0, -1.0, 1.0, 1.0, 1.0)
	sgl.v3f_t2f(1.0, 1.0, 1.0, 1.0, -1.0)
	sgl.v3f_t2f(-1.0, 1.0, 1.0, -1.0, -1.0)
	sgl.c3f(0.0, 0.0, 1.0)
	sgl.v3f_t2f(-1.0, -1.0, 1.0, -1.0, 1.0)
	sgl.v3f_t2f(-1.0, 1.0, 1.0, 1.0, 1.0)
	sgl.v3f_t2f(-1.0, 1.0, -1.0, 1.0, -1.0)
	sgl.v3f_t2f(-1.0, -1.0, -1.0, -1.0, -1.0)
	sgl.c3f(1.0, 0.5, 0.0)
	sgl.v3f_t2f(1.0, -1.0, 1.0, -1.0, 1.0)
	sgl.v3f_t2f(1.0, -1.0, -1.0, 1.0, 1.0)
	sgl.v3f_t2f(1.0, 1.0, -1.0, 1.0, -1.0)
	sgl.v3f_t2f(1.0, 1.0, 1.0, -1.0, -1.0)
	sgl.c3f(0.0, 0.5, 1.0)
	sgl.v3f_t2f(1.0, -1.0, -1.0, -1.0, 1.0)
	sgl.v3f_t2f(1.0, -1.0, 1.0, 1.0, 1.0)
	sgl.v3f_t2f(-1.0, -1.0, 1.0, 1.0, -1.0)
	sgl.v3f_t2f(-1.0, -1.0, -1.0, -1.0, -1.0)
	sgl.c3f(1.0, 0.0, 0.5)
	sgl.v3f_t2f(-1.0, 1.0, -1.0, -1.0, 1.0)
	sgl.v3f_t2f(-1.0, 1.0, 1.0, 1.0, 1.0)
	sgl.v3f_t2f(1.0, 1.0, 1.0, 1.0, -1.0)
	sgl.v3f_t2f(1.0, 1.0, -1.0, -1.0, -1.0)
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
	sgl.rotate(3.0 * sgl.rad(2 * rot[1]), 0.0, 0.0, 1.0)
	cube()
	sgl.pop_matrix()
	sgl.pop_matrix()
}

fn cube_t(r f32, g f32, b f32) {
	sgl.begin_quads()
	// edge color
	sgl.c3f(r, g, b)
	// edge coord
	// x,y,z, texture cord: u,v
	sgl.v3f_t2f(-1.0, 1.0, -1.0, 0.0, 0.25)
	sgl.v3f_t2f(1.0, 1.0, -1.0, 0.25, 0.25)
	sgl.v3f_t2f(1.0, -1.0, -1.0, 0.25, 0.0)
	sgl.v3f_t2f(-1.0, -1.0, -1.0, 0.0, 0.0)
	sgl.c3f(r, g, b)
	sgl.v3f_t2f(-1.0, -1.0, 1.0, 0.0, 0.25)
	sgl.v3f_t2f(1.0, -1.0, 1.0, 0.25, 0.25)
	sgl.v3f_t2f(1.0, 1.0, 1.0, 0.25, 0.0)
	sgl.v3f_t2f(-1.0, 1.0, 1.0, 0.0, 0.0)
	sgl.c3f(r, g, b)
	sgl.v3f_t2f(-1.0, -1.0, 1.0, 0.0, 0.25)
	sgl.v3f_t2f(-1.0, 1.0, 1.0, 0.25, 0.25)
	sgl.v3f_t2f(-1.0, 1.0, -1.0, 0.25, 0.0)
	sgl.v3f_t2f(-1.0, -1.0, -1.0, 0.0, 0.0)
	sgl.c3f(r, g, b)
	sgl.v3f_t2f(1.0, -1.0, 1.0, 0.0, 0.25)
	sgl.v3f_t2f(1.0, -1.0, -1.0, 0.25, 0.25)
	sgl.v3f_t2f(1.0, 1.0, -1.0, 0.25, 0.0)
	sgl.v3f_t2f(1.0, 1.0, 1.0, 0.0, 0.0)
	sgl.c3f(r, g, b)
	sgl.v3f_t2f(1.0, -1.0, -1.0, 0.0, 0.25)
	sgl.v3f_t2f(1.0, -1.0, 1.0, 0.25, 0.25)
	sgl.v3f_t2f(-1.0, -1.0, 1.0, 0.25, 0.0)
	sgl.v3f_t2f(-1.0, -1.0, -1.0, 0.0, 0.0)
	sgl.c3f(r, g, b)
	sgl.v3f_t2f(-1.0, 1.0, -1.0, 0.0, 0.25)
	sgl.v3f_t2f(-1.0, 1.0, 1.0, 0.25, 0.25)
	sgl.v3f_t2f(1.0, 1.0, 1.0, 0.25, 0.0)
	sgl.v3f_t2f(1.0, 1.0, -1.0, 0.0, 0.0)
	sgl.end()
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
	cube_t(1, 1, 1)
	sgl.push_matrix()
	sgl.translate(0.0, 0.0, 3.0)
	sgl.scale(0.5, 0.5, 0.5)
	sgl.rotate(-2.0 * sgl.rad(rot[0]), 1.0, 0.0, 0.0)
	sgl.rotate(-2.0 * sgl.rad(rot[1]), 0.0, 1.0, 0.0)
	cube_t(1, 1, 1)
	sgl.push_matrix()
	sgl.translate(0.0, 0.0, 3.0)
	sgl.scale(0.5, 0.5, 0.5)
	sgl.rotate(-3.0 * sgl.rad(2 * rot[0]), 1.0, 0.0, 0.0)
	sgl.rotate(3.0 * sgl.rad(2 * rot[1]), 0.0, 0.0, 1.0)
	cube_t(1, 1, 1)
	sgl.pop_matrix()
	sgl.pop_matrix()

	sgl.disable_texture()
}

fn cube_field(app App) {
	rot := [f32(app.mouse_x), f32(app.mouse_y)]
	xyz_sz := f32(2.0)
	field_size := 20

	sgl.defaults()
	sgl.load_pipeline(app.pip_3d)

	sgl.enable_texture()
	sgl.texture(app.texture)

	sgl.matrix_mode_projection()
	sgl.perspective(sgl.rad(45.0), 1.0, 0.1, 200.0)

	sgl.matrix_mode_modelview()

	sgl.translate(field_size, 0.0, -120.0)
	sgl.rotate(sgl.rad(rot[0]), 0.0, 1.0, 0.0)
	sgl.rotate(sgl.rad(rot[1]), 1.0, 0.0, 0.0)

	// draw field_size*field_size cubes
	for y in 0 .. field_size {
		for x in 0 .. field_size {
			sgl.push_matrix()
			z := f32(math.cos(f32(x * 2) / field_size) * math.sin(f32(y * 2) / field_size) * xyz_sz) * (xyz_sz * 5)
			sgl.translate(x * xyz_sz, z, y * xyz_sz)
			cube_t(f32(f32(x) / field_size), f32(f32(y) / field_size), 1)
			sgl.pop_matrix()
		}
	}
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

	app.gg.begin()
	// sgl.defaults()

	// 2d triangle
	sgl.viewport(x0, y0, ww, hh, true)
	draw_triangle()

	// colored cubes with viewport
	sgl.viewport(x0, y1, ww, hh, true)
	draw_cubes(app)

	// textured cubed with viewport
	sgl.viewport(0, int(dh / 5), dw, int(dh * ratio), true)
	draw_texture_cubes(app)

	// textured field of cubes with viewport
	sgl.viewport(0, int(dh / 5), dw, int(dh * ratio), true)
	cube_field(app)

	app.frame_count++

	app.gg.end()
}

/******************************************************************************
*
* Init / Cleanup
*
******************************************************************************/
fn my_init(mut app App) {
	app.init_flag = true

	// set max vertices,
	// for a large number of the same type of object it is better use the instances!!
	desc := sapp.create_desc()
	gfx.setup(&desc)
	sgl_desc := C.sgl_desc_t{
		max_vertices: 50 * 65536
	}
	sgl.setup(&sgl_desc)

	// 3d pipeline
	mut pipdesc := C.sg_pipeline_desc{}
	unsafe { C.memset(&pipdesc, 0, sizeof(pipdesc)) }

	color_state := C.sg_color_state{
		blend: C.sg_blend_state{
			enabled: true
			src_factor_rgb: gfx.BlendFactor(C.SG_BLENDFACTOR_SRC_ALPHA)
			dst_factor_rgb: gfx.BlendFactor(C.SG_BLENDFACTOR_ONE_MINUS_SRC_ALPHA)
		}
	}
	pipdesc.colors[0] = color_state

	pipdesc.depth = C.sg_depth_state{
		write_enabled: true
		compare: gfx.CompareFunc(C.SG_COMPAREFUNC_LESS_EQUAL)
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
				tmp_txt[i] = byte(0xFF)
				tmp_txt[i + 1] = byte(0)
				tmp_txt[i + 2] = byte(0)
				tmp_txt[i + 3] = byte(0xFF)
			}
			// low right corner
			else if x == 7 && y == 7 {
				tmp_txt[i] = byte(0)
				tmp_txt[i + 1] = byte(0xFF)
				tmp_txt[i + 2] = byte(0)
				tmp_txt[i + 3] = byte(0xFF)
			} else {
				col := if ((x + y) & 1) == 1 { 0xFF } else { 0 }
				tmp_txt[i] = byte(col) // red
				tmp_txt[i + 1] = byte(col) // green
				tmp_txt[i + 2] = byte(col) // blue
				tmp_txt[i + 3] = byte(0xFF) // alpha
			}
			i += 4
		}
	}
	unsafe {
		app.texture = create_texture(w, h, tmp_txt)
		free(tmp_txt)
	}
}

fn cleanup(mut app App) {
	gfx.shutdown()
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
// is needed for easier diagnostics on windows
[console]
fn main() {
	// App init
	mut app := &App{
		gg: 0
	}

	app.gg = gg.new_context(
		width: win_width
		height: win_height
		use_ortho: true // This is needed for 2D drawing
		create_window: true
		window_title: '3D Cube Demo'
		user_data: app
		bg_color: bg_color
		frame_fn: frame
		init_fn: my_init
		cleanup_fn: cleanup
		event_fn: my_event_manager
	)

	app.gg.run()
}
