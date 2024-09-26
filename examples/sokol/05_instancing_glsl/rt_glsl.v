/**********************************************************************
*
* Sokol 3d cube multishader demo
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
* - frame counter
**********************************************************************/
import gg
import gg.m4
import gx
import math
import sokol.gfx
// import sokol.sgl
import time

const win_width = 800
const win_height = 800
const bg_color = gx.white
const num_inst = 16384

struct App {
mut:
	gg          &gg.Context = unsafe { nil }
	texture     gfx.Image
	sampler     gfx.Sampler
	init_flag   bool
	frame_count int

	mouse_x    int = 903
	mouse_y    int = 638
	mouse_down bool
	// glsl
	cube_pip_glsl gfx.Pipeline
	cube_bind     gfx.Bindings

	pipe map[string]gfx.Pipeline
	bind map[string]gfx.Bindings
	// time
	ticks i64
	// instances
	inst_pos [num_inst]m4.Vec4
	// camera
	camera_x f32 = -8
	camera_z f32 = 47
}

/******************************************************************************
* GLSL Include and functions
******************************************************************************/
#include "@VMODROOT/rt_glsl_instancing.h" # It should be generated with `v shader .` (see the instructions at the top of this file)

fn C.instancing_shader_desc(gfx.Backend) &gfx.ShaderDesc

/******************************************************************************
* Texture functions
******************************************************************************/
fn create_texture(w int, h int, buf byteptr) (gfx.Image, gfx.Sampler) {
	sz := w * h * 4
	// vfmt off
	mut img_desc := gfx.ImageDesc{
		width:         w
		height:        h
		num_mipmaps:   0
//		min_filter:    .linear
//		mag_filter:    .linear
		//usage: .dynamic
//		wrap_u:        .clamp_to_edge
//		wrap_v:        .clamp_to_edge
		label:         &u8(0)
		d3d11_texture: 0
	}
	// vfmt on
	// comment if .dynamic is enabled
	img_desc.data.subimage[0][0] = gfx.Range{
		ptr:  buf
		size: usize(sz)
	}

	sg_img := gfx.make_image(&img_desc)

	mut smp_desc := gfx.SamplerDesc{
		min_filter: .linear
		mag_filter: .linear
		wrap_u:     .clamp_to_edge
		wrap_v:     .clamp_to_edge
	}

	sg_smp := gfx.make_sampler(&smp_desc)
	return sg_img, sg_smp
}

fn destroy_texture(sg_img gfx.Image) {
	gfx.destroy_image(sg_img)
}

// Use only if usage: .dynamic is enabled
fn update_text_texture(sg_img gfx.Image, w int, h int, buf byteptr) {
	sz := w * h * 4
	mut tmp_sbc := gfx.ImageData{}
	tmp_sbc.subimage[0][0] = gfx.Range{
		ptr:  buf
		size: usize(sz)
	}
	gfx.update_image(sg_img, &tmp_sbc)
}

/******************************************************************************
* Draw functions
******************************************************************************
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
	// u u16   // for compatibility with D3D11
	// v u16   // for compatibility with D3D11
	u f32
	v f32
}

// march shader init
fn init_cube_glsl_i(mut app App) {
	// cube vertex buffer
	// d := u16(32767)     // for compatibility with D3D11, 32767 stand for 1
	d := f32(1.0)
	c := u32(0xFFFFFF_FF) // color RGBA8
	// vfmt off
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
	// vfmt on

	mut vert_buffer_desc := gfx.BufferDesc{
		label: c'cube-vertices'
	}
	unsafe { vmemset(&vert_buffer_desc, 0, int(sizeof(vert_buffer_desc))) }
	vert_buffer_desc.size = usize(vertices.len * int(sizeof(Vertex_t)))
	vert_buffer_desc.data = gfx.Range{
		ptr:  vertices.data
		size: usize(vertices.len * int(sizeof(Vertex_t)))
	}
	vert_buffer_desc.@type = .vertexbuffer
	vbuf := gfx.make_buffer(&vert_buffer_desc)

	// create an instance buffer for the cube
	mut inst_buffer_desc := gfx.BufferDesc{
		label: c'instance-data'
	}
	unsafe { vmemset(&inst_buffer_desc, 0, int(sizeof(inst_buffer_desc))) }

	inst_buffer_desc.size = usize(num_inst * int(sizeof(m4.Vec4)))
	inst_buffer_desc.@type = .vertexbuffer
	inst_buffer_desc.usage = .stream
	inst_buf := gfx.make_buffer(&inst_buffer_desc)

	// create an index buffer for the cube
	// vfmt off
	indices := [
		u16(0),  1,  2,    0,  2,  3,
		     6,  5,  4,    7,  6,  4,
		     8,  9, 10,    8, 10, 11,
		    14, 13, 12,   15, 14, 12,
		    16, 17, 18,   16, 18, 19,
		    22, 21, 20,   23, 22, 20,
	]
	// vfmt on

	mut index_buffer_desc := gfx.BufferDesc{
		label: c'cube-indices'
	}
	unsafe { vmemset(&index_buffer_desc, 0, int(sizeof(index_buffer_desc))) }
	index_buffer_desc.size = usize(indices.len * int(sizeof(u16)))
	index_buffer_desc.data = gfx.Range{
		ptr:  indices.data
		size: usize(indices.len * int(sizeof(u16)))
	}
	index_buffer_desc.@type = .indexbuffer
	ibuf := gfx.make_buffer(&index_buffer_desc)

	// create shader
	shader := gfx.make_shader(C.instancing_shader_desc(C.sg_query_backend()))

	mut pipdesc := gfx.PipelineDesc{}
	unsafe { vmemset(&pipdesc, 0, int(sizeof(pipdesc))) }
	pipdesc.layout.buffers[0].stride = int(sizeof(Vertex_t))

	// vfmt off
	// the constants [C.ATTR_vs_m_pos, C.ATTR_vs_m_color0, C.ATTR_vs_m_texcoord0] are generated by sokol-shdc
	pipdesc.layout.attrs[C.ATTR_vs_i_pos      ].format       = .float3   // x,y,z as f32
	pipdesc.layout.attrs[C.ATTR_vs_i_pos      ].buffer_index = 0
	pipdesc.layout.attrs[C.ATTR_vs_i_color0   ].format       = .ubyte4n  // color as u32
	pipdesc.layout.attrs[C.ATTR_vs_i_pos      ].buffer_index = 0
	pipdesc.layout.attrs[C.ATTR_vs_i_texcoord0].format       = .float2   // u,v as f32
	pipdesc.layout.attrs[C.ATTR_vs_i_pos      ].buffer_index = 0

	// instancing
	// the constant ATTR_vs_i_inst_pos is generated by sokol-shdc
	pipdesc.layout.buffers[1].stride    = int(sizeof(m4.Vec4))
	pipdesc.layout.buffers[1].step_func = .per_instance  // we will pass a single parameter for each instance!!
	pipdesc.layout.attrs[C.ATTR_vs_i_inst_pos ].format        = .float4
	pipdesc.layout.attrs[C.ATTR_vs_i_inst_pos ].buffer_index  = 1
	// vfmt on

	pipdesc.shader = shader
	pipdesc.index_type = .uint16

	pipdesc.depth = gfx.DepthState{
		write_enabled: true
		compare:       .less_equal
	}
	pipdesc.cull_mode = .back

	pipdesc.label = 'glsl_shader pipeline'.str

	mut bind := gfx.Bindings{}
	unsafe { vmemset(&bind, 0, int(sizeof(bind))) }
	bind.vertex_buffers[0] = vbuf // vertex buffer
	bind.vertex_buffers[1] = inst_buf // instance buffer
	bind.index_buffer = ibuf
	bind.fs.images[C.SLOT_tex] = app.texture
	bind.fs.samplers[C.SLOT_smp] = app.sampler
	app.bind['inst'] = bind
	app.pipe['inst'] = gfx.make_pipeline(&pipdesc)

	println('GLSL March init DONE!')
}

fn calc_tr_matrices(w f32, h f32, rx f32, ry f32, in_scale f32) m4.Mat4 {
	// vfmt off
	proj := m4.perspective(60, w/h, 0.01, 4000.0)
	view := m4.look_at(m4.Vec4{e:[f32(0.0),100,6,0]!}, m4.Vec4{e:[f32(0),0,0,0]!}, m4.Vec4{e:[f32(0),1.0,0,0]!})
	view_proj := view * proj

	rxm := m4.rotate(m4.rad(rx), m4.Vec4{e:[f32(1),0,0,0]!})
	rym := m4.rotate(m4.rad(ry), m4.Vec4{e:[f32(0),1,0,0]!})
	// vfmt on

	model := rym * rxm
	scale_m := m4.scale(m4.Vec4{ e: [in_scale, in_scale, in_scale, 1]! })

	res := (scale_m * model) * view_proj
	return res
}

// triangles draw
fn draw_cube_glsl_i(mut app App) {
	if app.init_flag == false {
		return
	}

	ws := gg.window_size_real_pixels()
	// ratio := f32(ws.width) / ws.height
	dw := f32(ws.width / 2)
	dh := f32(ws.height / 2)

	rot := [f32(app.mouse_y), f32(app.mouse_x)]
	tr_matrix := calc_tr_matrices(dw, dh, rot[0], rot[1], 2.3)

	gfx.apply_pipeline(app.pipe['inst'])
	gfx.apply_bindings(app.bind['inst'])

	//***************
	// Instancing
	//***************
	// passing the instancing to the vs
	time_ticks := f32(time.ticks() - app.ticks) / 1000
	cube_size := 2
	sz := 128 // field size dimension
	cx := 64 // x center for the cubes
	cz := 64 // z center for the cubes
	// frame := (app.frame_count/4) % 100
	for index in 0 .. num_inst {
		x := f32(index % sz)
		z := f32(index / sz)
		// simply waves
		y := f32(math.cos((x + time_ticks) / 2.0) * math.sin(z / 2.0)) * 2
		// sombrero function
		// r := ((x-cx)*(x-cx)+(z-cz)*(z-cz))/(sz/2)
		// y := f32(math.sin(r+time_ticks)*4.0)
		spare_param := f32(index % 10)
		// vfmt off
		app.inst_pos[index] = m4.Vec4{e:[f32((x - cx - app.camera_x) * cube_size),y ,f32( (z - cz - app.camera_z) * cube_size),spare_param]!}
		// vfmt on
	}
	range := gfx.Range{
		ptr:  unsafe { &app.inst_pos }
		size: usize(num_inst * int(sizeof(m4.Vec4)))
	}
	gfx.update_buffer(app.bind['inst'].vertex_buffers[1], &range)

	// Uniforms
	// *** vertex shadeer uniforms ***
	// passing the view matrix as uniform
	// res is a 4x4 matrix of f32 thus: 4*16 byte of size
	vs_uniforms_range := gfx.Range{
		ptr:  unsafe { &tr_matrix }
		size: usize(4 * 16)
	}
	gfx.apply_uniforms(.vs, C.SLOT_vs_params_i, &vs_uniforms_range)

	/*
	// *** fragment shader uniforms ***
	time_ticks := f32(time.ticks() - app.ticks) / 1000
	// vfmt off
	mut tmp_fs_params := [
		f32(ws.width), ws.height * ratio,  // x,y resolution to pass to FS
		0,0,                         // dont send mouse position
		//app.mouse_x,               // mouse x
		//ws.height - app.mouse_y*2, // mouse y scaled
		time_ticks,                  // time as f32
		app.frame_count,             // frame count
		0,0                          // padding bytes , see "fs_params" struct paddings in rt_glsl.h
	]!
	// vfmt on
	fs_uniforms_range := gfx.Range{
		ptr: unsafe { &tmp_fs_params }
		size: usize(sizeof(tmp_fs_params))
	}
	gfx.apply_uniforms(.fs, C.SLOT_fs_params, &fs_uniforms_range)
	*/
	// 3 vertices for triangle * 2 triangles per face * 6 faces = 36 vertices to draw for num_inst times
	gfx.draw(0, (3 * 2) * 6, num_inst)
}

fn draw_start_glsl(app App) {
	if app.init_flag == false {
		return
	}

	ws := gg.window_size_real_pixels()
	// ratio := f32(ws.width) / ws.height
	// dw := f32(ws.width  / 2)
	// dh := f32(ws.height / 2)

	gfx.apply_viewport(0, 0, ws.width, ws.height, true)
}

fn draw_end_glsl(app App) {
	gfx.end_pass()
	gfx.commit()
}

fn frame(mut app App) {
	// clear
	mut color_action := gfx.ColorAttachmentAction{
		load_action: .clear
		clear_value: gfx.Color{
			r: 0.0
			g: 0.0
			b: 0.0
			a: 1.0
		}
	}
	mut pass_action := gfx.PassAction{}
	pass_action.colors[0] = color_action
	pass := gg.create_default_pass(pass_action)
	gfx.begin_pass(&pass)

	draw_start_glsl(app)
	draw_cube_glsl_i(mut app)
	draw_end_glsl(app)
	app.frame_count++
}

/******************************************************************************
* Init / Cleanup
******************************************************************************/
fn my_init(mut app App) {
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
				tmp_txt[i + 0] = u8(0xFF)
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
				tmp_txt[i + 0] = u8(col) // red
				tmp_txt[i + 1] = u8(col) // green
				tmp_txt[i + 2] = u8(col) // blue
				tmp_txt[i + 3] = u8(0xFF) // alpha
			}
			i += 4
		}
	}
	unsafe {
		app.texture, app.sampler = create_texture(w, h, tmp_txt)
		free(tmp_txt)
	}
	// glsl
	init_cube_glsl_i(mut app)
	app.init_flag = true
}

/******************************************************************************
* events handling
******************************************************************************/
fn my_event_manager(mut ev gg.Event, mut app App) {
	if ev.typ == .mouse_down {
		app.mouse_down = true
	}
	if ev.typ == .mouse_up {
		app.mouse_down = false
	}
	if app.mouse_down == true && ev.typ == .mouse_move {
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

	// keyboard
	if ev.typ == .key_down {
		step := f32(1.0)
		match ev.key_code {
			.w { app.camera_z += step }
			.s { app.camera_z -= step }
			.a { app.camera_x -= step }
			.d { app.camera_x += step }
			else {}
		}
	}
	eprintln('>> app.camera_x: ${app.camera_x} , app.camera_z: ${app.camera_z}, app.mouse_x: ${app.mouse_x}, app.mouse_y: ${app.mouse_y}')
}

fn main() {
	mut app := &App{}
	// vfmt off
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		create_window: true
		window_title:  'Instancing Cube'
		user_data:     app
		bg_color:      bg_color
		frame_fn:      frame
		init_fn:       my_init
		event_fn:      my_event_manager
	)
	// vfmt on
	app.ticks = time.ticks()
	app.gg.run()
}
