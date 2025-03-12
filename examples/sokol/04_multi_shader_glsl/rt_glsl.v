// vtest build: misc-tooling // needs .h files that are produced by `v shader`
/**********************************************************************
* Sokol 3d cube multishader demo
* Copyright (c) 2024 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* HOW TO COMPILE SHADERS:
* Run `v shader .` in this directory to compile the shaders.
* For more info and help with shader compilation see `docs.md` and `v help shader`.
**********************************************************************/
import gg
import gg.m4
import sokol.sapp
import sokol.gfx
import sokol.sgl
import time

// GLSL Include and functions
#include "@VMODROOT/rt_glsl_march.h" # It should be generated with `v shader .` (see the instructions at the top of this file)
#include "@VMODROOT/rt_glsl_puppy.h" # It should be generated with `v shader .` (see the instructions at the top of this file)

fn C.rt_march_shader_desc(gfx.Backend) &gfx.ShaderDesc
fn C.rt_puppy_shader_desc(gfx.Backend) &gfx.ShaderDesc

const start_ticks = time.ticks()

@[heap]
struct App {
mut:
	gg         &gg.Context = unsafe { nil }
	texture    gfx.Image
	sampler    gfx.Sampler
	init_flag  bool
	mouse_x    int = 502
	mouse_y    int = 394
	mouse_down bool
	// glsl
	cube_pip_glsl gfx.Pipeline
	cube_bind     gfx.Bindings
	pipe          map[string]gfx.Pipeline
	bind          map[string]gfx.Bindings
}

/******************************************************************************
* Texture functions
******************************************************************************/
fn create_texture(w int, h int, buf byteptr) (gfx.Image, gfx.Sampler) {
	sz := w * h * 4
	mut img_desc := gfx.ImageDesc{
		width:  w
		height: h
	}
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
		and WebGL2 / GLES2 does not support integer vertex shader inputs.
*/
struct Vertex_t {
	x     f32
	y     f32
	z     f32
	color u32
	u     f32
	v     f32
}

const vertices = cube_vertices()

fn cube_vertices() []Vertex_t {
	// cube vertex buffer
	d := f32(1.0)
	c := u32(0xFF_FF_FF_FF) // white color RGBA8
	// vfmt off
	// 6 faces, each defined by 4 vertices:
	cube := [
		Vertex_t{-1.0, -1.0, -1.0, c, 0, 0}, Vertex_t{ 1.0, -1.0, -1.0, c, d, 0}, Vertex_t{ 1.0,  1.0, -1.0, c, d, d}, Vertex_t{-1.0,  1.0, -1.0, c, 0, d},
		Vertex_t{-1.0, -1.0,  1.0, c, 0, 0}, Vertex_t{ 1.0, -1.0,  1.0, c, d, 0}, Vertex_t{ 1.0,  1.0,  1.0, c, d, d}, Vertex_t{-1.0,  1.0,  1.0, c, 0, d},
		Vertex_t{-1.0, -1.0, -1.0, c, 0, 0}, Vertex_t{-1.0,  1.0, -1.0, c, d, 0}, Vertex_t{-1.0,  1.0,  1.0, c, d, d}, Vertex_t{-1.0, -1.0,  1.0, c, 0, d},
		Vertex_t{ 1.0, -1.0, -1.0, c, 0, 0}, Vertex_t{ 1.0,  1.0, -1.0, c, d, 0}, Vertex_t{ 1.0,  1.0,  1.0, c, d, d}, Vertex_t{ 1.0, -1.0,  1.0, c, 0, d},
		Vertex_t{-1.0, -1.0, -1.0, c, 0, 0}, Vertex_t{-1.0, -1.0,  1.0, c, d, 0}, Vertex_t{ 1.0, -1.0,  1.0, c, d, d}, Vertex_t{ 1.0, -1.0, -1.0, c, 0, d},
		Vertex_t{-1.0,  1.0, -1.0, c, 0, 0}, Vertex_t{-1.0,  1.0,  1.0, c, d, 0}, Vertex_t{ 1.0,  1.0,  1.0, c, d, d}, Vertex_t{ 1.0,  1.0, -1.0, c, 0, d},
	]
	// vfmt off
	return cube
}

fn (mut app App) init_glsl_shader(shader_name string, shader_desc &gfx.ShaderDesc, indices []u16) {
	mut vert_buffer_desc := gfx.BufferDesc{}
	unsafe { vmemset(&vert_buffer_desc, 0, sizeof(vert_buffer_desc)) }
	vert_buffer_desc.label = c'cube-vertices'
	vert_buffer_desc.size = usize(vertices.len) * sizeof(Vertex_t)
	vert_buffer_desc.data = gfx.Range{
		ptr: vertices.data
		size: vert_buffer_desc.size
	}
	vert_buffer_desc.type = .vertexbuffer
	vbuf := gfx.make_buffer(&vert_buffer_desc)

	mut index_buffer_desc := gfx.BufferDesc{}
	unsafe { vmemset(&index_buffer_desc, 0, sizeof(index_buffer_desc)) }
	index_buffer_desc.label = c'cube-indices'
	index_buffer_desc.size = usize(indices.len) * sizeof(u16)
	index_buffer_desc.data = gfx.Range{
		ptr: indices.data
		size: index_buffer_desc.size
	}
	index_buffer_desc.type = .indexbuffer
	ibuf := gfx.make_buffer(&index_buffer_desc)

	// create shader
	shader := gfx.make_shader(shader_desc)

	mut pipdesc := gfx.PipelineDesc{}
	unsafe { vmemset(&pipdesc, 0, sizeof(pipdesc)) }
	pipdesc.label = c'glsl_shader pipeline'
	pipdesc.layout.buffers[0].stride = int(sizeof(Vertex_t))

	// the constants [C.ATTR_vs_m_pos, C.ATTR_vs_m_color0, C.ATTR_vs_m_texcoord0] are generated by sokol-shdc
	pipdesc.layout.attrs[C.ATTR_vs_m_pos].format = .float3 // x,y,z as f32
	pipdesc.layout.attrs[C.ATTR_vs_m_color0].format = .ubyte4n // color as u32
	pipdesc.layout.attrs[C.ATTR_vs_m_texcoord0].format = .float2 // u,v as f32
	// pipdesc.layout.attrs[C.ATTR_vs_m_texcoord0].format  = .short2n  // u,v as u16
	pipdesc.shader = shader
	pipdesc.index_type = .uint16
	pipdesc.depth = gfx.DepthState{
		write_enabled: true
		compare: .less_equal
	}
	pipdesc.cull_mode = .back

	mut bind := gfx.Bindings{}
	unsafe { vmemset(&bind, 0, sizeof(bind)) }
	bind.vertex_buffers[0] = vbuf
	bind.index_buffer = ibuf
	bind.fs.images[C.SLOT_tex] = app.texture
	bind.fs.samplers[C.SLOT_smp] = app.sampler
	app.bind[shader_name] = bind
	app.pipe[shader_name] = gfx.make_pipeline(&pipdesc)
	println('${@FN} for shader `${shader_name}` done.')
}

fn calc_tr_matrices(w f32, h f32, rx f32, ry f32, in_scale f32) m4.Mat4 {
	proj := m4.perspective(60, w / h, 0.01, 10.0)
	view := m4.look_at(m4.vec4(0.0, 0, 6, 0), m4.vec4(0, 0, 0, 0), m4.vec4(0, 1, 0, 0))
	view_proj := view * proj

	rxm := m4.rotate(m4.rad(rx), m4.vec4(1, 0, 0, 0))
	rym := m4.rotate(m4.rad(ry), m4.vec4(0, 1, 0, 0))

	model := rym * rxm
	scale_m := m4.scale(m4.vec4(in_scale, in_scale, in_scale, 1))

	res := (scale_m * model) * view_proj
	return res
}

fn (app &App) draw_glsl_shader(shader_name string) {
	ws := gg.window_size_real_pixels()
	ratio := f32(ws.width) / ws.height
	dw := f32(ws.width / 2)
	dh := f32(ws.height / 2)

	rot := [f32(app.mouse_y), f32(app.mouse_x)]
	tr_matrix := calc_tr_matrices(dw, dh, rot[0], rot[1], 2.3)

	// apply the pipeline and bindings
	gfx.apply_pipeline(app.pipe[shader_name])
	gfx.apply_bindings(app.bind[shader_name])

	// Uniforms
	// *** vertex shadeer uniforms ***
	// passing the view matrix as uniform
	// res is a 4x4 matrix of f32 thus: 4*16 byte of size
	vs_uniforms_range := gfx.Range{
		ptr: &tr_matrix
		size: usize(4 * 16)
	}
	gfx.apply_uniforms(.vs, C.SLOT_vs_params_m, &vs_uniforms_range)

	// *** fragment shader uniforms ***
	time_ticks := f32(time.ticks() - start_ticks) / 1000
	mut tmp_fs_params := [
		f32(ws.width),
		ws.height * ratio, // x,y resolution to pass to FS
		0,
		0, // dont send mouse position
		// app.mouse_x,               // mouse x
		// ws.height - app.mouse_y*2, // mouse y scaled
		time_ticks, // time as f32
		f32(app.gg.frame), // frame count
		0,
		0, // padding bytes , see "fs_params" struct paddings in rt_glsl.h
	]!
	fs_uniforms_range := gfx.Range{
		ptr: unsafe { &tmp_fs_params }
		size: usize(sizeof(tmp_fs_params))
	}
	gfx.apply_uniforms(.fs, C.SLOT_fs_params_p, &fs_uniforms_range)

	// 3 vertices for triangle * 2 triangles per face * 6 faces = 36 vertices to draw
	gfx.draw(0, (3 * 2) * 3, 1)
}

fn (mut app App) frame() {
	mut pass_action := gfx.PassAction{}
	pass_action.colors[0] = gfx.ColorAttachmentAction{
		load_action: .clear
		clear_value: gfx.Color{b: 0.9}
	}
	gfx.begin_pass(sapp.create_default_pass(pass_action))
	if !app.init_flag {
		return
	}
	ws := gg.window_size_real_pixels()
	gfx.apply_viewport(0, 0, ws.width, ws.height, true)
    app.draw_glsl_shader('march')
    app.draw_glsl_shader('puppy')
	gfx.end_pass()
	gfx.commit()
}

fn (mut app App) on_init() {
	// set max vertices, but note, that for a large number of the same type of object it is better use the instances!!
    gfx.setup(sapp.create_desc())
    sgl.setup(sgl.Desc{max_vertices: 50 * 65536})

	// create chessboard texture 256*256 RGBA
	w := 256
	h := 256
	sz := w * h * 4
	tmp_txt := unsafe { malloc(sz) }
	defer {
		unsafe { free(tmp_txt) }
	}
	mut i := 0
	for i < sz {
		unsafe {
			y := (i >> 0x8) >> 5 // 8 cell
			x := (i & 0xFF) >> 5 // 8 cell
			// upper left corner
			if x == 0 && y == 0 {
				tmp_txt[i + 0] = 0xFF
				tmp_txt[i + 1] = 0
				tmp_txt[i + 2] = 0
				tmp_txt[i + 3] = 0xFF
			}
			// low right corner
			else if x == 7 && y == 7 {
				tmp_txt[i + 0] = 0
				tmp_txt[i + 1] = 0xFF
				tmp_txt[i + 2] = 0
				tmp_txt[i + 3] = 0xFF
			} else {
				col := if ((x + y) & 1) == 1 { u8(0xFF) } else { 128 }
				tmp_txt[i + 0] = col // red
				tmp_txt[i + 1] = col // green
				tmp_txt[i + 2] = col // blue
				tmp_txt[i + 3] = 0xFF // alpha
			}
			i += 4
		}
	}
	app.texture, app.sampler = create_texture(w, h, tmp_txt)

    // vfmt off
    app.init_glsl_shader('march', voidptr(C.rt_march_shader_desc(C.sg_query_backend())), [
        u16(0), 1,  2,    0,   2,  3,
            6,  5,  4,    7,   6,  4,
            8,  9, 10,    8,  10, 11,
    ])
    app.init_glsl_shader('puppy', voidptr(C.rt_puppy_shader_desc(C.sg_query_backend())), [
        u16(14), 13, 12,  15, 14, 12,
            16,  17, 18,  16, 18, 19,
            22,  21, 20,  23, 22, 20,
    ])
	// vfmt on
	app.init_flag = true
}

/******************************************************************************
* events handling
******************************************************************************/
fn (mut app App) on_event(ev &gg.Event, x voidptr) {
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
	// eprintln('> app.mouse_x: ${app.mouse_x} | app.mouse_y: ${app.mouse_y}')
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        800
		height:       800
		window_title: '3D Dual shader Cube - click and rotate with the mouse'
		user_data:    app
		frame_fn:     app.frame
		init_fn:      app.on_init
		event_fn:     app.on_event
	)
	app.gg.run()
}
