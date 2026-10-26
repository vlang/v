module render

import sokol.gfx

pub enum Error {
	no_error
	vertices_full
	commands_full
	uniforms_full
	stack_overflow
	stack_underflow
	no_context
}

pub enum Primitive {
	points
	lines
	line_strip
	triangles
	triangle_strip
	quads
}

pub const max_stack_depth = 32

pub struct Desc {
pub mut:
	max_vertices int
	max_commands int
	face_winding gfx.FaceWinding = .ccw
	// sample_count must match the render pass the context draws into: the window's
	// framebuffer sample count for the default pass (0 falls back to 1).
	sample_count int
}

pub struct MatrixMode {
pub:
	value int
}

pub const matrixmode_modelview = MatrixMode{ value: 0 }
pub const matrixmode_projection = MatrixMode{ value: 1 }
pub const matrixmode_texture = MatrixMode{ value: 2 }

pub struct ImageData {
pub mut:
	data     voidptr
	pixels   []u8
	width    int
	height   int
	channels int
	filter   int
}

pub struct State {
pub mut:
	max_vertices      int
	max_commands      int
	face_winding      gfx.FaceWinding
	verts             []Vertex
	commands          []Command
	uniforms          []Uniform
	vbuf              gfx.Buffer
	vertex_count      int
	vtx_count         int
	base_vertex       int
	commands_count    int
	uniforms_count    int
	error             Error
	u                 f32
	vv                f32
	rgba              u32 = 0xffffffff
	point_size        f32 = 1.0
	texturing_enabled bool
	cur_img           gfx.Image
	cur_smp           gfx.Sampler
	pip_stack         [max_stack_depth]Pipeline
	pip_tos           int
	def_pip           Pipeline
	matrix_stack      [3][max_stack_depth]Mat4
	matrix_tos        [3]int
	cur_matrix_mode   MatrixMode
	matrix_dirty      bool = true
	frame_id          int
	update_frame_id   int = -1
	layer_id          int
	in_begin          bool
	cur_prim          Primitive
	def_img           gfx.Image
	def_smp           gfx.Sampler
	shader            gfx.Shader
	bindings          gfx.Bindings
	image_data        map[int]ImageData
	next_pipeline_id  int = 1
	sample_count      int = 1
}

@[heap]
pub struct Context {
	st &State
}

pub fn new_context(desc Desc) &Context {
	mut st := &State{}
	max_v := if desc.max_vertices != 0 { desc.max_vertices } else { 1 << 17 }
	max_c := if desc.max_commands != 0 { desc.max_commands } else { 1 << 15 }
	st.max_vertices = max_v
	st.max_commands = max_c
	st.face_winding = desc.face_winding
	st.sample_count = if desc.sample_count > 0 { desc.sample_count } else { 1 }
	st.verts = []Vertex{len: max_v}
	st.commands = []Command{len: max_c}
	st.uniforms = []Uniform{len: max_c}
	st.matrix_tos[0] = 0
	st.matrix_tos[1] = 0
	st.matrix_tos[2] = 0
	st.matrix_stack[0][0] = mat4_identity()
	st.matrix_stack[1][0] = mat4_identity()
	st.matrix_stack[2][0] = mat4_identity()
	st.cur_matrix_mode = matrixmode_modelview
	st.pip_tos = 0
	st.rgba = 0xffffffff
	st.point_size = 1.0
	st.texturing_enabled = false

	if gfx.is_valid() {
		buf_desc := gfx.BufferDesc{
			size:  usize(max_v) * usize(sizeof(Vertex))
			type:  .vertexbuffer
			usage: .stream
		}
		st.vbuf = gfx.make_buffer(&buf_desc)

		mut pixels := []u8{len: 64 * 4, init: 0xff}
		mut img_desc := gfx.ImageDesc{
			type:         ._2d
			width:        8
			height:       8
			num_mipmaps:  1
			pixel_format: .rgba8
		}
		img_desc.data.subimage[0][0] = gfx.Range{ ptr: unsafe { &pixels[0] }, size: 64 * 4 }
		st.def_img = gfx.make_image(&img_desc)
		smp_desc := gfx.SamplerDesc{
			min_filter: .nearest
			mag_filter: .nearest
		}
		st.def_smp = gfx.make_sampler(&smp_desc)
		st.shader = make_shader()
	}
	st.cur_img = st.def_img
	st.cur_smp = st.def_smp
	st.def_pip = Pipeline{ id: 0 }
	if gfx.is_valid() {
		st.def_pip = st_make_default_pipeline(st)
	}

	ctx := &Context{ st: st }
	ctx.defaults()
	if gfx.is_valid() {
		listener := gfx.CommitListener{
			func:      commit_listener_fn
			user_data: voidptr(ctx)
		}
		gfx.add_commit_listener(listener)
	}
	return ctx
}

fn st_make_default_pipeline(st &State) Pipeline {
	mut p := Pipeline{}
	p.id = 0
	for i in 0 .. num_primitive_types {
		mut d := gfx.PipelineDesc{}
		d.shader = st.shader
		d.layout.buffers[0].stride = int(sizeof(Vertex))
		d.layout.attrs[0].offset = 0
		d.layout.attrs[0].format = .float3
		d.layout.attrs[0].buffer_index = 0
		d.layout.attrs[1].offset = 12
		d.layout.attrs[1].format = .float2
		d.layout.attrs[1].buffer_index = 0
		d.layout.attrs[2].offset = 20
		d.layout.attrs[2].format = .ubyte4n
		d.layout.attrs[2].buffer_index = 0
		d.layout.attrs[3].offset = 24
		d.layout.attrs[3].format = .float
		d.layout.attrs[3].buffer_index = 0
		d.index_type = .none
		d.sample_count = st.sample_count
		d.face_winding = st.face_winding
		if d.colors[0].write_mask == ._default {
			d.colors[0].write_mask = .rgb
		}
		match i {
			0 { d.primitive_type = .points }
			1 { d.primitive_type = .lines }
			2 { d.primitive_type = .line_strip }
			3 { d.primitive_type = .triangles }
			4 { d.primitive_type = .triangle_strip }
			5 { d.primitive_type = .triangle_strip }
			else {}
		}
		if i == 5 {
			// quads are emulated with triangles (sgl does the same), so no
			// separate pipeline object is created for them.
			p.pips[i] = p.pips[3]
		} else {
			p.pips[i] = gfx.make_pipeline(&d)
		}
	}
	return p
}

fn commit_listener_fn(user_data voidptr) {
	ctx := &Context(user_data)
	if ctx == unsafe { nil } {
		return
	}
	ctx.rewind()
	mut st := ctx.st
	st.frame_id++
}

fn pack_rgba_f(r f32, g f32, b f32, a f32) u32 {
	clamp := fn (v f32) u8 {
		mut x := v
		if x < 0.0 { x = 0.0 }
		if x > 1.0 { x = 1.0 }
		return u8(x * 255.0)
	}
	rr := clamp(r)
	gg := clamp(g)
	bb := clamp(b)
	aa := clamp(a)
	return u32(rr) | (u32(gg) << 8) | (u32(bb) << 16) | (u32(aa) << 24)
}

pub fn (ctx &Context) register_image(id int, data voidptr, width int, height int, channels int, filter int) {
	mut st := ctx.st
	ch := if channels > 0 { channels } else { 4 }
	size := width * height * ch
	mut pixels := []u8{len: size}
	if data != unsafe { nil } && size > 0 {
		unsafe { C.memcpy(&pixels[0], data, size) }
	}
	st.image_data[id] = ImageData{
		data:     data
		pixels:   pixels
		width:    width
		height:   height
		channels: ch
		filter:   filter
	}
}

// is_image_registered reports whether a CPU-side copy of `id` already exists.
pub fn (ctx &Context) is_image_registered(id int) bool {
	return id in ctx.st.image_data
}

// update_image_region refreshes a sub-rectangle of an already registered image.
// Only the CPU rasterizer uses the registry, so callers should skip this on the
// GPU path. It avoids copying a whole atlas for every small dirty rectangle.
pub fn (ctx &Context) update_image_region(id int, data voidptr, x int, y int, w int, h int, channels int) {
	if data == unsafe { nil } || w <= 0 || h <= 0 {
		return
	}
	mut st := ctx.st
	img := st.image_data[id] or { return }
	if img.width <= 0 || img.height <= 0 || img.channels <= 0 {
		return
	}
	ch := if channels > 0 { channels } else { img.channels }
	if ch != img.channels || img.pixels.len < img.width * img.height * img.channels {
		return
	}
	x0 := if x < 0 { 0 } else { x }
	y0 := if y < 0 { 0 } else { y }
	x1 := if x + w > img.width { img.width } else { x + w }
	y1 := if y + h > img.height { img.height } else { y + h }
	if x1 <= x0 || y1 <= y0 {
		return
	}
	row_bytes := (x1 - x0) * ch
	src := unsafe { &u8(data) }
	for row in y0 .. y1 {
		unsafe {
			C.memcpy(&img.pixels[row * img.width * ch + x0 * ch], src +
				(row * img.width + x0) * ch, row_bytes)
		}
	}
}

// destroy detaches the context from sokol and drops its CPU-side image copies.
// It must be called before `gfx.shutdown()`. Pipelines created with
// `make_pipeline` are owned by the caller and stay alive until
// `destroy_pipeline` is called for them.
pub fn (ctx &Context) destroy() {
	if ctx == unsafe { nil } || ctx.st == unsafe { nil } {
		return
	}
	if gfx.is_valid() {
		gfx.remove_commit_listener(gfx.CommitListener{
			func:      commit_listener_fn
			user_data: voidptr(ctx)
		})
	}
	ctx.st.image_data.clear()
}

pub fn (ctx &Context) setup(desc Desc) {
	// no-op: construction happens in new_context; kept for API parity.
}
