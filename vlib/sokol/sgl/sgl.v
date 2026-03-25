@[has_globals]
module sgl

import math
import sokol.gfx
import sokol.memory

pub const version = gfx.version + 1

pub const default_context = Context{0x00010001}

__global sgl_state = SglState{}

// ========================
//   Pool Management
// ========================

fn init_pool(mut pool Pool, num int) {
	pool.size = num + 1
	pool.queue_top = 0
	pool.gen_ctrs = unsafe { &u32(C.calloc(usize(pool.size), sizeof(u32))) }
	pool.free_queue = unsafe { &int(C.calloc(usize(num), sizeof(int))) }
	// fill free queue in reverse order
	for i := pool.size - 1; i >= 1; i-- {
		unsafe {
			pool.free_queue[pool.queue_top] = i
		}
		pool.queue_top++
	}
}

fn discard_pool(mut pool Pool) {
	unsafe {
		C.free(pool.free_queue)
		pool.free_queue = nil
		C.free(pool.gen_ctrs)
		pool.gen_ctrs = nil
	}
	pool.size = 0
	pool.queue_top = 0
}

fn pool_alloc_index(mut pool Pool) int {
	if pool.queue_top > 0 {
		pool.queue_top--
		return unsafe { pool.free_queue[pool.queue_top] }
	}
	return invalid_slot_index
}

fn pool_free_index(mut pool Pool, slot_index int) {
	unsafe {
		pool.free_queue[pool.queue_top] = slot_index
	}
	pool.queue_top++
}

fn slot_alloc(mut pool Pool, mut s Slot, slot_index int) u32 {
	unsafe {
		pool.gen_ctrs[slot_index]++
		ctr := pool.gen_ctrs[slot_index]
		s.id = (ctr << slot_shift) | u32(slot_index & slot_mask)
		s.state = .alloc
		return s.id
	}
}

fn slot_index(id u32) int {
	return int(id & slot_mask)
}

// ========================
//   Pipeline Management
// ========================

fn setup_pipeline_pool(pool_size int) {
	init_pool(mut sgl_state.pip_pool.pool, pool_size)
	pool_byte_size := usize(sgl_state.pip_pool.pool.size) * sizeof(PipelineInternal)
	sgl_state.pip_pool.pips = unsafe { &PipelineInternal(C.calloc(1, pool_byte_size)) }
}

fn discard_pipeline_pool() {
	unsafe { C.free(sgl_state.pip_pool.pips) }
	sgl_state.pip_pool.pips = unsafe { nil }
	discard_pool(mut sgl_state.pip_pool.pool)
}

fn pipeline_at(pip_id u32) &PipelineInternal {
	idx := slot_index(pip_id)
	return unsafe { &sgl_state.pip_pool.pips[idx] }
}

fn lookup_pipeline(pip_id u32) &PipelineInternal {
	if pip_id != 0 {
		pip := pipeline_at(pip_id)
		if pip.slot.id == pip_id {
			return pip
		}
	}
	return unsafe { nil }
}

fn alloc_pipeline() Pipeline {
	idx := pool_alloc_index(mut sgl_state.pip_pool.pool)
	if idx != invalid_slot_index {
		mut s := unsafe { &sgl_state.pip_pool.pips[idx].slot }
		id := slot_alloc(mut sgl_state.pip_pool.pool, mut s, idx)
		return Pipeline{id}
	}
	return Pipeline{0}
}

@[direct_array_access]
fn init_pipeline(pip_id Pipeline, in_desc &gfx.PipelineDesc, ctx_desc &ContextDesc) {
	assert pip_id.id != 0

	mut desc := unsafe { *in_desc }
	desc.layout.buffers[0].stride = int(sizeof(Vertex))

	desc.layout.attrs[0].offset = 0 // pos: offset 0
	desc.layout.attrs[0].format = .float3
	desc.layout.attrs[1].offset = int(sizeof([3]f32)) // uv: after pos (3 floats = 12 bytes)
	desc.layout.attrs[1].format = .float2
	desc.layout.attrs[2].offset = int(sizeof([3]f32) + sizeof([2]f32)) // rgba: after uv (5 floats = 20 bytes)
	desc.layout.attrs[2].format = .ubyte4n
	desc.layout.attrs[3].offset = int(sizeof([3]f32) + sizeof([2]f32) + sizeof(u32)) // psize: after rgba (24 bytes)
	desc.layout.attrs[3].format = .float

	if in_desc.shader.id == 0 {
		desc.shader = sgl_state.shd
	}
	desc.index_type = .none
	desc.sample_count = ctx_desc.sample_count
	if int(desc.face_winding) == 0 {
		desc.face_winding = sgl_state.desc.face_winding
	}
	desc.depth.pixel_format = ctx_desc.depth_format
	if ctx_desc.depth_format == .none {
		desc.depth.write_enabled = false
	}
	desc.colors[0].pixel_format = ctx_desc.color_format
	if int(desc.colors[0].write_mask) == 0 {
		desc.colors[0].write_mask = .rgb
	}

	pip := lookup_pipeline(pip_id.id)
	assert pip != unsafe { nil }
	assert pip.slot.state == .alloc
	unsafe {
		mut p := pip
		p.slot.state = .valid
		for i in 0 .. num_primitive_types {
			prim := PrimitiveType(i)
			match prim {
				.points { desc.primitive_type = .points }
				.lines { desc.primitive_type = .lines }
				.line_strip { desc.primitive_type = .line_strip }
				.triangles { desc.primitive_type = .triangles }
				.triangle_strip { desc.primitive_type = .triangle_strip }
				.quads { desc.primitive_type = .triangles }
			}
			if prim == .quads {
				// quads emulated as triangles, reuse same pipeline
				p.pip[i] = p.pip[int(PrimitiveType.triangles)]
			} else {
				p.pip[i] = gfx.make_pipeline(&desc)
			}
		}
	}
}

fn internal_make_pipeline(desc &gfx.PipelineDesc, ctx_desc &ContextDesc) Pipeline {
	pip_id := alloc_pipeline()
	if pip_id.id != 0 {
		init_pipeline(pip_id, desc, ctx_desc)
	}
	return pip_id
}

@[direct_array_access]
fn internal_destroy_pipeline(pip_id Pipeline) {
	pip := lookup_pipeline(pip_id.id)
	if pip != unsafe { nil } {
		C.sg_push_debug_group(c'sokol-gl')
		unsafe {
			mut p := pip
			for i in 0 .. num_primitive_types {
				if i != int(PrimitiveType.quads) {
					gfx.destroy_pipeline(p.pip[i])
				}
			}
			p.slot = Slot{}
		}
		C.sg_pop_debug_group()
		pool_free_index(mut sgl_state.pip_pool.pool, slot_index(pip_id.id))
	}
}

fn get_pipeline(pip Pipeline, prim_type PrimitiveType) gfx.Pipeline {
	p := lookup_pipeline(pip.id)
	if p != unsafe { nil } {
		return p.pip[int(prim_type)]
	}
	return gfx.Pipeline{}
}

// ========================
//   Context Management
// ========================

fn setup_context_pool(pool_size int) {
	init_pool(mut sgl_state.context_pool.pool, pool_size)
	pool_byte_size := usize(sgl_state.context_pool.pool.size) * sizeof(ContextInternal)
	sgl_state.context_pool.contexts = unsafe { &ContextInternal(C.calloc(1, pool_byte_size)) }
}

fn discard_context_pool() {
	unsafe { C.free(sgl_state.context_pool.contexts) }
	sgl_state.context_pool.contexts = unsafe { nil }
	discard_pool(mut sgl_state.context_pool.pool)
}

fn context_at(ctx_id u32) &ContextInternal {
	idx := slot_index(ctx_id)
	return unsafe { &sgl_state.context_pool.contexts[idx] }
}

fn lookup_context(ctx_id u32) &ContextInternal {
	if ctx_id != 0 {
		ctx := context_at(ctx_id)
		if ctx.slot.id == ctx_id {
			return ctx
		}
	}
	return unsafe { nil }
}

fn alloc_context() Context {
	idx := pool_alloc_index(mut sgl_state.context_pool.pool)
	if idx != invalid_slot_index {
		mut s := unsafe { &sgl_state.context_pool.contexts[idx].slot }
		id := slot_alloc(mut sgl_state.context_pool.pool, mut s, idx)
		return Context{id}
	}
	return Context{0}
}

fn context_desc_defaults(desc &ContextDesc) ContextDesc {
	mut res := unsafe { *desc }
	if res.max_vertices == 0 {
		res.max_vertices = default_max_vertices
	}
	if res.max_commands == 0 {
		res.max_commands = default_max_commands
	}
	return res
}

fn as_context_desc(desc &Desc) ContextDesc {
	return ContextDesc{
		max_vertices: desc.max_vertices
		max_commands: desc.max_commands
		color_format: desc.color_format
		depth_format: desc.depth_format
		sample_count: desc.sample_count
	}
}

fn commit_listener_cb(userdata voidptr) {
	ctx_id := u32(usize(userdata))
	ctx := lookup_context(ctx_id)
	if ctx != unsafe { nil } {
		rewind_context(ctx)
	}
}

fn make_commit_listener(ctx &ContextInternal) gfx.CommitListener {
	return gfx.CommitListener{
		func:      commit_listener_cb
		user_data: voidptr(usize(ctx.slot.id))
	}
}

@[direct_array_access]
fn init_context(ctx_id Context, in_desc &ContextDesc) {
	ctx := lookup_context(ctx_id.id)
	if ctx == unsafe { nil } {
		return
	}
	unsafe {
		mut c := ctx
		c.desc = context_desc_defaults(in_desc)
		c.frame_id = 1
		c.cur_img = sgl_state.def_img
		c.cur_smp = sgl_state.def_smp

		// allocate buffers
		c.vertices_cap = c.desc.max_vertices
		c.uniforms_cap = c.desc.max_commands
		c.commands_cap = c.desc.max_commands
		c.vertices_ptr = &Vertex(C.calloc(usize(c.vertices_cap), sizeof(Vertex)))
		c.uniforms_ptr = &Uniform(C.calloc(usize(c.uniforms_cap), sizeof(Uniform)))
		c.commands_ptr = &Command(C.calloc(usize(c.commands_cap), sizeof(Command)))

		// create sokol-gfx resources
		C.sg_push_debug_group(c'sokol-gl')
		vbuf_desc := gfx.BufferDesc{
			@type: .vertexbuffer
			usage: .stream
			size:  usize(c.vertices_cap) * sizeof(Vertex)
			label: c'sgl-vertex-buffer'
		}
		c.vbuf = gfx.make_buffer(&vbuf_desc)
		c.bind.vertex_buffers[0] = c.vbuf

		def_pip_desc := gfx.PipelineDesc{
			depth: gfx.DepthState{
				write_enabled: true
			}
		}
		c.def_pip = internal_make_pipeline(&def_pip_desc, &c.desc)
		C.sg_add_commit_listener(make_commit_listener(ctx))
		C.sg_pop_debug_group()

		// initialize state
		c.rgba = 0xFFFFFFFF
		c.point_size = 1.0
		for i in 0 .. num_matrixmodes {
			mat4_identity(mut c.matrix_stack[i][0])
		}
		c.pip_stack[0] = c.def_pip
		c.matrix_dirty = true
	}
}

fn internal_make_context(desc &ContextDesc) Context {
	ctx_id := alloc_context()
	if ctx_id.id != 0 {
		init_context(ctx_id, desc)
	}
	return ctx_id
}

fn internal_destroy_context(ctx_id Context) {
	ctx := lookup_context(ctx_id.id)
	if ctx != unsafe { nil } {
		unsafe {
			C.free(ctx.vertices_ptr)
			C.free(ctx.uniforms_ptr)
			C.free(ctx.commands_ptr)
			mut c := ctx
			c.vertices_ptr = nil
			c.uniforms_ptr = nil
			c.commands_ptr = nil
		}
		C.sg_push_debug_group(c'sokol-gl')
		gfx.destroy_buffer(ctx.vbuf)
		internal_destroy_pipeline(ctx.def_pip)
		C.sg_remove_commit_listener(make_commit_listener(ctx))
		C.sg_pop_debug_group()

		unsafe {
			mut c := ctx
			c.slot = Slot{}
		}
		pool_free_index(mut sgl_state.context_pool.pool, slot_index(ctx_id.id))
	}
}

// ========================
//   Common Resources
// ========================

@[direct_array_access]
fn setup_common() {
	// 8x8 white default texture
	mut pixels := [64]u32{}
	for i in 0 .. 64 {
		pixels[i] = 0xFFFFFFFF
	}
	mut img_desc := gfx.ImageDesc{
		@type:        ._2d
		width:        8
		height:       8
		num_mipmaps:  1
		pixel_format: .rgba8
		label:        c'sgl-default-texture'
	}
	img_desc.data.subimage[0][0] = gfx.Range{
		ptr:  unsafe { &pixels[0] }
		size: sizeof([64]u32)
	}
	sgl_state.def_img = gfx.make_image(&img_desc)

	// default sampler (nearest filtering)
	smp_desc := gfx.SamplerDesc{
		min_filter: .nearest
		mag_filter: .nearest
	}
	sgl_state.def_smp = gfx.make_sampler(&smp_desc)

	// create shader
	shd_desc := make_shader_desc()
	sgl_state.shd = gfx.make_shader(&shd_desc)
}

fn discard_common() {
	gfx.destroy_shader(sgl_state.shd)
	gfx.destroy_sampler(sgl_state.def_smp)
	gfx.destroy_image(sgl_state.def_img)
}

// ========================
//   Vertex / Command Helpers
// ========================

fn next_vertex(ctx &ContextInternal) &Vertex {
	unsafe {
		mut c := ctx
		if c.vertices_next < c.vertices_cap {
			vtx := &c.vertices_ptr[c.vertices_next]
			c.vertices_next++
			return vtx
		} else {
			c.error = .vertices_full
			return nil
		}
	}
}

fn next_uniform(ctx &ContextInternal) &Uniform {
	unsafe {
		mut c := ctx
		if c.uniforms_next < c.uniforms_cap {
			uni := &c.uniforms_ptr[c.uniforms_next]
			c.uniforms_next++
			return uni
		} else {
			c.error = .uniforms_full
			return nil
		}
	}
}

fn next_command(ctx &ContextInternal) &Command {
	unsafe {
		mut c := ctx
		if c.commands_next < c.commands_cap {
			cmd := &c.commands_ptr[c.commands_next]
			c.commands_next++
			return cmd
		} else {
			c.error = .commands_full
			return nil
		}
	}
}

fn cur_command(ctx &ContextInternal) &Command {
	if ctx.commands_next > 0 {
		return unsafe { &ctx.commands_ptr[ctx.commands_next - 1] }
	}
	return unsafe { nil }
}

@[inline]
fn next_draw_chunk(base_vertex int, remaining_vertices int, max_vertices int) (int, int) {
	chunk_vertices := if max_vertices > 0 && remaining_vertices > max_vertices {
		max_vertices
	} else {
		remaining_vertices
	}
	return base_vertex, chunk_vertices
}

@[direct_array_access]
fn vtx(ctx &ContextInternal, x f32, y f32, z f32, u_ f32, v_ f32, rgba_ u32) {
	unsafe {
		mut c := ctx
		// quad emulation: before 4th vertex, duplicate 1st and 3rd to form second triangle
		if c.cur_prim_type == .quads && (c.vtx_count & 3) == 3 {
			// save vertex data before next_vertex increments vertices_next
			v0 := c.vertices_ptr[c.vertices_next - 3]
			v2 := c.vertices_ptr[c.vertices_next - 1]
			vtx1 := next_vertex(ctx)
			if vtx1 != nil {
				*vtx1 = v0
			}
			vtx2 := next_vertex(ctx)
			if vtx2 != nil {
				*vtx2 = v2
			}
		}
		v := next_vertex(ctx)
		if v != nil {
			v.pos[0] = x
			v.pos[1] = y
			v.pos[2] = z
			v.uv[0] = u_
			v.uv[1] = v_
			v.rgba = rgba_
			v.psize = c.point_size
		}
		c.vtx_count++
	}
}

fn rewind_context(ctx &ContextInternal) {
	unsafe {
		mut c := ctx
		c.frame_id++
		c.vertices_next = 0
		c.uniforms_next = 0
		c.commands_next = 0
		c.base_vertex = 0
		c.error = .no_error
		c.layer_id = 0
		c.matrix_dirty = true
	}
}

fn begin_(ctx &ContextInternal, mode PrimitiveType) {
	unsafe {
		mut c := ctx
		c.in_begin = true
		c.base_vertex = c.vertices_next
		c.vtx_count = 0
		c.cur_prim_type = mode
	}
}

fn desc_defaults(desc &Desc) Desc {
	mut res := unsafe { *desc }
	if res.max_vertices == 0 {
		res.max_vertices = default_max_vertices
	}
	if res.max_commands == 0 {
		res.max_commands = default_max_commands
	}
	if res.context_pool_size == 0 {
		res.context_pool_size = default_context_pool_size
	}
	if res.pipeline_pool_size == 0 {
		res.pipeline_pool_size = default_pipeline_pool_size
	}
	if int(res.face_winding) == 0 {
		res.face_winding = .ccw
	}
	return res
}

// ========================
//   Draw (command playback)
// ========================

@[direct_array_access]
fn internal_draw(ctx &ContextInternal, layer_id int) {
	if ctx.error != .no_error || ctx.vertices_next == 0 || ctx.commands_next == 0 {
		return
	}
	C.sg_push_debug_group(c'sokol-gl')

	mut cur_pip_id := u32(0)
	mut cur_img_id := u32(0)
	mut cur_smp_id := u32(0)
	mut cur_uniform_index := -1

	unsafe {
		mut c := ctx
		if c.update_frame_id != c.frame_id {
			c.update_frame_id = c.frame_id
			range := gfx.Range{
				ptr:  c.vertices_ptr
				size: usize(c.vertices_next) * sizeof(Vertex)
			}
			gfx.update_buffer(c.vbuf, &range)
		}
	}
	for i in 0 .. ctx.commands_next {
		cmd := unsafe { &ctx.commands_ptr[i] }
		if cmd.layer_id != layer_id {
			continue
		}
		match cmd.cmd {
			.viewport {
				args := unsafe { &cmd.args.viewport }
				gfx.apply_viewport(args.x, args.y, args.w, args.h, args.origin_top_left)
			}
			.scissor_rect {
				args := unsafe { &cmd.args.scissor_rect }
				gfx.apply_scissor_rect(args.x, args.y, args.w, args.h, args.origin_top_left)
			}
			.draw {
				args := unsafe { &cmd.args.draw }
				if args.pip.id != cur_pip_id {
					gfx.apply_pipeline(args.pip)
					cur_pip_id = args.pip.id
					// force rebind after pipeline change
					cur_img_id = 0
					cur_smp_id = 0
					cur_uniform_index = -1
				}
				if args.img.id != cur_img_id || args.smp.id != cur_smp_id {
					unsafe {
						mut c := ctx
						c.bind.fs.images[0] = args.img
						c.bind.fs.samplers[0] = args.smp
					}
					gfx.apply_bindings(&ctx.bind)
					cur_img_id = args.img.id
					cur_smp_id = args.smp.id
				}
				if cur_uniform_index != args.uniform_index {
					ub_range := gfx.Range{
						ptr:  unsafe { &ctx.uniforms_ptr[args.uniform_index] }
						size: sizeof(Uniform)
					}
					gfx.apply_uniforms(.vs, 0, &ub_range)
					cur_uniform_index = args.uniform_index
				}
				if args.num_vertices > 0 {
					if args.max_vertices > 0 && args.num_vertices > args.max_vertices {
						mut base_vertex := args.base_vertex
						mut remaining_vertices := args.num_vertices
						for remaining_vertices > 0 {
							chunk_base_vertex, chunk_vertices := next_draw_chunk(base_vertex,
								remaining_vertices, args.max_vertices)
							gfx.draw(chunk_base_vertex, chunk_vertices, 1)
							base_vertex += chunk_vertices
							remaining_vertices -= chunk_vertices
						}
					} else {
						gfx.draw(args.base_vertex, args.num_vertices, 1)
					}
				}
			}
		}
	}
	C.sg_pop_debug_group()
}

// ========================
//     PUBLIC API
// ========================

// setup/shutdown

pub fn setup(desc &Desc) {
	if desc.allocator.alloc_fn == unsafe { nil } && desc.allocator.free_fn == unsafe { nil } {
		unsafe {
			desc.allocator.alloc_fn = memory.salloc
			desc.allocator.free_fn = memory.sfree
			desc.allocator.user_data = voidptr(0x10000561)
		}
	}
	if desc.logger.func == unsafe { nil } {
		unsafe {
			desc.logger.func = memory.slog
		}
	}
	unsafe { C.memset(&sgl_state, 0, sizeof(SglState)) }
	sgl_state.init_cookie = init_cookie
	sgl_state.desc = desc_defaults(desc)
	setup_pipeline_pool(sgl_state.desc.pipeline_pool_size)
	setup_context_pool(sgl_state.desc.context_pool_size)
	setup_common()
	ctx_desc := as_context_desc(&sgl_state.desc)
	sgl_state.def_ctx_id = internal_make_context(&ctx_desc)
	set_context(sgl_state.def_ctx_id)
}

pub fn shutdown() {
	// destroy contexts first (they own pipelines)
	for i in 0 .. sgl_state.context_pool.pool.size {
		ctx := unsafe { &sgl_state.context_pool.contexts[i] }
		internal_destroy_context(Context{ctx.slot.id})
	}
	for i in 0 .. sgl_state.pip_pool.pool.size {
		pip := unsafe { &sgl_state.pip_pool.pips[i] }
		internal_destroy_pipeline(Pipeline{pip.slot.id})
	}
	discard_context_pool()
	discard_pipeline_pool()
	discard_common()
	sgl_state.init_cookie = 0
}

@[inline]
pub fn error() SglError {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		return ctx.error
	}
	return .no_context
}

@[inline]
pub fn context_error(ctx_handle Context) SglError {
	ctx := lookup_context(ctx_handle.id)
	if ctx != unsafe { nil } {
		return ctx.error
	}
	return .no_context
}

@[inline]
pub fn rad(deg f32) f32 {
	return deg * (math.pi / 180.0)
}

@[inline]
pub fn deg(r f32) f32 {
	return r * (180.0 / math.pi)
}

// context functions
@[inline]
pub fn make_context(desc &ContextDesc) Context {
	return internal_make_context(desc)
}

@[inline]
pub fn destroy_context(ctx Context) {
	internal_destroy_context(ctx)
}

@[inline]
pub fn set_context(ctx Context) {
	sgl_state.cur_ctx_id = ctx
	sgl_state.cur_ctx = lookup_context(ctx.id)
}

@[inline]
pub fn get_context() Context {
	return sgl_state.cur_ctx_id
}

@[inline]
pub fn default_context() Context {
	return sgl_state.def_ctx_id
}

// pipeline functions
@[inline]
pub fn make_pipeline(desc &gfx.PipelineDesc) Pipeline {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		return internal_make_pipeline(desc, &ctx.desc)
	}
	return Pipeline{0}
}

@[inline]
pub fn context_make_pipeline(ctx_handle Context, desc &gfx.PipelineDesc) Pipeline {
	ctx := lookup_context(ctx_handle.id)
	if ctx != unsafe { nil } {
		return internal_make_pipeline(desc, &ctx.desc)
	}
	return Pipeline{0}
}

@[inline]
pub fn destroy_pipeline(pip Pipeline) {
	internal_destroy_pipeline(pip)
}

// render state
pub fn defaults() {
	ctx := sgl_state.cur_ctx
	if ctx == unsafe { nil } {
		return
	}
	unsafe {
		mut c := ctx
		c.u = 0.0
		c.v = 0.0
		c.rgba = 0xFFFFFFFF
		c.point_size = 1.0
		c.texturing_enabled = false
		c.cur_img = sgl_state.def_img
		c.cur_smp = sgl_state.def_smp
	}
	load_default_pipeline()
	unsafe {
		mut c := ctx
		mat4_identity(mut c.matrix_stack[int(MatrixMode.texture)][c.matrix_tos[int(MatrixMode.texture)]])
		mat4_identity(mut c.matrix_stack[int(MatrixMode.modelview)][c.matrix_tos[int(MatrixMode.modelview)]])
		mat4_identity(mut c.matrix_stack[int(MatrixMode.projection)][c.matrix_tos[int(MatrixMode.projection)]])
		c.cur_matrix_mode = .modelview
		c.matrix_dirty = true
	}
}

@[inline]
pub fn viewport(x int, y int, w int, h int, origin_top_left bool) {
	ctx := sgl_state.cur_ctx
	if ctx == unsafe { nil } {
		return
	}
	cmd := next_command(ctx)
	if cmd != unsafe { nil } {
		unsafe {
			mut c := cmd
			c.cmd = .viewport
			c.layer_id = ctx.layer_id
			mut a := &ViewportArgs(&c.args)
			a.x = x
			a.y = y
			a.w = w
			a.h = h
			a.origin_top_left = origin_top_left
		}
	}
}

@[inline]
pub fn viewportf(x f32, y f32, w f32, h f32, origin_top_left bool) {
	viewport(int(x), int(y), int(w), int(h), origin_top_left)
}

@[inline]
pub fn scissor_rect(x int, y int, w int, h int, origin_top_left bool) {
	ctx := sgl_state.cur_ctx
	if ctx == unsafe { nil } {
		return
	}
	cmd := next_command(ctx)
	if cmd != unsafe { nil } {
		unsafe {
			mut c := cmd
			c.cmd = .scissor_rect
			c.layer_id = ctx.layer_id
			mut a := &ScissorRectArgs(&c.args)
			a.x = x
			a.y = y
			a.w = w
			a.h = h
			a.origin_top_left = origin_top_left
		}
	}
}

@[inline]
pub fn scissor_rectf(x f32, y f32, w f32, h f32, origin_top_left bool) {
	scissor_rect(int(x), int(y), int(w), int(h), origin_top_left)
}

@[inline]
pub fn enable_texture() {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.texturing_enabled = true
		}
	}
}

@[inline]
pub fn disable_texture() {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.texturing_enabled = false
		}
	}
}

@[inline]
pub fn texture(img gfx.Image, smp gfx.Sampler) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.cur_img = img
			c.cur_smp = smp
		}
	}
}

@[inline]
pub fn layer(layer_id int) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.layer_id = layer_id
		}
	}
}

// pipeline stack

@[inline]
pub fn load_default_pipeline() {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.pip_stack[c.pip_tos] = c.def_pip
			c.matrix_dirty = true
		}
	}
}

@[inline]
pub fn default_pipeline() {
	load_default_pipeline()
}

@[inline]
pub fn load_pipeline(pip Pipeline) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.pip_stack[c.pip_tos] = pip
			c.matrix_dirty = true
		}
	}
}

@[inline]
pub fn push_pipeline() {
	ctx := sgl_state.cur_ctx
	if ctx == unsafe { nil } {
		return
	}
	unsafe {
		mut c := ctx
		if c.pip_tos < max_stack_depth - 1 {
			c.pip_tos++
			c.pip_stack[c.pip_tos] = c.pip_stack[c.pip_tos - 1]
		} else {
			c.error = .stack_overflow
		}
	}
}

@[inline]
pub fn pop_pipeline() {
	ctx := sgl_state.cur_ctx
	if ctx == unsafe { nil } {
		return
	}
	unsafe {
		mut c := ctx
		if c.pip_tos > 0 {
			c.pip_tos--
		} else {
			c.error = .stack_underflow
		}
	}
}

// matrix functions

@[inline]
pub fn matrix_mode_modelview() {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.cur_matrix_mode = .modelview
		}
	}
}

@[inline]
pub fn matrix_mode_projection() {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.cur_matrix_mode = .projection
		}
	}
}

@[inline]
pub fn matrix_mode_texture() {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.cur_matrix_mode = .texture
		}
	}
}

@[inline]
pub fn load_identity() {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.matrix_dirty = true
			mut m := c.current_matrix_mut()
			mat4_identity(mut m)
		}
	}
}

@[direct_array_access; inline]
pub fn load_matrix(m []f32) {
	ctx := sgl_state.cur_ctx
	if ctx == unsafe { nil } || m.len < 16 {
		return
	}
	unsafe {
		mut c := ctx
		c.matrix_dirty = true
		mut dst := c.current_matrix_mut()
		for col in 0 .. 4 {
			for row in 0 .. 4 {
				dst.v[col][row] = m[col * 4 + row]
			}
		}
	}
}

@[direct_array_access; inline]
pub fn load_transpose_matrix(m []f32) {
	ctx := sgl_state.cur_ctx
	if ctx == unsafe { nil } || m.len < 16 {
		return
	}
	unsafe {
		mut c := ctx
		c.matrix_dirty = true
		mut dst := c.current_matrix_mut()
		for col in 0 .. 4 {
			for row in 0 .. 4 {
				dst.v[col][row] = m[row * 4 + col]
			}
		}
	}
}

@[direct_array_access; inline]
pub fn mult_matrix(m []f32) {
	ctx := sgl_state.cur_ctx
	if ctx == unsafe { nil } || m.len < 16 {
		return
	}
	unsafe {
		mut c := ctx
		c.matrix_dirty = true
		mut src := Matrix{}
		for col in 0 .. 4 {
			for row in 0 .. 4 {
				src.v[col][row] = m[col * 4 + row]
			}
		}
		mut dst := c.current_matrix_mut()
		mat4_mul_inplace(mut dst, &src)
	}
}

@[direct_array_access; inline]
pub fn mult_transpose_matrix(m []f32) {
	ctx := sgl_state.cur_ctx
	if ctx == unsafe { nil } || m.len < 16 {
		return
	}
	unsafe {
		mut c := ctx
		c.matrix_dirty = true
		mut src := Matrix{}
		for col in 0 .. 4 {
			for row in 0 .. 4 {
				src.v[col][row] = m[row * 4 + col]
			}
		}
		mut dst := c.current_matrix_mut()
		mat4_mul_inplace(mut dst, &src)
	}
}

@[inline]
pub fn rotate(angle_rad f32, x f32, y f32, z f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.matrix_dirty = true
			mut m := c.current_matrix_mut()
			mat4_rotate(mut m, angle_rad, x, y, z)
		}
	}
}

@[inline]
pub fn scale(x f32, y f32, z f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.matrix_dirty = true
			mut m := c.current_matrix_mut()
			mat4_scale(mut m, x, y, z)
		}
	}
}

@[inline]
pub fn translate(x f32, y f32, z f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.matrix_dirty = true
			mut m := c.current_matrix_mut()
			mat4_translate(mut m, x, y, z)
		}
	}
}

@[inline]
pub fn frustum(l f32, r f32, b f32, t f32, n f32, f f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.matrix_dirty = true
			mut m := c.current_matrix_mut()
			mat4_frustum(mut m, l, r, b, t, n, f)
		}
	}
}

@[inline]
pub fn ortho(l f32, r f32, b f32, t f32, n f32, f f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.matrix_dirty = true
			mut m := c.current_matrix_mut()
			mat4_ortho(mut m, l, r, b, t, n, f)
		}
	}
}

@[inline]
pub fn perspective(fov_y f32, aspect f32, z_near f32, z_far f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.matrix_dirty = true
			mut m := c.current_matrix_mut()
			mat4_perspective(mut m, fov_y, aspect, z_near, z_far)
		}
	}
}

@[inline]
pub fn lookat(eye_x f32, eye_y f32, eye_z f32, center_x f32, center_y f32, center_z f32, up_x f32, up_y f32, up_z f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.matrix_dirty = true
			mut m := c.current_matrix_mut()
			mat4_lookat(mut m, eye_x, eye_y, eye_z, center_x, center_y, center_z, up_x,
				up_y, up_z)
		}
	}
}

@[inline]
pub fn push_matrix() {
	ctx := sgl_state.cur_ctx
	if ctx == unsafe { nil } {
		return
	}
	unsafe {
		mut c := ctx
		mode := int(c.cur_matrix_mode)
		if c.matrix_tos[mode] < max_stack_depth - 1 {
			c.matrix_tos[mode]++
			c.matrix_stack[mode][c.matrix_tos[mode]] = c.matrix_stack[mode][c.matrix_tos[mode] - 1]
		} else {
			c.error = .stack_overflow
		}
	}
}

@[inline]
pub fn pop_matrix() {
	ctx := sgl_state.cur_ctx
	if ctx == unsafe { nil } {
		return
	}
	unsafe {
		mut c := ctx
		mode := int(c.cur_matrix_mode)
		if c.matrix_tos[mode] > 0 {
			c.matrix_tos[mode]--
			c.matrix_dirty = true
		} else {
			c.error = .stack_underflow
		}
	}
}

// texcoord / color / point size

@[inline]
pub fn t2f(u f32, v f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.u = u
			c.v = v
		}
	}
}

// c3f sets the current color using RGB float values (0.0-1.0).
@[inline]
pub fn c3f(r f32, g f32, b f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.rgba = pack_rgbaf(r, g, b, 1.0)
		}
	}
}

// c4f sets the current color using RGBA float values (0.0-1.0).
@[inline]
pub fn c4f(r f32, g f32, b f32, a f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.rgba = pack_rgbaf(r, g, b, a)
		}
	}
}

// c3b sets the current color using RGB byte values (0-255).
@[inline]
pub fn c3b(r u8, g u8, b u8) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.rgba = pack_rgba(r, g, b, 255)
		}
	}
}

// c4b sets the current color using RGBA byte values (0-255).
@[inline]
pub fn c4b(r u8, g u8, b u8, a u8) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.rgba = pack_rgba(r, g, b, a)
		}
	}
}

// c1i sets the current color using a packed RGBA u32 value.
@[inline]
pub fn c1i(rgba u32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.rgba = rgba
		}
	}
}

// point_size sets the size of points when drawing point primitives.
@[inline]
pub fn point_size(s f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		unsafe {
			mut c := ctx
			c.point_size = s
		}
	}
}

// color packing helpers

@[inline]
fn pack_rgba(r u8, g u8, b u8, a u8) u32 {
	return u32(r) | (u32(g) << 8) | (u32(b) << 16) | (u32(a) << 24)
}

@[inline]
fn pack_rgbaf(r f32, g f32, b f32, a f32) u32 {
	r_ := u8(r * 255.0)
	g_ := u8(g * 255.0)
	b_ := u8(b * 255.0)
	a_ := u8(a * 255.0)
	return pack_rgba(r_, g_, b_, a_)
}

// primitives

// begin_points begins drawing points.
@[inline]
pub fn begin_points() {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		begin_(ctx, .points)
	}
}

// begin_lines begins drawing lines.
@[inline]
pub fn begin_lines() {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		begin_(ctx, .lines)
	}
}

// begin_line_strip begins drawing a line strip (connected lines).
@[inline]
pub fn begin_line_strip() {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		begin_(ctx, .line_strip)
	}
}

// begin_triangles begins drawing triangles.
@[inline]
pub fn begin_triangles() {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		begin_(ctx, .triangles)
	}
}

// begin_triangle_strip begins drawing a triangle strip.
@[inline]
pub fn begin_triangle_strip() {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		begin_(ctx, .triangle_strip)
	}
}

// begin_quads begins drawing quads.
@[inline]
pub fn begin_quads() {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		begin_(ctx, .quads)
	}
}

// vertex submission

// v2f submits a 2D vertex position.
@[inline]
pub fn v2f(x f32, y f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, 0.0, ctx.u, ctx.v, ctx.rgba)
	}
}

// v3f submits a 3D vertex position.
@[inline]
pub fn v3f(x f32, y f32, z f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, z, ctx.u, ctx.v, ctx.rgba)
	}
}

// v2f_t2f submits a 2D vertex with texture coordinates.
@[inline]
pub fn v2f_t2f(x f32, y f32, u f32, v f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, 0.0, u, v, ctx.rgba)
	}
}

// v3f_t2f submits a 3D vertex with texture coordinates.
@[inline]
pub fn v3f_t2f(x f32, y f32, z f32, u f32, v f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, z, u, v, ctx.rgba)
	}
}

// v2f_c3f submits a 2D vertex with RGB color (float).
@[inline]
pub fn v2f_c3f(x f32, y f32, r f32, g f32, b f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, 0.0, ctx.u, ctx.v, pack_rgbaf(r, g, b, 1.0))
	}
}

// v2f_c3b submits a 2D vertex with RGB color (bytes).
@[inline]
pub fn v2f_c3b(x f32, y f32, r u8, g u8, b u8) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, 0.0, ctx.u, ctx.v, pack_rgba(r, g, b, 255))
	}
}

// v2f_c4f submits a 2D vertex with RGBA color (float).
@[inline]
pub fn v2f_c4f(x f32, y f32, r f32, g f32, b f32, a f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, 0.0, ctx.u, ctx.v, pack_rgbaf(r, g, b, a))
	}
}

// v2f_c4b submits a 2D vertex with RGBA color (bytes).
@[inline]
pub fn v2f_c4b(x f32, y f32, r u8, g u8, b u8, a u8) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, 0.0, ctx.u, ctx.v, pack_rgba(r, g, b, a))
	}
}

// v2f_c1i submits a 2D vertex with packed RGBA color.
@[inline]
pub fn v2f_c1i(x f32, y f32, rgba u32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, 0.0, ctx.u, ctx.v, rgba)
	}
}

// v3f_c3f submits a 3D vertex with RGB color (float).
@[inline]
pub fn v3f_c3f(x f32, y f32, z f32, r f32, g f32, b f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, z, ctx.u, ctx.v, pack_rgbaf(r, g, b, 1.0))
	}
}

// v3f_c3b submits a 3D vertex with RGB color (bytes).
@[inline]
pub fn v3f_c3b(x f32, y f32, z f32, r u8, g u8, b u8) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, z, ctx.u, ctx.v, pack_rgba(r, g, b, 255))
	}
}

@[inline]
pub fn v3f_c4f(x f32, y f32, z f32, r f32, g f32, b f32, a f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, z, ctx.u, ctx.v, pack_rgbaf(r, g, b, a))
	}
}

@[inline]
pub fn v3f_c4b(x f32, y f32, z f32, r u8, g u8, b u8, a u8) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, z, ctx.u, ctx.v, pack_rgba(r, g, b, a))
	}
}

@[inline]
pub fn v3f_c1i(x f32, y f32, z f32, rgba u32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, z, ctx.u, ctx.v, rgba)
	}
}

@[inline]
pub fn v2f_t2f_c3f(x f32, y f32, u f32, v f32, r f32, g f32, b f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, 0.0, u, v, pack_rgbaf(r, g, b, 1.0))
	}
}

@[inline]
pub fn v2f_t2f_c3b(x f32, y f32, u f32, v f32, r u8, g u8, b u8) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, 0.0, u, v, pack_rgba(r, g, b, 255))
	}
}

@[inline]
pub fn v2f_t2f_c4f(x f32, y f32, u f32, v f32, r f32, g f32, b f32, a f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, 0.0, u, v, pack_rgbaf(r, g, b, a))
	}
}

@[inline]
pub fn v2f_t2f_c4b(x f32, y f32, u f32, v f32, r u8, g u8, b u8, a u8) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, 0.0, u, v, pack_rgba(r, g, b, a))
	}
}

@[inline]
pub fn v2f_t2f_c1i(x f32, y f32, u f32, v f32, rgba u32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, 0.0, u, v, rgba)
	}
}

@[inline]
pub fn v3f_t2f_c3f(x f32, y f32, z f32, u f32, v f32, r f32, g f32, b f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, z, u, v, pack_rgbaf(r, g, b, 1.0))
	}
}

@[inline]
pub fn v3f_t2f_c3b(x f32, y f32, z f32, u f32, v f32, r u8, g u8, b u8) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, z, u, v, pack_rgba(r, g, b, 255))
	}
}

@[inline]
pub fn v3f_t2f_c4f(x f32, y f32, z f32, u f32, v f32, r f32, g f32, b f32, a f32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, z, u, v, pack_rgbaf(r, g, b, a))
	}
}

@[inline]
pub fn v3f_t2f_c4b(x f32, y f32, z f32, u f32, v f32, r u8, g u8, b u8, a u8) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, z, u, v, pack_rgba(r, g, b, a))
	}
}

@[inline]
pub fn v3f_t2f_c1i(x f32, y f32, z f32, u f32, v f32, rgba u32) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		vtx(ctx, x, y, z, u, v, rgba)
	}
}

// end primitive and record draw command

pub fn end() {
	ctx := sgl_state.cur_ctx
	if ctx == unsafe { nil } {
		return
	}
	assert ctx.in_begin

	unsafe {
		mut c := ctx
		c.in_begin = false
	}
	matrix_dirty := ctx.matrix_dirty
	if matrix_dirty {
		unsafe {
			mut c := ctx
			c.matrix_dirty = false
		}
		uni := next_uniform(ctx)
		if uni != unsafe { nil } {
			unsafe {
				mut u := uni
				mat4_mul(mut u.mvp, ctx.matrix_projection(), ctx.matrix_modelview())
				u.tm = *ctx.matrix_texture()
			}
		}
	}

	// check if command can be merged with current command
	pip := get_pipeline(ctx.pip_stack[ctx.pip_tos], ctx.cur_prim_type)
	img := if ctx.texturing_enabled { ctx.cur_img } else { sgl_state.def_img }
	smp := if ctx.texturing_enabled { ctx.cur_smp } else { sgl_state.def_smp }

	prev_cmd := cur_command(ctx)
	mut merge := false
	if prev_cmd != unsafe { nil } {
		prev_draw := unsafe { &prev_cmd.args.draw }
		if prev_cmd.cmd == .draw && prev_cmd.layer_id == ctx.layer_id
			&& ctx.cur_prim_type != .line_strip && ctx.cur_prim_type != .triangle_strip
			&& !matrix_dirty && prev_draw.img.id == img.id && prev_draw.smp.id == smp.id
			&& prev_draw.pip.id == pip.id {
			merge = true
		}
	}

	if merge {
		// extend previous draw command
		unsafe {
			mut draw_args := &DrawArgs(&prev_cmd.args)
			draw_args.num_vertices += ctx.vertices_next - ctx.base_vertex
		}
	} else {
		// new draw command
		cmd := next_command(ctx)
		if cmd != unsafe { nil } {
			assert ctx.uniforms_next > 0
			unsafe {
				mut c := cmd
				c.cmd = .draw
				c.layer_id = ctx.layer_id
				mut a := &DrawArgs(&c.args)
				a.img = img
				a.smp = smp
				a.pip = pip
				a.base_vertex = ctx.base_vertex
				a.num_vertices = ctx.vertices_next - ctx.base_vertex
				a.uniform_index = ctx.uniforms_next - 1
				a.max_vertices = if ctx.cur_prim_type == .points {
					max_point_batch_vertices
				} else {
					0
				}
			}
		}
	}
}

// render recorded commands

@[inline]
pub fn draw() {
	draw_layer(0)
}

@[inline]
pub fn draw_layer(layer_id int) {
	ctx := sgl_state.cur_ctx
	if ctx != unsafe { nil } {
		internal_draw(ctx, layer_id)
	}
}

@[inline]
pub fn context_draw(ctx_handle Context) {
	context_draw_layer(ctx_handle, 0)
}

@[inline]
pub fn context_draw_layer(ctx_handle Context, layer_id int) {
	ctx := lookup_context(ctx_handle.id)
	if ctx != unsafe { nil } {
		internal_draw(ctx, layer_id)
	}
}
