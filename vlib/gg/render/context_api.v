module render

import sokol.gfx

pub fn (ctx &Context) defaults() {
	mut st := ctx.st
	st.u = 0.0
	st.vv = 0.0
	st.rgba = 0xffffffff
	st.point_size = 1.0
	st.texturing_enabled = false
	st.cur_img = st.def_img
	st.cur_smp = st.def_smp
	ctx.load_default_pipeline()
	for i in 0 .. 3 {
		st.matrix_stack[i][0] = mat4_identity()
		st.matrix_tos[i] = 0
	}
	st.cur_matrix_mode = matrixmode_modelview
	st.matrix_dirty = true
}

pub fn (ctx &Context) begin(mode Primitive) {
	mut st := ctx.st
	st.in_begin = true
	st.base_vertex = st.vertex_count
	st.vtx_count = 0
	st.cur_prim = mode
}

pub fn (ctx &Context) end() {
	mut st := ctx.st
	if !st.in_begin {
		return
	}
	st.in_begin = false
	matrix_dirty := st.matrix_dirty
	if matrix_dirty {
		st.matrix_dirty = false
		mut uni := Uniform{}
		mvp := mat4_mul(st.matrix_stack[matrixmode_projection.value][st.matrix_tos[matrixmode_projection.value]],
			st.matrix_stack[matrixmode_modelview.value][st.matrix_tos[matrixmode_modelview.value]])
		uni.mvp = mvp
		uni.tm = st.matrix_stack[matrixmode_texture.value][st.matrix_tos[matrixmode_texture.value]]
		if st.uniforms_count < st.uniforms.len {
			st.uniforms[st.uniforms_count] = uni
			st.uniforms_count++
		} else {
			st.error = .uniforms_full
		}
	}
	pip := st.pip_stack[st.pip_tos]
	img := if st.texturing_enabled { st.cur_img } else { st.def_img }
	smp := if st.texturing_enabled { st.cur_smp } else { st.def_smp }

	mut merge := false
	if st.commands_count > 0 {
		cur := &st.commands[st.commands_count - 1]
		if cur.kind == .draw && cur.layer_id == st.layer_id && st.cur_prim != .line_strip
			&& st.cur_prim != .triangle_strip && !matrix_dirty && cur.image.id == img.id
			&& cur.sampler.id == smp.id && cur.pipeline.id == pip.pips[primitive_index(st.cur_prim)].id {
			merge = true
		}
	}
	if merge {
		st.commands[st.commands_count - 1].num_vertices += st.vertex_count - st.base_vertex
	} else {
		if st.commands_count < st.commands.len {
			cmd := Command{
				kind:          .draw
				primitive:     st.cur_prim
				base_vertex:   st.base_vertex
				num_vertices:  st.vertex_count - st.base_vertex
				pipeline:      pip.pips[primitive_index(st.cur_prim)]
				image:         img
				sampler:       smp
				uniform_index: st.uniforms_count - 1
				blend_mode:    pip.blend_mode
				layer_id:      st.layer_id
			}
			st.commands[st.commands_count] = cmd
			st.commands_count++
		} else {
			st.error = .commands_full
		}
	}
	st.base_vertex = st.vertex_count
}

fn primitive_index(p Primitive) int {
	match p {
		.points { return 0 }
		.lines { return 1 }
		.line_strip { return 2 }
		.triangles { return 3 }
		.triangle_strip { return 4 }
		.quads { return 5 }
		else { return 3 }
	}
}

pub fn (ctx &Context) begin_points() { ctx.begin(.points) }

pub fn (ctx &Context) begin_lines() { ctx.begin(.lines) }

pub fn (ctx &Context) begin_line_strip() { ctx.begin(.line_strip) }

pub fn (ctx &Context) begin_triangles() { ctx.begin(.triangles) }

pub fn (ctx &Context) begin_triangle_strip() { ctx.begin(.triangle_strip) }

pub fn (ctx &Context) begin_quads() { ctx.begin(.quads) }

pub fn (ctx &Context) c1i(rgba u32) {
	mut st := ctx.st
	st.rgba = rgba
}

pub fn (ctx &Context) c3b(r u8, g u8, b u8) {
	mut st := ctx.st
	st.rgba = (u32(r)) | (u32(g) << 8) | (u32(b) << 16) | (u32(255) << 24)
}

pub fn (ctx &Context) c3f(r f32, g f32, b f32) {
	mut st := ctx.st
	st.rgba = pack_rgba_f(r, g, b, 1.0)
}

pub fn (ctx &Context) c4b(r u8, g u8, b u8, a u8) {
	mut st := ctx.st
	st.rgba = (u32(r)) | (u32(g) << 8) | (u32(b) << 16) | (u32(a) << 24)
}

pub fn (ctx &Context) c4f(r f32, g f32, b f32, a f32) {
	mut st := ctx.st
	st.rgba = pack_rgba_f(r, g, b, a)
}

pub fn (ctx &Context) v2f(x f32, y f32) {
	mut st := ctx.st
	if !st.in_begin { return }
	if (st.cur_prim == .quads) && ((st.vtx_count & 3) == 3) {
		if st.vertex_count < st.max_vertices {
			st.verts[st.vertex_count] = st.verts[st.vertex_count - 3]
			st.vertex_count++
		}
		if st.vertex_count < st.max_vertices {
			st.verts[st.vertex_count] = st.verts[st.vertex_count - 2]
			st.vertex_count++
		}
	}
	if st.vertex_count < st.max_vertices {
		st.verts[st.vertex_count] = Vertex{ x: x, y: y, z: 0.0, u: st.u, v: st.vv, rgba: st.rgba, psize: st.point_size }
		st.vertex_count++
		st.vtx_count++
	} else {
		st.error = .vertices_full
	}
}

pub fn (ctx &Context) v3f(x f32, y f32, z f32) {
	mut st := ctx.st
	if !st.in_begin { return }
	if (st.cur_prim == .quads) && ((st.vtx_count & 3) == 3) {
		if st.vertex_count < st.max_vertices {
			st.verts[st.vertex_count] = st.verts[st.vertex_count - 3]
			st.vertex_count++
		}
		if st.vertex_count < st.max_vertices {
			st.verts[st.vertex_count] = st.verts[st.vertex_count - 2]
			st.vertex_count++
		}
	}
	if st.vertex_count < st.max_vertices {
		st.verts[st.vertex_count] = Vertex{ x: x, y: y, z: z, u: st.u, v: st.vv, rgba: st.rgba, psize: st.point_size }
		st.vertex_count++
		st.vtx_count++
	} else {
		st.error = .vertices_full
	}
}

pub fn (ctx &Context) t2f(u f32, v f32) {
	mut st := ctx.st
	st.u = u
	st.vv = v
}

pub fn (ctx &Context) v2f_t2f(x f32, y f32, u f32, v f32) {
	ctx.t2f(u, v)
	ctx.v2f(x, y)
}

pub fn (ctx &Context) v2f_c4b(x f32, y f32, r u8, g u8, b u8, a u8) {
	ctx.c4b(r, g, b, a)
	ctx.v2f(x, y)
}

pub fn (ctx &Context) v2f_c4f(x f32, y f32, r f32, g f32, b f32, a f32) {
	ctx.c4f(r, g, b, a)
	ctx.v2f(x, y)
}

pub fn (ctx &Context) v2f_c3b(x f32, y f32, r u8, g u8, b u8) {
	ctx.c3b(r, g, b)
	ctx.v2f(x, y)
}

pub fn (ctx &Context) v2f_c3f(x f32, y f32, r f32, g f32, b f32) {
	ctx.c3f(r, g, b)
	ctx.v2f(x, y)
}

pub fn (ctx &Context) v2f_c1i(x f32, y f32, rgba u32) {
	ctx.c1i(rgba)
	ctx.v2f(x, y)
}

pub fn (ctx &Context) v3f_t2f(x f32, y f32, z f32, u f32, v f32) {
	ctx.t2f(u, v)
	ctx.v3f(x, y, z)
}

pub fn (ctx &Context) v3f_c4b(x f32, y f32, z f32, r u8, g u8, b u8, a u8) {
	ctx.c4b(r, g, b, a)
	ctx.v3f(x, y, z)
}

pub fn (ctx &Context) v3f_c4f(x f32, y f32, z f32, r f32, g f32, b f32, a f32) {
	ctx.c4f(r, g, b, a)
	ctx.v3f(x, y, z)
}

pub fn (ctx &Context) v3f_c3b(x f32, y f32, z f32, r u8, g u8, b u8) {
	ctx.c3b(r, g, b)
	ctx.v3f(x, y, z)
}

pub fn (ctx &Context) v3f_c3f(x f32, y f32, z f32, r f32, g f32, b f32) {
	ctx.c3f(r, g, b)
	ctx.v3f(x, y, z)
}

pub fn (ctx &Context) v3f_c1i(x f32, y f32, z f32, rgba u32) {
	ctx.c1i(rgba)
	ctx.v3f(x, y, z)
}

pub fn (ctx &Context) v2f_t2f_c4b(x f32, y f32, u f32, v f32, r u8, g u8, b u8, a u8) {
	ctx.t2f(u, v)
	ctx.c4b(r, g, b, a)
	ctx.v2f(x, y)
}

pub fn (ctx &Context) v2f_t2f_c4f(x f32, y f32, u f32, v f32, r f32, g f32, b f32, a f32) {
	ctx.t2f(u, v)
	ctx.c4f(r, g, b, a)
	ctx.v2f(x, y)
}

pub fn (ctx &Context) v2f_t2f_c3b(x f32, y f32, u f32, v f32, r u8, g u8, b u8) {
	ctx.t2f(u, v)
	ctx.c3b(r, g, b)
	ctx.v2f(x, y)
}

pub fn (ctx &Context) v2f_t2f_c3f(x f32, y f32, u f32, v f32, r f32, g f32, b f32) {
	ctx.t2f(u, v)
	ctx.c3f(r, g, b)
	ctx.v2f(x, y)
}

pub fn (ctx &Context) v2f_t2f_c1i(x f32, y f32, u f32, v f32, rgba u32) {
	ctx.t2f(u, v)
	ctx.c1i(rgba)
	ctx.v2f(x, y)
}

pub fn (ctx &Context) v3f_t2f_c4b(x f32, y f32, z f32, u f32, v f32, r u8, g u8, b u8, a u8) {
	ctx.t2f(u, v)
	ctx.c4b(r, g, b, a)
	ctx.v3f(x, y, z)
}

pub fn (ctx &Context) v3f_t2f_c4f(x f32, y f32, z f32, u f32, v f32, r f32, g f32, b f32, a f32) {
	ctx.t2f(u, v)
	ctx.c4f(r, g, b, a)
	ctx.v3f(x, y, z)
}

pub fn (ctx &Context) v3f_t2f_c3b(x f32, y f32, z f32, u f32, v f32, r u8, g u8, b u8) {
	ctx.t2f(u, v)
	ctx.c3b(r, g, b)
	ctx.v3f(x, y, z)
}

pub fn (ctx &Context) v3f_t2f_c3f(x f32, y f32, z f32, u f32, v f32, r f32, g f32, b f32) {
	ctx.t2f(u, v)
	ctx.c3f(r, g, b)
	ctx.v3f(x, y, z)
}

pub fn (ctx &Context) v3f_t2f_c1i(x f32, y f32, z f32, u f32, v f32, rgba u32) {
	ctx.t2f(u, v)
	ctx.c1i(rgba)
	ctx.v3f(x, y, z)
}

pub fn (ctx &Context) matrix_mode_modelview() {
	mut st := ctx.st
	st.cur_matrix_mode = matrixmode_modelview
}

pub fn (ctx &Context) matrix_mode_projection() {
	mut st := ctx.st
	st.cur_matrix_mode = matrixmode_projection
}

pub fn (ctx &Context) matrix_mode_texture() {
	mut st := ctx.st
	st.cur_matrix_mode = matrixmode_texture
}

pub fn (ctx &Context) load_identity() {
	mut st := ctx.st
	st.matrix_stack[st.cur_matrix_mode.value][st.matrix_tos[st.cur_matrix_mode.value]] = mat4_identity()
	st.matrix_dirty = true
}

pub fn (ctx &Context) load_matrix(m []f32) {
	mut st := ctx.st
	st.matrix_stack[st.cur_matrix_mode.value][st.matrix_tos[st.cur_matrix_mode.value]] = mat4_from_array(m)
	st.matrix_dirty = true
}

pub fn (ctx &Context) load_transpose_matrix(m []f32) {
	mut st := ctx.st
	mm := mat4_from_array(m)
	mut t := Mat4{}
	for c in 0 .. 4 {
		for r in 0 .. 4 { t.v[r][c] = mm.v[c][r] }
	}
	st.matrix_stack[st.cur_matrix_mode.value][st.matrix_tos[st.cur_matrix_mode.value]] = t
	st.matrix_dirty = true
}

pub fn (ctx &Context) mult_matrix(m []f32) {
	mut st := ctx.st
	mm := mat4_from_array(m)
	cur := &st.matrix_stack[st.cur_matrix_mode.value][st.matrix_tos[st.cur_matrix_mode.value]]
	tmp := mat4_mul(*cur, mm)
	st.matrix_stack[st.cur_matrix_mode.value][st.matrix_tos[st.cur_matrix_mode.value]] = tmp
	st.matrix_dirty = true
}

pub fn (ctx &Context) mult_transpose_matrix(m []f32) {
	mut st := ctx.st
	mm := mat4_from_array(m)
	mut t := Mat4{}
	for c in 0 .. 4 {
		for r in 0 .. 4 { t.v[r][c] = mm.v[c][r] }
	}
	cur := &st.matrix_stack[st.cur_matrix_mode.value][st.matrix_tos[st.cur_matrix_mode.value]]
	tmp := mat4_mul(*cur, t)
	st.matrix_stack[st.cur_matrix_mode.value][st.matrix_tos[st.cur_matrix_mode.value]] = tmp
	st.matrix_dirty = true
}

pub fn (ctx &Context) push_matrix() {
	mut st := ctx.st
	idx := st.cur_matrix_mode.value
	if st.matrix_tos[idx] < max_stack_depth - 1 {
		st.matrix_tos[idx]++
		st.matrix_stack[idx][st.matrix_tos[idx]] = st.matrix_stack[idx][st.matrix_tos[idx] - 1]
	} else {
		st.error = .stack_overflow
	}
}

pub fn (ctx &Context) pop_matrix() {
	mut st := ctx.st
	idx := st.cur_matrix_mode.value
	if st.matrix_tos[idx] > 0 { st.matrix_tos[idx]-- } else { st.error = .stack_underflow }
	st.matrix_dirty = true
}

pub fn (ctx &Context) translate(x f32, y f32, z f32) {
	mut st := ctx.st
	idx := st.cur_matrix_mode.value
	mut m := st.matrix_stack[idx][st.matrix_tos[idx]]
	m.translate(x, y, z)
	st.matrix_stack[idx][st.matrix_tos[idx]] = m
	st.matrix_dirty = true
}

pub fn (ctx &Context) scale(x f32, y f32, z f32) {
	mut st := ctx.st
	idx := st.cur_matrix_mode.value
	mut m := st.matrix_stack[idx][st.matrix_tos[idx]]
	m.scale(x, y, z)
	st.matrix_stack[idx][st.matrix_tos[idx]] = m
	st.matrix_dirty = true
}

pub fn (ctx &Context) rotate(angle f32, x f32, y f32, z f32) {
	mut st := ctx.st
	idx := st.cur_matrix_mode.value
	mut m := st.matrix_stack[idx][st.matrix_tos[idx]]
	m.rotate(angle, x, y, z)
	st.matrix_stack[idx][st.matrix_tos[idx]] = m
	st.matrix_dirty = true
}

pub fn (ctx &Context) ortho(l f32, r f32, b f32, t f32, n f32, f f32) {
	mut st := ctx.st
	idx := st.cur_matrix_mode.value
	mut m := st.matrix_stack[idx][st.matrix_tos[idx]]
	m.ortho(l, r, b, t, n, f)
	st.matrix_stack[idx][st.matrix_tos[idx]] = m
	st.matrix_dirty = true
}

pub fn (ctx &Context) frustum(l f32, r f32, b f32, t f32, n f32, f f32) {
	mut st := ctx.st
	idx := st.cur_matrix_mode.value
	mut m := st.matrix_stack[idx][st.matrix_tos[idx]]
	m.frustum(l, r, b, t, n, f)
	st.matrix_stack[idx][st.matrix_tos[idx]] = m
	st.matrix_dirty = true
}

pub fn (ctx &Context) perspective(fov_y f32, aspect f32, z_near f32, z_far f32) {
	mut st := ctx.st
	idx := st.cur_matrix_mode.value
	mut m := st.matrix_stack[idx][st.matrix_tos[idx]]
	m.perspective(fov_y, aspect, z_near, z_far)
	st.matrix_stack[idx][st.matrix_tos[idx]] = m
	st.matrix_dirty = true
}

pub fn (ctx &Context) lookat(eye_x f32, eye_y f32, eye_z f32, center_x f32, center_y f32, center_z f32, up_x f32, up_y f32, up_z f32) {
	mut st := ctx.st
	idx := st.cur_matrix_mode.value
	mut m := st.matrix_stack[idx][st.matrix_tos[idx]]
	m.lookat(eye_x, eye_y, eye_z, center_x, center_y, center_z, up_x, up_y, up_z)
	st.matrix_stack[idx][st.matrix_tos[idx]] = m
	st.matrix_dirty = true
}

pub fn (ctx &Context) point_size(s f32) {
	mut st := ctx.st
	st.point_size = s
}

pub fn (ctx &Context) texture(img gfx.Image, smp gfx.Sampler) {
	mut st := ctx.st
	st.cur_img = img
	st.cur_smp = smp
	st.texturing_enabled = true
}

pub fn (ctx &Context) enable_texture() {
	mut st := ctx.st
	st.texturing_enabled = true
}

pub fn (ctx &Context) disable_texture() {
	mut st := ctx.st
	st.texturing_enabled = false
	st.cur_img = st.def_img
	st.cur_smp = st.def_smp
}

pub fn (ctx &Context) scissor_rect(x int, y int, w int, h int, origin_top_left bool) {
	mut st := ctx.st
	if st.commands_count < st.commands.len {
		cmd := Command{ kind: .scissor, x: x, y: y, w: w, h: h, origin_top_left: origin_top_left, layer_id: st.layer_id }
		st.commands[st.commands_count] = cmd
		st.commands_count++
	}
}

pub fn (ctx &Context) scissor_rectf(x f32, y f32, w f32, h f32, origin_top_left bool) {
	ctx.scissor_rect(int(x), int(y), int(w), int(h), origin_top_left)
}

pub fn (ctx &Context) viewport(x int, y int, w int, h int, origin_top_left bool) {
	mut st := ctx.st
	if st.commands_count < st.commands.len {
		cmd := Command{ kind: .viewport, x: x, y: y, w: w, h: h, origin_top_left: origin_top_left, layer_id: st.layer_id }
		st.commands[st.commands_count] = cmd
		st.commands_count++
	}
}

pub fn (ctx &Context) viewportf(x f32, y f32, w f32, h f32, origin_top_left bool) {
	ctx.viewport(int(x), int(y), int(w), int(h), origin_top_left)
}

pub fn (ctx &Context) layer(id int) {
	mut st := ctx.st
	st.layer_id = id
}

pub fn (ctx &Context) draw() {
	mut st := ctx.st
	if st.vertex_count == 0 || st.commands_count == 0 { return }
	if st.update_frame_id != st.frame_id {
		st.update_frame_id = st.frame_id
		if gfx.is_valid() {
			rng := gfx.Range{ ptr: &st.verts[0], size: usize(st.vertex_count) * usize(sizeof(Vertex)) }
			gfx.update_buffer(st.vbuf, &rng)
		}
	}
	mut cur_pip := gfx.Pipeline{}
	mut cur_img := gfx.Image{}
	mut cur_smp := gfx.Sampler{}
	mut cur_uniform := -1
	for i in 0 .. st.commands_count {
		cmd := st.commands[i]
		match cmd.kind {
			.viewport { gfx.apply_viewport(cmd.x, cmd.y, cmd.w, cmd.h, cmd.origin_top_left) }
			.scissor { gfx.apply_scissor_rect(cmd.x, cmd.y, cmd.w, cmd.h, cmd.origin_top_left) }
			.draw {
				if cmd.num_vertices <= 0 { continue }
				if cmd.pipeline.id != cur_pip.id {
					gfx.apply_pipeline(cmd.pipeline)
					cur_pip = cmd.pipeline
					cur_img = gfx.Image{}
					cur_smp = gfx.Sampler{}
					cur_uniform = -1
				}
				if cur_img.id != cmd.image.id || cur_smp.id != cmd.sampler.id {
					st.bindings.vertex_buffers[0] = st.vbuf
					st.bindings.fs.images[0] = cmd.image
					st.bindings.fs.samplers[0] = cmd.sampler
					gfx.apply_bindings(&st.bindings)
					cur_img = cmd.image
					cur_smp = cmd.sampler
				}
				if cur_uniform != cmd.uniform_index {
					if cmd.uniform_index >= 0 && cmd.uniform_index < st.uniforms_count {
						uni := st.uniforms[cmd.uniform_index]
						mut ub := Uniform{ mvp: uni.mvp, tm: uni.tm }
						rng := gfx.Range{ ptr: &ub, size: 128 }
						gfx.apply_uniforms(.vs, 0, &rng)
					}
					cur_uniform = cmd.uniform_index
				}
				gfx.draw(cmd.base_vertex, cmd.num_vertices, 1)
			}
		}
	}
}

// rewind discards the vertices and commands recorded since the last commit
// without submitting them, like sgl_rewind. A frame that is abandoned before
// its pass starts uses it, so the next frame starts from an empty queue.
pub fn (ctx &Context) rewind() {
	mut st := ctx.st
	st.vertex_count = 0
	st.commands_count = 0
	st.uniforms_count = 0
	st.base_vertex = 0
	st.layer_id = 0
	st.in_begin = false
	st.matrix_dirty = true
	st.error = .no_error
}

// num_vertices returns the number of vertices recorded since the last commit
// or rewind, like sgl_num_vertices.
pub fn (ctx &Context) num_vertices() int {
	return ctx.st.vertex_count
}

// num_commands returns the number of commands recorded since the last commit
// or rewind, like sgl_num_commands.
pub fn (ctx &Context) num_commands() int {
	return ctx.st.commands_count
}

pub fn (ctx &Context) error() Error {
	return ctx.st.error
}

pub fn (ctx &Context) rad(deg f32) f32 {
	return deg * 3.14159265358979323846 / 180.0
}

pub fn (ctx &Context) deg(rad f32) f32 {
	return rad * 180.0 / 3.14159265358979323846
}
