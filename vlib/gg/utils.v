module gg

import gl
import gx
import math

fn arc_vertices(x, y, r, start_angle, end_angle f32, segments int) []f32 {
	mut vertices := []f32{}
	start_rads := start_angle * 0.0174533 // deg -> rad approx
	end_rads := end_angle * 0.0174533
	increment := (end_rads - start_rads) / f32(segments)
	vertices << [x + f32(math.cos(start_rads)) * r, y + f32(math.sin(start_rads)) * r] !
	mut i := 1
	for i < segments {
		theta := f32(i) * increment + start_rads
		vertices << [x + f32(math.cos(theta)) * r, y + f32(math.sin(theta)) * r] !
		i++
	}
	vertices << [x + f32(math.cos(end_rads)) * r, y + f32(math.sin(end_rads)) * r] !
	return vertices
}

fn (ctx &GG) use_color_shader(color gx.Color) {
	ctx.shader.set_int('has_texture', 0)
	C.glDeleteBuffers(1, &ctx.vao)
	C.glDeleteBuffers(1, &ctx.vbo)
	ctx.shader.use()
	ctx.shader.set_color('color', color)
}

fn (ctx &GG) bind_vertices(vertices []f32) {
	gl.bind_vao(ctx.vao)
	gl.set_vbo(ctx.vbo, vertices, C.GL_STATIC_DRAW)
	gl.vertex_attrib_pointer(0, 2, C.GL_FLOAT, false, 2, 0)
	gl.enable_vertex_attrib_array(0)
	gl.bind_vao(ctx.vao)
}
