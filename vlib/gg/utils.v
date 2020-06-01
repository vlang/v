module gg

import gl
import gx
import math

fn arc_vertices(x, y, r, start_angle, end_angle f64, segments int) []f32 {
	mut vertices := []f32{}
	start_rads := start_angle * 0.017453292519943295769 // deg -> rad approx
	end_rads := end_angle * 0.017453292519943295769
	increment := (end_rads - start_rads) / segments
	vertices << [f32(x + math.cos(start_rads) * r), f32(y + math.sin(start_rads) * r)] !
	mut i := 1
	for i < segments {
		theta := i * increment + start_rads
		vertices << [f32(x + math.cos(theta) * r), f32(y + math.sin(theta) * r)] !
		i++
	}
	vertices << [f32(x + math.cos(end_rads) * r), f32(y + math.sin(end_rads) * r)] !
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
