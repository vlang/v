module gg

import (
	gl
	gx
	math
)

fn arc_vertices(x, y, r, start, end, increment f32, segments int) []f32 {
	mut vertices := []f32
	mut i := 0
	for i < segments {
		theta := f32(i) * increment - start 
		vertices << [x + f32(math.cos(theta)) * r, y + f32(math.sin(theta)) * r]
		i++
	}
	// Add the last vertex at the final arc angle.
	vertices << [x + f32(math.cos(end)) * r, y + f32(math.sin(end)) * r]
	return vertices
}

fn (ctx &GG) use_color_shader (color gx.Color) {
	ctx.shader.set_int('has_texture', 0)
	C.glDeleteBuffers(1, &ctx.vao)
	C.glDeleteBuffers(1, &ctx.vbo)
	ctx.shader.use()
	ctx.shader.set_color('color', color)
}

fn (ctx &GG) bind_vertices (vertices []f32) {
	gl.bind_vao(ctx.vao)
	gl.set_vbo(ctx.vbo, vertices, C.GL_STATIC_DRAW)
	gl.vertex_attrib_pointer(0, 2, C.GL_FLOAT, false, 2, 0)
	gl.enable_vertex_attrib_array(0)
	gl.bind_vao(ctx.vao)
}
