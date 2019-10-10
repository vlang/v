// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module gl

#flag  -I @VROOT/thirdparty/glad
#include "glad.h"
#flag @VROOT/thirdparty/glad/glad.o

// joe-c: fix & remove
enum TmpGlImportHack{}

pub fn init_glad() {
	ok := C.gladLoadGL()
	if isnil(ok) {
		println('Failed to initialize glad OpenGL context')
		exit(1)
	}
}

pub fn viewport(a, b, c, d int) {
	C.glViewport(a, b, c, d)
}

pub fn clear_color(r, g, b, a int) {
	C.glClearColor(f32(r)/255.0, f32(g)/255.0, f32(b)/255.0, f32(a)/255.0)
}

pub fn clear() {
	C.glClear(C.GL_COLOR_BUFFER_BIT | C.GL_DEPTH_BUFFER_BIT | C.GL_STENCIL_BUFFER_BIT)
}

pub fn create_shader(typ int) int {
	return C.glCreateShader(typ)
}

pub fn create_program() int {
	return C.glCreateProgram()
}

pub fn shader_source(shader, a int, source string, b int) {
	C.glShaderSource(shader, a, &source.str, b)
}

pub fn compile_shader(shader int) {
	C.glCompileShader(shader)
}

pub fn shader_compile_status(shader int) int {
	success := 0
	C.glGetShaderiv(shader, C.GL_COMPILE_STATUS, &success)
	return success
}

pub fn attach_shader(program, shader int) {
	// fn (s Shader) attach(program int) {
	C.glAttachShader(program, shader)
}

pub fn link_program(program int) {
	C.glLinkProgram(program)
}

pub fn get_program_link_status(program int) int {
	success := 0
	C.glGetProgramiv(program, C.GL_LINK_STATUS, &success)
	return success
}

pub fn delete_shader(shader int) {
	C.glDeleteShader(shader)
}

pub fn shader_info_log(shader int) string {
	info_log := [512]byte
	C.glGetShaderInfoLog(shader, 512, 0, info_log)
	return tos_clone(info_log)
}

pub fn get_program_info_log(program int) string {
	info_log := [1024]byte
	C.glGetProgramInfoLog(program, 1024, 0, info_log)
	return tos_clone(info_log)
}

pub fn bind_vao(vao u32) {
	C.glBindVertexArray(vao)
}

pub fn bind_buffer(typ int, vbo u32) {
	C.glBindBuffer(typ, vbo)
}

pub fn gen_texture() u32 {
	res := u32(0)
	C.glGenTextures(1, &res)
	return res
}

pub fn active_texture(t int) {
	C.glActiveTexture(t)
}

pub fn bind_2d_texture(texture u32) {
	C.glBindTexture(C.GL_TEXTURE_2D, texture)
}

pub fn delete_texture(texture u32) {
	C.glDeleteTextures(1, &texture)
}

pub fn buffer_data(typ, size int, arr voidptr, draw_typ int) {
	C.glBufferData(typ, size, arr, draw_typ)
}

pub fn buffer_data_int(typ int, vertices []int, draw_typ int) {
	size := sizeof(int) * vertices.len
	C.glBufferData(typ, size, vertices.data, draw_typ)
}

pub fn buffer_data_f32(typ int, vertices []f32, draw_typ int) {
	size := sizeof(f32) * vertices.len
	C.glBufferData(typ, size, vertices.data, draw_typ)
}

pub fn set_vbo(vbo u32, vertices []f32, draw_typ int) {
	gl.bind_buffer(C.GL_ARRAY_BUFFER, vbo)
	gl.buffer_data_f32(C.GL_ARRAY_BUFFER, vertices, draw_typ)
}

pub fn set_ebo(ebo u32, indices []int, draw_typ int) {
	gl.bind_buffer(C.GL_ELEMENT_ARRAY_BUFFER, ebo)
	// gl.buffer_data_int(GL_ELEMENT_ARRAY_BUFFER, indices, draw_typ)
	gl.buffer_data_int(C.GL_ELEMENT_ARRAY_BUFFER, indices, draw_typ)
}

// /////////////////////
// fn gen_vertex_arrays(a int, vao uint) {
// # glGenVertexArrays(a, &VAO);
// }
pub fn draw_arrays(typ, start, len int) {
	C.glDrawArrays(typ, start, len)
}

pub fn draw_elements(mode, count, typ, indices int) {
	C.glDrawElements(mode, count, typ, indices)
}

pub fn use_program(program int) {
	C.glUseProgram(program)
}

pub fn gen_vertex_array() u32 {
	vao := u32(0)
	C.glGenVertexArrays(1, &vao)
	return vao
}

pub fn enable_vertex_attrib_array(n int) {
	C.glEnableVertexAttribArray(n)
}

pub fn gen_buffer() u32 {
	vbo := u32(0)
	C.glGenBuffers(1, &vbo)
	return vbo
}

pub fn vertex_attrib_pointer(index, size int, typ int, normalized bool, _stride int, _ptr int) {
	mut stride := _stride
	mut ptr := _ptr
	if typ == C.GL_FLOAT {
		stride *= sizeof(f32)
		ptr *= sizeof(f32)
	}
	C.glVertexAttribPointer(index, size, typ, normalized, stride, ptr)
}

pub fn tex_param(key, val int) {
	C.glTexParameteri(C.GL_TEXTURE_2D, key, val)
}

pub fn enable(val int) {
	C.glEnable(val)
}

pub fn disable(val int) {
	C.glDisable(val)
}

pub fn scissor(a, b, c, d int) {
	C.glScissor(a, b, c, d)
}

pub fn generate_mipmap(typ int) {
	C.glGenerateMipmap(typ)
}

