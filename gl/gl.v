// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module gl

import const (
	GL_TEXTURE_2D
	GL_TEXTURE0
	GL_FLOAT
	GL_VERTEX_SHADER
	GL_ELEMENT_ARRAY_BUFFER
	GL_DEPTH_TEST
)

// TODO: windows support
#flag linux -I$HOME/code/v/thirdparty/glad
#flag darwin -I$HOME/code/v/thirdparty/glad

#include "glad.h"
#include "glad.c"

fn init_glad() {
	ok := C.gladLoadGL()
	if !ok {
		println('Failed to initialize glad OpenGL context')
		exit(1)
	}
}

pub fn viewport(a int, b int, c int, d int) {
	C.glViewport(a, b, c, d)
}

pub fn clear_color(r, g, b, a int) {
	# glClearColor(((float)r)/255.0,((float)g)/255.0,b/255.0, a/255.0);
}

pub fn clear() {
	# glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);
}

pub fn create_shader(typ int) int {
	return C.glCreateShader(typ)
}

pub fn create_program() int {
	return C.glCreateProgram()
}

pub fn shader_source(shader int, a int, source string, b int) {
	C.glShaderSource(shader, a, &source.str, b)
}

pub fn compile_shader(shader int) {
	C.glCompileShader(shader)
}

pub fn shader_compile_status(shader int) int {
	success := 0
	# glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
	return success
}

pub fn attach_shader(program int, shader int) {
	// fn (s Shader) attach(program int) {
	C.glAttachShader(program, shader)
}

pub fn link_program(program int) {
	C.glLinkProgram(program)
}

pub fn get_program_link_status(program int) int {
	success := 0
	# glGetProgramiv(program, GL_LINK_STATUS, &success);
	return success
}

pub fn delete_shader(shader int) {
	C.glDeleteShader(shader)
}

pub fn shader_info_log(shader int) string {
	# printf("GET info log\n");
	# char infoLog[512];
	# glGetShaderInfoLog(shader, 512, NULL, infoLog);
	# printf("log=%s\n", infoLog);
	# return tos_no_len(infoLog);
	return ''
}

pub fn get_program_info_log(program int) string {
	# char infoLog[512];
	# glGetProgramInfoLog(program, 1024, NULL, infoLog);
	# return tos_no_len(infoLog);
	return ''
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
	C.glBindTexture(GL_TEXTURE_2D, texture)
}

pub fn delete_texture(texture u32) {
	C.glDeleteTextures(1, &texture)
}

pub fn buffer_data(typ int, size int, arr voidptr, draw_typ int) {
	C.glBufferData(typ, size, arr, draw_typ)
}

pub fn buffer_data_int(typ int, vertices[]int, draw_typ int) {
	size := sizeof(int) * vertices.len
	C.glBufferData(typ, size, vertices.data, draw_typ)
}

pub fn buffer_data_float(typ int, vertices[]float, draw_typ int) {
	size := sizeof(float) * vertices.len
	C.glBufferData(typ, size, vertices.data, draw_typ)
}

pub fn set_vbo(vbo u32, vertices[]float, draw_typ int) {
	gl.bind_buffer(GL_ARRAY_BUFFER, vbo)
	gl.buffer_data_float(GL_ARRAY_BUFFER, vertices, draw_typ)
}

pub fn set_ebo(ebo u32, indices[]int, draw_typ int) {
	gl.bind_buffer(GL_ELEMENT_ARRAY_BUFFER, ebo)
	// gl.buffer_data_int(GL_ELEMENT_ARRAY_BUFFER, indices, draw_typ)
	gl.buffer_data_int(GL_ELEMENT_ARRAY_BUFFER, indices, draw_typ)
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
	VAO := u32(0)
	# glGenVertexArrays(1, &VAO);
	return VAO
}

pub fn enable_vertex_attrib_array(n int) {
	C.glEnableVertexAttribArray(n)
}

pub fn gen_buffer() u32 {
	VBO := u32(0)
	# glGenBuffers(1, &VBO);
	return VBO
}

pub fn vertex_attrib_pointer(index, size int, typ int, normalized bool, stride int, ptr int) {
	if typ == GL_FLOAT {
		stride *= sizeof(float)
		ptr *= sizeof(float)
	}
	C.glVertexAttribPointer(index, size, typ, normalized, stride, ptr)
}

pub fn tex_param(key, val int) {
	C.glTexParameteri(GL_TEXTURE_2D, key, val)
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

