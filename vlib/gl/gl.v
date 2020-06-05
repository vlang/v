// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module gl

import glm

#flag  -I @VROOT/thirdparty/glad
#include "glad.h"
#flag @VROOT/thirdparty/glad/glad.o


// joe-c: fix & remove
pub enum TmpGlImportHack{ non_empty }

fn C.gladLoadGL() int

fn C.glDisable()
fn C.glEnable()
fn C.glScissor()
fn C.glVertexAttribPointer()
fn C.glGenBuffers()
fn C.glEnableVertexAttribArray()
fn C.glDisableVertexAttribArray()
fn C.glGenVertexArrays()
fn C.glDrawElements()
fn C.glUseProgram()
fn C.glValidateProgram()
fn C.glDrawArrays()
fn C.glBufferData()
fn C.glGenerateMipmap()
fn C.glTexParameteri()
fn C.glDeleteTextures()
fn C.glBindTexture()
fn C.glActiveTexture()
fn C.glGenTextures()
fn C.glBindBuffer()
fn C.glBindVertexArray()
fn C.glGetProgramInfoLog()
fn C.glGetShaderInfoLog()
fn C.glDeleteShader()
fn C.glGetProgramiv()
fn C.glLinkProgram()
fn C.glAttachShader()
fn C.glDetachShader()
fn C.glGetShaderiv()
fn C.glCompileShader()
fn C.glShaderSource()
fn C.glCreateProgram() int
fn C.glDeleteProgram()
fn C.glClear()
fn C.glCreateShader() int
fn C.glClearColor()
fn C.glViewport()
fn C.glTexImage2D()
fn C.glPixelStorei()
fn C.glBlendFunc()
fn C.glPolygonMode()
fn C.glDeleteBuffers()
fn C.glDeleteVertexArrays()
fn C.glGetUniformLocation() int
fn C.glGetAttribLocation() int
fn C.glBindAttribLocation()

fn C.glUniform1f()


// init_glad initializes glad, which is needed to use other functions.
pub fn init_glad() {
	ok := C.gladLoadGL()
	if ok == 0 {
		println('Failed to initialize glad OpenGL context')
		exit(1)
	}
}

// viewport declares the area, which should be rendered in the window.
// Should be used on resizing the window.
pub fn viewport(x, y, width, height int) {
	C.glViewport(x, y, width, height)
}

// clear_color sets the color, which will be the background, where you can draw.
// Need only set once.
pub fn clear_color(r, g, b, a int) {
	C.glClearColor(f32(r)/255.0, f32(g)/255.0, f32(b)/255.0, f32(a)/255.0)
}

// clear clears the bits of the last frame (ColorBufferBit, DepthBufferBit, StencilBufferBit) to store new data.
// Should be done every frame.
pub fn clear() {
	C.glClear(C.GL_COLOR_BUFFER_BIT | C.GL_DEPTH_BUFFER_BIT | C.GL_STENCIL_BUFFER_BIT)
}

// create_shader creates a shader in OpenGL and returns the id. 
pub fn create_shader(typ int) int {
	return C.glCreateShader(typ)
}

// Create a program in OpenGL and returns the id.
// A shader can be attached to a program, which is required to use them.
pub fn create_program() int {
	return C.glCreateProgram()
}

// Delete a program by id.
// Cleanup method, should be used after the main game loop
pub fn delete_program(program int) {
	C.glDeleteProgram(program)
}

// shader_source attaches source code to the shader via the shaderID.
// Could be also used to load multiple sources into a shader.
// To just add one source code to one shader use: `gl.shader_source(shader, 1, src, 0)`
pub fn shader_source(shader, count int, source string, length int) {
	C.glShaderSource(shader, count, &source.str, length)
}

// compile_shader compiles the shader's source code.
// OpenGL compiles the source code at runtime.
pub fn compile_shader(shader int) {
	C.glCompileShader(shader)
}

// shader_compile_status returns the compile status of the shader compilation.
// Can be used to check the compilation and see for errors in the shader code via `gl.shader_info_log()`
pub fn shader_compile_status(shader int) int {
	success := 0
	C.glGetShaderiv(shader, C.GL_COMPILE_STATUS, &success)
	return success
}

// attach_shader attaches a shader to a program.
// Required for drawing things on the screen
pub fn attach_shader(program, shader int) {
	C.glAttachShader(program, shader)
}

// detach_shader detaches a shader of a program
// Cleanup method
pub fn detach_shader(program, shader int) {
	C.glDetachShader(program, shader)
}

// link_program links a program as target.
// This let OpenGL know, which program has to be use.
pub fn link_program(program int) {
	C.glLinkProgram(program)
}

// get_program_link_status returns the link status of linking the program
pub fn get_program_link_status(program int) int {
	success := 0
	C.glGetProgramiv(program, C.GL_LINK_STATUS, &success)
	return success
}

// validate_program checks that the shaders in the program can be executed
pub fn validate_program(program int) {
	C.glValidateProgram(program)
}

// delete_shader deletes a shader via the shaderID
// Cleanup method
pub fn delete_shader(shader int) {
	C.glDeleteShader(shader)
}

// shader_info_log returns a info log of the shader.
// Can be used to print compilation errors.
pub fn shader_info_log(shader int) string {
	info_log := [512]byte
	C.glGetShaderInfoLog(shader, 512, 0, info_log)
	return tos_clone(info_log)
}

// get_program_info_log returns a info log of the program. 
// Can be used to print linking errors etc.
pub fn get_program_info_log(program int) string {
	info_log := [1024]byte
	C.glGetProgramInfoLog(program, 1024, 0, info_log)
	return tos_clone(info_log)
}

// bind_vao binds a vertex array buffer to OpenGL.
// Says OpenGL which vao is the target.
pub fn bind_vao(vao u32) {
	C.glBindVertexArray(vao)
}

// bind_buffer binds a vertex buffer object to OpenGL.
// Says OpenGL which vbo is the target.
pub fn bind_buffer(typ int, vbo u32) {
	C.glBindBuffer(typ, vbo)
}

// gen_texture generates a textureID.
// Needed to use texturing.
pub fn gen_texture() u32 {
	res := u32(0)
	C.glGenTextures(1, &res)
	return res
}

// active_texture activates a texture.
// If you don't do this, texture isn't working.
pub fn active_texture(t int) {
	C.glActiveTexture(t)
}

// bind_2d_texture binds the activated texture as a 2D texture.
// Helper method.
pub fn bind_2d_texture(texture u32) {
	C.glBindTexture(C.GL_TEXTURE_2D, texture)
}

// bind_texture binds the activated texture to a texture type.
// Defines the type for texture.
pub fn bind_texture(texture, typ u32) {
	C.glBindTexture(typ, texture)
}

// delete_texture deletes a texture by ID.
// Cleanup method.
pub fn delete_texture(texture u32) {
	C.glDeleteTextures(1, &texture)
}

// buffer_data puts data into a buffer.
// With these methods, data can put into a buffer.
// Common usage for C.GL_ARRAY_BUFFER or C.GL_ELEMENT_ARRAY_BUFFER.
pub fn buffer_data(typ, size int, arr voidptr, draw_typ int) {
	C.glBufferData(typ, size, arr, draw_typ)
}

// buffer_data_int puts int into a buffer.
pub fn buffer_data_int(typ int, vertices []int, draw_typ int) {
	size := sizeof(int) * u32(vertices.len)
	C.glBufferData(typ, size, vertices.data, draw_typ)
}

// buffer_data_f32 puts float into a buffer.
pub fn buffer_data_f32(typ int, vertices []f32, draw_typ int) {
	size := sizeof(f32) * u32(vertices.len)
	C.glBufferData(typ, size, vertices.data, draw_typ)
}

// set_vbo sets vertices into a vertex buffer object.
// Helper method.
pub fn set_vbo(vbo u32, vertices []f32, draw_typ int) {
	gl.bind_buffer(C.GL_ARRAY_BUFFER, vbo)
	gl.buffer_data_f32(C.GL_ARRAY_BUFFER, vertices, draw_typ)
}

// set_ebo sets indices into a element array buffer.
// Helper method.
pub fn set_ebo(ebo u32, indices []int, draw_typ int) {
	gl.bind_buffer(C.GL_ELEMENT_ARRAY_BUFFER, ebo)
	gl.buffer_data_int(C.GL_ELEMENT_ARRAY_BUFFER, indices, draw_typ)
}

// delete_buffer deletes a vertex buffer object.
// Cleanup method.
pub fn delete_buffer(vbo u32) {
	C.glDeleteBuffers(1, vbo)
}

// delete_vao deletes a vertex array object.
// Cleanup method.
pub fn delete_vao(vao u32) {
	C.glDeleteVertexArrays(1, vao)
}

// get_uniform_location gets the uniform location for key in program.
// Required to put uniform data in shader at runtime.
pub fn get_uniform_location(program int, key string) int {
	return C.glGetUniformLocation(program, key.str)
}

// get_attrib_location gets the attribute location for key in program.
// Required to put attrib data in shader at runtime.
pub fn get_attrib_location(program int, key string) int {
	return C.glGetAttribLocation(program, key.str)
}

// bind_attrib_location binds a attrib on index in program to a name.
// Used to send data into a shader.
pub fn bind_attrib_location(program int, index int, name string) {
	C.glBindAttribLocation(program, index, name.str)
}

// draw_arrays draws the vertex buffer object on screen.
// Commonly start is 0 and len 3 (without textures) or 5 (with textures).
// Mode commonly C.GL_TRIANGLES.
pub fn draw_arrays(mode, start, len int) {
	C.glDrawArrays(mode, start, len)
}

// draw_elements draws the element object buffer on screen.
// Commonly typ is C.GL_UNSIGNED_INT and  mode C.GL_TRIANGLES.
pub fn draw_elements(mode, count, typ, indices int) {
	C.glDrawElements(mode, count, typ, indices)
}

// use_program binds program to OpenGL.
// Defines the program which is the target.
pub fn use_program(program int) {
	C.glUseProgram(program)
}

// gen_vertex_array generates a vertex array ID.
// Linked to an empty vertex array.
pub fn gen_vertex_array() u32 {
	vao := u32(0)
	C.glGenVertexArrays(1, &vao)
	return vao
}

// enable_vertex_attrib_array enables a vertex attrib array by index.
pub fn enable_vertex_attrib_array(n int) {
	C.glEnableVertexAttribArray(n)
}

// disable_vertex_attrib_array disabled a vertex attrib array by index.
pub fn disable_vertex_attrib_array(n int) {
	C.glDisableVertexAttribArray(n)
}

// gen_buffer generates an buffer ID.
// Linked to an empty buffer-
pub fn gen_buffer() u32 {
	vbo := u32(0)
	C.glGenBuffers(1, &vbo)
	return vbo
}

// vertex_attrib_pointer defines the activated array by index.
pub fn vertex_attrib_pointer(index, size int, typ int, normalized bool, _stride int, _ptr int) {
	mut stride := u32(_stride)
	mut ptr := _ptr
	if typ == C.GL_FLOAT {
		stride *= sizeof(f32)
		ptr *= int(sizeof(f32))
	}
	C.glVertexAttribPointer(index, size, typ, normalized, stride, ptr)
}

// tex_param attachs texture value as int to a texture by ID.
pub fn tex_param(key, val int) {
	C.glTexParameteri(C.GL_TEXTURE_2D, key, val)
}

// enable enables various capabilities for OpenGL.
pub fn enable(val int) {
	C.glEnable(val)
}

// disable disables various capabilities for OpenGL.
pub fn disable(val int) {
	C.glDisable(val)
}

// scissor defines a rectangle in the window.
pub fn scissor(x, y, width, height int) {
	C.glScissor(x, y, height, height)
}

pub fn generate_mipmap(typ int) {
	C.glGenerateMipmap(typ)
}

// set_mat4fv sets a mat4 at uniform location.
// Used for almost every view stuff in OpenGL.
pub fn set_mat4fv(loc, count int, transpose bool, val glm.Mat4) {
	C.glUniformMatrix4fv(loc, count, transpose, val.data)
}

// set_f32 sets a float at uniform location.
// Usable for global lighing.
pub fn set_f32(loc int, val f32) {
	C.glUniform1f(loc, val)
}

// set_vec sets a vec3 at uniform location.
// Usable to set locations or colors.
pub fn set_vec(loc int, x, y, z f32) {
	C.glUniform3f(loc, x, y, z)
}

// set_bool sets a bool at uniform location.
// Send states to the shader.
pub fn set_bool(loc int, val bool) {
	if val {
		set_f32(loc, 1)
	} else {
		set_f32(loc, 0)
	}
}
