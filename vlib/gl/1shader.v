// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module gl

// import os
import gx
import glm

// import darwin

pub struct Shader {
	program_id int
}
pub fn (s Shader) str() string {
	return 'Shader{ program_id: s.program_id }'
}

pub const (
	text_vert   = '#version 330 core
layout (location = 0) in vec4 vertex; // <vec2 pos, vec2 tex>
out vec2 TexCoords;

uniform mat4 projection;

void main()
{
    gl_Position = projection * vec4(vertex.xy, 0.0, 1.0);
    TexCoords = vertex.zw;
}  '
	text_frag   = '#version 330 core
in vec2 TexCoords;
out vec4 color;

uniform sampler2D text;
uniform vec3 textColor;

void main()
{
    vec4 sampled = vec4(1.0, 1.0, 1.0, texture(text, TexCoords).r);
    color = vec4(textColor, 1.0) * sampled;
}  '
	simple_vert = ' #version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;
layout (location = 2) in vec2 aTexCoord;


out vec3 ourColor;
out vec2 TexCoord;

uniform mat4 projection;

void main() {
    gl_Position = projection *  vec4(aPos, 1.0);
//    gl_Position = vec4(aPos, 1.0);

 ourColor = aColor;
//TexCoord = vec2(aTexCoord.x, aTexCoord.y);
    TexCoord = aTexCoord;
}
'
	simple_frag = '#version 330 core

out vec4 FragColor;
uniform vec3 color;

uniform bool has_texture;

in vec3 ourColor;
in vec2 TexCoord;

uniform sampler2D ourTexture;


void main()     {
//    FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);
//    FragColor = vec4(1.0f, 0.0f, 0.0f, 1.0f);
if (has_texture) {
	/*
	vec3 chromaKeyColor = texture(ourTexture,TexCoord.xy).xyz;

float alpha;
bool is_cyan = ((chromaKeyColor.x == 0)); // && chromaKeyColor.x <= 1) && (chromaKeyColor.y <= 255) &&
bool is_pink= ((chromaKeyColor.y == 0));
//bool is_pink= ((chromaKeyColor.x <= 255) && (chromaKeyColor.y == 0) &&(chromaKeyColor.z <= 255));
if (is_cyan || is_pink) {
    alpha = 0.;
}
else
{
    alpha = 1.0;
}
FragColor= vec4(texture(ourTexture,TexCoord.xy).xyz,alpha);
*/

    FragColor = texture(ourTexture, TexCoord);

}  else {
    FragColor = vec4(color, 1.0f);
}
}
'
)

pub fn new_shader(name string) Shader {
	// TODO This is not used, remove
	mut dir := ''
	// Already have absolute path
	if name.starts_with('/') {
		dir = ''
	}
	//vertex_path := '${dir}${name}.vert'
	//fragment_path := '${dir}${name}.frag'
	//println('shader path=$vertex_path,\n fpath="$fragment_path"')
	// vertex_src := os.read_file(vertex_path.trim_space())
	mut vertex_src := ''
	mut fragment_src := ''
	if name == 'text' {
		vertex_src = text_vert
		fragment_src = text_frag
	}
	else if name == 'simple' {
		// println('new shader simple!!!')
		// println(simple_vert)
		vertex_src = simple_vert
		fragment_src = simple_frag
	}
	// ////////////////////////////////////////
	vertex_shader := gl.create_shader(C.GL_VERTEX_SHADER)
	gl.shader_source(vertex_shader, 1, vertex_src, 0)
	gl.compile_shader(vertex_shader)
	if gl.shader_compile_status(vertex_shader) == 0 {
		cerror := gl.shader_info_log(vertex_shader)
		eprintln('vertex ${vertex_shader} shader compilation failed')
		eprintln('shader source = ${vertex_src}')
		eprintln('failed to compile, with error')
		eprintln(cerror)
		exit(1)
	}
	// fragment shader
	// fragment_src := os.read_file(fragment_path.trim_space())
	fragment_shader := gl.create_shader(C.GL_FRAGMENT_SHADER)
	gl.shader_source(fragment_shader, 1, fragment_src, 0)
	gl.compile_shader(fragment_shader)
	if gl.shader_compile_status(fragment_shader) == 0 {
		cerror := gl.shader_info_log(fragment_shader)
		eprintln('fragment ${fragment_shader} shader compilation failed')
		eprintln('shader source = ${fragment_src}')
		eprintln('failed to compile, with error')
		eprintln(cerror)
		exit(1)
	}
	// link shaders
	shader_program := gl.create_program()
	gl.attach_shader(shader_program, vertex_shader)
	gl.attach_shader(shader_program, fragment_shader)
	gl.link_program(shader_program)
	// check for linking errors
	success := gl.get_program_link_status(shader_program)
	if success == 0 {
		cerror := gl.shader_info_log(shader_program)
		eprintln('shader program linking failed')
		eprintln('vertex source = ${vertex_src}')
		eprintln('fragment source = ${fragment_src}')
		eprintln('failed to compile, with error')
		eprintln(cerror)
		exit(1)
	}
	shader := Shader {
		program_id: shader_program,
	}
	return shader
}

pub fn (s Shader) use() {
	gl.use_program(s.program_id)
}

fn C.glUniformMatrix4fv()
fn C.glUniform1i()
fn C.glUniform3f()

pub fn (s Shader) uni_location(key string) int {
	return C.glGetUniformLocation(s.program_id, key.str)
}

// fn (s Shader) set_mat4(str string, f *f32) {
pub fn (s Shader) set_mat4(str string, m glm.Mat4) {
	// TODO cache uniform location
	C.glUniformMatrix4fv(s.uni_location(str), 1, false, m.data)
}

pub fn (s Shader) set_int(str string, n int) {
	C.glUniform1i(s.uni_location(str), n)
}

pub fn (s Shader) set_color(str string, c gx.Color) {
	C.glUniform3f(s.uni_location(str), f32(c.r) / 255.0, f32(c.g) / 255.0, f32(c.b) / 255.0)
}
