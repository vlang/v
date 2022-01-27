// Copyright(C) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license file distributed with this software package
module main

// Example shader triangle adapted to V from https://github.com/floooh/sokol-samples/blob/1f2ad36/sapp/triangle-sapp.c
import sokol.sapp
import sokol.gfx

// Use `v shader` or `sokol-shdc` to generate the necessary `.h` file
// Using `v shader -v .` in this directory will show some additional
// info - and what you should include to make things work.
#flag -I @VMODROOT/.
#include "simple_shader.h"

// simple_shader_desc is a C function declaration defined by
// the `@program` entry in the `simple_shader.glsl` shader file.
// When the shader is compiled this function name is generated
// by the shader compiler for easier inclusion of universal shader code
// in C (and V) code.
fn C.simple_shader_desc(gfx.Backend) &gfx.ShaderDesc

// Vertex_t makes it possible to model vertex buffer data
// for use with the shader system
struct Vertex_t {
	// Position
	x f32
	y f32
	z f32
	// Color
	r f32
	g f32
	b f32
	a f32
}

fn main() {
	mut app := &App{
		width: 800
		height: 400
		pass_action: gfx.create_clear_pass(0.0, 0.0, 0.0, 1.0) // This will create a black color as a default pass (window background color)
	}
	app.run()
}

struct App {
	pass_action gfx.PassAction
mut:
	width           int
	height          int
	shader_pipeline gfx.Pipeline
	bind            gfx.Bindings
}

fn (mut a App) run() {
	title := 'V Simple Shader Example'
	desc := sapp.Desc{
		width: a.width
		height: a.height
		user_data: a
		init_userdata_cb: init
		frame_userdata_cb: frame
		window_title: title.str
		html5_canvas_name: title.str
		cleanup_userdata_cb: cleanup
		sample_count: 4 // Enables MSAA (Multisample anti-aliasing) x4 on rendered output, this can be omitted.
	}
	sapp.run(&desc)
}

fn init(user_data voidptr) {
	mut app := &App(user_data)
	mut desc := sapp.create_desc()

	gfx.setup(&desc)

	// `vertices` defines a vertex buffer with 3 vertices
	// with 3 position fields XYZ and 4 color components RGBA -
	// for drawing a multi-colored triangle.
	//
	// C code:
	// float vertices[] = {
	//    // Positions     // Colors
	//    0.0,  0.5, 0.5,     1.0, 0.0, 0.0, 1.0,
	//    0.5, -0.5, 0.5,     0.0, 1.0, 0.0, 1.0,
	//   -0.5, -0.5, 0.5,     0.0, 0.0, 1.0, 1.0
	// };
	//
	// Array entries in the following V code is the equivalent
	// of the C code entry described above:
	vertices := [
		Vertex_t{0.0, 0.5, 0.5, 1.0, 0.0, 0.0, 1.0},
		Vertex_t{0.5, -0.5, 0.5, 0.0, 1.0, 0.0, 1.0},
		Vertex_t{-0.5, -0.5, 0.5, 0.0, 0.0, 1.0, 1.0},
	]

	// Create a vertex buffer with the 3 vertices defined above.
	mut vertex_buffer_desc := gfx.BufferDesc{
		label: c'triangle-vertices'
	}
	unsafe { vmemset(&vertex_buffer_desc, 0, int(sizeof(vertex_buffer_desc))) }

	vertex_buffer_desc.size = usize(vertices.len * int(sizeof(Vertex_t)))
	vertex_buffer_desc.data = gfx.Range{
		ptr: vertices.data
		size: vertex_buffer_desc.size
	}

	app.bind.vertex_buffers[0] = gfx.make_buffer(&vertex_buffer_desc)

	// Create shader from the code-generated sg_shader_desc (gfx.ShaderDesc in V).
	// Note the function `C.simple_shader_desc()` (also defined above) - this is
	// the function that returns the compiled shader code/desciption we have
	// written in `simple_shader.glsl` and compiled with `v shader .` (`sokol-shdc`).
	shader := gfx.make_shader(C.simple_shader_desc(gfx.query_backend()))

	// Create a pipeline object (default render states are fine for triangle)
	mut pipeline_desc := gfx.PipelineDesc{}
	// This will zero the memory used to store the pipeline in.
	unsafe { vmemset(&pipeline_desc, 0, int(sizeof(pipeline_desc))) }

	// Populate the essential struct fields
	pipeline_desc.shader = shader
	// The vertex shader () takes 2 inputs:
	// ```glsl
	// in vec4 position;
	// in vec4 color0;
	// ```
	pipeline_desc.layout.attrs[C.ATTR_vs_position].format = .float3 // x,y,z as f32
	pipeline_desc.layout.attrs[C.ATTR_vs_color0].format = .float4 // r, g, b, a as f32
	// The .label is optional but can aid debugging sokol shader related issues
	// When things get complex - and you get tired :)
	pipeline_desc.label = c'triangle-pipeline'

	app.shader_pipeline = gfx.make_pipeline(&pipeline_desc)
}

fn cleanup(user_data voidptr) {
	gfx.shutdown()
}

fn frame(user_data voidptr) {
	mut app := &App(user_data)

	gfx.begin_default_pass(&app.pass_action, sapp.width(), sapp.height())

	gfx.apply_pipeline(app.shader_pipeline)
	gfx.apply_bindings(&app.bind)

	gfx.draw(0, 3, 1)

	gfx.end_pass()
	gfx.commit()
}
