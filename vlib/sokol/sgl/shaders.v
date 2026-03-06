module sgl

import sokol.gfx

// GLSL 410 vertex shader (for SOKOL_GLCORE - Linux/macOS/FreeBSD/OpenBSD/Windows)
const vs_source_glsl410 = '#version 410

uniform vec4 vs_params[8];
layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texcoord0;
layout(location = 2) in vec4 color0;
layout(location = 3) in float psize;
out vec2 uv;
out vec4 color;

void main() {
    gl_Position = mat4(vs_params[0], vs_params[1], vs_params[2], vs_params[3]) * position;
    gl_PointSize = psize;
    uv = (mat4(vs_params[4], vs_params[5], vs_params[6], vs_params[7]) * vec4(texcoord0, 0.0, 1.0)).xy;
    color = color0;
}
'

// GLSL 410 fragment shader
const fs_source_glsl410 = '#version 410

uniform sampler2D tex_smp;
in vec2 uv;
in vec4 color;
layout(location = 0) out vec4 frag_color;

void main() {
    frag_color = texture(tex_smp, uv) * color;
}
'

// GLES3 vertex shader (for SOKOL_GLES3 - Emscripten/mobile)
const vs_source_glsl300es = '#version 300 es

uniform vec4 vs_params[8];
layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texcoord0;
layout(location = 2) in vec4 color0;
layout(location = 3) in float psize;
out vec2 uv;
out vec4 color;

void main() {
    gl_Position = mat4(vs_params[0], vs_params[1], vs_params[2], vs_params[3]) * position;
    gl_PointSize = psize;
    uv = (mat4(vs_params[4], vs_params[5], vs_params[6], vs_params[7]) * vec4(texcoord0, 0.0, 1.0)).xy;
    color = color0;
}
'

// GLES3 fragment shader
const fs_source_glsl300es = '#version 300 es
precision mediump float;
precision highp int;

uniform highp sampler2D tex_smp;
in highp vec2 uv;
in highp vec4 color;
layout(location = 0) out highp vec4 frag_color;

void main() {
    frag_color = texture(tex_smp, uv) * color;
}
'

// Builds and returns the shader descriptor for the sgl shader.
// Selects the correct shader source/bytecode based on the active graphics backend.
fn make_shader_desc() gfx.ShaderDesc {
	mut desc := gfx.ShaderDesc{}
	desc.attrs[0].name = c'position'
	desc.attrs[1].name = c'texcoord0'
	desc.attrs[2].name = c'color0'
	desc.attrs[3].name = c'psize'
	desc.attrs[0].sem_name = c'TEXCOORD'
	desc.attrs[0].sem_index = 0
	desc.attrs[1].sem_name = c'TEXCOORD'
	desc.attrs[1].sem_index = 1
	desc.attrs[2].sem_name = c'TEXCOORD'
	desc.attrs[2].sem_index = 2
	desc.attrs[3].sem_name = c'TEXCOORD'
	desc.attrs[3].sem_index = 3
	desc.vs.uniform_blocks[0].size = sizeof(Uniform)
	desc.vs.uniform_blocks[0].uniforms[0].name = c'vs_params'
	desc.vs.uniform_blocks[0].uniforms[0].@type = .float4
	desc.vs.uniform_blocks[0].uniforms[0].array_count = 8
	desc.fs.images[0].used = true
	desc.fs.images[0].image_type = ._2d
	desc.fs.images[0].sample_type = .float
	desc.fs.samplers[0].used = true
	desc.fs.samplers[0].sampler_type = .filtering
	desc.fs.image_sampler_pairs[0].used = true
	desc.fs.image_sampler_pairs[0].image_slot = 0
	desc.fs.image_sampler_pairs[0].sampler_slot = 0
	desc.fs.image_sampler_pairs[0].glsl_name = c'tex_smp'
	desc.label = c'sgl-shader'

	backend := gfx.query_backend()
	match backend {
		.glcore33 {
			desc.vs.source = vs_source_glsl410.str
			desc.fs.source = fs_source_glsl410.str
		}
		.gles3 {
			desc.vs.source = vs_source_glsl300es.str
			desc.fs.source = fs_source_glsl300es.str
		}
		.metal_macos, .metal_ios, .metal_simulator {
			// Metal backends - TODO: embed Metal shader bytecode
			desc.vs.source = vs_source_glsl410.str
			desc.fs.source = fs_source_glsl410.str
		}
		.d3d11 {
			// D3D11 backend - TODO: embed HLSL shader bytecode
			desc.vs.source = vs_source_glsl410.str
			desc.fs.source = fs_source_glsl410.str
		}
		.wgpu {
			// WebGPU - TODO: embed WGSL shader source
			desc.vs.source = vs_source_glsl410.str
			desc.fs.source = fs_source_glsl410.str
		}
		.dummy {
			desc.vs.source = c''
			desc.fs.source = c''
		}
	}
	return desc
}
