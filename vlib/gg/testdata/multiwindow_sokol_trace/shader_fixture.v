module multiwindow_sokol_trace

$if gg_multiwindow ? {
	import sokol.gfx
}

#insert "@VMODROOT/vlib/gg/testdata/multiwindow_sokol_trace/shader_fixture_helpers.h"

fn C.v_multiwindow_fixture_hlsl4_vs() voidptr
fn C.v_multiwindow_fixture_hlsl4_vs_size() usize
fn C.v_multiwindow_fixture_hlsl4_fs() voidptr
fn C.v_multiwindow_fixture_hlsl4_fs_size() usize

const fixture_gl_vs = '#version 330\nuniform vec4 vs_params[8];\nlayout(location=0) in vec4 position;\nlayout(location=1) in vec2 texcoord0;\nlayout(location=2) in vec4 color0;\nlayout(location=3) in float psize;\nout vec4 uv;\nout vec4 color;\nvoid main() { mat4 mvp = mat4(vs_params[0], vs_params[1], vs_params[2], vs_params[3]); mat4 tm = mat4(vs_params[4], vs_params[5], vs_params[6], vs_params[7]); gl_Position = mvp * position; gl_PointSize = psize; uv = tm * vec4(texcoord0, 0.0, 1.0); color = color0; }\n'
const fixture_gl_fs = '#version 330\nuniform sampler2D tex_smp;\nin vec4 uv;\nin vec4 color;\nout vec4 frag_color;\nvoid main() { frag_color = texture(tex_smp, uv.xy) * color; }\n'
const fixture_gles_vs = '#version 300 es\nprecision highp float;\nuniform vec4 vs_params[8];\nlayout(location=0) in vec4 position;\nlayout(location=1) in vec2 texcoord0;\nlayout(location=2) in vec4 color0;\nlayout(location=3) in float psize;\nout vec4 uv;\nout vec4 color;\nvoid main() { mat4 mvp = mat4(vs_params[0], vs_params[1], vs_params[2], vs_params[3]); mat4 tm = mat4(vs_params[4], vs_params[5], vs_params[6], vs_params[7]); gl_Position = mvp * position; gl_PointSize = psize; uv = tm * vec4(texcoord0, 0.0, 1.0); color = color0; }\n'
const fixture_gles_fs = '#version 300 es\nprecision mediump float;\nuniform sampler2D tex_smp;\nin vec4 uv;\nin vec4 color;\nout vec4 frag_color;\nvoid main() { frag_color = texture(tex_smp, uv.xy) * color; }\n'
const fixture_msl = '#include <metal_stdlib>\nusing namespace metal;\nstruct fixture_params { float4x4 mvp; float4x4 tm; };\nstruct fixture_vs_in { float4 position [[attribute(0)]]; float2 texcoord0 [[attribute(1)]]; float4 color0 [[attribute(2)]]; float psize [[attribute(3)]]; };\nstruct fixture_vs_out { float4 position [[position]]; float point_size [[point_size]]; float4 uv [[user(locn0)]]; float4 color [[user(locn1)]]; };\nstruct fixture_fs_in { float4 uv [[user(locn0)]]; float4 color [[user(locn1)]]; };\nvertex fixture_vs_out fixture_vs(fixture_vs_in input [[stage_in]], constant fixture_params& params [[buffer(0)]]) { fixture_vs_out output; output.position = params.mvp * input.position; output.point_size = input.psize; output.uv = params.tm * float4(input.texcoord0, 0.0, 1.0); output.color = input.color0; return output; }\nfragment float4 fixture_fs(fixture_fs_in input [[stage_in]], texture2d<float> tex [[texture(0)]], sampler smp [[sampler(0)]]) { return tex.sample(smp, input.uv.xy) * input.color; }\n'
const fixture_position = 'position'
const fixture_texcoord = 'texcoord0'
const fixture_color = 'color0'
const fixture_psize = 'psize'
const fixture_semantic = 'TEXCOORD'
const fixture_uniform = 'vs_params'
const fixture_sampler_pair = 'tex_smp'
const fixture_vs_entry = 'fixture_vs'
const fixture_fs_entry = 'fixture_fs'

$if gg_multiwindow ? {
	// shader_descriptor is private testdata despite being exported from this helper module.
	pub fn shader_descriptor() !gfx.ShaderDesc {
		mut desc := gfx.ShaderDesc{}
		desc.attrs[0].name = &char(fixture_position.str)
		desc.attrs[1].name = &char(fixture_texcoord.str)
		desc.attrs[2].name = &char(fixture_color.str)
		desc.attrs[3].name = &char(fixture_psize.str)
		for index in 0 .. 4 {
			desc.attrs[index].sem_name = &char(fixture_semantic.str)
			desc.attrs[index].sem_index = index
		}
		desc.vs.uniform_blocks[0].size = 128
		desc.vs.uniform_blocks[0].uniforms[0].name = &char(fixture_uniform.str)
		desc.vs.uniform_blocks[0].uniforms[0].type = .float4
		desc.vs.uniform_blocks[0].uniforms[0].array_count = 8
		desc.fs.images[0].used = true
		desc.fs.images[0].image_type = ._2d
		desc.fs.images[0].sample_type = .float
		desc.fs.samplers[0].used = true
		desc.fs.samplers[0].sampler_type = .filtering
		desc.fs.image_sampler_pairs[0].used = true
		desc.fs.image_sampler_pairs[0].image_slot = 0
		desc.fs.image_sampler_pairs[0].sampler_slot = 0
		desc.fs.image_sampler_pairs[0].glsl_name = &char(fixture_sampler_pair.str)
		match gfx.query_backend() {
			.glcore33 {
				desc.vs.source = &char(fixture_gl_vs.str)
				desc.fs.source = &char(fixture_gl_fs.str)
			}
			.gles3 {
				desc.vs.source = &char(fixture_gles_vs.str)
				desc.fs.source = &char(fixture_gles_fs.str)
			}
			.d3d11 {
				if C.v_multiwindow_fixture_hlsl4_vs() == unsafe { nil }
					|| C.v_multiwindow_fixture_hlsl4_fs() == unsafe { nil } {
					return error('multi-window SM4 shader fixture is unavailable')
				}
				desc.vs.bytecode = gfx.Range{
					ptr:  C.v_multiwindow_fixture_hlsl4_vs()
					size: C.v_multiwindow_fixture_hlsl4_vs_size()
				}
				desc.fs.bytecode = gfx.Range{
					ptr:  C.v_multiwindow_fixture_hlsl4_fs()
					size: C.v_multiwindow_fixture_hlsl4_fs_size()
				}
			}
			.metal_ios, .metal_macos, .metal_simulator {
				desc.vs.source = &char(fixture_msl.str)
				desc.vs.entry = &char(fixture_vs_entry.str)
				desc.fs.source = &char(fixture_msl.str)
				desc.fs.entry = &char(fixture_fs_entry.str)
			}
			else {
				return error('multi-window fault shader fixture does not support `${gfx.query_backend()}`')
			}
		}

		return desc
	}
}
