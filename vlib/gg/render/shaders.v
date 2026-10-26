module render

import sokol.gfx

const glsl410_vs = r"#version 410

uniform vec4 vs_params[8];
layout(location = 0) in vec4 position;
layout(location = 3) in float psize;
layout(location = 0) out vec4 uv;
layout(location = 1) in vec2 texcoord0;
layout(location = 1) out vec4 color;
layout(location = 2) in vec4 color0;

void main()
{
    gl_Position = mat4(vs_params[0], vs_params[1], vs_params[2], vs_params[3]) * position;
    gl_PointSize = psize;
    uv = mat4(vs_params[4], vs_params[5], vs_params[6], vs_params[7]) * vec4(texcoord0, 0.0, 1.0);
    color = color0;
}
"

const glsl410_fs = r"#version 410

uniform sampler2D tex_smp;

layout(location = 0) out vec4 frag_color;
layout(location = 0) in vec4 uv;
layout(location = 1) in vec4 color;

void main()
{
    frag_color = texture(tex_smp, uv.xy) * color;
}
"

const glsl300es_vs = r"#version 300 es

uniform vec4 vs_params[8];
layout(location = 0) in vec4 position;
layout(location = 3) in float psize;
out vec4 uv;
layout(location = 1) in vec2 texcoord0;
out vec4 color;
layout(location = 2) in vec4 color0;

void main()
{
    gl_Position = mat4(vs_params[0], vs_params[1], vs_params[2], vs_params[3]) * position;
    gl_PointSize = psize;
    uv = mat4(vs_params[4], vs_params[5], vs_params[6], vs_params[7]) * vec4(texcoord0, 0.0, 1.0);
    color = color0;
}
"

const glsl300es_fs = r"#version 300 es
precision mediump float;
precision highp int;

uniform highp sampler2D tex_smp;

layout(location = 0) out highp vec4 frag_color;
in highp vec4 uv;
in highp vec4 color;

void main()
{
    frag_color = texture(tex_smp, uv.xy) * color;
}
"

const metal_sim_vs = r"#include <metal_stdlib>
#include <simd/simd.h>

using namespace metal;

struct vs_params
{
    float4x4 mvp;
    float4x4 tm;
};

struct main0_out
{
    float4 uv [[user(locn0)]];
    float4 color [[user(locn1)]];
    float4 gl_Position [[position]];
    float gl_PointSize [[point_size]];
};

struct main0_in
{
    float4 position [[attribute(0)]];
    float2 texcoord0 [[attribute(1)]];
    float4 color0 [[attribute(2)]];
    float psize [[attribute(3)]];
};

vertex main0_out main0(main0_in in [[stage_in]], constant vs_params& _19 [[buffer(0)]])
{
    main0_out out = {};
    out.gl_Position = _19.mvp * in.position;
    out.gl_PointSize = in.psize;
    out.uv = _19.tm * float4(in.texcoord0, 0.0, 1.0);
    out.color = in.color0;
    return out;
}
"

const metal_sim_fs = r"#include <metal_stdlib>
#include <simd/simd.h>

using namespace metal;

struct main0_out
{
    float4 frag_color [[color(0)]];
};

struct main0_in
{
    float4 uv [[user(locn0)]];
    float4 color [[user(locn1)]];
};

fragment main0_out main0(main0_in in [[stage_in]], texture2d<float> tex [[texture(0)]], sampler smp [[sampler(0)]])
{
    main0_out out = {};
    out.frag_color = tex.sample(smp, in.uv.xy) * in.color;
    return out;
}
"

const wgsl_vs = r"diagnostic(off, derivative_uniformity);

struct vs_params {
  mvp : mat4x4f,
  tm : mat4x4f,
}

@group(0) @binding(0) var<uniform> x_19 : vs_params;

var<private> position_1 : vec4f;
var<private> uv : vec4f;
var<private> texcoord0 : vec2f;
var<private> color : vec4f;
var<private> color0 : vec4f;
var<private> psize : f32;
var<private> gl_Position : vec4f;

fn main_1() {
  let x_22 : mat4x4f = x_19.mvp;
  let x_25 : vec4f = position_1;
  gl_Position = (x_22 * x_25);
  let x_32 : mat4x4f = x_19.tm;
  let x_36 : vec2f = texcoord0;
  uv = (x_32 * vec4f(x_36.x, x_36.y, 0.0f, 1.0f));
  let x_45 : vec4f = color0;
  color = x_45;
  return;
}

struct main_out {
  @builtin(position)
  gl_Position : vec4f,
  @location(0)
  uv_1 : vec4f,
  @location(1)
  color_1 : vec4f,
}

@vertex
fn main(@location(0) position_1_param : vec4f, @location(1) texcoord0_param : vec2f, @location(2) color0_param : vec4f, @location(3) psize_param : f32) -> main_out {
  position_1 = position_1_param;
  texcoord0 = texcoord0_param;
  color0 = color0_param;
  psize = psize_param;
  main_1();
  return main_out(gl_Position, uv, color);
}
"

const wgsl_fs = r"diagnostic(off, derivative_uniformity);

var<private> frag_color : vec4f;

@group(1) @binding(48) var tex : texture_2d<f32>;

@group(1) @binding(64) var smp : sampler;

var<private> uv : vec4f;
var<private> color : vec4f;

fn main_1() {
  let x_23 : vec4f = uv;
  let x_25 : vec4f = textureSample(tex, smp, vec2f(x_23.x, x_23.y));
  let x_27 : vec4f = color;
  frag_color = (x_25 * x_27);
  return;
}

struct main_out {
  @location(0)
  frag_color_1 : vec4f,
}

@fragment
fn main(@location(0) uv_param : vec4f, @location(1) color_param : vec4f) -> main_out {
  uv = uv_param;
  color = color_param;
  main_1();
  return main_out(frag_color);
}
"

const hlsl_vs = r"cbuffer vs_params : register(b0)
{
    float4 vs_params[8];
};

struct vs_in
{
    float3 position : TEXCOORD0;
    float2 texcoord0 : TEXCOORD1;
    float4 color0 : TEXCOORD2;
    float psize : TEXCOORD3;
};

struct vs_out
{
    float4 uv : TEXCOORD0;
    float4 color : TEXCOORD1;
    float4 gl_Position : SV_Position;
    float gl_PointSize : PSIZE;
};

vs_out main(vs_in inp)
{
    vs_out outp;
    column_major float4x4 mvp = float4x4(vs_params[0], vs_params[1], vs_params[2], vs_params[3]);
    column_major float4x4 tm = float4x4(vs_params[4], vs_params[5], vs_params[6], vs_params[7]);
    outp.gl_Position = mul(mvp, float4(inp.position, 1.0));
    outp.gl_PointSize = inp.psize;
    outp.uv = mul(tm, float4(inp.texcoord0, 0.0, 1.0));
    outp.color = inp.color0;
    return outp;
}
"

const hlsl_fs = r"Texture2D tex : register(t0);
SamplerState smp : register(s0);

struct fs_in
{
    float4 uv : TEXCOORD0;
    float4 color : TEXCOORD1;
};

struct fs_out
{
    float4 frag_color : SV_Target0;
};

fs_out main(fs_in inp)
{
    fs_out outp;
    outp.frag_color = tex.Sample(smp, inp.uv.xy) * inp.color;
    return outp;
}
"

// The font atlas is a single-channel (r8) texture whose red channel holds the
// glyph coverage. Sampling it with the generic shader above yields (r,0,0,1) —
// white glyphs turn red and empty texels draw black. The font shader follows
// sokol_fontstash: the red channel becomes the alpha coverage and rgb stays
// white, so the vertex color survives the multiply unchanged.

const font_glsl410_fs = r"#version 410

uniform sampler2D tex_smp;

layout(location = 0) out vec4 frag_color;
layout(location = 0) in vec4 uv;
layout(location = 1) in vec4 color;

void main()
{
    frag_color = vec4(1.0, 1.0, 1.0, texture(tex_smp, uv.xy).x) * color;
}
"

const font_glsl300es_fs = r"#version 300 es
precision mediump float;
precision highp int;

uniform highp sampler2D tex_smp;

layout(location = 0) out highp vec4 frag_color;
in highp vec4 uv;
in highp vec4 color;

void main()
{
    frag_color = vec4(1.0, 1.0, 1.0, texture(tex_smp, uv.xy).x) * color;
}
"

const font_metal_sim_fs = r"#include <metal_stdlib>
#include <simd/simd.h>

using namespace metal;

struct main0_out
{
    float4 frag_color [[color(0)]];
};

struct main0_in
{
    float4 uv [[user(locn0)]];
    float4 color [[user(locn1)]];
};

fragment main0_out main0(main0_in in [[stage_in]], texture2d<float> tex [[texture(0)]], sampler smp [[sampler(0)]])
{
    main0_out out = {};
    out.frag_color = float4(1.0, 1.0, 1.0, tex.sample(smp, in.uv.xy).r) * in.color;
    return out;
}
"

const font_wgsl_fs = r"diagnostic(off, derivative_uniformity);

var<private> frag_color : vec4f;

@group(1) @binding(48) var tex : texture_2d<f32>;

@group(1) @binding(64) var smp : sampler;

var<private> uv : vec4f;
var<private> color : vec4f;

fn main_1() {
  let x_23 : vec4f = uv;
  let x_25 : vec4f = textureSample(tex, smp, vec2f(x_23.x, x_23.y));
  let x_27 : vec4f = color;
  frag_color = (vec4f(1.0f, 1.0f, 1.0f, x_25.r) * x_27);
  return;
}

struct main_out {
  @location(0)
  frag_color_1 : vec4f,
}

@fragment
fn main(@location(0) uv_param : vec4f, @location(1) color_param : vec4f) -> main_out {
  uv = uv_param;
  color = color_param;
  main_1();
  return main_out(frag_color);
}
"

const font_hlsl_fs = r"Texture2D tex : register(t0);
SamplerState smp : register(s0);

struct fs_in
{
    float4 uv : TEXCOORD0;
    float4 color : TEXCOORD1;
};

struct fs_out
{
    float4 frag_color : SV_Target0;
};

fs_out main(fs_in inp)
{
    fs_out outp;
    outp.frag_color = float4(1.0, 1.0, 1.0, tex.Sample(smp, inp.uv.xy).r) * inp.color;
    return outp;
}
"

pub fn make_shader() gfx.Shader {
	return make_shader_with_frag(glsl410_fs, glsl300es_fs, metal_sim_fs, wgsl_fs, hlsl_fs, c'sgl-shader')
}

// make_font_shader builds the shader the text atlas is drawn with: identical
// vertex stage, fragment stage rewritten so a single-channel atlas's red
// channel acts as alpha coverage.
pub fn make_font_shader() gfx.Shader {
	return make_shader_with_frag(font_glsl410_fs, font_glsl300es_fs, font_metal_sim_fs, font_wgsl_fs, font_hlsl_fs,
		c'sgl-font-shader')
}

fn make_shader_with_frag(glsl410 string, glsl300es string, metal string, wgsl string, hlsl string, label &char) gfx.Shader {
	mut desc := gfx.ShaderDesc{}
	desc.attrs[0].name = c'position'
	desc.attrs[0].sem_name = c'TEXCOORD'
	desc.attrs[0].sem_index = 0
	desc.attrs[1].name = c'texcoord0'
	desc.attrs[1].sem_name = c'TEXCOORD'
	desc.attrs[1].sem_index = 1
	desc.attrs[2].name = c'color0'
	desc.attrs[2].sem_name = c'TEXCOORD'
	desc.attrs[2].sem_index = 2
	desc.attrs[3].name = c'psize'
	desc.attrs[3].sem_name = c'TEXCOORD'
	desc.attrs[3].sem_index = 3
	desc.vs.uniform_blocks[0].size = 128
	desc.vs.uniform_blocks[0].uniforms[0].name = c'vs_params'
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
	desc.fs.image_sampler_pairs[0].glsl_name = c'tex_smp'
	desc.label = label

	$if sokol_gles3 ? {
		desc.set_vert_src(glsl300es_vs)
		desc.set_frag_src(glsl300es)
	} $else $if sokol_metal ? {
		desc.vs.entry = c'main0'
		desc.fs.entry = c'main0'
		desc.set_vert_src(metal_sim_vs)
		desc.set_frag_src(metal)
	} $else $if sokol_d3d11 ? {
		desc.set_vert_src(hlsl_vs)
		desc.set_frag_src(hlsl)
	} $else $if sokol_wgpu ? {
		desc.set_vert_src(wgsl_vs)
		desc.set_frag_src(wgsl)
	} $else {
		desc.set_vert_src(glsl410_vs)
		desc.set_frag_src(glsl410)
	}
	return gfx.make_shader(&desc)
}
