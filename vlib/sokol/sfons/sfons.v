module sfons

import fontstash
import sokol.gfx
import sokol.sgl
import sokol.memory

// Fontstash-specific fragment shaders.
// These differ from sgl's shaders: they read the .r channel for alpha,
// producing: frag_color = vec4(1.0, 1.0, 1.0, texture(tex_smp, uv).r) * color
// The vertex shader is identical to sgl's.

const fs_source_glsl410 = '#version 410

uniform sampler2D tex_smp;
in vec2 uv;
in vec4 color;
layout(location = 0) out vec4 frag_color;

void main() {
    frag_color = vec4(1.0, 1.0, 1.0, texture(tex_smp, uv).r) * color;
}
'

const fs_source_glsl300es = '#version 300 es
precision mediump float;
precision highp int;

uniform highp sampler2D tex_smp;
in highp vec2 uv;
in highp vec4 color;
layout(location = 0) out highp vec4 frag_color;

void main() {
    frag_color = vec4(1.0, 1.0, 1.0, texture(tex_smp, uv).r) * color;
}
'

// The vertex shader is the same as sgl's
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

// Metal vertex shader source.
const vs_source_metal = '#include <metal_stdlib>
using namespace metal;

struct VSIn {
    float4 position [[attribute(0)]];
    float2 texcoord0 [[attribute(1)]];
    float4 color0 [[attribute(2)]];
    float psize [[attribute(3)]];
};

struct VSOut {
    float4 position [[position]];
    float2 uv;
    float4 color;
    float psize [[point_size]];
};

vertex VSOut main0(VSIn in [[stage_in]], constant float4* vs_params [[buffer(0)]]) {
    VSOut out;
    float4x4 mvp = float4x4(vs_params[0], vs_params[1], vs_params[2], vs_params[3]);
    float4x4 tm = float4x4(vs_params[4], vs_params[5], vs_params[6], vs_params[7]);
    out.position = mvp * in.position;
    out.psize = in.psize;
    out.uv = (tm * float4(in.texcoord0, 0.0, 1.0)).xy;
    out.color = in.color0;
    return out;
}
'

// Metal fragment shader source.
const fs_source_metal = '#include <metal_stdlib>
using namespace metal;

struct FSIn {
    float2 uv;
    float4 color;
    float psize [[point_size]];
};

fragment float4 main0(FSIn in [[stage_in]], texture2d<float> tex_smp [[texture(0)]],
    sampler smp [[sampler(0)]]) {
    return float4(1.0, 1.0, 1.0, tex_smp.sample(smp, in.uv).r) * in.color;
}
'

// Internal state for sokol-fontstash
struct SFons {
mut:
	shd        gfx.Shader
	pip        sgl.Pipeline
	img        gfx.Image
	smp        gfx.Sampler
	cur_width  int
	cur_height int
	img_dirty  bool
	alloc_fn   memory.FnAllocatorAlloc = unsafe { nil }
	free_fn    memory.FnAllocatorFree  = unsafe { nil }
	user_data  voidptr
}

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
	desc.vs.uniform_blocks[0].size = 128 // 2 mat4 = 8 vec4 = 128 bytes
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
	desc.label = c'sfons-shader'

	backend := gfx.query_backend()
	match backend {
		.glcore33 {
			desc.vs.source = &char(vs_source_glsl410.str)
			desc.fs.source = &char(fs_source_glsl410.str)
		}
		.gles3 {
			desc.vs.source = &char(vs_source_glsl300es.str)
			desc.fs.source = &char(fs_source_glsl300es.str)
		}
		.metal_macos, .metal_ios, .metal_simulator {
			desc.vs.entry = c'main0'
			desc.fs.entry = c'main0'
			desc.vs.source = &char(vs_source_metal.str)
			desc.fs.source = &char(fs_source_metal.str)
		}
		.d3d11 {
			// TODO: embed HLSL shader bytecode
			desc.vs.source = &char(vs_source_glsl410.str)
			desc.fs.source = &char(fs_source_glsl410.str)
		}
		.wgpu {
			// TODO: embed WGSL shader source
			desc.vs.source = &char(vs_source_glsl410.str)
			desc.fs.source = &char(fs_source_glsl410.str)
		}
		.dummy {
			desc.vs.source = c''
			desc.fs.source = c''
		}
	}
	return desc
}

// Rendering callbacks for fontstash

fn sfons_render_create(user_ptr voidptr, width int, height int) int {
	assert user_ptr != unsafe { nil }
	assert width > 8
	assert height > 8
	mut sf := unsafe { &SFons(user_ptr) }

	// Create shader if not yet created
	if sf.shd.id == 0 {
		shd_desc := make_shader_desc()
		sf.shd = gfx.make_shader(&shd_desc)
		$if trace_sfons ? {
			eprintln('[sfons] shader created: id=${sf.shd.id}')
		}
	}

	// Create sgl pipeline if not yet created
	if sf.pip.id == 0 {
		mut pip_desc := gfx.PipelineDesc{}
		pip_desc.shader = sf.shd
		pip_desc.colors[0].blend.enabled = true
		pip_desc.colors[0].blend.src_factor_rgb = .src_alpha
		pip_desc.colors[0].blend.dst_factor_rgb = .one_minus_src_alpha
		sf.pip = sgl.make_pipeline(&pip_desc)
		$if trace_sfons ? {
			eprintln('[sfons] pipeline created: id=${sf.pip.id}')
		}
	}

	// Create sampler if not yet created
	if sf.smp.id == 0 {
		smp_desc := gfx.SamplerDesc{
			min_filter:    .linear
			mag_filter:    .linear
			mipmap_filter: .none
		}
		sf.smp = gfx.make_sampler(&smp_desc)
	}

	// Destroy and recreate font atlas texture
	if sf.img.id != 0 {
		gfx.destroy_image(sf.img)
		sf.img = gfx.Image{}
	}
	sf.cur_width = width
	sf.cur_height = height

	img_desc := gfx.ImageDesc{
		width:        sf.cur_width
		height:       sf.cur_height
		usage:        .dynamic
		pixel_format: .r8
	}
	sf.img = gfx.make_image(&img_desc)
	$if trace_sfons ? {
		eprintln('[sfons] render_create: ${width}x${height} img.id=${sf.img.id} smp.id=${sf.smp.id} pip.id=${sf.pip.id} shd.id=${sf.shd.id}')
	}
	return 1
}

fn sfons_render_resize(user_ptr voidptr, width int, height int) int {
	return sfons_render_create(user_ptr, width, height)
}

fn sfons_render_update(user_ptr voidptr, rect &int, data &u8) {
	assert user_ptr != unsafe { nil }
	mut sf := unsafe { &SFons(user_ptr) }
	sf.img_dirty = true
}

fn sfons_render_draw(user_ptr voidptr, verts &f32, tcoords &f32, colors &u32, nverts int) {
	assert user_ptr != unsafe { nil }
	assert nverts > 0
	sf := unsafe { &SFons(user_ptr) }
	$if trace_sfons ? {
		eprintln('[sfons] render_draw: nverts=${nverts} img.id=${sf.img.id} smp.id=${sf.smp.id} pip.id=${sf.pip.id}')
		if nverts > 0 {
			unsafe {
				eprintln('[sfons]   vert[0]: pos=(${verts[0]}, ${verts[1]}) uv=(${tcoords[0]}, ${tcoords[1]}) col=0x${colors[0]:08x}')
			}
		}
	}
	sgl.enable_texture()
	sgl.texture(sf.img, sf.smp)
	sgl.push_pipeline()
	sgl.load_pipeline(sf.pip)
	sgl.begin_triangles()
	for i in 0 .. nverts {
		unsafe {
			sgl.v2f_t2f_c1i(verts[2 * i], verts[2 * i + 1], tcoords[2 * i], tcoords[2 * i + 1],
				colors[i])
		}
	}
	sgl.end()
	sgl.pop_pipeline()
	sgl.disable_texture()
}

fn sfons_render_delete(user_ptr voidptr) {
	assert user_ptr != unsafe { nil }
	mut sf := unsafe { &SFons(user_ptr) }
	if sf.img.id != 0 {
		gfx.destroy_image(sf.img)
		sf.img = gfx.Image{}
	}
	if sf.smp.id != 0 {
		gfx.destroy_sampler(sf.smp)
		sf.smp = gfx.Sampler{}
	}
	if sf.pip.id != 0 {
		sgl.destroy_pipeline(sf.pip)
		sf.pip = sgl.Pipeline{}
	}
	if sf.shd.id != 0 {
		gfx.destroy_shader(sf.shd)
		sf.shd = gfx.Shader{}
	}
}

// Public API

// create a new Context/font atlas, for rendering glyphs, given its dimensions `width` and `height`
pub fn create(width int, height int, flags int) &fontstash.Context {
	assert is_power_of_two(width)
	assert is_power_of_two(height)

	w := if width == 0 { 512 } else { width }
	h := if height == 0 { 512 } else { height }

	// Allocate the internal state
	sf := unsafe { &SFons(C.calloc(1, sizeof(SFons))) }

	params := C.FONSparams{
		width:        w
		height:       h
		flags:        char(flags)
		userPtr:      voidptr(sf)
		renderCreate: sfons_render_create
		renderResize: sfons_render_resize
		renderUpdate: sfons_render_update
		renderDraw:   sfons_render_draw
		renderDelete: sfons_render_delete
	}
	return fontstash.create_internal(&params)
}

// destroy frees the font stash context and associated resources.
pub fn destroy(ctx &fontstash.Context) {
	// Get the SFons pointer from the context's params before destroying
	sfons_ptr := unsafe { (&C.FONScontext(ctx)).params.userPtr }
	fontstash.delete_internal(ctx)
	unsafe { C.free(sfons_ptr) }
}

// rgba packs RGBA color components into a single u32 value.
pub fn rgba(r u8, g u8, b u8, a u8) u32 {
	return u32(r) | (u32(g) << 8) | (u32(b) << 16) | (u32(a) << 24)
}

// flush uploads the font texture to the GPU and renders any pending text.
pub fn flush(ctx &fontstash.Context) {
	fctx := unsafe { &C.FONScontext(ctx) }
	sf := unsafe { &SFons(fctx.params.userPtr) }
	if sf.img_dirty {
		unsafe {
			mut s := &SFons(fctx.params.userPtr)
			s.img_dirty = false
		}
		mut img_data := gfx.ImageData{}
		img_data.subimage[0][0] = gfx.Range{
			ptr:  unsafe { fctx.texData }
			size: usize(sf.cur_width * sf.cur_height)
		}
		$if trace_sfons ? {
			eprintln('[sfons] flush: uploading texture ${sf.cur_width}x${sf.cur_height} to img.id=${sf.img.id} texData=${voidptr(fctx.texData)}')
		}
		gfx.update_image(sf.img, &img_data)
	}
	$if trace_sfons ? {
		if !sf.img_dirty {
			eprintln('[sfons] flush: not dirty')
		}
	}
}

fn is_power_of_two(x int) bool {
	return x in [2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768]
}
