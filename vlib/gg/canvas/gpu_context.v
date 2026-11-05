module canvas

import sokol.gfx

$if macos {
	#include "@DIR/gpu_metal_darwin.h"
}

fn C.v_canvas_metal_device() voidptr

// The MSL version and fast-math mode Skia Graphite compiles its shaders with.
// Floating-point results depend on both, so the shaders must use them too.
const graphite_msl_major = 2
const graphite_msl_minor = 3

// The intrinsic uniform block: float4 viewport, float4 dstReadBounds.
const intrinsic_uniform_size = 32
// The solid-color paint uniforms: one float4 premultiplied color per draw of
// an instanced batch, selected by the instance's ssboIndex like Graphite's
// storage-buffer uniforms.
const solid_uniform_size = 16
// A Graphite draw list holds at most 4096 distinct uniform data blocks
// (DenseBiMap::kInvalidIndex), so a batch never needs more colors.
const max_batch_colors = 4096
const solid_uniform_block_size = solid_uniform_size * max_batch_colors

enum RenderStep {
	analytic_rrect
	cover_bounds
	// The stencil and cover steps of Graphite's MSAA stencil-and-cover path
	// renderer (StencilTessellatedWedges): wedges counted into the stencil
	// buffer with the winding or even-odd rule, then the path bounds filled
	// where the stencil is non-zero.
	stencil_wedges_winding
	stencil_wedges_even_odd
	cover_fill
	// The depth-only convex wedges of a clip element's draw (Graphite's
	// TessellateWedges_Convex with no paint), executed in MSAA passes.
	convex_wedges
	// TessellateWedges_Convex with a solid color paint, for convex path fills.
	wedges_shaded
	// TessellateStrokes with a solid color paint.
	tessellate_strokes
	// The depth-only inverse cover of an intersect clip element after its
	// stencil wedges (CoverBounds_InverseCover with no paint): the draw's
	// scissor where the stencil is zero.
	cover_inverse_clip
	// The depth-only regular cover of a non-convex difference clip element
	// (CoverBounds_RegularCover with no paint): its bounds where the stencil
	// is non-zero.
	cover_clip
	// The shaded inverse cover of an inverse-filled path after its stencil
	// wedges (CoverBounds_InverseCover): the draw's scissor where the stencil
	// is zero.
	cover_inverse_fill
}

// is_wedge_step reports the steps that draw tessellated wedge patches.
fn is_wedge_step(step RenderStep) bool {
	return step in [.stencil_wedges_winding, .stencil_wedges_even_odd, .convex_wedges, .wedges_shaded]
}

// writes_color reports the steps with a fragment function.
fn writes_color(step RenderStep) bool {
	return step !in [.stencil_wedges_winding, .stencil_wedges_even_odd, .convex_wedges,
		.cover_inverse_clip, .cover_clip]
}

// has_uniforms reports the steps with a uniform block; the others sort and
// batch with UniformDataCache::kInvalidIndex.
fn has_uniforms(step RenderStep) bool {
	return step !in [.cover_inverse_clip, .cover_clip]
}

// step_uniform_size returns the size of a step's uniform block: the
// localToDevice matrix of wedge steps (after the color for shaded wedges),
// or the paint's color or gradient with the analytic clip data.
fn step_uniform_size(step RenderStep, clipped bool, shading Shading) int {
	if shading == .image {
		return if clipped { image_uniform_size + clip_block_size } else { image_uniform_size }
	}
	if shading.is_gradient() {
		// The gradient replaces the color; the clip data and the step's
		// transform follow it.
		mut size := gradient_uniform_size(shading)
		if clipped {
			size += clip_block_size
		}
		return match step {
			.wedges_shaded { size + wedge_solid_uniform_size - solid_uniform_size }
			.tessellate_strokes { size + stroke_uniform_size - solid_uniform_size }
			else { size }
		}
	}
	return match true {
		step == .wedges_shaded && clipped { wedge_solid_uniform_size + clip_block_size }
		step == .wedges_shaded { wedge_solid_uniform_size }
		step == .tessellate_strokes && clipped { stroke_uniform_size + clip_block_size }
		step == .tessellate_strokes { stroke_uniform_size }
		is_wedge_step(step) || clipped { clip_uniform_size }
		else { solid_uniform_size }
	}
}

const analytic_rrect_vert = $embed_file('shaders/analytic_rrect_solid.vert.metal')
const wedges_solid_vert = $embed_file('shaders/tessellate_wedges_solid.vert.metal')
const wedges_solid_frag = $embed_file('shaders/tessellate_wedges_solid.frag.metal')
const strokes_solid_vert = $embed_file('shaders/tessellate_strokes_solid.vert.metal')
const strokes_solid_frag = $embed_file('shaders/tessellate_strokes_solid.frag.metal')
// The shaded wedges and strokes with Graphite's analytic NonMSAAClip.
const wedges_clip_vert = $embed_file('shaders/tessellate_wedges_solid_clip.vert.metal')
const wedges_clip_frag = $embed_file('shaders/tessellate_wedges_solid_clip.frag.metal')
const strokes_clip_vert = $embed_file('shaders/tessellate_strokes_solid_clip.vert.metal')
const strokes_clip_frag = $embed_file('shaders/tessellate_strokes_solid_clip.frag.metal')
// The analytic clip's rect, radiusPlusHalf (padded to 16 bytes) and
// edgeSelect, which follow the paint color in clipped uniform blocks.
const clip_block_size = 48

// The shaded wedge uniforms: float4 color, then the float4x4 localToDevice.
const wedge_solid_uniform_size = 80
const analytic_rrect_frag = $embed_file('shaders/analytic_rrect_solid.frag.metal')
const cover_bounds_vert = $embed_file('shaders/cover_bounds_solid.vert.metal')
const cover_bounds_frag = $embed_file('shaders/cover_bounds_solid.frag.metal')
// The same render steps with Graphite's analytic NonMSAAClip in the paint.
const analytic_rrect_clip_vert = $embed_file('shaders/analytic_rrect_solid_clip.vert.metal')
const analytic_rrect_clip_frag = $embed_file('shaders/analytic_rrect_solid_clip.frag.metal')
const cover_bounds_clip_vert = $embed_file('shaders/cover_bounds_solid_clip.vert.metal')
const cover_bounds_clip_frag = $embed_file('shaders/cover_bounds_solid_clip.frag.metal')
const convex_wedges_vert = $embed_file('shaders/tessellate_wedges_convex.vert.metal')
const cover_depth_vert = $embed_file('shaders/cover_bounds_depth.vert.metal')
// Graphite's LoadMSAAFromResolve pipeline (MtlGraphicsPipeline.mm): a
// fullscreen draw that copies the resolve texture into a multisampled
// attachment before the pass's draws.
const load_msaa_from_resolve = $embed_file('shaders/load_msaa_from_resolve.metal')

// The capacity of the shader and pipeline caches: more than the combinations
// of render steps, clipping, shading, sample counts and blending.
const max_shaders = 128
const max_pipelines = 512

// GpuContext owns the GPU resources shared by all surfaces: shaders,
// pipelines and the static vertex data of the render steps. It uses the
// application's sokol.gfx context, or sets up a headless Metal one when
// sokol.gfx is not initialized yet.
@[heap]
pub struct GpuContext {
mut:
	owns_gfx           bool
	shaders            []GpuShaderEntry
	pipelines          []GpuPipelineEntry
	rrect_vertices     gfx.Buffer
	rrect_indices      gfx.Buffer
	wedge_vertices     gfx.Buffer
	wedge_indices      gfx.Buffer
	instances          gfx.Buffer
	instance_capacity  int
	msaa_load_shader   gfx.Shader
	msaa_load_pipeline gfx.Pipeline
	msaa_load_sampler  gfx.Sampler
	// The dither LUT of gradient paints and its nearest, repeating sampler.
	dither_lut     gfx.Image
	dither_sampler gfx.Sampler
	// The nearest, clamping sampler of layer images.
	image_sampler gfx.Sampler
}

struct GpuShaderEntry {
	step    RenderStep
	clipped bool
	shading Shading
	shader  gfx.Shader
}

struct GpuPipelineEntry {
	step         RenderStep
	clipped      bool
	shading      Shading
	sample_count int
	blend        bool
	pipeline     gfx.Pipeline
}

// new_gpu_context creates a context for exact GPU rendering. Only Metal is
// supported; the shaders reproduce Skia Graphite's Metal output.
pub fn new_gpu_context() !&GpuContext {
	$if !macos {
		return error('gg.canvas: GPU rendering requires Metal')
	}
	mut ctx := &GpuContext{
		shaders:   []GpuShaderEntry{cap: max_shaders}
		pipelines: []GpuPipelineEntry{cap: max_pipelines}
	}
	if !gfx.is_valid() {
		mut desc := gfx.Desc{}
		desc.environment.defaults.color_format = .bgra8
		desc.environment.defaults.depth_format = .none
		desc.environment.defaults.sample_count = 1
		desc.environment.metal.device = C.v_canvas_metal_device()
		// Large batches upload up to 64KB of uniforms per draw call.
		desc.uniform_buffer_size = 16 * 1024 * 1024
		desc.shader_pool_size = max_shaders
		desc.pipeline_pool_size = max_pipelines
		if desc.environment.metal.device == unsafe { nil } {
			return error('gg.canvas: no Metal device')
		}
		gfx.setup(&desc)
		if !gfx.is_valid() {
			return error('gg.canvas: sokol.gfx setup failed')
		}
		ctx.owns_gfx = true
	}
	if gfx.query_backend() != .metal_macos {
		return error('gg.canvas: GPU rendering requires the Metal backend')
	}
	vertices := analytic_rrect_vertex_data()
	ctx.rrect_vertices = gfx.make_buffer(&gfx.BufferDesc{
		data: gfx.Range{
			ptr:  vertices.data
			size: u32(vertices.len)
		}
	})
	indices := analytic_rrect_indices
	ctx.rrect_indices = gfx.make_buffer(&gfx.BufferDesc{
		@type: .indexbuffer
		data:  gfx.Range{
			ptr:  unsafe { &indices[0] }
			size: u32(sizeof(u16) * u32(indices.len))
		}
	})
	wedge_vertices := wedge_vertex_data()
	ctx.wedge_vertices = gfx.make_buffer(&gfx.BufferDesc{
		data: gfx.Range{
			ptr:  wedge_vertices.data
			size: u32(sizeof(f32) * u32(wedge_vertices.len))
		}
	})
	wedge_indices := wedge_index_data()
	ctx.wedge_indices = gfx.make_buffer(&gfx.BufferDesc{
		@type: .indexbuffer
		data:  gfx.Range{
			ptr:  wedge_indices.data
			size: u32(sizeof(u16) * u32(wedge_indices.len))
		}
	})
	ctx.ensure_instance_capacity(64 * 1024)
	return ctx
}

// free releases the context's GPU resources, and shuts sokol.gfx down if the
// context set it up.
pub fn (mut ctx GpuContext) free() {
	for entry in ctx.pipelines {
		gfx.destroy_pipeline(entry.pipeline)
	}
	for entry in ctx.shaders {
		gfx.destroy_shader(entry.shader)
	}
	ctx.pipelines.clear()
	ctx.shaders.clear()
	if ctx.msaa_load_pipeline.id != 0 {
		gfx.destroy_pipeline(ctx.msaa_load_pipeline)
		gfx.destroy_shader(ctx.msaa_load_shader)
		gfx.destroy_sampler(ctx.msaa_load_sampler)
		ctx.msaa_load_pipeline = gfx.Pipeline{}
	}
	if ctx.dither_lut.id != 0 {
		gfx.destroy_image(ctx.dither_lut)
		gfx.destroy_sampler(ctx.dither_sampler)
		ctx.dither_lut = gfx.Image{}
	}
	if ctx.image_sampler.id != 0 {
		gfx.destroy_sampler(ctx.image_sampler)
		ctx.image_sampler = gfx.Sampler{}
	}
	gfx.destroy_buffer(ctx.rrect_vertices)
	gfx.destroy_buffer(ctx.rrect_indices)
	gfx.destroy_buffer(ctx.wedge_vertices)
	gfx.destroy_buffer(ctx.wedge_indices)
	gfx.destroy_buffer(ctx.instances)
	if ctx.owns_gfx {
		gfx.shutdown()
		ctx.owns_gfx = false
	}
}

// precompile creates every shader and pipeline that solid-color draws may
// use, so later draws compile nothing. Embedders whose allocations are scoped
// can call it where long-lived allocations belong. Gradient pipelines, of
// which a program usually needs few, are created on first use; the caches
// have room for every pipeline, so that never reallocates them.
pub fn (mut ctx GpuContext) precompile() {
	for step in [RenderStep.analytic_rrect, .cover_bounds] {
		for clipped in [false, true] {
			for sample_count in [1, 4] {
				for blend in [true, false] {
					ctx.pipeline(step, sample_count, blend, clipped, .solid)
				}
			}
		}
	}
	ctx.pipeline(.convex_wedges, 4, false, false, .solid)
	ctx.pipeline(.cover_inverse_clip, 4, false, false, .solid)
	ctx.pipeline(.cover_clip, 4, false, false, .solid)
	for step in [RenderStep.stencil_wedges_winding, .stencil_wedges_even_odd] {
		ctx.pipeline(step, 4, false, false, .solid)
	}
	for step in [RenderStep.cover_fill, .cover_inverse_fill, .wedges_shaded, .tessellate_strokes] {
		for blend in [true, false] {
			ctx.pipeline(step, 4, blend, false, .solid)
		}
		// Analytic clips need coverage blending.
		ctx.pipeline(step, 4, true, true, .solid)
	}
	ctx.load_msaa_pipeline()
}

fn (mut ctx GpuContext) ensure_instance_capacity(bytes int) {
	if ctx.instance_capacity >= bytes && ctx.instances.id != 0 {
		return
	}
	mut capacity := if ctx.instance_capacity > 0 { ctx.instance_capacity } else { 64 * 1024 }
	for capacity < bytes {
		capacity *= 2
	}
	if ctx.instances.id != 0 {
		gfx.destroy_buffer(ctx.instances)
	}
	ctx.instances = gfx.make_buffer(&gfx.BufferDesc{
		size:  u32(capacity)
		usage: .stream
	})
	ctx.instance_capacity = capacity
}

// solid_shader_sources returns the vertex and fragment MSL of a render step
// with a solid color paint.
fn solid_shader_sources(step RenderStep, clipped bool) (string, string) {
	// Depth-only steps have no fragment function, like Graphite's; their
	// fragment entry is unused.
	vert, frag := match true {
		step in [.cover_inverse_clip, .cover_clip] { cover_depth_vert, cover_depth_vert }
		!writes_color(step) { convex_wedges_vert, convex_wedges_vert }
		step == .wedges_shaded && clipped { wedges_clip_vert, wedges_clip_frag }
		step == .wedges_shaded { wedges_solid_vert, wedges_solid_frag }
		step == .tessellate_strokes && clipped { strokes_clip_vert, strokes_clip_frag }
		step == .tessellate_strokes { strokes_solid_vert, strokes_solid_frag }
		step == .analytic_rrect && clipped { analytic_rrect_clip_vert, analytic_rrect_clip_frag }
		step == .analytic_rrect { analytic_rrect_vert, analytic_rrect_frag }
		clipped { cover_bounds_clip_vert, cover_bounds_clip_frag }
		else { cover_bounds_vert, cover_bounds_frag }
	}
	return vert.to_string(), frag.to_string()
}

fn (mut ctx GpuContext) shader(step RenderStep, clipped bool, shading Shading) gfx.Shader {
	for entry in ctx.shaders {
		if entry.step == step && entry.clipped == clipped && entry.shading == shading {
			return entry.shader
		}
	}
	mut vs_source, mut fs_source := solid_shader_sources(step, clipped)
	if shading == .image {
		vs_source, fs_source = image_shader_sources(vs_source, fs_source, clipped)
	} else if shading.is_gradient() {
		vs_source = gradient_vertex_source(vs_source, step, shading, clipped)
		fs_source = gradient_fragment_source(fs_source, shading, clipped)
	}
	// The per-draw uniform block: the localToDevice matrix of wedge steps, or
	// the paint color with the analytic clip data.
	paint_uniform_size := step_uniform_size(step, clipped, shading)
	mut desc := gfx.ShaderDesc{}
	desc.vs.source = &char(vs_source.str)
	desc.vs.entry = c'vertexMain'
	if writes_color(step) {
		desc.fs.source = &char(fs_source.str)
		desc.fs.entry = c'fragmentMain'
	}
	for stage in [&desc.vs, &desc.fs] {
		unsafe {
			stage.uniform_blocks[0].size = intrinsic_uniform_size
			stage.uniform_blocks[1].size = u32(paint_uniform_size * max_batch_colors)
		}
	}
	if shading != .solid {
		// The Dither block's LUT or the image: texture(0) with sampler(0).
		desc.fs.images[0] = gfx.ShaderImageDesc{
			used:        true
			image_type:  ._2d
			sample_type: .float
		}
		desc.fs.samplers[0] = gfx.ShaderSamplerDesc{
			used:         true
			sampler_type: .filtering
		}
		desc.fs.image_sampler_pairs[0] = gfx.ShaderImageSamplerPairDesc{
			used:         true
			image_slot:   0
			sampler_slot: 0
		}
	}
	gfx.mtl_set_shader_compile_options(graphite_msl_major, graphite_msl_minor, true)
	shader := gfx.make_shader(&desc)
	gfx.mtl_set_shader_compile_options(0, 0, false)
	ctx.shaders << GpuShaderEntry{step, clipped, shading, shader}
	return shader
}

// layer_sampler returns the sampler of layer images: nearest filtering and
// clamped coordinates, like SkSamplingOptions() with hardware tiling.
fn (mut ctx GpuContext) layer_sampler() gfx.Sampler {
	if ctx.image_sampler.id == 0 {
		ctx.image_sampler = gfx.make_sampler(&gfx.SamplerDesc{
			min_filter: .nearest
			mag_filter: .nearest
			wrap_u:     .clamp_to_edge
			wrap_v:     .clamp_to_edge
		})
	}
	return ctx.image_sampler
}

// dither_bindings returns the dither LUT and its sampler, creating them on
// first use: an 8x8 R8 texture sampled with nearest filtering and repeat
// tiling, like Graphite's DitherShaderBlock.
fn (mut ctx GpuContext) dither_bindings() (gfx.Image, gfx.Sampler) {
	if ctx.dither_lut.id == 0 {
		lut := dither_lut()
		mut desc := gfx.ImageDesc{
			width:        8
			height:       8
			pixel_format: .r8
		}
		desc.data.subimage[0][0] = gfx.Range{
			ptr:  lut.data
			size: u32(lut.len)
		}
		ctx.dither_lut = gfx.make_image(&desc)
		ctx.dither_sampler = gfx.make_sampler(&gfx.SamplerDesc{
			min_filter: .nearest
			mag_filter: .nearest
			wrap_u:     .repeat
			wrap_v:     .repeat
		})
	}
	return ctx.dither_lut, ctx.dither_sampler
}

fn (mut ctx GpuContext) pipeline(step RenderStep, sample_count int, blend bool, clipped bool, shading Shading) gfx.Pipeline {
	for entry in ctx.pipelines {
		if entry.step == step && entry.clipped == clipped && entry.shading == shading
			&& entry.sample_count == sample_count && entry.blend == blend {
			return entry.pipeline
		}
	}
	shader := ctx.shader(step, clipped, shading)
	if gfx.query_shader_state(shader) != .valid {
		// The shader pool is exhausted (or compilation failed): cache an
		// invalid pipeline, which flush() reports.
		ctx.pipelines << GpuPipelineEntry{step, clipped, shading, sample_count, blend, gfx.Pipeline{}}
		return gfx.Pipeline{}
	}
	mut desc := gfx.PipelineDesc{
		shader:         shader
		primitive_type: .triangle_strip
		sample_count:   sample_count
		cull_mode:      .none
	}
	match step {
		.convex_wedges, .wedges_shaded, .stencil_wedges_winding, .stencil_wedges_even_odd {
			desc.primitive_type = .triangles
			desc.index_type = .uint16
			desc.layout.buffers[0].stride = 8
			desc.layout.buffers[1].stride = wedge_instance_stride
			desc.layout.buffers[1].step_func = .per_instance
			desc.layout.attrs[0] = gfx.VertexAttrDesc{
				buffer_index: 0
				format:       .float2
			}
			formats := [gfx.VertexFormat.float4, .float4, .float2, .float, .uint]
			offsets := [0, 16, 32, 40, 44]
			for i, format in formats {
				desc.layout.attrs[1 + i] = gfx.VertexAttrDesc{
					buffer_index: 1
					offset:       offsets[i]
					format:       format
				}
			}
		}
		.tessellate_strokes {
			// No vertex or index data: the shader uses the vertex ID.
			desc.index_type = .none
			desc.layout.buffers[0].stride = stroke_instance_stride
			desc.layout.buffers[0].step_func = .per_instance
			formats := [gfx.VertexFormat.float4, .float4, .float2, .float2, .float, .uint]
			offsets := [0, 16, 32, 40, 48, 52]
			for i, format in formats {
				desc.layout.attrs[i] = gfx.VertexAttrDesc{
					buffer_index: 0
					offset:       offsets[i]
					format:       format
				}
			}
		}
		.analytic_rrect {
			desc.index_type = .uint16
			desc.layout.buffers[0].stride = analytic_rrect_vertex_stride
			desc.layout.buffers[1].stride = analytic_rrect_instance_stride
			desc.layout.buffers[1].step_func = .per_instance
			formats := [gfx.VertexFormat.uint, .float2, .float2, .float, .float]
			offsets := [0, 4, 12, 20, 24]
			for i, format in formats {
				desc.layout.attrs[i] = gfx.VertexAttrDesc{
					buffer_index: 0
					offset:       offsets[i]
					format:       format
				}
			}
			instance_formats := [gfx.VertexFormat.float4, .float4, .float4, .float4, .float, .uint,
				.float3, .float3, .float3]
			instance_offsets := [0, 16, 32, 48, 64, 68, 72, 84, 96]
			for i, format in instance_formats {
				desc.layout.attrs[5 + i] = gfx.VertexAttrDesc{
					buffer_index: 1
					offset:       instance_offsets[i]
					format:       format
				}
			}
		}
		else {
			desc.index_type = .none
			desc.layout.buffers[0].stride = cover_bounds_instance_stride
			desc.layout.buffers[0].step_func = .per_instance
			formats := [gfx.VertexFormat.float4, .float, .uint, .float3, .float3, .float3]
			offsets := [0, 16, 20, 24, 36, 48]
			for i, format in formats {
				desc.layout.attrs[i] = gfx.VertexAttrDesc{
					buffer_index: 0
					offset:       offsets[i]
					format:       format
				}
			}
		}
	}
	desc.colors[0].pixel_format = .bgra8
	if !writes_color(step) {
		desc.colors[0].write_mask = .none
	}
	desc.colors[0].blend = gfx.BlendState{
		enabled:          blend
		src_factor_rgb:   .one
		dst_factor_rgb:   .one_minus_src_alpha
		op_rgb:           .add
		src_factor_alpha: .one
		dst_factor_alpha: .one_minus_src_alpha
		op_alpha:         .add
	}
	desc.depth = gfx.DepthState{
		pixel_format:  if sample_count > 1 {
			gfx.PixelFormat.depth_stencil
		} else {
			gfx.PixelFormat.depth
		}
		compare:       .less
		// The stencil passes leave the depth write to the cover pass.
		write_enabled: step !in [.stencil_wedges_winding, .stencil_wedges_even_odd]
	}
	// Graphite's kWindingStencilPass, kEvenOddStencilPass, kRegularCoverPass
	// and kInverseCoverPass.
	match step {
		.stencil_wedges_winding, .stencil_wedges_even_odd {
			winding := step == .stencil_wedges_winding
			desc.stencil = gfx.StencilState{
				enabled:    true
				front:      gfx.StencilFaceState{
					compare:       .always
					fail_op:       .keep
					depth_fail_op: .keep
					pass_op:       if winding { gfx.StencilOp.incr_wrap } else { .invert }
				}
				back:       gfx.StencilFaceState{
					compare:       .always
					fail_op:       .keep
					depth_fail_op: .keep
					pass_op:       if winding { gfx.StencilOp.decr_wrap } else { .invert }
				}
				read_mask:  0xff
				// kToggle only flips the lowest bit.
				write_mask: if winding { u8(0xff) } else { u8(0x01) }
			}
		}
		.cover_fill, .cover_clip {
			face := gfx.StencilFaceState{
				compare:       .not_equal
				fail_op:       .keep
				depth_fail_op: .zero
				pass_op:       .zero
			}
			desc.stencil = gfx.StencilState{
				enabled:    true
				front:      face
				back:       face
				read_mask:  0xff
				write_mask: 0xff
			}
		}
		.cover_inverse_clip, .cover_inverse_fill {
			// kInverseCoverPass (kPassZero).
			face := gfx.StencilFaceState{
				compare:       .equal
				fail_op:       .zero
				depth_fail_op: .keep
				pass_op:       .keep
			}
			desc.stencil = gfx.StencilState{
				enabled:    true
				front:      face
				back:       face
				read_mask:  0xff
				write_mask: 0xff
			}
		}
		else {}
	}
	pipeline := gfx.make_pipeline(&desc)
	ctx.pipelines << GpuPipelineEntry{step, clipped, shading, sample_count, blend, pipeline}
	return pipeline
}

// load_msaa_pipeline returns Graphite's LoadMSAAFromResolve pipeline for 4x
// MSAA passes: no blending, no depth test, and the resolve texture read with
// texel fetches.
fn (mut ctx GpuContext) load_msaa_pipeline() gfx.Pipeline {
	if ctx.msaa_load_pipeline.id != 0 {
		return ctx.msaa_load_pipeline
	}
	source := load_msaa_from_resolve.to_string()
	mut desc := gfx.ShaderDesc{}
	desc.vs.source = &char(source.str)
	desc.vs.entry = c'vertexMain'
	desc.fs.source = &char(source.str)
	desc.fs.entry = c'fragmentMain'
	desc.fs.images[0] = gfx.ShaderImageDesc{
		used:        true
		image_type:  ._2d
		sample_type: .float
	}
	// The shader reads texels without a sampler, but sokol pairs every image
	// with one; the sampler is bound and unused.
	desc.fs.samplers[0] = gfx.ShaderSamplerDesc{
		used:         true
		sampler_type: .filtering
	}
	desc.fs.image_sampler_pairs[0] = gfx.ShaderImageSamplerPairDesc{
		used:         true
		image_slot:   0
		sampler_slot: 0
	}
	gfx.mtl_set_shader_compile_options(graphite_msl_major, graphite_msl_minor, true)
	ctx.msaa_load_shader = gfx.make_shader(&desc)
	gfx.mtl_set_shader_compile_options(0, 0, false)
	mut pipeline := gfx.PipelineDesc{
		shader:         ctx.msaa_load_shader
		primitive_type: .triangle_strip
		sample_count:   4
		cull_mode:      .none
	}
	pipeline.colors[0].pixel_format = .bgra8
	pipeline.depth = gfx.DepthState{
		pixel_format: .depth_stencil
		compare:      .always
	}
	ctx.msaa_load_pipeline = gfx.make_pipeline(&pipeline)
	ctx.msaa_load_sampler = gfx.make_sampler(&gfx.SamplerDesc{
		min_filter: .nearest
		mag_filter: .nearest
	})
	return ctx.msaa_load_pipeline
}
