module gfx

// C.sg_desc describes
pub struct C.sg_desc {
pub mut:
	_start_canary                                   u32
	buffer_pool_size                                int
	image_pool_size                                 int
	sampler_pool_size                               int
	shader_pool_size                                int
	pipeline_pool_size                              int
	attachments_pool_size                           int
	uniform_buffer_size                             int
	max_commit_listeners                            int
	disable_validation                              bool // disable validation layer even in debug mode, useful for tests
	mtl_force_managed_storage_mode                  bool // for debugging: use Metal managed storage mode for resources even with UMA
	mtl_use_command_buffer_with_retained_references bool // Metal: use a managed MTLCommandBuffer which ref-counts used resources
	wgpu_disable_bindgroups_cache                   bool // set to true to disable the WebGPU backend BindGroup cache
	wgpu_bindgroups_cache_size                      int  // number of slots in the WebGPU bindgroup cache (must be 2^N)
	allocator                                       C.sg_allocator
	logger                                          C.sg_logger // optional log function override
	environment                                     C.sg_environment
	_end_canary                                     u32
}

pub type Desc = C.sg_desc

pub struct C.sg_color_target_state {
pub mut:
	pixel_format PixelFormat
	write_mask   ColorMask
	blend        BlendState
}

pub type ColorTargetState = C.sg_color_target_state

pub struct C.sg_pipeline_desc {
pub mut:
	shader                    Shader
	layout                    VertexLayoutState
	depth                     DepthState
	stencil                   StencilState
	color_count               int
	colors                    [4]ColorTargetState // C.SG_MAX_COLOR_ATTACHMENTS
	primitive_type            PrimitiveType
	index_type                IndexType
	cull_mode                 CullMode
	face_winding              FaceWinding
	sample_count              int
	blend_color               Color
	alpha_to_coverage_enabled bool
	label                     &char = &char(unsafe { nil })
}

pub type PipelineDesc = C.sg_pipeline_desc

pub struct C.sg_pipeline_info {
pub:
	slot SlotInfo
}

pub type PipelineInfo = C.sg_pipeline_info

pub struct C.sg_pipeline {
pub:
	id u32
}

pub type Pipeline = C.sg_pipeline

pub fn (mut p C.sg_pipeline) free() {
	C.sg_destroy_pipeline(*p)
}

pub struct C.sg_attachments {
	id u32
}

// Attachments represents data that can be attached to a render pass.
// See also: documentation at the top of thirdparty/sokol/sokol_gfx.h
pub type Attachments = C.sg_attachments

// free frees the resources occupied by the struct
pub fn (mut a C.sg_attachments) free() {
	C.sg_destroy_attachments(*a)
}

pub struct C.sg_attachment_desc {
pub mut:
	image     Image
	mip_level int
	slice     int
}

pub type AttachmentDesc = C.sg_attachment_desc

pub struct C.sg_attachments_desc {
pub mut:
	_start_canary u32
	colors        [4]AttachmentDesc
	resolves      [4]AttachmentDesc
	depth_stencil AttachmentDesc
	label         &char = unsafe { nil }
	_end_canary   u32
}

pub type AttachmentsDesc = C.sg_attachments_desc

pub struct C.sg_attachments_info {
pub mut:
	slot SlotInfo // resource pool slot info
}

pub type AttachmentsInfo = C.sg_attachments_info

pub struct C.sg_bindings {
pub mut:
	_start_canary         u32
	vertex_buffers        [8]Buffer
	vertex_buffer_offsets [8]int
	index_buffer          Buffer
	index_buffer_offset   int
	vs                    StageBindings
	fs                    StageBindings
	_end_canary           u32
	// vs_images             [8]Image // old
	// fs_images             [8]Image // old
}

pub type Bindings = C.sg_bindings

pub fn (mut b Bindings) set_vert_image(index int, img Image) {
	b.vs.images[index] = img
}

pub fn (mut b Bindings) set_frag_image(index int, img Image) {
	b.fs.images[index] = img
}

pub fn (b &Bindings) update_vert_buffer(index int, data voidptr, element_size int, element_count int) {
	range := Range{
		ptr:  data
		size: usize(element_size * element_count)
	}
	C.sg_update_buffer(b.vertex_buffers[index], &range)
}

pub fn (b &Bindings) append_vert_buffer(index int, data voidptr, element_size int, element_count int) int {
	range := Range{
		ptr:  data
		size: usize(element_size * element_count)
	}
	return C.sg_append_buffer(b.vertex_buffers[index], &range)
}

pub fn (b &Bindings) update_index_buffer(data voidptr, element_size int, element_count int) {
	range := Range{
		ptr:  data
		size: usize(element_size * element_count)
	}
	C.sg_update_buffer(b.index_buffer, &range)
}

pub fn (b &Bindings) append_index_buffer(data voidptr, element_size int, element_count int) int {
	range := Range{
		ptr:  data
		size: usize(element_size * element_count)
	}
	return C.sg_append_buffer(b.index_buffer, &range)
}

pub struct C.sg_stage_bindings {
pub mut:
	images   [12]Image
	samplers [8]Sampler
}

pub type StageBindings = C.sg_stage_bindings

@[heap]
pub struct C.sg_shader_desc {
pub mut:
	attrs [16]ShaderAttrDesc
	vs    ShaderStageDesc
	fs    ShaderStageDesc
	label &char
}

pub type ShaderDesc = C.sg_shader_desc

pub fn (mut desc C.sg_shader_desc) set_vert_src(src string) &ShaderDesc {
	desc.vs.source = &char(src.str)
	return desc
}

pub fn (mut desc C.sg_shader_desc) set_frag_src(src string) &ShaderDesc {
	desc.fs.source = &char(src.str)
	return desc
}

pub fn (mut desc C.sg_shader_desc) set_vert_image(index int, name string) &ShaderDesc {
	// desc.vs.images[index].name = &char(name.str)
	desc.vs.images[index].image_type = ._2d
	return desc
}

pub fn (mut desc C.sg_shader_desc) set_frag_image(index int, name string) &ShaderDesc {
	// desc.fs.images[index].name = &char(name.str)
	desc.fs.images[index].image_type = ._2d
	return desc
}

pub fn (mut desc C.sg_shader_desc) set_vert_uniform_block_size(block_index int, size usize) &ShaderDesc {
	desc.vs.uniform_blocks[block_index].size = size
	return desc
}

pub fn (mut desc C.sg_shader_desc) set_frag_uniform_block_size(block_index int, size usize) &ShaderDesc {
	desc.fs.uniform_blocks[block_index].size = size
	return desc
}

pub fn (mut desc C.sg_shader_desc) set_vert_uniform(block_index int, uniform_index int, name string, typ UniformType,
	array_count int) &ShaderDesc {
	desc.vs.uniform_blocks[block_index].uniforms[uniform_index].name = &char(name.str)
	desc.vs.uniform_blocks[block_index].uniforms[uniform_index].type = typ
	return desc
}

pub fn (mut desc C.sg_shader_desc) set_frag_uniform(block_index int, uniform_index int, name string, typ UniformType,
	array_count int) &ShaderDesc {
	desc.fs.uniform_blocks[block_index].uniforms[uniform_index].name = &char(name.str)
	desc.fs.uniform_blocks[block_index].uniforms[uniform_index].type = typ
	return desc
}

pub fn (desc &ShaderDesc) make_shader() Shader {
	return C.sg_make_shader(desc)
}

pub struct C.sg_shader_attr_desc {
pub mut:
	name      &char // GLSL vertex attribute name (only required for GLES2)
	sem_name  &char // HLSL semantic name
	sem_index int   // HLSL semantic index
}

pub type ShaderAttrDesc = C.sg_shader_attr_desc

pub struct C.sg_shader_stage_desc {
pub mut:
	source              &char
	bytecode            Range
	entry               &char
	d3d11_target        &char
	uniform_blocks      [4]ShaderUniformBlockDesc
	storage_buffers     [8]ShaderStorageBufferDesc
	images              [12]ShaderImageDesc
	samplers            [8]ShaderSamplerDesc
	image_sampler_pairs [12]ShaderImageSamplerPairDesc
}

pub type ShaderStageDesc = C.sg_shader_stage_desc

pub fn (mut desc ShaderStageDesc) set_image(index int, name string) ShaderStageDesc {
	// desc.images[index].name = &char(name.str)
	desc.images[index].image_type = ._2d
	return *desc
}

pub struct C.sg_shader_uniform_block_desc {
pub mut:
	size     usize
	layout   UniformLayout
	uniforms [16]ShaderUniformDesc
}

pub type ShaderUniformBlockDesc = C.sg_shader_uniform_block_desc

pub struct C.sg_shader_uniform_desc {
pub mut:
	name        &char
	type        UniformType
	array_count int
}

pub type ShaderUniformDesc = C.sg_shader_uniform_desc

pub struct C.sg_shader_image_desc {
pub mut:
	used         bool
	multisampled bool
	// name         &char
	image_type  ImageType
	sample_type ImageSampleType
}

pub type ShaderImageDesc = C.sg_shader_image_desc

pub struct C.sg_shader_storage_buffer_desc {
pub mut:
	used     bool
	readonly bool
}

pub type ShaderStorageBufferDesc = C.sg_shader_storage_buffer_desc

pub struct C.sg_shader_sampler_desc {
pub mut:
	used         bool
	sampler_type SamplerType
}

pub type ShaderSamplerDesc = C.sg_shader_sampler_desc

pub struct C.sg_shader_image_sampler_pair_desc {
pub mut:
	used         bool
	image_slot   int
	sampler_slot int
	glsl_name    &char
}

pub type ShaderImageSamplerPairDesc = C.sg_shader_image_sampler_pair_desc

pub struct C.sg_shader_info {
pub:
	slot SlotInfo
}

pub type ShaderInfo = C.sg_shader_info

pub struct C.sg_range {
pub mut:
	ptr  voidptr
	size usize
}

pub type Range = C.sg_range

pub struct C.sg_color {
pub mut:
	r f32
	g f32
	b f32
	a f32
}

pub type Color = C.sg_color

pub struct C.sg_shader {
pub:
	id u32
}

pub type Shader = C.sg_shader

pub fn (mut s Shader) free() {
	C.sg_destroy_shader(*s)
}

@[typedef]
pub struct C.sg_frame_stats_gl {
	num_bind_buffer                 u32
	num_active_texture              u32
	num_bind_texture                u32
	num_bind_sampler                u32
	num_use_program                 u32
	num_render_state                u32
	num_vertex_attrib_pointer       u32
	num_vertex_attrib_divisor       u32
	num_enable_vertex_attrib_array  u32
	num_disable_vertex_attrib_array u32
	num_uniform                     u32
}

pub type FrameStatsGL = C.sg_frame_stats_gl

@[typedef]
pub struct C.sg_frame_stats_d3d11_pass {
	num_om_set_render_targets    u32
	num_clear_render_target_view u32
	num_clear_depth_stencil_view u32
	num_resolve_subresource      u32
}

pub type FrameStatsD3D11Pass = C.sg_frame_stats_d3d11_pass

@[typedef]
pub struct C.sg_frame_stats_d3d11_pipeline {
	num_rs_set_state               u32
	num_om_set_depth_stencil_state u32
	num_om_set_blend_state         u32
	num_ia_set_primitive_topology  u32
	num_ia_set_input_layout        u32
	num_vs_set_shader              u32
	num_vs_set_constant_buffers    u32
	num_ps_set_shader              u32
	num_ps_set_constant_buffers    u32
}

pub type FrameStatsD3D11Pipeline = C.sg_frame_stats_d3d11_pipeline

@[typedef]
pub struct C.sg_frame_stats_d3d11_bindings {
	num_ia_set_vertex_buffers   u32
	num_ia_set_index_buffer     u32
	num_vs_set_shader_resources u32
	num_ps_set_shader_resources u32
	num_vs_set_samplers         u32
	num_ps_set_samplers         u32
}

pub type FrameStatsD3D11Bindings = C.sg_frame_stats_d3d11_bindings

@[typedef]
pub struct C.sg_frame_stats_d3d11_uniforms {
	num_update_subresource u32
}

pub type FrameStatsD3D11Uniforms = C.sg_frame_stats_d3d11_uniforms

@[typedef]
pub struct C.sg_frame_stats_d3d11_draw {
	num_draw_indexed_instanced u32
	num_draw_indexed           u32
	num_draw_instanced         u32
	num_draw                   u32
}

pub type FrameStatsD3D11Draw = C.sg_frame_stats_d3d11_draw

@[typedef]
pub struct C.sg_frame_stats_d3d11 {
	pass      FrameStatsD3D11Pass
	pipeline  FrameStatsD3D11Pipeline
	bindings  FrameStatsD3D11Bindings
	uniforms  FrameStatsD3D11Uniforms
	draw      FrameStatsD3D11Draw
	num_map   u32
	num_unmap u32
}

pub type FrameStatsD3D11 = C.sg_frame_stats_d3d11

@[typedef]
pub struct C.sg_frame_stats_metal_idpool {
	num_added             u32
	num_released          u32
	num_garbage_collected u32
}

pub type FrameStatsMetalIdpool = C.sg_frame_stats_metal_idpool

@[typedef]
pub struct C.sg_frame_stats_metal_pipeline {
	num_set_blend_color             u32
	num_set_cull_mode               u32
	num_set_front_facing_winding    u32
	num_set_stencil_reference_value u32
	num_set_depth_bias              u32
	num_set_render_pipeline_state   u32
	num_set_depth_stencil_state     u32
}

pub type FrameStatsMetalPipeline = C.sg_frame_stats_metal_pipeline

@[typedef]
pub struct C.sg_frame_stats_metal_bindings {
	num_set_vertex_buffer          u32
	num_set_vertex_texture         u32
	num_set_vertex_sampler_state   u32
	num_set_fragment_buffer        u32
	num_set_fragment_texture       u32
	num_set_fragment_sampler_state u32
}

pub type FrameStatsMetalBindings = C.sg_frame_stats_metal_bindings

@[typedef]
pub struct C.sg_frame_stats_metal_uniforms {
	num_set_vertex_buffer_offset   u32
	num_set_fragment_buffer_offset u32
}

pub type FrameStatsMetalUniforms = C.sg_frame_stats_metal_uniforms

@[typedef]
pub struct C.sg_frame_stats_metal {
	idpool   FrameStatsMetalIdpool
	pipeline FrameStatsMetalPipeline
	bindings FrameStatsMetalBindings
	uniforms FrameStatsMetalUniforms
}

pub type FrameStatsMetal = C.sg_frame_stats_metal

@[typedef]
pub struct C.sg_frame_stats_wgpu_uniforms {
	num_set_bindgroup u32
	size_write_buffer u32
}

pub type FrameStatsWGPUUniforms = C.sg_frame_stats_wgpu_uniforms

@[typedef]
pub struct C.sg_frame_stats_wgpu_bindings {
	num_set_vertex_buffer                    u32
	num_skip_redundant_vertex_buffer         u32
	num_set_index_buffer                     u32
	num_skip_redundant_index_buffer          u32
	num_create_bindgroup                     u32
	num_discard_bindgroup                    u32
	num_set_bindgroup                        u32
	num_skip_redundant_bindgroup             u32
	num_bindgroup_cache_hits                 u32
	num_bindgroup_cache_misses               u32
	num_bindgroup_cache_collisions           u32
	num_bindgroup_cache_hash_vs_key_mismatch u32
}

pub type FrameStatsWGPUBindings = C.sg_frame_stats_wgpu_bindings

@[typedef]
pub struct C.sg_frame_stats_wgpu {
	uniforms FrameStatsWGPUUniforms
	bindings FrameStatsWGPUBindings
}

pub type FrameStatsWGPU = C.sg_frame_stats_wgpu

@[typedef]
pub struct C.sg_frame_stats {
	frame_index u32 // current frame counter, starts at 0

	num_passes             u32
	num_apply_viewport     u32
	num_apply_scissor_rect u32
	num_apply_pipeline     u32
	num_apply_bindings     u32
	num_apply_uniforms     u32
	num_draw               u32
	num_update_buffer      u32
	num_append_buffer      u32
	num_update_image       u32

	size_apply_uniforms u32
	size_update_buffer  u32
	size_append_buffer  u32
	size_update_image   u32

	gl    FrameStatsGL
	d3d11 FrameStatsD3D11
	metal FrameStatsMetal
	wgpu  FrameStatsWGPU
}

pub type FrameStats = C.sg_frame_stats

pub struct C.sg_pass_action {
pub mut:
	colors  [4]ColorAttachmentAction
	depth   DepthAttachmentAction
	stencil StencilAttachmentAction
}

pub type PassAction = C.sg_pass_action

@[typedef]
pub struct C.sg_metal_swapchain {
pub mut:
	current_drawable      voidptr
	depth_stencil_texture voidptr // MTLTexture
	msaa_color_texture    voidptr // MTLTexture
}

pub type MetalSwapchain = C.sg_metal_swapchain

@[typedef]
pub struct C.sg_d3d11_swapchain {
pub mut:
	render_view        voidptr // ID3D11RenderTargetView
	resolve_view       voidptr // ID3D11RenderTargetView
	depth_stencil_view voidptr // ID3D11DepthStencilView
}

pub type D3d11Swapchain = C.sg_d3d11_swapchain

@[typedef]
pub struct C.sg_wgpu_swapchain {
pub mut:
	render_view        voidptr // WGPUTextureView
	resolve_view       voidptr // WGPUTextureView
	depth_stencil_view voidptr // WGPUTextureView
}

pub type WgpuSwapchain = C.sg_wgpu_swapchain

@[typedef]
pub struct C.sg_gl_swapchain {
pub mut:
	framebuffer u32
}

pub type GlSwapchain = C.sg_gl_swapchain

@[typedef]
pub struct C.sg_swapchain {
pub mut:
	width        int
	height       int
	sample_count int
	color_format PixelFormat
	depth_format PixelFormat
	metal        MetalSwapchain
	d3d11        D3d11Swapchain
	wgpu         WgpuSwapchain
	gl           GlSwapchain
}

pub type Swapchain = C.sg_swapchain

@[typedef]
pub struct C.sg_pass {
pub mut:
	_start_canary u32
	action        PassAction
	attachments   Attachments
	swapchain     Swapchain
	label         &char = unsafe { nil }
	_end_canary   u32
}

pub type Pass = C.sg_pass

pub struct C.sg_buffer_desc {
pub mut:
	size  usize
	type  BufferType
	usage Usage
	data  Range
	label &char
	// backend-specific resources
	gl_buffers   [2]u32
	mtl_buffers  [2]voidptr
	d3d11_buffer voidptr
	wgpu_buffer  voidptr
}

pub type BufferDesc = C.sg_buffer_desc

pub struct C.sg_slot_info {
	state  ResourceState
	res_id u32
	ctx_id u32
}

pub type SlotInfo = C.sg_slot_info

pub struct C.sg_buffer_info {
pub:
	slot               SlotInfo
	update_frame_index u32
	append_frame_index u32
	append_pos         int
	append_overflow    bool
	num_slots          int
	active_slot        int
}

pub type BufferInfo = C.sg_buffer_info

pub struct C.sg_buffer {
	id u32
}

pub type Buffer = C.sg_buffer

pub fn (mut b Buffer) free() {
	C.sg_destroy_buffer(*b)
}

pub struct C.sg_image_desc {
pub mut:
	type          ImageType
	render_target bool
	width         int
	height        int
	num_slices    int
	num_mipmaps   int
	usage         Usage
	pixel_format  PixelFormat
	sample_count  int
	// min_filter    Filter
	// mag_filter    Filter
	// wrap_u        Wrap
	// wrap_v        Wrap
	// wrap_w         Wrap
	// border_color   BorderColor
	// max_anisotropy u32
	// min_lod        f32
	// max_lod        f32
	data  ImageData
	label &char
	// backend-specific resources
	gl_textures                [2]u32
	gl_texture_target          u32
	mtl_textures               [2]voidptr
	d3d11_texture              voidptr
	d3d11_shader_resource_view voidptr
	wgpu_texture               voidptr
}

pub type ImageDesc = C.sg_image_desc

pub struct C.sg_sampler_desc {
	min_filter     Filter
	mag_filter     Filter
	mipmap_filter  Filter
	wrap_u         Wrap
	wrap_v         Wrap
	wrap_w         Wrap
	min_lod        f32
	max_lod        f32
	border_color   BorderColor
	compare        CompareFunc
	max_anisotropy u32
	label          &char
	// backend-specific resources
	gl_sampler    u32
	mtl_sampler   voidptr
	d3d11_sampler voidptr
	wgpu_sampler  voidptr
}

pub type SamplerDesc = C.sg_sampler_desc

pub struct C.sg_image_info {
pub mut:
	slot            SlotInfo // resource pool slot info
	upd_frame_index u32      // frame index of last sg_update_image()
	num_slots       int      // number of renaming-slots for dynamically updated images
	active_slot     int      // currently active write-slot for dynamically updated images
}

pub type ImageInfo = C.sg_image_info

pub struct C.sg_image {
pub mut:
	id u32
}

pub type Image = C.sg_image

pub fn (mut i Image) free() {
	C.sg_destroy_image(*i)
}

pub struct C.sg_sampler {
pub mut:
	id u32
}

pub type Sampler = C.sg_sampler

pub const sg_cubeface_num = 6

pub const sg_max_mipmaps = 16

pub struct C.sg_image_data {
pub mut:
	subimage [sg_cubeface_num][sg_max_mipmaps]Range
}

pub type ImageData = C.sg_image_data

pub struct C.sg_features {
pub:
	origin_top_left             bool // framebuffer and texture origin is in top left corner
	image_clamp_to_border       bool // border color and clamp-to-border UV-wrap mode is supported
	mrt_independent_blend_state bool // multiple-render-target rendering can use per-render-target blend state
	mrt_independent_write_mask  bool // multiple-render-target rendering can use per-render-target color write masks
	storage_buffer              bool // storage buffers are supported
}

pub type Features = C.sg_features

pub struct C.sg_limits {
pub:
	max_image_size_2d                   int // max width/height of SG_IMAGETYPE_2D images
	max_image_size_cube                 int // max width/height of SG_IMAGETYPE_CUBE images
	max_image_size_3d                   int // max width/height/depth of SG_IMAGETYPE_3D images
	max_image_size_array                int // max width/height pf SG_IMAGETYPE_ARRAY images
	max_image_array_layers              int // max number of layers in SG_IMAGETYPE_ARRAY images
	max_vertex_attrs                    int // <= SG_MAX_VERTEX_ATTRIBUTES (only on some GLES2 impls)
	gl_max_vertex_uniform_components    int // <= GL_MAX_VERTEX_UNIFORM_COMPONENTS (only on GL backends)
	gl_max_combined_texture_image_units int // <= GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS (only on GL backends)
}

pub type Limits = C.sg_limits

pub struct C.sg_vertex_layout_state {
pub mut:
	buffers [8]VertexBufferLayoutState
	attrs   [16]VertexAttrDesc
}

pub type VertexLayoutState = C.sg_vertex_layout_state

pub struct C.sg_vertex_buffer_layout_state {
pub mut:
	stride    int
	step_func VertexStep
	step_rate int
}

pub type VertexBufferLayoutState = C.sg_vertex_buffer_layout_state

pub struct C.sg_vertex_attr_state {
pub mut:
	buffer_index int
	offset       int
	format       VertexFormat
}

pub type VertexAttrDesc = C.sg_vertex_attr_state

pub struct C.sg_stencil_state {
	enabled    bool
	front      StencilFaceState
	back       StencilFaceState
	read_mask  u8
	write_mask u8
	ref        u8
}

pub type StencilState = C.sg_stencil_state

pub struct C.sg_depth_state {
	pixel_format     PixelFormat
	compare          CompareFunc
	write_enabled    bool
	bias             f32
	bias_slope_scale f32
	bias_clamp       f32
}

pub type DepthState = C.sg_depth_state

pub struct C.sg_stencil_face_state {
	compare       CompareFunc
	fail_op       StencilOp
	depth_fail_op StencilOp
	pass_op       StencilOp
}

pub type StencilFaceState = C.sg_stencil_face_state

pub struct C.sg_blend_state {
pub mut:
	enabled          bool
	src_factor_rgb   BlendFactor
	dst_factor_rgb   BlendFactor
	op_rgb           BlendOp
	src_factor_alpha BlendFactor
	dst_factor_alpha BlendFactor
	op_alpha         BlendOp
}

pub type BlendState = C.sg_blend_state

pub struct C.sg_color_attachment_action {
pub mut:
	load_action  LoadAction
	store_action StoreAction
	clear_value  Color
}

pub type ColorAttachmentAction = C.sg_color_attachment_action

/*
pub fn (mut action C.sg_color_attachment_action) set_color_values(r, g, b, a f32) {
    action.val[0] = r
    action.val[1] = g
    action.val[2] = b
    action.val[3] = a
}
*/
pub struct C.sg_depth_attachment_action {
pub mut:
	load_action  LoadAction
	store_action StoreAction
	clear_value  f32
}

pub type DepthAttachmentAction = C.sg_depth_attachment_action

pub struct C.sg_stencil_attachment_action {
pub mut:
	load_action  LoadAction
	store_action StoreAction
	clear_value  u8
}

pub type StencilAttachmentAction = C.sg_stencil_attachment_action

pub struct C.sg_pixelformat_info {
pub:
	sample bool // pixel format can be sampled in shaders
	filter bool // pixel format can be sampled with filtering
	render bool // pixel format can be used as render target
	blend  bool // alpha-blending is supported
	msaa   bool // pixel format can be used as MSAA render target
	depth  bool // pixel format is a depth format
}

pub type PixelFormatInfo = C.sg_pixelformat_info

@[typedef]
pub struct C.sg_environment_defaults {
pub mut:
	color_format PixelFormat
	depth_format PixelFormat
	sample_count int
}

pub type EnvironmentDefaults = C.sg_environment_defaults

@[typedef]
pub struct C.sg_metal_environment {
pub mut:
	device voidptr
}

pub type MetalEnvironment = C.sg_metal_environment

@[typedef]
pub struct C.sg_d3d11_environment {
pub mut:
	device         voidptr
	device_context voidptr
}

pub type D3d11Environment = C.sg_d3d11_environment

@[typedef]
pub struct C.sg_wgpu_environment {
pub mut:
	device voidptr
}

pub type WgpuEnvironment = C.sg_wgpu_environment

@[typedef]
pub struct C.sg_environment {
pub mut:
	defaults EnvironmentDefaults
	metal    MetalEnvironment
	d3d11    D3d11Environment
	wgpu     WgpuEnvironment
}

pub type Environment = C.sg_environment

// C.sg_commit_listener is used with sg_add_commit_listener, to add a callback,
// which will be called in sg_commit(). This is useful for libraries building
// on top of sokol-gfx to be notified about when a frame ends (instead of having
// to guess, or add a manual 'new-frame' function).
pub struct C.sg_commit_listener {
pub:
	func      fn (user_data voidptr)
	user_data voidptr
}

pub type CommitListener = C.sg_commit_listener

pub struct C.sg_trace_hooks {}

// pub struct C.sg_resource_state {} enum
pub struct C.sg_sampler_info {}
