module gfx

struct C.sg_desc {
pub mut:
	_start_canary      u32
	buffer_pool_size   int
	image_pool_size    int
	shader_pool_size   int
	pipeline_pool_size int
	pass_pool_size     int
	context_pool_size  int
	context            ContextDesc
	/*
	// GL specific
    gl_force_gles2 bool
    // Metal-specific
    mtl_device voidptr
    mtl_renderpass_descriptor_cb fn() voidptr
    mtl_drawable_cb fn() voidptr
    mtl_global_uniform_buffer_size int
    mtl_sampler_cache_size int
    // D3D11-specific
    d3d11_device voidptr
    d3d11_device_context voidptr
    d3d11_render_target_view_cb fn() voidptr
    d3d11_depth_stencil_view_cb fn() voidptr
	*/
	_end_canary u32
}

pub type Desc = C.sg_desc

struct C.sg_context_desc {
pub mut:
	color_format PixelFormat
	depth_format PixelFormat
	sample_count int
	gl           GLContextDesc
	metal        MetalContextDesc
	d3d11        D3D11ContextDesc
	// TODO C.sg_wgpu_context_desc wgpu;
}

pub type ContextDesc = C.sg_context_desc

struct C.sg_gl_context_desc {
	force_gles2 bool
}

pub type GLContextDesc = C.sg_gl_context_desc

struct C.sg_metal_context_desc {
	device                   voidptr
	renderpass_descriptor_cb fn () voidptr
	drawable_cb              fn () voidptr
}

pub type MetalContextDesc = C.sg_metal_context_desc

struct C.sg_d3d11_context_desc {
	device                voidptr
	device_context        voidptr
	render_target_view_cb fn () voidptr
	depth_stencil_view_cb fn () voidptr
}

pub type D3D11ContextDesc = C.sg_d3d11_context_desc

struct C.sg_color_state {
pub mut:
	pixel_format PixelFormat
	write_mask   ColorMask
	blend        BlendState
}

pub type ColorState = C.sg_color_state

struct C.sg_pipeline_desc {
pub mut:
	_start_canary             u32
	shader                    Shader
	layout                    LayoutDesc
	depth                     DepthState
	stencil                   StencilState
	color_count               int
	colors                    [4]ColorState // C.SG_MAX_COLOR_ATTACHMENTS
	primitive_type            PrimitiveType
	index_type                IndexType
	cull_mode                 CullMode
	face_winding              FaceWinding
	sample_count              int
	blend_color               Color
	alpha_to_coverage_enabled bool
	label                     &char = &char(0)
	_end_canary               u32
}

pub type PipelineDesc = C.sg_pipeline_desc

struct C.sg_pipeline_info {
}

pub type PipelineInfo = C.sg_pipeline_info

struct C.sg_pipeline {
pub:
	id u32
}

pub type Pipeline = C.sg_pipeline

pub fn (mut p C.sg_pipeline) free() {
	C.sg_destroy_pipeline(*p)
}

struct C.sg_bindings {
pub mut:
	_start_canary         u32
	vertex_buffers        [8]Buffer
	vertex_buffer_offsets [8]int
	index_buffer          Buffer
	index_buffer_offset   int
	vs_images             [8]Image
	fs_images             [8]Image
	_end_canary           u32
}

pub type Bindings = C.sg_bindings

pub fn (mut b Bindings) set_vert_image(index int, img Image) {
	b.vs_images[index] = img
}

pub fn (mut b Bindings) set_frag_image(index int, img Image) {
	b.fs_images[index] = img
}

pub fn (b &Bindings) update_vert_buffer(index int, data voidptr, element_size int, element_count int) {
	range := Range{
		ptr: data
		size: usize(element_size * element_count)
	}
	C.sg_update_buffer(b.vertex_buffers[index], &range)
}

pub fn (b &Bindings) append_vert_buffer(index int, data voidptr, element_size int, element_count int) int {
	range := Range{
		ptr: data
		size: usize(element_size * element_count)
	}
	return C.sg_append_buffer(b.vertex_buffers[index], &range)
}

pub fn (b &Bindings) update_index_buffer(data voidptr, element_size int, element_count int) {
	range := Range{
		ptr: data
		size: usize(element_size * element_count)
	}
	C.sg_update_buffer(b.index_buffer, &range)
}

pub fn (b &Bindings) append_index_buffer(data voidptr, element_size int, element_count int) int {
	range := Range{
		ptr: data
		size: usize(element_size * element_count)
	}
	return C.sg_append_buffer(b.index_buffer, &range)
}

[heap]
struct C.sg_shader_desc {
pub mut:
	_start_canary u32
	attrs         [16]ShaderAttrDesc
	vs            ShaderStageDesc
	fs            ShaderStageDesc
	label         &char
	_end_canary   u32
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
	desc.vs.images[index].name = &char(name.str)
	desc.vs.images[index].image_type = ._2d
	return desc
}

pub fn (mut desc C.sg_shader_desc) set_frag_image(index int, name string) &ShaderDesc {
	desc.fs.images[index].name = &char(name.str)
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

pub fn (mut desc C.sg_shader_desc) set_vert_uniform(block_index int, uniform_index int, name string, @type UniformType, array_count int) &ShaderDesc {
	desc.vs.uniform_blocks[block_index].uniforms[uniform_index].name = &char(name.str)
	desc.vs.uniform_blocks[block_index].uniforms[uniform_index].@type = @type
	return desc
}

pub fn (mut desc C.sg_shader_desc) set_frag_uniform(block_index int, uniform_index int, name string, @type UniformType, array_count int) &ShaderDesc {
	desc.fs.uniform_blocks[block_index].uniforms[uniform_index].name = &char(name.str)
	desc.fs.uniform_blocks[block_index].uniforms[uniform_index].@type = @type
	return desc
}

pub fn (desc &ShaderDesc) make_shader() Shader {
	return C.sg_make_shader(desc)
}

struct C.sg_shader_attr_desc {
pub mut:
	name      &char // GLSL vertex attribute name (only required for GLES2)
	sem_name  &char // HLSL semantic name
	sem_index int   // HLSL semantic index
}

pub type ShaderAttrDesc = C.sg_shader_attr_desc

struct C.sg_shader_stage_desc {
pub mut:
	source         &char
	bytecode       Range
	entry          &char
	uniform_blocks [4]ShaderUniformBlockDesc
	images         [12]ShaderImageDesc
}

pub type ShaderStageDesc = C.sg_shader_stage_desc

pub fn (mut desc ShaderStageDesc) set_image(index int, name string) ShaderStageDesc {
	desc.images[index].name = &char(name.str)
	desc.images[index].image_type = ._2d
	return *desc
}

struct C.sg_shader_uniform_block_desc {
pub mut:
	size     usize
	layout   UniformLayout
	uniforms [16]ShaderUniformDesc
}

pub type ShaderUniformBlockDesc = C.sg_shader_uniform_block_desc

struct C.sg_shader_uniform_desc {
pub mut:
	name        &char
	@type       UniformType
	array_count int
}

pub type ShaderUniformDesc = C.sg_shader_uniform_desc

struct C.sg_shader_image_desc {
pub mut:
	name       &char
	image_type ImageType
}

pub type ShaderImageDesc = C.sg_shader_image_desc

struct C.sg_shader_info {
}

pub type ShaderInfo = C.sg_shader_info

struct C.sg_context {
	id u32
}

pub type Context = C.sg_context

pub struct C.sg_range {
pub mut:
	ptr  voidptr
	size usize
}

pub type Range = C.sg_range

struct C.sg_color {
pub mut:
	r f32
	g f32
	b f32
	a f32
}

pub type Color = C.sg_color

struct C.sg_shader {
pub:
	id u32
}

pub type Shader = C.sg_shader

pub fn (mut s Shader) free() {
	C.sg_destroy_shader(*s)
}

struct C.sg_pass_desc {
pub mut:
	_start_canary            u32
	color_attachments        [4]PassAttachmentDesc
	depth_stencil_attachment PassAttachmentDesc
	label                    &char
	_end_canary              u32
}

pub type PassDesc = C.sg_pass_desc

struct C.sg_pass_info {
	info SlotInfo
}

pub type PassInfo = C.sg_pass_info

struct C.sg_pass_action {
pub mut:
	_start_canary u32
	colors        [4]ColorAttachmentAction
	depth         DepthAttachmentAction
	stencil       StencilAttachmentAction
	_end_canary   u32
}

pub type PassAction = C.sg_pass_action

struct C.sg_pass {
	id u32
}

pub type Pass = C.sg_pass

pub fn (mut p Pass) free() {
	C.sg_destroy_pass(*p)
}

struct C.sg_buffer_desc {
pub mut:
	_start_canary u32
	size          usize
	@type         BufferType
	usage         Usage
	data          Range
	label         &char
	// GL specific
	gl_buffers [2]u32
	// Metal specific
	mtl_buffers [2]voidptr
	// D3D11 specific
	d3d11_buffer voidptr
	_end_canary  u32
}

pub type BufferDesc = C.sg_buffer_desc

struct C.sg_slot_info {
	state  ResourceState
	res_id u32
	ctx_id u32
}

pub type SlotInfo = C.sg_slot_info

struct C.sg_buffer_info {
}

pub type BufferInfo = C.sg_buffer_info

struct C.sg_buffer {
	id u32
}

pub type Buffer = C.sg_buffer

pub fn (mut b Buffer) free() {
	C.sg_destroy_buffer(*b)
}

pub struct C.sg_image_desc {
pub mut:
	_start_canary  u32
	@type          ImageType
	render_target  bool
	width          int
	height         int
	num_slices     int
	num_mipmaps    int
	usage          Usage
	pixel_format   PixelFormat
	sample_count   int
	min_filter     Filter
	mag_filter     Filter
	wrap_u         Wrap
	wrap_v         Wrap
	wrap_w         Wrap
	border_color   BorderColor
	max_anisotropy u32
	min_lod        f32
	max_lod        f32
	data           ImageData
	label          &char
	// GL specific
	gl_textures       [2]u32
	gl_texture_target u32
	// Metal specific
	mtl_textures [2]voidptr
	// D3D11 specific
	d3d11_texture              voidptr
	d3d11_shader_resource_view voidptr
	// WebGPU specific
	wgpu_texture voidptr
	_end_canary  u32
}

pub type ImageDesc = C.sg_image_desc

pub struct C.sg_image_info {
pub mut:
	slot            SlotInfo // resource pool slot info
	upd_frame_index u32      // frame index of last sg_update_image()
	num_slots       int      // number of renaming-slots for dynamically updated images
	active_slot     int      // currently active write-slot for dynamically updated images
}

pub type ImageInfo = C.sg_image_info

pub struct C.sg_image {
pub:
	id u32
}

pub type Image = C.sg_image

pub fn (mut i Image) free() {
	C.sg_destroy_image(*i)
}

pub const sg_cubeface_num = 6

pub const sg_max_mipmaps = 16

pub struct C.sg_image_data {
pub mut:
	subimage [sg_cubeface_num][sg_max_mipmaps]Range
}

pub type ImageData = C.sg_image_data

struct C.sg_features {
pub:
	instancing                  bool // hardware instancing supported
	origin_top_left             bool // framebuffer and texture origin is in top left corner
	multiple_render_targets     bool // offscreen render passes can have multiple render targets attached
	msaa_render_targets         bool // offscreen render passes support MSAA antialiasing
	imagetype_3d                bool // creation of SG_IMAGETYPE_3D images is supported
	imagetype_array             bool // creation of SG_IMAGETYPE_ARRAY images is supported
	image_clamp_to_border       bool // border color and clamp-to-border UV-wrap mode is supported
	mrt_independent_blend_state bool // multiple-render-target rendering can use per-render-target blend state
	mrt_independent_write_mask  bool // multiple-render-target rendering can use per-render-target color write masks
}

pub type Features = C.sg_features

pub struct C.sg_limits {
pub:
	max_image_size_2d      u32 // max width/height of SG_IMAGETYPE_2D images
	max_image_size_cube    u32 // max width/height of SG_IMAGETYPE_CUBE images
	max_image_size_3d      u32 // max width/height/depth of SG_IMAGETYPE_3D images
	max_image_size_array   u32 // max width/height pf SG_IMAGETYPE_ARRAY images
	max_image_array_layers u32 // max number of layers in SG_IMAGETYPE_ARRAY images
	max_vertex_attrs       u32 // <= SG_MAX_VERTEX_ATTRIBUTES (only on some GLES2 impls)
}

pub type Limits = C.sg_limits

pub struct C.sg_layout_desc {
pub mut:
	buffers [8]BufferLayoutDesc
	attrs   [16]VertexAttrDesc
}

pub type LayoutDesc = C.sg_layout_desc

pub struct C.sg_buffer_layout_desc {
pub mut:
	stride    int
	step_func VertexStep
	step_rate int
}

pub type BufferLayoutDesc = C.sg_buffer_layout_desc

pub struct C.sg_vertex_attr_desc {
pub mut:
	buffer_index int
	offset       int
	format       VertexFormat
}

pub type VertexAttrDesc = C.sg_vertex_attr_desc

struct C.sg_stencil_state {
	enabled    bool
	front      StencilFaceState
	back       StencilFaceState
	read_mask  u8
	write_mask u8
	ref        u8
}

pub type StencilState = C.sg_stencil_state

struct C.sg_depth_state {
	pixel_format     PixelFormat
	compare          CompareFunc
	write_enabled    bool
	bias             f32
	bias_slope_scale f32
	bias_clamp       f32
}

pub type DepthState = C.sg_depth_state

struct C.sg_stencil_face_state {
	fail_op       StencilOp
	depth_fail_op StencilOp
	pass_op       StencilOp
	compare_func  CompareFunc
}

pub type StencilFaceState = C.sg_stencil_face_state

struct C.sg_blend_state {
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

struct C.sg_color_attachment_action {
pub mut:
	action Action
	value  Color
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
	action Action
	value  f32
}

pub type DepthAttachmentAction = C.sg_depth_attachment_action

pub struct C.sg_stencil_attachment_action {
pub mut:
	action Action
	value  u8
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

struct C.sg_pass_attachment_desc {
pub mut:
	image     Image
	mip_level int
	face      int
	// image sg_image
	// mip_level int
	// union {
	// face int
	// layer int
	// slice int
	// }
}

pub type PassAttachmentDesc = C.sg_pass_attachment_desc
