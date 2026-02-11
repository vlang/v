// BEAM Backend: Sokol GFX functions bridged to Erlang gl (OpenGL) via vbeam_sokol.
// Codegen intercepts all gfx.* calls -> vbeam_sokol:* (runtime module).
// The vbeam_sokol gen_server manages GL resources (buffers, shaders, textures, pipelines).
// Stub bodies below are dead code — codegen emits direct calls to vbeam_sokol.
module gfx

import sokol.memory

// Enums are defined in enums.v (shared across backends) and are NOT duplicated here.

pub const version = 1

pub const sg_cubeface_num = 6
pub const sg_max_mipmaps = 16

// --- Allocator and logger structs ---

pub struct Allocator {
pub mut:
	alloc_fn  memory.FnAllocatorAlloc = unsafe { nil }
	free_fn   memory.FnAllocatorFree  = unsafe { nil }
	user_data voidptr
}

pub struct Logger {
pub mut:
	func      memory.FnLogCb = unsafe { nil }
	user_data voidptr
}

// --- Resource handle structs ---

pub struct Buffer {
pub mut:
	id u32
}

pub fn (mut b Buffer) free() {
}

pub struct Image {
pub mut:
	id u32
}

pub fn (mut i Image) free() {
}

pub struct Sampler {
pub mut:
	id u32
}

pub struct Shader {
pub mut:
	id u32
}

pub fn (mut s Shader) free() {
}

pub struct Pipeline {
pub mut:
	id u32
}

pub fn (mut p Pipeline) free() {
}

pub struct Attachments {
	id u32
}

pub fn (mut a Attachments) free() {
}

// --- Core structs ---

pub struct Range {
pub mut:
	ptr  voidptr
	size usize
}

pub struct Color {
pub mut:
	r f32
	g f32
	b f32
	a f32
}

pub struct SlotInfo {
	state  ResourceState
	res_id u32
	ctx_id u32
}

// --- Desc ---

pub struct Desc {
pub mut:
	buffer_pool_size                                int
	image_pool_size                                 int
	sampler_pool_size                               int
	shader_pool_size                                int
	pipeline_pool_size                              int
	attachments_pool_size                           int
	uniform_buffer_size                             int
	max_commit_listeners                            int
	disable_validation                              bool
	mtl_force_managed_storage_mode                  bool
	mtl_use_command_buffer_with_retained_references bool
	wgpu_disable_bindgroups_cache                   bool
	wgpu_bindgroups_cache_size                      int
	allocator                                       Allocator
	logger                                          Logger
	environment                                     Environment
}

// --- Pipeline ---

pub struct ColorTargetState {
pub mut:
	pixel_format PixelFormat
	write_mask   ColorMask
	blend        BlendState
}

pub struct PipelineDesc {
pub mut:
	shader                    Shader
	layout                    VertexLayoutState
	depth                     DepthState
	stencil                   StencilState
	color_count               int
	colors                    [4]ColorTargetState
	primitive_type            PrimitiveType
	index_type                IndexType
	cull_mode                 CullMode
	face_winding              FaceWinding
	sample_count              int
	blend_color               Color
	alpha_to_coverage_enabled bool
	label                     &char = &char(unsafe { nil })
}

pub struct PipelineInfo {
pub:
	slot SlotInfo
}

// --- Attachments ---

pub struct AttachmentDesc {
pub mut:
	image     Image
	mip_level int
	slice     int
}

pub struct AttachmentsDesc {
pub mut:
	colors        [4]AttachmentDesc
	resolves      [4]AttachmentDesc
	depth_stencil AttachmentDesc
	label         &char = unsafe { nil }
}

pub struct AttachmentsInfo {
pub mut:
	slot SlotInfo
}

// --- Bindings ---

pub struct StageBindings {
pub mut:
	images   [12]Image
	samplers [8]Sampler
}

pub struct Bindings {
pub mut:
	vertex_buffers        [8]Buffer
	vertex_buffer_offsets [8]int
	index_buffer          Buffer
	index_buffer_offset   int
	vs                    StageBindings
	fs                    StageBindings
}

pub fn (mut b Bindings) set_vert_image(index int, img Image) {
	b.vs.images[index] = img
}

pub fn (mut b Bindings) set_frag_image(index int, img Image) {
	b.fs.images[index] = img
}

pub fn (b &Bindings) update_vert_buffer(index int, data voidptr, element_size int, element_count int) {
}

pub fn (b &Bindings) append_vert_buffer(index int, data voidptr, element_size int, element_count int) int {
	return 0
}

pub fn (b &Bindings) update_index_buffer(data voidptr, element_size int, element_count int) {
}

pub fn (b &Bindings) append_index_buffer(data voidptr, element_size int, element_count int) int {
	return 0
}

// --- Shader ---

pub struct ShaderAttrDesc {
pub mut:
	name      &char = &char(unsafe { nil })
	sem_name  &char = &char(unsafe { nil })
	sem_index int
}

pub struct ShaderUniformDesc {
pub mut:
	name        &char = &char(unsafe { nil })
	@type       UniformType
	array_count int
}

pub struct ShaderUniformBlockDesc {
pub mut:
	size     usize
	layout   UniformLayout
	uniforms [16]ShaderUniformDesc
}

pub struct ShaderImageDesc {
pub mut:
	used         bool
	multisampled bool
	image_type   ImageType
	sample_type  ImageSampleType
}

pub struct ShaderStorageBufferDesc {
pub mut:
	used     bool
	readonly bool
}

pub struct ShaderSamplerDesc {
pub mut:
	used         bool
	sampler_type SamplerType
}

pub struct ShaderImageSamplerPairDesc {
pub mut:
	used         bool
	image_slot   int
	sampler_slot int
	glsl_name    &char = &char(unsafe { nil })
}

pub struct ShaderStageDesc {
pub mut:
	source              &char = &char(unsafe { nil })
	bytecode            Range
	entry               &char = &char(unsafe { nil })
	d3d11_target        &char = &char(unsafe { nil })
	uniform_blocks      [4]ShaderUniformBlockDesc
	storage_buffers     [8]ShaderStorageBufferDesc
	images              [12]ShaderImageDesc
	samplers            [8]ShaderSamplerDesc
	image_sampler_pairs [12]ShaderImageSamplerPairDesc
}

pub fn (mut desc ShaderStageDesc) set_image(index int, name string) ShaderStageDesc {
	desc.images[index].image_type = ._2d
	return *desc
}

pub struct ShaderDesc {
pub mut:
	attrs [16]ShaderAttrDesc
	vs    ShaderStageDesc
	fs    ShaderStageDesc
	label &char = &char(unsafe { nil })
}

pub fn (mut desc ShaderDesc) set_vert_src(src string) &ShaderDesc {
	desc.vs.source = &char(src.str)
	return desc
}

pub fn (mut desc ShaderDesc) set_frag_src(src string) &ShaderDesc {
	desc.fs.source = &char(src.str)
	return desc
}

pub fn (mut desc ShaderDesc) set_vert_image(index int, name string) &ShaderDesc {
	desc.vs.images[index].image_type = ._2d
	return desc
}

pub fn (mut desc ShaderDesc) set_frag_image(index int, name string) &ShaderDesc {
	desc.fs.images[index].image_type = ._2d
	return desc
}

pub fn (mut desc ShaderDesc) set_vert_uniform_block_size(block_index int, size usize) &ShaderDesc {
	desc.vs.uniform_blocks[block_index].size = size
	return desc
}

pub fn (mut desc ShaderDesc) set_frag_uniform_block_size(block_index int, size usize) &ShaderDesc {
	desc.fs.uniform_blocks[block_index].size = size
	return desc
}

pub fn (mut desc ShaderDesc) set_vert_uniform(block_index int, uniform_index int, name string, typ UniformType,
	array_count int) &ShaderDesc {
	desc.vs.uniform_blocks[block_index].uniforms[uniform_index].name = &char(name.str)
	desc.vs.uniform_blocks[block_index].uniforms[uniform_index].@type = typ
	desc.vs.uniform_blocks[block_index].uniforms[uniform_index].array_count = array_count
	return desc
}

pub fn (mut desc ShaderDesc) set_frag_uniform(block_index int, uniform_index int, name string, typ UniformType,
	array_count int) &ShaderDesc {
	desc.fs.uniform_blocks[block_index].uniforms[uniform_index].name = &char(name.str)
	desc.fs.uniform_blocks[block_index].uniforms[uniform_index].@type = typ
	desc.fs.uniform_blocks[block_index].uniforms[uniform_index].array_count = array_count
	return desc
}

pub fn (desc &ShaderDesc) make_shader() Shader {
	return Shader{}
}

pub struct ShaderInfo {
pub:
	slot SlotInfo
}

// --- Buffer ---

pub struct BufferDesc {
pub mut:
	size  usize
	@type BufferType
	usage Usage
	data  Range
	label &char = &char(unsafe { nil })
}

pub struct BufferInfo {
pub:
	slot               SlotInfo
	update_frame_index u32
	append_frame_index u32
	append_pos         int
	append_overflow    bool
	num_slots          int
	active_slot        int
}

// --- Image ---

pub struct ImageDesc {
pub mut:
	@type         ImageType
	render_target bool
	width         int
	height        int
	num_slices    int
	num_mipmaps   int
	usage         Usage
	pixel_format  PixelFormat
	sample_count  int
	data          ImageData
	label         &char = &char(unsafe { nil })
	// backend-specific resources (stubs for BEAM)
	gl_textures                [2]u32
	gl_texture_target          u32
	mtl_textures               [2]voidptr
	d3d11_texture              voidptr
	d3d11_shader_resource_view voidptr
	wgpu_texture               voidptr
}

pub struct ImageData {
pub mut:
	subimage [sg_cubeface_num][sg_max_mipmaps]Range
}

pub struct ImageInfo {
pub mut:
	slot            SlotInfo
	upd_frame_index u32
	num_slots       int
	active_slot     int
}

// --- Sampler ---

pub struct SamplerDesc {
pub:
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
	label          &char = &char(unsafe { nil })
}

pub struct SamplerInfo {
}

// --- Pass ---

pub struct ColorAttachmentAction {
pub mut:
	load_action  LoadAction
	store_action StoreAction
	clear_value  Color
}

pub struct DepthAttachmentAction {
pub mut:
	load_action  LoadAction
	store_action StoreAction
	clear_value  f32
}

pub struct StencilAttachmentAction {
pub mut:
	load_action  LoadAction
	store_action StoreAction
	clear_value  u8
}

pub struct PassAction {
pub mut:
	colors  [4]ColorAttachmentAction
	depth   DepthAttachmentAction
	stencil StencilAttachmentAction
}

pub struct MetalSwapchain {
pub mut:
	current_drawable      voidptr
	depth_stencil_texture voidptr
	msaa_color_texture    voidptr
}

pub struct D3d11Swapchain {
pub mut:
	render_view        voidptr
	resolve_view       voidptr
	depth_stencil_view voidptr
}

pub struct WgpuSwapchain {
pub mut:
	render_view        voidptr
	resolve_view       voidptr
	depth_stencil_view voidptr
}

pub struct GlSwapchain {
pub mut:
	framebuffer u32
}

pub struct Swapchain {
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

pub struct Pass {
pub mut:
	action      PassAction
	attachments Attachments
	swapchain   Swapchain
	label       &char = unsafe { nil }
}

// --- Layout ---

pub struct VertexLayoutState {
pub mut:
	buffers [8]VertexBufferLayoutState
	attrs   [16]VertexAttrDesc
}

pub struct VertexBufferLayoutState {
pub mut:
	stride    int
	step_func VertexStep
	step_rate int
}

pub struct VertexAttrDesc {
pub mut:
	buffer_index int
	offset       int
	format       VertexFormat
}

// --- State structs ---

pub struct StencilState {
pub mut:
	enabled    bool
	front      StencilFaceState
	back       StencilFaceState
	read_mask  u8
	write_mask u8
	ref        u8
}

pub struct DepthState {
pub mut:
	pixel_format     PixelFormat
	compare          CompareFunc
	write_enabled    bool
	bias             f32
	bias_slope_scale f32
	bias_clamp       f32
}

pub struct StencilFaceState {
pub mut:
	compare       CompareFunc
	fail_op       StencilOp
	depth_fail_op StencilOp
	pass_op       StencilOp
}

pub struct BlendState {
pub mut:
	enabled          bool
	src_factor_rgb   BlendFactor
	dst_factor_rgb   BlendFactor
	op_rgb           BlendOp
	src_factor_alpha BlendFactor
	dst_factor_alpha BlendFactor
	op_alpha         BlendOp
}

// --- Features / Limits ---

pub struct Features {
pub:
	origin_top_left             bool
	image_clamp_to_border       bool
	mrt_independent_blend_state bool
	mrt_independent_write_mask  bool
	storage_buffer              bool
}

pub struct Limits {
pub:
	max_image_size_2d                   int
	max_image_size_cube                 int
	max_image_size_3d                   int
	max_image_size_array                int
	max_image_array_layers              int
	max_vertex_attrs                    int
	gl_max_vertex_uniform_components    int
	gl_max_combined_texture_image_units int
}

pub struct PixelFormatInfo {
pub:
	sample bool
	filter bool
	render bool
	blend  bool
	msaa   bool
	depth  bool
}

// --- Environment ---

pub struct EnvironmentDefaults {
pub mut:
	color_format PixelFormat
	depth_format PixelFormat
	sample_count int
}

pub struct MetalEnvironment {
pub mut:
	device voidptr
}

pub struct D3d11Environment {
pub mut:
	device         voidptr
	device_context voidptr
}

pub struct WgpuEnvironment {
pub mut:
	device voidptr
}

pub struct Environment {
pub mut:
	defaults EnvironmentDefaults
	metal    MetalEnvironment
	d3d11    D3d11Environment
	wgpu     WgpuEnvironment
}

// --- CommitListener ---

pub struct CommitListener {
pub:
	func      fn (user_data voidptr) = unsafe { nil }
	user_data voidptr
}

// --- Frame stats (simplified for BEAM) ---

pub struct FrameStatsGL {
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

pub struct FrameStatsD3D11Pass {
	num_om_set_render_targets    u32
	num_clear_render_target_view u32
	num_clear_depth_stencil_view u32
	num_resolve_subresource      u32
}

pub struct FrameStatsD3D11Pipeline {
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

pub struct FrameStatsD3D11Bindings {
	num_ia_set_vertex_buffers   u32
	num_ia_set_index_buffer     u32
	num_vs_set_shader_resources u32
	num_ps_set_shader_resources u32
	num_vs_set_samplers         u32
	num_ps_set_samplers         u32
}

pub struct FrameStatsD3D11Uniforms {
	num_update_subresource u32
}

pub struct FrameStatsD3D11Draw {
	num_draw_indexed_instanced u32
	num_draw_indexed           u32
	num_draw_instanced         u32
	num_draw                   u32
}

pub struct FrameStatsD3D11 {
	pass      FrameStatsD3D11Pass
	pipeline  FrameStatsD3D11Pipeline
	bindings  FrameStatsD3D11Bindings
	uniforms  FrameStatsD3D11Uniforms
	draw      FrameStatsD3D11Draw
	num_map   u32
	num_unmap u32
}

pub struct FrameStatsMetalIdpool {
	num_added             u32
	num_released          u32
	num_garbage_collected u32
}

pub struct FrameStatsMetalPipeline {
	num_set_blend_color             u32
	num_set_cull_mode               u32
	num_set_front_facing_winding    u32
	num_set_stencil_reference_value u32
	num_set_depth_bias              u32
	num_set_render_pipeline_state   u32
	num_set_depth_stencil_state     u32
}

pub struct FrameStatsMetalBindings {
	num_set_vertex_buffer          u32
	num_set_vertex_texture         u32
	num_set_vertex_sampler_state   u32
	num_set_fragment_buffer        u32
	num_set_fragment_texture       u32
	num_set_fragment_sampler_state u32
}

pub struct FrameStatsMetalUniforms {
	num_set_vertex_buffer_offset   u32
	num_set_fragment_buffer_offset u32
}

pub struct FrameStatsMetal {
	idpool   FrameStatsMetalIdpool
	pipeline FrameStatsMetalPipeline
	bindings FrameStatsMetalBindings
	uniforms FrameStatsMetalUniforms
}

pub struct FrameStatsWGPUUniforms {
	num_set_bindgroup u32
	size_write_buffer u32
}

pub struct FrameStatsWGPUBindings {
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

pub struct FrameStatsWGPU {
	uniforms FrameStatsWGPUUniforms
	bindings FrameStatsWGPUBindings
}

pub struct FrameStats {
	frame_index u32

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

// --- Functions ---

pub fn setup(desc &Desc) {
}

pub fn shutdown() {
}

pub fn is_valid() bool {
	return false
}

pub fn reset_state_cache() {
}

pub fn make_buffer(desc &BufferDesc) Buffer {
	return Buffer{}
}

pub fn make_image(desc &ImageDesc) Image {
	return Image{}
}

pub fn make_sampler(desc &SamplerDesc) Sampler {
	return Sampler{}
}

pub fn make_shader(desc &ShaderDesc) Shader {
	return Shader{}
}

pub fn make_pipeline(desc &PipelineDesc) Pipeline {
	return Pipeline{}
}

pub fn make_attachments(const_desc &AttachmentsDesc) Attachments {
	return Attachments{}
}

pub fn destroy_buffer(buf Buffer) {
}

pub fn destroy_image(img Image) {
}

pub fn destroy_sampler(smp Sampler) {
}

pub fn destroy_shader(shd Shader) {
}

pub fn destroy_pipeline(pip Pipeline) {
}

pub fn destroy_attachments(atts Attachments) {
}

pub fn update_buffer(buf Buffer, data &Range) {
}

pub fn update_image(img Image, data &ImageData) {
}

pub fn append_buffer(buf Buffer, data &Range) int {
	return 0
}

pub fn query_buffer_overflow(buf Buffer) bool {
	return false
}

pub fn begin_pass(const_pass &Pass) {
}

pub fn apply_viewport(x int, y int, width int, height int, origin_top_left bool) {
}

pub fn apply_scissor_rect(x int, y int, width int, height int, origin_top_left bool) {
}

pub fn apply_pipeline(pip Pipeline) {
}

pub fn apply_bindings(bindings &Bindings) {
}

pub fn apply_uniforms(stage ShaderStage, ub_index int, data &Range) {
}

pub fn draw(base_element int, num_elements int, num_instances int) {
}

pub fn end_pass() {
}

pub fn commit() {
}

pub fn query_desc() Desc {
	return Desc{}
}

pub fn query_backend() Backend {
	return .dummy
}

pub fn query_features() Features {
	return Features{}
}

pub fn query_limits() Limits {
	return Limits{}
}

pub fn query_pixelformat(fmt PixelFormat) PixelFormatInfo {
	return PixelFormatInfo{}
}

pub fn query_buffer_state(buf Buffer) ResourceState {
	return .invalid
}

pub fn query_image_state(img Image) ResourceState {
	return .invalid
}

pub fn query_shader_state(shd Shader) ResourceState {
	return .invalid
}

pub fn query_pipeline_state(pip Pipeline) ResourceState {
	return .invalid
}

pub fn query_attachments_state(atts Attachments) ResourceState {
	return .invalid
}

pub fn query_buffer_info(buf Buffer) BufferInfo {
	return BufferInfo{}
}

pub fn query_image_info(img Image) ImageInfo {
	return ImageInfo{}
}

pub fn query_shader_info(shd Shader) ShaderInfo {
	return ShaderInfo{}
}

pub fn query_pipeline_info(pip Pipeline) PipelineInfo {
	return PipelineInfo{}
}

pub fn query_attachments_info(atts Attachments) AttachmentsInfo {
	return AttachmentsInfo{}
}

pub fn query_buffer_defaults(desc &Buffer) BufferDesc {
	return BufferDesc{}
}

pub fn query_image_defaults(desc &Image) ImageDesc {
	return ImageDesc{}
}

pub fn query_shader_defaults(desc &Shader) ShaderDesc {
	return ShaderDesc{}
}

pub fn query_pipeline_defaults(desc &Pipeline) PipelineDesc {
	return PipelineDesc{}
}

pub fn query_attachments_defaults(desc &AttachmentsDesc) AttachmentsDesc {
	return AttachmentsDesc{}
}

pub fn enable_frame_stats() {
}

pub fn disable_frame_stats() {
}

pub fn frame_stats_enabled() bool {
	return false
}

pub fn query_frame_stats() FrameStats {
	return FrameStats{}
}

// create_clear_pass_action returns a *clearing* PassAction
pub fn create_clear_pass_action(r f32, g f32, b f32, a f32) PassAction {
	mut color_action := ColorAttachmentAction{
		load_action: .clear
		clear_value: Color{
			r: r
			g: g
			b: b
			a: a
		}
	}
	mut pass_action := PassAction{}
	pass_action.colors[0] = color_action
	return pass_action
}
