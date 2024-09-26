module gfx

import sokol.c as _
import sokol.memory

pub const version = 1

// setup initialises the SOKOL's gfx library, based on the information passed in `desc`
pub fn setup(desc &Desc) {
	if desc.allocator.alloc_fn == unsafe { nil } && desc.allocator.free_fn == unsafe { nil } {
		unsafe {
			desc.allocator.alloc_fn = memory.salloc
			desc.allocator.free_fn = memory.sfree
			desc.allocator.user_data = voidptr(0x1006fec5)
		}
	}
	if desc.logger.func == unsafe { nil } {
		unsafe {
			desc.logger.func = memory.slog
		}
	}
	C.sg_setup(desc)
}

// shutdown tells the SOKOL's gfx library to shutdown, and release the resources it is using
pub fn shutdown() {
	C.sg_shutdown()
}

@[inline]
pub fn is_valid() bool {
	return C.sg_isvalid()
}

@[inline]
pub fn reset_state_cache() {
	C.sg_reset_state_cache()
}

// resource creation, destruction and updating
@[inline]
pub fn make_buffer(desc &BufferDesc) Buffer {
	return C.sg_make_buffer(desc)
}

@[inline]
pub fn make_image(desc &ImageDesc) Image {
	return C.sg_make_image(desc)
}

@[inline]
pub fn make_sampler(desc &SamplerDesc) Sampler {
	return C.sg_make_sampler(desc)
}

@[inline]
pub fn make_shader(desc &ShaderDesc) Shader {
	return C.sg_make_shader(desc)
}

@[inline]
pub fn make_pipeline(desc &PipelineDesc) Pipeline {
	return C.sg_make_pipeline(desc)
}

// make_attachments creates an `Attachments` instance from `const_desc`.
// See also: documentation at the top of thirdparty/sokol/sokol_gfx.h
@[inline]
pub fn make_attachments(const_desc &AttachmentsDesc) Attachments {
	return C.sg_make_attachments(const_desc)
}

@[inline]
pub fn destroy_buffer(buf Buffer) {
	C.sg_destroy_buffer(buf)
}

@[inline]
pub fn destroy_image(img Image) {
	C.sg_destroy_image(img)
}

@[inline]
pub fn destroy_sampler(smp Sampler) {
	C.sg_destroy_sampler(smp)
}

@[inline]
pub fn destroy_shader(shd Shader) {
	C.sg_destroy_shader(shd)
}

@[inline]
pub fn destroy_pipeline(pip Pipeline) {
	C.sg_destroy_pipeline(pip)
}

// destroy_attachments destroys the `atts` `Attachments`
// See also: documentation at the top of thirdparty/sokol/sokol_gfx.h
@[inline]
pub fn destroy_attachments(atts Attachments) {
	C.sg_destroy_attachments(atts)
}

@[inline]
pub fn update_buffer(buf Buffer, data &Range) {
	C.sg_update_buffer(buf, data)
}

@[inline]
pub fn update_image(img Image, data &ImageData) {
	C.sg_update_image(img, data)
}

@[inline]
pub fn append_buffer(buf Buffer, data &Range) int {
	return C.sg_append_buffer(buf, data)
}

@[inline]
pub fn query_buffer_overflow(buf Buffer) bool {
	return C.sg_query_buffer_overflow(buf)
}

// rendering functions

@[deprecated: 'use begin_pass instead, please see examples/sokol/* for how to utilize new unified begin_pass']
@[inline]
pub fn begin_default_pass(actions &PassAction, width int, height int) {}

// begin_pass begins a rendering pass.
// See also: documentation at the top of thirdparty/sokol/sokol_gfx.h
@[inline]
pub fn begin_pass(const_pass &Pass) {
	C.sg_begin_pass(const_pass)
}

@[inline]
pub fn apply_viewport(x int, y int, width int, height int, origin_top_left bool) {
	C.sg_apply_viewport(x, y, width, height, origin_top_left)
}

@[inline]
pub fn apply_scissor_rect(x int, y int, width int, height int, origin_top_left bool) {
	C.sg_apply_scissor_rect(x, y, width, height, origin_top_left)
}

@[inline]
pub fn apply_pipeline(pip Pipeline) {
	C.sg_apply_pipeline(pip)
}

@[inline]
pub fn apply_bindings(bindings &Bindings) {
	C.sg_apply_bindings(bindings)
}

@[inline]
pub fn apply_uniforms(stage ShaderStage, ub_index int, data &Range) {
	C.sg_apply_uniforms(stage, ub_index, data)
}

@[inline]
pub fn draw(base_element int, num_elements int, num_instances int) {
	C.sg_draw(base_element, num_elements, num_instances)
}

@[inline]
pub fn end_pass() {
	C.sg_end_pass()
}

@[inline]
pub fn commit() {
	C.sg_commit()
}

// getting information
@[inline]
pub fn query_desc() Desc {
	return C.sg_query_desc()
}

@[inline]
pub fn query_backend() Backend {
	return C.sg_query_backend()
}

@[inline]
pub fn query_features() Features {
	return C.sg_query_features()
}

@[inline]
pub fn query_limits() Limits {
	return C.sg_query_limits()
}

@[inline]
pub fn query_pixelformat(fmt PixelFormat) PixelFormatInfo {
	return C.sg_query_pixelformat(fmt)
}

// get current state of a resource (INITIAL, ALLOC, VALID, FAILED, INVALID)
@[inline]
pub fn query_buffer_state(buf Buffer) ResourceState {
	return ResourceState(C.sg_query_buffer_state(buf))
}

@[inline]
pub fn query_image_state(img Image) ResourceState {
	return ResourceState(C.sg_query_image_state(img))
}

@[inline]
pub fn query_shader_state(shd Shader) ResourceState {
	return ResourceState(C.sg_query_shader_state(shd))
}

@[inline]
pub fn query_pipeline_state(pip Pipeline) ResourceState {
	return ResourceState(C.sg_query_pipeline_state(pip))
}

// query_attachments_state returns the current state of a resource (INITIAL, ALLOC, VALID, FAILED, INVALID)
// See also: documentation at the top of thirdparty/sokol/sokol_gfx.h
@[inline]
pub fn query_attachments_state(atts Attachments) ResourceState {
	return ResourceState(C.sg_query_attachments_state(atts))
}

// get runtime information about a resource
@[inline]
pub fn query_buffer_info(buf Buffer) BufferInfo {
	return C.sg_query_buffer_info(buf)
}

@[inline]
pub fn query_image_info(img Image) ImageInfo {
	return C.sg_query_image_info(img)
}

@[inline]
pub fn query_shader_info(shd Shader) ShaderInfo {
	return C.sg_query_shader_info(shd)
}

@[inline]
pub fn query_pipeline_info(pip Pipeline) PipelineInfo {
	return C.sg_query_pipeline_info(pip)
}

// query_attachments_info returns runtime information about the `atts` / `Attachments`.
// See also: documentation at the top of thirdparty/sokol/sokol_gfx.h
@[inline]
pub fn query_attachments_info(atts Attachments) AttachmentsInfo {
	return C.sg_query_attachments_info(atts)
}

// get resource creation desc struct with their default values replaced
@[inline]
pub fn query_buffer_defaults(desc &Buffer) BufferDesc {
	return C.sg_query_buffer_defaults(unsafe { &BufferDesc(voidptr(desc)) })
}

@[inline]
pub fn query_image_defaults(desc &Image) ImageDesc {
	return C.sg_query_image_defaults(unsafe { &ImageDesc(voidptr(desc)) })
}

@[inline]
pub fn query_shader_defaults(desc &Shader) ShaderDesc {
	return C.sg_query_shader_defaults(unsafe { &ShaderDesc(voidptr(desc)) })
}

@[inline]
pub fn query_pipeline_defaults(desc &Pipeline) PipelineDesc {
	return C.sg_query_pipeline_defaults(unsafe { &PipelineDesc(voidptr(desc)) })
}

// query_attachments_defaults returns `AttachmentsDesc` with default values replaced.
// See also: documentation at the top of thirdparty/sokol/sokol_gfx.h
@[inline]
pub fn query_attachments_defaults(desc &AttachmentsDesc) AttachmentsDesc {
	return C.sg_query_attachments_defaults(unsafe { &AttachmentsDesc(voidptr(desc)) })
}

// frame stats

// enable_frame_stats enables the sokol frame statistics.
@[inline]
pub fn enable_frame_stats() {
	C.sg_enable_frame_stats()
}

// disable_frame_stats disables the sokol frame statistics.
@[inline]
pub fn disable_frame_stats() {
	C.sg_disable_frame_stats()
}

// frame_stats_enabled returns `true` if the sokol frame statistics is enabled.
@[inline]
pub fn frame_stats_enabled() bool {
	return C.sg_frame_stats_enabled()
}

// query_frame_stats returns the sokol frame statistics for the current frame.
@[inline]
pub fn query_frame_stats() FrameStats {
	return C.sg_query_frame_stats()
}
