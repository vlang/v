module gfx

import sokol.c

pub const (
	version     = 1
	used_import = c.used_import
)

// setup and misc functions
[inline]
pub fn setup(desc &Desc) {
	C.sg_setup(desc)
}

[inline]
pub fn shutdown() {
	C.sg_shutdown()
}

[inline]
pub fn is_valid() bool {
	return C.sg_isvalid()
}

[inline]
pub fn reset_state_cache() {
	C.sg_reset_state_cache()
}

// resource creation, destruction and updating
[inline]
pub fn make_buffer(desc &BufferDesc) Buffer {
	return C.sg_make_buffer(desc)
}

[inline]
pub fn make_image(desc &ImageDesc) Image {
	return C.sg_make_image(desc)
}

[inline]
pub fn make_shader(desc &ShaderDesc) Shader {
	return C.sg_make_shader(desc)
}

[inline]
pub fn make_pipeline(desc &PipelineDesc) Pipeline {
	return C.sg_make_pipeline(desc)
}

[inline]
pub fn make_pass(desc &PassDesc) Pass {
	return C.sg_make_pass(desc)
}

[inline]
pub fn destroy_buffer(buf Buffer) {
	C.sg_destroy_buffer(buf)
}

[inline]
pub fn destroy_image(img Image) {
	C.sg_destroy_image(img)
}

[inline]
pub fn destroy_shader(shd Shader) {
	C.sg_destroy_shader(shd)
}

[inline]
pub fn destroy_pipeline(pip Pipeline) {
	C.sg_destroy_pipeline(pip)
}

[inline]
pub fn destroy_pass(pass Pass) {
	C.sg_destroy_pass(pass)
}

[inline]
pub fn update_buffer(buf Buffer, data &Range) {
	C.sg_update_buffer(buf, data)
}

[inline]
pub fn update_image(img Image, data &ImageData) {
	C.sg_update_image(img, data)
}

[inline]
pub fn append_buffer(buf Buffer, data &Range) int {
	return C.sg_append_buffer(buf, data)
}

[inline]
pub fn query_buffer_overflow(buf Buffer) bool {
	return C.sg_query_buffer_overflow(buf)
}

// rendering functions
[inline]
pub fn begin_default_pass(actions &PassAction, width int, height int) {
	C.sg_begin_default_pass(actions, width, height)
}

[inline]
pub fn begin_pass(pass Pass, actions &PassAction) {
	C.sg_begin_pass(pass, actions)
}

[inline]
pub fn apply_viewport(x int, y int, width int, height int, origin_top_left bool) {
	C.sg_apply_viewport(x, y, width, height, origin_top_left)
}

[inline]
pub fn apply_scissor_rect(x int, y int, width int, height int, origin_top_left bool) {
	C.sg_apply_scissor_rect(x, y, width, height, origin_top_left)
}

[inline]
pub fn apply_pipeline(pip Pipeline) {
	C.sg_apply_pipeline(pip)
}

[inline]
pub fn apply_bindings(bindings &Bindings) {
	C.sg_apply_bindings(bindings)
}

[inline]
pub fn apply_uniforms(stage ShaderStage, ub_index int, data &Range) {
	C.sg_apply_uniforms(int(stage), ub_index, data)
}

[inline]
pub fn draw(base_element int, num_elements int, num_instances int) {
	C.sg_draw(base_element, num_elements, num_instances)
}

[inline]
pub fn end_pass() {
	C.sg_end_pass()
}

[inline]
pub fn commit() {
	C.sg_commit()
}

// getting information
[inline]
pub fn query_desc() Desc {
	return C.sg_query_desc()
}

[inline]
pub fn query_backend() Backend {
	return C.sg_query_backend()
}

[inline]
pub fn query_features() Features {
	return C.sg_query_features()
}

[inline]
pub fn query_limits() Limits {
	return C.sg_query_limits()
}

[inline]
pub fn query_pixelformat(fmt PixelFormat) PixelFormatInfo {
	return C.sg_query_pixelformat(fmt)
}

// get current state of a resource (INITIAL, ALLOC, VALID, FAILED, INVALID)
[inline]
pub fn query_buffer_state(buf Buffer) ResourceState {
	return ResourceState(C.sg_query_buffer_state(buf))
}

[inline]
pub fn query_image_state(img Image) ResourceState {
	return ResourceState(C.sg_query_image_state(img))
}

[inline]
pub fn query_shader_state(shd Shader) ResourceState {
	return ResourceState(C.sg_query_shader_state(shd))
}

[inline]
pub fn query_pipeline_state(pip Pipeline) ResourceState {
	return ResourceState(C.sg_query_pipeline_state(pip))
}

[inline]
pub fn query_pass_state(pass Pass) ResourceState {
	return ResourceState(C.sg_query_pass_state(pass))
}

// get runtime information about a resource
[inline]
pub fn query_buffer_info(buf Buffer) BufferInfo {
	return C.sg_query_buffer_info(buf)
}

[inline]
pub fn query_image_info(img Image) ImageInfo {
	return C.sg_query_image_info(img)
}

[inline]
pub fn query_shader_info(shd Shader) ShaderInfo {
	return C.sg_query_shader_info(shd)
}

[inline]
pub fn query_pipeline_info(pip Pipeline) PipelineInfo {
	return C.sg_query_pipeline_info(pip)
}

[inline]
pub fn query_pass_info(pass Pass) PassInfo {
	return C.sg_query_pass_info(pass)
}

// get resource creation desc struct with their default values replaced
[inline]
pub fn query_buffer_defaults(desc &Buffer) BufferDesc {
	return C.sg_query_buffer_defaults(unsafe { &BufferDesc(voidptr(desc)) })
}

[inline]
pub fn query_image_defaults(desc &Image) ImageDesc {
	return C.sg_query_image_defaults(unsafe { &ImageDesc(voidptr(desc)) })
}

[inline]
pub fn query_shader_defaults(desc &Shader) ShaderDesc {
	return C.sg_query_shader_defaults(unsafe { &ShaderDesc(voidptr(desc)) })
}

[inline]
pub fn query_pipeline_defaults(desc &Pipeline) PipelineDesc {
	return C.sg_query_pipeline_defaults(unsafe { &PipelineDesc(voidptr(desc)) })
}

[inline]
pub fn query_pass_defaults(desc &Pass) PassDesc {
	return C.sg_query_pass_defaults(unsafe { &PassDesc(voidptr(desc)) })
}

// rendering contexts (optional)
[inline]
pub fn setup_context() Context {
	return C.sg_setup_context()
}

[inline]
pub fn activate_context(ctx_id Context) {
	C.sg_activate_context(ctx_id)
}

[inline]
pub fn discard_context(ctx_id Context) {
	C.sg_discard_context(ctx_id)
}
