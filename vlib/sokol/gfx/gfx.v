module gfx

import sokol.c

pub const (
	version = 1
	used_import = c.used_import
)

// setup and misc functions
[inline]
pub fn setup(desc &C.sg_desc) {
	C.sg_setup(desc)
}

[inline]
pub fn shutdown() {
	C.sg_shutdown()
}

[inline]
pub fn reset_state_cache() {
	C.sg_reset_state_cache()
}

// resource creation, destruction and updating
[inline]
pub fn make_buffer(desc &C.sg_buffer_desc) C.sg_buffer {
	return C.sg_make_buffer(desc)
}

[inline]
pub fn make_image(desc &C.sg_image_desc) C.sg_image {
	return C.sg_make_image(desc)
}

[inline]
pub fn make_shader(desc &C.sg_shader_desc) C.sg_shader {
	return C.sg_make_shader(desc)
}

[inline]
pub fn make_pipeline(desc &C.sg_pipeline_desc) C.sg_pipeline {
	return C.sg_make_pipeline(desc)
}

[inline]
pub fn make_pass(desc &C.sg_pass_desc) C.sg_pass {
	return C.sg_make_pass(desc)
}

[inline]
pub fn destroy_buffer(buf C.sg_buffer) {
	C.sg_destroy_buffer(buf)
}

[inline]
pub fn destroy_image(img C.sg_image) {
	C.sg_destroy_image(img)
}

[inline]
pub fn destroy_shader(shd C.sg_shader) {
	C.sg_destroy_shader(shd)
}

[inline]
pub fn destroy_pipeline(pip C.sg_pipeline) {
	C.sg_destroy_pipeline(pip)
}

[inline]
pub fn destroy_pass(pass C.sg_pass) {
	C.sg_destroy_pass(pass)
}

[inline]
pub fn update_buffer(buf C.sg_buffer, ptr voidptr, num_bytes int) {
	C.sg_update_buffer(buf, ptr, num_bytes)
}

[inline]
pub fn update_image(img C.sg_image, content &C.sg_image_content) {
	C.sg_update_image(img, content)
}

[inline]
pub fn append_buffer(buf C.sg_buffer, ptr voidptr, num_bytes int) int {
	return C.sg_append_buffer(buf, ptr, num_bytes)
}

[inline]
pub fn query_buffer_overflow(buf C.sg_buffer) bool {
	return C.sg_query_buffer_overflow(buf)
}

// rendering functions
[inline]
pub fn begin_default_pass(actions &C.sg_pass_action, width int, height int) {
	C.sg_begin_default_pass(actions, width, height)
}

[inline]
pub fn begin_pass(pass C.sg_pass, actions &C.sg_pass_action) {
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
pub fn apply_pipeline(pip C.sg_pipeline) {
	C.sg_apply_pipeline(pip)
}

[inline]
pub fn apply_bindings(bindings &C.sg_bindings) {
	C.sg_apply_bindings(bindings)
}

[inline]
pub fn apply_uniforms(stage int, ub_index int, data voidptr, num_bytes int) {
	C.sg_apply_uniforms(stage, ub_index, data, num_bytes)
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
pub fn query_desc() C.sg_desc {
	return C.sg_query_desc()
}

[inline]
pub fn query_backend() Backend {
	return C.sg_query_backend()
}

[inline]
pub fn query_features() C.sg_features {
	return C.sg_query_features()
}

[inline]
pub fn query_limits() C.sg_limits {
	return C.sg_query_limits()
}

[inline]
pub fn query_pixelformat(fmt PixelFormat) C.sg_pixelformat_info {
	return C.sg_query_pixelformat(fmt)
}

/* get current state of a resource (INITIAL, ALLOC, VALID, FAILED, INVALID) */
[inline]
pub fn query_buffer_state(buf C.sg_buffer) C.sg_resource_state {
	return C.sg_query_buffer_state(buf)
}

[inline]
pub fn query_image_state(img C.sg_image) C.sg_resource_state {
	return C.sg_query_image_state(img)
}

[inline]
pub fn query_shader_state(shd C.sg_shader) C.sg_resource_state {
	return C.sg_query_shader_state(shd)
}

[inline]
pub fn query_pipeline_state(pip C.sg_pipeline) C.sg_resource_state {
	return C.sg_query_pipeline_state(pip)
}

[inline]
pub fn query_pass_state(pass C.sg_pass) C.sg_resource_state {
	return C.sg_query_pass_state(pass)
}

// get runtime information about a resource
[inline]
pub fn query_buffer_info(buf C.sg_buffer) C.sg_buffer_info {
	return C.sg_query_buffer_info(buf)
}

[inline]
pub fn query_image_info(img C.sg_image) C.sg_image_info {
	return C.sg_query_image_info(img)
}

[inline]
pub fn query_shader_info(shd C.sg_shader) C.sg_shader_info {
	return C.sg_query_shader_info(shd)
}

[inline]
pub fn query_pipeline_info(pip C.sg_pipeline) C.sg_pipeline_info {
	return C.sg_query_pipeline_info(pip)
}

[inline]
pub fn query_pass_info(pass C.sg_pass) C.sg_pass_info {
	return C.sg_query_pass_info(pass)
}

// get resource creation desc struct with their default values replaced
[inline]
pub fn query_buffer_defaults(desc &C.sg_buffer) C.sg_buffer_desc {
	return C.sg_query_buffer_defaults(desc)
}

[inline]
pub fn query_image_defaults(desc &C.sg_image) C.sg_image_desc {
	return C.sg_query_image_defaults(desc)
}

[inline]
pub fn query_shader_defaults(desc &C.sg_shader) C.sg_shader_desc {
	return C.sg_query_shader_defaults(desc)
}

[inline]
pub fn query_pipeline_defaults(desc &C.sg_pipeline) C.sg_pipeline_desc {
	return C.sg_query_pipeline_defaults(desc)
}

[inline]
pub fn query_pass_defaults(desc &C.sg_pass) C.sg_pass_desc {
	return C.sg_query_pass_defaults(desc)
}

/* rendering contexts (optional) */
[inline]
pub fn setup_context() C.sg_context {
	return C.sg_setup_context()
}

[inline]
pub fn activate_context(ctx_id C.sg_context) {
	C.sg_activate_context(ctx_id)
}

[inline]
pub fn discard_context(ctx_id C.sg_context) {
	C.sg_discard_context(ctx_id)
}
