module gfx

// setup and misc functions
fn C.sg_setup(desc &C.sg_desc)
fn C.sg_shutdown()
fn C.sg_reset_state_cache()

// resource creation, destruction and updating
fn C.sg_make_buffer(desc &C.sg_buffer_desc) C.sg_buffer
fn C.sg_make_image(desc &C.sg_image_desc) C.sg_image
fn C.sg_make_shader(desc &C.sg_shader_desc) C.sg_shader
fn C.sg_make_pipeline(desc &C.sg_pipeline_desc) C.sg_pipeline
fn C.sg_make_pass(desc &C.sg_pass_desc) C.sg_pass
fn C.sg_destroy_buffer(buf C.sg_buffer)
fn C.sg_destroy_image(img C.sg_image)
fn C.sg_destroy_shader(shd C.sg_shader)
fn C.sg_destroy_pipeline(pip C.sg_pipeline)
fn C.sg_destroy_pass(pass C.sg_pass)
fn C.sg_update_buffer(buf C.sg_buffer, data &C.sg_range)
fn C.sg_update_image(img C.sg_image, data &C.sg_image_data)
fn C.sg_append_buffer(buf C.sg_buffer, data &C.sg_range) int
fn C.sg_query_buffer_overflow(buf C.sg_buffer) bool

// rendering functions
fn C.sg_begin_default_pass(actions &C.sg_pass_action, width int, height int)
fn C.sg_begin_pass(pass C.sg_pass, actions &C.sg_pass_action)
fn C.sg_apply_viewport(x int, y int, width int, height int, origin_top_left bool)
fn C.sg_apply_viewportf(x f32, y f32, width f32, height f32, origin_top_left bool)
fn C.sg_apply_scissor_rect(x int, y int, width int, height int, origin_top_left bool)
fn C.sg_apply_scissor_rectf(x f32, y f32, width f32, height f32, origin_top_left bool)
fn C.sg_apply_pipeline(pip C.sg_pipeline)
fn C.sg_apply_bindings(bindings &C.sg_bindings)
// stage == sg_shader_stage
fn C.sg_apply_uniforms(stage int, ub_index int, data &C.sg_range)
fn C.sg_draw(base_element int, num_elements int, num_instances int)
fn C.sg_end_pass()
fn C.sg_commit()

// getting information
fn C.sg_query_desc() C.sg_desc
fn C.sg_query_backend() Backend
fn C.sg_query_features() C.sg_features
fn C.sg_query_limits() C.sg_limits
fn C.sg_query_pixelformat(fmt PixelFormat) C.sg_pixelformat_info

/* get current state of a resource (INITIAL, ALLOC, VALID, FAILED, INVALID) */
fn C.sg_query_buffer_state(buf C.sg_buffer) C.sg_resource_state
fn C.sg_query_image_state(img C.sg_image) C.sg_resource_state
fn C.sg_query_shader_state(shd C.sg_shader) C.sg_resource_state
fn C.sg_query_pipeline_state(pip C.sg_pipeline) C.sg_resource_state
fn C.sg_query_pass_state(pass C.sg_pass) C.sg_resource_state

// get runtime information about a resource
fn C.sg_query_buffer_info(buf C.sg_buffer) C.sg_buffer_info
fn C.sg_query_image_info(img C.sg_image) C.sg_image_info
fn C.sg_query_shader_info(shd C.sg_shader) C.sg_shader_info
fn C.sg_query_pipeline_info(pip C.sg_pipeline) C.sg_pipeline_info
fn C.sg_query_pass_info(pass C.sg_pass) C.sg_pass_info

// get resource creation desc struct with their default values replaced
fn C.sg_query_buffer_defaults(desc &C.sg_buffer_desc) C.sg_buffer_desc
fn C.sg_query_image_defaults(desc &C.sg_image_desc) C.sg_image_desc
fn C.sg_query_shader_defaults(desc &C.sg_shader_desc) C.sg_shader_desc
fn C.sg_query_pipeline_defaults(desc &C.sg_pipeline_desc) C.sg_pipeline_desc
fn C.sg_query_pass_defaults(desc &C.sg_pass_desc) C.sg_pass_desc

/* rendering contexts (optional) */
fn C.sg_setup_context() C.sg_context
fn C.sg_activate_context(ctx_id C.sg_context)
fn C.sg_discard_context(ctx_id C.sg_context)
