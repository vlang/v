module gfx

// setup and misc functions
fn C.sg_setup(const_desc &C.sg_desc)
fn C.sg_shutdown()
fn C.sg_isvalid() bool
fn C.sg_reset_state_cache()
fn C.sg_install_trace_hooks(const_trace_hooks &C.sg_trace_hooks) C.sg_trace_hooks
fn C.sg_push_debug_group(const_name &char)
fn C.sg_pop_debug_group()
fn C.sg_add_commit_listener(listener C.sg_commit_listener) bool
fn C.sg_remove_commit_listener(listener C.sg_commit_listener) bool

// resource creation, destruction and updating
fn C.sg_make_buffer(const_desc &C.sg_buffer_desc) C.sg_buffer
fn C.sg_make_image(const_desc &C.sg_image_desc) C.sg_image
fn C.sg_make_sampler(const_desc &C.sg_sampler_desc) C.sg_sampler
fn C.sg_make_shader(const_desc &C.sg_shader_desc) C.sg_shader
fn C.sg_make_pipeline(const_desc &C.sg_pipeline_desc) C.sg_pipeline
fn C.sg_make_attachments(const_desc &C.sg_attachments_desc) C.sg_attachments
fn C.sg_destroy_buffer(buf C.sg_buffer)
fn C.sg_destroy_image(img C.sg_image)
fn C.sg_destroy_sampler(smp C.sg_sampler)
fn C.sg_destroy_shader(shd C.sg_shader)
fn C.sg_destroy_pipeline(pip C.sg_pipeline)
fn C.sg_destroy_attachments(atts C.sg_attachments)
fn C.sg_update_buffer(buf C.sg_buffer, data &C.sg_range)
fn C.sg_update_image(img C.sg_image, data &C.sg_image_data)
fn C.sg_append_buffer(buf C.sg_buffer, data &C.sg_range) int
fn C.sg_query_buffer_overflow(buf C.sg_buffer) bool
fn C.sg_query_buffer_will_overflow(buf C.sg_buffer, size usize) bool

// rendering functions
fn C.sg_begin_pass(const_pass &C.sg_pass)
fn C.sg_apply_viewport(x int, y int, width int, height int, origin_top_left bool)
fn C.sg_apply_viewportf(x f32, y f32, width f32, height f32, origin_top_left bool)
fn C.sg_apply_scissor_rect(x int, y int, width int, height int, origin_top_left bool)
fn C.sg_apply_scissor_rectf(x f32, y f32, width f32, height f32, origin_top_left bool)
fn C.sg_apply_pipeline(pip C.sg_pipeline)
fn C.sg_apply_bindings(bindings &C.sg_bindings)
fn C.sg_apply_uniforms(stage ShaderStage, ub_index int, const_data &C.sg_range)
fn C.sg_draw(base_element int, num_elements int, num_instances int)
fn C.sg_end_pass()
fn C.sg_commit()

// getting information
fn C.sg_query_desc() C.sg_desc
fn C.sg_query_backend() Backend
fn C.sg_query_features() C.sg_features
fn C.sg_query_limits() C.sg_limits
fn C.sg_query_pixelformat(fmt PixelFormat) C.sg_pixelformat_info

// get current state of a resource (INITIAL, ALLOC, VALID, FAILED, INVALID)
fn C.sg_query_buffer_state(buf C.sg_buffer) ResourceState
fn C.sg_query_image_state(img C.sg_image) ResourceState
fn C.sg_query_sampler_state(smp C.sg_sampler) ResourceState
fn C.sg_query_shader_state(shd C.sg_shader) ResourceState
fn C.sg_query_pipeline_state(pip C.sg_pipeline) ResourceState
fn C.sg_query_attachments_state(atts C.sg_attachments) ResourceState

// get runtime information about a resource
fn C.sg_query_buffer_info(buf C.sg_buffer) C.sg_buffer_info
fn C.sg_query_image_info(img C.sg_image) C.sg_image_info
fn C.sg_query_sampler_info(smp C.sg_sampler) C.sg_sampler_info
fn C.sg_query_shader_info(shd C.sg_shader) C.sg_shader_info
fn C.sg_query_pipeline_info(pip C.sg_pipeline) C.sg_pipeline_info
fn C.sg_query_attachments_info(atts C.sg_attachments) C.sg_attachments_info

// get desc structs matching a specific resource (NOTE that not all creation attributes may be provided)
fn C.sg_query_buffer_desc(buf C.sg_buffer) C.sg_buffer_desc
fn C.sg_query_image_desc(img C.sg_image) C.sg_image_desc
fn C.sg_query_sampler_desc(smp C.sg_sampler) C.sg_sampler_desc
fn C.sg_query_shader_desc(shd C.sg_shader) C.sg_shader_desc
fn C.sg_query_pipeline_desc(pip C.sg_pipeline) C.sg_pipeline_desc
fn C.sg_query_attachments_desc(atts C.sg_attachments) C.sg_attachments_desc

// get resource creation desc struct with their default values replaced
fn C.sg_query_buffer_defaults(const_desc &C.sg_buffer_desc) C.sg_buffer_desc
fn C.sg_query_image_defaults(const_desc &C.sg_image_desc) C.sg_image_desc
fn C.sg_query_sampler_defaults(const_desc &C.sg_sampler_desc) C.sg_sampler_desc
fn C.sg_query_shader_defaults(const_desc &C.sg_shader_desc) C.sg_shader_desc
fn C.sg_query_pipeline_defaults(const_desc &C.sg_pipeline_desc) C.sg_pipeline_desc
fn C.sg_query_attachments_defaults(const_desc &C.sg_attachments_desc) C.sg_attachments_desc

// separate resource allocation and initialization (for async setup)
fn C.sg_alloc_buffer() C.sg_buffer
fn C.sg_alloc_image() C.sg_image
fn C.sg_alloc_sampler() C.sg_sampler
fn C.sg_alloc_shader() C.sg_shader
fn C.sg_alloc_pipeline() C.sg_pipeline
fn C.sg_alloc_attachments() C.sg_attachments
fn C.sg_dealloc_buffer(buf C.sg_buffer)
fn C.sg_dealloc_image(img C.sg_image)
fn C.sg_dealloc_sampler(smp C.sg_sampler)
fn C.sg_dealloc_shader(shd C.sg_shader)
fn C.sg_dealloc_pipeline(pip C.sg_pipeline)
fn C.sg_dealloc_attachments(atts C.sg_attachments)
fn C.sg_init_buffer(buf C.sg_buffer, const_desc &C.sg_buffer_desc)
fn C.sg_init_image(img C.sg_image, const_desc &C.sg_buffer_desc)
fn C.sg_init_sampler(smg C.sg_sampler, const_desc &C.sg_sampler_desc)
fn C.sg_init_shader(shd C.sg_shader, const_desc &C.sg_shader_desc)
fn C.sg_init_pipeline(pip C.sg_pipeline, const_desc &C.sg_pipeline_desc)
fn C.sg_init_attachments(atts C.sg_attachments, const_desc &C.sg_attachments_desc)
fn C.sg_uninit_buffer(buf C.sg_buffer)
fn C.sg_uninit_image(img C.sg_image)
fn C.sg_uninit_sampler(smp C.sg_sampler)
fn C.sg_uninit_shader(shd C.sg_shader)
fn C.sg_uninit_pipeline(pip C.sg_pipeline)
fn C.sg_uninit_attachments(atts C.sg_attachments)
fn C.sg_fail_buffer(buf C.sg_buffer)
fn C.sg_fail_image(img C.sg_image)
fn C.sg_fail_sampler(smp C.sg_sampler)
fn C.sg_fail_shader(shd C.sg_shader)
fn C.sg_fail_pipeline(pip C.sg_pipeline)
fn C.sg_fail_attachments(atts C.sg_attachments)

// frame stats
fn C.sg_enable_frame_stats()
fn C.sg_disable_frame_stats()
fn C.sg_frame_stats_enabled() bool
fn C.sg_query_frame_stats() C.sg_frame_stats
