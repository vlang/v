module gg

import sokol.gfx

// WindowOffscreenPassConfig selects managed attachments and the action used to begin an offscreen pass.
pub struct WindowOffscreenPassConfig {
pub:
	attachments WindowAttachmentsId
	action      gfx.PassAction
}

fn multiwindow_render_value_guard() {
	$if !gg_multiwindow ? {
		panic(multiwindow_render_unavailable_message())
	}
}

// request_redraw makes a window eligible for a future on-demand frame.
pub fn (mut app App) request_redraw(id WindowId) ! {
	$if gg_multiwindow ? {
		app.request_redraw_managed(id)!
	} $else {
		_ = app
		_ = id
		return error(multiwindow_render_unavailable_message())
	}
}

// window_metrics returns the latest committed logical and framebuffer metrics for a window.
pub fn (app &App) window_metrics(id WindowId) !WindowMetrics {
	$if gg_multiwindow ? {
		return app.window_metrics_managed(id)
	} $else {
		_ = app
		_ = id
		return error(multiwindow_render_unavailable_message())
	}
}

// window_render_target_info returns the current color, depth, and sampling contract for a window.
pub fn (app &App) window_render_target_info(id WindowId) !WindowRenderTargetInfo {
	$if gg_multiwindow ? {
		return app.window_render_target_info_managed(id)
	} $else {
		_ = app
		_ = id
		return error(multiwindow_render_unavailable_message())
	}
}

// set_window_clear_color updates the clear color used for the window's managed swapchain pass.
pub fn (mut app App) set_window_clear_color(id WindowId, color Color) ! {
	$if gg_multiwindow ? {
		app.set_window_clear_color_managed(id, color)!
	} $else {
		_ = app
		_ = id
		_ = color
		return error(multiwindow_render_unavailable_message())
	}
}

// window_readback_capabilities reports the readback operations supported for a window.
pub fn (app &App) window_readback_capabilities(id WindowId) !WindowReadbackCapabilities {
	$if gg_multiwindow ? {
		return app.window_readback_capabilities_managed(id)
	} $else {
		_ = app
		_ = id
		return error(multiwindow_render_unavailable_message())
	}
}

// request_window_capture queues an asynchronous capture of a window's rendered pixels.
pub fn (mut app App) request_window_capture(id WindowId, config WindowReadbackConfig) !WindowReadbackId {
	$if gg_multiwindow ? {
		return app.request_window_capture_managed(id, config)
	} $else {
		_ = app
		_ = id
		_ = config
		return error(multiwindow_render_unavailable_message())
	}
}

// window_id identifies the window that owns this initialization callback.
pub fn (context &WindowInitContext) window_id() WindowId {
	multiwindow_render_value_guard()
	$if gg_multiwindow ? { context.validate_managed_or_panic() }
	return context.info.window
}

// metrics returns the immutable window metrics admitted for initialization.
pub fn (context &WindowInitContext) metrics() WindowMetrics {
	multiwindow_render_value_guard()
	$if gg_multiwindow ? { context.validate_managed_or_panic() }
	return context.info.metrics
}

// render_target_info returns the render-target contract admitted for initialization.
pub fn (context &WindowInitContext) render_target_info() WindowRenderTargetInfo {
	multiwindow_render_value_guard()
	$if gg_multiwindow ? { context.validate_managed_or_panic() }
	return context.info.target
}

// with_resources grants managed resource authority scoped to the initialization callback.
pub fn (mut context WindowInitContext) with_resources(f WindowResourceFn) ! {
	$if gg_multiwindow ? {
		context.with_resources_managed(.init, f)!
	} $else {
		_ = context
		_ = f
		return error(multiwindow_render_unavailable_message())
	}
}

// frame_info returns the immutable window and target snapshot bound to this frame callback.
pub fn (context &WindowContext) frame_info() WindowFrameInfo {
	multiwindow_render_value_guard()
	$if gg_multiwindow ? { context.validate_managed_or_panic() }
	return context.info
}

// logical_size returns the current drawable size in logical coordinates.
pub fn (context &WindowContext) logical_size() WindowLogicalSize {
	multiwindow_render_value_guard()
	$if gg_multiwindow ? { context.validate_managed_or_panic() }
	return context.info.metrics.logical_size
}

// logical_bounds returns the current logical drawable rectangle with an origin of zero.
pub fn (context &WindowContext) logical_bounds() WindowLogicalRect {
	size := context.logical_size()
	return WindowLogicalRect{
		width:  size.width
		height: size.height
	}
}

// pixel_bounds returns the current framebuffer rectangle with an origin of zero.
pub fn (context &WindowContext) pixel_bounds() WindowPixelRect {
	multiwindow_render_value_guard()
	$if gg_multiwindow ? { context.validate_managed_or_panic() }
	return WindowPixelRect{
		width:  context.info.metrics.framebuffer_size.width
		height: context.info.metrics.framebuffer_size.height
	}
}

// logical_to_pixel_rect converts a logical rectangle using this frame's admitted metrics.
pub fn (context &WindowContext) logical_to_pixel_rect(rect WindowLogicalRect) WindowPixelRect {
	$if gg_multiwindow ? {
		return context.logical_to_pixel_rect_managed(rect) or { panic(err.msg()) }
	} $else {
		_ = context
		_ = rect
		panic(multiwindow_render_unavailable_message())
	}
}

// pixel_to_logical_rect converts a framebuffer rectangle using this frame's admitted metrics.
pub fn (context &WindowContext) pixel_to_logical_rect(rect WindowPixelRect) WindowLogicalRect {
	$if gg_multiwindow ? {
		return context.pixel_to_logical_rect_managed(rect) or { panic(err.msg()) }
	} $else {
		_ = context
		_ = rect
		panic(multiwindow_render_unavailable_message())
	}
}

// with_resources grants managed resource authority scoped to the current frame callback.
pub fn (mut context WindowContext) with_resources(f WindowResourceFn) ! {
	$if gg_multiwindow ? {
		context.with_resources_managed(f)!
	} $else {
		_ = context
		_ = f
		return error(multiwindow_render_unavailable_message())
	}
}

// with_offscreen runs a managed render pass against managed offscreen attachments.
pub fn (mut context WindowContext) with_offscreen(config WindowOffscreenPassConfig, f WindowPassFn) ! {
	$if gg_multiwindow ? {
		context.with_offscreen_managed(config, f)!
	} $else {
		_ = context
		_ = config
		_ = f
		return error(multiwindow_render_unavailable_message())
	}
}

// with_swapchain runs a managed render pass against the current window swapchain.
pub fn (mut context WindowContext) with_swapchain(action gfx.PassAction, f WindowPassFn) ! {
	$if gg_multiwindow ? {
		context.with_swapchain_managed(action, f)!
	} $else {
		_ = context
		_ = action
		_ = f
		return error(multiwindow_render_unavailable_message())
	}
}

// with_offscreen_sgl runs scoped SGL drawing against managed offscreen attachments.
pub fn (mut context WindowContext) with_offscreen_sgl(config WindowOffscreenPassConfig, f WindowSglFn) ! {
	$if gg_multiwindow ? {
		context.with_offscreen_sgl_managed(config, f)!
	} $else {
		_ = context
		_ = config
		_ = f
		return error(multiwindow_render_unavailable_message())
	}
}

// with_swapchain_sgl runs scoped SGL drawing against the current window swapchain.
pub fn (mut context WindowContext) with_swapchain_sgl(action gfx.PassAction, f WindowSglFn) ! {
	$if gg_multiwindow ? {
		context.with_swapchain_sgl_managed(action, f)!
	} $else {
		_ = context
		_ = action
		_ = f
		return error(multiwindow_render_unavailable_message())
	}
}

// request_image_readback queues an asynchronous readback of a managed image.
pub fn (mut context WindowContext) request_image_readback(id WindowImageId, config WindowReadbackConfig) !WindowReadbackId {
	$if gg_multiwindow ? {
		return context.request_image_readback_managed(id, config)
	} $else {
		_ = context
		_ = id
		_ = config
		return error(multiwindow_render_unavailable_message())
	}
}

// window_id identifies the window that owns this cleanup callback.
pub fn (context &WindowCleanupContext) window_id() WindowId {
	multiwindow_render_value_guard()
	$if gg_multiwindow ? { context.validate_managed_or_panic() }
	return context.info.window
}

// metrics returns the final immutable metrics snapshot supplied to cleanup.
pub fn (context &WindowCleanupContext) metrics() WindowMetrics {
	multiwindow_render_value_guard()
	$if gg_multiwindow ? { context.validate_managed_or_panic() }
	return context.info.metrics
}

// render_target_info returns the final render-target contract supplied to cleanup.
pub fn (context &WindowCleanupContext) render_target_info() WindowRenderTargetInfo {
	multiwindow_render_value_guard()
	$if gg_multiwindow ? { context.validate_managed_or_panic() }
	return context.info.target
}

// reason identifies why the window's render lifetime is ending.
pub fn (context &WindowCleanupContext) reason() WindowCleanupReason {
	multiwindow_render_value_guard()
	$if gg_multiwindow ? { context.validate_managed_or_panic() }
	return context.cleanup_reason
}

// graphics_available reports whether managed graphics teardown is still permitted.
pub fn (context &WindowCleanupContext) graphics_available() bool {
	multiwindow_render_value_guard()
	$if gg_multiwindow ? { context.validate_managed_or_panic() }
	return context.has_graphics
}

// with_resources grants managed resource teardown authority during cleanup.
pub fn (mut context WindowCleanupContext) with_resources(f WindowResourceFn) ! {
	$if gg_multiwindow ? {
		context.with_resources_managed(.cleanup, f)!
	} $else {
		_ = context
		_ = f
		return error(multiwindow_render_unavailable_message())
	}
}

// with_native_window borrows the backend's live native window for the duration of the callback.
pub fn (mut context WindowCleanupContext) with_native_window(f NativeWindowBorrowFn) ! {
	$if gg_multiwindow ? {
		context.with_native_window_managed(f)!
	} $else {
		_ = context
		_ = f
		return error(multiwindow_render_unavailable_message())
	}
}

// make_buffer creates a buffer owned by the resource context's scope.
pub fn (mut resources WindowResourceContext) make_buffer(desc &gfx.BufferDesc) !WindowBufferId {
	$if gg_multiwindow ? {
		return resources.make_buffer_managed(desc)
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// make_image creates an image owned by the resource context's scope.
pub fn (mut resources WindowResourceContext) make_image(desc &gfx.ImageDesc) !WindowImageId {
	$if gg_multiwindow ? {
		return resources.make_image_managed(desc)
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// make_sampler creates a sampler owned by the resource context's scope.
pub fn (mut resources WindowResourceContext) make_sampler(desc &gfx.SamplerDesc) !WindowSamplerId {
	$if gg_multiwindow ? {
		return resources.make_sampler_managed(desc)
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// make_shader creates a shader owned by the resource context's scope.
pub fn (mut resources WindowResourceContext) make_shader(desc &gfx.ShaderDesc) !WindowShaderId {
	$if gg_multiwindow ? {
		return resources.make_shader_managed(desc)
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// make_pipeline creates a pipeline owned by the resource context's scope using a managed shader.
pub fn (mut resources WindowResourceContext) make_pipeline(desc &gfx.PipelineDesc, shader WindowShaderId) !WindowPipelineId {
	$if gg_multiwindow ? {
		return resources.make_pipeline_managed(desc, shader)
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// make_attachments creates attachments owned by the resource context's scope from managed images.
pub fn (mut resources WindowResourceContext) make_attachments(config WindowAttachmentsConfig) !WindowAttachmentsId {
	$if gg_multiwindow ? {
		return resources.make_attachments_managed(config)
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// make_sgl_pipeline creates an SGL pipeline owned by the resource context's scope.
pub fn (mut resources WindowResourceContext) make_sgl_pipeline(desc &gfx.PipelineDesc) !WindowSglPipelineId {
	$if gg_multiwindow ? {
		return resources.make_sgl_pipeline_managed(desc)
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// make_sgl_pipeline_with_shader creates an SGL pipeline owned by the resource context's scope using a managed shader.
pub fn (mut resources WindowResourceContext) make_sgl_pipeline_with_shader(desc &gfx.PipelineDesc, shader WindowShaderId) !WindowSglPipelineId {
	$if gg_multiwindow ? {
		return resources.make_sgl_pipeline_with_shader_managed(desc, shader)
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// update_buffer replaces the contents of a managed buffer.
pub fn (mut resources WindowResourceContext) update_buffer(id WindowBufferId, data &gfx.Range) ! {
	$if gg_multiwindow ? {
		resources.update_buffer_managed(id, data)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// append_buffer appends data to a managed streaming buffer and returns its byte offset.
pub fn (mut resources WindowResourceContext) append_buffer(id WindowBufferId, data &gfx.Range) !int {
	$if gg_multiwindow ? {
		return resources.append_buffer_managed(id, data)
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// update_image replaces the uploaded contents of a managed image.
pub fn (mut resources WindowResourceContext) update_image(id WindowImageId, data &gfx.ImageData) ! {
	$if gg_multiwindow ? {
		resources.update_image_managed(id, data)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// replace_image recreates a managed image and returns the replacement generation.
pub fn (mut resources WindowResourceContext) replace_image(id WindowImageId, desc &gfx.ImageDesc) !WindowImageId {
	$if gg_multiwindow ? {
		return resources.replace_image_managed(id, desc)
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// retire_buffer releases a managed buffer and invalidates its identifier.
pub fn (mut resources WindowResourceContext) retire_buffer(id WindowBufferId) ! {
	$if gg_multiwindow ? {
		resources.retire_managed(buffer_resource_key(id), .buffer)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// retire_image releases a managed image and invalidates its identifier.
pub fn (mut resources WindowResourceContext) retire_image(id WindowImageId) ! {
	$if gg_multiwindow ? {
		resources.retire_managed(image_resource_key(id), .image)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// retire_sampler releases a managed sampler and invalidates its identifier.
pub fn (mut resources WindowResourceContext) retire_sampler(id WindowSamplerId) ! {
	$if gg_multiwindow ? {
		resources.retire_managed(sampler_resource_key(id), .sampler)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// retire_shader releases a managed shader and invalidates its identifier.
pub fn (mut resources WindowResourceContext) retire_shader(id WindowShaderId) ! {
	$if gg_multiwindow ? {
		resources.retire_managed(shader_resource_key(id), .shader)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// retire_pipeline releases a managed pipeline and invalidates its identifier.
pub fn (mut resources WindowResourceContext) retire_pipeline(id WindowPipelineId) ! {
	$if gg_multiwindow ? {
		resources.retire_managed(pipeline_resource_key(id), .pipeline)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// retire_attachments releases managed attachments and invalidates their identifier.
pub fn (mut resources WindowResourceContext) retire_attachments(id WindowAttachmentsId) ! {
	$if gg_multiwindow ? {
		resources.retire_managed(attachments_resource_key(id), .attachments)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// retire_sgl_pipeline releases a managed SGL pipeline and invalidates its identifier.
pub fn (mut resources WindowResourceContext) retire_sgl_pipeline(id WindowSglPipelineId) ! {
	$if gg_multiwindow ? {
		resources.retire_managed(sgl_pipeline_resource_key(id), .sgl_pipeline)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// apply_pipeline selects a managed pipeline for subsequent draw calls in this pass.
pub fn (mut pass WindowPassContext) apply_pipeline(id WindowPipelineId) ! {
	$if gg_multiwindow ? {
		pass.apply_pipeline_managed(id)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// apply_bindings binds managed buffers, images, and samplers for this pass.
pub fn (mut pass WindowPassContext) apply_bindings(bindings WindowBindings) ! {
	$if gg_multiwindow ? {
		pass.apply_bindings_managed(bindings)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// apply_uniforms uploads one shader-stage uniform block for this pass.
pub fn (mut pass WindowPassContext) apply_uniforms(stage gfx.ShaderStage, block int, data &gfx.Range) ! {
	$if gg_multiwindow ? {
		pass.apply_uniforms_managed(stage, block, data)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// draw submits indexed or non-indexed geometry through the active managed pass.
pub fn (mut pass WindowPassContext) draw(base_element int, num_elements int, num_instances int) ! {
	$if gg_multiwindow ? {
		pass.draw_managed(base_element, num_elements, num_instances)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// apply_viewport sets the framebuffer-space viewport for the active managed pass.
pub fn (mut pass WindowPassContext) apply_viewport(rect WindowPixelRect) ! {
	$if gg_multiwindow ? {
		pass.apply_viewport_managed(rect)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// apply_scissor sets the framebuffer-space scissor rectangle for the active managed pass.
pub fn (mut pass WindowPassContext) apply_scissor(rect WindowPixelRect) ! {
	$if gg_multiwindow ? {
		pass.apply_scissor_managed(rect)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}
