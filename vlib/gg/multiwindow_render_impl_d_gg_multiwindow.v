module gg

import sokol.gfx
import sokol.sgl
import x.multiwindow

struct MultiWindowSglFlushState {
mut:
	flushed bool
}

fn (context &WindowInitContext) validate_managed_or_panic() {
	if context.app == unsafe { nil } || context.app_instance != context.app.app_instance {
		panic(err_multiwindow_render_stale_lease)
	}
	context.app.assert_owner_thread() or { panic(err_multiwindow_render_owner_thread) }
	context.app.render_runtime.validate_phase_lease(context.info.window, context.lease_epoch, .init) or {
		panic(err.msg())
	}
}

fn (context &WindowContext) validate_managed_or_panic() {
	context.validate_managed_frame() or { panic(err.msg()) }
}

fn (context &WindowContext) validate_managed_frame() ! {
	if context.app == unsafe { nil } || context.app_instance != context.app.app_instance {
		return error(err_multiwindow_render_stale_lease)
	}
	context.app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	context.app.render_runtime.validate_frame_lease(context.info.window, context.lease_epoch)!
}

fn (context &WindowCleanupContext) validate_managed_or_panic() {
	if context.app == unsafe { nil } || context.app_instance != context.app.app_instance {
		panic(err_multiwindow_render_stale_lease)
	}
	context.app.assert_owner_thread() or { panic(err_multiwindow_render_owner_thread) }
	context.app.render_runtime.validate_phase_lease(context.info.window, context.lease_epoch,
		.cleanup) or { panic(err.msg()) }
}

fn (context &WindowContext) logical_to_pixel_rect_managed(rect WindowLogicalRect) !WindowPixelRect {
	context.validate_managed_or_panic()
	x, y, width, height := context.app.core.logical_to_pixel_render_rect(context.info.window.core,
		context.info.metrics.metrics_sequence, rect.x, rect.y, rect.width, rect.height)!
	return WindowPixelRect{
		x:      x
		y:      y
		width:  width
		height: height
	}
}

fn (context &WindowContext) pixel_to_logical_rect_managed(rect WindowPixelRect) !WindowLogicalRect {
	context.validate_managed_or_panic()
	x, y, width, height := context.app.core.pixel_to_logical_render_rect(context.info.window.core,
		context.info.metrics.metrics_sequence, rect.x, rect.y, rect.width, rect.height)!
	return WindowLogicalRect{
		x:      x
		y:      y
		width:  width
		height: height
	}
}

fn (mut app App) request_redraw_managed(id WindowId) ! {
	app.ensure_initialized()!
	app.render_runtime.validate_redraw_admission(id)!
	app.core.request_redraw(id.core)!
}

fn (app &App) window_metrics_managed(id WindowId) !WindowMetrics {
	app.ensure_initialized()!
	snapshot := app.core.render_window_snapshot(id.core)!
	if !snapshot.metrics.metrics_available {
		return error(err_multiwindow_render_metrics_unavailable)
	}
	return window_metrics_from_core(snapshot)
}

fn (app &App) window_render_target_info_managed(id WindowId) !WindowRenderTargetInfo {
	app.ensure_initialized()!
	snapshot := app.core.render_window_snapshot(id.core)!
	if snapshot.target.sample_count <= 0 {
		return error(err_multiwindow_render_backend_unavailable)
	}
	return WindowRenderTargetInfo{
		color_format: unsafe { gfx.PixelFormat(snapshot.target.color_format) }
		depth_format: unsafe { gfx.PixelFormat(snapshot.target.depth_format) }
		sample_count: snapshot.target.sample_count
	}
}

fn (mut app App) set_window_clear_color_managed(id WindowId, color Color) ! {
	app.ensure_initialized()!
	app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	app.render_runtime.set_clear_color(id, color)!
	app.core.request_redraw(id.core)!
}

fn (app &App) window_readback_capabilities_managed(id WindowId) !WindowReadbackCapabilities {
	app.ensure_initialized()!
	app.core.render_window_snapshot(id.core)!
	// No current backend exposes a contract-correct asynchronous copy path.
	return WindowReadbackCapabilities{}
}

fn (mut app App) request_window_capture_managed(id WindowId, config WindowReadbackConfig) !WindowReadbackId {
	app.ensure_initialized()!
	app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	app.core.render_window_snapshot(id.core)!
	_ = config
	return error(err_multiwindow_render_readback_unsupported)
}

fn (mut context WindowInitContext) with_resources_managed(phase MultiWindowRenderPhase, f WindowResourceFn) ! {
	if f == unsafe { nil } {
		return error(err_multiwindow_render_nil_callback)
	}
	if context.app == unsafe { nil } || context.app_instance != context.app.app_instance {
		return error(err_multiwindow_render_stale_lease)
	}
	mut app := context.app
	app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	mut resources := app.render_runtime.begin_resource_section(app, context.info.window,
		context.lease_epoch, phase, .window)!
	app.render_runtime.begin_user_callback()
	mut callback_error := IError(none)
	f(mut resources) or { callback_error = err }
	app.render_runtime.end_user_callback()
	app.render_runtime.finish_resource_section(resources)
	if callback_error !is none {
		return callback_error
	}
}

fn (mut context WindowCleanupContext) with_resources_managed(phase MultiWindowRenderPhase, f WindowResourceFn) ! {
	if f == unsafe { nil } {
		return error(err_multiwindow_render_nil_callback)
	}
	if context.app == unsafe { nil } || context.app_instance != context.app.app_instance {
		return error(err_multiwindow_render_stale_lease)
	}
	mut app := context.app
	app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	mut resources := app.render_runtime.begin_resource_section(app, context.info.window,
		context.lease_epoch, phase, .window)!
	app.render_runtime.begin_user_callback()
	mut callback_error := IError(none)
	f(mut resources) or { callback_error = err }
	app.render_runtime.end_user_callback()
	app.render_runtime.finish_resource_section(resources)
	if callback_error !is none {
		return callback_error
	}
}

fn (mut context WindowContext) with_resources_managed(f WindowResourceFn) ! {
	if f == unsafe { nil } {
		return error(err_multiwindow_render_nil_callback)
	}
	if context.app == unsafe { nil } || context.app_instance != context.app.app_instance {
		return error(err_multiwindow_render_stale_lease)
	}
	mut app := context.app
	app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	mut resources := app.render_runtime.begin_resource_section(app, context.info.window,
		context.lease_epoch, .frame, .window)!
	app.render_runtime.begin_user_callback()
	mut callback_error := IError(none)
	f(mut resources) or { callback_error = err }
	app.render_runtime.end_user_callback()
	app.render_runtime.finish_resource_section(resources)
	if callback_error !is none {
		return callback_error
	}
}

fn (mut context WindowContext) with_offscreen_managed(config WindowOffscreenPassConfig, f WindowPassFn) ! {
	if f == unsafe { nil } {
		return error(err_multiwindow_render_nil_callback)
	}
	context.validate_managed_frame()!
	mut app := context.app
	window := context.info.window
	lease_epoch := context.lease_epoch
	attachments, _, target_key := app.managed_attachments(config.attachments, window)!
	pass_epoch := app.render_runtime.begin_pass(window, lease_epoch, false, false, target_key)!
	pass_desc := gfx.Pass{
		action:      config.action
		attachments: attachments
	}
	app.note_managed_gpu_work(app.active_batch_epoch) or {
		app.finish_render_epilogue(MultiWindowRenderEpilogue{
			window:      window
			lease_epoch: lease_epoch
			pass_epoch:  pass_epoch
		}, [err])!
		return
	}
	gfx.begin_pass(&pass_desc)
	mut pass_context := WindowPassContext{
		app:          app
		app_instance: context.app_instance
		window:       window
		lease_epoch:  lease_epoch
		pass_epoch:   pass_epoch
		info:         context.info
	}
	app.render_runtime.begin_user_callback()
	mut callback_error := IError(none)
	f(mut pass_context) or { callback_error = err }
	app.render_runtime.end_user_callback()
	if app.core.renderer_device_available_for_gg() {
		gfx.end_pass()
	} else if callback_error is none {
		callback_error = error(err_multiwindow_render_backend_unavailable)
	}
	mut errors := []IError{}
	if callback_error !is none {
		errors << callback_error
	}
	app.finish_render_epilogue(MultiWindowRenderEpilogue{
		window:      window
		lease_epoch: lease_epoch
		pass_epoch:  pass_epoch
	}, errors)!
}

fn (mut context WindowContext) with_swapchain_managed(action gfx.PassAction, f WindowPassFn) ! {
	if f == unsafe { nil } {
		return error(err_multiwindow_render_nil_callback)
	}
	context.validate_managed_frame()!
	mut app := context.app
	target_key := app.render_runtime.target_key(context.info.window, context.lease_epoch)!
	target_lease := app.render_runtime.target_lease(context.info.window, context.lease_epoch)!
	pass_epoch := app.render_runtime.begin_pass(context.info.window, context.lease_epoch, true,
		false, target_key)!
	mut pass_context := WindowPassContext{
		app:          app
		app_instance: context.app_instance
		window:       context.info.window
		lease_epoch:  context.lease_epoch
		pass_epoch:   pass_epoch
		info:         context.info
	}
	mut callback_error := IError(none)
	app.core.with_render_target_pass(target_lease, action, fn [mut app, mut pass_context, mut callback_error, f] () ! {
		app.render_runtime.begin_user_callback()
		f(mut pass_context) or { callback_error = err }
		app.render_runtime.end_user_callback()
		if callback_error !is none {
			return callback_error
		}
	}) or { callback_error = err }
	mut errors := []IError{}
	if callback_error !is none {
		errors << callback_error
	}
	app.finish_render_epilogue(MultiWindowRenderEpilogue{
		window:      context.info.window
		lease_epoch: context.lease_epoch
		pass_epoch:  pass_epoch
	}, errors)!
}

fn (mut context WindowContext) with_offscreen_sgl_managed(config WindowOffscreenPassConfig, f WindowSglFn) ! {
	if f == unsafe { nil } {
		return error(err_multiwindow_render_nil_callback)
	}
	context.validate_managed_frame()!
	mut app := context.app
	attachments, target, target_key := app.managed_attachments(config.attachments,
		context.info.window)!
	if !app.render_runtime.attachments_support_sgl(config.attachments, context.info.window)! {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	app.ensure_sgl_context(target_key, target)!
	context.with_sgl_pass(false, config.action, attachments, target_key, f)!
}

fn (mut context WindowContext) with_swapchain_sgl_managed(action gfx.PassAction, f WindowSglFn) ! {
	if f == unsafe { nil } {
		return error(err_multiwindow_render_nil_callback)
	}
	context.validate_managed_frame()!
	target_key := context.app.render_runtime.target_key(context.info.window, context.lease_epoch)!
	context.with_sgl_pass(true, action, gfx.Attachments{}, target_key, f)!
}

fn (mut context WindowContext) with_sgl_pass(swapchain bool, action gfx.PassAction, attachments gfx.Attachments, target_key string, f WindowSglFn) ! {
	if f == unsafe { nil } {
		return error(err_multiwindow_render_nil_callback)
	}
	context.validate_managed_frame()!
	mut app := context.app
	sgl_context := app.managed_sgl_context(target_key)!
	target_lease := if swapchain {
		app.render_runtime.target_lease(context.info.window, context.lease_epoch)!
	} else {
		multiwindow.RenderTargetLease{}
	}
	pass_epoch := app.render_runtime.begin_pass(context.info.window, context.lease_epoch,
		swapchain, true, target_key)!
	mut managed := WindowSglContext{
		app:          app
		app_instance: context.app_instance
		window:       context.info.window
		lease_epoch:  context.lease_epoch
		pass_epoch:   pass_epoch
		target_key:   target_key
	}
	mut callback_error := IError(none)
	previous := sgl.get_context()
	sgl.set_context(sgl_context)
	app.render_runtime.begin_user_callback()
	f(mut managed) or { callback_error = err }
	app.render_runtime.end_user_callback()
	if app.core.renderer_device_available_for_gg() {
		sgl.set_context(previous)
	} else if callback_error is none {
		callback_error = error(err_multiwindow_render_backend_unavailable)
	}
	mut flush_state := &MultiWindowSglFlushState{}
	flush_sgl := fn [app, mut flush_state, sgl_context] () ! {
		if !app.core.renderer_device_available_for_gg() {
			return error(err_multiwindow_render_backend_unavailable)
		}
		previous := sgl.get_context()
		sgl.set_context(sgl_context)
		sgl.context_draw(sgl_context)
		if !app.core.renderer_device_available_for_gg() {
			return error(err_multiwindow_render_backend_unavailable)
		}
		sgl_error := sgl.context_error(sgl_context)
		sgl.set_context(previous)
		flush_state.flushed = true
		if sgl_error != .no_error {
			return error(err_multiwindow_render_resource_failed)
		}
	}
	if callback_error is none && swapchain {
		app.core.with_render_target_pass(target_lease, action, flush_sgl) or {
			callback_error = err
		}
	} else if callback_error is none {
		pass_desc := gfx.Pass{
			action:      action
			attachments: attachments
		}
		app.note_managed_gpu_work(app.active_batch_epoch) or {
			if callback_error is none {
				callback_error = err
			}
		}
		if callback_error is none {
			gfx.begin_pass(&pass_desc)
			flush_sgl() or { callback_error = err }
			if app.core.renderer_device_available_for_gg() {
				gfx.end_pass()
			} else if callback_error is none {
				callback_error = error(err_multiwindow_render_backend_unavailable)
			}
		}
	}
	mut errors := []IError{}
	if callback_error !is none {
		errors << callback_error
	}
	app.finish_render_epilogue(MultiWindowRenderEpilogue{
		window:      context.info.window
		lease_epoch: context.lease_epoch
		pass_epoch:  pass_epoch
		sgl_flushed: flush_state.flushed
	}, errors)!
}

fn (mut context WindowContext) request_image_readback_managed(id WindowImageId, config WindowReadbackConfig) !WindowReadbackId {
	context.validate_managed_frame()!
	context.app.render_runtime.validate_readback_image(id, context.info.window)!
	_ = config
	return error(err_multiwindow_render_readback_unsupported)
}

fn (mut context WindowCleanupContext) with_native_window_managed(f NativeWindowBorrowFn) ! {
	if f == unsafe { nil } {
		return error(err_multiwindow_render_nil_callback)
	}
	if context.app == unsafe { nil } || context.app_instance != context.app.app_instance {
		return error(err_multiwindow_render_stale_lease)
	}
	context.app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	context.app.render_runtime.validate_phase_lease(context.info.window, context.lease_epoch,
		.cleanup)!
	return error(err_multiwindow_render_native_service_unavailable)
}

fn (mut context WindowSglContext) activate_sgl_managed() !sgl.Context {
	if context.app == unsafe { nil } || context.app_instance != context.app.app_instance {
		return error(err_multiwindow_render_stale_lease)
	}
	context.app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	context.app.render_runtime.validate_sgl_context(context)!
	sgl_context := context.app.managed_sgl_context(context.target_key)!
	sgl.set_context(sgl_context)
	return sgl_context
}

fn (mut context WindowSglContext) activate_sgl_managed_or_panic() {
	context.activate_sgl_managed() or { panic(err.msg()) }
}

fn (mut context WindowSglContext) texture_managed(image WindowImageId, sampler WindowSamplerId) ! {
	context.activate_sgl_managed()!
	native_image, native_sampler := context.app.managed_image_sampler(image, sampler,
		context.window)!
	sgl.texture(native_image, native_sampler)
}

fn (mut context WindowSglContext) load_pipeline_managed(id WindowSglPipelineId) ! {
	sgl_context := context.activate_sgl_managed()!
	pipeline := context.app.managed_sgl_pipeline(id, context.window, sgl_context,
		context.target_key)!
	$if test {
		multiwindow_sgl_load_pipeline_for_test(pipeline, context.window, sgl_context,
			context.target_key)
	} $else {
		sgl.load_pipeline(pipeline)
	}
}

fn (mut pass WindowPassContext) apply_pipeline_managed(id WindowPipelineId) ! {
	pass.validate_managed_pass()!
	pipeline := pass.app.managed_pipeline(id, pass.window)!
	gfx.apply_pipeline(pipeline)
}

fn (mut pass WindowPassContext) apply_bindings_managed(bindings WindowBindings) ! {
	pass.validate_managed_pass()!
	mut native := gfx.Bindings{}
	mut runtime := pass.app.render_runtime
	runtime.mutex.lock()
	mut used_vertex_slots := map[int]bool{}
	for binding in bindings.vertex_buffers {
		if binding.slot < 0 || binding.slot >= 8 || binding.offset < 0
			|| binding.slot in used_vertex_slots {
			runtime.mutex.unlock()
			return error(err_multiwindow_render_invalid_bindings)
		}
		index := runtime.resources.validate(buffer_resource_key(binding.buffer), .buffer,
			pass.window, .window) or {
			runtime.mutex.unlock()
			return err
		}
		buffer_slot := runtime.resources.slots[index]
		if buffer_slot.buffer_desc.type != .vertexbuffer
			|| usize(binding.offset) >= buffer_slot.buffer_capacity {
			runtime.mutex.unlock()
			return error(err_multiwindow_render_invalid_bindings)
		}
		native.vertex_buffers[binding.slot] = runtime.resources.slots[index].buffer
		native.vertex_buffer_offsets[binding.slot] = binding.offset
		used_vertex_slots[binding.slot] = true
	}
	if index_binding := bindings.index_buffer {
		if index_binding.slot != 0 || index_binding.offset < 0 {
			runtime.mutex.unlock()
			return error(err_multiwindow_render_invalid_bindings)
		}
		index := runtime.resources.validate(buffer_resource_key(index_binding.buffer), .buffer,
			pass.window, .window) or {
			runtime.mutex.unlock()
			return err
		}
		buffer_slot := runtime.resources.slots[index]
		if buffer_slot.buffer_desc.type != .indexbuffer
			|| usize(index_binding.offset) >= buffer_slot.buffer_capacity {
			runtime.mutex.unlock()
			return error(err_multiwindow_render_invalid_bindings)
		}
		native.index_buffer = runtime.resources.slots[index].buffer
		native.index_buffer_offset = index_binding.offset
	}
	apply_managed_stage_bindings(mut native.vs, bindings.vs, pass.window, runtime) or {
		runtime.mutex.unlock()
		return err
	}
	apply_managed_stage_bindings(mut native.fs, bindings.fs, pass.window, runtime) or {
		runtime.mutex.unlock()
		return err
	}
	runtime.mutex.unlock()
	gfx.apply_bindings(&native)
}

fn apply_managed_stage_bindings(mut native gfx.StageBindings, bindings WindowStageBindings, window WindowId, runtime &MultiWindowRenderRuntime) ! {
	mut image_slots := map[int]bool{}
	for binding in bindings.images {
		if binding.slot < 0 || binding.slot >= 12 || binding.slot in image_slots {
			return error(err_multiwindow_render_invalid_bindings)
		}
		index := runtime.resources.validate(image_resource_key(binding.image), .image, window,
			.window)!
		native.images[binding.slot] = runtime.resources.slots[index].image
		image_slots[binding.slot] = true
	}
	mut sampler_slots := map[int]bool{}
	for binding in bindings.samplers {
		if binding.slot < 0 || binding.slot >= 8 || binding.slot in sampler_slots {
			return error(err_multiwindow_render_invalid_bindings)
		}
		index := runtime.resources.validate(sampler_resource_key(binding.sampler), .sampler,
			window, .window)!
		native.samplers[binding.slot] = runtime.resources.slots[index].sampler
		sampler_slots[binding.slot] = true
	}
	mut storage_slots := map[int]bool{}
	for binding in bindings.storage_buffers {
		if binding.slot < 0 || binding.slot >= 8 || binding.offset != 0
			|| binding.slot in storage_slots {
			return error(err_multiwindow_render_invalid_bindings)
		}
		index := runtime.resources.validate(buffer_resource_key(binding.buffer), .buffer, window,
			.window)!
		if runtime.resources.slots[index].buffer_desc.type != .storagebuffer {
			return error(err_multiwindow_render_invalid_bindings)
		}
		native.storage_buffers[binding.slot] = runtime.resources.slots[index].buffer
		storage_slots[binding.slot] = true
	}
}

fn (mut pass WindowPassContext) apply_uniforms_managed(stage gfx.ShaderStage, block int, data &gfx.Range) ! {
	pass.validate_managed_pass()!
	if stage !in [.vs, .fs] || block < 0 || block >= 4 {
		return error(err_multiwindow_render_invalid_bindings)
	}
	validate_managed_range(data)!
	gfx.apply_uniforms(stage, block, data)
}

fn (mut pass WindowPassContext) draw_managed(base_element int, num_elements int, num_instances int) ! {
	pass.validate_managed_pass()!
	if base_element < 0 || num_elements <= 0 || num_instances <= 0 {
		return error(err_multiwindow_render_invalid_bindings)
	}
	gfx.draw(base_element, num_elements, num_instances)
}

fn (mut pass WindowPassContext) apply_viewport_managed(rect WindowPixelRect) ! {
	pass.validate_managed_pass()!
	validate_pixel_rect(rect)!
	gfx.apply_viewport(rect.x, rect.y, rect.width, rect.height, true)
}

fn (mut pass WindowPassContext) apply_scissor_managed(rect WindowPixelRect) ! {
	pass.validate_managed_pass()!
	validate_pixel_rect(rect)!
	gfx.apply_scissor_rect(rect.x, rect.y, rect.width, rect.height, true)
}

fn (pass &WindowPassContext) validate_managed_pass() ! {
	if pass.app == unsafe { nil } || pass.app_instance != pass.app.app_instance {
		return error(err_multiwindow_render_stale_lease)
	}
	pass.app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	pass.app.render_runtime.validate_pass_context(pass)!
	if !pass.app.gfx_started || !pass.app.core.renderer_is_usable() {
		return error(err_multiwindow_render_backend_unavailable)
	}
}

fn validate_pixel_rect(rect WindowPixelRect) ! {
	if rect.width <= 0 || rect.height <= 0 {
		return error(err_multiwindow_render_invalid_bindings)
	}
}

fn window_metrics_from_core(snapshot multiwindow.RenderWindowSnapshot) WindowMetrics {
	return WindowMetrics{
		logical_size:     WindowLogicalSize{
			width:  snapshot.metrics.logical_width
			height: snapshot.metrics.logical_height
		}
		framebuffer_size: WindowPixelSize{
			width:  snapshot.metrics.framebuffer_width
			height: snapshot.metrics.framebuffer_height
		}
		dpi_scale:        snapshot.metrics.dpi_scale
		metrics_sequence: snapshot.metrics.metrics_sequence
		submitted_frame:  snapshot.submitted_frame
	}
}
