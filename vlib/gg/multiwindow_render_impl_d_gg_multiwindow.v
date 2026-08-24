module gg

import sokol.gfx
import sokol.sgl
import x.multiwindow

$if linux {
	$if x_multiwindow_x11 ? || sokol_wayland ? {
		#insert "@VMODROOT/vlib/gg/multiwindow_gl_readback_helpers.h"

		fn C.v_gg_multiwindow_gl_readback_image_rgba8(image_id u32, image_height int, x int, y int, width int, height int, pixels &u8, pixels_len usize) int
		fn C.v_gg_multiwindow_gl_readback_window_rgba8(framebuffer_height int, x int, y int, width int, height int, pixels &u8, pixels_len usize) int
	}
}

struct MultiWindowPendingWindowCapture {
	id                     multiwindow.ServiceReadbackId
	window                 WindowId
	rect                   WindowPixelRect
	target_submitted_frame u64
mut:
	attempt_batch_epoch u64
	staged_batch_epoch  u64
	staged_pixels       []u8
}

struct MultiWindowPendingImageReadback {
	id                     multiwindow.ServiceReadbackId
	window                 WindowId
	image                  WindowImageId
	batch_epoch            u64
	target_submitted_frame u64
	width                  int
	height                 int
	stride                 int
	pixels                 []u8
}

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
	image := app.core.service_operation_capability(id.core, .image_readback)!
	window := app.window_operation_capability(id, .window_capture)!
	mut offscreen_image := false
	$if linux {
		$if x_multiwindow_x11 ? || sokol_wayland ? {
			offscreen_image = app.capabilities().backend in [.x11, .wayland]
				&& app.core.renderer_device_available_for_gg()
				&& gfx.query_backend() in [.glcore33, .gles3] && image.support == .available
		}
	}
	$if darwin {
		$if darwin_sokol_glcore33 ? {
		} $else {
			offscreen_image = app.capabilities().backend == .appkit
				&& app.core.renderer_device_available_for_gg()
				&& gfx.query_backend() == .metal_macos && image.support == .available
		}
	}
	return WindowReadbackCapabilities{
		offscreen_image: offscreen_image
		window_capture:  window.support == .available
	}
}

fn (mut app App) stage_appkit_window_capture(readback multiwindow.ServiceReadbackId, rect WindowPixelRect, producing_frame u64) ! {
	app.core.service_stage_window_readback_for_gg(readback, rect.x, rect.y, rect.width,
		rect.height, producing_frame)!
}

fn (mut app App) arm_appkit_image_readbacks_for_pass(id WindowId, image_id u32, pass_serial u64, producing_frame u64) ! {
	app.core.service_arm_image_readback_pass_for_gg(id.core, image_id, pass_serial, producing_frame)!
}

fn readback_rect_fits(rect WindowPixelRect, target_width int, target_height int) bool {
	if target_width <= 0 || target_height <= 0 || rect.x < 0 || rect.y < 0 || rect.width <= 0
		|| rect.height <= 0 || rect.width > target_width || rect.height > target_height {
		return false
	}
	return rect.x <= target_width - rect.width && rect.y <= target_height - rect.height
}

fn capture_block_reason_prevents_future_frame(reason multiwindow.RenderBlockReason) bool {
	return reason in [.no_workload, .not_configured, .hidden, .minimized, .occluded, .unmapped,
		.not_viewable, .zero_sized, .backend_unavailable, .renderer_failed]
}

fn managed_window_capture_has_producer(producer MultiWindowCaptureProducer, snapshot multiwindow.RenderWindowSnapshot) bool {
	if !snapshot.metrics.metrics_available || snapshot.metrics.framebuffer_width <= 0
		|| snapshot.metrics.framebuffer_height <= 0 || snapshot.target.sample_count <= 0 {
		return false
	}
	if producer.frame_active {
		return true
	}
	if !producer.frame_fn_configured {
		return false
	}
	return !capture_block_reason_prevents_future_frame(snapshot.block_reason)
}

fn unbound_managed_window_capture_is_permanently_blocked(producer MultiWindowCaptureProducer, snapshot multiwindow.RenderWindowSnapshot) bool {
	if producer.frame_active {
		return false
	}
	return !producer.frame_fn_configured
		|| capture_block_reason_prevents_future_frame(snapshot.block_reason)
}

fn (mut app App) reconcile_unbound_managed_window_captures() ! {
	if app.pending_window_captures.len == 0 {
		return
	}
	mut index := 0
	for index < app.pending_window_captures.len {
		capture := app.pending_window_captures[index]
		if capture.attempt_batch_epoch != 0 || !app.window_exists(capture.window) {
			index++
			continue
		}
		snapshot := app.core.render_window_snapshot(capture.window.core) or {
			app.core.service_fail_window_readback(capture.id,
				err_multiwindow_render_capture_cancelled)!
			app.pending_window_captures.delete(index)
			continue
		}
		producer := app.render_runtime.window_capture_producer(capture.window) or {
			app.core.service_fail_window_readback(capture.id,
				err_multiwindow_render_capture_cancelled)!
			app.pending_window_captures.delete(index)
			continue
		}
		if !unbound_managed_window_capture_is_permanently_blocked(producer, snapshot) {
			index++
			continue
		}
		app.core.service_fail_window_readback(capture.id, err_multiwindow_render_capture_cancelled)!
		app.pending_window_captures.delete(index)
	}
}

fn (mut app App) request_window_capture_redraw(id WindowId) ! {
	if message := app.render_runtime.take_internal_fault(.capture_request_redraw) {
		return error(message)
	}
	app.core.request_redraw(id.core)!
}

fn (mut app App) request_window_capture_managed(id WindowId, config WindowReadbackConfig) !WindowReadbackId {
	app.ensure_initialized()!
	app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	snapshot := app.core.render_window_snapshot(id.core)!
	backend := app.capabilities().backend
	renderer_active := backend in [.x11, .wayland, .appkit]
		&& app.core.renderer_device_available_for_gg()
	if renderer_active {
		if app.window_operation_capability(id, .window_capture)!.support != .available {
			return error(err_multiwindow_render_readback_unsupported)
		}
		if (backend in [.x11, .wayland] && gfx.query_backend() !in [.glcore33, .gles3])
			|| (backend == .appkit && gfx.query_backend() != .metal_macos) {
			return error(err_multiwindow_render_readback_unsupported)
		}
		mut width := snapshot.metrics.framebuffer_width
		mut height := snapshot.metrics.framebuffer_height
		mut x := 0
		mut y := 0
		if rect := config.rect {
			if !readback_rect_fits(rect, width, height) {
				return error(err_multiwindow_render_readback_unsupported)
			}
			width = rect.width
			height = rect.height
			x = rect.x
			y = rect.y
		}
		if snapshot.submitted_frame == u64(0xffffffffffffffff) {
			return error(err_multiwindow_render_readback_unsupported)
		}
		producer := app.render_runtime.window_capture_producer(id)!
		if !managed_window_capture_has_producer(producer, snapshot) {
			return error(err_multiwindow_render_readback_unsupported)
		}
		readback := app.core.service_begin_window_readback_with_payload_for_gg(id.core, width,
			height)!
		app.request_window_capture_redraw(id) or {
			app.core.service_rollback_window_readback_for_gg(readback)
			return err
		}
		if backend == .appkit {
			rect := WindowPixelRect{
				x:      x
				y:      y
				width:  width
				height: height
			}
			app.stage_appkit_window_capture(readback, rect, snapshot.submitted_frame + 1) or {
				app.core.service_abandon_window_readback_for_gg(readback, err.msg())!
				return window_readback_id_from_core(readback)
			}
			return window_readback_id_from_core(readback)
		}
		app.pending_window_captures << MultiWindowPendingWindowCapture{
			id:                     readback
			window:                 id
			rect:                   WindowPixelRect{
				x:      x
				y:      y
				width:  width
				height: height
			}
			target_submitted_frame: snapshot.submitted_frame + 1
			attempt_batch_epoch:    app.active_batch_epoch
		}
		return window_readback_id_from_core(readback)
	}
	if backend == .wayland {
		return error(err_multiwindow_render_readback_unsupported)
	}
	if backend == .appkit {
		return error(err_multiwindow_render_readback_unsupported)
	}
	if backend == .win32
		&& app.window_operation_capability(id, .window_capture)!.support == .unsupported {
		return error(err_multiwindow_render_readback_unsupported)
	}
	info := app.core.window_info(id.core)!
	mut width := info.width
	mut height := info.height
	mut x := 0
	mut y := 0
	if rect := config.rect {
		if !readback_rect_fits(rect, info.width, info.height) {
			return error(err_multiwindow_render_readback_unsupported)
		}
		width = rect.width
		height = rect.height
		x = rect.x
		y = rect.y
	}
	return window_readback_id_from_core(app.core.service_request_window_readback_region(id.core, x,
		y, width, height, snapshot.submitted_frame)!)
}

fn (mut app App) bind_managed_window_capture_attempts(id WindowId, target_submitted_frame u64, batch_epoch u64) {
	for index, capture in app.pending_window_captures {
		if capture.window == id && capture.target_submitted_frame == target_submitted_frame
			&& capture.attempt_batch_epoch == 0 {
			app.pending_window_captures[index].attempt_batch_epoch = batch_epoch
		}
	}
}

fn (mut app App) stage_managed_window_captures(id WindowId, target_submitted_frame u64, framebuffer_height int) ! {
	if app.capabilities().backend !in [.x11, .wayland] || app.pending_window_captures.len == 0 {
		return
	}
	$if linux {
		$if x_multiwindow_x11 ? || sokol_wayland ? {
			mut index := 0
			for index < app.pending_window_captures.len {
				capture := app.pending_window_captures[index]
				if capture.window != id || capture.target_submitted_frame != target_submitted_frame
					|| capture.attempt_batch_epoch != app.active_batch_epoch
					|| capture.staged_pixels.len > 0 {
					index++
					continue
				}
				mut pixels := []u8{len: capture.rect.width * capture.rect.height * 4}
				status := C.v_gg_multiwindow_gl_readback_window_rgba8(framebuffer_height,
					capture.rect.x, capture.rect.y, capture.rect.width, capture.rect.height,
					pixels.data, usize(pixels.len))
				if status != 0 {
					app.core.service_fail_window_readback(capture.id,
						err_multiwindow_render_readback_unsupported)!
					app.pending_window_captures.delete(index)
					continue
				}
				app.pending_window_captures[index].staged_batch_epoch = app.active_batch_epoch
				app.pending_window_captures[index].staged_pixels = pixels
				index++
			}
			return
		}
	}
	_ = id
	_ = target_submitted_frame
	_ = framebuffer_height
	return error(err_multiwindow_render_readback_unsupported)
}

fn (mut app App) fail_managed_window_captures_for_batch(batch_epoch u64, message string) ! {
	if app.pending_window_captures.len == 0 || batch_epoch == 0 {
		return
	}
	mut retained := []MultiWindowPendingWindowCapture{cap: app.pending_window_captures.len}
	mut errors := []string{}
	terminal_message := if message == '' {
		err_multiwindow_render_readback_unsupported
	} else {
		message
	}
	for capture in app.pending_window_captures {
		if capture.attempt_batch_epoch != batch_epoch {
			retained << capture
			continue
		}
		if !app.window_exists(capture.window) {
			continue
		}
		app.core.service_fail_window_readback(capture.id, terminal_message) or {
			errors << err.msg()
		}
	}
	app.pending_window_captures = retained
	if errors.len > 0 {
		return error('${err_multiwindow_render_callback_failed}: ${errors.join('; ')}')
	}
}

fn (mut app App) finish_managed_window_captures(outcome multiwindow.RenderBatchOutcome) ! {
	if app.pending_window_captures.len == 0 {
		return
	}
	mut retained := []MultiWindowPendingWindowCapture{cap: app.pending_window_captures.len}
	mut errors := []string{}
	for capture in app.pending_window_captures {
		if capture.attempt_batch_epoch != outcome.batch_epoch {
			retained << capture
			continue
		}
		if !app.window_exists(capture.window) {
			continue
		}
		snapshot := app.core.render_window_snapshot(capture.window.core) or {
			app.core.service_fail_window_readback(capture.id, err.msg()) or { errors << err.msg() }
			continue
		}
		succeeded := outcome.error == '' && outcome.committed
			&& capture.staged_batch_epoch == outcome.batch_epoch
			&& capture.staged_pixels.len == capture.rect.width * capture.rect.height * 4
			&& snapshot.submitted_frame == capture.target_submitted_frame
		if !succeeded {
			message := if outcome.error == '' {
				err_multiwindow_render_readback_unsupported
			} else {
				outcome.error
			}
			app.core.service_fail_window_readback(capture.id, message) or { errors << err.msg() }
			continue
		}
		app.core.service_finish_window_readback(capture.id, capture.rect.width,
			capture.rect.height, capture.rect.width * 4, capture.staged_pixels,
			capture.target_submitted_frame) or { errors << err.msg() }
	}
	app.pending_window_captures = retained
	if errors.len > 0 {
		return error('${err_multiwindow_render_callback_failed}: ${errors.join('; ')}')
	}
}

fn (mut app App) fail_linux_gl_image_readbacks_for_batch(batch_epoch u64, message string) ! {
	if app.pending_image_readbacks.len == 0 {
		return
	}
	mut retained := []MultiWindowPendingImageReadback{cap: app.pending_image_readbacks.len}
	mut errors := []string{}
	terminal_message := if message == '' {
		err_multiwindow_render_readback_unsupported
	} else {
		message
	}
	for readback in app.pending_image_readbacks {
		if readback.batch_epoch != batch_epoch {
			retained << readback
			continue
		}
		app.core.service_fail_window_readback(readback.id, terminal_message) or {
			errors << err.msg()
		}
	}
	app.pending_image_readbacks = retained
	if errors.len > 0 {
		return error('${err_multiwindow_render_callback_failed}: ${errors.join('; ')}')
	}
}

fn (mut app App) finish_linux_gl_image_readbacks(outcome multiwindow.RenderBatchOutcome) ! {
	if app.pending_image_readbacks.len == 0 {
		return
	}
	if outcome.error != '' || !outcome.committed || outcome.finalized_submissions <= 0 {
		app.fail_linux_gl_image_readbacks_for_batch(outcome.batch_epoch, outcome.error)!
		return
	}
	mut retained := []MultiWindowPendingImageReadback{cap: app.pending_image_readbacks.len}
	mut errors := []string{}
	for readback in app.pending_image_readbacks {
		if readback.batch_epoch != outcome.batch_epoch {
			retained << readback
			continue
		}
		snapshot := app.core.render_window_snapshot(readback.window.core) or {
			app.core.service_fail_window_readback(readback.id, err.msg()) or { errors << err.msg() }
			continue
		}
		if snapshot.submitted_frame != readback.target_submitted_frame {
			app.core.service_fail_window_readback(readback.id,
				err_multiwindow_render_readback_unsupported) or { errors << err.msg() }
			continue
		}
		app.core.service_finish_window_readback(readback.id, readback.width, readback.height,
			readback.stride, readback.pixels, readback.target_submitted_frame) or {
			errors << err.msg()
		}
	}
	app.pending_image_readbacks = retained
	if errors.len > 0 {
		return error('${err_multiwindow_render_callback_failed}: ${errors.join('; ')}')
	}
}

fn (mut app App) discard_window_capture_requests(id WindowId) {
	app.pending_window_captures = app.pending_window_captures.filter(it.window != id)
	app.pending_image_readbacks = app.pending_image_readbacks.filter(it.window != id)
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
	snapshot := context.app.render_runtime.readback_image_snapshot(id, context.info.window)!
	mut x := 0
	mut y := 0
	mut width := snapshot.desc.width
	mut height := snapshot.desc.height
	if rect := config.rect {
		if !readback_rect_fits(rect, width, height) {
			return error(err_multiwindow_render_readback_unsupported)
		}
		x = rect.x
		y = rect.y
		width = rect.width
		height = rect.height
	}
	capability := context.app.core.service_operation_capability(context.info.window.core,
		.image_readback)!
	if capability.support != .available {
		return error(err_multiwindow_render_readback_unsupported)
	}
	$if darwin {
		$if darwin_sokol_glcore33 ? {
		} $else {
			if context.app.capabilities().backend == .appkit && gfx.query_backend() == .metal_macos {
				if context.info.submitted_frame == u64(0xffffffffffffffff) {
					return error(err_multiwindow_render_readback_unsupported)
				}
				mut app := context.app
				readback := app.core.service_begin_window_readback_with_payload_for_gg(context.info.window.core,
					width, height)!
				producing_frame := context.info.submitted_frame + 1
				app.core.service_stage_image_readback_for_gg(readback, snapshot.image.id, x, y,
					width, height, producing_frame) or {
					app.core.service_abandon_window_readback_for_gg(readback, err.msg())!
					return window_readback_id_from_core(readback)
				}
				context.submit_appkit_image_readback_pass(snapshot.image, producing_frame) or {
					app.core.service_abandon_window_readback_for_gg(readback, err.msg())!
					return window_readback_id_from_core(readback)
				}
				return window_readback_id_from_core(readback)
			}
		}
	}
	$if linux {
		$if x_multiwindow_x11 ? || sokol_wayland ? {
			if gfx.query_backend() !in [.glcore33, .gles3] {
				return error(err_multiwindow_render_readback_unsupported)
			}
			if context.info.submitted_frame == u64(0xffffffffffffffff) {
				return error(err_multiwindow_render_readback_unsupported)
			}
			mut app := context.app
			readback := app.core.service_begin_window_readback_with_payload_for_gg(context.info.window.core,
				width, height)!
			mut pixels := []u8{len: width * height * 4}
			status := C.v_gg_multiwindow_gl_readback_image_rgba8(snapshot.image.id,
				snapshot.desc.height, x, y, width, height, pixels.data, usize(pixels.len))
			if status != 0 {
				app.core.service_rollback_window_readback_for_gg(readback)
				return error(err_multiwindow_render_readback_unsupported)
			}
			app.pending_image_readbacks << MultiWindowPendingImageReadback{
				id:                     readback
				window:                 context.info.window
				image:                  id
				batch_epoch:            app.active_batch_epoch
				target_submitted_frame: context.info.submitted_frame + 1
				width:                  width
				height:                 height
				stride:                 width * 4
				pixels:                 pixels
			}
			return window_readback_id_from_core(readback)
		} $else {
			return error(err_multiwindow_render_readback_unsupported)
		}
	} $else {
		return error(err_multiwindow_render_readback_unsupported)
	}
}

fn (mut context WindowContext) submit_appkit_image_readback_pass(native_image gfx.Image, producing_frame u64) ! {
	mut app := context.app
	app.note_managed_gpu_work(app.active_batch_epoch)!
	mut attachment_desc := gfx.AttachmentsDesc{}
	attachment_desc.colors[0].image = native_image
	attachments := gfx.make_attachments(&attachment_desc)
	if gfx.query_attachments_state(attachments) != .valid {
		gfx.destroy_attachments(attachments)
		return error(err_multiwindow_render_resource_failed)
	}
	target_key := app.render_runtime.target_key(context.info.window, context.lease_epoch) or {
		gfx.destroy_attachments(attachments)
		return err
	}
	pass_epoch := app.render_runtime.begin_pass(context.info.window, context.lease_epoch, false,
		false, target_key) or {
		gfx.destroy_attachments(attachments)
		return err
	}
	mut errors := []IError{}
	app.arm_appkit_image_readbacks_for_pass(context.info.window, native_image.id, pass_epoch,
		producing_frame) or { errors << err }
	if errors.len == 0 {
		gfx.begin_pass(&gfx.Pass{
			action:      load_pass
			attachments: attachments
		})
		if app.core.renderer_device_available_for_gg() {
			gfx.end_pass()
		} else {
			errors << error(err_multiwindow_render_backend_unavailable)
		}
	}
	app.finish_render_epilogue(MultiWindowRenderEpilogue{
		window:      context.info.window
		lease_epoch: context.lease_epoch
		pass_epoch:  pass_epoch
	}, errors) or {
		if app.core.renderer_device_available_for_gg() {
			gfx.destroy_attachments(attachments)
		}
		return err
	}
	if app.core.renderer_device_available_for_gg() {
		gfx.destroy_attachments(attachments)
	}
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
	mut app := context.app
	app.with_native_window(context.info.window, f)!
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
