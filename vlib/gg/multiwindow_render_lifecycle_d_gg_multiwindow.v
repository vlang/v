module gg

import sokol.gfx
import sokol.sgl
import time
import x.multiwindow

fn (mut app App) run_managed(config RunConfig) ! {
	app.ensure_initialized()!
	app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	if app.core.status() == .stopped {
		if app.terminal_error != '' {
			return error(app.terminal_error)
		}
		return error(err_multiwindow_render_stopped)
	}
	has_app_frame := config.frame_fn != unsafe { nil }
	has_events := config.event_fn != unsafe { nil }
	has_input := config.input_fn != unsafe { nil }
	has_window_frames := app.refresh_has_window_frames(has_app_frame)!
	has_window_initializers := app.render_runtime.has_pending_window_initializers()
	has_app_resources := config.app_resource_init_fn != unsafe { nil }
		|| config.app_resource_frame_fn != unsafe { nil }
		|| config.app_resource_cleanup_fn != unsafe { nil }
	if !has_app_frame && !has_window_frames && !has_window_initializers && !has_events && !has_input
		&& !has_app_resources {
		return error(err_multiwindow_nil_run_fn)
	}
	if (has_app_frame || has_window_frames || has_window_initializers || has_app_resources)
		&& !app.capabilities().explicit_swapchain {
		return error(err_multiwindow_renderer_unsupported)
	}

	// configure_run accepts the run and establishes cleanup obligations. No
	// error after this point bypasses the single terminalization path.
	app.render_runtime.configure_run(config)!
	app.legacy_render_mode = has_app_frame
	mut errors := []string{}
	app.run_accepted_managed(config, has_app_frame, has_window_frames, has_window_initializers,
		has_events, has_input, has_app_resources) or { errors << err.msg() }
	app.stop_now() or { errors << err.msg() }
	app.legacy_render_mode = false
	if app.terminal_error != '' {
		errors << app.terminal_error
	}
	if errors.len > 0 {
		app.terminal_error = '${err_multiwindow_render_callback_failed}: ${unique_managed_errors(errors).join('; ')}'
		return error(app.terminal_error)
	}
}

fn (app &App) refresh_has_window_frames(has_app_frame bool) !bool {
	has_window_frames := app.render_runtime.has_per_window_frame_callbacks()
	if has_app_frame && has_window_frames {
		return error(err_multiwindow_render_mixed_callbacks)
	}
	return has_window_frames
}

fn (mut app App) run_accepted_managed(config RunConfig, has_app_frame bool, initial_has_window_frames bool, initial_has_window_initializers bool, has_events bool, has_input bool, has_app_resources bool) ! {
	mut has_window_frames := initial_has_window_frames
	mut has_window_initializers := initial_has_window_initializers
	mut render_initialized := false
	mut logical_only_batch_done := false
	for app.core.status() == .running {
		polled_events := app.poll_events()!
		mut dispatched := app.dispatch_managed_events(config.event_fn, config.input_fn)!
		if app.core.status() != .running {
			break
		}
		may_progress := app.core.post_dispatch_error_gate_for_gg()!
		if !may_progress {
			time.sleep(multiwindow_event_idle_sleep)
			continue
		}
		has_window_frames = app.refresh_has_window_frames(has_app_frame)!
		has_window_initializers = app.render_runtime.has_pending_window_initializers()
		if (has_app_frame || has_window_frames || has_window_initializers || has_app_resources)
			&& !render_initialized {
			app.ensure_render_initialized()!
			if has_app_frame {
				for id in app.window_ids()! {
					app.core.set_render_workload(id.core, true)!
				}
			}
			render_initialized = true
		}
		mut drained_jobs := 0
		if config.max_pending_jobs > 0 {
			drained_jobs = app.core.drain_pending(config.max_pending_jobs) or {
				if app.core.status() != .running {
					break
				}
				return err
			}
		}
		if app.core.status() != .running {
			break
		}
		dispatched += app.dispatch_managed_events(config.event_fn, config.input_fn)!
		if app.core.status() != .running {
			break
		}
		may_render := app.core.post_dispatch_error_gate_for_gg()!
		if !may_render {
			time.sleep(multiwindow_event_idle_sleep)
			continue
		}
		has_window_frames = app.refresh_has_window_frames(has_app_frame)!
		has_window_initializers = app.render_runtime.has_pending_window_initializers()
		if (has_app_frame || has_window_frames || has_window_initializers || has_app_resources)
			&& !render_initialized {
			app.ensure_render_initialized()!
			if has_app_frame {
				for id in app.window_ids()! {
					app.core.set_render_workload(id.core, true)!
				}
			}
			render_initialized = true
		}
		mut rendered := 0
		has_window_frames = app.refresh_has_window_frames(has_app_frame)!
		has_window_initializers = app.render_runtime.has_pending_window_initializers()
		should_run_batch := has_app_frame || has_window_frames
			|| has_window_initializers
			|| config.app_resource_frame_fn != unsafe { nil }
			|| (!logical_only_batch_done && has_app_resources)
		if should_run_batch {
			rendered = app.run_render_batch(has_app_frame, config.frame_fn)!
			logical_only_batch_done = true
		}
		app.flush_deferred_transitions()!
		if app.core.status() != .running {
			break
		}
		has_window_frames = app.refresh_has_window_frames(has_app_frame)!
		if logical_only_batch_done && !has_app_frame && !has_window_frames && !has_events
			&& !has_input && config.app_resource_frame_fn == unsafe { nil }
			&& app.window_ids()!.len == 0 {
			app.stop()!
			break
		}
		if rendered == 0 && polled_events == 0 && drained_jobs == 0 && dispatched == 0 {
			time.sleep(multiwindow_event_idle_sleep)
		}
	}
}

fn (mut app App) dispatch_managed_events(event_fn AppEventFn, input_fn AppInputFn) !int {
	app_ptr := unsafe { voidptr(&app) }
	callback := fn [app_ptr, event_fn, input_fn] (event multiwindow.QueuedEvent) !bool {
		mut facade := unsafe { &App(app_ptr) }
		facade.dispatch_one_managed_event(event, event_fn, input_fn)!
		return !facade.render_runtime.has_deferred_transitions()
	}
	mut dispatched := 0
	for app.core.status() == .running {
		delivered, yielded := app.core.dispatch_events_for_gg(callback) or {
			mut errors := [err.msg()]
			app.flush_deferred_transitions() or { errors << err.msg() }
			return error('${err_multiwindow_render_callback_failed}: ${unique_managed_errors(errors).join('; ')}')
		}
		dispatched += delivered
		if !yielded {
			return dispatched
		}
		app.flush_deferred_transitions()!
	}
	return dispatched
}

fn (mut app App) dispatch_one_managed_event(event multiwindow.QueuedEvent, event_fn AppEventFn, input_fn AppInputFn) ! {
	match event.kind {
		.lifecycle {
			window_event := window_event_from_core(event.lifecycle)
			if event_fn != unsafe { nil } {
				app.invoke_event_callback(window_event, event_fn)!
			} else {
				app.render_runtime.begin_user_callback()
				mut callback_error := IError(none)
				app.dispatch_lifecycle_without_event_callback(window_event) or {
					callback_error = err
				}
				app.render_runtime.end_user_callback()
				if callback_error !is none {
					return callback_error
				}
			}
		}
		.input {
			if input_fn != unsafe { nil } {
				app.render_runtime.begin_user_callback()
				mut callback_error := IError(none)
				input_fn(window_input_event_from_core(event.input), mut app) or {
					callback_error = err
				}
				app.render_runtime.end_user_callback()
				if callback_error !is none {
					return callback_error
				}
			}
		}
	}
}

fn (mut app App) invoke_event_callback(event WindowEvent, callback AppEventFn) ! {
	app.render_runtime.begin_user_callback()
	mut callback_error := IError(none)
	callback(event, mut app) or { callback_error = err }
	app.render_runtime.end_user_callback()
	if callback_error !is none {
		return callback_error
	}
}

fn (mut app App) run_render_batch(legacy bool, frame_fn AppFrameFn) !int {
	app_ptr := unsafe { voidptr(&app) }
	callback := fn [app_ptr, legacy, frame_fn] (batch multiwindow.RenderBatchLease, candidates []multiwindow.RenderWindowSnapshot) ! {
		mut facade := unsafe { &App(app_ptr) }
		facade.execute_render_batch(batch, candidates, legacy, frame_fn)!
	}
	outcome := if legacy {
		app.core.with_legacy_render_batch(callback) or {
			app.latch_renderer_terminal_failure_if_unusable()
			app.render_runtime.abort_active_batch()
			app.clear_active_batch_state()
			return err
		}
	} else {
		app.core.with_scheduled_render_batch(callback) or {
			app.latch_renderer_terminal_failure_if_unusable()
			app.render_runtime.abort_active_batch()
			app.clear_active_batch_state()
			return err
		}
	}
	mut terminal_errors := []string{}
	sokol_available := app.core.renderer_device_available_for_gg()
	app.render_runtime.finish_batch(outcome.batch_epoch, outcome.committed, sokol_available) or {
		terminal_errors << err.msg()
	}
	if outcome.committed && sokol_available {
		app.flush_deferred_sgl_targets(outcome.batch_epoch, false)
	}
	app.clear_active_batch_state()
	if outcome.error != '' {
		terminal_errors << outcome.error
	}
	app.latch_renderer_terminal_failure_if_unusable()
	failed_init := app.failed_init_windows.clone()
	app.failed_init_windows.clear()
	for id in failed_init {
		if app.window_exists(id) {
			app.destroy_window_now(id, .init_failed) or { terminal_errors << err.msg() }
		}
	}
	app.flush_deferred_transitions() or { terminal_errors << err.msg() }
	if terminal_errors.len > 0 {
		return error('${err_multiwindow_render_callback_failed}: ${terminal_errors.join('; ')}')
	}
	return outcome.finalized_submissions
}

fn (mut app App) execute_render_batch(batch multiwindow.RenderBatchLease, candidates []multiwindow.RenderWindowSnapshot, legacy bool, frame_fn AppFrameFn) ! {
	app.render_runtime.begin_batch(candidates_batch_epoch(batch, candidates))!
	app.active_batch_epoch = candidates_batch_epoch(batch, candidates)
	app.active_batch_lease = batch
	app.active_render_snapshots.clear()
	app.active_drawn_windows.clear()
	mut errors := []IError{}
	app.ensure_app_resources_initialized() or { errors << err }
	if errors.len == 0 {
		app.invoke_app_resource_frame() or { errors << err }
	}
	if legacy && errors.len == 0 {
		app.refresh_has_window_frames(true) or { errors << err }
	}
	if legacy && errors.len == 0 {
		for candidate in candidates {
			app.active_render_snapshots[window_id_from_core(candidate.window).str()] = candidate
		}
		app.app_frame_active = true
		app.render_runtime.begin_user_callback()
		frame_fn(mut app) or { errors << err }
		app.render_runtime.end_user_callback()
		app.app_frame_active = false
	} else if !legacy && errors.len == 0 {
		for candidate in candidates {
			id := window_id_from_core(candidate.window)
			_, frame_callback := app.render_runtime.window_callbacks(id) or {
				errors << err
				break
			}
			app.ensure_window_initialized(id, candidate) or {
				errors << err
				app.failed_init_windows << id
				break
			}
			if frame_callback == unsafe { nil } {
				app.core.set_render_workload(id.core, false) or {
					errors << err
					break
				}
				continue
			}
			app.render_runtime.prepare_target_snapshot(id, candidate) or {
				errors << err
				break
			}
			target := app.render_runtime.target_info(id) or {
				errors << err
				break
			}
			target_key := app.render_runtime.target_key_for_window_without_lease(id) or {
				errors << err
				break
			}
			app.ensure_window_sgl_context(id, target_key, target, app.active_batch_epoch) or {
				errors << err
				break
			}
			// Reserve the target lease now; native acquisition remains deferred to
			// the first real swapchain pass after CPU-side callback work.
			acquisition := app.core.acquire_render_target(batch, candidate.window) or {
				errors << err
				break
			}
			if acquisition.status == .transient_unavailable {
				continue
			}
			app.render_runtime.attach_render_target(id, acquisition) or {
				errors << err
				break
			}
			app.invoke_window_frame(id, acquisition.snapshot, frame_callback) or {
				errors << err
				break
			}
		}
	}
	if errors.len > 0 {
		return aggregate_managed_ierror(err_multiwindow_render_callback_failed, errors)
	}
}

fn (mut app App) ensure_app_resources_initialized() ! {
	started, completed, terminal := app.render_runtime.app_initialization_state()
	if completed {
		return
	}
	if started {
		if terminal == '' {
			return error(err_multiwindow_render_app_init_failed)
		}
		return error(terminal)
	}
	init_fn, _, _ := app.render_runtime.app_callbacks()
	if init_fn == unsafe { nil } {
		app.render_runtime.mark_app_initialized_without_callback()
		return
	}
	mut context := app.render_runtime.begin_app_resource_lease(app, .init)!
	app.render_runtime.begin_user_callback()
	mut callback_error := IError(none)
	init_fn(mut context) or { callback_error = err }
	app.render_runtime.end_user_callback()
	app.render_runtime.finish_app_resource_lease(context.lease_epoch, callback_error is none,
		managed_callback_error_message(callback_error))!
	if callback_error !is none {
		return callback_error
	}
}

fn (mut app App) invoke_app_resource_frame() ! {
	_, frame_fn, _ := app.render_runtime.app_callbacks()
	if frame_fn == unsafe { nil } {
		return
	}
	mut context := app.render_runtime.begin_app_resource_lease(app, .frame)!
	app.render_runtime.begin_user_callback()
	mut callback_error := IError(none)
	frame_fn(mut context) or { callback_error = err }
	app.render_runtime.end_user_callback()
	app.render_runtime.finish_app_resource_lease(context.lease_epoch, callback_error is none,
		managed_callback_error_message(callback_error))!
	if callback_error !is none {
		return callback_error
	}
}

fn (mut app App) ensure_window_initialized(id WindowId, candidate multiwindow.RenderWindowSnapshot) ! {
	if app.render_runtime.window_initialized(id)! {
		return
	}
	app.render_runtime.prepare_target_snapshot(id, candidate)!
	init_callback, _ := app.render_runtime.window_callbacks(id)!
	if init_callback == unsafe { nil } {
		app.render_runtime.mark_initialized_without_callback(id)!
		return
	}
	info := app.frame_info_from_snapshot(id, candidate, 0)
	mut context := app.render_runtime.begin_init_lease(app, info)!
	app.render_runtime.begin_user_callback()
	mut callback_error := IError(none)
	init_callback(mut context) or { callback_error = err }
	app.render_runtime.end_user_callback()
	app.render_runtime.finish_init_lease(id, context.lease_epoch, callback_error is none)!
	if callback_error !is none {
		return callback_error
	}
}

fn (mut app App) invoke_window_frame(id WindowId, snapshot multiwindow.RenderWindowSnapshot, callback WindowFrameFn) ! {
	info := app.frame_info_from_snapshot(id, snapshot, snapshot.frame_serial)
	mut errors := []IError{}
	mut context := app.render_runtime.begin_frame_lease(app, info) or {
		errors << err
		WindowContext{}
	}
	if context.lease_epoch != 0 {
		app.render_runtime.begin_user_callback()
		callback(mut context) or { errors << err }
		app.render_runtime.end_user_callback()
		if errors.len == 0 {
			used := app.render_runtime.swapchain_pass_used(id, context.lease_epoch) or {
				errors << err
				false
			}
			if !used {
				action := app.render_runtime.clear_color(id) or {
					errors << err
					Color{}
				}
				if errors.len == 0 {
					context.with_swapchain_managed(clear_pass_action(action), fn (mut pass WindowPassContext) ! {
						_ = pass
					}) or { errors << err }
				}
			}
		}
	}
	app.finish_render_epilogue(MultiWindowRenderEpilogue{
		window:          id
		lease_epoch:     context.lease_epoch
		finish_frame:    context.lease_epoch != 0
		target_attached: true
	}, errors)!
}

fn (mut app App) draw_window_managed(id WindowId, draw WindowDrawFn) ! {
	if draw == unsafe { nil } {
		return error(err_multiwindow_nil_draw_fn)
	}
	app.ensure_initialized()!
	app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	if !app.app_frame_active || id.str() !in app.active_render_snapshots
		|| id.str() in app.active_drawn_windows {
		return error(err_multiwindow_render_frame_inactive)
	}
	candidate := app.active_render_snapshots[id.str()]
	app.ensure_window_initialized(id, candidate)!
	app.render_runtime.prepare_target_snapshot(id, candidate)!
	target := app.render_runtime.target_info(id)!
	target_key := app.render_runtime.target_key_for_window_without_lease(id)!
	sgl_context := app.ensure_window_sgl_context(id, target_key, target, app.active_batch_epoch)!
	acquisition := app.core.acquire_render_target(app.active_batch_lease, id.core)!
	if acquisition.status == .transient_unavailable {
		return error(err_multiwindow_render_backend_unavailable)
	}
	app.render_runtime.attach_render_target(id, acquisition)!
	info := app.frame_info_from_snapshot(id, acquisition.snapshot,
		acquisition.snapshot.frame_serial)
	mut errors := []IError{}
	mut context := app.render_runtime.begin_frame_lease(app, info) or {
		errors << err
		WindowContext{}
	}
	mut pass_epoch := u64(0)
	if context.lease_epoch != 0 {
		pass_epoch = app.render_runtime.begin_pass(id, context.lease_epoch, true, true, target_key) or {
			errors << err
			u64(0)
		}
	}
	mut callback_error := IError(none)
	mut sgl_flushed := false
	if pass_epoch != 0 {
		clear_color := app.render_runtime.clear_color(id) or {
			errors << err
			Color{}
		}
		if errors.len == 0 {
			previous := sgl.get_context()
			sgl.set_context(sgl_context)
			sgl.defaults()
			sgl.matrix_mode_projection()
			sgl.ortho(0.0, f32(context.info.metrics.framebuffer_size.width),
				f32(context.info.metrics.framebuffer_size.height), 0.0, -1.0, 1.0)
			app.render_runtime.begin_user_callback()
			draw(mut context) or { callback_error = err }
			app.render_runtime.end_user_callback()
			sgl.set_context(previous)
			if callback_error is none {
				app.core.with_render_target_pass(acquisition.lease, clear_pass_action(clear_color), fn [sgl_context] () ! {
					previous_context := sgl.get_context()
					sgl.set_context(sgl_context)
					sgl.context_draw(sgl_context)
					sgl_error := sgl.context_error(sgl_context)
					sgl.set_context(previous_context)
					if sgl_error != .no_error {
						return error(err_multiwindow_render_resource_failed)
					}
				}) or { callback_error = err }
				if callback_error is none {
					sgl_flushed = true
				}
			}
			_ = target_key
		}
	}
	app.active_drawn_windows[id.str()] = true
	if callback_error !is none {
		errors << callback_error
	}
	app.finish_render_epilogue(MultiWindowRenderEpilogue{
		window:          id
		lease_epoch:     context.lease_epoch
		pass_epoch:      pass_epoch
		finish_frame:    context.lease_epoch != 0
		target_attached: true
		sgl_flushed:     sgl_flushed
	}, errors)!
}

fn (mut app App) destroy_window_managed(id WindowId, reason WindowCleanupReason) ! {
	app.ensure_initialized()!
	app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	app.core.ensure_event_admission_for_gg()!
	if app.render_runtime.defer_destroy_in_callback(id)! {
		return
	}
	app.destroy_window_now(id, reason)!
	app.flush_deferred_transitions()!
}

fn (mut app App) destroy_window_now(id WindowId, reason WindowCleanupReason) ! {
	if !app.window_exists(id) {
		return app.core.destroy_window(id.core)
	}
	ticket := app.core.prepare_window_destroy(id.core)!
	app.render_runtime.begin_destroy(id, reason) or {
		app.core.rollback_window_destroy(ticket) or {}
		return err
	}
	app.core.seal_window_destroy(ticket) or {
		app.render_runtime.rollback_destroy(id)
		app.core.rollback_window_destroy(ticket) or {}
		return err
	}
	app.finish_sealed_window_destroy(ticket, id, reason, none)!
}

fn (mut app App) finish_sealed_window_destroy(ticket multiwindow.WindowDestroyTicket, id WindowId, reason WindowCleanupReason, final_snapshot ?multiwindow.RenderWindowSnapshot) ! {
	mut errors := []string{}
	if app.gfx_started && app.core.renderer_is_usable() {
		app.run_window_teardown_batch(id, reason, final_snapshot) or {
			errors << err.msg()
			app.run_window_cleanup_terminal(id, reason, final_snapshot) or { errors << err.msg() }
		}
	} else {
		app.run_window_cleanup_terminal(id, reason, final_snapshot) or { errors << err.msg() }
	}
	app.core.finish_window_destroy(ticket, errors) or { errors << err.msg() }
	if errors.len > 0 {
		return error('${err_multiwindow_render_cleanup_failed}: ${unique_managed_errors(errors).join('; ')}')
	}
}

fn (mut app App) run_window_cleanup_terminal(id WindowId, reason WindowCleanupReason, final_snapshot ?multiwindow.RenderWindowSnapshot) ! {
	mut errors := []string{}
	info := app.cleanup_frame_info(id, final_snapshot)
	app.run_window_cleanup(id, reason, false, info) or { errors << err.msg() }
	app.render_runtime.finish_destroy_terminal(id)
	if errors.len > 0 {
		return error('${err_multiwindow_render_cleanup_failed}: ${unique_managed_errors(errors).join('; ')}')
	}
}

fn (mut app App) run_window_teardown_batch(id WindowId, reason WindowCleanupReason, final_snapshot ?multiwindow.RenderWindowSnapshot) ! {
	app_ptr := unsafe { voidptr(&app) }
	callback := fn [app_ptr, id, reason, final_snapshot] (batch multiwindow.RenderBatchLease, candidates []multiwindow.RenderWindowSnapshot) ! {
		_ = candidates
		mut facade := unsafe { &App(app_ptr) }
		facade.render_runtime.begin_teardown_batch(batch_epoch(batch))!
		facade.active_batch_epoch = batch_epoch(batch)
		facade.active_batch_lease = batch
		mut errors := []string{}
		info := facade.cleanup_frame_info(id, final_snapshot)
		facade.run_window_cleanup(id, reason, true, info) or { errors << err.msg() }
		needs_boundary := facade.render_runtime.window_retirement_needs_boundary(id) or {
			errors << err.msg()
			false
		}
		if needs_boundary || facade.has_window_sgl_context(id) {
			facade.core.note_render_gpu_work(batch) or { errors << err.msg() }
		}
		facade.render_runtime.finish_destroy(id, batch_epoch(batch))
		if errors.len > 0 {
			return error('${err_multiwindow_render_cleanup_failed}: ${errors.join('; ')}')
		}
	}
	mut outcome := multiwindow.RenderBatchOutcome{}
	if app.core.status() == .running && !app.render_runtime.stopping {
		outcome = app.core.with_scheduled_render_batch(callback) or {
			app.render_runtime.abort_active_batch()
			app.clear_active_batch_state()
			return err
		}
	} else {
		outcome = app.core.with_teardown_render_batch(callback) or {
			app.render_runtime.abort_active_batch()
			app.clear_active_batch_state()
			return err
		}
	}
	mut errors := []string{}
	sokol_available := app.core.renderer_device_available_for_gg()
	app.render_runtime.finish_batch(outcome.batch_epoch, outcome.committed, sokol_available) or {
		errors << err.msg()
	}
	if outcome.committed && sokol_available {
		app.flush_deferred_sgl_targets(outcome.batch_epoch, false)
		app.discard_window_sgl_context(id)
	}
	app.clear_active_batch_state()
	if outcome.error != '' {
		errors << outcome.error
	}
	if errors.len > 0 {
		return error('${err_multiwindow_render_cleanup_failed}: ${unique_managed_errors(errors).join('; ')}')
	}
}

fn (mut app App) run_window_cleanup(id WindowId, reason WindowCleanupReason, graphics_available bool, info WindowFrameInfo) ! {
	plan := app.render_runtime.prepare_cleanup(app, id, info, reason, graphics_available) or {
		return error(err.msg())
	}
	if !plan.run {
		return
	}
	mut context := plan.context
	app.render_runtime.begin_user_callback()
	mut callback_error := IError(none)
	plan.callback(mut context) or { callback_error = err }
	app.render_runtime.end_user_callback()
	mut errors := []string{}
	app.render_runtime.finish_cleanup(id, context.lease_epoch) or { errors << err.msg() }
	if callback_error !is none {
		errors << callback_error.msg()
	}
	if errors.len > 0 {
		return error('${err_multiwindow_render_cleanup_failed}: ${errors.join('; ')}')
	}
}

fn (mut app App) consume_backend_teardowns() ! {
	notices := app.core.drain_render_teardown_notices()!
	mut errors := []string{}
	for notice in notices {
		id := window_id_from_core(notice.window)
		app.render_runtime.begin_destroy(id, .native_closed) or { errors << err.msg() }
		app.finish_sealed_window_destroy(notice.ticket, id, .native_closed, notice.snapshot) or {
			errors << err.msg()
		}
	}
	if errors.len > 0 {
		app.core.defer_dispatch_error_for_gg('${err_multiwindow_render_cleanup_failed}: ${errors.join('; ')}')!
	}
}

fn (mut app App) stop_managed() ! {
	app.ensure_initialized()!
	app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	if app.core.status() == .stopped {
		if app.terminal_error != '' {
			return error(app.terminal_error)
		}
		return
	}
	if app.terminal_error != '' {
		return error(app.terminal_error)
	}
	if app.render_runtime.defer_stop_in_callback() {
		return
	}
	app.stop_now()!
}

fn (mut app App) stop_now() ! {
	if app.core.status() == .stopped {
		if app.terminal_error != '' {
			return error(app.terminal_error)
		}
		return
	}
	if app.terminal_error != '' {
		return error(app.terminal_error)
	}
	renderer_unusable := app.gfx_started && !app.core.renderer_is_usable()
	if renderer_unusable {
		app.latch_renderer_terminal_failure()
	}
	stop_cleanup_reason := if app.renderer_terminal_failure || renderer_unusable {
		WindowCleanupReason.renderer_lost
	} else {
		WindowCleanupReason.app_stop
	}
	mut errors := []string{}
	mut stop_prepared := false
	stop_ticket := app.core.prepare_stop() or {
		errors << err.msg()
		multiwindow.AppStopTicket{}
	}
	if errors.len == 0 {
		stop_prepared = true
	}
	app.render_runtime.begin_stop()
	app.consume_backend_teardowns() or { errors << err.msg() }
	ids := app.core.window_ids() or {
		errors << err.msg()
		[]multiwindow.WindowId{}
	}
	for core_id in ids {
		id := window_id_from_core(core_id)
		mut ticket := multiwindow.WindowDestroyTicket{}
		mut prepared := false
		for _ in 0 .. 2 {
			ticket = app.core.prepare_window_destroy_for_stop(core_id) or {
				errors << err.msg()
				continue
			}
			prepared = true
			break
		}
		mut sealed := false
		if prepared {
			for attempt in 0 .. 2 {
				app.core.seal_window_destroy(ticket) or {
					errors << err.msg()
					app.core.rollback_window_destroy(ticket) or { errors << err.msg() }
					if attempt == 1 {
						break
					}
					ticket = app.core.prepare_window_destroy_for_stop(core_id) or {
						errors << err.msg()
						break
					}
					continue
				}
				sealed = true
				break
			}
		}
		if !sealed {
			ticket = app.core.seal_window_destroy_terminal_for_stop(core_id) or {
				errors << err.msg()
				app.run_window_cleanup_terminal(id, stop_cleanup_reason, none) or {
					errors << err.msg()
				}
				continue
			}
			sealed = true
		}
		app.render_runtime.begin_destroy(id, stop_cleanup_reason) or { errors << err.msg() }
		app.finish_sealed_window_destroy(ticket, id, stop_cleanup_reason, none) or {
			errors << err.msg()
		}
	}
	app.run_app_resource_cleanup_batch() or { errors << err.msg() }
	app.record_multiwindow_lifecycle_milestone(.runtime_finish_stop_enter)
	app.render_runtime.finish_stop()
	app.record_multiwindow_lifecycle_milestone(.runtime_finish_stop_complete)
	app.shutdown_renderer() or { errors << err.msg() }
	if stop_prepared {
		app.record_multiwindow_lifecycle_milestone(.core_finish_stop_enter)
		mut core_finish_failed := false
		app.core.finish_stop(stop_ticket, errors) or {
			core_finish_failed = true
			errors << err.msg()
		}
		app.record_multiwindow_lifecycle_milestone(if core_finish_failed {
			.core_finish_stop_failed
		} else {
			.core_finish_stop_complete
		})
	} else {
		app.record_multiwindow_lifecycle_milestone(.core_stop_enter)
		mut core_stop_failed := false
		app.core.stop() or {
			core_stop_failed = true
			errors << err.msg()
		}
		app.record_multiwindow_lifecycle_milestone(if core_stop_failed {
			.core_stop_failed
		} else {
			.core_stop_complete
		})
	}
	if errors.len > 0 {
		app.terminal_error = '${err_multiwindow_render_cleanup_failed}: ${unique_managed_errors(errors).join('; ')}'
		return error(app.terminal_error)
	}
}

fn (mut app App) run_app_resource_cleanup_batch() ! {
	app.record_multiwindow_lifecycle_milestone(.resource_cleanup_enter)
	if !app.gfx_started {
		app.run_app_resource_cleanup_terminal() or {
			app.record_multiwindow_lifecycle_milestone(.resource_cleanup_failed)
			return err
		}
		app.record_multiwindow_lifecycle_milestone(.resource_cleanup_terminal_complete)
		return
	}
	if !app.core.renderer_is_usable() {
		app.run_app_resource_cleanup_terminal() or {
			app.record_multiwindow_lifecycle_milestone(.resource_cleanup_failed)
			return err
		}
		app.record_multiwindow_lifecycle_milestone(.resource_cleanup_terminal_complete)
		return
	}
	app_ptr := unsafe { voidptr(&app) }
	callback := fn [app_ptr] (batch multiwindow.RenderBatchLease, candidates []multiwindow.RenderWindowSnapshot) ! {
		_ = candidates
		mut facade := unsafe { &App(app_ptr) }
		facade.render_runtime.begin_teardown_batch(batch_epoch(batch))!
		facade.active_batch_epoch = batch_epoch(batch)
		facade.active_batch_lease = batch
		mut errors := []string{}
		facade.ensure_app_resources_initialized() or { errors << err.msg() }
		_, _, cleanup_fn := facade.render_runtime.app_callbacks()
		if cleanup_fn != unsafe { nil } {
			mut context := facade.render_runtime.begin_app_resource_lease(facade, .cleanup) or {
				errors << err.msg()
				AppResourceContext{}
			}
			if context.lease_epoch != 0 {
				facade.render_runtime.begin_user_callback()
				mut callback_error := IError(none)
				cleanup_fn(mut context) or { callback_error = err }
				facade.render_runtime.end_user_callback()
				facade.render_runtime.finish_app_resource_lease(context.lease_epoch,
					callback_error is none, managed_callback_error_message(callback_error)) or {
					errors << err.msg()
				}
				if callback_error !is none {
					errors << callback_error.msg()
				}
			}
		}
		needs_boundary := facade.render_runtime.app_retirement_needs_boundary()
		if needs_boundary {
			facade.core.note_render_gpu_work(batch) or { errors << err.msg() }
		}
		facade.render_runtime.retire_app_resources_at_batch(batch_epoch(batch))
		if errors.len > 0 {
			return error('${err_multiwindow_render_cleanup_failed}: ${errors.join('; ')}')
		}
	}
	outcome := app.core.with_teardown_render_batch(callback) or {
		mut errors := []string{}
		errors << err.msg()
		app.render_runtime.abort_active_batch()
		app.clear_active_batch_state()
		app.run_app_resource_cleanup_terminal() or { errors << err.msg() }
		app.record_multiwindow_lifecycle_milestone(.resource_cleanup_failed)
		return error('${err_multiwindow_render_cleanup_failed}: ${unique_managed_errors(errors).join('; ')}')
	}
	mut errors := []string{}
	sokol_available := app.core.renderer_device_available_for_gg()
	app.render_runtime.finish_batch(outcome.batch_epoch, outcome.committed, sokol_available) or {
		errors << err.msg()
	}
	if outcome.committed && sokol_available {
		app.flush_deferred_sgl_targets(outcome.batch_epoch, false)
	}
	app.clear_active_batch_state()
	if outcome.error != '' {
		errors << outcome.error
	}
	if errors.len > 0 {
		app.run_app_resource_cleanup_terminal() or { errors << err.msg() }
		app.record_multiwindow_lifecycle_milestone(.resource_cleanup_failed)
		return error('${err_multiwindow_render_cleanup_failed}: ${unique_managed_errors(errors).join('; ')}')
	}
	app.record_multiwindow_lifecycle_milestone(.resource_cleanup_batch_complete)
}

fn (mut app App) run_app_resource_cleanup_terminal() ! {
	mut errors := []string{}
	started, completed, init_terminal := app.render_runtime.app_initialization_state()
	if !started {
		app.render_runtime.mark_app_initialized_without_callback()
	}
	_, _, cleanup_fn := app.render_runtime.app_callbacks()
	if started && !completed && init_terminal != '' {
		errors << init_terminal
	}
	cleanup_started, _ := app.render_runtime.app_cleanup_state()
	if cleanup_fn != unsafe { nil } && !cleanup_started {
		mut context := app.render_runtime.begin_app_resource_lease(app, .cleanup) or {
			errors << err.msg()
			AppResourceContext{}
		}
		if context.lease_epoch != 0 {
			app.render_runtime.begin_user_callback()
			mut callback_error := IError(none)
			cleanup_fn(mut context) or { callback_error = err }
			app.render_runtime.end_user_callback()
			app.render_runtime.finish_app_resource_lease(context.lease_epoch,
				callback_error is none, managed_callback_error_message(callback_error)) or {
				errors << err.msg()
			}
			if callback_error !is none {
				errors << callback_error.msg()
			}
		}
	}
	app.render_runtime.retire_app_resources_terminal()
	if errors.len > 0 {
		return error('${err_multiwindow_render_cleanup_failed}: ${errors.join('; ')}')
	}
}

fn (mut app App) flush_deferred_transitions() ! {
	transitions := app.render_runtime.take_deferred_transitions()
	app.apply_deferred_transitions(transitions)!
}

fn (mut app App) apply_deferred_transitions(transitions MultiWindowDeferredTransitions) ! {
	if transitions.stop {
		app.stop_now()!
		return
	}
	for id in transitions.windows {
		if app.window_exists(id) {
			app.destroy_window_now(id, .requested)!
		}
	}
}

fn (app &App) frame_info_from_snapshot(id WindowId, snapshot multiwindow.RenderWindowSnapshot, frame_serial u64) WindowFrameInfo {
	return WindowFrameInfo{
		window:          id
		frame_serial:    frame_serial
		submitted_frame: snapshot.submitted_frame
		metrics:         window_metrics_from_core(snapshot)
		target:          WindowRenderTargetInfo{
			color_format: unsafe { gfx.PixelFormat(snapshot.target.color_format) }
			depth_format: unsafe { gfx.PixelFormat(snapshot.target.depth_format) }
			sample_count: snapshot.target.sample_count
		}
	}
}

fn (app &App) cleanup_frame_info(id WindowId, final_snapshot ?multiwindow.RenderWindowSnapshot) WindowFrameInfo {
	if snapshot := final_snapshot {
		return app.frame_info_from_snapshot(id, snapshot, snapshot.frame_serial)
	}
	snapshot := app.core.render_window_snapshot(id.core) or {
		multiwindow.RenderWindowSnapshot{
			window: id.core
		}
	}
	return app.frame_info_from_snapshot(id, snapshot, snapshot.frame_serial)
}

fn (mut app App) clear_active_batch_state() {
	app.active_render_snapshots.clear()
	app.active_drawn_windows.clear()
	app.active_batch_epoch = 0
	app.active_batch_lease = multiwindow.RenderBatchLease{}
	app.app_frame_active = false
}

fn candidates_batch_epoch(batch multiwindow.RenderBatchLease, candidates []multiwindow.RenderWindowSnapshot) u64 {
	if candidates.len > 0 {
		return candidates[0].batch_epoch
	}
	return batch_epoch(batch)
}

fn batch_epoch(batch multiwindow.RenderBatchLease) u64 {
	// The lease is opaque. Empty batches carry their epoch through a helper on
	// the core instead of exposing mutable scheduler authority.
	return batch.epoch_for_gg()
}

fn clear_pass_action(color Color) gfx.PassAction {
	return gfx.create_clear_pass_action(f32(color.r) / 255.0, f32(color.g) / 255.0,
		f32(color.b) / 255.0, f32(color.a) / 255.0)
}

fn managed_callback_error_message(callback_error IError) string {
	if callback_error is none {
		return ''
	}
	return callback_error.msg()
}
