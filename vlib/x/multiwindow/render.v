module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

pub struct RendererConfig {
pub:
	buffer_pool_size      int = 128
	image_pool_size       int = 1024
	sampler_pool_size     int = 128
	shader_pool_size      int = 128
	pipeline_pool_size    int = 256
	attachments_pool_size int = 256
}

pub struct RendererInfo {
pub:
	color_format int
	depth_format int
	sample_count int
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	enum BackendTargetStatus {
		prepared
		acquired
		pass_closed
		finalized
		released
	}

	struct BackendTargetSlot {
	mut:
		epoch               u64
		lease               RenderTargetLease
		frame               RenderFrame
		snapshot            RenderWindowSnapshot
		status              BackendTargetStatus
		recoverable_outcome NativeRenderResult
		recoverable_error   string
	}

	struct RecoverableRenderAttemptError {
		target  RenderTargetLease
		outcome NativeRenderResult
		message string
	}

	fn (err RecoverableRenderAttemptError) msg() string {
		return err.message
	}

	fn (err RecoverableRenderAttemptError) code() int {
		return 0
	}

	struct RecoverableRenderOperationError {
		message string
	}

	fn (err RecoverableRenderOperationError) msg() string {
		return err.message
	}

	fn (err RecoverableRenderOperationError) code() int {
		return 0
	}

	@[heap]
	struct RenderBackendState {
	mut:
		started                     bool
		anchor_created              bool
		batch_active                bool
		batch_epoch                 u64
		next_target_epoch           u64 = 1
		targets                     []BackendTargetSlot
		sokol_touched               bool
		completed_user_passes       int
		anchor_acquired             bool
		anchor_pass_closed          bool
		failure_messages            []string
		terminal                    bool
		device_lost                 bool
		native_health               NativeRendererHealth
		shutdown_path               RendererShutdownPath
		suppressed_callback_attempt RecoverableRenderAttemptError
		shutdown_prepared           bool
		shutdown_batch_scope        bool
		fault_trace_token           u64
	}

	pub fn (mut app App) start_renderer(config RendererConfig) !RendererInfo {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		app.ensure_running_locked() or {
			app.state_mutex.unlock()
			return err
		}
		if app.render_runtime.renderer_terminal != '' {
			terminal := app.render_runtime.renderer_terminal
			app.state_mutex.unlock()
			return error(terminal)
		}
		app.backend.preflight_renderer_start() or {
			app.state_mutex.unlock()
			return err
		}
		initial_health := app.backend.renderer_health()
		if initial_health.blocks_graphics() {
			app.state_mutex.unlock()
			terminal := native_renderer_health_error(initial_health)
			app.mark_renderer_terminal(terminal)
			return error(terminal)
		}
		for slot in app.windows {
			if slot.status == .alive && slot.config.sample_count != 1 {
				app.state_mutex.unlock()
				return error(err_render_sample_count_unsupported)
			}
		}
		if app.render_bridge != unsafe { nil } {
			state := unsafe { &RenderBackendState(app.render_bridge) }
			info := app.renderer_info_from_environment() or {
				app.state_mutex.unlock()
				return err
			}
			_ = state
			app.state_mutex.unlock()
			return info
		}
		app.state_mutex.unlock()
		validate_renderer_config(config)!

		trace_token := app.begin_renderer_fault_trace_attempt()!
		app.backend.advance_renderer_native_operations(app.instance_id, trace_token)!
		app.record_renderer_fault_milestone(trace_token, .anchor_probe)
		if message := app.take_internal_fault(.renderer_anchor_create) {
			return error(message)
		}
		app.backend.ensure_renderer_started() or {
			if app.backend.renderer_health().blocks_graphics() {
				app.mark_renderer_terminal(native_renderer_health_error(app.backend.renderer_health()))
				app.backend.abandon_renderer_ownership()
			}
			return err
		}
		started_health := app.backend.renderer_health()
		if started_health.blocks_graphics() {
			app.mark_renderer_terminal(native_renderer_health_error(started_health))
			app.backend.abandon_renderer_ownership()
			return error(native_renderer_health_error(started_health))
		}
		app.backend.create_renderer_anchor() or {
			if app.backend.renderer_health().blocks_graphics() {
				app.mark_renderer_terminal(native_renderer_health_error(app.backend.renderer_health()))
				app.backend.abandon_renderer_ownership()
			}
			return err
		}
		anchor_health := app.backend.renderer_health()
		if anchor_health.blocks_graphics() {
			app.mark_renderer_terminal(native_renderer_health_error(anchor_health))
			app.backend.abandon_renderer_ownership()
			return error(native_renderer_health_error(anchor_health))
		}
		app.record_renderer_fault_milestone(trace_token, .anchor_completed)
		app.record_renderer_fault_milestone(trace_token, .environment_probe)
		if message := app.take_internal_fault(.renderer_environment) {
			app.discard_renderer_anchor_after_start_failure(trace_token)
			return error(message)
		}
		environment := app.backend.renderer_environment() or {
			app.discard_renderer_anchor_after_start_failure(trace_token)
			return err
		}
		if app.backend.renderer_health().blocks_graphics() {
			health_error := native_renderer_health_error(app.backend.renderer_health())
			app.discard_renderer_anchor_after_start_failure(trace_token)
			return error(health_error)
		}
		app.record_renderer_fault_milestone(trace_token, .environment_acquired)
		mut desc := gfx.Desc{
			buffer_pool_size:      config.buffer_pool_size
			image_pool_size:       config.image_pool_size
			sampler_pool_size:     config.sampler_pool_size
			shader_pool_size:      config.shader_pool_size
			pipeline_pool_size:    config.pipeline_pool_size
			attachments_pool_size: config.attachments_pool_size
			environment:           environment
		}
		setup_health := app.backend.renderer_health()
		if setup_health.blocks_graphics() {
			app.mark_renderer_terminal(native_renderer_health_error(setup_health))
			app.backend.abandon_renderer_ownership()
			return error(native_renderer_health_error(setup_health))
		}
		app.record_renderer_fault_milestone(trace_token, .setup_probe)
		if message := app.take_internal_fault(.renderer_setup) {
			app.discard_renderer_anchor_after_start_failure(trace_token)
			return error(message)
		}
		gfx.setup(&desc)
		if app.backend.renderer_health().blocks_graphics() {
			health_error := native_renderer_health_error(app.backend.renderer_health())
			app.mark_renderer_terminal(health_error)
			app.backend.abandon_renderer_ownership()
			return error(health_error)
		}
		if !gfx.is_valid() {
			gfx.shutdown()
			app.record_renderer_fault_milestone(trace_token, .gfx_shutdown_complete)
			app.discard_renderer_anchor_after_start_failure(trace_token)
			return error(err_renderer_unsupported)
		}
		app.record_renderer_fault_milestone(trace_token, .gfx_complete)
		state := &RenderBackendState{
			started:           true
			anchor_created:    true
			native_health:     app.backend.renderer_health()
			fault_trace_token: trace_token
		}
		app.state_mutex.lock()
		app.render_bridge = unsafe { voidptr(state) }
		app.state_mutex.unlock()
		app.record_renderer_fault_milestone(trace_token, .bridge_published)
		return RendererInfo{
			color_format: int(environment.defaults.color_format)
			depth_format: int(environment.defaults.depth_format)
			sample_count: environment.defaults.sample_count
		}
	}

	fn (mut app App) destroy_renderer_anchor_with_fault_trace(trace_token u64) ! {
		app.backend.destroy_renderer_anchor()!
		app.record_renderer_fault_milestone(trace_token, .anchor_destroyed)
	}

	fn (mut app App) discard_renderer_anchor_after_start_failure(trace_token u64) {
		if app.backend.renderer_health().blocks_graphics() {
			app.mark_renderer_terminal(native_renderer_health_error(app.backend.renderer_health()))
			app.backend.abandon_renderer_ownership()
			return
		}
		app.destroy_renderer_anchor_with_fault_trace(trace_token) or {}
	}

	pub fn (mut app App) shutdown_renderer() ! {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		bridge := app.render_bridge
		if bridge == unsafe { nil } {
			app.state_mutex.unlock()
			return
		}
		mut state := unsafe { &RenderBackendState(bridge) }
		if state.batch_active {
			app.state_mutex.unlock()
			return error(err_render_batch_active)
		}
		app.render_bridge = unsafe { nil }
		app.state_mutex.unlock()
		mut errors := []string{}
		app.sync_renderer_health(mut state)
		if state.native_health.blocks_graphics() {
			state.device_lost = true
			state.shutdown_path = .logical_abandon
		}
		if state.started && state.shutdown_path != .logical_abandon && !state.shutdown_prepared
			&& gfx.is_valid() {
			app.backend.begin_render_batch(NativeOperationSeed{
				call_site: .shutdown_anchor
				scope:     .batch
			}) or {
				errors << err.msg()
				app.sync_renderer_health(mut state)
				state.shutdown_path = .logical_abandon
			}
			if state.shutdown_path != .logical_abandon {
				state.shutdown_batch_scope = true
				proof := app.backend.prove_renderer_shutdown_anchor()
				state.shutdown_path = proof.path
				app.sync_renderer_health(mut state)
				if proof.path == .orderly_anchor {
					state.shutdown_prepared = true
				} else {
					errors << proof.outcome.error_text
				}
			}
		}
		if state.started && state.shutdown_prepared && state.shutdown_path == .orderly_anchor
			&& !state.native_health.blocks_graphics() && gfx.is_valid() {
			gfx.shutdown()
			app.record_renderer_fault_milestone(state.fault_trace_token, .gfx_shutdown_complete)
		}
		if state.shutdown_batch_scope && !state.native_health.blocks_graphics()
			&& state.shutdown_path == .orderly_anchor {
			app.backend.end_render_batch(NativeOperationSeed{
				call_site: .shutdown_release
				scope:     .batch
			}) or { errors << err.msg() }
			app.sync_renderer_health(mut state)
		}
		state.shutdown_batch_scope = false
		state.started = false
		if state.anchor_created && state.shutdown_path == .orderly_anchor
			&& !state.native_health.blocks_graphics() {
			app.destroy_renderer_anchor_with_fault_trace(state.fault_trace_token) or {
				errors << err.msg()
			}
			app.sync_renderer_health(mut state)
			state.anchor_created = false
		} else if state.anchor_created {
			app.backend.abandon_renderer_ownership()
			state.anchor_created = false
		}
		terminal := aggregate_terminal_errors(err_render_terminal_aggregate, errors)
		if terminal != '' {
			return error(terminal)
		}
	}

	pub fn (app &App) renderer_is_usable() bool {
		if app.render_bridge == unsafe { nil } {
			return false
		}
		live_health := app.backend.renderer_health()
		if live_health.blocks_graphics() || !gfx.is_valid() {
			return false
		}
		state := unsafe { &RenderBackendState(app.render_bridge) }
		return state.started && !state.terminal && !state.native_health.blocks_graphics()
			&& !live_health.blocks_graphics()
	}

	// renderer_device_available_for_gg distinguishes a terminal scheduler from
	// genuine device/context loss without exposing graphics handles.
	pub fn (app &App) renderer_device_available_for_gg() bool {
		if app.render_bridge == unsafe { nil } {
			return false
		}
		live_health := app.backend.renderer_health()
		if live_health.blocks_graphics() || !gfx.is_valid() {
			return false
		}
		state := unsafe { &RenderBackendState(app.render_bridge) }
		return state.started && !state.device_lost && !state.native_health.blocks_graphics()
			&& !live_health.blocks_graphics()
	}

	// prepare_renderer_shutdown_for_gg binds the private anchor before the gg
	// facade tears SGL down. Failure poisons the renderer and forbids Sokol calls.
	pub fn (mut app App) prepare_renderer_shutdown_for_gg() ! {
		app.assert_owner_thread()!
		mut state := app.render_backend_state()!
		if state.batch_active {
			return error(err_render_batch_active)
		}
		app.sync_renderer_health(mut state)
		if state.native_health.blocks_graphics() {
			state.device_lost = true
			state.terminal = true
			state.shutdown_path = .logical_abandon
			return error(err_render_native_renderer_lost)
		}
		app.backend.begin_render_batch(NativeOperationSeed{
			call_site: .shutdown_anchor
			scope:     .batch
		}) or {
			app.sync_renderer_health(mut state)
			state.device_lost = true
			state.terminal = true
			state.shutdown_path = .logical_abandon
			app.mark_renderer_terminal(err.msg())
			return err
		}
		state.shutdown_batch_scope = true
		proof := app.backend.prove_renderer_shutdown_anchor()
		state.shutdown_path = proof.path
		app.sync_renderer_health(mut state)
		if proof.path != .orderly_anchor || state.native_health.blocks_graphics() {
			mut errors := []string{}
			if proof.outcome.error_text != '' {
				errors << proof.outcome.error_text
			}
			state.shutdown_batch_scope = false
			state.device_lost = true
			state.terminal = true
			terminal := aggregate_terminal_errors(err_render_terminal_aggregate, errors)
			app.mark_renderer_terminal(terminal)
			return error(terminal)
		}
		state.shutdown_prepared = true
	}

	// abandon_renderer_for_gg invalidates this renderer instance without
	// invoking Sokol after a confirmed device/context loss.
	pub fn (mut app App) abandon_renderer_for_gg() ! {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		bridge := app.render_bridge
		app.render_bridge = unsafe { nil }
		app.render_runtime.renderer_terminal = err_render_renderer_failed
		app.state_mutex.unlock()
		if bridge == unsafe { nil } {
			return
		}
		mut state := unsafe { &RenderBackendState(bridge) }
		state.started = false
		state.terminal = true
		state.device_lost = true
		state.native_health = .abandoned
		state.shutdown_path = .logical_abandon
		state.shutdown_batch_scope = false
		if state.anchor_created {
			app.backend.abandon_renderer_ownership()
			state.anchor_created = false
		}
	}

	fn (mut app App) shutdown_render_bridge_for_stop() ! {
		app.shutdown_renderer()!
	}

	fn (app &App) renderer_info_from_environment() !RendererInfo {
		if app.backend.renderer_health().blocks_graphics() {
			return error(err_render_native_renderer_lost)
		}
		if !gfx.is_valid() {
			return error(err_renderer_unsupported)
		}
		desc := gfx.query_desc()
		return RendererInfo{
			color_format: int(desc.environment.defaults.color_format)
			depth_format: int(desc.environment.defaults.depth_format)
			sample_count: desc.environment.defaults.sample_count
		}
	}

	fn (mut app App) begin_backend_render_batch(lease RenderBatchLease) ! {
		mut state := app.render_backend_state()!
		app.sync_renderer_health(mut state)
		if state.terminal {
			return error(err_render_renderer_failed)
		}
		if state.native_health.blocks_graphics() {
			state.device_lost = true
			state.terminal = true
			return error(err_render_native_renderer_lost)
		}
		if state.batch_active {
			return error(err_render_batch_active)
		}
		app.backend.begin_render_batch(NativeOperationSeed{
			presence_mask: native_context_has_batch_epoch
			call_site:     .anchor_prepare
			scope:         .batch
			batch_epoch:   lease.epoch
		}) or {
			app.sync_renderer_health(mut state)
			if state.native_health.blocks_graphics() {
				state.device_lost = true
				state.terminal = true
				state.shutdown_path = .logical_abandon
				app.mark_renderer_terminal(native_renderer_health_error(state.native_health))
			}
			return err
		}
		app.sync_renderer_health(mut state)
		if state.native_health.blocks_graphics() {
			state.device_lost = true
			state.terminal = true
			state.shutdown_path = .logical_abandon
			terminal := native_renderer_health_error(state.native_health)
			app.mark_renderer_terminal(terminal)
			return error(terminal)
		}
		app.backend.prepare_renderer_anchor_context() or {
			mut errors := []string{}
			errors << err.msg()
			app.sync_renderer_health(mut state)
			if !state.native_health.blocks_graphics() {
				app.backend.end_render_batch(NativeOperationSeed{
					presence_mask: native_context_has_batch_epoch
					call_site:     .window_finalize
					scope:         .batch
					batch_epoch:   lease.epoch
				}) or { errors << err.msg() }
				app.sync_renderer_health(mut state)
			}
			if state.native_health.blocks_graphics() {
				state.terminal = true
				state.shutdown_path = .logical_abandon
			}
			state.device_lost = state.native_health.blocks_graphics()
			terminal := aggregate_terminal_errors(err_render_terminal_aggregate, errors)
			if state.terminal {
				app.mark_renderer_terminal(terminal)
			}
			return error(terminal)
		}
		state.batch_active = true
		state.batch_epoch = lease.epoch
		state.targets.clear()
		state.sokol_touched = false
		state.completed_user_passes = 0
		state.anchor_acquired = false
		state.anchor_pass_closed = false
		state.suppressed_callback_attempt = RecoverableRenderAttemptError{}
		state.failure_messages.clear()
	}

	pub fn (mut app App) acquire_render_target(batch RenderBatchLease, id WindowId) !RenderTargetAcquisition {
		app.assert_owner_thread()!
		mut state := app.validate_backend_batch(batch)!
		app.state_mutex.lock()
		claim := app.plan_render_target_claim_locked(id, batch, state.next_target_epoch) or {
			app.state_mutex.unlock()
			return err
		}
		if message := app.take_internal_fault(.renderer_target_acquire) {
			app.state_mutex.unlock()
			return RecoverableRenderOperationError{
				message: message
			}
		}
		app.commit_render_target_claim_locked(claim)
		state.next_target_epoch = claim.next_target_epoch
		app.state_mutex.unlock()
		index := claim.index
		candidate := claim.candidate
		target_epoch := claim.target_epoch
		window_epoch := claim.window_epoch
		native_attempt := NativeTargetAttempt{
			batch_epoch:        batch.epoch
			window_lease_epoch: window_epoch
			target_lease_epoch: target_epoch
		}
		attempt := app.backend.begin_render(id, candidate, native_attempt)
		if !attempt.outcome.succeeded() {
			app.apply_native_render_outcome(mut state, id, attempt.outcome)
			reason := render_acquire_block_reason(attempt.outcome)
			app.cancel_render_target_claim(index, batch, reason)
			if attempt.outcome.is_recoverable_target()
				|| attempt.outcome.disposition == .native_window_lost || reason == .not_configured
				|| reason == .resize_pending || reason == .zero_sized {
				return RenderTargetAcquisition{
					status:       .transient_unavailable
					block_reason: reason
					snapshot:     candidate
				}
			}
			app.record_backend_render_batch_failure(batch, attempt.outcome.error_text)
			return native_render_error(attempt.outcome)
		}
		frame := attempt.frame
		app.sync_renderer_health(mut state)
		if state.native_health.blocks_graphics() {
			state.device_lost = true
			state.terminal = true
			state.shutdown_path = .logical_abandon
			app.cancel_render_target_claim(index, batch, .renderer_failed)
			message := native_renderer_health_error(state.native_health)
			app.record_backend_render_batch_failure(batch, message)
			return error(message)
		}
		snapshot := app.complete_render_target_preparation(index, batch, candidate) or {
			app.abort_render_if_graphics_available(mut state, frame)
			app.cancel_render_target_claim(index, batch, .backend_unavailable)
			return err
		}
		target_lease := RenderTargetLease{
			app_instance: app.instance_id
			batch_epoch:  batch.epoch
			target_epoch: target_epoch
			window_epoch: window_epoch
			window:       id
		}
		state.targets << BackendTargetSlot{
			epoch:    target_epoch
			lease:    target_lease
			frame:    frame
			snapshot: snapshot
			status:   .prepared
		}
		return RenderTargetAcquisition{
			status:   .ready
			lease:    target_lease
			snapshot: snapshot
		}
	}

	pub fn (mut app App) with_render_target_pass(target RenderTargetLease, action gfx.PassAction, f RenderPassFn) ! {
		if f == unsafe { nil } {
			return error(err_render_nil_pass_callback)
		}
		app.assert_owner_thread()!
		mut state := app.render_backend_state()!
		app.sync_renderer_health(mut state)
		if state.native_health.blocks_graphics() {
			return error(err_render_native_renderer_lost)
		}
		index := validate_target_lease(state, app.instance_id, target)!
		if state.targets[index].status != .prepared {
			return error(err_render_target_pass_used)
		}
		if message := app.take_internal_fault(.renderer_pass_begin) {
			return RecoverableRenderOperationError{
				message: message
			}
		}
		prepared_frame := state.targets[index].frame
		attempt := app.backend.activate_render_frame(prepared_frame)
		if !attempt.outcome.succeeded() {
			app.apply_native_render_outcome(mut state, target.window, attempt.outcome)
			app.abort_render_if_graphics_available(mut state, prepared_frame)
			state.targets[index].status = .released
			submission_error := app.complete_render_submission(target.window, target.batch_epoch,
				false)
			if submission_error != '' {
				state.failure_messages << submission_error
				state.terminal = true
			}
			attempt_error := native_render_error(attempt.outcome)
			if attempt.outcome.is_recoverable_target()
				|| attempt.outcome.disposition == .native_window_lost {
				state.targets[index].recoverable_outcome = attempt.outcome
				state.targets[index].recoverable_error = attempt_error.msg()
				return RecoverableRenderAttemptError{
					target:  target
					outcome: attempt.outcome
					message: attempt_error.msg()
				}
			}
			return attempt_error
		}
		acquired_frame := attempt.frame
		expected := state.targets[index].snapshot
		if !render_frame_matches_snapshot(acquired_frame, expected) {
			app.release_acquired_target_after_validation_failure(mut state, index, acquired_frame,
				err_render_target_stale)
			return error(err_render_target_stale)
		}
		pass := gfx.Pass{
			action:    action
			swapchain: acquired_frame.swapchain
		}
		state.targets[index].frame = acquired_frame
		state.targets[index].status = .acquired
		linearized := app.linearize_render_target_acquisition(target, expected,
			acquired_frame.metrics, acquired_frame.target) or {
			app.release_acquired_target_after_validation_failure(mut state, index, acquired_frame,
				err.msg())
			return err
		}
		app.sync_renderer_health(mut state)
		if state.native_health.blocks_graphics() {
			state.device_lost = true
			state.terminal = true
			state.shutdown_path = .logical_abandon
			state.targets[index].status = .released
			submission_error := app.complete_render_submission(target.window, target.batch_epoch,
				false)
			if submission_error != '' {
				state.failure_messages << submission_error
			}
			return error(native_renderer_health_error(state.native_health))
		}
		state.sokol_touched = true
		gfx.begin_pass(&pass)
		state.targets[index].snapshot = linearized
		mut callback_error := IError(none)
		f() or { callback_error = err }
		if app.renderer_graphics_forbidden(mut state) {
			if callback_error !is none {
				state.failure_messages << callback_error.msg()
			}
			state.targets[index].status = .released
			submission_error := app.complete_render_submission(target.window, target.batch_epoch,
				false)
			if submission_error != '' {
				state.failure_messages << submission_error
			}
			return error(native_renderer_health_error(state.native_health))
		}
		gfx.end_pass()
		state.targets[index].status = .pass_closed
		state.completed_user_passes++
		if callback_error !is none {
			state.failure_messages << callback_error.msg()
			return callback_error
		}
	}

	pub fn (mut app App) note_render_gpu_work(batch RenderBatchLease) ! {
		app.assert_owner_thread()!
		mut state := app.validate_backend_batch(batch)!
		state.sokol_touched = true
	}

	fn (mut app App) fail_backend_render_batch(batch RenderBatchLease, callback_error IError) {
		mut state := app.validate_backend_batch_epilogue(batch) or { return }
		if callback_error is RecoverableRenderOperationError {
			return
		}
		if callback_error is RecoverableRenderAttemptError {
			if state.matches_recoverable_render_attempt(callback_error, app.instance_id) {
				state.suppressed_callback_attempt = callback_error
				return
			}
		}
		message := callback_error.msg()
		if message != '' && message !in state.failure_messages {
			state.failure_messages << message
		}
	}

	fn (mut app App) record_backend_render_batch_failure(batch RenderBatchLease, message string) {
		mut state := app.validate_backend_batch_epilogue(batch) or { return }
		if message != '' && message !in state.failure_messages {
			state.failure_messages << message
		}
	}

	fn (mut app App) finish_backend_render_batch(batch RenderBatchLease) !RenderBatchOutcome {
		mut state := app.validate_backend_batch_epilogue(batch)!
		mut errors := state.failure_messages.clone()
		app.sync_renderer_health(mut state)
		if state.native_health.blocks_graphics() {
			state.device_lost = true
			state.terminal = true
			state.shutdown_path = .logical_abandon
			health_error := native_renderer_health_error(state.native_health)
			if health_error !in errors {
				errors << health_error
			}
		}
		mut anchor_frame := RenderFrame{}
		if state.sokol_touched && state.completed_user_passes == 0
			&& !state.native_health.blocks_graphics() {
			mut anchor_failed := false
			if message := app.take_internal_fault(.renderer_anchor_begin) {
				errors << message
				state.terminal = true
				anchor_failed = true
			} else {
				anchor_frame = app.backend.begin_renderer_anchor() or {
					errors << err.msg()
					app.sync_renderer_health(mut state)
					if state.native_health.blocks_graphics() {
						state.device_lost = true
						state.terminal = true
					}
					anchor_failed = true
					RenderFrame{}
				}
				app.sync_renderer_health(mut state)
				if state.native_health.blocks_graphics() {
					state.device_lost = true
					state.terminal = true
					state.shutdown_path = .logical_abandon
					anchor_failed = true
				}
			}
			if !anchor_failed {
				state.anchor_acquired = true
				if anchor_frame.swapchain.width <= 0 || anchor_frame.swapchain.height <= 0 {
					errors << err_render_anchor_failed
					state.terminal = true
				} else {
					anchor_pass := gfx.Pass{
						swapchain: anchor_frame.swapchain
					}
					gfx.begin_pass(&anchor_pass)
					if app.renderer_graphics_forbidden(mut state) {
						anchor_failed = true
					} else {
						gfx.end_pass()
						state.anchor_pass_closed = true
					}
				}
			}
		}

		// A commit is legal only after a user or recovery-anchor swapchain pass
		// has closed. Resource-only Sokol work therefore requires the anchor.
		app.sync_renderer_health(mut state)
		if state.native_health.blocks_graphics() {
			state.device_lost = true
			state.terminal = true
			state.shutdown_path = .logical_abandon
		}
		mut committed := !state.native_health.blocks_graphics()
			&& (state.completed_user_passes > 0 || state.anchor_pass_closed)
		if committed {
			if message := app.take_internal_fault(.renderer_precommit) {
				errors << message
				state.terminal = true
				committed = false
			}
			// A closed pass must be physically committed before native resources are aborted.
			gfx.commit()
			app.sync_renderer_health(mut state)
			if state.native_health.blocks_graphics() {
				state.device_lost = true
				state.terminal = true
				state.shutdown_path = .logical_abandon
			}
		}
		mut finalized := 0
		mut native_calls_forbidden := state.native_health.blocks_graphics()
		for i, target in state.targets {
			if app.renderer_graphics_forbidden(mut state) {
				native_calls_forbidden = true
			}
			if target.status == .pass_closed && committed && !native_calls_forbidden
				&& !state.terminal {
				finalization := app.backend.end_render(target.frame)
				app.apply_native_render_outcome(mut state, target.lease.window,
					finalization.outcome)
				if app.renderer_graphics_forbidden(mut state) {
					native_calls_forbidden = true
				}
				mut succeeded := finalization.outcome.succeeded()
					&& finalization.status == .submitted && !native_calls_forbidden
					&& !state.terminal
				if succeeded {
					if message := app.take_internal_fault(.renderer_submission_finalize) {
						succeeded = false
						errors << message
						state.terminal = true
					}
				}
				state.targets[i].status = .finalized
				submission_error := app.complete_render_submission(target.lease.window,
					batch.epoch, succeeded)
				if submission_error != '' {
					succeeded = false
					errors << submission_error
					state.terminal = true
				}
				if succeeded {
					finalized++
				}
			} else if target.status != .released {
				if !native_calls_forbidden {
					app.backend.abort_render(target.frame) or { errors << err.msg() }
					app.sync_renderer_health(mut state)
					if state.native_health.blocks_graphics() {
						state.device_lost = true
						state.terminal = true
						state.shutdown_path = .logical_abandon
						native_calls_forbidden = true
					}
				}
				state.targets[i].status = .released
				submission_error := app.complete_render_submission(target.lease.window,
					batch.epoch, false)
				if submission_error != '' {
					errors << submission_error
					state.terminal = true
				}
			}
		}
		if app.renderer_graphics_forbidden(mut state) {
			native_calls_forbidden = true
		}
		if state.anchor_acquired && !native_calls_forbidden {
			if state.anchor_pass_closed && committed {
				app.backend.end_renderer_anchor(anchor_frame) or {
					errors << err.msg()
					app.sync_renderer_health(mut state)
					if state.native_health.blocks_graphics() {
						state.terminal = true
					}
				}
			} else {
				app.backend.abort_renderer_anchor(anchor_frame) or {
					errors << err.msg()
					app.sync_renderer_health(mut state)
					if state.native_health.blocks_graphics() {
						state.terminal = true
					}
				}
			}
			app.sync_renderer_health(mut state)
			if state.native_health.blocks_graphics() {
				state.device_lost = true
				state.terminal = true
				state.shutdown_path = .logical_abandon
				native_calls_forbidden = true
			}
		}
		if app.renderer_graphics_forbidden(mut state) {
			native_calls_forbidden = true
		}
		if !native_calls_forbidden {
			app.backend.end_render_batch(NativeOperationSeed{
				presence_mask: native_context_has_batch_epoch
				call_site:     .window_finalize
				scope:         .batch
				batch_epoch:   batch.epoch
			}) or {
				errors << err.msg()
				app.sync_renderer_health(mut state)
				if state.native_health.blocks_graphics() {
					state.device_lost = true
					state.terminal = true
					state.shutdown_path = .logical_abandon
					native_calls_forbidden = true
				}
			}
		}
		if native_calls_forbidden {
			state.device_lost = true
			state.terminal = true
			state.shutdown_path = .logical_abandon
			health_error := native_renderer_health_error(state.native_health)
			if health_error !in errors {
				errors << health_error
			}
		}
		if state.sokol_touched && !committed {
			state.terminal = true
		}
		for message in state.failure_messages {
			if message != '' && message !in errors {
				errors << message
			}
		}
		terminal_error := aggregate_terminal_errors(err_render_terminal_aggregate, errors)
		if state.terminal {
			app.state_mutex.lock()
			app.render_runtime.renderer_terminal = if terminal_error == '' {
				err_render_renderer_failed
			} else {
				terminal_error
			}
			app.state_mutex.unlock()
		}
		outcome := RenderBatchOutcome{
			suppressed_callback_target:  state.suppressed_callback_attempt.target
			suppressed_callback_outcome: state.suppressed_callback_attempt.outcome
			suppressed_callback_message: state.suppressed_callback_attempt.message
			batch_epoch:                 batch.epoch
			committed:                   committed
			had_gpu_work:                state.sokol_touched
			completed_user_passes:       state.completed_user_passes
			finalized_submissions:       finalized
			error:                       terminal_error
		}
		state.targets.clear()
		state.batch_active = false
		state.batch_epoch = 0
		state.sokol_touched = false
		state.completed_user_passes = 0
		state.anchor_acquired = false
		state.anchor_pass_closed = false
		state.suppressed_callback_attempt = RecoverableRenderAttemptError{}
		state.failure_messages.clear()
		return outcome
	}

	fn (mut app App) abort_backend_render_batch(batch RenderBatchLease, failure string) string {
		mut errors := []string{}
		if failure != '' {
			errors << failure
		}
		if app.render_bridge == unsafe { nil } {
			return aggregate_terminal_errors(err_render_terminal_aggregate, errors)
		}
		mut state := unsafe { &RenderBackendState(app.render_bridge) }
		if state.batch_active && state.batch_epoch == batch.epoch {
			app.sync_renderer_health(mut state)
			mut native_calls_forbidden := state.native_health.blocks_graphics()
			if native_calls_forbidden {
				state.device_lost = true
				state.shutdown_path = .logical_abandon
			}
			for target in state.targets {
				if target.status != .released {
					if app.renderer_graphics_forbidden(mut state) {
						native_calls_forbidden = true
					}
					if !native_calls_forbidden {
						app.backend.abort_render(target.frame) or { errors << err.msg() }
						app.sync_renderer_health(mut state)
						if state.native_health.blocks_graphics() {
							state.device_lost = true
							state.shutdown_path = .logical_abandon
							native_calls_forbidden = true
						}
					}
					submission_error := app.complete_render_submission(target.lease.window,
						batch.epoch, false)
					if submission_error != '' {
						errors << submission_error
					}
				}
			}
			if app.renderer_graphics_forbidden(mut state) {
				native_calls_forbidden = true
			}
			if !native_calls_forbidden {
				app.backend.end_render_batch(NativeOperationSeed{
					presence_mask: native_context_has_batch_epoch
					call_site:     .window_finalize
					scope:         .batch
					batch_epoch:   batch.epoch
				}) or { errors << err.msg() }
				app.sync_renderer_health(mut state)
				if state.native_health.blocks_graphics() {
					state.device_lost = true
					state.shutdown_path = .logical_abandon
				}
			}
			state.targets.clear()
			state.batch_active = false
			state.batch_epoch = 0
			state.sokol_touched = false
			state.completed_user_passes = 0
			state.anchor_acquired = false
			state.anchor_pass_closed = false
			state.failure_messages.clear()
			state.terminal = true
		}
		terminal := aggregate_terminal_errors(err_render_terminal_aggregate, errors)
		app.mark_renderer_terminal(terminal)
		return terminal
	}

	fn (app &App) render_backend_state() !&RenderBackendState {
		if app.render_bridge == unsafe { nil } {
			return error(err_renderer_unsupported)
		}
		state := unsafe { &RenderBackendState(app.render_bridge) }
		if !state.started {
			return error(err_renderer_unsupported)
		}
		if app.backend.renderer_health().blocks_graphics() {
			return error(err_render_native_renderer_lost)
		}
		if !gfx.is_valid() {
			return error(err_renderer_unsupported)
		}
		return state
	}

	fn (app &App) validate_backend_batch(batch RenderBatchLease) !&RenderBackendState {
		if batch.app_instance != app.instance_id {
			return error(err_app_identity_mismatch)
		}
		state := app.render_backend_state()!
		if !state.batch_active || state.batch_epoch != batch.epoch {
			return error(err_render_batch_stale)
		}
		return state
	}

	fn (app &App) validate_backend_batch_epilogue(batch RenderBatchLease) !&RenderBackendState {
		if batch.app_instance != app.instance_id {
			return error(err_app_identity_mismatch)
		}
		if app.render_bridge == unsafe { nil } {
			return error(err_renderer_unsupported)
		}
		state := unsafe { &RenderBackendState(app.render_bridge) }
		if !state.started || !gfx.is_valid() {
			return error(err_renderer_unsupported)
		}
		if !state.batch_active || state.batch_epoch != batch.epoch {
			return error(err_render_batch_stale)
		}
		return state
	}

	fn (mut app App) mark_renderer_terminal(message string) {
		if app.render_bridge != unsafe { nil } {
			mut state := unsafe { &RenderBackendState(app.render_bridge) }
			state.terminal = true
			app.sync_renderer_health(mut state)
		}
		app.state_mutex.lock()
		app.render_runtime.renderer_terminal = if message == '' {
			err_render_renderer_failed
		} else {
			message
		}
		app.state_mutex.unlock()
	}

	fn validate_target_lease(state &RenderBackendState, app_instance u64, target RenderTargetLease) !int {
		if target.app_instance != app_instance || target.batch_epoch != state.batch_epoch
			|| target.target_epoch == 0 || target.window_epoch == 0 {
			return error(err_render_target_stale)
		}
		for i, slot in state.targets {
			if slot.epoch == target.target_epoch && slot.lease == target {
				return i
			}
		}
		return error(err_render_target_stale)
	}

	fn (mut app App) sync_renderer_health(mut state RenderBackendState) {
		health := app.backend.renderer_health()
		match state.native_health {
			.lost, .abandoned {}
			.unavailable {
				if health == .lost {
					state.native_health = .lost
				}
			}
			else {
				state.native_health = health
			}
		}

		if state.native_health.blocks_graphics() {
			state.device_lost = true
		}
	}

	fn (mut app App) renderer_graphics_forbidden(mut state RenderBackendState) bool {
		app.sync_renderer_health(mut state)
		if !state.native_health.blocks_graphics() {
			return false
		}
		state.device_lost = true
		state.terminal = true
		state.shutdown_path = .logical_abandon
		return true
	}

	fn native_renderer_health_error(health NativeRendererHealth) string {
		return if health == .lost {
			err_render_native_renderer_lost
		} else {
			err_render_native_renderer_unavailable
		}
	}

	fn (mut app App) apply_native_render_outcome(mut state RenderBackendState, id WindowId, outcome NativeRenderResult) {
		app.sync_renderer_health(mut state)
		if state.native_health.blocks_graphics() {
			if outcome.error_text != '' && outcome.error_text !in state.failure_messages {
				state.failure_messages << outcome.error_text
			}
			state.device_lost = true
			state.terminal = true
			state.shutdown_path = .logical_abandon
			return
		}
		if outcome.succeeded() {
			return
		}
		app.backend.promote_native_render_window_loss(id, outcome)
		app.sync_renderer_health(mut state)
		if state.native_health.blocks_graphics() {
			if outcome.error_text != '' && outcome.error_text !in state.failure_messages {
				state.failure_messages << outcome.error_text
			}
			state.device_lost = true
			state.terminal = true
			state.shutdown_path = .logical_abandon
			return
		}
		match outcome.disposition {
			.transient, .target_lost, .not_presented, .native_window_lost {}
			.batch_terminal, .operation_failed {
				if outcome.error_text != '' && outcome.error_text !in state.failure_messages {
					state.failure_messages << outcome.error_text
				}
				state.terminal = true
			}
			.renderer_unavailable, .renderer_lost {
				if outcome.error_text != '' && outcome.error_text !in state.failure_messages {
					state.failure_messages << outcome.error_text
				}
				state.device_lost = true
				state.terminal = true
				state.shutdown_path = .logical_abandon
			}
			else {}
		}
	}

	fn (state &RenderBackendState) matches_recoverable_render_attempt(attempt RecoverableRenderAttemptError, app_instance u64) bool {
		if attempt.target.app_instance == 0
			|| attempt.target.app_instance != app_instance
			|| attempt.target.batch_epoch != state.batch_epoch
			|| (!attempt.outcome.is_recoverable_target()
			&& attempt.outcome.disposition != .native_window_lost)
			|| attempt.message == ''
			|| attempt.message != native_render_error(attempt.outcome).msg() {
			return false
		}
		for slot in state.targets {
			if slot.lease == attempt.target {
				return slot.status == .released && slot.recoverable_error == attempt.message
					&& native_render_results_identical(slot.recoverable_outcome, attempt.outcome)
			}
		}
		return false
	}

	fn native_render_results_identical(left NativeRenderResult, right NativeRenderResult) bool {
		return left.domain == right.domain && left.operation == right.operation
			&& left.scope == right.scope && left.disposition == right.disposition
			&& left.native_code == right.native_code && left.removal_reason == right.removal_reason
			&& left.native_status == right.native_status
			&& left.display_error == right.display_error
			&& left.current_draw_surface == right.current_draw_surface
			&& left.current_read_surface == right.current_read_surface
			&& left.current_context == right.current_context && left.error_text == right.error_text
			&& native_operation_contexts_identical(left.context, right.context)
			&& left.local_validation == right.local_validation
			&& native_primitive_evidence_identical(left.actual_primitive, right.actual_primitive)
			&& native_primitive_evidence_identical(left.primitive, right.primitive)
	}

	fn native_primitive_evidence_identical(left NativePrimitiveEvidence, right NativePrimitiveEvidence) bool {
		return left.valid_mask == right.valid_mask && left.return_value == right.return_value
			&& left.handle == right.handle && left.egl_error == right.egl_error
			&& left.native_errno == right.native_errno
			&& left.wayland_display_error == right.wayland_display_error
			&& left.dxgi_removal_reason == right.dxgi_removal_reason
			&& left.observed_count == right.observed_count
			&& left.observed_flags == right.observed_flags
			&& left.selected_value == right.selected_value
			&& left.object_identity_0 == right.object_identity_0
			&& left.object_identity_1 == right.object_identity_1
			&& left.object_identity_2 == right.object_identity_2
	}

	fn render_acquire_block_reason(result NativeRenderResult) RenderBlockReason {
		return match result.disposition {
			.transient, .not_presented { .drawable_unavailable }
			.target_lost { .resize_pending }
			.renderer_lost, .renderer_unavailable { .renderer_failed }
			else { .backend_unavailable }
		}
	}

	fn render_frame_matches_snapshot(frame RenderFrame, snapshot RenderWindowSnapshot) bool {
		return frame.acquired && frame.window_id == snapshot.window
			&& frame.metrics == snapshot.metrics && frame.target == snapshot.target
			&& snapshot.target.target_identity != 0
			&& frame.swapchain.width == snapshot.metrics.framebuffer_width
			&& frame.swapchain.height == snapshot.metrics.framebuffer_height
			&& int(frame.swapchain.color_format) == snapshot.target.color_format
			&& int(frame.swapchain.depth_format) == snapshot.target.depth_format
			&& frame.swapchain.sample_count == snapshot.target.sample_count
	}

	fn (mut app App) release_acquired_target_after_validation_failure(mut state RenderBackendState, index int, frame RenderFrame, message string) {
		app.abort_render_if_graphics_available(mut state, frame)
		state.targets[index].frame = frame
		state.targets[index].status = .released
		submission_error := app.complete_render_submission(state.targets[index].lease.window,
			state.batch_epoch, false)
		if submission_error != '' {
			state.failure_messages << submission_error
		}
		if message != '' {
			state.failure_messages << message
		}
		state.terminal = true
	}

	fn (mut app App) abort_render_if_graphics_available(mut state RenderBackendState, frame RenderFrame) {
		app.sync_renderer_health(mut state)
		if state.native_health.blocks_graphics() {
			state.device_lost = true
			state.terminal = true
			state.shutdown_path = .logical_abandon
			return
		}
		app.backend.abort_render(frame) or {
			state.failure_messages << err.msg()
			state.terminal = true
		}
		app.sync_renderer_health(mut state)
		if state.native_health.blocks_graphics() {
			state.device_lost = true
			state.terminal = true
			state.shutdown_path = .logical_abandon
		}
	}

	fn validate_renderer_config(config RendererConfig) ! {
		if config.buffer_pool_size <= 0 || config.image_pool_size <= 0
			|| config.sampler_pool_size <= 0 || config.shader_pool_size <= 0
			|| config.pipeline_pool_size <= 0 || config.attachments_pool_size <= 0 {
			return error(err_renderer_unsupported)
		}
	}
} $else {
	fn (mut app App) mark_renderer_terminal(message string) {
		_ = message
		app.state_mutex.lock()
		app.render_runtime.renderer_terminal = err_render_renderer_failed
		app.state_mutex.unlock()
	}

	pub fn (app &App) renderer_is_usable() bool {
		_ = app
		return false
	}

	pub fn (app &App) renderer_device_available_for_gg() bool {
		_ = app
		return false
	}

	pub fn (mut app App) prepare_renderer_shutdown_for_gg() ! {
		_ = app
		return error(err_renderer_unsupported)
	}

	pub fn (mut app App) abandon_renderer_for_gg() ! {
		_ = app
	}

	fn (mut app App) begin_backend_render_batch(lease RenderBatchLease) ! {
		_ = app
		_ = lease
		return error(err_renderer_unsupported)
	}

	fn (mut app App) fail_backend_render_batch(lease RenderBatchLease, callback_error IError) {
		_ = app
		_ = lease
		_ = callback_error
	}

	fn (mut app App) record_backend_render_batch_failure(lease RenderBatchLease, message string) {
		_ = app
		_ = lease
		_ = message
	}

	fn (mut app App) finish_backend_render_batch(lease RenderBatchLease) !RenderBatchOutcome {
		_ = app
		_ = lease
		return error(err_renderer_unsupported)
	}

	fn (mut app App) abort_backend_render_batch(lease RenderBatchLease, failure string) string {
		_ = app
		_ = lease
		return failure
	}

	fn (mut app App) shutdown_render_bridge_for_stop() ! {
		_ = app
	}
}
