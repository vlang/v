module multiwindow

import os

$if gg_multiwindow ? || x_multiwindow_render ? {
	import gg.testdata.multiwindow_probe_gate
	import gg.testdata.multiwindow_sokol_trace
	import sokol.gfx
	import time
}

#flag -DSOKOL_TRACE_HOOKS

$if gg_multiwindow ? || x_multiwindow_render ? {
	#flag darwin -DV_MULTIWINDOW_NATIVE_PROOF_TEST

	$if darwin {
		fn C.v_multiwindow_test_appkit_admit_window(state voidptr) int
	}
}

struct InternalFaultMatrixCase {
	stage InternalFaultStage
	label string
}

fn test_internal_fault_stage_inventory_is_complete() {
	cases := internal_fault_matrix_cases()
	assert cases.len == int(InternalFaultStage.teardown_backend_stop)
	for index, item in cases {
		assert int(item.stage) == index + 1
		assert item.label != ''
	}
	renderer_stages := core_renderer_fault_stage_inventory()
	assert renderer_stages.len == 9
	for index, stage in renderer_stages {
		assert cases[index].stage == stage
	}
}

fn test_renderer_fault_forced_runtime_gate_has_render_sources() {
	if os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') != '1' {
		return
	}
	$if gg_multiwindow ? || x_multiwindow_render ? {
		assert true
	} $else {
		assert false, 'forced renderer fault operations require gg_multiwindow or x_multiwindow_render'
	}
}

fn test_renderer_anchor_create_fault_drives_start_renderer_retry_and_cleanup() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !renderer_fault_runtime_requested_for_test() {
			return
		}
		renderer_fault_exercise_start_stage(.renderer_anchor_create)!
	} $else {
		return
	}
}

fn test_renderer_environment_fault_drives_start_renderer_retry_and_cleanup() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !renderer_fault_runtime_requested_for_test() {
			return
		}
		renderer_fault_exercise_start_stage(.renderer_environment)!
	} $else {
		return
	}
}

fn test_renderer_setup_fault_drives_start_renderer_retry_and_cleanup() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if !renderer_fault_runtime_requested_for_test() {
			return
		}
		renderer_fault_exercise_start_stage(.renderer_setup)!
	} $else {
		return
	}
}

fn test_renderer_batch_begin_fault_is_recoverable_before_scheduler_commit() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if renderer_fault_runtime_requested_for_test() {
			renderer_fault_exercise_batch_begin_for_test()!
		}
	}
}

fn test_renderer_target_acquire_fault_is_recoverable_before_claim_commit() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if renderer_fault_runtime_requested_for_test() {
			renderer_fault_exercise_target_acquire_for_test()!
		}
	}
}

fn test_renderer_pass_begin_fault_preserves_prepared_target_for_retry() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if renderer_fault_runtime_requested_for_test() {
			renderer_fault_exercise_pass_begin_for_test()!
		}
	}
}

fn test_renderer_anchor_begin_fault_terminalizes_before_anchor_sokol_call() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if renderer_fault_runtime_requested_for_test() {
			renderer_fault_exercise_anchor_begin_for_test()!
		}
	}
}

fn test_renderer_precommit_fault_terminalizes_after_anchor_pass_before_commit() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if renderer_fault_runtime_requested_for_test() {
			renderer_fault_exercise_precommit_for_test()!
		}
	}
}

fn test_renderer_submission_finalize_fault_terminalizes_after_one_native_submission() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if renderer_fault_runtime_requested_for_test() {
			renderer_fault_exercise_submission_finalize_for_test()!
		}
	}
}

fn test_renderer_submission_finalize_shutdown_replay_is_exact() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if renderer_fault_runtime_requested_for_test() {
			renderer_fault_exercise_submission_finalize_shutdown_replay_for_test()!
		}
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	@[heap]
	struct RendererFaultBatchRetryState {
	mut:
		callback_calls int
	}

	@[heap]
	struct RendererFaultTargetAcquireState {
	mut:
		failed_state          RenderWindowRuntime
		submitted_lease       RenderTargetLease
		x11_surface_before    RendererFaultX11SurfaceState
		wayland_target_before RendererFaultWaylandTargetState
		win32_target_before   RendererFaultWin32TargetState
		batch_callback_calls  int
		pass_callback_calls   int
	}

	@[heap]
	struct RendererFaultPassBeginState {
	mut:
		submitted_lease      RenderTargetLease
		batch_callback_calls int
		pass_callback_calls  int
	}

	@[heap]
	struct RendererFaultAnchorBeginState {
	mut:
		native_start   int
		callback_calls int
		batch_epoch    u64
		second_calls   int
	}

	@[heap]
	struct RendererFaultPrecommitState {
	mut:
		native_start   int
		callback_calls int
		batch_epoch    u64
		second_calls   int
	}

	struct RendererFaultX11SurfaceState {
		captured          bool
		target_generation u64
		surface_identity  u64
		surface_ticket    u64
		ticket_identity   u64
		ticket_context    NativeOperationContext
	}

	struct RendererFaultWaylandTargetState {
		captured             bool
		materialized         bool
		pending_egl_resize   bool
		target_generation    u64
		native_surface       u64
		display_identity     u64
		egl_display_identity u64
		wl_egl_window        RendererFaultLifetimeTeardownAuthority
		egl_surface          RendererFaultLifetimeTeardownAuthority
	}

	struct RendererFaultWin32TargetState {
		captured              bool
		materialized          bool
		render_resize_pending bool
		window_identity       u64
		target_generation     u64
		swapchain             RendererFaultLifetimeTeardownAuthority
		render_view           RendererFaultLifetimeTeardownAuthority
		depth_texture         RendererFaultLifetimeTeardownAuthority
		depth_view            RendererFaultLifetimeTeardownAuthority
	}

	struct RendererFaultWaylandFrameCallbackState {
		captured                bool
		target_generation       u64
		callback_identity       u64
		callback_ticket         u64
		parent_identity         u64
		ticket_count            int
		ticket_release_kind     NativeLifetimeReleaseKind
		ticket_state            NativeLifetimeTicketState
		ticket_native_identity  u64
		ticket_parent_identity  u64
		ticket_owner_window     WindowId
		ticket_owner_generation u64
		ticket_context          NativeOperationContext
	}

	@[heap]
	struct RendererFaultSubmissionFinalizeState {
	mut:
		submitted_lease              RenderTargetLease
		x11_surface_before           RendererFaultX11SurfaceState
		wayland_target_before        RendererFaultWaylandTargetState
		wayland_first_target_ordinal u64
		win32_target_before          RendererFaultWin32TargetState
		batch_callback_calls         int
		pass_callback_calls          int
		second_calls                 int
	}

	struct RendererFaultStateSnapshot {
		app_status                        AppStatus
		app_stopping                      bool
		admission_open                    bool
		stop_prepared                     bool
		stop_serial                       u64
		stop_terminal                     string
		pending_stop_errors               []string
		window                            WindowId
		window_slot_status                WindowStatus
		window_destroy_stage              WindowDestroyStage
		window_destroy_serial             u64
		window_backend_destroyed          bool
		window_destroy_terminal           string
		window_teardown_sequence          u64
		window_config_width               int
		window_config_height              int
		window_runtime                    RenderWindowRuntime
		runtime_windows                   int
		runtime_next_epoch                u64
		runtime_next_admission_id         u64
		runtime_next_lease_epoch          u64
		runtime_next_batch_epoch          u64
		runtime_next_destroy_serial       u64
		runtime_next_teardown_sequence    u64
		runtime_active_batch_epoch        u64
		runtime_batch_active              bool
		runtime_renderer_terminal         string
		renderer_exists                   bool
		renderer_started                  bool
		renderer_anchor_created           bool
		renderer_batch_active             bool
		renderer_batch_epoch              u64
		renderer_next_target_epoch        u64
		renderer_target_count             int
		renderer_sokol_touched            bool
		renderer_completed_user_passes    int
		renderer_anchor_acquired          bool
		renderer_anchor_pass_closed       bool
		renderer_failure_messages         []string
		renderer_terminal                 bool
		renderer_device_lost              bool
		renderer_native_health            NativeRendererHealth
		renderer_shutdown_path            RendererShutdownPath
		renderer_shutdown_prepared        bool
		renderer_shutdown_batch_scope     bool
		renderer_fault_trace_token        u64
		backend_anchor_generation         u64
		backend_windows                   int
		backend_anchor_identity           u64
		backend_anchor_ticket             u64
		wayland_anchor_egl_surface        RendererFaultLifetimeTeardownAuthority
		wayland_anchor_wl_egl_window      RendererFaultLifetimeTeardownAuthority
		wayland_anchor_wl_surface         RendererFaultLifetimeTeardownAuthority
		wayland_compositor_identity       u64
		backend_renderer_device_identity  u64
		backend_renderer_device_ticket    u64
		backend_renderer_context_identity u64
		backend_renderer_context_ticket   u64
		backend_factory_identity          u64
		backend_factory_ticket            u64
		backend_display_identity          u64
		backend_display_ticket            u64
		wayland_display_identity          u64
		backend_owner_thread_identity     u64
		backend_thread_ticket             u64
		win32_anchor_color_texture        u64
		win32_anchor_color_ticket         u64
		win32_anchor_render_view          u64
		win32_anchor_render_view_ticket   u64
		win32_anchor_depth_texture        u64
		win32_anchor_depth_ticket         u64
		win32_anchor_depth_view           u64
		win32_anchor_depth_view_ticket    u64
		appkit_batch_pool                 u64
		appkit_batch_pool_ticket          u64
		appkit_active_anchor_lease        u64
		appkit_active_anchor_drawable     u64
		appkit_active_anchor_ticket       u64
		appkit_window_frame_active        bool
		appkit_window_frame_lease         u64
		appkit_window_drawable            u64
		appkit_window_drawable_ticket     u64
	}

	struct RendererFaultTraceBindingSnapshot {
		renderer_exists bool
		token           u64
	}

	struct RendererFaultNativeProofSnapshot {
		generation         u64
		ordinal_floor      u64
		accepting_plans    bool
		plan               [native_primitive_plan_capacity]NativePrimitivePlanEntry
		trace              [native_operation_trace_capacity]NativeOperationTraceEntry
		trace_len          int
		trace_overflow     bool
		next_ordinal       u64
		sequence_exhausted bool
		terminal_cause     NativeLocalValidation
		live_tickets       int
	}

	struct RendererFaultNativeProofCapture {
		available bool
		snapshot  RendererFaultNativeProofSnapshot
	}

	struct RendererFaultLifetimeTeardownAuthority {
		captured                 bool
		value_identity           u64
		ticket_id                u64
		app_identity             u64
		authority_scope          NativeOperationAuthorityScope
		authority_token          u64
		domain                   NativeRenderDomain
		release_kind             NativeLifetimeReleaseKind
		owner_seed               NativeOperationSeed
		proof_generation         u64
		context                  NativeOperationContext
		ticket_native_identity   u64
		required_parent_identity u64
		parent_authority_scope   NativeOperationAuthorityScope
		parent_authority_token   u64
		state                    NativeLifetimeTicketState
	}

	struct RendererFaultWindowTeardownAuthority {
		captured               bool
		backend                BackendKind
		window                 WindowId
		target_generation      u64
		native_window_identity u64
		native_destroyed       bool
		resources              []RendererFaultLifetimeTeardownAuthority
	}

	struct RendererFaultDeliveryStateSnapshot {
		next_token u64
		events     []QueuedEvent
		deliveries map[u64]EventDeliveryState
	}

	struct RendererFaultDeliverySuffixSnapshot {
		before   RendererFaultDeliveryStateSnapshot
		after    RendererFaultDeliveryStateSnapshot
		accepted []QueuedEvent
	}

	struct RendererFaultHarvestReplayState {
	mut:
		window_runtime        RenderWindowRuntime
		window_config_width   int
		window_config_height  int
		runtime_next_epoch    u64
		readiness_metrics     RenderMetricsSnapshot
		readiness_target      RenderTargetSnapshot
		resize_events         int
		resize_input_events   int
		pending_resize_width  int
		pending_resize_height int
	}

	fn renderer_fault_runtime_requested_for_test() bool {
		return os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') == '1'
	}

	fn renderer_fault_backend_for_test() !BackendKind {
		return match os.getenv('VGG_MULTIWINDOW_RUNTIME_BACKEND') {
			'x11' { .x11 }
			'wayland' { .wayland }
			'appkit' { .appkit }
			'win32' { .win32 }
			else { error('VGG_MULTIWINDOW_RUNTIME_BACKEND must select x11, wayland, appkit, or win32') }
		}
	}

	fn renderer_fault_new_app_for_test() !&App {
		renderer_fault_require_parent_gate_for_test()!
		backend := renderer_fault_backend_for_test()!
		mut app := new_app(
			backend:          backend
			queue_size:       8
			require_renderer: true
		)!
		caps := app.capabilities()
		backend_matches := match backend {
			.x11 { caps.x11 && caps.gl }
			.wayland { caps.wayland && caps.gl }
			.appkit { caps.metal }
			.win32 { caps.win32 && caps.d3d11 }
			else { false }
		}

		if caps.backend != backend || !caps.native || !caps.multi_window || !caps.explicit_swapchain
			|| !backend_matches {
			message := 'selected renderer fault backend `${backend}` is unavailable: ${caps}'
			app.stop() or { return error('${message}; cleanup failed: ${err.msg()}') }
			return error(message)
		}
		return app
	}

	fn renderer_fault_require_parent_gate_for_test() ! {
		if os.getenv(multiwindow_probe_gate.environment_name) == '' {
			return error('renderer fault proof requires the parent process-tree watchdog start gate')
		}
		multiwindow_probe_gate.await_parent_release(2 * time.second)!
	}

	fn renderer_fault_exercise_start_stage(stage InternalFaultStage) ! {
		mut app := renderer_fault_new_app_for_test()!
		message := 'fault:${stage}:operation'
		app.set_internal_fault(stage, 0, message) or {
			arm_error := err.msg()
			renderer_fault_stop_twice_clean_for_test(mut app) or {
				return error('${arm_error}; cleanup failed: ${err.msg()}')
			}
			return error(arm_error)
		}
		mut first_error := ''
		app.start_renderer(RendererConfig{}) or { first_error = err.msg() }
		failed_trace := app.renderer_fault_trace_snapshot()
		failed_binding := renderer_fault_trace_binding_for_test(app)
		bridge_released := app.render_bridge == unsafe { nil }
		renderer_unusable := !app.renderer_is_usable()
		status_after_fault := app.status()
		mut retry_error := ''
		app.start_renderer(RendererConfig{}) or { retry_error = err.msg() }
		retry_trace := app.renderer_fault_trace_snapshot()
		retry_binding := renderer_fault_trace_binding_for_test(app)
		retry_usable := app.renderer_is_usable()
		mut first_stop_error := ''
		app.stop() or { first_stop_error = err.msg() }
		first_stop_trace := app.renderer_fault_trace_snapshot()
		first_stop_binding := renderer_fault_trace_binding_for_test(app)
		first_stop_status := app.status()
		mut first_stop_windows_error := ''
		first_stop_windows := app.window_ids() or {
			first_stop_windows_error = err.msg()
			[]WindowId{}
		}
		mut replay_stop_error := ''
		app.stop() or { replay_stop_error = err.msg() }
		replay_stop_trace := app.renderer_fault_trace_snapshot()
		replay_stop_binding := renderer_fault_trace_binding_for_test(app)
		replay_stop_status := app.status()
		mut replay_stop_windows_error := ''
		replay_stop_windows := app.window_ids() or {
			replay_stop_windows_error = err.msg()
			[]WindowId{}
		}

		failed_expected := renderer_fault_failed_start_milestones_for_test(stage)!
		success_expected := renderer_fault_successful_start_milestones_for_test()
		mut first_stop_expected := success_expected.clone()
		first_stop_expected << .gfx_shutdown_complete
		first_stop_expected << .anchor_destroyed

		assert first_error == message
		assert failed_trace.token != 0
		assert failed_trace.attempt_id != 0
		renderer_fault_assert_trace_for_test(failed_trace, failed_trace.token,
			failed_trace.attempt_id, false, failed_expected)
		assert !failed_binding.renderer_exists
		assert failed_binding.token == 0
		assert bridge_released
		assert renderer_unusable
		assert status_after_fault == .running
		assert retry_error == ''
		assert retry_trace.token != 0
		assert retry_trace.token != failed_trace.token
		assert retry_trace.attempt_id == failed_trace.attempt_id + 1
		renderer_fault_assert_trace_for_test(retry_trace, retry_trace.token,
			retry_trace.attempt_id, false, success_expected)
		assert retry_binding.renderer_exists
		assert retry_binding.token == retry_trace.token
		assert retry_usable
		assert first_stop_error == ''
		renderer_fault_assert_trace_for_test(first_stop_trace, retry_trace.token,
			retry_trace.attempt_id, false, first_stop_expected)
		assert !first_stop_binding.renderer_exists
		assert first_stop_binding.token == 0
		assert first_stop_status == .stopped
		assert first_stop_windows_error == ''
		assert first_stop_windows.len == 0
		assert replay_stop_error == ''
		renderer_fault_assert_trace_snapshots_equal_for_test(first_stop_trace, replay_stop_trace)
		assert !replay_stop_binding.renderer_exists
		assert replay_stop_binding.token == 0
		assert replay_stop_status == .stopped
		assert replay_stop_windows_error == ''
		assert replay_stop_windows.len == 0
	}

	fn renderer_fault_started_app_for_test() !&App {
		mut app := renderer_fault_new_app_for_test()!
		app.start_renderer(RendererConfig{}) or {
			message := err.msg()
			app.stop() or { return error('${message}; cleanup failed: ${err.msg()}') }
			return error(message)
		}
		if !app.renderer_is_usable() {
			app.stop() or {
				return error('renderer fault app is unusable; cleanup failed: ${err.msg()}')
			}
			return error('renderer fault app is unusable after successful start')
		}
		return app
	}

	fn renderer_fault_admit_appkit_window_for_test(app &App, window WindowId) ! {
		$if darwin {
			app.assert_owner_thread()!
			index := app.backend.appkit.window_record_index(window) or {
				return error(err_window_not_found)
			}
			state := app.backend.appkit.windows[index].state
			if state == unsafe { nil } {
				return error('AppKit fault-matrix native proof requires owner main thread and a live, visible, non-miniaturized native window')
			}
			status := C.v_multiwindow_test_appkit_admit_window(state)
			if status != 1 && status != 2 {
				return error('AppKit fault-matrix native proof requires owner main thread and a live, visible, non-miniaturized native window')
			}
			return
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn renderer_fault_window_failure_for_test(mut app App, primary_error string) IError {
		mut cleanup_error := ''
		app.stop() or { cleanup_error = err.msg() }
		if cleanup_error != '' {
			return error('${primary_error}; cleanup failed: ${cleanup_error}')
		}
		return error(primary_error)
	}

	fn renderer_fault_window_for_test(mut app App, title string) !WindowId {
		window := app.create_window(
			title:           title
			width:           160
			height:          112
			visible:         true
			redraw_mode:     .on_demand
			render_workload: true
		) or { return renderer_fault_window_failure_for_test(mut app, err.msg()) }
		if app.backend.kind == .appkit {
			renderer_fault_admit_appkit_window_for_test(app, window) or {
				return renderer_fault_window_failure_for_test(mut app, err.msg())
			}
			_ = app.poll_events() or {
				return renderer_fault_window_failure_for_test(mut app, err.msg())
			}
			eligible := app.render_window_eligible(window) or {
				return renderer_fault_window_failure_for_test(mut app, err.msg())
			}
			if eligible {
				return window
			}
			return renderer_fault_window_failure_for_test(mut app,
				'renderer fault AppKit window `${title}` did not become render eligible after bounded owner polling')
		}
		deadline := time.now().add(2 * time.second)
		for {
			_ = app.poll_events() or {
				return renderer_fault_window_failure_for_test(mut app, err.msg())
			}
			eligible := app.render_window_eligible(window) or {
				return renderer_fault_window_failure_for_test(mut app, err.msg())
			}
			if eligible {
				return window
			}
			if time.now() >= deadline {
				break
			}
		}
		return renderer_fault_window_failure_for_test(mut app,
			'renderer fault window `${title}` did not become render eligible')
	}

	fn renderer_fault_window_or_fail_for_test(mut app App, title string) ?WindowId {
		return renderer_fault_window_for_test(mut app, title) or {
			assert false, err.msg()
			return none
		}
	}

	fn renderer_fault_window_runtime_for_test(app &App, id WindowId) !RenderWindowRuntime {
		app.state_mutex.lock()
		index := app.render_window_index_locked(id) or {
			app.state_mutex.unlock()
			return err
		}
		state := app.render_runtime.windows[index]
		app.state_mutex.unlock()
		return state
	}

	fn renderer_fault_trace_binding_for_test(app &App) RendererFaultTraceBindingSnapshot {
		app.state_mutex.lock()
		defer {
			app.state_mutex.unlock()
		}
		if app.render_bridge == unsafe { nil } {
			return RendererFaultTraceBindingSnapshot{}
		}
		state := unsafe { &RenderBackendState(app.render_bridge) }
		return RendererFaultTraceBindingSnapshot{
			renderer_exists: true
			token:           state.fault_trace_token
		}
	}

	fn renderer_fault_state_snapshot_for_test(app &App, id WindowId) !RendererFaultStateSnapshot {
		app.state_mutex.lock()
		index := app.render_window_index_locked(id) or {
			app.state_mutex.unlock()
			return err
		}
		slot := app.windows[id.slot]
		window := app.render_runtime.windows[index]
		bridge := app.render_bridge
		mut snapshot := RendererFaultStateSnapshot{
			app_status:                     app.status
			app_stopping:                   app.stopping
			admission_open:                 app.admission_open
			stop_prepared:                  app.stop_prepared
			stop_serial:                    app.stop_serial
			stop_terminal:                  app.stop_terminal
			pending_stop_errors:            app.pending_stop_errors.clone()
			window:                         id
			window_slot_status:             slot.status
			window_destroy_stage:           slot.destroy_stage
			window_destroy_serial:          slot.destroy_serial
			window_backend_destroyed:       slot.backend_destroyed
			window_destroy_terminal:        slot.destroy_terminal
			window_teardown_sequence:       slot.teardown_sequence
			window_config_width:            slot.config.width
			window_config_height:           slot.config.height
			window_runtime:                 window
			runtime_windows:                app.render_runtime.windows.len
			runtime_next_epoch:             app.render_runtime.next_epoch
			runtime_next_admission_id:      app.render_runtime.next_admission_id
			runtime_next_lease_epoch:       app.render_runtime.next_lease_epoch
			runtime_next_batch_epoch:       app.render_runtime.next_batch_epoch
			runtime_next_destroy_serial:    app.render_runtime.next_destroy_serial
			runtime_next_teardown_sequence: app.render_runtime.next_teardown_sequence
			runtime_active_batch_epoch:     app.render_runtime.active_batch_epoch
			runtime_batch_active:           app.render_runtime.batch_active
			runtime_renderer_terminal:      app.render_runtime.renderer_terminal
			renderer_exists:                bridge != unsafe { nil }
		}
		app.state_mutex.unlock()
		if bridge != unsafe { nil } {
			state := unsafe { &RenderBackendState(bridge) }
			snapshot = RendererFaultStateSnapshot{
				...snapshot
				renderer_started:               state.started
				renderer_anchor_created:        state.anchor_created
				renderer_batch_active:          state.batch_active
				renderer_batch_epoch:           state.batch_epoch
				renderer_next_target_epoch:     state.next_target_epoch
				renderer_target_count:          state.targets.len
				renderer_sokol_touched:         state.sokol_touched
				renderer_completed_user_passes: state.completed_user_passes
				renderer_anchor_acquired:       state.anchor_acquired
				renderer_anchor_pass_closed:    state.anchor_pass_closed
				renderer_failure_messages:      state.failure_messages.clone()
				renderer_terminal:              state.terminal
				renderer_device_lost:           state.device_lost
				renderer_native_health:         state.native_health
				renderer_shutdown_path:         state.shutdown_path
				renderer_shutdown_prepared:     state.shutdown_prepared
				renderer_shutdown_batch_scope:  state.shutdown_batch_scope
				renderer_fault_trace_token:     state.fault_trace_token
			}
		}
		match app.backend.kind {
			.x11 {
				snapshot = RendererFaultStateSnapshot{
					...snapshot
					backend_windows:                   app.backend.x11.windows.len
					backend_anchor_generation:         app.backend.x11.anchor_generation
					backend_anchor_identity:           native_identity(app.backend.x11.anchor_surface)
					backend_anchor_ticket:             app.backend.x11.anchor_surface_ticket
					backend_renderer_context_identity: native_identity(app.backend.x11.egl_context)
					backend_renderer_context_ticket:   app.backend.x11.egl_context_ticket
					backend_display_identity:          native_identity(app.backend.x11.egl_display)
					backend_display_ticket:            app.backend.x11.egl_display_ticket
					backend_owner_thread_identity:     app.backend.native_operations.owner_thread_identity
					backend_thread_ticket:             app.backend.x11.egl_thread_ticket
				}
			}
			.wayland {
				snapshot = RendererFaultStateSnapshot{
					...snapshot
					backend_windows:                   app.backend.wayland.windows.len
					backend_anchor_generation:         app.backend.wayland.anchor_generation
					backend_anchor_identity:           native_identity(app.backend.wayland.anchor_surface)
					backend_anchor_ticket:             app.backend.wayland.anchor_surface_ticket
					wayland_anchor_egl_surface:        renderer_fault_lifetime_teardown_authority_for_test(app,
						native_identity(app.backend.wayland.anchor_surface),
						app.backend.wayland.anchor_surface_ticket)
					wayland_anchor_wl_egl_window:      renderer_fault_lifetime_teardown_authority_for_test(app,
						native_identity(app.backend.wayland.anchor_wl_egl_window),
						app.backend.wayland.anchor_wl_egl_window_ticket)
					wayland_anchor_wl_surface:         renderer_fault_lifetime_teardown_authority_for_test(app,
						native_identity(app.backend.wayland.anchor_wl_surface),
						app.backend.wayland.anchor_wl_surface_ticket)
					wayland_compositor_identity:       native_identity(app.backend.wayland.compositor)
					backend_renderer_context_identity: native_identity(app.backend.wayland.egl_context)
					backend_renderer_context_ticket:   app.backend.wayland.egl_context_ticket
					backend_display_identity:          native_identity(app.backend.wayland.egl_display)
					backend_display_ticket:            app.backend.wayland.egl_display_ticket
					wayland_display_identity:          native_identity(app.backend.wayland.display)
					backend_owner_thread_identity:     app.backend.native_operations.owner_thread_identity
					backend_thread_ticket:             app.backend.wayland.egl_thread_ticket
				}
			}
			.appkit {
				snapshot = RendererFaultStateSnapshot{
					...snapshot
					backend_windows:                  app.backend.appkit.windows.len
					backend_anchor_identity:          native_identity(app.backend.appkit.anchor_state)
					backend_anchor_ticket:            app.backend.appkit.anchor_state_ticket
					backend_renderer_device_identity: native_identity(app.backend.appkit.device)
					backend_renderer_device_ticket:   app.backend.appkit.device_ticket
					appkit_batch_pool:                native_identity(app.backend.appkit.batch_autorelease_pool)
					appkit_batch_pool_ticket:         app.backend.appkit.batch_autorelease_pool_ticket
					appkit_active_anchor_lease:       app.backend.appkit.active_anchor_lease
					appkit_active_anchor_drawable:    native_identity(app.backend.appkit.active_anchor_drawable)
					appkit_active_anchor_ticket:      app.backend.appkit.active_anchor_drawable_ticket
				}
				for record in app.backend.appkit.windows {
					if record.id == id {
						snapshot = RendererFaultStateSnapshot{
							...snapshot
							appkit_window_frame_active:    record.frame_active
							appkit_window_frame_lease:     record.active_frame_lease
							appkit_window_drawable:        native_identity(record.active_drawable)
							appkit_window_drawable_ticket: record.active_drawable_ticket
						}
						break
					}
				}
			}
			.win32 {
				snapshot = RendererFaultStateSnapshot{
					...snapshot
					backend_windows:                   app.backend.win32.windows.len
					backend_renderer_device_identity:  native_identity(app.backend.win32.device)
					backend_renderer_device_ticket:    app.backend.win32.device_ticket
					backend_renderer_context_identity: native_identity(app.backend.win32.device_context)
					backend_renderer_context_ticket:   app.backend.win32.device_context_ticket
					backend_factory_identity:          native_identity(app.backend.win32.factory)
					backend_factory_ticket:            app.backend.win32.factory_ticket
					win32_anchor_color_texture:        native_identity(app.backend.win32.anchor_color_texture)
					win32_anchor_color_ticket:         app.backend.win32.anchor_color_texture_ticket
					win32_anchor_render_view:          native_identity(app.backend.win32.anchor_render_view)
					win32_anchor_render_view_ticket:   app.backend.win32.anchor_render_view_ticket
					win32_anchor_depth_texture:        native_identity(app.backend.win32.anchor_depth_texture)
					win32_anchor_depth_ticket:         app.backend.win32.anchor_depth_texture_ticket
					win32_anchor_depth_view:           native_identity(app.backend.win32.anchor_depth_stencil_view)
					win32_anchor_depth_view_ticket:    app.backend.win32.anchor_depth_stencil_view_ticket
				}
			}
			else {}
		}

		return snapshot
	}

	fn renderer_fault_delivery_state_snapshot_for_test(app &App) RendererFaultDeliveryStateSnapshot {
		app.state_mutex.lock()
		defer {
			app.state_mutex.unlock()
		}
		mut deliveries := map[u64]EventDeliveryState{}
		for token, state in app.event_deliveries {
			deliveries[token] = state
		}
		return RendererFaultDeliveryStateSnapshot{
			next_token: app.next_event_delivery_token
			events:     app.events.clone()
			deliveries: deliveries
		}
	}

	fn renderer_fault_delivery_suffix_snapshot_for_test(before RendererFaultDeliveryStateSnapshot, app &App) RendererFaultDeliverySuffixSnapshot {
		after := renderer_fault_delivery_state_snapshot_for_test(app)
		mut accepted := []QueuedEvent{}
		if after.events.len >= before.events.len {
			accepted = after.events[before.events.len..].clone()
		}
		return RendererFaultDeliverySuffixSnapshot{
			before:   before
			after:    after
			accepted: accepted
		}
	}

	fn renderer_fault_native_proof_snapshot_from_for_test(app &App, proof &NativeOperationProofState) RendererFaultNativeProofSnapshot {
		return RendererFaultNativeProofSnapshot{
			generation:         proof.generation
			ordinal_floor:      proof.ordinal_floor
			accepting_plans:    proof.accepting_plans
			plan:               proof.plan
			trace:              proof.trace
			trace_len:          proof.trace_len
			trace_overflow:     proof.trace_overflow
			next_ordinal:       app.backend.native_operations.next_ordinal
			sequence_exhausted: app.backend.native_operations.sequence_exhausted
			terminal_cause:     app.backend.native_operations.terminal_cause
			live_tickets:       app.backend.native_operations.lifetime_tickets.len
		}
	}

	fn renderer_fault_native_proof_capture_for_test(app &App) RendererFaultNativeProofCapture {
		proof := app.backend.native_operations.proof
		if proof == unsafe { nil } {
			return RendererFaultNativeProofCapture{}
		}
		return RendererFaultNativeProofCapture{
			available: true
			snapshot:  renderer_fault_native_proof_snapshot_from_for_test(app, proof)
		}
	}

	fn renderer_fault_lifetime_teardown_authority_for_test(app &App, value_identity u64, ticket_id u64) RendererFaultLifetimeTeardownAuthority {
		mut authority := RendererFaultLifetimeTeardownAuthority{
			value_identity: value_identity
			ticket_id:      ticket_id
		}
		for ticket in app.backend.native_operations.lifetime_tickets {
			if ticket.ticket_id != ticket_id {
				continue
			}
			authority = RendererFaultLifetimeTeardownAuthority{
				captured:                 true
				value_identity:           value_identity
				ticket_id:                ticket_id
				app_identity:             ticket.app_identity
				authority_scope:          ticket.authority_scope
				authority_token:          ticket.authority_token
				domain:                   ticket.domain
				release_kind:             ticket.release_kind
				owner_seed:               ticket.owner_seed
				proof_generation:         ticket.proof_generation
				context:                  ticket.context
				ticket_native_identity:   ticket.native_identity
				required_parent_identity: ticket.required_parent_identity
				parent_authority_scope:   ticket.parent_authority_scope
				parent_authority_token:   ticket.parent_authority_token
				state:                    ticket.state
			}
			break
		}
		return authority
	}

	fn renderer_fault_window_teardown_authority_for_test(app &App, window WindowId) RendererFaultWindowTeardownAuthority {
		match app.backend.kind {
			.x11 {
				$if linux && x_multiwindow_x11 ? {
					index := app.backend.x11.window_record_index(window) or {
						return RendererFaultWindowTeardownAuthority{}
					}
					record := app.backend.x11.windows[index]
					return RendererFaultWindowTeardownAuthority{
						captured:               true
						backend:                .x11
						window:                 window
						target_generation:      record.render_target_generation
						native_window_identity: u64(record.window)
						native_destroyed:       record.native_destroyed
						resources:              [
							renderer_fault_lifetime_teardown_authority_for_test(app,
								native_identity(record.egl_surface), record.egl_surface_ticket),
						]
					}
				} $else {
					return RendererFaultWindowTeardownAuthority{}
				}
			}
			.wayland {
				$if linux && sokol_wayland ? {
					index := app.backend.wayland.window_record_index(window) or {
						return RendererFaultWindowTeardownAuthority{}
					}
					record := app.backend.wayland.windows[index]
					mut resources := []RendererFaultLifetimeTeardownAuthority{}
					if record.frame_callback != unsafe { nil } || record.frame_callback_ticket != 0 {
						resources << renderer_fault_lifetime_teardown_authority_for_test(app,
							native_identity(record.frame_callback), record.frame_callback_ticket)
					}
					resources << renderer_fault_lifetime_teardown_authority_for_test(app,
						native_identity(record.egl_surface), record.egl_surface_ticket)
					resources << renderer_fault_lifetime_teardown_authority_for_test(app,
						native_identity(record.wl_egl_window), record.wl_egl_window_ticket)
					return RendererFaultWindowTeardownAuthority{
						captured:               true
						backend:                .wayland
						window:                 window
						target_generation:      record.render_target_generation
						native_window_identity: native_identity(record.surface)
						native_destroyed:       record.native_destroyed
						resources:              resources
					}
				} $else {
					return RendererFaultWindowTeardownAuthority{}
				}
			}
			.appkit {
				$if darwin {
					index := app.backend.appkit.window_record_index(window) or {
						return RendererFaultWindowTeardownAuthority{}
					}
					record := app.backend.appkit.windows[index]
					return RendererFaultWindowTeardownAuthority{
						captured:               true
						backend:                .appkit
						window:                 window
						target_generation:      record.render_target_generation
						native_window_identity: native_identity(record.state)
						native_destroyed:       record.native_destroyed
						resources:              [
							renderer_fault_lifetime_teardown_authority_for_test(app,
								native_identity(record.state), record.state_ticket),
						]
					}
				} $else {
					return RendererFaultWindowTeardownAuthority{}
				}
			}
			.win32 {
				$if windows {
					index := app.backend.win32.window_record_index(window) or {
						return RendererFaultWindowTeardownAuthority{}
					}
					record := app.backend.win32.windows[index]
					mut resources := [
						renderer_fault_lifetime_teardown_authority_for_test(app,
							native_identity(record.depth_stencil_view),
							record.depth_stencil_view_ticket),
						renderer_fault_lifetime_teardown_authority_for_test(app,
							native_identity(record.depth_texture), record.depth_texture_ticket),
						renderer_fault_lifetime_teardown_authority_for_test(app,
							native_identity(record.render_view), record.render_view_ticket),
					]
					if record.pending_backbuffer != unsafe { nil }
						|| record.pending_backbuffer_ticket != 0 {
						resources << renderer_fault_lifetime_teardown_authority_for_test(app,
							native_identity(record.pending_backbuffer),
							record.pending_backbuffer_ticket)
					}
					resources << renderer_fault_lifetime_teardown_authority_for_test(app,
						native_identity(record.swapchain), record.swapchain_ticket)
					return RendererFaultWindowTeardownAuthority{
						captured:               true
						backend:                .win32
						window:                 window
						target_generation:      record.render_target_generation
						native_window_identity: native_identity(record.hwnd)
						native_destroyed:       record.destroyed
						resources:              resources
					}
				} $else {
					return RendererFaultWindowTeardownAuthority{}
				}
			}
			else {
				return RendererFaultWindowTeardownAuthority{}
			}
		}
	}

	fn renderer_fault_native_proof_snapshot_for_test(app &App) RendererFaultNativeProofSnapshot {
		assert app.backend.native_operations.proof != unsafe { nil }
		proof := app.backend.native_operations.proof
		return renderer_fault_native_proof_snapshot_from_for_test(app, proof)
	}

	fn renderer_fault_assert_idle_state_for_test(snapshot RendererFaultStateSnapshot) {
		assert !snapshot.runtime_batch_active
		assert snapshot.runtime_active_batch_epoch == 0
		assert !snapshot.window_runtime.in_frame
		assert snapshot.window_runtime.lease_epoch == 0
		assert snapshot.window_runtime.batch_epoch == 0
		assert !snapshot.window_runtime.candidate_active
		assert snapshot.window_runtime.candidate_batch_epoch == 0
		assert snapshot.window_runtime.candidate_frame_serial == 0
		if snapshot.renderer_exists {
			assert !snapshot.renderer_batch_active
			assert snapshot.renderer_batch_epoch == 0
			assert snapshot.renderer_target_count == 0
			assert !snapshot.renderer_sokol_touched
			assert snapshot.renderer_completed_user_passes == 0
			assert !snapshot.renderer_anchor_acquired
			assert !snapshot.renderer_anchor_pass_closed
			assert snapshot.renderer_failure_messages.len == 0
		}
		assert snapshot.appkit_batch_pool == 0
		assert snapshot.appkit_batch_pool_ticket == 0
		assert snapshot.appkit_active_anchor_lease == 0
		assert snapshot.appkit_active_anchor_drawable == 0
		assert snapshot.appkit_active_anchor_ticket == 0
		assert !snapshot.appkit_window_frame_active
		assert snapshot.appkit_window_frame_lease == 0
		assert snapshot.appkit_window_drawable == 0
		assert snapshot.appkit_window_drawable_ticket == 0
	}

	fn renderer_fault_assert_delivery_suffix_for_test(snapshot RendererFaultDeliverySuffixSnapshot) {
		assert snapshot.before.next_token != 0
		assert snapshot.after.next_token != 0
		assert snapshot.after.next_token >= snapshot.before.next_token
		assert snapshot.after.events.len >= snapshot.before.events.len
		assert snapshot.accepted.len == snapshot.after.events.len - snapshot.before.events.len
		for index, event in snapshot.before.events {
			assert snapshot.after.events[index] == event
		}

		mut expected_deliveries := map[u64]EventDeliveryState{}
		for token, state in snapshot.before.deliveries {
			expected_deliveries[token] = state
		}
		mut previous_token := snapshot.before.next_token - 1
		for index, event in snapshot.accepted {
			assert snapshot.after.events[snapshot.before.events.len + index] == event
			assert event.delivery_token >= snapshot.before.next_token
			assert event.delivery_token > previous_token
			assert event.delivery_token < snapshot.after.next_token
			assert event.delivery_token !in expected_deliveries
			expected_deliveries[event.delivery_token] = .queued
			previous_token = event.delivery_token
		}
		assert snapshot.after.deliveries.len == expected_deliveries.len
		for token, state in expected_deliveries {
			assert token in snapshot.after.deliveries
			assert snapshot.after.deliveries[token] == state
		}
	}

	fn renderer_fault_replay_mark_dirty_for_test(mut replay RendererFaultHarvestReplayState) ! {
		if replay.window_runtime.dirty_epoch > replay.window_runtime.consumed_dirty_epoch
			&& (!replay.window_runtime.in_frame
			|| replay.window_runtime.dirty_epoch != replay.window_runtime.claimed_dirty_epoch) {
			return
		}
		epoch, next_epoch := plan_nonwrapping_counter(replay.runtime_next_epoch)!
		replay.window_runtime.dirty_epoch = epoch
		replay.runtime_next_epoch = next_epoch
	}

	fn renderer_fault_replay_unavailable_observation_for_test(mut replay RendererFaultHarvestReplayState, logical_width int, logical_height int, framebuffer_width int, framebuffer_height int, reason RenderBlockReason) ! {
		replay.window_runtime.metrics = RenderMetricsSnapshot{
			logical_width:        if logical_width > 0 { f32(logical_width) } else { 0 }
			logical_height:       if logical_height > 0 { f32(logical_height) } else { 0 }
			framebuffer_width:    if framebuffer_width > 0 { framebuffer_width } else { 0 }
			framebuffer_height:   if framebuffer_height > 0 { framebuffer_height } else { 0 }
			dpi_scale:            0
			metrics_sequence:     0
			metrics_available:    false
			conversion_available: false
		}
		replay.window_runtime.target = RenderTargetSnapshot{}
		replay.window_runtime.ready_credit = false
		replay.window_runtime.ready_credit_consumed = false
		replay.window_runtime.block_reason = reason
		renderer_fault_replay_mark_dirty_for_test(mut replay)!
	}

	fn renderer_fault_replay_resize_metrics_for_test(mut replay RendererFaultHarvestReplayState, backend BackendKind, event InputEvent) {
		mut logical_width := f32(event.window_width)
		mut logical_height := f32(event.window_height)
		mut dpi_scale := f32(1)
		match backend {
			.appkit {
				dpi_scale = replay.readiness_metrics.dpi_scale
				assert dpi_scale > 0
				assert int(f32(event.window_width) * dpi_scale) == event.framebuffer_width
				assert int(f32(event.window_height) * dpi_scale) == event.framebuffer_height
			}
			.win32 {
				dpi_scale = replay.readiness_metrics.dpi_scale
				assert dpi_scale > 0
				logical_width = f32(event.framebuffer_width) / dpi_scale
				logical_height = f32(event.framebuffer_height) / dpi_scale
			}
			.x11, .wayland {
				assert event.window_width == event.framebuffer_width
				assert event.window_height == event.framebuffer_height
			}
			else {
				assert false, 'renderer stop harvest selected unsupported resize backend'
			}
		}

		replay.readiness_metrics = RenderMetricsSnapshot{
			logical_width:        logical_width
			logical_height:       logical_height
			framebuffer_width:    event.framebuffer_width
			framebuffer_height:   event.framebuffer_height
			dpi_scale:            dpi_scale
			metrics_available:    true
			conversion_available: replay.readiness_metrics.conversion_available
		}
	}

	fn renderer_fault_replay_harvest_observations_for_test(before RendererFaultStateSnapshot, snapshot RendererFaultDeliverySuffixSnapshot, backend BackendKind) !RendererFaultHarvestReplayState {
		mut replay := RendererFaultHarvestReplayState{
			window_runtime:       before.window_runtime
			window_config_width:  before.window_config_width
			window_config_height: before.window_config_height
			runtime_next_epoch:   before.runtime_next_epoch
			readiness_metrics:    before.window_runtime.metrics
			readiness_target:     before.window_runtime.target
		}
		for queued in snapshot.accepted {
			match queued.kind {
				.lifecycle {
					event := queued.lifecycle
					assert event.window_id == before.window
					match event.kind {
						.window_destroyed {
							assert false, 'renderer stop harvest must not destroy the live fixture window'
						}
						.window_resized {
							assert event.width > 0
							assert event.height > 0
							assert replay.pending_resize_width == 0
							assert replay.pending_resize_height == 0
							replay.window_config_width = event.width
							replay.window_config_height = event.height
							replay.pending_resize_width = event.width
							replay.pending_resize_height = event.height
							replay.resize_events++
							if backend != .appkit {
								replay.readiness_target = RenderTargetSnapshot{
									...replay.readiness_target
									target_identity: next_backend_target_generation(replay.readiness_target.target_identity)!
								}
							}
							renderer_fault_replay_unavailable_observation_for_test(mut replay,
								event.width, event.height, 0, 0, .resize_pending)!
						}
						else {}
					}
				}
				.input {
					event := queued.input
					assert event.window_id == before.window
					match event.kind {
						.focused {
							replay.window_runtime.focus_known = true
							replay.window_runtime.focused = true
						}
						.unfocused {
							replay.window_runtime.focus_known = true
							replay.window_runtime.focused = false
						}
						.iconified {
							replay.window_runtime.minimized_known = true
							replay.window_runtime.minimized = true
						}
						.restored {
							replay.window_runtime.minimized_known = true
							replay.window_runtime.minimized = false
						}
						.resized {
							assert event.window_width > 0
							assert event.window_height > 0
							assert event.framebuffer_width > 0
							assert event.framebuffer_height > 0
							assert replay.pending_resize_width == event.window_width
							assert replay.pending_resize_height == event.window_height
							replay.resize_input_events++
							replay.pending_resize_width = 0
							replay.pending_resize_height = 0
							if backend == .appkit {
								replay.readiness_target = RenderTargetSnapshot{
									...replay.readiness_target
									target_identity: next_backend_target_generation(replay.readiness_target.target_identity)!
								}
							}
							renderer_fault_replay_resize_metrics_for_test(mut replay, backend,
								event)
						}
						else {}
					}

					if event.kind in [.resized, .focused, .unfocused, .iconified, .restored] {
						reason := if replay.window_runtime.minimized_known
							&& replay.window_runtime.minimized {
							RenderBlockReason.minimized
						} else if event.kind == .resized {
							RenderBlockReason.resize_pending
						} else {
							RenderBlockReason.backend_unavailable
						}
						renderer_fault_replay_unavailable_observation_for_test(mut replay,
							event.window_width, event.window_height, event.framebuffer_width,
							event.framebuffer_height, reason)!
					}
				}
				.service {
					event := queued.service
					assert event.window == before.window
					assert event.kind in [.state, .metrics]
					assert event.metrics.metrics_available
					assert event.metrics.conversion_available
					assert event.metrics.logical_width > 0
					assert event.metrics.logical_height > 0
					assert event.metrics.framebuffer_width > 0
					assert event.metrics.framebuffer_height > 0
					assert event.metrics.dpi_scale > 0
					assert event.metrics.metrics_sequence == queued.delivery_token
				}
				.readback {
					assert false, 'renderer stop harvest fixture must not enqueue readback events'
				}
			}
		}
		return replay
	}

	fn renderer_fault_assert_prepared_stop_transition_for_test(before RendererFaultStateSnapshot, prepared RendererFaultStateSnapshot, ticket AppStopTicket, delivery_suffix RendererFaultDeliverySuffixSnapshot, backend BackendKind, callback_after RendererFaultWaylandFrameCallbackState) ! {
		renderer_fault_assert_idle_state_for_test(before)
		renderer_fault_assert_idle_state_for_test(prepared)
		renderer_fault_assert_delivery_suffix_for_test(delivery_suffix)
		assert before.app_status == .running
		assert !before.app_stopping
		assert before.admission_open
		assert !before.stop_prepared
		assert before.stop_serial == 0
		assert before.stop_terminal == ''
		assert before.pending_stop_errors.len == 0
		assert ticket.app_instance == before.window.app_instance
		assert ticket.serial == 1
		assert prepared.app_status == .running
		assert prepared.app_stopping
		assert !prepared.admission_open
		assert prepared.stop_prepared
		assert prepared.stop_serial == ticket.serial
		assert prepared.stop_terminal == ''
		assert prepared.pending_stop_errors.len == 0

		// Accepted native focus/minimize/resize observations and one checked backend
		// update are the only window payload changes permitted during prepare.
		replay := renderer_fault_replay_harvest_observations_for_test(before, delivery_suffix,
			backend)!
		assert replay.pending_resize_width == 0
		assert replay.pending_resize_height == 0
		assert replay.resize_input_events == replay.resize_events
		assert before.window_runtime.eligibility_sequence == before.window_runtime.metrics.metrics_sequence
		expected_sequence := next_nonwrapping_u64(before.window_runtime.eligibility_sequence)!
		mut expected_window := replay.window_runtime
		expected_window.eligibility_sequence = expected_sequence
		expected_window.ready_credit_consumed = false
		expected_window.metrics = RenderMetricsSnapshot{
			...replay.readiness_metrics
			metrics_sequence: expected_sequence
		}
		expected_window.target = replay.readiness_target
		expected_window.block_reason = if expected_window.minimized_known
			&& expected_window.minimized {
			RenderBlockReason.minimized
		} else if backend == .wayland && callback_after.callback_identity != 0 {
			RenderBlockReason.frame_callback_pending
		} else if backend == .win32 && replay.resize_events != 0 {
			RenderBlockReason.resize_pending
		} else {
			RenderBlockReason.none
		}
		expected_window.ready_credit = expected_window.block_reason == .none
		assert prepared.window_runtime == expected_window
		assert prepared.window_runtime.eligibility_sequence == expected_sequence
		assert prepared.window_runtime.metrics.metrics_sequence == expected_sequence
		assert !prepared.window_runtime.ready_credit_consumed
		assert prepared.window_runtime.ready_credit == (prepared.window_runtime.block_reason == .none)
		assert prepared.window_runtime.metrics.metrics_available
		assert prepared.window_runtime.metrics.logical_width > 0
		assert prepared.window_runtime.metrics.logical_height > 0
		assert prepared.window_runtime.metrics.framebuffer_width > 0
		assert prepared.window_runtime.metrics.framebuffer_height > 0
		assert prepared.window_runtime.metrics.dpi_scale > 0
		assert !prepared.window_runtime.metrics.conversion_available
			|| prepared.window_runtime.metrics.metrics_available
		assert prepared.window_runtime.target.target_identity != 0
		assert prepared.window_runtime.target.color_format != 0
		assert prepared.window_runtime.target.depth_format != 0
		assert prepared.window_runtime.target.sample_count > 0

		expected_prepared := RendererFaultStateSnapshot{
			...before
			app_stopping:         true
			admission_open:       false
			stop_prepared:        true
			stop_serial:          ticket.serial
			window_config_width:  replay.window_config_width
			window_config_height: replay.window_config_height
			window_runtime:       prepared.window_runtime
			runtime_next_epoch:   replay.runtime_next_epoch
		}
		assert prepared == expected_prepared
	}

	fn renderer_fault_assert_resource_only_terminal_transition_for_test(before RendererFaultStateSnapshot, after RendererFaultStateSnapshot, message string) {
		renderer_fault_assert_idle_state_for_test(before)
		renderer_fault_assert_idle_state_for_test(after)
		assert before.window == after.window
		assert before.app_status == .running
		assert after.app_status == .running
		assert !before.app_stopping && !after.app_stopping
		assert before.admission_open && after.admission_open
		assert before.stop_prepared == after.stop_prepared
		assert before.stop_serial == after.stop_serial
		assert before.stop_terminal == after.stop_terminal
		assert before.pending_stop_errors == after.pending_stop_errors
		assert before.window_slot_status == after.window_slot_status
		assert before.window_destroy_stage == after.window_destroy_stage
		assert before.window_destroy_serial == after.window_destroy_serial
		assert before.window_backend_destroyed == after.window_backend_destroyed
		assert before.window_destroy_terminal == after.window_destroy_terminal
		assert before.window_teardown_sequence == after.window_teardown_sequence
		mut expected_window := before.window_runtime
		expected_window.candidate_metrics = before.window_runtime.metrics
		expected_window.candidate_target = before.window_runtime.target
		assert after.window_runtime == expected_window
		assert after.window_runtime.frame_serial == before.window_runtime.frame_serial
		assert after.window_runtime.submitted_frame == before.window_runtime.submitted_frame
		assert after.window_runtime.dirty_epoch == before.window_runtime.dirty_epoch
		assert after.window_runtime.consumed_dirty_epoch == before.window_runtime.consumed_dirty_epoch
		assert after.window_runtime.claimed_dirty_epoch == before.window_runtime.claimed_dirty_epoch
		assert after.runtime_windows == before.runtime_windows
		assert after.runtime_next_epoch == before.runtime_next_epoch
		assert after.runtime_next_admission_id == before.runtime_next_admission_id
		assert after.runtime_next_lease_epoch == before.runtime_next_lease_epoch
		assert after.runtime_next_batch_epoch == before.runtime_next_batch_epoch + 1
		assert after.runtime_next_destroy_serial == before.runtime_next_destroy_serial
		assert after.runtime_next_teardown_sequence == before.runtime_next_teardown_sequence
		assert after.runtime_renderer_terminal == '${err_render_terminal_aggregate}: ${message}'
		assert before.renderer_exists && after.renderer_exists
		assert before.renderer_started == after.renderer_started
		assert before.renderer_anchor_created == after.renderer_anchor_created
		assert before.renderer_next_target_epoch == after.renderer_next_target_epoch
		assert !before.renderer_terminal
		assert after.renderer_terminal
		assert before.renderer_device_lost == after.renderer_device_lost
		assert before.renderer_native_health == after.renderer_native_health
		assert before.renderer_shutdown_path == after.renderer_shutdown_path
		assert before.renderer_shutdown_prepared == after.renderer_shutdown_prepared
		assert before.renderer_shutdown_batch_scope == after.renderer_shutdown_batch_scope
		assert before.renderer_fault_trace_token == after.renderer_fault_trace_token
		assert before.backend_windows == after.backend_windows
		assert before.backend_anchor_generation == after.backend_anchor_generation
		assert before.backend_anchor_identity == after.backend_anchor_identity
		assert before.backend_anchor_ticket == after.backend_anchor_ticket
		assert before.backend_renderer_device_identity == after.backend_renderer_device_identity
		assert before.backend_renderer_device_ticket == after.backend_renderer_device_ticket
		assert before.backend_renderer_context_identity == after.backend_renderer_context_identity
		assert before.backend_renderer_context_ticket == after.backend_renderer_context_ticket
		assert before.backend_factory_identity == after.backend_factory_identity
		assert before.backend_factory_ticket == after.backend_factory_ticket
		assert before.backend_display_identity == after.backend_display_identity
		assert before.backend_display_ticket == after.backend_display_ticket
		assert before.wayland_display_identity == after.wayland_display_identity
		assert before.backend_owner_thread_identity == after.backend_owner_thread_identity
		assert before.backend_thread_ticket == after.backend_thread_ticket
		assert before.win32_anchor_color_texture == after.win32_anchor_color_texture
		assert before.win32_anchor_color_ticket == after.win32_anchor_color_ticket
		assert before.win32_anchor_render_view == after.win32_anchor_render_view
		assert before.win32_anchor_render_view_ticket == after.win32_anchor_render_view_ticket
		assert before.win32_anchor_depth_texture == after.win32_anchor_depth_texture
		assert before.win32_anchor_depth_ticket == after.win32_anchor_depth_ticket
		assert before.win32_anchor_depth_view == after.win32_anchor_depth_view
		assert before.win32_anchor_depth_view_ticket == after.win32_anchor_depth_view_ticket
	}

	fn renderer_fault_assert_finalize_terminal_transition_for_test(before RendererFaultStateSnapshot, after RendererFaultStateSnapshot, message string) {
		renderer_fault_assert_idle_state_for_test(before)
		renderer_fault_assert_idle_state_for_test(after)
		assert before.window == after.window
		assert before.app_status == .running && after.app_status == .running
		assert !before.app_stopping && !after.app_stopping
		assert before.admission_open && after.admission_open
		assert before.window_slot_status == after.window_slot_status
		assert before.window_destroy_stage == after.window_destroy_stage
		assert before.window_destroy_serial == after.window_destroy_serial
		assert before.window_backend_destroyed == after.window_backend_destroyed
		assert before.window_destroy_terminal == after.window_destroy_terminal
		assert before.window_teardown_sequence == after.window_teardown_sequence
		mut expected_window := before.window_runtime
		expected_window.claimed_dirty_epoch = before.window_runtime.dirty_epoch
		expected_window.frame_serial = before.window_runtime.frame_serial + 1
		expected_window.ready_credit = false
		expected_window.ready_credit_consumed = true
		expected_window.candidate_metrics = before.window_runtime.metrics
		expected_window.candidate_target = before.window_runtime.target
		assert after.window_runtime == expected_window
		assert after.window_runtime.frame_serial == before.window_runtime.frame_serial + 1
		assert after.window_runtime.submitted_frame == before.window_runtime.submitted_frame
		assert after.window_runtime.dirty_epoch == before.window_runtime.dirty_epoch
		assert after.window_runtime.consumed_dirty_epoch == before.window_runtime.consumed_dirty_epoch
		assert after.window_runtime.claimed_dirty_epoch == before.window_runtime.dirty_epoch
		assert after.window_runtime.dirty_epoch > after.window_runtime.consumed_dirty_epoch
		assert after.runtime_windows == before.runtime_windows
		assert after.runtime_next_epoch == before.runtime_next_epoch
		assert after.runtime_next_admission_id == before.runtime_next_admission_id
		assert after.runtime_next_lease_epoch == before.runtime_next_lease_epoch + 1
		assert after.runtime_next_batch_epoch == before.runtime_next_batch_epoch + 1
		assert after.runtime_next_destroy_serial == before.runtime_next_destroy_serial
		assert after.runtime_next_teardown_sequence == before.runtime_next_teardown_sequence
		assert after.runtime_renderer_terminal == '${err_render_terminal_aggregate}: ${message}'
		assert before.renderer_exists && after.renderer_exists
		assert before.renderer_started == after.renderer_started
		assert before.renderer_anchor_created == after.renderer_anchor_created
		assert after.renderer_next_target_epoch == before.renderer_next_target_epoch + 1
		assert !before.renderer_terminal
		assert after.renderer_terminal
		assert before.renderer_device_lost == after.renderer_device_lost
		assert before.renderer_native_health == after.renderer_native_health
		assert before.renderer_shutdown_path == after.renderer_shutdown_path
		assert before.renderer_shutdown_prepared == after.renderer_shutdown_prepared
		assert before.renderer_shutdown_batch_scope == after.renderer_shutdown_batch_scope
		assert before.renderer_fault_trace_token == after.renderer_fault_trace_token
		assert before.backend_windows == after.backend_windows
		assert before.backend_anchor_generation == after.backend_anchor_generation
		assert before.backend_anchor_identity == after.backend_anchor_identity
		assert before.backend_anchor_ticket == after.backend_anchor_ticket
		assert before.backend_renderer_device_identity == after.backend_renderer_device_identity
		assert before.backend_renderer_device_ticket == after.backend_renderer_device_ticket
		assert before.backend_renderer_context_identity == after.backend_renderer_context_identity
		assert before.backend_renderer_context_ticket == after.backend_renderer_context_ticket
		assert before.backend_factory_identity == after.backend_factory_identity
		assert before.backend_factory_ticket == after.backend_factory_ticket
		assert before.backend_display_identity == after.backend_display_identity
		assert before.backend_display_ticket == after.backend_display_ticket
		assert before.wayland_display_identity == after.wayland_display_identity
		assert before.backend_owner_thread_identity == after.backend_owner_thread_identity
		assert before.backend_thread_ticket == after.backend_thread_ticket
		assert before.win32_anchor_color_texture == after.win32_anchor_color_texture
		assert before.win32_anchor_color_ticket == after.win32_anchor_color_ticket
		assert before.win32_anchor_render_view == after.win32_anchor_render_view
		assert before.win32_anchor_render_view_ticket == after.win32_anchor_render_view_ticket
		assert before.win32_anchor_depth_texture == after.win32_anchor_depth_texture
		assert before.win32_anchor_depth_ticket == after.win32_anchor_depth_ticket
		assert before.win32_anchor_depth_view == after.win32_anchor_depth_view
		assert before.win32_anchor_depth_view_ticket == after.win32_anchor_depth_view_ticket
	}

	fn renderer_fault_assert_first_stop_state_for_test(prepared RendererFaultStateSnapshot, stopped RendererFaultStateSnapshot, expected_renderer_terminal string) {
		assert prepared.app_status == .running
		assert prepared.app_stopping
		assert !prepared.admission_open
		assert prepared.stop_prepared
		assert prepared.stop_serial == 1
		assert stopped.app_status == .stopped
		assert !stopped.app_stopping
		assert !stopped.admission_open
		assert stopped.stop_prepared
		assert stopped.stop_serial == prepared.stop_serial
		assert stopped.stop_terminal == ''
		assert stopped.pending_stop_errors.len == 0
		assert stopped.window == prepared.window
		assert stopped.window_slot_status == .destroyed
		assert stopped.window_destroy_stage == .finished
		assert stopped.window_destroy_serial != 0
		assert !stopped.window_backend_destroyed
		assert stopped.window_destroy_terminal == ''
		assert stopped.window_teardown_sequence == 0
		assert stopped.window_config_width == prepared.window_config_width
		assert stopped.window_config_height == prepared.window_config_height
		mut expected_window := prepared.window_runtime
		expected_window.status = .destroyed
		expected_window.pending_admission = false
		expected_window.pending_admission_id = 0
		expected_window.pending_admission_epoch = 0
		expected_window.ready_credit = false
		expected_window.in_frame = false
		expected_window.lease_epoch = 0
		expected_window.batch_epoch = 0
		assert stopped.window_runtime == expected_window
		assert stopped.runtime_windows == prepared.runtime_windows
		assert stopped.runtime_next_epoch == prepared.runtime_next_epoch
		assert stopped.runtime_next_admission_id == prepared.runtime_next_admission_id
		assert stopped.runtime_next_lease_epoch == prepared.runtime_next_lease_epoch
		assert stopped.runtime_next_batch_epoch == prepared.runtime_next_batch_epoch
		assert stopped.runtime_next_destroy_serial == prepared.runtime_next_destroy_serial + 1
		assert stopped.runtime_next_teardown_sequence == prepared.runtime_next_teardown_sequence
		assert stopped.runtime_active_batch_epoch == 0
		assert !stopped.runtime_batch_active
		assert stopped.runtime_renderer_terminal == expected_renderer_terminal
		assert !stopped.renderer_exists
		assert !stopped.renderer_started
		assert !stopped.renderer_anchor_created
		assert !stopped.renderer_batch_active
		assert stopped.renderer_batch_epoch == 0
		assert stopped.renderer_next_target_epoch == 0
		assert stopped.renderer_target_count == 0
		assert !stopped.renderer_sokol_touched
		assert stopped.renderer_completed_user_passes == 0
		assert !stopped.renderer_anchor_acquired
		assert !stopped.renderer_anchor_pass_closed
		assert stopped.renderer_failure_messages.len == 0
		assert !stopped.renderer_terminal
		assert !stopped.renderer_device_lost
		assert stopped.renderer_native_health == .uninitialized
		assert stopped.renderer_shutdown_path == .unprepared
		assert !stopped.renderer_shutdown_prepared
		assert !stopped.renderer_shutdown_batch_scope
		assert stopped.renderer_fault_trace_token == 0
		assert stopped.backend_windows == 0
		if prepared.backend_anchor_generation != 0 {
			assert stopped.backend_anchor_generation == exhaust_backend_target_generation(prepared.backend_anchor_generation)
		} else {
			assert stopped.backend_anchor_generation == 0
		}
		assert stopped.backend_anchor_identity == 0
		assert stopped.backend_anchor_ticket == 0
		assert stopped.backend_renderer_device_identity == 0
		assert stopped.backend_renderer_device_ticket == 0
		assert stopped.backend_renderer_context_identity == 0
		assert stopped.backend_renderer_context_ticket == 0
		assert stopped.backend_factory_identity == 0
		assert stopped.backend_factory_ticket == 0
		assert stopped.backend_display_identity == 0
		assert stopped.backend_display_ticket == 0
		assert stopped.wayland_display_identity == 0
		assert stopped.backend_owner_thread_identity == prepared.backend_owner_thread_identity
		assert stopped.backend_thread_ticket == 0
		assert stopped.win32_anchor_color_texture == 0
		assert stopped.win32_anchor_color_ticket == 0
		assert stopped.win32_anchor_render_view == 0
		assert stopped.win32_anchor_render_view_ticket == 0
		assert stopped.win32_anchor_depth_texture == 0
		assert stopped.win32_anchor_depth_ticket == 0
		assert stopped.win32_anchor_depth_view == 0
		assert stopped.win32_anchor_depth_view_ticket == 0
		assert stopped.appkit_batch_pool == 0
		assert stopped.appkit_batch_pool_ticket == 0
		assert stopped.appkit_active_anchor_lease == 0
		assert stopped.appkit_active_anchor_drawable == 0
		assert stopped.appkit_active_anchor_ticket == 0
		assert !stopped.appkit_window_frame_active
		assert stopped.appkit_window_frame_lease == 0
		assert stopped.appkit_window_drawable == 0
		assert stopped.appkit_window_drawable_ticket == 0
	}

	fn renderer_fault_assert_first_stop_proof_for_test(fresh RendererFaultNativeProofSnapshot, stopped RendererFaultNativeProofSnapshot, backend BackendKind) {
		assert stopped.generation == fresh.generation
		assert stopped.ordinal_floor == fresh.ordinal_floor
		assert stopped.accepting_plans
		assert stopped.plan == fresh.plan
		for entry in stopped.plan {
			assert entry == NativePrimitivePlanEntry{}
		}
		assert stopped.trace_len > 0
		assert !stopped.trace_overflow
		if backend == .wayland {
			assert stopped.next_ordinal > fresh.next_ordinal
		} else {
			assert stopped.next_ordinal == fresh.next_ordinal
		}
		assert !stopped.sequence_exhausted
		assert stopped.terminal_cause == .none
		assert stopped.live_tickets == 0
		for index in 0 .. stopped.trace_len {
			entry := stopped.trace[index]
			if entry.milestone == .acceptance || entry.milestone == .authority_release {
				assert entry.result.disposition == .ok
				assert entry.result.error_text == ''
			}
			if entry.milestone == .health_latched {
				assert entry.health == .ready
			}
		}
	}

	fn renderer_fault_assert_wayland_frame_callback_state_for_test(app &App, window WindowId, state RendererFaultWaylandFrameCallbackState) {
		assert state.captured
		assert state.target_generation != 0
		assert state.parent_identity != 0
		assert (state.callback_identity == 0) == (state.callback_ticket == 0)
		if state.callback_identity == 0 {
			assert state.ticket_count == 0
			return
		}
		assert state.ticket_count == 1
		assert state.ticket_release_kind == .wayland_frame_callback
		assert state.ticket_state == .bound
		assert state.ticket_native_identity == state.callback_identity
		assert state.ticket_parent_identity == state.parent_identity
		assert state.ticket_owner_window == window
		assert state.ticket_owner_generation == state.target_generation
		renderer_fault_assert_scoped_authority_context_for_test(app, state.ticket_context)
		assert state.ticket_context.domain == .wayland
		assert state.ticket_context.operation == .surface_destroy
		assert state.ticket_context.call_site == .window_finalize
		assert state.ticket_context.scope == .window_target
		assert state.ticket_context.window == window
		assert state.ticket_context.target_generation == state.target_generation
		assert state.ticket_context.target_identity == state.callback_identity
	}

	fn renderer_fault_consume_wayland_harvest_callback_for_test(app &App, proof &NativeOperationProofState, start int, end int, release_expected bool, release_consumed bool, callback RendererFaultWaylandFrameCallbackState) (int, bool) {
		if !release_expected || release_consumed || start + 6 > end
			|| proof.trace[start].context != callback.ticket_context {
			return start, release_consumed
		}
		next := renderer_fault_assert_lifetime_release_chain_for_test(app, proof, start, .wayland,
			.surface_destroy, .window_finalize, .window_target,
			callback.ticket_context.authority_scope, callback.ticket_context.ordinal,
			callback.callback_identity)
		assert proof.trace[start].context == callback.ticket_context
		return next, true
	}

	fn renderer_fault_assert_wayland_harvest_transport_at_for_test(app &App, proof &NativeOperationProofState, start int, end int, operation NativeRenderOperation, ordinal u64, target_identity u64, display_identity u64, disposition NativeRenderDisposition) (int, NativeOperationContext) {
		assert start + 8 <= end
		next, context := renderer_fault_assert_wayland_chain_with_disposition_at_for_test(app,
			proof, start, operation, .display_transport, .batch, disposition)
		assert context.ordinal == ordinal
		assert context.presence_mask == native_context_has_target_identity
		assert context.target_identity == target_identity
		evidence_context := proof.trace[start + 3].context
		assert evidence_context.target_identity == display_identity
		return next, context
	}

	fn renderer_fault_assert_wayland_roundtrip_segment_for_test(app &App, proof &NativeOperationProofState, expected_ordinal u64, display_identity u64) int {
		assert app.backend.kind == .wayland
		assert expected_ordinal != 0
		assert display_identity != 0
		assert proof.trace_len == 8
		mut index := 0
		mut context := NativeOperationContext{}
		index, context = renderer_fault_assert_wayland_chain_at_for_test(app, proof, index,
			.display_roundtrip, .display_transport, .renderer)
		assert context.ordinal == expected_ordinal
		assert context.presence_mask == native_context_has_target_identity
		assert context.window == WindowId{}
		assert context.target_generation == 0
		assert context.target_identity == display_identity
		assert context.batch_epoch == 0
		assert context.window_lease_epoch == 0
		assert context.target_lease_epoch == 0
		assert proof.trace[3].context.target_identity == display_identity
		actual := proof.trace[1].actual
		assert actual.has(native_valid_return_value)
		assert actual.return_value >= 0
		assert index == 8
		assert index == proof.trace_len
		return index
	}

	fn renderer_fault_assert_wayland_harvest_dispatch_for_test(app &App, proof &NativeOperationProofState, start int, end int, ordinal u64, display_identity u64, release_expected bool, release_consumed bool, callback RendererFaultWaylandFrameCallbackState) (int, u64, bool, i64) {
		mut index, mut callback_released := renderer_fault_consume_wayland_harvest_callback_for_test(app,
			proof, start, end, release_expected, release_consumed, callback)
		dispatch_start := index
		mut dispatch := NativeOperationContext{}
		index, dispatch = renderer_fault_assert_wayland_harvest_transport_at_for_test(app, proof,
			index, end, .display_dispatch, ordinal, display_identity, display_identity, .ok)
		actual := proof.trace[dispatch_start + 1].actual
		assert actual.has(native_valid_return_value)
		assert actual.return_value >= 0
		return index, ordinal + 2, callback_released, actual.return_value
	}

	fn renderer_fault_assert_wayland_harvest_dispatch_until_empty_for_test(app &App, proof &NativeOperationProofState, start int, end int, ordinal u64, display_identity u64, release_expected bool, release_consumed bool, callback RendererFaultWaylandFrameCallbackState) (int, u64, bool) {
		mut index := start
		mut next_ordinal := ordinal
		mut callback_released := release_consumed
		for {
			mut return_value := i64(0)
			index, next_ordinal, callback_released, return_value = renderer_fault_assert_wayland_harvest_dispatch_for_test(app,
				proof, index, end, next_ordinal, display_identity, release_expected,
				callback_released, callback)
			if return_value == 0 {
				break
			}
		}
		return index, next_ordinal, callback_released
	}

	fn renderer_fault_assert_wayland_flush_actual_for_test(actual NativePrimitiveEvidence) {
		assert actual.has(native_valid_return_value)
		has_errno := actual.has(native_valid_errno)
		assert (actual.return_value >= 0 && !has_errno)
			|| (actual.return_value == -1 && has_errno && actual.native_errno == 11)
	}

	fn renderer_fault_assert_wayland_harvest_trace_for_test(app &App, proof &NativeOperationProofState, start int, end int, ordinal u64, display_identity u64, release_expected bool, callback RendererFaultWaylandFrameCallbackState) (int, u64, bool) {
		assert display_identity != 0
		mut index := start
		mut next_ordinal := ordinal
		mut callback_released := false
		mut flush := NativeOperationContext{}
		index, flush = renderer_fault_assert_wayland_harvest_transport_at_for_test(app, proof,
			index, end, .display_flush, next_ordinal, display_identity, display_identity, .ok)
		flush_actual := proof.trace[start + 1].actual
		renderer_fault_assert_wayland_flush_actual_for_test(flush_actual)
		next_ordinal += 2
		index, next_ordinal, callback_released = renderer_fault_assert_wayland_harvest_dispatch_until_empty_for_test(app,
			proof, index, end, next_ordinal, display_identity, release_expected, callback_released,
			callback)

		for {
			prepare_start := index
			assert prepare_start + 8 <= end
			prepare_actual := proof.trace[prepare_start + 1].actual
			assert prepare_actual.has(native_valid_return_value)
			prepare_disposition := if prepare_actual.return_value < 0 {
				NativeRenderDisposition.transient
			} else {
				NativeRenderDisposition.ok
			}
			prepare_ordinal := next_ordinal
			mut prepare := NativeOperationContext{}
			index, prepare = renderer_fault_assert_wayland_harvest_transport_at_for_test(app,
				proof, index, end, .display_prepare, prepare_ordinal, display_identity,
				display_identity, prepare_disposition)
			next_ordinal = prepare_ordinal + 10
			if prepare_actual.return_value < 0 {
				mut dispatch_return := i64(0)
				index, next_ordinal, callback_released, dispatch_return = renderer_fault_assert_wayland_harvest_dispatch_for_test(app,
					proof, index, end, next_ordinal, display_identity, release_expected,
					callback_released, callback)
				if dispatch_return == 0 {
					break
				}
				continue
			}
			assert prepare_actual.return_value == 0

			fd_start := index
			mut fd_context := NativeOperationContext{}
			index, fd_context = renderer_fault_assert_wayland_harvest_transport_at_for_test(app,
				proof, index, end, .display_fd_query, prepare_ordinal + 2, display_identity,
				display_identity, .ok)
			fd_actual := proof.trace[fd_start + 1].actual
			assert fd_actual.has(native_valid_return_value)
			assert fd_actual.return_value >= 0

			poll_start := index
			mut poll_context := NativeOperationContext{}
			index, poll_context = renderer_fault_assert_wayland_harvest_transport_at_for_test(app,
				proof, index, end, .display_poll, prepare_ordinal + 4, u64(fd_actual.return_value),
				display_identity, .ok)
			poll_actual := proof.trace[poll_start + 1].actual
			assert poll_actual.has(native_valid_return_value)
			assert poll_actual.has(native_valid_observed_flags)
			assert poll_actual.return_value >= 0
			if poll_actual.return_value == 0 || poll_actual.observed_flags & u64(1) == 0 {
				cancel_start := index
				mut cancel := NativeOperationContext{}
				index, cancel = renderer_fault_assert_wayland_harvest_transport_at_for_test(app,
					proof, index, end, .display_cancel, prepare_ordinal + 8, display_identity,
					display_identity, .ok)
				assert index < end
				release := proof.trace[index]
				assert release.milestone == .authority_release
				assert release.context == cancel
				assert release.actual == proof.trace[cancel_start + 6].actual
				assert release.effective == proof.trace[cancel_start + 6].effective
				assert release.result == proof.trace[cancel_start + 6].result
				index++
				break
			}

			read_start := index
			mut read := NativeOperationContext{}
			index, read = renderer_fault_assert_wayland_harvest_transport_at_for_test(app, proof,
				index, end, .display_read, prepare_ordinal + 6, display_identity, display_identity, .ok)
			read_actual := proof.trace[read_start + 1].actual
			assert read_actual.has(native_valid_return_value)
			assert read_actual.return_value >= 0
			index, next_ordinal, callback_released = renderer_fault_assert_wayland_harvest_dispatch_until_empty_for_test(app,
				proof, index, end, next_ordinal, display_identity, release_expected,
				callback_released, callback)
		}
		assert callback_released == release_expected
		return index, next_ordinal, callback_released
	}

	fn renderer_fault_assert_prepare_harvest_proof_for_test(app &App, window WindowId, authority RendererFaultStateSnapshot, fresh RendererFaultNativeProofSnapshot, harvested RendererFaultNativeProofSnapshot, stopped RendererFaultNativeProofSnapshot, callback_before RendererFaultWaylandFrameCallbackState, callback_after RendererFaultWaylandFrameCallbackState) int {
		assert fresh.accepting_plans
		assert fresh.trace_len == 0
		assert !fresh.trace_overflow
		assert harvested.generation == fresh.generation
		assert harvested.ordinal_floor == fresh.ordinal_floor
		assert harvested.accepting_plans == fresh.accepting_plans
		assert harvested.plan == fresh.plan
		assert !harvested.trace_overflow
		assert harvested.sequence_exhausted == fresh.sequence_exhausted
		assert harvested.terminal_cause == fresh.terminal_cause
		assert harvested.trace_len >= fresh.trace_len
		assert stopped.trace_len >= harvested.trace_len
		for index in 0 .. fresh.trace_len {
			assert harvested.trace[index] == fresh.trace[index]
		}
		for index in 0 .. harvested.trace_len {
			assert stopped.trace[index] == harvested.trace[index]
		}

		mut expected_trace_len := fresh.trace_len
		mut expected_next_ordinal := fresh.next_ordinal
		mut expected_live_tickets := fresh.live_tickets
		if app.backend.kind == .wayland {
			renderer_fault_assert_wayland_frame_callback_state_for_test(app, window,
				callback_before)
			renderer_fault_assert_wayland_frame_callback_state_for_test(app, window, callback_after)
			assert callback_after.target_generation == callback_before.target_generation
			assert callback_after.parent_identity == callback_before.parent_identity
			release_expected := callback_before.callback_identity != 0
				&& callback_after.callback_identity == 0
			if release_expected {
				assert expected_live_tickets > 0
				expected_live_tickets--
			} else {
				assert callback_after == callback_before
			}
			proof := renderer_fault_native_proof_for_test(app)
			mut callback_released := false
			expected_trace_len, expected_next_ordinal, callback_released = renderer_fault_assert_wayland_harvest_trace_for_test(app,
				proof, expected_trace_len, harvested.trace_len, expected_next_ordinal,
				authority.wayland_display_identity, release_expected, callback_before)
			assert callback_released == release_expected
		} else {
			assert callback_before == RendererFaultWaylandFrameCallbackState{}
			assert callback_after == RendererFaultWaylandFrameCallbackState{}
		}
		assert harvested.trace_len == expected_trace_len
		assert harvested.next_ordinal == expected_next_ordinal
		assert harvested.live_tickets == expected_live_tickets
		return expected_trace_len
	}

	fn renderer_fault_segmented_normal_stop_for_test(mut app App, window WindowId, before_stop RendererFaultStateSnapshot) ! {
		assert app.backend.native_operations.proof != unsafe { nil }
		assert app.status() == .running
		renderer_fault_assert_idle_state_for_test(before_stop)
		assert renderer_fault_native_submission_count_for_test(app) == 1
		initial_capture := renderer_fault_native_proof_capture_for_test(app)
		assert initial_capture.available
		initial := initial_capture.snapshot
		assert initial.trace_len > 0
		assert !initial.trace_overflow
		mut prepare_fresh := renderer_fault_clear_native_trace_for_test(mut app)
		assert prepare_fresh.trace_len == 0
		assert !prepare_fresh.trace_overflow
		assert prepare_fresh.next_ordinal == initial.next_ordinal
		assert renderer_fault_native_submission_count_for_test(app) == 0
		mut normalized_before_stop := before_stop
		if app.backend.kind == .wayland {
			$if linux && sokol_wayland ? {
				callback := renderer_fault_wayland_frame_callback_state_for_test(app, window)
				renderer_fault_assert_wayland_frame_callback_state_for_test(app, window, callback)
				assert callback.callback_identity != 0
				assert callback.callback_ticket != 0
				index := app.backend.wayland.window_record_index(window) or {
					return error(err_window_not_found)
				}
				mut record := app.backend.wayland.windows[index]
				assert native_identity(record.frame_callback) == callback.callback_identity
				assert record.frame_callback_ticket == callback.callback_ticket
				assert native_identity(record.surface) == callback.parent_identity
				assert record.render_target_generation == callback.target_generation
				assert !record.frame_ready
				release := app.backend.wayland.destroy_frame_callback_lifetime(mut record)
				release_succeeded := release.succeeded()
				callback_cleared := record.frame_callback == unsafe { nil }
					&& record.frame_callback_ticket == 0
				assert release_succeeded
				assert callback_cleared
				if release_succeeded && callback_cleared {
					record.frame_ready = true
				}
				assert record.frame_ready
				released_callback := renderer_fault_wayland_frame_callback_state_for_test(app,
					window)
				renderer_fault_assert_wayland_frame_callback_state_for_test(app, window,
					released_callback)
				assert released_callback.target_generation == callback.target_generation
				assert released_callback.parent_identity == callback.parent_identity
				assert released_callback.callback_identity == 0
				assert released_callback.callback_ticket == 0
				release_capture := renderer_fault_native_proof_capture_for_test(app)
				assert release_capture.available
				release_proof := release_capture.snapshot
				assert !release_proof.trace_overflow
				assert release_proof.trace_len == 6
				assert release_proof.next_ordinal == prepare_fresh.next_ordinal
				assert release_proof.live_tickets + 1 == prepare_fresh.live_tickets
				assert renderer_fault_native_submission_count_for_test(app) == 0
				assert callback.ticket_context.authority_scope == .renderer_attempt
				assert callback.ticket_context.authority_token == app.backend.native_operations.renderer_attempt_token
				release_cursor := renderer_fault_assert_lifetime_release_chain_for_test(app,
					app.backend.native_operations.proof, 0, .wayland, .surface_destroy,
					.window_finalize, .window_target, .renderer_attempt,
					callback.ticket_context.ordinal, callback.callback_identity)
				assert app.backend.native_operations.proof.trace[0].context == callback.ticket_context
				assert release_cursor == 6
				assert release_cursor == release_proof.trace_len
				roundtrip_fresh := renderer_fault_clear_native_trace_for_test(mut app)
				assert roundtrip_fresh.trace_len == 0
				assert !roundtrip_fresh.trace_overflow
				assert roundtrip_fresh.next_ordinal == release_proof.next_ordinal
				assert renderer_fault_native_submission_count_for_test(app) == 0

				display_identity := before_stop.wayland_display_identity
				assert display_identity != 0
				assert native_identity(app.backend.wayland.display) == display_identity
				assert app.backend.wayland.render_health == .ready
				assert !app.backend.wayland.wayland_display_unavailable
				roundtrip := app.backend.wayland.attempt_wayland_roundtrip(NativeOperationSeed{
					call_site: .display_transport
					scope:     .renderer
				})
				assert roundtrip.succeeded()
				roundtrip_capture := renderer_fault_native_proof_capture_for_test(app)
				assert roundtrip_capture.available
				roundtrip_proof := roundtrip_capture.snapshot
				assert !roundtrip_proof.trace_overflow
				assert roundtrip_proof.trace_len == 8
				assert roundtrip_proof.next_ordinal == roundtrip_fresh.next_ordinal + 2
				assert renderer_fault_native_submission_count_for_test(app) == 0
				roundtrip_cursor := renderer_fault_assert_wayland_roundtrip_segment_for_test(app,
					app.backend.native_operations.proof, roundtrip_fresh.next_ordinal,
					display_identity)
				assert roundtrip_cursor == roundtrip_proof.trace_len
				prepare_fresh = renderer_fault_clear_native_trace_for_test(mut app)
				assert prepare_fresh.trace_len == 0
				assert !prepare_fresh.trace_overflow
				assert prepare_fresh.next_ordinal == roundtrip_proof.next_ordinal
				assert renderer_fault_native_submission_count_for_test(app) == 0
				normalized_before_stop = renderer_fault_state_snapshot_for_test(app, window)!
				renderer_fault_assert_idle_state_for_test(normalized_before_stop)
			} $else {
				return error(err_backend_unsupported)
			}
		}

		delivery_before := renderer_fault_delivery_state_snapshot_for_test(app)
		callback_before := renderer_fault_wayland_frame_callback_state_for_test(app, window)
		ticket := app.prepare_stop()!
		prepared := renderer_fault_state_snapshot_for_test(app, window)!
		harvested_capture := renderer_fault_native_proof_capture_for_test(app)
		assert harvested_capture.available
		harvested := harvested_capture.snapshot
		assert !harvested.trace_overflow
		assert renderer_fault_native_submission_count_for_test(app) == 0
		delivery_suffix := renderer_fault_delivery_suffix_snapshot_for_test(delivery_before, app)
		callback_after := renderer_fault_wayland_frame_callback_state_for_test(app, window)
		window_teardown_authority := renderer_fault_window_teardown_authority_for_test(app, window)
		renderer_fault_assert_prepared_stop_transition_for_test(normalized_before_stop, prepared,
			ticket, delivery_suffix, app.backend.kind, callback_after)!
		harvest_cursor := renderer_fault_assert_prepare_harvest_proof_for_test(app, window,
			prepared, prepare_fresh, harvested, harvested, callback_before, callback_after)
		assert harvest_cursor == harvested.trace_len
		window_fresh := renderer_fault_clear_native_trace_for_test(mut app)
		assert window_fresh.trace_len == 0
		assert !window_fresh.trace_overflow
		assert window_fresh.next_ordinal == harvested.next_ordinal
		assert renderer_fault_native_submission_count_for_test(app) == 0

		mut teardown_errors := []string{}
		prepared_ticket := app.prepare_window_destroy_for_stop(window)!
		prepared_tickets := app.prepared_window_tickets_for_stop()
		assert prepared_tickets.len == 1
		assert prepared_tickets[0] == prepared_ticket
		for destroy_ticket in prepared_tickets {
			app.seal_window_destroy(destroy_ticket) or {
				teardown_errors << err.msg()
				continue
			}
			app.finish_window_destroy(destroy_ticket, []string{}) or {
				teardown_errors << err.msg()
			}
		}
		assert teardown_errors.len == 0
		assert app.sealed_window_tickets_for_stop().len == 0
		assert app.live_window_ids_for_stop()!.len == 0
		window_capture := renderer_fault_native_proof_capture_for_test(app)
		assert window_capture.available
		window_proof := window_capture.snapshot
		assert !window_proof.trace_overflow
		assert renderer_fault_native_submission_count_for_test(app) == 0
		window_cursor, shutdown_ordinal := renderer_fault_assert_window_teardown_native_prefix_for_test(app,
			prepared, window_teardown_authority, callback_after, 0, window_fresh.next_ordinal)
		assert window_cursor == window_proof.trace_len
		assert shutdown_ordinal == window_proof.next_ordinal
		shutdown_fresh := renderer_fault_clear_native_trace_for_test(mut app)
		assert shutdown_fresh.trace_len == 0
		assert !shutdown_fresh.trace_overflow
		assert shutdown_fresh.next_ordinal == window_proof.next_ordinal
		assert renderer_fault_native_submission_count_for_test(app) == 0

		app.shutdown_render_bridge_for_stop() or { teardown_errors << err.msg() }
		assert teardown_errors.len == 0
		bridge_capture := renderer_fault_native_proof_capture_for_test(app)
		assert bridge_capture.available
		bridge_proof := bridge_capture.snapshot
		assert !bridge_proof.trace_overflow
		assert renderer_fault_native_submission_count_for_test(app) == 0
		renderer_fault_assert_shutdown_native_phase_for_test(app, bridge_proof, prepared, 0,
			shutdown_fresh.next_ordinal, true)
		mut backend_fresh := renderer_fault_clear_native_trace_for_test(mut app)
		assert backend_fresh.trace_len == 0
		assert !backend_fresh.trace_overflow
		assert backend_fresh.next_ordinal == bridge_proof.next_ordinal
		assert renderer_fault_native_submission_count_for_test(app) == 0
		if app.backend.kind == .wayland {
			$if linux && sokol_wayland ? {
				display_identity := prepared.wayland_display_identity
				assert display_identity != 0
				assert native_identity(app.backend.wayland.display) == display_identity
				assert app.backend.wayland.render_health == .ready
				assert !app.backend.wayland.wayland_display_unavailable
				roundtrip_start := backend_fresh.next_ordinal
				roundtrip := app.backend.wayland.attempt_wayland_roundtrip(NativeOperationSeed{
					call_site: .display_transport
					scope:     .renderer
				})
				assert roundtrip.succeeded()
				roundtrip_capture := renderer_fault_native_proof_capture_for_test(app)
				assert roundtrip_capture.available
				roundtrip_proof := roundtrip_capture.snapshot
				assert !roundtrip_proof.trace_overflow
				assert roundtrip_proof.trace_len == 8
				assert roundtrip_proof.next_ordinal == roundtrip_start + 2
				assert renderer_fault_native_submission_count_for_test(app) == 0
				roundtrip_cursor := renderer_fault_assert_wayland_roundtrip_segment_for_test(app,
					app.backend.native_operations.proof, roundtrip_start, display_identity)
				assert roundtrip_cursor == roundtrip_proof.trace_len
				backend_fresh = renderer_fault_clear_native_trace_for_test(mut app)
				assert backend_fresh.trace_len == 0
				assert !backend_fresh.trace_overflow
				assert backend_fresh.next_ordinal == roundtrip_proof.next_ordinal
				assert renderer_fault_native_submission_count_for_test(app) == 0
			} $else {
				return error(err_backend_unsupported)
			}
		}

		mut first_stop_error := ''
		app.finish_stop(ticket, teardown_errors) or { first_stop_error = err.msg() }
		first_terminal := renderer_fault_state_snapshot_for_test(app, window)!
		first_capture := renderer_fault_native_proof_capture_for_test(app)
		assert first_capture.available
		first_proof := first_capture.snapshot
		assert !first_proof.trace_overflow
		assert first_stop_error == ''
		assert teardown_errors.len == 0
		assert app.status() == .stopped
		assert renderer_fault_native_submission_count_for_test(app) == 0
		renderer_fault_assert_first_stop_state_for_test(prepared, first_terminal,
			prepared.runtime_renderer_terminal)
		renderer_fault_assert_first_stop_proof_for_test(backend_fresh, first_proof,
			app.backend.kind)
		renderer_fault_assert_shutdown_native_phase_for_test(app, first_proof, prepared, 0,
			backend_fresh.next_ordinal, false)

		mut replay_error := ''
		app.stop() or { replay_error = err.msg() }
		second_terminal := renderer_fault_state_snapshot_for_test(app, window)!
		second_capture := renderer_fault_native_proof_capture_for_test(app)
		assert second_capture.available
		second_proof := second_capture.snapshot
		assert !second_proof.trace_overflow
		assert replay_error == first_stop_error
		assert second_terminal == first_terminal
		assert second_proof == first_proof
		assert renderer_fault_native_submission_count_for_test(app) == 0
		assert app.backend.native_operations.disarm_proof()
	}

	fn renderer_fault_exercise_batch_begin_for_test() ! {
		mut app := renderer_fault_started_app_for_test()!
		window := renderer_fault_window_or_fail_for_test(mut app, 'renderer batch begin fault') or {
			return
		}
		before := renderer_fault_window_runtime_for_test(app, window)!
		before_next_batch := app.render_runtime.next_batch_epoch
		message := 'fault:renderer_batch_begin:real-operation'
		app.set_internal_fault(.renderer_batch_begin, 0, message)!
		assert app.backend.native_operations.arm_proof()
		generation := multiwindow_sokol_trace.install_generation()!
		mut retry_state := &RendererFaultBatchRetryState{}
		mut fault_error := ''
		app.with_scheduled_render_batch(fn [mut retry_state] (_ RenderBatchLease, _ []RenderWindowSnapshot) ! {
			retry_state.callback_calls++
		}) or { fault_error = err.msg() }
		assert fault_error == message
		assert retry_state.callback_calls == 0
		assert app.render_runtime.next_batch_epoch == before_next_batch
		assert !app.render_runtime.batch_active
		assert renderer_fault_window_runtime_for_test(app, window)! == before
		renderer_fault_assert_zero_native_calls_for_test(app)
		renderer_fault_assert_sokol_operations_for_test(multiwindow_sokol_trace.typed_snapshot(),
			[])
		outcome := app.with_scheduled_render_batch(fn [window, before_next_batch, mut retry_state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			assert candidates.any(it.window == window)
			assert batch.epoch == before_next_batch
			retry_state.callback_calls++
		})!
		assert retry_state.callback_calls == 1
		assert outcome.error == ''
		assert !outcome.committed
		assert !outcome.had_gpu_work
		assert outcome.batch_epoch == before_next_batch
		assert app.renderer_is_usable()
		renderer_fault_assert_sokol_operations_for_test(multiwindow_sokol_trace.typed_snapshot(),
			[])
		renderer_fault_assert_batch_retry_native_delta_for_test(app, outcome.batch_epoch)
		assert renderer_fault_native_submission_count_for_test(app) == 0
		multiwindow_sokol_trace.uninstall_generation(generation)!
		app.stop()!
		assert app.status() == .stopped
		assert renderer_fault_native_submission_count_for_test(app) == 0
		assert app.backend.native_operations.disarm_proof()
	}

	fn renderer_fault_exercise_target_acquire_for_test() ! {
		mut app := renderer_fault_started_app_for_test()!
		window := renderer_fault_window_or_fail_for_test(mut app, 'renderer target acquire fault') or {
			return
		}
		message := 'fault:renderer_target_acquire:real-operation'
		app.set_internal_fault(.renderer_target_acquire, 0, message)!
		generation := multiwindow_sokol_trace.install_generation()!
		mut state := &RendererFaultTargetAcquireState{}
		outcome := app.with_scheduled_render_batch(fn [mut app, window, message, mut state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			state.batch_callback_calls++
			assert candidates.any(it.window == window)
			before := renderer_fault_window_runtime_for_test(app, window)!
			if app.backend.kind == .x11 {
				state.x11_surface_before = renderer_fault_x11_surface_state_for_test(app, window)
			} else if app.backend.kind == .wayland {
				state.wayland_target_before = renderer_fault_wayland_target_state_for_test(app,
					window)
				renderer_fault_assert_wayland_first_target_absent_for_test(state.wayland_target_before)
			} else if app.backend.kind == .win32 {
				state.win32_target_before = renderer_fault_win32_target_state_for_test(app, window)
			}
			assert app.backend.native_operations.arm_proof()
			mut fault_error := ''
			_ := app.acquire_render_target(batch, window) or {
				fault_error = err.msg()
				RenderTargetAcquisition{}
			}
			assert fault_error == message
			state.failed_state = renderer_fault_window_runtime_for_test(app, window)!
			assert state.failed_state == before
			if app.backend.kind == .wayland {
				assert renderer_fault_wayland_target_state_for_test(app, window) == state.wayland_target_before
			}
			renderer_fault_assert_zero_native_calls_for_test(app)
			renderer_fault_assert_sokol_operations_for_test(multiwindow_sokol_trace.typed_snapshot(),
				[])
			assert state.pass_callback_calls == 0
			acquisition := app.acquire_render_target(batch, window)!
			assert acquisition.status == .ready
			state.submitted_lease = acquisition.lease
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn [mut state] () ! {
				state.pass_callback_calls++
			})!
		})!
		assert state.failed_state.id == window
		assert state.batch_callback_calls == 1
		assert state.pass_callback_calls == 1
		assert outcome.error == ''
		assert outcome.committed
		assert outcome.completed_user_passes == 1
		assert outcome.finalized_submissions == 1
		assert app.renderer_is_usable()
		renderer_fault_assert_sokol_operations_for_test(multiwindow_sokol_trace.typed_snapshot(), [
			multiwindow_sokol_trace.Operation.begin_swapchain_pass,
			.end_pass,
			.commit,
		])
		renderer_fault_assert_target_acquire_native_delta_for_test(app, state.submitted_lease,
			state.x11_surface_before, state.wayland_target_before, state.win32_target_before)
		before_stop := renderer_fault_state_snapshot_for_test(app, window)!
		multiwindow_sokol_trace.uninstall_generation(generation)!
		renderer_fault_segmented_normal_stop_for_test(mut app, window, before_stop)!
	}

	fn renderer_fault_exercise_pass_begin_for_test() ! {
		mut app := renderer_fault_started_app_for_test()!
		window := renderer_fault_window_or_fail_for_test(mut app, 'renderer pass begin fault') or {
			return
		}
		message := 'fault:renderer_pass_begin:real-operation'
		app.set_internal_fault(.renderer_pass_begin, 0, message)!
		generation := multiwindow_sokol_trace.install_generation()!
		mut state := &RendererFaultPassBeginState{}
		outcome := app.with_scheduled_render_batch(fn [mut app, window, message, mut state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			state.batch_callback_calls++
			assert candidates.any(it.window == window)
			acquisition := app.acquire_render_target(batch, window)!
			assert acquisition.status == .ready
			state.submitted_lease = acquisition.lease
			assert app.backend.native_operations.arm_proof()
			mut fault_error := ''
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn [mut state] () ! {
				state.pass_callback_calls++
			}) or { fault_error = err.msg() }
			assert fault_error == message
			assert state.pass_callback_calls == 0
			backend_state := app.render_backend_state()!
			assert backend_state.targets.len == 1
			assert backend_state.targets[0].lease == acquisition.lease
			assert backend_state.targets[0].status == .prepared
			assert backend_state.failure_messages.len == 0
			renderer_fault_assert_zero_native_calls_for_test(app)
			renderer_fault_assert_sokol_operations_for_test(multiwindow_sokol_trace.typed_snapshot(),
				[])
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn [mut state] () ! {
				state.pass_callback_calls++
			})!
		})!
		assert state.batch_callback_calls == 1
		assert state.pass_callback_calls == 1
		assert outcome.error == ''
		assert outcome.committed
		assert outcome.completed_user_passes == 1
		assert outcome.finalized_submissions == 1
		assert app.renderer_is_usable()
		renderer_fault_assert_sokol_operations_for_test(multiwindow_sokol_trace.typed_snapshot(), [
			multiwindow_sokol_trace.Operation.begin_swapchain_pass,
			.end_pass,
			.commit,
		])
		renderer_fault_assert_frame_native_delta_for_test(app, state.submitted_lease, false, RendererFaultX11SurfaceState{})
		multiwindow_sokol_trace.uninstall_generation(generation)!
		app.stop()!
		assert app.status() == .stopped
		assert renderer_fault_native_submission_count_for_test(app) == 1
		assert app.backend.native_operations.disarm_proof()
	}

	fn renderer_fault_exercise_anchor_begin_for_test() ! {
		mut app := renderer_fault_started_app_for_test()!
		window := renderer_fault_window_or_fail_for_test(mut app, 'renderer anchor begin fault') or {
			return
		}
		before := renderer_fault_state_snapshot_for_test(app, window)!
		generation := multiwindow_sokol_trace.install_generation()!
		message := 'fault:renderer_anchor_begin:real-operation'
		app.set_internal_fault(.renderer_anchor_begin, 0, message)!
		mut state := &RendererFaultAnchorBeginState{}
		outcome := app.with_scheduled_render_batch(fn [mut app, mut state] (batch RenderBatchLease, _ []RenderWindowSnapshot) ! {
			state.callback_calls++
			state.batch_epoch = batch.epoch
			assert app.backend.native_operations.arm_proof()
			assert app.backend.native_operations.proof != unsafe { nil }
			state.native_start = app.backend.native_operations.proof.trace_len
			app.note_render_gpu_work(batch)!
		})!
		assert state.callback_calls == 1
		assert state.batch_epoch != 0
		first_trace := multiwindow_sokol_trace.typed_snapshot()
		renderer_fault_assert_sokol_operations_for_test(first_trace, [])
		assert outcome.error == '${err_render_terminal_aggregate}: ${message}'
		assert !outcome.committed
		assert outcome.had_gpu_work
		assert outcome.completed_user_passes == 0
		assert outcome.finalized_submissions == 0
		assert !app.renderer_is_usable()
		after := renderer_fault_state_snapshot_for_test(app, window)!
		renderer_fault_assert_resource_only_terminal_transition_for_test(before, after, message)
		renderer_fault_assert_anchor_begin_native_delta_for_test(app, state.native_start,
			state.batch_epoch)
		assert renderer_fault_native_submission_count_for_test(app) == 0
		mut second_error := ''
		app.with_scheduled_render_batch(fn [mut state] (_ RenderBatchLease, _ []RenderWindowSnapshot) ! {
			state.second_calls++
		}) or { second_error = err.msg() }
		assert state.second_calls == 0
		assert second_error == '${err_render_renderer_failed}: ${err_render_terminal_aggregate}: ${message}'
		renderer_fault_assert_sokol_snapshots_equal_for_test(first_trace,
			multiwindow_sokol_trace.typed_snapshot())
		multiwindow_sokol_trace.uninstall_generation(generation)!
		app.stop()!
		assert app.status() == .stopped
		assert renderer_fault_native_submission_count_for_test(app) == 0
		assert app.backend.native_operations.disarm_proof()
	}

	fn renderer_fault_exercise_precommit_for_test() ! {
		mut app := renderer_fault_started_app_for_test()!
		window := renderer_fault_window_or_fail_for_test(mut app, 'renderer precommit fault') or {
			return
		}
		before := renderer_fault_state_snapshot_for_test(app, window)!
		generation := multiwindow_sokol_trace.install_generation()!
		message := 'fault:renderer_precommit:real-operation'
		app.set_internal_fault(.renderer_precommit, 0, message)!
		mut state := &RendererFaultPrecommitState{}
		outcome := app.with_scheduled_render_batch(fn [mut app, mut state] (batch RenderBatchLease, _ []RenderWindowSnapshot) ! {
			state.callback_calls++
			state.batch_epoch = batch.epoch
			assert app.backend.native_operations.arm_proof()
			assert app.backend.native_operations.proof != unsafe { nil }
			state.native_start = app.backend.native_operations.proof.trace_len
			app.note_render_gpu_work(batch)!
		})!
		assert state.callback_calls == 1
		assert state.batch_epoch != 0
		first_trace := multiwindow_sokol_trace.typed_snapshot()
		renderer_fault_assert_sokol_operations_for_test(first_trace, [
			multiwindow_sokol_trace.Operation.begin_swapchain_pass,
			.end_pass,
			.commit,
		])
		assert outcome.error == '${err_render_terminal_aggregate}: ${message}'
		assert !outcome.committed
		assert outcome.had_gpu_work
		assert outcome.completed_user_passes == 0
		assert outcome.finalized_submissions == 0
		assert !app.renderer_is_usable()
		after := renderer_fault_state_snapshot_for_test(app, window)!
		renderer_fault_assert_resource_only_terminal_transition_for_test(before, after, message)
		renderer_fault_assert_precommit_native_delta_for_test(app, state.native_start,
			state.batch_epoch)
		assert renderer_fault_native_submission_count_for_test(app) == 0
		mut second_error := ''
		app.with_scheduled_render_batch(fn [mut state] (_ RenderBatchLease, _ []RenderWindowSnapshot) ! {
			state.second_calls++
		}) or { second_error = err.msg() }
		assert state.second_calls == 0
		assert second_error == '${err_render_renderer_failed}: ${err_render_terminal_aggregate}: ${message}'
		renderer_fault_assert_sokol_snapshots_equal_for_test(first_trace,
			multiwindow_sokol_trace.typed_snapshot())
		multiwindow_sokol_trace.uninstall_generation(generation)!
		app.stop()!
		assert app.status() == .stopped
		assert renderer_fault_native_submission_count_for_test(app) == 0
		assert app.backend.native_operations.disarm_proof()
	}

	fn renderer_fault_exercise_submission_finalize_for_test() ! {
		mut app := renderer_fault_started_app_for_test()!
		window := renderer_fault_window_or_fail_for_test(mut app,
			'renderer submission finalize fault') or { return }
		assert app.backend.native_operations.arm_proof()
		generation := multiwindow_sokol_trace.install_generation()!
		before := app.render_window_snapshot(window)!
		before_state := renderer_fault_state_snapshot_for_test(app, window)!
		message := 'fault:renderer_submission_finalize:real-operation'
		app.set_internal_fault(.renderer_submission_finalize, 0, message)!
		mut state := &RendererFaultSubmissionFinalizeState{}
		outcome := app.with_scheduled_render_batch(fn [mut app, window, mut state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			state.batch_callback_calls++
			assert candidates.any(it.window == window)
			if app.backend.kind == .x11 {
				state.x11_surface_before = renderer_fault_x11_surface_state_for_test(app, window)
			}
			if app.backend.kind == .wayland {
				state.wayland_target_before = renderer_fault_wayland_target_state_for_test(app,
					window)
				renderer_fault_assert_wayland_first_target_absent_for_test(state.wayland_target_before)
				anchor_capture := renderer_fault_native_proof_capture_for_test(app)
				assert anchor_capture.available
				anchor_snapshot := anchor_capture.snapshot
				assert !anchor_snapshot.trace_overflow
				assert anchor_snapshot.trace_len == 20
				proof := renderer_fault_native_proof_for_test(app)
				mut anchor_end := 0
				mut anchor := NativeOperationContext{}
				anchor_end, anchor = renderer_fault_assert_egl_binding_sequence_for_test(app,
					proof, 0, .anchor_prepare, .anchor, RenderTargetLease{})
				assert anchor.ordinal == anchor_snapshot.ordinal_floor
				assert anchor_end == 20
				assert anchor_end == proof.trace_len
				for index in 0 .. anchor_end {
					assert anchor_snapshot.trace[index] == proof.trace[index]
				}
				state.wayland_first_target_ordinal = anchor.ordinal + 5
				assert anchor_snapshot.next_ordinal == state.wayland_first_target_ordinal
				fresh := renderer_fault_clear_native_trace_for_test(mut app)
				assert fresh.trace_len == 0
				assert fresh.next_ordinal == state.wayland_first_target_ordinal
			}
			if app.backend.kind == .win32 {
				state.win32_target_before = renderer_fault_win32_target_state_for_test(app, window)
			}
			acquisition := app.acquire_render_target(batch, window)!
			assert acquisition.status == .ready
			state.submitted_lease = acquisition.lease
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn [mut state] () ! {
				state.pass_callback_calls++
			})!
		})!
		assert state.batch_callback_calls == 1
		assert state.pass_callback_calls == 1
		first_trace := multiwindow_sokol_trace.typed_snapshot()
		renderer_fault_assert_sokol_operations_for_test(first_trace, [
			multiwindow_sokol_trace.Operation.begin_swapchain_pass,
			.end_pass,
			.commit,
		])
		after := app.render_window_snapshot(window)!
		after_state := renderer_fault_state_snapshot_for_test(app, window)!
		assert outcome.error == '${err_render_terminal_aggregate}: ${message}'
		assert outcome.committed
		assert outcome.completed_user_passes == 1
		assert outcome.finalized_submissions == 0
		assert after.frame_serial == before.frame_serial + 1
		assert after.submitted_frame == before.submitted_frame
		assert after.dirty_epoch > after.consumed_epoch
		assert !app.renderer_is_usable()
		renderer_fault_assert_finalize_terminal_transition_for_test(before_state, after_state,
			message)
		proof_trace_len := if app.backend.kind == .win32 {
			renderer_fault_assert_win32_target_acquire_chain_for_test(app, state.submitted_lease,
				state.win32_target_before)
		} else if app.backend.kind == .wayland {
			renderer_fault_assert_wayland_first_target_frame_delta_for_test(app,
				state.submitted_lease, state.wayland_target_before,
				state.wayland_first_target_ordinal)
		} else {
			renderer_fault_assert_frame_native_delta_for_test(app, state.submitted_lease, true,
				state.x11_surface_before)
		}
		mut second_error := ''
		app.with_scheduled_render_batch(fn [mut state] (_ RenderBatchLease, _ []RenderWindowSnapshot) ! {
			state.second_calls++
		}) or { second_error = err.msg() }
		assert state.second_calls == 0
		assert second_error == '${err_render_renderer_failed}: ${err_render_terminal_aggregate}: ${message}'
		assert app.backend.native_operations.proof != unsafe { nil }
		assert app.backend.native_operations.proof.trace_len == proof_trace_len
		renderer_fault_assert_sokol_snapshots_equal_for_test(first_trace,
			multiwindow_sokol_trace.typed_snapshot())
		before_stop := renderer_fault_state_snapshot_for_test(app, window)!
		assert before_stop == after_state
		renderer_fault_assert_idle_state_for_test(before_stop)
		multiwindow_sokol_trace.uninstall_generation(generation)!
		renderer_fault_segmented_normal_stop_for_test(mut app, window, before_stop)!
	}

	fn renderer_fault_exercise_submission_finalize_shutdown_replay_for_test() ! {
		mut app := renderer_fault_started_app_for_test()!
		window := renderer_fault_window_for_test(mut app,
			'renderer submission finalize shutdown replay')!
		proof_was_nil := app.backend.native_operations.proof == unsafe { nil }
		generation := multiwindow_sokol_trace.install_generation()!
		before := renderer_fault_state_snapshot_for_test(app, window)!
		message := 'fault:renderer_submission_finalize:shutdown-replay'
		app.set_internal_fault(.renderer_submission_finalize, 0, message)!
		mut state := &RendererFaultSubmissionFinalizeState{}
		outcome := app.with_scheduled_render_batch(fn [mut app, window, mut state] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			state.batch_callback_calls++
			assert candidates.any(it.window == window)
			acquisition := app.acquire_render_target(batch, window)!
			assert acquisition.status == .ready
			state.submitted_lease = acquisition.lease
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn [mut state] () ! {
				state.pass_callback_calls++
			})!
		})!
		assert state.batch_callback_calls == 1
		assert state.pass_callback_calls == 1
		assert outcome.error == '${err_render_terminal_aggregate}: ${message}'
		assert outcome.committed
		assert outcome.completed_user_passes == 1
		assert outcome.finalized_submissions == 0
		assert state.submitted_lease.window == window
		after := renderer_fault_state_snapshot_for_test(app, window)!
		renderer_fault_assert_finalize_terminal_transition_for_test(before, after, message)
		renderer_fault_assert_sokol_operations_for_test(multiwindow_sokol_trace.typed_snapshot(), [
			multiwindow_sokol_trace.Operation.begin_swapchain_pass,
			.end_pass,
			.commit,
		])
		multiwindow_sokol_trace.uninstall_generation(generation)!
		before_stop := renderer_fault_state_snapshot_for_test(app, window)!
		proof_armed := app.backend.native_operations.arm_proof()
		fresh_capture := renderer_fault_native_proof_capture_for_test(app)
		mut fresh := fresh_capture.snapshot
		if app.backend.kind == .wayland {
			$if linux && sokol_wayland ? {
				callback := renderer_fault_wayland_frame_callback_state_for_test(app, window)
				renderer_fault_assert_wayland_frame_callback_state_for_test(app, window, callback)
				assert callback.callback_identity != 0
				assert callback.callback_ticket != 0
				index := app.backend.wayland.window_record_index(window) or {
					return error(err_window_not_found)
				}
				mut record := app.backend.wayland.windows[index]
				assert native_identity(record.frame_callback) == callback.callback_identity
				assert record.frame_callback_ticket == callback.callback_ticket
				assert native_identity(record.surface) == callback.parent_identity
				assert record.render_target_generation == callback.target_generation
				assert !record.frame_ready
				release := app.backend.wayland.destroy_frame_callback_lifetime(mut record)
				release_succeeded := release.succeeded()
				callback_cleared := record.frame_callback == unsafe { nil }
					&& record.frame_callback_ticket == 0
				assert release_succeeded
				assert callback_cleared
				if release_succeeded && callback_cleared {
					record.frame_ready = true
				}
				assert record.frame_ready
				released_callback := renderer_fault_wayland_frame_callback_state_for_test(app,
					window)
				renderer_fault_assert_wayland_frame_callback_state_for_test(app, window,
					released_callback)
				assert released_callback.target_generation == callback.target_generation
				assert released_callback.parent_identity == callback.parent_identity
				assert released_callback.callback_identity == 0
				assert released_callback.callback_ticket == 0
				release_capture := renderer_fault_native_proof_capture_for_test(app)
				assert release_capture.available
				release_proof := release_capture.snapshot
				assert release_proof.generation == fresh.generation
				assert !release_proof.trace_overflow
				assert release_proof.trace_len == 6
				assert release_proof.next_ordinal == fresh.next_ordinal
				assert release_proof.live_tickets + 1 == fresh.live_tickets
				assert callback.ticket_context.authority_scope == .renderer_attempt
				assert callback.ticket_context.authority_token == app.backend.native_operations.renderer_attempt_token
				release_cursor := renderer_fault_assert_lifetime_release_chain_for_test(app,
					app.backend.native_operations.proof, 0, .wayland, .surface_destroy,
					.window_finalize, .window_target, .renderer_attempt,
					callback.ticket_context.ordinal, callback.callback_identity)
				assert app.backend.native_operations.proof.trace[0].context == callback.ticket_context
				assert release_cursor == 6
				assert release_cursor == release_proof.trace_len

				roundtrip_fresh := renderer_fault_clear_native_trace_for_test(mut app)
				assert roundtrip_fresh.generation == release_proof.generation
				assert roundtrip_fresh.trace_len == 0
				assert !roundtrip_fresh.trace_overflow
				assert roundtrip_fresh.next_ordinal == release_proof.next_ordinal
				assert roundtrip_fresh.live_tickets == release_proof.live_tickets
				display_identity := before_stop.wayland_display_identity
				assert display_identity != 0
				assert native_identity(app.backend.wayland.display) == display_identity
				assert app.backend.wayland.render_health == .ready
				assert !app.backend.wayland.wayland_display_unavailable
				roundtrip := app.backend.wayland.attempt_wayland_roundtrip(NativeOperationSeed{
					call_site: .display_transport
					scope:     .renderer
				})
				assert roundtrip.succeeded()
				roundtrip_capture := renderer_fault_native_proof_capture_for_test(app)
				assert roundtrip_capture.available
				roundtrip_proof := roundtrip_capture.snapshot
				assert roundtrip_proof.generation == roundtrip_fresh.generation
				assert !roundtrip_proof.trace_overflow
				assert roundtrip_proof.trace_len == 8
				assert roundtrip_proof.next_ordinal == roundtrip_fresh.next_ordinal + 2
				assert roundtrip_proof.live_tickets == roundtrip_fresh.live_tickets
				roundtrip_cursor := renderer_fault_assert_wayland_roundtrip_segment_for_test(app,
					app.backend.native_operations.proof, roundtrip_fresh.next_ordinal,
					display_identity)
				assert roundtrip_cursor == roundtrip_proof.trace_len
				fresh = renderer_fault_clear_native_trace_for_test(mut app)
				assert fresh.generation == roundtrip_proof.generation
				assert fresh.trace_len == 0
				assert !fresh.trace_overflow
				assert fresh.next_ordinal == roundtrip_proof.next_ordinal
				assert fresh.live_tickets == roundtrip_proof.live_tickets
			} $else {
				return error(err_backend_unsupported)
			}
		}
		delivery_before := renderer_fault_delivery_state_snapshot_for_test(app)
		callback_before := renderer_fault_wayland_frame_callback_state_for_test(app, window)
		if app.backend.kind == .wayland {
			renderer_fault_assert_wayland_frame_callback_state_for_test(app, window,
				callback_before)
			assert callback_before.callback_identity == 0
			assert callback_before.callback_ticket == 0
		}

		mut prepare_error := ''
		ticket := app.prepare_stop() or {
			prepare_error = err.msg()
			AppStopTicket{}
		}
		mut prepared_capture_error := ''
		prepared := renderer_fault_state_snapshot_for_test(app, window) or {
			prepared_capture_error = err.msg()
			RendererFaultStateSnapshot{}
		}
		harvested_capture := renderer_fault_native_proof_capture_for_test(app)
		delivery_suffix := renderer_fault_delivery_suffix_snapshot_for_test(delivery_before, app)
		callback_after := renderer_fault_wayland_frame_callback_state_for_test(app, window)
		window_teardown_authority := renderer_fault_window_teardown_authority_for_test(app, window)
		assert prepare_error == ''
		assert prepared_capture_error == ''
		assert proof_was_nil
		assert proof_armed
		assert fresh_capture.available
		assert harvested_capture.available
		harvested_proof := harvested_capture.snapshot
		assert fresh.generation != 0
		assert fresh.ordinal_floor != 0
		assert fresh.trace_len == 0
		assert !fresh.trace_overflow
		assert fresh.accepting_plans
		assert before_stop == after
		renderer_fault_assert_idle_state_for_test(before_stop)
		renderer_fault_assert_prepared_stop_transition_for_test(before_stop, prepared, ticket,
			delivery_suffix, app.backend.kind, callback_after)!
		harvest_cursor := renderer_fault_assert_prepare_harvest_proof_for_test(app, window,
			prepared, fresh, harvested_proof, harvested_proof, callback_before, callback_after)
		assert harvest_cursor == harvested_proof.trace_len
		window_fresh := renderer_fault_clear_native_trace_for_test(mut app)
		assert window_fresh.next_ordinal == harvested_proof.next_ordinal

		mut teardown_errors := []string{}
		prepared_ticket := app.prepare_window_destroy_for_stop(window)!
		prepared_tickets := app.prepared_window_tickets_for_stop()
		assert prepared_tickets.len == 1
		assert prepared_tickets[0] == prepared_ticket
		for destroy_ticket in prepared_tickets {
			app.seal_window_destroy(destroy_ticket) or {
				teardown_errors << err.msg()
				continue
			}
			app.finish_window_destroy(destroy_ticket, []string{}) or {
				teardown_errors << err.msg()
			}
		}
		assert teardown_errors.len == 0
		assert app.sealed_window_tickets_for_stop().len == 0
		assert app.live_window_ids_for_stop()!.len == 0
		window_proof_capture := renderer_fault_native_proof_capture_for_test(app)
		assert window_proof_capture.available
		window_proof := window_proof_capture.snapshot
		assert !window_proof.trace_overflow, 'window teardown native proof overflowed after segmentation'
		window_cursor, shutdown_ordinal := renderer_fault_assert_window_teardown_native_prefix_for_test(app,
			prepared, window_teardown_authority, callback_after, 0, window_fresh.next_ordinal)
		assert window_cursor == window_proof.trace_len
		shutdown_fresh := renderer_fault_clear_native_trace_for_test(mut app)
		assert shutdown_fresh.next_ordinal == shutdown_ordinal
		app.shutdown_render_bridge_for_stop() or { teardown_errors << err.msg() }
		assert teardown_errors.len == 0
		bridge_proof_capture := renderer_fault_native_proof_capture_for_test(app)
		assert bridge_proof_capture.available
		bridge_proof := bridge_proof_capture.snapshot
		assert !bridge_proof.trace_overflow, 'renderer bridge shutdown native proof overflowed after segmentation'
		renderer_fault_assert_shutdown_native_phase_for_test(app, bridge_proof, prepared, 0,
			shutdown_ordinal, true)
		mut backend_fresh := renderer_fault_clear_native_trace_for_test(mut app)
		assert backend_fresh.next_ordinal == bridge_proof.next_ordinal
		if app.backend.kind == .wayland {
			$if linux && sokol_wayland ? {
				display_identity := prepared.wayland_display_identity
				assert display_identity != 0
				assert native_identity(app.backend.wayland.display) == display_identity
				assert app.backend.wayland.render_health == .ready
				assert !app.backend.wayland.wayland_display_unavailable
				roundtrip_start := backend_fresh.next_ordinal
				roundtrip := app.backend.wayland.attempt_wayland_roundtrip(NativeOperationSeed{
					call_site: .display_transport
					scope:     .renderer
				})
				assert roundtrip.succeeded()
				roundtrip_capture := renderer_fault_native_proof_capture_for_test(app)
				assert roundtrip_capture.available
				roundtrip_proof := roundtrip_capture.snapshot
				assert !roundtrip_proof.trace_overflow
				assert roundtrip_proof.trace_len == 8
				assert roundtrip_proof.next_ordinal == roundtrip_start + 2
				assert renderer_fault_native_submission_count_for_test(app) == 0
				roundtrip_cursor := renderer_fault_assert_wayland_roundtrip_segment_for_test(app,
					app.backend.native_operations.proof, roundtrip_start, display_identity)
				assert roundtrip_cursor == roundtrip_proof.trace_len
				backend_fresh = renderer_fault_clear_native_trace_for_test(mut app)
				assert backend_fresh.trace_len == 0
				assert !backend_fresh.trace_overflow
				assert backend_fresh.next_ordinal == roundtrip_proof.next_ordinal
				assert renderer_fault_native_submission_count_for_test(app) == 0
			} $else {
				return error(err_backend_unsupported)
			}
		}

		mut first_stop_error := ''
		app.finish_stop(ticket, teardown_errors) or { first_stop_error = err.msg() }
		mut first_terminal_capture_error := ''
		first_terminal := renderer_fault_state_snapshot_for_test(app, window) or {
			first_terminal_capture_error = err.msg()
			RendererFaultStateSnapshot{}
		}
		first_proof_capture := renderer_fault_native_proof_capture_for_test(app)
		mut second_stop_error := ''
		app.stop() or { second_stop_error = err.msg() }
		mut second_terminal_capture_error := ''
		second_terminal := renderer_fault_state_snapshot_for_test(app, window) or {
			second_terminal_capture_error = err.msg()
			RendererFaultStateSnapshot{}
		}
		second_proof_capture := renderer_fault_native_proof_capture_for_test(app)

		assert first_terminal_capture_error == ''
		assert second_terminal_capture_error == ''
		assert first_proof_capture.available
		assert second_proof_capture.available
		first_proof := first_proof_capture.snapshot
		second_proof := second_proof_capture.snapshot
		assert !first_proof.trace_overflow, 'renderer/backend shutdown native proof overflowed after segmentation'
		assert first_stop_error == ''
		assert app.status() == .stopped
		renderer_fault_assert_first_stop_state_for_test(prepared, first_terminal,
			'${err_render_terminal_aggregate}: ${message}')
		renderer_fault_assert_first_stop_proof_for_test(backend_fresh, first_proof,
			app.backend.kind)
		renderer_fault_assert_shutdown_native_phase_for_test(app, first_proof, prepared, 0,
			backend_fresh.next_ordinal, false)
		assert second_stop_error == first_stop_error
		assert second_terminal == first_terminal
		assert second_proof == first_proof
		assert app.backend.native_operations.disarm_proof()
	}

	fn renderer_fault_assert_batch_retry_native_delta_for_test(app &App, batch_epoch u64) int {
		assert batch_epoch != 0
		proof := renderer_fault_native_proof_for_test(app)
		mut index := 0
		match app.backend.kind {
			.x11, .wayland {
				mut binding := NativeOperationContext{}
				index, binding = renderer_fault_assert_egl_binding_sequence_for_test(app, proof,
					index, .anchor_prepare, .anchor, RenderTargetLease{})
				assert binding.ordinal == proof.ordinal_floor
			}
			.appkit {
				mut begin := NativeOperationContext{}
				mut pool_identity := u64(0)
				index, begin, pool_identity = renderer_fault_assert_appkit_batch_begin_for_test(app,
					proof, index, batch_epoch)
				assert begin.ordinal == proof.ordinal_floor
				index = renderer_fault_assert_appkit_pool_release_for_test(app, proof, index,
					batch_epoch, begin.ordinal + 1, pool_identity)
			}
			.win32 {}
			else {
				assert false, 'renderer batch proof selected unsupported backend'
			}
		}

		assert index == proof.trace_len
		return index
	}

	fn renderer_fault_x11_surface_state_for_test(app &App, window WindowId) RendererFaultX11SurfaceState {
		assert app.backend.kind == .x11
		index := app.backend.x11.window_record_index(window) or {
			assert false, 'renderer X11 surface proof requires a live window record'
			return RendererFaultX11SurfaceState{}
		}
		record := app.backend.x11.windows[index]
		assert record.render_target_generation != 0
		surface_identity := native_identity(record.egl_surface)
		if surface_identity == 0 {
			assert record.egl_surface_ticket == 0
			return RendererFaultX11SurfaceState{
				captured:          true
				target_generation: record.render_target_generation
			}
		}
		assert record.egl_surface_ticket != 0
		mut ticket_count := 0
		mut ticket_identity := u64(0)
		mut ticket_context := NativeOperationContext{}
		for ticket in app.backend.native_operations.lifetime_tickets {
			if ticket.ticket_id != record.egl_surface_ticket {
				continue
			}
			ticket_count++
			assert ticket.release_kind == .egl_surface
			assert ticket.owner_seed.window == window
			assert ticket.owner_seed.target_generation == record.render_target_generation
			assert ticket.state == .bound
			assert ticket.native_identity == surface_identity
			ticket_identity = ticket.native_identity
			ticket_context = ticket.context
		}
		assert ticket_count == 1
		return RendererFaultX11SurfaceState{
			captured:          true
			target_generation: record.render_target_generation
			surface_identity:  surface_identity
			surface_ticket:    record.egl_surface_ticket
			ticket_identity:   ticket_identity
			ticket_context:    ticket_context
		}
	}

	fn renderer_fault_wayland_target_state_for_test(app &App, window WindowId) RendererFaultWaylandTargetState {
		assert app.backend.kind == .wayland
		$if linux && sokol_wayland ? {
			index := app.backend.wayland.window_record_index(window) or {
				assert false, 'renderer Wayland target proof requires a live window record'
				return RendererFaultWaylandTargetState{}
			}
			record := app.backend.wayland.windows[index]
			native_surface := native_identity(record.surface)
			display_identity := native_identity(app.backend.wayland.display)
			egl_display_identity := native_identity(app.backend.wayland.egl_display)
			wl_egl_window_identity := native_identity(record.wl_egl_window)
			egl_surface_identity := native_identity(record.egl_surface)
			fully_absent := wl_egl_window_identity == 0 && record.wl_egl_window_ticket == 0
				&& egl_surface_identity == 0 && record.egl_surface_ticket == 0
			fully_materialized := wl_egl_window_identity != 0 && record.wl_egl_window_ticket != 0
				&& egl_surface_identity != 0 && record.egl_surface_ticket != 0
			assert record.id == window
			assert native_surface != 0
			assert display_identity != 0
			assert egl_display_identity != 0
			assert record.render_target_generation != 0
			assert fully_absent || fully_materialized
			assert !(fully_absent && fully_materialized)
			return RendererFaultWaylandTargetState{
				captured:             true
				materialized:         fully_materialized
				pending_egl_resize:   record.pending_egl_resize
				target_generation:    record.render_target_generation
				native_surface:       native_surface
				display_identity:     display_identity
				egl_display_identity: egl_display_identity
				wl_egl_window:        renderer_fault_lifetime_teardown_authority_for_test(app,
					wl_egl_window_identity, record.wl_egl_window_ticket)
				egl_surface:          renderer_fault_lifetime_teardown_authority_for_test(app,
					egl_surface_identity, record.egl_surface_ticket)
			}
		} $else {
			assert false, 'renderer Wayland target proof requires Linux Wayland support'
			return RendererFaultWaylandTargetState{}
		}
	}

	fn renderer_fault_assert_wayland_first_target_absent_for_test(state RendererFaultWaylandTargetState) {
		assert state.captured
		assert !state.materialized
		assert state.target_generation != 0
		assert state.native_surface != 0
		assert state.display_identity != 0
		assert state.egl_display_identity != 0
		assert !state.wl_egl_window.captured
		assert state.wl_egl_window.value_identity == 0
		assert state.wl_egl_window.ticket_id == 0
		assert !state.egl_surface.captured
		assert state.egl_surface.value_identity == 0
		assert state.egl_surface.ticket_id == 0
	}

	fn renderer_fault_assert_wayland_created_target_authority_for_test(app &App, authority RendererFaultLifetimeTeardownAuthority, lease RenderTargetLease, target_generation u64, expected_ticket u64, release_kind NativeLifetimeReleaseKind, identity u64, parent_identity u64, parent_scope NativeOperationAuthorityScope, proof_generation u64) {
		expected_seed := NativeOperationSeed{
			presence_mask:      native_context_window_target_fields
			call_site:          .window_prepare
			scope:              .window_target
			window:             lease.window
			target_generation:  target_generation
			batch_epoch:        lease.batch_epoch
			window_lease_epoch: lease.window_epoch
			target_lease_epoch: lease.target_epoch
		}
		parent_token := if parent_scope == .app_lifetime {
			app.backend.native_operations.app_lifetime_token
		} else {
			u64(0)
		}
		expected_context := NativeOperationContext{
			authority_scope:        .app_lifetime
			authority_token:        app.backend.native_operations.app_lifetime_token
			renderer_attempt_token: 0
			app_identity:           app.instance_id
			presence_mask:          native_context_window_target_fields | native_context_has_target_identity
			domain:                 native_lifetime_release_domain(release_kind)
			operation:              native_lifetime_release_operation(release_kind)
			call_site:              .window_prepare
			scope:                  .window_target
			window:                 lease.window
			target_generation:      target_generation
			target_identity:        identity
			batch_epoch:            lease.batch_epoch
			window_lease_epoch:     lease.window_epoch
			target_lease_epoch:     lease.target_epoch
			ordinal:                expected_ticket
		}
		assert authority.captured
		assert authority.value_identity == identity
		assert authority.ticket_id == expected_ticket
		assert authority.app_identity == app.instance_id
		assert authority.authority_scope == .app_lifetime
		assert authority.authority_token == app.backend.native_operations.app_lifetime_token
		assert authority.domain == native_lifetime_release_domain(release_kind)
		assert authority.release_kind == release_kind
		assert authority.owner_seed == expected_seed
		assert authority.proof_generation == proof_generation
		assert native_operation_contexts_identical(authority.context, expected_context)
		assert authority.ticket_native_identity == identity
		assert authority.required_parent_identity == parent_identity
		assert authority.parent_authority_scope == parent_scope
		assert authority.parent_authority_token == parent_token
		assert authority.state == .bound
	}

	fn renderer_fault_assert_wayland_lazy_first_target_prefix_for_test(app &App, proof &NativeOperationProofState, lease RenderTargetLease, before RendererFaultWaylandTargetState, expected_first_ordinal u64) (int, RendererFaultWaylandTargetState) {
		assert app.backend.kind == .wayland
		renderer_fault_assert_target_lease_for_test(app, lease)
		renderer_fault_assert_wayland_first_target_absent_for_test(before)
		assert expected_first_ordinal != 0
		assert proof.generation != 0
		assert proof.trace_len >= 13
		after := renderer_fault_wayland_target_state_for_test(app, lease.window)
		assert after.captured
		assert after.materialized
		assert !after.pending_egl_resize
		assert after.target_generation == before.target_generation
		assert after.native_surface == before.native_surface
		assert after.display_identity == before.display_identity
		assert after.egl_display_identity == before.egl_display_identity

		mut index := 0
		mut wl_egl_create := NativeOperationContext{}
		index, wl_egl_create = renderer_fault_assert_wayland_chain_at_for_test(app, proof, index,
			.window_surface_create, .window_prepare, .window_target)
		renderer_fault_assert_window_context_for_test(app, wl_egl_create, lease, true)
		assert wl_egl_create.ordinal == expected_first_ordinal
		assert wl_egl_create.target_generation == before.target_generation
		assert wl_egl_create.target_identity == before.native_surface
		assert proof.trace[1].actual.valid_mask == native_valid_handle
		assert proof.trace[1].actual.handle == after.wl_egl_window.value_identity
		assert proof.trace[3].context.target_identity == before.display_identity
		assert index == 8
		renderer_fault_assert_wayland_created_target_authority_for_test(app, after.wl_egl_window,
			lease, before.target_generation, expected_first_ordinal + 2, .wayland_egl_window,
			after.wl_egl_window.value_identity, before.native_surface, .none, proof.generation)

		egl_create := renderer_fault_assert_native_chain_at_for_test(proof, index, .egl,
			.window_surface_create, .window_prepare, .window_target)
		renderer_fault_assert_window_context_for_test(app, egl_create, lease, true)
		assert egl_create.ordinal == expected_first_ordinal + 3
		assert egl_create.target_generation == before.target_generation
		assert egl_create.target_identity == after.wl_egl_window.value_identity
		assert proof.trace[index + 1].actual.has(native_valid_handle)
		assert proof.trace[index + 1].actual.handle == after.egl_surface.value_identity
		index += 5
		assert index == 13
		renderer_fault_assert_wayland_created_target_authority_for_test(app, after.egl_surface,
			lease, before.target_generation, expected_first_ordinal + 5, .egl_surface,
			after.egl_surface.value_identity, before.egl_display_identity, .app_lifetime,
			proof.generation)
		return index, after
	}

	fn renderer_fault_assert_win32_target_authority_for_test(app &App, authority RendererFaultLifetimeTeardownAuthority, window WindowId, target_generation u64) {
		assert authority.captured
		assert authority.value_identity != 0
		assert authority.ticket_id != 0
		assert authority.app_identity == app.instance_id
		assert authority.authority_scope == .app_lifetime
		assert authority.authority_token == app.backend.native_operations.app_lifetime_token
		assert authority.domain == .dxgi
		assert authority.release_kind == .com_reference
		assert authority.ticket_native_identity == authority.value_identity
		assert authority.required_parent_identity == 0
		assert authority.state == .bound
		assert authority.owner_seed.scope == .window_target
		assert authority.owner_seed.window == window
		assert authority.owner_seed.target_generation == target_generation
		assert authority.owner_seed.target_identity == 0
		context := authority.context
		assert context.authority_scope == authority.authority_scope
		assert context.authority_token == authority.authority_token
		assert context.renderer_attempt_token == 0
		assert context.app_identity == app.instance_id
		assert context.presence_mask == authority.owner_seed.presence_mask | native_context_has_target_identity
		assert context.domain == .dxgi
		assert context.operation == .object_release
		assert context.call_site == authority.owner_seed.call_site
		assert context.scope == .window_target
		assert context.window == window
		assert context.target_generation == target_generation
		assert context.target_identity == authority.value_identity
		assert context.batch_epoch == authority.owner_seed.batch_epoch
		assert context.window_lease_epoch == authority.owner_seed.window_lease_epoch
		assert context.target_lease_epoch == authority.owner_seed.target_lease_epoch
		assert context.ordinal == authority.ticket_id
	}

	fn renderer_fault_assert_win32_preexisting_target_authority_for_test(app &App, authority RendererFaultLifetimeTeardownAuthority, window WindowId, target_generation u64) {
		renderer_fault_assert_win32_target_authority_for_test(app, authority, window,
			target_generation)
		assert authority.proof_generation == 0
		assert authority.owner_seed.presence_mask == native_context_has_window | native_context_has_target_generation
		assert authority.owner_seed.call_site == .display_transport
		assert authority.owner_seed.batch_epoch == 0
		assert authority.owner_seed.window_lease_epoch == 0
		assert authority.owner_seed.target_lease_epoch == 0
	}

	fn renderer_fault_assert_win32_preexisting_target_state_for_test(app &App, state RendererFaultWin32TargetState, window WindowId) {
		assert state.captured
		assert state.materialized
		assert !state.render_resize_pending
		renderer_fault_assert_win32_preexisting_target_authority_for_test(app, state.swapchain,
			window, state.target_generation)
		renderer_fault_assert_win32_preexisting_target_authority_for_test(app, state.render_view,
			window, state.target_generation)
		renderer_fault_assert_win32_preexisting_target_authority_for_test(app, state.depth_texture,
			window, state.target_generation)
		renderer_fault_assert_win32_preexisting_target_authority_for_test(app, state.depth_view,
			window, state.target_generation)
	}

	fn renderer_fault_win32_target_state_for_test(app &App, window WindowId) RendererFaultWin32TargetState {
		assert app.backend.kind == .win32
		$if windows && sokol_d3d11 ? {
			index := app.backend.win32.window_record_index(window) or {
				assert false, 'renderer Win32 target proof requires a live window record'
				return RendererFaultWin32TargetState{}
			}
			record := app.backend.win32.windows[index]
			window_identity := native_identity(record.hwnd)
			swapchain_identity := native_identity(record.swapchain)
			pending_backbuffer_identity := native_identity(record.pending_backbuffer)
			render_view_identity := native_identity(record.render_view)
			depth_texture_identity := native_identity(record.depth_texture)
			depth_view_identity := native_identity(record.depth_stencil_view)
			assert record.id == window
			assert window_identity != 0
			assert record.render_target_generation != 0
			fully_absent := swapchain_identity == 0 && record.swapchain_ticket == 0
				&& pending_backbuffer_identity == 0 && record.pending_backbuffer_ticket == 0
				&& render_view_identity == 0 && record.render_view_ticket == 0
				&& depth_texture_identity == 0 && record.depth_texture_ticket == 0
				&& depth_view_identity == 0 && record.depth_stencil_view_ticket == 0
			fully_materialized := swapchain_identity != 0 && record.swapchain_ticket != 0
				&& pending_backbuffer_identity == 0 && record.pending_backbuffer_ticket == 0
				&& render_view_identity != 0 && record.render_view_ticket != 0
				&& depth_texture_identity != 0 && record.depth_texture_ticket != 0
				&& depth_view_identity != 0 && record.depth_stencil_view_ticket != 0
				&& !record.render_resize_pending
			assert fully_absent || fully_materialized
			assert !(fully_absent && fully_materialized)
			state := RendererFaultWin32TargetState{
				captured:              true
				materialized:          fully_materialized
				render_resize_pending: record.render_resize_pending
				window_identity:       window_identity
				target_generation:     record.render_target_generation
				swapchain:             renderer_fault_lifetime_teardown_authority_for_test(app,
					swapchain_identity, record.swapchain_ticket)
				render_view:           renderer_fault_lifetime_teardown_authority_for_test(app,
					render_view_identity, record.render_view_ticket)
				depth_texture:         renderer_fault_lifetime_teardown_authority_for_test(app,
					depth_texture_identity, record.depth_texture_ticket)
				depth_view:            renderer_fault_lifetime_teardown_authority_for_test(app,
					depth_view_identity, record.depth_stencil_view_ticket)
			}
			return state
		} $else {
			return RendererFaultWin32TargetState{}
		}
	}

	fn renderer_fault_assert_win32_target_authority_unchanged_for_test(expected RendererFaultLifetimeTeardownAuthority, actual RendererFaultLifetimeTeardownAuthority) {
		assert actual.captured == expected.captured
		assert actual.value_identity == expected.value_identity
		assert actual.ticket_id == expected.ticket_id
		assert actual.app_identity == expected.app_identity
		assert actual.authority_scope == expected.authority_scope
		assert actual.authority_token == expected.authority_token
		assert actual.domain == expected.domain
		assert actual.release_kind == expected.release_kind
		assert actual.owner_seed == expected.owner_seed
		assert actual.proof_generation == expected.proof_generation
		assert native_operation_contexts_identical(actual.context, expected.context)
		assert actual.ticket_native_identity == expected.ticket_native_identity
		assert actual.required_parent_identity == expected.required_parent_identity
		assert actual.parent_authority_scope == expected.parent_authority_scope
		assert actual.parent_authority_token == expected.parent_authority_token
		assert actual.state == expected.state
	}

	fn renderer_fault_assert_win32_target_state_unchanged_for_test(app &App, window WindowId, expected RendererFaultWin32TargetState) RendererFaultWin32TargetState {
		renderer_fault_assert_win32_preexisting_target_state_for_test(app, expected, window)
		actual := renderer_fault_win32_target_state_for_test(app, window)
		renderer_fault_assert_win32_preexisting_target_state_for_test(app, actual, window)
		assert actual.window_identity == expected.window_identity
		assert actual.target_generation == expected.target_generation
		renderer_fault_assert_win32_target_authority_unchanged_for_test(expected.swapchain,
			actual.swapchain)
		renderer_fault_assert_win32_target_authority_unchanged_for_test(expected.render_view,
			actual.render_view)
		renderer_fault_assert_win32_target_authority_unchanged_for_test(expected.depth_texture,
			actual.depth_texture)
		renderer_fault_assert_win32_target_authority_unchanged_for_test(expected.depth_view,
			actual.depth_view)
		return actual
	}

	fn renderer_fault_wayland_frame_callback_state_for_test(app &App, window WindowId) RendererFaultWaylandFrameCallbackState {
		if app.backend.kind != .wayland {
			return RendererFaultWaylandFrameCallbackState{}
		}
		$if linux && sokol_wayland ? {
			index := app.backend.wayland.window_record_index(window) or {
				return RendererFaultWaylandFrameCallbackState{}
			}
			record := app.backend.wayland.windows[index]
			callback_identity := native_identity(record.frame_callback)
			parent_identity := native_identity(record.surface)
			mut ticket_count := 0
			mut ticket_release_kind := NativeLifetimeReleaseKind.none
			mut ticket_state := NativeLifetimeTicketState.reserved
			mut ticket_native_identity := u64(0)
			mut ticket_parent_identity := u64(0)
			mut ticket_owner_window := WindowId{}
			mut ticket_owner_generation := u64(0)
			mut ticket_context := NativeOperationContext{}
			if record.frame_callback_ticket != 0 {
				for ticket in app.backend.native_operations.lifetime_tickets {
					if ticket.ticket_id != record.frame_callback_ticket {
						continue
					}
					ticket_count++
					ticket_release_kind = ticket.release_kind
					ticket_state = ticket.state
					ticket_native_identity = ticket.native_identity
					ticket_parent_identity = ticket.required_parent_identity
					ticket_owner_window = ticket.owner_seed.window
					ticket_owner_generation = ticket.owner_seed.target_generation
					ticket_context = ticket.context
				}
			}
			return RendererFaultWaylandFrameCallbackState{
				captured:                true
				target_generation:       record.render_target_generation
				callback_identity:       callback_identity
				callback_ticket:         record.frame_callback_ticket
				parent_identity:         parent_identity
				ticket_count:            ticket_count
				ticket_release_kind:     ticket_release_kind
				ticket_state:            ticket_state
				ticket_native_identity:  ticket_native_identity
				ticket_parent_identity:  ticket_parent_identity
				ticket_owner_window:     ticket_owner_window
				ticket_owner_generation: ticket_owner_generation
				ticket_context:          ticket_context
			}
		} $else {
			return RendererFaultWaylandFrameCallbackState{}
		}
	}

	fn renderer_fault_assert_x11_surface_state_unchanged_for_test(app &App, window WindowId, expected RendererFaultX11SurfaceState) {
		assert expected.captured
		assert expected.surface_identity != 0
		assert expected.surface_ticket != 0
		actual := renderer_fault_x11_surface_state_for_test(app, window)
		assert actual.captured
		assert actual.target_generation == expected.target_generation
		assert actual.surface_identity == expected.surface_identity
		assert actual.surface_ticket == expected.surface_ticket
		assert actual.ticket_identity == expected.ticket_identity
		assert native_operation_contexts_identical(actual.ticket_context, expected.ticket_context)
	}

	fn renderer_fault_assert_wayland_frame_suffix_for_test(app &App, proof &NativeOperationProofState, lease RenderTargetLease, target RendererFaultWaylandTargetState, start int, expected_activate_ordinal u64) int {
		assert app.backend.kind == .wayland
		assert target.captured
		assert target.materialized
		assert !target.pending_egl_resize
		assert target.target_generation != 0
		assert target.native_surface != 0
		assert target.display_identity != 0
		assert target.egl_surface.captured
		assert target.egl_surface.value_identity != 0
		assert start >= 0
		assert expected_activate_ordinal != 0
		mut index := start
		mut activate := NativeOperationContext{}
		index, activate = renderer_fault_assert_egl_binding_sequence_for_test(app, proof, index,
			.window_activate, .window_target, lease)
		assert activate.ordinal == expected_activate_ordinal
		assert activate.target_generation == target.target_generation
		assert activate.target_identity == target.egl_surface.value_identity
		mut finalize := NativeOperationContext{}
		index, finalize = renderer_fault_assert_egl_binding_sequence_for_test(app, proof, index,
			.window_finalize, .window_target, lease)
		assert finalize.ordinal == activate.ordinal + 5
		assert finalize.target_generation == target.target_generation
		assert finalize.target_identity == target.egl_surface.value_identity
		frame_create_start := index
		mut frame_create := NativeOperationContext{}
		index, frame_create = renderer_fault_assert_wayland_chain_at_for_test(app, proof, index,
			.frame_callback, .window_finalize, .window_target)
		renderer_fault_assert_window_context_for_test(app, frame_create, lease, true)
		assert frame_create.ordinal == finalize.ordinal + 5
		assert frame_create.target_generation == target.target_generation
		assert frame_create.target_identity == target.native_surface
		assert proof.trace[frame_create_start + 3].context.target_identity == target.display_identity
		frame_callback_identity := proof.trace[index - 7].actual.handle
		assert frame_callback_identity != 0
		listener_start := index
		mut listener := NativeOperationContext{}
		index, listener = renderer_fault_assert_wayland_chain_at_for_test(app, proof, index,
			.frame_callback, .window_finalize, .window_target)
		renderer_fault_assert_window_context_for_test(app, listener, lease, true)
		assert listener.ordinal == frame_create.ordinal + 2
		assert listener.target_generation == target.target_generation
		assert listener.target_identity == frame_callback_identity
		assert listener.target_identity != frame_create.target_identity
		assert proof.trace[listener_start + 3].context.target_identity == target.display_identity
		swap := renderer_fault_assert_native_chain_at_for_test(proof, index, .egl, .swap_buffers,
			.window_finalize, .window_target)
		renderer_fault_assert_window_context_for_test(app, swap, lease, true)
		assert swap.target_generation == target.target_generation
		assert swap.target_identity == target.egl_surface.value_identity
		assert swap.ordinal == frame_create.ordinal + 5
		assert proof.trace[index + 1].actual.has(native_valid_return_value)
		assert proof.trace[index + 1].actual.return_value == 1
		index += 5
		flush_start := index
		mut flush := NativeOperationContext{}
		index, flush = renderer_fault_assert_wayland_chain_at_for_test(app, proof, index,
			.display_flush, .window_finalize, .window_target)
		renderer_fault_assert_window_context_for_test(app, flush, lease, true)
		assert flush.ordinal == swap.ordinal + 2
		assert flush.target_generation == target.target_generation
		assert flush.target_identity == target.display_identity
		assert proof.trace[flush_start + 3].context.target_identity == target.display_identity
		return index
	}

	fn renderer_fault_assert_wayland_first_target_frame_delta_for_test(app &App, lease RenderTargetLease, before RendererFaultWaylandTargetState, expected_first_ordinal u64) int {
		proof := renderer_fault_native_proof_for_test(app)
		assert proof.trace_len == 82
		mut index, target := renderer_fault_assert_wayland_lazy_first_target_prefix_for_test(app,
			proof, lease, before, expected_first_ordinal)
		assert index == 13
		assert target.target_generation == before.target_generation
		index = renderer_fault_assert_wayland_frame_suffix_for_test(app, proof, lease, target,
			index, expected_first_ordinal + 6)
		assert index == 82
		assert index == proof.trace_len
		assert renderer_fault_native_submission_count_for_test(app) == 1
		return index
	}

	fn renderer_fault_assert_frame_native_delta_for_test(app &App, lease RenderTargetLease, include_batch_begin bool, x11_surface_before RendererFaultX11SurfaceState) int {
		renderer_fault_assert_target_lease_for_test(app, lease)
		if app.backend.kind == .x11 && include_batch_begin {
			assert x11_surface_before.captured
			proof := renderer_fault_native_proof_for_test(app)
			mut index := 0
			mut batch_anchor := NativeOperationContext{}
			index, batch_anchor = renderer_fault_assert_egl_binding_sequence_for_test(app, proof,
				index, .anchor_prepare, .anchor, RenderTargetLease{})
			assert batch_anchor.ordinal == proof.ordinal_floor
			assert index == 20
			return renderer_fault_assert_x11_window_chain_for_test(app, proof, lease,
				x11_surface_before, index, batch_anchor.ordinal + 5)
		}
		proof := renderer_fault_native_proof_for_test(app)
		mut index := 0
		match app.backend.kind {
			.x11 {
				mut activate := NativeOperationContext{}
				index, activate = renderer_fault_assert_egl_binding_sequence_for_test(app, proof,
					index, .window_activate, .window_target, lease)
				assert activate.ordinal == proof.ordinal_floor
				if include_batch_begin {
					assert activate.target_identity == x11_surface_before.surface_identity
					assert activate.target_identity == x11_surface_before.ticket_identity
					assert activate.target_generation == x11_surface_before.target_generation
				}
				mut finalize := NativeOperationContext{}
				index, finalize = renderer_fault_assert_egl_binding_sequence_for_test(app, proof,
					index, .window_finalize, .window_target, lease)
				assert finalize.ordinal == activate.ordinal + 5
				assert finalize.target_generation == activate.target_generation
				assert finalize.target_identity == activate.target_identity
				swap := renderer_fault_assert_native_chain_at_for_test(proof, index, .egl,
					.swap_buffers, .window_finalize, .window_target)
				renderer_fault_assert_window_context_for_test(app, swap, lease, true)
				assert swap.target_generation == finalize.target_generation
				assert swap.target_identity == finalize.target_identity
				assert swap.ordinal == finalize.ordinal + 5
				assert proof.trace[index + 1].actual.has(native_valid_return_value)
				assert proof.trace[index + 1].actual.return_value == 1
				index += 5
			}
			.wayland {
				mut expected_activate_ordinal := proof.ordinal_floor
				if include_batch_begin {
					mut batch_anchor := NativeOperationContext{}
					index, batch_anchor = renderer_fault_assert_egl_binding_sequence_for_test(app,
						proof, index, .anchor_prepare, .anchor, RenderTargetLease{})
					assert batch_anchor.ordinal == proof.ordinal_floor
					expected_activate_ordinal = batch_anchor.ordinal + 5
				}
				target := renderer_fault_wayland_target_state_for_test(app, lease.window)
				index = renderer_fault_assert_wayland_frame_suffix_for_test(app, proof, lease,
					target, index, expected_activate_ordinal)
			}
			.appkit {
				mut batch_begin := NativeOperationContext{}
				mut pool_identity := u64(0)
				if include_batch_begin {
					index, batch_begin, pool_identity = renderer_fault_assert_appkit_batch_begin_for_test(app,
						proof, index, lease.batch_epoch)
					assert batch_begin.ordinal == proof.ordinal_floor
				}
				acquire := renderer_fault_assert_native_chain_at_for_test(proof, index, .metal,
					.drawable_acquire, .window_activate, .window_target)
				renderer_fault_assert_window_context_for_test(app, acquire, lease, true)
				expected_acquire_ordinal := if include_batch_begin {
					batch_begin.ordinal + 2
				} else {
					proof.ordinal_floor
				}
				assert acquire.ordinal == expected_acquire_ordinal
				assert proof.trace[index + 1].actual.has(native_valid_handle)
				drawable := proof.trace[index + 1].actual.handle
				assert drawable != 0
				index += 5
				present := renderer_fault_assert_native_chain_at_for_test(proof, index, .metal,
					.present, .window_finalize, .window_target)
				renderer_fault_assert_window_context_for_test(app, present, lease, true)
				assert present.target_generation == acquire.target_generation
				assert present.target_identity == drawable
				assert present.ordinal == acquire.ordinal + 2
				assert proof.trace[index + 1].actual.object_identity_0 == drawable
				index += 5
				index = renderer_fault_assert_appkit_drawable_release_for_test(app, proof, index,
					acquire, present, drawable)
				expected_pool_ordinal := if include_batch_begin {
					batch_begin.ordinal + 1
				} else {
					acquire.ordinal - 1
				}
				index = renderer_fault_assert_appkit_pool_release_for_test(app, proof, index,
					lease.batch_epoch, expected_pool_ordinal, pool_identity)
			}
			.win32 {
				present := renderer_fault_assert_native_chain_at_for_test(proof, index, .dxgi,
					.present, .window_finalize, .window_target)
				renderer_fault_assert_window_context_for_test(app, present, lease, true)
				assert present.ordinal == proof.ordinal_floor
				assert proof.trace[index + 1].actual.has(native_valid_return_value)
				assert proof.trace[index + 1].actual.return_value >= 0
				index += 5
			}
			else {
				assert false, 'renderer frame proof selected unsupported backend'
			}
		}

		assert index == proof.trace_len
		assert renderer_fault_native_submission_count_for_test(app) == 1
		return index
	}

	fn renderer_fault_assert_x11_window_chain_for_test(app &App, proof &NativeOperationProofState, lease RenderTargetLease, x11_surface_before RendererFaultX11SurfaceState, start int, expected_first_ordinal u64) int {
		assert app.backend.kind == .x11
		assert x11_surface_before.captured
		assert x11_surface_before.target_generation != 0
		assert start >= 0
		assert expected_first_ordinal != 0
		mut index := start
		mut surface_handle := x11_surface_before.surface_identity
		mut expected_activate_ordinal := expected_first_ordinal
		expected_trace_len := if surface_handle == 0 { start + 50 } else { start + 45 }
		if surface_handle == 0 {
			assert x11_surface_before.surface_ticket == 0
			surface_create := renderer_fault_assert_native_chain_at_for_test(proof, index, .egl,
				.window_surface_create, .window_prepare, .window_target)
			renderer_fault_assert_window_context_for_test(app, surface_create, lease, true)
			assert surface_create.ordinal == expected_first_ordinal
			assert surface_create.target_generation == x11_surface_before.target_generation
			assert proof.trace[index + 1].actual.has(native_valid_handle)
			surface_handle = proof.trace[index + 1].actual.handle
			assert surface_handle != 0
			index += 5

			mut surface_ticket_ordinal := u64(0)
			mut surface_ticket_id := u64(0)
			mut surface_ticket_context := NativeOperationContext{}
			mut surface_ticket_count := 0
			for ticket in app.backend.native_operations.lifetime_tickets {
				if ticket.release_kind != .egl_surface || ticket.owner_seed.window != lease.window {
					continue
				}
				surface_ticket_count++
				assert ticket.state == .bound
				assert ticket.native_identity == surface_handle
				assert ticket.owner_seed.target_generation == x11_surface_before.target_generation
				surface_ticket_ordinal = ticket.context.ordinal
				surface_ticket_id = ticket.ticket_id
				surface_ticket_context = ticket.context
			}
			assert surface_ticket_count == 1
			assert surface_ticket_id != 0
			assert surface_ticket_ordinal == surface_create.ordinal + 2
			surface_after := renderer_fault_x11_surface_state_for_test(app, lease.window)
			assert surface_after.captured
			assert surface_after.target_generation == x11_surface_before.target_generation
			assert surface_after.surface_identity == surface_handle
			assert surface_after.surface_ticket == surface_ticket_id
			assert surface_after.ticket_identity == surface_handle
			assert native_operation_contexts_identical(surface_after.ticket_context,
				surface_ticket_context)
			expected_activate_ordinal = surface_ticket_ordinal + 1
			assert expected_activate_ordinal == surface_create.ordinal + 3
		} else {
			assert x11_surface_before.surface_ticket != 0
			assert x11_surface_before.ticket_identity == surface_handle
			renderer_fault_assert_x11_surface_state_unchanged_for_test(app, lease.window,
				x11_surface_before)
		}

		mut activate := NativeOperationContext{}
		index, activate = renderer_fault_assert_egl_binding_sequence_for_test(app, proof, index,
			.window_activate, .window_target, lease)
		assert activate.ordinal == expected_activate_ordinal
		assert activate.target_identity == surface_handle
		assert activate.target_generation == x11_surface_before.target_generation
		if x11_surface_before.surface_identity != 0 {
			assert activate.target_identity == x11_surface_before.ticket_identity
		}

		mut finalize := NativeOperationContext{}
		index, finalize = renderer_fault_assert_egl_binding_sequence_for_test(app, proof, index,
			.window_finalize, .window_target, lease)
		assert finalize.ordinal == activate.ordinal + 5
		assert finalize.target_generation == activate.target_generation
		assert finalize.target_identity == surface_handle

		swap := renderer_fault_assert_native_chain_at_for_test(proof, index, .egl, .swap_buffers,
			.window_finalize, .window_target)
		renderer_fault_assert_window_context_for_test(app, swap, lease, true)
		assert swap.target_generation == finalize.target_generation
		assert swap.target_identity == surface_handle
		assert swap.ordinal == finalize.ordinal + 5
		assert proof.trace[index + 1].actual.has(native_valid_return_value)
		assert proof.trace[index + 1].actual.return_value == 1
		index += 5
		assert index == expected_trace_len
		assert index == proof.trace_len
		assert renderer_fault_native_submission_count_for_test(app) == 1
		return index
	}

	fn renderer_fault_assert_win32_success_evidence_for_test(proof &NativeOperationProofState, start int, has_handle bool) u64 {
		actual := proof.trace[start + 1].actual
		expected_mask := if has_handle {
			native_valid_return_value | native_valid_handle
		} else {
			native_valid_return_value
		}
		assert actual.valid_mask == expected_mask
		assert actual.return_value >= 0
		if has_handle {
			assert actual.handle != 0
			return actual.handle
		}
		assert actual.handle == 0
		return 0
	}

	fn renderer_fault_assert_win32_object_release_for_test(app &App, proof &NativeOperationProofState, start int, seed NativeOperationSeed, authority_scope NativeOperationAuthorityScope, ticket_id u64, identity u64) int {
		assert start >= 0
		assert start + 6 <= proof.trace_len
		assert ticket_id != 0
		assert identity != 0
		assert seed.presence_mask & native_context_has_target_identity == 0
		assert seed.target_identity == 0
		expected_milestones := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.authority_release,
			.health_latched,
		]
		context := proof.trace[start].context
		renderer_fault_assert_scoped_authority_context_for_test(app, context)
		assert context.authority_scope == authority_scope
		assert context.domain == .dxgi
		assert context.operation == .object_release
		assert context.call_site == seed.call_site
		assert context.scope == seed.scope
		assert context.presence_mask == seed.presence_mask | native_context_has_target_identity
		assert context.window == seed.window
		assert context.target_generation == seed.target_generation
		assert context.target_identity == identity
		assert context.batch_epoch == seed.batch_epoch
		assert context.window_lease_epoch == seed.window_lease_epoch
		assert context.target_lease_epoch == seed.target_lease_epoch
		assert context.ordinal == ticket_id
		for offset, milestone in expected_milestones {
			entry := proof.trace[start + offset]
			assert entry.milestone == milestone
			assert entry.context == context
		}
		real_call := proof.trace[start]
		actual_capture := proof.trace[start + 1]
		effective_capture := proof.trace[start + 2]
		accepted := proof.trace[start + 3]
		release := proof.trace[start + 4]
		health := proof.trace[start + 5]
		assert real_call.actual == NativePrimitiveEvidence{}
		assert real_call.effective == NativePrimitiveEvidence{}
		assert actual_capture.actual.valid_mask == native_valid_observed_count
		assert actual_capture.effective == NativePrimitiveEvidence{}
		assert effective_capture.actual == NativePrimitiveEvidence{}
		assert effective_capture.effective == actual_capture.actual
		assert accepted.actual == actual_capture.actual
		assert accepted.effective == actual_capture.actual
		assert accepted.local_validation == .void_completion
		expected_result := NativeRenderResult{
			domain:           .dxgi
			operation:        .object_release
			scope:            context.scope
			disposition:      .ok
			context:          context
			actual_primitive: actual_capture.actual
			primitive:        actual_capture.actual
			local_validation: .void_completion
		}
		assert accepted.result == expected_result
		assert release.actual == actual_capture.actual
		assert release.effective == actual_capture.actual
		assert release.local_validation == .void_completion
		assert release.result == expected_result
		assert health.actual == NativePrimitiveEvidence{}
		assert health.effective == NativePrimitiveEvidence{}
		assert health.local_validation == .none
		assert health.result == NativeRenderResult{}
		assert health.health == .ready
		return start + 6
	}

	fn renderer_fault_assert_win32_transient_backbuffer_release_for_test(app &App, proof &NativeOperationProofState, start int, lease RenderTargetLease, target_generation u64, expected_ordinal u64, identity u64) int {
		assert start == 20
		seed := NativeOperationSeed{
			presence_mask:      native_context_window_target_fields
			call_site:          .window_prepare
			scope:              .window_target
			window:             lease.window
			target_generation:  target_generation
			batch_epoch:        lease.batch_epoch
			window_lease_epoch: lease.window_epoch
			target_lease_epoch: lease.target_epoch
		}
		return renderer_fault_assert_win32_object_release_for_test(app, proof, start, seed,
			.renderer_attempt, expected_ordinal, identity)
	}

	fn renderer_fault_assert_win32_created_target_authority_for_test(app &App, authority RendererFaultLifetimeTeardownAuthority, lease RenderTargetLease, target_generation u64, expected_ticket u64, proof_generation u64) {
		renderer_fault_assert_win32_target_authority_for_test(app, authority, lease.window,
			target_generation)
		assert authority.ticket_id == expected_ticket
		assert proof_generation != 0
		assert authority.proof_generation == proof_generation
		assert authority.owner_seed.presence_mask == native_context_window_target_fields
		assert authority.owner_seed.call_site == .window_prepare
		assert authority.owner_seed.batch_epoch == lease.batch_epoch
		assert authority.owner_seed.window_lease_epoch == lease.window_epoch
		assert authority.owner_seed.target_lease_epoch == lease.target_epoch
	}

	fn renderer_fault_assert_win32_target_acquire_chain_for_test(app &App, lease RenderTargetLease, before RendererFaultWin32TargetState) int {
		assert app.backend.kind == .win32
		assert before.captured
		assert before.window_identity != 0
		assert before.target_generation != 0
		renderer_fault_assert_target_lease_for_test(app, lease)
		proof := renderer_fault_native_proof_for_test(app)
		assert proof.ordinal_floor != 0
		$if windows && sokol_d3d11 ? {
			if before.materialized {
				after := renderer_fault_assert_win32_target_state_unchanged_for_test(app,
					lease.window, before)
				present := renderer_fault_assert_native_chain_at_for_test(proof, 0, .dxgi,
					.present, .window_finalize, .window_target)
				renderer_fault_assert_window_context_for_test(app, present, lease, true)
				assert present.target_generation == after.target_generation
				assert present.target_identity == after.swapchain.value_identity
				assert present.ordinal == proof.ordinal_floor
				renderer_fault_assert_win32_success_evidence_for_test(proof, 0, false)
				assert proof.trace_len == 5
				assert renderer_fault_native_submission_count_for_test(app) == 1
				return proof.trace_len
			}

			assert !before.swapchain.captured
			assert !before.render_view.captured
			assert !before.depth_texture.captured
			assert !before.depth_view.captured
			after := renderer_fault_win32_target_state_for_test(app, lease.window)
			assert after.captured
			assert after.materialized
			assert !after.render_resize_pending
			assert after.window_identity == before.window_identity
			assert after.target_generation == before.target_generation
			first_ordinal := proof.ordinal_floor
			renderer_fault_assert_win32_created_target_authority_for_test(app, after.swapchain,
				lease, after.target_generation, first_ordinal + 4, proof.generation)
			renderer_fault_assert_win32_created_target_authority_for_test(app, after.render_view,
				lease, after.target_generation, first_ordinal + 14, proof.generation)
			renderer_fault_assert_win32_created_target_authority_for_test(app, after.depth_texture,
				lease, after.target_generation, first_ordinal + 15, proof.generation)
			renderer_fault_assert_win32_created_target_authority_for_test(app, after.depth_view,
				lease, after.target_generation, first_ordinal + 16, proof.generation)

			device_identity := native_identity(app.backend.win32.device)
			factory_identity := native_identity(app.backend.win32.factory)
			assert device_identity != 0
			assert factory_identity != 0
			assert proof.trace_len == 41
			mut index := 0
			assert index == 0
			swapchain_create := renderer_fault_assert_native_chain_at_for_test(proof, index, .dxgi,
				.swapchain_create, .window_prepare, .window_target)
			renderer_fault_assert_window_context_for_test(app, swapchain_create, lease, true)
			assert swapchain_create.target_generation == after.target_generation
			assert swapchain_create.target_identity == after.window_identity
			assert swapchain_create.ordinal == first_ordinal
			swapchain_identity := renderer_fault_assert_win32_success_evidence_for_test(proof,
				index, true)
			assert swapchain_identity == after.swapchain.value_identity
			index += 5
			assert index == 5

			association := renderer_fault_assert_native_chain_at_for_test(proof, index, .dxgi,
				.window_association, .window_prepare, .window_target)
			renderer_fault_assert_window_context_for_test(app, association, lease, true)
			assert association.target_generation == after.target_generation
			assert association.target_identity == after.window_identity
			assert association.ordinal == first_ordinal + 2
			renderer_fault_assert_win32_success_evidence_for_test(proof, index, false)
			index += 5
			assert index == 10

			backbuffer_acquire := renderer_fault_assert_native_chain_at_for_test(proof, index,
				.dxgi, .backbuffer_acquire, .window_prepare, .window_target)
			renderer_fault_assert_window_context_for_test(app, backbuffer_acquire, lease, true)
			assert backbuffer_acquire.target_generation == after.target_generation
			assert backbuffer_acquire.target_identity == swapchain_identity
			assert backbuffer_acquire.ordinal == first_ordinal + 5
			backbuffer_identity := renderer_fault_assert_win32_success_evidence_for_test(proof,
				index, true)
			index += 5
			assert index == 15

			render_view_create := renderer_fault_assert_native_chain_at_for_test(proof, index,
				.dxgi, .render_view_create, .window_prepare, .window_target)
			renderer_fault_assert_window_context_for_test(app, render_view_create, lease, true)
			assert render_view_create.target_generation == after.target_generation
			assert render_view_create.target_identity == backbuffer_identity
			assert render_view_create.ordinal == first_ordinal + 7
			render_view_identity := renderer_fault_assert_win32_success_evidence_for_test(proof,
				index, true)
			assert render_view_identity == after.render_view.value_identity
			index += 5
			assert index == 20

			index = renderer_fault_assert_win32_transient_backbuffer_release_for_test(app, proof,
				index, lease, after.target_generation, first_ordinal + 13, backbuffer_identity)
			assert index == 26

			depth_texture_create := renderer_fault_assert_native_chain_at_for_test(proof, index,
				.dxgi, .depth_texture_create, .window_prepare, .window_target)
			renderer_fault_assert_window_context_for_test(app, depth_texture_create, lease, true)
			assert depth_texture_create.target_generation == after.target_generation
			assert depth_texture_create.target_identity == device_identity
			assert depth_texture_create.ordinal == first_ordinal + 9
			depth_texture_identity := renderer_fault_assert_win32_success_evidence_for_test(proof,
				index, true)
			assert depth_texture_identity == after.depth_texture.value_identity
			index += 5
			assert index == 31

			depth_view_create := renderer_fault_assert_native_chain_at_for_test(proof, index,
				.dxgi, .depth_view_create, .window_prepare, .window_target)
			renderer_fault_assert_window_context_for_test(app, depth_view_create, lease, true)
			assert depth_view_create.target_generation == after.target_generation
			assert depth_view_create.target_identity == depth_texture_identity
			assert depth_view_create.ordinal == first_ordinal + 11
			depth_view_identity := renderer_fault_assert_win32_success_evidence_for_test(proof,
				index, true)
			assert depth_view_identity == after.depth_view.value_identity
			index += 5
			assert index == 36

			present := renderer_fault_assert_native_chain_at_for_test(proof, index, .dxgi,
				.present, .window_finalize, .window_target)
			renderer_fault_assert_window_context_for_test(app, present, lease, true)
			assert present.target_generation == after.target_generation
			assert present.target_identity == swapchain_identity
			assert present.ordinal == first_ordinal + 17
			renderer_fault_assert_win32_success_evidence_for_test(proof, index, false)
			index += 5
			assert index == 41
			assert index == proof.trace_len
			for ticket in app.backend.native_operations.lifetime_tickets {
				assert ticket.ticket_id != first_ordinal + 13
				assert ticket.native_identity != backbuffer_identity
			}
			assert renderer_fault_native_submission_count_for_test(app) == 1
			return index
		} $else {
			assert false, 'renderer Win32 target proof requires Windows D3D11 support'
			return 0
		}
	}

	fn renderer_fault_assert_target_acquire_native_delta_for_test(app &App, lease RenderTargetLease, x11_surface_before RendererFaultX11SurfaceState, wayland_target_before RendererFaultWaylandTargetState, win32_target_before RendererFaultWin32TargetState) int {
		match app.backend.kind {
			.x11 {
				renderer_fault_assert_target_lease_for_test(app, lease)
				proof := renderer_fault_native_proof_for_test(app)
				return renderer_fault_assert_x11_window_chain_for_test(app, proof, lease,
					x11_surface_before, 0, proof.ordinal_floor)
			}
			.wayland {
				proof := renderer_fault_native_proof_for_test(app)
				return renderer_fault_assert_wayland_first_target_frame_delta_for_test(app, lease,
					wayland_target_before, proof.ordinal_floor)
			}
			.win32 {
				return renderer_fault_assert_win32_target_acquire_chain_for_test(app, lease,
					win32_target_before)
			}
			else {
				return renderer_fault_assert_frame_native_delta_for_test(app, lease, false, RendererFaultX11SurfaceState{})
			}
		}
	}

	fn renderer_fault_assert_anchor_begin_native_delta_for_test(app &App, start int, batch_epoch u64) int {
		proof := renderer_fault_native_proof_for_test(app)
		assert start == 0
		mut index := start
		match app.backend.kind {
			.appkit {
				index = renderer_fault_assert_appkit_pool_release_for_test(app, proof, index,
					batch_epoch, proof.ordinal_floor - 1, 0)
			}
			.x11, .wayland, .win32 {}
			else {
				assert false, 'renderer anchor-begin proof selected unsupported backend'
			}
		}

		assert index == proof.trace_len
		return index
	}

	fn renderer_fault_assert_precommit_native_delta_for_test(app &App, start int, batch_epoch u64) int {
		proof := renderer_fault_native_proof_for_test(app)
		assert start == 0
		mut index := start
		match app.backend.kind {
			.x11, .wayland {
				mut acquire := NativeOperationContext{}
				index, acquire = renderer_fault_assert_egl_binding_sequence_for_test(app, proof,
					index, .anchor_prepare, .anchor, RenderTargetLease{})
				assert acquire.ordinal == proof.ordinal_floor
				mut abort := NativeOperationContext{}
				index, abort = renderer_fault_assert_egl_binding_sequence_for_test(app, proof,
					index, .anchor_prepare, .anchor, RenderTargetLease{})
				assert abort.target_generation == acquire.target_generation
				assert abort.target_identity == acquire.target_identity
				assert abort.ordinal == acquire.ordinal + 5
			}
			.appkit {
				acquire := renderer_fault_assert_native_chain_at_for_test(proof, index, .metal,
					.drawable_acquire, .anchor_prepare, .anchor)
				renderer_fault_assert_appkit_anchor_context_for_test(app, acquire)
				assert acquire.ordinal == proof.ordinal_floor
				drawable := proof.trace[index + 1].actual.handle
				assert drawable != 0
				index += 5
				abort := renderer_fault_assert_native_chain_at_for_test(proof, index, .metal,
					.clear_state, .window_finalize, .anchor)
				renderer_fault_assert_appkit_anchor_context_for_test(app, abort)
				assert abort.target_identity == drawable
				assert abort.ordinal == acquire.ordinal + 2
				assert proof.trace[index + 1].actual.object_identity_0 == drawable
				index += 5
				index = renderer_fault_assert_appkit_drawable_release_for_test(app, proof, index,
					acquire, abort, drawable)
				index = renderer_fault_assert_appkit_pool_release_for_test(app, proof, index,
					batch_epoch, acquire.ordinal - 1, 0)
			}
			.win32 {}
			else {
				assert false, 'renderer precommit proof selected unsupported backend'
			}
		}

		assert index == proof.trace_len
		return index
	}

	fn renderer_fault_assert_scoped_authority_context_for_test(app &App, context NativeOperationContext) {
		assert context.app_identity == app.instance_id
		assert context.ordinal != 0
		match context.authority_scope {
			.app_lifetime {
				assert context.authority_token == app.backend.native_operations.app_lifetime_token
				assert context.renderer_attempt_token == 0
			}
			.renderer_attempt {
				assert context.authority_token == app.backend.native_operations.renderer_attempt_token
				assert context.renderer_attempt_token == context.authority_token
			}
			else {
				assert false, 'native lifetime context has no authority scope'
			}
		}
	}

	fn renderer_fault_assert_lifetime_release_chain_for_test(app &App, proof &NativeOperationProofState, start int, domain NativeRenderDomain, operation NativeRenderOperation, call_site NativeRenderCallSite, scope NativeRenderScope, authority_scope NativeOperationAuthorityScope, ordinal u64, identity u64) int {
		assert ordinal != 0
		assert identity != 0
		context := renderer_fault_assert_native_chain_at_for_test(proof, start, domain, operation,
			call_site, scope)
		renderer_fault_assert_scoped_authority_context_for_test(app, context)
		assert context.authority_scope == authority_scope
		assert context.ordinal == ordinal
		assert context.presence_mask & native_context_has_target_identity != 0
		assert context.target_identity == identity
		release := proof.trace[start + 5]
		assert release.milestone == .authority_release
		assert release.context == context
		assert release.actual == proof.trace[start + 1].actual
		assert release.effective == proof.trace[start + 2].effective
		assert release.local_validation == proof.trace[start + 3].local_validation
		assert release.result.context == context
		assert release.result.disposition == .ok
		return start + 6
	}

	fn renderer_fault_assert_wayland_anchor_release_for_test(app &App, proof &NativeOperationProofState, start int, authority RendererFaultLifetimeTeardownAuthority, expected_domain NativeRenderDomain, expected_kind NativeLifetimeReleaseKind, expected_parent_identity u64, expected_parent_scope NativeOperationAuthorityScope, target_generation u64) int {
		assert authority.captured
		assert authority.value_identity != 0
		assert authority.ticket_id != 0
		assert authority.app_identity == app.instance_id
		assert authority.authority_scope == .renderer_attempt
		assert authority.authority_token == app.backend.native_operations.renderer_attempt_token
		assert authority.domain == expected_domain
		assert authority.release_kind == expected_kind
		assert authority.ticket_native_identity == authority.value_identity
		assert authority.required_parent_identity == expected_parent_identity
		assert authority.parent_authority_scope == expected_parent_scope
		assert authority.parent_authority_token == if expected_parent_scope == .app_lifetime {
			app.backend.native_operations.app_lifetime_token
		} else if expected_parent_scope == .renderer_attempt {
			app.backend.native_operations.renderer_attempt_token
		} else {
			u64(0)
		}
		assert authority.state == .bound
		assert authority.owner_seed.call_site == .anchor_create
		assert authority.owner_seed.scope == .anchor
		assert authority.owner_seed.window == WindowId{}
		assert authority.owner_seed.presence_mask == native_context_has_target_generation
		assert authority.owner_seed.target_generation == target_generation
		assert authority.owner_seed.target_identity == 0
		assert authority.proof_generation == 0
		context := authority.context
		renderer_fault_assert_scoped_authority_context_for_test(app, context)
		assert context.authority_scope == .renderer_attempt
		assert context.domain == expected_domain
		assert context.operation == native_lifetime_release_operation(expected_kind)
		assert context.call_site == .anchor_create
		assert context.scope == .anchor
		assert context.presence_mask == native_context_has_target_generation | native_context_has_target_identity
		assert context.window == WindowId{}
		assert context.target_generation == target_generation
		assert context.target_identity == authority.value_identity
		assert context.ordinal == authority.ticket_id
		assert start < proof.trace_len
		assert native_operation_contexts_identical(proof.trace[start].context, context)
		return renderer_fault_assert_lifetime_release_chain_for_test(app, proof, start,
			expected_domain, native_lifetime_release_operation(expected_kind), .anchor_create,
			.anchor, .renderer_attempt, authority.ticket_id, authority.value_identity)
	}

	fn renderer_fault_assert_window_lifetime_release_for_test(app &App, proof &NativeOperationProofState, start int, window WindowId, authority RendererFaultLifetimeTeardownAuthority, expected_domain NativeRenderDomain, expected_kind NativeLifetimeReleaseKind, expected_call_site NativeRenderCallSite, expected_authority_scope NativeOperationAuthorityScope, expected_parent_identity u64) int {
		assert authority.captured
		assert authority.value_identity != 0
		assert authority.ticket_id != 0
		assert authority.app_identity == app.instance_id
		assert authority.authority_scope == expected_authority_scope
		assert authority.authority_token != 0
		assert authority.domain == expected_domain
		assert authority.release_kind == expected_kind
		assert authority.ticket_native_identity == authority.value_identity
		assert authority.required_parent_identity == expected_parent_identity
		assert authority.state == .bound
		assert authority.owner_seed.call_site == expected_call_site
		assert authority.owner_seed.scope == .window_target
		assert authority.owner_seed.window == window
		assert authority.owner_seed.target_generation != 0
		assert authority.owner_seed.presence_mask & native_context_has_window != 0
		assert authority.owner_seed.presence_mask & native_context_has_target_generation != 0
		assert authority.owner_seed.presence_mask & native_context_has_target_identity == 0
		assert authority.owner_seed.target_identity == 0
		context := authority.context
		renderer_fault_assert_scoped_authority_context_for_test(app, context)
		assert context.authority_scope == authority.authority_scope
		assert context.authority_token == authority.authority_token
		assert context.domain == expected_domain
		assert context.operation == native_lifetime_release_operation(expected_kind)
		assert context.call_site == expected_call_site
		assert context.scope == .window_target
		assert context.presence_mask == authority.owner_seed.presence_mask | native_context_has_target_identity
		assert context.window == authority.owner_seed.window
		assert context.target_generation == authority.owner_seed.target_generation
		assert context.target_identity == authority.value_identity
		assert context.batch_epoch == authority.owner_seed.batch_epoch
		assert context.window_lease_epoch == authority.owner_seed.window_lease_epoch
		assert context.target_lease_epoch == authority.owner_seed.target_lease_epoch
		assert context.ordinal == authority.ticket_id
		assert start < proof.trace_len
		assert native_operation_contexts_identical(proof.trace[start].context, context)
		return renderer_fault_assert_lifetime_release_chain_for_test(app, proof, start,
			expected_domain, native_lifetime_release_operation(expected_kind), expected_call_site,
			.window_target, expected_authority_scope, authority.ticket_id, authority.value_identity)
	}

	fn renderer_fault_assert_win32_window_lifetime_release_for_test(app &App, proof &NativeOperationProofState, start int, window WindowId, target_generation u64, authority RendererFaultLifetimeTeardownAuthority) int {
		assert app.backend.kind == .win32
		renderer_fault_assert_win32_target_authority_for_test(app, authority, window,
			target_generation)
		assert start >= 0
		assert start < proof.trace_len
		assert native_operation_contexts_identical(authority.context, proof.trace[start].context)
		return renderer_fault_assert_win32_object_release_for_test(app, proof, start,
			authority.owner_seed, .app_lifetime, authority.ticket_id, authority.value_identity)
	}

	fn renderer_fault_assert_window_teardown_native_prefix_for_test(app &App, before_stop RendererFaultStateSnapshot, authority RendererFaultWindowTeardownAuthority, callback RendererFaultWaylandFrameCallbackState, start int, expected_first_ordinal u64) (int, u64) {
		proof := renderer_fault_native_proof_for_test(app)
		assert authority.captured
		assert authority.backend == app.backend.kind
		assert authority.window == before_stop.window
		assert authority.target_generation != 0
		assert authority.target_generation == before_stop.window_runtime.target.target_identity
		assert authority.native_window_identity != 0
		assert !authority.native_destroyed
		assert start >= 0
		assert start <= proof.trace_len
		assert expected_first_ordinal != 0
		mut index := start
		mut next_ordinal := expected_first_ordinal
		match authority.backend {
			.x11 {
				assert authority.resources.len == 1
				index = renderer_fault_assert_window_lifetime_release_for_test(app, proof, index,
					authority.window, authority.resources[0], .egl, .egl_surface, .window_prepare,
					.app_lifetime, before_stop.backend_display_identity)
			}
			.wayland {
				assert before_stop.wayland_display_identity != 0
				assert callback.captured
				assert callback.target_generation == authority.target_generation
				assert callback.parent_identity == authority.native_window_identity
				has_callback := callback.callback_identity != 0
				assert authority.resources.len == if has_callback {
					3
				} else {
					2
				}
				mut resource_index := 0
				if has_callback {
					callback_authority := authority.resources[resource_index]
					assert callback_authority.value_identity == callback.callback_identity
					assert callback_authority.ticket_id == callback.callback_ticket
					assert native_operation_contexts_identical(callback_authority.context,
						callback.ticket_context)
					index = renderer_fault_assert_window_lifetime_release_for_test(app, proof,
						index, authority.window, callback_authority, .wayland,
						.wayland_frame_callback, .window_finalize, .renderer_attempt,
						authority.native_window_identity)
					resource_index++
				}
				index = renderer_fault_assert_window_lifetime_release_for_test(app, proof, index,
					authority.window, authority.resources[resource_index], .egl, .egl_surface,
					.window_prepare, .app_lifetime, before_stop.backend_display_identity)
				resource_index++
				index = renderer_fault_assert_window_lifetime_release_for_test(app, proof, index,
					authority.window, authority.resources[resource_index], .wayland,
					.wayland_egl_window, .window_prepare, .app_lifetime,
					authority.native_window_identity)
				flush_start := index
				mut flush := NativeOperationContext{}
				index, flush = renderer_fault_assert_wayland_chain_at_for_test(app, proof, index,
					.display_flush, .shutdown_release, .window_target)
				assert flush.presence_mask == native_context_has_window | native_context_has_target_generation | native_context_has_target_identity
				assert flush.window == authority.window
				assert flush.target_generation == authority.target_generation
				assert flush.target_identity == before_stop.wayland_display_identity
				assert flush.ordinal == next_ordinal
				renderer_fault_assert_wayland_flush_actual_for_test(proof.trace[flush_start + 1].actual)
				next_ordinal += 2
			}
			.appkit {
				assert authority.resources.len == 1
				destroy := renderer_fault_assert_native_chain_at_for_test(proof, index, .metal,
					.surface_destroy, .shutdown_release, .window_target)
				renderer_fault_assert_authority_context_for_test(app, destroy)
				assert destroy.presence_mask == native_context_has_window | native_context_has_target_generation | native_context_has_target_identity
				assert destroy.window == authority.window
				assert destroy.target_generation == authority.target_generation
				assert destroy.target_identity == authority.native_window_identity
				assert destroy.ordinal == next_ordinal
				assert proof.trace[index + 1].actual == NativePrimitiveEvidence{}
				index = renderer_fault_assert_appkit_surface_destroy_authority_release_for_test(proof,
					index, destroy)
				next_ordinal++
				index = renderer_fault_assert_window_lifetime_release_for_test(app, proof, index,
					authority.window, authority.resources[0], .metal, .appkit_state,
					.window_prepare, .app_lifetime, 0)
			}
			.win32 {
				assert authority.resources.len == 4
				for resource in authority.resources {
					index = renderer_fault_assert_win32_window_lifetime_release_for_test(app,
						proof, index, authority.window, authority.target_generation, resource)
				}
			}
			else {
				assert false, 'renderer window teardown proof selected unsupported backend'
			}
		}

		return index, next_ordinal
	}

	fn renderer_fault_assert_shutdown_native_phase_for_test(app &App, snapshot RendererFaultNativeProofSnapshot, before_stop RendererFaultStateSnapshot, start int, expected_first_ordinal u64, bridge_phase bool) {
		proof := renderer_fault_native_proof_for_test(app)
		assert proof.generation == snapshot.generation
		assert proof.ordinal_floor == snapshot.ordinal_floor
		assert proof.trace_len == snapshot.trace_len
		assert !proof.trace_overflow
		assert start >= 0
		assert start <= proof.trace_len
		assert expected_first_ordinal != 0
		mut index := start
		match app.backend.kind {
			.x11 {
				if bridge_phase {
					mut bind := NativeOperationContext{}
					index, bind = renderer_fault_assert_egl_binding_sequence_for_test(app, proof,
						index, .shutdown_anchor, .anchor, RenderTargetLease{})
					assert bind.domain == .egl
					assert bind.ordinal == expected_first_ordinal
					assert bind.target_generation == before_stop.backend_anchor_generation
					assert bind.target_identity == before_stop.backend_anchor_identity
					unbind := renderer_fault_assert_native_chain_at_for_test(proof, index, .egl,
						.make_current, .shutdown_anchor, .anchor)
					renderer_fault_assert_anchor_context_for_test(app, unbind)
					assert unbind.target_generation == bind.target_generation
					assert unbind.target_identity == bind.target_identity
					assert unbind.ordinal == bind.ordinal + 5
					assert proof.trace[index + 1].actual.has(native_valid_return_value)
					assert proof.trace[index + 1].actual.return_value == 1
					index += 5
					index = renderer_fault_assert_lifetime_release_chain_for_test(app, proof,
						index, .egl, .surface_destroy, .anchor_create, .anchor, .renderer_attempt,
						before_stop.backend_anchor_ticket, before_stop.backend_anchor_identity)
				} else {
					index = renderer_fault_assert_lifetime_release_chain_for_test(app, proof,
						index, .egl, .context_destroy, .renderer_start, .renderer, .app_lifetime,
						before_stop.backend_renderer_context_ticket,
						before_stop.backend_renderer_context_identity)
					index = renderer_fault_assert_lifetime_release_chain_for_test(app, proof,
						index, .egl, .display_terminate, .renderer_start, .renderer, .app_lifetime,
						before_stop.backend_display_ticket, before_stop.backend_display_identity)
					index = renderer_fault_assert_lifetime_release_chain_for_test(app, proof,
						index, .egl, .release_thread, .renderer_start, .renderer, .app_lifetime,
						before_stop.backend_thread_ticket,
						before_stop.backend_owner_thread_identity)
				}
			}
			.wayland {
				if bridge_phase {
					mut bind := NativeOperationContext{}
					index, bind = renderer_fault_assert_egl_binding_sequence_for_test(app, proof,
						index, .shutdown_anchor, .anchor, RenderTargetLease{})
					assert bind.domain == .egl
					assert bind.ordinal == expected_first_ordinal
					assert bind.target_generation == before_stop.backend_anchor_generation
					assert bind.target_identity == before_stop.backend_anchor_identity
					unbind := renderer_fault_assert_native_chain_at_for_test(proof, index, .egl,
						.make_current, .shutdown_anchor, .anchor)
					renderer_fault_assert_anchor_context_for_test(app, unbind)
					assert unbind.target_generation == bind.target_generation
					assert unbind.target_identity == bind.target_identity
					assert unbind.ordinal == bind.ordinal + 5
					assert proof.trace[index + 1].actual.has(native_valid_return_value)
					assert proof.trace[index + 1].actual.return_value == 1
					index += 5

					anchor_surface := before_stop.wayland_anchor_egl_surface
					anchor_wl_egl_window := before_stop.wayland_anchor_wl_egl_window
					anchor_wl_surface := before_stop.wayland_anchor_wl_surface
					assert anchor_surface.value_identity == before_stop.backend_anchor_identity
					assert anchor_surface.ticket_id == before_stop.backend_anchor_ticket
					assert anchor_wl_egl_window.ticket_id == anchor_surface.ticket_id + 1
					assert anchor_wl_surface.ticket_id == anchor_wl_egl_window.ticket_id + 1
					assert anchor_surface.required_parent_identity == before_stop.backend_display_identity
					assert anchor_wl_egl_window.required_parent_identity == anchor_wl_surface.value_identity
					assert anchor_wl_surface.required_parent_identity == before_stop.wayland_compositor_identity
					index = renderer_fault_assert_wayland_anchor_release_for_test(app, proof,
						index, anchor_surface, .egl, .egl_surface,
						before_stop.backend_display_identity, .app_lifetime,
						before_stop.backend_anchor_generation)
					index = renderer_fault_assert_wayland_anchor_release_for_test(app, proof,
						index, anchor_wl_egl_window, .wayland, .wayland_egl_window,
						anchor_wl_surface.value_identity, .renderer_attempt,
						before_stop.backend_anchor_generation)
					assert proof.trace[index + 1].actual.has(native_valid_observed_flags)
					assert proof.trace[index + 1].actual.observed_flags == wayland_anchor_release_protocol_destroy
					index = renderer_fault_assert_wayland_anchor_release_for_test(app, proof,
						index, anchor_wl_surface, .wayland, .wayland_surface,
						before_stop.wayland_compositor_identity, .none,
						before_stop.backend_anchor_generation)
				} else {
					index, _, _ = renderer_fault_assert_wayland_harvest_trace_for_test(app, proof,
						index, proof.trace_len, expected_first_ordinal,
						before_stop.wayland_display_identity, false, RendererFaultWaylandFrameCallbackState{})
					index = renderer_fault_assert_lifetime_release_chain_for_test(app, proof,
						index, .egl, .context_destroy, .renderer_start, .renderer, .app_lifetime,
						before_stop.backend_renderer_context_ticket,
						before_stop.backend_renderer_context_identity)
					index = renderer_fault_assert_lifetime_release_chain_for_test(app, proof,
						index, .egl, .display_terminate, .renderer_start, .renderer, .app_lifetime,
						before_stop.backend_display_ticket, before_stop.backend_display_identity)
					index = renderer_fault_assert_lifetime_release_chain_for_test(app, proof,
						index, .egl, .release_thread, .renderer_start, .renderer, .app_lifetime,
						before_stop.backend_thread_ticket,
						before_stop.backend_owner_thread_identity)
				}
			}
			.appkit {
				if bridge_phase {
					begin := renderer_fault_assert_native_chain_at_for_test(proof, index, .metal,
						.render_batch_begin, .shutdown_anchor, .batch)
					renderer_fault_assert_authority_context_for_test(app, begin)
					assert begin.ordinal == expected_first_ordinal
					assert begin.presence_mask == 0
					assert begin.target_identity == 0
					pool_identity := proof.trace[index + 1].actual.handle
					assert pool_identity != 0
					index += 5
					pool_release := renderer_fault_assert_native_chain_at_for_test(proof, index,
						.metal, .render_batch_end, .shutdown_anchor, .batch)
					renderer_fault_assert_authority_context_for_test(app, pool_release)
					assert pool_release.presence_mask == native_context_has_target_identity
					assert pool_release.target_identity == pool_identity
					assert pool_release.ordinal == begin.ordinal + 1
					index += 5
					pool_authority_release := proof.trace[index]
					assert pool_authority_release.milestone == .authority_release
					assert pool_authority_release.context == pool_release
					assert pool_authority_release.actual.has(native_valid_object_identity_0)
					assert pool_authority_release.actual.object_identity_0 == pool_identity
					assert pool_authority_release.actual == pool_authority_release.effective
					assert pool_authority_release.local_validation == .void_completion
					assert pool_authority_release.result.context == pool_release
					index++
					destroy := renderer_fault_assert_native_chain_at_for_test(proof, index, .metal,
						.surface_destroy, .shutdown_release, .anchor)
					renderer_fault_assert_appkit_anchor_context_for_test(app, destroy)
					assert destroy.target_identity == before_stop.backend_anchor_identity
					assert destroy.ordinal == pool_release.ordinal + 1
					index = renderer_fault_assert_appkit_surface_destroy_authority_release_for_test(proof,
						index, destroy)
					index = renderer_fault_assert_lifetime_release_chain_for_test(app, proof,
						index, .metal, .object_release, .anchor_create, .anchor, .renderer_attempt,
						before_stop.backend_anchor_ticket, before_stop.backend_anchor_identity)
				} else {
					index = renderer_fault_assert_lifetime_release_chain_for_test(app, proof,
						index, .metal, .object_release, .renderer_start, .renderer, .app_lifetime,
						before_stop.backend_renderer_device_ticket,
						before_stop.backend_renderer_device_identity)
				}
			}
			.win32 {
				if bridge_phase {
					status := renderer_fault_assert_native_chain_at_for_test(proof, index, .dxgi,
						.device_status, .shutdown_anchor, .renderer)
					renderer_fault_assert_authority_context_for_test(app, status)
					assert status.ordinal == expected_first_ordinal
					assert status.presence_mask == native_context_has_target_identity
					assert status.target_identity == before_stop.backend_renderer_device_identity
					index += 5
					anchor_identities := [before_stop.win32_anchor_depth_view,
						before_stop.win32_anchor_depth_texture, before_stop.win32_anchor_render_view,
						before_stop.win32_anchor_color_texture]
					anchor_tickets := [before_stop.win32_anchor_depth_view_ticket,
						before_stop.win32_anchor_depth_ticket, before_stop.win32_anchor_render_view_ticket,
						before_stop.win32_anchor_color_ticket]
					anchor_seed := NativeOperationSeed{
						call_site: .anchor_create
						scope:     .anchor
					}
					for offset, identity in anchor_identities {
						index = renderer_fault_assert_win32_object_release_for_test(app, proof,
							index, anchor_seed, .renderer_attempt, anchor_tickets[offset], identity)
					}
				} else {
					renderer_identities := [before_stop.backend_factory_identity,
						before_stop.backend_renderer_context_identity,
						before_stop.backend_renderer_device_identity]
					renderer_tickets := [before_stop.backend_factory_ticket,
						before_stop.backend_renderer_context_ticket, before_stop.backend_renderer_device_ticket]
					renderer_seed := NativeOperationSeed{
						call_site: .renderer_start
						scope:     .renderer
					}
					for offset, identity in renderer_identities {
						index = renderer_fault_assert_win32_object_release_for_test(app, proof,
							index, renderer_seed, .app_lifetime, renderer_tickets[offset], identity)
					}
				}
			}
			else {
				assert false, 'renderer shutdown proof selected unsupported backend'
			}
		}

		assert index == proof.trace_len
	}

	fn renderer_fault_native_proof_for_test(app &App) &NativeOperationProofState {
		assert app.backend.native_operations.proof != unsafe { nil }
		proof := app.backend.native_operations.proof
		assert !proof.trace_overflow
		return proof
	}

	fn renderer_fault_clear_native_trace_for_test(mut app App) RendererFaultNativeProofSnapshot {
		mut proof := renderer_fault_native_proof_for_test(app)
		assert !app.backend.native_operations.has_pending_native_plans()
		assert app.backend.native_operations.has_live_lifetime_tickets()
		generation := proof.generation
		ordinal_floor := proof.ordinal_floor
		next_ordinal := app.backend.native_operations.next_ordinal
		for index in 0 .. native_operation_trace_capacity {
			proof.trace[index] = NativeOperationTraceEntry{}
		}
		proof.trace_len = 0
		proof.trace_overflow = false
		fresh := renderer_fault_native_proof_capture_for_test(app)
		assert fresh.available
		assert fresh.snapshot.generation == generation
		assert fresh.snapshot.ordinal_floor == ordinal_floor
		assert fresh.snapshot.next_ordinal == next_ordinal
		assert fresh.snapshot.trace_len == 0
		assert !fresh.snapshot.trace_overflow
		assert fresh.snapshot.live_tickets > 0
		return fresh.snapshot
	}

	fn renderer_fault_assert_target_lease_for_test(app &App, lease RenderTargetLease) {
		assert lease.app_instance == app.instance_id
		assert lease.window.app_instance == app.instance_id
		assert lease.window.slot >= 0
		assert lease.window.generation != 0
		assert lease.batch_epoch != 0
		assert lease.window_epoch != 0
		assert lease.target_epoch != 0
	}

	fn renderer_fault_assert_zero_native_calls_for_test(app &App) {
		proof := renderer_fault_native_proof_for_test(app)
		assert proof.trace_len == 0
	}

	fn renderer_fault_native_submission_count_for_test(app &App) int {
		assert app.backend.native_operations.proof != unsafe { nil }
		proof := app.backend.native_operations.proof
		expected_domain, expected_operation := match app.backend.kind {
			.x11, .wayland { NativeRenderDomain.egl, NativeRenderOperation.swap_buffers }
			.appkit { NativeRenderDomain.metal, NativeRenderOperation.present }
			.win32 { NativeRenderDomain.dxgi, NativeRenderOperation.present }
			else { NativeRenderDomain.none, NativeRenderOperation.none }
		}

		mut count := 0
		for index in 0 .. proof.trace_len {
			entry := proof.trace[index]
			if entry.milestone == .real_call && entry.context.domain == expected_domain
				&& entry.context.operation == expected_operation
				&& entry.context.call_site == .window_finalize
				&& entry.context.scope == .window_target {
				count++
			}
		}
		return count
	}

	fn renderer_fault_assert_native_chain_at_for_test(proof &NativeOperationProofState, start int, domain NativeRenderDomain, operation NativeRenderOperation, call_site NativeRenderCallSite, scope NativeRenderScope) NativeOperationContext {
		expected_len := 5
		assert start >= 0
		assert start + expected_len <= proof.trace_len
		expected_milestones := [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
			.acceptance,
			.health_latched,
		]
		context := proof.trace[start].context
		assert context.domain == domain
		assert context.operation == operation
		assert context.call_site == call_site
		assert context.scope == scope
		assert context.ordinal != 0
		for offset, milestone in expected_milestones {
			entry := proof.trace[start + offset]
			assert entry.milestone == milestone
			assert entry.context == context
		}
		real_call := proof.trace[start]
		actual_capture := proof.trace[start + 1]
		effective_primitive := proof.trace[start + 2]
		health_latch := proof.trace[start + 4]
		wayland_release_mode_evidence := domain == .wayland && operation == .surface_destroy
			&& actual_capture.actual.valid_mask == native_valid_observed_flags
			&& actual_capture.actual.observed_flags in [u64(wayland_anchor_release_protocol_destroy), u64(wayland_anchor_release_local_proxy_destroy)]
		void_release_without_evidence := operation == .surface_destroy
			&& domain in [.wayland, .metal] && !wayland_release_mode_evidence
		assert real_call.actual == NativePrimitiveEvidence{}
		assert real_call.effective == NativePrimitiveEvidence{}
		if void_release_without_evidence {
			assert actual_capture.actual == NativePrimitiveEvidence{}
			assert effective_primitive.effective == NativePrimitiveEvidence{}
		} else {
			assert actual_capture.actual.valid_mask != 0
			assert effective_primitive.effective.valid_mask != 0
		}
		assert actual_capture.effective == NativePrimitiveEvidence{}
		assert effective_primitive.actual == NativePrimitiveEvidence{}
		if wayland_release_mode_evidence {
			assert effective_primitive.effective.valid_mask == native_valid_observed_flags
			assert effective_primitive.effective.observed_flags == actual_capture.actual.observed_flags
		}
		assert health_latch.actual == NativePrimitiveEvidence{}
		assert health_latch.effective == NativePrimitiveEvidence{}
		actual := proof.trace[start + 1].actual
		effective := proof.trace[start + 2].effective
		accepted := proof.trace[start + 3]
		assert void_release_without_evidence || actual.valid_mask != 0
		assert actual == effective
		assert accepted.actual == actual
		assert accepted.effective == effective
		expected_validation := if operation == .object_release
			|| (domain == .wayland && operation == .surface_destroy)
			|| (domain == .metal
			&& operation in [.present, .clear_state, .render_batch_end, .surface_destroy]) {
			NativeLocalValidation.void_completion
		} else {
			NativeLocalValidation.none
		}
		assert accepted.local_validation == expected_validation
		assert accepted.result.context == context
		assert accepted.result.actual_primitive == actual
		assert accepted.result.primitive == effective
		assert accepted.result.domain == domain
		assert accepted.result.operation == operation
		assert accepted.result.scope == scope
		assert accepted.result.disposition == .ok
		assert accepted.result.local_validation == expected_validation
		assert proof.trace[start + 4].health == .ready
		return context
	}

	fn renderer_fault_assert_appkit_surface_destroy_authority_release_for_test(proof &NativeOperationProofState, start int, context NativeOperationContext) int {
		assert start >= 0
		assert start + 6 <= proof.trace_len
		assert context.domain == .metal
		assert context.operation == .surface_destroy
		accepted := proof.trace[start + 3]
		release := proof.trace[start + 5]
		assert release.milestone == .authority_release
		assert release.context == context
		assert release.actual == proof.trace[start + 1].actual
		assert release.effective == proof.trace[start + 2].effective
		assert release.local_validation == accepted.local_validation
		assert release.result == accepted.result
		return start + 6
	}

	fn renderer_fault_assert_authority_context_for_test(app &App, context NativeOperationContext) {
		assert context.authority_scope == .renderer_attempt
		assert context.authority_token != 0
		assert context.authority_token == context.renderer_attempt_token
		assert context.authority_token == app.backend.native_operations.renderer_attempt_token
		assert context.app_identity == app.instance_id
		assert context.ordinal != 0
	}

	fn renderer_fault_assert_window_context_for_test(app &App, context NativeOperationContext, lease RenderTargetLease, has_target_identity bool) {
		renderer_fault_assert_authority_context_for_test(app, context)
		expected_presence := if has_target_identity {
			native_context_window_target_fields | native_context_has_target_identity
		} else {
			native_context_window_target_fields
		}
		assert context.presence_mask == expected_presence
		assert context.window == lease.window
		assert context.target_generation != 0
		assert (context.target_identity != 0) == has_target_identity
		assert context.batch_epoch == lease.batch_epoch
		assert context.window_lease_epoch == lease.window_epoch
		assert context.target_lease_epoch == lease.target_epoch
	}

	fn renderer_fault_assert_anchor_context_for_test(app &App, context NativeOperationContext) {
		renderer_fault_assert_authority_context_for_test(app, context)
		assert context.presence_mask == native_context_has_target_generation | native_context_has_target_identity
		assert context.window == WindowId{}
		assert context.target_generation != 0
		assert context.target_identity != 0
		assert context.batch_epoch == 0
		assert context.window_lease_epoch == 0
		assert context.target_lease_epoch == 0
	}

	fn renderer_fault_assert_appkit_anchor_context_for_test(app &App, context NativeOperationContext) {
		renderer_fault_assert_authority_context_for_test(app, context)
		assert context.presence_mask == native_context_has_target_identity
		assert context.window == WindowId{}
		assert context.target_generation == 0
		assert context.target_identity != 0
		assert context.batch_epoch == 0
		assert context.window_lease_epoch == 0
		assert context.target_lease_epoch == 0
	}

	fn renderer_fault_assert_egl_binding_sequence_for_test(app &App, proof &NativeOperationProofState, start int, call_site NativeRenderCallSite, scope NativeRenderScope, lease RenderTargetLease) (int, NativeOperationContext) {
		bind := renderer_fault_assert_native_chain_at_for_test(proof, start, .egl, .make_current,
			call_site, scope)
		if scope == .anchor {
			renderer_fault_assert_anchor_context_for_test(app, bind)
		} else {
			renderer_fault_assert_window_context_for_test(app, bind, lease, true)
		}
		assert proof.trace[start + 1].actual.has(native_valid_return_value)
		assert proof.trace[start + 1].actual.return_value == 1
		draw := renderer_fault_assert_native_chain_at_for_test(proof, start + 5, .egl,
			.current_draw_query, call_site, scope)
		read := renderer_fault_assert_native_chain_at_for_test(proof, start + 10, .egl,
			.current_read_query, call_site, scope)
		current := renderer_fault_assert_native_chain_at_for_test(proof, start + 15, .egl,
			.current_context_query, call_site, scope)
		assert draw.ordinal == bind.ordinal + 2
		assert read.ordinal == bind.ordinal + 3
		assert current.ordinal == bind.ordinal + 4
		for query in [draw, read, current] {
			renderer_fault_assert_authority_context_for_test(app, query)
			expected := NativeOperationContext{
				...bind
				presence_mask:   bind.presence_mask & ~native_context_has_target_identity
				target_identity: 0
				operation:       query.operation
				ordinal:         query.ordinal
			}
			assert query == expected
		}
		assert proof.trace[start + 6].actual.has(native_valid_handle)
		assert proof.trace[start + 6].actual.handle == bind.target_identity
		assert proof.trace[start + 11].actual.has(native_valid_handle)
		assert proof.trace[start + 11].actual.handle == bind.target_identity
		assert proof.trace[start + 16].actual.has(native_valid_handle)
		assert proof.trace[start + 16].actual.handle != 0
		return start + 20, bind
	}

	fn renderer_fault_assert_wayland_chain_with_disposition_at_for_test(app &App, proof &NativeOperationProofState, start int, operation NativeRenderOperation, call_site NativeRenderCallSite, scope NativeRenderScope, disposition NativeRenderDisposition) (int, NativeOperationContext) {
		assert start >= 0
		assert start + 8 <= proof.trace_len
		context := proof.trace[start].context
		renderer_fault_assert_authority_context_for_test(app, context)
		assert context.domain == .wayland
		assert context.operation == operation
		assert context.call_site == call_site
		assert context.scope == scope
		for offset, milestone in [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
		] {
			entry := proof.trace[start + offset]
			assert entry.milestone == milestone
			assert entry.context == context
		}
		assert proof.trace[start].actual == NativePrimitiveEvidence{}
		assert proof.trace[start].effective == NativePrimitiveEvidence{}
		if operation == .display_cancel {
			assert proof.trace[start + 1].actual == NativePrimitiveEvidence{}
		} else {
			assert proof.trace[start + 1].actual.valid_mask != 0
		}
		assert proof.trace[start + 1].effective == NativePrimitiveEvidence{}
		assert proof.trace[start + 2].actual == NativePrimitiveEvidence{}
		if operation == .display_cancel {
			assert proof.trace[start + 2].effective == NativePrimitiveEvidence{}
		} else {
			assert proof.trace[start + 2].effective.valid_mask != 0
		}
		evidence_context := proof.trace[start + 3].context
		renderer_fault_assert_authority_context_for_test(app, evidence_context)
		assert evidence_context.domain == .wayland
		assert evidence_context.operation == .wayland_display_error_query
		assert evidence_context.call_site == .display_transport
		assert evidence_context.scope == .renderer
		assert evidence_context.presence_mask == native_context_has_target_identity
		assert evidence_context.target_identity != 0
		assert evidence_context.ordinal == context.ordinal + 1
		for offset, milestone in [
			NativeOperationTraceMilestone.real_call,
			.actual_primitive,
			.effective_primitive,
		] {
			entry := proof.trace[start + 3 + offset]
			assert entry.milestone == milestone
			assert entry.context == evidence_context
		}
		assert proof.trace[start + 3].actual == NativePrimitiveEvidence{}
		assert proof.trace[start + 3].effective == NativePrimitiveEvidence{}
		assert proof.trace[start + 4].actual.valid_mask != 0
		assert proof.trace[start + 4].effective == NativePrimitiveEvidence{}
		assert proof.trace[start + 5].actual == NativePrimitiveEvidence{}
		assert proof.trace[start + 5].effective.valid_mask != 0
		actual := proof.trace[start + 1].actual
		effective := proof.trace[start + 2].effective
		evidence_actual := proof.trace[start + 4].actual
		evidence_effective := proof.trace[start + 5].effective
		if operation == .display_cancel {
			assert actual == NativePrimitiveEvidence{}
			assert effective == NativePrimitiveEvidence{}
		} else {
			assert actual.valid_mask != 0
		}
		assert actual == effective
		assert evidence_actual.valid_mask != 0
		assert evidence_actual == evidence_effective
		accepted_actual := native_primitive_with_wayland_display_error(actual, evidence_actual)
		accepted_effective := native_primitive_with_wayland_display_error(effective,
			evidence_effective)
		accepted := proof.trace[start + 6]
		assert accepted.milestone == .acceptance
		assert accepted.context == context
		assert accepted.actual == accepted_actual
		assert accepted.effective == accepted_effective
		assert accepted.result.context == context
		assert accepted.result.actual_primitive == accepted.actual
		assert accepted.result.primitive == accepted.effective
		assert accepted.result.domain == .wayland
		assert accepted.result.operation == operation
		assert accepted.result.scope == scope
		assert accepted.result.disposition == disposition
		assert accepted.result.local_validation == if operation == .display_cancel {
			NativeLocalValidation.void_completion
		} else {
			NativeLocalValidation.none
		}
		assert proof.trace[start + 7].milestone == .health_latched
		assert proof.trace[start + 7].context == context
		assert proof.trace[start + 7].health == .ready
		assert proof.trace[start + 7].actual == NativePrimitiveEvidence{}
		assert proof.trace[start + 7].effective == NativePrimitiveEvidence{}
		return start + 8, context
	}

	fn renderer_fault_assert_wayland_chain_at_for_test(app &App, proof &NativeOperationProofState, start int, operation NativeRenderOperation, call_site NativeRenderCallSite, scope NativeRenderScope) (int, NativeOperationContext) {
		return renderer_fault_assert_wayland_chain_with_disposition_at_for_test(app, proof, start,
			operation, call_site, scope, .ok)
	}

	fn renderer_fault_assert_appkit_batch_begin_for_test(app &App, proof &NativeOperationProofState, start int, batch_epoch u64) (int, NativeOperationContext, u64) {
		context := renderer_fault_assert_native_chain_at_for_test(proof, start, .metal,
			.render_batch_begin, .anchor_prepare, .batch)
		renderer_fault_assert_authority_context_for_test(app, context)
		assert context.presence_mask == native_context_has_batch_epoch
		assert context.batch_epoch == batch_epoch
		assert context.target_identity == 0
		pool_identity := proof.trace[start + 1].actual.handle
		assert pool_identity != 0
		return start + 5, context, pool_identity
	}

	fn renderer_fault_assert_appkit_drawable_release_for_test(app &App, proof &NativeOperationProofState, index int, acquire NativeOperationContext, operation NativeOperationContext, drawable u64) int {
		assert index < proof.trace_len
		entry := proof.trace[index]
		assert entry.milestone == .authority_release
		renderer_fault_assert_authority_context_for_test(app, entry.context)
		assert entry.context.domain == .metal
		assert entry.context.operation == .clear_state
		assert entry.context.call_site == acquire.call_site
		assert entry.context.scope == acquire.scope
		assert entry.context.target_identity == drawable
		assert entry.context.ordinal == acquire.ordinal + 1
		assert entry.actual.has(native_valid_object_identity_0)
		assert entry.actual.object_identity_0 == drawable
		assert entry.actual == entry.effective
		assert entry.local_validation == .void_completion
		assert entry.result.context == operation
		assert entry.result.disposition == .ok
		assert entry.result.local_validation == .void_completion
		return index + 1
	}

	fn renderer_fault_assert_appkit_pool_release_for_test(app &App, proof &NativeOperationProofState, start int, batch_epoch u64, expected_ordinal u64, expected_pool_identity u64) int {
		context := renderer_fault_assert_native_chain_at_for_test(proof, start, .metal,
			.render_batch_end, .anchor_prepare, .batch)
		renderer_fault_assert_authority_context_for_test(app, context)
		assert context.presence_mask == native_context_has_batch_epoch | native_context_has_target_identity
		assert context.batch_epoch == batch_epoch
		assert context.target_identity != 0
		if expected_ordinal != 0 {
			assert context.ordinal == expected_ordinal
		}
		if expected_pool_identity != 0 {
			assert context.target_identity == expected_pool_identity
		}
		assert proof.trace[start + 1].actual.object_identity_0 == context.target_identity
		release := proof.trace[start + 5]
		assert release.milestone == .authority_release
		assert release.context == context
		assert release.actual.has(native_valid_object_identity_0)
		assert release.actual.object_identity_0 == context.target_identity
		assert release.actual == release.effective
		assert release.local_validation == .void_completion
		assert release.result.context == context
		assert release.result.disposition == .ok
		assert release.result.local_validation == .void_completion
		return start + 6
	}

	fn renderer_fault_assert_sokol_operations_for_test(snapshot multiwindow_sokol_trace.TypedSnapshot, expected []multiwindow_sokol_trace.Operation) {
		assert snapshot.install_generation != 0
		assert !snapshot.overflow
		assert snapshot.records.len == expected.len
		for index, operation in expected {
			record := snapshot.records[index]
			assert record.operation == operation
			assert record.sequence == u64(index + 1)
			if operation == .begin_swapchain_pass {
				assert record.width > 0
				assert record.height > 0
				assert record.sample_count > 0
				assert record.color_format != 0
			}
		}
	}

	fn renderer_fault_assert_sokol_snapshots_equal_for_test(expected multiwindow_sokol_trace.TypedSnapshot, actual multiwindow_sokol_trace.TypedSnapshot) {
		assert actual == expected
	}

	fn renderer_fault_failed_start_milestones_for_test(stage InternalFaultStage) ![]RendererFaultMilestone {
		return match stage {
			.renderer_anchor_create {
				[RendererFaultMilestone.anchor_probe]
			}
			.renderer_environment {
				[RendererFaultMilestone.anchor_probe, .anchor_completed, .environment_probe,
					.anchor_destroyed]
			}
			.renderer_setup {
				[RendererFaultMilestone.anchor_probe, .anchor_completed, .environment_probe,
					.environment_acquired, .setup_probe, .anchor_destroyed]
			}
			else {
				error('unsupported renderer start fault stage `${stage}`')
			}
		}
	}

	fn renderer_fault_successful_start_milestones_for_test() []RendererFaultMilestone {
		return [
			RendererFaultMilestone.anchor_probe,
			.anchor_completed,
			.environment_probe,
			.environment_acquired,
			.setup_probe,
			.gfx_complete,
			.bridge_published,
		]
	}

	fn renderer_fault_assert_trace_for_test(snapshot RendererFaultTraceSnapshot, token u64, attempt_id u64, fault_pending bool, expected []RendererFaultMilestone) {
		assert snapshot.trace_enabled
		assert snapshot.fault_pending == fault_pending
		assert token != 0
		assert attempt_id != 0
		assert snapshot.token == token
		assert snapshot.attempt_id == attempt_id
		assert !snapshot.overflow
		assert snapshot.len == expected.len
		for index, milestone in expected {
			assert snapshot.milestones[index] == milestone
		}
		for index in expected.len .. renderer_fault_trace_capacity {
			assert snapshot.milestones[index] == .invalid
		}
	}

	fn renderer_fault_assert_trace_snapshots_equal_for_test(expected RendererFaultTraceSnapshot, actual RendererFaultTraceSnapshot) {
		assert actual.trace_enabled == expected.trace_enabled
		assert actual.fault_pending == expected.fault_pending
		assert actual.attempt_id == expected.attempt_id
		assert actual.token == expected.token
		assert actual.len == expected.len
		assert actual.overflow == expected.overflow
		assert actual.milestones == expected.milestones
	}

	fn renderer_fault_stop_twice_clean_for_test(mut app App) ! {
		mut first_stop_error := ''
		app.stop() or { first_stop_error = err.msg() }
		first_status := app.status()
		mut first_windows_error := ''
		first_remaining := app.window_ids() or {
			first_windows_error = err.msg()
			[]WindowId{}
		}
		mut replay_stop_error := ''
		app.stop() or { replay_stop_error = err.msg() }
		replay_status := app.status()
		mut replay_windows_error := ''
		replay_remaining := app.window_ids() or {
			replay_windows_error = err.msg()
			[]WindowId{}
		}
		mut failures := []string{}
		if first_stop_error != '' {
			failures << 'first stop failed: ${first_stop_error}'
		}
		if first_status != .stopped {
			failures << 'first stop left app in ${first_status} state'
		}
		if first_windows_error != '' {
			failures << 'first cleanup inspection failed: ${first_windows_error}'
		} else if first_remaining.len != 0 {
			failures << 'first cleanup left ${first_remaining.len} live window(s)'
		}
		if replay_stop_error != '' {
			failures << 'replayed stop failed: ${replay_stop_error}'
		}
		if replay_status != .stopped {
			failures << 'replayed stop left app in ${replay_status} state'
		}
		if replay_windows_error != '' {
			failures << 'replayed cleanup inspection failed: ${replay_windows_error}'
		} else if replay_remaining.len != 0 {
			failures << 'replayed cleanup left ${replay_remaining.len} live window(s)'
		}
		if failures.len > 0 {
			return error(failures.join('; '))
		}
	}
}

fn core_renderer_fault_stage_inventory() []InternalFaultStage {
	return [
		InternalFaultStage.renderer_anchor_create,
		.renderer_environment,
		.renderer_setup,
		.renderer_batch_begin,
		.renderer_target_acquire,
		.renderer_pass_begin,
		.renderer_anchor_begin,
		.renderer_precommit,
		.renderer_submission_finalize,
	]
}

fn test_teardown_prepare_fault_is_nonterminal_and_retryable() {
	mut app := new_app()!
	window := app.create_window(title: 'prepare fault')!
	message := 'fault:teardown_prepare'
	app.set_internal_fault(.teardown_prepare, 0, message)!
	mut actual := ''
	app.prepare_window_destroy(window) or { actual = err.msg() }
	assert actual == message
	assert app.window_exists(window)
	assert app.windows[window.slot].destroy_stage == .none
	assert app.render_runtime.windows[window.slot].status == .alive

	ticket := app.prepare_window_destroy(window)!
	app.rollback_window_destroy(ticket)!
	assert app.window_exists(window)
	app.stop()!
}

fn test_teardown_seal_fault_preserves_reversible_prepared_state() {
	mut app := new_app()!
	window := app.create_window(title: 'seal fault')!
	ticket := app.prepare_window_destroy(window)!
	message := 'fault:teardown_seal'
	app.set_internal_fault(.teardown_seal, 0, message)!
	mut actual := ''
	app.seal_window_destroy(ticket) or { actual = err.msg() }
	assert actual == message
	assert app.window_exists(window)
	assert app.windows[window.slot].destroy_stage == .prepared
	assert app.render_runtime.windows[window.slot].status == .preparing_destroy

	app.seal_window_destroy(ticket)!
	app.finish_window_destroy(ticket, [])!
	assert !app.window_exists(window)
	app.stop()!
}

fn test_teardown_backend_finish_fault_is_aggregated_and_replayed_once() {
	mut app := new_app()!
	window := app.create_window(title: 'backend finish fault')!
	_ = app.drain_events()!
	ticket := app.prepare_window_destroy(window)!
	app.seal_window_destroy(ticket)!
	message := 'fault:teardown_backend_finish'
	expected := '${err_render_terminal_aggregate}: ${message}'
	app.set_internal_fault(.teardown_backend_finish, 0, message)!
	mut first_error := ''
	app.finish_window_destroy(ticket, []) or { first_error = err.msg() }
	assert first_error == expected
	assert !app.window_exists(window)
	assert app.window_status(window)! == .destroyed

	mut replay_error := ''
	app.finish_window_destroy(ticket, ['must not be appended']) or { replay_error = err.msg() }
	assert replay_error == expected
	events := app.drain_events()!
	assert events.len == 1
	assert events[0].kind == .window_destroyed
	assert events[0].window_id == window
	assert app.drain_events()!.len == 0
	app.stop()!
}

fn test_teardown_backend_stop_fault_is_aggregated_and_terminally_replayed() {
	mut app := new_app()!
	ticket := app.prepare_stop()!
	message := 'fault:teardown_backend_stop'
	expected := '${err_render_terminal_aggregate}: ${message}'
	app.set_internal_fault(.teardown_backend_stop, 0, message)!
	mut first_error := ''
	app.finish_stop(ticket, []) or { first_error = err.msg() }
	assert first_error == expected
	assert app.status() == .stopped

	mut replay_error := ''
	app.stop() or { replay_error = err.msg() }
	assert replay_error == expected
}

fn internal_fault_matrix_cases() []InternalFaultMatrixCase {
	return [
		InternalFaultMatrixCase{
			stage: .renderer_anchor_create
			label: 'renderer_anchor_create'
		},
		InternalFaultMatrixCase{
			stage: .renderer_environment
			label: 'renderer_environment'
		},
		InternalFaultMatrixCase{
			stage: .renderer_setup
			label: 'renderer_setup'
		},
		InternalFaultMatrixCase{
			stage: .renderer_batch_begin
			label: 'renderer_batch_begin'
		},
		InternalFaultMatrixCase{
			stage: .renderer_target_acquire
			label: 'renderer_target_acquire'
		},
		InternalFaultMatrixCase{
			stage: .renderer_pass_begin
			label: 'renderer_pass_begin'
		},
		InternalFaultMatrixCase{
			stage: .renderer_anchor_begin
			label: 'renderer_anchor_begin'
		},
		InternalFaultMatrixCase{
			stage: .renderer_precommit
			label: 'renderer_precommit'
		},
		InternalFaultMatrixCase{
			stage: .renderer_submission_finalize
			label: 'renderer_submission_finalize'
		},
		InternalFaultMatrixCase{
			stage: .teardown_prepare
			label: 'teardown_prepare'
		},
		InternalFaultMatrixCase{
			stage: .teardown_seal
			label: 'teardown_seal'
		},
		InternalFaultMatrixCase{
			stage: .teardown_backend_finish
			label: 'teardown_backend_finish'
		},
		InternalFaultMatrixCase{
			stage: .teardown_backend_stop
			label: 'teardown_backend_stop'
		},
	]
}
