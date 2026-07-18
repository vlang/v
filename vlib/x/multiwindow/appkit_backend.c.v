module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if darwin {
	#flag darwin -fobjc-arc
	#flag darwin -framework Cocoa
	#flag darwin -framework QuartzCore
	#flag darwin -framework Metal
	#insert "@VMODROOT/vlib/x/multiwindow/appkit_backend_helpers.h"
	$if sharedlive ? {
	} $else {
		#include "@VMODROOT/vlib/x/multiwindow/appkit_backend.m"
	}

	@[typedef]
	struct C.VMultiwindowAppKitQueuedEvent {
	mut:
		sequence           u64
		event_kind         int
		lifecycle_kind     int
		input_kind         int
		key_code           int
		char_code          u32
		key_repeat         int
		modifiers          u32
		mouse_button       int
		mouse_x            f32
		mouse_y            f32
		mouse_dx           f32
		mouse_dy           f32
		scroll_x           f32
		scroll_y           f32
		window_width       int
		window_height      int
		framebuffer_width  int
		framebuffer_height int
		dropped_file_count int
		dropped_files      &&char = unsafe { nil }
		touch_count        int
		touch_ids          [8]u64
		touch_x            [8]f32
		touch_y            [8]f32
		touch_changed      [8]int
	}

	fn C.v_multiwindow_appkit_is_main_thread() int
	fn C.v_multiwindow_appkit_prepare_application() int
	fn C.v_multiwindow_appkit_create_metal_device() C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_release_metal_device(device voidptr) C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_create_window(device voidptr, title &char, width int, height int, min_width int, min_height int, resizable int, visible int, high_dpi int, borderless int, fullscreen int, out_width &int, out_height &int, out_framebuffer_width &int, out_framebuffer_height &int) C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_configure_window_device(state voidptr, device voidptr) C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_destroy_window(state voidptr) C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_release_window(state voidptr) C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_set_window_title(state voidptr, title &char) int
	fn C.v_multiwindow_appkit_set_cursor_shape(state voidptr, shape int) int
	fn C.v_multiwindow_appkit_resize_window(state voidptr, width int, height int, out_width &int, out_height &int, out_framebuffer_width &int, out_framebuffer_height &int) int
	fn C.v_multiwindow_appkit_poll_events()
	fn C.v_multiwindow_appkit_event_sequence_exhausted() int
	fn C.v_multiwindow_appkit_take_queued_event(state voidptr, out_event &C.VMultiwindowAppKitQueuedEvent) int
	fn C.v_multiwindow_appkit_release_queued_event_resources(event &C.VMultiwindowAppKitQueuedEvent)
	fn C.v_multiwindow_appkit_begin_frame(state voidptr, device voidptr) C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_end_frame(state voidptr, drawable voidptr) C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_abort_frame(state voidptr, drawable voidptr) C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_release_drawable(state voidptr, drawable voidptr) C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_begin_render_batch() C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_end_render_batch(pool voidptr) C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_create_renderer_anchor(device voidptr) C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_destroy_renderer_anchor(state voidptr) C.VMultiwindowNativePrimitive
	fn C.v_multiwindow_appkit_render_snapshot(state voidptr, out_visible &int, out_miniaturized &int, out_occluded &int, out_width &int, out_height &int, out_framebuffer_width &int, out_framebuffer_height &int, out_scale &f32) int
	fn C.v_multiwindow_appkit_logical_to_pixel_rect(state voidptr, x f32, y f32, width f32, height f32, out_x &int, out_y &int, out_width &int, out_height &int) int
	fn C.v_multiwindow_appkit_pixel_to_logical_rect(state voidptr, x int, y int, width int, height int, out_x &f32, out_y &f32, out_width &f32, out_height &f32) int
}

struct AppKitNativeQueuedEvent {
	sequence u64
	event    QueuedEvent
}

struct AppKitCapturedRelease {
	capture NativePrimitiveCapture
	result  NativeRenderResult
}

struct AppKitPendingWindowState {
	id           WindowId
	state        voidptr
	state_ticket u64
}

struct AppKitPendingStartDevice {
mut:
	value       voidptr
	transaction NativeLifetimeAcquisitionTransaction
}

enum AppKitWindowDrawableReleaseMode {
	preserve_for_abort
	close_frame
}

@[heap; markused]
struct AppKitWindowRecord {
	id WindowId
mut:
	state                    voidptr
	state_ticket             u64
	width                    int
	height                   int
	framebuffer_width        int
	framebuffer_height       int
	native_destroyed         bool
	render_target_generation u64 = 1
	next_frame_lease         u64 = 1
	active_frame_lease       u64
	active_drawable          voidptr
	active_drawable_ticket   u64
	frame_active             bool
}

struct AppKitBackend {
mut:
	started                       bool
	device                        voidptr
	device_ticket                 u64
	anchor_state                  voidptr
	anchor_state_ticket           u64
	anchor_native_destroyed       bool
	batch_autorelease_pool        voidptr
	batch_autorelease_pool_ticket u64
	next_anchor_lease             u64 = 1
	active_anchor_lease           u64
	active_anchor_drawable        voidptr
	active_anchor_drawable_ticket u64
	render_sequence               u64
	poll_error                    string
	event_sequence_terminal       string
	render_health                 NativeRendererHealth
	native_operations             &NativeOperationAuthority = unsafe { nil }
	pending_start_device          AppKitPendingStartDevice
	pending_window_state          AppKitPendingWindowState
	windows                       []AppKitWindowRecord
}

fn new_appkit_backend() AppKitBackend {
	return AppKitBackend{}
}

fn appkit_metal_supported() bool {
	$if darwin_sokol_glcore33 ? {
		return false
	}
	return true
}

fn (backend &AppKitBackend) ensure_supported() ! {
	$if darwin {
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (backend &AppKitBackend) prevalidate_start_attempt() ! {
	$if darwin {
		if backend.native_operations == unsafe { nil }
			|| !backend.native_operations.owner_thread_is_current() {
			return error(err_owner_thread_required)
		}
		if C.v_multiwindow_appkit_is_main_thread() == 0 {
			return error(err_appkit_main_thread_required)
		}
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (backend &AppKitBackend) capabilities() Capabilities {
	return backend.capabilities_for_renderer(backend.renderer_ready())
}

fn (backend &AppKitBackend) capabilities_for_renderer(renderer_ready bool) Capabilities {
	return Capabilities{
		backend:            .appkit
		mock:               false
		native:             true
		multi_window:       true
		owner_queue:        true
		explicit_swapchain: appkit_metal_supported() && renderer_ready
		metal:              appkit_metal_supported() && renderer_ready
		input_events:       true
		mouse_events:       true
		keyboard_events:    true
		text_events:        true
		focus_events:       true
		drop_events:        true
		touch_events:       true
		cursor_shapes:      true
		native_decorations: true
	}
}

fn (mut backend AppKitBackend) start(require_renderer bool) ! {
	$if darwin {
		backend.prevalidate_start_attempt()!
		if backend.started {
			return
		}
		if backend.render_health.blocks_graphics() {
			return error(err_render_native_renderer_unavailable)
		}
		if require_renderer {
			if !appkit_metal_supported() {
				return error(err_renderer_unsupported)
			}
			backend.acquire_pending_start_device()!
			backend.configure_existing_windows_for_device(backend.pending_start_device.value) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				backend.release_pending_start_device()
				return err
			}
		}
		if C.v_multiwindow_appkit_prepare_application() == 0 {
			backend.release_pending_start_device()
			return error(err_appkit_application_failed)
		}
		if require_renderer {
			if !backend.commit_pending_start_device() {
				backend.release_pending_start_device()
				return error(err_appkit_metal_device_failed)
			}
		} else {
			backend.started = true
		}
		return
	} $else {
		_ = require_renderer
		return error(err_backend_unsupported)
	}
}

fn (mut backend AppKitBackend) probe_renderer_capabilities() !Capabilities {
	backend.prevalidate_start_attempt()!
	backend.acquire_pending_start_device() or {
		probe_error := err.msg()
		close_error := backend.close_start_attempt()
		return error(merge_backend_errors(probe_error, close_error))
	}
	caps := backend.capabilities_for_renderer(backend.provisional_renderer_ready())
	close_error := backend.close_start_attempt()
	if close_error != '' {
		return error(close_error)
	}
	return caps
}

fn (mut backend AppKitBackend) close_start_attempt() string {
	$if darwin {
		backend.release_pending_start_device()
		backend.started = false
		backend.render_health = .abandoned
		close_error := backend.take_poll_error()
		return close_error
	} $else {
		backend.pending_start_device = AppKitPendingStartDevice{}
		backend.device = unsafe { nil }
		backend.device_ticket = 0
		backend.started = false
		backend.render_health = .abandoned
		return ''
	}
}

fn (backend &AppKitBackend) start_attempt_closed() bool {
	mut native_operations_closed := backend.native_operations == unsafe { nil }
	if !native_operations_closed {
		native_operations_closed = !backend.native_operations.has_live_lifetime_tickets()
	}
	return !backend.started && backend.windows.len == 0
		&& appkit_pending_start_device_is_empty(backend.pending_start_device)
		&& appkit_lifetime_pair_is_empty(backend.device, backend.device_ticket)
		&& appkit_lifetime_pair_is_empty(backend.anchor_state, backend.anchor_state_ticket)
		&& appkit_lifetime_pair_is_empty(backend.batch_autorelease_pool, backend.batch_autorelease_pool_ticket)
		&& appkit_lifetime_pair_is_empty(backend.active_anchor_drawable, backend.active_anchor_drawable_ticket)
		&& appkit_lifetime_pair_is_empty(backend.pending_window_state.state, backend.pending_window_state.state_ticket)
		&& native_operations_closed
}

fn (backend &AppKitBackend) renderer_ready() bool {
	return appkit_metal_supported() && backend.device != unsafe { nil }
		&& backend.device_ticket != 0 && backend.render_health == .ready
}

fn (backend &AppKitBackend) provisional_renderer_ready() bool {
	return appkit_metal_supported() && backend.pending_start_device.value != unsafe { nil }
		&& backend.pending_start_device.transaction.ticket_id != 0
		&& backend.pending_start_device.transaction.native_identity == native_identity(backend.pending_start_device.value)
		&& !backend.render_health.blocks_graphics()
}

fn (mut backend AppKitBackend) init_renderer() ! {
	$if darwin {
		backend.prevalidate_start_attempt()!
		if backend.renderer_ready() {
			return
		}
		if !backend.started {
			return error(err_appkit_metal_device_failed)
		}
		backend.acquire_pending_start_device()!
		backend.configure_existing_windows_for_device(backend.pending_start_device.value) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			backend.release_pending_start_device()
			return err
		}
		if !backend.commit_pending_start_device() {
			backend.release_pending_start_device()
			return error(err_appkit_metal_device_failed)
		}
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (mut backend AppKitBackend) acquire_pending_start_device() ! {
	$if darwin {
		backend.prevalidate_start_attempt()!
		if !appkit_metal_supported() {
			return error(err_renderer_unsupported)
		}
		if !appkit_pending_start_device_is_empty(backend.pending_start_device)
			|| !appkit_lifetime_pair_is_empty(backend.device, backend.device_ticket)
			|| !appkit_lifetime_pair_is_empty(backend.pending_window_state.state, backend.pending_window_state.state_ticket)
			|| backend.render_health.blocks_graphics() {
			return error(err_appkit_metal_device_failed)
		}
		seed := NativeOperationSeed{
			call_site: .renderer_start
			scope:     .renderer
		}
		mut acquisition_ordinals := backend.native_operations.reserve_renderer_attempt_ordinals(1) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_appkit_metal_device_failed)
		}
		mut release_ordinals := backend.native_operations.reserve_app_lifetime_ordinals(1) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_appkit_metal_device_failed)
		}
		acquisition_context := acquisition_ordinals.materialize(backend.native_operations, .metal,
			.device_create, seed.without_target_identity()) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_appkit_metal_device_failed)
		}
		release_context := release_ordinals.materialize(backend.native_operations, .metal,
			.object_release, seed.without_target_identity()) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_appkit_metal_device_failed)
		}
		transaction := backend.native_operations.begin_lifetime_acquisition(acquisition_context,
			release_context, .metal_device, 0, seed) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_appkit_metal_device_failed)
		}
		backend.pending_start_device = AppKitPendingStartDevice{
			transaction: transaction
		}
		raw := C.v_multiwindow_appkit_create_metal_device()
		backend.pending_start_device.value = native_pointer(raw.handle)
		bound_transaction := backend.native_operations.bind_lifetime_acquisition_actual(transaction,
			native_identity(backend.pending_start_device.value)) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			backend.release_unbound_pending_start_device_after_bind_rejection()
			return error(err_appkit_metal_device_failed)
		}
		backend.pending_start_device.transaction = bound_transaction
		result := backend.accept_metal_result(acquisition_context, raw, .none,
			err_appkit_metal_device_failed)
		if !result.succeeded() || backend.render_health.blocks_graphics()
			|| !backend.provisional_renderer_ready() {
			backend.release_pending_start_device()
			return error(err_appkit_metal_device_failed)
		}
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (mut backend AppKitBackend) commit_pending_start_device() bool {
	transaction := backend.pending_start_device.transaction
	if backend.pending_start_device.value == unsafe { nil } || transaction.native_identity == 0
		|| transaction.native_identity != native_identity(backend.pending_start_device.value) {
		backend.append_release_diagnostic(err_appkit_metal_device_failed)
		return false
	}
	if !backend.native_operations.commit_lifetime_acquisition(transaction) {
		backend.append_release_diagnostic(err_appkit_metal_device_failed)
		return false
	}
	backend.device = backend.pending_start_device.value
	backend.device_ticket = transaction.ticket_id
	backend.pending_start_device = AppKitPendingStartDevice{}
	backend.render_health = .ready
	backend.started = true
	return true
}

fn (mut backend AppKitBackend) release_unbound_pending_start_device_after_bind_rejection() {
	$if darwin {
		transaction := backend.pending_start_device.transaction
		value := backend.pending_start_device.value
		backend.append_release_diagnostic(err_appkit_metal_device_failed)
		if value != unsafe { nil } {
			released_identity := native_identity(value)
			raw := C.v_multiwindow_appkit_release_metal_device(value)
			backend.pending_start_device.value = unsafe { nil }
			if appkit_lifetime_validation(raw, released_identity) != .void_completion {
				backend.append_release_diagnostic(err_appkit_metal_device_failed)
			}
		}
		abandoned :=
			backend.native_operations.abandon_physically_released_lifetime_acquisition(transaction)
		if !abandoned {
			backend.append_release_diagnostic(err_appkit_metal_device_failed)
			return
		}
		backend.pending_start_device = AppKitPendingStartDevice{}
	} $else {
		backend.pending_start_device = AppKitPendingStartDevice{}
	}
}

fn (mut backend AppKitBackend) release_pending_start_device() {
	$if darwin {
		transaction := backend.pending_start_device.transaction
		if transaction.ticket_id == 0 {
			if backend.pending_start_device.value == unsafe { nil } {
				backend.pending_start_device = AppKitPendingStartDevice{}
			} else {
				backend.append_release_diagnostic(err_appkit_metal_device_failed)
			}
			return
		}
		if backend.pending_start_device.value == unsafe { nil } {
			if backend.native_operations.cancel_lifetime_acquisition(transaction) {
				backend.pending_start_device = AppKitPendingStartDevice{}
			} else {
				backend.append_release_diagnostic(err_appkit_metal_device_failed)
			}
			return
		}
		if transaction.native_identity == 0 {
			backend.release_unbound_pending_start_device_after_bind_rejection()
			return
		}
		if transaction.native_identity != native_identity(backend.pending_start_device.value) {
			backend.append_release_diagnostic(err_appkit_metal_device_failed)
			return
		}
		claim := backend.native_operations.claim_provisional_lifetime_release(transaction) or {
			backend.append_release_diagnostic(err_appkit_metal_device_failed)
			return
		}
		released_identity := native_identity(backend.pending_start_device.value)
		raw := C.v_multiwindow_appkit_release_metal_device(backend.pending_start_device.value)
		backend.pending_start_device.value = unsafe { nil }
		if !backend.native_operations.mark_provisional_lifetime_native_released(claim) {
			backend.append_release_diagnostic(err_appkit_metal_device_failed)
			return
		}
		capture := backend.native_operations.capture_call(claim.context, raw)
		validation := appkit_lifetime_validation(raw, released_identity)
		mut result := backend.native_operations.accept_metal(claim.context, capture, validation,
			err_appkit_metal_device_failed)
		evidence_recorded := backend.native_operations.record_provisional_lifetime_release_evidence(claim,
			capture, result)
		result = backend.record_metal_result(result)
		if !evidence_recorded {
			backend.append_release_diagnostic(err_appkit_metal_device_failed)
			return
		}
		if !result.succeeded() {
			backend.append_release_diagnostic(if result.error_text != '' {
				result.error_text
			} else {
				err_appkit_metal_device_failed
			})
		}
		if !backend.native_operations.retire_provisional_lifetime_release(claim) {
			backend.append_release_diagnostic(err_appkit_metal_device_failed)
			return
		}
		backend.pending_start_device = AppKitPendingStartDevice{}
	} $else {
		transaction := backend.pending_start_device.transaction
		if transaction.ticket_id != 0 {
			backend.native_operations.cancel_lifetime_acquisition(transaction)
		}
		backend.pending_start_device = AppKitPendingStartDevice{}
	}
}

fn (mut backend AppKitBackend) configure_existing_windows_for_device(device voidptr) ! {
	$if darwin {
		if device == unsafe { nil } || backend.render_health.blocks_graphics() {
			return error(err_appkit_metal_device_failed)
		}
		for record in backend.windows {
			if record.state == unsafe { nil } || record.state_ticket == 0 {
				return error(err_appkit_metal_device_failed)
			}
			seed := NativeOperationSeed{
				presence_mask:     native_context_has_window | native_context_has_target_generation | native_context_has_target_identity
				call_site:         .renderer_start
				scope:             .window_target
				window:            record.id
				target_generation: record.render_target_generation
				target_identity:   native_identity(record.state)
			}
			mut ordinals := backend.native_operations.reserve_renderer_attempt_ordinals(1) or {
				return error(err_render_native_renderer_unavailable)
			}
			context := ordinals.materialize(backend.native_operations, .metal, .window_configure, seed) or {
				return error(err_render_native_renderer_unavailable)
			}
			raw := C.v_multiwindow_appkit_configure_window_device(record.state, device)
			result := backend.accept_metal_identity_result(context, raw,
				native_identity(record.state), native_identity(device),
				err_appkit_metal_device_failed)
			if !result.succeeded() || backend.render_health.blocks_graphics() {
				return error(err_appkit_metal_device_failed)
			}
		}
		return
	} $else {
		_ = device
		return error(err_backend_unsupported)
	}
}

fn (mut backend AppKitBackend) create_window(id WindowId, config WindowConfig) !WindowSize {
	$if darwin {
		if !backend.started || backend.render_health.blocks_graphics() {
			return error(err_appkit_application_failed)
		}
		if backend.pending_window_state.state != unsafe { nil }
			|| backend.pending_window_state.state_ticket != 0 {
			return error(err_appkit_create_window_failed)
		}
		seed := NativeOperationSeed{
			presence_mask:     native_context_has_window | native_context_has_target_generation
			call_site:         .window_prepare
			scope:             .window_target
			window:            id
			target_generation: 1
		}
		mut operation := backend.native_operations.reserve_renderer_attempt_ordinals(1) or {
			return error(err_render_native_renderer_unavailable)
		}
		mut cleanup := backend.native_operations.reserve_app_lifetime_ordinals(1) or {
			return error(err_render_native_renderer_unavailable)
		}
		mut state_ticket := backend.reserve_appkit_lifetime_ticket(mut cleanup, .appkit_state, seed) or {
			return error(err_render_native_renderer_unavailable)
		}
		mut width := 0
		mut height := 0
		mut framebuffer_width := 0
		mut framebuffer_height := 0
		context := operation.materialize(backend.native_operations, .metal, .window_surface_create, seed) or {
			if !backend.native_operations.burn_lifetime_ticket(state_ticket) {
				backend.pending_window_state = AppKitPendingWindowState{
					id:           id
					state:        unsafe { nil }
					state_ticket: state_ticket
				}
				_ = backend.release_pending_window_state_lifetime()
			}
			return error(err_render_native_renderer_unavailable)
		}
		raw := C.v_multiwindow_appkit_create_window(backend.device, &char(config.title.str),
			config.width, config.height, config.min_width, config.min_height,
			appkit_bool_to_int(config.resizable), appkit_bool_to_int(config.visible),
			appkit_bool_to_int(config.high_dpi), appkit_bool_to_int(config.borderless),
			appkit_bool_to_int(config.fullscreen), &width, &height, &framebuffer_width,
			&framebuffer_height)
		state := native_pointer(raw.handle)
		if state == unsafe { nil } {
			if backend.native_operations.burn_lifetime_ticket(state_ticket) {
				state_ticket = 0
			}
		} else {
			backend.native_operations.bind_lifetime_ticket(state_ticket, native_identity(state), 0)
		}
		backend.pending_window_state = AppKitPendingWindowState{
			id:           id
			state:        state
			state_ticket: state_ticket
		}
		created := backend.accept_metal_result(context, raw, .none, err_appkit_create_window_failed)
		if !created.succeeded() {
			_ = backend.release_pending_window_state_lifetime()
			return error(err_appkit_create_window_failed)
		}
		pending := backend.pending_window_state
		if pending.id != id {
			return error(err_appkit_create_window_failed)
		}
		backend.windows << AppKitWindowRecord{
			id:                 id
			state:              pending.state
			state_ticket:       pending.state_ticket
			width:              width
			height:             height
			framebuffer_width:  framebuffer_width
			framebuffer_height: framebuffer_height
		}
		backend.pending_window_state = AppKitPendingWindowState{}
		return WindowSize{
			width:  width
			height: height
		}
	} $else {
		_ = id
		_ = config
		return error(err_backend_unsupported)
	}
}

fn (mut backend AppKitBackend) destroy_window(id WindowId) ! {
	backend.finish_window_teardown(id)!
}

fn (mut backend AppKitBackend) finish_window_teardown(id WindowId) ! {
	$if darwin {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if backend.render_health.blocks_graphics() {
			if !backend.release_active_frames_lifetime() || backend.has_active_frames() {
				return error(err_appkit_metal_drawable_failed)
			}
			if !backend.release_batch_pool_lifetime(NativeOperationSeed{
				call_site: .shutdown_release
				scope:     .batch
			}) {
				return error(err_appkit_application_failed)
			}
			if backend.batch_autorelease_pool != unsafe { nil }
				|| backend.batch_autorelease_pool_ticket != 0 {
				return error(err_appkit_application_failed)
			}
		}
		mut record := &backend.windows[index]
		released := backend.release_window_resources(mut record, NativeOperationSeed{
			presence_mask:     native_context_has_window | native_context_has_target_generation
			call_site:         .shutdown_release
			scope:             .window_target
			window:            record.id
			target_generation: record.render_target_generation
		})
		if !released {
			return error(err_appkit_destroy_window_failed)
		}
		backend.windows.delete(index)
		return
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend AppKitBackend) set_window_title(id WindowId, title string) ! {
	$if darwin {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if backend.render_health.blocks_graphics() {
			return error(err_render_native_renderer_unavailable)
		}
		if C.v_multiwindow_appkit_set_window_title(backend.windows[index].state, &char(title.str)) == 0 {
			return error(err_appkit_set_window_title_failed)
		}
		return
	} $else {
		_ = id
		_ = title
		return error(err_backend_unsupported)
	}
}

fn (mut backend AppKitBackend) resize_window(id WindowId, width int, height int) !WindowSize {
	$if darwin {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if backend.render_health.blocks_graphics() {
			return error(err_render_native_renderer_unavailable)
		}
		mut actual_width := 0
		mut actual_height := 0
		mut framebuffer_width := 0
		mut framebuffer_height := 0
		if C.v_multiwindow_appkit_resize_window(backend.windows[index].state, width, height,
			&actual_width, &actual_height, &framebuffer_width, &framebuffer_height) == 0 {
			return error(err_appkit_resize_window_failed)
		}
		backend.windows[index].width = actual_width
		backend.windows[index].height = actual_height
		backend.windows[index].framebuffer_width = framebuffer_width
		backend.windows[index].framebuffer_height = framebuffer_height
		backend.windows[index].render_target_generation =
			next_backend_target_generation(backend.windows[index].render_target_generation)!
		return WindowSize{
			width:  actual_width
			height: actual_height
		}
	} $else {
		_ = id
		_ = width
		_ = height
		return error(err_backend_unsupported)
	}
}

$if darwin {
	@[markused]
	fn appkit_input_kind(kind int) InputEventKind {
		return match kind {
			1 { InputEventKind.key_down }
			2 { InputEventKind.key_up }
			3 { InputEventKind.char }
			4 { InputEventKind.mouse_down }
			5 { InputEventKind.mouse_up }
			6 { InputEventKind.mouse_scroll }
			7 { InputEventKind.mouse_move }
			8 { InputEventKind.mouse_enter }
			9 { InputEventKind.mouse_leave }
			10 { InputEventKind.focused }
			11 { InputEventKind.unfocused }
			12 { InputEventKind.resized }
			13 { InputEventKind.iconified }
			14 { InputEventKind.restored }
			15 { InputEventKind.clipboard_pasted }
			16 { InputEventKind.files_dropped }
			17 { InputEventKind.touches_began }
			18 { InputEventKind.touches_moved }
			19 { InputEventKind.touches_ended }
			20 { InputEventKind.touches_cancelled }
			else { InputEventKind.invalid }
		}
	}

	@[markused]
	fn appkit_dropped_files_from_native(native_event C.VMultiwindowAppKitQueuedEvent) []string {
		if native_event.dropped_file_count <= 0 || native_event.dropped_files == unsafe { nil } {
			return []string{}
		}
		mut files := []string{cap: native_event.dropped_file_count}
		for i in 0 .. native_event.dropped_file_count {
			path := unsafe { native_event.dropped_files[i] }
			if path != unsafe { nil } {
				files << unsafe { tos_clone(&u8(path)) }
			}
		}
		return files
	}

	@[markused]
	fn appkit_queued_event_from_native(record AppKitWindowRecord, native_event C.VMultiwindowAppKitQueuedEvent) ?QueuedEvent {
		if native_event.event_kind == 1 {
			kind := match native_event.lifecycle_kind {
				1 { EventKind.window_close_requested }
				2 { EventKind.window_destroyed }
				3 { EventKind.window_resized }
				else { return none }
			}

			mut event := Event{
				kind:      kind
				window_id: record.id
			}
			if kind == .window_resized {
				event = Event{
					kind:      kind
					window_id: record.id
					width:     native_event.window_width
					height:    native_event.window_height
				}
			}
			return queued_lifecycle_event(event)
		}
		if native_event.event_kind == 2 {
			input_kind := appkit_input_kind(native_event.input_kind)
			if input_kind == .invalid {
				return none
			}
			mut touch_count := native_event.touch_count
			if touch_count < 0 {
				touch_count = 0
			}
			if touch_count > 8 {
				touch_count = 8
			}
			mut touches := [8]InputTouchPoint{}
			for i in 0 .. touch_count {
				touches[i] = InputTouchPoint{
					identifier: native_event.touch_ids[i]
					pos_x:      native_event.touch_x[i]
					pos_y:      native_event.touch_y[i]
					changed:    native_event.touch_changed[i] != 0
				}
			}
			return queued_input_event(InputEvent{
				kind:               input_kind
				window_id:          record.id
				key_code:           native_event.key_code
				char_code:          native_event.char_code
				key_repeat:         native_event.key_repeat != 0
				modifiers:          native_event.modifiers
				mouse_button:       native_event.mouse_button
				mouse_x:            native_event.mouse_x
				mouse_y:            native_event.mouse_y
				mouse_dx:           native_event.mouse_dx
				mouse_dy:           native_event.mouse_dy
				scroll_x:           native_event.scroll_x
				scroll_y:           native_event.scroll_y
				num_touches:        touch_count
				touches:            touches
				window_width:       native_event.window_width
				window_height:      native_event.window_height
				framebuffer_width:  native_event.framebuffer_width
				framebuffer_height: native_event.framebuffer_height
				dropped_files:      appkit_dropped_files_from_native(native_event)
			})
		}
		return none
	}
}

fn (mut backend AppKitBackend) set_window_cursor(id WindowId, shape CursorShape) ! {
	$if darwin {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if C.v_multiwindow_appkit_set_cursor_shape(backend.windows[index].state, int(shape)) == 0 {
			return error(err_capability_unsupported)
		}
		return
	} $else {
		_ = id
		_ = shape
		return error(err_backend_unsupported)
	}
}

fn (mut backend AppKitBackend) poll_queued_events() ![]QueuedEvent {
	mut native_events := []AppKitNativeQueuedEvent{}
	$if darwin {
		if !backend.started || backend.render_health.blocks_graphics() {
			return []QueuedEvent{}
		}
		C.v_multiwindow_appkit_poll_events()
		mut i := 0
		for i < backend.windows.len {
			record := backend.windows[i]
			for {
				mut native_event := C.VMultiwindowAppKitQueuedEvent{}
				if C.v_multiwindow_appkit_take_queued_event(record.state, &native_event) == 0 {
					break
				}
				if native_event.event_kind == 1 && native_event.lifecycle_kind == 2 {
					backend.windows[i].native_destroyed = true
				}
				if native_event.window_width > 0 && native_event.window_height > 0
					&& ((native_event.event_kind == 1 && native_event.lifecycle_kind == 3)
					|| (native_event.event_kind == 2 && native_event.input_kind == 12)) {
					if backend.windows[i].width != native_event.window_width
						|| backend.windows[i].height != native_event.window_height
						|| backend.windows[i].framebuffer_width != native_event.framebuffer_width
						|| backend.windows[i].framebuffer_height != native_event.framebuffer_height {
						backend.windows[i].render_target_generation =
							exhaust_backend_target_generation(backend.windows[i].render_target_generation)
					}
					backend.windows[i].width = native_event.window_width
					backend.windows[i].height = native_event.window_height
					backend.windows[i].framebuffer_width = native_event.framebuffer_width
					backend.windows[i].framebuffer_height = native_event.framebuffer_height
				}
				event := appkit_queued_event_from_native(record, native_event) or {
					C.v_multiwindow_appkit_release_queued_event_resources(&native_event)
					continue
				}
				C.v_multiwindow_appkit_release_queued_event_resources(&native_event)
				native_events << AppKitNativeQueuedEvent{
					sequence: native_event.sequence
					event:    event
				}
			}
			i++
		}
	}
	appkit_sort_native_events(mut native_events)
	mut events := []QueuedEvent{cap: native_events.len}
	for native_event in native_events {
		events << native_event.event
	}
	return events
}

fn (mut backend AppKitBackend) take_poll_error() string {
	error_message := backend.poll_error
	backend.poll_error = ''
	return error_message
}

fn (mut backend AppKitBackend) event_sequence_terminal_error() string {
	$if darwin {
		if C.v_multiwindow_appkit_event_sequence_exhausted() != 0
			&& backend.event_sequence_terminal == '' {
			backend.event_sequence_terminal = err_backend_event_sequence_exhausted
		}
	}
	return backend.event_sequence_terminal
}

fn appkit_sort_native_events(mut events []AppKitNativeQueuedEvent) {
	for i in 1 .. events.len {
		mut j := i
		for j > 0 && events[j - 1].sequence > events[j].sequence {
			event := events[j]
			events[j] = events[j - 1]
			events[j - 1] = event
			j--
		}
	}
}

fn append_appkit_stop_error(mut errors []string, message string) {
	if message != '' && message !in errors {
		errors << message
	}
}

fn (mut backend AppKitBackend) stop() ! {
	$if darwin {
		mut errors := []string{}
		had_active_frames := backend.has_active_frames()
		if had_active_frames {
			append_appkit_stop_error(mut errors, err_render_target_stale)
		}
		if !backend.release_active_frames_lifetime() || backend.has_active_frames() {
			append_appkit_stop_error(mut errors, err_appkit_metal_drawable_failed)
		}
		if !backend.release_batch_pool_lifetime(NativeOperationSeed{
			call_site: .shutdown_release
			scope:     .batch
		}) {
			append_appkit_stop_error(mut errors, err_appkit_application_failed)
		}
		if backend.has_active_frames() || backend.batch_autorelease_pool != unsafe { nil }
			|| backend.batch_autorelease_pool_ticket != 0 {
			append_appkit_stop_error(mut errors, err_appkit_application_failed)
		}
		mut window_index := 0
		for window_index < backend.windows.len {
			mut record := &backend.windows[window_index]
			released := backend.release_window_resources(mut record, NativeOperationSeed{
				presence_mask:     native_context_has_window | native_context_has_target_generation
				call_site:         .shutdown_release
				scope:             .window_target
				window:            record.id
				target_generation: record.render_target_generation
			})
			if !released {
				append_appkit_stop_error(mut errors, err_appkit_destroy_window_failed)
				window_index++
				continue
			}
			backend.windows.delete(window_index)
		}
		if !backend.release_pending_window_state_lifetime() {
			append_appkit_stop_error(mut errors, err_appkit_destroy_window_failed)
		}
		if !backend.release_anchor_state_lifetime(NativeOperationSeed{
			call_site: .shutdown_release
			scope:     .anchor
		}) {
			append_appkit_stop_error(mut errors, err_render_anchor_failed)
		}
		backend.release_pending_start_device()
		if !appkit_pending_start_device_is_empty(backend.pending_start_device) {
			append_appkit_stop_error(mut errors, err_appkit_metal_device_failed)
		}
		if !backend.release_device_lifetime() {
			append_appkit_stop_error(mut errors, err_appkit_metal_device_failed)
		}
		retains_native_ownership := backend.retains_native_ownership()
		if retains_native_ownership {
			append_appkit_stop_error(mut errors, err_appkit_metal_device_failed)
		} else {
			backend.render_health = .abandoned
			backend.started = false
		}
		poll_error := backend.take_poll_error()
		append_appkit_stop_error(mut errors, poll_error)
		if errors.len > 0 {
			return error(errors.join('; '))
		}
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn (mut backend AppKitBackend) begin_render_batch(boundary_seed NativeOperationSeed) ! {
		$if darwin {
			if backend.batch_autorelease_pool != unsafe { nil }
				|| backend.batch_autorelease_pool_ticket != 0 {
				return error(err_render_batch_active)
			}
			if backend.render_health.blocks_graphics() {
				return error(err_render_native_renderer_unavailable)
			}
			if backend.native_operations == unsafe { nil } {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_appkit_application_failed)
			}
			seed := NativeOperationSeed{
				...boundary_seed
				scope: .batch
			}
			mut ordinals := backend.native_operations.reserve_ordinals(2) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_appkit_application_failed)
			}
			mut cleanup := ordinals.split_tail(1) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_appkit_application_failed)
			}
			mut pool_ticket := backend.reserve_appkit_lifetime_ticket(mut cleanup,
				.appkit_autorelease_pool, seed) or { return error(err_appkit_application_failed) }
			context := ordinals.materialize(backend.native_operations, .metal, .render_batch_begin,
				seed.without_target_identity()) or {
				if !backend.native_operations.burn_lifetime_ticket(pool_ticket) {
					backend.batch_autorelease_pool = unsafe { nil }
					backend.batch_autorelease_pool_ticket = pool_ticket
					_ = backend.release_batch_pool_lifetime(seed)
				}
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(err_appkit_application_failed)
			}
			raw := C.v_multiwindow_appkit_begin_render_batch()
			actual_pool := native_pointer(raw.handle)
			if actual_pool == unsafe { nil } {
				if backend.native_operations.burn_lifetime_ticket(pool_ticket) {
					pool_ticket = 0
				}
			} else {
				backend.native_operations.bind_lifetime_ticket(pool_ticket,
					native_identity(actual_pool), 0)
			}
			backend.batch_autorelease_pool = actual_pool
			backend.batch_autorelease_pool_ticket = pool_ticket
			result := backend.accept_metal_result(context, raw, .none,
				err_appkit_application_failed)
			if !result.succeeded() {
				_ = backend.release_batch_pool_lifetime(seed)
				return error(err_appkit_application_failed)
			}
		} $else {
			_ = boundary_seed
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend AppKitBackend) end_render_batch(seed NativeOperationSeed) ! {
		$if darwin {
			_ = seed
			blocked := backend.render_health.blocks_graphics()
			if backend.has_active_frames() && !blocked {
				return error(err_render_frame_active)
			}
			if blocked {
				if !backend.release_active_frames_lifetime() || backend.has_active_frames() {
					return error(err_appkit_metal_drawable_failed)
				}
			}
			if appkit_lifetime_pair_is_empty(backend.batch_autorelease_pool,
				backend.batch_autorelease_pool_ticket)
			{
				return error(err_appkit_application_failed)
			}
			if !backend.release_batch_pool_lifetime(seed) || blocked {
				return error(err_appkit_application_failed)
			}
		} $else {
			_ = seed
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend AppKitBackend) render_environment(id WindowId) !gfx.Environment {
		$if darwin {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			if !appkit_metal_supported() {
				return error(err_renderer_unsupported)
			}
			if !backend.renderer_ready() || backend.windows[index].state == unsafe { nil }
				|| backend.windows[index].state_ticket == 0 {
				return error(err_renderer_unsupported)
			}
			return gfx.Environment{
				defaults: gfx.EnvironmentDefaults{
					color_format: .bgra8
					depth_format: .depth_stencil
					sample_count: 1
				}
				metal:    gfx.MetalEnvironment{
					device: backend.device
				}
			}
		} $else {
			_ = id
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend AppKitBackend) begin_render(id WindowId, candidate RenderWindowSnapshot, native_attempt NativeTargetAttempt) BackendFrameAttempt {
		$if darwin {
			index := backend.window_record_index(id) or {
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.window_surface_create, .window_target, .operation_failed, 0, 0,
						err_window_not_found))
				}
			}
			if !appkit_metal_supported() {
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.window_surface_create, .renderer, .renderer_unavailable, 0, 0,
						err_renderer_unsupported))
				}
			}
			if !backend.renderer_ready() {
				disposition := if backend.render_health == .lost {
					NativeRenderDisposition.renderer_lost
				} else {
					NativeRenderDisposition.renderer_unavailable
				}
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.window_surface_create, .renderer, disposition, 0, 0,
						err_renderer_unsupported))
				}
			}
			if backend.batch_autorelease_pool == unsafe { nil }
				|| backend.batch_autorelease_pool_ticket == 0 {
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.window_surface_create, .batch, .operation_failed, 0, 0,
						err_render_batch_inactive))
				}
			}
			mut record := &backend.windows[index]
			if record.state == unsafe { nil } || record.state_ticket == 0 || record.frame_active
				|| record.active_drawable != unsafe { nil } || record.active_drawable_ticket != 0 {
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.window_surface_create, .batch, .operation_failed, 0, 0,
						err_render_frame_active))
				}
			}
			if candidate.window != id || candidate.target.target_identity == 0
				|| candidate.target.target_identity != record.render_target_generation
				|| candidate.metrics.framebuffer_width != record.framebuffer_width
				|| candidate.metrics.framebuffer_height != record.framebuffer_height
				|| candidate.target.color_format != int(gfx.PixelFormat.bgra8)
				|| candidate.target.depth_format != int(gfx.PixelFormat.depth_stencil)
				|| candidate.target.sample_count != 1 {
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.window_surface_create, .window_target, .target_lost, 0, 0,
						err_render_target_stale))
				}
			}
			lease, next_frame_lease := plan_nonwrapping_counter(record.next_frame_lease) or {
				return BackendFrameAttempt{
					outcome: backend.record_metal_result(native_render_outcome(.metal,
						.window_surface_create, .batch, .operation_failed, 0, 0,
						err_render_renderer_failed))
				}
			}
			record.next_frame_lease = next_frame_lease
			record.active_frame_lease = lease
			record.active_drawable = unsafe { nil }
			record.active_drawable_ticket = 0
			record.frame_active = true
			return BackendFrameAttempt{
				frame:   RenderFrame{
					window_id:          id
					backend_lease:      lease
					batch_epoch:        native_attempt.batch_epoch
					window_lease_epoch: native_attempt.window_lease_epoch
					target_lease_epoch: native_attempt.target_lease_epoch
					metrics:            candidate.metrics
					target:             candidate.target
					swapchain:          gfx.Swapchain{
						width:        candidate.metrics.framebuffer_width
						height:       candidate.metrics.framebuffer_height
						sample_count: 1
						color_format: .bgra8
						depth_format: .depth_stencil
					}
				}
				outcome: backend.record_metal_result(native_render_ok(.metal,
					.window_surface_create, .window_target))
			}
		} $else {
			_ = id
			_ = candidate
			_ = native_attempt
			return BackendFrameAttempt{
				outcome: native_render_outcome(.metal, .window_surface_create, .renderer,
					.renderer_unavailable, 0, 0, err_backend_unsupported)
			}
		}
	}

	fn (mut backend AppKitBackend) end_render(frame RenderFrame) BackendFinalizeAttempt {
		$if darwin {
			if !appkit_metal_supported() || backend.render_health.blocks_graphics() {
				_ = backend.release_active_frames_lifetime()
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.record_metal_result(native_render_outcome(.metal, .present,
						.renderer, .renderer_unavailable, 0, 0, err_renderer_unsupported))
				}
			}
			index := backend.window_record_index(frame.window_id) or {
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.record_metal_result(native_render_outcome(.metal, .present,
						.window_target, .operation_failed, 0, 0, err_window_not_found))
				}
			}
			mut record := &backend.windows[index]
			drawable := frame.swapchain.metal.current_drawable
			if !record.frame_active || frame.backend_lease == 0
				|| record.active_frame_lease != frame.backend_lease || !frame.acquired
				|| drawable == unsafe { nil } || record.active_drawable != drawable {
				_ = backend.release_active_frames_lifetime()
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.record_metal_result(native_render_outcome(.metal, .present,
						.window_target, .target_lost, 0, 0, err_render_target_stale))
				}
			}
			seed :=
				native_seed_for_frame(frame, .window_finalize).with_target_identity(native_identity(drawable))
			mut ordinals := backend.native_operations.reserve_ordinals(1) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.record_metal_result(native_render_outcome(.metal, .present,
						.renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
			}
			context := ordinals.materialize(backend.native_operations, .metal, .present, seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.record_metal_result(native_render_outcome(.metal, .present,
						.renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
			}
			claim := backend.native_operations.claim_lifetime_release(record.active_drawable_ticket,
				.metal_drawable, native_identity(drawable), native_identity(record.state)) or {
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.record_metal_result(native_render_outcome(.metal, .present,
						.window_target, .target_lost, 0, 0, err_render_target_stale))
				}
			}
			raw := C.v_multiwindow_appkit_end_frame(record.state, drawable)
			released_drawable := record.active_drawable
			released_ticket := record.active_drawable_ticket
			record.active_drawable = unsafe { nil }
			record.frame_active = false
			record.active_frame_lease = 0
			released, result := backend.complete_appkit_drawable_release(claim, context, raw,
				released_drawable, released_ticket, err_render_target_stale)
			record.active_drawable = released.value
			record.active_drawable_ticket = released.ticket_id
			return BackendFinalizeAttempt{
				status:  if result.succeeded() && released.ticket_retired {
					.submitted
				} else {
					.not_presented
				}
				outcome: result
			}
		} $else {
			_ = frame
			return BackendFinalizeAttempt{
				status:  .not_presented
				outcome: native_render_outcome(.metal, .present, .renderer, .renderer_unavailable,
					0, 0, err_backend_unsupported)
			}
		}
	}

	fn (mut backend AppKitBackend) abort_render(frame RenderFrame) ! {
		$if darwin {
			if !appkit_metal_supported() || backend.render_health.blocks_graphics() {
				_ = backend.release_active_frames_lifetime()
				return error(err_renderer_unsupported)
			}
			index := backend.window_record_index(frame.window_id) or {
				return error(err_window_not_found)
			}
			mut record := &backend.windows[index]
			drawable := frame.swapchain.metal.current_drawable
			if !record.frame_active || frame.backend_lease == 0
				|| record.active_frame_lease != frame.backend_lease {
				return error(err_render_target_stale)
			}
			if record.active_drawable != unsafe { nil } && record.active_drawable != drawable {
				_ = backend.release_active_frames_lifetime()
				return error(err_render_target_stale)
			}
			if record.active_drawable != unsafe { nil } {
				active_drawable := record.active_drawable
				seed :=
					native_seed_for_frame(frame, .window_finalize).with_target_identity(native_identity(active_drawable))
				mut ordinals := backend.native_operations.reserve_ordinals(1) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				context := ordinals.materialize(backend.native_operations, .metal, .clear_state, seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				claim := backend.native_operations.claim_lifetime_release(record.active_drawable_ticket,
					.metal_drawable, native_identity(active_drawable),
					native_identity(record.state)) or { return error(err_render_target_stale) }
				raw := C.v_multiwindow_appkit_abort_frame(record.state, active_drawable)
				released_ticket := record.active_drawable_ticket
				record.active_drawable = unsafe { nil }
				record.frame_active = false
				record.active_frame_lease = 0
				released, result := backend.complete_appkit_drawable_release(claim, context, raw,
					active_drawable, released_ticket, err_render_target_stale)
				record.active_drawable = released.value
				record.active_drawable_ticket = released.ticket_id
				if !result.succeeded() || !released.ticket_retired {
					return native_render_error(result)
				}
				return
			}
			if record.active_drawable_ticket != 0 {
				return error(err_render_target_stale)
			}
			record.frame_active = false
			record.active_frame_lease = 0
			return
		} $else {
			_ = frame
			return error(err_backend_unsupported)
		}
	}
}

fn (backend &AppKitBackend) window_record_index(id WindowId) ?int {
	for i, record in backend.windows {
		if record.id == id {
			return i
		}
	}
	return none
}

fn (mut backend AppKitBackend) reserve_appkit_lifetime_ticket(mut cleanup NativeOrdinalRange, release_kind NativeLifetimeReleaseKind, seed NativeOperationSeed) !u64 {
	if release_kind !in [.metal_device, .appkit_state, .appkit_autorelease_pool, .metal_drawable] {
		return error(err_render_native_renderer_unavailable)
	}
	context := cleanup.materialize(backend.native_operations, .metal,
		native_lifetime_release_operation(release_kind), seed.without_target_identity())!
	return backend.native_operations.reserve_lifetime_ticket(context, release_kind,
		seed.without_target_identity())
}

fn appkit_lifetime_validation(raw C.VMultiwindowNativePrimitive, identity u64) NativeLocalValidation {
	if raw.valid_mask & native_valid_object_identity_0 == 0 || raw.object_identity_0 != identity {
		return .binding_mismatch
	}
	return .void_completion
}

fn (mut backend AppKitBackend) append_release_diagnostic(message string) {
	if message != '' {
		backend.poll_error = merge_backend_errors(backend.poll_error, message)
	}
}

fn (mut backend AppKitBackend) release_appkit_lifetime_identity(value voidptr, ticket_id u64, release_kind NativeLifetimeReleaseKind, parent_identity u64, error_text string) NativeLifetimeReleaseResult {
	pending := NativeLifetimeReleaseResult{
		value:     value
		ticket_id: ticket_id
	}
	$if darwin {
		if backend.native_operations == unsafe { nil }
			|| !backend.native_operations.owner_thread_is_current() {
			return pending
		}
		if release_kind !in [.metal_device, .appkit_state, .appkit_autorelease_pool, .metal_drawable] {
			return pending
		}
		if value == unsafe { nil } {
			if ticket_id == 0 {
				return NativeLifetimeReleaseResult{
					native_released: true
					ticket_retired:  true
				}
			}
			retirement_claim := backend.native_operations.claim_native_released_lifetime_retirement(ticket_id,
				release_kind) or {
				if backend.native_operations.burn_lifetime_ticket(ticket_id) {
					return NativeLifetimeReleaseResult{
						native_released: true
						ticket_retired:  true
					}
				}
				return pending
			}
			backend.native_operations.retire_native_released_lifetime_claim(retirement_claim)
			return NativeLifetimeReleaseResult{
				native_released: true
				ticket_retired:  true
			}
		}
		if C.v_multiwindow_appkit_is_main_thread() == 0 {
			return pending
		}
		identity := native_identity(value)
		claim := backend.native_operations.claim_lifetime_release(ticket_id, release_kind,
			identity, parent_identity) or { return pending }
		raw := match release_kind {
			.metal_device {
				C.v_multiwindow_appkit_release_metal_device(value)
			}
			.appkit_state {
				C.v_multiwindow_appkit_release_window(value)
			}
			.appkit_autorelease_pool {
				C.v_multiwindow_appkit_end_render_batch(value)
			}
			.metal_drawable {
				C.v_multiwindow_appkit_release_drawable(native_pointer(parent_identity), value)
			}
			else {
				C.VMultiwindowNativePrimitive{}
			}
		}

		backend.native_operations.mark_claimed_lifetime_native_released(claim)
		accepted := backend.accept_metal_clear_result(claim.context, raw, identity, error_text)
		backend.native_operations.record_native_released_lifetime_evidence(claim, accepted.capture,
			accepted.result)
		if !accepted.result.succeeded() {
			backend.append_release_diagnostic(if accepted.result.error_text != '' {
				accepted.result.error_text
			} else {
				error_text
			})
		}
		backend.native_operations.retire_native_released_lifetime_claim(claim)
		return NativeLifetimeReleaseResult{
			native_released: true
			ticket_retired:  true
		}
	} $else {
		_ = backend
		_ = release_kind
		_ = parent_identity
		_ = error_text
		return pending
	}
}

fn (mut backend AppKitBackend) complete_appkit_drawable_release(claim NativeLifetimeReleaseClaim, context NativeOperationContext, raw C.VMultiwindowNativePrimitive, drawable voidptr, ticket_id u64, error_text string) (NativeLifetimeReleaseResult, NativeRenderResult) {
	released_after_native := NativeLifetimeReleaseResult{
		ticket_id:       ticket_id
		native_released: true
	}
	backend.native_operations.mark_claimed_lifetime_native_released(claim)
	accepted := backend.accept_metal_clear_result(context, raw, native_identity(drawable),
		error_text)
	backend.native_operations.record_native_released_lifetime_evidence(claim, accepted.capture,
		accepted.result)
	if !accepted.result.succeeded() {
		backend.append_release_diagnostic(if accepted.result.error_text != '' {
			accepted.result.error_text
		} else {
			error_text
		})
	}
	backend.native_operations.retire_native_released_lifetime_claim(claim)
	return NativeLifetimeReleaseResult{
		value:           released_after_native.value
		ticket_id:       0
		native_released: released_after_native.native_released
		ticket_retired:  true
	}, accepted.result
}

fn appkit_lifetime_pair_is_empty(value voidptr, ticket_id u64) bool {
	return value == unsafe { nil } && ticket_id == 0
}

fn appkit_pending_start_device_is_empty(pending AppKitPendingStartDevice) bool {
	return pending.value == unsafe { nil } && pending.transaction.ticket_id == 0
}

fn (mut backend AppKitBackend) release_window_drawable_lifetime(mut record &AppKitWindowRecord, mode AppKitWindowDrawableReleaseMode, error_text string) bool {
	if appkit_lifetime_pair_is_empty(record.active_drawable, record.active_drawable_ticket) {
		if mode == .close_frame {
			record.frame_active = false
			record.active_frame_lease = 0
		}
		return true
	}
	if backend.native_operations == unsafe { nil }
		|| (record.active_drawable != unsafe { nil } && record.state == unsafe { nil }) {
		return false
	}
	parent_identity := if record.state == unsafe { nil } {
		u64(0)
	} else {
		native_identity(record.state)
	}
	released := backend.release_appkit_lifetime_identity(record.active_drawable,
		record.active_drawable_ticket, .metal_drawable, parent_identity, error_text)
	record.active_drawable = released.value
	record.active_drawable_ticket = released.ticket_id
	if released.native_released && mode == .close_frame {
		record.frame_active = false
		record.active_frame_lease = 0
	}
	if !released.ticket_retired {
		return false
	}
	return true
}

fn (mut backend AppKitBackend) release_anchor_drawable_lifetime(error_text string) bool {
	if appkit_lifetime_pair_is_empty(backend.active_anchor_drawable,
		backend.active_anchor_drawable_ticket)
	{
		backend.active_anchor_lease = 0
		return true
	}
	if backend.native_operations == unsafe { nil }
		|| (backend.active_anchor_drawable != unsafe { nil }
		&& backend.anchor_state == unsafe { nil }) {
		return false
	}
	parent_identity := if backend.anchor_state == unsafe { nil } {
		u64(0)
	} else {
		native_identity(backend.anchor_state)
	}
	released := backend.release_appkit_lifetime_identity(backend.active_anchor_drawable,
		backend.active_anchor_drawable_ticket, .metal_drawable, parent_identity, error_text)
	backend.active_anchor_drawable = released.value
	backend.active_anchor_drawable_ticket = released.ticket_id
	if released.native_released {
		backend.active_anchor_lease = 0
	}
	if !released.ticket_retired {
		return false
	}
	return true
}

fn (mut backend AppKitBackend) release_active_frames_lifetime() bool {
	$if darwin {
		mut all_retired := true
		for i in 0 .. backend.windows.len {
			mut record := &backend.windows[i]
			if !backend.release_window_drawable_lifetime(mut record, .close_frame,
				err_appkit_metal_drawable_failed) {
				all_retired = false
			}
		}
		if !backend.release_anchor_drawable_lifetime(err_appkit_metal_drawable_failed) {
			all_retired = false
		}
		return all_retired
	} $else {
		return false
	}
}

fn (mut backend AppKitBackend) release_batch_pool_lifetime(seed NativeOperationSeed) bool {
	$if darwin {
		_ = seed
		if appkit_lifetime_pair_is_empty(backend.batch_autorelease_pool,
			backend.batch_autorelease_pool_ticket)
		{
			return true
		}
		if backend.native_operations == unsafe { nil } || backend.has_active_frames() {
			return false
		}
		released := backend.release_appkit_lifetime_identity(backend.batch_autorelease_pool,
			backend.batch_autorelease_pool_ticket, .appkit_autorelease_pool, 0,
			err_appkit_application_failed)
		backend.batch_autorelease_pool = released.value
		backend.batch_autorelease_pool_ticket = released.ticket_id
		return released.ticket_retired
	} $else {
		_ = seed
		return false
	}
}

fn (mut backend AppKitBackend) release_pending_window_state_lifetime() bool {
	pending := backend.pending_window_state
	if appkit_lifetime_pair_is_empty(pending.state, pending.state_ticket) {
		backend.pending_window_state = AppKitPendingWindowState{}
		return true
	}
	if backend.native_operations == unsafe { nil } || backend.has_active_frames()
		|| !appkit_lifetime_pair_is_empty(backend.batch_autorelease_pool, backend.batch_autorelease_pool_ticket) {
		return false
	}
	released := backend.release_appkit_lifetime_identity(pending.state, pending.state_ticket,
		.appkit_state, 0, err_appkit_create_window_failed)
	backend.pending_window_state = AppKitPendingWindowState{
		id:           pending.id
		state:        released.value
		state_ticket: released.ticket_id
	}
	if !released.ticket_retired {
		return false
	}
	backend.pending_window_state = AppKitPendingWindowState{}
	return true
}

fn (mut backend AppKitBackend) prepare_window_native_destroy(mut record &AppKitWindowRecord, seed NativeOperationSeed) bool {
	$if darwin {
		if record.native_destroyed {
			return true
		}
		if backend.native_operations == unsafe { nil }
			|| !backend.native_operations.owner_thread_is_current() {
			return false
		}
		if record.state == unsafe { nil } || backend.render_health.blocks_graphics() {
			record.native_destroyed = true
			return true
		}
		if C.v_multiwindow_appkit_is_main_thread() == 0 {
			return false
		}
		mut ordinals := backend.native_operations.reserve_ordinals(1) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			record.native_destroyed = true
			return true
		}
		destroy_context := ordinals.materialize(backend.native_operations, .metal,
			.surface_destroy, seed.with_target_identity(native_identity(record.state))) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			record.native_destroyed = true
			return true
		}
		raw := C.v_multiwindow_appkit_destroy_window(record.state)
		record.native_destroyed = true
		result := backend.accept_metal_result(destroy_context, raw, .void_completion,
			err_appkit_destroy_window_failed)
		if !result.succeeded() {
			backend.append_release_diagnostic(err_appkit_destroy_window_failed)
		}
		return true
	} $else {
		_ = backend
		_ = record
		_ = seed
		return false
	}
}

fn (mut backend AppKitBackend) release_window_resources(mut record &AppKitWindowRecord, seed NativeOperationSeed) bool {
	$if darwin {
		if !backend.release_window_drawable_lifetime(mut record, .close_frame,
			err_appkit_metal_drawable_failed) {
			return false
		}
		if backend.has_active_frames()
			|| !appkit_lifetime_pair_is_empty(backend.batch_autorelease_pool, backend.batch_autorelease_pool_ticket) {
			return false
		}
		if appkit_lifetime_pair_is_empty(record.state, record.state_ticket) {
			record.native_destroyed = true
			return true
		}
		if backend.native_operations == unsafe { nil }
			|| !backend.prepare_window_native_destroy(mut record, seed) {
			return false
		}
		released := backend.release_appkit_lifetime_identity(record.state, record.state_ticket,
			.appkit_state, 0, err_appkit_destroy_window_failed)
		record.state = released.value
		record.state_ticket = released.ticket_id
		return released.ticket_retired
	} $else {
		_ = backend
		_ = record
		_ = seed
		return false
	}
}

fn (mut backend AppKitBackend) prepare_anchor_native_destroy(seed NativeOperationSeed) bool {
	$if darwin {
		if backend.anchor_native_destroyed {
			return true
		}
		if backend.native_operations == unsafe { nil }
			|| !backend.native_operations.owner_thread_is_current() {
			return false
		}
		if backend.anchor_state == unsafe { nil } || backend.render_health.blocks_graphics() {
			backend.anchor_native_destroyed = true
			return true
		}
		if C.v_multiwindow_appkit_is_main_thread() == 0 {
			return false
		}
		mut ordinals := backend.native_operations.reserve_ordinals(1) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			backend.anchor_native_destroyed = true
			return true
		}
		context := ordinals.materialize(backend.native_operations, .metal, .surface_destroy,
			seed.with_target_identity(native_identity(backend.anchor_state))) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			backend.anchor_native_destroyed = true
			return true
		}
		raw := C.v_multiwindow_appkit_destroy_renderer_anchor(backend.anchor_state)
		backend.anchor_native_destroyed = true
		result := backend.accept_metal_result(context, raw, .void_completion,
			err_render_anchor_failed)
		if !result.succeeded() {
			backend.append_release_diagnostic(err_render_anchor_failed)
		}
		return true
	} $else {
		_ = backend
		_ = seed
		return false
	}
}

fn (mut backend AppKitBackend) release_anchor_state_lifetime(seed NativeOperationSeed) bool {
	if backend.has_active_frames()
		|| !appkit_lifetime_pair_is_empty(backend.batch_autorelease_pool, backend.batch_autorelease_pool_ticket) {
		return false
	}
	if appkit_lifetime_pair_is_empty(backend.anchor_state, backend.anchor_state_ticket) {
		backend.anchor_native_destroyed = false
		return true
	}
	if backend.native_operations == unsafe { nil } || !backend.prepare_anchor_native_destroy(seed) {
		return false
	}
	released := backend.release_appkit_lifetime_identity(backend.anchor_state,
		backend.anchor_state_ticket, .appkit_state, 0, err_render_anchor_failed)
	backend.anchor_state = released.value
	backend.anchor_state_ticket = released.ticket_id
	if !released.ticket_retired {
		return false
	}
	backend.anchor_native_destroyed = false
	return true
}

fn (backend &AppKitBackend) has_device_lifetime_dependents() bool {
	if !appkit_pending_start_device_is_empty(backend.pending_start_device)
		|| backend.has_active_frames()
		|| !appkit_lifetime_pair_is_empty(backend.batch_autorelease_pool, backend.batch_autorelease_pool_ticket)
		|| !appkit_lifetime_pair_is_empty(backend.anchor_state, backend.anchor_state_ticket)
		|| !appkit_lifetime_pair_is_empty(backend.pending_window_state.state, backend.pending_window_state.state_ticket) {
		return true
	}
	for record in backend.windows {
		if !appkit_lifetime_pair_is_empty(record.state, record.state_ticket)
			|| !appkit_lifetime_pair_is_empty(record.active_drawable, record.active_drawable_ticket) {
			return true
		}
	}
	return false
}

fn (mut backend AppKitBackend) release_device_lifetime() bool {
	if appkit_lifetime_pair_is_empty(backend.device, backend.device_ticket) {
		return true
	}
	if backend.native_operations == unsafe { nil } || backend.has_device_lifetime_dependents() {
		return false
	}
	released := backend.release_appkit_lifetime_identity(backend.device, backend.device_ticket,
		.metal_device, 0, err_appkit_metal_device_failed)
	backend.device = released.value
	backend.device_ticket = released.ticket_id
	return released.ticket_retired
}

fn (backend &AppKitBackend) retains_native_ownership() bool {
	if backend.native_operations != unsafe { nil }
		&& backend.native_operations.has_live_lifetime_tickets() {
		return true
	}
	if !appkit_lifetime_pair_is_empty(backend.device, backend.device_ticket)
		|| !appkit_pending_start_device_is_empty(backend.pending_start_device)
		|| !appkit_lifetime_pair_is_empty(backend.anchor_state, backend.anchor_state_ticket)
		|| !appkit_lifetime_pair_is_empty(backend.batch_autorelease_pool, backend.batch_autorelease_pool_ticket)
		|| !appkit_lifetime_pair_is_empty(backend.active_anchor_drawable, backend.active_anchor_drawable_ticket)
		|| !appkit_lifetime_pair_is_empty(backend.pending_window_state.state, backend.pending_window_state.state_ticket) {
		return true
	}
	for record in backend.windows {
		if !appkit_lifetime_pair_is_empty(record.state, record.state_ticket)
			|| !appkit_lifetime_pair_is_empty(record.active_drawable, record.active_drawable_ticket) {
			return true
		}
	}
	return false
}

fn (mut backend AppKitBackend) release_renderer_lifetime() bool {
	$if darwin {
		_ = backend.release_active_frames_lifetime()
		_ = backend.release_batch_pool_lifetime(NativeOperationSeed{
			call_site: .shutdown_release
			scope:     .batch
		})
		for i in 0 .. backend.windows.len {
			mut record := &backend.windows[i]
			_ = backend.release_window_resources(mut record, NativeOperationSeed{
				presence_mask:     native_context_has_window | native_context_has_target_generation
				call_site:         .shutdown_release
				scope:             .window_target
				window:            record.id
				target_generation: record.render_target_generation
			})
		}
		_ = backend.release_pending_window_state_lifetime()
		_ = backend.release_anchor_state_lifetime(NativeOperationSeed{
			call_site: .shutdown_release
			scope:     .anchor
		})
		backend.release_pending_start_device()
		_ = backend.release_device_lifetime()
		return !backend.retains_native_ownership()
	} $else {
		return false
	}
}

fn (backend &AppKitBackend) has_active_frames() bool {
	if backend.active_anchor_lease != 0 || backend.active_anchor_drawable != unsafe { nil }
		|| backend.active_anchor_drawable_ticket != 0 {
		return true
	}
	for record in backend.windows {
		if record.frame_active || record.active_frame_lease != 0
			|| record.active_drawable != unsafe { nil } || record.active_drawable_ticket != 0 {
			return true
		}
	}
	return false
}

fn (mut backend AppKitBackend) abandon_renderer_ownership() {
	backend.render_health = .abandoned
	_ = backend.release_renderer_lifetime()
}

fn (mut backend AppKitBackend) accept_native_render_window_loss(id WindowId) {
	index := backend.window_record_index(id) or { return }
	backend.windows[index].native_destroyed = true
}

fn (mut backend AppKitBackend) record_metal_result(result NativeRenderResult) NativeRenderResult {
	backend.render_health = renderer_health_after_result(backend.render_health, result)
	backend.native_operations.record_health_latch(result.context, backend.render_health)
	return result
}

fn (mut backend AppKitBackend) accept_metal_result(context NativeOperationContext, raw C.VMultiwindowNativePrimitive, validation NativeLocalValidation, error_text string) NativeRenderResult {
	capture := backend.native_operations.capture_call(context, raw)
	actual_validation := appkit_actual_output_validation(context, raw, validation)
	mut result := backend.native_operations.accept_metal(context, capture, actual_validation,
		error_text)
	result = backend.record_metal_result(result)
	if context.operation in [.object_release, .surface_destroy, .render_batch_end, .clear_state] {
		backend.native_operations.record_release(context, capture, result)
	}
	return result
}

fn appkit_actual_output_validation(context NativeOperationContext, raw C.VMultiwindowNativePrimitive, requested NativeLocalValidation) NativeLocalValidation {
	if requested != .none {
		return requested
	}
	match context.operation {
		.device_create, .window_surface_create, .anchor_surface_create, .render_batch_begin {
			if raw.valid_mask & native_valid_handle == 0 || raw.handle == 0 {
				return .null_output
			}
		}
		.drawable_acquire {
			if raw.valid_mask & native_valid_handle == 0 || raw.handle == 0
				|| raw.valid_mask & native_valid_object_identity_0 == 0
				|| raw.object_identity_0 == 0 || raw.valid_mask & native_valid_observed_count == 0
				|| raw.observed_count == 0 || raw.valid_mask & native_valid_selected_value == 0
				|| raw.selected_value <= 0 {
				return .null_output
			}
		}
		else {}
	}

	return .none
}

fn (mut backend AppKitBackend) accept_metal_clear_result(context NativeOperationContext, raw C.VMultiwindowNativePrimitive, expected_identity u64, error_text string) AppKitCapturedRelease {
	capture := backend.native_operations.capture_call(context, raw)
	validation := if !capture.actual.has(native_valid_object_identity_0)
		|| capture.actual.object_identity_0 != expected_identity
		|| !capture.effective.has(native_valid_object_identity_0)
		|| capture.effective.object_identity_0 != expected_identity {
		NativeLocalValidation.binding_mismatch
	} else {
		NativeLocalValidation.void_completion
	}
	mut result := backend.native_operations.accept_metal(context, capture, validation, error_text)
	result = backend.record_metal_result(result)
	return AppKitCapturedRelease{
		capture: capture
		result:  result
	}
}

fn (mut backend AppKitBackend) accept_metal_identity_result(context NativeOperationContext, raw C.VMultiwindowNativePrimitive, expected_identity_0 u64, expected_identity_1 u64, error_text string) NativeRenderResult {
	capture := backend.native_operations.capture_call(context, raw)
	validation := if !capture.actual.has(native_valid_object_identity_0)
		|| capture.actual.object_identity_0 != expected_identity_0
		|| !capture.actual.has(native_valid_object_identity_1)
		|| capture.actual.object_identity_1 != expected_identity_1
		|| !capture.actual.has(native_valid_object_identity_2)
		|| capture.actual.object_identity_2 == 0 || !capture.actual.has(native_valid_observed_count)
		|| capture.actual.observed_count == 0 || !capture.actual.has(native_valid_selected_value)
		|| capture.actual.selected_value <= 0
		|| !capture.effective.has(native_valid_object_identity_0)
		|| capture.effective.object_identity_0 != expected_identity_0
		|| !capture.effective.has(native_valid_object_identity_1)
		|| capture.effective.object_identity_1 != expected_identity_1
		|| !capture.effective.has(native_valid_object_identity_2)
		|| capture.effective.object_identity_2 == 0
		|| !capture.effective.has(native_valid_observed_count)
		|| capture.effective.observed_count == 0
		|| !capture.effective.has(native_valid_selected_value)
		|| capture.effective.selected_value <= 0 {
		NativeLocalValidation.binding_mismatch
	} else {
		NativeLocalValidation.void_completion
	}
	mut result := backend.native_operations.accept_metal(context, capture, validation, error_text)
	result = backend.record_metal_result(result)
	return result
}

fn appkit_bool_to_int(value bool) int {
	return if value { 1 } else { 0 }
}
