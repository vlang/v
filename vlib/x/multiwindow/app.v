module multiwindow

import sync
import x.executor

// App owns the low-level multi-window registry and owner queue.
@[heap]
pub struct App {
mut:
	config                      Config
	instance_id                 u64
	status                      AppStatus = .running
	stopping                    bool
	backend                     Backend
	windows                     []WindowSlot
	events                      []QueuedEvent
	event_deliveries            map[u64]EventDeliveryState
	next_event_delivery_token   u64 = 1
	event_dispatch_events       []QueuedEvent
	event_dispatch_active       bool
	event_dispatch_index        int
	event_delivery_terminal     bool
	teardown_acceptance_order   []WindowId
	window_finished             map[string]bool
	window_terminal             map[string]string
	frame_count                 u64
	deferred_poll_error         string
	deferred_poll_error_active  bool
	deferred_poll_barrier_token u64
	backend_event_terminal      string
	render_runtime              RenderRuntimeState
	render_bridge               voidptr
	owner_thread_id             u64
	admission_open              bool = true
	admission_epoch             u64  = 1
	owner_callback_depth        int
	next_stop_serial            u64 = 1
	stop_serial                 u64
	stop_prepared               bool
	stop_terminal               string
	pending_stop_errors         []string
	stop_native_retry_passes    u8
	internal_fault              InternalFaultPlan
	state_mutex                 &sync.Mutex        = sync.new_mutex()
	fault_mutex                 &sync.Mutex        = sync.new_mutex()
	owner                       &executor.Executor = unsafe { nil }
}

// new_app creates a low-level multi-window App with the mock backend by default.
pub fn new_app(config Config) !&App {
	if config.queue_size <= 0 {
		return error(err_queue_size_invalid)
	}
	instance_id := allocate_app_instance_id()!
	mut backend := new_backend(config.backend, config.require_renderer)!
	mut app := &App{
		config:           config
		instance_id:      instance_id
		status:           .running
		backend:          backend
		owner_thread_id:  sync.thread_id()
		state_mutex:      sync.new_mutex()
		fault_mutex:      sync.new_mutex()
		window_finished:  map[string]bool{}
		window_terminal:  map[string]string{}
		event_deliveries: map[u64]EventDeliveryState{}
	}
	app_lifetime_token := app.take_native_app_lifetime_token()!
	trace_token := app.begin_renderer_fault_trace_attempt()!
	app.backend.bind_app_native_operations(instance_id, app_lifetime_token, trace_token)!
	mut owner := executor.new(queue_size: config.queue_size)!
	app.owner = owner
	app.backend.start(config.require_renderer) or {
		app.owner.stop()
		return err
	}
	return app
}

// instance_id is process-monotonic and never reused. It stamps every handle,
// scheduler lease and accepted owner wrapper belonging to this App.
pub fn (app &App) instance_id() u64 {
	return app.instance_id
}

// status reports the application lifecycle state.
pub fn (app &App) status() AppStatus {
	app.state_mutex.lock()
	status := app.status
	app.state_mutex.unlock()
	return status
}

// capabilities reports the selected backend capabilities.
pub fn (app &App) capabilities() Capabilities {
	return app.backend.capabilities()
}

// create_window creates a backend window and returns its generation-checked id.
pub fn (mut app App) create_window(config WindowConfig) !WindowId {
	app.assert_owner_thread()!
	validate_window_config(config)!
	if app.config.require_renderer && config.sample_count != 1 {
		return error(err_render_sample_count_unsupported)
	}
	app.state_mutex.lock()
	app.ensure_running_locked() or {
		app.state_mutex.unlock()
		return err
	}
	app.ensure_event_admission_open_locked() or {
		app.state_mutex.unlock()
		return err
	}
	if app.render_bridge != unsafe { nil } && config.sample_count != 1 {
		app.state_mutex.unlock()
		return error(err_render_sample_count_unsupported)
	}
	id := app.allocate_window_id_locked() or {
		app.state_mutex.unlock()
		return err
	}
	created_delivery_token := app.reserve_event_delivery_tokens_locked(1) or {
		app.state_mutex.unlock()
		return err
	}
	app.state_mutex.unlock()
	actual_size := app.backend.create_window(id, config) or {
		app.state_mutex.lock()
		if id.slot >= 0 && id.slot < app.windows.len && app.windows[id.slot].id == id {
			app.windows[id.slot].status = .invalid
		}
		app.state_mutex.unlock()
		return err
	}
	app.state_mutex.lock()
	if app.status != .running || app.stopping {
		app.state_mutex.unlock()
		app.backend.finish_window_teardown(id) or {}
		return error(err_app_stopped)
	}
	native_teardown_serial := app.take_destroy_serial_locked() or {
		app.windows[id.slot].status = .invalid
		app.state_mutex.unlock()
		app.backend.finish_window_teardown(id) or {}
		return err
	}
	app.windows[id.slot] = WindowSlot{
		id:                     id
		config:                 window_config_with_size(config, actual_size.width,
			actual_size.height)
		status:                 .alive
		native_teardown_serial: native_teardown_serial
	}
	app.register_render_window_locked(id, config, actual_size) or {
		app.windows[id.slot].status = .invalid
		app.state_mutex.unlock()
		app.backend.finish_window_teardown(id) or {}
		return err
	}
	app.enqueue_reserved_event_locked(queued_lifecycle_event(Event{
		kind:      .window_created
		window_id: id
		width:     actual_size.width
		height:    actual_size.height
	}), created_delivery_token)
	app.state_mutex.unlock()
	return id
}

// destroy_window destroys one live window. The app remains alive when any window,
// including the last one, is destroyed.
pub fn (mut app App) destroy_window(id WindowId) ! {
	if app.window_destroy_finished(id) {
		return app.return_window_terminal(id)
	}
	ticket := app.prepare_window_destroy(id)!
	app.seal_window_destroy(ticket) or {
		app.rollback_window_destroy(ticket) or {}
		return err
	}
	app.finish_window_destroy(ticket, []string{})!
}

// set_window_title updates the native title and then the authoritative App state.
pub fn (mut app App) set_window_title(id WindowId, title string) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	index := app.live_window_index(id)!
	app.backend.set_window_title(id, title)!
	app.windows[index].config = window_config_with_title(app.windows[index].config, title)
}

// resize_window requests a native resize and then updates the authoritative App state.
pub fn (mut app App) resize_window(id WindowId, width int, height int) ! {
	app.assert_owner_thread()!
	validate_window_size(width, height)!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	index := app.live_window_index(id)!
	delivery_token := app.reserve_event_delivery_tokens_locked(1)!
	actual_size := app.backend.resize_window(id, width, height)!
	app.windows[index].config = window_config_with_size(app.windows[index].config,
		actual_size.width, actual_size.height)
	app.apply_unavailable_backend_observation_locked(index, id, actual_size.width,
		actual_size.height, 0, 0, .resize_pending)
	app.enqueue_reserved_event_locked(queued_lifecycle_event(Event{
		kind:      .window_resized
		window_id: id
		width:     actual_size.width
		height:    actual_size.height
	}), delivery_token)
}

// set_window_cursor updates the native hover cursor for a live window when the
// selected backend reports capabilities().cursor_shapes.
pub fn (mut app App) set_window_cursor(id WindowId, shape CursorShape) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.live_window_index(id)!
	app.backend.set_window_cursor(id, shape)!
}

// begin_window_move starts a user-driven native move for a live window.
// Backends that require a recent native input serial may reject the request
// when it is not made from a deliberate user-action path.
pub fn (mut app App) begin_window_move(id WindowId) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.live_window_index(id)!
	app.backend.begin_window_move(id)!
}

// begin_window_resize starts a user-driven native resize for a live resizable window.
pub fn (mut app App) begin_window_resize(id WindowId, edge WindowResizeEdge) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	index := app.live_window_index(id)!
	if !app.windows[index].config.resizable {
		return error(err_capability_unsupported)
	}
	app.backend.begin_window_resize(id, edge)!
}

// window_info returns a snapshot of the authoritative App-side window state.
pub fn (app &App) window_info(id WindowId) !WindowInfo {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	index := app.live_window_index(id)!
	slot := app.windows[index]
	native_decorations := app.backend.window_native_decorations(slot.id, slot.config)!
	return window_info_from_slot(slot, native_decorations)
}

// window_ids returns live window ids in stable slot order.
pub fn (app &App) window_ids() ![]WindowId {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	mut ids := []WindowId{cap: app.windows.len}
	for slot in app.windows {
		if slot.status == .alive {
			ids << slot.id
		}
	}
	return ids
}

// window_infos returns live window snapshots in stable slot order.
pub fn (app &App) window_infos() ![]WindowInfo {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	mut infos := []WindowInfo{cap: app.windows.len}
	for slot in app.windows {
		if slot.status == .alive {
			native_decorations := app.backend.window_native_decorations(slot.id, slot.config)!
			infos << window_info_from_slot(slot, native_decorations)
		}
	}
	return infos
}

// window_exists reports whether id currently points to a live window.
pub fn (app &App) window_exists(id WindowId) bool {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if id.app_instance != app.instance_id || id.slot < 0 || id.slot >= app.windows.len {
		return false
	}
	slot := app.windows[id.slot]
	return slot.status == .alive && slot.id == id && slot.destroy_stage in [.none, .prepared]
}

// window_status returns the lifecycle status for a valid generation.
pub fn (app &App) window_status(id WindowId) !WindowStatus {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if id.app_instance != app.instance_id {
		return error(err_app_identity_mismatch)
	}
	if id.slot < 0 || id.slot >= app.windows.len {
		return error(err_window_not_found)
	}
	slot := app.windows[id.slot]
	if slot.id != id {
		return error(err_stale_window)
	}
	return slot.status
}

// drain_events returns and clears pending lifecycle events.
pub fn (mut app App) drain_events() ![]Event {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if app.event_dispatch_active {
		return error(err_event_dispatch_active)
	}
	return app.drain_lifecycle_events_locked()!
}

// drain_input_events returns and clears pending input events without consuming
// lifecycle events.
pub fn (mut app App) drain_input_events() ![]InputEvent {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if app.event_dispatch_active {
		return error(err_event_dispatch_active)
	}
	return app.drain_input_events_locked()!
}

// drain_queued_events returns and clears pending lifecycle/input events in the
// exact order accepted by App.
pub fn (mut app App) drain_queued_events() ![]QueuedEvent {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if app.event_dispatch_active {
		return error(err_event_dispatch_active)
	}
	mut drain_count := 0
	for event in app.events {
		if app.queued_event_blocked_by_teardown_locked(event) {
			break
		}
		drain_count++
	}
	internal_events := app.events[..drain_count].clone()
	for event in internal_events {
		app.validate_queued_delivery_locked(event)!
	}
	for event in internal_events {
		app.complete_queued_delivery_locked(event)
	}
	app.events = app.events[drain_count..].clone()
	app.release_terminal_delivery_storage_locked()
	mut events := []QueuedEvent{cap: internal_events.len}
	for event in internal_events {
		events << queued_event_without_delivery_token(event)
	}
	return events
}

// poll_events lets the backend route native lifecycle and input events into App events.
pub fn (mut app App) poll_events() !int {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	if app.event_dispatch_active {
		app.state_mutex.unlock()
		return 0
	}
	if app.deferred_poll_error_active {
		if app.delivery_barrier_pending_locked(app.deferred_poll_barrier_token) {
			app.state_mutex.unlock()
			return 0
		}
		deferred_error := app.take_deferred_poll_error_locked()
		app.state_mutex.unlock()
		return error(deferred_error)
	}
	app.ensure_running_locked() or {
		app.state_mutex.unlock()
		return err
	}
	frame_count := next_nonwrapping_u64(app.frame_count) or {
		app.render_runtime.renderer_terminal = err_render_renderer_failed
		app.state_mutex.unlock()
		return error(err_render_renderer_failed)
	}
	app.state_mutex.unlock()
	events := app.backend.poll_queued_events()!

	// Apply observations in native order while Backend retains the batch. This
	// makes all state preceding a destruction part of its terminal snapshot.
	acceptance := app.accept_backend_event_batch(events, frame_count)!
	app.state_mutex.lock()
	running := app.status == .running && !app.stopping
	app.state_mutex.unlock()
	if acceptance.delivery_error != '' {
		return acceptance.accepted
	}
	if !running {
		return error(err_app_stopped)
	}

	// Event acceptance is durable before the fallible backend snapshot query.
	// In particular, a native-destroyed slot cannot disappear if metrics fail.
	render_updates := app.backend.render_updates() or {
		app.mark_renderer_terminal(err.msg())
		app.state_mutex.lock()
		app.defer_poll_error_locked(acceptance.barrier_token, err.msg())
		app.state_mutex.unlock()
		return acceptance.accepted
	}
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if app.status != .running || app.stopping {
		return error(err_app_stopped)
	}
	for update in render_updates {
		app.apply_backend_render_update_locked(update)
	}
	return acceptance.accepted
}

fn (mut app App) drain_lifecycle_events_locked() ![]Event {
	mut lifecycle_events := []Event{cap: app.events.len}
	mut delivered_events := []QueuedEvent{cap: app.events.len}
	mut remaining_events := []QueuedEvent{cap: app.events.len}
	mut blocked := false
	for event in app.events {
		if blocked || app.queued_event_blocked_by_teardown_locked(event) {
			blocked = true
			remaining_events << event
			continue
		}
		match event.kind {
			.lifecycle {
				lifecycle_events << event.lifecycle
				delivered_events << event
			}
			.input {
				remaining_events << event
			}
		}
	}
	for event in delivered_events {
		app.validate_queued_delivery_locked(event)!
	}
	for event in delivered_events {
		app.complete_queued_delivery_locked(event)
	}
	app.events = remaining_events
	app.release_terminal_delivery_storage_locked()
	return lifecycle_events
}

fn (mut app App) drain_input_events_locked() ![]InputEvent {
	mut input_events := []InputEvent{cap: app.events.len}
	mut delivered_events := []QueuedEvent{cap: app.events.len}
	mut remaining_events := []QueuedEvent{cap: app.events.len}
	mut blocked := false
	for event in app.events {
		if blocked || app.queued_event_blocked_by_teardown_locked(event) {
			blocked = true
			remaining_events << event
			continue
		}
		match event.kind {
			.lifecycle {
				remaining_events << event
			}
			.input {
				input_events << event.input
				delivered_events << event
			}
		}
	}
	for event in delivered_events {
		app.validate_queued_delivery_locked(event)!
	}
	for event in delivered_events {
		app.complete_queued_delivery_locked(event)
	}
	app.events = remaining_events
	app.release_terminal_delivery_storage_locked()
	return input_events
}

fn (app &App) queued_event_blocked_by_teardown_locked(event QueuedEvent) bool {
	if event.kind != .lifecycle || event.lifecycle.kind != .window_destroyed {
		return false
	}
	id := event.lifecycle.window_id
	if id.app_instance != app.instance_id || id.slot < 0 || id.slot >= app.windows.len {
		return false
	}
	slot := app.windows[id.slot]
	return slot.id == id && slot.destroy_event_queued && !slot.destroy_event_ready
}

fn (app &App) backend_event_generation_valid_locked(event QueuedEvent) bool {
	id := if event.kind == .lifecycle {
		event.lifecycle.window_id
	} else {
		event.input.window_id
	}
	if id.app_instance != app.instance_id || id.slot < 0 || id.slot >= app.windows.len {
		return false
	}
	slot := app.windows[id.slot]
	return slot.id == id && slot.status == .alive && slot.destroy_stage in [.none, .prepared]
}

fn (app &App) backend_window_generation_present_locked(id WindowId) bool {
	if id.app_instance != app.instance_id || id.slot < 0 || id.slot >= app.windows.len {
		return false
	}
	slot := app.windows[id.slot]
	return slot.id == id && slot.status == .alive
}

fn (mut app App) accept_lifecycle_event_locked(event Event, generation_valid bool, delivery_token u64) bool {
	match event.kind {
		.window_close_requested {
			if !generation_valid || !app.backend_window_generation_present_locked(event.window_id) {
				return false
			}
			app.enqueue_reserved_event_locked(queued_lifecycle_event(event), delivery_token)
			return true
		}
		.window_destroyed {
			return app.queue_backend_teardown_event_locked(event.window_id, delivery_token)
		}
		.window_resized {
			if !generation_valid || event.width <= 0 || event.height <= 0
				|| !app.backend_window_generation_present_locked(event.window_id) {
				return false
			}
			index := event.window_id.slot
			app.windows[index].config = window_config_with_size(app.windows[index].config,
				event.width, event.height)
			app.apply_unavailable_backend_observation_locked(index, event.window_id, event.width,
				event.height, 0, 0, .resize_pending)
			app.enqueue_reserved_event_locked(queued_lifecycle_event(event), delivery_token)
			return true
		}
		else {
			if !generation_valid || !app.backend_window_generation_present_locked(event.window_id) {
				return false
			}
			app.enqueue_reserved_event_locked(queued_lifecycle_event(event), delivery_token)
			return true
		}
	}
}

fn (mut app App) accept_input_event_locked(event InputEvent, frame_count u64, generation_valid bool, delivery_token u64) bool {
	if !generation_valid || !app.backend_window_generation_present_locked(event.window_id) {
		return false
	}
	input_event := input_event_with_frame_count(event, frame_count)
	if input_event.kind == .resized {
		if input_event.window_width <= 0 || input_event.window_height <= 0 {
			return false
		}
		index := input_event.window_id.slot
		app.windows[index].config = window_config_with_size(app.windows[index].config,
			input_event.window_width, input_event.window_height)
		app.apply_backend_input_observation_locked(index, input_event)
		app.enqueue_reserved_event_locked(queued_input_event(input_event), delivery_token)
		return true
	}
	app.apply_backend_input_observation_locked(input_event.window_id.slot, input_event)
	app.enqueue_reserved_event_locked(queued_input_event(input_event), delivery_token)
	return true
}

fn input_event_with_frame_count(event InputEvent, frame_count u64) InputEvent {
	if event.frame_count != 0 {
		return event
	}
	return InputEvent{
		kind:               event.kind
		window_id:          event.window_id
		frame_count:        frame_count
		key_code:           event.key_code
		char_code:          event.char_code
		key_repeat:         event.key_repeat
		modifiers:          event.modifiers
		mouse_button:       event.mouse_button
		mouse_x:            event.mouse_x
		mouse_y:            event.mouse_y
		mouse_dx:           event.mouse_dx
		mouse_dy:           event.mouse_dy
		scroll_x:           event.scroll_x
		scroll_y:           event.scroll_y
		num_touches:        event.num_touches
		touches:            event.touches
		window_width:       event.window_width
		window_height:      event.window_height
		framebuffer_width:  event.framebuffer_width
		framebuffer_height: event.framebuffer_height
		dropped_files:      event.dropped_files.clone()
	}
}

$if test {
	// enqueue_mock_close_requested_for_test injects a mock-native close request
	// into the backend queue only in test builds.
	pub fn (mut app App) enqueue_mock_close_requested_for_test(id WindowId) ! {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		defer {
			app.state_mutex.unlock()
		}
		app.ensure_running_locked()!
		app.ensure_event_admission_open_locked()!
		app.live_window_index(id)!
		if app.backend.kind != .mock {
			return error(err_capability_unsupported)
		}
		app.backend.mock.enqueue_event(Event{
			kind:      .window_close_requested
			window_id: id
		})
	}

	// enqueue_mock_input_for_test injects a mock-native input event into the
	// backend queue only in test builds.
	pub fn (mut app App) enqueue_mock_input_for_test(event InputEvent) ! {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		defer {
			app.state_mutex.unlock()
		}
		app.ensure_running_locked()!
		app.ensure_event_admission_open_locked()!
		app.live_window_index(event.window_id)!
		if app.backend.kind != .mock {
			return error(err_capability_unsupported)
		}
		app.backend.mock.enqueue_input_event(event)
	}

	// enqueue_mock_close_requested_unchecked_for_test intentionally bypasses
	// WindowId liveness validation so App.poll_events() filtering can be tested.
	fn (mut app App) enqueue_mock_close_requested_unchecked_for_test(id WindowId) ! {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		defer {
			app.state_mutex.unlock()
		}
		app.ensure_running_locked()!
		app.ensure_event_admission_open_locked()!
		if app.backend.kind != .mock {
			return error(err_capability_unsupported)
		}
		app.backend.mock.enqueue_event(Event{
			kind:      .window_close_requested
			window_id: id
		})
	}

	// enqueue_mock_input_unchecked_for_test intentionally bypasses WindowId
	// liveness validation so App.poll_events() filtering can be tested.
	fn (mut app App) enqueue_mock_input_unchecked_for_test(event InputEvent) ! {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		defer {
			app.state_mutex.unlock()
		}
		app.ensure_running_locked()!
		app.ensure_event_admission_open_locked()!
		if app.backend.kind != .mock {
			return error(err_capability_unsupported)
		}
		app.backend.mock.enqueue_input_event(event)
	}
}

// post submits a short owner-thread callback if queue capacity is available.
pub fn (mut app App) post(f AppJobFn) ! {
	app.try_post(f)!
}

// try_post submits a short owner-thread callback without waiting for capacity.
// It follows x.executor's ! contract: queue-full and closed-queue states are
// returned as errors instead of a bool.
pub fn (mut app App) try_post(f AppJobFn) ! {
	if f == unsafe { nil } {
		return error(err_nil_job)
	}
	app.state_mutex.lock()
	if app.status != .running || app.stopping || !app.admission_open {
		app.state_mutex.unlock()
		return error(err_app_stopped)
	}
	app_instance := app.instance_id
	admission_epoch := app.admission_epoch
	app_ptr := unsafe { voidptr(&app) }
	app.owner.try_post(fn [app_ptr, app_instance, admission_epoch, f] () ! {
		mut queued_app := unsafe { &App(app_ptr) }
		if !queued_app.accepted_wrapper_is_current(app_instance, admission_epoch) {
			return
		}
		queued_app.begin_owner_callback()
		defer {
			queued_app.end_owner_callback()
		}
		f(mut queued_app)!
	}) or {
		app.state_mutex.unlock()
		return wrap_executor_error(err)
	}
	app.state_mutex.unlock()
}

// drain_pending executes up to max_jobs queued owner callbacks while the app is running.
// After stop(), accepted-but-pending callbacks are not executed and this returns
// `multiwindow: app is stopped`.
pub fn (mut app App) drain_pending(max_jobs int) !int {
	app.assert_owner_thread()!
	app.ensure_running()!
	if max_jobs <= 0 {
		return error(err_drain_limit_invalid)
	}
	mut ran := 0
	for ran < max_jobs {
		app.ensure_running() or {
			_ = app.drain_cancelled_wrappers()
			return err
		}
		drained := app.owner.drain_pending(1) or {
			if app.status() != .running {
				_ = app.drain_cancelled_wrappers()
			}
			return wrap_executor_error(err)
		}
		if drained == 0 {
			return ran
		}
		ran += drained
		if app.status() != .running {
			_ = app.drain_cancelled_wrappers()
			return error(err_app_stopped)
		}
	}
	return ran
}

// stop destroys live windows, closes owner-queue admission and marks the app stopped.
// Pending owner callbacks are canceled logically: public drain_pending() refuses
// to run them after stop().
pub fn (mut app App) stop() ! {
	app.assert_owner_thread()!
	if app.status == .stopped {
		return app.return_stop_terminal()
	}
	app.state_mutex.lock()
	native_retry := app.stop_native_retry_passes > 0
	app.state_mutex.unlock()
	ticket := app.prepare_stop()!
	if native_retry {
		app.finish_stop(ticket, []string{})!
		return
	}
	mut errors := []string{}
	for destroy_ticket in app.prepared_window_tickets_for_stop() {
		app.seal_window_destroy(destroy_ticket) or {
			errors << err.msg()
			app.rollback_window_destroy(destroy_ticket) or { errors << err.msg() }
			continue
		}
		app.finish_window_destroy(destroy_ticket, []string{}) or { errors << err.msg() }
	}
	for destroy_ticket in app.sealed_window_tickets_for_stop() {
		app.finish_window_destroy(destroy_ticket, []string{}) or { errors << err.msg() }
	}
	ids := app.live_window_ids_for_stop()
	for id in ids {
		mut destroy_ticket := app.prepare_window_destroy_for_stop(id) or {
			errors << err.msg()
			continue
		}
		mut sealed := false
		for attempt in 0 .. 2 {
			app.seal_window_destroy(destroy_ticket) or {
				errors << err.msg()
				app.rollback_window_destroy(destroy_ticket) or { errors << err.msg() }
				if attempt == 1 {
					break
				}
				destroy_ticket = app.prepare_window_destroy_for_stop(id) or {
					errors << err.msg()
					break
				}
				continue
			}
			sealed = true
			break
		}
		if !sealed {
			continue
		}
		app.finish_window_destroy(destroy_ticket, []string{}) or { errors << err.msg() }
	}
	for destroy_ticket in app.seal_remaining_windows_terminal_for_stop() {
		app.finish_window_destroy(destroy_ticket, []string{}) or { errors << err.msg() }
	}
	app.shutdown_render_bridge_for_stop() or { errors << err.msg() }
	app.finish_stop(ticket, errors)!
}

fn (app &App) assert_owner_thread() ! {
	if app.owner_thread_id != sync.thread_id() {
		return error(err_owner_thread_required)
	}
}

fn (app &App) ensure_running() ! {
	app.state_mutex.lock()
	status := app.status
	stopping := app.stopping
	admission_open := app.admission_open
	backend_event_terminal := app.backend_event_terminal
	app.state_mutex.unlock()
	if backend_event_terminal != '' {
		return error(backend_event_terminal)
	}
	if status != .running || stopping || !admission_open {
		return error(err_app_stopped)
	}
}

fn (app &App) ensure_running_locked() ! {
	if app.backend_event_terminal != '' {
		return error(app.backend_event_terminal)
	}
	if app.status != .running || app.stopping || !app.admission_open {
		return error(err_app_stopped)
	}
}

fn validate_window_config(config WindowConfig) ! {
	validate_window_size(config.width, config.height)!
	if config.min_width < 0 || config.min_height < 0 {
		return error(err_invalid_window_size)
	}
	if config.sample_count <= 0 {
		return error(err_render_sample_count_invalid)
	}
}

fn validate_window_size(width int, height int) ! {
	if width <= 0 || height <= 0 {
		return error(err_invalid_window_size)
	}
}

fn window_config_with_title(config WindowConfig, title string) WindowConfig {
	return WindowConfig{
		title:           title
		width:           config.width
		height:          config.height
		min_width:       config.min_width
		min_height:      config.min_height
		resizable:       config.resizable
		visible:         config.visible
		high_dpi:        config.high_dpi
		borderless:      config.borderless
		fullscreen:      config.fullscreen
		sample_count:    config.sample_count
		redraw_mode:     config.redraw_mode
		render_workload: config.render_workload
	}
}

fn window_config_with_size(config WindowConfig, width int, height int) WindowConfig {
	return WindowConfig{
		title:           config.title
		width:           width
		height:          height
		min_width:       config.min_width
		min_height:      config.min_height
		resizable:       config.resizable
		visible:         config.visible
		high_dpi:        config.high_dpi
		borderless:      config.borderless
		fullscreen:      config.fullscreen
		sample_count:    config.sample_count
		redraw_mode:     config.redraw_mode
		render_workload: config.render_workload
	}
}

fn window_info_from_slot(slot WindowSlot, native_decorations bool) WindowInfo {
	config := slot.config
	return WindowInfo{
		id:                 slot.id
		status:             slot.status
		title:              config.title
		width:              config.width
		height:             config.height
		min_width:          config.min_width
		min_height:         config.min_height
		resizable:          config.resizable
		visible:            config.visible
		high_dpi:           config.high_dpi
		borderless:         config.borderless
		fullscreen:         config.fullscreen
		native_decorations: native_decorations
	}
}

fn (mut app App) allocate_window_id_locked() !WindowId {
	for i, slot in app.windows {
		if slot.status != .alive && !slot.generation_exhausted {
			if slot.id.generation == u32(0xffffffff) {
				app.windows[i].generation_exhausted = true
				continue
			}
			generation := slot.id.generation + 1
			return WindowId{
				app_instance: app.instance_id
				slot:         i
				generation:   generation
			}
		}
	}
	id := WindowId{
		app_instance: app.instance_id
		slot:         app.windows.len
		generation:   1
	}
	app.windows << WindowSlot{
		id:     id
		status: .invalid
	}
	return id
}

fn (app &App) live_window_index(id WindowId) !int {
	if id.app_instance != app.instance_id {
		return error(err_app_identity_mismatch)
	}
	if id.slot < 0 || id.slot >= app.windows.len {
		return error(err_window_not_found)
	}
	slot := app.windows[id.slot]
	if slot.id != id {
		return error(err_stale_window)
	}
	if slot.status != .alive || slot.destroy_stage !in [.none, .prepared] {
		return error(err_stale_window)
	}
	return id.slot
}

fn wrap_executor_error(err IError) IError {
	msg := err.msg()
	if msg.starts_with('multiwindow:') {
		return err
	}
	return match msg {
		'executor: queue is full' { error(err_owner_queue_full) }
		'executor: executor is closed' { error(err_owner_queue_closed) }
		'executor: executor is stopped' { error(err_owner_queue_closed) }
		'executor: drain limit must be positive' { error(err_drain_limit_invalid) }
		else { error('${err_owner_queue_failed}: ${msg}') }
	}
}
