module multiwindow

fn (app &App) ensure_mock_service_locked() ! {
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	if app.backend.kind != .mock {
		return error(err_capability_unsupported)
	}
}

// The core window slot is the authority for new service work. The service
// registry intentionally survives a sealed destroy long enough to publish
// cancellation terminals, so registry membership alone is not admission.
fn (app &App) service_window_index_for_admission_locked(id WindowId) !int {
	service_index := app.services.window_index(id)!
	index := app.live_window_index(id)!
	if app.windows[index].services_cancelled {
		return error(err_stale_window)
	}
	return service_index
}

fn (mut app App) enqueue_service_event_locked(event ServiceEvent) !u64 {
	token := app.reserve_event_delivery_tokens_locked(1)!
	app.enqueue_reserved_service_event_locked(event, token)
	return token
}

fn (mut app App) enqueue_reserved_service_event_locked(event ServiceEvent, token u64) {
	sequenced := service_event_with_sequence(event, token)
	app.enqueue_reserved_event_locked(queued_service_event(sequenced), token)
}

fn (mut app App) enqueue_readback_event_locked(result ServiceReadbackResult) !u64 {
	token := app.reserve_event_delivery_tokens_locked(1)!
	app.enqueue_reserved_event_locked(queued_readback_event(result), token)
	return token
}

fn (mut app App) publish_mock_state_locked(index int, operation ServiceOperation) ! {
	app.publish_state_candidate_locked(index, operation, app.services.windows[index].state)!
}

fn (mut app App) publish_state_candidate_locked(index int, operation ServiceOperation, candidate ServiceWindowState) ! {
	token := app.reserve_event_delivery_tokens_locked(1)!
	app.publish_reserved_state_candidate_locked(index, operation, candidate, token)
}

fn (mut app App) publish_reserved_state_candidate_locked(index int, operation ServiceOperation, candidate ServiceWindowState, token u64) {
	sequenced_state := service_window_state_with_sequence(candidate, token)
	app.services.windows[index].state = sequenced_state
	app.enqueue_reserved_event_locked(queued_service_event(ServiceEvent{
		kind:      .state
		window:    app.services.windows[index].id
		sequence:  token
		state:     sequenced_state
		operation: operation
	}), token)
}

struct NativeStateOperationAdmission {
	uses_mock            bool
	publication_deferred bool
	reserved_token       u64
}

fn (mut app App) publish_mock_focus_locked(index int) ! {
	mut losses := []int{}
	for i, window in app.services.windows {
		if i != index && (window.state.active == .on || window.state.focused == .on) {
			losses << i
		}
	}
	target := app.services.windows[index]
	needs_gain := target.state.active != .on || target.state.focused != .on
	count := losses.len + if needs_gain { 1 } else { 0 }
	if count == 0 {
		return
	}
	first_token := app.reserve_event_delivery_tokens_locked(count)!
	mut offset := u64(0)
	for loss_index in losses {
		token := first_token + offset
		state := service_window_state_with_sequence(ServiceWindowState{
			...app.services.windows[loss_index].state
			active:  .off
			focused: .off
		}, token)
		app.services.windows[loss_index].state = state
		app.enqueue_reserved_event_locked(queued_service_event(ServiceEvent{
			kind:      .state
			window:    app.services.windows[loss_index].id
			sequence:  token
			state:     state
			operation: .focus
		}), token)
		offset++
	}
	if needs_gain {
		token := first_token + offset
		state := service_window_state_with_sequence(ServiceWindowState{
			...app.services.windows[index].state
			active:  .on
			focused: .on
		}, token)
		app.services.windows[index].state = state
		app.enqueue_reserved_event_locked(queued_service_event(ServiceEvent{
			kind:      .state
			window:    app.services.windows[index].id
			sequence:  token
			state:     state
			operation: .focus
		}), token)
	}
}

// service_window_state returns the latest native observation for one live window.
pub fn (app &App) service_window_state(id WindowId) !ServiceWindowState {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	index := app.service_window_index_for_admission_locked(id)!
	state := app.services.windows[index].state
	if app.backend.kind == .win32 {
		observed := service_window_state_with_registered_monitor_membership(app.backend.service_window_state(id)!,
			app.services.monitors)
		return service_window_state_with_sequence(merge_service_window_state(state, observed),
			state.sequence)
	}
	return service_window_state_with_sequence(state, state.sequence)
}

fn service_window_state_with_registered_monitor_membership(observed ServiceWindowState, monitors []ServiceMonitorInfo) ServiceWindowState {
	if !service_window_state_observes_monitor_membership(observed) || observed.monitor_ids.len == 0 {
		return observed
	}
	for id in observed.monitor_ids {
		mut registered := false
		for monitor in monitors {
			if monitor.id == id && monitor.available {
				registered = true
				break
			}
		}
		if !registered {
			return ServiceWindowState{
				...observed
				monitor_ids:                 []
				monitor_membership_observed: false
			}
		}
	}
	return observed
}

// service_monitor_ids returns currently available generation-checked monitors.
pub fn (app &App) service_monitor_ids() ![]ServiceMonitorId {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	mut ids := []ServiceMonitorId{cap: app.services.monitors.len}
	for monitor in app.services.monitors {
		if monitor.available {
			ids << monitor.id
		}
	}
	return ids
}

// service_monitor_info returns the latest snapshot for one monitor generation.
pub fn (app &App) service_monitor_info(id ServiceMonitorId) !ServiceMonitorInfo {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	index := app.services.monitor_index(id)!
	monitor := app.services.monitors[index]
	return ServiceMonitorInfo{
		native_key: monitor.native_key
		id:         monitor.id
		name:       monitor.name
		geometry:   monitor.geometry
		work_area:  monitor.work_area
		scale:      monitor.scale
		primary:    monitor.primary
		available:  monitor.available
		sequence:   monitor.sequence
	}
}

// service_operation_capability reports authoritative runtime support for one
// operation on one live window. Query it immediately before optional operations.
pub fn (app &App) service_operation_capability(id WindowId, operation ServiceOperation) !ServiceOperationCapability {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.service_window_index_for_admission_locked(id)!
	return app.backend.service_operation_capability(id, operation)
}

// service_cursor_support reports runtime support for one native cursor shape.
pub fn (app &App) service_cursor_support(id WindowId, shape CursorShape) !ServiceSupportLevel {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.service_window_index_for_admission_locked(id)!
	return app.backend.cursor_support(shape)
}

// service_show_window requests that a live window become mapped and visible.
pub fn (mut app App) service_show_window(id WindowId) ! {
	admission := app.begin_native_state_operation(id, .show)!
	if admission.uses_mock {
		app.update_mock_mapping(id, true, .show)!
		return
	}
	state := app.backend.service_show_window(id)!
	app.publish_admitted_native_state(id, .show, state, admission)!
}

// service_hide_window requests that a live window become hidden or unmapped.
pub fn (mut app App) service_hide_window(id WindowId) ! {
	admission := app.begin_native_state_operation(id, .hide)!
	if admission.uses_mock {
		app.update_mock_mapping(id, false, .hide)!
		return
	}
	state := app.backend.service_hide_window(id)!
	app.publish_admitted_native_state(id, .hide, state, admission)!
}

fn (mut app App) update_mock_mapping(id WindowId, visible bool, operation ServiceOperation) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_mock_service_locked()!
	index := app.service_window_index_for_admission_locked(id)!
	candidate := ServiceWindowState{
		...app.services.windows[index].state
		mapping:    if visible { .mapped } else { .unmapped }
		visibility: if visible { .visible } else { .hidden }
	}
	app.publish_state_candidate_locked(index, operation, candidate)!
}

// service_request_focus asks the native platform to focus a live window.
pub fn (mut app App) service_request_focus(id WindowId) ! {
	admission := app.begin_native_state_operation(id, .focus)!
	if !admission.uses_mock {
		state := app.backend.service_focus_window(id)!
		app.publish_admitted_native_state(id, .focus, state, admission)!
		return
	}
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_mock_service_locked()!
	index := app.service_window_index_for_admission_locked(id)!
	app.publish_mock_focus_locked(index)!
}

// service_raise_window asks the native platform to raise a live window.
pub fn (mut app App) service_raise_window(id WindowId) ! {
	admission := app.begin_native_state_operation(id, .raise)!
	if admission.uses_mock {
		app.publish_mock_unchanged_state(id, .raise)!
		return
	}
	state := app.backend.service_raise_window(id)!
	app.publish_admitted_native_state(id, .raise, state, admission)!
}

// service_set_position requests a native top-level position when supported.
pub fn (mut app App) service_set_position(id WindowId, x int, y int) ! {
	admission := app.begin_native_state_operation(id, .position)!
	if !admission.uses_mock {
		state := app.backend.service_set_window_position(id, x, y)!
		app.publish_admitted_native_state(id, .position, state, admission)!
		return
	}
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_mock_service_locked()!
	index := app.service_window_index_for_admission_locked(id)!
	candidate := ServiceWindowState{
		...app.services.windows[index].state
		position: ServicePosition{
			known: true
			x:     x
			y:     y
		}
	}
	app.publish_state_candidate_locked(index, .position, candidate)!
}

// service_minimize_window requests native minimization.
pub fn (mut app App) service_minimize_window(id WindowId) ! {
	admission := app.begin_native_state_operation(id, .minimize)!
	if admission.uses_mock {
		app.update_mock_window_mode(id, .minimize)!
		return
	}
	state := app.backend.service_minimize_window(id)!
	app.publish_admitted_native_state(id, .minimize, state, admission)!
}

// service_maximize_window requests native maximization.
pub fn (mut app App) service_maximize_window(id WindowId) ! {
	admission := app.begin_native_state_operation(id, .maximize)!
	if admission.uses_mock {
		app.update_mock_window_mode(id, .maximize)!
		return
	}
	state := app.backend.service_maximize_window(id)!
	app.publish_admitted_native_state(id, .maximize, state, admission)!
}

// service_restore_window leaves a supported minimized/maximized/fullscreen state.
pub fn (mut app App) service_restore_window(id WindowId) ! {
	admission := app.begin_native_state_operation(id, .restore)!
	if admission.uses_mock {
		app.update_mock_window_mode(id, .restore)!
		return
	}
	state := app.backend.service_restore_window(id)!
	app.publish_admitted_native_state(id, .restore, state, admission)!
}

// service_set_fullscreen requests or leaves native fullscreen state.
pub fn (mut app App) service_set_fullscreen(id WindowId, enabled bool) ! {
	admission := app.begin_native_state_operation(id, .fullscreen)!
	if !admission.uses_mock {
		state := app.backend.service_set_fullscreen(id, enabled)!
		app.publish_admitted_native_state(id, .fullscreen, state, admission)!
		return
	}
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_mock_service_locked()!
	index := app.service_window_index_for_admission_locked(id)!
	candidate := ServiceWindowState{
		...app.services.windows[index].state
		fullscreen: if enabled { .on } else { .off }
		minimized:  .off
		maximized:  .off
	}
	app.publish_state_candidate_locked(index, .fullscreen, candidate)!
}

fn (mut app App) update_mock_window_mode(id WindowId, operation ServiceOperation) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_mock_service_locked()!
	index := app.service_window_index_for_admission_locked(id)!
	candidate := ServiceWindowState{
		...app.services.windows[index].state
		minimized:  if operation == .minimize { .on } else { .off }
		maximized:  if operation == .maximize { .on } else { .off }
		fullscreen: .off
		visibility: .visible
		mapping:    .mapped
	}
	app.publish_state_candidate_locked(index, operation, candidate)!
}

fn (mut app App) publish_mock_unchanged_state(id WindowId, operation ServiceOperation) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_mock_service_locked()!
	index := app.service_window_index_for_admission_locked(id)!
	app.publish_mock_state_locked(index, operation)!
}

// service_set_mouse_lock requests or releases relative pointer confinement.
pub fn (mut app App) service_set_mouse_lock(id WindowId, enabled bool) ! {
	admission := app.begin_native_mouse_lock_operation(id, enabled)!
	if !admission.uses_mock {
		state := app.backend.service_set_mouse_lock(id, enabled)!
		app.publish_admitted_native_state(id, .mouse_lock, state, admission)!
		return
	}
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_mock_service_locked()!
	index := app.service_window_index_for_admission_locked(id)!
	candidate := ServiceWindowState{
		...app.services.windows[index].state
		mouse_locked: if enabled { .on } else { .off }
	}
	app.publish_state_candidate_locked(index, .mouse_lock, candidate)!
}

fn (mut app App) begin_native_mouse_lock_operation(id WindowId, enabled bool) !NativeStateOperationAdmission {
	if enabled {
		return app.begin_native_state_operation(id, .mouse_lock)
	}
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	app.service_window_index_for_admission_locked(id)!
	capability := app.backend.service_operation_capability(id, .mouse_lock)
	if capability.support == .unsupported {
		if !app.backend.can_release_mouse_lock_after_capability_loss(id) {
			return error(err_capability_unsupported)
		}
		return NativeStateOperationAdmission{
			publication_deferred: true
		}
	}
	uses_mock := app.backend.kind == .mock
	reserved_token := if !uses_mock && !capability.asynchronous {
		app.reserve_event_delivery_tokens_locked(1)!
	} else {
		u64(0)
	}
	return NativeStateOperationAdmission{
		uses_mock:            uses_mock
		publication_deferred: capability.asynchronous
		reserved_token:       reserved_token
	}
}

fn (mut app App) service_operation_uses_mock(id WindowId, operation ServiceOperation) !bool {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	app.service_window_index_for_admission_locked(id)!
	capability := app.backend.service_operation_capability(id, operation)
	if capability.support == .unsupported {
		return error(err_capability_unsupported)
	}
	return app.backend.kind == .mock
}

fn (mut app App) begin_native_state_operation(id WindowId, operation ServiceOperation) !NativeStateOperationAdmission {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	app.service_window_index_for_admission_locked(id)!
	capability := app.backend.service_operation_capability(id, operation)
	if capability.support == .unsupported {
		return error(err_capability_unsupported)
	}
	uses_mock := app.backend.kind == .mock
	reserved_token := if !uses_mock && !capability.asynchronous {
		app.reserve_event_delivery_tokens_locked(1)!
	} else {
		u64(0)
	}
	return NativeStateOperationAdmission{
		uses_mock:            uses_mock
		publication_deferred: capability.asynchronous
		reserved_token:       reserved_token
	}
}

fn (mut app App) publish_native_state(id WindowId, operation ServiceOperation, observed ServiceWindowState) ! {
	app.publish_reserved_native_state(id, operation, observed, 0, app.backend.service_state_publication_is_deferred(id,
		operation))!
}

fn (mut app App) publish_admitted_native_state(id WindowId, operation ServiceOperation, observed ServiceWindowState, admission NativeStateOperationAdmission) ! {
	app.publish_reserved_native_state(id, operation, observed, admission.reserved_token,
		admission.publication_deferred)!
}

fn (mut app App) publish_reserved_native_state(id WindowId, operation ServiceOperation, observed ServiceWindowState, reserved_token u64, publication_deferred bool) ! {
	if publication_deferred {
		return
	}
	if !service_window_state_has_observation(observed) {
		return
	}
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	index := app.services.window_index(id)!
	registered_observed := service_window_state_with_registered_monitor_membership(observed,
		app.services.monitors)
	if !service_window_state_has_observation(registered_observed) {
		return
	}
	candidate := merge_service_window_state(app.services.windows[index].state, registered_observed)
	if reserved_token == 0 {
		app.publish_state_candidate_locked(index, operation, candidate)!
	} else {
		app.publish_reserved_state_candidate_locked(index, operation, candidate, reserved_token)
	}
}

fn service_window_state_has_observation(state ServiceWindowState) bool {
	return state.mapping != .unknown || state.visibility != .unknown || state.active != .unknown
		|| state.focused != .unknown || state.minimized != .unknown || state.maximized != .unknown
		|| state.fullscreen != .unknown || state.mouse_locked != .unknown || state.position.known
		|| service_window_state_observes_monitor_membership(state)
}

fn merge_service_window_state(current ServiceWindowState, observed ServiceWindowState) ServiceWindowState {
	current_membership_observed := service_window_state_observes_monitor_membership(current)
	observed_membership := service_window_state_observes_monitor_membership(observed)
	return ServiceWindowState{
		mapping:                     if observed.mapping == .unknown {
			current.mapping
		} else {
			observed.mapping
		}
		visibility:                  if observed.visibility == .unknown {
			current.visibility
		} else {
			observed.visibility
		}
		active:                      if observed.active == .unknown {
			current.active
		} else {
			observed.active
		}
		focused:                     if observed.focused == .unknown {
			current.focused
		} else {
			observed.focused
		}
		minimized:                   if observed.minimized == .unknown {
			current.minimized
		} else {
			observed.minimized
		}
		maximized:                   if observed.maximized == .unknown {
			current.maximized
		} else {
			observed.maximized
		}
		fullscreen:                  if observed.fullscreen == .unknown {
			current.fullscreen
		} else {
			observed.fullscreen
		}
		mouse_locked:                if observed.mouse_locked == .unknown {
			current.mouse_locked
		} else {
			observed.mouse_locked
		}
		position:                    if observed.position.known {
			observed.position
		} else {
			current.position
		}
		monitor_ids:                 if observed_membership {
			observed.monitor_ids.clone()
		} else {
			current.monitor_ids.clone()
		}
		sequence:                    current.sequence
		monitor_membership_observed: current_membership_observed || observed_membership
	}
}

fn service_window_state_observation_equal(left ServiceWindowState, right ServiceWindowState) bool {
	if left.mapping != right.mapping || left.visibility != right.visibility
		|| left.active != right.active || left.focused != right.focused
		|| left.minimized != right.minimized || left.maximized != right.maximized
		|| left.fullscreen != right.fullscreen || left.mouse_locked != right.mouse_locked
		|| left.position != right.position
		|| service_window_state_observes_monitor_membership(left) != service_window_state_observes_monitor_membership(right)
		|| left.monitor_ids.len != right.monitor_ids.len {
		return false
	}
	for index, monitor in left.monitor_ids {
		if monitor != right.monitor_ids[index] {
			return false
		}
	}
	return true
}

// service_set_titlebar_appearance requests a supported native titlebar theme.
pub fn (mut app App) service_set_titlebar_appearance(id WindowId, appearance ServiceTitlebarAppearance) ! {
	if !app.service_operation_uses_mock(id, .titlebar_appearance)! {
		app.backend.service_set_titlebar_appearance(id, appearance)!
		return
	}
	app.publish_mock_unchanged_state(id, .titlebar_appearance)!
}

// service_request_clipboard_text starts an asynchronous clipboard read and
// returns the id matched by a terminal clipboard ServiceEvent.
pub fn (mut app App) service_request_clipboard_text(id WindowId) !ServiceRequestId {
	if app.service_operation_uses_mock(id, .clipboard_read)! {
		return app.complete_mock_clipboard(id, false, '')!
	}
	admission := app.begin_native_clipboard_request(id, .clipboard_read, native_clipboard_requires_reserved_terminal(app.backend.kind,
		.clipboard_read))!
	request := admission.request
	start := app.backend.service_request_clipboard_text(id, request) or {
		app.rollback_native_service_request(request, admission.reserved_terminal)
		return err
	}
	if start.completed {
		app.complete_native_clipboard_start(request, id, .clipboard_read, start,
			admission.reserved_terminal) or {
			app.rollback_native_service_request(request, admission.reserved_terminal)
			return err
		}
	}
	return request
}

// service_set_clipboard_text starts an asynchronous clipboard write and returns
// the id matched by a terminal clipboard ServiceEvent.
pub fn (mut app App) service_set_clipboard_text(id WindowId, text string) !ServiceRequestId {
	if app.service_operation_uses_mock(id, .clipboard_write)! {
		return app.complete_mock_clipboard(id, true, text)!
	}
	admission := app.begin_native_clipboard_request_with_payload(id, .clipboard_write, native_clipboard_requires_reserved_terminal(app.backend.kind,
		.clipboard_write), u64(text.len))!
	request := admission.request
	start := app.backend.service_set_clipboard_text(id, request, text) or {
		app.rollback_native_service_request(request, admission.reserved_terminal)
		return err
	}
	if start.completed {
		app.complete_native_clipboard_start(request, id, .clipboard_write, start,
			admission.reserved_terminal) or {
			app.rollback_native_service_request(request, admission.reserved_terminal)
			return err
		}
	}
	return request
}

struct NativeClipboardRequestAdmission {
	request           ServiceRequestId
	reserved_terminal u64
}

fn native_clipboard_requires_reserved_terminal(backend BackendKind, operation ServiceOperation) bool {
	return match backend {
		.x11, .wayland { operation == .clipboard_write }
		.appkit { operation in [.clipboard_read, .clipboard_write] }
		else { false }
	}
}

fn (mut app App) complete_mock_clipboard(id WindowId, write bool, text string) !ServiceRequestId {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_mock_service_locked()!
	app.service_window_index_for_admission_locked(id)!
	if app.services.next_request == 0 {
		return error(err_service_request_exhausted)
	}
	if app.services.clipboard_pending_count() >= service_clipboard_pending_capacity {
		return error(err_clipboard_capacity)
	}
	result_text := if write { text } else { app.services.clipboard_text }
	payload_bytes := u64(result_text.len)
	if !payload_resize_fits(app.services.clipboard_payload_bytes, 0, payload_bytes,
		service_clipboard_payload_capacity) {
		return error(err_clipboard_capacity)
	}
	token := app.reserve_event_delivery_tokens_locked(1)!
	request := app.services.take_request_id()!
	if write {
		app.services.clipboard_text = text.clone()
	}
	result := ServiceClipboardResult{
		id:     request
		window: id
		status: .ready
		text:   result_text.clone()
	}
	app.services.pending << PendingServiceRequest{
		id:            request
		window:        id
		kind:          if write { .clipboard_write } else { .clipboard_read }
		terminal:      true
		payload_bytes: payload_bytes
	}
	app.services.clipboard_payload_bytes += payload_bytes
	app.enqueue_reserved_service_event_locked(ServiceEvent{
		kind:      .clipboard
		window:    id
		operation: if write { .clipboard_write } else { .clipboard_read }
		clipboard: result
	}, token)
	return request
}

fn (mut app App) begin_native_clipboard_request(id WindowId, kind PendingServiceKind, reserve_terminal bool) !NativeClipboardRequestAdmission {
	return app.begin_native_clipboard_request_with_payload(id, kind, reserve_terminal, 0)!
}

fn (mut app App) begin_native_clipboard_request_with_payload(id WindowId, kind PendingServiceKind, reserve_terminal bool, payload_bytes u64) !NativeClipboardRequestAdmission {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	app.service_window_index_for_admission_locked(id)!
	if app.services.next_request == 0 {
		return error(err_service_request_exhausted)
	}
	if app.services.clipboard_pending_count() >= service_clipboard_pending_capacity {
		return error(err_clipboard_capacity)
	}
	if !payload_resize_fits(app.services.clipboard_payload_bytes, 0, payload_bytes,
		service_clipboard_payload_capacity) {
		return error(err_clipboard_capacity)
	}
	reserved_terminal := if reserve_terminal {
		app.reserve_event_delivery_tokens_locked(1)!
	} else {
		u64(0)
	}
	request := app.services.take_request_id()!
	app.services.pending << PendingServiceRequest{
		id:            request
		window:        id
		kind:          kind
		payload_bytes: payload_bytes
	}
	app.services.clipboard_payload_bytes += payload_bytes
	return NativeClipboardRequestAdmission{
		request:           request
		reserved_terminal: reserved_terminal
	}
}

fn (mut app App) rollback_native_service_request(id ServiceRequestId, reserved_terminal u64) {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	mut removed := false
	for index, request in app.services.pending {
		if request.id == id && !request.terminal {
			app.services.release_pending_service_payload(index)
			app.services.pending.delete(index)
			removed = true
			break
		}
	}
	if reserved_terminal == 0 {
		return
	}
	if removed && reserved_terminal !in app.event_deliveries
		&& app.last_reserved_delivery_token_locked() == reserved_terminal {
		app.next_event_delivery_token = reserved_terminal
		return
	}
	app.defer_poll_error_locked(app.last_reserved_delivery_token_locked(), err_event_delivery_stale)
}

fn (mut app App) complete_native_clipboard_request(id ServiceRequestId, window WindowId, operation ServiceOperation, text string, reserved_terminal u64) ! {
	app.complete_native_clipboard_terminal(id, window, operation, .ready, text, '',
		reserved_terminal)!
}

fn (mut app App) complete_native_clipboard_start(id ServiceRequestId, window WindowId, operation ServiceOperation, start BackendClipboardStart, reserved_terminal u64) ! {
	status := if start.status == .failed { ServiceStatus.failed } else { ServiceStatus.ready }
	error_text := if status == .failed && start.error == '' {
		err_capability_unsupported
	} else {
		start.error
	}
	app.complete_native_clipboard_terminal(id, window, operation, status, start.text, error_text,
		reserved_terminal)!
}

fn (mut app App) complete_native_clipboard_terminal(id ServiceRequestId, window WindowId, operation ServiceOperation, status ServiceStatus, text string, error_text string, reserved_terminal u64) ! {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	mut matched_index := -1
	for index, request in app.services.pending {
		if request.id != id || request.window != window || request.terminal {
			continue
		}
		matched_index = index
		break
	}
	if matched_index < 0 {
		return error(err_service_request_stale)
	}
	if status == .ready {
		if operation == .clipboard_read {
			if !app.services.resize_pending_service_payload(matched_index, u64(text.len)) {
				return error(err_clipboard_capacity)
			}
		} else if operation == .clipboard_write
			&& u64(text.len) > app.services.pending[matched_index].payload_bytes {
			return error(err_clipboard_capacity)
		}
	} else {
		app.services.resize_pending_service_payload(matched_index, 0)
	}
	token := if reserved_terminal != 0 {
		reserved_terminal
	} else {
		app.reserve_event_delivery_tokens_locked(1)!
	}
	app.services.pending[matched_index].terminal = true
	terminal_text := if status == .ready { text.clone() } else { '' }
	app.enqueue_reserved_service_event_locked(ServiceEvent{
		kind:      .clipboard
		window:    window
		operation: operation
		clipboard: ServiceClipboardResult{
			id:     id
			window: window
			status: status
			text:   terminal_text
			error:  error_text
		}
	}, token)
}

// service_request_portal_parent starts an asynchronous native-parent export. A
// ready event carries an opaque identifier and an explicitly released lease.
pub fn (mut app App) service_request_portal_parent(id WindowId) !ServiceRequestId {
	if app.service_operation_uses_mock(id, .portal_parent)! {
		return app.complete_mock_portal_parent(id)!
	}
	request, lease := app.begin_portal_parent_request(id)!
	start := app.backend.service_start_portal_parent(id, request, lease) or {
		app.rollback_portal_parent_request(request, lease)
		return err
	}
	if start.completed {
		app.complete_portal_parent_request(request, id, lease, start.identifier) or {
			app.rollback_portal_parent_request(request, lease)
			return err
		}
	}
	return request
}

fn (mut app App) complete_mock_portal_parent(id WindowId) !ServiceRequestId {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_mock_service_locked()!
	app.service_window_index_for_admission_locked(id)!
	if app.services.portal_leases.len >= service_portal_lease_capacity {
		return error(err_portal_capacity)
	}
	if app.services.next_request == 0 {
		return error(err_service_request_exhausted)
	}
	token := app.reserve_event_delivery_tokens_locked(1)!
	request := app.services.take_request_id()!
	lease := ServicePortalLeaseId{
		app_instance: app.instance_id
		serial:       request.serial
	}
	app.services.portal_leases << ServicePortalLease{
		id:     lease
		window: id
	}
	app.services.pending << PendingServiceRequest{
		id:       request
		window:   id
		kind:     .portal_parent
		terminal: true
	}
	app.enqueue_reserved_service_event_locked(ServiceEvent{
		kind:          .portal_parent
		window:        id
		operation:     .portal_parent
		portal_parent: ServicePortalParentResult{
			id:         request
			window:     id
			status:     .ready
			lease:      lease
			identifier: 'mock:${id.str()}'
		}
	}, token)
	return request
}

fn (mut app App) begin_portal_parent_request(id WindowId) !(ServiceRequestId, ServicePortalLeaseId) {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	app.service_window_index_for_admission_locked(id)!
	if app.services.portal_leases.len >= service_portal_lease_capacity {
		return error(err_portal_capacity)
	}
	request := app.services.take_request_id()!
	lease := ServicePortalLeaseId{
		app_instance: app.instance_id
		serial:       request.serial
	}
	app.services.portal_leases << ServicePortalLease{
		id:     lease
		window: id
	}
	app.services.pending << PendingServiceRequest{
		id:     request
		window: id
		kind:   .portal_parent
	}
	return request, lease
}

fn (mut app App) complete_portal_parent_request(request ServiceRequestId, id WindowId, lease ServicePortalLeaseId, identifier string) ! {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	mut matched_index := -1
	for index, pending in app.services.pending {
		if pending.id == request && pending.window == id && pending.kind == .portal_parent
			&& !pending.terminal {
			matched_index = index
			break
		}
	}
	if matched_index < 0 {
		return error(err_service_request_stale)
	}
	token := app.reserve_event_delivery_tokens_locked(1)!
	app.services.pending[matched_index].terminal = true
	app.enqueue_reserved_service_event_locked(ServiceEvent{
		kind:          .portal_parent
		window:        id
		operation:     .portal_parent
		portal_parent: ServicePortalParentResult{
			id:         request
			window:     id
			status:     .ready
			lease:      lease
			identifier: identifier
		}
	}, token)
}

fn (mut app App) rollback_portal_parent_request(request ServiceRequestId, lease ServicePortalLeaseId) {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	for index, pending in app.services.pending {
		if pending.id == request && !pending.terminal {
			app.services.pending.delete(index)
			break
		}
	}
	for index, current in app.services.portal_leases {
		if current.id == lease {
			app.services.portal_leases.delete(index)
			return
		}
	}
}

// service_release_portal_parent releases a ready portal-parent export lease.
pub fn (mut app App) service_release_portal_parent(id ServicePortalLeaseId) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	app.ensure_running_locked() or {
		app.state_mutex.unlock()
		return err
	}
	if id.app_instance != app.instance_id {
		app.state_mutex.unlock()
		return error(err_app_identity_mismatch)
	}
	mut found := false
	for lease in app.services.portal_leases {
		if lease.id == id {
			found = true
			break
		}
	}
	app.state_mutex.unlock()
	if !found {
		return error(err_service_request_stale)
	}
	if app.backend.kind != .mock {
		app.backend.service_release_portal_parent(id)!
	}
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	for index, lease in app.services.portal_leases {
		if lease.id == id {
			app.services.portal_leases.delete(index)
			return
		}
	}
	return error(err_service_request_stale)
}

// service_request_window_readback requests an origin-based native readback of
// the supplied width and height and queues one terminal ServiceReadbackResult.
pub fn (mut app App) service_request_window_readback(id WindowId, width int, height int, submitted_frame u64) !ServiceReadbackId {
	return app.service_request_window_readback_region(id, 0, 0, width, height, submitted_frame)!
}

// service_begin_window_readback reserves a pending low-level readback identity.
pub fn (mut app App) service_begin_window_readback(id WindowId) !ServiceReadbackId {
	return app.begin_window_readback_with_payload(id, 0)!
}

// service_begin_window_readback_with_payload_for_gg reserves the tight RGBA8
// payload before the gg bridge allocates or stages native pixel storage.
pub fn (mut app App) service_begin_window_readback_with_payload_for_gg(id WindowId, width int, height int) !ServiceReadbackId {
	payload_bytes := service_readback_tight_payload_bytes(width, height)!
	return app.begin_window_readback_with_payload(id, payload_bytes)!
}

fn (mut app App) begin_window_readback_with_payload(id WindowId, payload_bytes u64) !ServiceReadbackId {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	app.service_window_index_for_admission_locked(id)!
	if app.services.readbacks.len >= service_readback_pending_capacity {
		return error(err_readback_capacity)
	}
	if !payload_resize_fits(app.services.readback_payload_bytes, 0, payload_bytes,
		service_readback_payload_capacity) {
		return error(err_readback_capacity)
	}
	readback := app.services.take_readback_id(id)!
	app.services.readbacks << PendingReadbackRequest{
		id:            readback
		payload_bytes: payload_bytes
	}
	app.services.readback_payload_bytes += payload_bytes
	return readback
}

fn service_readback_tight_payload_bytes(width int, height int) !u64 {
	if width <= 0 || height <= 0 {
		return error(err_readback_invalid)
	}
	width_u64 := u64(width)
	height_u64 := u64(height)
	if width_u64 > ~u64(0) / u64(4) {
		return error(err_readback_invalid)
	}
	row_bytes := width_u64 * u64(4)
	if height_u64 > ~u64(0) / row_bytes {
		return error(err_readback_invalid)
	}
	return row_bytes * height_u64
}

// service_rollback_window_readback_for_gg removes a pre-allocation reservation
// when the gg producer fails before any terminal result can own storage.
pub fn (mut app App) service_rollback_window_readback_for_gg(readback ServiceReadbackId) {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	for index, pending in app.services.readbacks {
		if pending.id == readback && !pending.terminal {
			app.services.release_pending_readback_payload(index)
			app.services.readbacks.delete(index)
			return
		}
	}
}

fn (app &App) pending_readback_index_locked(readback ServiceReadbackId) !int {
	if readback.app_instance != app.instance_id || readback.window.app_instance != app.instance_id {
		return error(err_app_identity_mismatch)
	}
	app.services.window_index(readback.window)!
	for index, pending in app.services.readbacks {
		if pending.id == readback {
			if pending.terminal {
				return error(err_service_request_stale)
			}
			return index
		}
	}
	return error(err_service_request_stale)
}

// service_stage_window_readback_for_gg is an internal gg-facade bridge; not user API.
pub fn (mut app App) service_stage_window_readback_for_gg(readback ServiceReadbackId, x int, y int, width int, height int, producing_frame u64) ! {
	app.assert_owner_thread()!
	if x < 0 || y < 0 || width <= 0 || height <= 0 || producing_frame == 0 {
		return error(err_readback_invalid)
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
	index := app.pending_readback_index_locked(readback) or {
		app.state_mutex.unlock()
		return err
	}
	payload_bytes := service_readback_tight_payload_bytes(width, height) or {
		app.state_mutex.unlock()
		return err
	}
	if !app.services.resize_pending_readback_payload(index, payload_bytes) {
		app.state_mutex.unlock()
		return error(err_readback_capacity)
	}
	app.state_mutex.unlock()
	app.backend.service_stage_window_readback(readback, x, y, width, height, producing_frame) or {
		app.state_mutex.lock()
		for pending_index, pending in app.services.readbacks {
			if pending.id == readback && !pending.terminal {
				app.services.release_pending_readback_payload(pending_index)
				break
			}
		}
		app.state_mutex.unlock()
		return err
	}
}

// service_stage_image_readback_for_gg is an internal gg-facade bridge; not user API.
pub fn (mut app App) service_stage_image_readback_for_gg(readback ServiceReadbackId, image_id u32, x int, y int, width int, height int, producing_frame u64) ! {
	app.assert_owner_thread()!
	if image_id == 0 || x < 0 || y < 0 || width <= 0 || height <= 0 || producing_frame == 0 {
		return error(err_readback_invalid)
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
	index := app.pending_readback_index_locked(readback) or {
		app.state_mutex.unlock()
		return err
	}
	payload_bytes := service_readback_tight_payload_bytes(width, height) or {
		app.state_mutex.unlock()
		return err
	}
	if !app.services.resize_pending_readback_payload(index, payload_bytes) {
		app.state_mutex.unlock()
		return error(err_readback_capacity)
	}
	app.state_mutex.unlock()
	app.backend.service_stage_image_readback(readback, image_id, x, y, width, height,
		producing_frame) or {
		app.state_mutex.lock()
		for pending_index, pending in app.services.readbacks {
			if pending.id == readback && !pending.terminal {
				app.services.release_pending_readback_payload(pending_index)
				break
			}
		}
		app.state_mutex.unlock()
		return err
	}
}

// service_arm_image_readback_pass_for_gg is an internal gg-facade bridge; not user API.
pub fn (mut app App) service_arm_image_readback_pass_for_gg(id WindowId, image_id u32, pass_serial u64, producing_frame u64) ! {
	app.assert_owner_thread()!
	if image_id == 0 || pass_serial == 0 || producing_frame == 0 {
		return error(err_readback_invalid)
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
	app.service_window_index_for_admission_locked(id) or {
		app.state_mutex.unlock()
		return err
	}
	app.state_mutex.unlock()
	app.backend.service_arm_image_readback_pass(id, image_id, pass_serial, producing_frame)!
}

// service_resolve_readbacks_after_submit_for_gg is an internal gg-facade bridge; not user API.
pub fn (mut app App) service_resolve_readbacks_after_submit_for_gg(id WindowId, submitted_frame u64, submission_succeeded bool) ! {
	app.assert_owner_thread()!
	if submitted_frame == 0 {
		return error(err_readback_invalid)
	}
	app.state_mutex.lock()
	app.ensure_running_locked() or {
		app.state_mutex.unlock()
		return err
	}
	app.services.window_index(id) or {
		app.state_mutex.unlock()
		return err
	}
	app.state_mutex.unlock()
	app.backend.service_resolve_readbacks_after_submit(id, submitted_frame, submission_succeeded)!
}

// service_abandon_window_readback_for_gg is an internal gg-facade bridge; not user API.
pub fn (mut app App) service_abandon_window_readback_for_gg(readback ServiceReadbackId, message string) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	app.ensure_running_locked() or {
		app.state_mutex.unlock()
		return err
	}
	app.pending_readback_index_locked(readback) or {
		app.state_mutex.unlock()
		return err
	}
	app.state_mutex.unlock()
	mut cancel_error := ''
	app.backend.service_cancel_readback(readback) or { cancel_error = err.msg() }
	app.service_fail_window_readback(readback, message)!
	if cancel_error != '' {
		return error(cancel_error)
	}
}

// service_finish_window_readback publishes a ready owned RGBA8 terminal result.
pub fn (mut app App) service_finish_window_readback(readback ServiceReadbackId, width int, height int, stride int, pixels []u8, submitted_frame u64) ! {
	validate_service_readback_rgba8_layout(width, height, stride, pixels.len)!
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	index := app.pending_readback_index_locked(readback)!
	actual_bytes := u64(pixels.len)
	reserved_bytes := app.services.readbacks[index].payload_bytes
	if reserved_bytes > 0 && actual_bytes > reserved_bytes {
		app.services.release_pending_readback_payload(index)
		app.finish_pending_window_readback_locked(readback, ServiceReadbackResult{
			id:     readback
			window: readback.window
			status: .failed
			error:  err_readback_invalid
		})!
		return
	}
	if !app.services.resize_pending_readback_payload(index, actual_bytes) {
		return error(err_readback_capacity)
	}
	app.finish_pending_window_readback_locked(readback, ServiceReadbackResult{
		id:              readback
		window:          readback.window
		status:          .ready
		submitted_frame: submitted_frame
		width:           width
		height:          height
		stride:          stride
		pixels_rgba8:    pixels.clone()
	})!
}

fn validate_service_readback_rgba8_layout(width int, height int, stride int, pixels_len int) ! {
	if width <= 0 || height <= 0 || stride <= 0 || pixels_len < 0 {
		return error(err_readback_invalid)
	}
	width_u64 := u64(width)
	height_u64 := u64(height)
	stride_u64 := u64(stride)
	if width_u64 > ~u64(0) / u64(4) {
		return error(err_readback_invalid)
	}
	row_bytes := width_u64 * u64(4)
	if row_bytes > stride_u64 || stride_u64 > ~u64(0) / height_u64
		|| stride_u64 * height_u64 != u64(pixels_len) {
		return error(err_readback_invalid)
	}
}

// service_fail_window_readback publishes a failed terminal result without pixels.
pub fn (mut app App) service_fail_window_readback(readback ServiceReadbackId, message string) ! {
	app.finish_pending_window_readback(readback, ServiceReadbackResult{
		id:     readback
		window: readback.window
		status: .failed
		error:  message
	})!
}

fn (mut app App) finish_pending_window_readback(readback ServiceReadbackId, result ServiceReadbackResult) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	app.finish_pending_window_readback_locked(readback, result)!
}

fn (mut app App) finish_pending_window_readback_locked(readback ServiceReadbackId, result ServiceReadbackResult) ! {
	index := app.pending_readback_index_locked(readback)!
	if result.status != .ready {
		app.services.release_pending_readback_payload(index)
	}
	app.enqueue_readback_event_locked(result)!
	app.services.readbacks[index].terminal = true
}

fn (mut app App) mark_pending_window_readback_terminal_locked(readback ServiceReadbackId) !int {
	index := app.pending_readback_index_locked(readback)!
	app.services.readbacks[index].terminal = true
	return index
}

// service_request_window_readback_region requests a positive bounded native
// pixel region and queues exactly one terminal result.
pub fn (mut app App) service_request_window_readback_region(id WindowId, x int, y int, width int, height int, submitted_frame u64) !ServiceReadbackId {
	if x < 0 || y < 0 || width <= 0 || height <= 0 || u64(width) * u64(height) > u64(0x1fffffff) {
		return error(err_readback_invalid)
	}
	stride := width * 4
	uses_mock := app.service_operation_uses_mock(id, .window_capture)!
	if !uses_mock {
		app.backend.service_window_readback_preflight(id, x, y, width, height)!
	}
	readback := app.begin_window_readback_with_payload(id, service_readback_tight_payload_bytes(width,
		height)!)!
	pixels := if uses_mock {
		[]u8{len: stride * height}
	} else {
		app.backend.service_window_readback(id, x, y, width, height) or {
			app.service_rollback_window_readback_for_gg(readback)
			return err
		}
	}
	app.service_finish_window_readback(readback, width, height, stride, pixels, submitted_frame) or {
		app.service_rollback_window_readback_for_gg(readback)
		return err
	}
	return readback
}

// service_complete_readback creates and immediately publishes one ready RGBA8 result.
pub fn (mut app App) service_complete_readback(id WindowId, width int, height int, stride int, pixels []u8, submitted_frame u64) !ServiceReadbackId {
	validate_service_readback_rgba8_layout(width, height, stride, pixels.len)!
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
	app.service_window_index_for_admission_locked(id)!
	if app.services.next_request == 0 {
		return error(err_service_request_exhausted)
	}
	if app.services.readbacks.len >= service_readback_pending_capacity {
		return error(err_readback_capacity)
	}
	payload_bytes := u64(pixels.len)
	if !payload_resize_fits(app.services.readback_payload_bytes, 0, payload_bytes,
		service_readback_payload_capacity) {
		return error(err_readback_capacity)
	}
	token := app.reserve_event_delivery_tokens_locked(1)!
	readback := app.services.take_readback_id(id)!
	app.services.readbacks << PendingReadbackRequest{
		id:            readback
		terminal:      true
		payload_bytes: payload_bytes
	}
	app.services.readback_payload_bytes += payload_bytes
	app.enqueue_reserved_event_locked(queued_readback_event(ServiceReadbackResult{
		id:              readback
		window:          id
		status:          .ready
		submitted_frame: submitted_frame
		width:           width
		height:          height
		stride:          stride
		pixels_rgba8:    pixels.clone()
	}), token)
	return readback
}

// with_native_window_for_gg is an internal gg-facade bridge; not user API.
pub fn (mut app App) with_native_window_for_gg(id WindowId, callback NativeWindowBorrowCallback) ! {
	app.assert_owner_thread()!
	if callback == unsafe { nil } {
		return error(err_native_borrow_nil_callback)
	}
	app.state_mutex.lock()
	app.ensure_running_locked() or {
		app.state_mutex.unlock()
		return err
	}
	app.service_window_index_for_admission_locked(id) or {
		app.state_mutex.unlock()
		return err
	}
	capability := app.backend.service_operation_capability(id, .native_borrow)
	app.state_mutex.unlock()
	if capability.support == .unsupported {
		return error(err_capability_unsupported)
	}
	borrow := app.backend.service_native_window_borrow(id)!
	app.with_native_window_borrow(id, borrow.backend, borrow.primary, borrow.secondary, callback)!
}

fn (mut app App) with_native_window_borrow_for_test(id WindowId, callback NativeWindowBorrowCallback) ! {
	app.with_native_window_borrow(id, .mock, unsafe { nil }, 0, callback)!
}

$if test {
	// with_mock_native_window_borrow_for_gg_test is an internal gg-facade bridge; not user API.
	// It gives same-module tests a real core borrow lifetime only in test builds.
	pub fn (mut app App) with_mock_native_window_borrow_for_gg_test(id WindowId, callback NativeWindowBorrowCallback) ! {
		app.with_native_window_borrow_for_test(id, callback)!
	}
}

fn (mut app App) with_native_window_borrow(id WindowId, backend NativeWindowBackend, primary voidptr, secondary u64, callback NativeWindowBorrowCallback) ! {
	app.assert_owner_thread()!
	if callback == unsafe { nil } {
		return error(err_native_borrow_nil_callback)
	}
	app.state_mutex.lock()
	app.ensure_running_locked() or {
		app.state_mutex.unlock()
		return err
	}
	index := app.service_window_index_for_admission_locked(id) or {
		app.state_mutex.unlock()
		return err
	}
	epoch := app.services.take_borrow_epoch() or {
		app.state_mutex.unlock()
		return err
	}
	app.services.windows[index].borrow_epochs << epoch
	app.native_borrow_depth++
	borrow := NativeWindowBorrow{
		app_instance: app.instance_id
		window:       id
		epoch:        epoch
		backend:      backend
		primary:      primary
		secondary:    secondary
	}
	app.state_mutex.unlock()

	mut callback_error := IError(none)
	callback(borrow) or { callback_error = err }

	app.state_mutex.lock()
	if current_index := app.services.window_index(id) {
		for epoch_index, active_epoch in app.services.windows[current_index].borrow_epochs {
			if active_epoch == epoch {
				app.services.windows[current_index].borrow_epochs.delete(epoch_index)
				break
			}
		}
	}
	if app.native_borrow_depth > 0 {
		app.native_borrow_depth--
	}
	flush := app.native_borrow_depth == 0
	app.state_mutex.unlock()
	if flush {
		app.flush_deferred_native_transitions()!
	}
	if callback_error !is none {
		return callback_error
	}
}

// validate_native_borrow_for_gg is an internal gg-facade bridge; not user API.
pub fn (app &App) validate_native_borrow_for_gg(id WindowId, epoch u64) !NativeWindowBackend {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	index := app.services.window_index(id)!
	record := app.services.windows[index]
	if epoch == 0 || epoch !in record.borrow_epochs {
		return error(err_native_borrow_stale)
	}
	return native_window_backend_for_kind(app.backend.kind)
}

fn native_window_backend_for_kind(kind BackendKind) NativeWindowBackend {
	return match kind {
		.x11 { .x11 }
		.wayland { .wayland }
		.appkit { .appkit }
		.win32 { .win32 }
		else { .mock }
	}
}

fn (mut app App) defer_native_destroy(id WindowId) !bool {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if app.native_borrow_depth == 0 {
		return false
	}
	app.services.window_index(id)!
	if id !in app.deferred_native_windows {
		app.deferred_native_windows << id
	}
	return true
}

fn (mut app App) defer_native_stop() bool {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if app.native_borrow_depth == 0 {
		return false
	}
	app.deferred_native_stop = true
	return true
}

fn (mut app App) flush_deferred_native_transitions() ! {
	app.state_mutex.lock()
	if app.native_borrow_depth != 0 {
		app.state_mutex.unlock()
		return
	}
	stop := app.deferred_native_stop
	windows := app.deferred_native_windows.clone()
	app.deferred_native_stop = false
	app.deferred_native_windows.clear()
	app.state_mutex.unlock()
	if stop {
		app.stop()!
		return
	}
	for id in windows {
		if app.window_exists(id) {
			app.destroy_window(id)!
		}
	}
}

struct WindowServiceCancellationPlan {
	window           WindowId
	service_indices  []int
	readback_indices []int
	service_events   []ServiceEvent
	readback_events  []ServiceReadbackResult
	first_token      u64
}

fn (mut app App) prepare_window_service_cancellation_locked(id WindowId) !WindowServiceCancellationPlan {
	plan := app.collect_window_service_cancellation_locked(id)!
	first := app.reserve_event_delivery_tokens_locked(plan.service_events.len +
		plan.readback_events.len)!
	return WindowServiceCancellationPlan{
		...plan
		first_token: first
	}
}

fn (mut app App) collect_window_service_cancellation_locked(id WindowId) !WindowServiceCancellationPlan {
	app.services.window_index(id)!
	return app.collect_present_window_service_cancellation_locked(id)
}

// The caller has already proved the service record. Keeping this collector
// infallible preserves native batch admission after earlier events mutate state.
fn (mut app App) collect_present_window_service_cancellation_locked(id WindowId) WindowServiceCancellationPlan {
	mut service_indices := []int{}
	mut service_events := []ServiceEvent{}
	for i, request in app.services.pending {
		if request.window != id || request.terminal {
			continue
		}
		service_indices << i
		if request.kind == .portal_parent {
			service_events << ServiceEvent{
				kind:          .portal_parent
				window:        id
				operation:     .portal_parent
				portal_parent: ServicePortalParentResult{
					id:     request.id
					window: id
					status: .cancelled
				}
			}
		} else {
			service_events << ServiceEvent{
				kind:      .clipboard
				window:    id
				operation: if request.kind == .clipboard_write {
					.clipboard_write
				} else {
					.clipboard_read
				}
				clipboard: ServiceClipboardResult{
					id:     request.id
					window: id
					status: .cancelled
				}
			}
		}
	}
	mut readback_indices := []int{}
	mut readback_events := []ServiceReadbackResult{}
	for i, request in app.services.readbacks {
		if request.id.window != id || request.terminal {
			continue
		}
		readback_indices << i
		readback_events << ServiceReadbackResult{
			id:     request.id
			window: id
			status: .cancelled
		}
	}
	return WindowServiceCancellationPlan{
		window:           id
		service_indices:  service_indices
		readback_indices: readback_indices
		service_events:   service_events
		readback_events:  readback_events
	}
}

fn (mut app App) commit_window_service_cancellation_locked(plan WindowServiceCancellationPlan) {
	for index in plan.service_indices {
		app.services.release_pending_service_payload(index)
		app.services.pending[index].terminal = true
	}
	for index in plan.readback_indices {
		app.services.release_pending_readback_payload(index)
		app.services.readbacks[index].terminal = true
	}
	mut offset := 0
	for event in plan.service_events {
		token := plan.first_token + u64(offset)
		app.enqueue_reserved_event_locked(queued_service_event(service_event_with_sequence(event,
			token)), token)
		offset++
	}
	for event in plan.readback_events {
		token := plan.first_token + u64(offset)
		app.enqueue_reserved_event_locked(queued_readback_event(event), token)
		offset++
	}
	mut retained_leases := []ServicePortalLease{cap: app.services.portal_leases.len}
	for lease in app.services.portal_leases {
		if lease.window != plan.window {
			retained_leases << lease
		}
	}
	app.services.portal_leases = retained_leases
}

// drain_service_events consumes only the contiguous service prefix of the
// canonical queue. It never skips lifecycle, input, or readback events.
pub fn (mut app App) drain_service_events() ![]ServiceEvent {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if app.event_dispatch_active {
		return error(err_event_dispatch_active)
	}
	mut selected := []ServiceEvent{}
	mut delivered := []QueuedEvent{}
	for event in app.events {
		if app.queued_event_blocked_by_teardown_locked(event) || event.kind != .service {
			break
		}
		selected << event.service
		delivered << event
	}
	for event in delivered {
		app.validate_queued_delivery_locked(event)!
		app.complete_queued_delivery_locked(event)
	}
	app.events = app.events[delivered.len..].clone()
	app.release_terminal_delivery_storage_locked()
	return selected
}

// drain_readback_events consumes only the contiguous readback prefix of the
// canonical queue. It never skips lifecycle, input, or service events.
pub fn (mut app App) drain_readback_events() ![]ServiceReadbackResult {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if app.event_dispatch_active {
		return error(err_event_dispatch_active)
	}
	mut selected := []ServiceReadbackResult{}
	mut delivered := []QueuedEvent{}
	for event in app.events {
		if app.queued_event_blocked_by_teardown_locked(event) || event.kind != .readback {
			break
		}
		selected << event.readback
		delivered << event
	}
	for event in delivered {
		app.validate_queued_delivery_locked(event)!
		app.complete_queued_delivery_locked(event)
	}
	app.events = app.events[delivered.len..].clone()
	app.release_terminal_delivery_storage_locked()
	return selected
}
