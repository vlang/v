module multiwindow

struct BackendNativeWindowBorrow {
	backend   NativeWindowBackend
	primary   voidptr
	secondary u64
}

struct BackendClipboardStart {
	completed bool
	status    ServiceStatus
	text      string
	error     string
}

struct BackendPortalStart {
	completed  bool
	identifier string
}

struct BackendReadbackResult {
	id              ServiceReadbackId
	status          ServiceReadbackStatus
	submitted_frame u64
	width           int
	height          int
	stride          int
	pixels_rgba8    []u8
	error           string
}

fn (mut backend Backend) claim_service_event_storage(event ServiceEvent) {
	if backend.kind == .win32 {
		backend.win32.claim_clipboard_terminal_storage(event)
	} else if backend.kind == .x11 {
		backend.x11.claim_clipboard_terminal_storage(event)
	}
}

fn (mut backend Backend) discard_unaccepted_service_event_storage(event ServiceEvent) {
	if backend.kind == .win32 {
		backend.win32.discard_unclaimed_clipboard_terminal_storage(event)
	} else if backend.kind == .x11 {
		backend.x11.discard_unclaimed_clipboard_terminal_storage(event)
	}
}

fn (mut backend Backend) release_delivered_service_event_storage(event ServiceEvent) {
	if backend.kind == .win32 {
		backend.win32.release_claimed_clipboard_terminal_storage(event)
	} else if backend.kind == .x11 {
		backend.x11.release_claimed_clipboard_terminal_storage(event)
	}
}

fn (backend &Backend) can_release_mouse_lock_after_capability_loss(id WindowId) bool {
	return backend.kind == .wayland
		&& backend.wayland.can_release_mouse_lock_after_capability_loss(id)
}

fn (backend &Backend) service_operation_capability(id WindowId, operation ServiceOperation) ServiceOperationCapability {
	return match backend.kind {
		.auto {
			ServiceOperationCapability{}
		}
		.mock {
			mock_service_operation_capability(operation)
		}
		.x11 {
			backend.x11.service_operation_capability(id, operation)
		}
		.wayland {
			backend.wayland.service_operation_capability(operation)
		}
		.appkit {
			backend.appkit.service_operation_capability(id, operation)
		}
		.win32 {
			backend.win32.service_operation_capability(id, operation)
		}
	}
}

fn (backend &Backend) service_window_state(id WindowId) !ServiceWindowState {
	return match backend.kind {
		.win32 { backend.win32.service_window_state(id)! }
		else { error(err_capability_unsupported) }
	}
}

fn (backend &Backend) service_state_publication_is_deferred(id WindowId, operation ServiceOperation) bool {
	return backend.service_operation_capability(id, operation).asynchronous
}

fn mock_service_operation_capability(operation ServiceOperation) ServiceOperationCapability {
	if operation in [.native_borrow, .image_readback] {
		return ServiceOperationCapability{}
	}
	return ServiceOperationCapability{
		support:          .available
		asynchronous:     operation in [.clipboard_read, .clipboard_write, .portal_parent,
			.window_capture]
		state_observable: operation in [.show, .hide, .focus, .raise, .position, .minimize, .maximize,
			.restore, .fullscreen, .mouse_lock]
	}
}

fn (mut backend Backend) service_show_window(id WindowId) !ServiceWindowState {
	return match backend.kind {
		.x11 { backend.x11.service_show_window(id)! }
		.wayland { backend.wayland.service_show_window(id)! }
		.appkit { backend.appkit.service_show_window(id)! }
		.win32 { backend.win32.service_show_window(id)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_hide_window(id WindowId) !ServiceWindowState {
	return match backend.kind {
		.x11 { backend.x11.service_hide_window(id)! }
		.wayland { backend.wayland.service_hide_window(id)! }
		.appkit { backend.appkit.service_hide_window(id)! }
		.win32 { backend.win32.service_hide_window(id)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_focus_window(id WindowId) !ServiceWindowState {
	return match backend.kind {
		.x11 { backend.x11.service_focus_window(id)! }
		.appkit { backend.appkit.service_focus_window(id)! }
		.win32 { backend.win32.service_focus_window(id)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_raise_window(id WindowId) !ServiceWindowState {
	return match backend.kind {
		.x11 { backend.x11.service_raise_window(id)! }
		.appkit { backend.appkit.service_raise_window(id)! }
		.win32 { backend.win32.service_raise_window(id)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_set_window_position(id WindowId, x int, y int) !ServiceWindowState {
	return match backend.kind {
		.x11 { backend.x11.service_set_window_position(id, x, y)! }
		.appkit { backend.appkit.service_set_window_position(id, x, y)! }
		.win32 { backend.win32.service_set_window_position(id, x, y)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_minimize_window(id WindowId) !ServiceWindowState {
	return match backend.kind {
		.x11 { backend.x11.service_minimize_window(id)! }
		.wayland { backend.wayland.service_minimize_window(id)! }
		.appkit { backend.appkit.service_minimize_window(id)! }
		.win32 { backend.win32.service_minimize_window(id)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_maximize_window(id WindowId) !ServiceWindowState {
	return match backend.kind {
		.x11 { backend.x11.service_maximize_window(id)! }
		.wayland { backend.wayland.service_maximize_window(id)! }
		.appkit { backend.appkit.service_maximize_window(id)! }
		.win32 { backend.win32.service_maximize_window(id)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_restore_window(id WindowId) !ServiceWindowState {
	return match backend.kind {
		.x11 { backend.x11.service_restore_window(id)! }
		.wayland { backend.wayland.service_restore_window(id)! }
		.appkit { backend.appkit.service_restore_window(id)! }
		.win32 { backend.win32.service_restore_window(id)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_set_fullscreen(id WindowId, enabled bool) !ServiceWindowState {
	return match backend.kind {
		.x11 { backend.x11.service_set_fullscreen(id, enabled)! }
		.wayland { backend.wayland.service_set_fullscreen(id, enabled)! }
		.appkit { backend.appkit.service_set_fullscreen(id, enabled)! }
		.win32 { backend.win32.service_set_fullscreen(id, enabled)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_set_mouse_lock(id WindowId, enabled bool) !ServiceWindowState {
	return match backend.kind {
		.x11 { backend.x11.service_set_mouse_lock(id, enabled)! }
		.wayland { backend.wayland.service_set_mouse_lock(id, enabled)! }
		.appkit { backend.appkit.service_set_mouse_lock(id, enabled)! }
		.win32 { backend.win32.service_set_mouse_lock(id, enabled)! }
		else { error(err_capability_unsupported) }
	}
}

fn (backend &Backend) service_native_window_borrow(id WindowId) !BackendNativeWindowBorrow {
	return match backend.kind {
		.x11 { backend.x11.service_native_window_borrow(id)! }
		.wayland { backend.wayland.service_native_window_borrow(id)! }
		.appkit { backend.appkit.service_native_window_borrow(id)! }
		.win32 { backend.win32.service_native_window_borrow(id)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_monitor_snapshot(app_instance u64) ![]ServiceMonitorInfo {
	return match backend.kind {
		.x11 { backend.x11.service_monitor_snapshot(app_instance)! }
		.wayland { backend.wayland.service_monitor_snapshot(app_instance)! }
		.appkit { backend.appkit.service_monitor_snapshot(app_instance)! }
		.win32 { backend.win32.service_monitor_snapshot(app_instance)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_start_portal_parent(id WindowId, request ServiceRequestId, lease ServicePortalLeaseId) !BackendPortalStart {
	return match backend.kind {
		.x11 {
			BackendPortalStart{
				completed:  true
				identifier: backend.x11.service_portal_parent_identifier(id)!
			}
		}
		.wayland {
			backend.wayland.service_start_portal_parent(id, request, lease)!
		}
		else {
			error(err_capability_unsupported)
		}
	}
}

fn (mut backend Backend) service_release_portal_parent(lease ServicePortalLeaseId) ! {
	match backend.kind {
		.x11 { return }
		.wayland { backend.wayland.service_release_portal_parent(lease)! }
		else { return error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_set_clipboard_text(id WindowId, request ServiceRequestId, text string) !BackendClipboardStart {
	return match backend.kind {
		.x11 { backend.x11.service_set_clipboard_text(id, request, text)! }
		.wayland { backend.wayland.service_set_clipboard_text(id, request, text)! }
		.appkit { backend.appkit.service_set_clipboard_text(id, request, text)! }
		.win32 { backend.win32.service_set_clipboard_text(id, request, text)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_request_clipboard_text(id WindowId, request ServiceRequestId) !BackendClipboardStart {
	return match backend.kind {
		.x11 { backend.x11.service_request_clipboard_text(id, request)! }
		.wayland { backend.wayland.service_request_clipboard_text(id, request)! }
		.appkit { backend.appkit.service_request_clipboard_text(id, request)! }
		.win32 { backend.win32.service_request_clipboard_text(id, request)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_window_readback(id WindowId, x int, y int, width int, height int) ![]u8 {
	return match backend.kind {
		.x11 { backend.x11.service_window_readback(id, x, y, width, height)! }
		.wayland { backend.wayland.service_window_readback(id, x, y, width, height)! }
		else { error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_window_readback_preflight(id WindowId, x int, y int, width int, height int) ! {
	match backend.kind {
		.x11 { backend.x11.service_window_readback_preflight(id, x, y, width, height)! }
		.wayland { return error(err_capability_unsupported) }
		else { return error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_stage_window_readback(readback ServiceReadbackId, x int, y int, width int, height int, producing_frame u64) ! {
	match backend.kind {
		.appkit {
			backend.appkit.service_stage_window_readback(readback, x, y, width, height,
				producing_frame)!
		}
		else {
			return error(err_capability_unsupported)
		}
	}
}

fn (mut backend Backend) service_stage_image_readback(readback ServiceReadbackId, image_id u32, x int, y int, width int, height int, producing_frame u64) ! {
	match backend.kind {
		.appkit {
			backend.appkit.service_stage_image_readback(readback, image_id, x, y, width, height,
				producing_frame)!
		}
		else {
			return error(err_capability_unsupported)
		}
	}
}

fn (mut backend Backend) service_arm_image_readback_pass(id WindowId, image_id u32, pass_serial u64, producing_frame u64) ! {
	match backend.kind {
		.appkit {
			backend.appkit.service_arm_image_readback_pass(id, image_id, pass_serial,
				producing_frame)!
		}
		else {
			return error(err_capability_unsupported)
		}
	}
}

fn (mut backend Backend) service_resolve_readbacks_after_submit(id WindowId, submitted_frame u64, submission_succeeded bool) ! {
	match backend.kind {
		.appkit {
			backend.appkit.service_resolve_readbacks_after_submit(id, submitted_frame,
				submission_succeeded)!
		}
		else {
			return error(err_capability_unsupported)
		}
	}
}

fn (mut backend Backend) service_cancel_readback(readback ServiceReadbackId) ! {
	match backend.kind {
		.appkit { backend.appkit.service_cancel_readback(readback)! }
		else { return error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) service_set_titlebar_appearance(id WindowId, appearance ServiceTitlebarAppearance) ! {
	match backend.kind {
		.appkit { backend.appkit.service_set_titlebar_appearance(id, appearance)! }
		else { return error(err_capability_unsupported) }
	}
}
