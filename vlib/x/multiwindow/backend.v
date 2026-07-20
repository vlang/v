module multiwindow

import os

const err_backend_event_sequence_exhausted = 'multiwindow: native event sequence exhausted'

struct Backend {
mut:
	kind                            BackendKind
	allow_mock_render_fallback      bool
	native_operations               NativeOperationAuthority
	teardown_notices                []BackendTeardownNotice
	pending_delivery                []QueuedEvent
	pending_delivery_active         bool
	pending_delivery_teardown_count int
	pending_delivery_error          string
	mock                            MockBackend
	x11                             X11Backend
	wayland                         WaylandBackend
	appkit                          AppKitBackend
	win32                           Win32Backend
}

fn (mut backend Backend) publish_native_operation_authority() {
	unsafe {
		authority := &backend.native_operations
		backend.x11.native_operations = authority
		backend.wayland.native_operations = authority
		backend.appkit.native_operations = authority
		backend.win32.native_operations = authority
	}
}

fn (mut backend Backend) bind_app_native_operations(app_identity u64, app_lifetime_token u64, initial_renderer_attempt_token u64) ! {
	backend.native_operations.bind_app_lifetime(app_identity, app_lifetime_token)!
	backend.native_operations.advance_renderer_attempt(app_identity, initial_renderer_attempt_token)!
	backend.publish_native_operation_authority()
}

fn (mut backend Backend) advance_renderer_native_operations(app_identity u64, renderer_attempt_token u64) ! {
	backend.native_operations.advance_renderer_attempt(app_identity, renderer_attempt_token)!
	backend.publish_native_operation_authority()
}

fn new_backend(kind BackendKind, require_renderer bool) !Backend {
	mut resolved_kind := kind
	auto_requested := kind == .auto
	if resolved_kind == .auto {
		resolved_kind = resolve_auto_backend_kind(require_renderer)
	}
	match resolved_kind {
		.auto {
			return error(err_backend_unsupported)
		}
		.mock {
			return Backend{
				kind:                       .mock
				allow_mock_render_fallback: allow_auto_mock_render_fallback(auto_requested,
					require_renderer)
				mock:                       new_mock_backend()
			}
		}
		.x11 {
			return Backend{
				kind: .x11
				x11:  new_x11_backend()
			}
		}
		.wayland {
			return Backend{
				kind:    .wayland
				wayland: new_wayland_backend()
			}
		}
		.appkit {
			return Backend{
				kind:   .appkit
				appkit: new_appkit_backend()
			}
		}
		.win32 {
			return Backend{
				kind:  .win32
				win32: new_win32_backend()
			}
		}
	}
}

fn resolve_auto_backend_kind(require_renderer bool) BackendKind {
	$if windows {
		return .win32
	}
	$if linux {
		if require_renderer {
			$if x_multiwindow_x11 ? {
				if os.getenv('DISPLAY') != '' {
					return .x11
				}
			}
			$if sokol_wayland ? {
				if os.getenv('WAYLAND_DISPLAY') != '' {
					return .wayland
				}
			}
		} else {
			$if sokol_wayland ? {
				if os.getenv('WAYLAND_DISPLAY') != '' {
					return .wayland
				}
			}
			$if x_multiwindow_x11 ? {
				if os.getenv('DISPLAY') != '' {
					return .x11
				}
			}
		}
	}
	$if darwin {
		return .appkit
	}
	_ = require_renderer
	return .mock
}

fn allow_auto_mock_render_fallback(auto_requested bool, require_renderer bool) bool {
	if !auto_requested || !require_renderer {
		return false
	}
	if !multiwindow_render_enabled() {
		return false
	}
	$if linux {
		$if x_multiwindow_x11 ? {
			if os.getenv('DISPLAY') != '' {
				return false
			}
		}
		$if sokol_wayland ? {
			return os.getenv('WAYLAND_DISPLAY') == ''
		}
		return true
	}
	$if windows {
		return false
	}
	return true
}

fn multiwindow_render_enabled() bool {
	$if gg_multiwindow ? {
		return true
	}
	$if x_multiwindow_render ? {
		return true
	}
	return false
}

// capabilities_for_backend reports capabilities without creating an App.
pub fn capabilities_for_backend(kind BackendKind) !Capabilities {
	return capabilities_for_backend_with_renderer(kind, false)
}

// capabilities_for_backend_with_renderer reports capabilities with the same
// backend resolution policy used by new_app(...).
pub fn capabilities_for_backend_with_renderer(kind BackendKind, require_renderer bool) !Capabilities {
	return capabilities_for_config(backend: kind, require_renderer: require_renderer)
}

// capabilities_for_config reports capabilities without starting an App while
// respecting Config.require_renderer for .auto backend selection.
pub fn capabilities_for_config(config Config) !Capabilities {
	mut backend := new_backend(config.backend, config.require_renderer)!
	backend.ensure_supported()!
	return backend.capabilities_for_renderer_requirement(config.require_renderer)!
}

fn (backend &Backend) capabilities() Capabilities {
	return match backend.kind {
		.auto {
			Capabilities{
				backend: .auto
			}
		}
		.mock {
			backend.mock.capabilities()
		}
		.x11 {
			backend.x11.capabilities()
		}
		.wayland {
			backend.wayland.capabilities()
		}
		.appkit {
			backend.appkit.capabilities()
		}
		.win32 {
			backend.win32.capabilities()
		}
	}
}

fn (mut backend Backend) capabilities_for_renderer_requirement(require_renderer bool) !Capabilities {
	if !require_renderer {
		return backend.capabilities()
	}
	if !multiwindow_render_enabled() {
		return error(err_renderer_unsupported)
	}
	probe_identity := allocate_app_instance_id()!
	backend.bind_app_native_operations(probe_identity, 1, 2)!
	return match backend.kind {
		.auto {
			error(err_backend_unsupported)
		}
		.mock {
			if backend.allow_mock_render_fallback {
				backend.mock.capabilities()
			} else {
				error(err_renderer_unsupported)
			}
		}
		.x11, .wayland, .appkit, .win32 {
			backend.probe_renderer_capabilities()!
		}
	}
}

fn (mut backend Backend) probe_renderer_capabilities() !Capabilities {
	return match backend.kind {
		.auto {
			error(err_backend_unsupported)
		}
		.mock {
			if backend.allow_mock_render_fallback {
				backend.mock.capabilities()
			} else {
				error(err_renderer_unsupported)
			}
		}
		.x11 {
			backend.x11.probe_renderer_capabilities()!
		}
		.wayland {
			backend.wayland.probe_renderer_capabilities()!
		}
		.appkit {
			backend.appkit.probe_renderer_capabilities()!
		}
		.win32 {
			backend.win32.probe_renderer_capabilities()!
		}
	}
}

fn (backend &Backend) ensure_supported() ! {
	match backend.kind {
		.auto { return error(err_backend_unsupported) }
		.mock {}
		.x11 { backend.x11.ensure_supported()! }
		.wayland { backend.wayland.ensure_supported()! }
		.appkit { backend.appkit.ensure_supported()! }
		.win32 { backend.win32.ensure_supported()! }
	}
}

fn (mut backend Backend) start(require_renderer bool) ! {
	backend.start_attempt(require_renderer) or {
		start_error := err.msg()
		close_error := backend.close_start_attempt()
		return error(merge_backend_errors(start_error, close_error))
	}
}

fn (mut backend Backend) start_attempt(require_renderer bool) ! {
	backend.ensure_supported()!
	if backend.kind != .mock && !backend.native_operations.owner_thread_is_current() {
		return error(err_owner_thread_required)
	}
	if backend.kind == .appkit {
		backend.appkit.prevalidate_start_attempt()!
	}
	backend.ensure_event_sequence_available()!
	if require_renderer && !multiwindow_render_enabled() {
		return error(err_renderer_unsupported)
	}
	match backend.kind {
		.auto {
			return error(err_backend_unsupported)
		}
		.mock {
			if require_renderer && !backend.allow_mock_render_fallback {
				return error(err_renderer_unsupported)
			}
			backend.mock.start()!
		}
		.x11 {
			backend.x11.start(require_renderer)!
		}
		.wayland {
			backend.wayland.start(require_renderer)!
		}
		.appkit {
			backend.appkit.start(require_renderer)!
		}
		.win32 {
			backend.win32.start(require_renderer)!
		}
	}

	if backend.kind != .appkit && backend.kind != .win32 {
		backend.ensure_event_sequence_available()!
	}
}

fn (mut backend Backend) create_window(id WindowId, config WindowConfig) !WindowSize {
	backend.ensure_event_sequence_available()!
	caps := backend.capabilities()
	if !caps.multi_window {
		return error(err_capability_unsupported)
	}
	size := match backend.kind {
		.auto { return error(err_backend_unsupported) }
		.mock { backend.mock.create_window(id, config)! }
		.x11 { backend.x11.create_window(id, config)! }
		.wayland { backend.wayland.create_window(id, config)! }
		.appkit { backend.appkit.create_window(id, config)! }
		.win32 { backend.win32.create_window(id, config)! }
	}

	backend.ensure_event_sequence_available()!
	return size
}

fn (mut backend Backend) destroy_window(id WindowId) ! {
	backend.finish_window_teardown(id)!
}

fn (mut backend Backend) finish_window_teardown(id WindowId) ! {
	mut operation_error := ''
	match backend.kind {
		.auto { return error(err_backend_unsupported) }
		.mock { backend.mock.finish_window_teardown(id) or { operation_error = err.msg() } }
		.x11 { backend.x11.finish_window_teardown(id) or { operation_error = err.msg() } }
		.wayland { backend.wayland.finish_window_teardown(id) or { operation_error = err.msg() } }
		.appkit { backend.appkit.finish_window_teardown(id) or { operation_error = err.msg() } }
		.win32 { backend.win32.finish_window_teardown(id) or { operation_error = err.msg() } }
	}

	sequence_error := backend.event_sequence_terminal_error()
	terminal := merge_backend_errors(operation_error, sequence_error)
	if terminal != '' {
		return error(terminal)
	}
}

fn (mut backend Backend) set_window_title(id WindowId, title string) ! {
	backend.ensure_event_sequence_available()!
	match backend.kind {
		.auto { return error(err_backend_unsupported) }
		.mock { backend.mock.set_window_title(id, title)! }
		.x11 { backend.x11.set_window_title(id, title)! }
		.wayland { backend.wayland.set_window_title(id, title)! }
		.appkit { backend.appkit.set_window_title(id, title)! }
		.win32 { backend.win32.set_window_title(id, title)! }
	}

	backend.ensure_event_sequence_available()!
}

fn (mut backend Backend) resize_window(id WindowId, width int, height int) !WindowSize {
	backend.ensure_event_sequence_available()!
	size := match backend.kind {
		.auto { return error(err_backend_unsupported) }
		.mock { backend.mock.resize_window(id, width, height)! }
		.x11 { backend.x11.resize_window(id, width, height)! }
		.wayland { backend.wayland.resize_window(id, width, height)! }
		.appkit { backend.appkit.resize_window(id, width, height)! }
		.win32 { backend.win32.resize_window(id, width, height)! }
	}

	backend.ensure_event_sequence_available()!
	return size
}

fn (backend &Backend) window_native_decorations(id WindowId, config WindowConfig) !bool {
	if config.borderless {
		return false
	}
	return match backend.kind {
		.auto { error(err_backend_unsupported) }
		.mock { false }
		.x11 { true }
		.wayland { backend.wayland.window_native_decorations(id)! }
		.appkit { true }
		.win32 { true }
	}
}

fn (mut backend Backend) set_window_cursor(id WindowId, shape CursorShape) ! {
	backend.ensure_event_sequence_available()!
	match backend.kind {
		.auto { return error(err_backend_unsupported) }
		.mock { backend.mock.set_window_cursor(id, shape)! }
		.x11 { backend.x11.set_window_cursor(id, shape)! }
		.wayland { backend.wayland.set_window_cursor(id, shape)! }
		.appkit { backend.appkit.set_window_cursor(id, shape)! }
		.win32 { backend.win32.set_window_cursor(id, shape)! }
	}

	backend.ensure_event_sequence_available()!
}

fn (mut backend Backend) begin_window_move(id WindowId) ! {
	backend.ensure_event_sequence_available()!
	match backend.kind {
		.wayland { backend.wayland.begin_window_move(id)! }
		.auto { return error(err_backend_unsupported) }
		else { return error(err_capability_unsupported) }
	}

	backend.ensure_event_sequence_available()!
}

fn (mut backend Backend) begin_window_resize(id WindowId, edge WindowResizeEdge) ! {
	backend.ensure_event_sequence_available()!
	match backend.kind {
		.wayland { backend.wayland.begin_window_resize(id, edge)! }
		.auto { return error(err_backend_unsupported) }
		else { return error(err_capability_unsupported) }
	}

	backend.ensure_event_sequence_available()!
}

fn (mut backend Backend) poll_queued_events() ![]QueuedEvent {
	if backend.pending_delivery_active {
		if backend.kind != .appkit {
			backend.pending_delivery_error = merge_backend_errors(backend.pending_delivery_error,
				backend.event_sequence_terminal_error())
		}
		return backend.pending_delivery.clone()
	}
	mut events := match backend.kind {
		.auto {
			return error(err_backend_unsupported)
		}
		.mock {
			backend.mock.poll_queued_events()!
		}
		.x11 {
			backend.x11.poll_queued_events()!
		}
		.wayland {
			backend.wayland.poll_queued_events()!
		}
		.appkit {
			backend.appkit.poll_queued_events()!
		}
		.win32 {
			backend.win32.poll_queued_events()!
		}
	}

	deferred_error := match backend.kind {
		.wayland { backend.wayland.take_poll_error() }
		.appkit { backend.appkit.take_poll_error() }
		.win32 { backend.win32.take_poll_error() }
		else { '' }
	}

	teardown_count := backend.teardown_notices.len
	for notice in backend.teardown_notices {
		events << queued_lifecycle_event(Event{
			kind:      .window_destroyed
			window_id: notice.window
		})
	}
	backend.pending_delivery = events.clone()
	backend.pending_delivery_active = true
	backend.pending_delivery_teardown_count = teardown_count
	backend.pending_delivery_error = deferred_error
	return events
}

fn (mut backend Backend) event_sequence_terminal_error() string {
	return match backend.kind {
		.wayland { backend.wayland.event_sequence_terminal_error() }
		.appkit { backend.appkit.event_sequence_terminal_error() }
		.win32 { backend.win32.event_sequence_terminal_error() }
		else { '' }
	}
}

fn (mut backend Backend) ensure_event_sequence_available() ! {
	terminal_error := backend.event_sequence_terminal_error()
	if terminal_error != '' {
		return error(terminal_error)
	}
}

fn merge_backend_errors(first string, second string) string {
	if first == '' {
		return second
	}
	if second == '' || second == first {
		return first
	}
	return '${first}; ${second}'
}

// acknowledge_queued_events releases only the batch durably applied by App.
// Native callbacks or teardown records appended after that batch remain queued.
fn (mut backend Backend) acknowledge_queued_events() string {
	if !backend.pending_delivery_active {
		return ''
	}
	deferred_error := backend.pending_delivery_error
	count := backend.pending_delivery_teardown_count
	if count > 0 {
		if count >= backend.teardown_notices.len {
			backend.teardown_notices.clear()
		} else {
			backend.teardown_notices = backend.teardown_notices[count..].clone()
		}
	}
	backend.pending_delivery.clear()
	backend.pending_delivery_active = false
	backend.pending_delivery_teardown_count = 0
	backend.pending_delivery_error = ''
	return deferred_error
}

// retained_delivery_error_for_stop observes errors without releasing a batch
// which has not yet been promoted into App delivery storage.
fn (mut backend Backend) retained_delivery_error_for_stop() string {
	deferred_error := backend.pending_delivery_error
	platform_error := match backend.kind {
		.wayland { backend.wayland.take_poll_error() }
		.appkit { backend.appkit.take_poll_error() }
		.win32 { backend.win32.take_poll_error() }
		else { '' }
	}

	return merge_backend_errors(deferred_error, platform_error)
}

fn (mut backend Backend) stop() ! {
	match backend.kind {
		.auto { return error(err_backend_unsupported) }
		.mock { backend.mock.stop()! }
		.x11 { backend.x11.stop()! }
		.wayland { backend.wayland.stop()! }
		.appkit { backend.appkit.stop()! }
		.win32 { backend.win32.stop()! }
	}
}

fn (mut backend Backend) close_start_attempt() string {
	return backend.close_start_attempt_once()
}

// Each branch closes the resources its platform start attempt can acquire before
// returning diagnostics to Backend.start or the capability probe.
fn (mut backend Backend) close_start_attempt_once() string {
	mut close_error := ''
	match backend.kind {
		.auto {
			close_error = err_backend_unsupported
		}
		.mock {
			backend.mock.stop() or { close_error = merge_backend_errors(close_error, err.msg()) }
		}
		.x11 {
			close_error = backend.x11.close_start_attempt()
		}
		.wayland {
			close_error = backend.wayland.close_start_attempt()
		}
		.appkit {
			close_error = backend.appkit.close_start_attempt()
		}
		.win32 {
			close_error = backend.win32.close_start_attempt()
		}
	}

	return close_error
}

fn (backend &Backend) start_attempt_closed() bool {
	platform_closed := match backend.kind {
		.auto { true }
		.mock { backend.mock.windows.len == 0 && backend.mock.pending_events.len == 0 }
		.x11 { backend.x11.start_attempt_closed() }
		.wayland { backend.wayland.start_attempt_closed() }
		.appkit { backend.appkit.start_attempt_closed() }
		.win32 { backend.win32.start_attempt_closed() }
	}

	return platform_closed && !backend.native_operations.has_live_lifetime_tickets()
}

fn (backend &Backend) retains_native_ownership_for_stop() bool {
	return match backend.kind {
		.x11 { backend.x11.retains_native_ownership() }
		.wayland { backend.wayland.retains_native_ownership() }
		.appkit { backend.appkit.retains_native_ownership() }
		.win32 { backend.win32.retains_native_ownership() }
		else { false }
	}
}
