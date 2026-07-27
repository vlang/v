module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if windows {
	#flag windows -DUNICODE
	#flag windows -D_UNICODE
	#flag windows -luser32
	#flag windows -lgdi32
	#flag windows -lshell32
	#include <windows.h>
	#insert "@VMODROOT/vlib/x/multiwindow/win32_backend_helpers.h"
}

@[markused]
struct Win32NativeQueuedEvent {
	sequence u64
	event    QueuedEvent
}

@[heap; markused]
struct Win32WindowRecord {
	id WindowId
mut:
	hwnd                      voidptr
	config                    WindowConfig
	width                     int
	height                    int
	framebuffer_width         int
	framebuffer_height        int
	destroyed                 bool
	render_resize_pending     bool
	suppress_resize_event     bool
	queued_events             []Win32NativeQueuedEvent
	mouse_x                   f32
	mouse_y                   f32
	mouse_dx                  f32
	mouse_dy                  f32
	mouse_pos_valid           bool
	iconified                 bool
	pending_dropped_files     []string
	pending_drop_modifiers    u32
	pending_high_surrogate    u32
	suppress_control_char     u32
	swapchain                 voidptr
	swapchain_ticket          u64
	pending_backbuffer        voidptr
	pending_backbuffer_ticket u64
	render_view               voidptr
	render_view_ticket        u64
	depth_texture             voidptr
	depth_texture_ticket      u64
	depth_stencil_view        voidptr
	depth_stencil_view_ticket u64
	render_target_generation  u64 = 1
}

struct Win32Backend {
mut:
	native_operations                &NativeOperationAuthority = unsafe { nil }
	started                          bool
	device                           voidptr
	device_ticket                    u64
	device_context                   voidptr
	device_context_ticket            u64
	factory                          voidptr
	factory_ticket                   u64
	pending_init_dxgi_device         voidptr
	pending_init_dxgi_device_ticket  u64
	pending_init_adapter             voidptr
	pending_init_adapter_ticket      u64
	using_warp                       bool
	anchor_color_texture             voidptr
	anchor_color_texture_ticket      u64
	anchor_render_view               voidptr
	anchor_render_view_ticket        u64
	anchor_depth_texture             voidptr
	anchor_depth_texture_ticket      u64
	anchor_depth_stencil_view        voidptr
	anchor_depth_stencil_view_ticket u64
	anchor_committed                 bool
	render_sequence                  u64
	render_health                    NativeRendererHealth
	poll_error                       string
	lifetime_release_error           string
	event_sequence_terminal          string
	windows                          []&Win32WindowRecord
}

struct Win32D3DDeviceLaneAttempt {
	outcome               NativeRenderResult
	device                voidptr
	device_ticket         u64
	device_context        voidptr
	device_context_ticket u64
}

struct Win32RawDeviceOutputsReleaseResult {
	context NativeLifetimeReleaseResult
	device  NativeLifetimeReleaseResult
}

fn (attempt Win32D3DDeviceLaneAttempt) has_owned_outputs() bool {
	return native_identity(attempt.device) != 0 || attempt.device_ticket != 0
		|| native_identity(attempt.device_context) != 0 || attempt.device_context_ticket != 0
}

fn (backend &Win32Backend) device_lane_attempt(outcome NativeRenderResult) Win32D3DDeviceLaneAttempt {
	return Win32D3DDeviceLaneAttempt{
		outcome:               outcome
		device:                backend.device
		device_ticket:         backend.device_ticket
		device_context:        backend.device_context
		device_context_ticket: backend.device_context_ticket
	}
}

fn win32_identity_is_owned(value voidptr, ticket_id u64) bool {
	return native_identity(value) != 0 || ticket_id != 0
}

fn (record &Win32WindowRecord) has_view_ownership() bool {
	return win32_identity_is_owned(record.depth_stencil_view, record.depth_stencil_view_ticket)
		|| win32_identity_is_owned(record.depth_texture, record.depth_texture_ticket)
		|| win32_identity_is_owned(record.render_view, record.render_view_ticket)
		|| win32_identity_is_owned(record.pending_backbuffer, record.pending_backbuffer_ticket)
}

fn (record &Win32WindowRecord) has_render_ownership() bool {
	return record.has_view_ownership()
		|| win32_identity_is_owned(record.swapchain, record.swapchain_ticket)
}

fn (backend &Win32Backend) has_anchor_ownership() bool {
	return
		win32_identity_is_owned(backend.anchor_depth_stencil_view, backend.anchor_depth_stencil_view_ticket)
		|| win32_identity_is_owned(backend.anchor_depth_texture, backend.anchor_depth_texture_ticket)
		|| win32_identity_is_owned(backend.anchor_render_view, backend.anchor_render_view_ticket)
		|| win32_identity_is_owned(backend.anchor_color_texture, backend.anchor_color_texture_ticket)
}

fn (backend &Win32Backend) has_renderer_root_ownership() bool {
	return win32_identity_is_owned(backend.factory, backend.factory_ticket)
		|| win32_identity_is_owned(backend.pending_init_adapter, backend.pending_init_adapter_ticket)
		|| win32_identity_is_owned(backend.pending_init_dxgi_device, backend.pending_init_dxgi_device_ticket)
		|| win32_identity_is_owned(backend.device_context, backend.device_context_ticket)
		|| win32_identity_is_owned(backend.device, backend.device_ticket)
}

fn (backend &Win32Backend) retains_native_ownership() bool {
	return backend.windows.len != 0 || backend.anchor_committed || backend.has_anchor_ownership()
		|| backend.has_renderer_root_ownership()
}

fn (mut backend Win32Backend) record_lifetime_release_outcome(result NativeRenderResult) {
	backend.render_health = renderer_health_after_result(backend.render_health, result)
	effective_failed := result.primitive.has(native_valid_return_value)
		&& dxgi_hresult_failed(result.primitive.return_value)
	if result.succeeded() && !effective_failed {
		return
	}
	message := if result.error_text != '' {
		result.error_text
	} else {
		err_render_native_renderer_unavailable
	}
	backend.lifetime_release_error = merge_backend_errors(backend.lifetime_release_error, message)
	backend.render_health = renderer_health_latch_unavailable(backend.render_health)
}

fn (mut backend Win32Backend) finish_renderer_shutdown_health() {
	if backend.lifetime_release_error != '' {
		backend.render_health = renderer_health_latch_unavailable(backend.render_health)
		return
	}
	backend.render_health = .abandoned
}

fn (mut backend Win32Backend) retained_stop_error(operation_error string) string {
	return merge_backend_errors(operation_error, merge_backend_errors(backend.lifetime_release_error,
		backend.event_sequence_terminal_error()))
}

fn (backend &Win32Backend) renderer_probe_error(operation_error string) string {
	return merge_backend_errors(operation_error, backend.lifetime_release_error)
}

$if windows {
	fn C.v_multiwindow_win32_register_class() int
	fn C.v_multiwindow_win32_create_window(title &u16, width int, height int, min_width int, min_height int, resizable int, borderless int, fullscreen int, visible int, data voidptr) voidptr
	fn C.v_multiwindow_win32_destroy_window(hwnd voidptr) int
	fn C.v_multiwindow_win32_set_window_text(hwnd voidptr, title &u16) int
	fn C.v_multiwindow_win32_set_cursor_shape(hwnd voidptr, shape int) int
	fn C.v_multiwindow_win32_set_client_size(hwnd voidptr, width int, height int, min_width int, min_height int, resizable int, borderless int, fullscreen int) int
	fn C.v_multiwindow_win32_client_width(hwnd voidptr) int
	fn C.v_multiwindow_win32_client_height(hwnd voidptr) int
	fn C.v_multiwindow_win32_pump_messages() int
	fn C.v_multiwindow_win32_event_sequence_exhausted() int
	fn C.v_multiwindow_win32_render_snapshot(hwnd voidptr, out_visible &int, out_minimized &int, out_logical_width &int, out_logical_height &int, out_framebuffer_width &int, out_framebuffer_height &int, out_scale &f32, out_conversion_available &int) int
	fn C.v_multiwindow_win32_logical_to_pixel_rect(hwnd voidptr, x f32, y f32, width f32, height f32, out_x &int, out_y &int, out_width &int, out_height &int) int
	fn C.v_multiwindow_win32_pixel_to_logical_rect(hwnd voidptr, x int, y int, width int, height int, out_x &f32, out_y &f32, out_width &f32, out_height &f32) int
}

$if windows {
	@[export: 'v_multiwindow_win32_window_close_requested']
	@[markused]
	fn win32_window_close_requested(data voidptr, sequence u64) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &Win32WindowRecord(data) }
		record.enqueue_native_event(sequence, queued_lifecycle_event(Event{
			kind:      .window_close_requested
			window_id: record.id
		}))
	}

	@[export: 'v_multiwindow_win32_window_destroyed']
	@[markused]
	fn win32_window_destroyed(data voidptr, sequence u64) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &Win32WindowRecord(data) }
		record.destroyed = true
		record.hwnd = unsafe { nil }
		record.enqueue_native_event(sequence, queued_lifecycle_event(Event{
			kind:      .window_destroyed
			window_id: record.id
		}))
	}

	@[export: 'v_multiwindow_win32_window_resized']
	@[markused]
	fn win32_window_resized(data voidptr, sequence u64, width int, height int) {
		if data == unsafe { nil } || width <= 0 || height <= 0 {
			return
		}
		mut record := unsafe { &Win32WindowRecord(data) }
		if record.width == width && record.height == height {
			return
		}
		record.width = width
		record.height = height
		record.render_resize_pending = true
		record.render_target_generation =
			exhaust_backend_target_generation(record.render_target_generation)
		if record.suppress_resize_event {
			return
		}
		record.enqueue_native_event(sequence, queued_lifecycle_event(Event{
			kind:      .window_resized
			window_id: record.id
			width:     width
			height:    height
		}))
		record.enqueue_native_event(sequence, queued_input_event(record.input_event(.resized)))
	}

	@[export: 'v_multiwindow_win32_window_input_event']
	@[markused]
	fn win32_window_input_event(data voidptr, sequence u64, kind int, key_code int, char_code u32, key_repeat int, modifiers u32, mouse_button int, mouse_x int, mouse_y int, wheel_delta_x int, wheel_delta_y int) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &Win32WindowRecord(data) }
		input_kind := win32_input_kind(kind)
		if input_kind == .invalid {
			return
		}
		if input_kind == .char {
			if key_repeat != 0 {
				record.enqueue_char_event_with_repeat(sequence, char_code, modifiers, true)
			} else {
				record.enqueue_char_event(sequence, char_code, modifiers)
			}
			return
		}
		mut input := record.input_event(input_kind)
		match input_kind {
			.key_down, .key_up {
				if key_code == 0 {
					return
				}
				input = InputEvent{
					kind:               input_kind
					window_id:          record.id
					key_code:           key_code
					key_repeat:         key_repeat != 0
					modifiers:          modifiers
					mouse_x:            record.mouse_x
					mouse_y:            record.mouse_y
					mouse_dx:           record.mouse_dx
					mouse_dy:           record.mouse_dy
					window_width:       record.width
					window_height:      record.height
					framebuffer_width:  record.width
					framebuffer_height: record.height
					mouse_button:       256
				}
			}
			.mouse_down, .mouse_up {
				if mouse_button == 256 {
					return
				}
				record.update_mouse_position(mouse_x, mouse_y, false)
				input = InputEvent{
					kind:               input_kind
					window_id:          record.id
					modifiers:          modifiers
					mouse_button:       mouse_button
					mouse_x:            record.mouse_x
					mouse_y:            record.mouse_y
					mouse_dx:           record.mouse_dx
					mouse_dy:           record.mouse_dy
					window_width:       record.width
					window_height:      record.height
					framebuffer_width:  record.width
					framebuffer_height: record.height
				}
			}
			.mouse_move {
				record.update_mouse_position(mouse_x, mouse_y, false)
				input = InputEvent{
					kind:               input_kind
					window_id:          record.id
					modifiers:          modifiers
					mouse_x:            record.mouse_x
					mouse_y:            record.mouse_y
					mouse_dx:           record.mouse_dx
					mouse_dy:           record.mouse_dy
					window_width:       record.width
					window_height:      record.height
					framebuffer_width:  record.width
					framebuffer_height: record.height
					mouse_button:       256
				}
			}
			.mouse_enter, .mouse_leave {
				record.update_mouse_position(mouse_x, mouse_y, true)
				input = InputEvent{
					kind:               input_kind
					window_id:          record.id
					modifiers:          modifiers
					mouse_x:            record.mouse_x
					mouse_y:            record.mouse_y
					window_width:       record.width
					window_height:      record.height
					framebuffer_width:  record.width
					framebuffer_height: record.height
					mouse_button:       256
				}
			}
			.mouse_scroll {
				record.update_mouse_position(mouse_x, mouse_y, true)
				input = InputEvent{
					kind:               input_kind
					window_id:          record.id
					modifiers:          modifiers
					mouse_x:            record.mouse_x
					mouse_y:            record.mouse_y
					scroll_x:           -f32(wheel_delta_x) / f32(30.0)
					scroll_y:           f32(wheel_delta_y) / f32(30.0)
					window_width:       record.width
					window_height:      record.height
					framebuffer_width:  record.width
					framebuffer_height: record.height
					mouse_button:       256
				}
			}
			.focused, .unfocused {}
			.clipboard_pasted {
				input = InputEvent{
					kind:               input_kind
					window_id:          record.id
					modifiers:          modifiers
					mouse_x:            record.mouse_x
					mouse_y:            record.mouse_y
					mouse_dx:           record.mouse_dx
					mouse_dy:           record.mouse_dy
					window_width:       record.width
					window_height:      record.height
					framebuffer_width:  record.width
					framebuffer_height: record.height
					mouse_button:       256
				}
			}
			.iconified {
				if record.iconified {
					return
				}
				record.iconified = true
			}
			.restored {
				if !record.iconified {
					return
				}
				record.iconified = false
			}
			else {
				return
			}
		}

		record.enqueue_native_event(sequence, queued_input_event(input))
		if input_kind == .key_down {
			control_char := win32_control_char_for_key(key_code)
			if control_char != 0 {
				if key_repeat != 0 {
					record.enqueue_char_event_with_repeat(sequence, control_char, modifiers, true)
				} else {
					record.enqueue_char_event(sequence, control_char, modifiers)
				}
				record.suppress_control_char = control_char
			}
		}
	}

	@[export: 'v_multiwindow_win32_window_drop_begin']
	@[markused]
	fn win32_window_drop_begin(data voidptr, sequence u64, mouse_x int, mouse_y int, modifiers u32) {
		_ = sequence
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &Win32WindowRecord(data) }
		record.pending_dropped_files.clear()
		record.pending_drop_modifiers = modifiers
		record.update_mouse_position(mouse_x, mouse_y, true)
	}

	@[export: 'v_multiwindow_win32_window_drop_file']
	@[markused]
	fn win32_window_drop_file(data voidptr, sequence u64, path &char) {
		_ = sequence
		if data == unsafe { nil } || path == unsafe { nil } {
			return
		}
		mut record := unsafe { &Win32WindowRecord(data) }
		record.pending_dropped_files << unsafe { tos_clone(&u8(path)) }
	}

	@[export: 'v_multiwindow_win32_window_drop_end']
	@[markused]
	fn win32_window_drop_end(data voidptr, sequence u64) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &Win32WindowRecord(data) }
		if record.pending_dropped_files.len == 0 {
			return
		}
		input := InputEvent{
			kind:               .files_dropped
			window_id:          record.id
			modifiers:          record.pending_drop_modifiers
			mouse_x:            record.mouse_x
			mouse_y:            record.mouse_y
			mouse_dx:           record.mouse_dx
			mouse_dy:           record.mouse_dy
			window_width:       record.width
			window_height:      record.height
			framebuffer_width:  record.width
			framebuffer_height: record.height
			mouse_button:       256
			dropped_files:      record.pending_dropped_files.clone()
		}
		record.pending_dropped_files.clear()
		record.enqueue_native_event(sequence, queued_input_event(input))
	}

	@[export: 'v_multiwindow_win32_window_touch_event']
	@[markused]
	fn win32_window_touch_event(data voidptr, sequence u64, kind int, modifiers u32, count int, ids &u64, xs &int, ys &int, changed &int) {
		if data == unsafe { nil } || count <= 0 || ids == unsafe { nil } || xs == unsafe { nil }
			|| ys == unsafe { nil } || changed == unsafe { nil } {
			return
		}
		input_kind := win32_input_kind(kind)
		if input_kind == .invalid {
			return
		}
		mut record := unsafe { &Win32WindowRecord(data) }
		touch_count := if count > 8 { 8 } else { count }
		mut touches := [8]InputTouchPoint{}
		for i in 0 .. touch_count {
			x := unsafe { xs[i] }
			y := unsafe { ys[i] }
			touches[i] = InputTouchPoint{
				identifier: unsafe { ids[i] }
				pos_x:      f32(x)
				pos_y:      f32(y)
				changed:    unsafe { changed[i] } != 0
			}
		}
		first_x := unsafe { xs[0] }
		first_y := unsafe { ys[0] }
		input := InputEvent{
			kind:               input_kind
			window_id:          record.id
			modifiers:          modifiers
			mouse_x:            f32(first_x)
			mouse_y:            f32(first_y)
			num_touches:        touch_count
			touches:            touches
			window_width:       record.width
			window_height:      record.height
			framebuffer_width:  record.width
			framebuffer_height: record.height
			mouse_button:       256
		}
		record.enqueue_native_event(sequence, queued_input_event(input))
	}
}

@[markused]
fn (mut record Win32WindowRecord) enqueue_native_event(sequence u64, event QueuedEvent) {
	if sequence == 0 {
		return
	}
	record.queued_events << Win32NativeQueuedEvent{
		sequence: sequence
		event:    event
	}
}

@[markused]
fn (record &Win32WindowRecord) input_event(kind InputEventKind) InputEvent {
	return InputEvent{
		kind:               kind
		window_id:          record.id
		mouse_x:            record.mouse_x
		mouse_y:            record.mouse_y
		mouse_dx:           record.mouse_dx
		mouse_dy:           record.mouse_dy
		window_width:       record.width
		window_height:      record.height
		framebuffer_width:  record.width
		framebuffer_height: record.height
		mouse_button:       256
	}
}

@[markused]
fn (mut record Win32WindowRecord) update_mouse_position(x int, y int, clear_delta bool) {
	new_x := f32(x)
	new_y := f32(y)
	if clear_delta || !record.mouse_pos_valid {
		record.mouse_x = new_x
		record.mouse_y = new_y
		record.mouse_dx = 0
		record.mouse_dy = 0
		record.mouse_pos_valid = true
		return
	}
	record.mouse_dx = new_x - record.mouse_x
	record.mouse_dy = new_y - record.mouse_y
	record.mouse_x = new_x
	record.mouse_y = new_y
}

@[markused]
fn (mut record Win32WindowRecord) enqueue_char_event(sequence u64, char_code u32, modifiers u32) {
	record.enqueue_char_event_with_repeat(sequence, char_code, modifiers, false)
}

@[markused]
fn (mut record Win32WindowRecord) enqueue_char_event_with_repeat(sequence u64, char_code u32, modifiers u32, key_repeat bool) {
	mut codepoint := char_code
	if record.suppress_control_char != 0 {
		if char_code == record.suppress_control_char {
			record.suppress_control_char = 0
			return
		}
		record.suppress_control_char = 0
	}
	if char_code >= 0xd800 && char_code <= 0xdbff {
		record.pending_high_surrogate = char_code
		return
	}
	if char_code >= 0xdc00 && char_code <= 0xdfff {
		if record.pending_high_surrogate == 0 {
			return
		}
		codepoint = 0x10000 + ((record.pending_high_surrogate - 0xd800) << 10) +
			(char_code - 0xdc00)
		record.pending_high_surrogate = 0
	} else {
		record.pending_high_surrogate = 0
	}
	if codepoint < 32 && !win32_is_useful_control_char(codepoint) {
		return
	}
	input := InputEvent{
		kind:               .char
		window_id:          record.id
		char_code:          codepoint
		key_repeat:         key_repeat
		modifiers:          modifiers
		window_width:       record.width
		window_height:      record.height
		framebuffer_width:  record.width
		framebuffer_height: record.height
	}
	record.enqueue_native_event(sequence, queued_input_event(input))
}

@[markused]
fn win32_input_kind(kind int) InputEventKind {
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
		12 { InputEventKind.iconified }
		13 { InputEventKind.restored }
		14 { InputEventKind.clipboard_pasted }
		15 { InputEventKind.touches_began }
		16 { InputEventKind.touches_moved }
		17 { InputEventKind.touches_ended }
		else { InputEventKind.invalid }
	}
}

@[markused]
fn win32_control_char_for_key(key_code int) u32 {
	return match key_code {
		259 { u32(8) }
		258 { u32(9) }
		257, 335 { u32(13) }
		261 { u32(127) }
		else { u32(0) }
	}
}

@[markused]
fn win32_is_useful_control_char(char_code u32) bool {
	return char_code == 8 || char_code == 9 || char_code == 13 || char_code == 127
}

fn win32_sort_native_events(mut events []Win32NativeQueuedEvent) {
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

fn new_win32_backend() Win32Backend {
	return Win32Backend{}
}

fn (backend &Win32Backend) ensure_supported() ! {
	$if windows {
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (backend &Win32Backend) capabilities() Capabilities {
	return Capabilities{
		backend:            .win32
		mock:               false
		native:             true
		multi_window:       true
		owner_queue:        true
		explicit_swapchain: backend.renderer_ready()
		d3d11:              backend.renderer_ready()
		win32:              true
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

fn (backend &Win32Backend) start_attempt_closed() bool {
	live_tickets := backend.native_operations != unsafe { nil }
		&& backend.native_operations.has_live_lifetime_tickets()
	// The registered window class is process-global, not instance ownership.
	return !backend.started && !backend.retains_native_ownership() && !live_tickets
}

fn (mut backend Win32Backend) close_start_attempt() string {
	mut close_error := ''
	backend.stop() or { close_error = err.msg() }
	if !backend.start_attempt_closed() {
		close_error = merge_backend_errors(close_error, err_render_native_renderer_unavailable)
	}
	return close_error
}

fn (mut backend Win32Backend) probe_renderer_capabilities() !Capabilities {
	$if windows {
		if !backend.start_attempt_closed() {
			return error(backend.renderer_probe_error(err_render_native_renderer_unavailable))
		}
		if backend.render_health.blocks_graphics() {
			return error(backend.renderer_probe_error(err_render_native_renderer_unavailable))
		}
		backend.init_renderer() or {
			probe_error := err.msg()
			backend.shutdown_renderer()
			if !backend.start_attempt_closed() {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(backend.renderer_probe_error(merge_backend_errors(probe_error,
					err_render_native_renderer_unavailable)))
			}
			return error(backend.renderer_probe_error(probe_error))
		}
		caps := backend.capabilities()
		backend.shutdown_renderer()
		if !backend.start_attempt_closed() {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(backend.renderer_probe_error(err_render_native_renderer_unavailable))
		}
		cleanup_error := backend.renderer_probe_error('')
		if cleanup_error != '' {
			return error(cleanup_error)
		}
		return caps
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (mut backend Win32Backend) start(require_renderer bool) ! {
	$if windows {
		if backend.started {
			return
		}
		if backend.retains_native_ownership() {
			backend.shutdown_renderer()
			if backend.retains_native_ownership() {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(backend.retained_stop_error(err_render_native_renderer_unavailable))
			}
			cleanup_error := backend.retained_stop_error('')
			if cleanup_error != '' {
				return error(cleanup_error)
			}
		}
		if backend.render_health.blocks_graphics() {
			return error(err_render_native_renderer_unavailable)
		}
		if require_renderer {
			backend.init_renderer() or {
				start_error := err
				backend.shutdown_renderer()
				if backend.retains_native_ownership() {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(backend.retained_stop_error(merge_backend_errors(start_error.msg(),
						err_render_native_renderer_unavailable)))
				}
				return error(backend.retained_stop_error(start_error.msg()))
			}
		}
		if C.v_multiwindow_win32_register_class() == 0 {
			if require_renderer {
				backend.shutdown_renderer()
				if backend.retains_native_ownership() {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(backend.retained_stop_error(merge_backend_errors(err_win32_register_class_failed,
						err_render_native_renderer_unavailable)))
				}
			}
			return error(backend.retained_stop_error(err_win32_register_class_failed))
		}
		backend.started = true
		return
	} $else {
		_ = require_renderer
		return error(err_backend_unsupported)
	}
}

fn (mut backend Win32Backend) create_window(id WindowId, config WindowConfig) !WindowSize {
	$if windows {
		if !backend.started {
			return error(err_backend_unsupported)
		}
		backend.windows << &Win32WindowRecord{
			id:     id
			config: config
			width:  config.width
			height: config.height
		}
		index := backend.windows.len - 1
		mut record := backend.windows[index]
		title := config.title.to_wide()
		record_data := unsafe { voidptr(record) }
		hwnd := C.v_multiwindow_win32_create_window(title, config.width, config.height,
			config.min_width, config.min_height, win32_bool_to_int(config.resizable),
			win32_bool_to_int(config.borderless), win32_bool_to_int(config.fullscreen),
			win32_bool_to_int(config.visible), record_data)
		if hwnd == unsafe { nil } {
			backend.windows.delete(index)
			return error(err_win32_create_window_failed)
		}
		record.hwnd = hwnd
		actual_width := C.v_multiwindow_win32_client_width(hwnd)
		actual_height := C.v_multiwindow_win32_client_height(hwnd)
		if actual_width > 0 {
			record.width = actual_width
		}
		if actual_height > 0 {
			record.height = actual_height
		}
		record.config = window_config_with_size(record.config, record.width, record.height)
		record.render_resize_pending = false
		return WindowSize{
			width:  record.width
			height: record.height
		}
	} $else {
		_ = id
		_ = config
		return error(err_backend_unsupported)
	}
}

fn (mut backend Win32Backend) destroy_window(id WindowId) ! {
	backend.finish_window_teardown(id)!
}

fn (mut backend Win32Backend) finish_window_teardown(id WindowId) ! {
	$if windows {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		mut record := backend.windows[index]
		if !backend.release_window_render_resources(index, win32_window_operation_seed(record.id,
			record.render_target_generation, .shutdown_release)) {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_render_native_renderer_unavailable)
		}
		if record.has_render_ownership() {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_render_native_renderer_unavailable)
		}
		if record.hwnd != unsafe { nil } {
			if C.v_multiwindow_win32_destroy_window(record.hwnd) == 0 {
				return error(err_win32_destroy_window_failed)
			}
			record.hwnd = unsafe { nil }
		}
		backend.windows.delete(index)
		return
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend Win32Backend) set_window_title(id WindowId, title string) ! {
	$if windows {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		mut record := backend.windows[index]
		if record.hwnd == unsafe { nil } {
			return error(err_window_not_found)
		}
		wide_title := title.to_wide()
		if C.v_multiwindow_win32_set_window_text(record.hwnd, wide_title) == 0 {
			return error(err_win32_set_window_title_failed)
		}
		record.config = window_config_with_title(record.config, title)
		return
	} $else {
		_ = id
		_ = title
		return error(err_backend_unsupported)
	}
}

fn (mut backend Win32Backend) resize_window(id WindowId, width int, height int) !WindowSize {
	$if windows {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		mut record := backend.windows[index]
		if record.hwnd == unsafe { nil } {
			return error(err_window_not_found)
		}
		record.suppress_resize_event = true
		resize_ok := C.v_multiwindow_win32_set_client_size(record.hwnd, width, height,
			record.config.min_width, record.config.min_height,
			win32_bool_to_int(record.config.resizable),
			win32_bool_to_int(record.config.borderless),
			win32_bool_to_int(record.config.fullscreen)) != 0
		record.suppress_resize_event = false
		if !resize_ok {
			return error(err_win32_resize_window_failed)
		}
		mut actual_width := C.v_multiwindow_win32_client_width(record.hwnd)
		mut actual_height := C.v_multiwindow_win32_client_height(record.hwnd)
		if actual_width <= 0 {
			actual_width = window_extent_for_minimum(width, record.config.min_width)
		}
		if actual_height <= 0 {
			actual_height = window_extent_for_minimum(height, record.config.min_height)
		}
		record.width = actual_width
		record.height = actual_height
		record.config = window_config_with_size(record.config, actual_width, actual_height)
		record.render_resize_pending = true
		record.render_target_generation =
			next_backend_target_generation(record.render_target_generation)!
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

fn (mut backend Win32Backend) set_window_cursor(id WindowId, shape CursorShape) ! {
	$if windows {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		record := backend.windows[index]
		if record.hwnd == unsafe { nil } {
			return error(err_window_not_found)
		}
		if C.v_multiwindow_win32_set_cursor_shape(record.hwnd, int(shape)) == 0 {
			return error(err_capability_unsupported)
		}
		return
	} $else {
		_ = id
		_ = shape
		return error(err_backend_unsupported)
	}
}

fn (mut backend Win32Backend) poll_queued_events() ![]QueuedEvent {
	mut native_events := []Win32NativeQueuedEvent{}
	$if windows {
		if !backend.started {
			return []QueuedEvent{}
		}
		C.v_multiwindow_win32_pump_messages()
		mut i := 0
		for i < backend.windows.len {
			mut record := backend.windows[i]
			for event in record.queued_events {
				native_events << event
			}
			record.queued_events.clear()
			i++
		}
	}
	win32_sort_native_events(mut native_events)
	mut events := []QueuedEvent{cap: native_events.len}
	for native_event in native_events {
		events << native_event.event
	}
	return events
}

fn (mut backend Win32Backend) take_poll_error() string {
	terminal_error := backend.event_sequence_terminal_error()
	error_message := backend.poll_error
	backend.poll_error = ''
	return merge_backend_errors(error_message, terminal_error)
}

fn (mut backend Win32Backend) event_sequence_terminal_error() string {
	$if windows {
		if C.v_multiwindow_win32_event_sequence_exhausted() != 0
			&& backend.event_sequence_terminal == '' {
			backend.event_sequence_terminal = err_backend_event_sequence_exhausted
		}
	}
	return backend.event_sequence_terminal
}

fn (mut backend Win32Backend) stop() ! {
	$if windows {
		if !backend.started && !backend.retains_native_ownership() {
			terminal_error := backend.retained_stop_error('')
			if terminal_error != '' {
				return error(terminal_error)
			}
			return
		}
		for backend.windows.len > 0 {
			mut record := backend.windows[0]
			if !backend.release_window_render_resources(0, win32_window_operation_seed(record.id,
				record.render_target_generation, .shutdown_release)) {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(backend.retained_stop_error(err_render_native_renderer_unavailable))
			}
			if record.has_render_ownership() {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return error(backend.retained_stop_error(err_render_native_renderer_unavailable))
			}
			if record.hwnd != unsafe { nil } {
				if C.v_multiwindow_win32_destroy_window(record.hwnd) == 0 {
					return error(backend.retained_stop_error(err_win32_destroy_window_failed))
				}
				record.hwnd = unsafe { nil }
			}
			backend.windows.delete(0)
		}
		backend.shutdown_renderer()
		if backend.retains_native_ownership() {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(backend.retained_stop_error(err_render_native_renderer_unavailable))
		}
		backend.started = false
		poll_error := backend.poll_error
		backend.poll_error = ''
		terminal_error := backend.retained_stop_error(poll_error)
		if terminal_error != '' {
			return error(terminal_error)
		}
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (backend &Win32Backend) window_record_index(id WindowId) ?int {
	for i, record in backend.windows {
		if record.id == id {
			return i
		}
	}
	return none
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	$if sokol_d3d11 ? {
		fn (mut backend Win32Backend) render_environment(id WindowId) !gfx.Environment {
			$if windows {
				index := backend.window_record_index(id) or { return error(err_window_not_found) }
				record := backend.windows[index]
				preparation := backend.ensure_window_render_target(index, NativeOperationSeed{
					presence_mask:     native_context_has_window | native_context_has_target_generation
					call_site:         .window_prepare
					scope:             .window_target
					window:            id
					target_generation: record.render_target_generation
				})
				if !preparation.succeeded() {
					return native_render_error(preparation)
				}
				return gfx.Environment{
					defaults: gfx.EnvironmentDefaults{
						color_format: .bgra8
						depth_format: .depth_stencil
						sample_count: 1
					}
					d3d11:    gfx.D3d11Environment{
						device:         backend.device
						device_context: backend.device_context
					}
				}
			} $else {
				_ = id
				return error(err_backend_unsupported)
			}
		}

		fn (mut backend Win32Backend) begin_render(id WindowId, candidate RenderWindowSnapshot, native_attempt NativeTargetAttempt) BackendFrameAttempt {
			$if windows {
				index := backend.window_record_index(id) or {
					return BackendFrameAttempt{
						outcome: native_render_outcome(.none, .backbuffer_acquire, .window_target,
							.operation_failed, 0, 0, err_window_not_found)
					}
				}
				preparation := backend.ensure_window_render_target(index, NativeOperationSeed{
					presence_mask:      native_context_window_target_fields
					call_site:          .window_prepare
					scope:              .window_target
					window:             id
					target_generation:  candidate.target.target_identity
					batch_epoch:        native_attempt.batch_epoch
					window_lease_epoch: native_attempt.window_lease_epoch
					target_lease_epoch: native_attempt.target_lease_epoch
				})
				if !preparation.succeeded() {
					return BackendFrameAttempt{
						outcome: preparation
					}
				}
				record := backend.windows[index]
				if candidate.window != id
					|| candidate.target.target_identity != record.render_target_generation
					|| candidate.metrics.framebuffer_width != record.framebuffer_width
					|| candidate.metrics.framebuffer_height != record.framebuffer_height
					|| candidate.target.color_format != int(gfx.PixelFormat.bgra8)
					|| candidate.target.depth_format != int(gfx.PixelFormat.depth_stencil)
					|| candidate.target.sample_count != 1 {
					return BackendFrameAttempt{
						outcome: native_render_outcome(.none, .backbuffer_acquire, .window_target,
							.target_lost, 0, 0, err_render_target_stale)
					}
				}
				return BackendFrameAttempt{
					frame:   RenderFrame{
						window_id:          id
						batch_epoch:        native_attempt.batch_epoch
						window_lease_epoch: native_attempt.window_lease_epoch
						target_lease_epoch: native_attempt.target_lease_epoch
						metrics:            candidate.metrics
						target:             candidate.target
						swapchain:          backend.swapchain_for_record(record)
					}
					outcome: native_render_ok(.dxgi, .backbuffer_acquire, .window_target)
				}
			} $else {
				_ = id
				_ = candidate
				_ = native_attempt
				return BackendFrameAttempt{
					outcome: native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
						.renderer_unavailable, 0, 0, err_backend_unsupported)
				}
			}
		}

		fn (mut backend Win32Backend) end_render(frame RenderFrame) BackendFinalizeAttempt {
			$if windows {
				index := backend.window_record_index(frame.window_id) or {
					return BackendFinalizeAttempt{
						status:  .not_presented
						outcome: native_render_outcome(.none, .present, .window_target,
							.operation_failed, 0, 0, err_window_not_found)
					}
				}
				mut record := backend.windows[index]
				if record.swapchain == unsafe { nil } {
					return BackendFinalizeAttempt{
						status:  .not_presented
						outcome: native_render_outcome(.none, .present, .window_target,
							.target_lost, 0, 0, err_render_target_lost)
					}
				}
				if backend.render_health.blocks_graphics() {
					disposition := if backend.render_health == .lost {
						NativeRenderDisposition.renderer_lost
					} else {
						NativeRenderDisposition.renderer_unavailable
					}
					return BackendFinalizeAttempt{
						status:  .not_presented
						outcome: native_render_outcome(.dxgi, .present, .renderer, disposition, 0,
							0, err_render_native_renderer_lost)
					}
				}
				seed :=
					native_seed_for_frame(frame, .window_finalize).with_target_identity(native_identity(record.swapchain))
				mut ordinals := backend.native_operations.reserve_ordinals(2) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return BackendFinalizeAttempt{
						status:  .not_presented
						outcome: native_render_outcome(.dxgi, .present, .renderer,
							.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
					}
				}
				context := ordinals.materialize(backend.native_operations, .dxgi, .present, seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return BackendFinalizeAttempt{
						status:  .not_presented
						outcome: native_render_outcome(.dxgi, .present, .renderer,
							.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
					}
				}
				mut raw := C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_win32_dxgi_present(native_identity(backend.device),
					native_identity(record.swapchain), &raw)
				result := backend.accept_dxgi_result(context, mut ordinals, seed,
					native_identity(backend.device), raw, .none)
				return BackendFinalizeAttempt{
					status:  if result.succeeded() { .submitted } else { .not_presented }
					outcome: result
				}
			} $else {
				_ = frame
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: native_render_outcome(.dxgi, .present, .renderer,
						.renderer_unavailable, 0, 0, err_backend_unsupported)
				}
			}
		}

		fn (backend &Win32Backend) renderer_ready() bool {
			return backend.device != unsafe { nil } && backend.device_context != unsafe { nil }
				&& backend.factory != unsafe { nil } && backend.device_ticket != 0
				&& backend.device_context_ticket != 0 && backend.factory_ticket != 0
				&& !win32_identity_is_owned(backend.pending_init_dxgi_device, backend.pending_init_dxgi_device_ticket)
				&& !win32_identity_is_owned(backend.pending_init_adapter, backend.pending_init_adapter_ticket)
				&& backend.render_health == .ready
		}

		fn (mut backend Win32Backend) create_d3d_device_lane(driver i64, seed NativeOperationSeed) Win32D3DDeviceLaneAttempt {
			$if windows {
				if win32_identity_is_owned(backend.device, backend.device_ticket)
					|| win32_identity_is_owned(backend.device_context, backend.device_context_ticket) {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.device_lane_attempt(native_render_outcome(.dxgi, .device_create,
						.renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
				mut ordinals := backend.native_operations.reserve_renderer_attempt_ordinals(4) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.device_lane_attempt(native_render_outcome(.dxgi, .device_create,
						.renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
				mut cleanup := backend.native_operations.reserve_app_lifetime_ordinals(4) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.device_lane_attempt(native_render_outcome(.dxgi, .device_create,
						.renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
				first_device_ticket := backend.reserve_com_lifetime_ticket(mut cleanup, seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.device_lane_attempt(native_render_outcome(.dxgi, .device_create,
						.renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
				first_context_ticket := backend.reserve_com_lifetime_ticket(mut cleanup, seed) or {
					backend.native_operations.burn_lifetime_ticket(first_device_ticket)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.device_lane_attempt(native_render_outcome(.dxgi, .device_create,
						.renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
				second_device_ticket := backend.reserve_com_lifetime_ticket(mut cleanup, seed) or {
					backend.native_operations.burn_lifetime_ticket(first_context_ticket)
					backend.native_operations.burn_lifetime_ticket(first_device_ticket)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.device_lane_attempt(native_render_outcome(.dxgi, .device_create,
						.renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
				second_context_ticket := backend.reserve_com_lifetime_ticket(mut cleanup, seed) or {
					backend.native_operations.burn_lifetime_ticket(second_device_ticket)
					backend.native_operations.burn_lifetime_ticket(first_context_ticket)
					backend.native_operations.burn_lifetime_ticket(first_device_ticket)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.device_lane_attempt(native_render_outcome(.dxgi, .device_create,
						.renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
				first_context := ordinals.materialize(backend.native_operations, .dxgi,
					.device_create, seed.without_target_identity()) or {
					backend.native_operations.burn_lifetime_ticket(second_context_ticket)
					backend.native_operations.burn_lifetime_ticket(second_device_ticket)
					backend.native_operations.burn_lifetime_ticket(first_context_ticket)
					backend.native_operations.burn_lifetime_ticket(first_device_ticket)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.device_lane_attempt(native_render_outcome(.dxgi, .device_create,
						.renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
				mut raw := C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_win32_d3d11_create_device_attempt(driver, 0, &raw)
				first_device := native_pointer(raw.handle)
				first_device_context := native_pointer(raw.object_identity_0)
				backend.bind_or_burn_com_lifetime_ticket(first_device_ticket, first_device)
				backend.device = first_device
				backend.device_ticket = if native_identity(first_device) == 0 {
					u64(0)
				} else {
					first_device_ticket
				}
				backend.bind_or_burn_com_lifetime_ticket(first_context_ticket, first_device_context)
				backend.device_context = first_device_context
				backend.device_context_ticket = if native_identity(first_device_context) == 0 {
					u64(0)
				} else {
					first_context_ticket
				}
				first := backend.accept_dxgi_result(first_context, mut ordinals, seed, raw.handle,
					raw, .none)
				if first.succeeded() {
					backend.native_operations.burn_lifetime_ticket(second_context_ticket)
					backend.native_operations.burn_lifetime_ticket(second_device_ticket)
					ordinals.skip(2) or {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return backend.device_lane_attempt(native_render_outcome(.dxgi,
							.device_create, .renderer, .renderer_unavailable, 0, 0,
							err_render_native_renderer_unavailable))
					}
					return backend.device_lane_attempt(first)
				}
				first_release := backend.release_raw_device_outputs(seed)
				if !first_release.context.ticket_retired || !first_release.device.ticket_retired
					|| backend.render_health.blocks_graphics() || first.local_validation != .none
					|| first.blocks_graphics()
					|| u32(first.actual_primitive.return_value) != u32(0x80070057)
					|| u32(first.primitive.return_value) != u32(0x80070057) {
					backend.native_operations.burn_lifetime_ticket(second_context_ticket)
					backend.native_operations.burn_lifetime_ticket(second_device_ticket)
					ordinals.skip(2) or {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return backend.device_lane_attempt(native_render_outcome(.dxgi,
							.device_create, .renderer, .renderer_unavailable, 0, 0,
							err_render_native_renderer_unavailable))
					}
					return backend.device_lane_attempt(first)
				}
				second_context := ordinals.materialize(backend.native_operations, .dxgi,
					.device_create, seed.without_target_identity()) or {
					backend.native_operations.burn_lifetime_ticket(second_context_ticket)
					backend.native_operations.burn_lifetime_ticket(second_device_ticket)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.device_lane_attempt(native_render_outcome(.dxgi, .device_create,
						.renderer, .renderer_unavailable, 0, 0,
						err_render_native_renderer_unavailable))
				}
				raw = C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_win32_d3d11_create_device_attempt(driver, 1, &raw)
				second_device := native_pointer(raw.handle)
				second_device_context := native_pointer(raw.object_identity_0)
				backend.bind_or_burn_com_lifetime_ticket(second_device_ticket, second_device)
				backend.device = second_device
				backend.device_ticket = if native_identity(second_device) == 0 {
					u64(0)
				} else {
					second_device_ticket
				}
				backend.bind_or_burn_com_lifetime_ticket(second_context_ticket,
					second_device_context)
				backend.device_context = second_device_context
				backend.device_context_ticket = if native_identity(second_device_context) == 0 {
					u64(0)
				} else {
					second_context_ticket
				}
				second := backend.accept_dxgi_result(second_context, mut ordinals, seed,
					raw.handle, raw, .none)
				if !second.succeeded() {
					_ = backend.release_raw_device_outputs(seed)
					return backend.device_lane_attempt(second)
				}
				return backend.device_lane_attempt(second)
			} $else {
				_ = backend
				_ = driver
				_ = seed
				return Win32D3DDeviceLaneAttempt{
					outcome: native_render_outcome(.dxgi, .device_create, .renderer,
						.renderer_unavailable, 0, 0, err_backend_unsupported)
				}
			}
		}

		fn (mut backend Win32Backend) init_renderer() ! {
			$if windows {
				if backend.renderer_ready() {
					return
				}
				if backend.render_health.blocks_graphics() {
					return error(err_render_native_renderer_unavailable)
				}
				if backend.native_operations == unsafe { nil } {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				mut allow_warp_fallback := false
				mut forced_warp := false
				$if multiwindow_d3d11_warp_fallback ? || gg_multiwindow_d3d11_warp_fallback ? {
					allow_warp_fallback = true
				}
				$if multiwindow_d3d11_warp ? || gg_multiwindow_d3d11_warp ? {
					forced_warp = true
					allow_warp_fallback = false
				}
				seed := NativeOperationSeed{
					call_site: .renderer_start
					scope:     .renderer
				}
				mut used_warp := forced_warp
				mut final_device_attempt := Win32D3DDeviceLaneAttempt{}
				if forced_warp {
					final_device_attempt = backend.create_d3d_device_lane(1, seed)
				} else {
					hardware_attempt := backend.create_d3d_device_lane(0, seed)
					hardware_result := hardware_attempt.outcome
					final_device_attempt = hardware_attempt
					if !hardware_result.succeeded() && allow_warp_fallback
						&& !hardware_attempt.has_owned_outputs()
						&& !backend.render_health.blocks_graphics()
						&& !hardware_result.blocks_graphics()
						&& hardware_result.local_validation == .none
						&& u32(hardware_result.actual_primitive.return_value) == u32(0x887a0004)
						&& u32(hardware_result.primitive.return_value) == u32(0x887a0004) {
						final_device_attempt = backend.create_d3d_device_lane(1, seed)
						used_warp = true
					}
				}
				final_device_result := final_device_attempt.outcome
				if !final_device_result.succeeded() {
					if !backend.render_health.blocks_graphics() {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
					}
					return error(err_win32_d3d_device_failed)
				}
				mut ordinals := backend.native_operations.reserve_renderer_attempt_ordinals(6) or {
					_ = backend.release_renderer_root_ownership(seed)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				mut transient_cleanup := backend.native_operations.reserve_renderer_attempt_ordinals(2) or {
					_ = backend.release_renderer_root_ownership(seed)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				mut persistent_cleanup := backend.native_operations.reserve_app_lifetime_ordinals(1) or {
					_ = backend.release_renderer_root_ownership(seed)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				dxgi_device_ticket := backend.reserve_com_lifetime_ticket(mut transient_cleanup, seed) or {
					_ = backend.release_renderer_root_ownership(seed)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				adapter_ticket := backend.reserve_com_lifetime_ticket(mut transient_cleanup, seed) or {
					backend.native_operations.burn_lifetime_ticket(dxgi_device_ticket)
					_ = backend.release_renderer_root_ownership(seed)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				factory_ticket := backend.reserve_com_lifetime_ticket(mut persistent_cleanup, seed) or {
					backend.native_operations.burn_lifetime_ticket(adapter_ticket)
					backend.native_operations.burn_lifetime_ticket(dxgi_device_ticket)
					_ = backend.release_renderer_root_ownership(seed)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				device_seed := seed.with_target_identity(native_identity(backend.device))
				device_query_context := ordinals.materialize(backend.native_operations, .dxgi,
					.device_query, device_seed) or {
					backend.native_operations.burn_lifetime_ticket(factory_ticket)
					backend.native_operations.burn_lifetime_ticket(adapter_ticket)
					backend.native_operations.burn_lifetime_ticket(dxgi_device_ticket)
					_ = backend.release_renderer_root_ownership(seed)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				mut raw := C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_win32_d3d11_query_dxgi_device(native_identity(backend.device), &raw)
				dxgi_device := native_pointer(raw.handle)
				backend.bind_or_burn_com_lifetime_ticket(dxgi_device_ticket, dxgi_device)
				backend.pending_init_dxgi_device = dxgi_device
				backend.pending_init_dxgi_device_ticket = if native_identity(dxgi_device) == 0 {
					u64(0)
				} else {
					dxgi_device_ticket
				}
				dxgi_result := backend.accept_dxgi_result(device_query_context, mut ordinals,
					device_seed, native_identity(backend.device), raw, .none)
				if !dxgi_result.succeeded() {
					backend.render_health = renderer_health_after_result(renderer_health_latch_unavailable(backend.render_health),
						dxgi_result)
					backend.native_operations.record_health_latch(dxgi_result.context,
						backend.render_health)
					backend.native_operations.burn_lifetime_ticket(factory_ticket)
					backend.native_operations.burn_lifetime_ticket(adapter_ticket)
					_ = backend.release_renderer_root_ownership(seed)
					return error(err_win32_d3d_device_failed)
				}
				adapter_seed :=
					seed.with_target_identity(native_identity(backend.pending_init_dxgi_device))
				adapter_context := ordinals.materialize(backend.native_operations, .dxgi,
					.adapter_acquire, adapter_seed) or {
					backend.native_operations.burn_lifetime_ticket(factory_ticket)
					backend.native_operations.burn_lifetime_ticket(adapter_ticket)
					_ = backend.release_renderer_root_ownership(seed)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				raw = C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_win32_dxgi_get_adapter(native_identity(backend.device),
					native_identity(backend.pending_init_dxgi_device), &raw)
				adapter := native_pointer(raw.handle)
				backend.bind_or_burn_com_lifetime_ticket(adapter_ticket, adapter)
				backend.pending_init_adapter = adapter
				backend.pending_init_adapter_ticket = if native_identity(adapter) == 0 {
					u64(0)
				} else {
					adapter_ticket
				}
				adapter_result := backend.accept_dxgi_result(adapter_context, mut ordinals,
					adapter_seed, native_identity(backend.device), raw, .none)
				if !adapter_result.succeeded() {
					backend.render_health = renderer_health_after_result(renderer_health_latch_unavailable(backend.render_health),
						adapter_result)
					backend.native_operations.record_health_latch(adapter_result.context,
						backend.render_health)
					backend.native_operations.burn_lifetime_ticket(factory_ticket)
					_ = backend.release_renderer_root_ownership(seed)
					return error(err_win32_d3d_device_failed)
				}
				dxgi_release := backend.release_identity(backend.pending_init_dxgi_device,
					backend.pending_init_dxgi_device_ticket, seed)
				backend.pending_init_dxgi_device = dxgi_release.value
				backend.pending_init_dxgi_device_ticket = dxgi_release.ticket_id
				if !dxgi_release.ticket_retired {
					backend.native_operations.burn_lifetime_ticket(factory_ticket)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				if backend.render_health.blocks_graphics() {
					backend.native_operations.burn_lifetime_ticket(factory_ticket)
					_ = backend.release_renderer_root_ownership(seed)
					return error(err_render_native_renderer_unavailable)
				}
				factory_seed :=
					seed.with_target_identity(native_identity(backend.pending_init_adapter))
				factory_context := ordinals.materialize(backend.native_operations, .dxgi,
					.factory_acquire, factory_seed) or {
					backend.native_operations.burn_lifetime_ticket(factory_ticket)
					_ = backend.release_renderer_root_ownership(seed)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				raw = C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_win32_dxgi_get_factory(native_identity(backend.device),
					native_identity(backend.pending_init_adapter), &raw)
				factory := native_pointer(raw.handle)
				backend.bind_or_burn_com_lifetime_ticket(factory_ticket, factory)
				backend.factory = factory
				backend.factory_ticket = if native_identity(factory) == 0 {
					u64(0)
				} else {
					factory_ticket
				}
				factory_result := backend.accept_dxgi_result(factory_context, mut ordinals,
					factory_seed, native_identity(backend.device), raw, .none)
				if !factory_result.succeeded() {
					backend.render_health = renderer_health_after_result(renderer_health_latch_unavailable(backend.render_health),
						factory_result)
					backend.native_operations.record_health_latch(factory_result.context,
						backend.render_health)
					_ = backend.release_renderer_root_ownership(seed)
					return error(err_win32_d3d_device_failed)
				}
				adapter_release := backend.release_identity(backend.pending_init_adapter,
					backend.pending_init_adapter_ticket, seed)
				backend.pending_init_adapter = adapter_release.value
				backend.pending_init_adapter_ticket = adapter_release.ticket_id
				if !adapter_release.ticket_retired {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return error(err_render_native_renderer_unavailable)
				}
				if backend.render_health.blocks_graphics() {
					_ = backend.release_renderer_root_ownership(seed)
					return error(err_render_native_renderer_unavailable)
				}
				backend.using_warp = used_warp
				backend.render_health = .ready
				return
			} $else {
				return error(err_backend_unsupported)
			}
		}

		fn (mut backend Win32Backend) shutdown_renderer() {
			$if windows {
				for i in 0 .. backend.windows.len {
					mut record := backend.windows[i]
					seed := win32_window_operation_seed(record.id, record.render_target_generation,
						.shutdown_release)
					if !backend.release_window_render_resources(i, seed) {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return
					}
				}
				anchor_seed := NativeOperationSeed{
					call_site: .shutdown_release
					scope:     .anchor
				}
				if !backend.release_anchor_ownership(anchor_seed) {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return
				}
				renderer_seed := NativeOperationSeed{
					call_site: .shutdown_release
					scope:     .renderer
				}
				if !backend.release_renderer_root_ownership(renderer_seed) {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return
				}
				backend.using_warp = false
			}
			backend.finish_renderer_shutdown_health()
		}

		fn (mut backend Win32Backend) create_window_views(index int, width int, height int, seed NativeOperationSeed) NativeRenderResult {
			$if windows {
				mut record := backend.windows[index]
				if (record.swapchain == unsafe { nil }) != (record.swapchain_ticket == 0)
					|| native_identity(record.pending_backbuffer) != 0
					|| record.pending_backbuffer_ticket != 0 || record.render_view != unsafe { nil }
					|| record.render_view_ticket != 0 || record.depth_texture != unsafe { nil }
					|| record.depth_texture_ticket != 0
					|| record.depth_stencil_view != unsafe { nil }
					|| record.depth_stencil_view_ticket != 0 {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .swapchain_create, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				mut ordinals := backend.native_operations.reserve_renderer_attempt_ordinals(8) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				mut transient_cleanup := backend.native_operations.reserve_renderer_attempt_ordinals(1) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				mut persistent_cleanup := backend.native_operations.reserve_app_lifetime_ordinals(3) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				backbuffer_ticket := backend.reserve_com_lifetime_ticket(mut transient_cleanup, seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				render_view_ticket := backend.reserve_com_lifetime_ticket(mut persistent_cleanup, seed) or {
					backend.native_operations.burn_lifetime_ticket(backbuffer_ticket)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				depth_texture_ticket := backend.reserve_com_lifetime_ticket(mut persistent_cleanup, seed) or {
					backend.native_operations.burn_lifetime_ticket(render_view_ticket)
					backend.native_operations.burn_lifetime_ticket(backbuffer_ticket)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				depth_view_ticket := backend.reserve_com_lifetime_ticket(mut persistent_cleanup, seed) or {
					backend.native_operations.burn_lifetime_ticket(depth_texture_ticket)
					backend.native_operations.burn_lifetime_ticket(render_view_ticket)
					backend.native_operations.burn_lifetime_ticket(backbuffer_ticket)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				backbuffer_seed := seed.with_target_identity(native_identity(record.swapchain))
				backbuffer_context := ordinals.materialize(backend.native_operations, .dxgi,
					.backbuffer_acquire, backbuffer_seed) or {
					backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
					backend.native_operations.burn_lifetime_ticket(depth_texture_ticket)
					backend.native_operations.burn_lifetime_ticket(render_view_ticket)
					backend.native_operations.burn_lifetime_ticket(backbuffer_ticket)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				mut raw := C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_win32_dxgi_get_backbuffer(native_identity(backend.device),
					native_identity(record.swapchain), &raw)
				backbuffer := native_pointer(raw.handle)
				backend.bind_or_burn_com_lifetime_ticket(backbuffer_ticket, backbuffer)
				record.pending_backbuffer = backbuffer
				record.pending_backbuffer_ticket = if native_identity(backbuffer) == 0 {
					u64(0)
				} else {
					backbuffer_ticket
				}
				backbuffer_result := backend.accept_dxgi_result(backbuffer_context, mut ordinals,
					backbuffer_seed, native_identity(backend.device), raw, .none)
				if !backbuffer_result.succeeded() {
					backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
					backend.native_operations.burn_lifetime_ticket(depth_texture_ticket)
					backend.native_operations.burn_lifetime_ticket(render_view_ticket)
					if !backend.release_window_views(index, seed) {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
					}
					return backbuffer_result
				}

				render_seed := seed.with_target_identity(native_identity(backbuffer))
				render_context := ordinals.materialize(backend.native_operations, .dxgi,
					.render_view_create, render_seed) or {
					backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
					backend.native_operations.burn_lifetime_ticket(depth_texture_ticket)
					backend.native_operations.burn_lifetime_ticket(render_view_ticket)
					_ = backend.release_window_views(index, seed)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .render_view_create, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				raw = C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_win32_d3d11_create_render_view(native_identity(backend.device),
					native_identity(backbuffer), &raw)
				render_view := native_pointer(raw.handle)
				backend.bind_or_burn_com_lifetime_ticket(render_view_ticket, render_view)
				record.render_view = render_view
				record.render_view_ticket = if native_identity(render_view) == 0 {
					u64(0)
				} else {
					render_view_ticket
				}
				render_view_result := backend.accept_dxgi_result(render_context, mut ordinals,
					render_seed, native_identity(backend.device), raw, .none)
				if !render_view_result.succeeded() {
					backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
					backend.native_operations.burn_lifetime_ticket(depth_texture_ticket)
					if !backend.release_window_views(index, seed) {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
					}
					return render_view_result
				}
				backbuffer_release := backend.release_identity(record.pending_backbuffer,
					record.pending_backbuffer_ticket, seed)
				record.pending_backbuffer = backbuffer_release.value
				record.pending_backbuffer_ticket = backbuffer_release.ticket_id
				if !backbuffer_release.ticket_retired {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				if backend.render_health.blocks_graphics() {
					backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
					backend.native_operations.burn_lifetime_ticket(depth_texture_ticket)
					_ = backend.release_window_views(index, seed)
					return native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}

				depth_texture_seed := seed.with_target_identity(native_identity(backend.device))
				depth_texture_context := ordinals.materialize(backend.native_operations, .dxgi,
					.depth_texture_create, depth_texture_seed) or {
					backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
					backend.native_operations.burn_lifetime_ticket(depth_texture_ticket)
					_ = backend.release_window_views(index, seed)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .depth_texture_create, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				raw = C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_win32_d3d11_create_texture(native_identity(backend.device), width,
					height, 1, &raw)
				depth_texture := native_pointer(raw.handle)
				backend.bind_or_burn_com_lifetime_ticket(depth_texture_ticket, depth_texture)
				record.depth_texture = depth_texture
				record.depth_texture_ticket = if native_identity(depth_texture) == 0 {
					u64(0)
				} else {
					depth_texture_ticket
				}
				depth_texture_result := backend.accept_dxgi_result(depth_texture_context, mut
					ordinals, depth_texture_seed, native_identity(backend.device), raw, .none)
				if !depth_texture_result.succeeded() {
					backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
					if !backend.release_window_views(index, seed) {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
					}
					return depth_texture_result
				}

				depth_view_seed := seed.with_target_identity(native_identity(depth_texture))
				depth_view_context := ordinals.materialize(backend.native_operations, .dxgi,
					.depth_view_create, depth_view_seed) or {
					backend.native_operations.burn_lifetime_ticket(depth_view_ticket)
					_ = backend.release_window_views(index, seed)
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .depth_view_create, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				raw = C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_win32_d3d11_create_depth_view(native_identity(backend.device),
					native_identity(depth_texture), &raw)
				depth_view := native_pointer(raw.handle)
				backend.bind_or_burn_com_lifetime_ticket(depth_view_ticket, depth_view)
				record.depth_stencil_view = depth_view
				record.depth_stencil_view_ticket = if native_identity(depth_view) == 0 {
					u64(0)
				} else {
					depth_view_ticket
				}
				depth_view_result := backend.accept_dxgi_result(depth_view_context, mut ordinals,
					depth_view_seed, native_identity(backend.device), raw, .none)
				if !depth_view_result.succeeded() {
					if !backend.release_window_views(index, seed) {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
					}
					return depth_view_result
				}
				return depth_view_result
			} $else {
				_ = backend
				_ = index
				_ = width
				_ = height
				_ = seed
				return native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
					.renderer_unavailable, 0, 0, err_backend_unsupported)
			}
		}

		fn (mut backend Win32Backend) release_window_views(index int, seed NativeOperationSeed) bool {
			mut record := backend.windows[index]
			depth_view := backend.release_identity(record.depth_stencil_view,
				record.depth_stencil_view_ticket, seed)
			record.depth_stencil_view = depth_view.value
			record.depth_stencil_view_ticket = depth_view.ticket_id
			if !depth_view.ticket_retired {
				return false
			}
			depth :=
				backend.release_identity(record.depth_texture, record.depth_texture_ticket, seed)
			record.depth_texture = depth.value
			record.depth_texture_ticket = depth.ticket_id
			if !depth.ticket_retired {
				return false
			}
			view := backend.release_identity(record.render_view, record.render_view_ticket, seed)
			record.render_view = view.value
			record.render_view_ticket = view.ticket_id
			if !view.ticket_retired {
				return false
			}
			backbuffer := backend.release_identity(record.pending_backbuffer,
				record.pending_backbuffer_ticket, seed)
			record.pending_backbuffer = backbuffer.value
			record.pending_backbuffer_ticket = backbuffer.ticket_id
			return backbuffer.ticket_retired
		}

		fn (mut backend Win32Backend) release_window_swapchain(index int, seed NativeOperationSeed) bool {
			mut record := backend.windows[index]
			swapchain := backend.release_identity(record.swapchain, record.swapchain_ticket, seed)
			record.swapchain = swapchain.value
			record.swapchain_ticket = swapchain.ticket_id
			return swapchain.ticket_retired
		}

		fn (mut backend Win32Backend) ensure_window_render_target(index int, seed NativeOperationSeed) NativeRenderResult {
			$if windows {
				if !backend.renderer_ready() {
					if backend.render_health.blocks_graphics() {
						disposition := if backend.render_health == .lost {
							NativeRenderDisposition.renderer_lost
						} else {
							NativeRenderDisposition.renderer_unavailable
						}
						return native_render_outcome(.dxgi, .device_status, .renderer, disposition,
							0, 0, err_render_native_renderer_lost)
					}
					return native_render_outcome(.dxgi, .device_status, .renderer,
						.renderer_unavailable, 0, 0, err_renderer_unsupported)
				}
				mut record := backend.windows[index]
				if (record.swapchain == unsafe { nil }) != (record.swapchain_ticket == 0) {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, .swapchain_create, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				if record.hwnd == unsafe { nil } {
					return native_render_outcome(.none, .swapchain_create, .window_target,
						.native_window_lost, 0, 0, err_window_not_found)
				}
				width := safe_win32_extent(if record.framebuffer_width > 0 {
					record.framebuffer_width
				} else {
					record.width
				})
				height := safe_win32_extent(if record.framebuffer_height > 0 {
					record.framebuffer_height
				} else {
					record.height
				})
				if record.swapchain == unsafe { nil } {
					mut ordinals := backend.native_operations.reserve_renderer_attempt_ordinals(4) or {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return native_render_outcome(.dxgi, .swapchain_create, .renderer,
							.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
					}
					mut cleanup := backend.native_operations.reserve_app_lifetime_ordinals(1) or {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return native_render_outcome(.dxgi, .swapchain_create, .renderer,
							.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
					}
					swapchain_ticket := backend.reserve_com_lifetime_ticket(mut cleanup, seed) or {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return native_render_outcome(.dxgi, .swapchain_create, .renderer,
							.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
					}
					create_seed := seed.with_target_identity(native_identity(record.hwnd))
					create_context := ordinals.materialize(backend.native_operations, .dxgi,
						.swapchain_create, create_seed) or {
						backend.native_operations.burn_lifetime_ticket(swapchain_ticket)
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return native_render_outcome(.dxgi, .swapchain_create, .renderer,
							.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
					}
					mut raw := C.VMultiwindowNativePrimitive{}
					C.v_multiwindow_win32_dxgi_create_swapchain(native_identity(backend.factory),
						native_identity(backend.device), native_identity(record.hwnd), width,
						height, &raw)
					swapchain := native_pointer(raw.handle)
					backend.bind_or_burn_com_lifetime_ticket(swapchain_ticket, swapchain)
					record.swapchain = swapchain
					record.swapchain_ticket = if native_identity(swapchain) == 0 {
						u64(0)
					} else {
						swapchain_ticket
					}
					creation := backend.accept_dxgi_result(create_context, mut ordinals,
						create_seed, native_identity(backend.device), raw, .none)
					if !creation.succeeded() {
						if !backend.release_window_swapchain(index, seed) {
							backend.render_health =
								renderer_health_latch_unavailable(backend.render_health)
						}
						return creation
					}
					association_seed := seed.with_target_identity(native_identity(record.hwnd))
					association_context := ordinals.materialize(backend.native_operations, .dxgi,
						.window_association, association_seed) or {
						if !backend.release_window_swapchain(index, seed) {
							backend.render_health =
								renderer_health_latch_unavailable(backend.render_health)
						}
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return native_render_outcome(.dxgi, .window_association, .renderer,
							.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
					}
					raw = C.VMultiwindowNativePrimitive{}
					C.v_multiwindow_win32_dxgi_make_window_association(native_identity(backend.factory),
						native_identity(backend.device), native_identity(record.hwnd), &raw)
					association := backend.accept_dxgi_result(association_context, mut ordinals,
						association_seed, native_identity(backend.device), raw, .none)
					if !association.succeeded() {
						if !backend.release_window_swapchain(index, seed) {
							backend.render_health =
								renderer_health_latch_unavailable(backend.render_health)
						}
						return association
					}
					views := backend.create_window_views(index, width, height, seed)
					if !views.succeeded() {
						if record.has_view_ownership()
							|| !backend.release_window_swapchain(index, seed) {
							backend.render_health =
								renderer_health_latch_unavailable(backend.render_health)
						}
						return views
					}
					record.render_resize_pending = false
					return views
				}
				if record.render_resize_pending {
					mut ordinals := backend.native_operations.reserve_ordinals(4) or {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return native_render_outcome(.dxgi, .resize_buffers, .renderer,
							.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
					}
					clear_seed := seed.with_target_identity(native_identity(backend.device_context))
					clear_context := ordinals.materialize(backend.native_operations, .dxgi,
						.clear_state, clear_seed) or {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return native_render_outcome(.dxgi, .clear_state, .renderer,
							.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
					}
					mut raw := C.VMultiwindowNativePrimitive{}
					C.v_multiwindow_win32_d3d11_clear_state(native_identity(backend.device_context),
						&raw)
					clear_result := backend.accept_dxgi_result(clear_context, mut ordinals,
						clear_seed, native_identity(backend.device), raw, .void_completion)
					if !backend.release_window_views(index, seed) {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return native_render_outcome(.dxgi, .object_release, .renderer,
							.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
					}
					if record.has_view_ownership() {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return native_render_outcome(.dxgi, .object_release, .renderer,
							.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
					}
					if backend.render_health.blocks_graphics() {
						if !backend.release_window_swapchain(index, seed) {
							backend.render_health =
								renderer_health_latch_unavailable(backend.render_health)
						}
						return native_render_outcome(.dxgi, .resize_buffers, .renderer,
							.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
					}
					if !clear_result.succeeded() {
						if !backend.release_window_swapchain(index, seed) {
							backend.render_health =
								renderer_health_latch_unavailable(backend.render_health)
						}
						return clear_result
					}
					resize_seed := seed.with_target_identity(native_identity(record.swapchain))
					resize_context := ordinals.materialize(backend.native_operations, .dxgi,
						.resize_buffers, resize_seed) or {
						if !backend.release_window_swapchain(index, seed) {
							backend.render_health =
								renderer_health_latch_unavailable(backend.render_health)
						}
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return native_render_outcome(.dxgi, .resize_buffers, .renderer,
							.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
					}
					raw = C.VMultiwindowNativePrimitive{}
					C.v_multiwindow_win32_dxgi_resize_buffers(native_identity(backend.device),
						native_identity(record.swapchain), width, height, &raw)
					resize_result := backend.accept_dxgi_result(resize_context, mut ordinals,
						resize_seed, native_identity(backend.device), raw, .none)
					if !resize_result.succeeded() {
						if !backend.release_window_swapchain(index, seed) {
							backend.render_health =
								renderer_health_latch_unavailable(backend.render_health)
						}
						return resize_result
					}
					views := backend.create_window_views(index, width, height, seed)
					if !views.succeeded() {
						if record.has_view_ownership()
							|| !backend.release_window_swapchain(index, seed) {
							backend.render_health =
								renderer_health_latch_unavailable(backend.render_health)
						}
						return views
					}
					if gfx.is_valid() && !backend.render_health.blocks_graphics() {
						gfx.reset_state_cache()
					}
					record.render_resize_pending = false
					return views
				}
				return native_render_ok(.dxgi, .backbuffer_acquire, .window_target)
			} $else {
				_ = index
				_ = seed
				return native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
					.renderer_unavailable, 0, 0, err_backend_unsupported)
			}
		}

		fn (backend &Win32Backend) swapchain_for_record(record &Win32WindowRecord) gfx.Swapchain {
			return gfx.Swapchain{
				width:        safe_win32_extent(if record.framebuffer_width > 0 {
					record.framebuffer_width
				} else {
					record.width
				})
				height:       safe_win32_extent(if record.framebuffer_height > 0 {
					record.framebuffer_height
				} else {
					record.height
				})
				sample_count: 1
				color_format: .bgra8
				depth_format: .depth_stencil
				d3d11:        gfx.D3d11Swapchain{
					render_view:        record.render_view
					depth_stencil_view: record.depth_stencil_view
				}
			}
		}

		fn (mut backend Win32Backend) release_window_render_resources(index int, seed NativeOperationSeed) bool {
			$if windows {
				mut record := backend.windows[index]
				if !backend.release_window_views(index, seed) {
					return false
				}
				if !backend.release_window_swapchain(index, seed) {
					return false
				}
				record.render_resize_pending = false
				return true
			} $else {
				_ = backend
				_ = index
				return false
			}
		}
	}

	$if !sokol_d3d11 ? {
		fn (mut backend Win32Backend) render_environment(id WindowId) !gfx.Environment {
			_ = backend
			_ = id
			return error(err_renderer_unsupported)
		}

		fn (mut backend Win32Backend) begin_render(id WindowId, candidate RenderWindowSnapshot, native_attempt NativeTargetAttempt) BackendFrameAttempt {
			_ = backend
			_ = id
			_ = candidate
			_ = native_attempt
			return BackendFrameAttempt{
				outcome: native_render_outcome(.dxgi, .backbuffer_acquire, .renderer,
					.renderer_unavailable, 0, 0, err_renderer_unsupported)
			}
		}

		fn (mut backend Win32Backend) end_render(frame RenderFrame) BackendFinalizeAttempt {
			_ = backend
			_ = frame
			return BackendFinalizeAttempt{
				status:  .not_presented
				outcome: native_render_outcome(.dxgi, .present, .renderer, .renderer_unavailable,
					0, 0, err_renderer_unsupported)
			}
		}

		fn (backend &Win32Backend) renderer_ready() bool {
			_ = backend
			return false
		}

		fn (mut backend Win32Backend) init_renderer() ! {
			_ = backend
			return error(err_renderer_unsupported)
		}

		fn (mut backend Win32Backend) shutdown_renderer() {
			backend.finish_renderer_shutdown_health()
		}

		fn (mut backend Win32Backend) release_window_render_resources(index int, seed NativeOperationSeed) bool {
			_ = seed
			backend.windows[index].render_resize_pending = false
			return true
		}
	}
} $else {
	fn (backend &Win32Backend) renderer_ready() bool {
		_ = backend
		return false
	}

	fn (mut backend Win32Backend) init_renderer() ! {
		_ = backend
		return error(err_renderer_unsupported)
	}

	fn (mut backend Win32Backend) shutdown_renderer() {
		backend.finish_renderer_shutdown_health()
	}

	fn (mut backend Win32Backend) release_window_render_resources(index int, seed NativeOperationSeed) bool {
		_ = seed
		backend.windows[index].render_resize_pending = false
		return true
	}
}

fn safe_win32_extent(value int) int {
	if value > 0 {
		return value
	}
	return 1
}

fn win32_bool_to_int(value bool) int {
	return if value { 1 } else { 0 }
}

fn win32_window_operation_seed(id WindowId, target_generation u64, call_site NativeRenderCallSite) NativeOperationSeed {
	return NativeOperationSeed{
		presence_mask:     native_context_has_window | native_context_has_target_generation
		call_site:         call_site
		scope:             .window_target
		window:            id
		target_generation: target_generation
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	$if windows && sokol_d3d11 ? {
		fn (mut backend Win32Backend) accept_dxgi_result(context NativeOperationContext, mut ordinals NativeOrdinalRange, seed NativeOperationSeed, device_identity u64, raw C.VMultiwindowNativePrimitive, validation NativeLocalValidation) NativeRenderResult {
			primary := backend.native_operations.capture_call(context, raw)
			mut capture := primary
			actual_validation := win32_actual_output_validation(context, raw, validation)
			effective_validation := native_validation_after_injection(context, primary.effective,
				actual_validation)
			if (dxgi_capture_requires_removal(primary)
				|| effective_validation !in [.none, .void_completion]) && device_identity != 0 {
				evidence_seed := seed.with_target_identity(device_identity)
				evidence_context := ordinals.materialize(backend.native_operations, .dxgi,
					.dxgi_removal_query, evidence_seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, context.operation, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
				mut evidence_raw := C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_win32_d3d11_get_removed_reason(device_identity, &evidence_raw)
				evidence := backend.native_operations.capture_evidence(evidence_context,
					evidence_raw)
				capture = native_capture_with_dxgi_removal(primary, evidence)
			} else {
				ordinals.skip(1) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return native_render_outcome(.dxgi, context.operation, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
			}
			result := backend.native_operations.accept_dxgi(context, capture, actual_validation)
			backend.render_health = renderer_health_after_result(backend.render_health, result)
			backend.native_operations.record_health_latch(result.context, backend.render_health)
			return result
		}
	}
}

fn win32_actual_output_validation(context NativeOperationContext, raw C.VMultiwindowNativePrimitive, requested NativeLocalValidation) NativeLocalValidation {
	if requested != .none || raw.valid_mask & native_valid_return_value == 0
		|| dxgi_hresult_failed(raw.return_value) {
		return requested
	}
	if context.operation in [.device_create, .swapchain_create, .backbuffer_acquire, .color_texture_create, .render_view_create, .depth_texture_create, .depth_view_create, .device_query, .adapter_acquire, .factory_acquire]
		&& (raw.valid_mask & native_valid_handle == 0 || raw.handle == 0) {
		return .null_output
	}
	if context.operation == .device_create
		&& (raw.valid_mask & native_valid_object_identity_0 == 0 || raw.object_identity_0 == 0) {
		return .null_output
	}
	return .none
}

fn (mut backend Win32Backend) reserve_com_lifetime_ticket(mut cleanup NativeOrdinalRange, seed NativeOperationSeed) !u64 {
	context := cleanup.materialize(backend.native_operations, .dxgi, .object_release,
		seed.without_target_identity())!
	return backend.native_operations.reserve_lifetime_ticket(context, .com_reference,
		seed.without_target_identity())
}

fn (mut backend Win32Backend) bind_or_burn_com_lifetime_ticket(ticket_id u64, value voidptr) {
	if value == unsafe { nil } {
		backend.native_operations.burn_lifetime_ticket(ticket_id)
		return
	}
	backend.native_operations.bind_lifetime_ticket(ticket_id, native_identity(value), 0)
}

fn (mut backend Win32Backend) release_identity(value voidptr, ticket_id u64, seed NativeOperationSeed) NativeLifetimeReleaseResult {
	pending := NativeLifetimeReleaseResult{
		value:     value
		ticket_id: ticket_id
	}
	$if windows && sokol_d3d11 ? {
		_ = seed
		if !backend.native_operations.owner_thread_is_current() {
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
				.com_reference) or { return pending }
			backend.native_operations.retire_native_released_lifetime_claim(retirement_claim)
			return NativeLifetimeReleaseResult{
				native_released: true
				ticket_retired:  true
			}
		}
		identity := native_identity(value)
		claim := backend.native_operations.claim_lifetime_release(ticket_id, .com_reference,
			identity, 0) or { return pending }
		mut raw := C.VMultiwindowNativePrimitive{}
		C.v_multiwindow_win32_release(identity, &raw)
		release_result := backend.native_operations.stage_lifetime_native_release(claim, raw,
			.void_completion, '')
		backend.record_lifetime_release_outcome(release_result)
		backend.native_operations.record_health_latch(claim.context, backend.render_health)
		backend.native_operations.retire_native_released_lifetime_claim(claim)
		return NativeLifetimeReleaseResult{
			native_released: true
			ticket_retired:  true
		}
	}
	return pending
}

fn (mut backend Win32Backend) release_raw_device_outputs(seed NativeOperationSeed) Win32RawDeviceOutputsReleaseResult {
	context := backend.release_identity(backend.device_context, backend.device_context_ticket, seed)
	backend.device_context = context.value
	backend.device_context_ticket = context.ticket_id
	if !context.ticket_retired {
		return Win32RawDeviceOutputsReleaseResult{
			context: context
			device:  NativeLifetimeReleaseResult{
				value:     backend.device
				ticket_id: backend.device_ticket
			}
		}
	}
	device := backend.release_identity(backend.device, backend.device_ticket, seed)
	backend.device = device.value
	backend.device_ticket = device.ticket_id
	return Win32RawDeviceOutputsReleaseResult{
		context: context
		device:  device
	}
}

fn (mut backend Win32Backend) release_anchor_ownership(seed NativeOperationSeed) bool {
	backend.anchor_committed = false
	depth_view := backend.release_identity(backend.anchor_depth_stencil_view,
		backend.anchor_depth_stencil_view_ticket, seed)
	backend.anchor_depth_stencil_view = depth_view.value
	backend.anchor_depth_stencil_view_ticket = depth_view.ticket_id
	if !depth_view.ticket_retired {
		return false
	}
	depth := backend.release_identity(backend.anchor_depth_texture,
		backend.anchor_depth_texture_ticket, seed)
	backend.anchor_depth_texture = depth.value
	backend.anchor_depth_texture_ticket = depth.ticket_id
	if !depth.ticket_retired {
		return false
	}
	view :=
		backend.release_identity(backend.anchor_render_view, backend.anchor_render_view_ticket, seed)
	backend.anchor_render_view = view.value
	backend.anchor_render_view_ticket = view.ticket_id
	if !view.ticket_retired {
		return false
	}
	color := backend.release_identity(backend.anchor_color_texture,
		backend.anchor_color_texture_ticket, seed)
	backend.anchor_color_texture = color.value
	backend.anchor_color_texture_ticket = color.ticket_id
	return color.ticket_retired
}

fn (mut backend Win32Backend) release_renderer_root_ownership(seed NativeOperationSeed) bool {
	factory := backend.release_identity(backend.factory, backend.factory_ticket, seed)
	backend.factory = factory.value
	backend.factory_ticket = factory.ticket_id
	if !factory.ticket_retired {
		return false
	}
	adapter := backend.release_identity(backend.pending_init_adapter,
		backend.pending_init_adapter_ticket, seed)
	backend.pending_init_adapter = adapter.value
	backend.pending_init_adapter_ticket = adapter.ticket_id
	if !adapter.ticket_retired {
		return false
	}
	dxgi_device := backend.release_identity(backend.pending_init_dxgi_device,
		backend.pending_init_dxgi_device_ticket, seed)
	backend.pending_init_dxgi_device = dxgi_device.value
	backend.pending_init_dxgi_device_ticket = dxgi_device.ticket_id
	if !dxgi_device.ticket_retired {
		return false
	}
	released := backend.release_raw_device_outputs(seed)
	return released.context.ticket_retired && released.device.ticket_retired
}

fn (mut backend Win32Backend) abandon_renderer_ownership() {
	backend.shutdown_renderer()
}

fn (mut backend Win32Backend) accept_native_render_window_loss(id WindowId) {
	index := backend.window_record_index(id) or { return }
	backend.windows[index].destroyed = true
}
