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
	hwnd                   voidptr
	config                 WindowConfig
	width                  int
	height                 int
	destroyed              bool
	render_resize_pending  bool
	suppress_resize_event  bool
	queued_events          []Win32NativeQueuedEvent
	mouse_x                f32
	mouse_y                f32
	mouse_dx               f32
	mouse_dy               f32
	mouse_pos_valid        bool
	iconified              bool
	pending_dropped_files  []string
	pending_drop_modifiers u32
	pending_high_surrogate u32
	suppress_control_char  u32
	swapchain              voidptr
	render_view            voidptr
	depth_texture          voidptr
	depth_stencil_view     voidptr
}

struct Win32Backend {
mut:
	started        bool
	device         voidptr
	device_context voidptr
	factory        voidptr
	windows        []&Win32WindowRecord
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

fn (mut backend Win32Backend) start(require_renderer bool) ! {
	$if windows {
		if backend.started {
			return
		}
		if C.v_multiwindow_win32_register_class() == 0 {
			return error(err_win32_register_class_failed)
		}
		if require_renderer {
			backend.init_renderer()!
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
		mut record := &Win32WindowRecord{
			id:     id
			config: config
			width:  config.width
			height: config.height
		}
		title := config.title.to_wide()
		record_data := unsafe { voidptr(record) }
		hwnd := C.v_multiwindow_win32_create_window(title, config.width, config.height,
			config.min_width, config.min_height, win32_bool_to_int(config.resizable),
			win32_bool_to_int(config.borderless), win32_bool_to_int(config.fullscreen),
			win32_bool_to_int(config.visible), record_data)
		if hwnd == unsafe { nil } {
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
		backend.windows << record
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
	$if windows {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		mut record := backend.windows[index]
		backend.release_window_render_resources(mut record)
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

fn (mut backend Win32Backend) poll_events() ![]Event {
	queued_events := backend.poll_queued_events()!
	mut events := []Event{cap: queued_events.len}
	for event in queued_events {
		if event.kind == .lifecycle {
			events << event.lifecycle
		}
	}
	return events
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
			if record.destroyed {
				backend.release_window_render_resources(mut record)
				backend.windows.delete(i)
				continue
			}
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

fn (mut backend Win32Backend) stop() ! {
	$if windows {
		if !backend.started {
			return
		}
		for backend.windows.len > 0 {
			mut record := backend.windows[0]
			backend.release_window_render_resources(mut record)
			if record.hwnd != unsafe { nil } {
				C.v_multiwindow_win32_destroy_window(record.hwnd)
				record.hwnd = unsafe { nil }
			}
			backend.windows.delete(0)
		}
		backend.shutdown_renderer()
		backend.started = false
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
				backend.ensure_window_render_target(index)!
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

		fn (mut backend Win32Backend) begin_render(id WindowId) !RenderFrame {
			$if windows {
				index := backend.window_record_index(id) or { return error(err_window_not_found) }
				backend.ensure_window_render_target(index)!
				record := backend.windows[index]
				return RenderFrame{
					window_id: id
					swapchain: backend.swapchain_for_record(record)
				}
			} $else {
				_ = id
				return error(err_backend_unsupported)
			}
		}

		fn (mut backend Win32Backend) end_render(frame RenderFrame) ! {
			$if windows {
				index := backend.window_record_index(frame.window_id) or {
					return error(err_window_not_found)
				}
				record := backend.windows[index]
				if record.swapchain == unsafe { nil } {
					return error(err_renderer_unsupported)
				}
				present_status := C.v_multiwindow_win32_d3d11_present(record.swapchain)
				win32_d3d11_check_present_status(present_status)!
				return
			} $else {
				_ = frame
				return error(err_backend_unsupported)
			}
		}

		fn (backend &Win32Backend) renderer_ready() bool {
			return backend.device != unsafe { nil } && backend.device_context != unsafe { nil }
				&& backend.factory != unsafe { nil }
		}

		fn (mut backend Win32Backend) init_renderer() ! {
			$if windows {
				if backend.renderer_ready() {
					return
				}
				mut device := voidptr(unsafe { nil })
				mut device_context := voidptr(unsafe { nil })
				mut factory := voidptr(unsafe { nil })
				if C.v_multiwindow_win32_d3d11_create_device(&device, &device_context, &factory) == 0 {
					return error(err_win32_d3d_device_failed)
				}
				backend.device = device
				backend.device_context = device_context
				backend.factory = factory
				return
			} $else {
				return error(err_backend_unsupported)
			}
		}

		fn (mut backend Win32Backend) shutdown_renderer() {
			$if windows {
				for i in 0 .. backend.windows.len {
					mut record := backend.windows[i]
					backend.release_window_render_resources(mut record)
				}
				C.v_multiwindow_win32_safe_release(&backend.factory)
				C.v_multiwindow_win32_safe_release(&backend.device_context)
				C.v_multiwindow_win32_safe_release(&backend.device)
			}
		}

		fn (mut backend Win32Backend) ensure_window_render_target(index int) ! {
			$if windows {
				if !backend.renderer_ready() {
					return error(err_renderer_unsupported)
				}
				mut record := backend.windows[index]
				if record.hwnd == unsafe { nil } {
					return error(err_window_not_found)
				}
				width := safe_win32_extent(record.width)
				height := safe_win32_extent(record.height)
				if record.swapchain == unsafe { nil } {
					mut swapchain := voidptr(unsafe { nil })
					if C.v_multiwindow_win32_d3d11_create_swapchain(backend.factory,
						backend.device, record.hwnd, width, height, &swapchain) == 0 {
						return error(err_win32_d3d_swapchain_failed)
					}
					record.swapchain = swapchain
					if C.v_multiwindow_win32_d3d11_create_views(backend.device, record.swapchain,
						width, height, &record.render_view, &record.depth_texture,
						&record.depth_stencil_view) == 0 {
						backend.release_window_render_resources(mut record)
						return error(err_win32_d3d_view_failed)
					}
					record.render_resize_pending = false
					return
				}
				if record.render_resize_pending {
					resize_ok := C.v_multiwindow_win32_d3d11_resize_swapchain(backend.device,
						backend.device_context, record.swapchain, width, height,
						&record.render_view, &record.depth_texture, &record.depth_stencil_view) != 0
					if gfx.is_valid() {
						gfx.reset_state_cache()
					}
					if !resize_ok {
						backend.release_window_render_resources(mut record)
						return error(err_win32_d3d_resize_failed)
					}
					record.render_resize_pending = false
				}
				return
			} $else {
				_ = index
				return error(err_backend_unsupported)
			}
		}

		fn (backend &Win32Backend) swapchain_for_record(record &Win32WindowRecord) gfx.Swapchain {
			return gfx.Swapchain{
				width:        safe_win32_extent(record.width)
				height:       safe_win32_extent(record.height)
				sample_count: 1
				color_format: .bgra8
				depth_format: .depth_stencil
				d3d11:        gfx.D3d11Swapchain{
					render_view:        record.render_view
					depth_stencil_view: record.depth_stencil_view
				}
			}
		}

		fn (mut backend Win32Backend) release_window_render_resources(mut record Win32WindowRecord) {
			$if windows {
				C.v_multiwindow_win32_d3d11_release_views(&record.render_view,
					&record.depth_texture, &record.depth_stencil_view)
				C.v_multiwindow_win32_safe_release(&record.swapchain)
				record.render_resize_pending = false
			} $else {
				_ = backend
				_ = record
			}
		}
	}

	$if !sokol_d3d11 ? {
		fn (mut backend Win32Backend) render_environment(id WindowId) !gfx.Environment {
			_ = backend
			_ = id
			return error(err_renderer_unsupported)
		}

		fn (mut backend Win32Backend) begin_render(id WindowId) !RenderFrame {
			_ = backend
			_ = id
			return error(err_renderer_unsupported)
		}

		fn (mut backend Win32Backend) end_render(frame RenderFrame) ! {
			_ = backend
			_ = frame
			return error(err_renderer_unsupported)
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
			_ = backend
		}

		fn (mut backend Win32Backend) release_window_render_resources(mut record Win32WindowRecord) {
			_ = backend
			record.render_resize_pending = false
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
		_ = backend
	}

	fn (mut backend Win32Backend) release_window_render_resources(mut record Win32WindowRecord) {
		_ = backend
		record.render_resize_pending = false
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

fn win32_d3d11_check_present_status(status int) ! {
	match status {
		0, 4 {
			return
		}
		2 {
			return error(err_win32_d3d_device_removed)
		}
		3 {
			return error(err_win32_d3d_device_reset)
		}
		else {
			return error(err_win32_d3d_present_failed)
		}
	}
}
