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
	fn C.v_multiwindow_appkit_create_metal_device(out_device &voidptr) int
	fn C.v_multiwindow_appkit_release_metal_device(device voidptr)
	fn C.v_multiwindow_appkit_create_window(device voidptr, title &char, width int, height int, min_width int, min_height int, resizable int, visible int, high_dpi int, borderless int, fullscreen int, out_state &voidptr, out_width &int, out_height &int, out_framebuffer_width &int, out_framebuffer_height &int) int
	fn C.v_multiwindow_appkit_destroy_window(state voidptr)
	fn C.v_multiwindow_appkit_release_window(state voidptr)
	fn C.v_multiwindow_appkit_set_window_title(state voidptr, title &char) int
	fn C.v_multiwindow_appkit_set_cursor_shape(state voidptr, shape int) int
	fn C.v_multiwindow_appkit_resize_window(state voidptr, width int, height int, out_width &int, out_height &int, out_framebuffer_width &int, out_framebuffer_height &int) int
	fn C.v_multiwindow_appkit_poll_events()
	fn C.v_multiwindow_appkit_take_queued_event(state voidptr, out_event &C.VMultiwindowAppKitQueuedEvent) int
	fn C.v_multiwindow_appkit_release_queued_event_resources(event &C.VMultiwindowAppKitQueuedEvent)
	fn C.v_multiwindow_appkit_begin_frame(state voidptr, device voidptr, out_drawable &voidptr, out_depth_texture &voidptr, out_framebuffer_width &int, out_framebuffer_height &int) int
	fn C.v_multiwindow_appkit_end_frame(state voidptr)
	fn C.v_multiwindow_appkit_abort_frame(state voidptr)
}

struct AppKitNativeQueuedEvent {
	sequence u64
	event    QueuedEvent
}

@[heap; markused]
struct AppKitWindowRecord {
	id    WindowId
	state voidptr
mut:
	width              int
	height             int
	framebuffer_width  int
	framebuffer_height int
}

struct AppKitBackend {
mut:
	started bool
	device  voidptr
	windows []AppKitWindowRecord
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

fn (backend &AppKitBackend) capabilities() Capabilities {
	return Capabilities{
		backend:            .appkit
		mock:               false
		native:             true
		multi_window:       true
		owner_queue:        true
		explicit_swapchain: appkit_metal_supported() && backend.renderer_ready()
		metal:              appkit_metal_supported() && backend.renderer_ready()
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
		if backend.started {
			return
		}
		if C.v_multiwindow_appkit_is_main_thread() == 0 {
			return error(err_appkit_main_thread_required)
		}
		if C.v_multiwindow_appkit_prepare_application() == 0 {
			return error(err_appkit_application_failed)
		}
		if require_renderer {
			if !appkit_metal_supported() {
				return error(err_renderer_unsupported)
			}
			backend.init_renderer()!
		}
		backend.started = true
		return
	} $else {
		_ = require_renderer
		return error(err_backend_unsupported)
	}
}

fn (backend &AppKitBackend) renderer_ready() bool {
	return appkit_metal_supported() && backend.device != unsafe { nil }
}

fn (mut backend AppKitBackend) init_renderer() ! {
	$if darwin {
		if !appkit_metal_supported() {
			return error(err_renderer_unsupported)
		}
		if backend.renderer_ready() {
			return
		}
		mut device := voidptr(unsafe { nil })
		if C.v_multiwindow_appkit_create_metal_device(&device) == 0 || device == unsafe { nil } {
			return error(err_appkit_metal_device_failed)
		}
		backend.device = device
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (mut backend AppKitBackend) create_window(id WindowId, config WindowConfig) !WindowSize {
	$if darwin {
		if !backend.started {
			return error(err_appkit_application_failed)
		}
		mut state := voidptr(unsafe { nil })
		mut width := 0
		mut height := 0
		mut framebuffer_width := 0
		mut framebuffer_height := 0
		if C.v_multiwindow_appkit_create_window(backend.device, &char(config.title.str), config.width, config.height, config.min_width, config.min_height, appkit_bool_to_int(config.resizable), appkit_bool_to_int(config.visible), appkit_bool_to_int(config.high_dpi), appkit_bool_to_int(config.borderless), appkit_bool_to_int(config.fullscreen), &state, &width, &height, &framebuffer_width, &framebuffer_height) == 0
			|| state == unsafe { nil } {
			return error(err_appkit_create_window_failed)
		}
		backend.windows << AppKitWindowRecord{
			id:                 id
			state:              state
			width:              width
			height:             height
			framebuffer_width:  framebuffer_width
			framebuffer_height: framebuffer_height
		}
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
	$if darwin {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		record := backend.windows[index]
		backend.release_window_resources(record)
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

fn (mut backend AppKitBackend) poll_events() ![]Event {
	queued_events := backend.poll_queued_events()!
	mut events := []Event{cap: queued_events.len}
	for event in queued_events {
		if event.kind == .lifecycle {
			events << event.lifecycle
		}
	}
	return events
}

fn (mut backend AppKitBackend) poll_queued_events() ![]QueuedEvent {
	mut native_events := []AppKitNativeQueuedEvent{}
	$if darwin {
		if !backend.started {
			return []QueuedEvent{}
		}
		C.v_multiwindow_appkit_poll_events()
		mut i := 0
		for i < backend.windows.len {
			record := backend.windows[i]
			mut destroyed := false
			for {
				mut native_event := C.VMultiwindowAppKitQueuedEvent{}
				if C.v_multiwindow_appkit_take_queued_event(record.state, &native_event) == 0 {
					break
				}
				if native_event.event_kind == 1 && native_event.lifecycle_kind == 2 {
					destroyed = true
				}
				if native_event.window_width > 0 && native_event.window_height > 0
					&& ((native_event.event_kind == 1 && native_event.lifecycle_kind == 3)
					|| (native_event.event_kind == 2 && native_event.input_kind == 12)) {
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
			if destroyed {
				C.v_multiwindow_appkit_release_window(record.state)
				backend.windows.delete(i)
				continue
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

fn (mut backend AppKitBackend) stop() ! {
	$if darwin {
		for backend.windows.len > 0 {
			record := backend.windows[0]
			backend.release_window_resources(record)
			backend.windows.delete(0)
		}
		if backend.device != unsafe { nil } {
			C.v_multiwindow_appkit_release_metal_device(backend.device)
		}
		backend.device = unsafe { nil }
		backend.started = false
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn (mut backend AppKitBackend) render_environment(id WindowId) !gfx.Environment {
		$if darwin {
			backend.window_record_index(id) or { return error(err_window_not_found) }
			if !appkit_metal_supported() {
				return error(err_renderer_unsupported)
			}
			if !backend.renderer_ready() {
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

	fn (mut backend AppKitBackend) begin_render(id WindowId) !RenderFrame {
		$if darwin {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			if !appkit_metal_supported() {
				return error(err_renderer_unsupported)
			}
			if !backend.renderer_ready() {
				return error(err_renderer_unsupported)
			}
			mut drawable := voidptr(unsafe { nil })
			mut depth_texture := voidptr(unsafe { nil })
			mut framebuffer_width := 0
			mut framebuffer_height := 0
			if C.v_multiwindow_appkit_begin_frame(backend.windows[index].state, backend.device, &drawable, &depth_texture, &framebuffer_width, &framebuffer_height) == 0
				|| drawable == unsafe { nil } || depth_texture == unsafe { nil } {
				return error(err_appkit_metal_drawable_failed)
			}
			backend.windows[index].framebuffer_width = framebuffer_width
			backend.windows[index].framebuffer_height = framebuffer_height
			return RenderFrame{
				window_id: id
				swapchain: gfx.Swapchain{
					width:        framebuffer_width
					height:       framebuffer_height
					sample_count: 1
					color_format: .bgra8
					depth_format: .depth_stencil
					metal:        gfx.MetalSwapchain{
						current_drawable:      drawable
						depth_stencil_texture: depth_texture
					}
				}
			}
		} $else {
			_ = id
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend AppKitBackend) end_render(frame RenderFrame) ! {
		$if darwin {
			if !appkit_metal_supported() {
				return error(err_renderer_unsupported)
			}
			index := backend.window_record_index(frame.window_id) or {
				return error(err_window_not_found)
			}
			C.v_multiwindow_appkit_end_frame(backend.windows[index].state)
			return
		} $else {
			_ = frame
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend AppKitBackend) abort_render(frame RenderFrame) ! {
		$if darwin {
			if !appkit_metal_supported() {
				return error(err_renderer_unsupported)
			}
			index := backend.window_record_index(frame.window_id) or {
				return error(err_window_not_found)
			}
			C.v_multiwindow_appkit_abort_frame(backend.windows[index].state)
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

fn (mut backend AppKitBackend) release_window_resources(record AppKitWindowRecord) {
	$if darwin {
		if record.state != unsafe { nil } {
			C.v_multiwindow_appkit_destroy_window(record.state)
			C.v_multiwindow_appkit_release_window(record.state)
		}
	}
}

fn appkit_bool_to_int(value bool) int {
	return if value { 1 } else { 0 }
}
