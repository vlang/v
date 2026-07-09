module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if linux && x_multiwindow_x11 ? {
	#flag linux -lX11
	#flag linux -lEGL
	#flag linux -lGL
	#include <X11/Xlib.h>
	#include <X11/Xatom.h>
	#insert "@VMODROOT/vlib/x/multiwindow/x11_egl_backend_helpers.h"
}

const x11_client_message = 33
const x11_configure_notify = 22
const x11_destroy_notify = 17
const x11_key_press = 2
const x11_key_release = 3
const x11_button_press = 4
const x11_button_release = 5
const x11_motion_notify = 6
const x11_enter_notify = 7
const x11_leave_notify = 8
const x11_focus_in = 9
const x11_focus_out = 10
const x11_property_notify = 28
const x11_selection_notify = 31
const x11_success = 0
const x11_scroll_up = 4
const x11_scroll_down = 5
const x11_scroll_right = 6
const x11_scroll_left = 7
const x11_invalid_mouse_button = 256
const x11_property_new_value = 0
const x11_prop_mode_replace = 0
const x11_normal_state = 1
const x11_iconic_state = 3
const x11_modifier_shift = u32(1)
const x11_modifier_ctrl = u32(2)
const x11_modifier_alt = u32(4)
const x11_modifier_super = u32(8)
const x11_key_v = 86
const x11_xdnd_version = 5
const x11_xdnd_max_payload_bytes = 1024 * 1024
const x11_xdnd_max_payload_units = (x11_xdnd_max_payload_bytes + 3) / 4
const x11_xdnd_max_type_atoms = 64
const x11_inline_char_codes = 8

$if x32 {
	type X11NativeLong = int
	type X11NativeULong = u32
} $else {
	type X11NativeLong = i64
	type X11NativeULong = u64
}

type X11NativeAtom = X11NativeULong
type X11NativeColormap = X11NativeULong
type X11NativeCursor = X11NativeULong
type X11NativeWindow = X11NativeULong

struct C.Display {}

$if linux && x_multiwindow_x11 ? {
	@[typedef]
	union C.XClientMessageData {
	mut:
		b [20]u8
		s [10]i16
		l [5]X11NativeLong
	}

	@[typedef]
	struct C.XClientMessageEvent {
	mut:
		@type        int
		serial       X11NativeULong
		send_event   int
		display      &C.Display = unsafe { nil }
		window       X11NativeWindow
		message_type X11NativeAtom
		format       int
		data         C.XClientMessageData
	}

	@[typedef]
	struct C.XDestroyWindowEvent {
	mut:
		@type      int
		serial     X11NativeULong
		send_event int
		display    &C.Display = unsafe { nil }
		event      X11NativeWindow
		window     X11NativeWindow
	}

	@[typedef]
	struct C.XConfigureEvent {
	mut:
		@type             int
		serial            X11NativeULong
		send_event        int
		display           &C.Display = unsafe { nil }
		event             X11NativeWindow
		window            X11NativeWindow
		x                 int
		y                 int
		width             int
		height            int
		border_width      int
		above             X11NativeWindow
		override_redirect int
	}

	@[typedef]
	struct C.XSelectionEvent {
	mut:
		@type      int
		serial     X11NativeULong
		send_event int
		display    &C.Display = unsafe { nil }
		requestor  X11NativeWindow
		selection  X11NativeAtom
		target     X11NativeAtom
		property   X11NativeAtom
		time       X11NativeULong
	}

	@[typedef]
	union C.XEvent {
	mut:
		@type          int
		xclient        C.XClientMessageEvent
		xconfigure     C.XConfigureEvent
		xdestroywindow C.XDestroyWindowEvent
		xselection     C.XSelectionEvent
		pad            [24]X11NativeLong
	}

	fn C.XInitThreads() int
	fn C.XOpenDisplay(name &char) &C.Display
	fn C.XCloseDisplay(display &C.Display) int
	fn C.XDefaultScreen(display &C.Display) int
	fn C.XDefaultRootWindow(display &C.Display) X11NativeWindow
	fn C.XStoreName(display &C.Display, window X11NativeWindow, name &char) int
	fn C.XInternAtom(display &C.Display, name &char, only_if_exists int) X11NativeAtom
	fn C.XSetWMProtocols(display &C.Display, window X11NativeWindow, protocols &X11NativeAtom, count int) int
	fn C.XCreateSimpleWindow(display &C.Display, parent X11NativeWindow, x int, y int, width u32, height u32, border_width u32, border X11NativeULong, background X11NativeULong) X11NativeWindow
	fn C.XSelectInput(display &C.Display, window X11NativeWindow, event_mask X11NativeLong) int
	fn C.XMapWindow(display &C.Display, window X11NativeWindow) int
	fn C.XResizeWindow(display &C.Display, window X11NativeWindow, width u32, height u32) int
	fn C.XDestroyWindow(display &C.Display, window X11NativeWindow) int
	fn C.XDefineCursor(display &C.Display, window X11NativeWindow, cursor X11NativeCursor) int
	fn C.XUndefineCursor(display &C.Display, window X11NativeWindow) int
	fn C.XFreeCursor(display &C.Display, cursor X11NativeCursor) int
	fn C.XFreeColormap(display &C.Display, colormap X11NativeColormap) int
	fn C.XFlush(display &C.Display) int
	fn C.XSync(display &C.Display, discard int) int
	fn C.XPending(display &C.Display) int
	fn C.XNextEvent(display &C.Display, event &C.XEvent) int
	fn C.XGetWindowProperty(display &C.Display, window X11NativeWindow, property X11NativeAtom, long_offset X11NativeLong, long_length X11NativeLong, delete int, req_type X11NativeAtom, actual_type_return &X11NativeAtom, actual_format_return &int, nitems_return &X11NativeULong, bytes_after_return &X11NativeULong, prop_return &&u8) int
	fn C.XChangeProperty(display &C.Display, window X11NativeWindow, property X11NativeAtom, @type X11NativeAtom, format int, mode int, data &u8, nelements int) int
	fn C.XConvertSelection(display &C.Display, selection X11NativeAtom, target X11NativeAtom, property X11NativeAtom, requestor X11NativeWindow, time X11NativeULong) int
	fn C.XSendEvent(display &C.Display, window X11NativeWindow, propagate int, event_mask X11NativeLong, event_send &C.XEvent) int
	fn C.XTranslateCoordinates(display &C.Display, src_w X11NativeWindow, dest_w X11NativeWindow, src_x int, src_y int, dest_x_return &int, dest_y_return &int, child_return &X11NativeWindow) int
	fn C.XFilterEvent(event &C.XEvent, window X11NativeWindow) int
	fn C.XFree(data voidptr) int
	fn C.v_multiwindow_x11_event_mask() X11NativeLong
	fn C.v_multiwindow_x11_event_window(event &C.XEvent) X11NativeWindow
	fn C.v_multiwindow_x11_event_x(event &C.XEvent) int
	fn C.v_multiwindow_x11_event_y(event &C.XEvent) int
	fn C.v_multiwindow_x11_event_state(event &C.XEvent) u32
	fn C.v_multiwindow_x11_event_keycode(event &C.XEvent) u32
	fn C.v_multiwindow_x11_event_button(event &C.XEvent) u32
	fn C.v_multiwindow_x11_property_atom(event &C.XEvent) X11NativeAtom
	fn C.v_multiwindow_x11_property_state(event &C.XEvent) int
	fn C.v_multiwindow_x11_focus_mode(event &C.XEvent) int
	fn C.v_multiwindow_x11_is_notify_grab_or_ungrab(mode int) int
	fn C.v_multiwindow_x11_enable_detectable_auto_repeat(display &C.Display) int
	fn C.v_multiwindow_x11_is_auto_repeat_release(display &C.Display, event &C.XEvent) int
	fn C.v_multiwindow_x11_modifiers(state u32) int
	fn C.v_multiwindow_x11_key_modifier_bit(key_code int) int
	fn C.v_multiwindow_x11_mouse_button(button u32) int
	fn C.v_multiwindow_x11_button_modifier_bit(mouse_button int) int
	fn C.v_multiwindow_x11_open_im(display &C.Display) voidptr
	fn C.v_multiwindow_x11_close_im(im voidptr)
	fn C.v_multiwindow_x11_create_ic(im voidptr, window X11NativeWindow) voidptr
	fn C.v_multiwindow_x11_destroy_ic(ic voidptr)
	fn C.v_multiwindow_x11_set_ic_focus(ic voidptr)
	fn C.v_multiwindow_x11_unset_ic_focus(ic voidptr)
	fn C.v_multiwindow_x11_init_keycodes(display &C.Display, keycodes &int, keycodes_len int)
	fn C.v_multiwindow_x11_key_code(event &C.XEvent, keycodes &int, keycodes_len int) int
	fn C.v_multiwindow_x11_char_codes(ic voidptr, event &C.XEvent, codes &u32, codes_len int, required_codes &int) int
	fn C.v_multiwindow_x11_create_cursor_for_shape(display &C.Display, shape int) X11NativeCursor
	fn C.v_multiwindow_x11_apply_config_hints(display &C.Display, window X11NativeWindow, width int, height int, min_width int, min_height int, resizable int, borderless int, fullscreen int) int
	fn C.v_multiwindow_x11_get_window_size(display &C.Display, window X11NativeWindow, out_width &int, out_height &int) int
	fn C.v_multiwindow_x11_egl_get_display(display &C.Display) voidptr
	fn C.v_multiwindow_x11_egl_initialize(egl_display voidptr) int
	fn C.v_multiwindow_x11_egl_bind_opengl_api() int
	fn C.v_multiwindow_x11_egl_choose_config(egl_display voidptr, out_config &voidptr, out_visual_id &int) int
	fn C.v_multiwindow_x11_egl_create_context(egl_display voidptr, egl_config voidptr) voidptr
	fn C.v_multiwindow_x11_create_egl_window(display &C.Display, root X11NativeWindow, screen int, native_visual_id int, width int, height int, out_colormap &X11NativeColormap) X11NativeWindow
	fn C.v_multiwindow_x11_egl_create_window_surface(egl_display voidptr, egl_config voidptr, window X11NativeWindow) voidptr
	fn C.v_multiwindow_x11_egl_make_current(egl_display voidptr, egl_surface voidptr, egl_context voidptr) int
	fn C.v_multiwindow_x11_egl_clear_current(egl_display voidptr)
	fn C.v_multiwindow_x11_egl_swap_buffers(egl_display voidptr, egl_surface voidptr) int
	fn C.v_multiwindow_x11_egl_destroy_surface(egl_display voidptr, egl_surface voidptr)
	fn C.v_multiwindow_x11_egl_destroy_context(egl_display voidptr, egl_context voidptr)
	fn C.v_multiwindow_x11_egl_terminate(egl_display voidptr)
}

struct X11WindowRecord {
	id          WindowId
	window      X11NativeWindow
	colormap    X11NativeColormap
	xic         voidptr
	egl_surface voidptr
mut:
	cursor          X11NativeCursor
	config          WindowConfig
	cursor_shape    CursorShape
	width           int
	height          int
	mouse_x         f32
	mouse_y         f32
	mouse_dx        f32
	mouse_dy        f32
	mouse_pos_valid bool
	mouse_buttons   u8
	key_repeat      [256]bool
	window_state    int
}

struct X11Backend {
mut:
	display          &C.Display = unsafe { nil }
	screen           int
	root             X11NativeWindow
	wm_protocols     X11NativeAtom
	wm_delete_window X11NativeAtom
	wm_state         X11NativeAtom
	xdnd_aware       X11NativeAtom
	xdnd_enter       X11NativeAtom
	xdnd_position    X11NativeAtom
	xdnd_status      X11NativeAtom
	xdnd_action_copy X11NativeAtom
	xdnd_drop        X11NativeAtom
	xdnd_leave       X11NativeAtom
	xdnd_finished    X11NativeAtom
	xdnd_selection   X11NativeAtom
	xdnd_type_list   X11NativeAtom
	text_uri_list    X11NativeAtom
	xdnd_source      X11NativeWindow
	xdnd_target      X11NativeWindow
	xdnd_format      X11NativeAtom
	xdnd_version     X11NativeLong
	xim              voidptr
	egl_display      voidptr
	egl_config       voidptr
	egl_context      voidptr
	native_visual_id int
	started          bool
	windows          []X11WindowRecord
	keycodes         [256]int
}

fn new_x11_backend() X11Backend {
	return X11Backend{}
}

fn (backend &X11Backend) ensure_supported() ! {
	$if linux && x_multiwindow_x11 ? {
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (backend &X11Backend) capabilities() Capabilities {
	return Capabilities{
		backend:            .x11
		mock:               false
		native:             true
		multi_window:       true
		owner_queue:        true
		explicit_swapchain: backend.renderer_ready()
		x11:                true
		gl:                 backend.renderer_ready()
		input_events:       true
		mouse_events:       true
		keyboard_events:    true
		text_events:        true
		focus_events:       true
		drop_events:        true
		touch_events:       false
		cursor_shapes:      true
		native_decorations: true
	}
}

fn (mut backend X11Backend) start(require_renderer bool) ! {
	$if linux && x_multiwindow_x11 ? {
		if backend.started {
			return
		}
		C.XInitThreads()
		display := C.XOpenDisplay(unsafe { nil })
		if display == unsafe { nil } {
			return error(err_x11_open_display_failed)
		}
		backend.display = display
		backend.screen = C.XDefaultScreen(display)
		backend.root = C.XDefaultRootWindow(display)
		backend.wm_protocols = C.XInternAtom(display, c'WM_PROTOCOLS', 0)
		backend.wm_delete_window = C.XInternAtom(display, c'WM_DELETE_WINDOW', 0)
		backend.wm_state = C.XInternAtom(display, c'WM_STATE', 0)
		backend.xdnd_aware = C.XInternAtom(display, c'XdndAware', 0)
		backend.xdnd_enter = C.XInternAtom(display, c'XdndEnter', 0)
		backend.xdnd_position = C.XInternAtom(display, c'XdndPosition', 0)
		backend.xdnd_status = C.XInternAtom(display, c'XdndStatus', 0)
		backend.xdnd_action_copy = C.XInternAtom(display, c'XdndActionCopy', 0)
		backend.xdnd_drop = C.XInternAtom(display, c'XdndDrop', 0)
		backend.xdnd_leave = C.XInternAtom(display, c'XdndLeave', 0)
		backend.xdnd_finished = C.XInternAtom(display, c'XdndFinished', 0)
		backend.xdnd_selection = C.XInternAtom(display, c'XdndSelection', 0)
		backend.xdnd_type_list = C.XInternAtom(display, c'XdndTypeList', 0)
		backend.text_uri_list = C.XInternAtom(display, c'text/uri-list', 0)
		backend.xim = C.v_multiwindow_x11_open_im(display)
		C.v_multiwindow_x11_init_keycodes(display, &backend.keycodes[0], 256)
		C.v_multiwindow_x11_enable_detectable_auto_repeat(display)
		if require_renderer {
			backend.init_renderer() or {
				if backend.xim != unsafe { nil } {
					C.v_multiwindow_x11_close_im(backend.xim)
					backend.xim = unsafe { nil }
				}
				C.XCloseDisplay(display)
				backend.display = unsafe { nil }
				return err
			}
		}
		backend.started = true
		return
	} $else {
		_ = require_renderer
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) create_window(id WindowId, config WindowConfig) !WindowSize {
	$if linux && x_multiwindow_x11 ? {
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_x11_open_display_failed)
		}
		mut colormap := X11NativeColormap(0)
		renderer_ready := backend.renderer_ready()
		mut actual_size := window_size_for_config(config, config.width, config.height)
		window := if renderer_ready {
			C.v_multiwindow_x11_create_egl_window(backend.display, backend.root, backend.screen,
				backend.native_visual_id, actual_size.width, actual_size.height, &colormap)
		} else {
			created := C.XCreateSimpleWindow(backend.display, backend.root, 0, 0,
				u32(actual_size.width), u32(actual_size.height), 0, 0, 0)
			if created != X11NativeWindow(0) {
				C.XSelectInput(backend.display, created, C.v_multiwindow_x11_event_mask())
			}
			created
		}
		if window == X11NativeWindow(0) {
			return error(err_x11_create_window_failed)
		}
		mut egl_surface := voidptr(unsafe { nil })
		if renderer_ready {
			egl_surface = C.v_multiwindow_x11_egl_create_window_surface(backend.egl_display,
				backend.egl_config, window)
		}
		if renderer_ready && egl_surface == unsafe { nil } {
			backend.destroy_native_window(window, colormap) or {
				return error(err_x11_egl_surface_failed)
			}
			return error(err_x11_egl_surface_failed)
		}
		C.XStoreName(backend.display, window, &char(config.title.str))
		if backend.wm_protocols != X11NativeAtom(0) && backend.wm_delete_window != X11NativeAtom(0) {
			protocols := [backend.wm_delete_window]
			if C.XSetWMProtocols(backend.display, window, &protocols[0], 1) == 0 {
				backend.release_window_resources(X11WindowRecord{
					id:          id
					window:      window
					colormap:    colormap
					egl_surface: egl_surface
					config:      window_config_with_size(config, actual_size.width,
						actual_size.height)
					width:       actual_size.width
					height:      actual_size.height
				}, true) or {}
				return error(err_x11_set_wm_protocols_failed)
			}
		}
		if C.v_multiwindow_x11_apply_config_hints(backend.display, window, actual_size.width,
			actual_size.height, config.min_width, config.min_height,
			x11_bool_to_int(config.resizable), x11_bool_to_int(config.borderless),
			x11_bool_to_int(config.fullscreen)) == 0 {
			backend.release_window_resources(X11WindowRecord{
				id:          id
				window:      window
				colormap:    colormap
				egl_surface: egl_surface
				config:      window_config_with_size(config, actual_size.width, actual_size.height)
				width:       actual_size.width
				height:      actual_size.height
			}, true) or {}
			return error(err_x11_create_window_failed)
		}
		backend.announce_xdnd_for_window(window)
		xic := C.v_multiwindow_x11_create_ic(backend.xim, window)
		if config.visible {
			C.XMapWindow(backend.display, window)
		}
		if C.XSync(backend.display, 0) == 0 {
			backend.release_window_resources(X11WindowRecord{
				id:          id
				window:      window
				colormap:    colormap
				xic:         xic
				egl_surface: egl_surface
				config:      window_config_with_size(config, actual_size.width, actual_size.height)
				width:       actual_size.width
				height:      actual_size.height
			}, true) or {}
			return error(err_x11_create_window_failed)
		}
		mut actual_width := 0
		mut actual_height := 0
		if C.v_multiwindow_x11_get_window_size(backend.display, window, &actual_width,
			&actual_height) == 0 {
			backend.release_window_resources(X11WindowRecord{
				id:          id
				window:      window
				colormap:    colormap
				xic:         xic
				egl_surface: egl_surface
				config:      window_config_with_size(config, actual_size.width, actual_size.height)
				width:       actual_size.width
				height:      actual_size.height
			}, true) or {}
			return error(err_x11_create_window_failed)
		}
		actual_size = WindowSize{
			width:  actual_width
			height: actual_height
		}
		backend.windows << X11WindowRecord{
			id:           id
			window:       window
			colormap:     colormap
			xic:          xic
			egl_surface:  egl_surface
			config:       window_config_with_size(config, actual_size.width, actual_size.height)
			width:        actual_size.width
			height:       actual_size.height
			window_state: backend.window_state(window)
		}
		return actual_size
	} $else {
		_ = id
		_ = config
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) destroy_window(id WindowId) ! {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		record := backend.windows[index]
		backend.release_window_resources(record, true)!
		backend.windows.delete(index)
		return
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) set_window_title(id WindowId, title string) ! {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_x11_open_display_failed)
		}
		window := backend.windows[index].window
		if C.XStoreName(backend.display, window, &char(title.str)) == 0 {
			return error(err_x11_set_window_title_failed)
		}
		C.XFlush(backend.display)
		return
	} $else {
		_ = id
		_ = title
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) resize_window(id WindowId, width int, height int) !WindowSize {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_x11_open_display_failed)
		}
		window := backend.windows[index].window
		config := backend.windows[index].config
		if !config.resizable {
			return error(err_capability_unsupported)
		}
		requested_size := window_size_for_config(config, width, height)
		if C.XResizeWindow(backend.display, window, u32(requested_size.width),
			u32(requested_size.height)) == 0 {
			return error(err_x11_resize_window_failed)
		}
		if C.XSync(backend.display, 0) == 0 {
			return error(err_x11_resize_window_failed)
		}
		mut actual_width := 0
		mut actual_height := 0
		if C.v_multiwindow_x11_get_window_size(backend.display, window, &actual_width,
			&actual_height) == 0 {
			return error(err_x11_resize_window_failed)
		}
		actual_size := WindowSize{
			width:  actual_width
			height: actual_height
		}
		backend.windows[index].width = actual_size.width
		backend.windows[index].height = actual_size.height
		backend.windows[index].config = window_config_with_size(backend.windows[index].config,
			actual_size.width, actual_size.height)
		return actual_size
	} $else {
		_ = id
		_ = width
		_ = height
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) set_window_cursor(id WindowId, shape CursorShape) ! {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_x11_open_display_failed)
		}
		if backend.windows[index].cursor_shape == shape {
			return
		}
		window := backend.windows[index].window
		if shape == .default {
			C.XUndefineCursor(backend.display, window)
			backend.free_cursor_for_record(index)
		} else {
			cursor := C.v_multiwindow_x11_create_cursor_for_shape(backend.display, int(shape))
			if cursor == X11NativeCursor(0) {
				return error(err_capability_unsupported)
			}
			if C.XDefineCursor(backend.display, window, cursor) == 0 {
				C.XFreeCursor(backend.display, cursor)
				return error(err_capability_unsupported)
			}
			backend.free_cursor_for_record(index)
			backend.windows[index].cursor = cursor
		}
		backend.windows[index].cursor_shape = shape
		C.XFlush(backend.display)
		return
	} $else {
		_ = id
		_ = shape
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) poll_events() ![]Event {
	queued_events := backend.poll_queued_events()!
	mut events := []Event{cap: queued_events.len}
	for event in queued_events {
		if event.kind == .lifecycle {
			events << event.lifecycle
		}
	}
	return events
}

fn (mut backend X11Backend) poll_queued_events() ![]QueuedEvent {
	mut events := []QueuedEvent{}
	$if linux && x_multiwindow_x11 ? {
		if !backend.started || backend.display == unsafe { nil } {
			return events
		}
		for C.XPending(backend.display) > 0 {
			mut event := C.XEvent{}
			C.XNextEvent(backend.display, &event)
			if C.XFilterEvent(&event, X11NativeWindow(0)) != 0 {
				continue
			}
			event_type := unsafe { event.@type }
			match event_type {
				x11_client_message {
					message_type := unsafe { event.xclient.message_type }
					format := unsafe { event.xclient.format }
					protocol := unsafe { X11NativeAtom(event.xclient.data.l[0]) }
					if message_type == backend.wm_protocols && format == 32
						&& protocol == backend.wm_delete_window {
						native_window := unsafe { event.xclient.window }
						id := backend.window_id_for_native(native_window) or { continue }
						events << queued_lifecycle_event(Event{
							kind:      .window_close_requested
							window_id: id
						})
					} else if format == 32 {
						events << backend.queued_xdnd_client_message_events(&event)
					}
				}
				x11_configure_notify {
					native_window := unsafe { event.xconfigure.window }
					index := backend.window_record_index_for_native(native_window) or { continue }
					width := unsafe { event.xconfigure.width }
					height := unsafe { event.xconfigure.height }
					if width > 0 && height > 0 && (backend.windows[index].width != width
						|| backend.windows[index].height != height) {
						id := backend.windows[index].id
						backend.windows[index].width = width
						backend.windows[index].height = height
						record := backend.windows[index]
						events << queued_lifecycle_event(Event{
							kind:      .window_resized
							window_id: id
							width:     width
							height:    height
						})
						events << queued_input_event(backend.input_event_from_record(record,
							.resized))
					}
				}
				x11_destroy_notify {
					native_window := unsafe { event.xdestroywindow.window }
					id := backend.remove_native_window(native_window) or { continue }
					events << queued_lifecycle_event(Event{
						kind:      .window_destroyed
						window_id: id
					})
				}
				x11_key_press {
					index := backend.window_record_index_for_event(&event) or { continue }
					events << backend.queued_key_press_event(index, &event)
				}
				x11_key_release {
					if C.v_multiwindow_x11_is_auto_repeat_release(backend.display, &event) != 0 {
						continue
					}
					index := backend.window_record_index_for_event(&event) or { continue }
					events << backend.queued_key_release_event(index, &event)
				}
				x11_button_press {
					index := backend.window_record_index_for_event(&event) or { continue }
					events << backend.queued_button_press_event(index, &event)
				}
				x11_button_release {
					index := backend.window_record_index_for_event(&event) or { continue }
					events << backend.queued_button_release_event(index, &event)
				}
				x11_motion_notify {
					index := backend.window_record_index_for_event(&event) or { continue }
					events << backend.queued_mouse_position_event(index, &event, .mouse_move, false)
				}
				x11_enter_notify {
					index := backend.window_record_index_for_event(&event) or { continue }
					if backend.windows[index].mouse_buttons == 0 {
						events << backend.queued_mouse_position_event(index, &event, .mouse_enter, true)
					}
				}
				x11_leave_notify {
					index := backend.window_record_index_for_event(&event) or { continue }
					if backend.windows[index].mouse_buttons == 0 {
						events << backend.queued_mouse_position_event(index, &event, .mouse_leave, true)
					}
				}
				x11_focus_in {
					if C.v_multiwindow_x11_is_notify_grab_or_ungrab(C.v_multiwindow_x11_focus_mode(&event)) != 0 {
						continue
					}
					index := backend.window_record_index_for_event(&event) or { continue }
					C.v_multiwindow_x11_set_ic_focus(backend.windows[index].xic)
					events << queued_input_event(backend.input_event_from_record(backend.windows[index],
						.focused))
				}
				x11_focus_out {
					if C.v_multiwindow_x11_is_notify_grab_or_ungrab(C.v_multiwindow_x11_focus_mode(&event)) != 0 {
						continue
					}
					index := backend.window_record_index_for_event(&event) or { continue }
					C.v_multiwindow_x11_unset_ic_focus(backend.windows[index].xic)
					backend.clear_input_state(index)
					events << queued_input_event(backend.input_event_from_record(backend.windows[index],
						.unfocused))
				}
				x11_property_notify {
					if C.v_multiwindow_x11_property_state(&event) != x11_property_new_value
						|| C.v_multiwindow_x11_property_atom(&event) != backend.wm_state {
						continue
					}
					index := backend.window_record_index_for_event(&event) or { continue }
					state := backend.window_state(backend.windows[index].window)
					if state == backend.windows[index].window_state {
						continue
					}
					backend.windows[index].window_state = state
					if state == x11_iconic_state {
						events << queued_input_event(backend.input_event_from_record(backend.windows[index],
							.iconified))
					} else if state == x11_normal_state {
						events << queued_input_event(backend.input_event_from_record(backend.windows[index],
							.restored))
					}
				}
				x11_selection_notify {
					events << backend.queued_xdnd_selection_events(&event)
				}
				else {}
			}
		}
	}
	return events
}

fn (backend &X11Backend) input_event_from_record(record X11WindowRecord, kind InputEventKind) InputEvent {
	return backend.input_event_with_payload(record, kind, 0, false, 0, x11_invalid_mouse_button, 0,
		0)
}

fn (backend &X11Backend) input_event_with_payload(record X11WindowRecord, kind InputEventKind, key_code int, key_repeat bool, modifiers u32, mouse_button int, scroll_x f32, scroll_y f32) InputEvent {
	return InputEvent{
		kind:               kind
		window_id:          record.id
		key_code:           key_code
		key_repeat:         key_repeat
		modifiers:          modifiers
		mouse_x:            record.mouse_x
		mouse_y:            record.mouse_y
		mouse_dx:           record.mouse_dx
		mouse_dy:           record.mouse_dy
		mouse_button:       mouse_button
		scroll_x:           scroll_x
		scroll_y:           scroll_y
		window_width:       record.width
		window_height:      record.height
		framebuffer_width:  record.width
		framebuffer_height: record.height
	}
}

fn (backend &X11Backend) input_char_event(record X11WindowRecord, char_code u32, key_repeat bool, modifiers u32) InputEvent {
	return InputEvent{
		kind:               .char
		window_id:          record.id
		char_code:          char_code
		key_repeat:         key_repeat
		modifiers:          modifiers
		window_width:       record.width
		window_height:      record.height
		framebuffer_width:  record.width
		framebuffer_height: record.height
	}
}

fn (backend &X11Backend) input_files_dropped_event(record X11WindowRecord, files []string) InputEvent {
	return InputEvent{
		kind:               .files_dropped
		window_id:          record.id
		mouse_x:            record.mouse_x
		mouse_y:            record.mouse_y
		mouse_dx:           record.mouse_dx
		mouse_dy:           record.mouse_dy
		window_width:       record.width
		window_height:      record.height
		framebuffer_width:  record.width
		framebuffer_height: record.height
		mouse_button:       x11_invalid_mouse_button
		dropped_files:      files.clone()
	}
}

$if linux && x_multiwindow_x11 ? {
	fn (mut backend X11Backend) clear_input_state(index int) {
		backend.windows[index].mouse_buttons = 0
		for i in 0 .. 256 {
			backend.windows[index].key_repeat[i] = false
		}
	}

	fn (mut backend X11Backend) queued_key_press_event(index int, event &C.XEvent) []QueuedEvent {
		mut events := []QueuedEvent{}
		key_code := C.v_multiwindow_x11_key_code(event, &backend.keycodes[0], 256)
		native_keycode := int(C.v_multiwindow_x11_event_keycode(event))
		mut repeat := false
		if native_keycode >= 0 && native_keycode < 256 {
			repeat = backend.windows[index].key_repeat[native_keycode]
			backend.windows[index].key_repeat[native_keycode] = true
		}
		mut modifiers := u32(C.v_multiwindow_x11_modifiers(C.v_multiwindow_x11_event_state(event)))
		if key_code != 0 {
			modifiers |= u32(C.v_multiwindow_x11_key_modifier_bit(key_code))
			input := backend.input_event_with_payload(backend.windows[index], .key_down, key_code,
				repeat, modifiers, x11_invalid_mouse_button, 0, 0)
			events << queued_input_event(input)
			if x11_is_clipboard_paste(key_code, modifiers) {
				events << queued_input_event(backend.input_event_with_payload(backend.windows[index],
					.clipboard_pasted, 0, false, modifiers, x11_invalid_mouse_button, 0, 0))
			}
		}
		backend.append_key_press_char_events(mut events, backend.windows[index], event, repeat,
			modifiers)
		return events
	}

	fn (backend &X11Backend) append_key_press_char_events(mut events []QueuedEvent, record X11WindowRecord, event &C.XEvent, repeat bool, modifiers u32) {
		mut inline_char_codes := [x11_inline_char_codes]u32{}
		mut required_char_codes := 0
		mut char_count := C.v_multiwindow_x11_char_codes(record.xic, event,
			unsafe { &inline_char_codes[0] }, x11_inline_char_codes, &required_char_codes)
		if required_char_codes > x11_inline_char_codes {
			mut char_codes := []u32{len: required_char_codes}
			char_count = C.v_multiwindow_x11_char_codes(record.xic, event,
				unsafe { &char_codes[0] }, required_char_codes, &required_char_codes)
			for i in 0 .. char_count {
				events << queued_input_event(backend.input_char_event(record, char_codes[i],
					repeat, modifiers))
			}
			return
		}
		for i in 0 .. char_count {
			events << queued_input_event(backend.input_char_event(record, inline_char_codes[i],
				repeat, modifiers))
		}
	}

	fn (mut backend X11Backend) queued_key_release_event(index int, event &C.XEvent) []QueuedEvent {
		mut events := []QueuedEvent{}
		key_code := C.v_multiwindow_x11_key_code(event, &backend.keycodes[0], 256)
		if key_code == 0 {
			return events
		}
		native_keycode := int(C.v_multiwindow_x11_event_keycode(event))
		if native_keycode >= 0 && native_keycode < 256 {
			backend.windows[index].key_repeat[native_keycode] = false
		}
		mut modifiers := C.v_multiwindow_x11_modifiers(C.v_multiwindow_x11_event_state(event))
		modifiers &= ~C.v_multiwindow_x11_key_modifier_bit(key_code)
		input := backend.input_event_with_payload(backend.windows[index], .key_up, key_code, false,
			u32(modifiers), x11_invalid_mouse_button, 0, 0)
		events << queued_input_event(input)
		return events
	}

	fn (mut backend X11Backend) queued_button_press_event(index int, event &C.XEvent) []QueuedEvent {
		mut events := []QueuedEvent{}
		x := C.v_multiwindow_x11_event_x(event)
		y := C.v_multiwindow_x11_event_y(event)
		backend.update_mouse_position(index, x, y, false)
		button := C.v_multiwindow_x11_event_button(event)
		mut modifiers := C.v_multiwindow_x11_modifiers(C.v_multiwindow_x11_event_state(event))
		match button {
			x11_scroll_up {
				events << queued_input_event(backend.scroll_event(index, 0.0, 1.0, u32(modifiers)))
			}
			x11_scroll_down {
				events << queued_input_event(backend.scroll_event(index, 0.0, -1.0, u32(modifiers)))
			}
			x11_scroll_right {
				events << queued_input_event(backend.scroll_event(index, 1.0, 0.0, u32(modifiers)))
			}
			x11_scroll_left {
				events << queued_input_event(backend.scroll_event(index, -1.0, 0.0, u32(modifiers)))
			}
			else {
				mouse_button := C.v_multiwindow_x11_mouse_button(button)
				if mouse_button != x11_invalid_mouse_button {
					modifiers |= C.v_multiwindow_x11_button_modifier_bit(mouse_button)
					backend.windows[index].mouse_buttons |= u8(1 << mouse_button)
					input := backend.input_event_with_payload(backend.windows[index], .mouse_down,
						0, false, u32(modifiers), mouse_button, 0, 0)
					events << queued_input_event(input)
				}
			}
		}

		return events
	}

	fn (mut backend X11Backend) queued_button_release_event(index int, event &C.XEvent) []QueuedEvent {
		mut events := []QueuedEvent{}
		x := C.v_multiwindow_x11_event_x(event)
		y := C.v_multiwindow_x11_event_y(event)
		backend.update_mouse_position(index, x, y, false)
		mouse_button := C.v_multiwindow_x11_mouse_button(C.v_multiwindow_x11_event_button(event))
		if mouse_button == x11_invalid_mouse_button {
			return events
		}
		mut modifiers := C.v_multiwindow_x11_modifiers(C.v_multiwindow_x11_event_state(event))
		modifiers &= ~C.v_multiwindow_x11_button_modifier_bit(mouse_button)
		backend.windows[index].mouse_buttons &= ~u8(1 << mouse_button)
		input := backend.input_event_with_payload(backend.windows[index], .mouse_up, 0, false,
			u32(modifiers), mouse_button, 0, 0)
		events << queued_input_event(input)
		return events
	}

	fn (mut backend X11Backend) queued_mouse_position_event(index int, event &C.XEvent, kind InputEventKind, clear_delta bool) QueuedEvent {
		x := C.v_multiwindow_x11_event_x(event)
		y := C.v_multiwindow_x11_event_y(event)
		backend.update_mouse_position(index, x, y, clear_delta)
		input := backend.input_event_with_payload(backend.windows[index], kind, 0, false,
			u32(C.v_multiwindow_x11_modifiers(C.v_multiwindow_x11_event_state(event))),
			x11_invalid_mouse_button, 0, 0)
		return queued_input_event(input)
	}

	fn (mut backend X11Backend) update_mouse_position(index int, x int, y int, clear_delta bool) {
		new_x := f32(x)
		new_y := f32(y)
		if clear_delta || !backend.windows[index].mouse_pos_valid {
			backend.windows[index].mouse_x = new_x
			backend.windows[index].mouse_y = new_y
			backend.windows[index].mouse_dx = 0
			backend.windows[index].mouse_dy = 0
			backend.windows[index].mouse_pos_valid = true
			return
		}
		backend.windows[index].mouse_dx = new_x - backend.windows[index].mouse_x
		backend.windows[index].mouse_dy = new_y - backend.windows[index].mouse_y
		backend.windows[index].mouse_x = new_x
		backend.windows[index].mouse_y = new_y
	}

	fn (backend &X11Backend) scroll_event(index int, x f32, y f32, modifiers u32) InputEvent {
		return backend.input_event_with_payload(backend.windows[index], .mouse_scroll, 0, false,
			modifiers, x11_invalid_mouse_button, x, y)
	}

	fn (backend &X11Backend) window_record_index_for_event(event &C.XEvent) ?int {
		return backend.window_record_index_for_native(C.v_multiwindow_x11_event_window(event))
	}

	fn (backend &X11Backend) window_state(window X11NativeWindow) int {
		mut result := x11_normal_state
		mut actual_type := X11NativeAtom(0)
		mut actual_format := 0
		mut item_count := X11NativeULong(0)
		mut bytes_after := X11NativeULong(0)
		mut state := &X11NativeLong(unsafe { nil })
		status := C.XGetWindowProperty(backend.display, window, backend.wm_state, X11NativeLong(0),
			X11NativeLong(0x7fffffff), 0, backend.wm_state, &actual_type, &actual_format,
			&item_count, &bytes_after, unsafe { &&u8(&state) })
		if status == x11_success && actual_type == backend.wm_state && actual_format == 32
			&& item_count >= X11NativeULong(2) && state != unsafe { nil } {
			state_value := unsafe { state[0] }
			result = int(state_value)
		}
		if state != unsafe { nil } {
			C.XFree(unsafe { voidptr(state) })
		}
		return result
	}

	fn (mut backend X11Backend) announce_xdnd_for_window(window X11NativeWindow) {
		if backend.xdnd_aware == X11NativeAtom(0) {
			return
		}
		version := X11NativeAtom(x11_xdnd_version)
		C.XChangeProperty(backend.display, window, backend.xdnd_aware, X11NativeAtom(4), 32,
			x11_prop_mode_replace, unsafe { &u8(&version) }, 1)
	}

	fn (mut backend X11Backend) queued_xdnd_client_message_events(event &C.XEvent) []QueuedEvent {
		message_type := unsafe { event.xclient.message_type }
		if message_type == backend.xdnd_enter {
			backend.handle_xdnd_enter(event)
		} else if message_type == backend.xdnd_position {
			backend.handle_xdnd_position(event)
		} else if message_type == backend.xdnd_drop {
			backend.handle_xdnd_drop(event)
		} else if message_type == backend.xdnd_leave {
			backend.clear_xdnd_state()
		}
		return []QueuedEvent{}
	}

	fn (mut backend X11Backend) handle_xdnd_enter(event &C.XEvent) {
		backend.xdnd_source = unsafe { X11NativeWindow(event.xclient.data.l[0]) }
		backend.xdnd_target = unsafe { event.xclient.window }
		backend.xdnd_version = unsafe { event.xclient.data.l[1] >> 24 }
		backend.xdnd_format = X11NativeAtom(0)
		if backend.xdnd_version > x11_xdnd_version {
			return
		}
		is_list := (unsafe { event.xclient.data.l[1] } & X11NativeLong(1)) != 0
		if is_list {
			backend.xdnd_format = backend.xdnd_format_from_type_list()
			return
		}
		for i in 2 .. 5 {
			format := unsafe { X11NativeAtom(event.xclient.data.l[i]) }
			if format == backend.text_uri_list {
				backend.xdnd_format = backend.text_uri_list
				return
			}
		}
	}

	fn (mut backend X11Backend) handle_xdnd_position(event &C.XEvent) {
		if backend.xdnd_version > x11_xdnd_version || backend.xdnd_source == X11NativeWindow(0) {
			return
		}
		backend.xdnd_target = unsafe { event.xclient.window }
		backend.update_xdnd_mouse_position(event)
		backend.send_xdnd_status(backend.xdnd_format != X11NativeAtom(0))
	}

	fn (mut backend X11Backend) handle_xdnd_drop(event &C.XEvent) {
		if backend.xdnd_version > x11_xdnd_version {
			return
		}
		backend.xdnd_target = unsafe { event.xclient.window }
		if backend.xdnd_source == X11NativeWindow(0) || backend.xdnd_format == X11NativeAtom(0) {
			backend.send_xdnd_finished(backend.xdnd_target, false)
			backend.clear_xdnd_state()
			return
		}
		time := if backend.xdnd_version >= 1 {
			unsafe { X11NativeULong(event.xclient.data.l[2]) }
		} else {
			X11NativeULong(0)
		}
		C.XConvertSelection(backend.display, backend.xdnd_selection, backend.xdnd_format,
			backend.xdnd_selection, backend.xdnd_target, time)
	}

	fn (mut backend X11Backend) update_xdnd_mouse_position(event &C.XEvent) {
		index := backend.window_record_index_for_native(backend.xdnd_target) or { return }
		root_x, root_y := x11_xdnd_position_coords(unsafe { event.xclient.data.l[2] })
		mut window_x := root_x
		mut window_y := root_y
		mut child := X11NativeWindow(0)
		if backend.root != X11NativeWindow(0)
			&& C.XTranslateCoordinates(backend.display, backend.root, backend.xdnd_target, root_x, root_y, &window_x, &window_y, &child) != 0 {
			backend.update_mouse_position(index, window_x, window_y, false)
			return
		}
		backend.update_mouse_position(index, root_x, root_y, false)
	}

	fn (mut backend X11Backend) queued_xdnd_selection_events(event &C.XEvent) []QueuedEvent {
		mut events := []QueuedEvent{}
		if unsafe { event.xselection.selection } != backend.xdnd_selection {
			return events
		}
		requestor := unsafe { event.xselection.requestor }
		property := unsafe { event.xselection.property }
		if property == X11NativeAtom(0) {
			backend.send_xdnd_finished(requestor, false)
			backend.clear_xdnd_state()
			return events
		}
		index := backend.window_record_index_for_native(requestor) or {
			backend.send_xdnd_finished(requestor, false)
			backend.clear_xdnd_state()
			return events
		}
		mut actual_type := X11NativeAtom(0)
		mut actual_format := 0
		mut item_count := X11NativeULong(0)
		mut bytes_after := X11NativeULong(0)
		mut data := &u8(unsafe { nil })
		status := C.XGetWindowProperty(backend.display, requestor, property, X11NativeLong(0),
			X11NativeLong(x11_xdnd_max_payload_units), 1, backend.text_uri_list, &actual_type,
			&actual_format, &item_count, &bytes_after, &&u8(&data))
		valid_payload := status == x11_success && actual_type == backend.text_uri_list
			&& actual_format == 8 && bytes_after == X11NativeULong(0)
			&& item_count <= X11NativeULong(x11_xdnd_max_payload_bytes)
		if !valid_payload {
			if data != unsafe { nil } {
				C.XFree(data)
			}
			backend.send_xdnd_finished(requestor, false)
			backend.clear_xdnd_state()
			return events
		}
		payload := if data != unsafe { nil } && item_count > X11NativeULong(0) {
			unsafe { tos(data, int(item_count)).clone() }
		} else {
			''
		}
		if data != unsafe { nil } {
			C.XFree(data)
		}
		files := dropped_files_from_uri_list(payload)
		if files.len > 0 {
			events << queued_input_event(backend.input_files_dropped_event(backend.windows[index],
				files))
		}
		backend.send_xdnd_finished(requestor, files.len > 0)
		backend.clear_xdnd_state()
		return events
	}

	fn (mut backend X11Backend) xdnd_format_from_type_list() X11NativeAtom {
		if backend.xdnd_source == X11NativeWindow(0) {
			return X11NativeAtom(0)
		}
		mut actual_type := X11NativeAtom(0)
		mut actual_format := 0
		mut item_count := X11NativeULong(0)
		mut bytes_after := X11NativeULong(0)
		mut formats := &X11NativeAtom(unsafe { nil })
		status := C.XGetWindowProperty(backend.display, backend.xdnd_source,
			backend.xdnd_type_list, X11NativeLong(0), X11NativeLong(x11_xdnd_max_type_atoms), 0,
			X11NativeAtom(4), &actual_type, &actual_format, &item_count, &bytes_after,
			unsafe { &&u8(&formats) })
		valid_type_list := status == x11_success && actual_type == X11NativeAtom(4)
			&& actual_format == 32 && bytes_after == X11NativeULong(0) && formats != unsafe { nil }
			&& item_count <= X11NativeULong(x11_xdnd_max_type_atoms)
		if !valid_type_list {
			if formats != unsafe { nil } {
				C.XFree(formats)
			}
			return X11NativeAtom(0)
		}
		mut result := X11NativeAtom(0)
		for i in 0 .. int(item_count) {
			if unsafe { formats[i] } == backend.text_uri_list {
				result = backend.text_uri_list
				break
			}
		}
		C.XFree(formats)
		return result
	}

	fn (backend &X11Backend) send_xdnd_status(accepted bool) {
		if backend.xdnd_source == X11NativeWindow(0) {
			return
		}
		mut reply := C.XEvent{}
		reply.@type = x11_client_message
		unsafe {
			reply.xclient.window = backend.xdnd_source
			reply.xclient.message_type = backend.xdnd_status
			reply.xclient.format = 32
			reply.xclient.data.l[0] = X11NativeLong(backend.xdnd_target)
			if accepted {
				reply.xclient.data.l[1] = X11NativeLong(1)
				if backend.xdnd_version >= 2 {
					reply.xclient.data.l[4] = X11NativeLong(backend.xdnd_action_copy)
				}
			}
		}
		C.XSendEvent(backend.display, backend.xdnd_source, 0, X11NativeLong(0), &reply)
		C.XFlush(backend.display)
	}

	fn (backend &X11Backend) send_xdnd_finished(requestor X11NativeWindow, accepted bool) {
		if backend.xdnd_source == X11NativeWindow(0) || backend.xdnd_version < 2 {
			return
		}
		mut reply := C.XEvent{}
		reply.@type = x11_client_message
		unsafe {
			reply.xclient.window = backend.xdnd_source
			reply.xclient.message_type = backend.xdnd_finished
			reply.xclient.format = 32
			reply.xclient.data.l[0] = X11NativeLong(requestor)
			if accepted {
				reply.xclient.data.l[1] = X11NativeLong(1)
				reply.xclient.data.l[2] = X11NativeLong(backend.xdnd_action_copy)
			}
		}
		C.XSendEvent(backend.display, backend.xdnd_source, 0, X11NativeLong(0), &reply)
		C.XFlush(backend.display)
	}

	fn (mut backend X11Backend) clear_xdnd_state() {
		backend.xdnd_source = X11NativeWindow(0)
		backend.xdnd_target = X11NativeWindow(0)
		backend.xdnd_format = X11NativeAtom(0)
		backend.xdnd_version = X11NativeLong(0)
	}
}

fn x11_xdnd_position_coords(value X11NativeLong) (int, int) {
	packed := u32(value)
	return x11_signed_16(packed >> 16), x11_signed_16(packed)
}

fn x11_signed_16(value u32) int {
	mut result := int(value & u32(0xffff))
	if result >= 0x8000 {
		result -= 0x10000
	}
	return result
}

fn (mut backend X11Backend) stop() ! {
	$if linux && x_multiwindow_x11 ? {
		if !backend.started {
			return
		}
		for backend.windows.len > 0 {
			record := backend.windows[0]
			backend.destroy_window(record.id)!
		}
		backend.shutdown_renderer()
		if backend.xim != unsafe { nil } {
			C.v_multiwindow_x11_close_im(backend.xim)
			backend.xim = unsafe { nil }
		}
		if backend.display != unsafe { nil } {
			if C.XCloseDisplay(backend.display) != 0 {
				return error(err_x11_close_display_failed)
			}
		}
		backend.display = unsafe { nil }
		backend.started = false
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (backend &X11Backend) window_record_index(id WindowId) ?int {
	for i, record in backend.windows {
		if record.id == id {
			return i
		}
	}
	return none
}

fn (backend &X11Backend) window_record_index_for_native(window X11NativeWindow) ?int {
	for i, record in backend.windows {
		if record.window == window {
			return i
		}
	}
	return none
}

fn (backend &X11Backend) window_id_for_native(window X11NativeWindow) ?WindowId {
	for record in backend.windows {
		if record.window == window {
			return record.id
		}
	}
	return none
}

fn (mut backend X11Backend) remove_native_window(window X11NativeWindow) ?WindowId {
	for i, record in backend.windows {
		if record.window == window {
			backend.release_window_resources(record, false) or {}
			backend.windows.delete(i)
			return record.id
		}
	}
	return none
}

fn (backend &X11Backend) renderer_ready() bool {
	return backend.egl_display != unsafe { nil } && backend.egl_config != unsafe { nil }
		&& backend.egl_context != unsafe { nil }
}

fn (mut backend X11Backend) init_renderer() ! {
	$if linux && x_multiwindow_x11 ? {
		if backend.renderer_ready() {
			return
		}
		egl_display := C.v_multiwindow_x11_egl_get_display(backend.display)
		if egl_display == unsafe { nil } {
			return error(err_x11_egl_display_failed)
		}
		if C.v_multiwindow_x11_egl_initialize(egl_display) == 0 {
			return error(err_x11_egl_display_failed)
		}
		if C.v_multiwindow_x11_egl_bind_opengl_api() == 0 {
			C.v_multiwindow_x11_egl_terminate(egl_display)
			return error(err_x11_egl_context_failed)
		}
		mut egl_config := voidptr(unsafe { nil })
		mut native_visual_id := 0
		if C.v_multiwindow_x11_egl_choose_config(egl_display, &egl_config, &native_visual_id) == 0 {
			C.v_multiwindow_x11_egl_terminate(egl_display)
			return error(err_x11_egl_config_failed)
		}
		egl_context := C.v_multiwindow_x11_egl_create_context(egl_display, egl_config)
		if egl_context == unsafe { nil } {
			C.v_multiwindow_x11_egl_terminate(egl_display)
			return error(err_x11_egl_context_failed)
		}
		backend.egl_display = egl_display
		backend.egl_config = egl_config
		backend.egl_context = egl_context
		backend.native_visual_id = native_visual_id
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) shutdown_renderer() {
	$if linux && x_multiwindow_x11 ? {
		if backend.egl_display != unsafe { nil } {
			C.v_multiwindow_x11_egl_clear_current(backend.egl_display)
			if backend.egl_context != unsafe { nil } {
				C.v_multiwindow_x11_egl_destroy_context(backend.egl_display, backend.egl_context)
			}
			C.v_multiwindow_x11_egl_terminate(backend.egl_display)
		}
		backend.egl_display = unsafe { nil }
		backend.egl_config = unsafe { nil }
		backend.egl_context = unsafe { nil }
		backend.native_visual_id = 0
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn (mut backend X11Backend) render_environment(id WindowId) !gfx.Environment {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			record := backend.windows[index]
			backend.make_current(record)!
			return gfx.Environment{
				defaults: gfx.EnvironmentDefaults{
					color_format: .rgba8
					depth_format: .depth_stencil
					sample_count: 1
				}
			}
		} $else {
			_ = id
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend X11Backend) begin_render(id WindowId) !RenderFrame {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			record := backend.windows[index]
			backend.make_current(record)!
			return RenderFrame{
				window_id: id
				swapchain: backend.swapchain_for_record(record)
			}
		} $else {
			_ = id
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend X11Backend) end_render(frame RenderFrame) ! {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(frame.window_id) or {
				return error(err_window_not_found)
			}
			record := backend.windows[index]
			backend.make_current(record)!
			if C.v_multiwindow_x11_egl_swap_buffers(backend.egl_display, record.egl_surface) == 0 {
				return error(err_x11_egl_swap_buffers_failed)
			}
			return
		} $else {
			_ = frame
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend X11Backend) make_current(record X11WindowRecord) ! {
		$if linux && x_multiwindow_x11 ? {
			if !backend.renderer_ready() || record.egl_surface == unsafe { nil } {
				return error(err_renderer_unsupported)
			}
			if C.v_multiwindow_x11_egl_make_current(backend.egl_display, record.egl_surface,
				backend.egl_context) == 0 {
				return error(err_x11_egl_make_current_failed)
			}
			return
		} $else {
			_ = record
			return error(err_backend_unsupported)
		}
	}

	fn (backend &X11Backend) swapchain_for_record(record X11WindowRecord) gfx.Swapchain {
		width := if record.width > 0 { record.width } else { 1 }
		height := if record.height > 0 { record.height } else { 1 }
		return gfx.Swapchain{
			width:        width
			height:       height
			sample_count: 1
			color_format: .rgba8
			depth_format: .depth_stencil
			gl:           gfx.GlSwapchain{
				framebuffer: 0
			}
		}
	}
}

fn (mut backend X11Backend) free_cursor_for_record(index int) {
	$if linux && x_multiwindow_x11 ? {
		if index < 0 || index >= backend.windows.len {
			return
		}
		cursor := backend.windows[index].cursor
		if backend.display != unsafe { nil } && cursor != X11NativeCursor(0) {
			C.XFreeCursor(backend.display, cursor)
		}
		backend.windows[index].cursor = X11NativeCursor(0)
		return
	} $else {
		_ = index
	}
}

fn (mut backend X11Backend) release_window_resources(record X11WindowRecord, destroy_native bool) ! {
	$if linux && x_multiwindow_x11 ? {
		if backend.egl_display != unsafe { nil } && record.egl_surface != unsafe { nil } {
			C.v_multiwindow_x11_egl_clear_current(backend.egl_display)
			C.v_multiwindow_x11_egl_destroy_surface(backend.egl_display, record.egl_surface)
		}
		if backend.display != unsafe { nil } && record.cursor != X11NativeCursor(0) {
			C.XFreeCursor(backend.display, record.cursor)
		}
		if record.xic != unsafe { nil } {
			C.v_multiwindow_x11_destroy_ic(record.xic)
		}
		if destroy_native {
			backend.destroy_native_window(record.window, record.colormap)!
		} else if backend.display != unsafe { nil } && record.colormap != X11NativeColormap(0) {
			C.XFreeColormap(backend.display, record.colormap)
			C.XFlush(backend.display)
		}
		return
	} $else {
		_ = record
		_ = destroy_native
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) destroy_native_window(window X11NativeWindow, colormap X11NativeColormap) ! {
	$if linux && x_multiwindow_x11 ? {
		if backend.started && backend.display != unsafe { nil } {
			if C.XDestroyWindow(backend.display, window) == 0 {
				return error(err_x11_destroy_window_failed)
			}
			if colormap != X11NativeColormap(0) {
				C.XFreeColormap(backend.display, colormap)
			}
			C.XFlush(backend.display)
		}
		return
	} $else {
		_ = window
		_ = colormap
		return error(err_backend_unsupported)
	}
}

fn x11_bool_to_int(value bool) int {
	return if value { 1 } else { 0 }
}

fn x11_is_clipboard_paste(key_code int, modifiers u32) bool {
	return key_code == x11_key_v && modifiers == x11_modifier_ctrl
}
