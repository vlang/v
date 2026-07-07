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

$if x32 {
	type X11NativeLong = int
	type X11NativeULong = u32
} $else {
	type X11NativeLong = i64
	type X11NativeULong = u64
}

type X11NativeAtom = X11NativeULong
type X11NativeColormap = X11NativeULong
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
	union C.XEvent {
	mut:
		@type          int
		xclient        C.XClientMessageEvent
		xconfigure     C.XConfigureEvent
		xdestroywindow C.XDestroyWindowEvent
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
	fn C.XFreeColormap(display &C.Display, colormap X11NativeColormap) int
	fn C.XFlush(display &C.Display) int
	fn C.XSync(display &C.Display, discard int) int
	fn C.XPending(display &C.Display) int
	fn C.XNextEvent(display &C.Display, event &C.XEvent) int
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
	egl_surface voidptr
mut:
	config WindowConfig
	width  int
	height int
}

struct X11Backend {
mut:
	display          &C.Display = unsafe { nil }
	screen           int
	root             X11NativeWindow
	wm_protocols     X11NativeAtom
	wm_delete_window X11NativeAtom
	egl_display      voidptr
	egl_config       voidptr
	egl_context      voidptr
	native_visual_id int
	started          bool
	windows          []X11WindowRecord
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
		if require_renderer {
			backend.init_renderer() or {
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
				C.XSelectInput(backend.display, created, 1 << 17)
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
		if config.visible {
			C.XMapWindow(backend.display, window)
		}
		if C.XSync(backend.display, 0) == 0 {
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
		mut actual_width := 0
		mut actual_height := 0
		if C.v_multiwindow_x11_get_window_size(backend.display, window, &actual_width,
			&actual_height) == 0 {
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
		actual_size = WindowSize{
			width:  actual_width
			height: actual_height
		}
		backend.windows << X11WindowRecord{
			id:          id
			window:      window
			colormap:    colormap
			egl_surface: egl_surface
			config:      window_config_with_size(config, actual_size.width, actual_size.height)
			width:       actual_size.width
			height:      actual_size.height
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

fn (mut backend X11Backend) poll_events() ![]Event {
	mut events := []Event{}
	$if linux && x_multiwindow_x11 ? {
		if !backend.started || backend.display == unsafe { nil } {
			return events
		}
		for C.XPending(backend.display) > 0 {
			mut event := C.XEvent{}
			C.XNextEvent(backend.display, &event)
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
						events << Event{
							kind:      .window_close_requested
							window_id: id
						}
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
						events << Event{
							kind:      .window_resized
							window_id: id
							width:     width
							height:    height
						}
					}
				}
				x11_destroy_notify {
					native_window := unsafe { event.xdestroywindow.window }
					id := backend.remove_native_window(native_window) or { continue }
					events << Event{
						kind:      .window_destroyed
						window_id: id
					}
				}
				else {}
			}
		}
	}
	return events
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

fn (mut backend X11Backend) release_window_resources(record X11WindowRecord, destroy_native bool) ! {
	$if linux && x_multiwindow_x11 ? {
		if backend.egl_display != unsafe { nil } && record.egl_surface != unsafe { nil } {
			C.v_multiwindow_x11_egl_clear_current(backend.egl_display)
			C.v_multiwindow_x11_egl_destroy_surface(backend.egl_display, record.egl_surface)
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
