module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if linux && sokol_wayland ? {
	#flag linux -lwayland-client
	#flag linux -lwayland-egl
	#flag linux -lEGL
	#flag linux -lGL
	#flag linux -I @VEXEROOT/thirdparty/sokol
	#flag linux @VMODROOT/vlib/x/multiwindow/wayland_xdg_shell_private.c
	#include <poll.h>
	#include <string.h>
	#include <wayland-client.h>
	#insert "@VMODROOT/vlib/x/multiwindow/wayland_backend_helpers.h"
}

const wayland_poll_in = i16(0x001)
const err_wayland_egl_config_failed = 'multiwindow: wayland egl config failed'
const err_wayland_egl_context_failed = 'multiwindow: wayland egl context failed'
const err_wayland_egl_display_failed = 'multiwindow: wayland egl display failed'
const err_wayland_egl_make_current_failed = 'multiwindow: wayland egl make current failed'
const err_wayland_egl_surface_failed = 'multiwindow: wayland egl surface failed'
const err_wayland_egl_swap_buffers_failed = 'multiwindow: wayland egl swap buffers failed'
const err_wayland_surface_not_configured = 'multiwindow: wayland surface is not configured'

@[heap]
struct WaylandWindowRecord {
	id           WindowId
	surface      voidptr
	xdg_surface  voidptr
	xdg_toplevel voidptr
mut:
	width                   int
	height                  int
	min_width               int
	min_height              int
	pending_toplevel_width  int
	pending_toplevel_height int
	configured              bool
	pending_egl_resize      bool
	resize_event_pending    bool
	close_requested         bool
	wl_egl_window           voidptr
	egl_surface             voidptr
}

struct WaylandBackend {
mut:
	display            voidptr
	registry           voidptr
	compositor         voidptr
	compositor_version u32
	wm_base            voidptr
	egl_display        voidptr
	egl_config         voidptr
	egl_context        voidptr
	started            bool
	windows            []&WaylandWindowRecord
}

@[markused]
fn (mut backend WaylandBackend) registry_listener_data() voidptr {
	return unsafe { voidptr(&backend) }
}

@[markused]
fn (record &WaylandWindowRecord) listener_data() voidptr {
	return unsafe { voidptr(record) }
}

$if linux && sokol_wayland ? {
	struct C.pollfd {
	mut:
		fd      int
		events  i16
		revents i16
	}

	struct C.wl_display {}

	struct C.wl_registry {}

	struct C.wl_compositor {}

	struct C.wl_surface {}

	struct C.wl_output {}

	struct C.wl_array {
		size  usize
		alloc usize
		data  voidptr
	}

	struct C.xdg_wm_base {}

	struct C.xdg_surface {}

	struct C.xdg_toplevel {}

	fn C.poll(fds &C.pollfd, nfds u64, timeout int) int
	fn C.strcmp(a &char, b &char) int
	fn C.v_multiwindow_wayland_compositor_bind_version(version u32) u32
	fn C.v_multiwindow_wayland_bind_compositor(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_bind_xdg_wm_base(registry &C.wl_registry, name u32) voidptr
	fn C.v_multiwindow_wayland_add_registry_listener(registry &C.wl_registry, data voidptr) int
	fn C.v_multiwindow_wayland_add_xdg_wm_base_listener(wm_base &C.xdg_wm_base, data voidptr) int
	fn C.v_multiwindow_wayland_add_xdg_surface_listener(xdg_surface &C.xdg_surface, data voidptr) int
	fn C.v_multiwindow_wayland_add_xdg_toplevel_listener(toplevel &C.xdg_toplevel, data voidptr) int
	fn C.wl_display_connect(name &char) &C.wl_display
	fn C.wl_display_disconnect(display &C.wl_display)
	fn C.wl_display_get_fd(display &C.wl_display) int
	fn C.wl_display_get_registry(display &C.wl_display) &C.wl_registry
	fn C.wl_display_prepare_read(display &C.wl_display) int
	fn C.wl_display_cancel_read(display &C.wl_display)
	fn C.wl_display_read_events(display &C.wl_display) int
	fn C.wl_display_dispatch_pending(display &C.wl_display) int
	fn C.wl_display_roundtrip(display &C.wl_display) int
	fn C.wl_display_flush(display &C.wl_display) int
	fn C.wl_registry_destroy(registry &C.wl_registry)
	fn C.wl_compositor_create_surface(compositor &C.wl_compositor) &C.wl_surface
	fn C.wl_compositor_destroy(compositor &C.wl_compositor)
	fn C.wl_surface_commit(surface &C.wl_surface)
	fn C.wl_surface_destroy(surface &C.wl_surface)
	fn C.v_multiwindow_wayland_xdg_wm_base_get_xdg_surface(wm_base &C.xdg_wm_base, surface &C.wl_surface) &C.xdg_surface
	fn C.v_multiwindow_wayland_xdg_wm_base_destroy(wm_base &C.xdg_wm_base)
	fn C.v_multiwindow_wayland_xdg_wm_base_pong(wm_base &C.xdg_wm_base, serial u32)
	fn C.v_multiwindow_wayland_xdg_surface_get_toplevel(xdg_surface &C.xdg_surface) &C.xdg_toplevel
	fn C.v_multiwindow_wayland_xdg_surface_ack_configure(xdg_surface &C.xdg_surface, serial u32)
	fn C.v_multiwindow_wayland_xdg_surface_destroy(xdg_surface &C.xdg_surface)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_title(toplevel &C.xdg_toplevel, title &char)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_app_id(toplevel &C.xdg_toplevel, app_id &char)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_min_size(toplevel &C.xdg_toplevel, width i32, height i32)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_max_size(toplevel &C.xdg_toplevel, width i32, height i32)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_fullscreen(toplevel &C.xdg_toplevel, output &C.wl_output)
	fn C.v_multiwindow_wayland_xdg_toplevel_destroy(toplevel &C.xdg_toplevel)
	fn C.v_multiwindow_wayland_egl_get_display(display &C.wl_display) voidptr
	fn C.v_multiwindow_wayland_egl_initialize(egl_display voidptr) int
	fn C.v_multiwindow_wayland_egl_bind_opengl_api() int
	fn C.v_multiwindow_wayland_egl_choose_config(egl_display voidptr, out_config &voidptr) int
	fn C.v_multiwindow_wayland_egl_create_context(egl_display voidptr, egl_config voidptr) voidptr
	fn C.v_multiwindow_wayland_egl_create_window(surface &C.wl_surface, width int, height int) voidptr
	fn C.v_multiwindow_wayland_egl_resize_window(egl_window voidptr, width int, height int)
	fn C.v_multiwindow_wayland_egl_destroy_window(egl_window voidptr)
	fn C.v_multiwindow_wayland_egl_create_window_surface(egl_display voidptr, egl_config voidptr, egl_window voidptr) voidptr
	fn C.v_multiwindow_wayland_egl_make_current(egl_display voidptr, egl_surface voidptr, egl_context voidptr) int
	fn C.v_multiwindow_wayland_egl_clear_current(egl_display voidptr)
	fn C.v_multiwindow_wayland_egl_swap_buffers(egl_display voidptr, egl_surface voidptr) int
	fn C.v_multiwindow_wayland_egl_destroy_surface(egl_display voidptr, egl_surface voidptr)
	fn C.v_multiwindow_wayland_egl_destroy_context(egl_display voidptr, egl_context voidptr)
	fn C.v_multiwindow_wayland_egl_terminate(egl_display voidptr)

	@[export: 'v_multiwindow_wayland_registry_handle_global']
	@[markused]
	fn wayland_registry_handle_global(data voidptr, registry &C.wl_registry, name u32, iface &char, version u32) {
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		if C.strcmp(iface, c'wl_compositor') == 0 {
			backend.compositor = C.v_multiwindow_wayland_bind_compositor(registry, name, version)
			backend.compositor_version = C.v_multiwindow_wayland_compositor_bind_version(version)
		} else if C.strcmp(iface, c'xdg_wm_base') == 0 {
			backend.wm_base = C.v_multiwindow_wayland_bind_xdg_wm_base(registry, name)
		}
	}

	@[export: 'v_multiwindow_wayland_registry_handle_global_remove']
	@[markused]
	fn wayland_registry_handle_global_remove(data voidptr, registry &C.wl_registry, name u32) {
		_ = data
		_ = registry
		_ = name
	}

	@[export: 'v_multiwindow_wayland_xdg_wm_base_ping']
	@[markused]
	fn wayland_xdg_wm_base_ping(data voidptr, wm_base voidptr, serial u32) {
		_ = data
		C.v_multiwindow_wayland_xdg_wm_base_pong(unsafe { &C.xdg_wm_base(wm_base) }, serial)
	}

	@[export: 'v_multiwindow_wayland_xdg_surface_configure']
	@[markused]
	fn wayland_xdg_surface_configure(data voidptr, xdg_surface voidptr, serial u32) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		mut width := record.width
		mut height := record.height
		if record.pending_toplevel_width > 0 {
			width = record.pending_toplevel_width
		}
		if record.pending_toplevel_height > 0 {
			height = record.pending_toplevel_height
		}
		if width <= 0 {
			width = 1
		}
		if height <= 0 {
			height = 1
		}
		width = window_extent_for_minimum(width, record.min_width)
		height = window_extent_for_minimum(height, record.min_height)
		if record.configured && (record.width != width || record.height != height) {
			record.resize_event_pending = true
			record.pending_egl_resize = true
		}
		record.width = width
		record.height = height
		record.pending_toplevel_width = 0
		record.pending_toplevel_height = 0
		record.configured = true
		C.v_multiwindow_wayland_xdg_surface_ack_configure(unsafe {
			&C.xdg_surface(xdg_surface)
		}, serial)
	}

	@[export: 'v_multiwindow_wayland_xdg_toplevel_configure']
	@[markused]
	fn wayland_xdg_toplevel_configure(data voidptr, toplevel voidptr, width int, height int, states &C.wl_array) {
		_ = toplevel
		_ = states
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		if width > 0 {
			record.pending_toplevel_width = width
		} else {
			record.pending_toplevel_width = 0
		}
		if height > 0 {
			record.pending_toplevel_height = height
		} else {
			record.pending_toplevel_height = 0
		}
	}

	@[export: 'v_multiwindow_wayland_xdg_toplevel_close']
	@[markused]
	fn wayland_xdg_toplevel_close(data voidptr, toplevel voidptr) {
		_ = toplevel
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		record.close_requested = true
	}
}

fn new_wayland_backend() WaylandBackend {
	return WaylandBackend{}
}

fn (backend &WaylandBackend) ensure_supported() ! {
	$if linux && sokol_wayland ? {
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (backend &WaylandBackend) capabilities() Capabilities {
	return Capabilities{
		backend:            .wayland
		mock:               false
		native:             true
		multi_window:       true
		owner_queue:        true
		explicit_swapchain: backend.renderer_ready()
		wayland:            true
		gl:                 backend.renderer_ready()
	}
}

fn (mut backend WaylandBackend) start(require_renderer bool) ! {
	$if linux && sokol_wayland ? {
		if backend.started {
			return
		}
		display := C.wl_display_connect(unsafe { nil })
		if display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		registry := C.wl_display_get_registry(display)
		if registry == unsafe { nil } {
			C.wl_display_disconnect(display)
			return error(err_wayland_registry_failed)
		}
		backend.display = unsafe { voidptr(display) }
		backend.registry = unsafe { voidptr(registry) }
		if C.v_multiwindow_wayland_add_registry_listener(registry, backend.registry_listener_data()) < 0 {
			backend.close_connection()
			return error(err_wayland_registry_failed)
		}
		if C.wl_display_roundtrip(display) < 0 {
			backend.close_connection()
			return error(err_wayland_dispatch_failed)
		}
		C.wl_registry_destroy(registry)
		backend.registry = unsafe { nil }
		if backend.compositor == unsafe { nil } || backend.wm_base == unsafe { nil } {
			backend.close_connection()
			return error(err_wayland_required_globals_missing)
		}
		wm_base := unsafe { &C.xdg_wm_base(backend.wm_base) }
		if C.v_multiwindow_wayland_add_xdg_wm_base_listener(wm_base, unsafe { nil }) < 0 {
			backend.close_connection()
			return error(err_wayland_registry_failed)
		}
		if require_renderer {
			backend.init_renderer() or {
				backend.close_connection()
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

fn (backend &WaylandBackend) renderer_ready() bool {
	return backend.egl_display != unsafe { nil } && backend.egl_config != unsafe { nil }
		&& backend.egl_context != unsafe { nil }
}

fn (mut backend WaylandBackend) init_renderer() ! {
	$if linux && sokol_wayland ? {
		if backend.renderer_ready() {
			return
		}
		if backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		display := unsafe { &C.wl_display(backend.display) }
		egl_display := C.v_multiwindow_wayland_egl_get_display(display)
		if egl_display == unsafe { nil } {
			return error(err_wayland_egl_display_failed)
		}
		if C.v_multiwindow_wayland_egl_initialize(egl_display) == 0 {
			return error(err_wayland_egl_display_failed)
		}
		if C.v_multiwindow_wayland_egl_bind_opengl_api() == 0 {
			C.v_multiwindow_wayland_egl_terminate(egl_display)
			return error(err_wayland_egl_context_failed)
		}
		mut egl_config := voidptr(unsafe { nil })
		if C.v_multiwindow_wayland_egl_choose_config(egl_display, &egl_config) == 0 {
			C.v_multiwindow_wayland_egl_terminate(egl_display)
			return error(err_wayland_egl_config_failed)
		}
		egl_context := C.v_multiwindow_wayland_egl_create_context(egl_display, egl_config)
		if egl_context == unsafe { nil } {
			C.v_multiwindow_wayland_egl_terminate(egl_display)
			return error(err_wayland_egl_context_failed)
		}
		backend.egl_display = egl_display
		backend.egl_config = egl_config
		backend.egl_context = egl_context
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) shutdown_renderer() {
	$if linux && sokol_wayland ? {
		if backend.egl_display != unsafe { nil } {
			C.v_multiwindow_wayland_egl_clear_current(backend.egl_display)
			if backend.egl_context != unsafe { nil } {
				C.v_multiwindow_wayland_egl_destroy_context(backend.egl_display,
					backend.egl_context)
			}
			C.v_multiwindow_wayland_egl_terminate(backend.egl_display)
		}
		backend.egl_display = unsafe { nil }
		backend.egl_config = unsafe { nil }
		backend.egl_context = unsafe { nil }
	}
}

fn (mut backend WaylandBackend) create_window(id WindowId, config WindowConfig) !WindowSize {
	$if linux && sokol_wayland ? {
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		if backend.compositor == unsafe { nil } || backend.wm_base == unsafe { nil } {
			return error(err_wayland_required_globals_missing)
		}
		if !config.visible {
			return error(err_capability_unsupported)
		}
		display := unsafe { &C.wl_display(backend.display) }
		compositor := unsafe { &C.wl_compositor(backend.compositor) }
		wm_base := unsafe { &C.xdg_wm_base(backend.wm_base) }
		surface := C.wl_compositor_create_surface(compositor)
		if surface == unsafe { nil } {
			return error(err_wayland_create_surface_failed)
		}
		xdg_surface := C.v_multiwindow_wayland_xdg_wm_base_get_xdg_surface(wm_base, surface)
		if xdg_surface == unsafe { nil } {
			C.wl_surface_destroy(surface)
			return error(err_wayland_create_surface_failed)
		}
		xdg_toplevel := C.v_multiwindow_wayland_xdg_surface_get_toplevel(xdg_surface)
		if xdg_toplevel == unsafe { nil } {
			C.v_multiwindow_wayland_xdg_surface_destroy(xdg_surface)
			C.wl_surface_destroy(surface)
			return error(err_wayland_create_surface_failed)
		}
		actual_size := window_size_for_config(config, config.width, config.height)
		record_min_width := if config.resizable { config.min_width } else { actual_size.width }
		record_min_height := if config.resizable { config.min_height } else { actual_size.height }
		mut record := &WaylandWindowRecord{
			id:           id
			surface:      unsafe { voidptr(surface) }
			xdg_surface:  unsafe { voidptr(xdg_surface) }
			xdg_toplevel: unsafe { voidptr(xdg_toplevel) }
			width:        actual_size.width
			height:       actual_size.height
			min_width:    record_min_width
			min_height:   record_min_height
		}
		if C.v_multiwindow_wayland_add_xdg_surface_listener(xdg_surface, record.listener_data()) < 0 {
			backend.destroy_window_record(record)
			return error(err_wayland_create_surface_failed)
		}
		if C.v_multiwindow_wayland_add_xdg_toplevel_listener(xdg_toplevel, record.listener_data()) < 0 {
			backend.destroy_window_record(record)
			return error(err_wayland_create_surface_failed)
		}
		C.v_multiwindow_wayland_xdg_toplevel_set_title(xdg_toplevel, &char(config.title.str))
		C.v_multiwindow_wayland_xdg_toplevel_set_app_id(xdg_toplevel, c'v.x.multiwindow')
		if config.min_width > 0 || config.min_height > 0 {
			C.v_multiwindow_wayland_xdg_toplevel_set_min_size(xdg_toplevel, i32(config.min_width),
				i32(config.min_height))
		}
		if !config.resizable {
			C.v_multiwindow_wayland_xdg_toplevel_set_min_size(xdg_toplevel, i32(actual_size.width),
				i32(actual_size.height))
			C.v_multiwindow_wayland_xdg_toplevel_set_max_size(xdg_toplevel, i32(actual_size.width),
				i32(actual_size.height))
		}
		if config.fullscreen {
			C.v_multiwindow_wayland_xdg_toplevel_set_fullscreen(xdg_toplevel, unsafe { nil })
		}
		backend.windows << record
		C.wl_surface_commit(surface)
		if C.wl_display_flush(display) < 0 {
			backend.remove_window_record(id)
			backend.destroy_window_record(record)
			return error(err_wayland_flush_failed)
		}
		if C.wl_display_roundtrip(display) < 0 {
			backend.remove_window_record(id)
			backend.destroy_window_record(record)
			return error(err_wayland_dispatch_failed)
		}
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

fn (mut backend WaylandBackend) destroy_window(id WindowId) ! {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		mut record := backend.windows[index]
		backend.destroy_window_record(record)
		backend.windows.delete(index)
		if backend.started && backend.display != unsafe { nil } {
			display := unsafe { &C.wl_display(backend.display) }
			if C.wl_display_flush(display) < 0 {
				return error(err_wayland_flush_failed)
			}
		}
		return
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) set_window_title(id WindowId, title string) ! {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		record := backend.windows[index]
		if record.xdg_toplevel == unsafe { nil } {
			return error(err_window_not_found)
		}
		C.v_multiwindow_wayland_xdg_toplevel_set_title(unsafe {
			&C.xdg_toplevel(record.xdg_toplevel)
		}, &char(title.str))
		display := unsafe { &C.wl_display(backend.display) }
		if C.wl_display_flush(display) < 0 {
			return error(err_wayland_flush_failed)
		}
		return
	} $else {
		_ = id
		_ = title
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) resize_window(id WindowId, width int, height int) !WindowSize {
	$if linux && sokol_wayland ? {
		_ = width
		_ = height
		backend.window_record_index(id) or { return error(err_window_not_found) }
		return error(err_capability_unsupported)
	} $else {
		_ = id
		_ = width
		_ = height
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) poll_events() ![]Event {
	mut events := []Event{}
	$if linux && sokol_wayland ? {
		if !backend.started || backend.display == unsafe { nil } {
			return events
		}
		backend.dispatch_pending_nonblocking()!
		mut resized_windows := []Event{}
		mut close_requested_windows := []WindowId{}
		for i, record in backend.windows {
			if record.resize_event_pending {
				backend.windows[i].resize_event_pending = false
				resized_windows << Event{
					kind:      .window_resized
					window_id: record.id
					width:     record.width
					height:    record.height
				}
			}
			if record.close_requested {
				backend.windows[i].close_requested = false
				close_requested_windows << record.id
			}
		}
		for event in resized_windows {
			events << event
		}
		for id in close_requested_windows {
			events << Event{
				kind:      .window_close_requested
				window_id: id
			}
		}
	}
	return events
}

fn (mut backend WaylandBackend) stop() ! {
	$if linux && sokol_wayland ? {
		backend.close_connection()
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) dispatch_pending_nonblocking() ! {
	$if linux && sokol_wayland ? {
		if backend.display == unsafe { nil } {
			return
		}
		display := unsafe { &C.wl_display(backend.display) }
		if C.wl_display_flush(display) < 0 {
			return error(err_wayland_flush_failed)
		}
		for {
			dispatched := C.wl_display_dispatch_pending(display)
			if dispatched < 0 {
				return error(err_wayland_dispatch_failed)
			}
			if dispatched == 0 {
				break
			}
		}
		for {
			prepare_result := C.wl_display_prepare_read(display)
			if prepare_result != 0 {
				dispatched := C.wl_display_dispatch_pending(display)
				if dispatched < 0 {
					return error(err_wayland_dispatch_failed)
				}
				if dispatched == 0 {
					break
				}
				continue
			}
			fd := C.wl_display_get_fd(display)
			mut poll_fd := C.pollfd{
				fd:      fd
				events:  wayland_poll_in
				revents: i16(0)
			}
			poll_result := C.poll(&poll_fd, u64(1), 0)
			if poll_result < 0 {
				C.wl_display_cancel_read(display)
				return error(err_wayland_dispatch_failed)
			}
			if poll_result == 0 || (poll_fd.revents & wayland_poll_in) == i16(0) {
				C.wl_display_cancel_read(display)
				break
			}
			if C.wl_display_read_events(display) < 0 {
				return error(err_wayland_dispatch_failed)
			}
			for {
				dispatched := C.wl_display_dispatch_pending(display)
				if dispatched < 0 {
					return error(err_wayland_dispatch_failed)
				}
				if dispatched == 0 {
					break
				}
			}
		}
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn (mut backend WaylandBackend) render_environment(id WindowId) !gfx.Environment {
		$if linux && sokol_wayland ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			backend.prepare_render_window(index)!
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

	fn (mut backend WaylandBackend) begin_render(id WindowId) !RenderFrame {
		$if linux && sokol_wayland ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			backend.prepare_render_window(index)!
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

	fn (mut backend WaylandBackend) end_render(frame RenderFrame) ! {
		$if linux && sokol_wayland ? {
			index := backend.window_record_index(frame.window_id) or {
				return error(err_window_not_found)
			}
			record := backend.windows[index]
			backend.make_current(record)!
			if C.v_multiwindow_wayland_egl_swap_buffers(backend.egl_display, record.egl_surface) == 0 {
				return error(err_wayland_egl_swap_buffers_failed)
			}
			if backend.display != unsafe { nil } {
				display := unsafe { &C.wl_display(backend.display) }
				if C.wl_display_flush(display) < 0 {
					return error(err_wayland_flush_failed)
				}
			}
			return
		} $else {
			_ = frame
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) prepare_render_window(index int) ! {
		$if linux && sokol_wayland ? {
			if !backend.renderer_ready() {
				return error(err_renderer_unsupported)
			}
			backend.ensure_configured(index)!
			backend.apply_pending_configure(index)!
			backend.ensure_window_render_target(index)!
			record := backend.windows[index]
			backend.make_current(record)!
			return
		} $else {
			_ = index
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) ensure_configured(index int) ! {
		$if linux && sokol_wayland ? {
			mut record := backend.windows[index]
			if record.configured {
				return
			}
			backend.dispatch_pending_nonblocking()!
			if record.configured {
				return
			}
			if backend.display == unsafe { nil } {
				return error(err_wayland_connect_failed)
			}
			display := unsafe { &C.wl_display(backend.display) }
			if C.wl_display_roundtrip(display) < 0 {
				return error(err_wayland_dispatch_failed)
			}
			if !record.configured {
				return error(err_wayland_surface_not_configured)
			}
			return
		} $else {
			_ = index
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) apply_pending_configure(index int) ! {
		$if linux && sokol_wayland ? {
			mut record := backend.windows[index]
			if !record.pending_egl_resize {
				return
			}
			if record.wl_egl_window != unsafe { nil } {
				C.v_multiwindow_wayland_egl_resize_window(record.wl_egl_window,
					safe_wayland_extent(record.width), safe_wayland_extent(record.height))
			}
			record.pending_egl_resize = false
			return
		} $else {
			_ = index
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) ensure_window_render_target(index int) ! {
		$if linux && sokol_wayland ? {
			mut record := backend.windows[index]
			if record.wl_egl_window == unsafe { nil } {
				if record.surface == unsafe { nil } {
					return error(err_window_not_found)
				}
				egl_window := C.v_multiwindow_wayland_egl_create_window(unsafe {
					&C.wl_surface(record.surface)
				}, safe_wayland_extent(record.width), safe_wayland_extent(record.height))
				if egl_window == unsafe { nil } {
					return error(err_wayland_egl_surface_failed)
				}
				record.wl_egl_window = egl_window
			}
			if record.egl_surface == unsafe { nil } {
				egl_surface := C.v_multiwindow_wayland_egl_create_window_surface(backend.egl_display,
					backend.egl_config, record.wl_egl_window)
				if egl_surface == unsafe { nil } {
					return error(err_wayland_egl_surface_failed)
				}
				record.egl_surface = egl_surface
			}
			return
		} $else {
			_ = index
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) make_current(record &WaylandWindowRecord) ! {
		$if linux && sokol_wayland ? {
			if !backend.renderer_ready() || record.egl_surface == unsafe { nil } {
				return error(err_renderer_unsupported)
			}
			if C.v_multiwindow_wayland_egl_make_current(backend.egl_display, record.egl_surface,
				backend.egl_context) == 0 {
				return error(err_wayland_egl_make_current_failed)
			}
			return
		} $else {
			_ = record
			return error(err_backend_unsupported)
		}
	}

	fn (backend &WaylandBackend) swapchain_for_record(record &WaylandWindowRecord) gfx.Swapchain {
		width := safe_wayland_extent(record.width)
		height := safe_wayland_extent(record.height)
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

fn safe_wayland_extent(value int) int {
	if value > 0 {
		return value
	}
	return 1
}

fn (mut backend WaylandBackend) close_connection() {
	$if linux && sokol_wayland ? {
		for backend.windows.len > 0 {
			record := backend.windows[0]
			backend.destroy_window_record(record)
			backend.windows.delete(0)
		}
		backend.shutdown_renderer()
		if backend.wm_base != unsafe { nil } {
			C.v_multiwindow_wayland_xdg_wm_base_destroy(unsafe { &C.xdg_wm_base(backend.wm_base) })
		}
		if backend.compositor != unsafe { nil } && backend.compositor_version >= u32(4) {
			C.wl_compositor_destroy(unsafe { &C.wl_compositor(backend.compositor) })
		}
		if backend.registry != unsafe { nil } {
			C.wl_registry_destroy(unsafe { &C.wl_registry(backend.registry) })
		}
		if backend.display != unsafe { nil } {
			display := unsafe { &C.wl_display(backend.display) }
			C.wl_display_flush(display)
			C.wl_display_disconnect(display)
		}
		backend.display = unsafe { nil }
		backend.registry = unsafe { nil }
		backend.compositor = unsafe { nil }
		backend.compositor_version = 0
		backend.wm_base = unsafe { nil }
		backend.started = false
	}
}

fn (backend &WaylandBackend) destroy_window_record(record &WaylandWindowRecord) {
	$if linux && sokol_wayland ? {
		if backend.egl_display != unsafe { nil } && record.egl_surface != unsafe { nil } {
			C.v_multiwindow_wayland_egl_clear_current(backend.egl_display)
			C.v_multiwindow_wayland_egl_destroy_surface(backend.egl_display, record.egl_surface)
		}
		if record.wl_egl_window != unsafe { nil } {
			C.v_multiwindow_wayland_egl_destroy_window(record.wl_egl_window)
		}
		if record.xdg_toplevel != unsafe { nil } {
			C.v_multiwindow_wayland_xdg_toplevel_destroy(unsafe {
				&C.xdg_toplevel(record.xdg_toplevel)
			})
		}
		if record.xdg_surface != unsafe { nil } {
			C.v_multiwindow_wayland_xdg_surface_destroy(unsafe { &C.xdg_surface(record.xdg_surface) })
		}
		if record.surface != unsafe { nil } {
			C.wl_surface_destroy(unsafe { &C.wl_surface(record.surface) })
		}
	}
}

fn (backend &WaylandBackend) window_record_index(id WindowId) ?int {
	for i, record in backend.windows {
		if record.id == id {
			return i
		}
	}
	return none
}

fn (mut backend WaylandBackend) remove_window_record(id WindowId) {
	if index := backend.window_record_index(id) {
		backend.windows.delete(index)
	}
}
