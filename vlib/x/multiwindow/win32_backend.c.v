module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if windows {
	#flag windows -DUNICODE
	#flag windows -D_UNICODE
	#flag windows -luser32
	#flag windows -lgdi32
	#include <windows.h>
	#insert "@VMODROOT/vlib/x/multiwindow/win32_backend_helpers.h"
}

@[heap; markused]
struct Win32WindowRecord {
	id WindowId
mut:
	hwnd                  voidptr
	config                WindowConfig
	width                 int
	height                int
	close_requested       bool
	destroyed             bool
	resize_event_pending  bool
	render_resize_pending bool
	swapchain             voidptr
	render_view           voidptr
	depth_texture         voidptr
	depth_stencil_view    voidptr
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
	fn C.v_multiwindow_win32_set_client_size(hwnd voidptr, width int, height int, min_width int, min_height int, resizable int, borderless int, fullscreen int) int
	fn C.v_multiwindow_win32_client_width(hwnd voidptr) int
	fn C.v_multiwindow_win32_client_height(hwnd voidptr) int
	fn C.v_multiwindow_win32_pump_messages() int
}

$if windows {
	@[export: 'v_multiwindow_win32_window_close_requested']
	@[markused]
	fn win32_window_close_requested(data voidptr) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &Win32WindowRecord(data) }
		record.close_requested = true
	}

	@[export: 'v_multiwindow_win32_window_destroyed']
	@[markused]
	fn win32_window_destroyed(data voidptr) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &Win32WindowRecord(data) }
		record.destroyed = true
		record.hwnd = unsafe { nil }
	}

	@[export: 'v_multiwindow_win32_window_resized']
	@[markused]
	fn win32_window_resized(data voidptr, width int, height int) {
		if data == unsafe { nil } || width <= 0 || height <= 0 {
			return
		}
		mut record := unsafe { &Win32WindowRecord(data) }
		if record.width == width && record.height == height {
			return
		}
		record.width = width
		record.height = height
		record.resize_event_pending = true
		record.render_resize_pending = true
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
		record.resize_event_pending = false
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
		if C.v_multiwindow_win32_set_client_size(record.hwnd, width, height,
			record.config.min_width, record.config.min_height,
			win32_bool_to_int(record.config.resizable),
			win32_bool_to_int(record.config.borderless),
			win32_bool_to_int(record.config.fullscreen)) == 0 {
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
		record.resize_event_pending = false
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

fn (mut backend Win32Backend) poll_events() ![]Event {
	mut events := []Event{}
	$if windows {
		if !backend.started {
			return events
		}
		C.v_multiwindow_win32_pump_messages()
		mut i := 0
		for i < backend.windows.len {
			mut record := backend.windows[i]
			if record.close_requested {
				record.close_requested = false
				events << Event{
					kind:      .window_close_requested
					window_id: record.id
				}
			}
			if record.resize_event_pending {
				record.resize_event_pending = false
				if record.width > 0 && record.height > 0 {
					events << Event{
						kind:      .window_resized
						window_id: record.id
						width:     record.width
						height:    record.height
					}
				}
			}
			if record.destroyed {
				id := record.id
				backend.release_window_render_resources(mut record)
				backend.windows.delete(i)
				events << Event{
					kind:      .window_destroyed
					window_id: id
				}
				continue
			}
			i++
		}
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
