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

	fn C.v_multiwindow_appkit_is_main_thread() int
	fn C.v_multiwindow_appkit_prepare_application() int
	fn C.v_multiwindow_appkit_create_metal_device(out_device &voidptr) int
	fn C.v_multiwindow_appkit_release_metal_device(device voidptr)
	fn C.v_multiwindow_appkit_create_window(device voidptr, title &char, width int, height int, min_width int, min_height int, resizable int, visible int, high_dpi int, borderless int, fullscreen int, out_state &voidptr, out_width &int, out_height &int, out_framebuffer_width &int, out_framebuffer_height &int) int
	fn C.v_multiwindow_appkit_destroy_window(state voidptr)
	fn C.v_multiwindow_appkit_release_window(state voidptr)
	fn C.v_multiwindow_appkit_set_window_title(state voidptr, title &char) int
	fn C.v_multiwindow_appkit_resize_window(state voidptr, width int, height int, out_width &int, out_height &int, out_framebuffer_width &int, out_framebuffer_height &int) int
	fn C.v_multiwindow_appkit_poll_events()
	fn C.v_multiwindow_appkit_take_close_requested(state voidptr) int
	fn C.v_multiwindow_appkit_take_resized(state voidptr, out_width &int, out_height &int, out_framebuffer_width &int, out_framebuffer_height &int) int
	fn C.v_multiwindow_appkit_take_destroyed(state voidptr) int
	fn C.v_multiwindow_appkit_begin_frame(state voidptr, device voidptr, out_drawable &voidptr, out_depth_texture &voidptr, out_framebuffer_width &int, out_framebuffer_height &int) int
	fn C.v_multiwindow_appkit_end_frame(state voidptr)
	fn C.v_multiwindow_appkit_abort_frame(state voidptr)
}

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

fn (mut backend AppKitBackend) poll_events() ![]Event {
	mut events := []Event{}
	$if darwin {
		if !backend.started {
			return events
		}
		C.v_multiwindow_appkit_poll_events()
		mut i := 0
		for i < backend.windows.len {
			record := backend.windows[i]
			if C.v_multiwindow_appkit_take_destroyed(record.state) != 0 {
				C.v_multiwindow_appkit_release_window(record.state)
				backend.windows.delete(i)
				events << Event{
					kind:      .window_destroyed
					window_id: record.id
				}
				continue
			}
			mut width := 0
			mut height := 0
			mut framebuffer_width := 0
			mut framebuffer_height := 0
			if C.v_multiwindow_appkit_take_resized(record.state, &width, &height,
				&framebuffer_width, &framebuffer_height) != 0 {
				backend.windows[i].width = width
				backend.windows[i].height = height
				backend.windows[i].framebuffer_width = framebuffer_width
				backend.windows[i].framebuffer_height = framebuffer_height
				events << Event{
					kind:      .window_resized
					window_id: record.id
					width:     width
					height:    height
				}
			}
			if C.v_multiwindow_appkit_take_close_requested(record.state) != 0 {
				events << Event{
					kind:      .window_close_requested
					window_id: record.id
				}
			}
			i++
		}
	}
	return events
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
