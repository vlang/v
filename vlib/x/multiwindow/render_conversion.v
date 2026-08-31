module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import math
}

pub fn (app &App) logical_to_pixel_render_rect(id WindowId, metrics_sequence u64, x f32, y f32, width f32, height f32) !(int, int, int, int) {
	app.assert_owner_thread()!
	app.validate_render_conversion_snapshot(id, metrics_sequence)!
	$if gg_multiwindow ? || x_multiwindow_render ? {
		return app.backend.logical_to_pixel_rect(id, x, y, width, height)
	} $else {
		return error(err_renderer_unsupported)
	}
}

pub fn (app &App) pixel_to_logical_render_rect(id WindowId, metrics_sequence u64, x int, y int, width int, height int) !(f32, f32, f32, f32) {
	app.assert_owner_thread()!
	app.validate_render_conversion_snapshot(id, metrics_sequence)!
	$if gg_multiwindow ? || x_multiwindow_render ? {
		return app.backend.pixel_to_logical_rect(id, x, y, width, height)
	} $else {
		return error(err_renderer_unsupported)
	}
}

fn (app &App) validate_render_conversion_snapshot(id WindowId, metrics_sequence u64) ! {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	index := app.render_window_index_locked(id)!
	metrics := app.render_runtime.windows[index].metrics
	if !metrics.metrics_available || !metrics.conversion_available || metrics.metrics_sequence == 0
		|| metrics.metrics_sequence != metrics_sequence {
		return error(err_render_conversion_unavailable)
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn (backend &Backend) logical_to_pixel_rect(id WindowId, x f32, y f32, width f32, height f32) !(int, int, int, int) {
		match backend.kind {
			.x11, .wayland {
				left := int(math.floor(f64(x)))
				top := int(math.floor(f64(y)))
				right := int(math.ceil(f64(x + width)))
				bottom := int(math.ceil(f64(y + height)))
				return left, top, right - left, bottom - top
			}
			.appkit {
				return backend.appkit.logical_to_pixel_rect(id, x, y, width, height)
			}
			.win32 {
				return backend.win32.logical_to_pixel_rect(id, x, y, width, height)
			}
			else {
				return error(err_render_conversion_unavailable)
			}
		}
	}

	fn (backend &Backend) pixel_to_logical_rect(id WindowId, x int, y int, width int, height int) !(f32, f32, f32, f32) {
		match backend.kind {
			.x11, .wayland { return f32(x), f32(y), f32(width), f32(height) }
			.appkit { return backend.appkit.pixel_to_logical_rect(id, x, y, width, height) }
			.win32 { return backend.win32.pixel_to_logical_rect(id, x, y, width, height) }
			else { return error(err_render_conversion_unavailable) }
		}
	}

	fn (backend &AppKitBackend) logical_to_pixel_rect(id WindowId, x f32, y f32, width f32, height f32) !(int, int, int, int) {
		$if darwin {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			mut out_x := 0
			mut out_y := 0
			mut out_width := 0
			mut out_height := 0
			if C.v_multiwindow_appkit_logical_to_pixel_rect(backend.windows[index].state, x, y,
				width, height, &out_x, &out_y, &out_width, &out_height) == 0 {
				return error(err_render_conversion_unavailable)
			}
			return out_x, out_y, out_width, out_height
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn (backend &AppKitBackend) pixel_to_logical_rect(id WindowId, x int, y int, width int, height int) !(f32, f32, f32, f32) {
		$if darwin {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			mut out_x := f32(0)
			mut out_y := f32(0)
			mut out_width := f32(0)
			mut out_height := f32(0)
			if C.v_multiwindow_appkit_pixel_to_logical_rect(backend.windows[index].state, x, y,
				width, height, &out_x, &out_y, &out_width, &out_height) == 0 {
				return error(err_render_conversion_unavailable)
			}
			return out_x, out_y, out_width, out_height
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn (backend &Win32Backend) logical_to_pixel_rect(id WindowId, x f32, y f32, width f32, height f32) !(int, int, int, int) {
		$if windows {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			mut out_x := 0
			mut out_y := 0
			mut out_width := 0
			mut out_height := 0
			if C.v_multiwindow_win32_logical_to_pixel_rect(backend.windows[index].hwnd, x, y,
				width, height, &out_x, &out_y, &out_width, &out_height) == 0 {
				return error(err_render_conversion_unavailable)
			}
			return out_x, out_y, out_width, out_height
		} $else {
			return error(err_backend_unsupported)
		}
	}

	fn (backend &Win32Backend) pixel_to_logical_rect(id WindowId, x int, y int, width int, height int) !(f32, f32, f32, f32) {
		$if windows {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			mut out_x := f32(0)
			mut out_y := f32(0)
			mut out_width := f32(0)
			mut out_height := f32(0)
			if C.v_multiwindow_win32_pixel_to_logical_rect(backend.windows[index].hwnd, x, y,
				width, height, &out_x, &out_y, &out_width, &out_height) == 0 {
				return error(err_render_conversion_unavailable)
			}
			return out_x, out_y, out_width, out_height
		} $else {
			return error(err_backend_unsupported)
		}
	}
}
