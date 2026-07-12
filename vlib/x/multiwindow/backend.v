module multiwindow

import os

struct Backend {
mut:
	kind                       BackendKind
	allow_mock_render_fallback bool
	mock                       MockBackend
	x11                        X11Backend
	wayland                    WaylandBackend
	appkit                     AppKitBackend
	win32                      Win32Backend
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
			backend.x11.start(true)!
			caps := backend.x11.capabilities()
			backend.x11.stop()!
			caps
		}
		.wayland {
			backend.wayland.start(true)!
			caps := backend.wayland.capabilities()
			backend.wayland.stop()!
			caps
		}
		.appkit {
			backend.appkit.start(true)!
			caps := backend.appkit.capabilities()
			backend.appkit.stop()!
			caps
		}
		.win32 {
			backend.win32.start(true)!
			caps := backend.win32.capabilities()
			backend.win32.stop()!
			caps
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
	backend.ensure_supported()!
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
}

fn (mut backend Backend) create_window(id WindowId, config WindowConfig) !WindowSize {
	caps := backend.capabilities()
	if !caps.multi_window {
		return error(err_capability_unsupported)
	}
	match backend.kind {
		.auto { return error(err_backend_unsupported) }
		.mock { return backend.mock.create_window(id, config)! }
		.x11 { return backend.x11.create_window(id, config)! }
		.wayland { return backend.wayland.create_window(id, config)! }
		.appkit { return backend.appkit.create_window(id, config)! }
		.win32 { return backend.win32.create_window(id, config)! }
	}
}

fn (mut backend Backend) destroy_window(id WindowId) ! {
	match backend.kind {
		.auto { return error(err_backend_unsupported) }
		.mock { backend.mock.destroy_window(id)! }
		.x11 { backend.x11.destroy_window(id)! }
		.wayland { backend.wayland.destroy_window(id)! }
		.appkit { backend.appkit.destroy_window(id)! }
		.win32 { backend.win32.destroy_window(id)! }
	}
}

fn (mut backend Backend) set_window_title(id WindowId, title string) ! {
	match backend.kind {
		.auto { return error(err_backend_unsupported) }
		.mock { backend.mock.set_window_title(id, title)! }
		.x11 { backend.x11.set_window_title(id, title)! }
		.wayland { backend.wayland.set_window_title(id, title)! }
		.appkit { backend.appkit.set_window_title(id, title)! }
		.win32 { backend.win32.set_window_title(id, title)! }
	}
}

fn (mut backend Backend) resize_window(id WindowId, width int, height int) !WindowSize {
	match backend.kind {
		.auto { return error(err_backend_unsupported) }
		.mock { return backend.mock.resize_window(id, width, height)! }
		.x11 { return backend.x11.resize_window(id, width, height)! }
		.wayland { return backend.wayland.resize_window(id, width, height)! }
		.appkit { return backend.appkit.resize_window(id, width, height)! }
		.win32 { return backend.win32.resize_window(id, width, height)! }
	}
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
	match backend.kind {
		.auto { return error(err_backend_unsupported) }
		.mock { backend.mock.set_window_cursor(id, shape)! }
		.x11 { backend.x11.set_window_cursor(id, shape)! }
		.wayland { backend.wayland.set_window_cursor(id, shape)! }
		.appkit { backend.appkit.set_window_cursor(id, shape)! }
		.win32 { backend.win32.set_window_cursor(id, shape)! }
	}
}

fn (mut backend Backend) begin_window_move(id WindowId) ! {
	match backend.kind {
		.wayland { backend.wayland.begin_window_move(id)! }
		.auto { return error(err_backend_unsupported) }
		else { return error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) begin_window_resize(id WindowId, edge WindowResizeEdge) ! {
	match backend.kind {
		.wayland { backend.wayland.begin_window_resize(id, edge)! }
		.auto { return error(err_backend_unsupported) }
		else { return error(err_capability_unsupported) }
	}
}

fn (mut backend Backend) poll_events() ![]Event {
	match backend.kind {
		.auto { return error(err_backend_unsupported) }
		.mock { return backend.mock.poll_events()! }
		.x11 { return backend.x11.poll_events()! }
		.wayland { return backend.wayland.poll_events()! }
		.appkit { return backend.appkit.poll_events()! }
		.win32 { return backend.win32.poll_events()! }
	}
}

fn (mut backend Backend) poll_queued_events() ![]QueuedEvent {
	match backend.kind {
		.mock {
			return backend.mock.poll_queued_events()!
		}
		.x11 {
			return backend.x11.poll_queued_events()!
		}
		.wayland {
			return backend.wayland.poll_queued_events()!
		}
		.appkit {
			return backend.appkit.poll_queued_events()!
		}
		.win32 {
			return backend.win32.poll_queued_events()!
		}
		else {
			events := backend.poll_events()!
			mut queued_events := []QueuedEvent{cap: events.len}
			for event in events {
				queued_events << queued_lifecycle_event(event)
			}
			return queued_events
		}
	}
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
