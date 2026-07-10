module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn (mut backend Backend) render_environment(id WindowId) !gfx.Environment {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.mock { return error(err_renderer_unsupported) }
			.x11 { return backend.x11.render_environment(id)! }
			.wayland { return backend.wayland.render_environment(id)! }
			.appkit { return backend.appkit.render_environment(id)! }
			.win32 { return backend.win32.render_environment(id)! }
		}
	}

	fn (mut backend Backend) begin_render(id WindowId) !RenderFrame {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.mock { return error(err_renderer_unsupported) }
			.x11 { return backend.x11.begin_render(id)! }
			.wayland { return backend.wayland.begin_render(id)! }
			.appkit { return backend.appkit.begin_render(id)! }
			.win32 { return backend.win32.begin_render(id)! }
		}
	}

	fn (mut backend Backend) end_render(frame RenderFrame) ! {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.mock { return error(err_renderer_unsupported) }
			.x11 { backend.x11.end_render(frame)! }
			.wayland { backend.wayland.end_render(frame)! }
			.appkit { backend.appkit.end_render(frame)! }
			.win32 { backend.win32.end_render(frame)! }
		}
	}

	fn (mut backend Backend) abort_render(frame RenderFrame) ! {
		match backend.kind {
			.auto { return error(err_backend_unsupported) }
			.mock { return error(err_renderer_unsupported) }
			.x11 {}
			.wayland {}
			.appkit { backend.appkit.abort_render(frame)! }
			.win32 {}
		}
	}
}
