module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	// RenderFrame describes a window render target prepared by begin_render().
	pub struct RenderFrame {
	pub:
		window_id WindowId
		swapchain gfx.Swapchain
	}

	// render_environment makes the window's renderer context current and returns
	// the environment gg should pass to gfx.setup(...).
	pub fn (mut app App) render_environment(id WindowId) !gfx.Environment {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		defer {
			app.state_mutex.unlock()
		}
		app.ensure_running_locked()!
		app.live_window_index(id)!
		return app.backend.render_environment(id)!
	}

	// begin_render makes the window current and returns its current swapchain.
	pub fn (mut app App) begin_render(id WindowId) !RenderFrame {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		defer {
			app.state_mutex.unlock()
		}
		app.ensure_running_locked()!
		app.live_window_index(id)!
		return app.backend.begin_render(id)!
	}

	// end_render presents the frame prepared by begin_render().
	pub fn (mut app App) end_render(frame RenderFrame) ! {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		defer {
			app.state_mutex.unlock()
		}
		app.ensure_running_locked()!
		app.live_window_index(frame.window_id)!
		app.backend.end_render(frame)!
	}

	// abort_render releases backend resources acquired by begin_render() without
	// presenting the frame. It is intended for error paths before gfx.commit().
	pub fn (mut app App) abort_render(frame RenderFrame) ! {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		defer {
			app.state_mutex.unlock()
		}
		app.live_window_index(frame.window_id)!
		app.backend.abort_render(frame)!
	}
}
