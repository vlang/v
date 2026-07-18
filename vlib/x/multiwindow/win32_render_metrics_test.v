module multiwindow

$if windows {
	#flag windows -DV_MULTIWINDOW_WIN32_RENDER_METRICS_TEST

	fn C.v_multiwindow_test_win32_configure_render_fixture(client_width int, client_height int, visible int, minimized int, dpi u32, conversion_mode int)
	fn C.v_multiwindow_test_win32_reset_render_fixture()
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	$if windows && sokol_d3d11 ? {
		fn win32_logical_conversion_error_for_test(backend &Win32Backend, id WindowId) string {
			backend.logical_to_pixel_rect(id, 1, 2, 3, 4) or { return err.msg() }
			return ''
		}

		fn win32_pixel_conversion_error_for_test(backend &Win32Backend, id WindowId) string {
			backend.pixel_to_logical_rect(id, 1, 2, 3, 4) or { return err.msg() }
			return ''
		}
	}
}

fn test_win32_render_readiness_does_not_depend_on_coordinate_conversion() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if windows && sokol_d3d11 ? {
			id := WindowId{
				app_instance: 1
				slot:         0
				generation:   1
			}
			mut native_window_sentinel := 0
			record := &Win32WindowRecord{
				id:   id
				hwnd: voidptr(&native_window_sentinel)
			}
			backend := Win32Backend{
				windows: [record]
			}
			for conversion_mode in [1, 2] {
				C.v_multiwindow_test_win32_configure_render_fixture(640, 480, 1, 0, 192,
					conversion_mode)
				mut visible := 0
				mut minimized := 0
				mut logical_width := 0
				mut logical_height := 0
				mut framebuffer_width := 0
				mut framebuffer_height := 0
				mut dpi_scale := f32(0)
				mut conversion_available := 0
				available := C.v_multiwindow_win32_render_snapshot(record.hwnd, &visible,
					&minimized, &logical_width, &logical_height, &framebuffer_width,
					&framebuffer_height, &dpi_scale, &conversion_available) != 0
				assert available
				assert visible == 1
				assert minimized == 0
				assert logical_width == 320
				assert logical_height == 240
				assert framebuffer_width == 640
				assert framebuffer_height == 480
				assert dpi_scale == f32(2)
				assert conversion_available == if conversion_mode == 2 {
					1
				} else {
					0
				}

				observation := win32_render_observation(available, visible, minimized,
					framebuffer_width, framebuffer_height, dpi_scale, conversion_available)
				update := win32_render_update(id, u64(conversion_mode), .ready, 1, false,
					observation)
				assert update.ready_credit
				assert update.block_reason == .none
				assert update.metrics.metrics_available
				assert update.metrics.logical_width == f32(320)
				assert update.metrics.logical_height == f32(240)
				assert update.metrics.framebuffer_width == 640
				assert update.metrics.framebuffer_height == 480
				assert update.metrics.dpi_scale == f32(2)
				assert update.metrics.conversion_available == (conversion_mode == 2)

				assert win32_logical_conversion_error_for_test(&backend, id) == err_render_conversion_unavailable
				assert win32_pixel_conversion_error_for_test(&backend, id) == err_render_conversion_unavailable
			}
			C.v_multiwindow_test_win32_reset_render_fixture()
		}
	}
}

fn test_win32_observed_framebuffer_change_resizes_target_once() {
	$if windows && sokol_d3d11 ? && (gg_multiwindow ? || x_multiwindow_render ?)
		&& (multiwindow_d3d11_warp ? || gg_multiwindow_d3d11_warp ?) {
		mut app := new_app(
			backend:          .win32
			queue_size:       8
			require_renderer: true
		)!
		defer {
			C.v_multiwindow_test_win32_reset_render_fixture()
			if app.status() != .stopped {
				app.stop() or {}
			}
			if app.backend.native_operations.proof != unsafe { nil } {
				_ = app.backend.native_operations.disarm_proof()
			}
		}
		app.start_renderer(RendererConfig{})!
		assert app.backend.win32.using_warp
		C.v_multiwindow_test_win32_configure_render_fixture(320, 240, 1, 0, 96, 1)
		window := app.create_window(
			title:           'observed framebuffer resize'
			width:           320
			height:          240
			visible:         true
			redraw_mode:     .on_demand
			render_workload: true
		)!
		for _ in 0 .. 64 {
			app.poll_events()!
			if app.render_window_eligible(window)! {
				break
			}
		}
		assert app.render_window_eligible(window)!
		index := app.backend.win32.window_record_index(window) or { panic(err_window_not_found) }
		_ = app.backend.win32.render_environment(window)!
		baseline := app.backend.win32.windows[index]
		assert baseline.framebuffer_width == 320
		assert baseline.framebuffer_height == 240
		assert !baseline.render_resize_pending
		assert baseline.swapchain != unsafe { nil }
		assert baseline.render_view != unsafe { nil }
		assert baseline.depth_texture != unsafe { nil }
		assert baseline.depth_stencil_view != unsafe { nil }
		generation := baseline.render_target_generation
		expected_generation := next_backend_target_generation(generation)!
		_ = app.drain_events()!
		_ = app.drain_input_events()!
		queued_events := baseline.queued_events.clone()
		assert app.backend.native_operations.arm_proof()

		C.v_multiwindow_test_win32_configure_render_fixture(640, 480, 0, 0, 96, 1)
		hidden_updates := app.backend.win32.collect_render_updates()!
		assert hidden_updates.len == 1
		hidden := hidden_updates[0]
		assert hidden.window == window
		assert !hidden.ready_credit
		assert hidden.block_reason == .hidden
		assert hidden.target.target_identity == expected_generation
		assert hidden.metrics.framebuffer_width == 640
		assert hidden.metrics.framebuffer_height == 480
		after_hidden := app.backend.win32.windows[index]
		assert after_hidden.render_target_generation == expected_generation
		assert after_hidden.render_resize_pending
		assert after_hidden.queued_events == queued_events

		C.v_multiwindow_test_win32_configure_render_fixture(640, 480, 1, 0, 96, 1)
		resized_updates := app.backend.win32.collect_render_updates()!
		assert resized_updates.len == 1
		resized := resized_updates[0]
		assert resized.window == window
		assert !resized.ready_credit
		assert resized.block_reason == .resize_pending
		assert resized.target.target_identity == expected_generation
		after_resize := app.backend.win32.windows[index]
		assert after_resize.render_target_generation == expected_generation
		assert !after_resize.render_resize_pending
		assert after_resize.queued_events == queued_events
		proof := app.backend.native_operations.proof
		resize_calls := proof.trace[..proof.trace_len].filter(it.milestone == .real_call
			&& it.context.operation == .resize_buffers)
		assert resize_calls.len == 1
		resize_context := resize_calls[0].context
		assert resize_context.domain == .dxgi
		assert resize_context.call_site == .display_transport
		assert resize_context.scope == .window_target
		assert resize_context.window == window
		assert resize_context.target_generation == expected_generation
		resize_acceptances := proof.trace[..proof.trace_len].filter(it.milestone == .acceptance
			&& it.context.operation == .resize_buffers)
		assert resize_acceptances.len == 1
		assert resize_acceptances[0].result.succeeded()

		stable_updates := app.backend.win32.collect_render_updates()!
		assert stable_updates.len == 1
		stable := stable_updates[0]
		assert stable.window == window
		assert stable.ready_credit
		assert stable.block_reason == .none
		assert stable.target.target_identity == expected_generation
		stable_record := app.backend.win32.windows[index]
		assert stable_record.render_target_generation == expected_generation
		assert !stable_record.render_resize_pending
		assert stable_record.queued_events == queued_events
		stable_resize_calls := proof.trace[..proof.trace_len].filter(it.milestone == .real_call
			&& it.context.operation == .resize_buffers)
		assert stable_resize_calls.len == 1
		assert app.drain_events()!.len == 0
		assert app.drain_input_events()!.len == 0

		C.v_multiwindow_test_win32_reset_render_fixture()
		app.stop()!
		assert app.backend.native_operations.disarm_proof()
	}
}

fn test_win32_observed_framebuffer_change_does_not_advance_pending_resize() {
	$if windows && sokol_d3d11 ? && (gg_multiwindow ? || x_multiwindow_render ?) {
		mut native_window_sentinel := 0
		record := &Win32WindowRecord{
			hwnd:                     voidptr(&native_window_sentinel)
			framebuffer_width:        320
			framebuffer_height:       240
			render_resize_pending:    true
			render_target_generation: 2
		}
		mut backend := Win32Backend{
			render_health: .ready
			windows:       [record]
		}
		C.v_multiwindow_test_win32_configure_render_fixture(640, 480, 0, 0, 96, 1)
		defer {
			C.v_multiwindow_test_win32_reset_render_fixture()
		}
		generation := record.render_target_generation
		updates := backend.collect_render_updates()!
		assert updates.len == 1
		assert backend.windows[0].render_resize_pending
		assert backend.windows[0].render_target_generation == generation
		assert backend.windows[0].framebuffer_width == 640
		assert backend.windows[0].framebuffer_height == 480
	}
}
