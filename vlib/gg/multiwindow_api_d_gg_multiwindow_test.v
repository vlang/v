// vtest build: gg_multiwindow?
module gg

// Compile with `-d gg_multiwindow`; this test exercises the opt-in facade.
import os
import time
import sokol.sapp
import x.multiwindow
import gg.testdata.multiwindow_probe_watchdog

fn test_multiwindow_new_app_reports_core_capabilities() {
	mut app := new_app(backend: .mock, queue_size: 4)!
	caps := app.capabilities()

	assert caps.backend == .mock
	assert caps.mock
	assert !caps.native
	assert caps.multi_window
	assert caps.owner_queue
	assert !caps.explicit_swapchain
	assert caps.input_events
	assert caps.mouse_events
	assert caps.keyboard_events
	assert caps.text_events
	assert caps.focus_events
	assert caps.drop_events
	assert caps.touch_events
	assert !caps.cursor_shapes
	assert !caps.interactive_move_resize
	assert !caps.native_decorations

	app.stop()!
}

fn test_multiwindow_app_config_defaults_to_auto() {
	config := AppConfig{}

	assert config.backend == .auto
}

fn test_multiwindow_app_config_hides_renderer_requirement() {
	config := AppConfig{}
	core_config := config.to_core()

	assert config.backend == .auto
	assert !core_config.require_renderer
	assert AppConfig{
		require_renderer: true
	}.to_core().require_renderer
}

fn test_multiwindow_app_config_can_request_renderer_upfront() {
	render_config := AppConfig{
		backend:          .auto
		require_renderer: true
	}
	core_config := render_config.to_core()

	assert core_config.backend == .auto
	assert core_config.require_renderer
}

fn test_multiwindow_capabilities_for_backend_delegates_to_core() {
	caps := capabilities_for_backend(.mock)!

	assert caps.backend == .mock
	assert caps.mock
	assert caps.multi_window
	assert caps.owner_queue
	assert caps.input_events
	assert caps.mouse_events
	assert caps.keyboard_events
	assert caps.text_events
	assert caps.focus_events
	assert caps.drop_events
	assert caps.touch_events
	assert !caps.cursor_shapes
	assert !caps.interactive_move_resize
	assert !caps.native_decorations
}

fn test_multiwindow_plain_capabilities_use_core_probe_without_app_source_guard() {
	source := multiwindow_facade_source()
	plain_source :=
		source.all_after('// capabilities_for_backend reports capabilities for a backend policy.').all_before('// capabilities_for_backend_with_renderer')
	render_source :=
		source.all_after('// capabilities_for_backend_with_renderer reports capabilities while requiring').all_before('// create_window creates')

	assert plain_source.contains('multiwindow.capabilities_for_backend(backend_to_core(backend))!')
	assert !plain_source.contains('capabilities_for_app_config')
	assert !plain_source.contains('multiwindow.new_app')
	assert !plain_source.contains('.start(')
	assert !plain_source.contains('require_renderer: true')
	assert render_source.contains('multiwindow.capabilities_for_backend_with_renderer')
	assert render_source.contains('backend_to_core(backend)')
	assert render_source.contains('true')
}

fn test_multiwindow_resize_edge_conversion_matches_core() {
	assert window_resize_edge_to_core(.top) == multiwindow.WindowResizeEdge.top
	assert window_resize_edge_to_core(.bottom) == multiwindow.WindowResizeEdge.bottom
	assert window_resize_edge_to_core(.left) == multiwindow.WindowResizeEdge.left
	assert window_resize_edge_to_core(.right) == multiwindow.WindowResizeEdge.right
	assert window_resize_edge_to_core(.top_left) == multiwindow.WindowResizeEdge.top_left
	assert window_resize_edge_to_core(.top_right) == multiwindow.WindowResizeEdge.top_right
	assert window_resize_edge_to_core(.bottom_left) == multiwindow.WindowResizeEdge.bottom_left
	assert window_resize_edge_to_core(.bottom_right) == multiwindow.WindowResizeEdge.bottom_right
}

fn test_multiwindow_cursor_shape_conversion_matches_core() {
	assert window_cursor_shape_to_core(.default) == multiwindow.CursorShape.default
	assert window_cursor_shape_to_core(.pointer) == multiwindow.CursorShape.pointer
	assert window_cursor_shape_to_core(.move) == multiwindow.CursorShape.move
	assert window_cursor_shape_to_core(.n_resize) == multiwindow.CursorShape.n_resize
	assert window_cursor_shape_to_core(.s_resize) == multiwindow.CursorShape.s_resize
	assert window_cursor_shape_to_core(.e_resize) == multiwindow.CursorShape.e_resize
	assert window_cursor_shape_to_core(.w_resize) == multiwindow.CursorShape.w_resize
	assert window_cursor_shape_to_core(.ne_resize) == multiwindow.CursorShape.ne_resize
	assert window_cursor_shape_to_core(.nw_resize) == multiwindow.CursorShape.nw_resize
	assert window_cursor_shape_to_core(.se_resize) == multiwindow.CursorShape.se_resize
	assert window_cursor_shape_to_core(.sw_resize) == multiwindow.CursorShape.sw_resize
	assert window_cursor_shape_to_core(.ew_resize) == multiwindow.CursorShape.ew_resize
	assert window_cursor_shape_to_core(.ns_resize) == multiwindow.CursorShape.ns_resize
	assert window_cursor_shape_to_core(.nesw_resize) == multiwindow.CursorShape.nesw_resize
	assert window_cursor_shape_to_core(.nwse_resize) == multiwindow.CursorShape.nwse_resize
	assert window_cursor_shape_to_core(.grab) == multiwindow.CursorShape.grab
	assert window_cursor_shape_to_core(.grabbing) == multiwindow.CursorShape.grabbing
}

fn test_multiwindow_ci_does_not_run_gg_api_tests_under_fake_wayland_display() {
	source := multiwindow_business_multiwindow_workflow_sources()
	for line in source.split_into_lines() {
		if line.contains('vlib/gg/multiwindow_api_d_gg_multiwindow_test.v') {
			assert !line.contains('WAYLAND_DISPLAY=wayland-v-multiwindow-fake')
		}
	}
}

fn test_multiwindow_auto_capabilities_match_new_app_lifecycle_policy() {
	caps := capabilities_for_backend(.auto)!
	mut app := new_app() or {
		assert false, 'gg.new_app failed after lifecycle capabilities succeeded: ${err.msg()}'
		return
	}
	app_caps := app.capabilities()

	assert caps.backend == app_caps.backend
	assert caps.mock == app_caps.mock
	assert caps.native == app_caps.native
	assert caps.multi_window == app_caps.multi_window
	assert caps.owner_queue == app_caps.owner_queue
	assert caps.x11 == app_caps.x11
	assert caps.wayland == app_caps.wayland
	assert caps.win32 == app_caps.win32

	$if linux {
		match caps.backend {
			.x11 {
				assert caps.x11
				assert caps.native
			}
			.wayland {
				assert caps.wayland
				assert caps.native
			}
			.mock {
				assert caps.mock
				assert !caps.native
			}
			else {
				assert false, 'unexpected linux lifecycle backend: ${caps.backend}'
			}
		}
	} $else {
		$if windows {
			assert caps.backend == .win32
			assert caps.win32
			assert caps.native
			assert !caps.mock
			if caps.explicit_swapchain {
				assert caps.d3d11
			}
		} $else {
			$if darwin {
				assert caps.backend == .appkit
				assert caps.native
				assert !caps.mock
				if caps.explicit_swapchain {
					assert caps.metal
				}
			} $else {
				assert caps.backend == .mock
				assert caps.mock
				assert !caps.native
			}
		}
	}
	assert caps.multi_window
	assert caps.owner_queue

	app.stop()!
}

fn test_multiwindow_auto_capabilities_headless_match_new_app_mock_fallback() {
	snapshot := snapshot_graphical_env()
	defer {
		restore_graphical_env(snapshot)
	}
	set_graphical_env('', '')

	caps := capabilities_for_backend(.auto)!
	mut app := new_app() or { panic(err) }
	app_caps := app.capabilities()

	$if linux {
		assert caps.backend == .mock
		assert app_caps.backend == .mock
		assert caps.mock
		assert app_caps.mock
		assert !caps.native
		assert !app_caps.native
	} $else {
		$if windows {
			assert caps.backend == .win32
			assert app_caps.backend == .win32
			assert caps.win32
			assert app_caps.win32
			assert caps.native
			assert app_caps.native
			assert !caps.mock
			assert !app_caps.mock
		} $else {
			$if darwin {
				assert caps.backend == .appkit
				assert app_caps.backend == .appkit
				assert caps.native
				assert app_caps.native
				assert !caps.mock
				assert !app_caps.mock
			} $else {
				assert caps.backend == .mock
				assert app_caps.backend == .mock
				assert caps.mock
				assert app_caps.mock
				assert !caps.native
				assert !app_caps.native
			}
		}
	}
	assert caps.explicit_swapchain == app_caps.explicit_swapchain

	app.stop()!
}

fn test_multiwindow_auto_capabilities_with_invalid_wayland_display_fail_cleanly() {
	$if linux {
		snapshot := snapshot_graphical_env()
		defer {
			restore_graphical_env(snapshot)
		}
		set_graphical_env('', 'wayland-v-multiwindow-fake')

		new_app(backend: .wayland) or {
			assert err.msg() == 'multiwindow: wayland connect failed'
				|| err.msg() == 'multiwindow: backend is unsupported'
			return
		}
		assert false, 'gg.new_app connected to a fake Wayland display'
	} $else {
		return
	}
}

fn test_multiwindow_new_app_does_not_require_renderer_by_default() {
	$if linux {
		snapshot := snapshot_graphical_env()
		defer {
			restore_graphical_env(snapshot)
		}
		set_graphical_env('', '')
	}

	mut app := new_app()!
	assert !app.config.require_renderer
	caps := app.capabilities()
	$if windows {
		assert caps.backend == .win32
		assert caps.win32
	} $else {
		$if darwin {
			assert caps.backend == .appkit
		} $else {
			$if linux {
				assert caps.backend in [.mock, .x11, .wayland]
			}
		}
	}
	app.stop()!
}

fn test_multiwindow_x11_capabilities_match_new_app_render_policy() {
	mut app := new_app(backend: .x11, require_renderer: true) or { return }
	caps := capabilities_for_backend_with_renderer(.x11) or {
		app.stop()!
		return
	}
	app_caps := app.capabilities()

	assert caps.backend == .x11
	assert caps.backend == app_caps.backend
	assert caps.mock == app_caps.mock
	assert caps.native == app_caps.native
	assert caps.multi_window == app_caps.multi_window
	assert caps.owner_queue == app_caps.owner_queue
	assert caps.explicit_swapchain == app_caps.explicit_swapchain
	assert caps.x11 == app_caps.x11
	assert caps.gl == app_caps.gl

	app.stop()!
}

fn test_multiwindow_x11_runtime_create_destroy_when_display_is_available() {
	$if linux {
		$if !x_multiwindow_x11 ? {
			new_app(backend: .x11) or {
				assert err.msg() == 'multiwindow: backend is unsupported'
				return
			}
			assert false, 'x11 app creation succeeded without -d x_multiwindow_x11'
			return
		}
		if os.getenv('DISPLAY') == '' {
			eprintln('skip gg x11 runtime test: DISPLAY is not set')
			return
		}
		mut app := new_app(backend: .x11) or {
			if err.msg() == 'multiwindow: x11 open display failed' {
				eprintln('skip gg x11 runtime test: DISPLAY could not be opened')
				return
			}
			panic(err.msg())
		}
		main := app.create_window(title: 'V gg.App X11 main', width: 320, height: 200)!
		child := app.create_window(title: 'V gg.App X11 child', width: 240, height: 160)!
		assert app.window_exists(main)
		assert app.window_exists(child)
		app.destroy_window(child)!
		assert app.window_exists(main)
		assert !app.window_exists(child)
		app.stop()!
		assert !app.window_exists(main)
	} $else {
		new_app(backend: .x11) or {
			assert err.msg() == 'multiwindow: backend is unsupported'
			return
		}
		assert false, 'x11 app creation succeeded on an unsupported OS'
	}
}

fn test_multiwindow_wayland_capabilities_match_new_app_render_policy() {
	mut app := new_app(backend: .wayland, require_renderer: true) or { return }
	caps := capabilities_for_backend_with_renderer(.wayland) or {
		app.stop()!
		return
	}
	app_caps := app.capabilities()

	assert caps.backend == .wayland
	assert caps.backend == app_caps.backend
	assert caps.mock == app_caps.mock
	assert caps.native == app_caps.native
	assert caps.multi_window == app_caps.multi_window
	assert caps.owner_queue == app_caps.owner_queue
	assert caps.explicit_swapchain == app_caps.explicit_swapchain
	assert caps.wayland == app_caps.wayland
	assert caps.x11 == app_caps.x11

	app.stop()!
}

fn test_multiwindow_wayland_runtime_create_destroy_when_display_is_available() {
	$if linux {
		if os.getenv('WAYLAND_DISPLAY') == '' {
			eprintln('skip gg wayland runtime test: WAYLAND_DISPLAY is not set')
			return
		}
		mut app := new_app(backend: .wayland) or {
			if err.msg() == 'multiwindow: wayland connect failed' {
				eprintln('skip gg wayland runtime test: Wayland display connection failed')
				return
			}
			if err.msg() == 'multiwindow: backend is unsupported' {
				eprintln('skip gg wayland runtime test: Wayland backend is unsupported')
				return
			}
			panic(err.msg())
		}
		main := app.create_window(title: 'V gg.App Wayland main', width: 320, height: 200)!
		child := app.create_window(title: 'V gg.App Wayland child', width: 240, height: 160)!
		assert app.window_exists(main)
		assert app.window_exists(child)
		app.destroy_window(child)!
		assert app.window_exists(main)
		assert !app.window_exists(child)
		app.stop()!
		assert !app.window_exists(main)
	} $else {
		new_app(backend: .wayland) or {
			assert err.msg() == 'multiwindow: backend is unsupported'
			return
		}
		assert false, 'wayland app creation succeeded on an unsupported OS'
	}
}

fn test_multiwindow_create_destroy_window_lifecycle_and_events() {
	mut app := new_app(backend: .mock)!
	win := app.create_window(title: 'Inspector', width: 320, height: 240)!

	assert app.window_exists(win)

	created_events := app.drain_events()!
	assert created_events.len == 1
	assert created_events[0].kind == .window_created
	assert created_events[0].window == win

	app.destroy_window(win)!
	assert !app.window_exists(win)

	destroyed_events := app.drain_events()!
	assert destroyed_events.len == 1
	assert destroyed_events[0].kind == .window_destroyed
	assert destroyed_events[0].window == win

	app.stop()!
}

fn test_multiwindow_interactive_move_resize_mock_reports_unsupported() {
	mut app := new_app(backend: .mock)!
	win := app.create_window(title: 'Interactive inspector', width: 320, height: 240)!
	assert app.drain_events()!.len == 1

	mut move_rejected := false
	app.begin_window_move(win) or {
		assert err.msg() == 'multiwindow: backend capability is unsupported'
		move_rejected = true
	}
	assert move_rejected

	for edge in all_window_resize_edges_for_test() {
		mut resize_rejected := false
		app.begin_window_resize(win, edge) or {
			assert err.msg() == 'multiwindow: backend capability is unsupported'
			resize_rejected = true
		}
		assert resize_rejected, 'mock backend accepted interactive resize edge ${edge}'
	}

	fixed := app.create_window(title: 'Fixed inspector', width: 320, height: 240, resizable: false)!
	assert app.drain_events()!.len == 1
	mut fixed_resize_rejected := false
	app.begin_window_resize(fixed, .bottom_right) or {
		assert err.msg() == 'multiwindow: backend capability is unsupported'
		fixed_resize_rejected = true
	}
	assert fixed_resize_rejected
	app.stop()!
}

fn test_multiwindow_cursor_shape_mock_reports_unsupported() {
	mut app := new_app(backend: .mock)!
	win := app.create_window(title: 'Cursor inspector', width: 320, height: 240)!
	assert app.drain_events()!.len == 1
	assert !app.capabilities().cursor_shapes

	mut cursor_rejected := false
	app.set_window_cursor(win, .pointer) or {
		assert err.msg() == 'multiwindow: backend capability is unsupported'
		cursor_rejected = true
	}
	assert cursor_rejected
	app.stop()!
}

fn all_window_resize_edges_for_test() []WindowResizeEdge {
	return [
		WindowResizeEdge.top,
		.bottom,
		.left,
		.right,
		.top_left,
		.top_right,
		.bottom_left,
		.bottom_right,
	]
}

fn test_multiwindow_drain_input_events_routes_gg_event_without_lifecycle_pollution() {
	mut app := new_app(backend: .mock)!
	win := app.create_window(title: 'Input inspector', width: 320, height: 240)!
	assert app.drain_events()!.len == 1

	app.enqueue_mock_input_for_test(WindowInputEvent{
		window:        win
		event:         Event{
			typ:      .mouse_scroll
			mouse_x:  10
			mouse_y:  20
			scroll_y: -1.5
		}
		dropped_files: ['/tmp/input.txt']
	})!

	assert app.poll_events()! == 1
	assert app.drain_events()!.len == 0
	input_events := app.drain_input_events()!
	assert input_events.len == 1
	assert input_events[0].window == win
	assert input_events[0].event.typ == .mouse_scroll
	assert input_events[0].event.mouse_x == 10
	assert input_events[0].event.mouse_y == 20
	assert input_events[0].event.scroll_y == -1.5
	assert input_events[0].dropped_files == ['/tmp/input.txt']
	assert app.drain_input_events()!.len == 0
	app.stop()!
}

fn test_multiwindow_window_input_event_roundtrip_covers_all_real_sokol_event_types() {
	event_types := multiwindow_real_sokol_event_types()
	assert event_types.len == int(sapp.EventType.num)
	assert input_event_type_to_core(.num) == multiwindow.InputEventKind.invalid
	mut app := new_app(backend: .mock)!
	win := app.create_window(title: 'Input roundtrip', width: 320, height: 240)!
	assert app.drain_events()!.len == 1

	mut expected_events := []WindowInputEvent{cap: event_types.len}
	for i, typ in event_types {
		expected_events << multiwindow_window_input_event_for_type(win, typ, u64(i + 1))
	}
	for event in expected_events {
		app.enqueue_mock_input_for_test(event)!
	}

	assert app.poll_events()! == expected_events.len
	actual_events := app.drain_input_events()!
	assert actual_events.len == expected_events.len
	for i, actual in actual_events {
		assert_window_input_event_roundtrip(actual, expected_events[i])
	}
	assert app.drain_events()!.len == 0
	assert app.drain_input_events()!.len == 0
	app.stop()!
}

fn assert_window_input_event_roundtrip(actual WindowInputEvent, expected WindowInputEvent) {
	assert actual.window == expected.window
	assert actual.dropped_files == expected.dropped_files
	assert actual.event.frame_count == expected.event.frame_count
	assert actual.event.typ == expected.event.typ
	assert actual.event.key_code == expected.event.key_code
	assert actual.event.char_code == expected.event.char_code
	assert actual.event.key_repeat == expected.event.key_repeat
	assert actual.event.modifiers == expected.event.modifiers
	assert actual.event.mouse_button == expected.event.mouse_button
	assert actual.event.mouse_x == expected.event.mouse_x
	assert actual.event.mouse_y == expected.event.mouse_y
	assert actual.event.mouse_dx == expected.event.mouse_dx
	assert actual.event.mouse_dy == expected.event.mouse_dy
	assert actual.event.scroll_x == expected.event.scroll_x
	assert actual.event.scroll_y == expected.event.scroll_y
	assert actual.event.num_touches == expected.event.num_touches
	for i in 0 .. actual.event.num_touches {
		assert actual.event.touches[i].identifier == expected.event.touches[i].identifier
		assert actual.event.touches[i].pos_x == expected.event.touches[i].pos_x
		assert actual.event.touches[i].pos_y == expected.event.touches[i].pos_y
		assert actual.event.touches[i].android_tooltype == expected.event.touches[i].android_tooltype
		assert actual.event.touches[i].changed == expected.event.touches[i].changed
	}
	assert actual.event.window_width == expected.event.window_width
	assert actual.event.window_height == expected.event.window_height
	assert actual.event.framebuffer_width == expected.event.framebuffer_width
	assert actual.event.framebuffer_height == expected.event.framebuffer_height
}

fn multiwindow_real_sokol_event_types() []sapp.EventType {
	return [
		sapp.EventType.invalid,
		.key_down,
		.key_up,
		.char,
		.mouse_down,
		.mouse_up,
		.mouse_scroll,
		.mouse_move,
		.mouse_enter,
		.mouse_leave,
		.touches_began,
		.touches_moved,
		.touches_ended,
		.touches_cancelled,
		.resized,
		.iconified,
		.restored,
		.focused,
		.unfocused,
		.suspended,
		.resumed,
		.quit_requested,
		.clipboard_pasted,
		.files_dropped,
	]
}

fn multiwindow_window_input_event_for_type(win WindowId, typ sapp.EventType, frame_count u64) WindowInputEvent {
	mut touches := [8]TouchPoint{}
	touches[0] = TouchPoint{
		identifier:       7
		pos_x:            11.5
		pos_y:            12.5
		android_tooltype: unsafe { sapp.TouchToolType(1) }
		changed:          true
	}
	touches[1] = TouchPoint{
		identifier:       8
		pos_x:            21.5
		pos_y:            22.5
		android_tooltype: unsafe { sapp.TouchToolType(2) }
		changed:          false
	}
	dropped_files := if typ == .files_dropped {
		['/tmp/a.txt', '/tmp/b.txt']
	} else {
		[]string{}
	}
	return WindowInputEvent{
		window:        win
		event:         Event{
			frame_count:        frame_count
			typ:                typ
			key_code:           .escape
			char_code:          u32(0xe9)
			key_repeat:         true
			modifiers:          0x105
			mouse_button:       .middle
			mouse_x:            40.5
			mouse_y:            41.5
			mouse_dx:           -2.25
			mouse_dy:           3.5
			scroll_x:           -1.25
			scroll_y:           2.5
			num_touches:        2
			touches:            touches
			window_width:       640
			window_height:      360
			framebuffer_width:  1280
			framebuffer_height: 720
		}
		dropped_files: dropped_files
	}
}

fn test_multiwindow_rejects_invalid_window_size_from_core() {
	mut app := new_app(backend: .mock)!
	app.create_window(width: 0, height: 240) or {
		assert err.msg() == 'multiwindow: window size must be positive'
		app.stop()!
		return
	}
	assert false, 'create_window accepted a zero width'
}

fn test_multiwindow_post_try_post_and_drain_pending_delegate_to_core() {
	mut app := new_app(backend: .mock, queue_size: 2)!
	ran := chan int{cap: 2}

	app.post(fn [ran] (mut app App) ! {
		ran <- 1
	})!
	app.try_post(fn [ran] (mut app App) ! {
		ran <- 2
	})!
	app.try_post(fn [ran] (mut app App) ! {
		ran <- 3
	}) or { assert err.msg() == 'multiwindow: owner queue is full' }

	assert app.drain_pending(2)! == 2
	assert <-ran == 1
	assert <-ran == 2
	assert app.drain_pending(1)! == 0

	app.stop()!
}

fn test_multiwindow_jobs_can_mutate_app_through_core_queue() {
	mut app := new_app(backend: .mock, queue_size: 1)!

	app.post(fn (mut app App) ! {
		_ = app.create_window(title: 'Created by job')!
	})!

	assert app.drain_pending(1)! == 1
	events := app.drain_events()!
	assert events.len == 1
	assert events[0].kind == .window_created

	app.stop()!
}

fn test_multiwindow_stop_delegates_to_core_and_closes_admission() {
	mut app := new_app(backend: .mock, queue_size: 1)!
	win := app.create_window(title: 'Main')!
	app.stop()!

	assert !app.window_exists(win)
	events := app.drain_events()!
	assert events.len == 2
	assert events[0].kind == .window_created
	assert events[1].kind == .window_destroyed

	app.create_window() or {
		assert err.msg() == 'multiwindow: app is stopped'
		return
	}
	assert false, 'create_window accepted work after stop'
}

fn test_multiwindow_render_api_rejects_mock_without_running_callbacks() {
	mut app := new_app(backend: .mock)!
	win := app.create_window(title: 'Render target')!
	mut frame_ran := false
	mut draw_ran := false

	app.run(
		frame_fn: fn [mut frame_ran] (mut app App) ! {
			_ = app
			frame_ran = true
		}
	) or {
		assert err.msg() == err_multiwindow_renderer_unsupported
		assert !frame_ran
	}
	events_after_rejected_run := app.drain_events()!
	assert events_after_rejected_run.len == 1
	assert events_after_rejected_run[0].kind == .window_created
	assert events_after_rejected_run[0].window == win

	app.draw_window(win, fn [mut draw_ran] (mut window WindowContext) ! {
		_ = window
		draw_ran = true
	}) or {
		assert err.msg() == err_multiwindow_render_frame_inactive
		assert !draw_ran
		app.stop()!
		return
	}
	assert false, 'draw_window accepted a call outside an active app frame callback'
}

fn test_multiwindow_run_rejects_missing_callbacks() {
	mut app := new_app(backend: .mock)!
	app.run() or {
		assert err.msg() == err_multiwindow_nil_run_fn
		app.stop()!
		return
	}
	assert false, 'run accepted missing callbacks'
}

fn test_multiwindow_zero_value_app_methods_return_initialized_error() {
	mut app := App{}
	id := WindowId{}
	mut rejected := 0

	_ := app.create_window(title: 'zero') or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
		WindowId{}
	}
	app.set_window_title(id, 'zero') or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
	}
	app.resize_window(id, 1, 1) or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
	}
	app.begin_window_move(id) or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
	}
	app.begin_window_resize(id, .bottom_right) or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
	}
	_ := app.window_info(id) or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
		WindowInfo{}
	}
	_ := app.window_ids() or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
		[]WindowId{}
	}
	_ := app.window_infos() or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
		[]WindowInfo{}
	}
	app.destroy_window(id) or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
	}
	_ := app.drain_events() or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
		[]WindowEvent{}
	}
	_ := app.drain_input_events() or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
		[]WindowInputEvent{}
	}
	_ := app.poll_events() or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
		0
	}
	app.post(fn (mut app App) ! {
		_ = app
	}) or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
	}
	app.try_post(fn (mut app App) ! {
		_ = app
	}) or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
	}
	_ := app.drain_pending(1) or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
		0
	}
	app.run(
		event_fn: fn (event WindowEvent, mut app App) ! {
			_ = event
			_ = app
		}
	) or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
	}
	app.draw_window(id, fn (mut window WindowContext) ! {
		_ = window
	}) or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
	}
	app.stop() or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
	}

	assert rejected == 18
	assert !app.window_exists(id)
	assert !app.capabilities().multi_window
}

struct MockCloseCallbackSeen {
mut:
	created         bool
	close_requested bool
}

struct ManagedStopDispatchSeen {
mut:
	callback_order []string
	delivered      []WindowId
}

fn test_multiwindow_event_callback_stop_completes_current_cut_and_retains_ordered_suffix() {
	mut app := new_app(backend: .mock, queue_size: 2)!
	first := app.create_window(title: 'stop cut first')!
	second := app.create_window(title: 'stop cut second')!
	third := app.create_window(title: 'stop cut third')!
	created := app.drain_events()!
	assert created.len == 3
	assert created.map(it.window) == [first, second, third]

	app.core.enqueue_mock_close_requested_for_test(first.core)!
	app.core.enqueue_mock_close_requested_for_test(second.core)!
	app.core.enqueue_mock_close_requested_for_test(third.core)!
	mut seen := &ManagedStopDispatchSeen{}

	app.run(
		event_fn: fn [mut seen] (event WindowEvent, mut app App) ! {
			assert event.kind == .window_close_requested
			seen.delivered << event.window
			seen.callback_order << 'before-stop'
			app.stop()!
			assert app.core.status() == multiwindow.AppStatus.running
			seen.callback_order << 'after-stop'
		}
	)!

	assert seen.delivered == [first]
	assert seen.callback_order == ['before-stop', 'after-stop']
	assert app.core.status() == multiwindow.AppStatus.stopped
	remaining := app.drain_events()!
	assert remaining.len == 5
	assert remaining[0].kind == .window_close_requested
	assert remaining[0].window == second
	assert remaining[1].kind == .window_close_requested
	assert remaining[1].window == third
	assert remaining[2].kind == .window_destroyed
	assert remaining[2].window == first
	assert remaining[3].kind == .window_destroyed
	assert remaining[3].window == second
	assert remaining[4].kind == .window_destroyed
	assert remaining[4].window == third
	assert app.drain_events()!.len == 0

	mut rejected := 0
	_ := app.create_window(title: 'rejected after stop') or {
		assert err.msg() == 'multiwindow: app is stopped'
		rejected++
		WindowId{}
	}
	app.try_post(fn (mut queued_app App) ! {
		_ = queued_app
	}) or {
		assert err.msg() == 'multiwindow: app is stopped'
		rejected++
	}
	app.core.enqueue_mock_close_requested_for_test(first.core) or {
		assert err.msg() == 'multiwindow: app is stopped'
		rejected++
	}
	assert rejected == 3
}

struct DispatchFailureSuffixSeen {
mut:
	windows []multiwindow.WindowId
}

fn test_multiwindow_dispatch_callback_failure_requeues_only_failed_event_and_suffix() {
	mut app := new_app(backend: .mock)!
	first := app.create_window(title: 'dispatch prefix')!
	second := app.create_window(title: 'dispatch failed event')!
	third := app.create_window(title: 'dispatch suffix')!
	assert app.drain_events()!.len == 3
	app.core.enqueue_mock_close_requested_for_test(first.core)!
	app.core.enqueue_mock_close_requested_for_test(second.core)!
	app.core.enqueue_mock_close_requested_for_test(third.core)!
	assert app.core.poll_events()! == 3

	mut failed_cut := &DispatchFailureSuffixSeen{}
	mut callback_error := ''
	_, _ := app.core.dispatch_events_for_gg(fn [mut failed_cut] (event multiwindow.QueuedEvent) !bool {
		assert event.kind == .lifecycle
		assert event.lifecycle.kind == .window_close_requested
		failed_cut.windows << event.lifecycle.window_id
		if failed_cut.windows.len == 2 {
			return error('injected managed event callback failure')
		}
		return true
	}) or {
		callback_error = err.msg()
		0, false
	}
	assert callback_error == 'injected managed event callback failure'
	assert failed_cut.windows == [first.core, second.core]

	mut replayed := &DispatchFailureSuffixSeen{}
	delivered, yielded := app.core.dispatch_events_for_gg(fn [mut replayed] (event multiwindow.QueuedEvent) !bool {
		assert event.kind == .lifecycle
		assert event.lifecycle.kind == .window_close_requested
		replayed.windows << event.lifecycle.window_id
		return true
	})!
	assert delivered == 2
	assert !yielded
	assert replayed.windows == [second.core, third.core]
	assert app.core.drain_queued_events()!.len == 0

	app.stop()!
	destroyed := app.drain_events()!
	assert destroyed.len == 3
	assert destroyed.map(it.window) == [first, second, third]
}

fn test_multiwindow_run_event_callback_routes_mock_close_requested_from_core_poll() {
	mut app := new_app(backend: .mock, queue_size: 2)!
	win := app.create_window(title: 'Close callback target')!
	app.core.enqueue_mock_close_requested_for_test(win.core)!
	mut seen := &MockCloseCallbackSeen{}

	app.run(
		event_fn: fn [win, mut seen] (event WindowEvent, mut app App) ! {
			match event.kind {
				.window_created {
					assert event.window == win
					seen.created = true
					app.post(fn (mut app App) ! {
						app.stop()!
					})!
				}
				.window_close_requested {
					assert event.window == win
					assert event.width == 0
					assert event.height == 0
					assert app.window_exists(win)
					assert app.core.capabilities().backend == multiwindow.BackendKind.mock
					seen.close_requested = true
					app.stop()!
				}
				else {}
			}
		}
	)!

	assert seen.created
	assert seen.close_requested
}

struct MockInputCallbackSeen {
mut:
	created bool
	input   bool
}

fn test_multiwindow_run_input_callback_preserves_lifecycle_input_order() {
	mut app := new_app(backend: .mock, queue_size: 2)!
	win := app.create_window(title: 'Input callback target')!
	app.enqueue_mock_input_for_test(WindowInputEvent{
		window: win
		event:  Event{
			typ:      .key_down
			key_code: .a
		}
	})!
	mut seen := &MockInputCallbackSeen{}

	app.run(
		event_fn: fn [win, mut seen] (event WindowEvent, mut app App) ! {
			if event.kind == .window_created {
				assert event.window == win
				assert !seen.input
				seen.created = true
			}
			_ = app
		}
		input_fn: fn [win, mut seen] (event WindowInputEvent, mut app App) ! {
			assert event.window == win
			assert event.event.typ == .key_down
			assert event.event.key_code == .a
			assert seen.created
			seen.input = true
			app.stop()!
		}
	)!

	assert seen.created
	assert seen.input
}

struct MockInputLifecycleInputCallbackSeen {
mut:
	order []string
}

fn test_multiwindow_run_input_callback_preserves_input_lifecycle_input_order() {
	mut app := new_app(backend: .mock, queue_size: 2)!
	win := app.create_window(title: 'Input lifecycle input callback target')!
	assert app.drain_events()!.len == 1
	app.enqueue_mock_input_for_test(WindowInputEvent{
		window: win
		event:  Event{
			typ: .mouse_enter
		}
	})!
	app.core.enqueue_mock_close_requested_for_test(win.core)!
	app.enqueue_mock_input_for_test(WindowInputEvent{
		window: win
		event:  Event{
			typ:       .char
			char_code: u32(120)
		}
	})!
	mut seen := &MockInputLifecycleInputCallbackSeen{}

	app.run(
		event_fn: fn [win, mut seen] (event WindowEvent, mut app App) ! {
			assert event.window == win
			assert event.kind == .window_close_requested
			seen.order << 'lifecycle-close'
			_ = app
		}
		input_fn: fn [win, mut seen] (event WindowInputEvent, mut app App) ! {
			assert event.window == win
			match event.event.typ {
				.mouse_enter {
					seen.order << 'input-enter'
				}
				.char {
					assert event.event.char_code == u32(120)
					seen.order << 'input-char'
					app.stop()!
				}
				else {
					assert false, 'unexpected input event ${event.event.typ}'
				}
			}
		}
	)!

	assert seen.order == ['input-enter', 'lifecycle-close', 'input-char']
}

fn test_multiwindow_run_input_only_handles_close_requested_without_event_callback() {
	mut app := new_app(backend: .mock, queue_size: 2)!
	win := app.create_window(title: 'Input-only close target')!
	app.core.enqueue_mock_close_requested_for_test(win.core)!

	app.run(
		input_fn: fn (event WindowInputEvent, mut app App) ! {
			_ = event
			_ = app
			assert false, 'input-only close test should not receive lifecycle as input'
		}
	)!

	assert !app.window_exists(win)
	assert app.core.status() == multiwindow.AppStatus.stopped
}

fn test_multiwindow_public_drain_without_event_callback_returns_lifecycle_events() {
	mut app := new_app(backend: .mock)!
	win := app.create_window(title: 'Frame-only lifecycle drain')!

	created := app.drain_events()!
	assert created.len == 1
	assert created[0].kind == .window_created
	assert created[0].window == win

	app.core.destroy_window(win.core)!

	destroyed := app.drain_events()!
	assert destroyed.len == 1
	assert destroyed[0].kind == .window_destroyed
	assert destroyed[0].window == win

	app.stop()!
}

fn test_multiwindow_mock_run_with_event_and_frame_rejects_before_callbacks() {
	mut app := new_app(backend: .mock)!
	win := app.create_window(title: 'Mixed callbacks')!
	mut event_ran := false
	mut frame_ran := false

	app.run(
		event_fn: fn [mut event_ran] (event WindowEvent, mut app App) ! {
			_ = event
			_ = app
			event_ran = true
		}
		frame_fn: fn [mut frame_ran] (mut app App) ! {
			_ = app
			frame_ran = true
		}
	) or {
		assert err.msg() == err_multiwindow_renderer_unsupported
		assert !event_ran
		assert !frame_ran
		events := app.drain_events()!
		assert events.len == 1
		assert events[0].kind == .window_created
		assert events[0].window == win
		app.stop()!
		return
	}
	app.stop()!
	assert false, 'mock run accepted frame_fn with no renderer because event_fn was present'
}

fn test_multiwindow_render_owner_guard_rejects_mixed_owners() {
	legacy_token := voidptr(usize(0x1001))
	app_token := voidptr(usize(0x2002))
	gg_release_gfx_render_owner(.legacy_context, legacy_token)
	gg_release_gfx_render_owner(.multiwindow_app, app_token)
	gg_claim_gfx_render_owner(.legacy_context, legacy_token)!
	gg_claim_gfx_render_owner(.multiwindow_app, app_token) or {
		assert err.msg() == 'gg.multiwindow: sokol.gfx is already owned by gg.Context'
		gg_release_gfx_render_owner(.legacy_context, legacy_token)
		return
	}
	gg_release_gfx_render_owner(.legacy_context, legacy_token)
	assert false, 'multiwindow owner claimed sokol.gfx while legacy owner was active'
}

fn test_multiwindow_two_apps_cannot_both_own_renderer() {
	mut app1 := new_app(backend: .mock)!
	mut app2 := new_app(backend: .mock)!

	app1.ensure_render_owner()!
	app2.ensure_render_owner() or {
		assert err.msg() == 'gg.multiwindow: sokol.gfx is already owned by gg.App'
		app1.shutdown_renderer()!
		app1.stop()!
		app2.stop()!
		return
	}
	app1.shutdown_renderer()!
	app1.stop()!
	app2.stop()!
	assert false, 'second gg.App claimed sokol.gfx while first app owned it'
}

fn test_multiwindow_same_app_can_reclaim_renderer_owner() {
	mut app := new_app(backend: .mock)!

	app.ensure_render_owner()!
	app.ensure_render_owner()!
	app.shutdown_renderer()!
	app.stop()!
}

fn test_multiwindow_release_render_owner_allows_other_owner_after_claim() {
	legacy_token := voidptr(usize(0x5005))
	gg_release_gfx_render_owner(.legacy_context, legacy_token)
	mut app := new_app(backend: .mock)!

	app.ensure_render_owner()!
	app.release_render_owner()
	gg_claim_gfx_render_owner(.legacy_context, legacy_token)!
	gg_release_gfx_render_owner(.legacy_context, legacy_token)
	app.stop()!
}

fn test_multiwindow_foreign_thread_stop_does_not_release_render_owner() {
	legacy_token := voidptr(usize(0x7007))
	gg_release_gfx_render_owner(.legacy_context, legacy_token)
	mut app := new_app(backend: .mock)!
	app.ensure_render_owner()!
	result := chan string{cap: 1}
	t := spawn fn [mut app, result] () {
		app.stop() or {
			result <- err.msg()
			return
		}
		result <- 'accepted'
	}()
	foreign_stop_message := <-result
	t.wait()

	mut blocked_claim_message := ''
	mut blocked_claim_accepted := false
	gg_claim_gfx_render_owner(.legacy_context, legacy_token) or {
		blocked_claim_message = err.msg()
	}
	if blocked_claim_message == '' {
		blocked_claim_accepted = true
		gg_release_gfx_render_owner(.legacy_context, legacy_token)
	}

	mut owner_stop_message := ''
	mut owner_release_fallback := false
	app.stop() or {
		owner_stop_message = err.msg()
		owner_release_fallback = true
		app.release_render_owner()
	}

	mut post_cleanup_claim_message := ''
	mut post_cleanup_claimed := false
	gg_claim_gfx_render_owner(.legacy_context, legacy_token) or {
		post_cleanup_claim_message = err.msg()
	}
	if post_cleanup_claim_message == '' {
		post_cleanup_claimed = true
		gg_release_gfx_render_owner(.legacy_context, legacy_token)
	}

	assert foreign_stop_message == err_multiwindow_render_owner_thread
	assert blocked_claim_message == 'gg.multiwindow: sokol.gfx is already owned by gg.App'
	assert !blocked_claim_accepted
	assert owner_stop_message == ''
	assert !owner_release_fallback
	assert post_cleanup_claim_message == ''
	assert post_cleanup_claimed
}

fn test_multiwindow_legacy_then_app_ownership_is_rejected_without_window() {
	legacy_token := voidptr(usize(0x3003))
	mut app := new_app(backend: .mock)!

	gg_claim_gfx_render_owner(.legacy_context, legacy_token)!
	mut ensure_message := ''
	app.ensure_render_owner() or { ensure_message = err.msg() }
	gg_release_gfx_render_owner(.legacy_context, legacy_token)
	mut stop_message := ''
	app.stop() or {
		stop_message = err.msg()
		app.release_render_owner()
	}

	assert ensure_message == 'gg.multiwindow: sokol.gfx is already owned by gg.Context'
	assert stop_message == ''
}

fn test_multiwindow_app_then_legacy_ownership_is_rejected_without_window() {
	mut app := new_app(backend: .mock)!
	legacy_token := voidptr(usize(0x4004))

	app.ensure_render_owner()!
	mut claim_message := ''
	mut legacy_claimed := false
	gg_claim_gfx_render_owner(.legacy_context, legacy_token) or { claim_message = err.msg() }
	if claim_message == '' {
		legacy_claimed = true
		gg_release_gfx_render_owner(.legacy_context, legacy_token)
	}
	app.shutdown_renderer()!
	app.stop()!

	assert claim_message == 'gg.multiwindow: sokol.gfx is already owned by gg.App'
	assert !legacy_claimed
}

fn test_multiwindow_user_program_imports_only_gg() {
	vlib_dir := os.dir(@DIR)
	source_path, bin_path := multiwindow_temp_paths('gg_multiwindow_import_only_smoke')
	source := "import gg

fn main() {
	mut app := gg.new_app(backend: .mock, queue_size: 2)!
	caps := gg.capabilities_for_backend(.mock)!
	assert caps.mock
	assert app.capabilities().mock
	win := app.create_window(title: 'Main')!
	assert app.window_exists(win)
	mut no_input := []gg.WindowInputEvent{}
	no_input = app.drain_input_events()!
	assert no_input.len == 0
	app.try_post(fn (mut app gg.App) ! {
		_ = app.create_window(title: 'Queued')!
	})!
	assert app.drain_pending(1)! == 1
	assert app.drain_events()!.len == 2
	app.destroy_window(win)!
	app.stop()!
}
"
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(bin_path) or {}
	}

	cmd := '${os.quoted_path(@VEXE)}${multiwindow_child_v_flags()} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(bin_path)} ${os.quoted_path(source_path)}'
	compile := os.execute(cmd)
	multiwindow_assert_command_ok('compile child gg import smoke', cmd, compile)
	run_cmd := os.quoted_path(bin_path)
	run := os.execute(run_cmd)
	multiwindow_assert_command_ok('run child gg import smoke', run_cmd, run)
}

fn test_multiwindow_user_program_input_fn_compile_imports_only_gg() {
	vlib_dir := os.dir(@DIR)
	source_path, out_path_base := multiwindow_temp_paths('gg_multiwindow_input_fn_compile')
	c_path := '${out_path_base}.c'
	source := 'import gg

fn on_input(event gg.WindowInputEvent, mut app gg.App) ! {
	_ = event.window
	_ = event.event.typ
	_ = event.dropped_files
	_ = app.capabilities()
}

fn main() {
	mut app := gg.new_app(backend: .mock)!
	empty := gg.WindowInputEvent{}
	_ = empty.event.typ
	app.run(input_fn: on_input) or {}
	app.stop() or {}
}
'
	assert source.contains('import gg')
	assert !source.contains('x.multiwindow')
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(c_path) or {}
		os.rm(out_path_base) or {}
	}

	cmd := '${os.quoted_path(@VEXE)}${multiwindow_child_v_flags()} -b c -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(c_path)} ${os.quoted_path(source_path)}'
	compile := os.execute(cmd)
	multiwindow_assert_command_ok('compile child input_fn API smoke', cmd, compile)
}

fn test_multiwindow_user_program_window_info_title_and_resize_imports_only_gg() {
	vlib_dir := os.dir(@DIR)
	source_path, bin_path := multiwindow_temp_paths('gg_multiwindow_window_info_smoke')
	source := "import gg

fn main() {
	mut app := gg.new_app(backend: .mock, queue_size: 2)!
	win := app.create_window(
		title:      'Initial'
		width:      320
		height:     200
		min_width:  100
		min_height: 80
		resizable:  false
		visible:    false
		high_dpi:   false
		borderless: true
	)!

	info := app.window_info(win)!
	assert info.id == win
	assert info.title == 'Initial'
	assert info.width == 320
	assert info.height == 200
	assert info.min_width == 100
	assert info.min_height == 80
	assert !info.resizable
	assert !info.visible
	assert !info.high_dpi
	assert info.borderless
	assert !info.fullscreen

	app.set_window_title(win, 'Updated')!
	title_info := app.window_info(win)!
	assert title_info.title == 'Updated'
	assert title_info.width == 320
	assert title_info.height == 200

	app.resize_window(win, 640, 360)!
	resized_info := app.window_info(win)!
	assert resized_info.title == 'Updated'
	assert resized_info.width == 640
	assert resized_info.height == 360
	assert resized_info.min_width == 100
	assert resized_info.min_height == 80

	app.destroy_window(win)!
	assert !app.window_exists(win)

	mut rejected_info := false
	app.window_info(win) or {
		assert err.msg() == 'multiwindow: window handle is stale'
		rejected_info = true
	}
	assert rejected_info

	mut rejected_title := false
	app.set_window_title(win, 'Destroyed') or {
		assert err.msg() == 'multiwindow: window handle is stale'
		rejected_title = true
	}
	assert rejected_title

	mut rejected_resize := false
	app.resize_window(win, 100, 100) or {
		assert err.msg() == 'multiwindow: window handle is stale'
		rejected_resize = true
	}
	assert rejected_resize

	app.stop()!
}
"
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(bin_path) or {}
	}

	cmd := '${os.quoted_path(@VEXE)}${multiwindow_child_v_flags()} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(bin_path)} ${os.quoted_path(source_path)}'
	compile := os.execute(cmd)
	multiwindow_assert_command_ok('compile child window info smoke', cmd, compile)
	run_cmd := os.quoted_path(bin_path)
	run := os.execute(run_cmd)
	multiwindow_assert_command_ok('run child window info smoke', run_cmd, run)
}

fn test_multiwindow_user_program_enumeration_and_resize_events_imports_only_gg() {
	vlib_dir := os.dir(@DIR)
	source_path, bin_path := multiwindow_temp_paths('gg_multiwindow_enumeration_events_smoke')
	source := "import gg

fn main() {
	close_kind := gg.WindowEventKind.window_close_requested
	assert close_kind == .window_close_requested

	mut app := gg.new_app(backend: .mock, queue_size: 2)!
	assert app.window_ids()!.len == 0
	assert app.window_infos()!.len == 0

	first := app.create_window(title: 'First', width: 100, height: 80)!
	second := app.create_window(title: 'Second', width: 200, height: 120)!
	ids := app.window_ids()!
	assert ids == [first, second]

	infos := app.window_infos()!
	assert infos.len == 2
	assert infos[0].id == first
	assert infos[0].title == 'First'
	assert infos[0].width == 100
	assert infos[0].height == 80
	assert infos[1].id == second
	assert infos[1].title == 'Second'
	assert infos[1].width == 200
	assert infos[1].height == 120

	created_events := app.drain_events()!
	assert created_events.len == 2
	assert created_events[0].kind == .window_created
	assert created_events[0].window == first
	assert created_events[1].kind == .window_created
	assert created_events[1].window == second

	app.resize_window(second, 640, 360)!
	resize_events := app.drain_events()!
	assert resize_events.len == 1
	assert resize_events[0].kind == .window_resized
	assert resize_events[0].window == second
	assert resize_events[0].width == 640
	assert resize_events[0].height == 360

	resized := app.window_infos()!
	assert resized[1].id == second
	assert resized[1].width == 640
	assert resized[1].height == 360

	assert app.poll_events()! == 0
	app.destroy_window(first)!
	assert app.window_ids()! == [second]
	app.stop()!
}
"
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(bin_path) or {}
	}

	cmd := '${os.quoted_path(@VEXE)}${multiwindow_child_v_flags()} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(bin_path)} ${os.quoted_path(source_path)}'
	compile := os.execute(cmd)
	multiwindow_assert_command_ok('compile child enumeration smoke', cmd, compile)
	run_cmd := os.quoted_path(bin_path)
	run := os.execute(run_cmd)
	multiwindow_assert_command_ok('run child enumeration smoke', run_cmd, run)
}

fn test_multiwindow_user_program_run_event_callback_imports_only_gg() {
	vlib_dir := os.dir(@DIR)
	source_path, bin_path := multiwindow_temp_paths('gg_multiwindow_event_callback_smoke')
	source := "import gg

struct Seen {
mut:
	created int
	resized int
	resize_width int
	resize_height int
}

fn main() {
	mut seen := &Seen{}
	mut app := gg.new_app(backend: .mock, queue_size: 2)!
	win := app.create_window(title: 'Event target', width: 160, height: 90)!
	app.resize_window(win, 320, 180)!

	app.run(
		event_fn: fn [win, mut seen] (event gg.WindowEvent, mut app gg.App) ! {
			match event.kind {
				.window_created {
					assert event.window == win
					seen.created++
				}
				.window_resized {
					assert event.window == win
					assert event.width == 320
					assert event.height == 180
					seen.resized++
					seen.resize_width = event.width
					seen.resize_height = event.height
					app.stop()!
				}
				else {}
			}
		}
	)!

	assert seen.created == 1
	assert seen.resized == 1
	assert seen.resize_width == 320
	assert seen.resize_height == 180
}
"
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(bin_path) or {}
	}

	cmd := '${os.quoted_path(@VEXE)}${multiwindow_child_v_flags()} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(bin_path)} ${os.quoted_path(source_path)}'
	compile := os.execute(cmd)
	multiwindow_assert_command_ok('compile child event callback smoke', cmd, compile)
	run_cmd := os.quoted_path(bin_path)
	run := os.execute(run_cmd)
	multiwindow_assert_command_ok('run child event callback smoke', run_cmd, run)
}

fn test_multiwindow_user_program_run_event_callback_sees_queued_job_events() {
	vlib_dir := os.dir(@DIR)
	source_path, bin_path := multiwindow_temp_paths('gg_multiwindow_event_callback_queued_smoke')
	source := "import gg

struct Seen {
mut:
	created bool
	resized bool
}

fn main() {
	mut seen := &Seen{}
	created := chan gg.WindowId{cap: 1}
	mut app := gg.new_app(backend: .mock, queue_size: 2)!
	app.post(fn [created] (mut app gg.App) ! {
		win := app.create_window(title: 'Queued window', width: 120, height: 80)!
		app.resize_window(win, 240, 160)!
		created <- win
	})!

	app.run(
		event_fn: fn [created, mut seen] (event gg.WindowEvent, mut app gg.App) ! {
			win := <-created
			match event.kind {
				.window_created {
					assert event.window == win
					seen.created = true
					created <- win
				}
				.window_resized {
					assert seen.created
					assert event.window == win
					assert event.width == 240
					assert event.height == 160
					seen.resized = true
					created <- win
					app.stop()!
				}
				else {
					created <- win
				}
			}
		}
	)!

	assert seen.created
	assert seen.resized
}
"
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(bin_path) or {}
	}

	cmd := '${os.quoted_path(@VEXE)}${multiwindow_child_v_flags()} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(bin_path)} ${os.quoted_path(source_path)}'
	compile := os.execute(cmd)
	multiwindow_assert_command_ok('compile child queued event smoke', cmd, compile)
	run_cmd := os.quoted_path(bin_path)
	run := os.execute(run_cmd)
	multiwindow_assert_command_ok('run child queued event smoke', run_cmd, run)
}

fn test_multiwindow_async_background_work_posts_owner_mutation() {
	vlib_dir := os.dir(@DIR)
	source_path, bin_path := multiwindow_temp_paths('gg_multiwindow_async_post_smoke')
	source := "import context
import gg
import x.async as xasync

fn main() {
	mut app := gg.new_app(backend: .mock, queue_size: 2)!
	created := chan gg.WindowId{cap: 1}
	mut task := xasync.run[int](fn (mut ctx context.Context) !int {
		_ = ctx
		mut total := 0
		for i in 0 .. 6 {
			total += i
		}
		return total
	})!
	result := task.wait()!
	assert result == 15
	app.post(fn [created, result] (mut app gg.App) ! {
		assert result == 15
		win := app.create_window(title: 'Created from async result')!
		created <- win
	})!
	assert app.drain_pending(1)! == 1
	win := <-created
	assert app.window_exists(win)
	events := app.drain_events()!
	assert events.len == 1
	assert events[0].kind == .window_created
	assert events[0].window == win
	app.stop()!
}
"
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(bin_path) or {}
	}

	cmd := '${os.quoted_path(@VEXE)}${multiwindow_child_v_flags()} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(bin_path)} ${os.quoted_path(source_path)}'
	compile := os.execute(cmd)
	multiwindow_assert_command_ok('compile child async post smoke', cmd, compile)
	run_cmd := os.quoted_path(bin_path)
	run := os.execute(run_cmd)
	multiwindow_assert_command_ok('run child async post smoke', run_cmd, run)
}

fn test_multiwindow_auto_render_api_user_program_has_no_renderer_flag() {
	vlib_dir := os.dir(@DIR)
	source_path, out_path_base := multiwindow_temp_paths('gg_multiwindow_auto_render_api_compile')
	c_path := '${out_path_base}.c'
	source := 'import gg

fn main() {
	mut app := gg.new_app(require_renderer: true) or { return }
	app.run(frame_fn: fn (mut app gg.App) ! {
		_ = app.capabilities()
	}) or {}
	app.stop() or {}
}
'
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(c_path) or {}
		os.rm(out_path_base) or {}
	}

	cmd := '${os.quoted_path(@VEXE)}${multiwindow_child_v_flags()} -b c -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(c_path)} ${os.quoted_path(source_path)}'
	compile := os.execute(cmd)
	multiwindow_assert_command_ok('compile child auto render API smoke', cmd, compile)
}

fn test_multiwindow_checked_in_example_compiles_without_running() {
	vlib_dir := os.dir(@DIR)
	_, out_path_base := multiwindow_temp_paths('gg_multiwindow_checked_in_example_compile')
	c_path := '${out_path_base}.c'
	example_path := os.join_path(vlib_dir, '..', 'examples', 'gg', 'multiwindow.v')
	defer {
		os.rm(c_path) or {}
		os.rm(out_path_base) or {}
	}

	cmd := '${os.quoted_path(@VEXE)}${multiwindow_child_v_flags()} -b c -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(c_path)} ${os.quoted_path(example_path)}'
	compile := os.execute(cmd)
	multiwindow_assert_command_ok('compile checked-in gg multiwindow example', cmd, compile)
}

fn test_multiwindow_checked_in_example_headless_mock_outputs_event_markers() {
	$if linux {
		vlib_dir := os.dir(@DIR)
		example_path := os.join_path(vlib_dir, '..', 'examples', 'gg', 'multiwindow.v')
		env_executable := os.find_abs_path_of_executable('env') or { '/usr/bin/env' }
		mut args := ['-u', 'DISPLAY', '-u', 'WAYLAND_DISPLAY', '-u', 'XDG_SESSION_TYPE', '-u',
			'VGG_MULTIWINDOW_EXAMPLE_UNATTENDED', @VEXE]
		args << multiwindow_child_v_flags().fields()
		args << ['-path', '${vlib_dir}|@vlib|@vmodules', 'run', example_path]
		_, gate_path := multiwindow_temp_paths('gg_multiwindow_checked_in_example_watchdog_gate')
		defer {
			os.rm(gate_path) or {}
		}
		run := multiwindow_probe_watchdog.run(
			executable: env_executable
			args:       args
			timeout:    30 * time.second
			start_file: gate_path
		)!
		assert !run.timed_out, 'checked-in gg multiwindow example timed out\n${run.combined_output()}'
		assert run.reaped, 'checked-in gg multiwindow example leader was not reaped'
		assert run.exit_code == 0, run.combined_output()
		assert run.confinement_empty, 'checked-in gg multiwindow example confinement was not proven empty'
		assert !run.forced_cleanup, 'checked-in gg multiwindow example required forced cleanup'
		mut final_stdout_record := ''
		for line in run.stdout.split_into_lines() {
			if line.trim_space() != '' {
				final_stdout_record = line.trim_space()
			}
		}
		expected_pass := '{"example":"multiwindow","status":"PASS","cleanup":"complete"}'
		assert final_stdout_record == expected_pass, 'checked-in gg multiwindow example final post-cleanup record mismatch\nexpected: ${expected_pass}\nactual: ${final_stdout_record}\n${run.combined_output()}'
		output := run.combined_output()
		for marker in [
			'gg multi-window backend: mock',
			'capability families:',
			'windows=true owner_queue=true native=false',
			'render explicit_swapchain=false',
			'input=true mouse=true keyboard=true text=true focus=true drop=true touch=true',
			'mock backend selected; stopping after initial lifecycle events',
			'live windows:',
			'window created:',
			'window resized:',
		] {
			assert output.contains(marker), 'example output is missing marker `${marker}`:
${output}'
		}
		assert !output.contains('multi-window example failed')
		assert !output.contains('input marker:')
	} $else {
		return
	}
}

fn test_multiwindow_does_not_break_legacy_context_compile() {
	vlib_dir := os.dir(@DIR)
	source_path, out_path_base := multiwindow_temp_paths('gg_legacy_context_compile')
	c_path := '${out_path_base}.c'
	source := 'import gg

fn main() {
	ctx := gg.new_context(width: 1, height: 1, create_window: false)
	assert ctx.width == 1
	assert ctx.height == 1
}
'
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(c_path) or {}
		os.rm(out_path_base) or {}
	}

	cmd := '${os.quoted_path(@VEXE)}${multiwindow_child_v_flags()} -b c -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(c_path)} ${os.quoted_path(source_path)}'
	compile := os.execute(cmd)
	multiwindow_assert_command_ok('compile child legacy gg.Context smoke', cmd, compile)
}

fn multiwindow_facade_source() string {
	return os.read_file(os.join_path(@DIR, 'multiwindow_d_gg_multiwindow.v')) or { panic(err) }
}

fn multiwindow_business_multiwindow_workflow_sources() string {
	vlib_dir := os.dir(@DIR)
	workflow_dir := os.join_path(vlib_dir, '..', '.github', 'workflows')
	mut sources := []string{}
	for workflow_name in ['linux_ci.yml', 'macos_ci.yml', 'windows_ci_msvc.yml', 'windows_ci_gcc.yml',
		'windows_ci_tcc.yml'] {
		workflow_path := os.join_path(workflow_dir, workflow_name)
		if os.exists(workflow_path) {
			sources << os.read_file(workflow_path) or { panic(err) }
		}
	}
	assert sources.len > 0
	return sources.join('\n')
}

fn multiwindow_temp_paths(name string) (string, string) {
	unique := '${name}_${os.getpid()}_${time.now().unix_nano()}'
	source_path := os.join_path(os.temp_dir(), '${unique}.v')
	mut bin_path := os.join_path(os.temp_dir(), '${unique}_bin')
	$if windows {
		bin_path += '.exe'
	}
	return source_path, bin_path
}

fn multiwindow_child_v_flags() string {
	mut flags := ''
	$if gg_multiwindow ? {
		flags += ' -d gg_multiwindow'
	}
	$if x_multiwindow_x11 ? {
		flags += ' -d x_multiwindow_x11'
	}
	$if gcc {
		flags += ' -cc gcc'
	}
	$if msvc {
		flags += ' -cc msvc'
	}
	$if sokol_wayland ? {
		flags += ' -d sokol_wayland'
	}
	$if sokol_metal ? {
		flags += ' -d sokol_metal'
	}
	$if sokol_d3d11 ? {
		flags += ' -d sokol_d3d11'
	}
	return flags
}

fn multiwindow_assert_command_ok(label string, cmd string, result os.Result) {
	assert result.exit_code == 0, '${label} failed
command: ${cmd}
exit_code: ${result.exit_code}
output:
${result.output}'
}

fn multiwindow_expect_win32_renderer_unsupported_without_d3d11(message string) bool {
	$if windows {
		$if !sokol_d3d11 ? {
			return message == 'multiwindow: renderer is unsupported'
		}
	}
	_ = message
	return false
}

fn multiwindow_skip_win32_d3d_device_failure(message string) bool {
	$if windows {
		if message == 'multiwindow: win32 d3d11 device failed' {
			eprintln('skip win32 render capability probe: ${message}')
			return true
		}
	}
	_ = message
	return false
}

struct GraphicalEnvSnapshot {
	display                 string
	display_existed         bool
	wayland_display         string
	wayland_display_existed bool
}

fn snapshot_graphical_env() GraphicalEnvSnapshot {
	env := os.environ()
	mut display := ''
	mut display_existed := false
	if 'DISPLAY' in env {
		display = env['DISPLAY']
		display_existed = true
	}
	mut wayland_display := ''
	mut wayland_display_existed := false
	if 'WAYLAND_DISPLAY' in env {
		wayland_display = env['WAYLAND_DISPLAY']
		wayland_display_existed = true
	}
	return GraphicalEnvSnapshot{
		display:                 display
		display_existed:         display_existed
		wayland_display:         wayland_display
		wayland_display_existed: wayland_display_existed
	}
}

fn restore_graphical_env(snapshot GraphicalEnvSnapshot) {
	if snapshot.display_existed {
		os.setenv('DISPLAY', snapshot.display, true)
	} else {
		os.unsetenv('DISPLAY')
	}
	if snapshot.wayland_display_existed {
		os.setenv('WAYLAND_DISPLAY', snapshot.wayland_display, true)
	} else {
		os.unsetenv('WAYLAND_DISPLAY')
	}
}

fn set_graphical_env(display string, wayland_display string) {
	if display == '' {
		os.unsetenv('DISPLAY')
	} else {
		os.setenv('DISPLAY', display, true)
	}
	if wayland_display == '' {
		os.unsetenv('WAYLAND_DISPLAY')
	} else {
		os.setenv('WAYLAND_DISPLAY', wayland_display, true)
	}
}
