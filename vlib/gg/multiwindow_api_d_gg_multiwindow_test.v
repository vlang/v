// vtest build: gg_multiwindow?
module gg

// Compile with `-d gg_multiwindow`; this test exercises the opt-in facade.
import os
import time
import sokol.sgl
import x.multiwindow

fn test_multiwindow_new_app_reports_core_capabilities() {
	mut app := new_app(backend: .mock, queue_size: 4)!
	caps := app.capabilities()

	assert caps.backend == .mock
	assert caps.mock
	assert !caps.native
	assert caps.multi_window
	assert caps.owner_queue
	assert !caps.explicit_swapchain

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

fn test_multiwindow_capabilities_with_renderer_signature_source_guard() {
	signature := 'pub fn capabilities_for_backend_with_renderer(backend MultiWindowBackend) !Capabilities'
	source := multiwindow_facade_source()

	assert source.contains(signature)
	assert !source.contains('pub fn capabilities_for_backend_with_renderer(backend MultiWindowBackend,')
}

fn test_multiwindow_ci_does_not_run_gg_api_tests_under_fake_wayland_display() {
	source := multiwindow_gg_regressions_workflow_source()
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

fn test_multiwindow_window_context_wraps_live_window() {
	mut app := new_app(backend: .mock)!
	win := app.create_window(title: 'Render target')!
	context := app.window_context(win)!

	assert context.window_id() == win
	assert context.exists()
	assert context.capabilities().backend == .mock

	app.destroy_window(win)!
	assert !context.exists()
	app.stop()!
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
		assert err.msg() == err_multiwindow_renderer_unsupported
		assert !draw_ran
		app.stop()!
		return
	}
	assert false, 'draw_window accepted mock backend without a swapchain'
}

fn test_multiwindow_draw_window_commits_each_frame_source_guard() {
	source := multiwindow_facade_source()
	draw_window_source :=
		source.all_after('// draw_window renders one live window through its WindowContext.').all_before('// stop shuts down the app')

	assert !source.contains('pending_render_frames')
	assert !source.contains('finish_pending_render_frames')
	assert !source.contains('abort_pending_render_frames')
	assert !source.contains('in_run_frame')
	assert source.count('gfx.commit()') == 1
	assert draw_window_source.count('gfx.commit()') == 1
	assert draw_window_source.contains('gfx.end_pass()\n\tgfx.commit()\n\tapp.core.end_render(frame)!')
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
	_ := app.window_context(id) or {
		assert err.msg() == err_multiwindow_app_not_initialized
		rejected += 1
		WindowContext{}
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

	assert rejected == 16
	assert !app.window_exists(id)
	assert !app.capabilities().multi_window
}

fn test_multiwindow_stop_validates_owner_before_renderer_shutdown_source_guard() {
	source := multiwindow_facade_source()
	stop_source :=
		source.all_after('// stop shuts down the app and destroys live windows.').all_before('// window_id returns')

	assert stop_source.contains('app.ensure_initialized()!')
	assert !stop_source.all_before('app.assert_owner_thread()!').contains('app.shutdown_renderer()')
	assert stop_source.all_after('app.assert_owner_thread()!').contains('app.shutdown_renderer()')
	assert stop_source.all_after('app.shutdown_renderer()').contains('app.core.stop()!')
}

struct MockCloseCallbackSeen {
mut:
	created         bool
	close_requested bool
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

fn test_multiwindow_event_only_run_idles_when_idle_source_guard() {
	source := multiwindow_facade_source()
	run_source :=
		source.all_after('// run starts the multi-window owner loop.').all_before('fn (mut app App) dispatch_run_events')
	dispatch_source :=
		source.all_after('fn (mut app App) dispatch_run_events').all_before('// draw_window renders one live window')

	assert source.contains('const multiwindow_event_idle_sleep = 8 * time.millisecond')
	assert run_source.contains('polled_events := app.poll_events()!')
	assert run_source.contains('drained_jobs = app.core.drain_pending(config.max_pending_jobs) or {')
	assert run_source.contains('dispatched_events := app.dispatch_run_events(config.event_fn)!')
	assert run_source.contains('} else if polled_events == 0 && drained_jobs == 0 && dispatched_events == 0 {\n\t\t\ttime.sleep(multiwindow_event_idle_sleep)\n\t\t}')
	assert dispatch_source.all_before('if event_fn == unsafe { nil }').contains('events := app.drain_events()!')
	assert !source.contains('event_idle_sleeps')
	assert !source.contains('stop_after_idle_sleeps')
}

fn test_multiwindow_dispatch_without_event_callback_still_drains_lifecycle_events() {
	mut app := new_app(backend: .mock)!
	win := app.create_window(title: 'Frame-only lifecycle drain')!

	drained_created := app.dispatch_run_events(unsafe { nil })!
	assert drained_created == 1

	app.sgl_contexts[win.str()] = sgl.Context{}
	app.core.destroy_window(win.core)!

	drained_destroyed := app.dispatch_run_events(unsafe { nil })!
	assert drained_destroyed == 1
	assert win.str() !in app.sgl_contexts

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
		app1.shutdown_renderer()
		app1.stop()!
		app2.stop()!
		return
	}
	app1.shutdown_renderer()
	app1.stop()!
	app2.stop()!
	assert false, 'second gg.App claimed sokol.gfx while first app owned it'
}

fn test_multiwindow_same_app_can_reclaim_renderer_owner() {
	mut app := new_app(backend: .mock)!

	app.ensure_render_owner()!
	app.ensure_render_owner()!
	app.shutdown_renderer()
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
	msg := <-result
	t.wait()

	assert msg == 'multiwindow: operation requires the owner thread'
	gg_claim_gfx_render_owner(.legacy_context, legacy_token) or {
		assert err.msg() == 'gg.multiwindow: sokol.gfx is already owned by gg.App'
		app.stop()!
		return
	}
	gg_release_gfx_render_owner(.legacy_context, legacy_token)
	app.stop()!
	assert false, 'foreign-thread stop released gg.App render ownership'
}

fn test_multiwindow_render_init_failure_releases_owner_source_guard() {
	source := multiwindow_facade_source()
	init_source :=
		source.all_after('fn (mut app App) ensure_render_initialized').all_before('fn (mut app App) window_context_for_frame')
	shutdown_source :=
		source.all_after('fn (mut app App) shutdown_renderer').all_before('fn (mut app App) owner_token')

	assert init_source.contains('app.core.render_environment(id.core) or {')
	assert init_source.contains('app.release_render_owner()')
	assert shutdown_source.contains('app.release_render_owner()')
}

fn test_multiwindow_destroy_window_discards_sgl_context() {
	mut app := new_app(backend: .mock)!
	win := app.create_window(title: 'Context owner')!
	app.sgl_contexts[win.str()] = sgl.Context{}

	app.destroy_window(win)!

	assert win.str() !in app.sgl_contexts
	app.stop()!
}

fn test_multiwindow_drain_events_discards_destroyed_window_sgl_context() {
	mut app := new_app(backend: .mock)!
	win := app.create_window(title: 'Native destroyed')!
	assert app.drain_events()!.len == 1
	app.sgl_contexts[win.str()] = sgl.Context{}
	app.core.destroy_window(win.core)!

	events := app.drain_events()!

	assert events.len == 1
	assert events[0].kind == .window_destroyed
	assert events[0].window == win
	assert win.str() !in app.sgl_contexts
	app.stop()!
}

fn test_multiwindow_sgl_context_lifecycle_uses_discard_path_source_guard() {
	source := multiwindow_facade_source()
	destroy_window_source :=
		source.all_after('// destroy_window destroys a live window.').all_before('// window_exists reports')
	drain_events_source :=
		source.all_after('// drain_events returns and clears pending window lifecycle events.').all_before('// poll_events lets')
	discard_source :=
		source.all_after('fn (mut app App) discard_window_sgl_context').all_before('fn (mut app App) shutdown_renderer')

	assert destroy_window_source.contains('app.core.destroy_window(id.core)!\n\tapp.discard_window_sgl_context(id)')
	assert drain_events_source.contains('if window_event.kind == .window_destroyed {\n\t\t\tapp.discard_window_sgl_context(window_event.window)\n\t\t}')
	assert discard_source.contains('sgl.destroy_context(context)')
	assert discard_source.contains('app.sgl_contexts.delete(key)')
	assert !source.contains('sgl_context_discards')
}

fn test_multiwindow_legacy_then_app_ownership_is_rejected_without_window() {
	legacy_token := voidptr(usize(0x3003))
	mut app := new_app(backend: .mock)!

	gg_claim_gfx_render_owner(.legacy_context, legacy_token)!
	app.ensure_render_owner() or {
		assert err.msg() == 'gg.multiwindow: sokol.gfx is already owned by gg.Context'
		gg_release_gfx_render_owner(.legacy_context, legacy_token)
		app.stop()!
		return
	}
	gg_release_gfx_render_owner(.legacy_context, legacy_token)
	app.stop()!
	assert false, 'gg.App claimed sokol.gfx while legacy gg.Context token owned it'
}

fn test_multiwindow_app_then_legacy_ownership_is_rejected_without_window() {
	mut app := new_app(backend: .mock)!
	legacy_token := voidptr(usize(0x4004))

	app.ensure_render_owner()!
	gg_claim_gfx_render_owner(.legacy_context, legacy_token) or {
		assert err.msg() == 'gg.multiwindow: sokol.gfx is already owned by gg.App'
		app.shutdown_renderer()
		app.stop()!
		return
	}
	app.shutdown_renderer()
	app.stop()!
	assert false, 'legacy gg.Context claimed sokol.gfx while gg.App owned it'
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

fn test_multiwindow_render_api_user_program_imports_only_gg() {
	vlib_dir := os.dir(@DIR)
	source_path, out_path_base := multiwindow_temp_paths('gg_multiwindow_render_api_compile')
	c_path := '${out_path_base}.c'
	source := "import gg

fn main() {
	mut app := gg.new_app(backend: .mock)!
	win := app.create_window(title: 'Main')!
	context := app.window_context(win)!
	assert context.exists()
	assert context.window_id().str() == win.str()
	app.draw_window(win, fn (mut window gg.WindowContext) ! {
		_ = window.window_id()
	}) or {}
	app.run(frame_fn: fn (mut app gg.App) ! {
		_ = app.capabilities()
	}) or {}
	app.stop()!
}
"
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(c_path) or {}
		os.rm(out_path_base) or {}
	}

	cmd := '${os.quoted_path(@VEXE)}${multiwindow_child_v_flags()} -b c -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(c_path)} ${os.quoted_path(source_path)}'
	compile := os.execute(cmd)
	multiwindow_assert_command_ok('compile child render API smoke', cmd, compile)
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

fn test_multiwindow_checked_in_example_wraps_resize_unsupported_source_guard() {
	source := multiwindow_example_source()

	assert source.contains('resize_or_ignore_unsupported')
	assert source.contains("err.msg() == 'multiwindow: backend capability is unsupported'")
	assert !multiwindow_source_has_unwrapped_resize_window(source)
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

fn multiwindow_gg_regressions_workflow_source() string {
	vlib_dir := os.dir(@DIR)
	workflow_path := os.join_path(vlib_dir, '..', '.github', 'workflows', 'gg_regressions_ci.yml')
	return os.read_file(workflow_path) or { panic(err) }
}

fn multiwindow_example_source() string {
	vlib_dir := os.dir(@DIR)
	example_path := os.join_path(vlib_dir, '..', 'examples', 'gg', 'multiwindow.v')
	return os.read_file(example_path) or { panic(err) }
}

fn multiwindow_source_has_unwrapped_resize_window(source string) bool {
	for line in source.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed.starts_with('app.resize_window(') && trimmed.ends_with(')!') {
			return true
		}
	}
	return false
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
