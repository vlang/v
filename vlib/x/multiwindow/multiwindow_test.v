module multiwindow

import os

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}
import time

fn test_window_id_generation_rejects_stale_after_slot_reuse() {
	mut app := new_app()!
	first := app.create_window(title: 'First')!

	assert app.window_exists(first)
	app.destroy_window(first)!
	assert !app.window_exists(first)
	assert app.window_status(first)! == .destroyed

	second := app.create_window(title: 'Second')!
	assert app.window_exists(second)
	assert second.slot == first.slot
	assert second.generation == first.generation + 1

	app.destroy_window(first) or {
		assert err.msg() == err_stale_window
		app.stop()!
		return
	}
	assert false, 'destroy_window accepted a stale WindowId'
}

fn test_destroy_child_keeps_app_and_other_windows_alive() {
	mut app := new_app()!
	main := app.create_window(title: 'Main')!
	child := app.create_window(title: 'Child')!

	app.destroy_window(child)!

	assert app.status() == .running
	assert app.window_exists(main)
	assert !app.window_exists(child)
	assert app.window_status(child)! == .destroyed
	app.stop()!
}

fn test_destroy_last_window_keeps_app_running_without_live_windows() {
	mut app := new_app()!
	only := app.create_window(title: 'Only')!

	app.destroy_window(only)!

	assert app.status() == .running
	assert !app.window_exists(only)
	assert app.window_status(only)! == .destroyed
	app.stop()!
}

fn test_stop_invalidates_live_windows_and_emits_destroy_events() {
	mut app := new_app()!
	main := app.create_window(title: 'Main')!
	tools := app.create_window(title: 'Tools')!

	app.stop()!

	assert app.status() == .stopped
	assert !app.window_exists(main)
	assert !app.window_exists(tools)
	assert app.window_status(main)! == .destroyed
	assert app.window_status(tools)! == .destroyed
	events := app.drain_events()!
	assert events.len == 4
	assert events[0].kind == .window_created
	assert events[0].window_id == main
	assert events[1].kind == .window_created
	assert events[1].window_id == tools
	assert events[2].kind == .window_destroyed
	assert events[2].window_id == main
	assert events[3].kind == .window_destroyed
	assert events[3].window_id == tools
}

fn test_events_are_routed_by_window_id() {
	mut app := new_app()!
	main := app.create_window(title: 'Main')!
	tools := app.create_window(title: 'Tools')!
	app.destroy_window(main)!

	events := app.drain_events()!
	assert events.len == 3
	assert events[0].kind == .window_created
	assert events[0].window_id == main
	assert events[0].width == 800
	assert events[0].height == 600
	assert events[1].kind == .window_created
	assert events[1].window_id == tools
	assert events[1].width == 800
	assert events[1].height == 600
	assert events[2].kind == .window_destroyed
	assert events[2].window_id == main
	assert app.drain_events()!.len == 0
	app.stop()!
}

fn test_resize_window_emits_resized_event_after_state_update() {
	mut app := new_app()!
	win := app.create_window(title: 'Resize')!

	app.resize_window(win, 640, 360)!

	info := app.window_info(win)!
	assert info.width == 640
	assert info.height == 360
	events := app.drain_events()!
	assert events.len == 2
	assert events[0].kind == .window_created
	assert events[0].window_id == win
	assert events[1].kind == .window_resized
	assert events[1].window_id == win
	assert events[1].width == 640
	assert events[1].height == 360
	app.stop()!
}

fn test_resize_window_uses_actual_backend_size_for_state_and_event() {
	mut app := new_app()!
	win := app.create_window(
		title:      'Actual resize'
		width:      320
		height:     240
		min_width:  300
		min_height: 200
	)!

	app.resize_window(win, 120, 80)!

	info := app.window_info(win)!
	assert info.width == 300
	assert info.height == 200
	events := app.drain_events()!
	assert events.len == 2
	assert events[0].kind == .window_created
	assert events[0].window_id == win
	assert events[1].kind == .window_resized
	assert events[1].window_id == win
	assert events[1].width == 300
	assert events[1].height == 200
	app.stop()!
}

fn test_create_window_uses_actual_backend_initial_size_for_state_and_event() {
	mut app := new_app()!
	win := app.create_window(
		title:      'Actual create'
		width:      120
		height:     80
		min_width:  300
		min_height: 200
	)!

	info := app.window_info(win)!
	assert info.width == 300
	assert info.height == 200
	events := app.drain_events()!
	assert events.len == 1
	assert events[0].kind == .window_created
	assert events[0].window_id == win
	assert events[0].width == 300
	assert events[0].height == 200
	app.stop()!
}

fn test_mock_close_requested_routes_through_poll_events_without_destroying_window() {
	mut app := new_app()!
	win := app.create_window(title: 'Close request')!
	assert app.drain_events()!.len == 1

	app.enqueue_mock_close_requested_for_test(win)!

	assert app.poll_events()! == 1
	events := app.drain_events()!
	assert events.len == 1
	assert events[0].kind == .window_close_requested
	assert events[0].window_id == win
	assert events[0].width == 0
	assert events[0].height == 0
	assert app.window_exists(win)
	assert app.window_status(win)! == .alive

	assert app.poll_events()! == 0
	assert app.drain_events()!.len == 0
	app.stop()!
}

fn test_mock_close_requested_test_helper_rejects_destroyed_window_without_event() {
	mut app := new_app()!
	win := app.create_window(title: 'Destroyed close request')!
	assert app.drain_events()!.len == 1
	app.destroy_window(win)!
	assert app.drain_events()!.len == 1

	app.enqueue_mock_close_requested_for_test(win) or {
		assert err.msg() == err_stale_window
		assert app.poll_events()! == 0
		assert app.drain_events()!.len == 0
		app.stop()!
		return
	}
	assert false, 'mock close request helper accepted a destroyed WindowId'
}

fn test_mock_stale_backend_close_requested_is_filtered_by_app_poll_events() {
	mut app := new_app()!
	first := app.create_window(title: 'First close request target')!
	assert app.drain_events()!.len == 1
	app.destroy_window(first)!
	assert app.drain_events()!.len == 1
	second := app.create_window(title: 'Reused close request target')!
	assert second.slot == first.slot
	assert second.generation != first.generation
	assert app.drain_events()!.len == 1

	app.enqueue_mock_close_requested_unchecked_for_test(first)!

	assert app.poll_events()! == 0
	assert app.drain_events()!.len == 0
	assert app.window_exists(second)
	assert app.window_status(second)! == .alive
	app.window_status(first) or {
		assert err.msg() == err_stale_window
		app.stop()!
		return
	}
	assert false, 'stale WindowId remained valid after slot reuse'
}

fn test_owner_queue_runs_jobs_and_rejects_destroyed_window_work() {
	mut app := new_app(queue_size: 4)!
	win := app.create_window(title: 'Queued')!
	ran := chan bool{cap: 1}

	app.post(fn [ran] (mut queued_app App) ! {
		assert queued_app.status() == .running
		ran <- true
	})!
	assert app.drain_pending(1)! == 1
	assert <-ran

	app.destroy_window(win)!
	app.post(fn [win] (mut queued_app App) ! {
		queued_app.destroy_window(win)!
	})!
	app.drain_pending(1) or {
		assert err.msg() == err_stale_window
		app.stop()!
		return
	}
	assert false, 'queued work accepted a destroyed WindowId'
}

fn test_queue_full_returns_multiwindow_error() {
	mut app := new_app(queue_size: 1)!
	app.try_post(fn (mut queued_app App) ! {
		_ = queued_app.status()
	})!

	app.try_post(fn (mut queued_app App) ! {
		_ = queued_app.status()
	}) or {
		assert err.msg() == err_owner_queue_full
		app.stop()!
		return
	}
	assert false, 'try_post accepted work beyond queue capacity'
}

fn test_nil_job_is_rejected() {
	mut app := new_app()!
	nil_job := unsafe { AppJobFn(nil) }

	app.try_post(nil_job) or {
		assert err.msg() == err_nil_job
		app.stop()!
		return
	}
	assert false, 'try_post accepted a nil job'
}

fn test_post_is_rejected_after_stop() {
	mut app := new_app(queue_size: 1)!
	app.stop()!

	app.post(fn (mut queued_app App) ! {
		_ = queued_app.status()
	}) or {
		assert err.msg() == err_app_stopped
		return
	}
	assert false, 'post accepted work after stop'
}

fn test_stop_cancels_pending_jobs_and_drain_after_stop_is_stable() {
	mut app := new_app(queue_size: 2)!
	ran := chan bool{cap: 1}
	app.try_post(fn [ran] (mut queued_app App) ! {
		_ = queued_app.status()
		ran <- true
	})!

	app.stop()!

	app.drain_pending(1) or {
		assert err.msg() == err_app_stopped
		assert_no_bool_signal(ran, 'pending job ran after stop')
		return
	}
	assert false, 'drain_pending succeeded after stop'
}

fn test_stop_during_drain_cancels_remaining_jobs() {
	mut app := new_app(queue_size: 2)!
	ran_after_stop := chan bool{cap: 1}
	app.try_post(fn (mut queued_app App) ! {
		queued_app.stop()!
	})!
	app.try_post(fn [ran_after_stop] (mut queued_app App) ! {
		_ = queued_app.status()
		ran_after_stop <- true
	})!

	app.drain_pending(2) or {
		assert err.msg() == err_app_stopped
		assert_no_bool_signal(ran_after_stop, 'pending job ran after stop during drain')
		return
	}
	assert false, 'drain_pending continued after a queued job stopped the app'
}

fn test_invalid_app_and_window_config_are_rejected() {
	new_app(queue_size: 0) or { assert err.msg() == err_queue_size_invalid }

	mut app := new_app()!
	app.create_window(width: 0) or {
		assert err.msg() == err_invalid_window_size
		app.stop()!
		return
	}
	assert false, 'create_window accepted zero width'
}

fn test_unknown_and_stale_window_ids_are_rejected() {
	mut app := new_app()!
	unknown := WindowId{
		slot:       42
		generation: 1
	}
	app.destroy_window(unknown) or { assert err.msg() == err_window_not_found }

	first := app.create_window(title: 'First')!
	app.destroy_window(first)!
	second := app.create_window(title: 'Second')!
	assert second.generation != first.generation

	app.window_status(first) or {
		assert err.msg() == err_stale_window
		app.stop()!
		return
	}
	assert false, 'window_status accepted a stale WindowId'
}

fn test_drain_limit_invalid_returns_multiwindow_error() {
	mut app := new_app()!
	app.drain_pending(0) or {
		assert err.msg() == err_drain_limit_invalid
		app.stop()!
		return
	}
	assert false, 'drain_pending accepted a zero limit'
}

fn test_backend_capabilities_are_stable() {
	mut app := new_app()!
	caps_before := app.capabilities()
	win := app.create_window(title: 'Caps')!
	app.destroy_window(win)!
	caps_after := app.capabilities()

	assert caps_before.backend == .mock
	assert caps_after.backend == .mock
	assert caps_before.mock == caps_after.mock
	assert caps_before.native == caps_after.native
	assert caps_before.multi_window == caps_after.multi_window
	assert caps_before.owner_queue == caps_after.owner_queue
	assert caps_before.explicit_swapchain == caps_after.explicit_swapchain
	app.stop()!
}

fn test_window_info_title_and_resize_use_authoritative_app_state() {
	mut app := new_app()!
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
	assert info.status == .alive
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
	updated_title := app.window_info(win)!
	assert updated_title.title == 'Updated'
	assert updated_title.width == 320
	assert updated_title.height == 200

	app.resize_window(win, 640, 360)!
	resized := app.window_info(win)!
	assert resized.title == 'Updated'
	assert resized.width == 640
	assert resized.height == 360
	assert resized.min_width == 100
	assert resized.min_height == 80
	assert !resized.resizable

	app.stop()!
}

fn test_window_enumeration_returns_live_windows_in_slot_order() {
	mut app := new_app()!
	first := app.create_window(title: 'First', width: 100, height: 80)!
	second := app.create_window(title: 'Second', width: 200, height: 120)!
	third := app.create_window(title: 'Third', width: 300, height: 160)!

	assert app.window_ids()! == [first, second, third]
	app.destroy_window(second)!
	assert app.window_ids()! == [first, third]

	reused := app.create_window(title: 'Reused', width: 400, height: 200)!
	assert reused.slot == second.slot
	assert app.window_ids()! == [first, reused, third]

	infos := app.window_infos()!
	assert infos.len == 3
	assert infos[0].id == first
	assert infos[0].title == 'First'
	assert infos[1].id == reused
	assert infos[1].title == 'Reused'
	assert infos[1].width == 400
	assert infos[2].id == third
	assert infos[2].title == 'Third'
	app.stop()!
}

fn test_window_info_and_mutation_validate_window_ids() {
	mut app := new_app()!
	win := app.create_window(title: 'Validation')!

	mut rejected_resize := false
	app.resize_window(win, 0, 100) or {
		assert err.msg() == err_invalid_window_size
		rejected_resize = true
	}
	assert rejected_resize
	info_after_rejected_resize := app.window_info(win)!
	assert info_after_rejected_resize.width == 800
	assert info_after_rejected_resize.height == 600

	app.destroy_window(win)!

	mut rejected_info := false
	app.window_info(win) or {
		assert err.msg() == err_stale_window
		rejected_info = true
	}
	assert rejected_info

	mut rejected_title := false
	app.set_window_title(win, 'Destroyed') or {
		assert err.msg() == err_stale_window
		rejected_title = true
	}
	assert rejected_title

	mut rejected_destroyed_resize := false
	app.resize_window(win, 100, 100) or {
		assert err.msg() == err_stale_window
		rejected_destroyed_resize = true
	}
	assert rejected_destroyed_resize

	unknown := WindowId{
		slot:       99
		generation: 1
	}
	mut rejected_unknown_info := false
	app.window_info(unknown) or {
		assert err.msg() == err_window_not_found
		rejected_unknown_info = true
	}
	assert rejected_unknown_info

	app.stop()!
}

fn test_capabilities_for_backend_uses_backend_seam_without_app() {
	caps := capabilities_for_backend(.mock)!

	assert caps.backend == .mock
	assert caps.mock
	assert !caps.native
	assert caps.multi_window
	assert caps.owner_queue
	assert !caps.explicit_swapchain
}

fn test_mock_render_seam_reports_renderer_unsupported() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		mut app := new_app()!
		win := app.create_window(title: 'No renderer')!
		mut rejected_environment := false
		app.render_environment(win) or {
			assert err.msg() == err_renderer_unsupported
			rejected_environment = true
		}
		assert rejected_environment
		mut rejected_begin := false
		app.begin_render(win) or {
			assert err.msg() == err_renderer_unsupported
			rejected_begin = true
		}
		assert rejected_begin
		app.stop()!
	} $else {
		return
	}
}

fn test_auto_render_resolver_prefers_x11_when_render_is_required() {
	$if linux {
		snapshot := snapshot_graphical_env()
		defer {
			restore_graphical_env(snapshot)
		}
		set_graphical_env(':v-multiwindow-fake', 'wayland-v-multiwindow-fake')

		$if x_multiwindow_x11 ? {
			assert resolve_auto_backend_kind(true) == .x11
			$if sokol_wayland ? {
				assert resolve_auto_backend_kind(false) == .wayland
			} $else {
				assert resolve_auto_backend_kind(false) == .x11
			}
		} $else {
			$if sokol_wayland ? {
				assert resolve_auto_backend_kind(true) == .wayland
				assert resolve_auto_backend_kind(false) == .wayland
			} $else {
				assert resolve_auto_backend_kind(true) == .mock
				assert resolve_auto_backend_kind(false) == .mock
			}
		}
	} $else {
		$if windows {
			assert resolve_auto_backend_kind(true) == .win32
			assert resolve_auto_backend_kind(false) == .win32
		} $else {
			$if darwin {
				assert resolve_auto_backend_kind(true) == .appkit
				assert resolve_auto_backend_kind(false) == .appkit
			} $else {
				assert resolve_auto_backend_kind(true) == .mock
				assert resolve_auto_backend_kind(false) == .mock
			}
		}
	}
}

fn test_capabilities_for_config_render_required_prefers_x11_over_wayland() {
	$if linux {
		snapshot := snapshot_graphical_env()
		defer {
			restore_graphical_env(snapshot)
		}
		set_graphical_env(':v-multiwindow-fake', 'wayland-v-multiwindow-fake')

		$if x_multiwindow_x11 ? {
			assert resolve_auto_backend_kind(true) == .x11
			$if sokol_wayland ? {
				assert resolve_auto_backend_kind(false) == .wayland
			} $else {
				assert resolve_auto_backend_kind(false) == .x11
			}
		} $else {
			$if sokol_wayland ? {
				assert resolve_auto_backend_kind(true) == .wayland
				assert resolve_auto_backend_kind(false) == .wayland
			} $else {
				assert resolve_auto_backend_kind(true) == .mock
				assert resolve_auto_backend_kind(false) == .mock
			}
		}

		if !multiwindow_render_enabled() {
			capabilities_for_config(backend: .auto, require_renderer: true) or {
				assert err.msg() == err_renderer_unsupported
				return
			}
			assert false, 'render capability succeeded without a render-enabled build'
		}
		$if x_multiwindow_x11 ? {
			assert_auto_render_config_x11_caps_or_fake_display_open_failure()
			assert_auto_render_helper_x11_caps_or_fake_display_open_failure()
		}

		lifecycle_caps := capabilities_for_backend(.auto)!
		$if sokol_wayland ? {
			assert lifecycle_caps.backend == .wayland
			assert lifecycle_caps.wayland
			assert !lifecycle_caps.explicit_swapchain
		} $else {
			$if x_multiwindow_x11 ? {
				assert_x11_lifecycle_capabilities(lifecycle_caps)
			} $else {
				assert lifecycle_caps.backend == .mock
				assert lifecycle_caps.mock
				assert !lifecycle_caps.native
			}
		}
	} $else {
		$if windows {
			$if sokol_d3d11 ? {
				caps := capabilities_for_config(backend: .auto, require_renderer: true) or {
					if err.msg() == err_win32_d3d_device_failed {
						eprintln('skip win32 render capability probe: ${err.msg()}')
						return
					}
					assert false, 'unexpected win32 render capability error: ${err.msg()}'
					return
				}
				assert caps.backend == .win32
				assert caps.win32
				assert caps.native
				assert caps.explicit_swapchain
				assert caps.d3d11
			} $else {
				capabilities_for_config(backend: .auto, require_renderer: true) or {
					assert err.msg() == err_renderer_unsupported
					return
				}
				assert false, 'win32 render capability succeeded without -d sokol_d3d11'
			}
		} $else {
			$if darwin {
				if !multiwindow_render_enabled() {
					capabilities_for_config(backend: .auto, require_renderer: true) or {
						assert err.msg() == err_renderer_unsupported
						return
					}
					assert false, 'appkit render capability succeeded without a render-enabled build'
				}
				$if darwin_sokol_glcore33 ? {
					capabilities_for_config(backend: .auto, require_renderer: true) or {
						assert err.msg() == err_renderer_unsupported
						return
					}
					assert false, 'appkit render capability succeeded with darwin_sokol_glcore33'
				} $else {
					caps := capabilities_for_config(backend: .auto, require_renderer: true)!
					assert caps.backend == .appkit
					assert caps.native
					assert caps.explicit_swapchain
					assert caps.metal
				}
			} $else {
				caps := capabilities_for_config(backend: .auto, require_renderer: true)!
				assert caps.backend == .mock
				assert caps.mock
			}
		}
	}
}

fn assert_auto_render_config_x11_caps_or_fake_display_open_failure() {
	caps := capabilities_for_config(backend: .auto, require_renderer: true) or {
		assert err.msg() == err_x11_open_display_failed
		return
	}
	assert_x11_render_capabilities(caps)
}

fn assert_auto_render_helper_x11_caps_or_fake_display_open_failure() {
	caps := capabilities_for_backend_with_renderer(.auto, true) or {
		assert err.msg() == err_x11_open_display_failed
		return
	}
	assert_x11_render_capabilities(caps)
}

fn assert_x11_render_capabilities(caps Capabilities) {
	assert caps.backend == .x11
	assert caps.x11
	assert !caps.wayland
	assert caps.native
	assert caps.explicit_swapchain
	assert caps.gl
}

fn assert_x11_lifecycle_capabilities(caps Capabilities) {
	assert caps.backend == .x11
	assert caps.x11
	assert !caps.wayland
	assert caps.native
	assert caps.multi_window
	assert caps.owner_queue
	assert !caps.explicit_swapchain
}

fn test_fake_wayland_lifecycle_capabilities_do_not_claim_render_ready() {
	$if linux {
		snapshot := snapshot_graphical_env()
		defer {
			restore_graphical_env(snapshot)
		}
		set_graphical_env('', 'wayland-v-multiwindow-fake')

		$if sokol_wayland ? {
			caps := capabilities_for_backend(.auto) or {
				assert err.msg() == err_wayland_connect_failed
				return
			}
			assert caps.backend == .wayland
			assert caps.wayland
			assert !caps.explicit_swapchain
			assert !caps.gl

			helper_caps := capabilities_for_backend(.wayland) or {
				assert err.msg() == err_wayland_connect_failed
				return
			}
			assert helper_caps.backend == .wayland
			assert !helper_caps.explicit_swapchain
			assert !helper_caps.gl
		} $else {
			caps := capabilities_for_backend(.auto)!
			assert caps.backend == .mock
			assert caps.mock
			assert !caps.native
			assert !caps.explicit_swapchain
			capabilities_for_backend(.wayland) or {
				assert err.msg() == err_backend_unsupported
				return
			}
			assert false, 'wayland capabilities succeeded without -d sokol_wayland'
		}
	} $else {
		return
	}
}

fn test_auto_event_only_ignores_wayland_without_sokol_wayland() {
	$if linux {
		$if sokol_wayland ? {
			return
		}
		snapshot := snapshot_graphical_env()
		defer {
			restore_graphical_env(snapshot)
		}
		set_graphical_env('', 'wayland-v-multiwindow-fake')

		assert resolve_auto_backend_kind(false) == .mock
		mut app := new_app(backend: .auto, require_renderer: false)!
		assert app.capabilities().mock
		win := app.create_window(title: 'Event only')!
		assert app.window_status(win)! == .alive
		app.stop()!
	} $else {
		return
	}
}

fn test_capabilities_for_config_render_required_allows_headless_mock() {
	if !multiwindow_render_enabled() {
		capabilities_for_config(backend: .auto, require_renderer: true) or {
			assert err.msg() == err_renderer_unsupported
			return
		}
		assert false, 'render capability succeeded without a render-enabled build'
		return
	}
	$if windows {
		assert resolve_auto_backend_kind(true) == .win32
		return
	}
	$if darwin {
		return
	}
	snapshot := snapshot_graphical_env()
	defer {
		restore_graphical_env(snapshot)
	}
	set_graphical_env('', '')

	caps := capabilities_for_config(backend: .auto, require_renderer: true)!
	assert caps.backend == .mock
	assert caps.mock
	assert !caps.native
	assert !caps.explicit_swapchain

	helper_caps := capabilities_for_backend_with_renderer(.auto, true)!
	assert helper_caps.backend == .mock
	assert helper_caps.mock
	assert !helper_caps.explicit_swapchain
}

fn test_auto_render_request_reports_wayland_connect_failure_for_invalid_display() {
	$if linux {
		snapshot := snapshot_graphical_env()
		defer {
			restore_graphical_env(snapshot)
		}
		set_graphical_env('', 'wayland-v-multiwindow-fake')

		if !multiwindow_render_enabled() {
			new_app(backend: .auto, require_renderer: true) or {
				assert err.msg() == err_renderer_unsupported
				return
			}
			assert false, 'auto render request succeeded without a render-enabled build'
			return
		}
		$if sokol_wayland ? {
			assert resolve_auto_backend_kind(true) == .wayland
			new_app(backend: .auto, require_renderer: true) or {
				assert err.msg() == err_wayland_connect_failed
				return
			}
			assert false, 'auto render request connected to a fake wayland display'
		} $else {
			assert resolve_auto_backend_kind(true) == .mock
			mut app := new_app(backend: .auto, require_renderer: true)!
			assert app.capabilities().mock
			app.stop()!
		}
	} $else {
		return
	}
}

fn test_auto_render_request_allows_mock_only_when_true_headless() {
	if !multiwindow_render_enabled() {
		new_app(backend: .auto, require_renderer: true) or {
			assert err.msg() == err_renderer_unsupported
			return
		}
		assert false, 'auto render request succeeded without a render-enabled build'
		return
	}
	$if windows {
		assert resolve_auto_backend_kind(true) == .win32
		return
	}
	$if darwin {
		return
	}
	snapshot := snapshot_graphical_env()
	defer {
		restore_graphical_env(snapshot)
	}
	set_graphical_env('', '')

	assert resolve_auto_backend_kind(true) == .mock
	mut app := new_app(backend: .auto, require_renderer: true)!
	caps := app.capabilities()
	assert caps.backend == .mock
	assert caps.mock
	assert !caps.explicit_swapchain
	app.stop()!
}

fn test_wayland_render_request_with_invalid_display_reports_connect_failure_on_linux() {
	$if linux {
		snapshot := snapshot_graphical_env()
		defer {
			restore_graphical_env(snapshot)
		}
		set_graphical_env('', 'wayland-v-multiwindow-fake')

		if !multiwindow_render_enabled() {
			new_app(backend: .wayland, require_renderer: true) or {
				$if sokol_wayland ? {
					assert err.msg() == err_renderer_unsupported
				} $else {
					assert err.msg() == err_backend_unsupported
				}
				return
			}
			assert false, 'wayland render app creation succeeded without a render-enabled build'
			return
		}
		new_app(backend: .wayland, require_renderer: true) or {
			$if sokol_wayland ? {
				assert err.msg() == err_wayland_connect_failed
			} $else {
				assert err.msg() == err_backend_unsupported
			}
			return
		}
		assert false, 'wayland render app creation connected to a fake display'
	} $else {
		new_app(backend: .wayland, require_renderer: true) or {
			assert err.msg() == err_backend_unsupported
			return
		}
		assert false, 'wayland app creation succeeded on an unsupported OS'
	}
}

fn test_auto_capabilities_for_backend_reports_selected_backend() {
	caps := capabilities_for_backend(.auto)!

	$if linux {
		$if sokol_wayland ? {
			if os.getenv('WAYLAND_DISPLAY') != '' {
				assert caps.backend == .wayland
				assert caps.wayland
				assert caps.native
				assert caps.multi_window
				assert caps.owner_queue
				return
			}
		}
		if os.getenv('DISPLAY') != '' {
			$if x_multiwindow_x11 ? {
				assert caps.backend == .x11
				assert caps.x11
				assert caps.native
				return
			}
		}
		assert caps.backend == .mock
		assert caps.mock
		assert !caps.native
	} $else {
		$if windows {
			assert caps.backend == .win32
			assert caps.win32
			assert caps.native
		} $else {
			$if darwin {
				assert caps.backend == .appkit
				assert caps.native
			} $else {
				assert caps.backend == .mock
				assert caps.mock
				assert !caps.native
			}
		}
	}
	assert caps.multi_window
	assert caps.owner_queue
}

fn test_win32_capabilities_for_backend_do_not_require_d3d_on_windows() {
	$if windows {
		caps := capabilities_for_backend(.win32)!
		assert caps.backend == .win32
		assert !caps.mock
		assert caps.native
		assert caps.multi_window
		assert caps.owner_queue
		assert caps.win32
		assert !caps.x11
		assert !caps.wayland
		assert !caps.explicit_swapchain
		assert !caps.d3d11
	} $else {
		capabilities_for_backend(.win32) or {
			assert err.msg() == err_backend_unsupported
			return
		}
		assert false, 'win32 capabilities succeeded on an unsupported OS'
	}
}

fn test_win32_render_capabilities_probe_d3d_on_windows_only() {
	$if windows {
		$if sokol_d3d11 ? {
			caps := capabilities_for_backend_with_renderer(.win32, true) or {
				if err.msg() == err_win32_d3d_device_failed {
					eprintln('skip win32 render capabilities probe: ${err.msg()}')
					return
				}
				assert false, 'unexpected win32 render capability error: ${err.msg()}'
				return
			}
			assert caps.backend == .win32
			assert caps.win32
			assert caps.native
			assert caps.explicit_swapchain
			assert caps.d3d11
		} $else {
			capabilities_for_backend_with_renderer(.win32, true) or {
				assert err.msg() == err_renderer_unsupported
				return
			}
			assert false, 'win32 render capabilities succeeded without -d sokol_d3d11'
		}
	} $else {
		capabilities_for_backend_with_renderer(.win32, true) or {
			assert err.msg() == err_backend_unsupported
			return
		}
		assert false, 'win32 render capabilities succeeded on an unsupported OS'
	}
}

fn test_win32_d3d11_present_status_mapping_treats_occluded_as_nonfatal() {
	win32_d3d11_check_present_status(0)!
	win32_d3d11_check_present_status(4)!
	assert_win32_d3d11_present_status_error(2, err_win32_d3d_device_removed)
	assert_win32_d3d11_present_status_error(3, err_win32_d3d_device_reset)
	assert_win32_d3d11_present_status_error(1, err_win32_d3d_present_failed)
	assert_win32_d3d11_present_status_error(99, err_win32_d3d_present_failed)
}

fn test_win32_min_size_plumbing_is_present() {
	win32_backend_source := multiwindow_source_file('win32_backend.c.v')
	win32_helper_source := multiwindow_source_file('win32_backend_helpers.h')
	assert win32_backend_source.contains('fn C.v_multiwindow_win32_create_window(title &u16, width int, height int, min_width int, min_height int')
	assert win32_backend_source.contains('fn C.v_multiwindow_win32_set_client_size(hwnd voidptr, width int, height int, min_width int, min_height int')
	assert win32_backend_source.contains('config.min_width, config.min_height')
	assert win32_backend_source.contains('record.config.min_width, record.config.min_height')
	assert win32_helper_source.contains('WM_GETMINMAXINFO')
	assert win32_helper_source.contains('MINMAXINFO *mmi')
	assert win32_helper_source.contains('ptMinTrackSize')
	assert win32_helper_source.contains('v_multiwindow_win32_min_width_prop')
	assert win32_helper_source.contains('v_multiwindow_win32_min_height_prop')
	assert win32_helper_source.contains('v_multiwindow_win32_set_hwnd_int_prop')
	assert win32_helper_source.contains('v_multiwindow_win32_max_int(width, min_width)')
	assert win32_helper_source.contains('v_multiwindow_win32_max_int(height, min_height)')
}

fn test_win32_runtime_resize_clamps_to_min_size_when_available() {
	$if windows {
		mut app := new_app(backend: .win32)!
		win := app.create_window(
			title:      'Win32 min size'
			width:      320
			height:     240
			min_width:  300
			min_height: 200
			visible:    false
		)!

		app.resize_window(win, 120, 80)!

		info := app.window_info(win)!
		assert info.width >= 300
		assert info.height >= 200
		events := app.drain_events()!
		assert events.len == 2
		assert events[1].kind == .window_resized
		assert events[1].width >= 300
		assert events[1].height >= 200
		app.stop()!
	} $else {
		return
	}
}

fn test_appkit_capabilities_for_backend_are_platform_scoped() {
	$if darwin {
		caps := capabilities_for_backend(.appkit)!
		assert caps.backend == .appkit
		assert !caps.mock
		assert caps.native
		assert caps.multi_window
		assert caps.owner_queue
		assert !caps.explicit_swapchain
		assert caps.metal == false
	} $else {
		capabilities_for_backend(.appkit) or {
			assert err.msg() == err_backend_unsupported
			return
		}
		assert false, 'appkit capabilities succeeded on an unsupported OS'
	}
}

fn test_appkit_metal_renderer_is_disabled_for_darwin_glcore_source_guard() {
	source := multiwindow_source_file('appkit_backend.c.v')
	assert source.contains('fn appkit_metal_supported() bool')
	assert source.contains('$if darwin_sokol_glcore33 ? {')
	assert source.contains('return error(err_renderer_unsupported)')
	assert source.contains('explicit_swapchain: appkit_metal_supported() && backend.renderer_ready()')
	assert source.contains('metal:              appkit_metal_supported() && backend.renderer_ready()')
	assert_source_order(source, 'if require_renderer {', 'if !appkit_metal_supported()')
	assert_source_order(source, 'if !appkit_metal_supported()', 'backend.init_renderer()!')
}

fn test_appkit_sharedlive_source_excludes_objc_implementation() {
	source := multiwindow_source_file('appkit_backend.c.v')
	assert source.contains('#insert "@VMODROOT/vlib/x/multiwindow/appkit_backend_helpers.h"')
	assert_source_order(source, '#insert "@VMODROOT/vlib/x/multiwindow/appkit_backend_helpers.h"',
		'$if sharedlive ? {')
	assert_source_order(source, '$if sharedlive ? {',
		'#include "@VMODROOT/vlib/x/multiwindow/appkit_backend.m"')
}

fn test_appkit_prototype_header_is_c_only_and_shared_with_implementation() {
	header := multiwindow_source_file('appkit_backend_helpers.h')
	implementation := multiwindow_source_file('appkit_backend.m')

	assert header.contains('int v_multiwindow_appkit_is_main_thread(void);')
	assert header.contains('int v_multiwindow_appkit_create_window(void *device_ptr, const char *title')
	assert header.contains('int v_multiwindow_appkit_resize_window(void *state_ptr, int width, int height')
	assert header.contains('int v_multiwindow_appkit_begin_frame(void *state_ptr, void *device_ptr')
	assert header.contains('void v_multiwindow_appkit_end_frame(void *state_ptr);')
	assert header.contains('void v_multiwindow_appkit_abort_frame(void *state_ptr);')
	assert !header.contains('@interface')
	assert !header.contains('@implementation')
	assert !header.contains('VMultiwindowAppKitWindowState')
	assert !header.contains('NSWindow')

	assert implementation.contains('#include "appkit_backend_helpers.h"')
	assert_source_order(implementation, '#include <stdint.h>',
		'#include "appkit_backend_helpers.h"')
	assert_source_order(implementation, '#include "appkit_backend_helpers.h"',
		'@interface VMultiwindowAppKitWindowState')
}

fn test_appkit_sharedlive_dylib_does_not_export_objc_classes_when_feasible() {
	$if macos {
		nm_path := os.find_abs_path_of_executable('nm') or {
			eprintln('skip appkit sharedlive objc export check: nm is unavailable')
			return
		}
		test_root := os.join_path(os.vtmp_dir(), 'x_multiwindow_appkit_sharedlive_${os.getpid()}')
		os.rmdir_all(test_root) or {}
		os.mkdir_all(test_root)!
		defer {
			os.rmdir_all(test_root) or {}
		}
		source_path := os.join_path(test_root, 'appkit_sharedlive_probe.v')
		dylib_path := os.join_path(test_root, 'appkit_sharedlive_probe.dylib')
		source := 'module main

import x.multiwindow

pub fn appkit_sharedlive_probe() {
	caps := multiwindow.capabilities_for_backend(.appkit) or { panic(err) }
	_ = caps
}
'
		os.write_file(source_path, source)!
		cmd := '${os.quoted_path(@VEXE)} -nocolor -cc clang -sharedlive -shared -o ${os.quoted_path(dylib_path)} ${os.quoted_path(source_path)}'
		build_res := os.execute(cmd)
		assert build_res.exit_code == 0, 'appkit sharedlive build failed
command: ${cmd}
exit_code: ${build_res.exit_code}
output:
${build_res.output}'

		nm_res := os.execute('${os.quoted_path(nm_path)} -gjU ${os.quoted_path(dylib_path)}')
		assert nm_res.exit_code == 0, nm_res.output
		assert !nm_res.output.contains(r'_OBJC_CLASS_$_VMultiwindowAppKitWindowState')
		assert !nm_res.output.contains(r'_OBJC_METACLASS_$_VMultiwindowAppKitWindowState')
	} $else {
		return
	}
}

fn test_appkit_initial_content_rect_is_clamped_to_min_size_source_guard() {
	source := multiwindow_source_file('appkit_backend.m')
	assert source.contains('static int v_multiwindow_appkit_max_int(int value, int minimum)')
	assert source.contains('int content_width = v_multiwindow_appkit_max_int(width, min_width);')
	assert source.contains('int content_height = v_multiwindow_appkit_max_int(height, min_height);')
	assert source.contains('NSRect rect = NSMakeRect(0, 0, (CGFloat)content_width, (CGFloat)content_height);')
	assert_source_order(source,
		'int content_width = v_multiwindow_appkit_max_int(width, min_width);',
		'NSWindow *window = [[NSWindow alloc] initWithContentRect:rect')
}

fn test_appkit_create_destroy_on_darwin() {
	$if darwin {
		mut app := new_app(backend: .appkit)!
		win := app.create_window(
			title:      'V x.multiwindow AppKit lifecycle'
			width:      96
			height:     64
			min_width:  160
			min_height: 120
			visible:    false
		)!
		assert app.window_exists(win)
		info := app.window_info(win)!
		assert info.width >= 160
		assert info.height >= 120
		app.destroy_window(win)!
		assert !app.window_exists(win)
		app.stop()!
	}
}

fn test_appkit_title_and_resize_on_darwin() {
	$if darwin {
		mut app := new_app(backend: .appkit)!
		win := app.create_window(
			title:   'V x.multiwindow AppKit initial'
			width:   96
			height:  64
			visible: false
		)!
		app.set_window_title(win, 'V x.multiwindow AppKit updated')!
		app.resize_window(win, 128, 72)!
		info := app.window_info(win)!
		assert info.title == 'V x.multiwindow AppKit updated'
		assert info.width == 128
		assert info.height == 72
		app.stop()!
	}
}

fn test_appkit_render_begin_abort_and_end_on_darwin_when_metal_is_available() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if darwin {
			$if darwin_sokol_glcore33 ? {
				capabilities_for_backend_with_renderer(.appkit, true) or {
					assert err.msg() == err_renderer_unsupported
					return
				}
				assert false, 'appkit render smoke succeeded with darwin_sokol_glcore33'
				return
			}
			render_caps := capabilities_for_backend_with_renderer(.appkit, true) or {
				if err.msg() == err_appkit_metal_device_failed {
					eprintln('skip appkit render smoke: Metal device is unavailable')
					return
				}
				assert false, 'unexpected appkit render capability error: ${err.msg()}'
				return
			}
			assert render_caps.backend == .appkit
			assert render_caps.native
			assert render_caps.explicit_swapchain
			assert render_caps.metal

			mut app := new_app(backend: .appkit, require_renderer: true)!
			win := app.create_window(
				title:  'V x.multiwindow AppKit Metal render'
				width:  128
				height: 96
			)!
			app.poll_events()!
			env := app.render_environment(win)!
			assert env.defaults.color_format == gfx.PixelFormat.bgra8
			assert env.defaults.depth_format == gfx.PixelFormat.depth_stencil
			assert env.defaults.sample_count == 1
			assert env.metal.device != unsafe { nil }

			aborted_frame := app.begin_render(win)!
			assert aborted_frame.window_id == win
			assert aborted_frame.swapchain.width > 0
			assert aborted_frame.swapchain.height > 0
			assert aborted_frame.swapchain.color_format == gfx.PixelFormat.bgra8
			assert aborted_frame.swapchain.depth_format == gfx.PixelFormat.depth_stencil
			assert aborted_frame.swapchain.metal.current_drawable != unsafe { nil }
			assert aborted_frame.swapchain.metal.depth_stencil_texture != unsafe { nil }
			app.abort_render(aborted_frame)!

			frame := app.begin_render(win)!
			assert frame.window_id == win
			assert frame.swapchain.width > 0
			assert frame.swapchain.height > 0
			assert frame.swapchain.color_format == gfx.PixelFormat.bgra8
			assert frame.swapchain.depth_format == gfx.PixelFormat.depth_stencil
			assert frame.swapchain.metal.current_drawable != unsafe { nil }
			assert frame.swapchain.metal.depth_stencil_texture != unsafe { nil }
			app.end_render(frame)!
			app.stop()!
		}
	} $else {
		return
	}
}

fn test_x11_capabilities_for_backend_do_not_require_display_on_linux() {
	$if linux {
		$if !x_multiwindow_x11 ? {
			capabilities_for_backend(.x11) or {
				assert err.msg() == err_backend_unsupported
				return
			}
			assert false, 'x11 capabilities succeeded without -d x_multiwindow_x11'
			return
		}
		caps := capabilities_for_backend(.x11)!
		assert caps.backend == .x11
		assert !caps.mock
		assert caps.native
		assert caps.multi_window
		assert caps.owner_queue
		assert caps.x11
		assert !caps.explicit_swapchain
	} $else {
		capabilities_for_backend(.x11) or {
			assert err.msg() == err_backend_unsupported
			return
		}
		assert false, 'x11 capabilities succeeded on an unsupported OS'
	}
}

fn test_x11_render_smoke_when_display_is_available() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if linux {
			$if !x_multiwindow_x11 ? {
				new_app(backend: .x11, require_renderer: true) or {
					assert err.msg() == err_backend_unsupported
					return
				}
				assert false, 'x11 render app creation succeeded without -d x_multiwindow_x11'
				return
			}
			if os.getenv('DISPLAY') == '' {
				eprintln('skip x11 render smoke: DISPLAY is not set')
				return
			}
			mut app := new_app(backend: .x11, require_renderer: true) or {
				if err.msg() == err_x11_open_display_failed {
					eprintln('skip x11 render smoke: DISPLAY could not be opened')
					return
				}
				panic(err.msg())
			}
			caps := app.capabilities()
			assert caps.explicit_swapchain
			assert caps.gl
			win := app.create_window(title: 'V x.multiwindow X11 render', width: 160, height: 96)!
			env := app.render_environment(win)!
			assert env.defaults.color_format == gfx.PixelFormat.rgba8
			assert env.defaults.depth_format == gfx.PixelFormat.depth_stencil
			assert env.defaults.sample_count == 1
			frame := app.begin_render(win)!
			assert frame.window_id == win
			assert frame.swapchain.width == 160
			assert frame.swapchain.height == 96
			assert frame.swapchain.sample_count == 1
			assert frame.swapchain.color_format == gfx.PixelFormat.rgba8
			assert frame.swapchain.depth_format == gfx.PixelFormat.depth_stencil
			assert frame.swapchain.gl.framebuffer == 0
			app.end_render(frame)!
			app.stop()!
		} $else {
			new_app(backend: .x11, require_renderer: true) or {
				assert err.msg() == err_backend_unsupported
				return
			}
			assert false, 'x11 render app creation succeeded on an unsupported OS'
		}
	} $else {
		return
	}
}

fn test_x11_runtime_create_destroy_when_display_is_available() {
	$if linux {
		$if !x_multiwindow_x11 ? {
			new_app(backend: .x11) or {
				assert err.msg() == err_backend_unsupported
				return
			}
			assert false, 'x11 app creation succeeded without -d x_multiwindow_x11'
			return
		}
		if os.getenv('DISPLAY') == '' {
			eprintln('skip x11 runtime test: DISPLAY is not set')
			return
		}
		mut app := new_app(backend: .x11) or {
			if err.msg() == err_x11_open_display_failed {
				eprintln('skip x11 runtime test: DISPLAY could not be opened')
				return
			}
			panic(err.msg())
		}
		main := app.create_window(title: 'V x.multiwindow X11 main', width: 320, height: 200)!
		child := app.create_window(title: 'V x.multiwindow X11 child', width: 240, height: 160)!
		assert app.window_exists(main)
		assert app.window_exists(child)
		app.set_window_title(main, 'V x.multiwindow X11 updated')!
		app.resize_window(main, 300, 180)!
		main_info := app.window_info(main)!
		assert main_info.title == 'V x.multiwindow X11 updated'
		assert main_info.width > 0
		assert main_info.height > 0
		if os.getenv('V_MULTIWINDOW_X11_EXPECT_EXACT_RESIZE') == '1' {
			assert main_info.width == 300
			assert main_info.height == 180
		}
		min_sized := app.create_window(
			title:      'V x.multiwindow X11 min sized'
			width:      100
			height:     80
			min_width:  220
			min_height: 140
			visible:    false
		)!
		min_sized_info := app.window_info(min_sized)!
		assert min_sized_info.width >= 220
		assert min_sized_info.height >= 140
		fixed := app.create_window(
			title:     'V x.multiwindow X11 fixed'
			width:     220
			height:    140
			resizable: false
			visible:   false
		)!
		mut fixed_resize_rejected := false
		app.resize_window(fixed, 300, 180) or {
			assert err.msg() == err_capability_unsupported
			fixed_info := app.window_info(fixed)!
			assert fixed_info.width == 220
			assert fixed_info.height == 140
			fixed_resize_rejected = true
		}
		assert fixed_resize_rejected
		assert app.poll_events()! >= 0
		app.destroy_window(fixed)!
		app.destroy_window(min_sized)!
		app.destroy_window(child)!
		assert app.window_exists(main)
		assert !app.window_exists(child)
		app.stop()!
		assert !app.window_exists(main)
	} $else {
		new_app(backend: .x11) or {
			assert err.msg() == err_backend_unsupported
			return
		}
		assert false, 'x11 app creation succeeded on an unsupported OS'
	}
}

fn test_x11_native_handle_aliases_follow_xlib_unsigned_long_source_guard() {
	source := multiwindow_source_file('x11_backend.c.v')
	assert source.contains('$if linux && x_multiwindow_x11 ? {')
	assert source.contains('$if x32 {')
	assert source.contains('type X11NativeULong = u32')
	assert source.contains('type X11NativeULong = u64')
	assert source.contains('type X11NativeAtom = X11NativeULong')
	assert source.contains('type X11NativeColormap = X11NativeULong')
	assert source.contains('type X11NativeWindow = X11NativeULong')
	assert source.contains('l [5]X11NativeLong')
	assert source.contains('serial       X11NativeULong')
	assert source.contains('event_mask X11NativeLong')
	assert !source.contains('type X11NativeWindow = u64')
	assert !source.contains('type X11NativeAtom = u64')
	assert !source.contains('l [5]i64')
}

fn test_x11_backend_native_deps_are_flag_gated_source_guard() {
	source := multiwindow_source_file('x11_backend.c.v')
	assert source.contains('$if linux && x_multiwindow_x11 ? {\n\t#flag linux -lX11')
	assert source.contains('#flag linux -lEGL')
	assert source.contains('#flag linux -lGL')
	assert source.contains('$if gg_multiwindow ? || x_multiwindow_render ? {\n\timport sokol.gfx')
	assert_source_order(source, '$if linux && x_multiwindow_x11 ? {\n\t#flag linux -lX11',
		'#include <X11/Xlib.h>')
}

fn test_x11_config_hints_are_applied_before_mapping() {
	x11_backend_source := multiwindow_source_file('x11_backend.c.v')
	x11_helper_source := multiwindow_source_file('x11_egl_backend_helpers.h')
	assert x11_backend_source.contains('v_multiwindow_x11_apply_config_hints')
	assert x11_backend_source.contains('return error(err_capability_unsupported)')
	assert x11_backend_source.contains('C.XSync(backend.display, 0)')
	assert x11_backend_source.contains('C.v_multiwindow_x11_get_window_size')
	assert_source_order_after_marker(x11_backend_source,
		'fn (mut backend X11Backend) resize_window', 'C.XResizeWindow',
		'C.XSync(backend.display, 0)')
	assert_source_order_after_marker(x11_backend_source,
		'fn (mut backend X11Backend) resize_window', 'C.XSync(backend.display, 0)',
		'C.v_multiwindow_x11_get_window_size')
	assert_source_order(x11_backend_source, 'if C.v_multiwindow_x11_apply_config_hints',
		'C.XMapWindow(backend.display, window)')
	assert x11_helper_source.contains('XSizeHints')
	assert x11_helper_source.contains('XWindowAttributes')
	assert x11_helper_source.contains('XGetWindowAttributes')
	assert x11_helper_source.contains('XSetWMNormalHints')
	assert x11_helper_source.contains('PMinSize')
	assert x11_helper_source.contains('PMaxSize')
	assert x11_helper_source.contains('_MOTIF_WM_HINTS')
	assert x11_helper_source.contains('MWM_HINTS_DECORATIONS')
	assert x11_helper_source.contains('_NET_WM_STATE')
	assert x11_helper_source.contains('_NET_WM_STATE_FULLSCREEN')
}

fn test_wayland_capabilities_for_backend_do_not_require_display_on_linux() {
	$if linux {
		$if sokol_wayland ? {
			caps := capabilities_for_backend(.wayland)!
			assert caps.backend == .wayland
			assert !caps.mock
			assert caps.native
			assert caps.multi_window
			assert caps.owner_queue
			assert caps.wayland
			assert !caps.x11
			assert !caps.explicit_swapchain
		} $else {
			capabilities_for_backend(.wayland) or {
				assert err.msg() == err_backend_unsupported
				return
			}
			assert false, 'wayland capabilities succeeded without -d sokol_wayland'
		}
	} $else {
		capabilities_for_backend(.wayland) or {
			assert err.msg() == err_backend_unsupported
			return
		}
		assert false, 'wayland capabilities succeeded on an unsupported OS'
	}
}

fn test_wayland_hidden_windows_are_rejected_before_mapping() {
	wayland_source := multiwindow_source_file('wayland_backend.c.v')
	assert wayland_source.contains('if !config.visible')
	assert wayland_source.contains('return error(err_capability_unsupported)')
	assert_source_order_after_marker(wayland_source,
		'fn (mut backend WaylandBackend) create_window', 'if !config.visible',
		'surface := C.wl_compositor_create_surface')
	assert_source_order_after_marker(wayland_source,
		'fn (mut backend WaylandBackend) create_window', 'if !config.visible',
		'C.wl_surface_commit(surface)')
	assert_source_order_after_marker(wayland_source,
		'fn (mut backend WaylandBackend) create_window', 'if !config.visible',
		'C.wl_display_roundtrip(display)')
}

fn test_wayland_initial_size_is_clamped_before_mapping() {
	wayland_source := multiwindow_source_file('wayland_backend.c.v')
	assert wayland_source.contains('actual_size := window_size_for_config(config, config.width, config.height)')
	assert wayland_source.contains('width:        actual_size.width')
	assert wayland_source.contains('height:       actual_size.height')
	assert wayland_source.contains('min_width:    record_min_width')
	assert wayland_source.contains('min_height:   record_min_height')
	assert wayland_source.contains('width = window_extent_for_minimum(width, record.min_width)')
	assert wayland_source.contains('height = window_extent_for_minimum(height, record.min_height)')
	assert wayland_source.contains('C.v_multiwindow_wayland_xdg_toplevel_set_min_size(xdg_toplevel, i32(actual_size.width),')
	assert wayland_source.contains('i32(actual_size.height))')
	assert_source_order_after_marker(wayland_source,
		'fn (mut backend WaylandBackend) create_window',
		'actual_size := window_size_for_config(config, config.width, config.height)',
		'mut record := &WaylandWindowRecord')
	assert_source_order_after_marker(wayland_source,
		'fn (mut backend WaylandBackend) create_window', 'width:        actual_size.width',
		'C.wl_surface_commit(surface)')
}

fn test_default_gg_multiwindow_build_does_not_link_wayland_without_sokol_wayland() {
	$if linux {
		$if sokol_wayland ? {
			return
		}
		if !multiwindow_linux_gg_graphics_headers_available() {
			multiwindow_skip_linux_gg_graphics_probe('no_wayland_deps')
			return
		}
		output := multiwindow_dump_c_flags('no_wayland_deps', '-d gg_multiwindow')
		assert !output.contains('-lwayland-client')
		assert !output.contains('-lwayland-egl')
		assert !output.contains('wayland_xdg_shell_private.c')
	} $else {
		return
	}
}

fn test_default_x_multiwindow_mock_build_does_not_link_x11_egl_or_gl() {
	$if linux {
		output := multiwindow_dump_c_flags('no_x11_deps', '')
		assert !output.contains('-lX11')
		assert !output.contains('-lEGL')
		assert !output.contains('-lGL')
		assert !output.contains('x11_egl_backend_helpers.h')
	} $else {
		return
	}
}

fn test_x11_enabled_build_links_x11_egl_and_gl() {
	$if linux && x_multiwindow_x11 ? {
		output := multiwindow_dump_c_flags('with_x11_deps', '-d x_multiwindow_x11')
		assert output.contains('-lX11')
		assert output.contains('-lEGL')
		assert output.contains('-lGL')
	} $else {
		return
	}
}

fn test_sokol_wayland_build_links_wayland_when_flag_is_active() {
	$if linux && sokol_wayland ? {
		output := multiwindow_dump_c_flags('with_wayland_deps',
			'-d gg_multiwindow -d sokol_wayland')
		assert output.contains('-lwayland-client')
		assert output.contains('-lwayland-egl')
		assert output.contains('wayland_xdg_shell_private.c')
	} $else {
		return
	}
}

fn test_gg_import_only_windows_build_keeps_win32_callback_record_declaration() {
	c_source := multiwindow_emit_windows_gg_import_c()
	assert c_source.contains('struct x__multiwindow__Win32WindowRecord {')
	assert_source_order(c_source, 'struct x__multiwindow__Win32WindowRecord {',
		'VV_LOC void x__multiwindow__win32_window_close_requested(voidptr data)')
}

fn test_wayland_render_smoke_when_display_is_available() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		$if linux {
			$if !sokol_wayland ? {
				new_app(backend: .wayland, require_renderer: true) or {
					assert err.msg() == err_backend_unsupported
					return
				}
				assert false, 'wayland render app creation succeeded without -d sokol_wayland'
				return
			}
			if os.getenv('WAYLAND_DISPLAY') == '' {
				eprintln('skip wayland render smoke: WAYLAND_DISPLAY is not set')
				return
			}
			mut app := new_app(backend: .wayland, require_renderer: true) or {
				if err.msg() == err_wayland_connect_failed {
					eprintln('skip wayland render smoke: Wayland display connection failed')
					return
				}
				panic(err.msg())
			}
			caps := app.capabilities()
			assert caps.explicit_swapchain
			assert caps.gl
			win :=
				app.create_window(title: 'V x.multiwindow Wayland render', width: 160, height: 96)!
			env := app.render_environment(win)!
			assert env.defaults.color_format == gfx.PixelFormat.rgba8
			assert env.defaults.depth_format == gfx.PixelFormat.depth_stencil
			assert env.defaults.sample_count == 1
			frame := app.begin_render(win)!
			assert frame.window_id == win
			assert frame.swapchain.width > 0
			assert frame.swapchain.height > 0
			assert frame.swapchain.sample_count == 1
			assert frame.swapchain.color_format == gfx.PixelFormat.rgba8
			assert frame.swapchain.depth_format == gfx.PixelFormat.depth_stencil
			assert frame.swapchain.gl.framebuffer == 0
			app.end_render(frame)!
			app.stop()!
		} $else {
			new_app(backend: .wayland, require_renderer: true) or {
				assert err.msg() == err_backend_unsupported
				return
			}
			assert false, 'wayland render app creation succeeded on an unsupported OS'
		}
	} $else {
		return
	}
}

fn test_wayland_runtime_create_destroy_when_display_is_available() {
	$if linux {
		$if !sokol_wayland ? {
			new_app(backend: .wayland) or {
				assert err.msg() == err_backend_unsupported
				return
			}
			assert false, 'wayland app creation succeeded without -d sokol_wayland'
			return
		}
		if os.getenv('WAYLAND_DISPLAY') == '' {
			eprintln('skip wayland runtime test: WAYLAND_DISPLAY is not set')
			return
		}
		mut app := new_app(backend: .wayland) or {
			if err.msg() == err_wayland_connect_failed {
				eprintln('skip wayland runtime test: Wayland display connection failed')
				return
			}
			panic(err.msg())
		}
		main := app.create_window(title: 'V x.multiwindow Wayland main', width: 320, height: 200)!
		child := app.create_window(title: 'V x.multiwindow Wayland child', width: 240, height: 160)!
		min_sized := app.create_window(
			title:      'V x.multiwindow Wayland min sized'
			width:      100
			height:     80
			min_width:  220
			min_height: 140
		)!
		assert app.window_exists(main)
		assert app.window_exists(child)
		min_sized_info := app.window_info(min_sized)!
		assert min_sized_info.width >= 220
		assert min_sized_info.height >= 140
		app.set_window_title(main, 'V x.multiwindow Wayland updated')!
		main_info := app.window_info(main)!
		assert main_info.title == 'V x.multiwindow Wayland updated'
		mut rejected_resize := false
		app.resize_window(main, 300, 180) or {
			assert err.msg() == err_capability_unsupported
			rejected_resize = true
		}
		assert rejected_resize
		info_after_resize := app.window_info(main)!
		assert info_after_resize.width == 320
		assert info_after_resize.height == 200
		assert app.poll_events()! >= 0
		app.destroy_window(min_sized)!
		app.destroy_window(child)!
		assert app.window_exists(main)
		assert !app.window_exists(min_sized)
		assert !app.window_exists(child)
		app.stop()!
		assert !app.window_exists(main)
	} $else {
		new_app(backend: .wayland) or {
			assert err.msg() == err_backend_unsupported
			return
		}
		assert false, 'wayland app creation succeeded on an unsupported OS'
	}
}

fn test_registry_mutation_from_foreign_thread_is_rejected() {
	mut app := new_app()!
	result := chan string{cap: 1}
	t := spawn fn [mut app, result] () {
		app.create_window(title: 'Foreign') or {
			result <- err.msg()
			return
		}
		result <- 'accepted'
	}()
	msg := wait_for_string(result, 'foreign create_window did not report a result')
	t.wait()
	assert msg == err_owner_thread_required
	app.stop()!
}

fn test_window_info_from_foreign_thread_is_rejected() {
	mut app := new_app()!
	win := app.create_window(title: 'Owner')!
	owner_info := app.window_info(win)!
	assert owner_info.title == 'Owner'
	result := chan string{cap: 1}
	t := spawn fn [mut app, win, result] () {
		app.window_info(win) or {
			result <- err.msg()
			return
		}
		result <- 'accepted'
	}()
	msg := wait_for_string(result, 'foreign window_info did not report a result')
	t.wait()
	assert msg == err_owner_thread_required
	assert app.status() == .running
	assert app.window_exists(win)
	assert app.window_status(win)! == .alive
	app.stop()!
}

fn test_window_enumeration_from_foreign_thread_is_rejected() {
	mut app := new_app()!
	_ := app.create_window(title: 'Owner')!
	result := chan string{cap: 1}
	t := spawn fn [mut app, result] () {
		app.window_ids() or {
			result <- err.msg()
			return
		}
		result <- 'accepted'
	}()
	msg := wait_for_string(result, 'foreign window_ids did not report a result')
	t.wait()
	assert msg == err_owner_thread_required
	app.stop()!
}

fn test_lifecycle_and_registry_reads_work_from_foreign_thread() {
	mut app := new_app()!
	main := app.create_window(title: 'Main')!
	child := app.create_window(title: 'Child')!
	result := chan string{cap: 1}

	t := spawn fn [mut app, main, child, result] () {
		if app.status() != .running {
			result <- 'bad status'
			return
		}
		if !app.window_exists(main) {
			result <- 'missing main'
			return
		}
		status := app.window_status(child) or {
			result <- err.msg()
			return
		}
		if status != .alive {
			result <- 'bad child status'
			return
		}
		result <- 'ok'
	}()

	msg := wait_for_string(result, 'foreign reads did not report a result')
	t.wait()
	assert msg == 'ok'
	app.stop()!
}

fn multiwindow_linux_gg_graphics_headers_available() bool {
	$if linux {
		for header in [
			'X11/XKBlib.h',
			'X11/Xatom.h',
			'X11/Xcursor/Xcursor.h',
			'X11/Xlib.h',
			'X11/Xmd.h',
			'X11/Xresource.h',
			'X11/Xutil.h',
			'X11/cursorfont.h',
			'X11/extensions/XInput2.h',
			'GL/gl.h',
		] {
			if !multiwindow_c_header_available(header) {
				return false
			}
		}
	}
	return true
}

fn multiwindow_c_header_available(header string) bool {
	header_label := header.replace('/', '_').replace('.', '_')
	source_path := os.join_path(os.temp_dir(),
		'x_multiwindow_header_${os.getpid()}_${header_label}.c')
	os.write_file(source_path, '#include <${header}>\n') or { return false }
	defer {
		os.rm(source_path) or {}
	}
	cc := if os.getenv('CC') == '' { 'cc' } else { os.getenv('CC') }
	result := os.execute('${cc} -fsyntax-only ${os.quoted_path(source_path)}')
	return result.exit_code == 0
}

fn multiwindow_skip_linux_gg_graphics_probe(label string) {
	eprintln('skip ${label}: gg multiwindow link probe requires host X11/GL headers')
}

fn multiwindow_dump_c_flags(label string, flags string) string {
	vlib_dir := os.dir(os.dir(@DIR))
	unique := 'x_multiwindow_${label}_${os.getpid()}_${time.now().unix_nano()}'
	source_path := os.join_path(os.temp_dir(), '${unique}.v')
	mut bin_path := os.join_path(os.temp_dir(), '${unique}_bin')
	$if windows {
		bin_path += '.exe'
	}
	source := 'import x.multiwindow

fn main() {
	caps := multiwindow.capabilities_for_backend(.mock)!
	assert caps.mock
}
'
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(bin_path) or {}
	}

	cmd := '${os.quoted_path(@VEXE)} -dump-c-flags - ${flags} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(bin_path)} ${os.quoted_path(source_path)}'
	result := os.execute(cmd)
	assert result.exit_code == 0, 'dump-c-flags ${label} failed
command: ${cmd}
exit_code: ${result.exit_code}
output:
${result.output}'

	return result.output
}

fn multiwindow_emit_windows_gg_import_c() string {
	vlib_dir := os.dir(os.dir(@DIR))
	unique := 'x_multiwindow_win32_record_${os.getpid()}_${time.now().unix_nano()}'
	source_path := os.join_path(os.temp_dir(), '${unique}.v')
	c_path := os.join_path(os.temp_dir(), '${unique}.c')
	source := 'import gg

fn main() {
	_ = gg.Color{}
}
'
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(c_path) or {}
	}

	cmd := '${os.quoted_path(@VEXE)} -os windows -d gg_multiwindow -d sokol_d3d11 -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(c_path)} ${os.quoted_path(source_path)}'
	result := os.execute(cmd)
	assert result.exit_code == 0, 'emit Windows gg import C failed
command: ${cmd}
exit_code: ${result.exit_code}
output:
${result.output}'

	return os.read_file(c_path) or { panic(err) }
}

fn multiwindow_source_file(name string) string {
	return os.read_file(os.join_path(@DIR, name)) or { panic(err) }
}

fn assert_source_order(source string, before string, after string) {
	before_index := source.index(before) or {
		assert false, 'source does not contain `${before}`'
		return
	}
	after_index := source.index(after) or {
		assert false, 'source does not contain `${after}`'
		return
	}
	assert before_index < after_index, '`${before}` should appear before `${after}`'
}

fn assert_source_order_after_marker(source string, marker string, before string, after string) {
	marker_index := source.index(marker) or {
		assert false, 'source does not contain marker `${marker}`'
		return
	}
	section := source[marker_index..]
	assert_source_order(section, before, after)
}

fn assert_win32_d3d11_present_status_error(status int, expected string) {
	win32_d3d11_check_present_status(status) or {
		assert err.msg() == expected
		return
	}
	assert false, 'D3D11 Present status ${status} did not fail with ${expected}'
}

fn assert_no_bool_signal(signal chan bool, message string) {
	select {
		_ := <-signal {
			assert false, message
		}
		20 * time.millisecond {}
	}
}

fn wait_for_string(signal chan string, message string) string {
	select {
		value := <-signal {
			return value
		}
		1 * time.second {
			assert false, message
		}
	}
	return ''
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
