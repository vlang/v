module multiwindow

import os
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

	app.window_status(first) or {
		assert err.msg() == err_stale_window
		app.stop()!
		return
	}
	assert false, 'window_status accepted a stale WindowId after slot reuse'
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

fn test_dropped_files_from_uri_list_decodes_local_file_urls() {
	payload := '# comment\r\nfile:///tmp/a%20b.txt\r\nfile://localhost/home/me/c%23d.txt\nhttp://ignored\nfile://remotehost/not-local\n'
	files := dropped_files_from_uri_list(payload)
	assert files == ['/tmp/a b.txt', '/home/me/c#d.txt']
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

fn test_interactive_move_resize_api_exists_and_mock_reports_unsupported() {
	mut app := new_app()!
	win := app.create_window(title: 'Interactive mock')!

	mut move_rejected := false
	app.begin_window_move(win) or {
		assert err.msg() == err_capability_unsupported
		move_rejected = true
	}
	assert move_rejected, 'mock backend accepted interactive move'

	edges := [
		WindowResizeEdge.top,
		.bottom,
		.left,
		.right,
		.top_left,
		.top_right,
		.bottom_left,
		.bottom_right,
	]
	for edge in edges {
		mut resize_rejected := false
		app.begin_window_resize(win, edge) or {
			assert err.msg() == err_capability_unsupported
			resize_rejected = true
		}
		assert resize_rejected, 'mock backend accepted interactive resize edge ${edge}'
	}

	fixed := app.create_window(title: 'Fixed', resizable: false)!
	mut fixed_resize_rejected := false
	app.begin_window_resize(fixed, .bottom_right) or {
		assert err.msg() == err_capability_unsupported
		fixed_resize_rejected = true
	}
	assert fixed_resize_rejected, 'non-resizable window accepted interactive resize'
	app.stop()!
}

fn test_cursor_shape_api_exists_and_mock_reports_unsupported() {
	mut app := new_app()!
	assert !app.capabilities().cursor_shapes
	win := app.create_window(title: 'Cursor mock')!
	mut cursor_rejected := false
	app.set_window_cursor(win, .pointer) or {
		assert err.msg() == err_capability_unsupported
		cursor_rejected = true
	}
	assert cursor_rejected, 'mock backend accepted cursor shape'
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

fn test_mock_input_events_are_routed_without_polluting_lifecycle_drain() {
	mut app := new_app()!
	win := app.create_window(title: 'Input route')!
	assert app.drain_events()!.len == 1

	app.enqueue_mock_input_for_test(InputEvent{
		kind:      .mouse_move
		window_id: win
		mouse_x:   12.5
		mouse_y:   34.5
	})!

	assert app.poll_events()! == 1
	assert app.drain_events()!.len == 0
	input_events := app.drain_input_events()!
	assert input_events.len == 1
	assert input_events[0].kind == .mouse_move
	assert input_events[0].window_id == win
	assert input_events[0].mouse_x == 12.5
	assert input_events[0].mouse_y == 34.5
	assert app.drain_input_events()!.len == 0
	app.stop()!
}

fn test_input_events_without_frame_count_are_stamped_per_poll() {
	mut app := new_app()!
	win := app.create_window(title: 'Input frame count')!
	assert app.drain_events()!.len == 1

	app.enqueue_mock_input_for_test(InputEvent{
		kind:      .mouse_move
		window_id: win
		mouse_x:   1
		mouse_y:   2
	})!
	assert app.poll_events()! == 1
	first := app.drain_input_events()!
	assert first.len == 1
	assert first[0].frame_count == 1

	app.enqueue_mock_input_for_test(InputEvent{
		kind:        .key_down
		window_id:   win
		frame_count: 99
		key_code:    65
	})!
	assert app.poll_events()! == 1
	second := app.drain_input_events()!
	assert second.len == 1
	assert second[0].frame_count == 99

	app.enqueue_mock_input_for_test(InputEvent{
		kind:      .mouse_up
		window_id: win
	})!
	assert app.poll_events()! == 1
	third := app.drain_input_events()!
	assert third.len == 1
	assert third[0].frame_count == 3
	app.stop()!
}

fn test_mock_input_roundtrip_covers_all_input_event_kinds() {
	event_kinds := all_input_event_kinds_for_test()
	assert_input_event_kind_list_matches_types_source(event_kinds)
	mut app := new_app()!
	win := app.create_window(title: 'Input kind coverage')!
	assert app.drain_events()!.len == 1

	mut expected_events := []InputEvent{cap: event_kinds.len}
	for i, kind in event_kinds {
		expected_events << input_event_for_kind_for_test(win, kind, u64(i + 1))
	}
	for event in expected_events {
		app.enqueue_mock_input_for_test(event)!
	}

	assert app.poll_events()! == expected_events.len
	actual_events := app.drain_input_events()!
	assert actual_events.len == expected_events.len
	for i, actual in actual_events {
		assert_input_event_roundtrip(actual, expected_events[i])
	}
	assert app.drain_events()!.len == 0
	assert app.drain_input_events()!.len == 0
	app.stop()!
}

fn test_input_event_default_mouse_button_is_invalid() {
	event := InputEvent{}
	assert event.mouse_button == input_event_invalid_mouse_button
	source := multiwindow_source_file('types.v')
	assert source.contains('const input_event_invalid_mouse_button = 256')
	assert source.contains('mouse_button       int = input_event_invalid_mouse_button')
}

fn test_specialized_input_drain_does_not_overtake_lifecycle_events() {
	mut app := new_app()!
	win := app.create_window(title: 'Lifecycle remains')!

	app.enqueue_mock_input_for_test(InputEvent{
		kind:      .mouse_move
		window_id: win
		mouse_x:   48
		mouse_y:   96
	})!

	assert app.poll_events()! == 1
	input_events := app.drain_input_events()!
	assert input_events.len == 0

	lifecycle_events := app.drain_events()!
	assert lifecycle_events.len == 1
	assert lifecycle_events[0].kind == .window_created
	assert lifecycle_events[0].window_id == win
	remaining_input := app.drain_input_events()!
	assert remaining_input.len == 1
	assert remaining_input[0].kind == .mouse_move
	assert remaining_input[0].window_id == win
	assert app.drain_events()!.len == 0
	assert app.drain_input_events()!.len == 0
	app.stop()!
}

fn test_mock_input_and_lifecycle_events_preserve_backend_order() {
	mut app := new_app()!
	win := app.create_window(title: 'Ordered input')!
	assert app.drain_events()!.len == 1

	app.enqueue_mock_input_for_test(InputEvent{
		kind:         .mouse_down
		window_id:    win
		mouse_button: 0
	})!
	app.enqueue_mock_close_requested_for_test(win)!
	app.enqueue_mock_input_for_test(InputEvent{
		kind:      .key_down
		window_id: win
		key_code:  65
	})!

	assert app.poll_events()! == 3
	queued_events := app.drain_queued_events()!
	assert queued_events.len == 3
	assert queued_events[0].kind == .input
	assert queued_events[0].input.kind == .mouse_down
	assert queued_events[1].kind == .lifecycle
	assert queued_events[1].lifecycle.kind == .window_close_requested
	assert queued_events[2].kind == .input
	assert queued_events[2].input.kind == .key_down
	app.stop()!
}

fn test_mock_stale_backend_input_event_is_filtered_by_app_poll_events() {
	mut app := new_app()!
	first := app.create_window(title: 'First input target')!
	assert app.drain_events()!.len == 1
	app.destroy_window(first)!
	assert app.drain_events()!.len == 1
	second := app.create_window(title: 'Reused input target')!
	assert second.slot == first.slot
	assert second.generation != first.generation
	assert app.drain_events()!.len == 1

	app.enqueue_mock_input_unchecked_for_test(InputEvent{
		kind:      .mouse_move
		window_id: first
		mouse_x:   99
		mouse_y:   88
	})!

	assert app.poll_events()! == 0
	assert app.drain_input_events()!.len == 0
	assert app.window_exists(second)
	app.stop()!
}

fn test_mock_resized_input_event_updates_window_state() {
	mut app := new_app()!
	win := app.create_window(title: 'Input resize', width: 320, height: 200)!
	assert app.drain_events()!.len == 1

	app.enqueue_mock_input_for_test(InputEvent{
		kind:               .resized
		window_id:          win
		window_width:       640
		window_height:      360
		framebuffer_width:  1280
		framebuffer_height: 720
	})!

	assert app.poll_events()! == 1
	info := app.window_info(win)!
	assert info.width == 640
	assert info.height == 360
	input_events := app.drain_input_events()!
	assert input_events.len == 1
	assert input_events[0].kind == .resized
	assert input_events[0].window_width == 640
	assert input_events[0].window_height == 360
	assert input_events[0].framebuffer_width == 1280
	assert input_events[0].framebuffer_height == 720
	app.stop()!
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
		queued_app.set_window_title(win, 'Destroyed queued window')!
	})!
	app.drain_pending(1) or {
		assert err.msg() == err_stale_window
		mut stop_rejected := false
		app.stop() or {
			assert err.msg() == '${err_render_terminal_aggregate}: ${err_stale_window}'
			stop_rejected = true
		}
		assert stop_rejected
		assert app.status() == .stopped
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

fn assert_ownerless_modal_rejected_without_side_effects(mut app App, visible bool) {
	windows_len := app.windows.len
	native_windows_len := app.backend.mock.windows.len
	service_windows_len := app.services.windows.len
	render_windows_len := app.render_runtime.windows.len
	events_len := app.events.len
	next_delivery_token := app.next_event_delivery_token

	app.create_window(
		title:   'ownerless modal'
		modal:   true
		visible: visible
	) or {
		assert err.msg() == err_owner_relation_invalid
		assert app.windows.len == windows_len
		assert app.backend.mock.windows.len == native_windows_len
		assert app.services.windows.len == service_windows_len
		assert app.render_runtime.windows.len == render_windows_len
		assert app.events.len == events_len
		assert app.next_event_delivery_token == next_delivery_token
		return
	}
	assert false, 'create_window accepted an ownerless modal window'
}

fn test_ownerless_modal_config_is_rejected_before_visible_or_hidden_allocation() {
	mut app := new_app()!
	assert_ownerless_modal_rejected_without_side_effects(mut app, true)
	assert_ownerless_modal_rejected_without_side_effects(mut app, false)
	app.stop()!
}

fn test_unknown_and_stale_window_ids_are_rejected() {
	mut app := new_app()!
	unknown := WindowId{
		app_instance: app.instance_id()
		slot:         42
		generation:   1
	}
	mut unknown_destroy_rejected := false
	app.destroy_window(unknown) or {
		assert err.msg() == err_window_not_found
		unknown_destroy_rejected = true
	}
	assert unknown_destroy_rejected

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
	assert caps_before.input_events
	assert caps_before.mouse_events
	assert caps_before.keyboard_events
	assert caps_before.text_events
	assert caps_before.focus_events
	assert caps_before.drop_events
	assert caps_before.touch_events
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
		app_instance: app.instance_id()
		slot:         99
		generation:   1
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
	assert caps.input_events
	assert caps.mouse_events
	assert caps.keyboard_events
	assert caps.text_events
	assert caps.focus_events
	assert caps.drop_events
	assert caps.touch_events
	assert caps.readback
}

fn test_mock_opaque_renderer_start_reports_renderer_unsupported() {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		mut app := new_app()!
		mut rejected_first_start := false
		app.start_renderer(RendererConfig{}) or {
			assert err.msg() == err_renderer_unsupported
			rejected_first_start = true
		}
		assert rejected_first_start
		assert app.render_runtime.renderer_terminal == ''
		assert app.render_bridge == unsafe { nil }

		mut rejected_second_start := false
		app.start_renderer(RendererConfig{}) or {
			assert err.msg() == err_renderer_unsupported
			rejected_second_start = true
		}
		assert rejected_second_start
		assert app.render_runtime.renderer_terminal == ''
		assert app.render_bridge == unsafe { nil }

		app.stop()!
		assert app.render_runtime.renderer_terminal == ''
		assert app.render_bridge == unsafe { nil }
		app.stop()!
		assert app.render_runtime.renderer_terminal == ''
		assert app.render_bridge == unsafe { nil }
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
				assert_win32_input_capabilities(caps)
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
					assert caps.readback
					assert_appkit_input_capabilities(caps)
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
	assert_x11_input_capabilities(caps)
}

fn assert_x11_lifecycle_capabilities(caps Capabilities) {
	assert caps.backend == .x11
	assert caps.x11
	assert !caps.wayland
	assert caps.native
	assert caps.multi_window
	assert caps.owner_queue
	assert !caps.explicit_swapchain
	assert_x11_input_capabilities(caps)
}

fn assert_native_input_capabilities_not_claimed(caps Capabilities) {
	assert !caps.input_events
	assert !caps.mouse_events
	assert !caps.keyboard_events
	assert !caps.text_events
	assert !caps.focus_events
	assert !caps.drop_events
	assert !caps.touch_events
}

fn assert_x11_input_capabilities(caps Capabilities) {
	assert caps.input_events
	assert caps.mouse_events
	assert caps.keyboard_events
	assert caps.text_events
	assert caps.focus_events
	assert caps.drop_events
	assert !caps.touch_events
	assert caps.cursor_shapes
}

fn assert_wayland_input_capabilities(caps Capabilities) {
	assert caps.input_events
	assert caps.mouse_events
	assert caps.keyboard_events
	assert caps.text_events
	assert caps.focus_events
	assert caps.drop_events
	assert caps.touch_events
	assert !caps.cursor_shapes
}

fn assert_win32_input_capabilities(caps Capabilities) {
	assert caps.input_events
	assert caps.mouse_events
	assert caps.keyboard_events
	assert caps.text_events
	assert caps.focus_events
	assert caps.drop_events
	assert caps.touch_events
	assert caps.cursor_shapes
}

fn assert_appkit_input_capabilities(caps Capabilities) {
	assert caps.input_events
	assert caps.mouse_events
	assert caps.keyboard_events
	assert caps.text_events
	assert caps.focus_events
	assert caps.drop_events
	assert caps.touch_events
	assert caps.cursor_shapes
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
			assert_wayland_input_capabilities(caps)

			helper_caps := capabilities_for_backend(.wayland) or {
				assert err.msg() == err_wayland_connect_failed
				return
			}
			assert helper_caps.backend == .wayland
			assert !helper_caps.explicit_swapchain
			assert !helper_caps.gl
			assert_wayland_input_capabilities(helper_caps)
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
				assert_wayland_input_capabilities(caps)
				return
			}
		}
		if os.getenv('DISPLAY') != '' {
			$if x_multiwindow_x11 ? {
				assert caps.backend == .x11
				assert caps.x11
				assert caps.native
				assert_x11_input_capabilities(caps)
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
			assert_win32_input_capabilities(caps)
		} $else {
			$if darwin {
				assert caps.backend == .appkit
				assert caps.native
				assert_appkit_input_capabilities(caps)
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
		assert_win32_input_capabilities(caps)
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
			assert_win32_input_capabilities(caps)
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

$if gg_multiwindow ? || x_multiwindow_render ? {
	$if windows && sokol_d3d11 ? {
		fn test_win32_d3d11_present_hresult_mapping_treats_occluded_as_nonfatal() {
			assert_win32_d3d11_present_disposition(0, .ok)
			assert_win32_d3d11_present_disposition(i64(u32(0x087a0001)), .not_presented)
			assert_win32_d3d11_present_disposition(i64(u32(0x887a0005)), .renderer_lost)
			assert_win32_d3d11_present_disposition(i64(u32(0x887a0007)), .renderer_lost)
			assert_win32_d3d11_present_disposition(i64(u32(0x80004005)), .operation_failed)
			assert_win32_d3d11_present_disposition(i64(u32(0x80070057)), .operation_failed)
		}
	}
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

fn test_win32_input_events_are_queued_and_capability_scoped_source_guard() {
	backend_source := multiwindow_source_file('backend.v')
	win32_backend_source := multiwindow_source_file('win32_backend.c.v')
	win32_helper_source := multiwindow_source_file('win32_backend_helpers.h')
	backend_poll_body :=
		backend_source.all_after('fn (mut backend Backend) poll_queued_events() ![]QueuedEvent').all_before('fn (mut backend Backend) event_sequence_terminal_error')
	compact_backend_poll_body :=
		backend_poll_body.replace(' ', '').replace('\t', '').replace('\n', '')
	compact_win32_backend_source :=
		win32_backend_source.replace(' ', '').replace('\t', '').replace('\n', '')

	assert compact_backend_poll_body.contains('.win32{backend.win32.poll_queued_events()!}')
	assert win32_backend_source.contains('struct Win32NativeQueuedEvent')
	assert win32_backend_source.contains('queued_events')
	assert win32_backend_source.contains('[]Win32NativeQueuedEvent')
	assert win32_backend_source.contains('fn (mut backend Win32Backend) poll_queued_events() ![]QueuedEvent')
	assert win32_backend_source.contains('win32_sort_native_events(mut native_events)')
	assert win32_backend_source.contains('record.enqueue_native_event(sequence, queued_input_event')
	assert win32_backend_source.contains('record.enqueue_char_event(sequence, char_code, modifiers)')
	assert win32_backend_source.contains('pending_high_surrogate')
	assert win32_backend_source.contains('suppress_control_char')
	assert !win32_backend_source.contains('win32_scroll_delta')
	assert !win32_backend_source.contains('...input')
	assert win32_backend_source.contains('scroll_x:           -f32(wheel_delta_x) / f32(30.0)')
	assert win32_backend_source.contains('scroll_y:           f32(wheel_delta_y) / f32(30.0)')
	assert win32_backend_source.contains('fn win32_control_char_for_key(key_code int) u32')
	assert win32_backend_source.contains('257, 335 { u32(13) }')
	assert win32_backend_source.contains('261 { u32(127) }')
	assert win32_backend_source.contains('input_events:       true')
	assert win32_backend_source.contains('mouse_events:       true')
	assert win32_backend_source.contains('keyboard_events:    true')
	assert win32_backend_source.contains('text_events:        true')
	assert win32_backend_source.contains('focus_events:       true')
	assert win32_backend_source.contains('drop_events:        true')
	assert win32_backend_source.contains('touch_events:       true')
	assert compact_win32_backend_source.contains('pending_dropped_files[]string')
	assert compact_win32_backend_source.contains('dropped_files:record.pending_dropped_files.clone()')
	assert win32_backend_source.contains('.files_dropped')
	assert win32_backend_source.contains('InputEventKind.clipboard_pasted')
	assert win32_backend_source.contains('InputEventKind.touches_began')
	assert win32_backend_source.contains('InputEventKind.touches_moved')
	assert win32_backend_source.contains('InputEventKind.touches_ended')
	assert !win32_backend_source.contains('InputEventKind.touches_cancelled')
	assert win32_backend_source.contains('fn win32_window_drop_begin')
	assert win32_backend_source.contains('fn win32_window_drop_file')
	assert win32_backend_source.contains('fn win32_window_drop_end')
	assert win32_backend_source.contains('fn win32_window_touch_event')
	assert win32_backend_source.contains('clone_native_path(path)')
	assert !win32_backend_source.contains('tos_clone(&u8(path))')
	assert !win32_backend_source.contains('cstring_to_vstring(path)')
	assert compact_win32_backend_source.contains('iconifiedbool')
	assert win32_backend_source.contains('12 { InputEventKind.iconified }')
	assert win32_backend_source.contains('13 { InputEventKind.restored }')
	assert win32_backend_source.contains('if record.iconified')
	assert win32_backend_source.contains('if !record.iconified')
	win32_mouse_button_body :=
		win32_backend_source.all_after('.mouse_down, .mouse_up {').all_before('.mouse_move {')
	assert win32_mouse_button_body.contains('record.update_mouse_position(mouse_x, mouse_y, false)')
	assert !win32_mouse_button_body.contains('record.update_mouse_position(mouse_x, mouse_y, true)')
	assert win32_mouse_button_body.contains('mouse_dx:           record.mouse_dx')
	assert win32_mouse_button_body.contains('mouse_dy:           record.mouse_dy')
	win32_clipboard_pasted_body :=
		win32_backend_source.all_after('.clipboard_pasted {').all_before('.iconified {')
	assert win32_clipboard_pasted_body.contains('input = InputEvent{')
	assert win32_clipboard_pasted_body.contains('kind:               input_kind')
	assert win32_clipboard_pasted_body.contains('window_id:          record.id')
	assert win32_clipboard_pasted_body.contains('modifiers:          modifiers')
	assert win32_clipboard_pasted_body.contains('mouse_button:       256')

	assert win32_helper_source.contains('GWLP_USERDATA')
	assert win32_helper_source.contains('v_multiwindow_win32_next_event_sequence')
	assert win32_helper_source.contains('V_MULTIWINDOW_WIN32_INPUT_ICONIFIED')
	assert win32_helper_source.contains('V_MULTIWINDOW_WIN32_INPUT_RESTORED')
	assert win32_helper_source.contains('V_MULTIWINDOW_WIN32_INPUT_CLIPBOARD_PASTED')
	assert win32_helper_source.contains('V_MULTIWINDOW_WIN32_INPUT_TOUCHES_BEGAN')
	assert win32_helper_source.contains('V_MULTIWINDOW_WIN32_INPUT_TOUCHES_MOVED')
	assert win32_helper_source.contains('V_MULTIWINDOW_WIN32_INPUT_TOUCHES_ENDED')
	assert !win32_helper_source.contains('V_MULTIWINDOW_WIN32_INPUT_TOUCHES_CANCELLED')
	assert win32_helper_source.contains('WM_MOUSEMOVE')
	assert win32_helper_source.contains('TrackMouseEvent')
	assert win32_helper_source.contains('WM_MOUSELEAVE')
	assert win32_helper_source.contains('WM_LBUTTONDOWN')
	assert win32_helper_source.contains('WM_RBUTTONDOWN')
	assert win32_helper_source.contains('WM_MBUTTONDOWN')
	assert win32_helper_source.contains('WM_MOUSEWHEEL')
	assert win32_helper_source.contains('WM_MOUSEHWHEEL')
	assert win32_helper_source.contains('WM_KEYDOWN')
	assert win32_helper_source.contains('WM_SYSKEYDOWN')
	assert win32_helper_source.contains('WM_KEYUP')
	assert win32_helper_source.contains('WM_SYSKEYUP')
	assert win32_helper_source.contains('WM_CHAR')
	assert win32_helper_source.contains('WM_SETFOCUS')
	assert win32_helper_source.contains('WM_KILLFOCUS')
	assert win32_helper_source.contains('wparam == SIZE_MINIMIZED')
	assert win32_helper_source.contains('V_MULTIWINDOW_WIN32_INPUT_ICONIFIED')
	assert win32_helper_source.contains('V_MULTIWINDOW_WIN32_INPUT_RESTORED')
	assert win32_helper_source.contains('v_multiwindow_win32_key_code')
	assert win32_helper_source.contains('v_multiwindow_win32_modifiers')
	assert win32_helper_source.contains('GetAsyncKeyState(VK_LBUTTON)')
	win32_keydown_start := win32_helper_source.index('case WM_KEYDOWN:') or {
		assert false, 'Win32 helper does not handle WM_KEYDOWN'
		0
	}
	win32_syskeydown_start := win32_helper_source.index('case WM_SYSKEYDOWN:') or {
		assert false, 'Win32 helper does not handle WM_SYSKEYDOWN'
		0
	}
	win32_keydown_body := win32_helper_source[win32_keydown_start..win32_syskeydown_start]
	assert win32_keydown_body.contains('uint32_t modifiers = v_multiwindow_win32_modifiers();')
	assert win32_keydown_body.contains('V_MULTIWINDOW_WIN32_INPUT_KEY_DOWN, key_code, 0, v_multiwindow_win32_key_repeat(lparam), modifiers')
	assert win32_keydown_body.contains('V_MULTIWINDOW_WIN32_INPUT_CLIPBOARD_PASTED, 0, 0, 0, modifiers')
	assert_source_order(win32_keydown_body,
		'uint32_t modifiers = v_multiwindow_win32_modifiers();',
		'V_MULTIWINDOW_WIN32_INPUT_KEY_DOWN')
	assert_source_order(win32_keydown_body,
		'uint32_t modifiers = v_multiwindow_win32_modifiers();',
		'V_MULTIWINDOW_WIN32_INPUT_CLIPBOARD_PASTED')
	assert win32_helper_source.contains('V_MULTIWINDOW_WIN32_MODIFIER_LMB 0x100')
	assert win32_helper_source.contains('V_MULTIWINDOW_WIN32_MODIFIER_RMB 0x200')
	assert win32_helper_source.contains('V_MULTIWINDOW_WIN32_MODIFIER_MMB 0x400')
	assert win32_helper_source.contains('v_multiwindow_win32_is_char_code')
	assert win32_helper_source.contains('c >= 32 || c == 8 || c == 9 || c == 13 || c == 127')
	assert win32_helper_source.contains('return (lparam & 0x01000000) ? 335 : 257;')
	assert !win32_backend_source.contains('win32_input_quit_requested')
	assert !win32_backend_source.contains('.quit_requested')
	assert !win32_helper_source.contains('V_MULTIWINDOW_WIN32_INPUT_QUIT_REQUESTED')
	assert !win32_helper_source.contains('WM_SYSCHAR')
	assert win32_helper_source.contains('WM_DROPFILES')
	assert win32_helper_source.contains('DragAcceptFiles')
	assert win32_helper_source.contains('DragQueryFileW')
	assert win32_helper_source.contains('DragQueryPoint')
	assert win32_helper_source.contains('DragFinish')
	assert win32_helper_source.contains('v_multiwindow_win32_window_drop_begin')
	assert win32_helper_source.contains('v_multiwindow_win32_window_drop_file')
	assert win32_helper_source.contains('v_multiwindow_win32_window_drop_file(void *data, uint64_t sequence, char *path)')
	assert !win32_helper_source.contains('v_multiwindow_win32_window_drop_file(void *data, uint64_t sequence, const char *path)')
	assert win32_helper_source.contains('v_multiwindow_win32_window_drop_end')
	assert win32_helper_source.contains('WM_TOUCH')
	assert win32_helper_source.contains('v_multiwindow_win32_register_touch_window')
	assert win32_helper_source.contains('RegisterTouchWindow')
	assert win32_helper_source.contains('GetTouchInputInfo')
	assert win32_helper_source.contains('CloseTouchInputHandle')
	assert win32_helper_source.contains('v_multiwindow_win32_window_touch_event(void *data, uint64_t sequence, int kind, uint32_t modifiers, int count, uint64_t *ids, int *xs, int *ys, int *changed)')
	assert !win32_helper_source.contains('v_multiwindow_win32_window_touch_event(void *data, uint64_t sequence, int kind, uint32_t modifiers, int count, const uint64_t *ids')
	assert !win32_helper_source.contains('const int *xs')
	assert !win32_helper_source.contains('const int *ys')
	assert !win32_helper_source.contains('const int *changed')
	assert win32_helper_source.contains('ScreenToClient(hwnd, &point)')
	assert win32_helper_source.contains('changed[out_count] = 1;')
	assert_source_order(win32_helper_source,
		'v_multiwindow_win32_get_touch_input_info(handle, count, inputs)',
		'v_multiwindow_win32_close_touch_input_handle(handle);')
	assert_source_order(win32_helper_source,
		'v_multiwindow_win32_close_touch_input_handle(handle);',
		'v_multiwindow_win32_emit_touch_group(hwnd, data, inputs, count, TOUCHEVENTF_DOWN')
	assert !win32_helper_source.contains('WM_POINTERCANCEL')
	assert !win32_helper_source.contains('WM_IME')

	syskey_down_start := win32_helper_source.index('case WM_SYSKEYDOWN:') or {
		assert false, 'Win32 helper does not handle WM_SYSKEYDOWN'
		0
	}
	syskey_up_start := win32_helper_source.index('case WM_SYSKEYUP:') or {
		assert false, 'Win32 helper does not handle WM_SYSKEYUP'
		0
	}
	key_up_start := win32_helper_source.index('case WM_KEYUP:') or {
		assert false, 'Win32 helper does not handle WM_KEYUP'
		0
	}
	char_start := win32_helper_source.index('case WM_CHAR:') or {
		assert false, 'Win32 helper does not handle WM_CHAR'
		0
	}
	assert !win32_helper_source[syskey_down_start..key_up_start].contains('return 0;')
	assert !win32_helper_source[syskey_up_start..char_start].contains('return 0;')
}

fn test_win32_focus_cleanup_and_zero_window_monitor_refresh_source_guard() {
	backend_source := multiwindow_source_file('win32_backend.c.v')
	service_source := multiwindow_source_file('win32_service_backend.c.v')
	service_api_source := multiwindow_source_file('service_api.v')
	event_delivery_source := multiwindow_source_file('event_delivery.v')
	native_source := multiwindow_source_file('win32_service_native.h')
	helper_source := multiwindow_source_file('win32_backend_helpers.h')
	capability_body :=
		service_source.all_after('fn (backend &Win32Backend) service_operation_capability').all_before('fn (backend &Win32Backend) service_window_state')
	assert capability_body.contains('C.v_multiwindow_win32_service_fullscreen_known')
	assert capability_body.contains('.fullscreen, .restore')
	fullscreen_capability_body :=
		capability_body.all_after('.fullscreen, .restore {').all_before('.maximize {')
	assert_source_order(fullscreen_capability_body, 'support:', 'if fullscreen_known')
	assert fullscreen_capability_body.contains('state_observable: true')
	assert native_source.contains('v_multiwindow_win32_service_fullscreen_known')
	clipboard_body :=
		service_source.all_after('fn (mut backend Win32Backend) collect_clipboard_events').all_before('fn (mut backend Win32Backend) purge_clipboard_window')
	assert_source_order(clipboard_body, 'if status == win32_clipboard_attempt_retry {',
		'now_ns := backend.clipboard_now_ns()')
	assert_source_order(clipboard_body, 'now_ns := backend.clipboard_now_ns()',
		"backend.finish_clipboard_head(.failed, '', err_clipboard_timeout)")
	kill_focus_body :=
		helper_source.all_after('case WM_KILLFOCUS:').all_before('case WM_SETCURSOR:')
	assert_source_order(kill_focus_body, 'v_multiwindow_win32_window_focus_lost(data);',
		'V_MULTIWINDOW_WIN32_INPUT_UNFOCUSED')
	assert backend_source.contains('mouse_focus_cleanup_pending  bool')
	assert backend_source.contains('mouse_focus_cleanup_reported bool')
	focus_callback_body :=
		backend_source.all_after('fn win32_window_focus_lost(data voidptr)').all_before("@[export: 'v_multiwindow_win32_window_raw_mouse_event']")
	assert focus_callback_body.contains('record.mouse_focus_cleanup_pending')
	assert focus_callback_body.contains('record.mouse_focus_cleanup_reported')
	assert_source_order(focus_callback_body, 'record.mouse_focus_cleanup_pending',
		'C.v_multiwindow_win32_service_focus_lost')
	focus_retry_body :=
		backend_source.all_after('if record.mouse_focus_cleanup_pending').all_before('if record.raw_input_failed')
	assert focus_retry_body.contains('C.v_multiwindow_win32_service_focus_lost')
	assert focus_retry_body.contains('backend.record_native_input_release_error')
	assert focus_retry_body.contains('backend.resolve_native_input_release_error')
	assert native_source.contains('return v_multiwindow_win32_service_mouse_cleanup(')
	poll_body :=
		backend_source.all_after('fn (mut backend Win32Backend) poll_queued_events').all_before('fn (mut backend Win32Backend) take_poll_error')
	assert_source_order(poll_body, 'if record.mouse_focus_cleanup_pending',
		'backend.collect_service_refresh_events()!')

	repository_dir := os.dir(os.dir(os.dir(@DIR)))
	w3_runner := os.read_file(os.join_path(repository_dir, '.github', 'workflows',
		'run_multiwindow_win32_w3_green.ps1')) or { panic(err) }
	assert w3_runner.count('test_win32_zero_window_monitor_snapshot_equality_uses_all_public_fields') == 1
	assert w3_runner.contains('schema=package2-win32-w3-green-surface-v1')
	assert w3_runner.contains('[string]$ExpectedCompositeSha256')
	assert w3_runner.contains('[string]$ExpectedRunnerSha256')

	create_body :=
		backend_source.all_after('fn (mut backend Win32Backend) create_window').all_before('fn (mut backend Win32Backend) set_window_title')
	assert_source_order(create_body, 'if backend.windows.len == 0 {',
		'backend.windows << &Win32WindowRecord{')
	assert create_body.contains('backend.refresh_service_monitors_before_first_window()!')
	assert create_body.contains('backend.service_monitor_pending_records')
	assert create_body.contains('&& staged_monitor_ids.len == 0')
	assert service_source.contains('fn win32_service_raw_monitor_snapshots_equal')
	assert service_source.contains('right_indices[monitor.native_id]')
	assert service_source.contains('monitor.work_height != other.work_height')
	assert service_source.contains('monitor.dpi != other.dpi')
	assert service_source.contains('monitor.primary != other.primary')
	precreate_refresh_body :=
		service_source.all_after('fn (mut backend Win32Backend) refresh_service_monitors_before_first_window').all_before('fn win32_service_monitor_ids_equal')
	assert precreate_refresh_body.contains('backend.service_monitor_pending_records = plan.records.clone()')
	assert precreate_refresh_body.contains('backend.service_monitor_pending_raw = raw.clone()')
	assert !precreate_refresh_body.contains('backend.service_monitors = plan.records')
	assert !precreate_refresh_body.contains('backend.service_monitor_raw = raw.clone()')
	assert_source_order(precreate_refresh_body,
		'win32_service_raw_monitor_snapshots_equal(backend.service_monitor_raw, raw)',
		'backend.service_monitor_pending_sequence != 0')
	assert_source_order(precreate_refresh_body,
		'win32_service_raw_monitor_snapshots_equal(backend.service_monitor_raw, raw)',
		'backend.clear_pending_service_monitor_refresh()')
	refresh_body :=
		service_source.all_after('fn (mut backend Win32Backend) collect_service_refresh_events').all_before('fn (mut backend Win32Backend) service_show_window')
	assert_source_order(refresh_body, 'if backend.service_monitor_pending_sequence != 0 {',
		'if backend.windows.len == 0 {')
	pending_body :=
		refresh_body.all_after('if backend.service_monitor_pending_sequence != 0 {').all_before('if backend.windows.len == 0 {')
	assert pending_body.contains('return events')
	pending_net_zero_body :=
		pending_body.all_after('if win32_service_raw_monitor_snapshots_equal(backend.service_monitor_raw, latest_raw) {').all_before('} else {')
	assert pending_net_zero_body.contains('suppress_net_zero_monitor = true')
	assert !pending_net_zero_body.contains('backend.clear_pending_service_monitor_refresh()')
	assert !pending_net_zero_body.contains('return events')
	assert_source_order(pending_body,
		'win32_service_raw_monitor_snapshots_equal(backend.service_monitor_raw, latest_raw)',
		'backend.clear_pending_service_monitor_refresh()')
	assert_source_order(pending_body, 'win32_service_monitor_event(app_instance, staged_monitors)',
		'mut observations := []Win32ServiceRefreshObservation')
	assert_source_order(pending_body,
		'backend.service_metrics_observation(index, staged_records) or {',
		'backend.service_monitors = staged_records')
	assert_source_order(pending_body, 'backend.service_monitors = staged_records',
		'record.service_monitor_ids = observation.monitor_ids.clone()')
	assert_source_order(pending_body, 'latest_raw := win32_service_raw_monitor_snapshot() or {',
		'backend.service_monitors = staged_records')
	assert_source_order(pending_body, 'latest_plan := win32_plan_service_monitors',
		'backend.service_metrics_observation(index, staged_records) or {')
	pending_commit_tail :=
		pending_body.all_after('record.service_monitor_ids = observation.monitor_ids.clone()')
	assert pending_commit_tail.contains('backend.clear_pending_service_monitor_refresh()')
	display_refresh_body :=
		refresh_body.all_after('if display_sequence != 0 {').all_before('mut pending_indices := []int{}')
	assert display_refresh_body.contains('raw_changed := !win32_service_raw_monitor_snapshots_equal')
	assert display_refresh_body.contains('emit_monitor := raw_changed || !suppress_net_zero_monitor')
	display_monitor_body :=
		display_refresh_body.all_after('if emit_monitor {').all_before('mut observations := []Win32ServiceRefreshObservation')
	assert display_monitor_body.contains('win32_service_monitor_event(app_instance, monitors)')
	assert_source_order(display_refresh_body,
		'mut observations := []Win32ServiceRefreshObservation',
		'backend.service_monitor_poll_dirty = false')
	assert_source_order(display_refresh_body,
		'record.service_monitor_ids = observation.monitor_ids.clone()',
		'backend.clear_pending_service_monitor_refresh()')
	assert refresh_body.contains('backend.service_monitor_poll_dirty = true')
	query_body :=
		service_api_source.all_after('pub fn (app &App) service_window_state').all_before('// service_monitor_ids returns')
	assert query_body.contains('service_window_state_with_registered_monitor_membership')
	publish_body :=
		service_api_source.all_after('fn (mut app App) publish_native_state').all_before('fn service_window_state_has_observation')
	assert_source_order(publish_body, 'service_window_state_with_registered_monitor_membership',
		'merge_service_window_state')
	accept_body :=
		event_delivery_source.all_after('mut sequenced := service_event_with_sequence').all_before('app.enqueue_reserved_event_locked(queued_service_event(sequenced)')
	assert accept_body.count('service_window_state_with_registered_monitor_membership') == 2
	assert_source_order(accept_body, 'if event.kind == .state {',
		'service_window_state_with_registered_monitor_membership')
	metrics_body := accept_body.all_after('} else if event.kind == .metrics {')
	assert_source_order(metrics_body, 'service_window_state_with_registered_monitor_membership',
		'merge_service_window_state')
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
		assert !caps.readback
		assert_appkit_input_capabilities(caps)
	} $else {
		capabilities_for_backend(.appkit) or {
			assert err.msg() == err_backend_unsupported
			return
		}
		assert false, 'appkit capabilities succeeded on an unsupported OS'
	}
}

fn test_appkit_readback_capability_requires_ready_metal_renderer() {
	backend := AppKitBackend{}
	rendererless := backend.capabilities_for_renderer(false)
	assert !rendererless.readback
	assert !rendererless.metal
	ready := backend.capabilities_for_renderer(true)
	assert ready.readback == appkit_metal_supported()
	assert ready.readback == ready.metal
}

fn test_appkit_sharedlive_source_excludes_objc_implementation() {
	source := multiwindow_source_file('appkit_backend.c.v')
	assert source.contains('#insert "@VMODROOT/vlib/x/multiwindow/appkit_backend_helpers.h"')
	assert_source_order(source, '#insert "@VMODROOT/vlib/x/multiwindow/appkit_backend_helpers.h"',
		'$if sharedlive ? {')
	assert_source_order(source, '$if sharedlive ? {',
		'#include "@VMODROOT/vlib/x/multiwindow/appkit_backend.m"')
}

fn test_appkit_monitor_refresh_precedes_native_observation_conversion_source_guard() {
	source := multiwindow_source_file('appkit_backend.c.v')
	poll_body :=
		source.all_after('fn (mut backend AppKitBackend) poll_queued_events() ![]QueuedEvent').all_before('$if darwin {\n\t@[markused]')
	assert poll_body.contains('pending_monitor_events = backend.monitor_change_event()!')
	assert_source_order(poll_body, 'pending_monitor_events = backend.monitor_change_event()!',
		'for i < backend.windows.len')
	emission_body := poll_body.all_after('for native_event in native_events {')
	assert_source_order(emission_body,
		'appkit_service_event_depends_on_monitor_snapshot(native_event.event)',
		'events << native_event.event')
	monitor_body :=
		source.all_after('fn (mut backend AppKitBackend) monitor_change_event() ![]QueuedEvent').all_before('fn (mut backend AppKitBackend) take_poll_error')
	assert monitor_body.contains('backend.service_monitor_snapshot(app_instance)!')
	snapshot_body :=
		source.all_after('fn (mut backend AppKitBackend) service_monitor_snapshot(app_instance u64) ![]ServiceMonitorInfo').all_before('fn (mut backend AppKitBackend) service_set_clipboard_text')
	assert snapshot_body.contains('backend.monitor_failures_for_test')
}

fn appkit_service_v_declaration_names(source string) []string {
	marker := 'fn C.v_multiwindow_appkit_service_'
	mut names := []string{}
	for line in source.split_into_lines() {
		declaration := line.trim_space()
		if !declaration.starts_with(marker) {
			continue
		}
		open_parenthesis := declaration.index('(') or { continue }
		names << declaration['fn C.'.len..open_parenthesis]
	}
	names.sort()
	return names
}

fn appkit_header_prototype_names(header string) []string {
	prefix := 'v_multiwindow_appkit_'
	mut names := []string{}
	for statement in header.split(';') {
		name_start := statement.index(prefix) or { continue }
		candidate := statement[name_start..]
		open_parenthesis := candidate.index('(') or { continue }
		names << candidate[..open_parenthesis].trim_space()
	}
	names.sort()
	return names
}

fn test_appkit_prototype_header_is_c_only_and_shared_with_implementation() {
	backend := multiwindow_source_file('appkit_backend.c.v')
	header := multiwindow_source_file('appkit_backend_helpers.h')
	implementation := multiwindow_source_file('appkit_backend.m')
	service_declarations := appkit_service_v_declaration_names(backend)
	header_prototypes := appkit_header_prototype_names(header)
	service_prototypes := header_prototypes.filter(it.starts_with('v_multiwindow_appkit_service_'))
	render_prototypes := [
		'v_multiwindow_appkit_logical_to_pixel_rect',
		'v_multiwindow_appkit_pixel_to_logical_rect',
		'v_multiwindow_appkit_render_snapshot',
	]
	render_header_prototypes := header_prototypes.filter(it in render_prototypes)

	assert header.contains('int v_multiwindow_appkit_is_main_thread(void);')
	assert header.contains('VMultiwindowNativePrimitive v_multiwindow_appkit_create_window(void *device_ptr, const char *title, int width, int height, int min_width, int min_height, int resizable, int visible, int high_dpi, int borderless, int fullscreen, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height);')
	assert header.contains('int v_multiwindow_appkit_resize_window(void *state_ptr, int width, int height, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height);')
	assert header.contains('VMultiwindowNativePrimitive v_multiwindow_appkit_begin_frame(void *state_ptr, void *device_ptr);')
	assert header.contains('VMultiwindowNativePrimitive v_multiwindow_appkit_end_frame(void *state_ptr, void *drawable_ptr);')
	assert header.contains('VMultiwindowNativePrimitive v_multiwindow_appkit_abort_frame(void *state_ptr, void *drawable_ptr);')
	assert !header.contains('int v_multiwindow_appkit_create_window(')
	assert !header.contains('int v_multiwindow_appkit_begin_frame(')
	assert !header.contains('void v_multiwindow_appkit_end_frame(void *state_ptr);')
	assert !header.contains('void v_multiwindow_appkit_abort_frame(void *state_ptr);')
	assert !header.contains('@interface')
	assert !header.contains('@implementation')
	assert !header.contains('VMultiwindowAppKitWindowState')
	assert !header.contains('NSWindow')
	assert service_declarations.len == 35
	assert service_prototypes.len == service_declarations.len
	assert service_prototypes == service_declarations
	assert service_declarations.len + render_prototypes.len == 38
	for prototype in render_prototypes {
		assert backend.contains('fn C.${prototype}(')
	}
	assert render_header_prototypes == render_prototypes

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
		cmd := '${os.quoted_path(@VEXE)} -nocolor -cc clang -d gg_multiwindow -d sokol_metal -sharedlive -shared -o ${os.quoted_path(dylib_path)} ${os.quoted_path(source_path)}'
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

fn test_appkit_input_events_are_queued_and_capability_scoped_source_guard() {
	backend_source := multiwindow_source_file('backend.v')
	appkit_backend_source := multiwindow_source_file('appkit_backend.c.v')
	appkit_header_source := multiwindow_source_file('appkit_backend_helpers.h')
	appkit_impl_source := multiwindow_source_file('appkit_backend.m')
	backend_poll_body :=
		backend_source.all_after('fn (mut backend Backend) poll_queued_events() ![]QueuedEvent').all_before('fn (mut backend Backend) event_sequence_terminal_error')
	compact_backend_poll_body :=
		backend_poll_body.replace(' ', '').replace('\t', '').replace('\n', '')

	assert compact_backend_poll_body.contains('.appkit{backend.appkit.poll_queued_events()!}')
	assert appkit_backend_source.contains('struct AppKitNativeQueuedEvent')
	assert appkit_backend_source.contains('fn (mut backend AppKitBackend) poll_queued_events() ![]QueuedEvent')
	assert appkit_backend_source.contains('appkit_sort_native_events(mut native_events)')
	assert appkit_backend_source.contains('appkit_queued_event_from_native(record, native_event)')
	assert appkit_backend_source.contains('@[heap; markused]\nstruct AppKitWindowRecord')
	assert !appkit_backend_source.contains('const appkit_input_')
	assert !appkit_backend_source.contains('const appkit_native_event_')
	assert appkit_backend_source.contains('input_events:       true')
	assert appkit_backend_source.contains('mouse_events:       true')
	assert appkit_backend_source.contains('keyboard_events:    true')
	assert appkit_backend_source.contains('text_events:        true')
	assert appkit_backend_source.contains('focus_events:       true')
	assert appkit_backend_source.contains('drop_events:        true')
	assert appkit_backend_source.contains('touch_events:       true')
	assert appkit_backend_source.contains('13 { InputEventKind.iconified }')
	assert appkit_backend_source.contains('14 { InputEventKind.restored }')
	assert appkit_backend_source.contains('15 { InputEventKind.clipboard_pasted }')
	assert appkit_backend_source.contains('16 { InputEventKind.files_dropped }')
	assert appkit_backend_source.contains('17 { InputEventKind.touches_began }')
	assert appkit_backend_source.contains('18 { InputEventKind.touches_moved }')
	assert appkit_backend_source.contains('19 { InputEventKind.touches_ended }')
	assert appkit_backend_source.contains('20 { InputEventKind.touches_cancelled }')
	assert appkit_backend_source.contains('dropped_files:      appkit_dropped_files_from_native(native_event)')
	assert appkit_backend_source.contains('mut touch_count := native_event.touch_count')
	assert appkit_backend_source.contains('if touch_count > 8')
	assert appkit_backend_source.contains('mut touches := [8]InputTouchPoint{}')
	assert appkit_backend_source.contains('num_touches:        touch_count')
	assert appkit_backend_source.contains('touches:            touches')
	assert appkit_backend_source.contains('fn appkit_dropped_files_from_native')
	assert appkit_backend_source.contains('clone_native_path(path)')
	assert !appkit_backend_source.contains('tos_clone(&u8(path))')
	assert !appkit_backend_source.contains('cstring_to_vstring(path)')
	assert !appkit_backend_source.contains('fn appkit_touch_count')
	assert !appkit_backend_source.contains('fn appkit_touches_from_native')
	assert appkit_backend_source.contains('C.v_multiwindow_appkit_release_queued_event_resources')

	assert appkit_header_source.contains('typedef struct VMultiwindowAppKitQueuedEvent')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_INPUT_MOUSE_MOVE')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_INPUT_RESIZED')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_INPUT_ICONIFIED')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_INPUT_RESTORED')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_INPUT_CLIPBOARD_PASTED')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_INPUT_FILES_DROPPED')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_BEGAN')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_MOVED')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_ENDED')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_CANCELLED')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS 8')
	assert appkit_header_source.contains('int dropped_file_count;')
	assert appkit_header_source.contains('char **dropped_files;')
	assert appkit_header_source.contains('int touch_count;')
	assert appkit_header_source.contains('uint64_t touch_ids[V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS];')
	assert appkit_header_source.contains('float touch_x[V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS];')
	assert appkit_header_source.contains('float touch_y[V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS];')
	assert appkit_header_source.contains('int touch_changed[V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS];')
	assert appkit_header_source.contains('int v_multiwindow_appkit_take_queued_event')
	assert appkit_header_source.contains('void v_multiwindow_appkit_release_queued_event_resources')
	assert !appkit_header_source.contains('v_multiwindow_appkit_take_close_requested')
	assert !appkit_header_source.contains('v_multiwindow_appkit_take_resized')
	assert !appkit_header_source.contains('v_multiwindow_appkit_take_destroyed')
	assert !appkit_backend_source.contains('v_multiwindow_appkit_take_close_requested')
	assert !appkit_backend_source.contains('v_multiwindow_appkit_take_resized')
	assert !appkit_backend_source.contains('v_multiwindow_appkit_take_destroyed')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_MODIFIER_LMB 0x100')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_MODIFIER_RMB 0x200')
	assert appkit_header_source.contains('V_MULTIWINDOW_APPKIT_MODIFIER_MMB 0x400')

	assert appkit_impl_source.contains('@interface VMultiwindowAppKitView : NSView')
	assert appkit_impl_source.contains('- (BOOL)acceptsFirstResponder')
	assert appkit_impl_source.contains('- (BOOL)acceptsFirstMouse:(NSEvent *)event')
	assert appkit_impl_source.contains('NSTrackingMouseEnteredAndExited')
	assert appkit_impl_source.contains('NSTrackingEnabledDuringMouseDrag')
	assert appkit_impl_source.contains('windowDidBecomeKey')
	assert appkit_impl_source.contains('windowDidResignKey')
	assert appkit_impl_source.contains('windowDidMiniaturize')
	assert appkit_impl_source.contains('windowDidDeminiaturize')
	assert appkit_impl_source.contains('V_MULTIWINDOW_APPKIT_INPUT_ICONIFIED')
	assert appkit_impl_source.contains('V_MULTIWINDOW_APPKIT_INPUT_RESTORED')
	assert appkit_impl_source.contains('V_MULTIWINDOW_APPKIT_INPUT_CLIPBOARD_PASTED')
	assert appkit_impl_source.contains('V_MULTIWINDOW_APPKIT_INPUT_FILES_DROPPED')
	assert appkit_impl_source.contains('V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_BEGAN')
	assert appkit_impl_source.contains('V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_MOVED')
	assert appkit_impl_source.contains('V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_ENDED')
	assert appkit_impl_source.contains('V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_CANCELLED')
	assert appkit_impl_source.contains('clearKeyDownState')
	assert appkit_impl_source.contains('windowShouldClose')
	assert appkit_impl_source.contains('queueResizeEvents')
	assert appkit_impl_source.contains('suppressResizeEvent')
	assert !appkit_impl_source.contains('@property(assign) BOOL closeRequested')
	assert !appkit_impl_source.contains('@property(assign) BOOL destroyed')
	assert !appkit_impl_source.contains('@property(assign) BOOL resized')
	assert !appkit_impl_source.contains('int v_multiwindow_appkit_take_close_requested')
	assert !appkit_impl_source.contains('int v_multiwindow_appkit_take_resized')
	assert !appkit_impl_source.contains('int v_multiwindow_appkit_take_destroyed')
	assert !appkit_impl_source.contains('self.closeRequested')
	assert !appkit_impl_source.contains('state.closeRequested')
	assert !appkit_impl_source.contains('self.destroyed')
	assert !appkit_impl_source.contains('state.destroyed')
	assert !appkit_impl_source.contains('self.resized')
	assert !appkit_impl_source.contains('state.resized')
	assert appkit_impl_source.contains('makeFirstResponder:view')
	assert appkit_impl_source.contains('mouseDown:')
	assert appkit_impl_source.contains('rightMouseDown:')
	assert appkit_impl_source.contains('otherMouseDown:')
	assert appkit_impl_source.contains('scrollWheel:')
	assert appkit_impl_source.contains('event.hasPreciseScrollingDeltas')
	assert appkit_impl_source.contains('keyDown:')
	assert appkit_impl_source.contains('keyUp:')
	assert appkit_impl_source.contains('flagsChanged:')
	assert appkit_impl_source.contains('NSDraggingDestination')
	assert appkit_impl_source.contains('registerForDraggedTypes')
	assert appkit_impl_source.contains('NSPasteboardTypeFileURL')
	assert appkit_impl_source.contains('performDragOperation')
	assert appkit_impl_source.contains('readObjectsForClasses:@[ NSURL.class ]')
	assert appkit_impl_source.contains('dropped_files')
	assert appkit_impl_source.contains('v_multiwindow_appkit_release_queued_event_resources')
	assert appkit_impl_source.contains('view.acceptsTouchEvents = YES;')
	assert appkit_impl_source.contains('touchesBeganWithEvent:')
	assert appkit_impl_source.contains('touchesMovedWithEvent:')
	assert appkit_impl_source.contains('touchesEndedWithEvent:')
	assert appkit_impl_source.contains('touchesCancelledWithEvent:')
	assert appkit_impl_source.contains('touchesMatchingPhase:NSTouchPhaseAny inView:view')
	assert appkit_impl_source.contains('NSTouchPhaseBegan')
	assert appkit_impl_source.contains('NSTouchPhaseMoved')
	assert appkit_impl_source.contains('NSTouchPhaseEnded')
	assert appkit_impl_source.contains('NSTouchPhaseCancelled')
	assert appkit_impl_source.contains('v_multiwindow_appkit_touch_sets_share_identity')
	assert appkit_impl_source.contains('v_multiwindow_appkit_fill_touch_point')
	assert appkit_impl_source.contains('touch.normalizedPosition')
	assert appkit_impl_source.contains('event->touch_changed[index] = changed ? 1 : 0;')
	assert appkit_impl_source.contains('#include <IOKit/hidsystem/IOLLEvent.h>')
	assert appkit_impl_source.contains('v_multiwindow_appkit_modifier_flag_for_key_code')
	assert appkit_impl_source.contains('v_multiwindow_appkit_modifier_key_is_pressed')
	assert appkit_impl_source.contains('NX_DEVICELSHIFTKEYMASK')
	assert appkit_impl_source.contains('NX_DEVICERSHIFTKEYMASK')
	assert appkit_impl_source.contains('NX_DEVICELCTLKEYMASK')
	assert appkit_impl_source.contains('NX_DEVICERCTLKEYMASK')
	assert appkit_impl_source.contains('NX_DEVICELALTKEYMASK')
	assert appkit_impl_source.contains('NX_DEVICERALTKEYMASK')
	assert appkit_impl_source.contains('NX_DEVICELCMDKEYMASK')
	assert appkit_impl_source.contains('NX_DEVICERCMDKEYMASK')
	flags_changed_start := appkit_impl_source.index('- (void)flagsChanged:(NSEvent *)event') or {
		assert false, 'AppKit view does not implement flagsChanged:'
		0
	}
	flags_changed_body := appkit_impl_source[flags_changed_start..].all_before('@end')
	key_down_body :=
		appkit_impl_source.all_after('- (void)keyDown:(NSEvent *)event').all_before('- (BOOL)performKeyEquivalent:(NSEvent *)event')
	assert flags_changed_body.contains('uint32_t oldFlags = state.flagsChangedStore;')
	assert flags_changed_body.contains('uint32_t newFlags = (uint32_t)event.modifierFlags;')
	assert flags_changed_body.contains('state.flagsChangedStore = newFlags;')
	assert flags_changed_body.contains('v_multiwindow_appkit_modifier_key_is_pressed((NSEventModifierFlags)oldFlags')
	assert flags_changed_body.contains('v_multiwindow_appkit_modifier_key_is_pressed((NSEventModifierFlags)newFlags')
	assert flags_changed_body.contains('wasPressed == isPressed')
	assert flags_changed_body.contains('[state setKeyDown:isPressed forKeyCode:keyCode]')
	assert flags_changed_body.contains('isPressed ? V_MULTIWINDOW_APPKIT_INPUT_KEY_DOWN')
	assert !flags_changed_body.contains('wasKeyDown')
	assert !flags_changed_body.contains('isKeyDownForKeyCode')
	assert !flags_changed_body.contains('v_multiwindow_appkit_key_code_is_sided_modifier')
	assert appkit_impl_source.contains('event.isARepeat')
	assert appkit_impl_source.contains('event.characters')
	assert key_down_body.contains('[state queueKeyEvent:V_MULTIWINDOW_APPKIT_INPUT_KEY_DOWN')
	assert key_down_body.contains('if ((modifiers & 8u) == 0) {')
	assert key_down_body.contains('[state queueCharEventsFromString:event.characters')
	assert key_down_body.contains('if (modifiers == 8 && keyCode == 86) {')
	assert key_down_body.contains('V_MULTIWINDOW_APPKIT_INPUT_CLIPBOARD_PASTED')
	assert_source_order(key_down_body, '[state queueKeyEvent:V_MULTIWINDOW_APPKIT_INPUT_KEY_DOWN',
		'if ((modifiers & 8u) == 0) {')
	assert_source_order(key_down_body, 'if ((modifiers & 8u) == 0) {',
		'[state queueCharEventsFromString:event.characters')
	assert_source_order(key_down_body, '[state queueCharEventsFromString:event.characters',
		'if (modifiers == 8 && keyCode == 86) {')
	assert appkit_impl_source.contains('v_multiwindow_appkit_keycodes')
	assert !appkit_impl_source.contains('nextKeyDownStateForKeyCode')
}

fn test_appkit_macos_cgen_emits_record_and_literal_input_mapping() {
	c_source := multiwindow_emit_macos_multiwindow_test_c()
	leaked_display_field := ['Dis', 'play* display;'].join('')
	leaked_probe_type := ['VMultiwindowX11', 'ReadbackProbe'].join('')
	assert !c_source.contains(leaked_display_field)
	assert !c_source.contains('Optional_${leaked_probe_type}')
	assert !c_source.contains('_option_C__${leaked_probe_type}')
	v1_typedef := 'typedef struct x__multiwindow__AppKitWindowRecord x__multiwindow__AppKitWindowRecord;'
	v1_struct := 'struct x__multiwindow__AppKitWindowRecord {'
	v1_mapping := 'VV_LOC _option_x__multiwindow__QueuedEvent x__multiwindow__appkit_queued_event_from_native('
	v3_typedef := 'typedef struct multiwindow__AppKitWindowRecord multiwindow__AppKitWindowRecord;'
	v3_struct := 'struct multiwindow__AppKitWindowRecord {'
	v3_mapping := 'Optional_multiwindow__QueuedEvent multiwindow__appkit_queued_event_from_native('
	c_lines := c_source.split_into_lines()
	mut v1_typedef_indices := []int{}
	mut v1_struct_indices := []int{}
	mut v1_prototype_indices := []int{}
	mut v1_definition_indices := []int{}
	mut v1_mapping_line_count := 0
	mut v3_typedef_indices := []int{}
	mut v3_struct_indices := []int{}
	mut v3_prototype_indices := []int{}
	mut v3_definition_indices := []int{}
	mut v3_mapping_line_count := 0
	for line_index, line in c_lines {
		trimmed := line.trim_space()
		if trimmed == v1_typedef {
			v1_typedef_indices << line_index
		}
		if trimmed == v1_struct {
			v1_struct_indices << line_index
		}
		if trimmed.starts_with(v1_mapping) {
			v1_mapping_line_count++
			if trimmed.ends_with(';') {
				v1_prototype_indices << line_index
			}
			if trimmed.ends_with('{') {
				v1_definition_indices << line_index
			}
		}
		if trimmed == v3_typedef {
			v3_typedef_indices << line_index
		}
		if trimmed == v3_struct {
			v3_struct_indices << line_index
		}
		if trimmed.starts_with(v3_mapping) {
			v3_mapping_line_count++
			if trimmed.ends_with(';') {
				v3_prototype_indices << line_index
			}
			if trimmed.ends_with('{') {
				v3_definition_indices << line_index
			}
		}
		if trimmed.starts_with('#define ') || trimmed.starts_with('static ')
			|| trimmed.starts_with('string ') || trimmed.starts_with('const ')
			|| trimmed.starts_with('enum ') {
			declaration := trimmed.all_before('=')
			assert !declaration.contains('_const_x__multiwindow__appkit_')
			assert !declaration.contains('_const_multiwindow__appkit_')
		}
	}
	v1_exact := v1_typedef_indices.len == 1 && v1_struct_indices.len == 1
		&& v1_prototype_indices.len == 1 && v1_definition_indices.len == 1
		&& v1_mapping_line_count == 2
	v3_exact := v3_typedef_indices.len == 2 && v3_struct_indices.len == 1
		&& v3_prototype_indices.len == 1 && v3_definition_indices.len == 1
		&& v3_mapping_line_count == 2
	assert v1_exact != v3_exact
	if v1_exact {
		assert v3_typedef_indices.len == 0
		assert v3_struct_indices.len == 0
		assert v3_prototype_indices.len == 0
		assert v3_definition_indices.len == 0
		assert v3_mapping_line_count == 0
		assert v1_prototype_indices[0] < v1_definition_indices[0]
		assert v1_typedef_indices[0] < v1_prototype_indices[0]
		assert v1_struct_indices[0] < v1_definition_indices[0]
	} else {
		assert v1_typedef_indices.len == 0
		assert v1_struct_indices.len == 0
		assert v1_prototype_indices.len == 0
		assert v1_definition_indices.len == 0
		assert v1_mapping_line_count == 0
		assert v3_typedef_indices[0] < v3_typedef_indices[1]
		assert v3_prototype_indices[0] < v3_definition_indices[0]
		assert v3_typedef_indices[0] < v3_prototype_indices[0]
		assert v3_typedef_indices[1] < v3_prototype_indices[0]
		assert v3_struct_indices[0] < v3_definition_indices[0]
	}
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
		assert_x11_input_capabilities(caps)
	} $else {
		capabilities_for_backend(.x11) or {
			assert err.msg() == err_backend_unsupported
			return
		}
		assert false, 'x11 capabilities succeeded on an unsupported OS'
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
	guarded_native_deps :=
		source.all_after('$if linux && x_multiwindow_x11 ? {').all_before('\n}\n\nconst x11_client_message')
	assert guarded_native_deps.contains('#flag linux -lX11-xcb')
	assert guarded_native_deps.split_into_lines().count(it == '\t#flag linux -lX11') == 1
	assert guarded_native_deps.contains('#flag linux -lxcb')
	assert guarded_native_deps.contains('#flag linux -lEGL')
	assert guarded_native_deps.contains('#flag linux -lGL')
	assert source.count('#flag linux -lX11-xcb') == 1
	assert source.split_into_lines().count(it == '\t#flag linux -lX11') == 1
	assert source.count('#flag linux -lxcb') == 1
	assert source.count('#flag linux -lEGL') == 1
	assert source.count('#flag linux -lGL') == 1
	assert source.contains('$if gg_multiwindow ? || x_multiwindow_render ? {\n\timport sokol.gfx')
	assert guarded_native_deps.contains('\t#flag linux -lX11-xcb\n\t#flag linux -lX11\n')
	assert_source_order(guarded_native_deps, '#flag linux -lX11', '#include <X11/Xlib.h>')
}

fn test_x11_c_interop_types_are_native_flag_gated_source_guard() {
	source := multiwindow_source_file('x11_backend.c.v')
	native_declarations :=
		source.all_after('struct X11ReadbackProbe {').all_after('$if linux && x_multiwindow_x11 ? {').all_before('\n}\n\nstruct X11WindowRecord')
	assert source.count('struct C.Display {}') == 1
	assert source.count('struct C.VMultiwindowX11ReadbackProbe {') == 1
	assert native_declarations.contains('struct C.Display {}')
	assert native_declarations.contains('struct C.VMultiwindowX11ReadbackProbe {')
	assert source.contains('display                                      voidptr')
	assert source.contains('!X11ReadbackProbe {')
	assert !source.contains('!C.VMultiwindowX11ReadbackProbe {')
}

fn test_x11_input_support_queues_key_char_and_focus_source_guard() {
	backend_source := multiwindow_source_file('backend.v')
	x11_backend_source := multiwindow_source_file('x11_backend.c.v')
	x11_helper_source := multiwindow_source_file('x11_egl_backend_helpers.h')
	backend_poll_body :=
		backend_source.all_after('fn (mut backend Backend) poll_queued_events() ![]QueuedEvent').all_before('fn (mut backend Backend) event_sequence_terminal_error')
	compact_backend_poll_body :=
		backend_poll_body.replace(' ', '').replace('\t', '').replace('\n', '')

	assert compact_backend_poll_body.contains('.x11{backend.x11.poll_queued_events()!}')
	assert x11_backend_source.contains('input_events:       true')
	assert x11_backend_source.contains('mouse_events:       true')
	assert x11_backend_source.contains('keyboard_events:    true')
	assert x11_backend_source.contains('text_events:        true')
	assert x11_backend_source.contains('focus_events:       true')
	assert x11_backend_source.contains('drop_events:        true')
	assert x11_backend_source.contains('touch_events:       false')
	assert x11_backend_source.contains('.clipboard_pasted')
	assert x11_backend_source.contains('x11_is_clipboard_paste')
	assert x11_backend_source.contains('const x11_modifier_ctrl = u32(2)')
	assert x11_backend_source.contains('const x11_key_v = 86')
	assert x11_helper_source.contains('XkbSetDetectableAutoRepeat')
	assert x11_helper_source.contains('v_multiwindow_x11_is_auto_repeat_release')
	assert x11_helper_source.contains('XPeekEvent(display, &next)')
	assert x11_backend_source.contains('C.v_multiwindow_x11_enable_detectable_auto_repeat(display)')
	assert x11_backend_source.contains('C.v_multiwindow_x11_is_auto_repeat_release(backend.display, &event) != 0')
	x11_paste_body :=
		x11_backend_source.all_after('fn x11_is_clipboard_paste').all_before('$if linux && x_multiwindow_x11 ?')
	assert x11_paste_body.contains('return key_code == x11_key_v && modifiers == x11_modifier_ctrl')
	assert !x11_paste_body.contains('keyboard_modifiers')
	for required_xdnd in ['XdndAware', 'XdndEnter', 'XdndPosition', 'XdndStatus', 'XdndActionCopy',
		'XdndDrop', 'XdndLeave', 'XdndFinished', 'XdndSelection', 'XdndTypeList', 'text/uri-list'] {
		assert x11_backend_source.contains(required_xdnd)
	}
	assert x11_backend_source.contains('announce_xdnd_for_window')
	assert x11_backend_source.contains('C.XChangeProperty')
	assert x11_backend_source.contains('C.XConvertSelection')
	assert x11_backend_source.contains('C.XSendEvent')
	assert x11_backend_source.contains('#flag linux -lxcb')
	assert x11_helper_source.contains('#include <xcb/xcb.h>')
	assert x11_helper_source.contains('#include <X11/Xlib-xcb.h>')
	assert x11_helper_source.contains('XGetXCBConnection(display)')
	assert !x11_helper_source.contains('XSetEventQueueOwner')
	assert x11_helper_source.contains('xcb_connect(display_name, NULL)')
	assert x11_helper_source.contains('xcb_send_event_checked')
	assert x11_helper_source.contains('xcb_request_check')
	assert x11_helper_source.contains('xcb_disconnect(connection)')
	assert !x11_helper_source.contains('XSetErrorHandler')
	assert x11_backend_source.contains('C.XFilterEvent(&event, X11NativeWindow(0))')
	assert_source_order(x11_backend_source, 'C.XNextEvent(backend.display, &event)',
		'C.XFilterEvent(&event, X11NativeWindow(0))')
	assert_source_order(x11_backend_source, 'C.XFilterEvent(&event, X11NativeWindow(0))',
		'event_type := unsafe { event.@type }')
	assert x11_backend_source.contains('queued_xdnd_client_message_events')
	assert x11_backend_source.contains('queued_xdnd_selection_events')
	assert x11_backend_source.contains('send_xdnd_status')
	assert x11_backend_source.contains('send_xdnd_finished')
	assert x11_backend_source.contains('.files_dropped')
	assert x11_backend_source.contains('dropped_files_from_uri_list(drop.data.bytestr())')
	assert x11_backend_source.contains('const x11_xdnd_max_payload_bytes = 1024 * 1024')
	assert x11_backend_source.contains('const x11_xdnd_max_payload_units = (x11_xdnd_max_payload_bytes + 3) / 4')
	assert x11_backend_source.contains('const x11_xdnd_max_type_atoms = 64')
	assert x11_backend_source.contains('const x11_xdnd_timeout_ns = i64(2_000_000_000)')
	assert x11_backend_source.contains('fn (mut backend X11Backend) poll_queued_events')
	assert x11_backend_source.contains('queued_input_event')
	assert x11_backend_source.contains('C.v_multiwindow_x11_is_notify_grab_or_ungrab')
	assert x11_backend_source.contains('queued_key_press_event')
	assert x11_backend_source.contains('queued_key_release_event')
	assert_source_order(x11_backend_source, 'events << queued_input_event(input)',
		'events << queued_input_event(backend.input_char_event')
	assert x11_backend_source.contains('fn (backend &X11Backend) input_char_event')
	assert x11_backend_source.contains('const x11_inline_char_codes = 8')
	assert x11_backend_source.contains('fn (backend &X11Backend) append_key_press_char_events')
	assert x11_backend_source.contains('required_codes &int')
	assert !x11_backend_source.contains('const x11_max_char_codes = 32768')
	assert !x11_backend_source.contains('[]u32{len: x11_max_char_codes}')
	assert x11_helper_source.contains('required_codes != NULL')
	assert x11_helper_source.contains('*required_codes = total_count')
	assert x11_helper_source.contains('out_count < codes_len')
	handle_xdnd_position_body :=
		x11_backend_source.all_after('fn (mut backend X11Backend) handle_xdnd_position').all_before('fn (mut backend X11Backend) handle_xdnd_drop')
	update_xdnd_mouse_position_body :=
		x11_backend_source.all_after('fn (mut backend X11Backend) update_xdnd_mouse_position').all_before('fn (mut backend X11Backend) queued_xdnd_selection_events')
	assert handle_xdnd_position_body.contains('backend.update_xdnd_mouse_position(event)')
	assert_source_order(handle_xdnd_position_body, 'backend.update_xdnd_mouse_position(event)',
		'backend.send_xdnd_status')
	assert update_xdnd_mouse_position_body.contains('root_x, root_y := x11_xdnd_position_coords(unsafe { event.xclient.data.l[2] })')
	assert update_xdnd_mouse_position_body.contains('C.XTranslateCoordinates')
	assert x11_backend_source.contains('fn x11_xdnd_position_coords(value X11NativeLong) (int, int)')
	assert x11_backend_source.contains('return x11_signed_16(packed >> 16), x11_signed_16(packed)')
	assert x11_backend_source.contains('fn x11_signed_16(value u32) int')
	xdnd_selection_body :=
		x11_backend_source.all_after('fn (mut backend X11Backend) queued_xdnd_selection_events').all_before('fn (mut backend X11Backend) queued_xdnd_property_events')
	xdnd_property_body :=
		x11_backend_source.all_after('fn (mut backend X11Backend) queued_xdnd_property_events').all_before('fn (mut backend X11Backend) begin_xdnd_incremental')
	assert xdnd_selection_body.contains('status := C.XGetWindowProperty')
	assert xdnd_selection_body.contains('X11NativeLong(x11_xdnd_max_payload_units)')
	assert !xdnd_selection_body.contains('X11NativeLong(0x7fffffff)')
	assert xdnd_selection_body.contains('0, X11NativeAtom(0), &actual_type')
	assert xdnd_selection_body.contains('actual_type == backend.clipboard_incr')
	assert xdnd_selection_body.contains('actual_format == 32')
	assert xdnd_selection_body.contains('item_count == X11NativeULong(1)')
	assert xdnd_selection_body.contains('advertised > u64(x11_xdnd_max_payload_bytes)') == false
	assert xdnd_selection_body.contains('backend.begin_xdnd_incremental(advertised)')
	assert_source_order(xdnd_selection_body, 'backend.begin_xdnd_incremental(advertised)',
		'backend.delete_xdnd_property_if_live(drop.requestor, drop.property)')
	assert xdnd_selection_body.contains('actual_type == backend.text_uri_list')
	assert xdnd_selection_body.contains('actual_format == 8')
	assert xdnd_selection_body.contains('bytes_after == X11NativeULong(0)')
	assert xdnd_selection_body.contains('item_count <= X11NativeULong(x11_xdnd_max_payload_bytes)')
	assert xdnd_selection_body.contains('backend.delete_xdnd_property_if_live(drop.requestor, drop.property)')
	assert xdnd_property_body.contains('property_state != x11_property_new_value')
	assert xdnd_property_body.contains('requestor != backend.xdnd_drop_state.requestor')
	assert xdnd_property_body.contains('property != backend.xdnd_drop_state.property')
	assert xdnd_property_body.contains('X11NativeLong(x11_xdnd_max_payload_units), 0, X11NativeAtom(0)')
	assert xdnd_property_body.contains('actual_type == backend.text_uri_list')
	assert xdnd_property_body.contains('actual_format == 8')
	assert xdnd_property_body.contains('bytes_after == X11NativeULong(0)')
	assert xdnd_property_body.contains('return backend.finish_xdnd_payload()')
	assert_source_order(xdnd_property_body,
		'events := backend.accept_xdnd_incremental_chunk(payload)',
		'if backend.xdnd_drop_state.active {')
	assert x11_backend_source.contains('remaining := x11_xdnd_max_payload_bytes - backend.xdnd_drop_state.data.len')
	assert x11_backend_source.contains('backend.xdnd_drop_state.deadline_ns = vtime.sys_mono_now() + x11_xdnd_timeout_ns')
	assert !xdnd_property_body.contains('reserved_bytes')
	xdnd_poll_body :=
		x11_backend_source.all_after('fn (mut backend X11Backend) poll_queued_events').all_before('fn (backend &X11Backend) input_event_from_record')
	assert_source_order(xdnd_poll_body, 'events << backend.queued_xdnd_property_events(&event)',
		'backend.expire_xdnd_drop(vtime.sys_mono_now())')
	assert x11_backend_source.contains('backend.purge_xdnd_window(id, native_window)')
	assert x11_backend_source.contains('backend.cancel_xdnd_drop()')
	xdnd_payload_body :=
		x11_backend_source.all_after('fn (mut backend X11Backend) finish_xdnd_payload').all_before('fn (mut backend X11Backend) xdnd_format_from_type_list')
	assert_source_order(xdnd_payload_body, 'backend.delete_xdnd_property_and_sync_if_live',
		'backend.finish_xdnd_drop_with_cleanup')
	xdnd_finish_body :=
		x11_backend_source.all_after('fn (mut backend X11Backend) finish_xdnd_drop_with_cleanup').all_before('fn (mut backend X11Backend) cancel_xdnd_drop')
	assert_source_order(xdnd_finish_body, 'backend.send_xdnd_finished_to',
		'backend.delete_xdnd_property_if_live')
	xdnd_type_list_body :=
		x11_backend_source.all_after('fn (mut backend X11Backend) xdnd_format_from_type_list').all_before('fn (backend &X11Backend) send_xdnd_status')
	assert xdnd_type_list_body.contains('C.v_multiwindow_x11_checked_property_has_atom')
	assert xdnd_type_list_body.contains('u32(x11_xdnd_max_type_atoms)')
	assert !xdnd_type_list_body.contains('C.XGetWindowProperty')
	assert !xdnd_type_list_body.contains('C.XFree')
	x11_helpers_source := multiwindow_source_file('x11_egl_backend_helpers.h')
	checked_atom_body :=
		x11_helpers_source.all_after('static inline int v_multiwindow_x11_checked_property_has_atom').all_before('static inline int v_multiwindow_x11_send_event_checked')
	assert checked_atom_body.contains('xcb_get_property(connection, 0,')
	assert checked_atom_body.contains('xcb_get_property_reply(connection, cookie, &error)')
	assert checked_atom_body.contains('error == NULL')
	assert checked_atom_body.contains('reply->type == XCB_ATOM_ATOM')
	assert checked_atom_body.contains('reply->format == 32')
	assert checked_atom_body.contains('reply->bytes_after == 0')
	assert checked_atom_body.contains('reply->value_len <= max_atoms')
	assert checked_atom_body.contains('atoms[i] == (xcb_atom_t)expected')
	key_press_body :=
		x11_backend_source.all_after('fn (mut backend X11Backend) queued_key_press_event').all_before('fn (mut backend X11Backend) queued_key_release_event')
	assert !key_press_body.contains('if key_code == 0 {\n\t\t\treturn events\n\t\t}')
	assert key_press_body.contains('if key_code != 0 {')
	assert key_press_body.contains('mut inline_char_codes := [x11_inline_char_codes]u32{}')
	assert key_press_body.contains('mut required_char_codes := 0')
	assert key_press_body.contains('required_char_codes > x11_inline_char_codes')
	assert key_press_body.contains('mut char_codes := []u32{len: required_char_codes}')
	assert key_press_body.contains('C.v_multiwindow_x11_char_codes(record.xic, event,')
	assert key_press_body.contains('for i in 0 .. char_count')
	focus_out_body :=
		x11_backend_source.all_after('x11_focus_out {').all_before('x11_property_notify {')
	clear_input_state_body :=
		x11_backend_source.all_after('fn (mut backend X11Backend) clear_input_state').all_before('fn (mut backend X11Backend) queued_key_press_event')
	assert focus_out_body.contains('backend.clear_input_state(index)')
	assert_source_order(focus_out_body, 'C.v_multiwindow_x11_unset_ic_focus',
		'backend.clear_input_state(index)')
	assert_source_order(focus_out_body, 'backend.clear_input_state(index)', '.unfocused')
	assert clear_input_state_body.contains('backend.windows[index].mouse_buttons = 0')
	assert clear_input_state_body.contains('backend.windows[index].key_repeat[i] = false')
	assert_source_order(x11_backend_source, 'events << queued_input_event(input)',
		'.clipboard_pasted')
	assert_source_order(x11_backend_source, '.clipboard_pasted',
		'events << queued_input_event(backend.input_char_event')
	assert x11_backend_source.contains('queued_button_press_event')
	assert x11_backend_source.contains('queued_button_release_event')
	assert x11_backend_source.contains('queued_mouse_position_event')
	assert x11_backend_source.contains('mouse_buttons')
	assert x11_backend_source.contains('wm_state')
	assert x11_backend_source.contains('window_state')
	assert x11_backend_source.contains('x11_property_notify')
	assert x11_backend_source.contains('C.v_multiwindow_x11_property_atom')
	assert x11_backend_source.contains('C.v_multiwindow_x11_property_state')
	assert x11_backend_source.contains('x11_property_new_value')
	assert x11_backend_source.contains('x11_iconic_state')
	assert x11_backend_source.contains('.iconified')
	assert x11_backend_source.contains('x11_normal_state')
	assert x11_backend_source.contains('.restored')
	assert x11_backend_source.contains('fn (backend &X11Backend) window_state(window X11NativeWindow) ?int')
	window_state_body :=
		x11_backend_source.all_after('fn (backend &X11Backend) window_state(window X11NativeWindow) ?int').all_before('fn (mut backend X11Backend) announce_xdnd_for_window')
	assert window_state_body.contains('C.v_multiwindow_x11_checked_wm_state')
	assert window_state_body.contains('return none')
	assert !window_state_body.contains('C.XGetWindowProperty')
	checked_wm_state_body :=
		x11_helper_source.all_after('static inline int v_multiwindow_x11_checked_wm_state').all_before('static inline int v_multiwindow_x11_send_event_checked')
	for required in ['xcb_get_property_reply', 'xcb_generic_error_t', 'xcb_connection_has_error',
		'reply->type == (xcb_atom_t)wm_state', 'reply->format == 32', '2U * sizeof(uint32_t)',
		'free(reply)', 'free(error)'] {
		assert checked_wm_state_body.contains(required), 'missing checked WM_STATE invariant `${required}`'
	}
	assert !checked_wm_state_body.contains('XGetWindowProperty')

	for required_mask in ['StructureNotifyMask', 'KeyPressMask', 'KeyReleaseMask',
		'PointerMotionMask', 'ButtonPressMask', 'ButtonReleaseMask', 'FocusChangeMask',
		'EnterWindowMask', 'LeaveWindowMask', 'PropertyChangeMask'] {
		assert x11_helper_source.contains(required_mask)
	}
	assert x11_helper_source.contains('PropertyNotify')
	assert x11_helper_source.contains('v_multiwindow_x11_property_atom')
	assert x11_helper_source.contains('v_multiwindow_x11_property_state')
	assert x11_helper_source.contains('v_multiwindow_x11_event_window')
	assert x11_helper_source.contains('v_multiwindow_x11_modifiers')
	assert x11_helper_source.contains('Button1Mask')
	assert x11_helper_source.contains('Button2Mask')
	assert x11_helper_source.contains('Button3Mask')
	assert x11_helper_source.contains('v_multiwindow_x11_key_code')
	assert x11_helper_source.contains('XLookupKeysym')
	assert x11_helper_source.contains('v_multiwindow_x11_open_im')
	assert x11_helper_source.contains('XSetLocaleModifiers')
	assert x11_helper_source.contains('XOpenIM')
	assert x11_helper_source.contains('v_multiwindow_x11_create_ic')
	assert x11_helper_source.contains('XCreateIC')
	assert x11_helper_source.contains('XNInputStyle')
	assert x11_helper_source.contains('XIMPreeditNothing | XIMStatusNothing')
	assert x11_helper_source.contains('v_multiwindow_x11_destroy_ic')
	assert x11_helper_source.contains('XDestroyIC')
	assert x11_helper_source.contains('v_multiwindow_x11_set_ic_focus')
	assert x11_helper_source.contains('XSetICFocus')
	assert x11_helper_source.contains('v_multiwindow_x11_unset_ic_focus')
	assert x11_helper_source.contains('XUnsetICFocus')
	assert x11_helper_source.contains('v_multiwindow_x11_char_codes')
	assert x11_helper_source.contains('Xutf8LookupString')
	assert x11_helper_source.contains('XBufferOverflow')
	assert x11_helper_source.contains('V_MULTIWINDOW_X11_XIM_STACK_BYTES')
	assert x11_helper_source.contains('V_MULTIWINDOW_X11_XIM_MAX_BYTES')
	assert x11_helper_source.contains('char stack_buf[V_MULTIWINDOW_X11_XIM_STACK_BYTES]')
	assert x11_helper_source.contains('buf = (char *)malloc((size_t)count)')
	assert x11_helper_source.contains('free(buf)')
	assert_source_order(x11_helper_source, 'status == XBufferOverflow', 'malloc((size_t)count)')
	assert_source_order_after_marker(x11_helper_source, 'buf = (char *)malloc((size_t)count)',
		'status = 0', 'count = Xutf8LookupString')
	assert x11_helper_source.contains('v_multiwindow_x11_decode_utf8_codes')
	assert x11_helper_source.contains('v_multiwindow_x11_utf8_decode_next')
	assert !x11_helper_source.contains('char buf[128]')
	assert !x11_helper_source.contains('v_multiwindow_x11_utf8_decode_one')
	assert !x11_helper_source.contains('XLookupString')
	assert !x11_helper_source.contains('v_multiwindow_x11_keysym_to_unicode')
}

fn test_x11_clipboard_conversion_and_root_monitor_refresh_source_guard() {
	source := multiwindow_source_file('x11_backend.c.v')
	helper := multiwindow_source_file('x11_egl_backend_helpers.h')
	assert helper.contains('v_multiwindow_x11_create_clipboard_requestor')
	assert helper.contains('InputOnly, CopyFromParent, CWEventMask, &attributes')
	start_body :=
		source.all_after('fn (mut backend X11Backend) start_next_clipboard_read').all_before('fn (backend &X11Backend) service_window_readback')
	assert_source_order(start_body, 'C.v_multiwindow_x11_create_clipboard_requestor',
		'C.XConvertSelection')
	reply_body :=
		source.all_after('fn (mut backend X11Backend) handle_clipboard_selection_request').all_before('fn (mut backend X11Backend) queued_clipboard_selection_events')
	assert reply_body.contains('v_multiwindow_x11_checked_change_property')
	assert reply_body.contains('v_multiwindow_x11_checked_selection_notify')
	assert reply_body.contains('v_multiwindow_x11_checked_barrier')
	assert reply_body.contains('drain_checked_clipboard_transfer_events_with_limit')
	assert reply_body.contains('clipboard_transfer_queue')
	assert reply_body.contains('queue == .checked')
	assert_source_order(reply_body, 'v_multiwindow_x11_checked_barrier', 'if pair_active {')
	assert_source_order(reply_body, 'v_multiwindow_x11_checked_barrier',
		'pair_active := backend.clipboard_transfers.any')
	assert_source_order(reply_body, 'if pair_active {', 'mut response_property')
	assert !reply_body.contains('C.XChangeProperty')
	assert !reply_body.contains('v_multiwindow_x11_select_property_changes')
	assert !reply_body.contains('v_multiwindow_x11_send_selection_notify')
	advance_body :=
		source.all_after('fn (mut backend X11Backend) advance_clipboard_transfer').all_before('fn (mut backend X11Backend) drain_checked_clipboard_transfer_events')
	assert advance_body.contains('transfer.queue != queue')
	assert advance_body.contains('v_multiwindow_x11_checked_change_property')
	assert !advance_body.contains('C.XChangeProperty')
	assert helper.contains('xcb_change_property_checked')
	assert helper.contains('xcb_send_event_checked')
	assert helper.contains('xcb_change_window_attributes_checked')
	assert helper.contains('xcb_poll_for_event')
	assert helper.contains('xcb_get_input_focus_reply')
	selection_body :=
		source.all_after('fn (mut backend X11Backend) queued_clipboard_selection_events').all_before('fn (mut backend X11Backend) queued_clipboard_property_events')
	assert selection_body.contains('selection != backend.clipboard || target != backend.clipboard_utf8')
	assert selection_body.contains('requestor != read.requestor')
	assert selection_body.contains('property != read.property')
	property_read_body :=
		source.all_after('fn (mut backend X11Backend) queued_clipboard_property_events').all_before('fn (backend &X11Backend) clipboard_read_terminal_event')
	assert !property_read_body.contains('next_len > reserved')
	assert property_read_body.contains('clipboard_incremental_reservation_after_chunk')
	assert_source_order(property_read_body, 'clipboard_incremental_reservation_after_chunk',
		'backend.clipboard_reads[0].data <<')
	finish_body :=
		source.all_after('fn (mut backend X11Backend) finish_clipboard_read').all_before('fn (mut backend X11Backend) clear_clipboard_state')
	assert_source_order(finish_body, 'backend.destroy_clipboard_requestor(read.requestor)',
		'events << backend.start_queued_clipboard_reads()')
	assert finish_body.contains("events << backend.clipboard_read_terminal_event(failed, .failed, '', start_error)")
	purge_body :=
		source.all_after('fn (mut backend X11Backend) purge_clipboard_window').all_before('fn (mut backend X11Backend) advance_clipboard_transfer')
	assert purge_body.contains('backend.pending_clipboard_terminal_events << backend.start_queued_clipboard_reads()')
	assert !purge_body.contains('backend.start_next_clipboard_read() or {}')

	selection_clear_body :=
		source.all_after('x11_selection_clear {').all_before('x11_selection_notify {')
	assert selection_clear_body.contains('C.XGetSelectionOwner(backend.display, backend.clipboard)')
	assert selection_clear_body.contains('owner != backend.clipboard_owner_window')
	assert source.contains("backend.net_workarea = C.XInternAtom(display, c'_NET_WORKAREA', 0)")
	assert source.contains("backend.net_current_desktop = C.XInternAtom(display, c'_NET_CURRENT_DESKTOP', 0)")
	intersection_body :=
		source.all_after('fn x11_intersect_monitor_work_area').all_before('fn (mut backend X11Backend) service_monitor_snapshot')
	assert intersection_body.contains('i64(monitor.x) + i64(monitor.width)')
	assert intersection_body.contains('i64(work_area.x) + i64(work_area.width)')
	assert intersection_body.contains('i64(monitor.y) + i64(monitor.height)')
	assert intersection_body.contains('i64(work_area.y) + i64(work_area.height)')
	property_body := source.all_after('x11_property_notify {').all_before('x11_selection_clear {')
	assert property_body.contains('property == backend.net_workarea')
	assert property_body.contains('property == backend.net_current_desktop')
	assert property_body.contains('backend.monitor_snapshot_dirty = true')
	poll_body :=
		source.all_after('fn (mut backend X11Backend) poll_queued_events').all_before('fn (backend &X11Backend) input_event_from_record')
	assert poll_body.contains('backend.drain_checked_clipboard_transfer_events()')
	assert_source_order(poll_body, 'if backend.monitor_snapshot_dirty {',
		'events << backend.expire_clipboard_operations')
	assert poll_body.contains('backend.queued_randr_monitor_events() or { []QueuedEvent{} }')
	configure_body := poll_body.all_after('x11_configure_notify {').all_before('x11_map_notify {')
	assert configure_body.contains('backend.recenter_locked_pointer(index, true)')
	assert configure_body.contains('mouse_lock_center_x != width / 2')
	assert configure_body.contains('mouse_lock_center_y != height / 2')
	assert_source_order(configure_body, '!backend.recenter_locked_pointer(index, true)',
		'backend.release_mouse_lock(index)')
	assert_source_order(configure_body, 'backend.release_mouse_lock(index)',
		'backend.queued_service_state_event(index, .mouse_lock)')
	motion_body :=
		source.all_after('fn (mut backend X11Backend) queued_mouse_position_event').all_before('fn (mut backend X11Backend) update_mouse_position')
	assert motion_body.contains('backend.recenter_locked_pointer(index, false)')
	assert !motion_body.contains('mut ignored_x := 0')
	assert_source_order(motion_body, 'backend.recenter_locked_pointer(index, false)',
		'backend.release_mouse_lock(index)')
	recenter_body :=
		source.all_after('fn (mut backend X11Backend) recenter_locked_pointer').all_before('fn (mut backend X11Backend) release_mouse_lock')
	assert recenter_body.contains('mouse_lock_center_x = center_x')
	assert recenter_body.contains('mouse_lock_center_y = center_y')
	assert recenter_body.contains('if clear_delta {')
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
		'C.XMapWindow(backend.display, backend.pending_window.window)')
	assert_source_order_after_marker(x11_backend_source,
		'fn (mut backend X11Backend) create_window', 'backend.pending_window = X11WindowRecord{',
		'C.XSelectInput(backend.display, backend.pending_window.window')
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

fn test_x11_stale_window_snapshots_use_only_checked_xcb_requests() {
	helpers := multiwindow_source_file('x11_egl_backend_helpers.h')
	shared_connection_body :=
		helpers.all_after('v_multiwindow_x11_shared_connection(Display *display)').all_before('static inline int v_multiwindow_x11_shared_connection_usable')
	assert shared_connection_body.contains('XGetXCBConnection(display)')
	assert shared_connection_body.contains('xcb_connection_has_error(connection)')
	assert !helpers.contains('XSetEventQueueOwner')
	snapshot :=
		helpers.all_after('v_multiwindow_x11_checked_window_snapshot').all_before('static inline int v_multiwindow_x11_send_event_checked')
	for required in ['xcb_get_window_attributes_reply', 'xcb_get_geometry_reply',
		'xcb_generic_error_t', 'xcb_connection_has_error'] {
		assert snapshot.contains(required), 'missing checked XCB window snapshot `${required}`'
	}
	query :=
		helpers.all_after('v_multiwindow_x11_query_service_state').all_before('static inline int v_multiwindow_x11_send_net_wm_state')
	for required in ['v_multiwindow_x11_checked_window_snapshot', 'xcb_get_input_focus_reply',
		'xcb_translate_coordinates_reply', 'xcb_get_property_reply', 'xcb_generic_error_t',
		'xcb_connection_has_error'] {
		assert query.contains(required), 'missing checked XCB service-state query `${required}`'
	}
	for forbidden in ['XGetWindowAttributes', 'XGetInputFocus', 'XTranslateCoordinates',
		'XGetWindowProperty', 'XInternAtom', 'XSetErrorHandler'] {
		assert !query.contains(forbidden), 'unsafe Xlib call `${forbidden}` remains in service-state query'
	}
	wm_state :=
		helpers.all_after('v_multiwindow_x11_checked_wm_state').all_before('static inline int v_multiwindow_x11_send_event_checked')
	for required in ['xcb_get_property_reply', 'xcb_generic_error_t', 'xcb_connection_has_error'] {
		assert wm_state.contains(required), 'missing checked XCB WM_STATE query `${required}`'
	}
	assert !wm_state.contains('XGetWindowProperty')
	readback :=
		helpers.all_after('v_multiwindow_x11_readback_rgba8').all_before('#ifdef V_MULTIWINDOW_NATIVE_PROOF_TEST')
	for required in ['xcb_get_image_reply', 'xcb_generic_error_t', 'xcb_connection_has_error',
		'XGetVisualInfo', 'XCreateImage', 'XGetPixel', 'image->data = NULL', 'XDestroyImage'] {
		assert readback.contains(required), 'missing checked XCB readback `${required}`'
	}
	for forbidden in ['XGetWindowAttributes', 'XGetImage', 'XSetErrorHandler'] {
		assert !readback.contains(forbidden), 'unsafe Xlib call `${forbidden}` remains in readback'
	}
	probe :=
		helpers.all_after('v_multiwindow_x11_readback_probe').all_before('static inline int v_multiwindow_x11_owner_modal_matches')
	size :=
		helpers.all_after('v_multiwindow_x11_get_window_size').all_before('static inline unsigned long v_multiwindow_x11_create_egl_window')
	render := helpers.all_after('v_multiwindow_x11_render_snapshot').all_before('#endif')
	for body in [probe, size, render] {
		assert body.contains('v_multiwindow_x11_checked_window_snapshot')
		assert !body.contains('XGetWindowAttributes')
	}
	mouse_lock :=
		helpers.all_after('v_multiwindow_x11_acquire_mouse_lock_checked').all_before('static inline int v_multiwindow_x11_center_pointer_checked')
	for required in ['v_multiwindow_x11_shared_connection', 'xcb_grab_pointer_reply',
		'v_multiwindow_x11_checked_window_snapshot', 'xcb_warp_pointer_checked',
		'xcb_ungrab_pointer_checked', 'v_multiwindow_x11_checked_void_request_ok'] {
		assert mouse_lock.contains(required), 'missing checked same-client mouse-lock `${required}`'
	}
	for forbidden in ['XGrabPointer', 'XGetWindowAttributes', 'XWarpPointer', 'XSetErrorHandler'] {
		assert !mouse_lock.contains(forbidden), 'unsafe Xlib mouse-lock call `${forbidden}` remains'
	}
	recenter :=
		helpers.all_after('v_multiwindow_x11_center_pointer_checked').all_before('static inline int v_multiwindow_x11_set_selection_owner_checked')
	for required in ['v_multiwindow_x11_checked_window_snapshot', 'xcb_warp_pointer_checked',
		'v_multiwindow_x11_checked_void_request_ok'] {
		assert recenter.contains(required), 'missing checked same-client recenter `${required}`'
	}
	clipboard_owner :=
		helpers.all_after('v_multiwindow_x11_set_selection_owner_checked').all_before('#ifdef V_MULTIWINDOW_NATIVE_PROOF_TEST')
	for required in ['xcb_set_selection_owner_checked', 'v_multiwindow_x11_checked_void_request_ok',
		'xcb_get_selection_owner_reply'] {
		assert clipboard_owner.contains(required), 'missing checked same-client clipboard owner `${required}`'
	}
	for forbidden in ['XSetSelectionOwner', 'XGetSelectionOwner', 'XSetErrorHandler'] {
		assert !clipboard_owner.contains(forbidden), 'unsafe Xlib clipboard-owner call `${forbidden}` remains'
	}
	x11_backend := multiwindow_source_file('x11_backend.c.v')
	window_state :=
		x11_backend.all_after('fn (backend &X11Backend) window_state').all_before('fn (mut backend X11Backend) announce_xdnd_for_window')
	assert window_state.contains('v_multiwindow_x11_checked_wm_state')
	assert !window_state.contains('XGetWindowProperty')
	property_notify :=
		x11_backend.all_after('x11_property_notify {').all_before('x11_selection_request {')
	compact_property_notify := property_notify.replace_each(['\n', '', '\r', '', '\t', '', ' ',
		''])
	assert compact_property_notify.contains('backend.window_state(backend.windows[index].window)or{continue}')
	assert compact_property_notify.contains('backend.queued_observed_state_transitions(index,property)or{continue}')
	unmap_notify := x11_backend.all_after('x11_unmap_notify {').all_before('x11_destroy_notify {')
	compact_unmap_notify := unmap_notify.replace_each(['\n', '', '\r', '', '\t', '', ' ', ''])
	assert compact_unmap_notify.contains('backend.queued_service_state_event(index,.hide)or{continue}')
	poll_body :=
		x11_backend.all_after('fn (mut backend X11Backend) poll_queued_events').all_before('fn (backend &X11Backend) input_event_from_record')
	compact_poll_body := poll_body.replace_each(['\n', '', '\r', '', '\t', '', ' ', ''])
	for operation in ['.mouse_lock', '.show', '.hide'] {
		assert compact_poll_body.contains('backend.queued_service_state_event(index,${operation})or{continue}'), 'missing fail-closed event-loop observation `${operation}`'
	}
	assert compact_poll_body.count('backend.queued_service_state_event(index,.focus)or{continue}') == 2
}

fn test_x11_window_capture_capability_requires_a_positive_viewable_native_extent() {
	source := multiwindow_source_file('x11_backend.c.v')
	body := source.all_after('fn (backend &X11Backend) service_window_capture_available')
		.all_before('fn (backend &X11Backend) service_operation_capability')
	assert body.contains('C.v_multiwindow_x11_readback_probe(')
	assert body.contains('probe.attributes_available != 0')
	assert body.contains('probe.map_state == 2')
	assert body.contains('probe.actual_width > 0')
	assert body.contains('probe.actual_height > 0')
}

fn test_win32_clipboard_read_budget_survives_native_terminal_until_core_delivery() {
	mut backend := Win32Backend{}
	request := ServiceRequestId{
		app_instance: 1
		serial:       1
	}
	window := WindowId{
		app_instance: 1
		slot:         0
		generation:   1
	}
	backend.admit_clipboard_request(request, window, voidptr(usize(1)), .clipboard_read, []u16{})!
	assert backend.clipboard_pending.len == 1
	assert backend.clipboard_pending_bytes == usize(win32_clipboard_max_pending_bytes)

	event := backend.finish_clipboard_head(.ready, 'tiny read', '')
	assert backend.clipboard_pending.len == 0
	assert event.event.service.clipboard.id == request
	assert event.event.service.clipboard.status == .ready
	assert backend.clipboard_pending_bytes == usize('tiny read'.len + 1)
	assert backend.clipboard_retained.len == 1
	assert !backend.clipboard_retained[0].claimed_by_app
	mismatched := ServiceEvent{
		...event.event.service
		clipboard: ServiceClipboardResult{
			...event.event.service.clipboard
			window: WindowId{
				...window
				generation: 2
			}
		}
	}
	backend.claim_clipboard_terminal_storage(mismatched)
	backend.discard_unclaimed_clipboard_terminal_storage(mismatched)
	assert backend.clipboard_pending_bytes == usize('tiny read'.len + 1)
	assert !backend.clipboard_retained[0].claimed_by_app

	backend.claim_clipboard_terminal_storage(event.event.service)
	assert backend.clipboard_retained.len == 1
	assert backend.clipboard_retained[0].claimed_by_app
	backend.purge_clipboard_window(window)
	assert backend.clipboard_pending_bytes == usize('tiny read'.len + 1)
	backend.discard_unclaimed_clipboard_terminal_storage(event.event.service)
	assert backend.clipboard_pending_bytes == usize('tiny read'.len + 1)
	assert backend.clipboard_retained.len == 1
	backend.release_claimed_clipboard_terminal_storage(event.event.service)
	assert backend.clipboard_pending_bytes == 0
	assert backend.clipboard_retained.len == 0
	backend.release_claimed_clipboard_terminal_storage(event.event.service)
	assert backend.clipboard_pending_bytes == 0

	second_request := ServiceRequestId{
		app_instance: 1
		serial:       2
	}
	backend.admit_clipboard_request(second_request, window, voidptr(usize(1)), .clipboard_read,
		[]u16{})!
	rejected := backend.finish_clipboard_head(.ready, 'rejected read', '')
	assert backend.clipboard_pending_bytes == usize('rejected read'.len + 1)
	backend.discard_unclaimed_clipboard_terminal_storage(rejected.event.service)
	assert backend.clipboard_pending_bytes == 0
	assert backend.clipboard_retained.len == 0
	backend.discard_unclaimed_clipboard_terminal_storage(rejected.event.service)
	assert backend.clipboard_pending_bytes == 0
}

fn test_win32_clipboard_retained_storage_hooks_wrap_backend_acceptance_and_delivery() {
	backend_source := multiwindow_source_file('service_backend.v')
	delivery_source := multiwindow_source_file('event_delivery.v')
	assert backend_source.contains('backend.win32.claim_clipboard_terminal_storage(event)')
	assert backend_source.contains('backend.win32.discard_unclaimed_clipboard_terminal_storage(event)')
	assert backend_source.contains('backend.win32.release_claimed_clipboard_terminal_storage(event)')
	assert_source_order_after_marker(delivery_source, '.service {',
		'app.accept_backend_service_event_locked(event.service, delivery_token)',
		'app.backend.claim_service_event_storage(event.service)')
	assert_source_order_after_marker(delivery_source, '.service {', '} else {',
		'app.backend.discard_unaccepted_service_event_storage(event.service)')
	assert_source_order_after_marker(delivery_source,
		'fn (mut app App) complete_queued_delivery_locked',
		'app.event_deliveries.delete(event.delivery_token)',
		'app.backend.release_delivered_service_event_storage(event.service)')
}

fn test_x11_clipboard_retained_storage_hooks_are_window_exact_and_idempotent() {
	request := ServiceRequestId{
		app_instance: 1
		serial:       9
	}
	window := WindowId{
		app_instance: 1
		slot:         2
		generation:   3
	}
	other_window := WindowId{
		...window
		slot: 4
	}
	event := ServiceEvent{
		kind:      .clipboard
		window:    window
		operation: .clipboard_read
		clipboard: ServiceClipboardResult{
			id:     request
			window: window
			status: .ready
			text:   'retained'
		}
	}
	mut backend := Backend{
		kind: .x11
		x11:  X11Backend{
			clipboard_retained: [
				X11ClipboardRetainedCharge{
					request: request
					window:  window
					bytes:   x11_clipboard_max_pending_bytes
				},
			]
		}
	}
	assert backend.x11.clipboard_pending_bytes() == u64(x11_clipboard_max_pending_bytes)
	assert backend.x11.clipboard_can_reserve(0)
	assert !backend.x11.clipboard_can_reserve(1)
	backend.claim_service_event_storage(ServiceEvent{
		...event
		clipboard: ServiceClipboardResult{
			...event.clipboard
			window: other_window
		}
	})
	assert !backend.x11.clipboard_retained[0].claimed_by_app
	backend.claim_service_event_storage(event)
	assert backend.x11.clipboard_retained[0].claimed_by_app
	backend.discard_unaccepted_service_event_storage(event)
	assert backend.x11.clipboard_retained.len == 1
	backend.release_delivered_service_event_storage(event)
	assert backend.x11.clipboard_retained.len == 0
	backend.release_delivered_service_event_storage(event)
	assert backend.x11.clipboard_retained.len == 0

	backend.x11.clipboard_retained << X11ClipboardRetainedCharge{
		request: request
		window:  window
		bytes:   7
	}
	backend.discard_unaccepted_service_event_storage(ServiceEvent{
		...event
		window: other_window
	})
	assert backend.x11.clipboard_retained.len == 1
	backend.discard_unaccepted_service_event_storage(event)
	assert backend.x11.clipboard_retained.len == 0
	backend.discard_unaccepted_service_event_storage(event)
	assert backend.x11.clipboard_retained.len == 0
}

fn test_x11_clipboard_ready_storage_replacement_is_subtractive_and_bounded() {
	maximum := x11_clipboard_max_pending_bytes
	assert x11_clipboard_ready_storage_fits(u64(maximum), maximum, maximum)
	assert x11_clipboard_ready_storage_fits(u64(maximum), 1, 1)
	assert x11_clipboard_ready_storage_fits(u64(maximum), 0, 0)
	assert !x11_clipboard_ready_storage_fits(0, 1, 0)
	assert !x11_clipboard_ready_storage_fits(u64(maximum) + 1, 0, 0)
	assert !x11_clipboard_ready_storage_fits(u64(maximum), 0, 1)
	assert !x11_clipboard_ready_storage_fits(u64(maximum), maximum, maximum + 1)
	assert !x11_clipboard_ready_storage_fits(u64(maximum), -1, 0)
	assert !x11_clipboard_ready_storage_fits(u64(maximum), 0, -1)
}

fn test_gg_unbound_capture_reconciliation_is_terminal_before_facade_removal() {
	vlib_dir := os.dir(os.dir(@DIR))
	render_source := os.read_file(os.join_path(vlib_dir, 'gg',
		'multiwindow_render_impl_d_gg_multiwindow.v')) or { panic(err) }
	gg_source := os.read_file(os.join_path(vlib_dir, 'gg', 'multiwindow_d_gg_multiwindow.v')) or {
		panic(err)
	}
	reconcile_body := render_source.all_after('fn (mut app App) reconcile_unbound_managed_window_captures()')
		.all_before('fn (mut app App) request_window_capture_redraw')
	blocked_body := render_source.all_after('fn unbound_managed_window_capture_is_permanently_blocked')
		.all_before('fn (mut app App) reconcile_unbound_managed_window_captures()')
	assert reconcile_body.contains('capture.attempt_batch_epoch != 0')
	assert blocked_body.contains('capture_block_reason_prevents_future_frame(snapshot.block_reason)')
	assert reconcile_body.contains('unbound_managed_window_capture_is_permanently_blocked(producer, snapshot)')
	assert_source_order(reconcile_body, 'app.core.service_fail_window_readback(capture.id,',
		'app.pending_window_captures.delete(index)')
	poll_body :=
		gg_source.all_after('pub fn (mut app App) poll_events()').all_before('pub fn (mut app App) drain_input_events()')
	assert_source_order(poll_body, 'app.core.poll_events()!', 'app.consume_backend_teardowns()!')
	assert_source_order(poll_body, 'app.consume_backend_teardowns()!',
		'app.reconcile_unbound_managed_window_captures()!')
}

fn test_wayland_output_scale_is_staged_until_done_before_member_refresh() {
	wayland_source := multiwindow_source_file('wayland_backend.c.v')
	scale_body :=
		wayland_source.all_after('fn wayland_output_scale(').all_before("@[export: 'v_multiwindow_wayland_output_name']")
	done_body :=
		wayland_source.all_after('fn wayland_output_done(').all_before("@[export: 'v_multiwindow_wayland_output_scale']")
	refresh_body := wayland_source.all_after('fn (mut backend WaylandBackend) refresh_windows_for_output_scale')
		.all_before('fn (backend &WaylandBackend) transport_can_marshal')
	assert scale_body.contains('record.pending_scale = factor')
	assert !scale_body.contains('record.scale = factor')
	assert_source_order(done_body, 'record.scale = record.pending_scale',
		'record.publish_output_if_ready()')
	assert_source_order(done_body, 'record.publish_output_if_ready()',
		'backend.refresh_windows_for_output_scale(record.slot)')
	assert refresh_body.contains('slot in backend.windows[index].output_slots')
	assert refresh_body.contains('backend.refresh_window_output_scale(index)')
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
			assert_wayland_input_capabilities(caps)
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

fn test_wayland_hidden_windows_are_created_for_later_mapping() {
	wayland_source := multiwindow_source_file('wayland_backend.c.v')
	create_window_body :=
		wayland_source.all_after('fn (mut backend WaylandBackend) create_window').all_before('fn (mut backend WaylandBackend) destroy_window')
	compact_create_window_body := create_window_body.replace(' ', '').replace('\t', '')
	assert compact_create_window_body.contains('requested_visible:config.visible')
	assert create_window_body.contains('else if !config.visible')
	assert_source_order(create_window_body, 'else if !config.visible',
		'record.enqueue_service_state(.hide)')
	assert wayland_source.contains('fn (mut backend WaylandBackend) service_show_window')
	assert wayland_source.contains('fn (mut backend WaylandBackend) service_hide_window')
	assert wayland_source.contains('C.v_multiwindow_wayland_unmap_surface')
	assert_source_order(create_window_body, 'backend.prepare_create_transport_plan(',
		'C.wl_compositor_create_surface(compositor)')
	assert_source_order(create_window_body, 'lifecycle_flush_attempt = transport.take()',
		'C.wl_compositor_create_surface(compositor)')
	create_transport_body := wayland_source.all_after('fn (mut backend WaylandBackend) prepare_create_transport_plan')
		.all_before('fn (mut backend WaylandBackend) prepare_cleanup_transport_plan')
	assert create_transport_body.contains('operation_count := if prepare_lifecycle_buffer { 4 } else { 3 }')
	assert_source_order(create_transport_body, 'reserve_ordinals(u64(operation_count) * u64(2))',
		'materialize_prepared_wayland_transport(prepare_seed')
	assert_source_order(create_transport_body,
		'lifecycle_seed := wayland_window_operation_seed(id, 1, .display_transport)',
		'materialize_prepared_wayland_transport(lifecycle_seed')
	assert_source_order(create_window_body, 'C.wl_surface_commit(surface)',
		'backend.finish_prepared_service_request(flush_attempt)')
	assert_source_order(create_window_body,
		'backend.finish_prepared_service_request(flush_attempt)',
		'backend.finish_prepared_service_request(roundtrip_attempt)')
	assert_source_order(create_window_body,
		'backend.finish_prepared_service_request(roundtrip_attempt)',
		'backend.finish_prepared_service_request(ack_flush_attempt)')
	assert_source_order(create_window_body,
		'backend.finish_prepared_service_request(ack_flush_attempt)',
		'backend.ensure_lifecycle_buffer_with_prepared_transport(index, lifecycle_flush_attempt)')
	lifecycle_error_body :=
		create_window_body.all_after('backend.ensure_lifecycle_buffer_with_prepared_transport(index, lifecycle_flush_attempt) or {')
	assert_source_order(lifecycle_error_body, 'backend.destroy_window_slot(index)',
		'backend.consume_prepared_service_cleanup(lifecycle_flush_attempt)')
	assert !create_window_body.contains('backend.attempt_wayland_flush(')
	assert !create_window_body.contains('backend.attempt_wayland_roundtrip(')
	show_body := wayland_source.all_after('fn (mut backend WaylandBackend) service_show_window')
		.all_before('fn (mut backend WaylandBackend) service_hide_window')
	hide_body := wayland_source.all_after('fn (mut backend WaylandBackend) service_hide_window')
		.all_before('fn (mut backend WaylandBackend) service_minimize_window')
	assert show_body.contains('backend.drain_hidden_window_before_show(mut transport)!')
	assert show_body.contains('backend.prepare_window_show_handshake(index, mut transport, requested_maximized,')
	compact_show_body :=
		show_body.replace_each(['\r', '', '\n', '', '\t', '', ' ', '']).replace(',]', ']')
	assert_source_order(compact_show_body,
		'backend.prepare_service_transport_plan(seed,[.display_roundtrip,.display_flush,.display_roundtrip,.display_flush,.display_roundtrip,.display_flush,.display_roundtrip,.display_flush])!',
		'backend.drain_hidden_window_before_show(muttransport)!')
	assert_source_order(show_body, 'backend.drain_hidden_window_before_show(mut transport)!',
		'backend.prepare_window_show_handshake(index, mut transport, requested_maximized,')
	assert !show_body.contains('requested_visible = true')
	show_handshake_body := wayland_source.all_after('fn (mut backend WaylandBackend) prepare_window_show_handshake')
		.all_before('fn (mut backend WaylandBackend) reapply_parent_to_live_children')
	assert show_handshake_body.count('run_window_show_handshake_boundary(index, mut transport, true)') == 1
	assert show_handshake_body.count('run_window_show_handshake_boundary(index, mut transport, false)') == 2
	assert !show_handshake_body.contains('time.sleep')
	assert !show_handshake_body.contains('\n\tfor ')
	assert !show_handshake_body.contains('\n\twhile ')
	assert !show_handshake_body.contains('pending_egl_resize = false')
	assert_source_order(show_handshake_body, 'backend.replay_window_toplevel_attributes(index)',
		'backend.request_window_show_probe(index, first_axis, first_enabled)')
	assert_source_order(show_handshake_body,
		'backend.request_window_show_probe(index, first_axis, first_enabled)',
		'backend.run_window_show_handshake_boundary(index, mut transport, true)')
	assert_source_order(show_handshake_body,
		'backend.run_window_show_handshake_boundary(index, mut transport, true)',
		'backend.request_window_show_final_intents(index, maximized, fullscreen)')
	final_show_section :=
		show_handshake_body.all_after('backend.request_window_show_final_intents(index, maximized, fullscreen)')
	assert final_show_section.count('backend.run_window_show_handshake_boundary(index, mut transport, false)') == 1
	assert_source_order(final_show_section,
		'backend.run_window_show_handshake_boundary(index, mut transport, false)',
		'backend.finish_window_show_handshake(index, mut transport, maximized, fullscreen)')
	show_boundary_body := wayland_source.all_after('fn (mut backend WaylandBackend) run_window_show_handshake_boundary')
		.all_before('fn (mut backend WaylandBackend) ack_window_show_configure')
	assert_source_order(show_boundary_body, 'if commit {',
		'backend.commit_window_show_handshake(index)')
	assert_source_order(show_boundary_body, 'backend.commit_window_show_handshake(index)',
		'backend.execute_prepared_service_transport(flush_attempt)')
	assert_source_order(show_boundary_body,
		'backend.execute_prepared_service_transport(flush_attempt)',
		'backend.execute_prepared_service_transport(roundtrip_attempt)')
	assert !show_boundary_body.contains('backend.attempt_wayland_')
	assert show_boundary_body.count('commit_window_show_handshake(index)') == 1
	finish_show_body := wayland_source.all_after('fn (mut backend WaylandBackend) finish_window_show_handshake')
		.all_before('fn (mut backend WaylandBackend) prepare_window_show_handshake')
	assert_source_order(finish_show_body, 'flush_attempt = transport.take()',
		'backend.ack_window_show_configure(index, false)')
	assert_source_order(finish_show_body, 'backend.ack_window_show_configure(index, false)',
		'backend.flush_window_show_ack(flush_attempt)!')
	assert_source_order(finish_show_body, 'backend.flush_window_show_ack(flush_attempt)!',
		'record.configured = true')
	assert_source_order(finish_show_body, 'record.configured = true',
		'record.requested_visible = true')
	configure_callback_body := wayland_source.all_after('fn wayland_xdg_surface_configure')
		.all_before('fn wayland_xdg_toplevel_configure')
	show_configure_branch := configure_callback_body.all_after('if record.show_handshake_active {')
		.all_before('if record.hide_barrier_active {')
	assert show_configure_branch.contains('record.show_configure_serial = serial')
	assert show_configure_branch.contains('record.show_configure_received = true')
	assert show_configure_branch.contains('if record.pending_service_state_valid {')
	assert !show_configure_branch.contains('record.show_configure_state_valid = record.pending_service_state_valid')
	assert !show_configure_branch.contains('xdg_surface_ack_configure')
	show_ack_body := wayland_source.all_after('fn (mut backend WaylandBackend) ack_window_show_configure')
		.all_before('fn (mut backend WaylandBackend) abort_window_show_handshake')
	assert show_ack_body.contains('if !record.show_configure_received {')
	assert !show_ack_body.contains('show_configure_serial == 0')
	native_show_ack_body := show_ack_body.all_after('if backend.transport_can_marshal()')
	assert_source_order(native_show_ack_body, 'xdg_surface_ack_configure',
		'record.show_configure_received = false')
	hidden_configure_branch := configure_callback_body.all_after('if !record.requested_visible {')
	assert hidden_configure_branch.contains('xdg_surface_ack_configure')
	assert hide_body.contains('backend.execute_prepared_wayland_hide_barrier(barrier_attempt)')
	assert !hide_body.contains('attempt_wayland_roundtrip')
	assert_source_order(hide_body,
		'mut barrier_transport := backend.prepare_service_transport_plan(seed,',
		'hide_barrier_active = true')
	assert_source_order(hide_body,
		'backend.execute_prepared_wayland_hide_barrier(barrier_attempt)',
		'backend.release_window_render_target_for_hide(index)')
	assert_source_order(hide_body, 'backend.release_window_render_target_for_hide(index)',
		'requested_visible = false')
	assert_source_order(hide_body, 'requested_visible = false',
		'C.v_multiwindow_wayland_unmap_surface')
	assert_source_order(hide_body, 'C.v_multiwindow_wayland_unmap_surface',
		'backend.finish_prepared_service_request(flush_attempt)')
	hide_target_body := wayland_source.all_after('fn (mut backend WaylandBackend) release_window_render_target_for_hide')
		.all_before('fn (mut backend WaylandBackend) service_hide_window')
	assert_source_order(hide_target_body, 'backend.make_renderer_anchor_current(.anchor_prepare)',
		'backend.destroy_frame_callback_lifetime(mut record)')
	assert_source_order(hide_target_body, 'backend.destroy_frame_callback_lifetime(mut record)',
		'backend.release_egl_surface_ticket(record.egl_surface_ticket, old_surface)')
	actual_hide_release_body :=
		hide_target_body.all_after('release := backend.release_egl_surface_ticket(record.egl_surface_ticket, old_surface)')
	assert actual_hide_release_body.contains('if !release.claimed || !release.terminal {')
	assert_source_order(actual_hide_release_body, 'if !release.claimed || !release.terminal {',
		'record.egl_surface = unsafe { nil }')
	assert_source_order(actual_hide_release_body, 'record.egl_surface = unsafe { nil }',
		'exhaust_backend_target_generation(old_generation)')
	assert !hide_target_body.contains('destroy_wl_egl_window_lifetime')
	assert !hide_target_body.contains('pending_egl_resize = false')
	assert !hide_body.contains('pending_egl_resize = false')
	abort_show_body := wayland_source.all_after('fn (mut backend WaylandBackend) abort_window_show_handshake')
		.all_before('fn (mut backend WaylandBackend) flush_window_show_ack')
	assert !abort_show_body.contains('pending_egl_resize = false')
	apply_configure_body := wayland_source.all_after('fn (mut backend WaylandBackend) apply_pending_configure')
		.all_before('fn (mut backend WaylandBackend) ensure_window_render_target')
	assert_source_order(apply_configure_body, 'C.v_multiwindow_wayland_egl_resize_window',
		'record.pending_egl_resize = false')
	hide_barrier_body := wayland_source.all_after('fn (mut backend WaylandBackend) execute_prepared_wayland_hide_barrier')
		.all_before('fn (mut backend WaylandBackend) abandon_renderer_ownership')
	assert hide_barrier_body.contains('backend.execute_prepared_service_transport(attempt)')
	replay_body := wayland_source.all_after('fn (mut backend WaylandBackend) replay_window_toplevel_attributes(index int)')
		.all_before('fn (mut backend WaylandBackend) request_window_show_probe')
	for replay_attribute in ['record.title', 'record.app_id', 'record.owner_id', 'record.min_width',
		'record.max_width', 'record.request_server_decoration', 'record.requested_maximized',
		'record.requested_fullscreen'] {
		assert replay_body.contains(replay_attribute)
	}
	for replay_operation in ['C.v_multiwindow_wayland_xdg_toplevel_set_maximized',
		'C.v_multiwindow_wayland_xdg_toplevel_unset_maximized',
		'C.v_multiwindow_wayland_xdg_toplevel_set_fullscreen',
		'C.v_multiwindow_wayland_xdg_toplevel_unset_fullscreen'] {
		assert replay_body.contains(replay_operation)
	}
	wayland_test_source := multiwindow_source_file('service_native_wayland_test.v')
	remap_test_body := wayland_test_source.all_after('fn test_wayland_hidden_window_remaps_through_fresh_xdg_configure_cycle()')
		.all_before('fn test_wayland_unavailable_window_controls_are_not_advertised()')
	second_remap_body := remap_test_body.all_after('app.service_hide_window(window)!')
	assert second_remap_body.count('app.service_show_window(window)!') == 1
	assert second_remap_body.count('attempt_wayland_roundtrip(') == 0
	assert second_remap_body.count('_ = app.poll_events()!') == 2
	assert second_remap_body.count('second_show :=') == 1
	assert_source_order(second_remap_body, 'app.service_show_window(window)!',
		'assert app.backend.wayland.windows[index].configured')
	assert_source_order(second_remap_body, 'assert app.backend.wayland.windows[index].configured',
		'_ = app.poll_events()!')
	assert_source_order(second_remap_body, '_ = app.poll_events()!', 'second_show :=')
	assert wayland_source.contains('reapply_parent_to_live_children_on_first_map')
	assert wayland_source.contains('if was_mapped {')
	end_render_body := wayland_source.all_after('fn (mut backend WaylandBackend) end_render(frame RenderFrame)')
		.all_before('fn (mut backend WaylandBackend) ensure_lifecycle_buffers()')
	assert_source_order(end_render_body, 'reserve_ordinals(9)',
		'C.v_multiwindow_wayland_surface_frame')
	assert_source_order(end_render_body, 'swap_context := swap_ordinals.materialize',
		'C.v_multiwindow_wayland_surface_frame')
	assert_source_order(end_render_body, 'materialize_prepared_wayland_transport(frame_seed',
		'C.v_multiwindow_wayland_surface_frame')
	assert_source_order(end_render_body, 'C.v_multiwindow_linux_egl_swap_buffers',
		'backend.reapply_parent_to_live_children_on_first_map')
	final_end_render :=
		end_render_body.all_after('backend.reapply_parent_to_live_children_on_first_map')
	assert final_end_render.contains('backend.execute_prepared_wayland_transport(flush_attempt)')
	assert !end_render_body.contains('backend.attempt_wayland_flush(frame_seed)')
	lifecycle_body := wayland_source.all_after('fn (mut backend WaylandBackend) ensure_lifecycle_buffer(index int)')
		.all_before('fn (mut record WaylandWindowRecord) release_fallback_buffer')
	assert lifecycle_body.contains('record.fallback_buffer_for_extent(width, height)')
	assert_source_order(lifecycle_body,
		'record.fallback_buffers.len >= wayland_max_fallback_buffers',
		'backend.prepare_service_transport_plan(')
	assert_source_order(lifecycle_body, 'backend.prepare_service_transport_plan(',
		'C.v_multiwindow_wayland_create_shm_buffer')
	assert_source_order(lifecycle_body, 'C.v_multiwindow_wayland_attach_buffer',
		'backend.reapply_parent_to_live_children_on_first_map')
	assert_source_order(lifecycle_body, 'backend.reapply_parent_to_live_children_on_first_map',
		'backend.finish_prepared_service_request(flush_attempt)')
	assert !lifecycle_body.contains('backend.attempt_wayland_flush')
}

fn test_wayland_initial_size_is_clamped_before_mapping() {
	wayland_source := multiwindow_source_file('wayland_backend.c.v')
	create_window_body :=
		wayland_source.all_after('fn (mut backend WaylandBackend) create_window').all_before('fn (mut backend WaylandBackend) destroy_window')
	compact_create_window_body := create_window_body.replace(' ', '').replace('\t', '')
	assert wayland_source.contains('actual_size := window_size_for_config(config, config.width, config.height)')
	assert compact_create_window_body.contains('width:actual_size.width')
	assert compact_create_window_body.contains('height:actual_size.height')
	assert compact_create_window_body.contains('min_width:record_min_width')
	assert compact_create_window_body.contains('min_height:record_min_height')
	assert wayland_source.contains('width = window_extent_for_minimum(width, record.min_width)')
	assert wayland_source.contains('height = window_extent_for_minimum(height, record.min_height)')
	assert wayland_source.contains('C.v_multiwindow_wayland_xdg_toplevel_set_min_size(xdg_toplevel, i32(actual_size.width),')
	assert wayland_source.contains('i32(actual_size.height))')
	assert_source_order_after_marker(wayland_source,
		'fn (mut backend WaylandBackend) create_window',
		'actual_size := window_size_for_config(config, config.width, config.height)',
		'mut record := &WaylandWindowRecord')
	assert_source_order(compact_create_window_body, 'width:actual_size.width',
		'C.wl_surface_commit(surface)')
}

fn test_wayland_server_side_decorations_are_requested_source_guard() {
	wayland_source := multiwindow_source_file('wayland_backend.c.v')
	wayland_helper_source := multiwindow_source_file('wayland_backend_helpers.h')
	decoration_private_source := multiwindow_source_file('wayland_xdg_decoration_private.c')
	create_window_body :=
		wayland_source.all_after('fn (mut backend WaylandBackend) create_window').all_before('fn (mut backend WaylandBackend) destroy_window')
	destroy_window_record_body :=
		wayland_source.all_after('fn (mut backend WaylandBackend) destroy_window_record').all_before('fn (mut backend WaylandBackend) destroy_window_slot')
	compact_wayland_source := wayland_source.replace(' ', '').replace('\t', '')
	decoration_configure_body :=
		wayland_source.all_after("@[export: 'v_multiwindow_wayland_xdg_toplevel_decoration_configure']").all_before("@[export: 'v_multiwindow_wayland_seat_capabilities']")
	decorated_window_body :=
		wayland_source.all_after('fn (backend &WaylandBackend) has_server_side_decorated_window').all_before('fn (backend &WaylandBackend) window_native_decorations')
	window_native_decorations_body :=
		wayland_source.all_after('fn (backend &WaylandBackend) window_native_decorations').all_before('fn (record &WaylandWindowRecord) has_server_side_decoration')
	server_side_decoration_body :=
		wayland_source.all_after('fn (record &WaylandWindowRecord) has_server_side_decoration').all_before('fn (record &WaylandWindowRecord) has_client_side_decoration')
	client_side_decoration_body :=
		wayland_source.all_after('fn (record &WaylandWindowRecord) has_client_side_decoration').all_before('fn (mut backend WaylandBackend) start')

	assert wayland_source.contains('wayland_xdg_decoration_private.c')
	assert decoration_private_source.contains('#define xdg_toplevel_interface v_multiwindow_xdg_toplevel_interface')
	assert decoration_private_source.contains('#define zxdg_decoration_manager_v1_interface v_multiwindow_zxdg_decoration_manager_v1_interface')
	assert decoration_private_source.contains('#define zxdg_toplevel_decoration_v1_interface v_multiwindow_zxdg_toplevel_decoration_v1_interface')
	assert decoration_private_source.contains('#include "xdg-decoration-unstable-v1-protocol.c"')

	assert wayland_source.contains('toplevel_decoration')
	assert wayland_source.contains('toplevel_decoration_configured bool')
	assert wayland_source.contains('toplevel_decoration_mode')
	assert compact_wayland_source.contains('decoration_managervoidptr')
	assert wayland_source.contains('C.v_multiwindow_wayland_bind_xdg_decoration_manager')
	assert wayland_source.contains("C.strcmp(iface, c'zxdg_decoration_manager_v1') == 0")
	assert wayland_source.contains('backend.destroy_xdg_decoration_manager()')
	assert wayland_source.contains("v_multiwindow_wayland_xdg_toplevel_decoration_configure']")
	assert decoration_configure_body.contains('mode u32')
	assert decoration_configure_body.contains('mut record := unsafe { &WaylandWindowRecord(data) }')
	assert decoration_configure_body.contains('record.toplevel_decoration_configured = true')
	assert decoration_configure_body.contains('record.toplevel_decoration_mode = mode')
	assert decoration_configure_body.contains('record.toplevel_decoration_mode = wayland_xdg_toplevel_decoration_mode_client_side')
	assert wayland_helper_source.contains('v_multiwindow_zxdg_decoration_manager_v1_interface')
	assert wayland_helper_source.contains('v_multiwindow_zxdg_toplevel_decoration_v1_interface')
	assert wayland_helper_source.contains('void v_multiwindow_wayland_xdg_toplevel_decoration_configure(void *data, void *decoration, uint32_t mode);')
	assert wayland_helper_source.contains('v_multiwindow_wayland_xdg_toplevel_decoration_configure_trampoline')
	assert wayland_helper_source.contains('v_multiwindow_wayland_xdg_toplevel_decoration_configure(data, (void *)decoration, mode);')
	assert !wayland_helper_source.contains('(void)data;\n\t(void)decoration;\n\t(void)mode;')
	assert wayland_helper_source.contains('v_multiwindow_wayland_xdg_toplevel_decoration_set_server_side')
	assert wayland_helper_source.contains('ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE')
	assert wayland_helper_source.contains('ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE')

	assert create_window_body.contains('if !config.borderless && backend.decoration_manager != unsafe { nil }')
	assert create_window_body.contains('C.v_multiwindow_wayland_xdg_decoration_manager_get_toplevel_decoration')
	assert create_window_body.contains('C.v_multiwindow_wayland_add_xdg_toplevel_decoration_listener')
	assert create_window_body.contains('C.v_multiwindow_wayland_xdg_toplevel_decoration_set_server_side(decoration)')
	assert create_window_body.contains('record.toplevel_decoration = unsafe { voidptr(decoration) }')
	assert_source_order(create_window_body,
		'C.v_multiwindow_wayland_xdg_toplevel_decoration_set_server_side(decoration)',
		'C.wl_surface_commit(surface)')
	assert destroy_window_record_body.contains('C.v_multiwindow_wayland_xdg_toplevel_decoration_destroy')
	assert_source_order(destroy_window_record_body,
		'C.v_multiwindow_wayland_xdg_toplevel_decoration_destroy',
		'C.v_multiwindow_wayland_xdg_toplevel_destroy')
	assert wayland_source.contains('fn (backend &WaylandBackend) has_server_side_decorated_window() bool')
	assert wayland_source.contains('fn (backend &WaylandBackend) window_native_decorations(id WindowId) !bool')
	assert wayland_source.contains('fn (record &WaylandWindowRecord) has_server_side_decoration() bool')
	assert wayland_source.contains('fn (record &WaylandWindowRecord) has_client_side_decoration() bool')
	assert decorated_window_body.contains('mut has_server_side := false')
	assert decorated_window_body.contains('if record.has_client_side_decoration()')
	assert decorated_window_body.contains('return false')
	assert decorated_window_body.contains('has_server_side = true')
	assert decorated_window_body.contains('return has_server_side')
	assert window_native_decorations_body.contains('return backend.windows[index].has_server_side_decoration()')
	assert server_side_decoration_body.contains('record.toplevel_decoration_configured')
	assert server_side_decoration_body.contains('wayland_xdg_toplevel_decoration_mode_server_side')
	assert client_side_decoration_body.contains('record.toplevel_decoration == unsafe { nil }')
	assert client_side_decoration_body.contains('wayland_xdg_toplevel_decoration_mode_client_side')
}

fn test_cursor_shape_core_source_guard() {
	types_source := multiwindow_source_file('types.v')
	app_source := multiwindow_source_file('app.v')
	backend_source := multiwindow_source_file('backend.v')
	mock_source := multiwindow_source_file('mock_backend.v')
	wayland_source := multiwindow_source_file('wayland_backend.c.v')
	x11_source := multiwindow_source_file('x11_backend.c.v')
	x11_helper_source := multiwindow_source_file('x11_egl_backend_helpers.h')
	wayland_helper_source := multiwindow_source_file('wayland_backend_helpers.h')
	wayland_cursor_private_source := multiwindow_source_file('wayland_cursor_shape_private.c')
	win32_source := multiwindow_source_file('win32_backend.c.v')
	win32_helper_source := multiwindow_source_file('win32_backend_helpers.h')
	appkit_source := multiwindow_source_file('appkit_backend.c.v')
	appkit_objc_source := multiwindow_source_file('appkit_backend.m')
	appkit_helper_source := multiwindow_source_file('appkit_backend_helpers.h')
	backend_cursor_body :=
		backend_source.all_after('fn (mut backend Backend) set_window_cursor').all_before('fn (mut backend Backend) begin_window_move')
	wayland_cursor_body :=
		wayland_source.all_after('fn (mut backend WaylandBackend) set_window_cursor').all_before('fn (mut backend WaylandBackend) begin_window_move')
	wayland_registry_body :=
		wayland_source.all_after('fn wayland_registry_handle_global').all_before("@[export: 'v_multiwindow_wayland_registry_handle_global_remove']")
	wayland_seat_body :=
		wayland_source.all_after("@[export: 'v_multiwindow_wayland_seat_capabilities']").all_before("@[export: 'v_multiwindow_wayland_seat_name']")
	wayland_pointer_enter_body :=
		wayland_source.all_after("@[export: 'v_multiwindow_wayland_pointer_enter']").all_before("@[export: 'v_multiwindow_wayland_pointer_leave']")
	wayland_pointer_leave_body :=
		wayland_source.all_after("@[export: 'v_multiwindow_wayland_pointer_leave']").all_before("@[export: 'v_multiwindow_wayland_pointer_motion']")
	wayland_capabilities_body :=
		wayland_source.all_after('fn (backend &WaylandBackend) capabilities() Capabilities').all_before('fn (backend &WaylandBackend) can_deliver_drop_events')
	wayland_cursor_capability_body :=
		wayland_source.all_after('fn (backend &WaylandBackend) can_set_cursor_shapes() bool').all_before('fn (backend &WaylandBackend) has_server_side_decorated_window')
	wayland_cursor_shape_mapping_body :=
		wayland_source.all_after('fn wayland_cursor_shape_for_shape').all_before('fn (mut backend WaylandBackend) begin_window_move')

	assert types_source.contains('pub enum CursorShape {')
	for shape_name in ['default', 'pointer', 'move', 'n_resize', 's_resize', 'e_resize', 'w_resize',
		'ne_resize', 'nw_resize', 'se_resize', 'sw_resize', 'ew_resize', 'ns_resize', 'nesw_resize',
		'nwse_resize', 'grab', 'grabbing'] {
		assert types_source.contains('\t${shape_name}')
	}
	assert types_source.contains('cursor_shapes           bool')
	assert app_source.contains('pub fn (mut app App) set_window_cursor(id WindowId, shape CursorShape) !')
	assert app_source.contains('app.backend.set_window_cursor(id, shape)!')
	assert backend_cursor_body.contains('.mock { backend.mock.set_window_cursor(id, shape)! }')
	assert backend_cursor_body.contains('.x11 { backend.x11.set_window_cursor(id, shape)! }')
	assert backend_cursor_body.contains('.wayland { backend.wayland.set_window_cursor(id, shape)! }')
	assert backend_cursor_body.contains('.appkit { backend.appkit.set_window_cursor(id, shape)! }')
	assert backend_cursor_body.contains('.win32 { backend.win32.set_window_cursor(id, shape)! }')
	assert mock_source.contains('fn (mut backend MockBackend) set_window_cursor(id WindowId, shape CursorShape) !')
	assert mock_source.contains('return error(err_capability_unsupported)')

	assert wayland_source.contains('wayland_cursor_shape_private.c')
	assert wayland_source.contains('cursor_shape_manager')
	assert wayland_source.contains('cursor_shape_device')
	assert wayland_source.contains('pointer_enter_serial')
	assert wayland_source.contains('pointer_enter_serial_valid')
	assert wayland_source.contains('const wayland_cursor_shape_default = u32(1)')
	assert wayland_source.contains('const wayland_cursor_shape_pointer = u32(4)')
	assert wayland_source.contains('const wayland_cursor_shape_move = u32(13)')
	assert wayland_source.contains('const wayland_cursor_shape_nwse_resize = u32(29)')
	assert wayland_capabilities_body.contains('cursor_shapes:           backend.can_set_cursor_shapes()')
	assert wayland_cursor_capability_body.contains('return backend.pointer != unsafe { nil } && backend.cursor_shape_manager != unsafe { nil }')
	assert wayland_cursor_capability_body.contains('&& backend.cursor_shape_device != unsafe { nil }')
	assert !wayland_cursor_capability_body.contains('if !backend.started')
	assert wayland_registry_body.contains("C.strcmp(iface, c'wp_cursor_shape_manager_v1') == 0")
	assert wayland_registry_body.contains('C.v_multiwindow_wayland_bind_cursor_shape_manager')
	assert wayland_registry_body.contains('backend.ensure_cursor_shape_device()')
	assert wayland_source.contains('backend.destroy_cursor_shape_manager()')
	assert wayland_seat_body.contains('backend.ensure_cursor_shape_device()')
	assert wayland_seat_body.contains('backend.destroy_cursor_shape_device()')
	assert wayland_pointer_enter_body.contains('backend.pointer_enter_serial = serial')
	assert wayland_pointer_enter_body.contains('backend.pointer_enter_serial_valid = true')
	assert !wayland_pointer_enter_body.contains('_ = serial')
	assert wayland_pointer_leave_body.contains('backend.pointer_enter_serial_valid = false')
	assert wayland_source.contains('fn (mut backend WaylandBackend) set_window_cursor(id WindowId, shape CursorShape) !')
	assert wayland_cursor_body.contains('backend.window_record_index(id) or { return error(err_window_not_found) }')
	assert wayland_cursor_body.contains('!backend.can_set_cursor_shapes() || !backend.pointer_focused')
	assert wayland_cursor_body.contains('backend.pointer_focus != id || !backend.pointer_enter_serial_valid')
	assert wayland_cursor_body.contains('C.v_multiwindow_wayland_cursor_shape_device_set_shape')
	assert wayland_cursor_body.contains('backend.pointer_enter_serial')
	assert wayland_cursor_body.contains('wayland_cursor_shape_for_shape(shape)')
	assert_source_order(wayland_cursor_body, 'backend.prepare_service_transport_plan(',
		'C.v_multiwindow_wayland_cursor_shape_device_set_shape')
	assert_source_order(wayland_cursor_body,
		'C.v_multiwindow_wayland_cursor_shape_device_set_shape',
		'backend.finish_prepared_service_request(flush_attempt)')
	assert !wayland_cursor_body.contains('backend.attempt_wayland_flush(')
	assert wayland_cursor_body.contains('return error(err_capability_unsupported)')
	assert wayland_cursor_shape_mapping_body.contains('.pointer { wayland_cursor_shape_pointer }')
	assert wayland_cursor_shape_mapping_body.contains('.move { wayland_cursor_shape_move }')
	assert wayland_cursor_shape_mapping_body.contains('.ew_resize { wayland_cursor_shape_ew_resize }')
	assert wayland_cursor_shape_mapping_body.contains('.nwse_resize { wayland_cursor_shape_nwse_resize }')
	assert wayland_helper_source.contains('v_multiwindow_wp_cursor_shape_manager_v1_interface')
	assert wayland_helper_source.contains('v_multiwindow_wayland_bind_cursor_shape_manager')
	assert wayland_helper_source.contains('v_multiwindow_wayland_cursor_shape_manager_get_pointer')
	assert wayland_helper_source.contains('v_multiwindow_wayland_cursor_shape_device_set_shape')
	assert wayland_helper_source.contains('WP_CURSOR_SHAPE_DEVICE_V1_SET_SHAPE')
	assert os.exists(os.join_path(@DIR, 'wayland_cursor_shape_private.c'))
	assert wayland_cursor_private_source.contains('v_multiwindow_wp_cursor_shape_manager_v1_interface')
	assert wayland_cursor_private_source.contains('v_multiwindow_wp_cursor_shape_device_v1_interface')
	assert wayland_cursor_private_source.contains('"wp_cursor_shape_manager_v1"')
	assert wayland_cursor_private_source.contains('"get_pointer"')
	assert wayland_cursor_private_source.contains('"set_shape"')
	assert !wayland_cursor_private_source.contains('zwp_tablet_tool_v2_interface')

	assert x11_source.contains('cursor_shapes:      true')
	assert x11_source.contains('fn (mut backend X11Backend) set_window_cursor(id WindowId, shape CursorShape) !')
	assert x11_source.contains('C.XDefineCursor')
	assert x11_helper_source.contains('#include <X11/cursorfont.h>')
	assert x11_helper_source.contains('XCreateFontCursor')
	assert x11_helper_source.contains('XC_hand2')
	assert x11_helper_source.contains('XC_fleur')
	assert x11_helper_source.contains('XC_sb_h_double_arrow')
	assert x11_helper_source.contains('XC_sb_v_double_arrow')

	assert win32_source.contains('cursor_shapes:      true')
	assert win32_source.contains('fn (mut backend Win32Backend) set_window_cursor(id WindowId, shape CursorShape) !')
	assert win32_helper_source.contains('WM_SETCURSOR')
	assert win32_helper_source.contains('IDC_HAND')
	assert win32_helper_source.contains('IDC_SIZEALL')
	assert win32_helper_source.contains('IDC_SIZEWE')
	assert win32_helper_source.contains('IDC_SIZENS')
	assert win32_helper_source.contains('IDC_SIZENWSE')
	assert win32_helper_source.contains('IDC_SIZENESW')

	assert appkit_source.contains('cursor_shapes:      true')
	assert appkit_source.contains('fn (mut backend AppKitBackend) set_window_cursor(id WindowId, shape CursorShape) !')
	assert appkit_helper_source.contains('v_multiwindow_appkit_set_cursor_shape')
	assert appkit_objc_source.contains('- (void)cursorUpdate:(NSEvent *)event')
	assert appkit_objc_source.contains('[NSCursor pointingHandCursor]')
	assert appkit_objc_source.contains('[NSCursor openHandCursor]')
	assert appkit_objc_source.contains('[NSCursor resizeLeftRightCursor]')
	assert appkit_objc_source.contains('[NSCursor resizeUpDownCursor]')
}

fn test_wayland_interactive_move_resize_forbidden_paths_are_absent() {
	backend_source := multiwindow_source_file('backend.v')
	wayland_source := multiwindow_source_file('wayland_backend.c.v')
	wayland_helper_source := multiwindow_source_file('wayland_backend_helpers.h')
	decoration_private_source := multiwindow_source_file('wayland_xdg_decoration_private.c')

	assert !backend_source.contains('fn (mut backend Backend) poll_events()')
	assert !wayland_source.contains('fn (mut backend WaylandBackend) poll_events()')

	for forbidden in ['gtk_shell1', 'xdg_toplevel_drag_manager_v1'] {
		assert !wayland_source.contains(forbidden)
		assert !wayland_helper_source.contains(forbidden)
		assert !decoration_private_source.contains(forbidden)
	}
}

fn test_wayland_user_action_serial_is_one_shot_and_poll_generation_scoped() {
	mut record := WaylandWindowRecord{}
	record.store_user_action_serial(41, 7)
	first := record.take_user_action_serial(7) or {
		assert false, 'current-generation user-action serial was unavailable'
		return
	}
	assert first == 41
	assert !record.user_action_serial_valid
	assert record.user_action_serial == 0
	assert record.user_action_poll == 0
	if replayed := record.take_user_action_serial(7) {
		assert false, 'one-shot user-action serial replayed as ${replayed}'
	}

	record.store_user_action_serial(42, 8)
	if stale := record.take_user_action_serial(9) {
		assert false, 'stale-generation user-action serial was accepted as ${stale}'
	}
	assert !record.user_action_serial_valid
	if resurrected := record.take_user_action_serial(8) {
		assert false, 'rejected user-action serial was retained as ${resurrected}'
	}
}

fn test_wayland_poll_path_advances_user_action_generation() {
	if os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') != '1'
		|| os.getenv('VGG_MULTIWINDOW_RUNTIME_BACKEND') != 'wayland' {
		return
	}
	$if linux && sokol_wayland ? {
		mut app := new_app(backend: .wayland)!
		defer {
			app.stop() or {
				assert false, 'failed to stop Wayland poll-generation probe app: ${err.msg()}'
			}
		}
		old_window := app.create_window(title: 'Wayland old serial', width: 160, height: 120)!
		current_window := app.create_window(
			title:  'Wayland current serial'
			width:  160
			height: 120
		)!
		old_index := app.backend.wayland.window_record_index(old_window) or {
			assert false, 'old Wayland poll-generation window record was not retained'
			return
		}
		current_index := app.backend.wayland.window_record_index(current_window) or {
			assert false, 'current Wayland poll-generation window record was not retained'
			return
		}
		generation_before := app.backend.wayland.poll_generation

		app.poll_events()!

		generation_after := app.backend.wayland.poll_generation
		assert generation_after == generation_before + 1
		app.backend.wayland.windows[old_index].store_user_action_serial(61, generation_before)
		app.backend.wayland.windows[current_index].store_user_action_serial(62, generation_after)
		app.backend.wayland.expire_user_action_serials()

		assert !app.backend.wayland.windows[old_index].user_action_serial_valid
		assert app.backend.wayland.windows[old_index].user_action_serial == 0
		assert app.backend.wayland.windows[old_index].user_action_poll == 0
		assert app.backend.wayland.windows[current_index].user_action_serial_valid
		assert app.backend.wayland.windows[current_index].user_action_serial == 62
		assert app.backend.wayland.windows[current_index].user_action_poll == generation_after
		retained := app.backend.wayland.windows[current_index].take_user_action_serial(generation_after) or {
			assert false, 'current poll-generation serial was expired'
			return
		}
		assert retained == 62
		if replayed := app.backend.wayland.windows[current_index].take_user_action_serial(generation_after) {
			assert false, 'current poll-generation serial replayed as ${replayed}'
		}
		if stale := app.backend.wayland.windows[old_index].take_user_action_serial(generation_after) {
			assert false, 'expired poll-generation serial remained consumable as ${stale}'
		}
	} $else {
		assert false, 'forced Wayland runtime serial probe requires linux and -d sokol_wayland'
	}
}

fn test_wayland_resize_edges_map_to_xdg_toplevel_values() {
	edges := [WindowResizeEdge.top, .bottom, .left, .right, .top_left, .top_right, .bottom_left,
		.bottom_right]
	expected := [u32(1), 2, 4, 8, 5, 9, 6, 10]
	assert edges.len == expected.len
	for index, edge in edges {
		assert wayland_resize_edge(edge) == expected[index]
	}

	// Native xdg_toplevel move/resize delivery and display flushing remain mandatory
	// runtime-lane proofs; source layout is not evidence for those operations.
}

fn test_wayland_input_support_is_queued_with_xkb_text_and_touch_source_guard() {
	backend_source := multiwindow_source_file('backend.v')
	wayland_source := multiwindow_source_file('wayland_backend.c.v')
	wayland_helper_source := multiwindow_source_file('wayland_backend_helpers.h')
	compact_wayland_source := wayland_source.replace(' ', '').replace('\t', '').replace('\n', '')
	capabilities_body :=
		wayland_source.all_after('fn (backend &WaylandBackend) capabilities() Capabilities').all_before('fn (backend &WaylandBackend) can_deliver_drop_events')
	backend_poll_body :=
		backend_source.all_after('fn (mut backend Backend) poll_queued_events() ![]QueuedEvent').all_before('fn (mut backend Backend) event_sequence_terminal_error')
	compact_backend_poll_body :=
		backend_poll_body.replace(' ', '').replace('\t', '').replace('\n', '')

	assert compact_backend_poll_body.contains('.wayland{backend.wayland.poll_queued_events()!}')
	assert wayland_source.contains('fn (mut backend WaylandBackend) poll_queued_events() ![]QueuedEvent')
	assert wayland_source.contains('pending_events')
	assert wayland_source.contains('[]QueuedEvent')
	assert wayland_source.contains('queued_lifecycle_event')
	assert wayland_source.contains('queued_input_event')
	assert !wayland_source.contains('resize_event_pending')
	assert !wayland_source.contains('close_requested         bool')
	assert !wayland_source.contains('mut close_requested_windows := []WindowId{}')

	assert wayland_source.contains('input_events:            true')
	assert wayland_source.contains('mouse_events:            true')
	assert wayland_source.contains('keyboard_events:         true')
	assert wayland_source.contains('text_events:             true')
	assert wayland_source.contains('focus_events:            true')
	assert capabilities_body.contains('drop_events:             backend.can_deliver_drop_events()')
	assert capabilities_body.contains('touch_events:            backend.can_deliver_touch_events()')
	assert capabilities_body.contains('interactive_move_resize: backend.can_begin_interactive_move_resize()')
	assert wayland_source.contains('fn (backend &WaylandBackend) can_deliver_drop_events() bool')
	assert wayland_source.contains('fn (backend &WaylandBackend) can_deliver_touch_events() bool')
	assert wayland_source.contains('fn (backend &WaylandBackend) can_begin_interactive_move_resize() bool')
	assert wayland_source.contains('return backend.seat != unsafe { nil } && backend.data_device_manager != unsafe { nil }')
	assert wayland_source.contains('&& backend.data_device != unsafe { nil }')
	assert wayland_source.contains('return backend.seat != unsafe { nil } && backend.touch != unsafe { nil }')
	assert wayland_source.contains('return backend.seat != unsafe { nil } && backend.wm_base != unsafe { nil }')
	assert wayland_source.contains('xkb_context_new')
	assert wayland_source.contains('xkb_keymap_new_from_string')
	assert wayland_source.contains('xkb_keymap_key_repeats')
	assert wayland_source.contains('xkb_state_new')
	assert wayland_source.contains('xkb_state_update_mask')
	assert wayland_source.contains('xkb_state_key_get_utf8')
	assert wayland_source.contains('wayland_keyboard_keymap')
	keyboard_key_body :=
		wayland_source.all_after("@[export: 'v_multiwindow_wayland_keyboard_key']").all_before("@[export: 'v_multiwindow_wayland_keyboard_modifiers']")
	repeat_info_body :=
		wayland_source.all_after("@[export: 'v_multiwindow_wayland_keyboard_repeat_info']").all_before("@[export: 'v_multiwindow_wayland_keyboard_key']")
	repeat_handler_body :=
		wayland_source.all_after('fn (mut backend WaylandBackend) enqueue_due_key_repeats()').all_before('fn wayland_key_repeat_interval_ns')
	start_repeat_body :=
		wayland_source.all_after('fn (mut backend WaylandBackend) start_key_repeat').all_before('fn (mut backend WaylandBackend) stop_key_repeat')
	poll_body :=
		wayland_source.all_after('fn (mut backend WaylandBackend) poll_queued_events() ![]QueuedEvent').all_before('fn (mut record WaylandWindowRecord) enqueue_native_event')
	compact_poll_body := poll_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert compact_poll_body.count('for_,mutrecordinbackend.windows{fornative_eventinrecord.pending_events{native_events<<native_event}record.pending_events.clear()}') == 2
	keyboard_leave_body :=
		wayland_source.all_after("@[export: 'v_multiwindow_wayland_keyboard_leave']").all_before("@[export: 'v_multiwindow_wayland_keyboard_keymap']")
	clear_keys_down_body :=
		wayland_source.all_after('fn (mut backend WaylandBackend) clear_keys_down()').all_before('fn wayland_utf8_decode')
	destroy_seat_body :=
		wayland_source.all_after('fn (mut backend WaylandBackend) destroy_seat_devices()').all_before('fn (mut backend WaylandBackend) destroy_window_record')
	destroy_window_body :=
		wayland_source.all_after('fn (mut backend WaylandBackend) destroy_window_record').all_before('fn (mut backend WaylandBackend) destroy_data_offer')
	repeat_trampoline_body :=
		wayland_helper_source.all_after('v_multiwindow_wayland_keyboard_repeat_info_trampoline').all_before('static void v_multiwindow_wayland_touch_down_trampoline')
	touch_event_body :=
		wayland_source.all_after('fn (backend &WaylandBackend) touch_event_for_record').all_before('fn (backend &WaylandBackend) touch_cancel_event_for_record')
	touch_cancel_body :=
		wayland_source.all_after('fn (backend &WaylandBackend) touch_cancel_event_for_record').all_before('fn (backend &WaylandBackend) touch_mouse_position_for_record')
	touch_mouse_position_body :=
		wayland_source.all_after('fn (backend &WaylandBackend) touch_mouse_position_for_record').all_before('fn (mut record WaylandWindowRecord) update_mouse_position')
	key_repeats_body :=
		wayland_source.all_after('fn (backend &WaylandBackend) key_repeats(raw_key u32) bool').all_before('fn (backend &WaylandBackend) char_code_for_key')
	assert keyboard_key_body.contains('backend.windows[index].input_char_event')
	assert wayland_helper_source.contains('void v_multiwindow_wayland_keyboard_repeat_info(void *data, void *keyboard, int32_t rate, int32_t delay);')
	assert repeat_trampoline_body.contains('v_multiwindow_wayland_keyboard_repeat_info(data, (void *)keyboard, rate, delay);')
	assert wayland_source.contains("@[export: 'v_multiwindow_wayland_keyboard_repeat_info']")
	assert wayland_source.contains('fn C.xkb_keymap_key_repeats(keymap &C.xkb_keymap, key u32) int')
	assert key_repeats_body.contains('if backend.xkb_keymap == unsafe { nil }')
	assert key_repeats_body.contains('C.xkb_keymap_key_repeats(unsafe { &C.xkb_keymap(backend.xkb_keymap) }')
	assert key_repeats_body.contains('raw_key) != 0')
	for repeat_field in ['keyboard_repeat_rateint', 'keyboard_repeat_delayint',
		'keyboard_repeat_activebool', 'keyboard_repeat_raw_keyu32', 'keyboard_repeat_key_codeint',
		'keyboard_repeat_windowWindowId', 'keyboard_repeat_next_nsu64',
		'keyboard_repeat_interval_nsu64'] {
		assert compact_wayland_source.contains(repeat_field)
	}
	assert !wayland_source.contains('keyboard_repeat_modifiers')
	assert !wayland_source.contains('keyboard_repeat_char_code')
	assert repeat_info_body.contains('backend.keyboard_repeat_rate = if rate > 0 { rate } else { 0 }')
	assert repeat_info_body.contains('backend.keyboard_repeat_delay = if delay > 0 { delay } else { 0 }')
	assert repeat_info_body.contains('if backend.keyboard_repeat_rate == 0 {')
	assert repeat_info_body.contains('backend.stop_key_repeat()')
	assert keyboard_key_body.contains('raw_key := key + 8')
	assert keyboard_key_body.contains('backend.start_key_repeat(backend.windows[index], raw_key, key_code)')
	assert keyboard_key_body.contains('backend.stop_key_repeat_for_key(raw_key)')
	assert start_repeat_body.contains('if !backend.key_repeats(raw_key) {')
	non_repeatable_body :=
		start_repeat_body.all_after('if !backend.key_repeats(raw_key) {').all_before('if backend.keyboard_repeat_rate <= 0')
	assert non_repeatable_body.contains('return')
	assert !non_repeatable_body.contains('backend.stop_key_repeat()')
	assert_source_order(start_repeat_body, 'if !backend.key_repeats(raw_key) {',
		'backend.keyboard_repeat_active = true')
	assert poll_body.contains('backend.enqueue_due_key_repeats()')
	assert repeat_handler_body.contains('now := vtime.sys_mono_now()')
	assert repeat_handler_body.contains('if now < backend.keyboard_repeat_next_ns {')
	assert repeat_handler_body.contains('modifiers := backend.event_modifiers()')
	assert !repeat_handler_body.contains('mut record := backend.windows[index]')
	assert repeat_handler_body.contains('backend.windows[index].input_event_with_payload(.key_down')
	assert repeat_handler_body.contains('backend.keyboard_repeat_key_code, true')
	assert repeat_handler_body.contains('backend.windows[index].enqueue_native_event')
	assert repeat_handler_body.contains('char_code := backend.char_code_for_key(backend.keyboard_repeat_raw_key)')
	assert repeat_handler_body.contains('backend.windows[index].input_char_event(char_code, true')
	assert repeat_handler_body.contains('backend.keyboard_repeat_next_ns += backend.keyboard_repeat_interval_ns')
	assert repeat_handler_body.contains('if backend.keyboard_repeat_next_ns <= now {')
	assert repeat_handler_body.contains('skipped_intervals')
	assert keyboard_key_body.contains('backend.stop_key_repeat_for_key(raw_key)')
	assert keyboard_leave_body.contains('backend.stop_key_repeat()')
	assert clear_keys_down_body.contains('backend.stop_key_repeat()')
	assert destroy_seat_body.contains('backend.clear_key_repeat_info()')
	assert destroy_window_body.contains('backend.stop_key_repeat_for_window(record.id)')
	assert destroy_window_body.contains('backend.pointer_buttons = 0')
	assert_source_order(destroy_window_body, 'backend.pointer_enter_serial_valid = false',
		'backend.pointer_buttons = 0')
	assert wayland_source.contains('.clipboard_pasted')
	assert wayland_source.contains('wayland_is_clipboard_paste')
	assert wayland_source.contains('const wayland_key_v = 86')
	wayland_paste_body :=
		wayland_source.all_after('fn wayland_is_clipboard_paste').all_before('fn (backend &WaylandBackend) char_code_for_key')
	assert wayland_paste_body.contains('return key_code == wayland_key_v && modifiers == wayland_modifier_ctrl')
	assert !wayland_paste_body.contains('keyboard_modifiers')
	assert_source_order(keyboard_key_body, 'queued_input_event(input))', '.clipboard_pasted')
	assert_source_order(keyboard_key_body, '.clipboard_pasted',
		'backend.windows[index].input_char_event')
	for required_wayland_drop in ['wl_data_device_manager', 'wl_data_device', 'wl_data_offer',
		'v_multiwindow_wayland_bind_data_device_manager',
		'v_multiwindow_wayland_data_device_manager_get_data_device',
		'v_multiwindow_wayland_add_data_device_listener',
		'v_multiwindow_wayland_add_data_offer_listener', 'v_multiwindow_wayland_data_offer_accept',
		'v_multiwindow_wayland_data_offer_set_copy_action',
		'v_multiwindow_wayland_data_offer_receive', 'v_multiwindow_wayland_data_offer_finish',
		'v_multiwindow_wayland_data_offer_destroy', 'v_multiwindow_wayland_data_device_destroy',
		'v_multiwindow_wayland_data_device_manager_destroy'] {
		assert wayland_source.contains(required_wayland_drop)
			|| wayland_helper_source.contains(required_wayland_drop)
	}
	assert wayland_source.contains('wayland_data_offer_offer')
	assert wayland_source.contains('wayland_data_offer_source_actions')
	assert wayland_source.contains('backend.data_offer_source_actions = source_actions')
	assert wayland_source.contains('backend.pending_drop_source_actions = source_actions')
	assert wayland_source.contains('wayland_data_offer_action')
	assert wayland_source.contains('backend.data_offer_selected_action = dnd_action')
	assert wayland_source.contains('backend.data_offer_action_received = true')
	assert wayland_source.contains('backend.pending_drop_selected_action = dnd_action')
	assert wayland_source.contains('backend.pending_drop_action_received = true')
	assert wayland_source.contains('wayland_data_device_drop')
	assert !wayland_source.contains('read_data_offer_uri_list')
	assert wayland_source.contains('begin_pending_data_offer_drop')
	assert wayland_source.contains('drain_pending_data_offer_drop')
	assert wayland_source.contains('finish_pending_data_offer_drop')
	assert wayland_source.contains('wayland_dnd_action_allows_finish')
	assert wayland_source.contains('source_actions != wayland_dnd_action_none')
	assert wayland_source.contains('(source_actions & action) == 0')
	assert wayland_source.contains('wayland_dnd_action_copy')
	assert wayland_source.contains('wayland_dnd_action_move')
	assert wayland_source.contains('pending_drop_offer')
	assert wayland_source.contains('pending_drop_fd')
	assert wayland_source.contains('pending_drop_buffer')
	assert wayland_source.contains('pending_drop_poll_cycles')
	assert wayland_source.contains('wayland_data_offer_max_pending_poll_cycles')
	assert wayland_source.contains('pending_drop_poll_cycle_expired')
	assert wayland_source.contains('C.v_multiwindow_wayland_fd_set_nonblocking')
	assert wayland_source.contains('C.v_multiwindow_wayland_read_would_block')
	assert wayland_source.contains('pending_drop_poll_once')
	assert wayland_source.contains('pending_drop_read_once')
	assert wayland_source.contains('return C.poll(unsafe { &C.pollfd(poll_fd) }, u64(1), 0)')
	assert wayland_source.contains('for _ in 0 .. wayland_data_offer_max_read_chunks')
	assert wayland_source.contains('backend.drain_pending_data_offer_drop()')
	assert_source_order(poll_body, 'dispatch := backend.dispatch_pending_nonblocking()',
		'if !dispatch.succeeded()')
	assert_source_order(poll_body, 'if !dispatch.succeeded()',
		'deferred_error = backend.cleanup_after_fatal_dispatch(dispatch)')
	assert_source_order(poll_body,
		'deferred_error = backend.cleanup_after_fatal_dispatch(dispatch)',
		'backend.drain_clipboard_read()')
	dispatch_cleanup_body :=
		wayland_source.all_after('fn (mut backend WaylandBackend) cleanup_after_fatal_dispatch').all_before('fn (mut backend WaylandBackend) poll_queued_events')
	assert dispatch_cleanup_body.contains('backend.render_health.blocks_graphics()')
	assert dispatch_cleanup_body.contains('backend.transport_can_marshal()')
	assert dispatch_cleanup_body.contains("backend.finish_clipboard_read(.failed, '', message)")
	assert dispatch_cleanup_body.contains('backend.fail_nonterminal_portal_exports(message)')
	assert dispatch_cleanup_body.contains('backend.close_all_clipboard_sends()')
	assert dispatch_cleanup_body.contains('backend.clear_data_offer(true)')
	dispatch_message_body :=
		wayland_source.all_after('fn wayland_dispatch_failure_message').all_before('fn (mut backend WaylandBackend) fail_nonterminal_portal_exports')
	assert dispatch_message_body.contains('err_wayland_dispatch_failed')
	portal_failure_body :=
		wayland_source.all_after('fn (mut backend WaylandBackend) fail_nonterminal_portal_exports').all_before('fn (mut backend WaylandBackend) cleanup_after_fatal_dispatch')
	assert portal_failure_body.contains('if portal.terminal')
	assert portal_failure_body.contains('status: .failed')
	assert portal_failure_body.contains('backend.destroy_portal_export_at(index, !backend.transport_can_marshal())')
	assert_source_order(poll_body,
		'deferred_error = backend.cleanup_after_fatal_dispatch(dispatch)',
		'backend.drain_pending_data_offer_drop()')
	assert wayland_source.contains('.files_dropped')
	assert wayland_source.contains('dropped_files_from_uri_list(payload)')
	drop_callback_start := wayland_source.index('fn wayland_data_device_drop(data voidptr, device voidptr)') or {
		assert false, 'Wayland data-device drop callback is missing'
		0
	}
	drop_callback_body :=
		wayland_source[drop_callback_start..].all_before("@[export: 'v_multiwindow_wayland_data_device_selection']")
	assert !drop_callback_body.contains('C.read(')
	assert !drop_callback_body.contains('dropped_files_from_uri_list(payload)')
	assert !drop_callback_body.contains('v_multiwindow_wayland_data_offer_finish')
	assert drop_callback_body.contains('backend.data_offer_allows_finish()')
	assert drop_callback_body.contains('backend.begin_pending_data_offer_drop()')
	expire_body := wayland_source.all_after('fn (mut backend WaylandBackend) pending_drop_poll_cycle_expired() bool')
		.all_before('fn (mut backend WaylandBackend) pending_drop_poll_once')
	assert expire_body.contains('backend.pending_drop_poll_cycles++')
	assert expire_body.contains('wayland_data_offer_max_pending_poll_cycles')
	assert !expire_body.contains('v_multiwindow_wayland_data_offer_finish')
	assert !expire_body.contains('C.read(')
	drain_body := wayland_source.all_after('fn (mut backend WaylandBackend) drain_pending_data_offer_drop()')
		.all_before('fn (mut backend WaylandBackend) finish_pending_data_offer_drop()')
	assert drain_body.contains('if backend.pending_drop_poll_cycle_expired() {')
	assert drain_body.contains('backend.clear_data_offer(true)')
	assert drain_body.contains('!read_interrupted && backend.pending_drop_poll_cycle_expired()')
	assert_source_order(drain_body, 'poll_result := backend.pending_drop_poll_once',
		'if poll_result == 0')
	assert_source_order(drain_body, 'if poll_result == 0',
		'backend.pending_drop_poll_cycle_expired()')
	finish_body := wayland_source.all_after('fn (mut backend WaylandBackend) finish_pending_data_offer_drop()')
		.all_before('fn (mut backend WaylandBackend) destroy_data_device()')
	assert_source_order(finish_body, 'backend.pending_drop_allows_finish()',
		'C.v_multiwindow_wayland_data_offer_finish')
	assert finish_body.contains('if should_finish {')
	assert finish_body.contains('backend.clear_data_offer(true)')
	assert wayland_helper_source.contains('#include <fcntl.h>')
	assert wayland_helper_source.contains('#include <errno.h>')
	assert wayland_helper_source.contains('v_multiwindow_wayland_fd_set_nonblocking')
	assert !wayland_helper_source.contains('v_multiwindow_wayland_set_nonblocking')
	assert wayland_helper_source.contains('O_NONBLOCK')
	assert wayland_helper_source.contains('v_multiwindow_wayland_read_would_block')
	assert wayland_helper_source.contains('EAGAIN')
	assert wayland_helper_source.contains('EWOULDBLOCK')

	has_surface_routing := wayland_source.contains('window_record_index_for_surface')
		|| wayland_source.contains('window_record_index_for_native_surface')
		|| wayland_source.contains('record_index_for_surface')
	assert has_surface_routing
	assert wayland_source.contains('pointer_focus')
	assert wayland_source.contains('keyboard_focus')
	assert wayland_source.contains('WindowId')
	assert !wayland_source.contains('pointer_surface')
	assert !wayland_source.contains('keyboard_surface')

	assert wayland_helper_source.contains('wl_seat_listener')
	assert wayland_helper_source.contains('wl_pointer_listener')
	assert wayland_helper_source.contains('wl_keyboard_listener')
	assert wayland_helper_source.contains('wl_touch_listener')
	assert wayland_helper_source.contains('wl_fixed_to_double')
	assert wayland_helper_source.contains('v_multiwindow_wayland_pointer_frame_trampoline')
	assert wayland_helper_source.contains('v_multiwindow_wayland_keyboard_repeat_info_trampoline')
	assert wayland_helper_source.contains('wl_seat_get_pointer')
	assert wayland_helper_source.contains('wl_seat_get_keyboard')
	assert wayland_helper_source.contains('wl_seat_get_touch')
	uses_protocol_release := wayland_helper_source.contains('wl_pointer_release')
		|| wayland_helper_source.contains('WL_POINTER_RELEASE')
	if uses_protocol_release {
		assert wayland_helper_source.contains('wl_keyboard_release')
			|| wayland_helper_source.contains('WL_KEYBOARD_RELEASE')
		assert wayland_helper_source.contains('wl_touch_release')
			|| wayland_helper_source.contains('WL_TOUCH_RELEASE')
		assert wayland_helper_source.contains('wl_seat_release')
			|| wayland_helper_source.contains('WL_SEAT_RELEASE')
	} else {
		assert wayland_helper_source.contains('wl_pointer_destroy')
		assert wayland_helper_source.contains('wl_keyboard_destroy')
		assert wayland_helper_source.contains('wl_touch_destroy')
		assert wayland_helper_source.contains('wl_seat_destroy')
	}

	assert wayland_source.contains('mouse_scroll')
	assert wayland_source.contains('const wayland_scroll_scale = 10.0')
	assert wayland_source.contains('/ f32(wayland_scroll_scale)')
	assert wayland_source.contains('340, 344 { wayland_modifier_shift }')
	assert wayland_source.contains('341, 345 { wayland_modifier_ctrl }')
	assert wayland_source.contains('342, 346 { wayland_modifier_alt }')
	assert wayland_source.contains('343, 347 { wayland_modifier_super }')
	assert wayland_source.contains('touch_slot_for_down')
	assert wayland_source.contains('touch_slot_for_id')
	assert wayland_source.contains('touch_event_for_record')
	assert wayland_source.contains('touch_cancel_event_for_record')
	assert wayland_source.contains('touch_mouse_position_for_record')
	assert touch_event_body.contains('mouse_x, mouse_y := backend.touch_mouse_position_for_record(record, changed_slot)')
	assert touch_event_body.contains('mouse_x:            mouse_x')
	assert touch_event_body.contains('mouse_y:            mouse_y')
	assert touch_cancel_body.contains('mouse_x, mouse_y := backend.touch_mouse_position_for_record(record, -1)')
	assert touch_cancel_body.contains('mouse_x:            mouse_x')
	assert touch_cancel_body.contains('mouse_y:            mouse_y')
	assert touch_mouse_position_body.contains('changed_slot >= 0')
	assert touch_mouse_position_body.contains('touch.active && touch.window_id == record.id')
	assert touch_mouse_position_body.contains('return touch.x, touch.y')
	assert touch_mouse_position_body.contains('return record.mouse_x, record.mouse_y')
	assert wayland_source.contains('.touches_began')
	assert wayland_source.contains('.touches_moved')
	assert wayland_source.contains('.touches_ended')
	assert wayland_source.contains('.touches_cancelled')
	assert wayland_source.contains('42 { 340 }')
	assert wayland_source.contains('54 { 344 }')
	assert wayland_source.contains('29 { 341 }')
	assert wayland_source.contains('97 { 345 }')
}

fn test_wayland_registry_global_remove_and_optional_callbacks_are_guarded_source_guard() {
	wayland_source := multiwindow_source_file('wayland_backend.c.v')
	wayland_helper_source := multiwindow_source_file('wayland_backend_helpers.h')

	has_compositor_name := wayland_source.contains('compositor_name')
		|| wayland_source.contains('compositor_global_name')
	has_wm_base_name := wayland_source.contains('wm_base_name')
		|| wayland_source.contains('wm_base_global_name')
		|| wayland_source.contains('xdg_wm_base_name')
	has_seat_name := wayland_source.contains('seat_global_name')
		|| wayland_source.contains('seat_registry_name')
		|| (wayland_source.contains('seat_name') && wayland_source.contains('u32'))
	assert has_compositor_name
	assert has_wm_base_name
	assert has_seat_name
	wm_base_listener_registration :=
		wayland_source.all_after('wm_base := unsafe { &C.xdg_wm_base(backend.wm_base) }').all_before('if backend.seat != unsafe { nil }')
	assert wm_base_listener_registration.contains('C.v_multiwindow_wayland_add_xdg_wm_base_listener')
	assert wm_base_listener_registration.contains('backend.registry_listener_data()')
	assert !wm_base_listener_registration.contains('unsafe { nil }')

	ping_body :=
		wayland_source.all_after('fn wayland_xdg_wm_base_ping').all_before("@[export: 'v_multiwindow_wayland_xdg_surface_configure']")
	ping_after_nil_guard := ping_body.all_after('if data == unsafe { nil }')
	assert ping_after_nil_guard.all_before('backend := unsafe { &WaylandBackend(data) }').contains('return')
	ping_after_backend :=
		ping_after_nil_guard.all_after('backend := unsafe { &WaylandBackend(data) }')
	ping_after_transport_guard :=
		ping_after_backend.all_after('if !backend.transport_can_marshal()')
	assert ping_after_transport_guard.all_before('C.v_multiwindow_wayland_xdg_wm_base_pong').contains('return')
	assert ping_body.count('C.v_multiwindow_wayland_xdg_wm_base_pong') == 1
	assert ping_after_transport_guard.contains('C.v_multiwindow_wayland_xdg_wm_base_pong(unsafe { &C.xdg_wm_base(wm_base) }, serial)')

	global_remove_body :=
		wayland_source.all_after('fn wayland_registry_handle_global_remove').all_before("@[export: 'v_multiwindow_wayland_xdg_wm_base_ping']")
	assert global_remove_body.contains('mut backend := unsafe { &WaylandBackend(data) }')
	assert global_remove_body.contains('name == backend.')
	assert global_remove_body.contains('unsafe { nil }')
	assert global_remove_body.contains('destroy_seat_devices')
	assert !global_remove_body.contains('_ = data\n\t\t_ = registry\n\t\t_ = name')
	wm_base_remove_body :=
		global_remove_body.all_after('name == backend.wm_base_name').all_before('} else if name == backend.compositor_name')
	assert wm_base_remove_body.contains('backend.wm_base_name = 0')
	assert wm_base_remove_body.contains('backend.destroy_removed_wm_base_if_unused()')
	assert !wm_base_remove_body.contains('C.v_multiwindow_wayland_xdg_wm_base_destroy')
	compositor_remove_body := global_remove_body.all_after('name == backend.compositor_name')
	assert compositor_remove_body.contains('backend.compositor_name = 0')
	assert compositor_remove_body.contains('backend.compositor = unsafe { nil }')
	assert compositor_remove_body.contains('backend.compositor_version = 0')
	assert !compositor_remove_body.contains('if backend.windows.len == 0 && backend.compositor != unsafe { nil }')

	optional_pointer_callbacks := [
		'axis_value120',
		'axis_relative_direction',
	]
	for callback in optional_pointer_callbacks {
		trampoline_name := 'v_multiwindow_wayland_pointer_${callback}_trampoline'
		if wayland_helper_source.contains(trampoline_name) {
			assert wayland_helper_source.contains('WL_POINTER_' + callback.to_upper())
		}
	}

	assert wayland_helper_source.contains('v_multiwindow_wayland_keyboard_keymap_trampoline')
	keymap_body :=
		wayland_helper_source.all_after('v_multiwindow_wayland_keyboard_keymap_trampoline').all_before('static void v_multiwindow_wayland_keyboard_enter_trampoline')
	assert keymap_body.contains('v_multiwindow_wayland_keyboard_keymap(data, (void *)keyboard, format, fd, size);')
	assert !keymap_body.contains('close(fd);')
	assert wayland_source.contains("@[export: 'v_multiwindow_wayland_keyboard_keymap']")
	wayland_keymap_body :=
		wayland_source.all_after('fn wayland_keyboard_keymap(data voidptr, keyboard voidptr, format u32, fd int, size u32)').all_before("@[export: 'v_multiwindow_wayland_keyboard_key']")
	assert wayland_keymap_body.contains('C.close(fd)')
	assert wayland_keymap_body.contains('C.mmap')
	assert wayland_keymap_body.contains('C.munmap')
}

fn test_multiwindow_events_do_not_add_second_opt_in_source_guard() {
	vlib_dir := os.dir(os.dir(@DIR))
	sources := [
		multiwindow_source_file('types.v'),
		multiwindow_source_file('app.v'),
		multiwindow_source_file('backend.v'),
		multiwindow_source_file('mock_backend.v'),
		multiwindow_source_file('wayland_backend.c.v'),
		os.read_file(os.join_path(vlib_dir, 'gg', 'multiwindow_d_gg_multiwindow.v')) or {
			panic(err)
		},
		os.read_file(os.join_path(vlib_dir, 'gg', 'multiwindow_notd_gg_multiwindow.v')) or {
			panic(err)
		},
	]
	for source in sources {
		assert !source.contains('wayland_input')
		assert !source.contains('x_multiwindow_input')
		assert !source.contains('gg_multiwindow_events')
		assert !source.contains('gg_multiwindow_input')
		assert !source.contains('x_multiwindow_events')
	}
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
		assert !output.contains('wayland_xdg_decoration_private.c')
	} $else {
		return
	}
}

fn test_default_x_multiwindow_mock_build_does_not_link_x11_egl_or_gl() {
	$if linux {
		output := multiwindow_dump_c_flags('no_x11_deps', '')
		assert !output.contains('-lX11')
		assert !output.contains('-lxcb')
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
		assert output.contains('-lxcb')
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
		assert output.contains('wayland_xdg_decoration_private.c')
	} $else {
		return
	}
}

fn test_gg_import_only_windows_build_keeps_win32_callback_record_declaration() {
	c_source := multiwindow_emit_windows_gg_import_c()
	assert c_source.contains('struct x__multiwindow__Win32WindowRecord {')
	assert_source_order(c_source, 'struct x__multiwindow__Win32WindowRecord {',
		'VV_LOC void x__multiwindow__win32_window_close_requested(voidptr data, u64 sequence)')
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

fn all_input_event_kinds_for_test() []InputEventKind {
	return [
		InputEventKind.invalid,
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

fn input_event_for_kind_for_test(win WindowId, kind InputEventKind, frame_count u64) InputEvent {
	mut touches := [8]InputTouchPoint{}
	touches[0] = InputTouchPoint{
		identifier:       7
		pos_x:            11.5
		pos_y:            12.5
		android_tooltype: 1
		changed:          true
	}
	touches[1] = InputTouchPoint{
		identifier:       8
		pos_x:            21.5
		pos_y:            22.5
		android_tooltype: 2
		changed:          false
	}
	dropped_files := if kind == .files_dropped {
		['/tmp/a.txt', '/tmp/b.txt']
	} else {
		[]string{}
	}
	return InputEvent{
		kind:               kind
		window_id:          win
		frame_count:        frame_count
		key_code:           65
		char_code:          u32(0xe9)
		key_repeat:         true
		modifiers:          0x105
		mouse_button:       2
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
		dropped_files:      dropped_files
	}
}

fn assert_input_event_roundtrip(actual InputEvent, expected InputEvent) {
	assert actual.kind == expected.kind
	assert actual.window_id == expected.window_id
	assert actual.frame_count == expected.frame_count
	assert actual.key_code == expected.key_code
	assert actual.char_code == expected.char_code
	assert actual.key_repeat == expected.key_repeat
	assert actual.modifiers == expected.modifiers
	assert actual.mouse_button == expected.mouse_button
	assert actual.mouse_x == expected.mouse_x
	assert actual.mouse_y == expected.mouse_y
	assert actual.mouse_dx == expected.mouse_dx
	assert actual.mouse_dy == expected.mouse_dy
	assert actual.scroll_x == expected.scroll_x
	assert actual.scroll_y == expected.scroll_y
	assert actual.num_touches == expected.num_touches
	for i in 0 .. actual.num_touches {
		assert actual.touches[i].identifier == expected.touches[i].identifier
		assert actual.touches[i].pos_x == expected.touches[i].pos_x
		assert actual.touches[i].pos_y == expected.touches[i].pos_y
		assert actual.touches[i].android_tooltype == expected.touches[i].android_tooltype
		assert actual.touches[i].changed == expected.touches[i].changed
	}
	assert actual.window_width == expected.window_width
	assert actual.window_height == expected.window_height
	assert actual.framebuffer_width == expected.framebuffer_width
	assert actual.framebuffer_height == expected.framebuffer_height
	assert actual.dropped_files == expected.dropped_files
}

fn assert_input_event_kind_list_matches_types_source(kinds []InputEventKind) {
	source := multiwindow_source_file('types.v')
	enum_body := source.all_after('pub enum InputEventKind {').all_before('}')
	mut declared := []string{}
	for line in enum_body.split_into_lines() {
		name := line.trim_space()
		if name == '' || name.starts_with('//') {
			continue
		}
		declared << name
	}
	assert declared.len == kinds.len
	for i, kind in kinds {
		assert declared[i] == kind.str()
	}
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

fn multiwindow_emit_macos_multiwindow_test_c() string {
	vlib_dir := os.dir(os.dir(@DIR))
	unique := 'x_multiwindow_appkit_cgen_${os.getpid()}_${time.now().unix_nano()}'
	c_path := os.join_path(os.temp_dir(), '${unique}.c')
	target_path := os.join_path(@DIR, 'multiwindow_test.v')
	defer {
		os.rm(c_path) or {}
	}

	cmd := '${os.quoted_path(@VEXE)} -os macos -d gg_multiwindow -d sokol_metal -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(c_path)} ${os.quoted_path(target_path)}'
	result := os.execute(cmd)
	assert result.exit_code == 0, 'emit macOS multiwindow C failed
command: ${cmd}
exit_code: ${result.exit_code}
output:
${result.output}'

	return os.read_file(c_path) or { panic(err) }
}

fn multiwindow_source_file(name string) string {
	return os.read_file(os.join_path(@DIR, name)) or { panic(err) }
}

fn test_wayland_renderer_anchor_secondary_architecture_source_guard() {
	runtime_source := multiwindow_source_file('wayland_render_runtime.c.v')
	marker := 'fn (mut backend WaylandBackend) create_private_renderer_anchor'
	assert runtime_source.contains(marker)
	anchor_body :=
		runtime_source.all_after(marker).all_before('fn (mut backend WaylandBackend) create_renderer_anchor')
	// Native proof tests establish acquisition, rollback, ownership, and release order.
	for forbidden in ['wl_surface_commit', 'xdg_', 'display_flush', 'frame_callback', 'swap_buffers',
		'WindowId'] {
		assert !anchor_body.contains(forbidden)
	}

	egl_source := multiwindow_source_file('linux_egl_native_helpers.h')
	wayland_config :=
		egl_source.all_after('v_multiwindow_linux_egl_choose_wayland_config').all_before('v_multiwindow_linux_egl_get_native_visual')
	for required in ['EGL_WINDOW_BIT', 'EGL_OPENGL_BIT', 'EGL_RED_SIZE, 8', 'EGL_GREEN_SIZE, 8',
		'EGL_BLUE_SIZE, 8', 'EGL_ALPHA_SIZE, 8', 'EGL_DEPTH_SIZE, 24', 'EGL_STENCIL_SIZE, 8',
		'EGL_SAMPLE_BUFFERS, 0', 'EGL_SAMPLES, 0', 'eglGetConfigAttrib'] {
		assert wayland_config.contains(required)
	}
	assert !wayland_config.contains('EGL_PBUFFER_BIT')
	x11_config :=
		egl_source.all_after('v_multiwindow_linux_egl_choose_config').all_before('v_multiwindow_linux_egl_choose_wayland_config')
	assert x11_config.contains('EGL_WINDOW_BIT | EGL_PBUFFER_BIT')
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

$if gg_multiwindow ? || x_multiwindow_render ? {
	$if windows && sokol_d3d11 ? {
		fn assert_win32_d3d11_present_disposition(hresult i64, expected NativeRenderDisposition) {
			context := NativeOperationContext{
				domain:    .dxgi
				operation: .present
				scope:     .window_target
			}
			evidence := NativePrimitiveEvidence{
				valid_mask:   native_valid_return_value
				return_value: hresult
			}
			result := native_result_from_dxgi(context, NativePrimitiveCapture{
				actual:    evidence
				effective: evidence
			}, .none)
			assert result.disposition == expected
			assert u32(result.native_code) == u32(hresult)
		}
	}
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

fn monitor_registry_test_info(app_instance u64, key ServiceMonitorNativeKey, slot int, generation u32, available bool, name string) ServiceMonitorInfo {
	return ServiceMonitorInfo{
		native_key: key
		id:         ServiceMonitorId{
			app_instance: app_instance
			slot:         slot
			generation:   generation
		}
		name:       name
		available:  available
	}
}

fn assert_monitor_reconcile_rejected_without_mutation(mut registry ServiceRegistry, snapshot []ServiceMonitorInfo, sequence u64) {
	before := registry.monitors.clone()
	registry.reconcile_monitor_snapshot(snapshot, sequence) or {
		assert registry.monitors == before
		return
	}
	assert false, 'invalid monitor snapshot was accepted'
}

fn test_service_monitor_registry_accepts_sparse_backend_ids_and_replug() {
	instance := u64(610)
	key := ServiceMonitorNativeKey{
		kind:    .appkit_display
		numeric: 501
	}
	initial := monitor_registry_test_info(instance, key, 5, 4, true, 'sparse')
	mut registry := new_service_registry(instance, .appkit)
	registry.replace_monitors([initial])
	assert registry.monitors.len == 1
	assert registry.monitor_index(initial.id)! == 0

	registry.reconcile_monitor_snapshot([], 2) or { panic('valid sparse removal was rejected') }
	assert registry.monitors.len == 1
	assert !registry.monitors[0].available
	replugged := monitor_registry_test_info(instance, key, 5, 5, true, 'sparse-replugged')
	available := registry.reconcile_monitor_snapshot([replugged], 3) or {
		panic('valid sparse replug was rejected')
	}
	assert available.len == 1
	assert available[0].id == replugged.id
	assert registry.monitor_index(replugged.id)! == 0
	mut stale_rejected := false
	_ = registry.monitor_index(initial.id) or {
		stale_rejected = err.msg() == err_service_request_stale
		-1
	}
	assert stale_rejected
}

fn test_service_window_state_monitor_membership_is_registered_all_or_nothing() {
	app_instance := u64(614)
	available := monitor_registry_test_info(app_instance, ServiceMonitorNativeKey{
		kind:    .wayland_global
		numeric: 1001
	}, 0, 1, true, 'available')
	unavailable := monitor_registry_test_info(app_instance, ServiceMonitorNativeKey{
		kind:    .wayland_global
		numeric: 1002
	}, 1, 1, false, 'unavailable')
	current := service_window_state_with_observed_monitor_membership(ServiceWindowState{
		focused:     .off
		monitor_ids: [available.id]
	})
	mixed := service_window_state_with_registered_monitor_membership(ServiceWindowState{
		focused:                     .on
		monitor_ids:                 [available.id, unavailable.id]
		monitor_membership_observed: true
	}, [available, unavailable])
	assert mixed.focused == .on
	assert mixed.monitor_ids.len == 0
	assert !service_window_state_observes_monitor_membership(mixed)
	merged := merge_service_window_state(current, mixed)
	assert merged.focused == .on
	assert merged.monitor_ids == current.monitor_ids
	assert service_window_state_observes_monitor_membership(merged)
	missing := service_window_state_with_registered_monitor_membership(ServiceWindowState{
		monitor_ids: [available.id, ServiceMonitorId{
			app_instance: app_instance
			slot:         9
			generation:   1
		}]
	}, [available, unavailable])
	assert missing.monitor_ids.len == 0
	assert !service_window_state_observes_monitor_membership(missing)

	known_empty := service_window_state_with_registered_monitor_membership(ServiceWindowState{
		monitor_membership_observed: true
	}, [available, unavailable])
	assert service_window_state_observes_monitor_membership(known_empty)
	assert known_empty.monitor_ids.len == 0
	valid := service_window_state_with_registered_monitor_membership(ServiceWindowState{
		monitor_ids: [available.id]
	}, [available, unavailable])
	assert valid.monitor_ids == [available.id]
	assert service_window_state_observes_monitor_membership(valid)
	unknown := service_window_state_with_registered_monitor_membership(ServiceWindowState{
		focused: .on
	}, [available, unavailable])
	assert unknown.focused == .on
	assert !service_window_state_observes_monitor_membership(unknown)
}

fn test_service_monitor_registry_rejects_identity_and_transition_errors_atomically() {
	instance := u64(611)
	key := ServiceMonitorNativeKey{
		kind:    .wayland_global
		numeric: 701
	}
	initial := monitor_registry_test_info(instance, key, 5, 3, true, 'valid')
	mut registry := new_service_registry(instance, .wayland)
	registry.replace_monitors([initial])

	wrong_app := monitor_registry_test_info(instance + 1, key, 5, 3, true, 'wrong-app')
	assert_monitor_reconcile_rejected_without_mutation(mut registry, [wrong_app], 2)
	wrong_kind := monitor_registry_test_info(instance, ServiceMonitorNativeKey{
		kind:    .x11_atom
		numeric: 701
	}, 5, 3, true, 'wrong-kind')
	assert_monitor_reconcile_rejected_without_mutation(mut registry, [wrong_kind], 3)
	invalid_key := monitor_registry_test_info(instance, ServiceMonitorNativeKey{
		kind: .wayland_global
	}, 5, 3, true, 'invalid-key')
	assert_monitor_reconcile_rejected_without_mutation(mut registry, [invalid_key], 4)
	duplicate_key := monitor_registry_test_info(instance, key, 6, 1, true, 'duplicate-key')
	assert_monitor_reconcile_rejected_without_mutation(mut registry, [initial, duplicate_key], 5)
	duplicate_id := monitor_registry_test_info(instance, ServiceMonitorNativeKey{
		kind:    .wayland_global
		numeric: 702
	}, 5, 3, true, 'duplicate-id')
	assert_monitor_reconcile_rejected_without_mutation(mut registry, [initial, duplicate_id], 6)
	wrong_active_generation := monitor_registry_test_info(instance, key, 5, 4, true,
		'wrong-active-generation')
	assert_monitor_reconcile_rejected_without_mutation(mut registry, [
		wrong_active_generation,
	], 7)

	registry.reconcile_monitor_snapshot([], 8) or { panic('valid removal was rejected') }
	wrong_replug_generation := monitor_registry_test_info(instance, key, 5, 5, true,
		'wrong-replug-generation')
	assert_monitor_reconcile_rejected_without_mutation(mut registry, [
		wrong_replug_generation,
	], 9)

	before_replace := registry.monitors.clone()
	registry.replace_monitors([wrong_kind])
	assert registry.monitors == before_replace
}

fn test_service_monitor_registry_validates_backend_plan_for_reorder_and_exhaustion() {
	instance := u64(612)
	key_a := ServiceMonitorNativeKey{
		kind:    .wayland_global
		numeric: 801
	}
	key_b := ServiceMonitorNativeKey{
		kind:    .wayland_global
		numeric: 802
	}
	key_c := ServiceMonitorNativeKey{
		kind:    .wayland_global
		numeric: 803
	}
	mut registry := new_service_registry(instance, .wayland)
	registry.replace_monitors([
		monitor_registry_test_info(instance, key_a, 0, max_u32, false, 'A'),
		monitor_registry_test_info(instance, key_b, 1, 9, false, 'B'),
	])
	stale_b := registry.monitors[1].id
	available := registry.reconcile_monitor_snapshot([
		monitor_registry_test_info(instance, key_a, 1, 10, true, 'A-reappeared'),
		monitor_registry_test_info(instance, key_c, 2, 1, true, 'C'),
	], 2) or { panic('valid max-generation backend plan was rejected') }
	assert available.len == 2
	assert registry.monitors[0].id.generation == max_u32
	assert !registry.monitors[0].available
	assert registry.monitor_index(ServiceMonitorId{
		app_instance: instance
		slot:         1
		generation:   10
	})! == 1
	assert registry.monitor_index(ServiceMonitorId{
		app_instance: instance
		slot:         2
		generation:   1
	})! == 2
	mut stale_rejected := false
	_ = registry.monitor_index(stale_b) or {
		stale_rejected = err.msg() == err_service_request_stale
		-1
	}
	assert stale_rejected

	appkit_instance := u64(613)
	appkit_a := ServiceMonitorNativeKey{
		kind:    .appkit_display
		numeric: 901
	}
	appkit_b := ServiceMonitorNativeKey{
		kind:    .appkit_display
		numeric: 902
	}
	appkit_c := ServiceMonitorNativeKey{
		kind:    .appkit_display
		numeric: 903
	}
	mut appkit_registry := new_service_registry(appkit_instance, .appkit)
	appkit_registry.replace_monitors([
		monitor_registry_test_info(appkit_instance, appkit_a, 0, 4, false, 'duplicate-name'),
		monitor_registry_test_info(appkit_instance, appkit_b, 1, 9, false, 'duplicate-name'),
	])
	appkit_registry.reconcile_monitor_snapshot([
		monitor_registry_test_info(appkit_instance, appkit_c, 1, 10, true, 'duplicate-name'),
		monitor_registry_test_info(appkit_instance, appkit_a, 0, 5, true, 'duplicate-name'),
	], 2) or { panic('valid [C,A] backend plan was rejected') }
	assert appkit_registry.monitors[0].native_key == appkit_a
	assert appkit_registry.monitors[0].id.generation == 5
	assert appkit_registry.monitors[1].native_key == appkit_c
	assert appkit_registry.monitors[1].id.generation == 10

	mut exhausted_registry := new_service_registry(u64(618), .wayland)
	exhausted_key := ServiceMonitorNativeKey{
		kind:    .wayland_global
		numeric: 904
	}
	exhausted_registry.replace_monitors([
		monitor_registry_test_info(618, exhausted_key, 0, max_u32, false, 'exhausted'),
	])
	assert_monitor_reconcile_rejected_without_mutation(mut exhausted_registry, [
		monitor_registry_test_info(618, exhausted_key, 7, 99, true, 'invalid-generation'),
	], 2)
	mut appkit_empty_registry := new_service_registry(u64(619), .appkit)
	assert_monitor_reconcile_rejected_without_mutation(mut appkit_empty_registry, [
		monitor_registry_test_info(619, ServiceMonitorNativeKey{
			kind:    .appkit_display
			numeric: 905
		}, 8, 99, true, 'invalid-new-appkit-generation'),
	], 3)
	appended := exhausted_registry.reconcile_monitor_snapshot([
		monitor_registry_test_info(618, exhausted_key, 7, 1, true, 'fresh-generation'),
	], 4) or { panic('known exhausted key with fresh generation was rejected') }
	assert appended.len == 1
	assert appended[0].id == ServiceMonitorId{
		app_instance: 618
		slot:         7
		generation:   1
	}
}

fn test_service_monitor_backend_plans_are_validated_before_commit() {
	appkit_records := [
		AppKitServiceMonitorRecord{
			native_id:  11
			slot:       0
			generation: 4
			available:  false
		},
		AppKitServiceMonitorRecord{
			native_id:  22
			slot:       1
			generation: 9
			available:  false
		},
	]
	appkit_plan := appkit_plan_service_monitors(appkit_records, [
		AppKitServiceRawMonitor{
			native_id: 33
			name:      'duplicate-name'
		},
		AppKitServiceRawMonitor{
			native_id: 11
			name:      'duplicate-name'
		},
	], 614)!
	assert appkit_records[0].native_id == 11
	assert !appkit_records[0].available
	assert appkit_plan.monitors[0].id == ServiceMonitorId{
		app_instance: 614
		slot:         1
		generation:   10
	}
	assert appkit_plan.monitors[1].id == ServiceMonitorId{
		app_instance: 614
		slot:         0
		generation:   5
	}
	mut appkit_invalid_rejected := false
	_ := appkit_plan_service_monitors(appkit_records, [
		AppKitServiceRawMonitor{
			native_id: 44
		},
		AppKitServiceRawMonitor{
			native_id: 44
		},
	], 614) or {
		appkit_invalid_rejected = true
		assert appkit_records[0].native_id == 11
		assert !appkit_records[0].available
		AppKitServiceMonitorPlan{}
	}
	assert appkit_invalid_rejected, 'invalid AppKit duplicate native-key plan was accepted'

	win32_records := [
		Win32ServiceMonitorRecord{
			native_id:  51
			name:       'A'
			slot:       0
			generation: max_u32
			available:  false
		},
		Win32ServiceMonitorRecord{
			native_id:  52
			name:       'B'
			slot:       1
			generation: 9
			available:  false
		},
	]
	win32_plan := win32_plan_service_monitors(win32_records, [
		Win32ServiceRawMonitor{
			native_id: 61
			name:      'A'
		},
		Win32ServiceRawMonitor{
			native_id: 62
			name:      'C'
		},
	], 615)!
	assert win32_records[0].generation == max_u32
	assert !win32_records[0].available
	assert win32_plan.monitors[0].id == ServiceMonitorId{
		app_instance: 615
		slot:         1
		generation:   10
	}
	assert win32_plan.monitors[1].id == ServiceMonitorId{
		app_instance: 615
		slot:         2
		generation:   1
	}
	mut win32_invalid_rejected := false
	_ := win32_plan_service_monitors(win32_records, [
		Win32ServiceRawMonitor{
			native_id: 63
			name:      'duplicate-device'
		},
		Win32ServiceRawMonitor{
			native_id: 64
			name:      'duplicate-device'
		},
	], 615) or {
		win32_invalid_rejected = true
		assert win32_records[0].generation == max_u32
		assert !win32_records[0].available
		Win32ServiceMonitorPlan{}
	}
	assert win32_invalid_rejected, 'invalid Win32 duplicate native-key plan was accepted'
}

fn test_wayland_monitor_snapshot_rejects_duplicate_native_identity() {
	mut backend := WaylandBackend{
		started: true
		outputs: [
			&WaylandOutputRecord{
				slot:        0
				global_name: 71
			},
			&WaylandOutputRecord{
				slot:        1
				global_name: 71
			},
		]
	}
	backend.service_monitor_snapshot(616) or {
		assert backend.outputs[0].global_name == 71
		assert backend.outputs[1].global_name == 71
		return
	}
	assert false, 'invalid Wayland duplicate native-key snapshot was accepted'
}

fn test_wayland_cold_sparse_snapshot_accepts_backend_generation_for_unseen_slot() {
	instance := u64(617)
	mut outputs := []&WaylandOutputRecord{}
	for slot in 0 .. 5 {
		outputs << &WaylandOutputRecord{
			slot:        slot
			global_name: u32(100 + slot)
			generation:  1
			available:   false
		}
	}
	outputs << &WaylandOutputRecord{
		slot:        5
		global_name: 105
		generation:  1
		available:   true
	}
	mut backend := WaylandBackend{
		started: true
		outputs: outputs
	}
	cold := backend.service_monitor_snapshot(instance)!
	assert cold.len == 1
	assert cold[0].id.slot == 5
	key_b := cold[0].native_key
	assert key_b == ServiceMonitorNativeKey{
		kind:    .wayland_global
		numeric: 105
	}
	mut registry := new_service_registry(instance, .wayland)
	registry.replace_monitors(cold)
	assert registry.monitor_index(cold[0].id)! == 0

	backend.outputs[0].global_name = 200
	backend.outputs[0].generation = 2
	backend.outputs[0].available = true
	updated := backend.service_monitor_snapshot(instance)!
	assert updated.len == 2
	assert updated[0].id == ServiceMonitorId{
		app_instance: instance
		slot:         0
		generation:   2
	}
	key_c := updated[0].native_key
	assert key_c == ServiceMonitorNativeKey{
		kind:    .wayland_global
		numeric: 200
	}
	assert key_c != key_b
	assert updated[1].native_key == key_b
	assert updated[1].id == cold[0].id
	rejected := [updated[0],
		monitor_registry_test_info(instance, key_b, 5, 2, true, 'wrong-active-generation')]
	assert_monitor_reconcile_rejected_without_mutation(mut registry, rejected, 2)
	available := registry.reconcile_monitor_snapshot(updated, 3) or {
		panic('valid cold-sparse Wayland backend plan was rejected')
	}
	assert available.len == 2
	assert registry.monitor_index(updated[0].id)! == 1
	assert registry.monitor_index(cold[0].id)! == 0
	assert registry.monitors[0].native_key == key_b
	assert registry.monitors[1].native_key == key_c
}

fn test_invalid_monitor_service_event_does_not_mutate_registry_or_queue() {
	mut app := new_app()!
	defer {
		app.stop() or {}
	}
	before_monitors := app.services.monitors.clone()
	before_events := app.events.clone()
	invalid := monitor_registry_test_info(app.instance_id, ServiceMonitorNativeKey{
		kind:    .appkit_display
		numeric: 1001
	}, 0, 1, true, 'wrong-backend')
	app.state_mutex.lock()
	accepted := app.accept_backend_service_event_locked(ServiceEvent{
		kind:     .monitor
		monitor:  invalid
		monitors: [invalid]
	}, 1)
	app.state_mutex.unlock()
	assert !accepted.accepted
	assert app.services.monitors == before_monitors
	assert app.events == before_events
}
