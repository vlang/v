module multiwindow

fn test_service_app_id_reaches_wayland_native_adapter_state() {
	mut backend := new_backend(.wayland, false)!
	backend.configure_app(Config{
		backend: .wayland
		app_id:  'org.vlang.package2.test'
	})
	assert backend.wayland.native_app_id() == 'org.vlang.package2.test'

	mut default_backend := new_backend(.wayland, false)!
	default_backend.configure_app(Config{
		backend: .wayland
	})
	assert default_backend.wayland.native_app_id() == 'v.x.multiwindow'
}

fn test_service_cursor_support_is_shape_and_runtime_specific() {
	mut mock := new_backend(.mock, false)!
	assert mock.cursor_support(.resize_all) == .available

	x11 := new_x11_backend()
	assert x11.cursor_support(.pointer) == .available
	assert x11.cursor_support(.not_allowed) == .conditional
	assert x11.cursor_support(.resize_all) == .conditional

	mut wayland := new_wayland_backend()
	assert wayland.cursor_support(.pointer) == .conditional
	assert wayland.cursor_support(.resize_all) == .unsupported
	wayland.pointer = voidptr(1)
	wayland.cursor_shape_manager = voidptr(1)
	wayland.cursor_shape_device = voidptr(1)
	assert wayland.cursor_support(.pointer) == .available

	appkit := new_appkit_backend()
	assert appkit.cursor_support(.pointer) == .available
	assert appkit.cursor_support(.resize_all) == .conditional

	win32 := new_win32_backend()
	assert win32.cursor_support(.resize_all) == .available
}

fn test_mock_native_borrow_is_not_advertised_or_exposed() {
	mut app := new_app()!
	window := app.create_window()!
	assert app.service_operation_capability(window, .native_borrow)!.support == .unsupported
	app.with_native_window_for_gg(window, fn (borrow NativeWindowBorrow) ! {
		_ = borrow
	}) or {
		assert err.msg() == err_capability_unsupported
		app.stop()!
		return
	}
	assert false, 'mock backend exposed a native window borrow'
}

fn test_mock_image_readback_is_not_advertised_but_window_capture_remains_available() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_queued_events()!
	image := app.service_operation_capability(window, .image_readback)!
	capture := app.service_operation_capability(window, .window_capture)!
	assert image.support == .unsupported
	assert !image.asynchronous
	assert capture.support == .available
	assert capture.asynchronous
	capture_id := app.service_request_window_readback_region(window, 0, 0, 2, 1, 1)!
	capture_events := app.drain_queued_events()!
	capture_results := capture_events.filter(it.kind == .readback && it.readback.id == capture_id)
	assert capture_results.len == 1
	assert capture_results[0].readback.status == .ready
	assert capture_results[0].readback.width == 2
	assert capture_results[0].readback.height == 1
	assert capture_results[0].readback.stride == 8
	assert capture_results[0].readback.pixels_rgba8 == []u8{len: 8}
	assert app.services.readbacks.len == 0
	readbacks_before := app.services.readbacks.len
	app.service_arm_image_readback_pass_for_gg(window, 1, 1, 1) or {
		assert err.msg() == err_capability_unsupported
		assert app.services.readbacks.len == readbacks_before
		assert app.drain_queued_events()!.len == 0
		app.stop()!
		return
	}
	assert false, 'mock backend advertised or armed unsupported image readback'
}

fn invoke_mock_state_operation(mut app App, window WindowId, operation ServiceOperation) ! {
	match operation {
		.show { app.service_show_window(window)! }
		.hide { app.service_hide_window(window)! }
		.position { app.service_set_position(window, 37, 41)! }
		.minimize { app.service_minimize_window(window)! }
		.maximize { app.service_maximize_window(window)! }
		.restore { app.service_restore_window(window)! }
		.fullscreen { app.service_set_fullscreen(window, true)! }
		.mouse_lock { app.service_set_mouse_lock(window, true)! }
		else { return error(err_capability_unsupported) }
	}
}

fn test_mock_state_mutations_reserve_delivery_before_commit_and_retry_exactly_once() {
	for operation in [ServiceOperation.show, .hide, .position, .minimize, .maximize, .restore,
		.fullscreen, .mouse_lock] {
		mut app := new_app()!
		window := app.create_window(
			title:   'mock state delivery preflight ${operation}'
			visible: operation != .show
		)!
		_ = app.drain_queued_events()!
		if operation == .restore {
			app.service_maximize_window(window)!
			_ = app.drain_queued_events()!
		}
		before := app.service_window_state(window)!
		saved_delivery_token := app.next_event_delivery_token
		app.state_mutex.lock()
		app.next_event_delivery_token = 0
		app.state_mutex.unlock()
		mut rejected := false
		invoke_mock_state_operation(mut app, window, operation) or {
			assert err.msg() == err_event_delivery_exhausted
			rejected = true
		}
		assert rejected
		assert app.service_window_state(window)! == before
		assert app.drain_queued_events()!.len == 0

		app.state_mutex.lock()
		app.next_event_delivery_token = saved_delivery_token
		app.state_mutex.unlock()
		invoke_mock_state_operation(mut app, window, operation)!
		events := app.drain_queued_events()!
		assert events.len == 1
		assert events[0].kind == .service
		assert events[0].service.operation == operation
		assert app.service_window_state(window)!.sequence == events[0].sequence
		app.stop()!
	}
}

fn test_native_state_publication_reserves_delivery_before_core_registry_commit() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_queued_events()!
	before := app.service_window_state(window)!
	saved_delivery_token := app.next_event_delivery_token
	app.state_mutex.lock()
	app.next_event_delivery_token = 0
	app.state_mutex.unlock()
	mut rejected := false
	app.publish_native_state(window, .show, ServiceWindowState{
		mapping:    .mapped
		visibility: .visible
	}) or {
		assert err.msg() == err_event_delivery_exhausted
		rejected = true
	}
	assert rejected
	assert app.service_window_state(window)! == before
	assert app.drain_queued_events()!.len == 0

	app.state_mutex.lock()
	app.next_event_delivery_token = saved_delivery_token
	app.state_mutex.unlock()
	app.publish_native_state(window, .show, ServiceWindowState{
		mapping:    .mapped
		visibility: .visible
	})!
	events := app.drain_queued_events()!
	assert events.len == 1
	assert events[0].service.operation == .show
	assert app.service_window_state(window)!.sequence == events[0].sequence
	app.stop()!
}

fn test_all_specialized_drains_are_strict_prefix_projections() {
	mut app := new_app()!
	window := app.create_window()!
	app.service_show_window(window)!
	assert app.drain_service_events()!.len == 0
	assert app.drain_input_events()!.len == 0
	assert app.drain_events()!.len == 1

	app.enqueue_mock_input_for_test(InputEvent{
		kind:      .mouse_move
		window_id: window
	})!
	assert app.poll_events()! == 1
	_ = app.service_request_window_readback(window, 1, 1, 1)!
	assert app.drain_input_events()!.len == 0
	assert app.drain_readback_events()!.len == 0
	assert app.drain_service_events()!.len == 1
	assert app.drain_readback_events()!.len == 0
	assert app.drain_input_events()!.len == 1
	assert app.drain_readback_events()!.len == 1
	assert app.drain_queued_events()!.len == 0
	app.stop()!
}

fn test_delivered_terminal_requests_are_purged_once() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_events()!
	_ = app.service_request_clipboard_text(window)!
	_ = app.service_request_window_readback(window, 1, 1, 1)!
	assert app.services.pending.len == 1
	assert app.services.readbacks.len == 1
	assert app.drain_service_events()!.len == 1
	assert app.services.pending.len == 0
	assert app.services.readbacks.len == 1
	assert app.drain_readback_events()!.len == 1
	assert app.services.readbacks.len == 0
	app.stop()!
}

fn test_service_window_config_copies_preserve_owner_and_modal_relation() {
	owner := WindowId{
		app_instance: 91
		slot:         2
		generation:   7
	}
	config := WindowConfig{
		title:  'child'
		width:  320
		height: 200
		owner:  owner
		modal:  true
	}
	titled := window_config_with_title(config, 'renamed')
	sized := window_config_with_size(config, 640, 480)
	assert titled.owner == config.owner
	assert titled.modal
	assert sized.owner == config.owner
	assert sized.modal
}

fn test_service_create_rollback_does_not_leave_registry_record() {
	mut app := new_app()!
	app.state_mutex.lock()
	app.render_runtime.next_epoch = 0
	app.state_mutex.unlock()
	app.create_window(title: 'rollback') or {
		assert err.msg() == err_window_generation_exhausted
		assert app.services.windows.len == 0
		app.stop()!
		return
	}
	assert false, 'create_window unexpectedly succeeded'
}

fn test_service_monitor_identity_and_native_unknown_state() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_events()!
	ids := app.service_monitor_ids()!
	assert ids.len == 1
	info := app.service_monitor_info(ids[0])!
	assert info.geometry.value.width == 1920
	assert info.geometry.value.height == 1080

	mut foreign := new_app()!
	foreign.service_monitor_info(ids[0]) or {
		assert err.msg() == err_app_identity_mismatch
		app.stop()!
		foreign.stop()!
		return
	}
	assert false, 'foreign monitor identity was accepted'
	_ = window
}

fn test_native_registry_starts_without_mock_observations() {
	mut registry := new_service_registry(77, .x11)
	id := WindowId{
		app_instance: 77
		slot:         0
		generation:   1
	}
	registry.register_window(id, WindowConfig{ visible: true, fullscreen: true }, WindowSize{
		width:  640
		height: 480
	}, false)
	assert registry.monitors.len == 0
	state := registry.windows[0].state
	assert state.mapping == .unknown
	assert state.visibility == .unknown
	assert state.fullscreen == .unknown
	assert !registry.windows[0].metrics.metrics_available
}

fn test_service_drains_are_prefix_projections_of_canonical_queue() {
	mut app := new_app()!
	window := app.create_window()!
	app.service_show_window(window)!
	assert app.drain_service_events()!.len == 0
	lifecycle := app.drain_events()!
	assert lifecycle.len == 1
	assert lifecycle[0].kind == .window_created
	services := app.drain_service_events()!
	assert services.len == 1
	assert services[0].kind == .state
	app.stop()!
}

fn test_mock_readback_is_canonical_owned_rgba8() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_events()!
	id := app.service_request_window_readback(window, 2, 3, 9)!
	events := app.drain_queued_events()!
	assert events.len == 1
	assert events[0].kind == .readback
	result := events[0].readback
	assert result.id == id
	assert result.status == .ready
	assert result.submitted_frame == 9
	assert result.width == 2
	assert result.height == 3
	assert result.stride == 8
	assert result.pixels_rgba8.len == 24
	assert result.pixels_rgba8.all(it == 0)
	app.stop()!
}

fn test_service_readback_layout_validation_is_overflow_safe_and_atomic() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_events()!
	request_before := app.services.next_request
	delivery_before := app.next_event_delivery_token
	app.service_complete_readback(window, 600_000_000, 1, 1, [u8(0)], 1) or {
		assert err.msg() == err_readback_invalid
		assert app.services.next_request == request_before
		assert app.next_event_delivery_token == delivery_before
		assert app.services.readbacks.len == 0
		assert app.drain_queued_events()!.len == 0
	}
	assert app.services.next_request == request_before
	assert app.services.readbacks.len == 0

	readback := app.service_begin_window_readback(window)!
	request_after_begin := app.services.next_request
	delivery_after_begin := app.next_event_delivery_token
	app.service_finish_window_readback(readback, 600_000_000, 1, 1, [u8(0)], 2) or {
		assert err.msg() == err_readback_invalid
		assert app.services.next_request == request_after_begin
		assert app.next_event_delivery_token == delivery_after_begin
		assert app.services.readbacks.len == 1
		assert app.services.readbacks[0].id == readback
		assert !app.services.readbacks[0].terminal
		assert app.drain_queued_events()!.len == 0
	}
	assert app.services.readbacks.len == 1
	assert !app.services.readbacks[0].terminal

	app.service_finish_window_readback(readback, 2, 2, 12, []u8{len: 24}, 3)!
	finished := app.drain_queued_events()!
	assert finished.len == 1
	assert finished[0].kind == .readback
	assert finished[0].readback.id == readback
	assert finished[0].readback.status == .ready
	assert finished[0].readback.width == 2
	assert finished[0].readback.height == 2
	assert finished[0].readback.stride == 12
	assert finished[0].readback.pixels_rgba8.len == 24
	assert app.services.readbacks.len == 0
	request_before_exhaustion := app.services.next_request
	app.state_mutex.lock()
	saved_delivery_token := app.next_event_delivery_token
	app.next_event_delivery_token = 0
	app.state_mutex.unlock()
	app.service_complete_readback(window, 1, 1, 4, []u8{len: 4}, 4) or {
		assert err.msg() == err_event_delivery_exhausted
		assert app.services.next_request == request_before_exhaustion
		assert app.services.readbacks.len == 0
		assert app.drain_queued_events()!.len == 0
	}
	assert app.services.next_request == request_before_exhaustion
	assert app.services.readbacks.len == 0
	app.state_mutex.lock()
	app.next_event_delivery_token = saved_delivery_token
	saved_request := app.services.next_request
	app.services.next_request = 0
	delivery_before_request_exhaustion := app.next_event_delivery_token
	app.state_mutex.unlock()
	app.service_complete_readback(window, 1, 1, 4, []u8{len: 4}, 5) or {
		assert err.msg() == err_service_request_exhausted
		assert app.services.next_request == 0
		assert app.next_event_delivery_token == delivery_before_request_exhaustion
		assert app.services.readbacks.len == 0
	}
	app.state_mutex.lock()
	app.services.next_request = saved_request
	app.state_mutex.unlock()

	completed := app.service_complete_readback(window, 1, 2, 8, []u8{len: 16}, 6)!
	completed_events := app.drain_queued_events()!
	assert completed_events.len == 1
	assert completed_events[0].readback.id == completed
	assert completed_events[0].readback.stride == 8
	assert completed_events[0].readback.pixels_rgba8.len == 16
	app.stop()!
}

fn test_mock_clipboard_and_portal_delivery_exhaustion_is_atomic_and_retryable() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_queued_events()!
	app.state_mutex.lock()
	app.services.clipboard_text = 'before'
	saved_delivery_token := app.next_event_delivery_token
	app.next_event_delivery_token = 0
	app.state_mutex.unlock()
	request_before := app.services.next_request
	pending_before := app.services.pending.len
	leases_before := app.services.portal_leases.len

	mut rejected := 0
	_ = app.service_set_clipboard_text(window, 'after') or {
		assert err.msg() == err_event_delivery_exhausted
		rejected++
		ServiceRequestId{}
	}
	_ = app.service_request_clipboard_text(window) or {
		assert err.msg() == err_event_delivery_exhausted
		rejected++
		ServiceRequestId{}
	}
	_ = app.service_request_portal_parent(window) or {
		assert err.msg() == err_event_delivery_exhausted
		rejected++
		ServiceRequestId{}
	}
	assert rejected == 3
	assert app.services.next_request == request_before
	assert app.services.clipboard_text == 'before'
	assert app.services.pending.len == pending_before
	assert app.services.portal_leases.len == leases_before
	assert app.drain_queued_events()!.len == 0

	app.state_mutex.lock()
	app.next_event_delivery_token = ~u64(0)
	app.state_mutex.unlock()
	clipboard := app.service_set_clipboard_text(window, 'after')!
	assert clipboard.serial == request_before
	assert app.next_event_delivery_token == 0
	clipboard_events := app.drain_queued_events()!
	assert clipboard_events.len == 1
	assert clipboard_events[0].sequence == ~u64(0)
	assert clipboard_events[0].service.clipboard.text == 'after'
	request_after_clipboard := app.services.next_request
	pending_after_clipboard := app.services.pending.len
	leases_after_clipboard := app.services.portal_leases.len
	mut rejected_after_max := 0
	_ = app.service_set_clipboard_text(window, 'must-not-commit') or {
		assert err.msg() == err_event_delivery_exhausted
		rejected_after_max++
		ServiceRequestId{}
	}
	_ = app.service_request_clipboard_text(window) or {
		assert err.msg() == err_event_delivery_exhausted
		rejected_after_max++
		ServiceRequestId{}
	}
	_ = app.service_request_portal_parent(window) or {
		assert err.msg() == err_event_delivery_exhausted
		rejected_after_max++
		ServiceRequestId{}
	}
	assert rejected_after_max == 3
	assert app.services.next_request == request_after_clipboard
	assert app.services.clipboard_text == 'after'
	assert app.services.pending.len == pending_after_clipboard
	assert app.services.portal_leases.len == leases_after_clipboard
	assert app.drain_queued_events()!.len == 0

	app.state_mutex.lock()
	app.next_event_delivery_token = saved_delivery_token
	app.state_mutex.unlock()
	portal := app.service_request_portal_parent(window)!
	assert portal.serial == request_after_clipboard
	portal_events := app.drain_queued_events()!
	assert portal_events.len == 1
	assert portal_events[0].service.portal_parent.id == portal
	app.service_release_portal_parent(portal_events[0].service.portal_parent.lease)!
	app.stop()!
}

fn test_native_clipboard_and_portal_completion_reserve_before_terminal_mutation() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_queued_events()!
	clipboard := app.begin_native_clipboard_request(window, .clipboard_read, false)!.request
	second_clipboard := app.begin_native_clipboard_request_with_payload(window, .clipboard_write,
		false, u64('native-write'.len))!.request
	portal, lease := app.begin_portal_parent_request(window)!
	saved_delivery_token := app.next_event_delivery_token
	app.state_mutex.lock()
	app.next_event_delivery_token = 0
	app.state_mutex.unlock()

	mut rejected := 0
	app.complete_native_clipboard_request(clipboard, window, .clipboard_read, 'native', 0) or {
		assert err.msg() == err_event_delivery_exhausted
		rejected++
	}
	app.complete_native_clipboard_request(second_clipboard, window, .clipboard_write,
		'native-write', 0) or {
		assert err.msg() == err_event_delivery_exhausted
		rejected++
	}
	app.complete_portal_parent_request(portal, window, lease, 'native-parent') or {
		assert err.msg() == err_event_delivery_exhausted
		rejected++
	}
	assert rejected == 3
	assert app.services.pending.filter(it.id == clipboard && it.terminal).len == 0
	assert app.services.pending.filter(it.id == second_clipboard && it.terminal).len == 0
	assert app.services.pending.filter(it.id == portal && it.terminal).len == 0
	assert app.services.portal_leases.any(it.id == lease && it.window == window)
	assert app.drain_queued_events()!.len == 0

	app.state_mutex.lock()
	app.next_event_delivery_token = ~u64(0)
	app.state_mutex.unlock()
	app.complete_native_clipboard_request(clipboard, window, .clipboard_read, 'native', 0)!
	assert app.next_event_delivery_token == 0
	max_event := app.drain_queued_events()!
	assert max_event.len == 1
	assert max_event[0].sequence == ~u64(0)
	assert max_event[0].service.clipboard.id == clipboard
	assert app.services.pending.filter(it.id == clipboard && it.terminal).len == 0

	mut rejected_after_max := 0
	app.complete_native_clipboard_request(second_clipboard, window, .clipboard_write,
		'native-write', 0) or {
		assert err.msg() == err_event_delivery_exhausted
		rejected_after_max++
	}
	app.complete_portal_parent_request(portal, window, lease, 'native-parent') or {
		assert err.msg() == err_event_delivery_exhausted
		rejected_after_max++
	}
	assert rejected_after_max == 2
	assert app.services.pending.filter(it.id == second_clipboard && it.terminal).len == 0
	assert app.services.pending.filter(it.id == portal && it.terminal).len == 0
	assert app.services.portal_leases.any(it.id == lease && it.window == window)
	assert app.drain_queued_events()!.len == 0

	app.state_mutex.lock()
	app.next_event_delivery_token = saved_delivery_token
	app.state_mutex.unlock()
	app.complete_native_clipboard_request(second_clipboard, window, .clipboard_write,
		'native-write', 0)!
	app.complete_portal_parent_request(portal, window, lease, 'native-parent')!
	events := app.drain_queued_events()!
	assert events.len == 2
	assert events[0].service.clipboard.id == second_clipboard
	assert events[0].service.clipboard.text == 'native-write'
	assert events[1].service.portal_parent.id == portal
	assert events[1].service.portal_parent.lease == lease
	assert app.services.pending.len == 0
	app.service_release_portal_parent(lease)!
	app.stop()!
}

fn test_public_synchronous_portal_completion_exhaustion_rolls_back_and_retries() {
	mut app := new_app()!
	window := app.create_window(title: 'synchronous portal rollback')!
	_ = app.drain_queued_events()!
	original_backend := app.backend.kind
	app.backend.kind = .x11
	mut fake_x11_record := X11WindowRecord{
		id: window
	}
	fake_x11_record.window = ~fake_x11_record.window
	expected_parent := 'x11:${u64(fake_x11_record.window):x}'
	app.backend.x11.windows << fake_x11_record
	request_before := app.services.next_request
	saved_delivery_token := app.next_event_delivery_token
	app.state_mutex.lock()
	app.next_event_delivery_token = 0
	app.state_mutex.unlock()

	app.service_request_portal_parent(window) or {
		assert err.msg() == err_event_delivery_exhausted
		assert app.services.next_request == request_before + 1
		assert app.services.pending.len == 0
		assert app.services.portal_leases.len == 0
		assert app.drain_queued_events()!.len == 0

		app.state_mutex.lock()
		app.next_event_delivery_token = saved_delivery_token
		app.state_mutex.unlock()
		retry := app.service_request_portal_parent(window)!
		assert retry.serial == request_before + 1
		assert app.services.next_request == request_before + 2
		assert app.services.pending.len == 1
		assert app.services.pending[0].id == retry
		assert app.services.pending[0].terminal
		assert app.services.portal_leases.len == 1
		events := app.drain_queued_events()!
		assert events.len == 1
		assert events[0].kind == .service
		assert events[0].service.kind == .portal_parent
		assert events[0].service.portal_parent.id == retry
		assert events[0].service.portal_parent.identifier == expected_parent
		assert app.services.pending.len == 0
		app.service_release_portal_parent(events[0].service.portal_parent.lease)!
		assert app.services.portal_leases.len == 0

		app.backend.x11.windows.clear()
		app.backend.kind = original_backend
		app.stop()!
		return
	}
	assert false, 'synchronous portal completion unexpectedly consumed an exhausted delivery token'
}

fn test_native_clipboard_write_terminal_preflight_matches_backend_completion_mode() {
	assert native_clipboard_requires_reserved_terminal(.x11, .clipboard_write)
	assert !native_clipboard_requires_reserved_terminal(.x11, .clipboard_read)
	assert native_clipboard_requires_reserved_terminal(.wayland, .clipboard_write)
	assert !native_clipboard_requires_reserved_terminal(.wayland, .clipboard_read)
	assert native_clipboard_requires_reserved_terminal(.appkit, .clipboard_write)
	assert native_clipboard_requires_reserved_terminal(.appkit, .clipboard_read)
	assert !native_clipboard_requires_reserved_terminal(.win32, .clipboard_write)
	assert !native_clipboard_requires_reserved_terminal(.win32, .clipboard_read)
	assert !native_clipboard_requires_reserved_terminal(.mock, .clipboard_write)
}

fn test_mock_focus_publishes_losses_then_gain_with_authoritative_sequences() {
	mut app := new_app()!
	a := app.create_window(title: 'focus A')!
	b := app.create_window(title: 'focus B')!
	c := app.create_window(title: 'focus C')!
	_ = app.drain_events()!

	app.service_request_focus(a)!
	first := app.drain_queued_events()!
	assert first.len == 1
	assert first[0].kind == .service
	assert first[0].service.window == a
	assert first[0].service.state.active == .on
	assert first[0].service.state.focused == .on
	c_before := app.service_window_state(c)!

	app.service_request_focus(b)!
	transfer := app.drain_queued_events()!
	assert transfer.len == 2
	assert transfer[0].kind == .service
	assert transfer[0].service.window == a
	assert transfer[0].service.operation == .focus
	assert transfer[0].service.state.active == .off
	assert transfer[0].service.state.focused == .off
	assert transfer[1].kind == .service
	assert transfer[1].service.window == b
	assert transfer[1].service.operation == .focus
	assert transfer[1].service.state.active == .on
	assert transfer[1].service.state.focused == .on
	assert transfer[0].sequence < transfer[1].sequence
	assert transfer[0].service.sequence == transfer[0].sequence
	assert transfer[0].service.state.sequence == transfer[0].sequence
	assert transfer[1].service.sequence == transfer[1].sequence
	assert transfer[1].service.state.sequence == transfer[1].sequence
	assert app.service_window_state(a)!.sequence == transfer[0].sequence
	assert app.service_window_state(b)!.sequence == transfer[1].sequence
	c_after := app.service_window_state(c)!
	assert c_after.active == c_before.active
	assert c_after.focused == c_before.focused
	assert c_after.sequence == c_before.sequence

	app.service_request_focus(b)!
	assert app.drain_queued_events()!.len == 0
	assert app.service_window_state(a)!.sequence == transfer[0].sequence
	assert app.service_window_state(b)!.sequence == transfer[1].sequence
	app.stop()!
}

fn test_mock_focus_delivery_exhaustion_is_atomic() {
	mut app := new_app()!
	a := app.create_window(title: 'focus exhaustion A')!
	b := app.create_window(title: 'focus exhaustion B')!
	_ = app.drain_events()!
	app.service_request_focus(a)!
	_ = app.drain_queued_events()!
	a_before := app.service_window_state(a)!
	b_before := app.service_window_state(b)!
	saved_delivery_token := app.next_event_delivery_token
	app.state_mutex.lock()
	app.next_event_delivery_token = ~u64(0)
	app.state_mutex.unlock()

	mut rejected := false
	app.service_request_focus(b) or {
		assert err.msg() == err_event_delivery_exhausted
		rejected = true
	}
	assert rejected
	assert app.next_event_delivery_token == ~u64(0)
	assert app.drain_queued_events()!.len == 0
	a_after := app.service_window_state(a)!
	b_after := app.service_window_state(b)!
	assert a_after.active == a_before.active
	assert a_after.focused == a_before.focused
	assert a_after.sequence == a_before.sequence
	assert b_after.active == b_before.active
	assert b_after.focused == b_before.focused
	assert b_after.sequence == b_before.sequence

	app.state_mutex.lock()
	app.next_event_delivery_token = saved_delivery_token
	app.state_mutex.unlock()
	app.stop()!
}

fn test_owner_modal_registry_and_child_first_cascade() {
	mut app := new_app()!
	owner := app.create_window(title: 'owner')!
	child := app.create_window(WindowConfig{
		title: 'child'
		owner: owner
		modal: true
	})!
	grandchild := app.create_window(WindowConfig{
		title: 'grandchild'
		owner: child
	})!
	sibling := app.create_window(WindowConfig{
		title: 'sibling'
		owner: owner
	})!
	order := app.window_destroy_order(owner)!
	assert order == [grandchild, child, sibling, owner]
	child_index := app.services.window_index(child)!
	assert app.services.windows[child_index].modal

	mut foreign := new_app()!
	foreign_owner := foreign.create_window()!
	app.create_window(WindowConfig{
		owner: foreign_owner
	}) or {
		assert err.msg() == err_app_identity_mismatch
		app.stop()!
		foreign.stop()!
		return
	}
	assert false, 'foreign owner was accepted'
}

fn test_service_cancellation_is_exactly_once_before_registry_removal() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_events()!
	clipboard := app.services.take_request_id()!
	portal := app.services.take_request_id()!
	readback := app.services.take_readback_id(window)!
	lease := ServicePortalLeaseId{
		app_instance: app.instance_id
		serial:       portal.serial
	}
	app.services.pending << PendingServiceRequest{
		id:     clipboard
		window: window
		kind:   .clipboard_read
	}
	app.services.pending << PendingServiceRequest{
		id:     portal
		window: window
		kind:   .portal_parent
	}
	app.services.readbacks << PendingReadbackRequest{
		id: readback
	}
	app.services.portal_leases << ServicePortalLease{
		id:     lease
		window: window
	}

	ticket := app.prepare_window_destroy(window)!
	app.seal_window_destroy(ticket)!
	assert app.services.window_index(window)! >= 0
	assert app.services.pending.all(it.terminal)
	assert app.services.readbacks.all(it.terminal)
	assert app.services.portal_leases.len == 0
	app.finish_window_destroy(ticket, []string{})!
	events := app.drain_queued_events()!
	assert events.len == 4
	assert events.map(it.kind) == [.service, .service, .readback, .lifecycle]
	for index in 1 .. events.len {
		assert events[index - 1].sequence < events[index].sequence
	}
	assert events[0].service.clipboard.status == .cancelled
	assert events[1].service.portal_parent.status == .cancelled
	assert events[2].readback.status == .cancelled
	assert events[3].lifecycle.kind == .window_destroyed
	assert app.services.pending.len == 0
	assert app.services.readbacks.len == 0
	app.services.window_index(window) or {
		assert err.msg() == err_stale_window
		app.stop()!
		return
	}
	assert false, 'destroyed service record remained registered'
}

fn test_sealed_window_rejects_all_new_service_admissions_atomically() {
	mut app := new_app()!
	window := app.create_window(title: 'sealed service admission')!
	_ = app.drain_queued_events()!
	ticket := app.prepare_window_destroy(window)!
	assert app.service_operation_capability(window, .clipboard_write)!.support == .available
	prepared_request := app.service_set_clipboard_text(window, 'prepared remains live')!
	prepared_events := app.drain_queued_events()!
	assert prepared_events.len == 1
	assert prepared_events[0].service.clipboard.id == prepared_request
	assert prepared_events[0].service.clipboard.status == .ready
	app.seal_window_destroy(ticket)!
	assert app.services.window_index(window)! >= 0
	assert app.windows[window.slot].services_cancelled
	request_before := app.services.next_request
	borrow_before := app.services.next_borrow_epoch
	delivery_before := app.next_event_delivery_token
	clipboard_before := app.services.clipboard_text
	state_before := app.services.windows[app.services.window_index(window)!].state
	mut rejected := 0

	_ = app.service_window_state(window) or {
		assert err.msg() == err_stale_window
		rejected++
		ServiceWindowState{}
	}
	_ = app.service_operation_capability(window, .show) or {
		assert err.msg() == err_stale_window
		rejected++
		ServiceOperationCapability{}
	}
	_ = app.service_cursor_support(window, .default) or {
		assert err.msg() == err_stale_window
		rejected++
		ServiceSupportLevel.unsupported
	}
	app.service_show_window(window) or {
		assert err.msg() == err_stale_window
		rejected++
	}
	_ = app.service_request_clipboard_text(window) or {
		assert err.msg() == err_stale_window
		rejected++
		ServiceRequestId{}
	}
	_ = app.service_set_clipboard_text(window, 'must not publish') or {
		assert err.msg() == err_stale_window
		rejected++
		ServiceRequestId{}
	}
	_ = app.service_request_portal_parent(window) or {
		assert err.msg() == err_stale_window
		rejected++
		ServiceRequestId{}
	}
	_ = app.service_begin_window_readback(window) or {
		assert err.msg() == err_stale_window
		rejected++
		ServiceReadbackId{}
	}
	app.service_arm_image_readback_pass_for_gg(window, 1, 1, 1) or {
		assert err.msg() == err_stale_window
		rejected++
	}
	mut borrow_calls := 0
	callback := fn [mut borrow_calls] (borrow NativeWindowBorrow) ! {
		_ = borrow
		borrow_calls++
	}
	app.with_mock_native_window_borrow_for_gg_test(window, callback) or {
		assert err.msg() == err_stale_window
		rejected++
	}

	assert rejected == 10
	assert borrow_calls == 0
	assert app.services.next_request == request_before
	assert app.services.next_borrow_epoch == borrow_before
	assert app.next_event_delivery_token == delivery_before
	assert app.services.clipboard_text == clipboard_before
	assert app.services.pending.len == 0
	assert app.services.readbacks.len == 0
	assert app.services.portal_leases.len == 0
	assert app.services.windows[app.services.window_index(window)!].state == state_before
	assert app.drain_queued_events()!.len == 0

	app.finish_window_destroy(ticket, []string{})!
	events := app.drain_queued_events()!
	assert events.len == 1
	assert events[0].kind == .lifecycle
	assert events[0].lifecycle.kind == .window_destroyed
	app.stop()!
}

fn test_owner_cascade_cancels_descendant_clipboard_portal_and_leases_exactly_once() {
	mut app := new_app()!
	owner := app.create_window(title: 'service cascade owner')!
	child := app.create_window(WindowConfig{
		title: 'service cascade child'
		owner: owner
	})!
	grandchild := app.create_window(WindowConfig{
		title: 'service cascade grandchild'
		owner: child
	})!
	sibling := app.create_window(WindowConfig{
		title: 'service cascade sibling'
		owner: owner
	})!
	order := [grandchild, child, sibling, owner]
	descendants := order[..order.len - 1]
	assert app.drain_events()!.len == order.len
	mut clipboard_requests := []ServiceRequestId{cap: descendants.len}
	mut portal_requests := []ServiceRequestId{cap: descendants.len}
	mut portal_leases := []ServicePortalLeaseId{cap: descendants.len}
	for window in descendants {
		clipboard := app.services.take_request_id()!
		portal := app.services.take_request_id()!
		lease := ServicePortalLeaseId{
			app_instance: app.instance_id
			serial:       portal.serial
		}
		app.services.pending << PendingServiceRequest{
			id:     clipboard
			window: window
			kind:   .clipboard_read
		}
		app.services.pending << PendingServiceRequest{
			id:     portal
			window: window
			kind:   .portal_parent
		}
		app.services.portal_leases << ServicePortalLease{
			id:     lease
			window: window
		}
		clipboard_requests << clipboard
		portal_requests << portal
		portal_leases << lease
	}
	assert app.services.pending.len == descendants.len * 2
	assert app.services.portal_leases.len == descendants.len

	app.destroy_window(owner)!
	assert app.services.pending.len == descendants.len * 2
	assert app.services.pending.all(it.terminal)
	assert app.services.portal_leases.len == 0
	for lease in portal_leases {
		mut rejected := false
		app.service_release_portal_parent(lease) or {
			assert err.msg() == err_service_request_stale
			rejected = true
		}
		assert rejected
	}
	events := app.drain_queued_events()!
	assert events.len == descendants.len * 3 + 1
	for index in 1 .. events.len {
		assert events[index - 1].sequence < events[index].sequence
	}
	mut event_index := 0
	for index, window in descendants {
		clipboard_event := events[event_index]
		portal_event := events[event_index + 1]
		destroyed_event := events[event_index + 2]
		assert clipboard_event.kind == .service
		assert clipboard_event.service.kind == .clipboard
		assert clipboard_event.service.clipboard.id == clipboard_requests[index]
		assert clipboard_event.service.clipboard.window == window
		assert clipboard_event.service.clipboard.status == .cancelled
		assert portal_event.kind == .service
		assert portal_event.service.kind == .portal_parent
		assert portal_event.service.portal_parent.id == portal_requests[index]
		assert portal_event.service.portal_parent.window == window
		assert portal_event.service.portal_parent.status == .cancelled
		assert destroyed_event.kind == .lifecycle
		assert destroyed_event.lifecycle.kind == .window_destroyed
		assert destroyed_event.lifecycle.window_id == window
		event_index += 3
	}
	assert events[event_index].kind == .lifecycle
	assert events[event_index].lifecycle.kind == .window_destroyed
	assert events[event_index].lifecycle.window_id == owner
	assert app.services.pending.len == 0
	for window in order {
		app.services.window_index(window) or {
			assert err.msg() == err_stale_window
			continue
		}
		assert false, 'destroyed service cascade record remained registered'
	}

	app.destroy_window(owner)!
	assert app.drain_queued_events()!.len == 0
	assert app.services.pending.len == 0
	assert app.services.portal_leases.len == 0
	app.stop()!
}

fn test_backend_readback_acceptance_terminalizes_pending_before_destroy() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_events()!
	readback := app.service_begin_window_readback(window)!
	acceptance := app.accept_backend_event_batch([
		queued_readback_event(ServiceReadbackResult{
			id:              readback
			window:          window
			status:          .ready
			submitted_frame: 1
			width:           1
			height:          1
			stride:          4
			pixels_rgba8:    [u8(1), 2, 3, 4]
		}),
	], 1)!
	assert acceptance.accepted == 1
	assert app.services.readbacks.len == 1
	assert app.services.readbacks[0].terminal

	ticket := app.prepare_window_destroy(window)!
	app.seal_window_destroy(ticket)!
	app.finish_window_destroy(ticket, []string{})!
	events := app.drain_queued_events()!
	assert events.filter(it.kind == .readback).len == 1
	assert events.filter(it.kind == .readback)[0].readback.status == .ready
	app.stop()!
}

fn test_native_backend_close_cancels_pending_readback_once_before_finish() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_events()!
	readback := app.service_begin_window_readback(window)!
	accepted := app.accept_backend_event_batch([
		queued_lifecycle_event(Event{
			kind:      .window_close_requested
			window_id: window
		}),
		queued_lifecycle_event(Event{
			kind:      .window_destroyed
			window_id: window
		}),
	], 1)!
	assert accepted.accepted == 2
	assert app.windows[window.slot].services_cancelled
	before_finish := app.drain_queued_events()!
	assert before_finish.len == 2
	assert before_finish[0].kind == .lifecycle
	assert before_finish[0].lifecycle.kind == .window_close_requested
	assert before_finish[1].kind == .readback
	assert before_finish[1].readback.id == readback
	assert before_finish[1].readback.status == .cancelled
	assert before_finish[0].sequence < before_finish[1].sequence
	assert app.services.readbacks.len == 0

	duplicate := app.accept_backend_event_batch([
		queued_lifecycle_event(Event{
			kind:      .window_destroyed
			window_id: window
		}),
	], 2)!
	assert duplicate.accepted == 0
	notices := app.drain_render_teardown_notices()!
	assert notices.len == 1
	app.finish_window_destroy(notices[0].ticket, []string{})!
	after_finish := app.drain_queued_events()!
	assert after_finish.len == 1
	assert after_finish[0].kind == .lifecycle
	assert after_finish[0].lifecycle.kind == .window_destroyed
	assert after_finish[0].lifecycle.window_id == window
	app.finish_window_destroy(notices[0].ticket, []string{})!
	assert app.drain_queued_events()!.len == 0
	app.stop()!
	assert app.drain_queued_events()!.len == 0
}

fn exercise_native_owner_teardown_permutation(root_first bool) ! {
	mut app := new_app()!
	owner := app.create_window(title: 'native cascade owner')!
	child := app.create_window(WindowConfig{
		title: 'native cascade child'
		owner: owner
	})!
	grandchild := app.create_window(WindowConfig{
		title: 'native cascade grandchild'
		owner: child
	})!
	sibling := app.create_window(WindowConfig{
		title: 'native cascade sibling'
		owner: owner
	})!
	order := [grandchild, child, sibling, owner]
	_ = app.drain_queued_events()!
	for window in order {
		clipboard := app.services.take_request_id()!
		portal := app.services.take_request_id()!
		readback := app.services.take_readback_id(window)!
		app.services.pending << PendingServiceRequest{
			id:     clipboard
			window: window
			kind:   .clipboard_read
		}
		app.services.pending << PendingServiceRequest{
			id:     portal
			window: window
			kind:   .portal_parent
		}
		app.services.readbacks << PendingReadbackRequest{
			id: readback
		}
		app.services.portal_leases << ServicePortalLease{
			id:     ServicePortalLeaseId{
				app_instance: app.instance_id
				serial:       portal.serial
			}
			window: window
		}
	}
	backend_events := if root_first {
		[
			queued_lifecycle_event(Event{
				kind:      .window_destroyed
				window_id: owner
			}),
			queued_lifecycle_event(Event{
				kind:      .window_destroyed
				window_id: child
			}),
			queued_lifecycle_event(Event{
				kind:      .window_destroyed
				window_id: owner
			}),
		]
	} else {
		[
			queued_lifecycle_event(Event{
				kind:      .window_destroyed
				window_id: child
			}),
			queued_lifecycle_event(Event{
				kind:      .window_destroyed
				window_id: owner
			}),
			queued_lifecycle_event(Event{
				kind:      .window_destroyed
				window_id: child
			}),
		]
	}
	acceptance := app.accept_backend_event_batch(backend_events, 1)!
	assert acceptance.accepted == if root_first {
		1
	} else {
		2
	}
	assert app.services.pending.all(it.terminal)
	assert app.services.readbacks.all(it.terminal)
	assert app.services.portal_leases.len == 0
	assert app.windows[grandchild.slot].destroy_stage == .sealed
	assert app.windows[child.slot].destroy_stage == .sealed
	assert app.windows[sibling.slot].destroy_stage == .sealed
	assert app.windows[owner.slot].destroy_stage == .sealed
	assert !app.windows[grandchild.slot].backend_destroyed
	assert app.windows[child.slot].backend_destroyed
	assert !app.windows[sibling.slot].backend_destroyed
	assert app.windows[owner.slot].backend_destroyed

	notices := app.drain_render_teardown_notices()!
	assert notices.map(it.window) == order
	mut owner_rejected := false
	app.finish_window_destroy(notices[3].ticket, []string{}) or {
		assert err.msg() == err_owner_relation_invalid
		owner_rejected = true
	}
	assert owner_rejected
	for notice in notices {
		app.finish_window_destroy(notice.ticket, []string{})!
	}
	events := app.drain_queued_events()!
	assert events.len == order.len * 4
	for index, window in order {
		offset := index * 4
		assert events[offset].kind == .service
		assert events[offset].service.kind == .clipboard
		assert events[offset].service.clipboard.window == window
		assert events[offset].service.clipboard.status == .cancelled
		assert events[offset + 1].kind == .service
		assert events[offset + 1].service.kind == .portal_parent
		assert events[offset + 1].service.portal_parent.window == window
		assert events[offset + 1].service.portal_parent.status == .cancelled
		assert events[offset + 2].kind == .readback
		assert events[offset + 2].readback.window == window
		assert events[offset + 2].readback.status == .cancelled
		assert events[offset + 3].kind == .lifecycle
		assert events[offset + 3].lifecycle.kind == .window_destroyed
		assert events[offset + 3].lifecycle.window_id == window
	}
	for index in 1 .. events.len {
		assert events[index - 1].sequence < events[index].sequence
	}
	assert app.services.pending.len == 0
	assert app.services.readbacks.len == 0
	assert app.services.windows.len == 0
	app.stop()!
}

fn test_native_owner_teardown_is_atomic_child_first_and_deduplicated() {
	exercise_native_owner_teardown_permutation(true)!
	exercise_native_owner_teardown_permutation(false)!
}

fn test_native_owner_teardown_delivery_exhaustion_is_atomic_and_retryable() {
	mut app := new_app()!
	owner := app.create_window(title: 'native exhaustion owner')!
	child := app.create_window(WindowConfig{
		title: 'native exhaustion child'
		owner: owner
	})!
	_ = app.drain_queued_events()!
	clipboard := app.services.take_request_id()!
	app.services.pending << PendingServiceRequest{
		id:     clipboard
		window: child
		kind:   .clipboard_read
	}
	saved_delivery_token := app.next_event_delivery_token
	frame_before := app.frame_count
	app.next_event_delivery_token = ~u64(0)
	mut rejected := false
	app.accept_backend_event_batch([
		queued_lifecycle_event(Event{
			kind:      .window_destroyed
			window_id: owner
		}),
	], 1) or {
		assert err.msg() == err_event_delivery_exhausted
		rejected = true
	}
	assert rejected
	assert app.next_event_delivery_token == ~u64(0)
	assert app.windows[child.slot].destroy_stage == .none
	assert app.windows[owner.slot].destroy_stage == .none
	assert !app.windows[child.slot].services_cancelled
	assert !app.windows[owner.slot].services_cancelled
	assert app.services.pending.len == 1
	assert !app.services.pending[0].terminal
	assert app.teardown_acceptance_order.len == 0
	assert app.frame_count == frame_before
	assert app.drain_queued_events()!.len == 0

	app.next_event_delivery_token = saved_delivery_token
	acceptance := app.accept_backend_event_batch([
		queued_lifecycle_event(Event{
			kind:      .window_destroyed
			window_id: owner
		}),
	], 2)!
	assert acceptance.accepted == 1
	notices := app.drain_render_teardown_notices()!
	assert notices.map(it.window) == [child, owner]
	for notice in notices {
		app.finish_window_destroy(notice.ticket, []string{})!
	}
	events := app.drain_queued_events()!
	assert events.filter(it.kind == .readback).len == 0
	assert events.filter(it.kind == .service).len == 1
	assert events.filter(it.kind == .service)[0].service.clipboard.id == clipboard
	assert events.filter(it.kind == .lifecycle).map(it.lifecycle.window_id) == [child, owner]
	app.stop()!
}

fn test_native_owner_teardown_recollects_cancellation_after_earlier_batch_terminal() {
	mut app := new_app()!
	owner := app.create_window(title: 'native ready owner')!
	child := app.create_window(WindowConfig{
		title: 'native ready child'
		owner: owner
	})!
	_ = app.drain_queued_events()!
	readback := app.service_begin_window_readback(child)!
	acceptance := app.accept_backend_event_batch([
		queued_readback_event(ServiceReadbackResult{
			id:              readback
			window:          child
			status:          .ready
			submitted_frame: 1
			width:           1
			height:          1
			stride:          4
			pixels_rgba8:    [u8(1), 2, 3, 4]
		}),
		queued_lifecycle_event(Event{
			kind:      .window_destroyed
			window_id: owner
		}),
	], 1)!
	assert acceptance.accepted == 2
	notices := app.drain_render_teardown_notices()!
	assert notices.map(it.window) == [child, owner]
	for notice in notices {
		app.finish_window_destroy(notice.ticket, []string{})!
	}
	events := app.drain_queued_events()!
	assert events.map(it.kind) == [.readback, .lifecycle, .lifecycle]
	assert events[0].readback.id == readback
	assert events[0].readback.status == .ready
	assert events.filter(it.kind == .readback).len == 1
	assert app.services.readbacks.len == 0
	app.stop()!

	mut after := new_app()!
	after_owner := after.create_window(title: 'native cancelled owner')!
	after_child := after.create_window(WindowConfig{
		title: 'native cancelled child'
		owner: after_owner
	})!
	_ = after.drain_queued_events()!
	after_readback := after.service_begin_window_readback(after_child)!
	after_acceptance := after.accept_backend_event_batch([
		queued_lifecycle_event(Event{
			kind:      .window_destroyed
			window_id: after_owner
		}),
		queued_readback_event(ServiceReadbackResult{
			id:              after_readback
			window:          after_child
			status:          .ready
			submitted_frame: 1
			width:           1
			height:          1
			stride:          4
			pixels_rgba8:    [u8(1), 2, 3, 4]
		}),
	], 1)!
	assert after_acceptance.accepted == 1
	after_notices := after.drain_render_teardown_notices()!
	for notice in after_notices {
		after.finish_window_destroy(notice.ticket, []string{})!
	}
	after_events := after.drain_queued_events()!
	assert after_events.filter(it.kind == .readback).len == 1
	assert after_events.filter(it.kind == .readback)[0].readback.id == after_readback
	assert after_events.filter(it.kind == .readback)[0].readback.status == .cancelled
	after.stop()!
}

fn test_rejected_readback_keeps_its_reserved_batch_position() {
	mut app := new_app()!
	window := app.create_window(title: 'readback token gap')!
	_ = app.drain_queued_events()!
	first_token := app.next_event_delivery_token
	acceptance := app.accept_backend_event_batch([
		queued_readback_event(ServiceReadbackResult{
			id:     ServiceReadbackId{
				app_instance: app.instance_id
				serial:       0x1234
				window:       window
			}
			window: window
			status: .failed
			error:  err_readback_invalid
		}),
		queued_lifecycle_event(Event{
			kind:      .window_close_requested
			window_id: window
		}),
	], 1)!
	assert acceptance.accepted == 1
	assert acceptance.barrier_token == first_token + 1
	assert app.next_event_delivery_token == first_token + 2
	events := app.drain_queued_events()!
	assert events.len == 1
	assert events[0].kind == .lifecycle
	assert events[0].lifecycle.kind == .window_close_requested
	assert events[0].sequence == first_token + 1
	app.destroy_window(window)!
	app.stop()!
}

fn test_native_owner_teardown_descendant_borrow_is_retryable() {
	mut app := new_app()!
	owner := app.create_window(title: 'native borrow owner')!
	child := app.create_window(WindowConfig{
		title: 'native borrow child'
		owner: owner
	})!
	_ = app.drain_queued_events()!
	app_ptr := unsafe { voidptr(app) }
	callback := fn [app_ptr, owner, child] (_ NativeWindowBorrow) ! {
		mut borrowed_app := unsafe { &App(app_ptr) }
		acceptance := borrowed_app.accept_backend_event_batch([
			queued_lifecycle_event(Event{
				kind:      .window_destroyed
				window_id: owner
			}),
		], 1)!
		assert acceptance.accepted == 1
		notices := borrowed_app.drain_render_teardown_notices()!
		assert notices.map(it.window) == [child, owner]
		mut child_rejected := false
		borrowed_app.finish_window_destroy(notices[0].ticket, []string{}) or {
			assert err.msg() == err_native_borrow_active
			child_rejected = true
		}
		assert child_rejected
		mut owner_rejected := false
		borrowed_app.finish_window_destroy(notices[1].ticket, []string{}) or {
			assert err.msg() == err_owner_relation_invalid
			owner_rejected = true
		}
		assert owner_rejected
		assert borrowed_app.services.window_index(child)! >= 0
		assert borrowed_app.services.window_index(owner)! >= 0
	}
	app.with_native_window_borrow_for_test(child, callback)!
	notices := app.drain_render_teardown_notices()!
	for notice in notices {
		app.finish_window_destroy(notice.ticket, []string{})!
	}
	assert app.drain_queued_events()!.filter(it.kind == .lifecycle).map(it.lifecycle.window_id) == [
		child,
		owner,
	]
	app.stop()!
}

fn test_native_teardown_adopts_presealed_root_and_descendant_tickets() {
	mut root_app := new_app()!
	root := root_app.create_window(title: 'native presealed root')!
	_ = root_app.drain_queued_events()!
	root_request := root_app.service_request_clipboard_text(root)!
	root_ticket := root_app.prepare_window_destroy(root)!
	root_app.seal_window_destroy(root_ticket)!
	root_acceptance := root_app.accept_backend_event_batch([
		queued_lifecycle_event(Event{
			kind:      .window_destroyed
			window_id: root
		}),
	], 1)!
	assert root_acceptance.accepted == 1
	root_notices := root_app.drain_render_teardown_notices()!
	assert root_notices.len == 1
	assert root_notices[0].ticket == root_ticket
	assert root_app.windows[root.slot].backend_destroyed
	root_app.finish_window_destroy(root_ticket, []string{})!
	root_events := root_app.drain_queued_events()!
	assert root_events.filter(it.kind == .service && it.service.clipboard.id == root_request).len == 1
	assert root_events.filter(it.kind == .lifecycle).map(it.lifecycle.window_id) == [
		root,
	]
	root_app.stop()!

	mut app := new_app()!
	owner := app.create_window(title: 'native owner with presealed child')!
	child := app.create_window(WindowConfig{
		title: 'native presealed child'
		owner: owner
	})!
	_ = app.drain_queued_events()!
	child_request := app.service_request_clipboard_text(child)!
	child_ticket := app.prepare_window_destroy(child)!
	app.seal_window_destroy(child_ticket)!
	acceptance := app.accept_backend_event_batch([
		queued_lifecycle_event(Event{
			kind:      .window_destroyed
			window_id: owner
		}),
	], 1)!
	assert acceptance.accepted == 1
	notices := app.drain_render_teardown_notices()!
	assert notices.map(it.window) == [child, owner]
	assert notices[0].ticket == child_ticket
	assert !app.windows[child.slot].backend_destroyed
	assert app.windows[owner.slot].backend_destroyed
	for notice in notices {
		app.finish_window_destroy(notice.ticket, []string{})!
	}
	events := app.drain_queued_events()!
	assert events.filter(it.kind == .service && it.service.clipboard.id == child_request).len == 1
	assert events.filter(it.kind == .lifecycle).map(it.lifecycle.window_id) == [child, owner]
	app.stop()!
}

fn test_stop_finishes_pending_native_owner_cascade_child_first_without_errors() {
	mut app := new_app()!
	owner := app.create_window(title: 'native pending stop owner')!
	child := app.create_window(WindowConfig{
		title: 'native pending stop child'
		owner: owner
	})!
	_ = app.drain_queued_events()!
	acceptance := app.accept_backend_event_batch([
		queued_lifecycle_event(Event{
			kind:      .window_destroyed
			window_id: owner
		}),
	], 1)!
	assert acceptance.accepted == 1
	assert app.teardown_acceptance_order == [child, owner]
	app.stop()!
	assert app.status == .stopped
	assert app.services.windows.len == 0
	assert app.backend.mock.windows.len == 0
	assert app.windows[child.slot].destroy_stage == .finished
	assert app.windows[owner.slot].destroy_stage == .finished
}

fn test_stop_finishes_mixed_owner_destroy_stages_in_global_child_first_order() {
	for seal_owner in [false, true] {
		mut app := new_app()!
		owner := app.create_window(title: 'mixed stop owner')!
		child := app.create_window(WindowConfig{
			title: 'mixed stop child'
			owner: owner
		})!
		_ = app.drain_queued_events()!
		owner_ticket := app.prepare_window_destroy(owner)!
		if seal_owner {
			app.seal_window_destroy(owner_ticket)!
		}
		app.stop()!
		assert app.status == .stopped
		assert app.services.windows.len == 0
		assert app.backend.mock.windows.len == 0
		assert app.windows[child.slot].destroy_stage == .finished
		assert app.windows[owner.slot].destroy_stage == .finished
	}
}

fn test_create_window_rejects_prepared_or_native_sealed_owner_before_side_effects() {
	mut app := new_app()!
	owner := app.create_window(title: 'closing owner admission')!
	_ = app.drain_queued_events()!
	prepared := app.prepare_window_destroy(owner)!
	windows_before := app.windows.len
	backend_before := app.backend.mock.windows.len
	token_before := app.next_event_delivery_token
	mut prepared_rejected := false
	app.create_window(WindowConfig{
		title: 'prepared child rejected'
		owner: owner
	}) or {
		assert err.msg() == err_stale_window
		prepared_rejected = true
	}
	assert prepared_rejected
	assert app.windows.len == windows_before
	assert app.backend.mock.windows.len == backend_before
	assert app.next_event_delivery_token == token_before
	assert app.drain_queued_events()!.len == 0
	app.rollback_window_destroy(prepared)!
	child := app.create_window(WindowConfig{
		title: 'child after owner rollback'
		owner: owner
	})!
	_ = app.drain_queued_events()!

	acceptance := app.accept_backend_event_batch([
		queued_lifecycle_event(Event{
			kind:      .window_destroyed
			window_id: owner
		}),
	], 1)!
	assert acceptance.accepted == 1
	sealed_windows_before := app.windows.len
	sealed_backend_before := app.backend.mock.windows.len
	sealed_token_before := app.next_event_delivery_token
	mut sealed_rejected := false
	app.create_window(WindowConfig{
		title: 'sealed child rejected'
		owner: owner
	}) or {
		assert err.msg() == err_stale_window
		sealed_rejected = true
	}
	assert sealed_rejected
	assert app.windows.len == sealed_windows_before
	assert app.backend.mock.windows.len == sealed_backend_before
	assert app.next_event_delivery_token == sealed_token_before
	notices := app.drain_render_teardown_notices()!
	assert notices.map(it.window) == [child, owner]
	for notice in notices {
		app.finish_window_destroy(notice.ticket, []string{})!
	}
	assert app.drain_queued_events()!.filter(it.kind == .lifecycle).map(it.lifecycle.window_id) == [
		child,
		owner,
	]
	app.stop()!
}

fn test_destroy_window_replays_remembered_terminal_before_live_validation() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_events()!
	app.destroy_window(window)!
	app.destroy_window(window)!
	app.stop()!
}

fn test_backend_destroy_during_native_borrow_keeps_registry_until_retry() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_events()!
	app_ptr := unsafe { voidptr(app) }
	callback := fn [app_ptr, window] (_ NativeWindowBorrow) ! {
		mut owner := unsafe { &App(app_ptr) }
		accepted := owner.accept_backend_event_batch([
			queued_lifecycle_event(Event{
				kind:      .window_destroyed
				window_id: window
			}),
		], 1)!
		assert accepted.accepted == 1
		notices := owner.drain_render_teardown_notices()!
		assert notices.len == 1
		owner.finish_window_destroy(notices[0].ticket, []string{}) or {
			assert err.msg() == err_native_borrow_active
			assert owner.services.window_index(window)! >= 0
			backend_index := owner.backend.mock.window_record_index(window) or { -1 }
			assert backend_index >= 0
			return
		}
		assert false, 'backend teardown removed a window during an active native borrow'
	}
	app.with_native_window_borrow_for_test(window, callback)!
	notices := app.drain_render_teardown_notices()!
	assert notices.len == 1
	app.finish_window_destroy(notices[0].ticket, []string{})!
	app.services.window_index(window) or {
		assert err.msg() == err_stale_window
		app.stop()!
		return
	}
	assert false, 'backend teardown retry left the service record registered'
}

fn test_nested_native_borrows_defer_destroy_until_outer_callback_returns() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_events()!
	app_ptr := unsafe { voidptr(app) }
	inner := fn [app_ptr, window] (borrow NativeWindowBorrow) ! {
		mut owner := unsafe { &App(app_ptr) }
		assert owner.validate_native_borrow_for_gg(window, borrow.epoch_for_gg())! == .mock
		owner.destroy_window(window)!
		assert owner.window_exists(window)
	}
	outer := fn [app_ptr, window, inner] (borrow NativeWindowBorrow) ! {
		mut owner := unsafe { &App(app_ptr) }
		assert owner.validate_native_borrow_for_gg(window, borrow.epoch_for_gg())! == .mock
		owner.with_native_window_borrow_for_test(window, inner)!
		assert owner.window_exists(window)
	}
	app.with_native_window_borrow_for_test(window, outer)!
	assert !app.window_exists(window)
	events := app.drain_queued_events()!
	assert events.len == 1
	assert events[0].lifecycle.kind == .window_destroyed
	app.stop()!
}

fn test_app_level_monitor_event_does_not_require_live_window() {
	mut app := new_app()!
	monitor := app.services.monitors[0]
	accepted := app.accept_backend_event_batch([
		queued_service_event(ServiceEvent{
			kind:    .monitor
			monitor: monitor
		}),
	], 1)!
	assert accepted.accepted == 1
	events := app.drain_queued_events()!
	assert events.len == 1
	assert events[0].kind == .service
	assert events[0].service.kind == .monitor
	assert events[0].service.monitor.id == monitor.id
	app.stop()!
}

fn test_service_metrics_event_uses_one_authoritative_sequence() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_events()!
	accepted := app.accept_backend_event_batch([
		queued_service_event(ServiceEvent{
			kind:    .metrics
			window:  window
			metrics: RenderMetricsSnapshot{
				logical_width:        320
				logical_height:       200
				framebuffer_width:    640
				framebuffer_height:   400
				dpi_scale:            2
				metrics_available:    true
				conversion_available: true
			}
		}),
	], 1)!
	assert accepted.accepted == 1
	events := app.drain_queued_events()!
	assert events.len == 1
	assert events[0].sequence == events[0].service.sequence
	assert events[0].service.sequence == events[0].service.metrics.metrics_sequence
	assert events[0].service.metrics.framebuffer_width == 640
	assert events[0].service.state.monitor_membership_observed
	assert events[0].service.state.monitor_ids == app.service_monitor_ids()!
	app.stop()!
}

fn test_service_monitor_membership_distinguishes_observed_empty_from_partial_state() {
	first := ServiceMonitorId{
		app_instance: 7
		slot:         1
		generation:   2
	}
	second := ServiceMonitorId{
		app_instance: 7
		slot:         3
		generation:   4
	}
	current := service_window_state_with_observed_monitor_membership(ServiceWindowState{
		focused:     .off
		monitor_ids: [first, second]
	})

	partial := merge_service_window_state(current, ServiceWindowState{
		focused: .on
	})
	assert partial.focused == .on
	assert partial.monitor_ids == [first, second]
	assert partial.monitor_membership_observed

	observed_empty := service_window_state_with_observed_monitor_membership(ServiceWindowState{
		focused: .on
	})
	assert service_window_state_has_observation(observed_empty)
	cleared := merge_service_window_state(partial, observed_empty)
	assert cleared.monitor_ids.len == 0
	assert cleared.monitor_membership_observed
	assert !service_window_state_observation_equal(partial, cleared)

	reordered := merge_service_window_state(cleared, ServiceWindowState{
		monitor_ids: [second, first]
	})
	assert reordered.monitor_ids == [second, first]
	assert reordered.monitor_membership_observed
	sequenced := service_window_state_with_sequence(reordered, 91)
	assert sequenced.sequence == 91
	assert sequenced.monitor_ids == [second, first]
	assert sequenced.monitor_membership_observed

	appkit_empty := appkit_service_window_state_from_raw(AppKitServiceRawWindowState{},
		[]AppKitServiceMonitorRecord{}, 7)
	assert appkit_empty.monitor_ids.len == 0
	assert appkit_empty.monitor_membership_observed
}

fn test_clipboard_terminal_payload_budget_is_retained_until_delivery() {
	mut app := new_app()!
	window := app.create_window(title: 'clipboard terminal payload budget')!
	_ = app.drain_queued_events()!
	limit := int(service_clipboard_payload_capacity)
	first_text := 'a'.repeat(limit / 2 + 1)
	second_text := 'b'.repeat(limit / 2 + 1)
	first := app.service_set_clipboard_text(window, first_text)!
	request_before_reject := app.services.next_request
	mut second_error := ''
	app.service_set_clipboard_text(window, second_text) or {
		second_error = err.msg()
		ServiceRequestId{}
	}
	assert second_error == err_clipboard_capacity
	assert app.services.next_request == request_before_reject
	assert app.services.clipboard_text == first_text
	assert app.events.len == 1
	assert app.events[0].service.clipboard.id == first
	assert app.events[0].service.clipboard.text == first_text
	assert app.services.clipboard_payload_bytes == u64(first_text.len)

	first_events := app.drain_queued_events()!
	assert first_events.len == 1
	assert first_events[0].service.clipboard.id == first
	assert app.services.clipboard_payload_bytes == 0
	second := app.service_set_clipboard_text(window, second_text)!
	second_events := app.drain_queued_events()!
	assert second_events.len == 1
	assert second_events[0].service.clipboard.id == second
	assert second_events[0].service.clipboard.text == second_text

	first_read := app.service_request_clipboard_text(window)!
	request_before_read_reject := app.services.next_request
	mut second_read_error := ''
	app.service_request_clipboard_text(window) or {
		second_read_error = err.msg()
		ServiceRequestId{}
	}
	assert second_read_error == err_clipboard_capacity
	assert app.services.next_request == request_before_read_reject
	assert app.events.len == 1
	assert app.events[0].service.clipboard.id == first_read
	read_events := app.drain_queued_events()!
	assert read_events.len == 1
	assert read_events[0].service.clipboard.text == second_text
	retry_read := app.service_request_clipboard_text(window)!
	retry_events := app.drain_queued_events()!
	assert retry_events.len == 1
	assert retry_events[0].service.clipboard.id == retry_read
	assert app.services.clipboard_payload_bytes == 0
	clipboard_before_oversize := app.services.clipboard_text
	request_before_oversize := app.services.next_request
	app.service_set_clipboard_text(window, 'z'.repeat(limit + 1)) or {
		assert err.msg() == err_clipboard_capacity
		assert app.services.next_request == request_before_oversize
		assert app.services.clipboard_text == clipboard_before_oversize
	}
	app.stop()!
}

fn test_terminal_payload_operation_quotas_survive_until_delivery() {
	mut app := new_app()!
	window := app.create_window(title: 'terminal payload operation quotas')!
	_ = app.drain_queued_events()!
	for _ in 0 .. service_clipboard_pending_capacity {
		_ = app.service_set_clipboard_text(window, '')!
	}
	assert app.services.clipboard_pending_count() == service_clipboard_pending_capacity
	request_before_clipboard_reject := app.services.next_request
	delivery_before_clipboard_reject := app.next_event_delivery_token
	app.service_request_clipboard_text(window) or {
		assert err.msg() == err_clipboard_capacity
		assert app.services.next_request == request_before_clipboard_reject
		assert app.next_event_delivery_token == delivery_before_clipboard_reject
	}
	assert app.drain_queued_events()!.len == service_clipboard_pending_capacity
	assert app.services.clipboard_pending_count() == 0
	_ = app.service_request_clipboard_text(window)!
	assert app.drain_queued_events()!.len == 1

	for _ in 0 .. service_readback_pending_capacity {
		_ = app.service_complete_readback(window, 1, 1, 4, [u8(0), 0, 0, 0], 1)!
	}
	assert app.services.readbacks.len == service_readback_pending_capacity
	request_before_readback_reject := app.services.next_request
	delivery_before_readback_reject := app.next_event_delivery_token
	app.service_complete_readback(window, 1, 1, 4, [u8(0), 0, 0, 0], 2) or {
		assert err.msg() == err_readback_capacity
		assert app.services.next_request == request_before_readback_reject
		assert app.next_event_delivery_token == delivery_before_readback_reject
	}
	assert app.drain_queued_events()!.len == service_readback_pending_capacity
	assert app.services.readbacks.len == 0
	_ = app.service_complete_readback(window, 1, 1, 4, [u8(0), 0, 0, 0], 3)!
	assert app.drain_queued_events()!.len == 1

	retry_window := app.create_window(title: 'zero-charge retry')!
	_ = app.drain_queued_events()!
	for _ in 0 .. service_readback_pending_capacity {
		_ = app.service_begin_window_readback(window)!
	}
	assert app.services.readbacks.len == service_readback_pending_capacity
	request_before_zero_charge_reject := app.services.next_request
	delivery_before_zero_charge_reject := app.next_event_delivery_token
	app.service_begin_window_readback(retry_window) or {
		assert err.msg() == err_readback_capacity
		assert app.services.next_request == request_before_zero_charge_reject
		assert app.next_event_delivery_token == delivery_before_zero_charge_reject
	}
	app.destroy_window(window)!
	assert app.services.readbacks.len == service_readback_pending_capacity
	assert app.services.readbacks.all(it.terminal)
	app.service_begin_window_readback(retry_window) or { assert err.msg() == err_readback_capacity }
	cancellation := app.drain_queued_events()!
	assert cancellation.filter(it.kind == .readback).len == service_readback_pending_capacity
	assert app.services.readbacks.len == 0
	retry_readback := app.service_begin_window_readback(retry_window)!
	app.service_fail_window_readback(retry_readback, 'cleanup')!
	assert app.drain_queued_events()!.len == 1
	app.stop()!
}

fn test_portal_lease_quota_survives_delivery_until_explicit_release() {
	mut app := new_app()!
	window := app.create_window(title: 'portal lease quota')!
	_ = app.drain_queued_events()!
	for _ in 0 .. service_portal_lease_capacity {
		_ = app.service_request_portal_parent(window)!
	}
	assert app.services.pending.len == service_portal_lease_capacity
	assert app.services.portal_leases.len == service_portal_lease_capacity
	request_before_reject := app.services.next_request
	delivery_before_reject := app.next_event_delivery_token
	app.service_request_portal_parent(window) or {
		assert err.msg() == err_portal_capacity
		assert app.services.next_request == request_before_reject
		assert app.next_event_delivery_token == delivery_before_reject
	}
	assert app.services.pending.len == service_portal_lease_capacity
	assert app.services.portal_leases.len == service_portal_lease_capacity

	events := app.drain_queued_events()!
	assert events.len == service_portal_lease_capacity
	assert events.all(it.kind == .service && it.service.kind == .portal_parent
		&& it.service.portal_parent.status == .ready)
	assert app.services.pending.len == 0
	assert app.services.portal_leases.len == service_portal_lease_capacity
	request_after_delivery := app.services.next_request
	delivery_after_delivery := app.next_event_delivery_token
	app.service_request_portal_parent(window) or {
		assert err.msg() == err_portal_capacity
		assert app.services.next_request == request_after_delivery
		assert app.next_event_delivery_token == delivery_after_delivery
	}

	app.service_release_portal_parent(events[0].service.portal_parent.lease)!
	retry := app.service_request_portal_parent(window)!
	retry_events := app.drain_queued_events()!
	assert retry_events.len == 1
	assert retry_events[0].service.portal_parent.id == retry
	for event in events[1..] {
		app.service_release_portal_parent(event.service.portal_parent.lease)!
	}
	app.service_release_portal_parent(retry_events[0].service.portal_parent.lease)!
	assert app.services.portal_leases.len == 0
	app.stop()!
}

fn test_native_portal_admission_quota_rolls_back_and_reopens() {
	mut app := new_app()!
	window := app.create_window(title: 'native portal admission quota')!
	_ = app.drain_queued_events()!
	mut requests := []ServiceRequestId{cap: service_portal_lease_capacity}
	mut leases := []ServicePortalLeaseId{cap: service_portal_lease_capacity}
	for _ in 0 .. service_portal_lease_capacity {
		request, lease := app.begin_portal_parent_request(window)!
		requests << request
		leases << lease
	}
	assert app.services.pending.len == service_portal_lease_capacity
	assert app.services.portal_leases.len == service_portal_lease_capacity
	request_before_reject := app.services.next_request
	delivery_before_reject := app.next_event_delivery_token
	_, _ := app.begin_portal_parent_request(window) or {
		assert err.msg() == err_portal_capacity
		ServiceRequestId{}, ServicePortalLeaseId{}
	}
	assert app.services.next_request == request_before_reject
	assert app.next_event_delivery_token == delivery_before_reject
	assert app.services.pending.len == service_portal_lease_capacity
	assert app.services.portal_leases.len == service_portal_lease_capacity

	app.rollback_portal_parent_request(requests[0], leases[0])
	retry_request, retry_lease := app.begin_portal_parent_request(window)!
	assert retry_request.serial == request_before_reject
	assert app.services.pending.len == service_portal_lease_capacity
	assert app.services.portal_leases.len == service_portal_lease_capacity
	for index in 1 .. requests.len {
		app.rollback_portal_parent_request(requests[index], leases[index])
	}
	app.rollback_portal_parent_request(retry_request, retry_lease)
	assert app.services.pending.len == 0
	assert app.services.portal_leases.len == 0
	app.stop()!
}

fn test_readback_payload_budget_is_exact_retained_and_fail_closed() {
	mut app := new_app()!
	window := app.create_window(title: 'readback payload budget')!
	_ = app.drain_queued_events()!
	assert payload_resize_fits(service_readback_payload_capacity - 4, 0, 4,
		service_readback_payload_capacity)
	assert !payload_resize_fits(service_readback_payload_capacity - 4, 0, 5,
		service_readback_payload_capacity)
	assert !payload_resize_fits(~u64(0), 0, 1, service_readback_payload_capacity)

	sentinel := app.service_begin_window_readback(window)!
	app.state_mutex.lock()
	sentinel_index := app.pending_readback_index_locked(sentinel)!
	assert app.services.resize_pending_readback_payload(sentinel_index,
		service_readback_payload_capacity - 4)
	app.state_mutex.unlock()
	first := app.service_complete_readback(window, 1, 1, 4, [u8(1), 2, 3, 4], 1)!
	assert app.services.readback_payload_bytes == service_readback_payload_capacity
	request_before_reject := app.services.next_request
	app.service_complete_readback(window, 1, 1, 4, [u8(5), 6, 7, 8], 2) or {
		assert err.msg() == err_readback_capacity
		assert app.services.next_request == request_before_reject
	}
	drained := app.drain_queued_events()!
	assert drained.len == 1
	assert drained[0].readback.id == first
	assert app.services.readback_payload_bytes == service_readback_payload_capacity - 4
	retry := app.service_complete_readback(window, 1, 1, 4, [u8(9), 10, 11, 12], 3)!
	app.service_fail_window_readback(sentinel, 'sentinel released')!
	assert app.services.readback_payload_bytes == 4
	terminal := app.drain_queued_events()!
	assert terminal.len == 2
	assert terminal[0].readback.id == retry
	assert terminal[1].readback.id == sentinel
	assert app.services.readback_payload_bytes == 0

	corrupt := app.service_begin_window_readback(window)!
	corrupt_index := app.pending_readback_index_locked(corrupt)!
	app.services.readback_payload_bytes = 3
	app.services.readbacks[corrupt_index].payload_bytes = 4
	app.services.release_pending_readback_payload(corrupt_index)
	assert app.services.readback_payload_bytes == 3
	assert app.services.readbacks[corrupt_index].payload_bytes == 4
	app.services.readbacks[corrupt_index].payload_bytes = 3
	app.service_fail_window_readback(corrupt, 'cleanup')!
	assert app.drain_queued_events()!.len == 1
	assert app.services.readback_payload_bytes == 0

	clipboard_request := app.begin_native_clipboard_request(window, .clipboard_read, false)!.request
	clipboard_index := app.services.pending.len - 1
	assert app.services.pending[clipboard_index].id == clipboard_request
	app.services.clipboard_payload_bytes = 3
	app.services.pending[clipboard_index].payload_bytes = 4
	app.services.release_pending_service_payload(clipboard_index)
	assert app.services.clipboard_payload_bytes == 3
	assert app.services.pending[clipboard_index].payload_bytes == 4
	app.services.pending[clipboard_index].payload_bytes = 3
	app.complete_native_clipboard_terminal(clipboard_request, window, .clipboard_read, .failed, '',
		'cleanup', 0)!
	assert app.drain_queued_events()!.len == 1
	assert app.services.clipboard_payload_bytes == 0
	app.stop()!
}

fn test_backend_readback_payload_violation_normalizes_to_failed_terminal() {
	mut app := new_app()!
	window := app.create_window(title: 'backend readback payload normalization')!
	_ = app.drain_queued_events()!
	readback := app.begin_window_readback_with_payload(window, 4)!
	acceptance := app.accept_backend_event_batch([
		queued_readback_event(ServiceReadbackResult{
			id:              readback
			window:          window
			status:          .ready
			submitted_frame: 1
			width:           2
			height:          1
			stride:          8
			pixels_rgba8:    [u8(1), 2, 3, 4, 5, 6, 7, 8]
		}),
	], 1)!
	assert acceptance.accepted == 1
	assert app.services.readback_payload_bytes == 0
	terminal := app.drain_queued_events()!
	assert terminal.len == 1
	assert terminal[0].readback.id == readback
	assert terminal[0].readback.status == .failed
	assert terminal[0].readback.error == err_readback_invalid
	assert terminal[0].readback.pixels_rgba8.len == 0
	assert app.services.readbacks.len == 0

	failed_readback := app.begin_window_readback_with_payload(window, 8)!
	failed_acceptance := app.accept_backend_event_batch([
		queued_readback_event(ServiceReadbackResult{
			id:              failed_readback
			window:          window
			status:          .failed
			submitted_frame: 99
			width:           2
			height:          1
			stride:          8
			pixels_rgba8:    [u8(9), 8, 7, 6, 5, 4, 3, 2]
			error:           'backend failed with untrusted payload'
		}),
	], 2)!
	assert failed_acceptance.accepted == 1
	assert app.services.readback_payload_bytes == 0
	failed_terminal := app.drain_queued_events()!
	assert failed_terminal.len == 1
	assert failed_terminal[0].readback.id == failed_readback
	assert failed_terminal[0].readback.status == .failed
	assert failed_terminal[0].readback.error == 'backend failed with untrusted payload'
	assert failed_terminal[0].readback.submitted_frame == 0
	assert failed_terminal[0].readback.width == 0
	assert failed_terminal[0].readback.height == 0
	assert failed_terminal[0].readback.stride == 0
	assert failed_terminal[0].readback.pixels_rgba8.len == 0
	app.stop()!
}

fn test_backend_clipboard_ready_payload_is_normalized_claimed_and_replay_safe() {
	mut app := new_app()!
	window := app.create_window(title: 'backend clipboard payload normalization')!
	_ = app.drain_queued_events()!
	sentinel := app.begin_native_clipboard_request_with_payload(window, .clipboard_write, false,
		service_clipboard_payload_capacity)!
	read := app.begin_native_clipboard_request(window, .clipboard_read, false)!.request
	ready := queued_service_event(ServiceEvent{
		kind:      .clipboard
		window:    window
		operation: .clipboard_read
		clipboard: ServiceClipboardResult{
			id:     read
			window: window
			status: .ready
			text:   'would exceed the queued payload quota'
		}
	})
	first := app.accept_backend_event_batch([ready], 1)!
	assert first.accepted == 1
	assert app.services.clipboard_payload_bytes == service_clipboard_payload_capacity
	replay := app.accept_backend_event_batch([ready], 2)!
	assert replay.accepted == 0
	assert app.services.clipboard_payload_bytes == service_clipboard_payload_capacity
	terminal := app.drain_queued_events()!
	assert terminal.len == 1
	assert terminal[0].service.clipboard.id == read
	assert terminal[0].service.clipboard.status == .failed
	assert terminal[0].service.clipboard.error == err_clipboard_capacity
	assert terminal[0].service.clipboard.text == ''
	assert app.services.clipboard_payload_bytes == service_clipboard_payload_capacity
	app.rollback_native_service_request(sentinel.request, sentinel.reserved_terminal)
	assert app.services.clipboard_payload_bytes == 0

	ready_text := 'charged backend read until delivery'
	ready_read := app.begin_native_clipboard_request(window, .clipboard_read, false)!.request
	ready_event := queued_service_event(ServiceEvent{
		kind:      .clipboard
		window:    window
		operation: .clipboard_read
		clipboard: ServiceClipboardResult{
			id:     ready_read
			window: window
			status: .ready
			text:   ready_text
		}
	})
	ready_acceptance := app.accept_backend_event_batch([ready_event], 3)!
	assert ready_acceptance.accepted == 1
	assert app.services.clipboard_payload_bytes == u64(ready_text.len)
	ready_replay := app.accept_backend_event_batch([ready_event], 4)!
	assert ready_replay.accepted == 0
	assert app.services.clipboard_payload_bytes == u64(ready_text.len)
	ready_terminal := app.drain_queued_events()!
	assert ready_terminal.len == 1
	assert ready_terminal[0].service.clipboard.id == ready_read
	assert ready_terminal[0].service.clipboard.status == .ready
	assert ready_terminal[0].service.clipboard.text == ready_text
	assert app.services.clipboard_payload_bytes == 0

	failed_read := app.begin_native_clipboard_request(window, .clipboard_read, false)!.request
	failed_event := queued_service_event(ServiceEvent{
		kind:      .clipboard
		window:    window
		operation: .clipboard_read
		clipboard: ServiceClipboardResult{
			id:     failed_read
			window: window
			status: .failed
			text:   'backend failure payload must not escape its released charge'
			error:  'backend read failed'
		}
	})
	failed_acceptance := app.accept_backend_event_batch([failed_event], 5)!
	assert failed_acceptance.accepted == 1
	assert app.services.clipboard_payload_bytes == 0
	failed_terminal := app.drain_queued_events()!
	assert failed_terminal.len == 1
	assert failed_terminal[0].service.clipboard.id == failed_read
	assert failed_terminal[0].service.clipboard.status == .failed
	assert failed_terminal[0].service.clipboard.text == ''
	assert failed_terminal[0].service.clipboard.error == 'backend read failed'

	sync_failed := app.begin_native_clipboard_request(window, .clipboard_read, false)!.request
	app.complete_native_clipboard_terminal(sync_failed, window, .clipboard_read, .failed,
		'synchronous failure payload must also be dropped', 'sync read failed', 0)!
	sync_terminal := app.drain_queued_events()!
	assert sync_terminal.len == 1
	assert sync_terminal[0].service.clipboard.id == sync_failed
	assert sync_terminal[0].service.clipboard.status == .failed
	assert sync_terminal[0].service.clipboard.text == ''
	assert sync_terminal[0].service.clipboard.error == 'sync read failed'
	app.stop()!
}

fn test_stop_preserves_queued_terminal_payload_charges_until_delivery() {
	mut app := new_app()!
	window := app.create_window(title: 'terminal payload stop ownership')!
	_ = app.drain_queued_events()!
	text := 'queued clipboard storage survives stop sealing'
	clipboard := app.service_set_clipboard_text(window, text)!
	readback := app.service_complete_readback(window, 1, 1, 4, [u8(1), 2, 3, 4], 1)!
	assert app.services.clipboard_payload_bytes == u64(text.len)
	assert app.services.readback_payload_bytes == 4

	app.stop()!
	assert app.status == .stopped
	assert app.services.clipboard_payload_bytes == u64(text.len)
	assert app.services.readback_payload_bytes == 4
	assert app.services.pending.any(it.id == clipboard && it.terminal)
	assert app.services.readbacks.any(it.id == readback && it.terminal)

	terminal := app.drain_queued_events()!
	assert terminal.any(it.kind == .service && it.service.clipboard.id == clipboard
		&& it.service.clipboard.text == text)
	assert terminal.any(it.kind == .readback && it.readback.id == readback
		&& it.readback.pixels_rgba8 == [u8(1), 2, 3, 4])
	assert app.services.clipboard_payload_bytes == 0
	assert app.services.readback_payload_bytes == 0
	assert !app.services.pending.any(it.id == clipboard)
	assert !app.services.readbacks.any(it.id == readback)
	app.stop()!
	assert app.services.clipboard_payload_bytes == 0
	assert app.services.readback_payload_bytes == 0
}
