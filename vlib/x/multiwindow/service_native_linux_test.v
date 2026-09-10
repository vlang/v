module multiwindow

import os
import time

const x11_stale_xid_child_marker = 'V_MULTIWINDOW_X11_STALE_XID_CHILD'

fn before_each() {
	$if linux && x_multiwindow_x11 ? {
		mode := os.getenv(x11_stale_xid_child_marker)
		if mode != '' {
			x11_run_stale_xid_child_for_test(mode) or {
				eprintln(err.msg())
				exit(90)
			}
			exit(0)
		}
	}
}

enum X11StaleClipboardReplyKind {
	inline_reply
	none_reply
	incr_reply
	eof_reply
}

fn test_x11_service_state_transitions_are_qualified_by_native_property() {
	assert x11_service_state_transition_operations(true, false, false, true, false, false, false,
		false) == [.minimize]
	assert x11_service_state_transition_operations(true, false, true, false, false, false, false,
		false) == [.restore]
	assert x11_service_state_transition_operations(false, true, false, false, false, true, false,
		false) == [.maximize]
	assert x11_service_state_transition_operations(false, true, false, false, true, false, false,
		false) == [.restore]
	assert x11_service_state_transition_operations(false, true, false, false, false, false, false,
		true) == [.fullscreen]
	assert x11_service_state_transition_operations(false, true, false, false, true, false, true,
		false) == [.restore]
}

@[markused]
fn x11_run_stale_xid_child_for_test(mode string) ! {
	$if linux && x_multiwindow_x11 ? {
		mut app := new_app(backend: .x11)!
		window := app.create_window(title: 'x11 checked expired xid', width: 32, height: 24)!
		live_window := app.create_window(title: 'x11 checked live peer', width: 32, height: 24)!
		_ = app.drain_queued_events()!
		if mode in ['requestor', 'requestor_bad_atom', 'requestor_incr', 'requestor_supersede',
			'requestor_reuse_after_eof', 'requestor_destroy_before_chunk'] {
			owner_index := app.backend.x11.window_record_index(window) or {
				return error(err_window_not_found)
			}
			payload := 'x'.repeat(x11_clipboard_inline_bytes + 1)
			write := app.service_set_clipboard_text(window, payload)!
			write_events := app.drain_queued_events()!
			assert write_events.len == 1
			assert write_events[0].service.clipboard.id == write
			requestor := C.XCreateSimpleWindow(app.backend.x11.display, app.backend.x11.root, 0, 0,
				1, 1, 0, 0, 0)
			assert requestor != X11NativeWindow(0)
			C.XSync(app.backend.x11.display, 0)
			if mode == 'requestor' {
				C.XDestroyWindow(app.backend.x11.display, requestor)
				C.XSync(app.backend.x11.display, 0)
			}
			mut event := C.XEvent{}
			unsafe {
				event.xselectionrequest.@type = x11_selection_request
				event.xselectionrequest.display = app.backend.x11.display
				event.xselectionrequest.owner = app.backend.x11.windows[owner_index].window
				event.xselectionrequest.requestor = requestor
				event.xselectionrequest.selection = app.backend.x11.clipboard
				event.xselectionrequest.target = app.backend.x11.clipboard_utf8
				event.xselectionrequest.property = if mode == 'requestor_bad_atom' {
					X11NativeAtom(~u64(0))
				} else {
					app.backend.x11.clipboard_property
				}
			}
			app.backend.x11.handle_clipboard_selection_request(&event)
			if mode in ['requestor_incr', 'requestor_supersede', 'requestor_reuse_after_eof',
				'requestor_destroy_before_chunk'] {
				assert app.backend.x11.clipboard_transfers.len == 1
				assert app.backend.x11.clipboard_transfers[0].queue == .checked
				assert C.v_multiwindow_x11_checked_requestor_events_enabled(app.backend.x11.checked_connection,
					requestor) != 0
				if mode == 'requestor_supersede' {
					// Leave the old transfer's Delete pending on the checked queue.
					// The next SelectionRequest must drain that prefix before it
					// replaces the same (requestor, property) transaction.
					C.XDeleteProperty(app.backend.x11.display, requestor,
						app.backend.x11.clipboard_property)
					C.XSync(app.backend.x11.display, 0)
					app.backend.x11.handle_clipboard_selection_request(&event)
					assert app.backend.x11.clipboard_transfers.len == 1
					assert app.backend.x11.clipboard_transfers[0].offset == x11_clipboard_chunk_bytes
					assert C.v_multiwindow_x11_checked_requestor_events_enabled(app.backend.x11.checked_connection,
						requestor) != 0
					for _ in 0 .. 3 {
						C.XDeleteProperty(app.backend.x11.display, requestor,
							app.backend.x11.clipboard_property)
						C.XSync(app.backend.x11.display, 0)
						app.backend.x11.drain_checked_clipboard_transfer_events()
					}
					assert app.backend.x11.clipboard_transfers.len == 0
					assert C.v_multiwindow_x11_checked_requestor_events_enabled(app.backend.x11.checked_connection,
						requestor) == 0
					app.backend.x11.handle_clipboard_selection_request(&event)
					assert app.backend.x11.clipboard_transfers.len == 1
					assert app.backend.x11.clipboard_transfers[0].offset == 0
				} else if mode == 'requestor_reuse_after_eof' {
					for _ in 0 .. 3 {
						C.XDeleteProperty(app.backend.x11.display, requestor,
							app.backend.x11.clipboard_property)
						C.XSync(app.backend.x11.display, 0)
						app.backend.x11.drain_checked_clipboard_transfer_events()
					}
					assert app.backend.x11.clipboard_transfers.len == 1
					assert app.backend.x11.clipboard_transfers[0].offset == payload.len
					// Leave the final Delete queued. Admission must drain it, remove the
					// completed transfer, then recheck the pair before accepting this request.
					C.XDeleteProperty(app.backend.x11.display, requestor,
						app.backend.x11.clipboard_property)
					C.XSync(app.backend.x11.display, 0)
					app.backend.x11.handle_clipboard_selection_request(&event)
					assert app.backend.x11.clipboard_transfers.len == 1
					assert app.backend.x11.clipboard_transfers[0].offset == 0
				} else if mode == 'requestor_incr' {
					C.XDeleteProperty(app.backend.x11.display, requestor,
						app.backend.x11.clipboard_property)
					C.XSync(app.backend.x11.display, 0)
					app.backend.x11.drain_checked_clipboard_transfer_events()
					assert app.backend.x11.clipboard_transfers.len == 1
					assert app.backend.x11.clipboard_transfers[0].offset == x11_clipboard_chunk_bytes
				} else {
					C.XDeleteProperty(app.backend.x11.display, requestor,
						app.backend.x11.clipboard_property)
					C.XDestroyWindow(app.backend.x11.display, requestor)
					C.XSync(app.backend.x11.display, 0)
					app.backend.x11.drain_checked_clipboard_transfer_events()
					assert app.backend.x11.clipboard_transfers.len == 0
				}
				if mode != 'requestor_destroy_before_chunk' {
					C.XDestroyWindow(app.backend.x11.display, requestor)
					C.XSync(app.backend.x11.display, 0)
					app.backend.x11.drain_checked_clipboard_transfer_events()
					assert app.backend.x11.clipboard_transfers.len == 0
				}
			} else {
				assert app.backend.x11.clipboard_transfers.len == 0
				if mode == 'requestor_bad_atom' {
					assert C.v_multiwindow_x11_checked_requestor_events_enabled(app.backend.x11.checked_connection,
						requestor) == 0
					C.XDestroyWindow(app.backend.x11.display, requestor)
					C.XSync(app.backend.x11.display, 0)
				}
			}
			assert app.backend.x11.service_checked_connection_usable_for_test()
			utf8, legacy := app.backend.x11.service_clipboard_targets_for_test(window, live_window)!
			assert utf8
			assert !legacy
			assert app.backend.x11.clipboard_transfers.len == 0
			_ = app.backend.x11.service_window_state(live_window)!
			app.stop()!
			return
		}
		index := app.backend.x11.window_record_index(window) or {
			return error(err_window_not_found)
		}
		native := app.backend.x11.windows[index].window
		if mode == 'xdnd_source' {
			source := C.XCreateSimpleWindow(app.backend.x11.display, app.backend.x11.root, 0, 0, 1,
				1, 0, 0, 0)
			assert source != X11NativeWindow(0)
			format := app.backend.x11.text_uri_list
			C.XChangeProperty(app.backend.x11.display, source, app.backend.x11.xdnd_type_list,
				X11NativeAtom(4), 32, x11_prop_mode_replace, unsafe { &u8(&format) }, 1)
			C.XSync(app.backend.x11.display, 0)
			mut event := C.XEvent{}
			unsafe {
				event.xclient.@type = x11_client_message
				event.xclient.display = app.backend.x11.display
				event.xclient.window = native
				event.xclient.message_type = app.backend.x11.xdnd_enter
				event.xclient.format = 32
				event.xclient.data.l[0] = X11NativeLong(source)
				event.xclient.data.l[1] = X11NativeLong((x11_xdnd_version << 24) | 1)
			}
			C.XDestroyWindow(app.backend.x11.display, source)
			C.XSync(app.backend.x11.display, 0)
			app.backend.x11.handle_xdnd_enter(&event)
			assert app.backend.x11.xdnd_source == source
			assert app.backend.x11.xdnd_format == X11NativeAtom(0)
			assert app.backend.x11.service_checked_connection_usable_for_test()
			_ = app.backend.x11.service_window_state(live_window)!
			app.stop()!
			return
		}
		if mode == 'clipboard_owner' {
			sentinel := app.service_set_clipboard_text(live_window, 'checked-owner-sentinel')!
			sentinel_events := app.drain_queued_events()!
			assert sentinel_events.len == 1
			assert sentinel_events[0].service.clipboard.id == sentinel
			assert sentinel_events[0].service.clipboard.status == .ready
		}
		if mode == 'property' {
			app.backend.x11.service_queue_wm_state_then_destroy_for_test(window)!
		} else if mode !in ['readback', 'mouse_lock_after_grab'] {
			app.backend.x11.service_destroy_native_window_retaining_record_for_test(window)!
		}
		retained_index := app.backend.x11.window_record_index(window) or {
			return error(err_window_not_found)
		}
		assert retained_index == index
		assert app.backend.x11.windows[index].window == native
		assert !app.backend.x11.windows[index].native_destroyed

		mut already_polled := false
		match mode {
			'show' {
				mut show_error := ''
				app.backend.x11.service_show_window(window) or { show_error = err.msg() }
				assert show_error == err_capability_unsupported
			}
			'hide' {
				mut hide_error := ''
				app.backend.x11.service_hide_window(window) or { hide_error = err.msg() }
				assert hide_error == err_capability_unsupported
			}
			'focus' {
				mut focus_error := ''
				app.backend.x11.service_focus_window(window) or { focus_error = err.msg() }
				assert focus_error == err_capability_unsupported
			}
			'raise' {
				mut raise_error := ''
				app.backend.x11.service_raise_window(window) or { raise_error = err.msg() }
				assert raise_error == err_capability_unsupported
			}
			'position' {
				mut position_error := ''
				app.backend.x11.service_set_window_position(window, 4, 5) or {
					position_error = err.msg()
				}
				assert position_error == err_capability_unsupported
			}
			'mouse_lock' {
				mut mouse_lock_error := ''
				app.backend.x11.service_set_mouse_lock(window, true) or {
					mouse_lock_error = err.msg()
				}
				assert mouse_lock_error == err_capability_unsupported
				assert !app.backend.x11.windows[index].mouse_locked
				_ = app.backend.x11.service_set_mouse_lock(live_window, true)!
				live_index := app.backend.x11.window_record_index(live_window) or {
					return error(err_window_not_found)
				}
				assert app.backend.x11.windows[live_index].mouse_locked
				_ = app.backend.x11.service_set_mouse_lock(live_window, false)!
				assert !app.backend.x11.windows[live_index].mouse_locked
			}
			'mouse_lock_after_grab' {
				C.v_multiwindow_x11_destroy_mouse_lock_target_after_grab_once_for_test()
				mut mouse_lock_error := ''
				app.backend.x11.service_set_mouse_lock(window, true) or {
					mouse_lock_error = err.msg()
				}
				assert mouse_lock_error == err_capability_unsupported
				assert !app.backend.x11.windows[index].mouse_locked
				_ = app.backend.x11.service_set_mouse_lock(live_window, true)!
				live_index := app.backend.x11.window_record_index(live_window) or {
					return error(err_window_not_found)
				}
				assert app.backend.x11.windows[live_index].mouse_locked
				_ = app.backend.x11.service_set_mouse_lock(live_window, false)!
			}
			'clipboard_owner' {
				live_index := app.backend.x11.window_record_index(live_window) or {
					return error(err_window_not_found)
				}
				live_native := app.backend.x11.windows[live_index].window
				assert app.backend.x11.clipboard_owner_window == live_native
				assert app.backend.x11.clipboard_owner_id == live_window
				assert app.backend.x11.clipboard_text == 'checked-owner-sentinel'
				mut clipboard_error := ''
				app.service_set_clipboard_text(window, 'must-not-commit') or {
					clipboard_error = err.msg()
				}
				assert clipboard_error == err_capability_unsupported
				assert app.backend.x11.clipboard_owner_window == live_native
				assert app.backend.x11.clipboard_owner_id == live_window
				assert app.backend.x11.clipboard_text == 'checked-owner-sentinel'
				utf8, legacy := app.backend.x11.service_clipboard_targets_for_test(live_window,
					live_window)!
				assert utf8
				assert !legacy
				live_write := app.service_set_clipboard_text(live_window,
					'checked-owner-live-retry')!
				live_events := app.drain_queued_events()!
				assert live_events.len == 1
				assert live_events[0].service.clipboard.id == live_write
				assert live_events[0].service.clipboard.status == .ready
			}
			'restore' {
				mut restore_error := ''
				app.backend.x11.service_restore_window(window) or { restore_error = err.msg() }
				assert restore_error == err_capability_unsupported
			}
			'state' {
				mut state_error := ''
				app.backend.x11.service_window_state(window) or { state_error = err.msg() }
				assert state_error == err_capability_unsupported
			}
			'probe' {
				probe := app.backend.x11.service_readback_probe_for_test(window, 1, 1)!
				assert probe.attributes_available == 0
				assert app.service_operation_capability(window, .window_capture)!.support == .unsupported
			}
			'size' {
				assert !app.backend.x11.service_window_size_available_for_test(window)!
			}
			'readback' {
				app.backend.x11.service_destroy_readback_after_probe_for_test()
				mut readback_error := ''
				app.backend.x11.service_window_readback(window, 0, 0, 1, 1) or {
					readback_error = err.msg()
				}
				assert readback_error == err_readback_invalid
			}
			'property' {
				_ = app.poll_events()!
				already_polled = true
			}
			'render' {
				$if gg_multiwindow ? || x_multiwindow_render ? {
					updates := app.backend.x11.collect_render_updates()!
					window_updates := updates.filter(it.window == window)
					assert window_updates.len == 1
					assert window_updates[0].block_reason == .backend_unavailable
				} $else {
					return error('X11 render snapshot child requires a render build')
				}
			}
			else {
				return error('unknown X11 stale-XID child mode `${mode}`')
			}
		}
		assert app.backend.x11.service_checked_connection_usable_for_test()
		assert app.backend.x11.service_shared_connection_usable_for_test()
		_ = app.backend.x11.service_window_state(live_window)!
		assert app.service_operation_capability(live_window, .window_capture)!.support == .available
		if !already_polled {
			_ = app.poll_events()!
		}
		notices := app.drain_render_teardown_notices()!
		assert notices.len == 1
		assert notices[0].window == window
		app.finish_window_destroy(notices[0].ticket, []string{})!
		events := app.drain_queued_events()!
		assert events.filter(it.kind == .lifecycle && it.lifecycle.kind == .window_destroyed
			&& it.lifecycle.window_id == window).len == 1
		assert events.filter(it.kind == .service && it.service.window == window
			&& it.service.kind == .state).len == 0
		app.stop()!
	} $else {
		_ = mode
	}
}

fn test_x11_checked_queries_survive_retained_destroyed_window() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut modes := ['show', 'hide', 'focus', 'raise', 'position', 'mouse_lock',
			'mouse_lock_after_grab', 'clipboard_owner', 'restore', 'state', 'probe', 'size',
			'readback', 'property', 'xdnd_source', 'requestor', 'requestor_bad_atom',
			'requestor_incr', 'requestor_supersede', 'requestor_reuse_after_eof',
			'requestor_destroy_before_chunk']
		$if gg_multiwindow ? || x_multiwindow_render ? {
			modes << 'render'
		}
		for mode in modes {
			command := 'env ${x11_stale_xid_child_marker}=${mode} ${os.quoted_path(os.executable())}'
			result := os.execute(command)
			assert result.exit_code == 0, 'X11 stale-XID ${mode} child failed with exit ${result.exit_code}:\n${result.output}'
			assert result.output.trim_space() == '', 'X11 stale-XID ${mode} child emitted diagnostics:\n${result.output}'
		}
	}
}

fn test_x11_checked_query_connection_lifecycle_is_fail_closed() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		window := app.create_window(
			title:  'x11 checked connection lifecycle'
			width:  32
			height: 24
		)!
		_ = app.drain_queued_events()!
		assert app.backend.x11.service_checked_connection_usable_for_test()
		app.backend.x11.service_close_checked_connection_for_test()
		assert !app.backend.x11.service_checked_connection_usable_for_test()
		mut state_error := ''
		app.backend.x11.service_window_state(window) or { state_error = err.msg() }
		assert state_error == err_capability_unsupported
		assert app.service_operation_capability(window, .window_capture)!.support == .unsupported
		app.stop()!
		assert app.backend.x11.checked_connection == unsafe { nil }
	}
}

fn test_x11_focus_capability_defers_to_authoritative_focus_events_and_deduplicates() {
	mut backend := Backend{
		kind: .x11
		x11:  new_x11_backend()
	}
	missing := backend.service_operation_capability(WindowId{}, .focus)
	assert missing.support == .unsupported
	assert !missing.asynchronous
	assert !backend.service_state_publication_is_deferred(WindowId{}, .focus)

	backend.x11.ewmh_active_window = true
	available := backend.service_operation_capability(WindowId{}, .focus)
	assert available.support == .available
	assert available.asynchronous
	assert available.state_observable
	assert backend.service_state_publication_is_deferred(WindowId{}, .focus)

	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_queued_events()!
	focus := queued_service_event(ServiceEvent{
		kind:      .state
		window:    window
		operation: .focus
		state:     ServiceWindowState{
			active:  .on
			focused: .on
		}
	})
	assert app.accept_backend_event_batch([focus], 1)!.accepted == 1
	assert app.accept_backend_event_batch([focus], 2)!.accepted == 0
	events := app.drain_queued_events()!
	assert events.filter(it.kind == .service && it.service.operation == .focus).len == 1
	app.stop()!
}

fn test_x11_position_capability_defers_publication_until_native_observation() {
	mut backend := Backend{
		kind: .x11
		x11:  new_x11_backend()
	}
	capability := backend.service_operation_capability(WindowId{}, .position)
	assert capability.support == .available
	assert capability.asynchronous
	assert capability.state_observable
	assert backend.service_state_publication_is_deferred(WindowId{}, .position)
}

fn test_x11_monitor_work_area_intersection_uses_widened_endpoints() {
	normal := x11_intersect_monitor_work_area(ServiceRect{
		x:      0
		y:      0
		width:  100
		height: 80
	}, ServiceRect{
		x:      10
		y:      20
		width:  50
		height: 40
	})
	assert normal == ServiceKnownRect{
		known: true
		value: ServiceRect{
			x:      10
			y:      20
			width:  50
			height: 40
		}
	}
	assert !x11_intersect_monitor_work_area(ServiceRect{
		x:      0
		y:      0
		width:  10
		height: 10
	}, ServiceRect{
		x:      20
		y:      20
		width:  5
		height: 5
	}).known

	monitor_endpoint_overflow := x11_intersect_monitor_work_area(ServiceRect{
		x:      2_147_483_640
		y:      2_147_483_640
		width:  32
		height: 32
	}, ServiceRect{
		x:      2_147_483_644
		y:      2_147_483_644
		width:  1
		height: 1
	})
	assert monitor_endpoint_overflow.known
	assert monitor_endpoint_overflow.value == ServiceRect{
		x:      2_147_483_644
		y:      2_147_483_644
		width:  1
		height: 1
	}

	work_area_endpoint_overflow := x11_intersect_monitor_work_area(ServiceRect{
		x:      2_147_483_640
		y:      2_147_483_640
		width:  7
		height: 7
	}, ServiceRect{
		x:      2_147_483_644
		y:      2_147_483_644
		width:  32
		height: 32
	})
	assert work_area_endpoint_overflow.known
	assert work_area_endpoint_overflow.value == ServiceRect{
		x:      2_147_483_644
		y:      2_147_483_644
		width:  3
		height: 3
	}
	assert !x11_intersect_monitor_work_area(ServiceRect{
		width:  0
		height: 10
	}, ServiceRect{
		width:  10
		height: 10
	}).known
}

fn test_x11_window_manager_state_capabilities_defer_to_native_observations() {
	mut backend := Backend{
		kind: .x11
		x11:  new_x11_backend()
	}
	minimize := backend.service_operation_capability(WindowId{}, .minimize)
	assert minimize.support == .conditional
	assert minimize.asynchronous
	assert minimize.state_observable
	assert backend.service_state_publication_is_deferred(WindowId{}, .minimize)

	for operation in [ServiceOperation.maximize, .fullscreen, .restore] {
		missing := backend.service_operation_capability(WindowId{}, operation)
		assert missing.support == .unsupported
		assert !missing.asynchronous
		assert !backend.service_state_publication_is_deferred(WindowId{}, operation)
	}

	backend.x11.ewmh_maximize = true
	maximize := backend.service_operation_capability(WindowId{}, .maximize)
	restore_from_maximize := backend.service_operation_capability(WindowId{}, .restore)
	assert maximize.support == .available
	assert maximize.asynchronous
	assert restore_from_maximize.support == .available
	assert restore_from_maximize.asynchronous
	assert backend.service_operation_capability(WindowId{}, .fullscreen).support == .unsupported

	backend.x11.ewmh_maximize = false
	backend.x11.ewmh_fullscreen = true
	fullscreen := backend.service_operation_capability(WindowId{}, .fullscreen)
	restore_from_fullscreen := backend.service_operation_capability(WindowId{}, .restore)
	assert fullscreen.support == .available
	assert fullscreen.asynchronous
	assert restore_from_fullscreen.support == .available
	assert restore_from_fullscreen.asynchronous
	assert backend.service_operation_capability(WindowId{}, .maximize).support == .unsupported

	backend.x11.ewmh_maximize = true
	for operation in [ServiceOperation.maximize, .fullscreen, .restore] {
		available := backend.service_operation_capability(WindowId{}, operation)
		assert available.support == .available
		assert available.asynchronous
		assert available.state_observable
		assert backend.service_state_publication_is_deferred(WindowId{}, operation)
	}
}

fn test_x11_configure_observations_publish_move_only_and_preserve_resize_events() {
	window := WindowId{
		app_instance: 1
		slot:         0
		generation:   1
	}
	mut backend := new_x11_backend()
	backend.windows << X11WindowRecord{
		id:     window
		width:  100
		height: 80
	}
	first_state := ServiceWindowState{
		position: ServicePosition{
			known: true
			x:     10
			y:     20
		}
	}
	first := backend.queued_configure_observation_events(0, 100, 80, first_state, true)
	assert first.len == 1
	assert first[0].kind == .service
	assert first[0].service.operation == .position
	assert first[0].service.state.position == first_state.position
	assert backend.queued_configure_observation_events(0, 100, 80, first_state, true).len == 0

	move_state := ServiceWindowState{
		position: ServicePosition{
			known: true
			x:     30
			y:     40
		}
	}
	move_only := backend.queued_configure_observation_events(0, 100, 80, move_state, true)
	assert move_only.len == 1
	assert move_only[0].kind == .service
	assert move_only[0].service.operation == .position
	assert move_only[0].service.state.position == move_state.position

	resize_only := backend.queued_configure_observation_events(0, 120, 90, move_state, true)
	assert resize_only.len == 2
	assert resize_only[0].kind == .lifecycle
	assert resize_only[0].lifecycle.kind == .window_resized
	assert resize_only[1].kind == .input
	assert resize_only[1].input.kind == .resized

	resize_move_state := ServiceWindowState{
		position: ServicePosition{
			known: true
			x:     50
			y:     60
		}
	}
	resize_and_move :=
		backend.queued_configure_observation_events(0, 140, 110, resize_move_state, true)
	assert resize_and_move.len == 3
	assert resize_and_move[0].kind == .lifecycle
	assert resize_and_move[1].kind == .input
	assert resize_and_move[2].kind == .service
	assert resize_and_move[2].service.operation == .position
	assert resize_and_move[2].service.state.position == resize_move_state.position
}

fn test_x11_native_service_controls_borrow_monitors_and_readback() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		owner := app.create_window(title: 'package2-owner', width: 96, height: 72)!
		child := app.create_window(
			title:  'package2-child'
			width:  64
			height: 48
		)!
		modal_child := app.create_window(
			title:  'package2-modal-child'
			width:  40
			height: 30
			owner:  owner
			modal:  true
		)!
		_ = app.drain_events()!
		assert app.service_operation_capability(child, .image_readback)!.support == .unsupported
		assert app.service_operation_capability(child, .window_capture)!.support == .available

		monitors := app.service_monitor_ids()!
		assert monitors.len > 0
		monitor := app.service_monitor_info(monitors[0])!
		assert monitor.available
		assert monitor.geometry.known
		assert monitor.geometry.value.width > 0
		assert monitor.geometry.value.height > 0
		assert !monitor.scale.known, 'X11 physical monitor DPI must not be exposed as logical UI scale'
		assert app.backend.x11.service_owner_modal_matches_for_test(modal_child, owner, true)!
		assert app.backend.x11.service_ewmh_capabilities_match_root_for_test()
		assert app.backend.x11.service_root_property_subscription_for_test(), 'X11 backend did not subscribe to root property changes'
		assert app.backend.x11.service_randr_subscription_for_test(), 'X11 backend did not subscribe to RandR topology events'
		randr_events := app.backend.x11.service_randr_snapshot_events_for_test()!
		assert randr_events.len == 1
		accepted_randr := app.accept_backend_event_batch(randr_events, app.frame_count + 1)!
		assert accepted_randr.accepted == 1
		randr_delivery := app.drain_queued_events()!
		assert randr_delivery.len == 1
		assert randr_delivery[0].kind == .service
		assert randr_delivery[0].service.kind == .monitor
		assert randr_delivery[0].service.window == WindowId{}
		assert randr_delivery[0].service.monitors.len > 0
		assert randr_delivery[0].service.monitors[0].sequence > 0
		initial_monitor_id := monitors[0]
		empty_randr := app.backend.x11.service_randr_events_for_snapshot_for_test([])
		assert empty_randr.len == 1
		accepted_empty := app.accept_backend_event_batch(empty_randr, app.frame_count + 2)!
		assert accepted_empty.accepted == 1
		empty_delivery := app.drain_queued_events()!
		assert empty_delivery.len == 1
		assert empty_delivery[0].service.kind == .monitor
		assert empty_delivery[0].service.monitors.len == 0
		assert app.service_monitor_ids()!.len == 0
		replug_randr := app.backend.x11.service_randr_snapshot_events_for_test()!
		accepted_replug := app.accept_backend_event_batch(replug_randr, app.frame_count + 3)!
		assert accepted_replug.accepted == 1
		_ = app.drain_queued_events()!
		replugged_ids := app.service_monitor_ids()!
		assert replugged_ids.len > 0
		assert replugged_ids[0].slot_for_gg() == initial_monitor_id.slot_for_gg()
		assert replugged_ids[0].generation_for_gg() == initial_monitor_id.generation_for_gg() + 1
		mut stale_monitor_rejected := false
		app.service_monitor_info(initial_monitor_id) or {
			assert err.msg() == err_service_request_stale
			stale_monitor_rejected = true
		}
		assert stale_monitor_rejected
		replugged_monitor := app.service_monitor_info(replugged_ids[0])!
		if replugged_monitor.work_area.known {
			assert replugged_monitor.work_area.value.width > 0
			assert replugged_monitor.work_area.value.height > 0
		}
		if replugged_monitor.scale.known {
			assert replugged_monitor.scale.value > 0
		}

		// Separate native MapNotify/UnmapNotify observations from the synchronous
		// state returned by the service calls.
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		app.service_show_window(child)!
		_ = app.drain_queued_events()!
		mut mapped_before_hide := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			_ = app.drain_queued_events()!
			if app.backend.x11.service_window_state(child)!.mapping == .mapped {
				mapped_before_hide = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert mapped_before_hide, 'X11 service test window was not mapped before hide'
		app.state_mutex.lock()
		saved_delivery_token := app.next_event_delivery_token
		app.next_event_delivery_token = 0
		app.state_mutex.unlock()
		mut exhausted_hide_rejected := false
		app.service_hide_window(child) or {
			assert err.msg() == err_event_delivery_exhausted
			exhausted_hide_rejected = true
		}
		assert exhausted_hide_rejected
		assert app.backend.x11.service_window_state(child)!.mapping == .mapped
		assert app.drain_queued_events()!.len == 0
		app.state_mutex.lock()
		app.next_event_delivery_token = saved_delivery_token
		app.state_mutex.unlock()
		app.service_hide_window(child)!
		_ = app.drain_queued_events()!
		mut native_hidden := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			hide_events := app.drain_queued_events()!
			if hide_events.any(it.kind == .service && it.service.kind == .state
				&& it.service.window == child && it.service.state.mapping == .unmapped
				&& it.service.state.visibility == .hidden)
			{
				native_hidden = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert native_hidden, 'X11 UnmapNotify did not publish a canonical state transition'
		app.service_show_window(child)!
		_ = app.drain_queued_events()!
		mut native_visible := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			show_events := app.drain_queued_events()!
			if show_events.any(it.kind == .service && it.service.kind == .state
				&& it.service.window == child && it.service.state.mapping == .mapped
				&& it.service.state.visibility == .visible)
			{
				native_visible = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert native_visible, 'X11 MapNotify did not publish a canonical state transition'

		app.service_hide_window(child)!
		app.service_show_window(child)!
		app.service_set_position(child, 8, 8)!
		app.service_raise_window(child)!
		raise_capability := app.service_operation_capability(child, .raise)!
		assert raise_capability.support == .available
		assert !raise_capability.state_observable
		_ = app.drain_queued_events()!
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		assert app.service_operation_capability(child, .mouse_lock)!.support == .conditional
		mut lock_acquired := false
		for _ in 0 .. 100 {
			app.service_set_mouse_lock(child, true) or {
				time.sleep(time.millisecond)
				continue
			}
			lock_acquired = true
			break
		}
		assert lock_acquired, 'X11 pointer grab was not acquired for the native proof'
		assert app.backend.x11.service_mouse_locked_for_test(child)!
		_ = app.drain_queued_events()!
		app.backend.x11.service_warp_relative_for_test(child, 3, 2)!
		mut relative_motion := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			motion_events := app.drain_queued_events()!
			if motion_events.any(it.kind == .input && it.input.window_id == child
				&& it.input.kind == .mouse_move && it.input.mouse_dx == 3 && it.input.mouse_dy == 2)
			{
				relative_motion = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert relative_motion, 'X11 locked pointer did not report relative motion'
		assert app.backend.x11.service_pointer_recentered_for_test(child)!
		_ = app.poll_events()!
		settled_motion := app.drain_queued_events()!
		assert !settled_motion.any(it.kind == .input && it.input.window_id == child
			&& it.input.kind == .mouse_move && (it.input.mouse_dx != 0 || it.input.mouse_dy != 0))

		app.resize_window(child, 128, 80)!
		_ = app.drain_queued_events()!
		mut public_resize_recentered := false
		mut public_resize_phantom_motion := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			resize_events := app.drain_queued_events()!
			if resize_events.any(it.kind == .input && it.input.window_id == child
				&& it.input.kind == .mouse_move
				&& (it.input.mouse_dx != 0 || it.input.mouse_dy != 0))
			{
				public_resize_phantom_motion = true
			}
			center_x, center_y := app.backend.x11.service_mouse_lock_center_for_test(child)!
			if center_x == 64 && center_y == 40
				&& app.backend.x11.service_pointer_recentered_for_test(child)! {
				public_resize_recentered = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert public_resize_recentered, 'X11 public resize left the locked pointer center stale'
		assert !public_resize_phantom_motion, 'X11 public resize emitted phantom relative motion'
		_ = app.poll_events()!
		settled_resize := app.drain_queued_events()!
		assert !settled_resize.any(it.kind == .input && it.input.window_id == child
			&& it.input.kind == .mouse_move && (it.input.mouse_dx != 0 || it.input.mouse_dy != 0))

		app.backend.x11.service_native_resize_for_test(child, 180, 120)!
		mut configure_recentered := false
		mut configure_phantom_motion := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			configure_events := app.drain_queued_events()!
			if configure_events.any(it.kind == .input && it.input.window_id == child
				&& it.input.kind == .mouse_move
				&& (it.input.mouse_dx != 0 || it.input.mouse_dy != 0))
			{
				configure_phantom_motion = true
			}
			center_x, center_y := app.backend.x11.service_mouse_lock_center_for_test(child)!
			if center_x == 90 && center_y == 60
				&& app.backend.x11.service_pointer_recentered_for_test(child)! {
				configure_recentered = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert configure_recentered, 'X11 ConfigureNotify left the locked pointer center stale'
		assert !configure_phantom_motion, 'X11 ConfigureNotify emitted phantom relative motion'
		app.backend.x11.service_warp_relative_for_test(child, -4, 3)!
		mut resized_relative_motion := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			motion_events := app.drain_queued_events()!
			if motion_events.any(it.kind == .input && it.input.window_id == child
				&& it.input.kind == .mouse_move && it.input.mouse_dx == -4 && it.input.mouse_dy == 3)
			{
				resized_relative_motion = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert resized_relative_motion, 'X11 locked pointer lost relative motion after resize'
		assert app.backend.x11.service_pointer_recentered_for_test(child)!
		app.service_hide_window(child)!
		_ = app.drain_queued_events()!
		mut unmap_released_lock := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			unmap_events := app.drain_queued_events()!
			if !app.backend.x11.service_mouse_locked_for_test(child)!
				&& unmap_events.any(it.kind == .service && it.service.kind == .state
				&& it.service.window == child && it.service.state.mouse_locked == .off) {
				unmap_released_lock = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert unmap_released_lock, 'X11 UnmapNotify did not release and publish mouse lock state'
		app.service_show_window(child)!
		_ = app.drain_queued_events()!
		mut focus_lock_acquired := false
		for _ in 0 .. 100 {
			app.service_set_mouse_lock(child, true) or {
				time.sleep(time.millisecond)
				continue
			}
			focus_lock_acquired = true
			break
		}
		assert focus_lock_acquired
		_ = app.drain_queued_events()!
		app.backend.x11.service_send_focus_out_for_test(child)!
		mut focus_released_lock := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			focus_events := app.drain_queued_events()!
			if !app.backend.x11.service_mouse_locked_for_test(child)!
				&& focus_events.any(it.kind == .service && it.service.kind == .state
				&& it.service.window == child && it.service.state.mouse_locked == .off) {
				focus_released_lock = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert focus_released_lock, 'X11 FocusOut did not release and publish mouse lock state'
		focus_capability := app.service_operation_capability(child, .focus)!
		if focus_capability.support == .available {
			app.service_request_focus(child)!
		} else {
			assert focus_capability.support == .unsupported
		}

		app_ptr := unsafe { voidptr(app) }
		callback := fn [app_ptr, child] (borrow NativeWindowBorrow) ! {
			owner_app := unsafe { &App(app_ptr) }
			assert borrow.backend_for_gg() == .x11
			assert borrow.primary_for_gg() != unsafe { nil }
			assert borrow.secondary_for_gg() != 0
			assert owner_app.validate_native_borrow_for_gg(child, borrow.epoch_for_gg())! == .x11
		}
		app.with_native_window_for_gg(child, callback)!

		portal_capability := app.service_operation_capability(child, .portal_parent)!
		assert portal_capability.support == .available
		assert portal_capability.asynchronous
		portal_request := app.service_request_portal_parent(child)!
		portal_events := app.drain_queued_events()!
		portal_results := portal_events.filter(it.kind == .service
			&& it.service.kind == .portal_parent && it.service.portal_parent.id == portal_request)
		assert portal_results.len == 1
		portal := portal_results[0].service.portal_parent
		assert portal.status == .ready
		assert portal.identifier.starts_with('x11:')
		assert portal.identifier.len > 4
		app.service_release_portal_parent(portal.lease)!

		clipboard_text := 'x.multiwindow native X11 clipboard'
		clipboard_write := app.service_set_clipboard_text(child, clipboard_text)!
		clipboard_write_events := app.drain_queued_events()!
		clipboard_write_results := clipboard_write_events.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == clipboard_write)
		assert clipboard_write_results.len == 1
		assert clipboard_write_results[0].service.clipboard.status == .ready
		utf8_advertised, legacy_advertised := app.backend.x11.service_clipboard_targets_for_test(child,
			owner)!
		assert utf8_advertised
		assert !legacy_advertised
		clipboard_read := app.service_request_clipboard_text(child)!
		mut clipboard_result := ServiceClipboardResult{}
		mut clipboard_found := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			clipboard_events := app.drain_queued_events()!
			clipboard_results := clipboard_events.filter(it.kind == .service
				&& it.service.kind == .clipboard && it.service.clipboard.id == clipboard_read)
			if clipboard_results.len == 1 {
				clipboard_result = clipboard_results[0].service.clipboard
				clipboard_found = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert clipboard_found
		assert clipboard_result.status == .ready
		assert clipboard_result.text == clipboard_text

		large_clipboard_text := '0123456789abcdef'.repeat(8192)
		large_write := app.service_set_clipboard_text(child, large_clipboard_text)!
		large_write_events := app.drain_queued_events()!
		assert large_write_events.any(it.kind == .service && it.service.kind == .clipboard
			&& it.service.clipboard.id == large_write && it.service.clipboard.status == .ready)
		large_read := app.service_request_clipboard_text(child)!
		mut large_result := ServiceClipboardResult{}
		mut large_found := false
		for _ in 0 .. 200 {
			_ = app.poll_events()!
			large_events := app.drain_queued_events()!
			large_results := large_events.filter(it.kind == .service
				&& it.service.kind == .clipboard && it.service.clipboard.id == large_read)
			if large_results.len == 1 {
				large_result = large_results[0].service.clipboard
				large_found = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert large_found
		assert large_result.status == .ready
		assert large_result.text == large_clipboard_text

		// A peer that starts INCR but never deletes the property must not retain
		// the copied payload forever.
		mut completed_transfer_released := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			_ = app.drain_queued_events()!
			_, completed_transfers := app.backend.x11.service_clipboard_pending_counts_for_test()
			if completed_transfers == 0 {
				completed_transfer_released = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert completed_transfer_released, 'completed X11 INCR transfer was not released'
		app.backend.x11.service_start_unresponsive_incr_peer_for_test(owner)!
		mut transfer_started := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			_ = app.drain_queued_events()!
			_, transfers := app.backend.x11.service_clipboard_pending_counts_for_test()
			if transfers == 1 {
				transfer_started = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert transfer_started, 'X11 INCR transfer did not enter the bounded pending state'
		assert app.backend.x11.clipboard_transfers.len == 1
		assert app.backend.x11.clipboard_transfers[0].queue == .xlib
		app.backend.x11.service_expire_clipboard_for_test()
		_ = app.poll_events()!
		_, transfers_after_timeout := app.backend.x11.service_clipboard_pending_counts_for_test()
		assert transfers_after_timeout == 0

		// A selection owner that never answers must produce one terminal failure.
		app.backend.x11.service_make_clipboard_peer_unresponsive_for_test(owner)!
		stalled_read := app.service_request_clipboard_text(child)!
		reads_before_timeout, _ := app.backend.x11.service_clipboard_pending_counts_for_test()
		assert reads_before_timeout == 1
		app.backend.x11.service_expire_clipboard_for_test()
		_ = app.poll_events()!
		stalled_events := app.drain_queued_events()!
		stalled_results := stalled_events.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == stalled_read)
		assert stalled_results.len == 1
		assert stalled_results[0].service.clipboard.status == .failed
		assert stalled_results[0].service.clipboard.error == err_clipboard_timeout

		// Destroying the requestor cancels the canonical request once and purges
		// the backend state before the native window disappears.
		interrupted := app.create_window(title: 'clipboard-interrupted', width: 32, height: 24)!
		_ = app.drain_queued_events()!
		interrupted_read := app.service_request_clipboard_text(interrupted)!
		app.destroy_window(interrupted)!
		interrupted_events := app.drain_queued_events()!
		interrupted_results := interrupted_events.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == interrupted_read)
		assert interrupted_results.len == 1
		assert interrupted_results[0].service.clipboard.status == .cancelled
		reads_after_destroy, transfers_after_destroy :=
			app.backend.x11.service_clipboard_pending_counts_for_test()
		assert reads_after_destroy == 0
		assert transfers_after_destroy == 0

		mut probe := app.backend.x11.service_readback_probe_for_test(child, 32, 24)!
		for _ in 0 .. 100 {
			if probe.map_state == 2 {
				break
			}
			_ = app.poll_events()!
			time.sleep(5 * time.millisecond)
			probe = app.backend.x11.service_readback_probe_for_test(child, 32, 24)!
		}
		assert probe.attributes_available == 1, 'X11 readback probe has no window attributes'
		assert probe.map_state == 2, 'X11 readback probe map_state=${probe.map_state}, native=${probe.actual_width}x${probe.actual_height}, requested=${probe.requested_width}x${probe.requested_height}, pixels=${probe.pixels_length}, expected=${probe.expected_pixels_length}'
		assert probe.actual_width >= probe.requested_width
		assert probe.actual_height >= probe.requested_height
		assert probe.pixels_length == probe.expected_pixels_length
		app.backend.x11.service_paint_readback_pattern_for_test(child, 5, 7)!
		pattern_readback := app.service_request_window_readback_region(child, 5, 7, 2, 2, 1)!
		pattern_events := app.drain_queued_events()!
		pattern_results := pattern_events.filter(it.kind == .readback
			&& it.readback.id == pattern_readback)
		assert pattern_results.len == 1
		pattern := pattern_results[0].readback.pixels_rgba8
		assert pattern.len == 16
		assert pattern[0] > 200 && pattern[1] < 55 && pattern[2] < 55 && pattern[3] == 255
		assert pattern[4] < 55 && pattern[5] > 200 && pattern[6] < 55 && pattern[7] == 255
		assert pattern[8] < 55 && pattern[9] < 55 && pattern[10] > 200 && pattern[11] == 255
		assert pattern[12] > 200 && pattern[13] > 200 && pattern[14] > 200 && pattern[15] == 255
		readback := app.service_request_window_readback(child, 32, 24, 1)!
		events := app.drain_queued_events()!
		results := events.filter(it.kind == .readback && it.readback.id == readback)
		assert results.len == 1
		assert results[0].readback.status == .ready
		assert results[0].readback.pixels_rgba8.len == 32 * 24 * 4
		app.stop()!
	}
}

fn test_x11_window_capture_capability_tracks_native_viewability() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		window := app.create_window(title: 'x11 capture viewability', width: 48, height: 32)!
		_ = app.drain_queued_events()!
		assert app.service_operation_capability(window, .window_capture)!.support == .available

		app.service_hide_window(window)!
		hide_events := app.drain_queued_events()!
		assert hide_events.any(it.kind == .service && it.service.kind == .state
			&& it.service.window == window && it.service.operation == .hide)
		assert app.service_operation_capability(window, .window_capture)!.support == .unsupported
		readbacks_before := app.services.readbacks.len
		app.service_request_window_readback(window, 1, 1, 1) or {
			assert err.msg() == err_capability_unsupported
			assert app.services.readbacks.len == readbacks_before
			assert app.drain_readback_events()!.len == 0
		}

		app.service_show_window(window)!
		show_events := app.drain_queued_events()!
		assert show_events.any(it.kind == .service && it.service.kind == .state
			&& it.service.window == window && it.service.operation == .show)
		assert app.service_operation_capability(window, .window_capture)!.support == .available
		index := app.backend.x11.window_record_index(window) or { panic(err_window_not_found) }
		C.XUnmapWindow(app.backend.x11.display, app.backend.x11.windows[index].window)
		C.XSync(app.backend.x11.display, 0)
		assert app.service_operation_capability(window, .window_capture)!.support == .unsupported
		C.XMapWindow(app.backend.x11.display, app.backend.x11.windows[index].window)
		C.XSync(app.backend.x11.display, 0)
		assert app.service_operation_capability(window, .window_capture)!.support == .available

		parent := C.XCreateSimpleWindow(app.backend.x11.display, app.backend.x11.root, 0, 0, 64,
			48, 0, 0, 0)
		assert parent != X11NativeWindow(0)
		C.XMapWindow(app.backend.x11.display, parent)
		C.XReparentWindow(app.backend.x11.display, app.backend.x11.windows[index].window, parent,
			0, 0)
		C.XMapWindow(app.backend.x11.display, app.backend.x11.windows[index].window)
		C.XSync(app.backend.x11.display, 0)
		C.XUnmapWindow(app.backend.x11.display, parent)
		C.XSync(app.backend.x11.display, 0)
		assert app.service_operation_capability(window, .window_capture)!.support == .unsupported
		C.XMapWindow(app.backend.x11.display, parent)
		C.XSync(app.backend.x11.display, 0)
		assert app.service_operation_capability(window, .window_capture)!.support == .available
		remapped_capture := app.service_request_window_readback_region(window, 0, 0, 1, 1, 2)!
		remapped_events := app.drain_readback_events()!
		remapped_results := remapped_events.filter(it.id == remapped_capture)
		assert remapped_results.len == 1
		assert remapped_results[0].status == .ready
		assert remapped_results[0].width == 1
		assert remapped_results[0].height == 1
		assert remapped_results[0].stride == 4
		assert remapped_results[0].pixels_rgba8.len == 4
		C.XReparentWindow(app.backend.x11.display, app.backend.x11.windows[index].window,
			app.backend.x11.root, 0, 0)
		C.XDestroyWindow(app.backend.x11.display, parent)
		C.XSync(app.backend.x11.display, 0)
		app.stop()!
	}
}

fn test_x11_workarea_property_refreshes_are_coalesced_and_retry_atomically() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		_ = app.create_window(title: 'workarea-refresh-window', width: 32, height: 24)!
		_ = app.drain_queued_events()!
		ids := app.service_monitor_ids()!
		assert ids.len > 0
		initial_id := ids[0]
		initial := app.service_monitor_info(initial_id)!
		assert initial.geometry.known
		geometry := initial.geometry.value
		assert geometry.width > 16
		assert geometry.height > 16
		first_area := ServiceRect{
			x:      geometry.x + 2
			y:      geometry.y + 3
			width:  geometry.width - 4
			height: geometry.height - 6
		}
		second_area := ServiceRect{
			x:      geometry.x + 4
			y:      geometry.y + 5
			width:  geometry.width - 8
			height: geometry.height - 10
		}
		app.backend.x11.service_set_workareas_for_test(1, [geometry, first_area])!
		_ = app.poll_events()!
		first_events := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .monitor)
		assert first_events.len == 1
		assert app.service_monitor_ids()![0] == initial_id
		first := app.service_monitor_info(initial_id)!
		assert first.work_area.known
		assert first.work_area.value == first_area

		app.backend.x11.service_fail_monitor_snapshots_for_test(1)
		app.backend.x11.service_set_workareas_for_test(1, [geometry, second_area])!
		_ = app.poll_events()!
		failed_events := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .monitor)
		assert failed_events.len == 0
		assert app.backend.x11.service_monitor_snapshot_dirty_for_test()
		assert app.service_monitor_info(initial_id)!.work_area.value == first_area

		_ = app.poll_events()!
		retry_events := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .monitor)
		assert retry_events.len == 1
		assert !app.backend.x11.service_monitor_snapshot_dirty_for_test()
		assert app.service_monitor_ids()![0] == initial_id
		retried := app.service_monitor_info(initial_id)!
		assert retried.work_area.known
		assert retried.work_area.value == second_area

		app.backend.x11.service_delete_workarea_for_test()!
		_ = app.poll_events()!
		delete_events := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .monitor)
		assert delete_events.len == 1
		assert app.service_monitor_ids()![0] == initial_id
		assert !app.service_monitor_info(initial_id)!.work_area.known
		app.stop()!
	}
}

fn test_x11_clipboard_global_operation_and_byte_limits() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut read_app := new_app(backend: .x11)!
		read_owner := read_app.create_window(
			title:  'clipboard-capacity-owner'
			width:  32
			height: 24
		)!
		reader := read_app.create_window(title: 'clipboard-capacity-reader', width: 32, height: 24)!
		_ = read_app.drain_queued_events()!

		read_app.backend.x11.service_make_clipboard_peer_unresponsive_for_test(read_owner)!
		mut reads := []ServiceRequestId{cap: x11_clipboard_max_pending_operations}
		for _ in 0 .. x11_clipboard_max_pending_operations {
			reads << read_app.service_request_clipboard_text(reader)!
		}
		mut overflow_error := ''
		read_app.service_request_clipboard_text(reader) or { overflow_error = err.msg() }
		assert overflow_error == err_clipboard_capacity
		pending_reads, pending_transfers :=
			read_app.backend.x11.service_clipboard_pending_counts_for_test()
		assert pending_reads == reads.len
		assert pending_transfers == 0
		read_app.destroy_window(reader)!
		cancelled_events := read_app.drain_queued_events()!
		cancelled := cancelled_events.filter(it.kind == .service && it.service.kind == .clipboard
			&& it.service.clipboard.status == .cancelled && it.service.window == reader)
		assert cancelled.len == reads.len
		read_app.stop()!

		mut transfer_app := new_app(backend: .x11)!
		transfer_owner := transfer_app.create_window(
			title:  'clipboard-byte-owner'
			width:  32
			height: 24
		)!
		first_peer := transfer_app.create_window(
			title:  'clipboard-capacity-peer-a'
			width:  32
			height: 24
		)!
		second_peer := transfer_app.create_window(
			title:  'clipboard-capacity-peer-b'
			width:  32
			height: 24
		)!
		_ = transfer_app.drain_queued_events()!
		payload := 'x'.repeat(x11_clipboard_max_pending_bytes / 2 + 1)
		_ = transfer_app.service_set_clipboard_text(transfer_owner, payload)!
		_ = transfer_app.drain_queued_events()!
		transfer_app.backend.x11.service_start_unresponsive_incr_peer_for_test(first_peer)!
		mut first_started := false
		for _ in 0 .. 100 {
			_ = transfer_app.poll_events()!
			_ = transfer_app.drain_queued_events()!
			_, transfers := transfer_app.backend.x11.service_clipboard_pending_counts_for_test()
			if transfers == 1 {
				first_started = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert first_started
		first_bytes := transfer_app.backend.x11.service_clipboard_pending_bytes_for_test()
		assert first_bytes == u64(payload.len)
		transfer_app.backend.x11.service_start_unresponsive_incr_peer_for_test(second_peer)!
		for _ in 0 .. 20 {
			_ = transfer_app.poll_events()!
			_ = transfer_app.drain_queued_events()!
			time.sleep(time.millisecond)
		}
		_, transfers_after_overflow :=
			transfer_app.backend.x11.service_clipboard_pending_counts_for_test()
		assert transfers_after_overflow == 1
		assert transfer_app.backend.x11.service_clipboard_pending_bytes_for_test() == first_bytes
		transfer_app.stop()!
	}
}

fn test_x11_clipboard_ready_payload_remains_charged_until_core_delivery() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		defer {
			app.stop() or {}
		}
		owner := app.create_window(title: 'clipboard-retained-owner', width: 32, height: 24)!
		reader := app.create_window(title: 'clipboard-retained-reader', width: 32, height: 24)!
		_ = app.drain_queued_events()!
		payload := 'retained until core delivery'
		_ = app.service_set_clipboard_text(owner, payload)!
		_ = app.drain_queued_events()!
		request := app.service_request_clipboard_text(reader)!
		mut accepted := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			if app.services.pending.any(it.id == request && it.terminal) {
				accepted = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert accepted
		reads, transfers := app.backend.x11.service_clipboard_pending_counts_for_test()
		assert reads == 0
		assert transfers == 0
		assert app.backend.x11.service_clipboard_pending_bytes_for_test() == u64(payload.len)
		assert app.backend.x11.clipboard_retained.len == 1
		assert app.backend.x11.clipboard_retained[0].claimed_by_app
		app.destroy_window(reader)!
		assert app.backend.x11.service_clipboard_pending_bytes_for_test() == u64(payload.len)
		results := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == request)
		assert results.len == 1
		assert results[0].service.clipboard.status == .ready
		assert results[0].service.clipboard.text == payload
		assert app.backend.x11.service_clipboard_pending_bytes_for_test() == 0

		retry_reader := app.create_window(title: 'clipboard-retained-retry', width: 32, height: 24)!
		_ = app.drain_queued_events()!
		retry := app.service_request_clipboard_text(retry_reader)!
		mut retry_accepted := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			if app.services.pending.any(it.id == retry && it.terminal) {
				retry_accepted = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert retry_accepted
		assert app.backend.x11.service_clipboard_pending_bytes_for_test() == u64(payload.len)
		retry_results := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == retry)
		assert retry_results.len == 1
		assert retry_results[0].service.clipboard.status == .ready
		assert retry_results[0].service.clipboard.text == payload
		assert app.backend.x11.service_clipboard_pending_bytes_for_test() == 0
	}
}

fn test_x11_queued_selection_notify_wins_over_expired_deadline() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		owner := app.create_window(title: 'queued-selection-owner')!
		reader := app.create_window(title: 'queued-selection-reader')!
		_ = app.drain_queued_events()!
		app.backend.x11.service_make_clipboard_peer_unresponsive_for_test(owner)!
		request := app.service_request_clipboard_text(reader)!
		app.backend.x11.service_queue_clipboard_selection_reply_for_test('queued-reply', true)!
		_ = app.poll_events()!
		results := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == request)
		assert results.len == 1
		assert results[0].service.clipboard.status == .ready
		assert results[0].service.clipboard.text == 'queued-reply'
		reads, _ := app.backend.x11.service_clipboard_pending_counts_for_test()
		assert reads == 0
		app.stop()!
	}
}

fn test_x11_irrelevant_queued_selection_notify_still_expires_after_drain() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		owner := app.create_window(title: 'irrelevant-selection-owner')!
		reader := app.create_window(title: 'irrelevant-selection-reader')!
		_ = app.drain_queued_events()!
		app.backend.x11.service_make_clipboard_peer_unresponsive_for_test(owner)!
		request := app.service_request_clipboard_text(reader)!
		app.backend.x11.service_queue_clipboard_selection_reply_for_test('irrelevant', false)!
		_ = app.poll_events()!
		results := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == request)
		assert results.len == 1
		assert results[0].service.clipboard.status == .failed
		assert results[0].service.clipboard.error == err_clipboard_timeout
		app.stop()!
	}
}

fn test_x11_synchronous_clipboard_write_reserves_terminal_before_native_mutation() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(title: 'clipboard terminal preflight')!
		_ = app.drain_queued_events()!
		first := app.service_set_clipboard_text(window, 'before-exhaustion')!
		first_events := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == first)
		assert first_events.len == 1
		assert first_events[0].service.clipboard.status == .ready
		request_before := app.services.next_request
		pending_before := app.services.pending.len
		owner_before, text_len_before := app.backend.x11.service_clipboard_owner_for_test()
		assert owner_before
		assert text_len_before == 'before-exhaustion'.len

		app.state_mutex.lock()
		saved_delivery_token := app.next_event_delivery_token
		app.next_event_delivery_token = 0
		app.state_mutex.unlock()
		mut rejected := false
		app.service_set_clipboard_text(window, 'must-not-commit') or {
			assert err.msg() == err_event_delivery_exhausted
			rejected = true
			ServiceRequestId{}
		}
		assert rejected
		owner_after, text_len_after := app.backend.x11.service_clipboard_owner_for_test()
		assert owner_after
		assert text_len_after == text_len_before
		assert app.services.next_request == request_before
		assert app.services.pending.len == pending_before
		assert app.drain_queued_events()!.len == 0

		app.state_mutex.lock()
		app.next_event_delivery_token = ~u64(0)
		app.state_mutex.unlock()
		app.backend.x11.clipboard_write_failures_for_test = 1
		mut backend_rejected := false
		app.service_set_clipboard_text(window, 'must-fail-before-mutation') or {
			assert err.msg() == err_capability_unsupported
			backend_rejected = true
			ServiceRequestId{}
		}
		assert backend_rejected
		assert app.next_event_delivery_token == ~u64(0)
		assert app.services.pending.len == pending_before
		assert !app.deferred_poll_error_active
		_, text_len_after_backend_failure := app.backend.x11.service_clipboard_owner_for_test()
		assert text_len_after_backend_failure == text_len_before

		retry := app.service_set_clipboard_text(window, 'after-retry')!
		assert app.next_event_delivery_token == 0
		retry_events := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == retry)
		assert retry_events.len == 1
		assert retry_events[0].sequence == ~u64(0)
		assert retry_events[0].service.clipboard.status == .ready
		assert retry_events[0].service.clipboard.text == 'after-retry'
		app.state_mutex.lock()
		app.next_event_delivery_token = saved_delivery_token
		app.state_mutex.unlock()
	}
}

fn test_x11_late_selection_notify_does_not_complete_the_next_clipboard_read() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		owner := app.create_window(title: 'late-selection-owner')!
		reader := app.create_window(title: 'late-selection-reader')!
		_ = app.drain_queued_events()!
		app.backend.x11.service_make_clipboard_peer_unresponsive_for_test(owner)!
		first := app.service_request_clipboard_text(reader)!
		stale := app.backend.x11.clipboard_reads[0]
		second := app.service_request_clipboard_text(reader)!
		app.backend.x11.service_expire_clipboard_for_test()
		_ = app.poll_events()!
		first_results := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == first)
		assert first_results.len == 1
		assert first_results[0].service.clipboard.status == .failed
		reads_after_first, _ := app.backend.x11.service_clipboard_pending_counts_for_test()
		assert reads_after_first == 1

		mut event := C.XEvent{}
		unsafe {
			event.xselection.@type = x11_selection_notify
			event.xselection.display = app.backend.x11.display
			event.xselection.requestor = stale.requestor
			event.xselection.selection = app.backend.x11.clipboard
			event.xselection.target = app.backend.x11.clipboard_utf8
			event.xselection.property = X11NativeAtom(0)
		}
		C.XPutBackEvent(app.backend.x11.display, &event)
		_ = app.poll_events()!
		second_results := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == second)
		assert second_results.len == 0
		reads_after_stale, _ := app.backend.x11.service_clipboard_pending_counts_for_test()
		assert reads_after_stale == 1
		app.stop()!
	}
}

fn test_x11_all_late_clipboard_reply_forms_are_isolated_from_the_next_read() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		for reply_kind in [X11StaleClipboardReplyKind.inline_reply, .none_reply, .incr_reply,
			.eof_reply] {
			mut app := new_app(backend: .x11)!
			owner := app.create_window(title: 'stale-reply-owner')!
			reader := app.create_window(title: 'stale-reply-reader')!
			_ = app.drain_queued_events()!
			app.backend.x11.service_make_clipboard_peer_unresponsive_for_test(owner)!
			first := app.service_request_clipboard_text(reader)!
			stale := app.backend.x11.clipboard_reads[0]
			second := app.service_request_clipboard_text(reader)!
			app.backend.x11.service_expire_clipboard_for_test()
			_ = app.poll_events()!
			first_results := app.drain_queued_events()!.filter(it.kind == .service
				&& it.service.kind == .clipboard && it.service.clipboard.id == first)
			assert first_results.len == 1
			assert app.backend.x11.clipboard_reads.len == 1
			current := app.backend.x11.clipboard_reads[0]
			assert current.request == second
			assert current.requestor != X11NativeWindow(0)
			assert current.requestor != stale.requestor

			mut terminals := []QueuedEvent{}
			if reply_kind == .eof_reply {
				app.backend.x11.clipboard_reads[0].incremental = true
				app.backend.x11.clipboard_reads[0].data = 'current-read'.bytes()
				app.backend.x11.clipboard_reads[0].reserved_bytes = 'current-read'.len
				C.XChangeProperty(app.backend.x11.display, current.requestor, current.property,
					app.backend.x11.clipboard_utf8, 8, x11_prop_mode_replace, unsafe { nil }, 0)
				mut event := C.XEvent{}
				unsafe {
					event.xproperty.@type = x11_property_notify
					event.xproperty.display = app.backend.x11.display
					event.xproperty.window = stale.requestor
					event.xproperty.atom = stale.property
					event.xproperty.state = x11_property_new_value
				}
				terminals = app.backend.x11.queued_clipboard_property_events(&event)
			} else {
				if reply_kind == .inline_reply {
					payload := 'late-inline'.bytes()
					C.XChangeProperty(app.backend.x11.display, current.requestor, current.property,
						app.backend.x11.clipboard_utf8, 8, x11_prop_mode_replace, payload.data,
						payload.len)
				} else if reply_kind == .incr_reply {
					advertised := X11NativeULong(1)
					C.XChangeProperty(app.backend.x11.display, current.requestor, current.property,
						app.backend.x11.clipboard_incr, 32, x11_prop_mode_replace,
						unsafe { &u8(&advertised) }, 1)
				}
				mut event := C.XEvent{}
				unsafe {
					event.xselection.@type = x11_selection_notify
					event.xselection.display = app.backend.x11.display
					event.xselection.requestor = stale.requestor
					event.xselection.selection = app.backend.x11.clipboard
					event.xselection.target = app.backend.x11.clipboard_utf8
					event.xselection.property = if reply_kind == .none_reply {
						X11NativeAtom(0)
					} else {
						stale.property
					}
				}
				terminals = app.backend.x11.queued_clipboard_selection_events(&event)
			}
			assert terminals.len == 0
			assert app.backend.x11.clipboard_reads.len == 1
			assert app.backend.x11.clipboard_reads[0].request == second
			app.stop()!
		}
	}
}

fn test_x11_clipboard_incr_advertised_size_is_a_lower_bound() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		defer {
			app.stop() or {}
		}
		owner := app.create_window(title: 'incr-lower-bound-owner')!
		reader := app.create_window(title: 'incr-lower-bound-reader')!
		_ = app.drain_queued_events()!
		app.backend.x11.service_make_clipboard_peer_unresponsive_for_test(owner)!
		request := app.service_request_clipboard_text(reader)!
		baseline_bytes := app.backend.x11.service_clipboard_pending_bytes_for_test()
		assert baseline_bytes == 0

		app.backend.x11.service_queue_clipboard_incr_start_for_test(1)!
		_ = app.poll_events()!
		assert app.drain_queued_events()!.len == 0
		assert app.backend.x11.service_clipboard_pending_bytes_for_test() == 1

		first := 'lower-'.bytes()
		app.backend.x11.service_queue_clipboard_incr_chunk_for_test(first)!
		_ = app.poll_events()!
		assert app.drain_queued_events()!.len == 0
		assert app.backend.x11.service_clipboard_pending_bytes_for_test() == u64(first.len)

		second := 'bound'.bytes()
		app.backend.x11.service_queue_clipboard_incr_chunk_for_test(second)!
		_ = app.poll_events()!
		assert app.drain_queued_events()!.len == 0
		assert app.backend.x11.service_clipboard_pending_bytes_for_test() == u64(first.len +
			second.len)

		app.backend.x11.service_queue_clipboard_incr_chunk_for_test([]u8{})!
		_ = app.poll_events()!
		terminals := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == request)
		assert terminals.len == 1
		assert terminals[0].service.clipboard.status == .ready
		assert terminals[0].service.clipboard.text == 'lower-bound'
		assert app.backend.x11.service_clipboard_pending_bytes_for_test() == baseline_bytes
		_ = app.poll_events()!
		assert app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == request).len == 0
	}
}

fn test_x11_clipboard_incr_dynamic_reservation_preserves_hard_and_aggregate_caps() {
	mut lower_bound_backend := X11Backend{
		clipboard_reads: [X11ClipboardRead{
			reserved_bytes: 1
		}]
	}
	assert lower_bound_backend.clipboard_incremental_reservation_after_chunk(6)! == 6

	mut hard_cap_backend := X11Backend{
		clipboard_reads: [X11ClipboardRead{}]
	}
	mut hard_cap_error := ''
	_ = hard_cap_backend.clipboard_incremental_reservation_after_chunk(
		X11NativeULong(x11_clipboard_max_bytes) + 1) or {
		hard_cap_error = err.msg()
		0
	}
	assert hard_cap_error == err_clipboard_capacity

	mut aggregate_backend := X11Backend{
		clipboard_reads: [X11ClipboardRead{}, X11ClipboardRead{
			reserved_bytes: x11_clipboard_max_pending_bytes - 1
		}]
	}
	assert aggregate_backend.clipboard_incremental_reservation_after_chunk(1)! == 1
	aggregate_backend.clipboard_reads[0].reserved_bytes = 1
	mut aggregate_error := ''
	_ = aggregate_backend.clipboard_incremental_reservation_after_chunk(2) or {
		aggregate_error = err.msg()
		0
	}
	assert aggregate_error == err_clipboard_capacity
}

fn test_x11_clipboard_incr_actual_overflow_terminalizes_once_and_releases_bytes() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		defer {
			app.stop() or {}
		}
		owner := app.create_window(title: 'incr-overflow-owner')!
		reader := app.create_window(title: 'incr-overflow-reader')!
		_ = app.drain_queued_events()!
		app.backend.x11.service_make_clipboard_peer_unresponsive_for_test(owner)!
		request := app.service_request_clipboard_text(reader)!
		app.backend.x11.clipboard_reads[0].incremental = true
		app.backend.x11.clipboard_reads[0].data = []u8{len: x11_clipboard_max_bytes}
		app.backend.x11.clipboard_reads[0].reserved_bytes = x11_clipboard_max_bytes
		assert app.backend.x11.service_clipboard_pending_bytes_for_test() == u64(x11_clipboard_max_bytes)

		app.backend.x11.service_queue_clipboard_incr_chunk_for_test([u8(1)])!
		_ = app.poll_events()!
		terminals := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == request)
		assert terminals.len == 1
		assert terminals[0].service.clipboard.status == .failed
		assert terminals[0].service.clipboard.error == err_clipboard_capacity
		assert app.backend.x11.service_clipboard_pending_bytes_for_test() == 0
		reads, transfers := app.backend.x11.service_clipboard_pending_counts_for_test()
		assert reads == 0
		assert transfers == 0
		_ = app.poll_events()!
		assert app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == request).len == 0
	}
}

fn test_x11_start_next_clipboard_failure_terminalizes_the_queued_read_once() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		owner := app.create_window(title: 'start-next-failure-owner')!
		reader := app.create_window(title: 'start-next-failure-reader')!
		_ = app.drain_queued_events()!
		app.backend.x11.service_make_clipboard_peer_unresponsive_for_test(owner)!
		first := app.service_request_clipboard_text(reader)!
		second := app.service_request_clipboard_text(reader)!
		app.backend.x11.clipboard_requestor_create_failures_for_test = 1
		app.backend.x11.service_expire_clipboard_for_test()
		_ = app.poll_events()!
		results := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.operation == .clipboard_read)
		first_results := results.filter(it.service.clipboard.id == first)
		second_results := results.filter(it.service.clipboard.id == second)
		assert first_results.len == 1
		assert first_results[0].service.clipboard.status == .failed
		assert first_results[0].service.clipboard.error == err_clipboard_timeout
		assert second_results.len == 1
		assert second_results[0].service.clipboard.status == .failed
		assert second_results[0].service.clipboard.error == err_capability_unsupported
		reads, _ := app.backend.x11.service_clipboard_pending_counts_for_test()
		assert reads == 0
		_ = app.poll_events()!
		assert app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == second).len == 0
		app.stop()!
	}
}

fn test_x11_purge_active_clipboard_read_terminalizes_a_failed_queued_start_once() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		owner := app.create_window(title: 'purge-start-failure-owner')!
		first_reader := app.create_window(title: 'purge-start-failure-first')!
		second_reader := app.create_window(title: 'purge-start-failure-second')!
		_ = app.drain_queued_events()!
		app.backend.x11.service_make_clipboard_peer_unresponsive_for_test(owner)!
		first := app.service_request_clipboard_text(first_reader)!
		second := app.service_request_clipboard_text(second_reader)!
		app.backend.x11.clipboard_requestor_create_failures_for_test = 1
		app.destroy_window(first_reader)!
		_ = app.poll_events()!
		results := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.operation == .clipboard_read)
		first_results := results.filter(it.service.clipboard.id == first)
		second_results := results.filter(it.service.clipboard.id == second)
		assert first_results.len == 1
		assert first_results[0].service.clipboard.status == .cancelled
		assert second_results.len == 1
		assert second_results[0].service.clipboard.status == .failed
		assert second_results[0].service.clipboard.error == err_capability_unsupported
		reads, _ := app.backend.x11.service_clipboard_pending_counts_for_test()
		assert reads == 0
		_ = app.poll_events()!
		assert app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == second).len == 0
		app.stop()!
	}
}

fn test_x11_queued_incr_property_notify_wins_over_expired_deadline() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		owner := app.create_window(title: 'queued-incr-owner')!
		reader := app.create_window(title: 'queued-incr-reader')!
		_ = app.drain_queued_events()!
		app.backend.x11.service_make_clipboard_peer_unresponsive_for_test(owner)!
		request := app.service_request_clipboard_text(reader)!
		app.backend.x11.service_queue_clipboard_incr_terminal_for_test('queued-incr')!
		_ = app.poll_events()!
		results := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == request)
		assert results.len == 1
		assert results[0].service.clipboard.status == .ready
		assert results[0].service.clipboard.text == 'queued-incr'
		reads, _ := app.backend.x11.service_clipboard_pending_counts_for_test()
		assert reads == 0
		app.stop()!
	}
}

fn test_x11_readback_bounds_accept_exact_edge_and_reject_max_int_without_event() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		window := app.create_window(title: 'readback-overflow-bounds', width: 32, height: 24)!
		_ = app.drain_queued_events()!
		mut probe := app.backend.x11.service_readback_probe_for_test(window, 32, 24)!
		for _ in 0 .. 100 {
			if probe.map_state == 2 {
				break
			}
			_ = app.poll_events()!
			_ = app.drain_queued_events()!
			time.sleep(time.millisecond)
			probe = app.backend.x11.service_readback_probe_for_test(window, 32, 24)!
		}
		assert probe.map_state == 2

		edge := app.service_request_window_readback_region(window, probe.actual_width - 1,
			probe.actual_height - 1, 1, 1, 1)!
		edge_results := app.drain_queued_events()!.filter(it.kind == .readback
			&& it.readback.id == edge)
		assert edge_results.len == 1
		assert edge_results[0].readback.status == .ready
		pending_before := app.services.readbacks.len
		request_before := app.services.next_request

		mut overflow_error := ''
		app.service_request_window_readback_region(window, 0x7fffffff, 0, 1, 1, 2) or {
			overflow_error = err.msg()
		}
		assert overflow_error == err_readback_invalid
		assert app.services.readbacks.len == pending_before
		assert app.services.next_request == request_before
		assert app.drain_queued_events()!.len == 0

		mut impractical_error := ''
		app.service_request_window_readback_region(window, 0, 0, 0x1fffffff, 1, 3) or {
			impractical_error = err.msg()
		}
		assert impractical_error == err_readback_invalid
		assert app.services.readbacks.len == pending_before
		assert app.services.next_request == request_before
		assert app.drain_queued_events()!.len == 0
		app.stop()!
	}
}

fn test_x11_native_readback_rect_preflight_is_subtractive_and_fail_closed() {
	$if linux && x_multiwindow_x11 ? {
		assert x11_native_readback_rect_fits(2, 31, 23, 1, 1, 32, 24)
		assert x11_native_readback_rect_fits(2, 0, 0, 32, 24, 32, 24)
		assert !x11_native_readback_rect_fits(0, 0, 0, 1, 1, 32, 24)
		assert !x11_native_readback_rect_fits(2, 0, 0, 0x1fffffff, 1, 32, 24)
		assert !x11_native_readback_rect_fits(2, 0x7fffffff, 0, 1, 1, 32, 24)
		assert !x11_native_readback_rect_fits(2, 0, 0x7fffffff, 1, 1, 32, 24)
	}
}

fn test_x11_xdnd_incremental_payload_uses_advertised_size_as_lower_bound() {
	$if linux && x_multiwindow_x11 ? {
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut record := X11WindowRecord{
			id: window
		}
		record.window = ~record.window
		mut backend := X11Backend{
			windows: [record]
		}
		backend.text_uri_list = ~backend.text_uri_list
		backend.xdnd_action_copy = backend.text_uri_list
		backend.xdnd_drop_state = X11XdndDrop{
			active:      true
			source:      record.window
			requestor:   record.window
			window:      window
			property:    backend.text_uri_list
			target_type: backend.text_uri_list
			version:     x11_xdnd_version
		}
		assert backend.begin_xdnd_incremental(1)
		first_deadline := backend.xdnd_drop_state.deadline_ns
		assert first_deadline > 0
		assert backend.accept_xdnd_incremental_chunk('file:///tmp/'.bytes()).len == 0
		assert backend.xdnd_drop_state.active
		assert backend.xdnd_drop_state.deadline_ns >= first_deadline
		assert backend.accept_xdnd_incremental_chunk('multiwindow.txt\n'.bytes()).len == 0
		assert backend.xdnd_drop_state.data.len > 1
		events := backend.accept_xdnd_incremental_chunk([]u8{})
		assert events.len == 1
		assert events[0].kind == .input
		assert events[0].input.kind == .files_dropped
		assert events[0].input.window_id == window
		assert events[0].input.dropped_files == ['/tmp/multiwindow.txt']
		assert !backend.xdnd_drop_state.active
		assert backend.xdnd_finished_count == 1
		assert backend.xdnd_last_finished_accepted

		backend.xdnd_drop_state = X11XdndDrop{
			active:      true
			incremental: true
			source:      record.window
			requestor:   record.window
			window:      window
			property:    backend.text_uri_list
			target_type: backend.text_uri_list
			version:     x11_xdnd_version
			data:        []u8{len: x11_xdnd_max_payload_bytes}
		}
		assert backend.accept_xdnd_incremental_chunk([u8(1)]).len == 0
		assert !backend.xdnd_drop_state.active
		assert backend.xdnd_finished_count == 2
		assert !backend.xdnd_last_finished_accepted

		backend.xdnd_drop_state = X11XdndDrop{
			active:      true
			source:      record.window
			requestor:   record.window
			window:      window
			version:     x11_xdnd_version
			deadline_ns: 1
		}
		backend.expire_xdnd_drop(2)
		backend.expire_xdnd_drop(3)
		assert backend.xdnd_finished_count == 3
		assert !backend.xdnd_drop_state.active
	}
}

fn test_x11_xdnd_incr_multipart_is_transactional_and_exactly_once() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		source := app.create_window(title: 'xdnd-incr-source', width: 32, height: 24)!
		target := app.create_window(title: 'xdnd-incr-target', width: 32, height: 24)!
		_ = app.drain_queued_events()!
		app.backend.x11.service_queue_xdnd_incr_start_for_test(source, target, 1, 32, true)!
		_ = app.poll_events()!
		initial_events := app.drain_queued_events()!
		assert !initial_events.any(it.kind == .input && it.input.kind == .files_dropped)
		active, incremental, bytes, initial_deadline, finished, _ :=
			app.backend.x11.service_xdnd_state_for_test()
		assert active
		assert incremental
		assert bytes == 0
		assert initial_deadline > 0
		assert finished == 0
		assert !app.backend.x11.service_xdnd_property_exists_for_test()

		app.backend.x11.service_queue_xdnd_chunk_for_test('ignored'.bytes(), false, true, 8)!
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		stale_active, _, stale_bytes, stale_deadline, stale_finished, _ :=
			app.backend.x11.service_xdnd_state_for_test()
		assert stale_active
		assert stale_bytes == 0
		assert stale_deadline == initial_deadline
		assert stale_finished == 0

		app.backend.x11.service_queue_xdnd_chunk_for_test('file:///tmp/'.bytes(), true, true, 8)!
		_ = app.poll_events()!
		first_events := app.drain_queued_events()!
		assert !first_events.any(it.kind == .input && it.input.kind == .files_dropped)
		first_active, _, first_bytes, first_deadline, first_finished, _ :=
			app.backend.x11.service_xdnd_state_for_test()
		assert first_active
		assert first_bytes == 'file:///tmp/'.len
		assert first_deadline >= initial_deadline
		assert first_finished == 0
		assert !app.backend.x11.service_xdnd_property_exists_for_test()

		app.backend.x11.service_queue_xdnd_chunk_for_test('multiwindow-a\nfile:///tmp/multiwindow-b\n'.bytes(),
			true, true, 8)!
		_ = app.poll_events()!
		second_events := app.drain_queued_events()!
		assert !second_events.any(it.kind == .input && it.input.kind == .files_dropped)
		second_active, _, second_bytes, second_deadline, second_finished, _ :=
			app.backend.x11.service_xdnd_state_for_test()
		assert second_active
		assert second_bytes > 1
		assert second_deadline >= first_deadline
		assert second_finished == 0

		app.backend.x11.service_queue_xdnd_chunk_for_test([]u8{}, true, true, 8)!
		_ = app.poll_events()!
		terminal_events := app.drain_queued_events()!
		drops := terminal_events.filter(it.kind == .input && it.input.kind == .files_dropped)
		assert drops.len == 1
		assert drops[0].input.window_id == target
		assert drops[0].input.dropped_files == ['/tmp/multiwindow-a', '/tmp/multiwindow-b']
		terminal_active, _, _, _, terminal_finished, terminal_accepted :=
			app.backend.x11.service_xdnd_state_for_test()
		assert !terminal_active
		assert terminal_finished == 1
		assert terminal_accepted
		assert app.backend.x11.service_xdnd_wire_finished_for_test() == 1
		finished_sequence, property_sequence :=
			app.backend.x11.service_xdnd_terminal_order_for_test()
		assert property_sequence < finished_sequence
		_ = app.poll_events()!
		assert !app.drain_queued_events()!.any(it.kind == .input && it.input.kind == .files_dropped)
		_, _, _, _, replay_finished, _ := app.backend.x11.service_xdnd_state_for_test()
		assert replay_finished == 1

		app.backend.x11.service_queue_xdnd_incr_start_for_test(source, target, 1, 32, true)!
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		app.backend.x11.service_set_xdnd_property_without_event_for_test('late-timeout'.bytes())!
		assert app.backend.x11.service_xdnd_property_exists_for_test()
		app.backend.x11.service_expire_xdnd_for_test()
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		timeout_active, _, _, _, timeout_finished, timeout_accepted :=
			app.backend.x11.service_xdnd_state_for_test()
		assert !timeout_active
		assert timeout_finished == 2
		assert !timeout_accepted
		assert app.backend.x11.service_xdnd_wire_finished_for_test() == 2
		assert !app.backend.x11.service_xdnd_property_exists_for_test()
		timeout_finished_sequence, timeout_property_sequence :=
			app.backend.x11.service_xdnd_terminal_order_for_test()
		assert timeout_finished_sequence < timeout_property_sequence
		app.backend.x11.service_queue_stale_xdnd_selection_for_test('stale'.bytes())!
		assert app.backend.x11.service_xdnd_property_exists_for_test()
		_ = app.poll_events()!
		assert !app.drain_queued_events()!.any(it.kind == .input && it.input.kind == .files_dropped)
		assert !app.backend.x11.service_xdnd_property_exists_for_test()
		app.backend.x11.service_queue_stale_xdnd_property_for_test('stale-chunk'.bytes())!
		assert app.backend.x11.service_xdnd_property_exists_for_test()
		_ = app.poll_events()!
		assert !app.drain_queued_events()!.any(it.kind == .input && it.input.kind == .files_dropped)
		assert !app.backend.x11.service_xdnd_property_exists_for_test()
		_ = app.poll_events()!
		_, _, _, _, timeout_replay_finished, _ := app.backend.x11.service_xdnd_state_for_test()
		assert timeout_replay_finished == 2

		app.backend.x11.service_queue_xdnd_incr_start_for_test(source, target, 1, 32, true)!
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		app.destroy_window(target)!
		_ = app.drain_queued_events()!
		destroy_active, _, _, _, destroy_finished, destroy_accepted :=
			app.backend.x11.service_xdnd_state_for_test()
		assert !destroy_active
		assert destroy_finished == 3
		assert !destroy_accepted
		assert app.backend.x11.service_xdnd_wire_finished_for_test() == 3
		bad_source := app.create_window(title: 'xdnd destroyed source', width: 32, height: 24)!
		bad_target := app.create_window(title: 'xdnd checked finish target', width: 32, height: 24)!
		_ = app.drain_queued_events()!
		app.backend.x11.service_queue_xdnd_incr_start_for_test(bad_source, bad_target, 1, 32, true)!
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		bad_wire_before := app.backend.x11.service_xdnd_wire_finished_for_test()
		_, _, _, _, bad_finished_before, _ := app.backend.x11.service_xdnd_state_for_test()
		app.backend.x11.service_destroy_xdnd_source_then_finish_for_test()!
		bad_active_after, _, _, _, bad_finished_after, _ :=
			app.backend.x11.service_xdnd_state_for_test()
		assert !bad_active_after
		assert bad_finished_after == bad_finished_before + 1
		assert app.backend.x11.service_xdnd_wire_finished_for_test() == bad_wire_before
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		app.stop()!

		mut self_app := new_app(backend: .x11)!
		self_window := self_app.create_window(
			title:  'xdnd self source target'
			width:  32
			height: 24
		)!
		_ = self_app.drain_queued_events()!
		self_app.backend.x11.service_queue_xdnd_incr_start_for_test(self_window, self_window, 1,
			32, true)!
		_ = self_app.poll_events()!
		_ = self_app.drain_queued_events()!
		self_active, self_incremental, _, _, self_finished_before, _ :=
			self_app.backend.x11.service_xdnd_state_for_test()
		assert self_active
		assert self_incremental
		delete_before := self_app.backend.x11.service_xdnd_property_delete_count_for_test()
		wire_before := self_app.backend.x11.service_xdnd_wire_finished_for_test()
		self_app.destroy_window(self_window)!
		_ = self_app.drain_queued_events()!
		self_active_after, _, _, _, self_finished_after, _ :=
			self_app.backend.x11.service_xdnd_state_for_test()
		assert !self_active_after
		assert self_finished_after == self_finished_before
		assert self_app.backend.x11.service_xdnd_property_delete_count_for_test() == delete_before
		assert self_app.backend.x11.service_xdnd_wire_finished_for_test() == wire_before
		self_app.stop()!
	}
}

fn test_x11_xdnd_invalid_incr_is_terminal_after_validation() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		source := app.create_window(title: 'xdnd-invalid-source', width: 32, height: 24)!
		target := app.create_window(title: 'xdnd-invalid-target', width: 32, height: 24)!
		_ = app.drain_queued_events()!

		app.backend.x11.service_queue_xdnd_incr_start_for_test(source, target,

			u64(x11_xdnd_max_payload_bytes) + 1, 32, true)!
		_ = app.poll_events()!
		assert !app.drain_queued_events()!.any(it.kind == .input && it.input.kind == .files_dropped)
		active_after_oversize, _, _, _, finished_after_oversize, accepted_after_oversize :=
			app.backend.x11.service_xdnd_state_for_test()
		assert !active_after_oversize
		assert finished_after_oversize == 1
		assert !accepted_after_oversize
		assert app.backend.x11.service_xdnd_wire_finished_for_test() == 1
		assert !app.backend.x11.service_xdnd_property_exists_for_test()
		oversize_finished_sequence, oversize_property_sequence :=
			app.backend.x11.service_xdnd_terminal_order_for_test()
		assert oversize_finished_sequence < oversize_property_sequence

		app.backend.x11.service_queue_xdnd_incr_start_for_test(source, target, 1, 8, true)!
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		active_after_header, _, _, _, finished_after_header, accepted_after_header :=
			app.backend.x11.service_xdnd_state_for_test()
		assert !active_after_header
		assert finished_after_header == 2
		assert !accepted_after_header
		assert app.backend.x11.service_xdnd_wire_finished_for_test() == 2
		assert !app.backend.x11.service_xdnd_property_exists_for_test()
		header_finished_sequence, header_property_sequence :=
			app.backend.x11.service_xdnd_terminal_order_for_test()
		assert header_finished_sequence < header_property_sequence

		app.backend.x11.service_queue_xdnd_incr_start_for_test(source, target, 1, 32, true)!
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		app.backend.x11.service_queue_xdnd_chunk_for_test('invalid'.bytes(), true, false, 8)!
		_ = app.poll_events()!
		assert !app.drain_queued_events()!.any(it.kind == .input && it.input.kind == .files_dropped)
		active_after_type, _, _, _, finished_after_type, accepted_after_type :=
			app.backend.x11.service_xdnd_state_for_test()
		assert !active_after_type
		assert finished_after_type == 3
		assert !accepted_after_type
		assert app.backend.x11.service_xdnd_wire_finished_for_test() == 3
		assert !app.backend.x11.service_xdnd_property_exists_for_test()
		type_finished_sequence, type_property_sequence :=
			app.backend.x11.service_xdnd_terminal_order_for_test()
		assert type_finished_sequence < type_property_sequence

		app.backend.x11.service_queue_xdnd_incr_start_for_test(source, target, 1, 32, true)!
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		app.backend.x11.service_queue_xdnd_chunk_for_test([]u8{}, true, true, 32)!
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		active_after_format, _, _, _, finished_after_format, accepted_after_format :=
			app.backend.x11.service_xdnd_state_for_test()
		assert !active_after_format
		assert finished_after_format == 4
		assert !accepted_after_format
		assert app.backend.x11.service_xdnd_wire_finished_for_test() == 4
		assert !app.backend.x11.service_xdnd_property_exists_for_test()
		format_finished_sequence, format_property_sequence :=
			app.backend.x11.service_xdnd_terminal_order_for_test()
		assert format_finished_sequence < format_property_sequence
		_ = app.poll_events()!
		assert app.drain_queued_events()!.len == 0
		_, _, _, _, replay_finished, _ := app.backend.x11.service_xdnd_state_for_test()
		assert replay_finished == 4
		app.stop()!
	}
}

fn test_x11_selection_clear_destroy_and_stop_purge_clipboard_state() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		owner := app.create_window(title: 'clipboard-purge-owner', width: 32, height: 24)!
		peer := app.create_window(title: 'clipboard-purge-peer', width: 32, height: 24)!
		thief := app.create_window(title: 'clipboard-purge-thief', width: 32, height: 24)!
		_ = app.drain_queued_events()!
		payload := 'selection-clear'.repeat(8192)
		_ = app.service_set_clipboard_text(owner, payload)!
		_ = app.drain_queued_events()!
		app.backend.x11.service_start_unresponsive_incr_peer_for_test(peer)!
		mut transfer_started := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			_ = app.drain_queued_events()!
			_, transfers := app.backend.x11.service_clipboard_pending_counts_for_test()
			if transfers == 1 {
				transfer_started = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert transfer_started
		owned_before_clear, text_before_clear := app.backend.x11.service_clipboard_owner_for_test()
		assert owned_before_clear
		assert text_before_clear == payload.len
		app.backend.x11.service_take_clipboard_selection_for_test(thief)!
		mut selection_clear_observed := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			_ = app.drain_queued_events()!
			owned, _ := app.backend.x11.service_clipboard_owner_for_test()
			if !owned {
				selection_clear_observed = true
				break
			}
			time.sleep(time.millisecond)
		}
		assert selection_clear_observed
		owned_after_clear, text_after_clear := app.backend.x11.service_clipboard_owner_for_test()
		reads_after_clear, transfers_after_clear :=
			app.backend.x11.service_clipboard_pending_counts_for_test()
		assert !owned_after_clear
		assert text_after_clear == 0
		assert reads_after_clear == 0
		assert transfers_after_clear == 0

		_ = app.service_set_clipboard_text(owner, payload)!
		_ = app.drain_queued_events()!
		app.backend.x11.service_start_unresponsive_incr_peer_for_test(peer)!
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			_ = app.drain_queued_events()!
			_, transfers := app.backend.x11.service_clipboard_pending_counts_for_test()
			if transfers == 1 {
				break
			}
			time.sleep(time.millisecond)
		}
		app.destroy_window(owner)!
		_ = app.drain_queued_events()!
		owned_after_destroy, text_after_destroy :=
			app.backend.x11.service_clipboard_owner_for_test()
		reads_after_destroy, transfers_after_destroy :=
			app.backend.x11.service_clipboard_pending_counts_for_test()
		assert !owned_after_destroy
		assert text_after_destroy == 0
		assert reads_after_destroy == 0
		assert transfers_after_destroy == 0

		_ = app.service_set_clipboard_text(thief, 'stop-purge')!
		_ = app.drain_queued_events()!
		app.backend.x11.service_make_clipboard_peer_unresponsive_for_test(thief)!
		_ = app.service_request_clipboard_text(peer)!
		app.stop()!
		owned_after_stop, text_after_stop := app.backend.x11.service_clipboard_owner_for_test()
		reads_after_stop, transfers_after_stop :=
			app.backend.x11.service_clipboard_pending_counts_for_test()
		assert !owned_after_stop
		assert text_after_stop == 0
		assert reads_after_stop == 0
		assert transfers_after_stop == 0
	}
}

fn test_x11_stale_selection_clear_preserves_reacquired_clipboard() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		owner := app.create_window(title: 'clipboard-reacquire-owner', width: 32, height: 24)!
		thief := app.create_window(title: 'clipboard-reacquire-thief', width: 32, height: 24)!
		_ = app.drain_queued_events()!
		_ = app.service_set_clipboard_text(owner, 'old-generation')!
		_ = app.drain_queued_events()!
		app.backend.x11.service_take_clipboard_selection_for_test(thief)!
		_ = app.service_set_clipboard_text(owner, 'new-generation')!
		_ = app.drain_queued_events()!
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		owned, text_len := app.backend.x11.service_clipboard_owner_for_test()
		assert owned
		assert text_len == 'new-generation'.len
		app.stop()!
	}
}

fn x11_registry_monitor_candidate(app_instance u64, atom u64, slot int, generation u32, name string, x int) ServiceMonitorInfo {
	return ServiceMonitorInfo{
		native_key: ServiceMonitorNativeKey{
			kind:    .x11_atom
			numeric: atom
		}
		id:         ServiceMonitorId{
			app_instance: app_instance
			slot:         slot
			generation:   generation
		}
		name:       name
		geometry:   ServiceKnownRect{
			known: true
			value: ServiceRect{
				x:      x
				width:  100
				height: 100
			}
		}
		available:  true
	}
}

fn test_monitor_reconciliation_uses_native_keys_for_duplicate_names_and_replug() {
	instance := u64(73)
	mut registry := new_service_registry(instance, .x11)
	first_a := service_monitor_info_for_slot(x11_registry_monitor_candidate(instance, 101, 0, 1,
		'same-name', 10), instance, 0, 3, true, 1)
	first_b := service_monitor_info_for_slot(x11_registry_monitor_candidate(instance, 202, 1, 1,
		'same-name', 20), instance, 1, 7, true, 1)
	registry.replace_monitors([first_a, first_b])

	reversed := registry.reconcile_monitor_snapshot([
		x11_registry_monitor_candidate(instance, 202, 0, 1, 'same-name', 222),
		x11_registry_monitor_candidate(instance, 101, 1, 1, 'same-name', 111),
	], 2) or { panic('valid reversed monitor snapshot was rejected') }
	assert reversed.len == 2
	assert registry.monitors[0].native_key.numeric == 101
	assert registry.monitors[0].geometry.value.x == 111
	assert registry.monitors[0].id == first_a.id
	assert registry.monitors[1].native_key.numeric == 202
	assert registry.monitors[1].geometry.value.x == 222
	assert registry.monitors[1].id == first_b.id

	removed_id := registry.monitors[0].id
	registry.reconcile_monitor_snapshot([
		x11_registry_monitor_candidate(instance, 202, 0, 1, 'same-name', 223),
	], 3) or { panic('valid monitor removal snapshot was rejected') }
	assert !registry.monitors[0].available
	assert registry.monitors[1].id == first_b.id

	registry.reconcile_monitor_snapshot([
		x11_registry_monitor_candidate(instance, 202, 0, 1, 'same-name', 224),
		x11_registry_monitor_candidate(instance, 101, 1, 1, 'renamed-a', 112),
	], 4) or { panic('valid monitor replug snapshot was rejected') }
	assert registry.monitors[0].available
	assert registry.monitors[0].id.slot == removed_id.slot
	assert registry.monitors[0].id.generation == removed_id.generation + 1
	assert registry.monitors[0].name == 'renamed-a'
	mut removed_stale := false
	_ = registry.monitor_index(removed_id) or {
		removed_stale = err.msg() == err_service_request_stale
		-1
	}
	assert removed_stale
	assert registry.monitors[0].id != removed_id

	registry.reconcile_monitor_snapshot([
		x11_registry_monitor_candidate(instance, 202, 0, 1, 'same-name', 225),
	], 5) or { panic('valid second removal snapshot was rejected') }
	replugged_id := registry.monitors[0].id
	registry.reconcile_monitor_snapshot([
		x11_registry_monitor_candidate(instance, 202, 0, 1, 'same-name', 226),
		x11_registry_monitor_candidate(instance, 303, 1, 1, 'same-name', 333),
	], 6) or { panic('valid replacement snapshot was rejected') }
	assert registry.monitors[0].native_key.numeric == 303
	assert registry.monitors[0].id.slot == replugged_id.slot
	assert registry.monitors[0].id.generation == replugged_id.generation + 1
	assert registry.monitors[1].native_key.numeric == 202
	assert registry.monitors[1].id == first_b.id
	mut replugged_stale := false
	_ = registry.monitor_index(replugged_id) or {
		replugged_stale = err.msg() == err_service_request_stale
		-1
	}
	assert replugged_stale
	assert registry.monitors[0].id != replugged_id

	before := registry.monitors.clone()
	registry.reconcile_monitor_snapshot([
		x11_registry_monitor_candidate(instance, 202, 0, 1, 'same-name', 1),
		x11_registry_monitor_candidate(instance, 202, 1, 1, 'same-name', 2),
	], 7) or {
		assert registry.monitors == before
		return
	}
	assert false, 'duplicate native monitor keys were accepted'
}

fn test_monitor_reconciliation_does_not_wrap_exhausted_generation() {
	instance := u64(74)
	mut registry := new_service_registry(instance, .x11)
	exhausted := service_monitor_info_for_slot(x11_registry_monitor_candidate(instance, 404, 0, 1,
		'exhausted', 0), instance, 0, max_u32, false, 1)
	registry.replace_monitors([exhausted])
	reappeared := registry.reconcile_monitor_snapshot([
		x11_registry_monitor_candidate(instance, 404, 0, 1, 'exhausted', 1),
	], 2) or { panic('valid exhausted-generation reappearance was rejected') }
	assert reappeared.len == 1
	assert reappeared[0].id.slot == 1
	assert reappeared[0].id.generation == 1
	assert registry.monitors[0].id.generation == max_u32
	assert !registry.monitors[0].available
}

fn test_monitor_native_key_survives_service_event_sequence_copy() {
	monitor := x11_registry_monitor_candidate(75, 505, 0, 1, 'sequence-copy', 0)
	sequenced := service_event_with_sequence(ServiceEvent{
		kind:     .monitor
		monitor:  monitor
		monitors: [monitor]
	}, 9)
	assert sequenced.monitor.native_key == monitor.native_key
	assert sequenced.monitors.len == 1
	assert sequenced.monitors[0].native_key == monitor.native_key
}

fn test_x11_native_borrow_copy_is_stale_after_callback() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		window := app.create_window(title: 'borrow-stale-proof')!
		_ = app.drain_queued_events()!
		mut copied := NativeWindowBorrow{}
		callback := fn [mut copied] (borrow NativeWindowBorrow) ! {
			copied = borrow
		}
		app.with_native_window_for_gg(window, callback)!
		app.validate_native_borrow_for_gg(window, copied.epoch_for_gg()) or {
			assert err.msg() == err_native_borrow_stale
			app.stop()!
			return
		}
		assert false, 'copied X11 native borrow remained valid after its callback'
	}
}

fn test_x11_stop_purges_unfinished_clipboard_state() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		owner := app.create_window(title: 'clipboard-unresponsive-owner')!
		requestor := app.create_window(title: 'clipboard-stop-requestor')!
		_ = app.drain_queued_events()!
		app.backend.x11.service_make_clipboard_peer_unresponsive_for_test(owner)!
		_ = app.service_request_clipboard_text(requestor)!
		reads, _ := app.backend.x11.service_clipboard_pending_counts_for_test()
		assert reads == 1
		app.stop()!
		reads_after, transfers_after := app.backend.x11.service_clipboard_pending_counts_for_test()
		assert reads_after == 0
		assert transfers_after == 0
	}
}

fn test_x11_native_borrow_defers_destroy_until_callback_return() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		window := app.create_window(title: 'borrow-destroy-proof')!
		_ = app.drain_queued_events()!
		app_ptr := unsafe { voidptr(app) }
		callback := fn [app_ptr, window] (_ NativeWindowBorrow) ! {
			mut owner := unsafe { &App(app_ptr) }
			owner.destroy_window(window)!
			assert owner.window_exists(window)
			assert owner.backend.x11.window_record_index(window) != none
		}
		app.with_native_window_for_gg(window, callback)!
		assert !app.window_exists(window)
		_ = app.backend.x11.window_record_index(window) or {
			app.stop()!
			return
		}
		assert false, 'X11 native window survived deferred destroy flush'
	}
}

fn test_x11_native_borrow_defers_stop_until_callback_return() {
	$if linux && x_multiwindow_x11 ? {
		if os.getenv('DISPLAY') == '' {
			return
		}
		mut app := new_app(backend: .x11)!
		window := app.create_window(title: 'borrow-stop-proof')!
		_ = app.drain_queued_events()!
		app_ptr := unsafe { voidptr(app) }
		callback := fn [app_ptr] (_ NativeWindowBorrow) ! {
			mut owner := unsafe { &App(app_ptr) }
			owner.stop()!
			assert owner.status() == .running
		}
		app.with_native_window_for_gg(window, callback)!
		assert app.status() == .stopped
	}
}
