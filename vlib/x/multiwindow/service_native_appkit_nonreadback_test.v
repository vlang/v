module multiwindow

import os

$if darwin {
	#flag darwin -fobjc-arc
	#flag darwin -framework Cocoa
	#include "@VMODROOT/vlib/x/multiwindow/testdata/appkit_nonreadback_test_helpers.h"

	fn C.v_multiwindow_appkit_test_set_clipboard_ascii_payload(length usize) int
	fn C.v_multiwindow_appkit_test_install_release_mouse_lock_failure(window voidptr) int
	fn C.v_multiwindow_appkit_test_restore_release_mouse_lock()
	fn C.v_multiwindow_appkit_test_window_is_visible(window voidptr) int
	fn C.v_multiwindow_appkit_test_window_is_minimized(window voidptr) int
	fn C.v_multiwindow_appkit_test_attach_accessibility_child(window voidptr) int
	fn C.v_multiwindow_appkit_test_accessibility_child_is_detached() int
	fn C.v_multiwindow_appkit_test_release_accessibility_child()
}

struct AppKitNonreadbackCallbackState {
mut:
	result int
}

fn appkit_nonreadback_source(name string) string {
	return os.read_file(os.join_path(@DIR, name)) or { panic(err) }
}

fn appkit_nonreadback_compact(source string) string {
	return source.replace('\r', '').replace('\n', '').replace('\t', '').replace(' ', '')
}

fn appkit_nonreadback_body(source string, signature string) string {
	start := source.index(signature) or { return '' }
	relative_open := source[start..].index('{') or { return '' }
	open := start + relative_open
	mut depth := 0
	for index := open; index < source.len; index++ {
		if source[index] == `{` {
			depth++
		} else if source[index] == `}` {
			depth--
			if depth == 0 {
				return source[open + 1..index]
			}
		}
	}
	return ''
}

fn appkit_native_nonreadback_probes_required() bool {
	return os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') == '1'
		&& os.getenv('VGG_MULTIWINDOW_RUNTIME_BACKEND') == 'appkit'
}

fn test_appkit_move_active_fullscreen_and_occlusion_enter_canonical_queue() {
	mut app := new_app()!
	id := app.create_window()!
	_ = app.drain_queued_events()!
	record := AppKitWindowRecord{
		id: id
	}
	position := appkit_service_events_from_snapshot(record, AppKitServiceQueuedSnapshot{
		valid:     true
		kind:      1
		operation: int(ServiceOperation.position)
		state:     AppKitServiceRawWindowState{
			position_known: 1
			x:              31
			y:              47
		}
	}, [], id.app_instance)
	assert position.len == 1
	assert position[0].kind == .service
	assert position[0].service.operation == .position
	assert position[0].service.state.position == ServicePosition{
		known: true
		x:     31
		y:     47
	}

	active := appkit_service_events_from_snapshot(record, AppKitServiceQueuedSnapshot{
		valid:     true
		kind:      1
		operation: int(ServiceOperation.focus)
		state:     AppKitServiceRawWindowState{
			active:  2
			focused: 2
		}
	}, [], id.app_instance)
	assert active.len == 1
	assert active[0].service.state.active == .on
	assert active[0].service.state.focused == .on

	fullscreen := appkit_service_events_from_snapshot(record, AppKitServiceQueuedSnapshot{
		valid:     true
		kind:      1
		operation: int(ServiceOperation.fullscreen)
		state:     AppKitServiceRawWindowState{
			fullscreen: 2
		}
	}, [], id.app_instance)
	assert fullscreen.len == 1
	assert fullscreen[0].service.operation == .fullscreen
	assert fullscreen[0].service.state.fullscreen == .on

	occluded := appkit_service_events_from_snapshot(record, AppKitServiceQueuedSnapshot{
		valid:     true
		kind:      1
		operation: int(ServiceOperation.show)
		state:     AppKitServiceRawWindowState{
			mapping:    2
			visibility: 3
		}
	}, [], id.app_instance)
	assert occluded.len == 1
	assert occluded[0].service.state.mapping == .mapped
	assert occluded[0].service.state.visibility == .occluded
	accepted :=
		app.accept_backend_event_batch([position[0], active[0], fullscreen[0], occluded[0]], 1)!
	assert accepted.accepted == 4
	canonical := app.drain_queued_events()!
	assert canonical.map(it.service.operation) == [.position, .focus, .fullscreen, .show]
	app.stop()!

	header := appkit_nonreadback_source('appkit_backend_helpers.h')
	assert header.contains('V_MULTIWINDOW_APPKIT_EVENT_SERVICE')
	assert header.contains('service_snapshot_valid')
}

fn test_appkit_capabilities_are_queried_for_the_requested_window() {
	api := appkit_nonreadback_compact(appkit_nonreadback_source('service_api.v'))
	dispatch := appkit_nonreadback_compact(appkit_nonreadback_source('service_backend.v'))
	backend := appkit_nonreadback_compact(appkit_nonreadback_source('appkit_backend.c.v'))
	mut missing := []string{}
	if !api.contains('app.backend.service_operation_capability(id,operation)') {
		missing << 'public service API drops WindowId'
	}
	if !dispatch.contains('fn(backend&Backend)service_operation_capability(idWindowId,operationServiceOperation)') {
		missing << 'backend dispatch has no WindowId'
	}
	if !dispatch.contains('.appkit{backend.appkit.service_operation_capability(id,operation)}') {
		missing << 'AppKit dispatch does not forward WindowId'
	}
	if !backend.contains('fn(backend&AppKitBackend)service_operation_capability(idWindowId,operationServiceOperation)') {
		missing << 'AppKit selects an arbitrary live window'
	}
	assert missing.len == 0, 'AppKit per-window capability path is incomplete: ${missing}'

	$if darwin {
		if !appkit_native_nonreadback_probes_required() {
			return
		}
		mut app := new_app(backend: .appkit)!
		defer {
			app.stop() or {}
		}
		titled := app.create_window(
			title:      'AppKit titled capability'
			visible:    false
			resizable:  true
			borderless: false
		)!
		borderless := app.create_window(
			title:      'AppKit borderless capability'
			visible:    false
			resizable:  false
			borderless: true
		)!
		assert app.service_operation_capability(titled, .titlebar_appearance)!.support == .available
		assert app.service_operation_capability(borderless, .titlebar_appearance)!.support == .unsupported
		assert app.service_operation_capability(titled, .maximize)!.support == .available
		assert app.service_operation_capability(borderless, .maximize)!.support == .unsupported
	}
}

fn test_appkit_monitor_event_precedes_only_dependent_window_observations() {
	state := queued_service_event(ServiceEvent{
		kind:  .state
		state: ServiceWindowState{
			monitor_membership_observed: true
		}
	})
	metrics := queued_service_event(ServiceEvent{
		kind:  .metrics
		state: ServiceWindowState{
			monitor_membership_observed: true
		}
	})
	unknown := queued_service_event(ServiceEvent{
		kind: .state
	})
	monitor := queued_service_event(ServiceEvent{
		kind: .monitor
	})
	assert appkit_service_event_depends_on_monitor_snapshot(state)
	assert appkit_service_event_depends_on_monitor_snapshot(metrics)
	assert !appkit_service_event_depends_on_monitor_snapshot(unknown)
	assert !appkit_service_event_depends_on_monitor_snapshot(monitor)
}

fn test_appkit_duplicate_metrics_observations_are_coalesced_per_poll() {
	window := WindowId{
		app_instance: 7
		slot:         2
		generation:   1
	}
	metrics := queued_service_event(ServiceEvent{
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
	})
	assert !appkit_service_metrics_event_already_queued([], metrics)
	assert appkit_service_metrics_event_already_queued([metrics], metrics)
	different := queued_service_event(ServiceEvent{
		...metrics.service
		metrics: RenderMetricsSnapshot{
			...metrics.service.metrics
			logical_width: 321
		}
	})
	assert !appkit_service_metrics_event_already_queued([metrics], different)
}

fn test_appkit_focus_request_and_delegate_publish_once() {
	api := appkit_nonreadback_source('service_api.v')
	publish_body := appkit_nonreadback_body(api, 'fn (mut app App) publish_native_state')
	assert publish_body.contains('service_state_publication_is_deferred')
	assert appkit_service_capability_from_native(.focus, 1, false).asynchronous

	mut common := new_app()!
	window := common.create_window()!
	_ = common.drain_queued_events()!
	focus := queued_service_event(ServiceEvent{
		kind:      .state
		window:    window
		operation: .focus
		state:     ServiceWindowState{
			active:  .on
			focused: .on
		}
	})
	assert common.accept_backend_event_batch([focus], 1)!.accepted == 1
	assert common.accept_backend_event_batch([focus], 2)!.accepted == 0
	events := common.drain_queued_events()!
	assert events.filter(it.kind == .service && it.service.operation == .focus).len == 1
	common.stop()!

	$if darwin {
		if !appkit_native_nonreadback_probes_required() {
			return
		}
		mut app := new_app(backend: .appkit)!
		defer {
			app.stop() or {}
		}
		native_window := app.create_window(
			title:   'AppKit focus exactly once'
			visible: false
		)!
		app.poll_events()!
		_ = app.drain_queued_events()!
		app.service_request_focus(native_window)!
		app.poll_events()!
		native_events := app.drain_queued_events()!
		mut focus_events := 0
		for event in native_events {
			if event.kind == .service && event.service.window == native_window
				&& event.service.kind == .state && event.service.operation == .focus {
				focus_events++
			}
		}
		assert focus_events == 1, 'focus request produced ${focus_events} canonical state events'
	}
}

fn test_appkit_hide_failure_rolls_back_before_window_hide() {
	assert appkit_hide_requires_rollback(service_appkit_result_failed)
	assert !appkit_hide_requires_rollback(service_appkit_result_ok)
	common := appkit_nonreadback_body(appkit_nonreadback_source('appkit_backend.c.v'),
		'fn (mut backend AppKitBackend) service_hide_window')
	assert common.contains('v_multiwindow_appkit_service_show_window')

	$if darwin {
		if !appkit_native_nonreadback_probes_required() {
			return
		}
		mut app := new_app(backend: .appkit)!
		defer {
			C.v_multiwindow_appkit_test_restore_release_mouse_lock()
			app.stop() or {}
		}
		window := app.create_window(title: 'AppKit hide rollback', visible: true)!
		mut install_state := &AppKitNonreadbackCallbackState{}
		install := fn [mut install_state] (borrow NativeWindowBorrow) ! {
			install_state.result =
				C.v_multiwindow_appkit_test_install_release_mouse_lock_failure(borrow.primary_for_gg())
		}
		app.with_native_window_for_gg(window, install)!
		assert install_state.result == 1
		app.service_hide_window(window) or {
			mut visibility_state := &AppKitNonreadbackCallbackState{}
			inspect := fn [mut visibility_state] (borrow NativeWindowBorrow) ! {
				visibility_state.result =
					C.v_multiwindow_appkit_test_window_is_visible(borrow.primary_for_gg())
			}
			app.with_native_window_for_gg(window, inspect)!
			assert visibility_state.result == 1
			return
		}
		assert false, 'AppKit hide unexpectedly succeeded after mouse reassociation failure'
	}
}

fn test_appkit_minimize_failure_stops_before_window_mutation() {
	native := appkit_nonreadback_source('appkit_backend.m')
	body := appkit_nonreadback_body(native, 'int v_multiwindow_appkit_service_minimize_window')
	unlock := body.index('if (![state releaseMouseLock])') or { -1 }
	miniaturize := body.index('[state.window miniaturize:nil]') or { -1 }
	assert unlock >= 0 && miniaturize > unlock, 'AppKit minimize mutates the native window before mouse unlock succeeds'

	assert body[unlock..miniaturize].contains('return V_MULTIWINDOW_APPKIT_SERVICE_RESULT_FAILED;')

	$if darwin {
		if !appkit_native_nonreadback_probes_required() {
			return
		}
		mut app := new_app(backend: .appkit)!
		defer {
			C.v_multiwindow_appkit_test_restore_release_mouse_lock()
			app.stop() or {}
		}
		window := app.create_window(title: 'AppKit minimize admission', visible: true)!
		mut install_state := &AppKitNonreadbackCallbackState{}
		install := fn [mut install_state] (borrow NativeWindowBorrow) ! {
			install_state.result =
				C.v_multiwindow_appkit_test_install_release_mouse_lock_failure(borrow.primary_for_gg())
		}
		app.with_native_window_for_gg(window, install)!
		assert install_state.result == 1
		app.service_minimize_window(window) or {
			assert err.msg() == err_capability_unsupported
			mut minimized_state := &AppKitNonreadbackCallbackState{}
			inspect := fn [mut minimized_state] (borrow NativeWindowBorrow) ! {
				minimized_state.result =
					C.v_multiwindow_appkit_test_window_is_minimized(borrow.primary_for_gg())
			}
			app.with_native_window_for_gg(window, inspect)!
			assert minimized_state.result == 0
			return
		}
		assert false, 'AppKit minimize unexpectedly succeeded after mouse reassociation failure'
	}
}

fn test_appkit_direct_close_preserves_terminal_order_and_v_teardown_replay() {
	mut record := AppKitWindowRecord{}
	assert appkit_complete_window_service_release(mut record)
	assert !appkit_complete_window_service_release(mut record)

	native := appkit_nonreadback_source('appkit_backend.m')
	body := appkit_nonreadback_body(native, '- (void)windowWillClose:')
	terminal := body.index('V_MULTIWINDOW_APPKIT_LIFECYCLE_DESTROYED') or { -1 }
	release := body.index('[self releaseServices];') or { -1 }
	assert terminal >= 0 && (release < 0 || terminal < release), 'direct NSWindow close releases Package2 services before the terminal event can order future text teardown'
}

fn test_appkit_accessibility_detaches_children_and_parent_links() {
	native := appkit_nonreadback_source('appkit_backend.m')
	body := appkit_nonreadback_body(native, '- (void)detachAccessibility {')
	assert body.contains('setAccessibilityChildren'), 'AppKit accessibility detach does not inspect attached children'
	assert body.contains('setAccessibilityParent:nil'), 'AppKit accessibility detach clears child arrays but leaves parent links attached'

	$if darwin {
		if !appkit_native_nonreadback_probes_required() {
			return
		}
		mut app := new_app(backend: .appkit)!
		defer {
			C.v_multiwindow_appkit_test_release_accessibility_child()
			app.stop() or {}
		}
		window := app.create_window(title: 'AppKit accessibility detach', visible: false)!
		mut attach_state := &AppKitNonreadbackCallbackState{}
		attach := fn [mut attach_state] (borrow NativeWindowBorrow) ! {
			attach_state.result =
				C.v_multiwindow_appkit_test_attach_accessibility_child(borrow.primary_for_gg())
		}
		app.with_native_window_for_gg(window, attach)!
		assert attach_state.result == 1
		app.destroy_window(window)!
		assert C.v_multiwindow_appkit_test_accessibility_child_is_detached() == 1
	}
}

fn test_appkit_oversized_clipboard_is_capacity_not_unsupported() {
	appkit_require_clipboard_result(service_appkit_result_capacity) or {
		assert err.msg() == err_clipboard_capacity
	}
	appkit_require_clipboard_result(service_appkit_result_unavailable) or {
		assert err.msg() == err_capability_unsupported
	}
	header := appkit_nonreadback_source('appkit_backend_helpers.h')
	assert header.contains('V_MULTIWINDOW_APPKIT_SERVICE_RESULT_CAPACITY -2')

	$if darwin {
		if !appkit_native_nonreadback_probes_required() {
			return
		}
		mut app := new_app(backend: .appkit)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(
			title:   'AppKit clipboard capacity'
			visible: false
		)!
		assert C.v_multiwindow_appkit_test_set_clipboard_ascii_payload(
			usize(service_appkit_clipboard_max_bytes) + 1) == 1
		app.service_request_clipboard_text(window) or {
			assert err.msg() == err_clipboard_capacity
			return
		}
		assert false, 'oversized AppKit clipboard read unexpectedly succeeded'
	}
}
