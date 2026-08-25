module multiwindow

import os

$if darwin {
	#flag darwin -fobjc-arc
	#flag darwin -framework Cocoa
	#include "@VMODROOT/vlib/x/multiwindow/testdata/appkit_nonreadback_test_helpers.h"

	fn C.v_multiwindow_appkit_test_invoke_fullscreen_failure(window voidptr, entering int) int
	fn C.v_multiwindow_appkit_test_invoke_metrics_notification(window voidptr, notification_kind int) int
	fn C.v_multiwindow_appkit_test_change_bounds_and_invoke_metrics_notification(window voidptr, notification_kind int, width int, height int) int
	fn C.v_multiwindow_appkit_test_window_identities(window voidptr, out_window &u64, out_private_handle &u64, out_first_responder &u64) int
	fn C.v_multiwindow_appkit_test_current_first_responder(window voidptr, out_first_responder &u64) int
	fn C.v_multiwindow_appkit_test_close_window(window voidptr, perform_close int) int
	fn C.v_multiwindow_appkit_test_install_release_services_counter(window voidptr) int
	fn C.v_multiwindow_appkit_test_release_services_count() int
	fn C.v_multiwindow_appkit_test_restore_release_services_counter()
	fn C.v_multiwindow_appkit_test_install_clipboard_failure(mode int) int
	fn C.v_multiwindow_appkit_test_restore_clipboard_failure()
	fn C.v_multiwindow_appkit_test_seed_multitype_clipboard() int
	fn C.v_multiwindow_appkit_test_multitype_clipboard_matches_seed() int
	fn C.v_multiwindow_appkit_test_clipboard_has_external_takeover() int
}

struct AppKitFinalBorrowProbe {
mut:
	first_window                 u64
	first_private                u64
	first_responder              u64
	first_responder_after_focus  u64
	second_window                u64
	second_private               u64
	second_responder             u64
	second_responder_after_focus u64
	nested_borrow_reached        bool
	invoked                      int
	counter_installed            int
}

fn appkit_final_source(name string) string {
	return os.read_file(os.join_path(@DIR, name)) or { panic(err) }
}

fn appkit_final_method_body(source string, signature string) string {
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

fn appkit_final_assert_source_order(source string, first string, second string) {
	first_index := source.index(first) or { -1 }
	second_index := source.index(second) or { -1 }
	assert first_index >= 0, 'missing source marker `${first}`'
	assert second_index > first_index, '`${second}` must follow `${first}`'
}

fn appkit_final_runtime_probes_required() bool {
	return os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') == '1'
		&& os.getenv('VGG_MULTIWINDOW_RUNTIME_BACKEND') == 'appkit'
}

fn test_appkit_fullscreen_failure_callbacks_queue_authoritative_fullscreen_terminal() {
	native := appkit_final_source('appkit_backend.m')
	for signature in ['- (void)windowDidFailToEnterFullScreen:',
		'- (void)windowDidFailToExitFullScreen:'] {
		body := appkit_final_method_body(native, signature)
		assert body.contains('queueServiceStateForOperation:V_MULTIWINDOW_APPKIT_SERVICE_FULLSCREEN'), '${signature} does not queue a canonical fullscreen terminal snapshot'
	}
}

fn test_appkit_screen_callbacks_queue_window_metrics_without_resize() {
	native := appkit_final_source('appkit_backend.m')
	for signature in ['- (void)windowDidChangeScreen:', '- (void)windowDidChangeBackingProperties:'] {
		body := appkit_final_method_body(native, signature)
		assert body.contains('queueService'), '${signature} refreshes state but does not queue a window metrics snapshot'
	}
	parameters := appkit_final_method_body(native, '- (void)screenParametersDidChange:')
	assert parameters.contains('queue_service_windows') || parameters.contains('queueService'), 'screenParametersDidChange refreshes windows but does not queue per-window metrics snapshots'
}

fn test_appkit_native_fullscreen_failure_terminal_is_authoritative() {
	$if darwin {
		if !appkit_final_runtime_probes_required() {
			return
		}
		mut app := new_app(backend: .appkit)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(title: 'AppKit fullscreen failure terminal', visible: false)!
		app.poll_events()!
		_ = app.drain_queued_events()!

		for entering in [1, 0] {
			expected := app.backend.appkit.service_window_state(window)!
			mut probe := &AppKitFinalBorrowProbe{}
			invoke := fn [entering, mut probe] (borrow NativeWindowBorrow) ! {
				probe.invoked = C.v_multiwindow_appkit_test_invoke_fullscreen_failure(borrow.primary_for_gg(),
					entering)
			}
			app.with_native_window_for_gg(window, invoke)!
			assert probe.invoked == 1
			app.poll_events()!
			events := app.drain_queued_events()!
			terminals := events.filter(it.kind == .service && it.service.window == window
				&& it.service.kind == .state && it.service.operation == .fullscreen)
			assert terminals.len == 1, 'fullscreen failure produced ${terminals.len} canonical terminal events'

			assert terminals[0].service.state.fullscreen == expected.fullscreen
			assert terminals[0].service.state.sequence == terminals[0].service.sequence
		}
	}
}

fn test_appkit_native_screen_notifications_publish_window_metrics_revision_without_resize() {
	$if darwin {
		if !appkit_final_runtime_probes_required() {
			return
		}
		mut app := new_app(backend: .appkit)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(
			title:    'AppKit screen metrics snapshot'
			width:    333
			height:   211
			visible:  false
			high_dpi: true
		)!
		_ = app.backend.appkit.service_monitor_snapshot(app.instance_id)!
		app.poll_events()!
		_ = app.drain_queued_events()!

		mut last_sequence := u64(0)
		for notification_kind in [1, 2, 3] {
			before_revision := C.v_multiwindow_appkit_service_monitor_revision()
			mut probe := &AppKitFinalBorrowProbe{}
			invoke := fn [notification_kind, mut probe] (borrow NativeWindowBorrow) ! {
				probe.invoked = C.v_multiwindow_appkit_test_invoke_metrics_notification(borrow.primary_for_gg(),
					notification_kind)
			}
			app.with_native_window_for_gg(window, invoke)!
			assert probe.invoked == 1
			app.poll_events()!
			events := app.drain_queued_events()!
			assert events.filter(it.kind == .lifecycle && it.lifecycle.kind == .window_resized).len == 0
			metrics := events.filter(it.kind == .service && it.service.window == window
				&& it.service.kind == .metrics)
			assert metrics.len == 1, 'notification ${notification_kind} produced ${metrics.len} window metrics snapshots'

			snapshot := metrics[0].service
			assert snapshot.state.monitor_ids.len == 1
			assert snapshot.metrics.logical_width == f32(333)
			assert snapshot.metrics.logical_height == f32(211)
			assert snapshot.metrics.dpi_scale > 0
			assert snapshot.sequence > last_sequence
			assert snapshot.state.sequence == snapshot.sequence
			assert snapshot.metrics.metrics_sequence == snapshot.sequence
			last_sequence = snapshot.sequence
			if notification_kind == 3 {
				assert C.v_multiwindow_appkit_service_monitor_revision() > before_revision
			}
		}
	}
}

fn test_appkit_monitor_refresh_precedes_dependent_metrics_and_retries_atomically() {
	$if darwin {
		if !appkit_final_runtime_probes_required() {
			return
		}
		mut app := new_app(backend: .appkit)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(
			title:    'AppKit monitor-before-metrics'
			width:    271
			height:   173
			visible:  false
			high_dpi: true
		)!
		_ = app.backend.appkit.service_monitor_snapshot(app.instance_id)!
		app.poll_events()!
		_ = app.drain_queued_events()!
		before_revision := app.backend.appkit.service_monitor_revision
		app.backend.appkit.service_monitors.clear()
		app.backend.appkit.monitor_failures_for_test = 1

		mut probe := &AppKitFinalBorrowProbe{}
		invoke := fn [mut probe] (borrow NativeWindowBorrow) ! {
			probe.invoked =
				C.v_multiwindow_appkit_test_invoke_metrics_notification(borrow.primary_for_gg(), 3)
		}
		app.with_native_window_for_gg(window, invoke)!
		assert probe.invoked == 1
		mut failed := false
		app.backend.appkit.poll_queued_events() or {
			assert err.msg() == err_capability_unsupported
			failed = true
			[]QueuedEvent{}
		}
		assert failed
		assert app.backend.appkit.service_monitors.len == 0
		assert app.backend.appkit.service_monitor_revision == before_revision
		assert app.drain_queued_events()!.len == 0

		app.poll_events()!
		events := app.drain_queued_events()!
		mut monitor_position := -1
		mut metrics_position := -1
		mut metrics := ServiceEvent{}
		for index, event in events {
			if event.kind == .service && event.service.kind == .monitor && monitor_position < 0 {
				monitor_position = index
			}
			if event.kind == .service && event.service.kind == .metrics
				&& event.service.window == window && metrics_position < 0 {
				metrics_position = index
				metrics = event.service
			}
		}
		assert monitor_position >= 0
		assert metrics_position > monitor_position
		assert metrics.state.monitor_membership_observed
		assert metrics.state.monitor_ids.len == 1
		assert app.service_monitor_info(metrics.state.monitor_ids[0])!.available
		assert app.backend.appkit.monitor_failures_for_test == 0
	}
}

fn test_appkit_synchronous_clipboard_operations_reserve_terminal_before_native_access() {
	$if darwin {
		if !appkit_final_runtime_probes_required() {
			return
		}
		mut app := new_app(backend: .appkit)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(title: 'AppKit clipboard delivery preflight', visible: false)!
		_ = app.drain_queued_events()!
		before := app.service_set_clipboard_text(window, 'before-exhaustion')!
		before_events := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == before)
		assert before_events.len == 1
		request_before := app.services.next_request
		pending_before := app.services.pending.len

		app.state_mutex.lock()
		saved_delivery_token := app.next_event_delivery_token
		app.next_event_delivery_token = 0
		app.state_mutex.unlock()
		mut write_rejected := false
		app.service_set_clipboard_text(window, 'must-not-commit') or {
			assert err.msg() == err_event_delivery_exhausted
			write_rejected = true
			ServiceRequestId{}
		}
		mut read_rejected := false
		app.service_request_clipboard_text(window) or {
			assert err.msg() == err_event_delivery_exhausted
			read_rejected = true
			ServiceRequestId{}
		}
		assert write_rejected
		assert read_rejected
		assert app.services.next_request == request_before
		assert app.services.pending.len == pending_before
		assert app.drain_queued_events()!.len == 0

		app.state_mutex.lock()
		app.next_event_delivery_token = saved_delivery_token
		app.state_mutex.unlock()
		read_request := app.service_request_clipboard_text(window)!
		read_events := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == read_request)
		assert read_events.len == 1
		assert read_events[0].service.clipboard.status == .ready
		assert read_events[0].service.clipboard.text == 'before-exhaustion'
	}
}

fn test_appkit_clipboard_replacement_is_bounded_transaction_or_failed_terminal() {
	native := appkit_final_source('appkit_backend.m')
	backend := appkit_final_source('appkit_backend.c.v')
	dispatch := appkit_final_source('service_backend.v')
	api := appkit_final_source('service_api.v')
	assert native.contains('v_multiwindow_appkit_snapshot_pasteboard')
	assert native.contains('v_multiwindow_appkit_restore_pasteboard_snapshot')
	assert native.contains('V_MULTIWINDOW_APPKIT_SERVICE_CLIPBOARD_SNAPSHOT_MAX_ITEMS')
	assert native.contains('V_MULTIWINDOW_APPKIT_SERVICE_CLIPBOARD_SNAPSHOT_MAX_TYPES')
	assert native.contains('V_MULTIWINDOW_APPKIT_SERVICE_RESULT_INDETERMINATE')
	set_body := appkit_final_method_body(native,
		'int v_multiwindow_appkit_service_set_clipboard_text(')
	appkit_final_assert_source_order(set_body, 'v_multiwindow_appkit_snapshot_pasteboard',
		'[pasteboard clearContents]')
	appkit_final_assert_source_order(set_body, '@try {',
		'pasteboard = [NSPasteboard generalPasteboard]')
	appkit_final_assert_source_order(set_body, 'clear_attempted = YES',
		'[pasteboard clearContents]')
	assert set_body.contains('if (!clear_attempted)')
	assert native.contains('[expected_types isEqualToArray:actual_types]')
	appkit_final_assert_source_order(set_body, '[pasteboard clearContents]',
		'[pasteboard setString:')
	appkit_final_assert_source_order(set_body, '[pasteboard setString:',
		'v_multiwindow_appkit_restore_pasteboard_snapshot')
	assert backend.contains('service_appkit_result_indeterminate')
	assert backend.contains('status:    .failed')
	assert dispatch.replace(' ', '').contains('statusServiceStatus')
	assert api.contains('complete_native_clipboard_start(')
}

fn test_appkit_clipboard_replacement_rolls_back_all_types_or_publishes_failed_terminal() {
	$if darwin {
		if !appkit_final_runtime_probes_required() {
			return
		}
		mut app := new_app(backend: .appkit)!
		defer {
			C.v_multiwindow_appkit_test_restore_clipboard_failure()
			app.stop() or {}
		}
		window := app.create_window(
			title:   'AppKit clipboard replacement transaction'
			visible: false
		)!
		_ = app.drain_queued_events()!

		for mode in [5, 1, 2] {
			assert C.v_multiwindow_appkit_test_seed_multitype_clipboard() == 1
			assert C.v_multiwindow_appkit_test_install_clipboard_failure(mode) == 1
			request_before := app.services.next_request
			mut rejected := false
			app.service_set_clipboard_text(window, 'replacement') or {
				assert err.msg() == err_capability_unsupported
				rejected = true
				ServiceRequestId{}
			}
			C.v_multiwindow_appkit_test_restore_clipboard_failure()
			assert rejected
			assert app.services.next_request == request_before + 1
			assert C.v_multiwindow_appkit_test_multitype_clipboard_matches_seed() == 1
			assert app.drain_queued_events()!.len == 0
			retry := app.service_set_clipboard_text(window, 'retry-${mode}')!
			assert retry.serial > request_before
			retry_events := app.drain_queued_events()!.filter(it.kind == .service
				&& it.service.kind == .clipboard && it.service.clipboard.id == retry)
			assert retry_events.len == 1
			assert retry_events[0].service.clipboard.status == .ready
			assert retry_events[0].service.clipboard.text == 'retry-${mode}'
			assert app.drain_queued_events()!.len == 0
		}

		for mode in [3, 4] {
			assert C.v_multiwindow_appkit_test_seed_multitype_clipboard() == 1
			assert C.v_multiwindow_appkit_test_install_clipboard_failure(mode) == 1
			request := app.service_set_clipboard_text(window, 'replacement')!
			C.v_multiwindow_appkit_test_restore_clipboard_failure()
			events := app.drain_queued_events()!.filter(it.kind == .service
				&& it.service.kind == .clipboard && it.service.clipboard.id == request)
			assert events.len == 1
			assert events[0].service.clipboard.status == .failed
			assert events[0].service.clipboard.error == err_capability_unsupported
			assert app.drain_queued_events()!.len == 0
			if mode == 3 {
				assert C.v_multiwindow_appkit_test_clipboard_has_external_takeover() == 1
			}
		}

		success := app.service_set_clipboard_text(window, 'committed')!
		events := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .clipboard && it.service.clipboard.id == success)
		assert events.len == 1
		assert events[0].service.clipboard.status == .ready
		assert events[0].service.clipboard.text == 'committed'
	}
}

fn test_appkit_native_screen_and_backing_dimension_change_publish_one_metrics_snapshot() {
	$if darwin {
		if !appkit_final_runtime_probes_required() {
			return
		}
		mut app := new_app(backend: .appkit)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(
			title:    'AppKit changed metrics snapshot'
			width:    320
			height:   200
			visible:  false
			high_dpi: true
		)!
		_ = app.backend.appkit.service_monitor_snapshot(app.instance_id)!
		app.poll_events()!
		_ = app.drain_queued_events()!

		for notification_kind in [1, 2] {
			logical_width := 320 + notification_kind * 17
			logical_height := 200 + notification_kind * 11
			mut probe := &AppKitFinalBorrowProbe{}
			invoke := fn [notification_kind, logical_width, logical_height, mut probe] (borrow NativeWindowBorrow) ! {
				probe.invoked = C.v_multiwindow_appkit_test_change_bounds_and_invoke_metrics_notification(borrow.primary_for_gg(),
					notification_kind, logical_width, logical_height)
			}
			app.with_native_window_for_gg(window, invoke)!
			assert probe.invoked == 1
			app.poll_events()!
			events := app.drain_queued_events()!
			metrics := events.filter(it.kind == .service && it.service.window == window
				&& it.service.kind == .metrics)
			assert metrics.len == 1, 'dimension-changing notification ${notification_kind} produced ${metrics.len} window metrics snapshots'
			assert metrics[0].service.metrics.logical_width == f32(logical_width)
			assert metrics[0].service.metrics.logical_height == f32(logical_height)
			assert metrics[0].service.state.sequence == metrics[0].service.sequence
			assert metrics[0].service.metrics.metrics_sequence == metrics[0].service.sequence
		}
	}
}

fn test_appkit_native_two_window_close_retains_services_until_finish_and_releases_once() {
	$if darwin {
		if !appkit_final_runtime_probes_required() {
			return
		}
		mut app := new_app(backend: .appkit)!
		defer {
			C.v_multiwindow_appkit_test_restore_release_services_counter()
			app.stop() or {}
		}
		first := app.create_window(title: 'AppKit performClose', visible: false)!
		second := app.create_window(title: 'AppKit direct close', visible: false)!
		app.poll_events()!
		_ = app.drain_queued_events()!

		mut probe := &AppKitFinalBorrowProbe{}
		app_ptr := unsafe { voidptr(app) }
		inner := fn [mut probe] (borrow NativeWindowBorrow) ! {
			probe.nested_borrow_reached = true
			assert C.v_multiwindow_appkit_test_window_identities(borrow.primary_for_gg(),
				&probe.second_window, &probe.second_private, &probe.second_responder) == 1
		}
		outer := fn [mut probe, app_ptr, second, inner] (borrow NativeWindowBorrow) ! {
			assert C.v_multiwindow_appkit_test_window_identities(borrow.primary_for_gg(),
				&probe.first_window, &probe.first_private, &probe.first_responder) == 1
			mut owner := unsafe { &App(app_ptr) }
			owner.with_native_window_for_gg(second, inner)!
		}
		app.with_native_window_for_gg(first, outer)!
		assert probe.nested_borrow_reached
		assert probe.first_window != 0 && probe.second_window != 0
		assert probe.first_window != probe.second_window
		assert probe.first_private != probe.first_window
		assert probe.second_private != probe.second_window
		assert probe.first_private != probe.second_private
		assert probe.first_responder != probe.first_window
		assert probe.second_responder != probe.second_window
		assert probe.first_responder != probe.second_responder

		app.service_request_focus(first)!
		app.poll_events()!
		read_first_responder := fn [mut probe] (borrow NativeWindowBorrow) ! {
			assert C.v_multiwindow_appkit_test_current_first_responder(borrow.primary_for_gg(),
				&probe.first_responder_after_focus) == 1
		}
		app.with_native_window_for_gg(first, read_first_responder)!
		assert probe.first_responder_after_focus == probe.first_responder

		app.service_request_focus(second)!
		app.poll_events()!
		read_second_responder := fn [mut probe] (borrow NativeWindowBorrow) ! {
			assert C.v_multiwindow_appkit_test_current_first_responder(borrow.primary_for_gg(),
				&probe.second_responder_after_focus) == 1
		}
		app.with_native_window_for_gg(second, read_second_responder)!
		assert probe.second_responder_after_focus == probe.second_responder
		_ = app.drain_queued_events()!

		probe.invoked = 0
		perform_close := fn [mut probe] (borrow NativeWindowBorrow) ! {
			probe.invoked = C.v_multiwindow_appkit_test_close_window(borrow.primary_for_gg(), 1)
		}
		app.with_native_window_for_gg(first, perform_close)!
		assert probe.invoked == 1
		app.poll_events()!
		perform_events := app.drain_queued_events()!
		assert perform_events.filter(it.kind == .lifecycle
			&& it.lifecycle.kind == .window_close_requested && it.lifecycle.window_id == first).len == 1
		assert app.services.window_index(first)! >= 0
		first_backend_index := app.backend.appkit.window_record_index(first) or { -1 }
		assert first_backend_index >= 0

		probe.invoked = 0
		probe.counter_installed = 0
		close_direct := fn [mut probe] (borrow NativeWindowBorrow) ! {
			probe.counter_installed =
				C.v_multiwindow_appkit_test_install_release_services_counter(borrow.primary_for_gg())
			probe.invoked = C.v_multiwindow_appkit_test_close_window(borrow.primary_for_gg(), 0)
		}
		app.with_native_window_for_gg(second, close_direct)!
		assert probe.counter_installed == 1
		assert probe.invoked == 1
		app.poll_events()!
		notices := app.drain_render_teardown_notices()!
		assert notices.len == 1 && notices[0].window == second
		assert app.services.window_index(second)! >= 0
		backend_index := app.backend.appkit.window_record_index(second) or { -1 }
		assert backend_index >= 0
		assert app.backend.appkit.windows[backend_index].native_destroyed
		assert !app.backend.appkit.windows[backend_index].services_released
		assert app.backend.appkit.windows[backend_index].state != unsafe { nil }
		mut sealed_state_error := ''
		_ = app.service_window_state(second) or {
			sealed_state_error = err.msg()
			ServiceWindowState{}
		}
		assert sealed_state_error == err_stale_window
		assert C.v_multiwindow_appkit_test_release_services_count() == 0

		app.finish_window_destroy(notices[0].ticket, []string{})!
		assert C.v_multiwindow_appkit_test_release_services_count() == 1
		app.finish_window_destroy(notices[0].ticket, []string{})!
		assert C.v_multiwindow_appkit_test_release_services_count() == 1
		terminal := app.drain_queued_events()!
		assert terminal.filter(it.kind == .lifecycle && it.lifecycle.kind == .window_destroyed
			&& it.lifecycle.window_id == second).len == 1
		assert app.drain_queued_events()!.len == 0
		app.services.window_index(second) or {
			assert err.msg() == err_stale_window
			app.backend.appkit.window_record_index(second) or {
				C.v_multiwindow_appkit_test_restore_release_services_counter()
				app.destroy_window(first)!
				return
			}
			assert false, 'AppKit backend retained the finished direct-close record'
			return
		}
		assert false, 'Package2 retained the finished direct-close service record'
	}
}
