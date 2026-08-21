// vtest retry: 0
module gg

import os
import time

#flag windows -DV_MULTIWINDOW_WIN32_SERVICE_TEST

$if windows {
	#include "@VMODROOT/vlib/x/multiwindow/testdata/win32_monitor_enumeration_test_storage.h"
}

$if windows && gg_multiwindow ? {
	#include "@VMODROOT/vlib/x/multiwindow/testdata/win32_nonreadback_test_oracle.h"

	fn C.v_multiwindow_win32_service_test_set_focus_refused(refused int)
	fn C.v_multiwindow_test_win32_emit_display_changes(hwnd voidptr, count int) int
	fn C.v_multiwindow_test_win32_emit_display_change(hwnd voidptr) int
	fn C.v_multiwindow_test_win32_monitor_enumeration_capture() int
	fn C.v_multiwindow_test_win32_monitor_enumeration_use_empty()
	fn C.v_multiwindow_test_win32_monitor_enumeration_use_replay() int
	fn C.v_multiwindow_test_win32_monitor_enumeration_reset()
	fn C.v_multiwindow_test_win32_monitor_enumeration_empty_calls() int
	fn C.v_multiwindow_test_win32_monitor_enumeration_replay_calls() int
	fn C.v_multiwindow_test_win32_clipboard_equals(expected &u16, expected_units usize) int
	fn C.v_multiwindow_test_win32_set_clipboard(owner voidptr, text &u16, units usize) int
	fn C.SetForegroundWindow(hwnd voidptr) int
	fn C.GetForegroundWindow() voidptr
	fn C.SetFocus(hwnd voidptr) voidptr
	fn C.GetFocus() voidptr
}

struct Win32PublicBorrowRouteProbe {
mut:
	callback_count int
	accessor_count int
	first_hwnd     voidptr
	second_hwnd    voidptr
	first_epoch    u64
	second_epoch   u64
	first_lease    NativeWindowLease
}

struct Win32PublicHwndProbe {
mut:
	hwnd voidptr
}

fn record_disabled_public_service(label string, err IError, expected string, count int) int {
	assert err.msg() == expected, '${label} returned `${err.msg()}` without -d gg_multiwindow'
	return count + 1
}

fn win32_public_state_is_observed(state WindowState) bool {
	return state.mapping != .unknown && state.visibility != .unknown && state.active != .unknown
		&& state.focused != .unknown && state.minimized != .unknown && state.maximized != .unknown
		&& state.fullscreen != .unknown && state.position.known
}

fn win32_public_hwnd(mut app App, window WindowId) !voidptr {
	mut probe := &Win32PublicHwndProbe{}
	callback := fn [mut probe] (mut lease NativeWindowLease) ! {
		lease.with_win32(fn [mut probe] (borrowed voidptr) ! {
			probe.hwnd = borrowed
		})!
	}
	app.with_native_window(window, callback)!
	if probe.hwnd == unsafe { nil } {
		return error('gg.App.with_native_window returned a nil HWND')
	}
	return probe.hwnd
}

fn win32_public_utf16_units(text string) usize {
	mut units := usize(1)
	for codepoint in text.runes() {
		units += if codepoint > 0xffff { usize(2) } else { usize(1) }
	}
	return units
}

fn win32_public_clipboard_events(events []WindowQueuedEvent, request ClipboardRequestId) []WindowQueuedEvent {
	return events.filter(it.kind == .service && it.service.kind == .clipboard
		&& it.service.clipboard.id == request)
}

fn win32_public_events_are_globally_ordered(events []WindowQueuedEvent) bool {
	mut previous := u64(0)
	for event in events {
		if event.sequence == 0 || event.sequence <= previous {
			return false
		}
		if event.kind == .service && event.service.sequence != event.sequence {
			return false
		}
		previous = event.sequence
	}
	return true
}

fn win32_public_w4_add_infra(mut issues []string, message string) {
	issues << 'PACKAGE2_W4_INFRA=${message}'
}

fn win32_public_poll_until_clipboard(mut app App, request ClipboardRequestId, attempts int, label string, mut issues []string) []WindowQueuedEvent {
	mut delivered := []WindowQueuedEvent{}
	for _ in 0 .. attempts {
		mut poll_failed := false
		app.poll_events() or {
			win32_public_w4_add_infra(mut issues, '${label}: poll failed: ${err.msg()}')
			poll_failed = true
		}
		if poll_failed {
			break
		}
		delivered << app.drain_window_queued_events() or {
			win32_public_w4_add_infra(mut issues, '${label}: event drain failed: ${err.msg()}')
			[]WindowQueuedEvent{}
		}
		if win32_public_clipboard_events(delivered, request).len > 0 {
			break
		}
		time.sleep(5 * time.millisecond)
	}
	return delivered
}

fn win32_public_poll_collect(mut app App, attempts int, label string, mut issues []string) []WindowQueuedEvent {
	mut delivered := []WindowQueuedEvent{}
	for _ in 0 .. attempts {
		mut poll_failed := false
		app.poll_events() or {
			win32_public_w4_add_infra(mut issues, '${label}: poll failed: ${err.msg()}')
			poll_failed = true
		}
		if poll_failed {
			break
		}
		delivered << app.drain_window_queued_events() or {
			win32_public_w4_add_infra(mut issues, '${label}: event drain failed: ${err.msg()}')
			[]WindowQueuedEvent{}
		}
		time.sleep(5 * time.millisecond)
	}
	return delivered
}

fn win32_public_clipboard_envelope_matches(event WindowQueuedEvent, request ClipboardRequestId, window WindowId, operation WindowOperation, status WindowServiceStatus) bool {
	return event.kind == .service && event.sequence > 0 && event.service.kind == .clipboard
		&& event.service.sequence == event.sequence && event.service.operation == operation
		&& event.service.window == window && event.service.clipboard.id == request
		&& event.service.clipboard.window == window && event.service.clipboard.status == status
}

fn win32_public_run_no_opt_clipboard_child() ! {
	parent_compiler := @CCOMPILER
	child_compiler := if parent_compiler == 'tinyc' { 'tcc' } else { parent_compiler }
	temp_dir := os.join_path(os.temp_dir(), 'v_multiwindow_win32_clipboard_no_opt_${os.getpid()}')
	os.mkdir_all(temp_dir)!
	defer {
		os.rmdir_all(temp_dir) or {}
	}
	source_path := os.join_path(@VMODROOT, 'vlib', 'gg', 'testdata',
		'multiwindow_win32_clipboard_no_optin_probe.v')
	output_path := os.join_path(temp_dir, 'clipboard_no_opt.exe')
	old_vflags := os.getenv('VFLAGS')
	os.setenv('VFLAGS', '', true)
	compile_result :=
		os.execute('${os.quoted_path(@VEXE)} -no-retry-compilation -cc ${os.quoted_path(child_compiler)} -gc none -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
	os.setenv('VFLAGS', old_vflags, true)
	assert compile_result.exit_code == 0, 'no-opt child compile failed with ${child_compiler}:\n${compile_result.output}'
	run_result := os.execute(os.quoted_path(output_path))
	assert run_result.exit_code == 0, 'no-opt child failed:\n${run_result.output}'
	assert run_result.output.trim_space() == 'CCOMPILER=${parent_compiler}'
}

fn win32_public_request_refused_focus(mut app App, window WindowId) ! {
	$if windows && gg_multiwindow ? {
		C.v_multiwindow_win32_service_test_set_focus_refused(1)
		defer {
			C.v_multiwindow_win32_service_test_set_focus_refused(0)
		}
		app.request_window_focus(window)!
	} $else $if windows {
		_ = app
		_ = window
		return error(err_multiwindow_not_enabled)
	} $else {
		_ = app
		_ = window
		return error('Win32 focus refusal test helper is unavailable')
	}
}

fn test_win32_public_services_stay_disabled_without_opt_in() {
	$if !gg_multiwindow ? {
		mut app := App{}
		window := WindowId{}
		mut rejected := 0
		mut callback_called := false

		_ = app.window_state(window) or {
			rejected = record_disabled_public_service('window_state', err,
				err_multiwindow_not_enabled, rejected)
			WindowState{}
		}
		_ = app.window_operation_capability(window, .native_borrow) or {
			rejected = record_disabled_public_service('window_operation_capability', err,
				err_multiwindow_not_enabled, rejected)
			WindowOperationCapability{}
		}
		callback := fn [mut callback_called] (mut lease NativeWindowLease) ! {
			callback_called = true
			_ = lease
		}
		app.with_native_window(window, callback) or {
			rejected = record_disabled_public_service('with_native_window', err,
				err_multiwindow_not_enabled, rejected)
		}
		app.show_window(window) or {
			rejected = record_disabled_public_service('show_window', err,
				err_multiwindow_not_enabled, rejected)
		}
		app.hide_window(window) or {
			rejected = record_disabled_public_service('hide_window', err,
				err_multiwindow_not_enabled, rejected)
		}
		app.request_window_focus(window) or {
			rejected = record_disabled_public_service('request_window_focus', err,
				err_multiwindow_not_enabled, rejected)
		}
		app.raise_window(window) or {
			rejected = record_disabled_public_service('raise_window', err,
				err_multiwindow_not_enabled, rejected)
		}
		app.set_window_position(window, 1, 2) or {
			rejected = record_disabled_public_service('set_window_position', err,
				err_multiwindow_not_enabled, rejected)
		}
		app.minimize_window(window) or {
			rejected = record_disabled_public_service('minimize_window', err,
				err_multiwindow_not_enabled, rejected)
		}
		app.maximize_window(window) or {
			rejected = record_disabled_public_service('maximize_window', err,
				err_multiwindow_not_enabled, rejected)
		}
		app.restore_window(window) or {
			rejected = record_disabled_public_service('restore_window', err,
				err_multiwindow_not_enabled, rejected)
		}
		app.set_window_fullscreen(window, true) or {
			rejected = record_disabled_public_service('set_window_fullscreen', err,
				err_multiwindow_not_enabled, rejected)
		}
		_ = app.request_clipboard_text(window) or {
			rejected = record_disabled_public_service('request_clipboard_text', err,
				err_multiwindow_not_enabled, rejected)
			ClipboardRequestId{}
		}
		_ = app.set_clipboard_text(window, 'disabled') or {
			rejected = record_disabled_public_service('set_clipboard_text', err,
				err_multiwindow_not_enabled, rejected)
			ClipboardRequestId{}
		}

		assert rejected == 14
		assert !callback_called
	} $else $if windows {
		win32_public_run_no_opt_clipboard_child()!
	}
}

fn test_win32_public_hwnd_borrow_route_red() {
	$if windows {
		$if gg_multiwindow ? {
			mut app := new_app(backend: .win32)!
			defer {
				app.stop() or {}
			}
			window := app.create_window(title: 'Win32 public HWND borrow RED')!
			mut probe := &Win32PublicBorrowRouteProbe{}
			capability := app.window_operation_capability(window, .native_borrow)!
			assert capability.support == .available
			assert !capability.asynchronous
			assert !capability.requires_user_action
			assert !capability.state_observable
			first_callback := fn [mut probe] (mut lease NativeWindowLease) ! {
				probe.callback_count++
				probe.first_epoch = lease.lease_epoch
				probe.first_lease = lease
				lease.with_win32(fn [mut probe] (hwnd voidptr) ! {
					probe.accessor_count++
					probe.first_hwnd = hwnd
				})!
			}

			app.with_native_window(window, first_callback)!
			assert probe.callback_count == 1
			assert probe.accessor_count == 1
			assert probe.first_hwnd != unsafe { nil }
			assert probe.first_epoch != 0

			mut stale_rejected := false
			probe.first_lease.with_win32(fn (_ voidptr) ! {}) or { stale_rejected = true }
			assert stale_rejected, 'copied gg.NativeWindowLease remained valid after callback'

			second_callback := fn [mut probe] (mut lease NativeWindowLease) ! {
				probe.callback_count++
				probe.second_epoch = lease.lease_epoch
				lease.with_win32(fn [mut probe] (hwnd voidptr) ! {
					probe.accessor_count++
					probe.second_hwnd = hwnd
				})!
			}
			app.with_native_window(window, second_callback)!
			assert probe.callback_count == 2
			assert probe.accessor_count == 2
			assert probe.second_hwnd == probe.first_hwnd
			assert probe.second_epoch != 0
			assert probe.second_epoch != probe.first_epoch

			stale_window := app.create_window(title: 'Win32 stale public WindowId RED')!
			app.destroy_window(stale_window)!
			_ = app.create_window(title: 'Win32 replacement public WindowId RED')!
			mut stale_id_callback_called := false
			stale_id_callback := fn [mut stale_id_callback_called] (mut lease NativeWindowLease) ! {
				stale_id_callback_called = true
				lease.with_win32(fn (_ voidptr) ! {})!
			}
			mut stale_id_rejected := false
			app.with_native_window(stale_window, stale_id_callback) or { stale_id_rejected = true }
			assert stale_id_rejected, 'stale gg.WindowId unexpectedly retained native borrow authority'
			assert !stale_id_callback_called
		}
	}
}

fn test_win32_public_conditional_focus_refusal_is_not_a_capability_error_red() {
	$if windows && gg_multiwindow ? {
		mut app := new_app(backend: .win32)!
		defer {
			C.v_multiwindow_win32_service_test_set_focus_refused(0)
			app.stop() or {}
		}
		target := app.create_window(
			title:   'Win32 public refused focus target RED'
			visible: false
		)!
		peer := app.create_window(title: 'Win32 public refused focus peer RED')!
		app.show_window(target)!
		target_hwnd := win32_public_hwnd(mut app, target)!
		peer_hwnd := win32_public_hwnd(mut app, peer)!
		assert C.SetForegroundWindow(peer_hwnd) != 0
		_ = C.SetFocus(peer_hwnd)
		assert C.GetForegroundWindow() == peer_hwnd
		assert C.GetFocus() == peer_hwnd

		before := app.window_state(target)!
		assert before.active == .off
		assert before.focused == .off
		win32_public_request_refused_focus(mut app, target)!
		after := app.window_state(target)!
		assert after.active == .off
		assert after.focused == .off
		assert target_hwnd != peer_hwnd
	}
}

fn test_win32_public_partial_focus_is_never_reported_as_success_red() {
	$if windows && gg_multiwindow ? {
		mut app := new_app(backend: .win32)!
		defer {
			C.v_multiwindow_win32_service_test_set_focus_refused(0)
			app.stop() or {}
		}
		target := app.create_window(title: 'Win32 public partial focus target RED')!
		peer := app.create_window(title: 'Win32 public partial focus peer RED')!
		peer_hwnd := win32_public_hwnd(mut app, peer)!
		assert C.SetForegroundWindow(peer_hwnd) != 0
		_ = C.SetFocus(peer_hwnd)
		assert C.GetForegroundWindow() == peer_hwnd
		assert C.GetFocus() == peer_hwnd

		win32_public_request_refused_focus(mut app, target)!
		assert C.GetForegroundWindow() == peer_hwnd
		assert C.GetFocus() == peer_hwnd
		state := app.window_state(target)!
		assert state.active == .off
		assert state.focused == .off
	}
}

fn test_win32_public_controls_publish_observed_state_red() {
	$if windows {
		$if gg_multiwindow ? {
			mut app := new_app(backend: .win32)!
			defer {
				app.stop() or {}
			}
			window := app.create_window(
				title:   'Win32 public controls and state RED'
				visible: false
			)!
			mut issues := []string{}
			for _ in 0 .. 4 {
				app.poll_events() or {
					issues << 'initial event polling failed: ${err.msg()}'
					break
				}
			}
			initial := app.window_state(window)!
			if !win32_public_state_is_observed(initial) {
				issues << 'initial Win32 state is not observable through gg.App'
			}
			_ = app.drain_window_queued_events()!

			for operation in [WindowOperation.show, .hide, .raise, .position, .minimize, .maximize,
				.restore, .fullscreen] {
				capability := app.window_operation_capability(window, operation)!
				if capability.support != .available || capability.asynchronous
					|| capability.requires_user_action || !capability.state_observable {
					issues << '${operation} is not available/synchronous/observable through gg.App'
				}
			}
			focus_capability := app.window_operation_capability(window, .focus)!
			if focus_capability.support != .conditional || focus_capability.asynchronous
				|| !focus_capability.requires_user_action || !focus_capability.state_observable {
				issues << 'focus is not conditional/user-action/observable through gg.App'
			}

			app.show_window(window) or { issues << 'show_window failed: ${err.msg()}' }
			for _ in 0 .. 2 {
				app.poll_events() or { issues << 'show event polling failed: ${err.msg()}' }
			}
			shown := app.window_state(window)!
			if shown.mapping != .mapped || shown.visibility != .visible {
				issues << 'show_window did not publish mapped/visible state'
			}

			app.raise_window(window) or { issues << 'raise_window failed: ${err.msg()}' }
			app.set_window_position(window, 48, 64) or {
				issues << 'set_window_position failed: ${err.msg()}'
			}
			for _ in 0 .. 2 {
				app.poll_events() or { issues << 'position event polling failed: ${err.msg()}' }
			}
			positioned := app.window_state(window)!
			if !positioned.position.known || positioned.position.x != 48
				|| positioned.position.y != 64 {
				issues << 'set_window_position did not publish the requested position'
			}

			app.minimize_window(window) or { issues << 'minimize_window failed: ${err.msg()}' }
			for _ in 0 .. 4 {
				app.poll_events() or { issues << 'minimize event polling failed: ${err.msg()}' }
			}
			minimized := app.window_state(window)!
			if minimized.minimized != .on {
				issues << 'minimize_window did not publish minimized state'
			}

			app.restore_window(window) or {
				issues << 'restore_window after minimize failed: ${err.msg()}'
			}
			for _ in 0 .. 4 {
				app.poll_events() or { issues << 'restore event polling failed: ${err.msg()}' }
			}
			restored_from_minimize := app.window_state(window)!
			if restored_from_minimize.minimized != .off {
				issues << 'restore_window did not clear minimized state'
			}

			app.maximize_window(window) or { issues << 'maximize_window failed: ${err.msg()}' }
			for _ in 0 .. 4 {
				app.poll_events() or { issues << 'maximize event polling failed: ${err.msg()}' }
			}
			maximized := app.window_state(window)!
			if maximized.maximized != .on {
				issues << 'maximize_window did not publish maximized state'
			}

			app.restore_window(window) or {
				issues << 'restore_window after maximize failed: ${err.msg()}'
			}
			for _ in 0 .. 4 {
				app.poll_events() or { issues << 'restore event polling failed: ${err.msg()}' }
			}
			restored_from_maximize := app.window_state(window)!
			if restored_from_maximize.maximized != .off {
				issues << 'restore_window did not clear maximized state'
			}

			app.set_window_fullscreen(window, true) or {
				issues << 'set_window_fullscreen(true) failed: ${err.msg()}'
			}
			for _ in 0 .. 4 {
				app.poll_events() or { issues << 'fullscreen event polling failed: ${err.msg()}' }
			}
			fullscreen := app.window_state(window)!
			if fullscreen.fullscreen != .on {
				issues << 'set_window_fullscreen(true) did not publish fullscreen state'
			}

			app.set_window_fullscreen(window, false) or {
				issues << 'set_window_fullscreen(false) failed: ${err.msg()}'
			}
			for _ in 0 .. 4 {
				app.poll_events() or { issues << 'fullscreen event polling failed: ${err.msg()}' }
			}
			windowed := app.window_state(window)!
			if windowed.fullscreen != .off {
				issues << 'set_window_fullscreen(false) did not clear fullscreen state'
			}

			hide_baseline_sequence := windowed.sequence
			_ = app.drain_window_queued_events()!
			app.hide_window(window) or { issues << 'hide_window failed: ${err.msg()}' }
			for _ in 0 .. 2 {
				app.poll_events() or { issues << 'hide event polling failed: ${err.msg()}' }
			}
			final_state := app.window_state(window)!
			if final_state.mapping != .unmapped || final_state.visibility != .hidden {
				issues << 'hide_window did not publish unmapped/hidden state'
			}
			if !win32_public_state_is_observed(final_state) {
				issues << 'post-control Win32 state is not observable through gg.App'
			}
			if final_state.sequence <= initial.sequence {
				issues << 'public Win32 state sequence did not advance after controls'
			}
			events := app.drain_window_queued_events()!
			hide_state_events := events.filter(it.kind == .service && it.service.kind == .state
				&& it.service.window == window)
			if hide_state_events.len != 1 {
				issues << 'hide_window did not publish exactly one fresh canonical gg.WindowServiceEvent state'
			} else {
				hide_event := hide_state_events[0].service
				if hide_event.operation != .hide || hide_event.sequence <= hide_baseline_sequence
					|| hide_event.state.mapping != .unmapped
					|| hide_event.state.visibility != .hidden {
					issues << 'hide_window canonical state event did not belong to the hide action'
				}
			}
			assert issues.len == 0, 'Win32 gg.App controls/state RED:\n${issues.join('\n')}'
		}
	}
}

fn test_win32_public_monitor_projection_and_event_order_red() {
	$if windows {
		$if gg_multiwindow ? {
			mut app := new_app(backend: .win32)!
			defer {
				app.stop() or {}
			}
			window := app.create_window(
				title:    'Win32 public monitor projection RED'
				width:    320
				height:   200
				high_dpi: true
			)!
			for _ in 0 .. 4 {
				app.poll_events()!
			}
			_ = app.drain_window_queued_events()!

			before_ids := app.monitor_ids()!
			assert before_ids.len > 0
			mut primary_count := 0
			for id in before_ids {
				info := app.monitor_info(id)!
				assert info.id == id
				assert info.name != ''
				assert info.available
				assert info.geometry.known
				assert info.geometry.value.width > 0
				assert info.geometry.value.height > 0
				assert info.work_area.known
				assert info.work_area.value.width > 0
				assert info.work_area.value.height > 0
				assert info.scale.known
				assert info.scale.value > 0
				assert info.primary != .unknown
				if info.primary == .on {
					primary_count++
				}
			}
			assert primary_count == 1
			before_state := app.window_state(window)!
			assert before_state.monitor_ids.len > 0
			for id in before_state.monitor_ids {
				assert id in before_ids
			}

			hwnd := win32_public_hwnd(mut app, window)!
			assert C.v_multiwindow_test_win32_emit_display_changes(hwnd, 3) == 1
			for _ in 0 .. 4 {
				app.poll_events()!
			}
			after_ids := app.monitor_ids()!
			assert after_ids == before_ids
			after_state := app.window_state(window)!
			assert after_state.monitor_ids == before_state.monitor_ids

			queued := app.drain_window_queued_events()!
			monitor_events := queued.filter(it.kind == .service && it.service.kind == .monitor)
			metrics_events := queued.filter(it.kind == .service && it.service.kind == .metrics
				&& it.service.window == window)
			assert monitor_events.len == 1
			assert metrics_events.len == 1
			monitor_envelope := monitor_events[0]
			metrics_envelope := metrics_events[0]
			assert monitor_envelope.sequence == monitor_envelope.service.sequence
			assert metrics_envelope.sequence == metrics_envelope.service.sequence
			assert metrics_envelope.service.sequence == metrics_envelope.service.metrics.metrics_sequence
			assert metrics_envelope.service.state.sequence == metrics_envelope.sequence
			assert monitor_envelope.sequence < metrics_envelope.sequence
			assert metrics_envelope.service.state.monitor_ids == after_state.monitor_ids

			projected := monitor_envelope.service.monitors
			assert projected.len == after_ids.len
			singular_matches := projected.filter(it.id == monitor_envelope.service.monitor.id)
			assert singular_matches.len == 1
			assert monitor_envelope.service.monitor == singular_matches[0]
			for id in after_ids {
				info := app.monitor_info(id)!
				id_matches := projected.filter(it.id == id)
				assert id_matches.len == 1
				event_info := id_matches[0]
				assert event_info.id == id
				assert event_info.name == info.name
				assert event_info.geometry == info.geometry
				assert event_info.work_area == info.work_area
				assert event_info.scale == info.scale
				assert event_info.primary == info.primary
				assert event_info.available == info.available
				assert event_info.sequence == monitor_envelope.sequence
				assert info.sequence == monitor_envelope.sequence
			}

			stale_info := app.monitor_info(after_ids[0])!
			assert C.v_multiwindow_test_win32_monitor_enumeration_capture() == after_ids.len
			defer {
				C.v_multiwindow_test_win32_monitor_enumeration_reset()
			}
			C.v_multiwindow_test_win32_monitor_enumeration_use_empty()
			_ = app.drain_window_queued_events()!
			assert C.v_multiwindow_test_win32_emit_display_change(hwnd) == 1
			for _ in 0 .. 4 {
				app.poll_events()!
			}
			assert C.v_multiwindow_test_win32_monitor_enumeration_empty_calls() > 0
			assert app.monitor_ids()!.len == 0
			unavailable := app.monitor_info(stale_info.id)!
			assert unavailable.id == stale_info.id
			assert !unavailable.available
			unplug_state := app.window_state(window)!
			assert unplug_state.monitor_ids.len == 0
			unplug_events := app.drain_window_queued_events()!
			unplug_monitors := unplug_events.filter(it.kind == .service
				&& it.service.kind == .monitor)
			unplug_metrics := unplug_events.filter(it.kind == .service
				&& it.service.kind == .metrics && it.service.window == window)
			assert unplug_monitors.len == 1
			assert unplug_monitors[0].service.monitors.len == 0
			assert unplug_metrics.len == 1
			assert unplug_metrics[0].service.state.monitor_ids.len == 0
			assert unplug_monitors[0].sequence < unplug_metrics[0].sequence

			assert C.v_multiwindow_test_win32_monitor_enumeration_use_replay() == 1
			_ = app.drain_window_queued_events()!
			assert C.v_multiwindow_test_win32_emit_display_change(hwnd) == 1
			for _ in 0 .. 4 {
				app.poll_events()!
			}
			assert C.v_multiwindow_test_win32_monitor_enumeration_replay_calls() > 0
			replug_ids := app.monitor_ids()!
			assert replug_ids.len == after_ids.len
			assert stale_info.id !in replug_ids
			mut replacement := WindowMonitorInfo{}
			mut replacement_found := false
			for id in replug_ids {
				info := app.monitor_info(id)!
				if info.name == stale_info.name {
					replacement = info
					replacement_found = true
					break
				}
			}
			assert replacement_found
			assert replacement.id != stale_info.id
			replug_state := app.window_state(window)!
			assert replacement.id in replug_state.monitor_ids
			assert stale_info.id !in replug_state.monitor_ids
			replug_events := app.drain_window_queued_events()!
			replug_monitors := replug_events.filter(it.kind == .service
				&& it.service.kind == .monitor)
			replug_metrics := replug_events.filter(it.kind == .service
				&& it.service.kind == .metrics && it.service.window == window)
			assert replug_monitors.len == 1
			assert replug_metrics.len == 1
			assert replacement.id in replug_metrics[0].service.state.monitor_ids
			assert stale_info.id !in replug_metrics[0].service.state.monitor_ids
			assert replug_monitors[0].sequence < replug_metrics[0].sequence
			replacement_events :=
				replug_monitors[0].service.monitors.filter(it.id == replacement.id)
			assert replacement_events.len == 1
			assert replug_monitors[0].service.monitors.all(it.id != stale_info.id)
			mut stale_id_rejected := false
			_ = app.monitor_info(stale_info.id) or {
				stale_id_rejected = true
				WindowMonitorInfo{}
			}
			assert stale_id_rejected
		}
	}
}

fn test_win32_public_clipboard_cf_unicodetext_bmp_astral_roundtrip_red() {
	$if windows && gg_multiwindow ? {
		eprintln('PACKAGE2_RED_TEST=test_win32_public_clipboard_cf_unicodetext_bmp_astral_roundtrip_red')
		eprintln('PACKAGE2_RED_FAMILY=public_clipboard_unicode')
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(title: 'Win32 public clipboard Unicode RED')!
		_ = app.drain_window_queued_events()!
		hwnd := win32_public_hwnd(mut app, window)!
		mut issues := []string{}
		for operation in [WindowOperation.clipboard_read, .clipboard_write] {
			capability := app.window_operation_capability(window, operation) or {
				issues << '${operation} capability query failed: ${err.msg()}'
				WindowOperationCapability{}
			}
			if capability.support != .available || !capability.asynchronous
				|| capability.requires_user_action || capability.state_observable {
				issues << '${operation} capability is not available asynchronous'
			}
		}
		mut last_sequence := u64(0)

		external := 'external BMP € 漢字 astral 🙂 𝄞'
		external_fixture_ready := C.v_multiwindow_test_win32_set_clipboard(hwnd,
			external.to_wide(), win32_public_utf16_units(external)) == 1
		if !external_fixture_ready {
			win32_public_w4_add_infra(mut issues,
				'public clipboard oracle could not install Unicode fixture')
		}
		if external_fixture_ready {
			read_request := app.request_clipboard_text(window) or {
				issues << 'gg clipboard read was not admitted: ${err.msg()}'
				ClipboardRequestId{}
			}
			if read_request != ClipboardRequestId{} {
				delivered := win32_public_poll_until_clipboard(mut app, read_request, 8,
					'public Unicode read', mut issues)
				terminals := win32_public_clipboard_events(delivered, read_request)
				if !win32_public_events_are_globally_ordered(delivered) {
					issues << 'gg clipboard read lost adjacent global event ordering'
				}
				if terminals.len != 1
					|| !win32_public_clipboard_envelope_matches(terminals[0], read_request, window, .clipboard_read, .ready)
					|| terminals[0].service.clipboard.text != external {
					issues << 'gg clipboard read did not publish one exact ready envelope'
				} else {
					last_sequence = terminals[0].sequence
				}
				late_delivery := win32_public_poll_collect(mut app, 4, 'public Unicode read late', mut
					issues)
				late := win32_public_clipboard_events(late_delivery, read_request)
				if late.len != 0 {
					issues << 'gg clipboard read produced a duplicate late terminal'
				}
				if !win32_public_events_are_globally_ordered(late_delivery) {
					issues << 'gg clipboard read late events lost global ordering'
				}
			}
		}

		written := 'public BMP Ω Ж astral 🙂 𝄞'
		written_units := win32_public_utf16_units(written)
		write_request := app.set_clipboard_text(window, written) or {
			issues << 'gg clipboard write was not admitted: ${err.msg()}'
			ClipboardRequestId{}
		}
		if write_request != ClipboardRequestId{} {
			delivered := win32_public_poll_until_clipboard(mut app, write_request, 8,
				'public Unicode write', mut issues)
			terminals := win32_public_clipboard_events(delivered, write_request)
			if !win32_public_events_are_globally_ordered(delivered) {
				issues << 'gg clipboard write lost adjacent global event ordering'
			}
			if terminals.len != 1
				|| !win32_public_clipboard_envelope_matches(terminals[0], write_request, window, .clipboard_write, .ready)
				|| terminals[0].sequence <= last_sequence {
				issues << 'gg clipboard write did not publish one exact ordered ready envelope'
			}
			if C.v_multiwindow_test_win32_clipboard_equals(written.to_wide(), written_units) != 1 {
				issues << 'gg clipboard write did not preserve BMP and astral UTF-16 text'
			}
			late_delivery := win32_public_poll_collect(mut app, 4, 'public Unicode write late', mut
				issues)
			late := win32_public_clipboard_events(late_delivery, write_request)
			if late.len != 0 {
				issues << 'gg clipboard write produced a duplicate late terminal'
			}
			if !win32_public_events_are_globally_ordered(late_delivery) {
				issues << 'gg clipboard write late events lost global ordering'
			}
		}
		eprintln('PACKAGE2_W4_REACHED=public_clipboard_unicode')
		infra_issues := issues.filter(it.starts_with('PACKAGE2_W4_INFRA='))
		contract_issues := issues.filter(!it.starts_with('PACKAGE2_W4_INFRA='))
		if infra_issues.len == 0 && contract_issues.len > 0 {
			eprintln('PACKAGE2_RED_TERMINAL=behavioral_red:public_clipboard_unicode')
		}
		assert issues.len == 0, 'Win32 public CF_UNICODETEXT RED:\n${issues.join('\n')}'
	}
}
