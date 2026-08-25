module main

$if windows && gg_multiwindow ? {
	import gg
	import time
}

const w5_a1_identity = 'win32_public_mouse_lock_raw_delta'
const w5_a1_test = 'test_win32_public_mouse_lock_real_raw_delta_red'
const w5_a1_family = 'mouse_lock_raw_delta_public'
const w5_a1_tag = u32(0x57354131)
const w5_a1_baseline_mask_all = u32(0x1f)
const w5_a1_locked_mask_all = u32(0x07)

enum W5A1ResultKind {
	behavioral_green
	behavioral_red
	infra
}

struct W5A1Result {
	kind    W5A1ResultKind
	reached string
	reason  string
	cleanup string
}

fn w5_a1_green() W5A1Result {
	return W5A1Result{
		kind:    .behavioral_green
		reached: 'live_public_mouse_lock_raw_delta'
		reason:  'mouse_lock_acquire_clipped_delta_explicit_unlock'
		cleanup: 'baseline_restored_without_rescue'
	}
}

fn w5_a1_red(reached string, reason string, cleanup string) W5A1Result {
	return W5A1Result{
		kind:    .behavioral_red
		reached: reached
		reason:  reason
		cleanup: cleanup
	}
}

fn w5_a1_infra(reached string, reason string, cleanup string) W5A1Result {
	return W5A1Result{
		kind:    .infra
		reached: reached
		reason:  reason
		cleanup: cleanup
	}
}

fn w5_a1_emit(result W5A1Result) int {
	eprintln('PACKAGE2_W5_A1_IDENTITY=${w5_a1_identity}')
	eprintln('PACKAGE2_RED_TEST=${w5_a1_test}')
	eprintln('PACKAGE2_RED_FAMILY=${w5_a1_family}')
	eprintln('PACKAGE2_W5_A1_REACHED=${result.reached}')
	match result.kind {
		.behavioral_green {
			eprintln('PACKAGE2_W5_A1_PRODUCT_OK=${result.reason}')
			eprintln('PACKAGE2_W5_A1_CLEANUP_OK=${result.cleanup}')
			eprintln('PACKAGE2_W5_A1_SUMMARY=accepted:1 rejected:0 total:1')
			eprintln('PACKAGE2_W5_A1_TERMINAL=native_pass:${w5_a1_family}')
			return 0
		}
		.behavioral_red {
			eprintln('PACKAGE2_W5_A1_PRODUCT_GAP=${result.reason}')
			eprintln('PACKAGE2_W5_A1_CLEANUP_OK=${result.cleanup}')
			eprintln('PACKAGE2_W5_A1_SUMMARY=accepted:0 rejected:1 total:1')
			eprintln('PACKAGE2_RED_TERMINAL=behavioral_red:${w5_a1_family}')
			return 1
		}
		.infra {
			eprintln('PACKAGE2_W5_A1_INFRA=${result.reason}')
			eprintln('PACKAGE2_W5_A1_CLEANUP=${result.cleanup}')
			eprintln('PACKAGE2_W5_A1_SUMMARY=accepted:0 rejected:1 total:1')
			eprintln('PACKAGE2_W5_A1_TERMINAL=infra:${w5_a1_family}')
			return 2
		}
	}
}

$if windows && gg_multiwindow ? {
	#flag windows -DUNICODE
	#flag windows -D_UNICODE
	#flag windows -luser32
	#include "@VMODROOT/vlib/x/multiwindow/testdata/win32_raw_input_w5_a1_oracle.h"

	fn C.v_multiwindow_test_win32_raw_input_w5_a1_new(out_error &u32) voidptr
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_last_error(oracle voidptr) u32
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_baseline(oracle voidptr, target voidptr, out_mask &u32) int
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_target_ready(oracle voidptr, target voidptr) int
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_mark_product_attempted(oracle voidptr) int
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_locked(oracle voidptr, target voidptr, out_mask &u32) int
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_position_edge(oracle voidptr, target voidptr, out_client_x &int, out_client_y &int) int
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_arm_send(oracle voidptr, tag u32, dx int, dy int) int
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_hook_result(oracle voidptr, out_exact &int, out_unexpected &int) int
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_cursor_at_edge(oracle voidptr) int
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_unhook(oracle voidptr) int
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_restore_cursor(oracle voidptr) int
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_unlocked(oracle voidptr, target voidptr, out_mask &u32) int
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_rescue_after_failure(oracle voidptr) int
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_rescue_used(oracle voidptr) int
	fn C.v_multiwindow_test_win32_raw_input_w5_a1_dispose(oracle voidptr) int

	struct W5A1NativeProbe {
	mut:
		primary   int
		secondary int
		mask      u32
		client_x  int
		client_y  int
	}

	struct W5A1PollResult {
		ok          bool
		quiet       bool
		hook_exact  bool
		hook_failed bool
		events      []gg.WindowQueuedEvent
	}

	struct W5A1Runtime {
	mut:
		oracle         voidptr
		lock_attempted bool
		rescue_armed   bool
	}

	fn w5_a1_events_ordered(events []gg.WindowQueuedEvent) bool {
		mut previous := u64(0)
		for event in events {
			if event.sequence == 0 || event.sequence <= previous {
				return false
			}
			if event.kind == .service {
				if event.service.sequence != event.sequence {
					return false
				}
				if event.service.kind == .state && event.service.state.sequence != event.sequence {
					return false
				}
			}
			previous = event.sequence
		}
		return true
	}

	fn w5_a1_collect_until_quiet(mut app gg.App, attempts int) W5A1PollResult {
		mut events := []gg.WindowQueuedEvent{}
		mut quiet_cycles := 0
		for _ in 0 .. attempts {
			accepted := app.poll_events() or { return W5A1PollResult{} }
			batch := app.drain_window_queued_events() or { return W5A1PollResult{} }
			events << batch
			if accepted == 0 && batch.len == 0 {
				quiet_cycles++
			} else {
				quiet_cycles = 0
			}
			if quiet_cycles >= 3 {
				return W5A1PollResult{
					ok:     true
					quiet:  true
					events: events
				}
			}
			time.sleep(5 * time.millisecond)
		}
		return W5A1PollResult{
			ok:     true
			quiet:  false
			events: events
		}
	}

	fn w5_a1_collect_raw_until_quiet(mut app gg.App, oracle voidptr) W5A1PollResult {
		deadline := time.now().add(5 * time.second)
		mut events := []gg.WindowQueuedEvent{}
		mut quiet_cycles := 0
		mut hook_exact := false
		mut public_mouse_move_seen := false
		for time.now() < deadline {
			accepted := app.poll_events() or { return W5A1PollResult{} }
			batch := app.drain_window_queued_events() or { return W5A1PollResult{} }
			events << batch
			if batch.any(it.kind == .input && it.input.event.typ == .mouse_move) {
				public_mouse_move_seen = true
			}
			mut exact_hook := 0
			mut unexpected_hook := 0
			hook_result := C.v_multiwindow_test_win32_raw_input_w5_a1_hook_result(oracle,
				&exact_hook, &unexpected_hook)
			if hook_result < 0 || unexpected_hook != 0 || exact_hook > 1 {
				return W5A1PollResult{
					ok:          true
					hook_failed: true
					events:      events
				}
			}
			if hook_result == 1 && exact_hook == 1 {
				hook_exact = true
			}
			if hook_exact && accepted == 0 && batch.len == 0 {
				quiet_cycles++
			} else {
				quiet_cycles = 0
			}
			if hook_exact && public_mouse_move_seen && quiet_cycles >= 3 {
				return W5A1PollResult{
					ok:         true
					quiet:      true
					hook_exact: true
					events:     events
				}
			}
			time.sleep(5 * time.millisecond)
		}
		return W5A1PollResult{
			ok:         true
			quiet:      hook_exact && quiet_cycles >= 3
			hook_exact: hook_exact
			events:     events
		}
	}

	fn w5_a1_mouse_lock_events(events []gg.WindowQueuedEvent) []gg.WindowQueuedEvent {
		return events.filter(it.kind == .service && it.service.kind == .state
			&& it.service.operation == .mouse_lock && it.service.sequence == it.sequence)
	}

	fn w5_a1_mouse_moves(events []gg.WindowQueuedEvent) []gg.WindowQueuedEvent {
		return events.filter(it.kind == .input && it.input.event.typ == .mouse_move)
	}

	fn w5_a1_borrow_baseline(mut app gg.App, window gg.WindowId, oracle voidptr) !&W5A1NativeProbe {
		mut probe := &W5A1NativeProbe{}
		callback := fn [oracle, mut probe] (mut lease gg.NativeWindowLease) ! {
			lease.with_win32(fn [oracle, mut probe] (hwnd voidptr) ! {
				probe.primary = C.v_multiwindow_test_win32_raw_input_w5_a1_baseline(oracle, hwnd,
					&probe.mask)
			})!
		}
		app.with_native_window(window, callback)!
		return probe
	}

	fn w5_a1_borrow_target_ready(mut app gg.App, window gg.WindowId, oracle voidptr) !&W5A1NativeProbe {
		mut probe := &W5A1NativeProbe{}
		callback := fn [oracle, mut probe] (mut lease gg.NativeWindowLease) ! {
			lease.with_win32(fn [oracle, mut probe] (hwnd voidptr) ! {
				probe.primary =
					C.v_multiwindow_test_win32_raw_input_w5_a1_target_ready(oracle, hwnd)
			})!
		}
		app.with_native_window(window, callback)!
		return probe
	}

	fn w5_a1_borrow_locked_and_position(mut app gg.App, window gg.WindowId, oracle voidptr) !&W5A1NativeProbe {
		mut probe := &W5A1NativeProbe{}
		callback := fn [oracle, mut probe] (mut lease gg.NativeWindowLease) ! {
			lease.with_win32(fn [oracle, mut probe] (hwnd voidptr) ! {
				probe.primary = C.v_multiwindow_test_win32_raw_input_w5_a1_locked(oracle, hwnd,
					&probe.mask)
				if probe.primary == 1 {
					probe.secondary = C.v_multiwindow_test_win32_raw_input_w5_a1_position_edge(oracle,
						hwnd, &probe.client_x, &probe.client_y)
				}
			})!
		}
		app.with_native_window(window, callback)!
		return probe
	}

	fn w5_a1_borrow_locked(mut app gg.App, window gg.WindowId, oracle voidptr) !&W5A1NativeProbe {
		mut probe := &W5A1NativeProbe{}
		callback := fn [oracle, mut probe] (mut lease gg.NativeWindowLease) ! {
			lease.with_win32(fn [oracle, mut probe] (hwnd voidptr) ! {
				probe.primary = C.v_multiwindow_test_win32_raw_input_w5_a1_locked(oracle, hwnd,
					&probe.mask)
			})!
		}
		app.with_native_window(window, callback)!
		return probe
	}

	fn w5_a1_borrow_recheck_and_send(mut app gg.App, window gg.WindowId, oracle voidptr) !&W5A1NativeProbe {
		mut probe := &W5A1NativeProbe{}
		callback := fn [oracle, mut probe] (mut lease gg.NativeWindowLease) ! {
			lease.with_win32(fn [oracle, mut probe] (hwnd voidptr) ! {
				probe.primary =
					C.v_multiwindow_test_win32_raw_input_w5_a1_target_ready(oracle, hwnd)
				if probe.primary == 1 {
					probe.secondary = C.v_multiwindow_test_win32_raw_input_w5_a1_locked(oracle,
						hwnd, &probe.mask)
				}
				if probe.primary == 1 && probe.secondary == 1 {
					probe.secondary = C.v_multiwindow_test_win32_raw_input_w5_a1_arm_send(oracle,
						w5_a1_tag, 7, 0)
				}
			})!
		}
		app.with_native_window(window, callback)!
		return probe
	}

	fn w5_a1_borrow_unlocked(mut app gg.App, window gg.WindowId, oracle voidptr) !&W5A1NativeProbe {
		mut probe := &W5A1NativeProbe{}
		callback := fn [oracle, mut probe] (mut lease gg.NativeWindowLease) ! {
			lease.with_win32(fn [oracle, mut probe] (hwnd voidptr) ! {
				probe.primary = C.v_multiwindow_test_win32_raw_input_w5_a1_unlocked(oracle, hwnd,
					&probe.mask)
			})!
		}
		app.with_native_window(window, callback)!
		return probe
	}

	fn w5_a1_execute(mut app gg.App, window gg.WindowId, peer gg.WindowId, mut runtime W5A1Runtime) W5A1Result {
		initial := w5_a1_collect_until_quiet(mut app, 20)
		if !initial.ok || !initial.quiet || !w5_a1_events_ordered(initial.events) {
			return w5_a1_infra('initial_settle', 'initial_settle_failed', 'pending_cleanup')
		}
		baseline := w5_a1_borrow_baseline(mut app, window, runtime.oracle) or {
			return w5_a1_infra('baseline', 'native_borrow_failed', 'pending_cleanup')
		}
		if baseline.primary != 1 {
			return w5_a1_infra('baseline', if baseline.primary < 0 {
				'baseline_query_failed'
			} else {
				'baseline_not_globally_clean'
			}, 'pending_cleanup')
		}
		app.request_window_focus(window) or {
			return w5_a1_infra('focus', 'focus_request_failed', 'pending_cleanup')
		}
		focused_settle := w5_a1_collect_until_quiet(mut app, 20)
		if !focused_settle.ok || !focused_settle.quiet
			|| !w5_a1_events_ordered(focused_settle.events) {
			return w5_a1_infra('focus', 'focus_settle_failed', 'pending_cleanup')
		}
		focus := w5_a1_borrow_target_ready(mut app, window, runtime.oracle) or {
			return w5_a1_infra('focus', 'focus_borrow_failed', 'pending_cleanup')
		}
		if focus.primary != 1 {
			return w5_a1_infra('focus', 'focus_authority_unavailable', 'pending_cleanup')
		}
		if C.v_multiwindow_test_win32_raw_input_w5_a1_mark_product_attempted(runtime.oracle) != 1 {
			return w5_a1_infra('lock_acquire', 'product_attempt_marker_failed', 'pending_cleanup')
		}
		runtime.rescue_armed = true
		runtime.lock_attempted = true
		app.set_window_mouse_lock(window, true) or {
			return w5_a1_red('lock_acquire', 'mouse_lock_acquire_failed', 'pending_cleanup')
		}
		locked_events := w5_a1_collect_until_quiet(mut app, 20)
		if !locked_events.ok || !locked_events.quiet || !w5_a1_events_ordered(locked_events.events) {
			return w5_a1_infra('lock_acquire', 'lock_event_settle_failed', 'pending_cleanup')
		}
		locked_state := app.window_state(window) or {
			return w5_a1_infra('lock_acquire', 'locked_state_query_failed', 'pending_cleanup')
		}
		peer_locked_state := app.window_state(peer) or {
			return w5_a1_infra('lock_acquire', 'peer_state_query_failed', 'pending_cleanup')
		}
		on_events := w5_a1_mouse_lock_events(locked_events.events)
		if locked_state.mouse_locked != .on || peer_locked_state.mouse_locked != .off
			|| on_events.len != 1 || on_events[0].service.window != window
			|| on_events[0].service.state.mouse_locked != .on {
			return w5_a1_red('lock_acquire', 'locked_state_event_mismatch', 'pending_cleanup')
		}
		if w5_a1_mouse_moves(locked_events.events).len != 0 {
			return w5_a1_red('lock_acquire', 'lock_acquire_mouse_move', 'pending_cleanup')
		}
		positioned := w5_a1_borrow_locked_and_position(mut app, window, runtime.oracle) or {
			return w5_a1_infra('lock_acquire', 'locked_borrow_failed', 'pending_cleanup')
		}
		if positioned.primary < 0 || positioned.secondary < 0 {
			return w5_a1_infra('edge_setup', 'edge_setup_query_failed', 'pending_cleanup')
		}
		if positioned.primary != 1 {
			return w5_a1_red('lock_acquire', 'locked_native_resources_mismatch', 'pending_cleanup')
		}
		if positioned.secondary != 1 {
			return w5_a1_infra('edge_setup', 'clipped_edge_unavailable', 'pending_cleanup')
		}
		setup_events := w5_a1_collect_until_quiet(mut app, 20)
		if !setup_events.ok || !setup_events.quiet || !w5_a1_events_ordered(setup_events.events) {
			return w5_a1_infra('edge_setup', 'edge_settle_failed', 'pending_cleanup')
		}
		if w5_a1_mouse_moves(setup_events.events).len != 0 {
			return w5_a1_red('edge_setup', 'legacy_setup_move_published', 'pending_cleanup')
		}
		if w5_a1_mouse_lock_events(setup_events.events).len != 0 {
			return w5_a1_red('edge_setup', 'unexpected_mouse_lock_event', 'pending_cleanup')
		}
		sent := w5_a1_borrow_recheck_and_send(mut app, window, runtime.oracle) or {
			return w5_a1_infra('raw_send', 'send_borrow_failed', 'pending_cleanup')
		}
		if sent.primary != 1 {
			return w5_a1_infra('raw_send', 'focus_changed_before_send', 'pending_cleanup')
		}
		if sent.secondary < 0 {
			return w5_a1_infra('raw_send', 'tagged_send_failed', 'pending_cleanup')
		}
		if sent.secondary != 1 {
			return w5_a1_red('raw_send', 'locked_resources_changed_before_send', 'pending_cleanup')
		}
		raw_events := w5_a1_collect_raw_until_quiet(mut app, runtime.oracle)
		if !raw_events.ok {
			return w5_a1_infra('raw_delivery', 'raw_delivery_poll_failed', 'pending_cleanup')
		}
		if raw_events.hook_failed {
			return w5_a1_infra('raw_delivery', 'hook_noise_or_duplicate', 'pending_cleanup')
		}
		if !raw_events.hook_exact {
			return w5_a1_infra('raw_delivery', 'tagged_hook_not_observed', 'pending_cleanup')
		}
		if !raw_events.quiet || !w5_a1_events_ordered(raw_events.events) {
			return w5_a1_infra('raw_delivery', 'raw_delivery_settle_failed', 'pending_cleanup')
		}
		if w5_a1_mouse_lock_events(raw_events.events).len != 0 {
			return w5_a1_red('raw_delivery', 'unexpected_mouse_lock_event', 'pending_cleanup')
		}
		moves := w5_a1_mouse_moves(raw_events.events)
		if moves.len != 1 || moves[0].input.window != window {
			return w5_a1_red('raw_delivery', 'public_relative_move_cardinality', 'pending_cleanup')
		}
		move := moves[0]
		if move.input.event.mouse_x != f32(positioned.client_x)
			|| move.input.event.mouse_y != f32(positioned.client_y)
			|| move.input.event.mouse_dx <= 0 || move.input.event.mouse_dy != 0 {
			return w5_a1_red('raw_delivery', 'public_relative_delta_mismatch', 'pending_cleanup')
		}
		cursor_at_edge := C.v_multiwindow_test_win32_raw_input_w5_a1_cursor_at_edge(runtime.oracle)
		if cursor_at_edge < 0 {
			return w5_a1_infra('raw_delivery', 'cursor_edge_query_failed', 'pending_cleanup')
		}
		if cursor_at_edge != 1 {
			return w5_a1_red('raw_delivery', 'cursor_left_clipped_edge', 'pending_cleanup')
		}
		if on_events[0].sequence >= move.sequence {
			return w5_a1_red('raw_delivery', 'lock_and_input_order_mismatch', 'pending_cleanup')
		}
		target_raw_state := app.window_state(window) or {
			return w5_a1_infra('raw_delivery', 'locked_state_query_failed', 'pending_cleanup')
		}
		peer_raw_state := app.window_state(peer) or {
			return w5_a1_infra('raw_delivery', 'peer_state_query_failed', 'pending_cleanup')
		}
		if target_raw_state.mouse_locked != .on || peer_raw_state.mouse_locked != .off {
			return w5_a1_red('raw_delivery', 'mouse_lock_state_changed', 'pending_cleanup')
		}
		locked_after_raw := w5_a1_borrow_locked(mut app, window, runtime.oracle) or {
			return w5_a1_infra('raw_delivery', 'locked_borrow_failed', 'pending_cleanup')
		}
		if locked_after_raw.primary < 0 {
			return w5_a1_infra('raw_delivery', 'locked_query_failed', 'pending_cleanup')
		}
		if locked_after_raw.primary != 1 || locked_after_raw.mask != w5_a1_locked_mask_all {
			return w5_a1_red('raw_delivery', 'locked_resources_changed', 'pending_cleanup')
		}
		if C.v_multiwindow_test_win32_raw_input_w5_a1_unhook(runtime.oracle) != 1 {
			return w5_a1_infra('raw_delivery', 'hook_release_failed', 'pending_cleanup')
		}
		app.set_window_mouse_lock(window, false) or {
			return w5_a1_red('explicit_unlock', 'mouse_lock_unlock_failed', 'pending_cleanup')
		}
		runtime.lock_attempted = false
		unlocked_events := w5_a1_collect_until_quiet(mut app, 20)
		if !unlocked_events.ok || !unlocked_events.quiet
			|| !w5_a1_events_ordered(unlocked_events.events) {
			return w5_a1_infra('explicit_unlock', 'unlock_event_settle_failed', 'pending_cleanup')
		}
		off_events := w5_a1_mouse_lock_events(unlocked_events.events)
		if off_events.len != 1 || off_events[0].service.window != window
			|| off_events[0].service.state.mouse_locked != .off {
			return w5_a1_red('explicit_unlock', 'unlocked_state_event_mismatch', 'pending_cleanup')
		}
		if w5_a1_mouse_moves(unlocked_events.events).len != 0 {
			return w5_a1_red('explicit_unlock', 'late_public_mouse_move', 'pending_cleanup')
		}
		if move.sequence >= off_events[0].sequence {
			return w5_a1_red('explicit_unlock', 'input_and_unlock_order_mismatch',
				'pending_cleanup')
		}
		restored_cursor := C.v_multiwindow_test_win32_raw_input_w5_a1_restore_cursor(runtime.oracle)
		if restored_cursor != 1 {
			return w5_a1_infra('explicit_unlock', 'cursor_restore_failed', 'pending_cleanup')
		}
		final_events := w5_a1_collect_until_quiet(mut app, 20)
		if !final_events.ok || !final_events.quiet || !w5_a1_events_ordered(final_events.events) {
			return w5_a1_infra('explicit_unlock', 'final_settle_failed', 'pending_cleanup')
		}
		final_lock_events := w5_a1_mouse_lock_events(final_events.events)
		if final_lock_events.len != 0 {
			return w5_a1_red('explicit_unlock', 'late_mouse_lock_event', 'pending_cleanup')
		}
		unlocked_state := app.window_state(window) or {
			return w5_a1_infra('explicit_unlock', 'unlocked_state_query_failed', 'pending_cleanup')
		}
		peer_unlocked_state := app.window_state(peer) or {
			return w5_a1_infra('explicit_unlock', 'peer_state_query_failed', 'pending_cleanup')
		}
		if unlocked_state.mouse_locked != .off || peer_unlocked_state.mouse_locked != .off {
			return w5_a1_red('explicit_unlock', 'unlocked_state_mismatch', 'pending_cleanup')
		}
		restored := w5_a1_borrow_unlocked(mut app, window, runtime.oracle) or {
			return w5_a1_infra('explicit_unlock', 'unlock_borrow_failed', 'pending_cleanup')
		}
		if restored.primary < 0 {
			return w5_a1_infra('explicit_unlock', 'unlock_query_failed', 'pending_cleanup')
		}
		if restored.primary != 1 {
			return w5_a1_red('explicit_unlock', 'unlock_native_resources_mismatch',
				'pending_cleanup')
		}
		if C.v_multiwindow_test_win32_raw_input_w5_a1_rescue_used(runtime.oracle) != 0 {
			return w5_a1_infra('explicit_unlock', 'unexpected_rescue_use', 'pending_cleanup')
		}
		runtime.rescue_armed = false
		return w5_a1_green()
	}

	fn w5_a1_finalize(mut app gg.App, window gg.WindowId, peer gg.WindowId, mut runtime W5A1Runtime, result W5A1Result) W5A1Result {
		mut cleanup_failed := false
		rescue_was_armed := runtime.rescue_armed
		if result.kind != .behavioral_green {
			if rescue_was_armed {
				if runtime.oracle == unsafe { nil } {
					cleanup_failed = true
				} else {
					if C.v_multiwindow_test_win32_raw_input_w5_a1_unhook(runtime.oracle) != 1 {
						cleanup_failed = true
					}
				}
				if runtime.lock_attempted {
					if _ := app.set_window_mouse_lock(window, false) {
						runtime.lock_attempted = false
					} else {
						cleanup_failed = true
					}
				}
				if runtime.oracle == unsafe { nil } {
					cleanup_failed = true
				} else if C.v_multiwindow_test_win32_raw_input_w5_a1_rescue_after_failure(runtime.oracle) != 1 {
					cleanup_failed = true
				} else {
					runtime.rescue_armed = false
				}
				cleanup_events := w5_a1_collect_until_quiet(mut app, 20)
				if !cleanup_events.ok || !cleanup_events.quiet
					|| !w5_a1_events_ordered(cleanup_events.events) {
					cleanup_failed = true
				} else {
					cleanup_lock_events := w5_a1_mouse_lock_events(cleanup_events.events)
					if cleanup_lock_events.len > 0
						&& (cleanup_lock_events.last().service.window != window
						|| cleanup_lock_events.last().service.state.mouse_locked != .off) {
						cleanup_failed = true
					}
					if cleanup_lock_events.any(it.service.window != window
						&& it.service.state.mouse_locked == .on)
					{
						cleanup_failed = true
					}
				}
				if cleanup_state := app.window_state(window) {
					if cleanup_state.mouse_locked != .off {
						cleanup_failed = true
					}
				} else {
					cleanup_failed = true
				}
				if cleanup_peer_state := app.window_state(peer) {
					if cleanup_peer_state.mouse_locked != .off {
						cleanup_failed = true
					}
				} else {
					cleanup_failed = true
				}
				if runtime.oracle == unsafe { nil } {
					cleanup_failed = true
				} else if cleanup_native := w5_a1_borrow_unlocked(mut app, window, runtime.oracle) {
					if cleanup_native.primary != 1 || cleanup_native.mask != w5_a1_baseline_mask_all {
						cleanup_failed = true
					}
				} else {
					cleanup_failed = true
				}
			}
		} else {
			if runtime.rescue_armed {
				cleanup_failed = true
			}
			if runtime.oracle != unsafe { nil }
				&& C.v_multiwindow_test_win32_raw_input_w5_a1_rescue_used(runtime.oracle) != 0 {
				cleanup_failed = true
			}
		}
		if runtime.oracle != unsafe { nil } {
			if C.v_multiwindow_test_win32_raw_input_w5_a1_dispose(runtime.oracle) != 1 {
				cleanup_failed = true
			}
			runtime.oracle = unsafe { nil }
		}
		app.stop() or { cleanup_failed = true }
		if cleanup_failed {
			return w5_a1_infra('cleanup', 'cleanup_failed', 'failed')
		}
		if result.kind != .behavioral_green {
			return W5A1Result{
				...result
				cleanup: if rescue_was_armed {
					'failure_rescue_verified'
				} else {
					'no_product_oracle_mutation'
				}
			}
		}
		return result
	}

	fn w5_a1_finish_without_oracle(mut app gg.App, result W5A1Result) W5A1Result {
		app.stop() or { return w5_a1_infra('cleanup', 'app_stop_failed', 'failed') }
		return result
	}

	fn w5_a1_run_windows() W5A1Result {
		mut app := gg.new_app(backend: .win32, require_renderer: false) or {
			return w5_a1_infra('app_create', 'app_create_failed', 'no_oracle_mutation')
		}
		window := app.create_window(
			title:   'Win32 public Raw Input W5 A1'
			width:   320
			height:  180
			visible: true
		) or {
			return w5_a1_finish_without_oracle(mut app, w5_a1_infra('window_create',
				'window_create_failed', 'no_oracle_mutation'))
		}
		capability := app.window_operation_capability(window, .mouse_lock) or {
			return w5_a1_finish_without_oracle(mut app, w5_a1_infra('live_public_mouse_lock_capability',
				'capability_query_failed', 'no_oracle_mutation'))
		}
		if capability.support == .unsupported {
			return w5_a1_finish_without_oracle(mut app, w5_a1_red('live_public_mouse_lock_capability',
				'mouse_lock_capability_unsupported', 'no_oracle_mutation'))
		}
		if capability.support != .conditional || capability.asynchronous
			|| capability.requires_user_action || !capability.state_observable {
			return w5_a1_finish_without_oracle(mut app, w5_a1_red('live_public_mouse_lock_capability',
				'mouse_lock_capability_contract', 'no_oracle_mutation'))
		}
		peer := app.create_window(
			title:   'Win32 public Raw Input W5 A1 peer'
			width:   240
			height:  140
			visible: true
		) or {
			return w5_a1_finish_without_oracle(mut app, w5_a1_infra('peer_window_create',
				'peer_window_create_failed', 'no_oracle_mutation'))
		}
		mut oracle_error := u32(0)
		oracle := C.v_multiwindow_test_win32_raw_input_w5_a1_new(&oracle_error)
		if oracle == unsafe { nil } {
			return w5_a1_finish_without_oracle(mut app, w5_a1_infra('oracle_create',
				'oracle_create_failed', 'no_oracle_mutation'))
		}
		mut runtime := W5A1Runtime{
			oracle: oracle
		}
		result := w5_a1_execute(mut app, window, peer, mut runtime)
		return w5_a1_finalize(mut app, window, peer, mut runtime, result)
	}
}

fn test_win32_public_mouse_lock_real_raw_delta_red() {
	$if windows && gg_multiwindow ? {
		assert w5_a1_emit(w5_a1_run_windows()) == 0
	} $else {
		assert true
	}
}
