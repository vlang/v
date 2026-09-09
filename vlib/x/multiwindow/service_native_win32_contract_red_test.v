// vtest retry: 0
// vtest build: windows && gg_multiwindow?
module multiwindow

import time

const win32_red_clipboard_max_bytes = 16 * 1024 * 1024
const win32_red_wm_close = u32(0x0010)
const win32_red_ws_caption = u64(0x00c00000)
const win32_red_ws_child = u64(0x40000000)
const win32_red_dpi_context_unavailable = 1
const win32_red_dpi_context_rejected = 2

#flag windows -DV_MULTIWINDOW_WIN32_SERVICE_TEST

$if windows {
	#include "@VMODROOT/vlib/x/multiwindow/testdata/win32_monitor_enumeration_test_storage.h"
	#include "@VMODROOT/vlib/x/multiwindow/testdata/win32_nonreadback_test_oracle.h"

	fn C.v_multiwindow_win32_service_test_set_focus_refused(refused int)
	fn C.v_multiwindow_win32_service_test_set_show_failure(fail int)
	fn C.v_multiwindow_win32_service_test_set_fullscreen_exit_failure(failure int)
	fn C.v_multiwindow_win32_service_test_set_fullscreen_rollback_failure(failure_mask int)
	fn C.v_multiwindow_win32_service_test_fullscreen_rollback_attempts() int
	fn C.v_multiwindow_win32_test_modal_trace_reset(owner voidptr, window voidptr)
	fn C.v_multiwindow_win32_test_modal_set_enable_failure(fail int)
	fn C.v_multiwindow_win32_test_modal_set_enable_failures(count int)
	fn C.v_multiwindow_win32_test_modal_set_show_created_failures(count int)
	fn C.v_multiwindow_win32_test_modal_set_destroy_failures(count int)
	fn C.v_multiwindow_win32_test_modal_trace_window_value() voidptr
	fn C.v_multiwindow_win32_test_modal_owner_disable_count_value() int
	fn C.v_multiwindow_win32_test_modal_owner_enable_count_value() int
	fn C.v_multiwindow_win32_test_modal_show_count_value() int
	fn C.v_multiwindow_win32_test_modal_destroy_count_value() int
	fn C.v_multiwindow_win32_test_modal_owner_destroy_count_value() int
	fn C.v_multiwindow_win32_test_modal_destroy_attempt_count_value() int
	fn C.v_multiwindow_win32_test_modal_owner_destroy_attempt_count_value() int
	fn C.v_multiwindow_win32_test_modal_owner_disable_sequence_value() u64
	fn C.v_multiwindow_win32_test_modal_owner_enable_sequence_value() u64
	fn C.v_multiwindow_win32_test_modal_show_sequence_value() u64
	fn C.v_multiwindow_win32_test_modal_destroy_sequence_value() u64
	fn C.v_multiwindow_win32_test_modal_owner_destroy_sequence_value() u64
	fn C.v_multiwindow_win32_test_dpi_creation_configure(context_mode int, frame_bias_width int, frame_bias_height int)
	fn C.v_multiwindow_win32_test_dpi_creation_reset()
	fn C.v_multiwindow_win32_test_dpi_context_attempt_count() int
	fn C.v_multiwindow_win32_test_dpi_context_fallback_count() int
	fn C.v_multiwindow_win32_test_dpi_exact_resize_count() int
	fn C.v_multiwindow_win32_test_client_size_matches(hwnd voidptr, width int, height int) int
	fn C.v_multiwindow_test_win32_is_window(hwnd voidptr) int
	fn C.v_multiwindow_test_win32_is_visible(hwnd voidptr) int
	fn C.v_multiwindow_test_win32_is_enabled(hwnd voidptr) int
	fn C.v_multiwindow_test_win32_set_enabled(hwnd voidptr, enabled int) int
	fn C.v_multiwindow_test_win32_is_iconic(hwnd voidptr) int
	fn C.v_multiwindow_test_win32_is_zoomed(hwnd voidptr) int
	fn C.v_multiwindow_test_win32_foreground() voidptr
	fn C.v_multiwindow_test_win32_focus() voidptr
	fn C.v_multiwindow_test_win32_establish_foreground_focus(hwnd voidptr) int
	fn C.v_multiwindow_test_win32_swap_user_data(hwnd voidptr, replacement voidptr) voidptr
	fn C.v_multiwindow_test_win32_owner(hwnd voidptr) voidptr
	fn C.v_multiwindow_test_win32_style(hwnd voidptr) u64
	fn C.v_multiwindow_test_win32_ex_style(hwnd voidptr) u64
	fn C.v_multiwindow_test_win32_rect(hwnd voidptr, left &int, top &int, right &int, bottom &int) int
	fn C.v_multiwindow_test_win32_is_above(upper voidptr, lower voidptr) int
	fn C.v_multiwindow_test_win32_window_snapshot_new(hwnd voidptr) voidptr
	fn C.v_multiwindow_test_win32_window_snapshot_free(snapshot voidptr)
	fn C.v_multiwindow_test_win32_window_snapshot_matches(snapshot voidptr, hwnd voidptr) int
	fn C.v_multiwindow_test_win32_synthesized_windowed_matches(hwnd voidptr, resizable int, borderless int, requested_width int, requested_height int, expected_visible int, expected_show_command u32) int
	fn C.v_multiwindow_test_win32_service_wrong_thread_rejected(service_state voidptr) int
	fn C.v_multiwindow_test_win32_service_wrong_thread_timing(worker_delay u32, wait_timeout u32)
	fn C.v_multiwindow_test_win32_service_wrong_thread_active_count() int
	fn C.v_multiwindow_test_win32_service_wrong_thread_wait_cleanup(timeout u32) int
	fn C.v_multiwindow_test_win32_dpi(hwnd voidptr) u32
	fn C.v_multiwindow_test_win32_window_dpi_awareness(hwnd voidptr) int
	fn C.v_multiwindow_test_win32_thread_dpi_awareness_context() voidptr
	fn C.v_multiwindow_test_win32_dpi_awareness_contexts_equal(first voidptr, second voidptr) int
	fn C.v_multiwindow_test_win32_monitor_snapshot_new() voidptr
	fn C.v_multiwindow_test_win32_monitor_snapshot_free(snapshot voidptr)
	fn C.v_multiwindow_test_win32_monitor_snapshot(snapshot voidptr) int
	fn C.v_multiwindow_test_win32_monitor_identity(snapshot voidptr, index int) u64
	fn C.v_multiwindow_test_win32_monitor_name(snapshot voidptr, index int) &u16
	fn C.v_multiwindow_test_win32_monitor_info(snapshot voidptr, index int, x &int, y &int, width &int, height &int, work_x &int, work_y &int, work_width &int, work_height &int, primary &int) int
	fn C.v_multiwindow_test_win32_emit_display_change(hwnd voidptr) int
	fn C.v_multiwindow_test_win32_emit_display_changes(hwnd voidptr, count int) int
	fn C.v_multiwindow_test_win32_emit_dpi_change(hwnd voidptr, dpi u32, left int, top int, width int, height int) int
	fn C.v_multiwindow_test_win32_monitor_enumeration_capture() int
	fn C.v_multiwindow_test_win32_monitor_enumeration_use_empty()
	fn C.v_multiwindow_test_win32_monitor_enumeration_use_replay() int
	fn C.v_multiwindow_test_win32_monitor_enumeration_use_changed() int
	fn C.v_multiwindow_test_win32_monitor_enumeration_reset()
	fn C.v_multiwindow_test_win32_monitor_enumeration_empty_calls() int
	fn C.v_multiwindow_test_win32_monitor_enumeration_replay_calls() int
	fn C.v_multiwindow_test_win32_monitor_enumeration_use_info_failure() int
	fn C.v_multiwindow_test_win32_monitor_enumeration_use_growth(count int) int
	fn C.v_multiwindow_test_win32_monitor_enumeration_info_failure_calls() int
	fn C.v_multiwindow_test_win32_monitor_enumeration_growth_calls() int
	fn C.v_multiwindow_test_win32_monitor_enumeration_growth_callbacks() int
	fn C.v_multiwindow_test_win32_clipboard_equals(expected &u16, expected_units usize) int
	fn C.v_multiwindow_test_win32_set_clipboard(owner voidptr, text &u16, units usize) int
	fn C.v_multiwindow_test_win32_clipboard_unterminated_parser_probe() int
	fn C.v_multiwindow_test_win32_set_clipboard_malformed(owner voidptr, kind int) int
	fn C.v_multiwindow_win32_service_test_clipboard_configure(backend voidptr, now_ns i64, fail_open_attempts int)
	fn C.v_multiwindow_win32_service_test_clipboard_set_now_ns(backend voidptr, now_ns i64)
	fn C.v_multiwindow_win32_service_test_clipboard_use_real_clock(backend voidptr)
	fn C.v_multiwindow_win32_service_test_clipboard_fail_before_transfer(backend voidptr, count int)
	fn C.v_multiwindow_win32_service_test_clipboard_attempts(backend voidptr) int
	fn C.v_multiwindow_win32_service_test_clipboard_request_attempts(backend voidptr, request_app u64, request_serial u64) int
	fn C.v_multiwindow_win32_service_test_clipboard_last_open_owner(backend voidptr) voidptr
	fn C.v_multiwindow_win32_service_test_clipboard_pending_count(backend voidptr) int
	fn C.v_multiwindow_win32_service_test_clipboard_pending_deadline_ns(backend voidptr, index int) i64
	fn C.v_multiwindow_win32_service_test_clipboard_pending_write_matches(backend voidptr, index int, request_app u64, request_serial u64, window_app u64, window_slot int, window_generation u32, text &u16, units usize) int
	fn C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend voidptr) int
	fn C.v_multiwindow_win32_service_test_clipboard_owned_globals_peak(backend voidptr) int
	fn C.v_multiwindow_win32_service_test_clipboard_global_allocations(backend voidptr) int
	fn C.v_multiwindow_win32_service_test_clipboard_global_transfers(backend voidptr) int
	fn C.v_multiwindow_win32_service_test_clipboard_global_frees(backend voidptr) int
	fn C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend voidptr) int
	fn C.v_multiwindow_win32_service_test_clipboard_timeout_ns(backend voidptr) i64
	fn C.v_multiwindow_test_win32_dwm_dark(hwnd voidptr, value &int) int
	fn C.SendMessageW(hwnd voidptr, msg u32, wparam usize, lparam isize) isize
}

fn C.v_multiwindow_test_win32_raw_mouse_target() voidptr
fn C.v_multiwindow_test_win32_raw_mouse_registered_for(hwnd voidptr) int
fn C.v_multiwindow_test_win32_emit_focus_loss(hwnd voidptr, next_hwnd voidptr) int
fn C.v_multiwindow_test_win32_clip_matches_client(hwnd voidptr) int
fn C.v_multiwindow_test_win32_clip_is_virtual_screen() int
fn C.v_multiwindow_test_win32_capture() voidptr

fn win32_red_hwnd(app &App, id WindowId) !voidptr {
	index := app.backend.win32.window_record_index(id) or { return error(err_window_not_found) }
	hwnd := app.backend.win32.windows[index].hwnd
	if hwnd == unsafe { nil } {
		return error(err_window_not_found)
	}
	return hwnd
}

fn win32_red_poll(mut app App, attempts int) ! {
	for _ in 0 .. attempts {
		app.poll_events()!
		time.sleep(5 * time.millisecond)
	}
}

fn win32_red_add(mut issues []string, label string, ok bool) {
	if !ok {
		issues << label
	}
}

fn win32_red_monitor_membership_is_public(app &App, state ServiceWindowState) bool {
	public_ids := app.service_monitor_ids() or { return false }
	for id in state.monitor_ids {
		if id !in public_ids {
			return false
		}
		info := app.service_monitor_info(id) or { return false }
		if !info.available {
			return false
		}
	}
	return true
}

fn win32_w3_raw_monitor(native_id u64, name string) Win32ServiceRawMonitor {
	return Win32ServiceRawMonitor{
		native_id:   native_id
		name:        name
		width:       100
		height:      100
		work_width:  100
		work_height: 100
		dpi:         96
	}
}

fn test_win32_zero_window_monitor_snapshot_equality_uses_all_public_fields() {
	base := Win32ServiceRawMonitor{
		native_id:   7
		name:        'DISPLAY-A'
		x:           1
		y:           2
		width:       300
		height:      200
		work_x:      3
		work_y:      4
		work_width:  280
		work_height: 180
		dpi:         144
		primary:     1
	}
	assert win32_service_raw_monitor_snapshots_equal([base], [base])
	second := Win32ServiceRawMonitor{
		...base
		native_id: 17
		name:      'DISPLAY-B'
		primary:   0
	}
	assert win32_service_raw_monitor_snapshots_equal([base, second], [second, base])
	variants := [
		Win32ServiceRawMonitor{
			...base
			native_id: 8
		},
		Win32ServiceRawMonitor{
			...base
			name: 'DISPLAY-B'
		},
		Win32ServiceRawMonitor{
			...base
			x: 9
		},
		Win32ServiceRawMonitor{
			...base
			y: 9
		},
		Win32ServiceRawMonitor{
			...base
			width: 301
		},
		Win32ServiceRawMonitor{
			...base
			height: 201
		},
		Win32ServiceRawMonitor{
			...base
			work_x: 9
		},
		Win32ServiceRawMonitor{
			...base
			work_y: 9
		},
		Win32ServiceRawMonitor{
			...base
			work_width: 281
		},
		Win32ServiceRawMonitor{
			...base
			work_height: 181
		},
		Win32ServiceRawMonitor{
			...base
			dpi: 192
		},
		Win32ServiceRawMonitor{
			...base
			primary: 0
		},
	]
	for changed in variants {
		assert !win32_service_raw_monitor_snapshots_equal([base], [changed])
	}
	assert !win32_service_raw_monitor_snapshots_equal([base], [])
}

fn win32_w3_monitor_candidate(app_instance u64, name string, slot int, generation u32) ServiceMonitorInfo {
	return ServiceMonitorInfo{
		native_key: ServiceMonitorNativeKey{
			kind: .win32_device
			text: name
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
				width:  100
				height: 100
			}
		}
		work_area:  ServiceKnownRect{
			known: true
			value: ServiceRect{
				width:  100
				height: 100
			}
		}
		scale:      ServiceKnownScale{
			known: true
			value: 1
		}
		primary:    .off
		available:  true
	}
}

fn win32_red_rect_inside(inner ServiceRect, outer ServiceRect) bool {
	return inner.width > 0 && inner.height > 0 && outer.width > 0 && outer.height > 0
		&& inner.x >= outer.x && inner.y >= outer.y
		&& inner.x + inner.width <= outer.x + outer.width
		&& inner.y + inner.height <= outer.y + outer.height
}

fn win32_red_capability_matches(actual ServiceOperationCapability, support ServiceSupportLevel, asynchronous bool, requires_user_action bool, state_observable bool) bool {
	return actual.support == support && actual.asynchronous == asynchronous
		&& actual.requires_user_action == requires_user_action
		&& actual.state_observable == state_observable
}

fn win32_w1_wrong_thread_service_state(backend_pointer voidptr, id WindowId) string {
	unsafe {
		backend := &Win32Backend(backend_pointer)
		backend.service_window_state(id) or { return err.msg() }
	}
	return ''
}

struct Win32W1BorrowEpochProof {
mut:
	epoch        u64
	valid_inside bool
}

fn win32_red_utf16_units(text string) usize {
	mut units := usize(1)
	for codepoint in text.runes() {
		units += if codepoint > 0xffff { usize(2) } else { usize(1) }
	}
	return units
}

fn win32_red_backend_pointer(app &App) voidptr {
	return unsafe { voidptr(&app.backend.win32) }
}

fn win32_red_core_pending(app &App, request ServiceRequestId) []PendingServiceRequest {
	return app.services.pending.filter(it.id == request)
}

fn win32_red_clipboard_events(events []QueuedEvent, request ServiceRequestId) []QueuedEvent {
	return events.filter(it.kind == .service && it.service.kind == .clipboard
		&& it.service.clipboard.id == request)
}

fn win32_red_events_are_globally_ordered(events []QueuedEvent) bool {
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

fn win32_w4_settle_window_setup(mut app App, window WindowId, label string, mut issues []string) bool {
	mut quiet_cycles := 0
	for attempt in 0 .. 12 {
		accepted := app.poll_events() or {
			win32_w4_add_infra(mut issues, '${label}: poll failed: ${err.msg()}')
			return false
		}
		batch := app.drain_queued_events() or {
			win32_w4_add_infra(mut issues, '${label}: event drain failed: ${err.msg()}')
			return false
		}
		for event in batch {
			if event.kind == .lifecycle && event.lifecycle.window_id == window
				&& (event.lifecycle.kind == .window_close_requested
				|| event.lifecycle.kind == .window_destroyed) {
				issues << '${label}: target window emitted close/destroy during setup'
				return false
			}
		}
		if accepted == 0 && batch.len == 0 {
			quiet_cycles++
			if quiet_cycles == 3 {
				return true
			}
		} else {
			quiet_cycles = 0
		}
		if attempt + 1 < 12 {
			time.sleep(5 * time.millisecond)
		}
	}
	win32_w4_add_infra(mut issues, '${label}: did not reach three consecutive quiet cycles')
	return false
}

fn win32_w4_poll_collect(mut app App, attempts int, label string, mut issues []string) []QueuedEvent {
	mut delivered := []QueuedEvent{}
	for _ in 0 .. attempts {
		mut poll_failed := false
		app.poll_events() or {
			win32_w4_add_infra(mut issues, '${label}: poll failed: ${err.msg()}')
			poll_failed = true
		}
		if poll_failed {
			break
		}
		delivered << app.drain_queued_events() or {
			win32_w4_add_infra(mut issues, '${label}: event drain failed: ${err.msg()}')
			[]QueuedEvent{}
		}
		time.sleep(5 * time.millisecond)
	}
	return delivered
}

fn win32_w4_finish_single_clipboard(mut app App, backend voidptr, request ServiceRequestId, attempts int, label string, mut issues []string) []QueuedEvent {
	admitted := win32_red_core_pending(app, request)
	win32_red_add(mut issues, '${label}: core request was not admitted non-terminal',

		admitted.len == 1 && !admitted[0].terminal)
	win32_red_add(mut issues, '${label}: native request was not admitted',
		C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 1)
	for _ in 0 .. attempts {
		mut poll_failed := false
		app.poll_events() or {
			win32_w4_add_infra(mut issues, '${label}: poll failed: ${err.msg()}')
			poll_failed = true
		}
		if poll_failed || C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0 {
			break
		}
		time.sleep(5 * time.millisecond)
	}
	win32_red_add(mut issues, '${label}: native request did not become terminal',
		C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0)
	retained := win32_red_core_pending(app, request)
	win32_red_add(mut issues, '${label}: core terminal was not retained before delivery',

		retained.len == 1 && retained[0].terminal)
	delivered := app.drain_queued_events() or {
		win32_w4_add_infra(mut issues, '${label}: terminal event drain failed: ${err.msg()}')
		[]QueuedEvent{}
	}
	win32_red_add(mut issues, '${label}: core terminal survived delivery', win32_red_core_pending(app,
		request).len == 0)
	win32_red_add(mut issues, '${label}: terminal delivery lost global ordering',
		win32_red_events_are_globally_ordered(delivered))
	return delivered
}

fn win32_w4_add_infra(mut issues []string, message string) {
	issues << 'PACKAGE2_W4_INFRA=${message}'
}

fn win32_w4_is_infra(issue string) bool {
	return issue.starts_with('PACKAGE2_W4_INFRA=')
}

fn win32_w4_epilogue(family string, label string, issues []string) {
	eprintln('PACKAGE2_W4_REACHED=${family}')
	infra_issues := issues.filter(win32_w4_is_infra(it))
	contract_issues := issues.filter(!win32_w4_is_infra(it))
	if infra_issues.len == 0 && contract_issues.len > 0 {
		eprintln('PACKAGE2_RED_TERMINAL=behavioral_red:${family}')
	}
	assert issues.len == 0, '${label}:\n${issues.join('\n')}'
}

fn win32_red_clipboard_envelope_matches(event QueuedEvent, request ServiceRequestId, window WindowId, operation ServiceOperation, status ServiceStatus) bool {
	return event.kind == .service && event.sequence > 0 && event.service.kind == .clipboard
		&& event.service.sequence == event.sequence && event.service.operation == operation
		&& event.service.window == window && event.service.clipboard.id == request
		&& event.service.clipboard.window == window && event.service.clipboard.status == status
}

fn win32_red_exact_mixed_clipboard_text() string {
	max_units_without_nul := win32_red_clipboard_max_bytes / 2 - 1
	non_ascii := '漢🙂'
	non_ascii_units := int(win32_red_utf16_units(non_ascii) - 1)
	return 'A'.repeat(max_units_without_nul - non_ascii_units) + non_ascii
}

fn win32_red_exact_utf8_clipboard_text() string {
	return '漢'.repeat((win32_red_clipboard_max_bytes - 1) / 3)
}

fn test_win32_w3_late_exact_name_reserves_unavailable_slot_and_stales_old_ids_red() {
	eprintln('PACKAGE2_RED_TEST=test_win32_w3_late_exact_name_reserves_unavailable_slot_and_stales_old_ids_red')
	mut native_records := [
		Win32ServiceMonitorRecord{
			native_id:  11
			name:       'A'
			slot:       0
			generation: 4
			available:  false
		},
		Win32ServiceMonitorRecord{
			native_id:  22
			name:       'B'
			slot:       1
			generation: 9
			available:  false
		},
	]
	native := win32_reconcile_service_monitors(mut native_records, [
		win32_w3_raw_monitor(33, 'C'),
		win32_w3_raw_monitor(44, 'A'),
	], 71)
	assert native.len == 2
	native_c := native.filter(it.name == 'C')[0]
	native_a := native.filter(it.name == 'A')[0]
	assert native_c.id.slot_for_gg() == 1
	assert native_c.id.generation_for_gg() == 10
	assert native_a.id.slot_for_gg() == 0
	assert native_a.id.generation_for_gg() == 5

	instance := u64(72)
	mut registry := ServiceRegistry{
		app_instance: instance
		backend:      .win32
		monitors:     [
			service_monitor_info_for_slot(win32_w3_monitor_candidate(instance, 'A', 0, 4),
				instance, 0, 4, false, 1),
			service_monitor_info_for_slot(win32_w3_monitor_candidate(instance, 'B', 1, 9),
				instance, 1, 9, false, 1),
		]
	}
	stale_a := registry.monitors[0].id
	stale_b := registry.monitors[1].id
	public := registry.reconcile_monitor_snapshot([
		win32_w3_monitor_candidate(instance, 'C', 1, 10),
		win32_w3_monitor_candidate(instance, 'A', 0, 5),
	], 2) or { panic('valid Win32 monitor snapshot was rejected') }
	assert public.len == 2
	public_c := public.filter(it.name == 'C')[0]
	public_a := public.filter(it.name == 'A')[0]
	assert public_c.id.slot_for_gg() == 1
	assert public_c.id.generation_for_gg() == 10
	assert public_a.id.slot_for_gg() == 0
	assert public_a.id.generation_for_gg() == 5
	assert registry.monitor_index(public_c.id)! == 1
	assert registry.monitor_index(public_a.id)! == 0
	mut stale_a_rejected := false
	_ = registry.monitor_index(stale_a) or {
		stale_a_rejected = err.msg() == err_service_request_stale
		-1
	}
	mut stale_b_rejected := false
	_ = registry.monitor_index(stale_b) or {
		stale_b_rejected = err.msg() == err_service_request_stale
		-1
	}
	assert stale_a_rejected
	assert stale_b_rejected
}

fn test_win32_w1_native_authority_show_focus_and_fullscreen_contract() {
	$if windows {
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		target := app.create_window(
			title:   'Win32 W1 target'
			width:   220
			height:  140
			visible: false
		)!
		peer := app.create_window(
			title:  'Win32 W1 visible peer'
			width:  180
			height: 120
		)!
		_ = app.drain_queued_events()!
		index := app.backend.win32.window_record_index(target) or {
			assert false, 'W1 target has no Win32 record'
			return
		}
		record := app.backend.win32.windows[index]
		peer_index := app.backend.win32.window_record_index(peer) or {
			assert false, 'W1 peer has no Win32 record'
			return
		}
		peer_record := app.backend.win32.windows[peer_index]
		hwnd := record.hwnd
		peer_hwnd := peer_record.hwnd
		assert hwnd != unsafe { nil }
		assert peer_hwnd != unsafe { nil }
		assert record.service_state != unsafe { nil }

		assert C.v_multiwindow_test_win32_service_wrong_thread_rejected(record.service_state) == 1
		C.v_multiwindow_test_win32_service_wrong_thread_timing(25, 1)
		assert C.v_multiwindow_test_win32_service_wrong_thread_rejected(record.service_state) == 0
		assert C.v_multiwindow_test_win32_service_wrong_thread_active_count() == 1
		assert C.v_multiwindow_test_win32_service_wrong_thread_wait_cleanup(1000) == 1
		assert C.v_multiwindow_test_win32_service_wrong_thread_active_count() == 0
		C.v_multiwindow_test_win32_service_wrong_thread_timing(0, 5000)
		backend_pointer := unsafe { voidptr(&app.backend.win32) }
		wrong_thread := spawn win32_w1_wrong_thread_service_state(backend_pointer, target)
		assert wrong_thread.wait() == err_owner_thread_required

		show_capability := app.backend.win32.service_operation_capability(target, .show)
		assert win32_red_capability_matches(show_capability, .available, false, false, true)
		focus_capability := app.backend.win32.service_operation_capability(target, .focus)
		assert win32_red_capability_matches(focus_capability, .conditional, false, true, true)
		fullscreen_capability := app.backend.win32.service_operation_capability(target, .fullscreen)
		assert win32_red_capability_matches(fullscreen_capability, .available, false, false, true)
		restore_capability := app.backend.win32.service_operation_capability(target, .restore)
		assert win32_red_capability_matches(restore_capability, .available, false, false, true)

		foreground_before_show := C.v_multiwindow_test_win32_foreground()
		first_show := app.backend.win32.service_show_window(target)!
		second_show := app.backend.win32.service_show_window(target)!
		assert C.v_multiwindow_test_win32_is_visible(hwnd) == 1
		assert first_show.mapping == .mapped && first_show.visibility == .visible
		assert second_show.mapping == .mapped && second_show.visibility == .visible
		if foreground_before_show != unsafe { nil } && foreground_before_show != hwnd {
			assert C.v_multiwindow_test_win32_foreground() != hwnd, 'idempotent show unexpectedly activated the target window'
		}

		hidden_before_focus := app.backend.win32.service_hide_window(target)!
		assert hidden_before_focus.active == .off
		assert hidden_before_focus.focused == .off
		assert C.v_multiwindow_test_win32_establish_foreground_focus(peer_hwnd) == 1
		foreground_before_refusal := C.v_multiwindow_test_win32_foreground()
		focus_before_refusal := C.v_multiwindow_test_win32_focus()
		assert foreground_before_refusal == peer_hwnd
		assert focus_before_refusal == peer_hwnd
		C.v_multiwindow_win32_service_test_set_focus_refused(1)
		focus_after := app.backend.win32.service_focus_window(target)!
		C.v_multiwindow_win32_service_test_set_focus_refused(0)
		assert focus_after.active == .off
		assert focus_after.focused == .off
		assert focus_after.focused != .on || focus_after.active == .on
		assert C.v_multiwindow_test_win32_foreground() == foreground_before_refusal
		assert C.v_multiwindow_test_win32_focus() == focus_before_refusal
		_ = app.backend.win32.service_show_window(target)!

		replacement_data := unsafe { voidptr(peer_record) }
		expected_data := unsafe { voidptr(record) }
		original_data := C.v_multiwindow_test_win32_swap_user_data(hwnd, replacement_data)
		mut replaced_state_error := ''
		if _ := app.backend.win32.service_window_state(target) {
			replaced_state_error = 'replaced GWLP_USERDATA unexpectedly retained authority'
		} else {
			replaced_state_error = err.msg()
		}
		mut replaced_borrow_error := ''
		if _ := app.backend.win32.service_native_window_borrow(target) {
			replaced_borrow_error = 'recycled GWLP_USERDATA unexpectedly retained HWND borrow authority'
		} else {
			replaced_borrow_error = err.msg()
		}
		replaced_data := C.v_multiwindow_test_win32_swap_user_data(hwnd, original_data)
		assert original_data == expected_data
		assert replaced_data == replacement_data
		assert replaced_state_error == err_window_not_found
		assert replaced_borrow_error == err_window_not_found
		_ = app.backend.win32.service_window_state(target)!

		windowed_snapshot := C.v_multiwindow_test_win32_window_snapshot_new(hwnd)
		assert windowed_snapshot != unsafe { nil }
		defer {
			C.v_multiwindow_test_win32_window_snapshot_free(windowed_snapshot)
		}
		first_fullscreen := app.backend.win32.service_set_fullscreen(target, true)!
		assert first_fullscreen.fullscreen == .on
		fullscreen_snapshot := C.v_multiwindow_test_win32_window_snapshot_new(hwnd)
		assert fullscreen_snapshot != unsafe { nil }
		defer {
			C.v_multiwindow_test_win32_window_snapshot_free(fullscreen_snapshot)
		}
		second_fullscreen := app.backend.win32.service_set_fullscreen(target, true)!
		assert second_fullscreen.fullscreen == .on
		assert C.v_multiwindow_test_win32_window_snapshot_matches(fullscreen_snapshot, hwnd) == 1, 'idempotent fullscreen enter changed native state'

		for failure in 1 .. 4 {
			C.v_multiwindow_win32_service_test_set_fullscreen_exit_failure(failure)
			mut failure_error := ''
			if _ := app.backend.win32.service_set_fullscreen(target, false) {
				failure_error = 'injected fullscreen exit failure unexpectedly succeeded'
			} else {
				failure_error = err.msg()
			}
			C.v_multiwindow_win32_service_test_set_fullscreen_exit_failure(0)
			assert failure_error == err_capability_unsupported
			rollback_state := app.backend.win32.service_window_state(target)!
			assert rollback_state.fullscreen == .on
			assert C.v_multiwindow_test_win32_window_snapshot_matches(fullscreen_snapshot, hwnd) == 1, 'fullscreen exit failure ${failure} left partial native state'
		}

		first_restore := app.backend.win32.service_set_fullscreen(target, false)!
		assert first_restore.fullscreen == .off
		assert C.v_multiwindow_test_win32_window_snapshot_matches(windowed_snapshot, hwnd) == 1, 'fullscreen exit did not restore style/exstyle/WINDOWPLACEMENT'
		second_restore := app.backend.win32.service_set_fullscreen(target, false)!
		assert second_restore.fullscreen == .off
		assert C.v_multiwindow_test_win32_window_snapshot_matches(windowed_snapshot, hwnd) == 1, 'idempotent fullscreen exit changed restored native state'

		rollback_target := app.create_window(
			title:  'Win32 W1 rollback failure'
			width:  240
			height: 160
		)!
		rollback_index := app.backend.win32.window_record_index(rollback_target) or {
			assert false, 'W1 rollback target has no Win32 record'
			return
		}
		rollback_hwnd := app.backend.win32.windows[rollback_index].hwnd
		_ = app.backend.win32.service_set_fullscreen(rollback_target, true)!
		rollback_fullscreen_snapshot :=
			C.v_multiwindow_test_win32_window_snapshot_new(rollback_hwnd)
		assert rollback_fullscreen_snapshot != unsafe { nil }
		defer {
			C.v_multiwindow_test_win32_window_snapshot_free(rollback_fullscreen_snapshot)
		}
		C.v_multiwindow_win32_service_test_set_fullscreen_rollback_failure(1)
		C.v_multiwindow_win32_service_test_set_fullscreen_exit_failure(1)
		mut rollback_failure_error := ''
		if _ := app.backend.win32.service_set_fullscreen(rollback_target, false) {
			rollback_failure_error = 'injected rollback failure unexpectedly succeeded'
		} else {
			rollback_failure_error = err.msg()
		}
		C.v_multiwindow_win32_service_test_set_fullscreen_exit_failure(0)
		rollback_attempts := C.v_multiwindow_win32_service_test_fullscreen_rollback_attempts()
		C.v_multiwindow_win32_service_test_set_fullscreen_rollback_failure(0)
		poisoned_attempts_before :=
			C.v_multiwindow_win32_service_test_fullscreen_rollback_attempts()
		assert rollback_failure_error == err_capability_unsupported
		assert rollback_attempts == 15
		assert poisoned_attempts_before == 0
		unknown_rollback_state := app.backend.win32.service_window_state(rollback_target)!
		assert unknown_rollback_state.fullscreen == .unknown
		assert C.v_multiwindow_test_win32_window_snapshot_matches(rollback_fullscreen_snapshot,
			rollback_hwnd) == 0, 'injected rollback failure unexpectedly restored an exact native snapshot'
		poisoned_fullscreen := app.backend.win32.service_operation_capability(rollback_target,
			.fullscreen)
		poisoned_restore := app.backend.win32.service_operation_capability(rollback_target,
			.restore)
		assert win32_red_capability_matches(poisoned_fullscreen, .unsupported, false, false, true)
		assert win32_red_capability_matches(poisoned_restore, .unsupported, false, false, true)
		poisoned_snapshot := C.v_multiwindow_test_win32_window_snapshot_new(rollback_hwnd)
		assert poisoned_snapshot != unsafe { nil }
		defer {
			C.v_multiwindow_test_win32_window_snapshot_free(poisoned_snapshot)
		}
		mut poisoned_fullscreen_error := ''
		app.backend.win32.service_set_fullscreen(rollback_target, false) or {
			poisoned_fullscreen_error = err.msg()
		}
		mut poisoned_restore_error := ''
		app.backend.win32.service_restore_window(rollback_target) or {
			poisoned_restore_error = err.msg()
		}
		assert poisoned_fullscreen_error == err_capability_unsupported
		assert poisoned_restore_error == err_capability_unsupported
		assert C.v_multiwindow_win32_service_test_fullscreen_rollback_attempts() == poisoned_attempts_before
		assert C.v_multiwindow_test_win32_window_snapshot_matches(poisoned_snapshot, rollback_hwnd) == 1, 'poisoned fullscreen capabilities still mutated native state'

		initial_fullscreen := app.create_window(
			title:      'Win32 W1 initial fullscreen'
			width:      320
			height:     200
			resizable:  true
			borderless: false
			fullscreen: true
		)!
		initial_index := app.backend.win32.window_record_index(initial_fullscreen) or {
			assert false, 'W1 initial-fullscreen window has no Win32 record'
			return
		}
		initial_hwnd := app.backend.win32.windows[initial_index].hwnd
		initial_state := app.backend.win32.service_window_state(initial_fullscreen)!
		assert initial_state.fullscreen == .on
		synthesized_restore := app.backend.win32.service_set_fullscreen(initial_fullscreen, false)!
		assert synthesized_restore.fullscreen == .off
		assert C.v_multiwindow_test_win32_synthesized_windowed_matches(initial_hwnd, 1, 0, 320,
			200, 1, 3) == 1
		synthesized_snapshot := C.v_multiwindow_test_win32_window_snapshot_new(initial_hwnd)
		assert synthesized_snapshot != unsafe { nil }
		defer {
			C.v_multiwindow_test_win32_window_snapshot_free(synthesized_snapshot)
		}
		second_synthesized_restore := app.backend.win32.service_set_fullscreen(initial_fullscreen,
			false)!
		assert second_synthesized_restore.fullscreen == .off
		assert C.v_multiwindow_test_win32_window_snapshot_matches(synthesized_snapshot,
			initial_hwnd) == 1, 'idempotent synthesized restore changed native state'

		hidden_initial_fullscreen := app.create_window(
			title:      'Win32 W1 hidden initial fullscreen'
			width:      320
			height:     200
			resizable:  true
			borderless: false
			fullscreen: true
			visible:    false
		)!
		hidden_initial_index := app.backend.win32.window_record_index(hidden_initial_fullscreen) or {
			assert false, 'W1 hidden initial-fullscreen window has no Win32 record'
			return
		}
		hidden_initial_hwnd := app.backend.win32.windows[hidden_initial_index].hwnd
		assert C.v_multiwindow_test_win32_is_visible(hidden_initial_hwnd) == 0
		hidden_initial_state := app.backend.win32.service_window_state(hidden_initial_fullscreen)!
		assert hidden_initial_state.fullscreen == .on
		hidden_synthesized_restore := app.backend.win32.service_set_fullscreen(hidden_initial_fullscreen,
			false)!
		assert hidden_synthesized_restore.fullscreen == .off
		assert hidden_synthesized_restore.visibility == .hidden
		assert C.v_multiwindow_test_win32_is_visible(hidden_initial_hwnd) == 0
		assert C.v_multiwindow_test_win32_synthesized_windowed_matches(hidden_initial_hwnd, 1, 0,
			320, 200, 0, 1) == 1

		stale := app.create_window(title: 'Win32 W1 stale generation', visible: false)!
		app.destroy_window(stale)!
		replacement := app.create_window(title: 'Win32 W1 replacement', visible: false)!
		assert replacement.slot == stale.slot
		assert replacement.generation == stale.generation + 1
		mut stale_error := ''
		if _ := app.backend.win32.service_window_state(stale) {
			stale_error = 'stale WindowId unexpectedly resolved'
		} else {
			stale_error = err.msg()
		}
		assert stale_error == err_window_not_found
	}
}

fn test_win32_w1_native_borrow_is_bounded_and_epoch_checked() {
	$if windows {
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(title: 'Win32 W1 native borrow')!
		_ = app.drain_queued_events()!
		raw := app.backend.win32.service_native_window_borrow(window)!
		assert raw.backend == .win32
		assert raw.primary != unsafe { nil }
		assert raw.secondary == 0
		assert C.v_multiwindow_test_win32_is_window(raw.primary) == 1

		app_pointer := unsafe { voidptr(app) }
		shared epoch_proof := Win32W1BorrowEpochProof{}
		copy_callback := fn [app_pointer, shared epoch_proof, window, raw] (borrow NativeWindowBorrow) ! {
			owner := unsafe { &App(app_pointer) }
			assert borrow.window_for_gg() == window
			assert borrow.backend_for_gg() == .win32
			assert borrow.primary_for_gg() == raw.primary
			backend := owner.validate_native_borrow_for_gg(window, borrow.epoch_for_gg())!
			lock epoch_proof {
				epoch_proof.epoch = borrow.epoch_for_gg()
				epoch_proof.valid_inside = backend == .win32
			}
		}
		app.with_native_window_borrow(window, raw.backend, raw.primary, raw.secondary,
			copy_callback)!
		epoch, valid_inside := rlock epoch_proof {
			epoch_proof.epoch, epoch_proof.valid_inside
		}
		assert epoch != 0
		assert valid_inside
		mut epoch_error := ''
		if _ := app.validate_native_borrow_for_gg(window, epoch) {
			epoch_error = 'borrow epoch remained valid after callback'
		} else {
			epoch_error = err.msg()
		}
		assert epoch_error == err_native_borrow_stale

		bounded := app.backend.win32.service_native_window_borrow(window)!
		foreign_callback := fn [app_pointer, window] (borrow NativeWindowBorrow) ! {
			mut owner := unsafe { &App(app_pointer) }
			assert owner.validate_native_borrow_for_gg(window, borrow.epoch_for_gg())! == .win32
			result := chan string{cap: 1}
			worker := spawn fn [app_pointer, result, window] () {
				mut foreign := unsafe { &App(app_pointer) }
				foreign.destroy_window(window) or {
					result <- err.msg()
					return
				}
				result <- 'accepted'
			}()
			assert <-result == err_owner_thread_required
			worker.wait()
			owner.state_mutex.lock()
			queued := window in owner.deferred_native_windows
			owner.state_mutex.unlock()
			assert !queued
			assert owner.window_exists(window)
		}
		app.with_native_window_borrow(window, bounded.backend, bounded.primary, bounded.secondary,
			foreign_callback)!
		assert app.window_exists(window)
		assert C.v_multiwindow_test_win32_is_window(bounded.primary) == 1

		destroy_callback := fn [app_pointer, window, bounded] (borrow NativeWindowBorrow) ! {
			mut owner := unsafe { &App(app_pointer) }
			assert owner.validate_native_borrow_for_gg(window, borrow.epoch_for_gg())! == .win32
			assert borrow.primary_for_gg() == bounded.primary
			owner.destroy_window(window)!
			assert owner.window_exists(window)
			assert C.v_multiwindow_test_win32_is_window(bounded.primary) == 1
		}
		app.with_native_window_borrow(window, bounded.backend, bounded.primary, bounded.secondary,
			destroy_callback)!
		assert !app.window_exists(window)
		assert C.v_multiwindow_test_win32_is_window(bounded.primary) == 0
		mut destroyed_events := 0
		for event in app.drain_queued_events()! {
			if event.kind == .lifecycle && event.lifecycle.kind == .window_destroyed
				&& event.lifecycle.window_id == window {
				destroyed_events++
			}
		}
		assert destroyed_events == 1
	}
}

fn test_win32_native_controls_state_and_independent_window_oracles_red() {
	$if windows {
		eprintln('PACKAGE2_RED_TEST=test_win32_native_controls_state_and_independent_window_oracles_red')
		eprintln('PACKAGE2_RED_FAMILY=controls_state')
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		target := app.create_window(
			title:   'Win32 controls target'
			width:   220
			height:  140
			visible: false
		)!
		blocker := app.create_window(
			title:  'Win32 raise blocker'
			width:  180
			height: 120
		)!
		_ = app.drain_queued_events()!
		hwnd := win32_red_hwnd(app, target)!
		blocker_hwnd := win32_red_hwnd(app, blocker)!
		mut issues := []string{}

		for operation in [ServiceOperation.show, .hide, .raise, .position, .minimize, .maximize,
			.restore, .fullscreen] {
			capability := app.service_operation_capability(target, operation)!
			win32_red_add(mut issues, '${operation} capability is not available/observable', win32_red_capability_matches(capability,
				.available, false, false, true))
		}
		focus_capability := app.service_operation_capability(target, .focus)!
		win32_red_add(mut issues, 'focus must be conditional and require user action', win32_red_capability_matches(focus_capability,
			.conditional, false, true, true))

		app.service_show_window(target) or { issues << 'show failed: ${err.msg()}' }
		win32_red_poll(mut app, 2)!
		win32_red_add(mut issues, 'ShowWindow oracle remains hidden',
			C.v_multiwindow_test_win32_is_visible(hwnd) == 1)
		state_after_show := app.service_window_state(target)!
		win32_red_add(mut issues, 'show state is not mapped/visible',

			state_after_show.mapping == .mapped && state_after_show.visibility == .visible)

		app.service_set_position(target, 37, 41) or { issues << 'position failed: ${err.msg()}' }
		win32_red_poll(mut app, 2)!
		mut left := 0
		mut top := 0
		mut right := 0
		mut bottom := 0
		assert C.v_multiwindow_test_win32_rect(hwnd, &left, &top, &right, &bottom) == 1, 'GetWindowRect oracle admission failed'

		win32_red_add(mut issues, 'GetWindowRect does not observe requested position', left == 37
			&& top == 41)

		app.service_minimize_window(target) or { issues << 'minimize failed: ${err.msg()}' }
		win32_red_poll(mut app, 4)!
		win32_red_add(mut issues, 'IsIconic did not observe minimize',
			C.v_multiwindow_test_win32_is_iconic(hwnd) == 1)

		app.service_restore_window(target) or {
			issues << 'restore after minimize failed: ${err.msg()}'
		}
		win32_red_poll(mut app, 4)!
		win32_red_add(mut issues, 'restore left the window iconic',
			C.v_multiwindow_test_win32_is_iconic(hwnd) == 0)

		app.service_maximize_window(target) or { issues << 'maximize failed: ${err.msg()}' }
		win32_red_poll(mut app, 4)!
		win32_red_add(mut issues, 'IsZoomed did not observe maximize',
			C.v_multiwindow_test_win32_is_zoomed(hwnd) == 1)

		app.service_restore_window(target) or {
			issues << 'restore after maximize failed: ${err.msg()}'
		}
		win32_red_poll(mut app, 4)!
		win32_red_add(mut issues, 'restore left the window zoomed',
			C.v_multiwindow_test_win32_is_zoomed(hwnd) == 0)

		app.service_raise_window(target) or { issues << 'raise failed: ${err.msg()}' }
		win32_red_poll(mut app, 2)!
		win32_red_add(mut issues, 'z-order oracle did not place target above peer', C.v_multiwindow_test_win32_is_above(hwnd,
			blocker_hwnd) == 1)

		// Windows may deny foreground activation even when the request is valid.
		app.service_request_focus(target) or {}
		win32_red_poll(mut app, 4)!
		if C.v_multiwindow_test_win32_foreground() == hwnd {
			focused_state := app.service_window_state(target)!
			win32_red_add(mut issues, 'foreground HWND is not reflected as focused/active',

				focused_state.focused == .on && focused_state.active == .on)
		}

		style_before_fullscreen := C.v_multiwindow_test_win32_style(hwnd)
		app.service_set_fullscreen(target, true) or {
			issues << 'fullscreen enter failed: ${err.msg()}'
		}
		win32_red_poll(mut app, 4)!
		fullscreen_state := app.service_window_state(target)!
		win32_red_add(mut issues, 'fullscreen state did not become on',
			fullscreen_state.fullscreen == .on)
		win32_red_add(mut issues, 'native style did not change for fullscreen',
			C.v_multiwindow_test_win32_style(hwnd) != style_before_fullscreen)
		app.service_set_fullscreen(target, false) or {
			issues << 'fullscreen exit failed: ${err.msg()}'
		}
		win32_red_poll(mut app, 4)!
		win32_red_add(mut issues, 'native style was not restored after fullscreen',
			C.v_multiwindow_test_win32_style(hwnd) == style_before_fullscreen)

		app.service_hide_window(target) or { issues << 'hide failed: ${err.msg()}' }
		win32_red_poll(mut app, 2)!
		win32_red_add(mut issues, 'ShowWindow oracle remains visible after hide',
			C.v_multiwindow_test_win32_is_visible(hwnd) == 0)
		state_after_hide := app.service_window_state(target)!
		win32_red_add(mut issues, 'hide state is not unmapped/hidden',

			state_after_hide.mapping == .unmapped && state_after_hide.visibility == .hidden)

		if issues.len > 0 {
			eprintln('PACKAGE2_RED_TERMINAL=behavioral_red:controls_state')
		}
		assert issues.len == 0, 'Win32 controls/state RED:\n${issues.join('\n')}'
	}
}

fn win32_w2_native_modal_fault_path_regressions(mut issues []string) ! {
	$if windows {
		mut create_app := new_app(backend: .win32)!
		defer {
			C.v_multiwindow_win32_test_modal_set_enable_failures(0)
			C.v_multiwindow_win32_test_modal_set_show_created_failures(0)
			create_app.stop() or {}
		}
		create_owner := create_app.create_window(title: 'Win32 modal create-fault owner')!
		create_owner_hwnd := win32_red_hwnd(create_app, create_owner)!
		before_create_records := create_app.backend.win32.windows.len
		C.v_multiwindow_win32_test_modal_trace_reset(create_owner_hwnd, unsafe { nil })
		C.v_multiwindow_win32_test_modal_set_show_created_failures(1)
		C.v_multiwindow_win32_test_modal_set_enable_failures(1)
		mut create_error := ''
		create_app.create_window(
			title: 'Win32 modal create-fault child'
			owner: create_owner
			modal: true
		) or { create_error = err.msg() }
		C.v_multiwindow_win32_test_modal_set_show_created_failures(0)
		C.v_multiwindow_win32_test_modal_set_enable_failures(0)
		attempted_hwnd := C.v_multiwindow_win32_test_modal_trace_window_value()
		win32_red_add(mut issues, 'create show/release fault suppressed rollback failure',
			create_error.contains(err_win32_create_window_failed)
			&& create_error.contains('modal rollback failed:'))
		win32_red_add(mut issues, 'create show/release fault left a backend record',
			create_app.backend.win32.windows.len == before_create_records)
		win32_red_add(mut issues, 'create show/release fault left a native HWND',
			attempted_hwnd != unsafe { nil }
			&& C.v_multiwindow_test_win32_is_window(attempted_hwnd) == 0)
		win32_red_add(mut issues, 'create show/release fault left owner disabled',
			C.v_multiwindow_test_win32_is_enabled(create_owner_hwnd) == 1)
		win32_red_add(mut issues, 'create show/release recovery violated release-before-destroy',
			C.v_multiwindow_win32_test_modal_owner_enable_count_value() == 1
			&& C.v_multiwindow_win32_test_modal_destroy_count_value() == 1
			&& C.v_multiwindow_win32_test_modal_owner_enable_sequence_value() > 0
			&& C.v_multiwindow_win32_test_modal_owner_enable_sequence_value() < C.v_multiwindow_win32_test_modal_destroy_sequence_value())
		create_app.destroy_window(create_owner) or {
			issues << 'create-fault owner cleanup failed: ${err.msg()}'
		}

		mut destroy_app := new_app(backend: .win32)!
		defer {
			C.v_multiwindow_win32_test_modal_set_destroy_failures(0)
			destroy_app.stop() or {}
		}
		destroy_owner := destroy_app.create_window(title: 'Win32 modal destroy-fault owner')!
		destroy_modal := destroy_app.create_window(
			title: 'Win32 modal destroy-fault child'
			owner: destroy_owner
			modal: true
		)!
		_ = destroy_app.drain_queued_events()!
		destroy_owner_hwnd := win32_red_hwnd(destroy_app, destroy_owner)!
		destroy_modal_hwnd := win32_red_hwnd(destroy_app, destroy_modal)!
		C.v_multiwindow_win32_test_modal_trace_reset(destroy_owner_hwnd, destroy_modal_hwnd)
		C.v_multiwindow_win32_test_modal_set_destroy_failures(1)
		mut destroy_error := ''
		destroy_app.destroy_window(destroy_modal) or { destroy_error = err.msg() }
		C.v_multiwindow_win32_test_modal_set_destroy_failures(0)
		expected_child_destroy_error := 'multiwindow: terminal lifecycle failed: multiwindow: win32 destroy window failed'
		win32_red_add(mut issues, 'DestroyWindow fault was not propagated',
			destroy_error == expected_child_destroy_error)
		mut child_destroy_events := 0
		for event in destroy_app.drain_queued_events()! {
			if event.kind == .lifecycle && event.lifecycle.kind == .window_destroyed
				&& event.lifecycle.window_id == destroy_modal {
				child_destroy_events++
			}
		}
		win32_red_add(mut issues, 'failed child destroy did not become core-terminal once',

			destroy_app.window_destroy_finished(destroy_modal) && child_destroy_events == 1)
		if destroy_modal_index := destroy_app.backend.win32.window_record_index(destroy_modal) {
			if destroy_owner_index := destroy_app.backend.win32.window_record_index(destroy_owner) {
				destroy_record := destroy_app.backend.win32.windows[destroy_modal_index]
				mut retained_owner_matches := false
				if retained_owner := destroy_record.config.owner {
					retained_owner_matches = retained_owner == destroy_owner
				}
				win32_red_add(mut issues,
					'DestroyWindow fault did not retain complete released child debt',
					destroy_record.hwnd == destroy_modal_hwnd
					&& destroy_record.service_state != unsafe { nil } && retained_owner_matches
					&& !destroy_record.modal_active
					&& destroy_app.backend.win32.windows[destroy_owner_index].modal_child_count == 0
					&& !destroy_app.backend.win32.windows[destroy_owner_index].modal_restore_enabled)
			} else {
				issues << 'DestroyWindow fault removed the owner record'
			}
		} else {
			issues << 'DestroyWindow fault removed the modal record'
		}
		win32_red_add(mut issues, 'DestroyWindow fault did not retain released child HWND',
			C.v_multiwindow_test_win32_is_window(destroy_modal_hwnd) == 1
			&& C.v_multiwindow_test_win32_is_enabled(destroy_owner_hwnd) == 1)
		win32_red_add(mut issues, 'DestroyWindow fault reactivated native modality',
			C.v_multiwindow_win32_test_modal_owner_enable_count_value() == 1
			&& C.v_multiwindow_win32_test_modal_owner_disable_count_value() == 0
			&& C.v_multiwindow_win32_test_modal_destroy_count_value() == 0
			&& C.v_multiwindow_win32_test_modal_destroy_attempt_count_value() == 1)
		mut retained_child_records := 0
		for retained_record in destroy_app.backend.win32.windows {
			if retained_record.id == destroy_modal {
				retained_child_records++
			}
		}
		win32_red_add(mut issues, 'DestroyWindow fault did not retain exactly one child debt',

			retained_child_records == 1 && destroy_app.backend.win32.windows.len == 2)
		mut replay_error := ''
		destroy_app.destroy_window(destroy_modal) or { replay_error = err.msg() }
		mut replay_destroy_events := 0
		for event in destroy_app.drain_queued_events()! {
			if event.kind == .lifecycle && event.lifecycle.kind == .window_destroyed
				&& event.lifecycle.window_id == destroy_modal {
				replay_destroy_events++
			}
		}
		win32_red_add(mut issues, 'second child destroy did not replay terminal error',
			replay_error == destroy_error)
		win32_red_add(mut issues, 'second child destroy retried HWND or emitted another event',
			C.v_multiwindow_win32_test_modal_destroy_attempt_count_value() == 1
			&& replay_destroy_events == 0)
		mut owner_destroy_error := ''
		destroy_app.destroy_window(destroy_owner) or { owner_destroy_error = err.msg() }
		expected_owner_destroy_error := 'multiwindow: terminal lifecycle failed: multiwindow: window owner relation is invalid'
		win32_red_add(mut issues, 'retained child debt did not reject owner destroy',
			owner_destroy_error == expected_owner_destroy_error
			&& destroy_app.window_destroy_finished(destroy_owner))
		mut owner_destroy_events := 0
		for event in destroy_app.drain_queued_events()! {
			if event.kind == .lifecycle && event.lifecycle.kind == .window_destroyed
				&& event.lifecycle.window_id == destroy_owner {
				owner_destroy_events++
			}
		}
		win32_red_add(mut issues, 'rejected owner destroy did not emit exactly one event',
			owner_destroy_events == 1)
		win32_red_add(mut issues, 'rejected owner destroy reached native DestroyWindow',
			C.v_multiwindow_test_win32_is_window(destroy_owner_hwnd) == 1
			&& C.v_multiwindow_test_win32_is_window(destroy_modal_hwnd) == 1
			&& C.v_multiwindow_win32_test_modal_owner_destroy_attempt_count_value() == 0)
		win32_red_add(mut issues, 'rejected owner destroy changed released modality',
			C.v_multiwindow_test_win32_is_enabled(destroy_owner_hwnd) == 1
			&& C.v_multiwindow_win32_test_modal_owner_enable_count_value() == 1
			&& C.v_multiwindow_win32_test_modal_owner_disable_count_value() == 0)
		mut owner_replay_error := ''
		destroy_app.destroy_window(destroy_owner) or { owner_replay_error = err.msg() }
		mut owner_replay_destroy_events := 0
		for event in destroy_app.drain_queued_events()! {
			if event.kind == .lifecycle && event.lifecycle.kind == .window_destroyed
				&& event.lifecycle.window_id == destroy_owner {
				owner_replay_destroy_events++
			}
		}
		win32_red_add(mut issues, 'second owner destroy did not replay terminal error',
			owner_replay_error == expected_owner_destroy_error)
		win32_red_add(mut issues, 'second owner destroy retried HWND or emitted another event',
			C.v_multiwindow_win32_test_modal_owner_destroy_attempt_count_value() == 0
			&& owner_replay_destroy_events == 0)
		destroy_app.stop() or { issues << 'retained child debt stop cleanup failed: ${err.msg()}' }
		win32_red_add(mut issues, 'one stop left retained Win32 records',
			destroy_app.backend.win32.windows.len == 0)
		win32_red_add(mut issues, 'one stop left native owner/modal HWNDs alive',
			C.v_multiwindow_test_win32_is_window(destroy_owner_hwnd) == 0
			&& C.v_multiwindow_test_win32_is_window(destroy_modal_hwnd) == 0)
		win32_red_add(mut issues, 'stop did not retry retained child before owner',
			C.v_multiwindow_win32_test_modal_destroy_attempt_count_value() == 2
			&& C.v_multiwindow_win32_test_modal_owner_destroy_attempt_count_value() == 1
			&& C.v_multiwindow_win32_test_modal_destroy_count_value() == 1
			&& C.v_multiwindow_win32_test_modal_owner_destroy_count_value() == 1
			&& C.v_multiwindow_win32_test_modal_destroy_sequence_value() > 0
			&& C.v_multiwindow_win32_test_modal_destroy_sequence_value() < C.v_multiwindow_win32_test_modal_owner_destroy_sequence_value())
	}
}

fn test_win32_native_modal_reenable_and_child_first_hwnd_destruction_red() {
	$if windows {
		eprintln('PACKAGE2_RED_TEST=test_win32_native_modal_reenable_and_child_first_hwnd_destruction_red')
		eprintln('PACKAGE2_RED_FAMILY=modal_child_first')
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		owner := app.create_window(title: 'Win32 modal owner')!
		child := app.create_window(
			title:   'Win32 modal child'
			owner:   owner
			modal:   true
			visible: false
		)!
		grandchild := app.create_window(
			title:   'Win32 modal grandchild'
			owner:   child
			modal:   true
			visible: false
		)!
		modal_peer := app.create_window(
			title:   'Win32 modal peer'
			owner:   owner
			modal:   true
			visible: false
		)!
		_ = app.drain_queued_events()!
		owner_hwnd := win32_red_hwnd(app, owner)!
		child_hwnd := win32_red_hwnd(app, child)!
		grandchild_hwnd := win32_red_hwnd(app, grandchild)!
		modal_peer_hwnd := win32_red_hwnd(app, modal_peer)!
		mut issues := []string{}
		win32_w2_native_modal_fault_path_regressions(mut issues) or {
			issues << 'modal fault-path setup failed: ${err.msg()}'
		}

		app.service_show_window(child) or { issues << 'modal show failed: ${err.msg()}' }
		app.service_show_window(child) or { issues << 'idempotent modal show failed: ${err.msg()}' }
		win32_red_poll(mut app, 2)!
		win32_red_add(mut issues, 'GW_OWNER does not match configured owner',
			C.v_multiwindow_test_win32_owner(child_hwnd) == owner_hwnd)
		win32_red_add(mut issues, 'owned window was incorrectly converted to WS_CHILD',
			C.v_multiwindow_test_win32_style(child_hwnd) & win32_red_ws_child == 0)
		win32_red_add(mut issues, 'shown modal child did not disable owner',
			C.v_multiwindow_test_win32_is_enabled(owner_hwnd) == 0)

		app.service_show_window(grandchild) or {
			issues << 'nested modal show failed: ${err.msg()}'
		}
		win32_red_add(mut issues, 'nested modal did not disable its direct owner',
			C.v_multiwindow_test_win32_is_enabled(child_hwnd) == 0)
		win32_red_add(mut issues, 'nested modal unexpectedly re-enabled the root owner',
			C.v_multiwindow_test_win32_is_enabled(owner_hwnd) == 0)
		app.service_hide_window(grandchild) or {
			issues << 'nested modal hide failed: ${err.msg()}'
		}
		win32_red_add(mut issues, 'nested modal did not restore its direct owner',
			C.v_multiwindow_test_win32_is_enabled(child_hwnd) == 1)
		win32_red_add(mut issues, 'nested modal release unexpectedly restored the root owner',
			C.v_multiwindow_test_win32_is_enabled(owner_hwnd) == 0)

		app.service_show_window(modal_peer) or { issues << 'modal peer show failed: ${err.msg()}' }
		win32_red_add(mut issues, 'modal peer GW_OWNER does not match configured owner',
			C.v_multiwindow_test_win32_owner(modal_peer_hwnd) == owner_hwnd)
		app.service_hide_window(child) or { issues << 'modal hide failed: ${err.msg()}' }
		app.service_hide_window(child) or { issues << 'idempotent modal hide failed: ${err.msg()}' }
		win32_red_poll(mut app, 2)!
		win32_red_add(mut issues, 'hiding one modal re-enabled owner while peer remained visible',
			C.v_multiwindow_test_win32_is_enabled(owner_hwnd) == 0)
		app.service_hide_window(modal_peer) or { issues << 'modal peer hide failed: ${err.msg()}' }
		win32_red_add(mut issues, 'hiding final modal child did not re-enable owner',
			C.v_multiwindow_test_win32_is_enabled(owner_hwnd) == 1)

		app.service_show_window(child) or { issues << 'second modal show failed: ${err.msg()}' }
		C.v_multiwindow_win32_test_modal_trace_reset(owner_hwnd, child_hwnd)
		app.destroy_window(child)!
		win32_red_poll(mut app, 2)!
		destroy_events := app.drain_queued_events()!
		mut destroyed_ids := []WindowId{}
		for event in destroy_events {
			if event.kind == .lifecycle && event.lifecycle.kind == .window_destroyed {
				destroyed_ids << event.lifecycle.window_id
			}
		}
		win32_red_add(mut issues, 'destroying modal child did not re-enable owner',
			C.v_multiwindow_test_win32_is_enabled(owner_hwnd) == 1)
		win32_red_add(mut issues, 'child HWND survived public destroy',
			C.v_multiwindow_test_win32_is_window(child_hwnd) == 0)
		win32_red_add(mut issues, 'grandchild HWND survived child-first cascade',
			C.v_multiwindow_test_win32_is_window(grandchild_hwnd) == 0)
		win32_red_add(mut issues, 'modal owner was not re-enabled exactly once before destroy',
			C.v_multiwindow_win32_test_modal_owner_enable_count_value() == 1
			&& C.v_multiwindow_win32_test_modal_destroy_count_value() == 1
			&& C.v_multiwindow_win32_test_modal_owner_enable_sequence_value() > 0
			&& C.v_multiwindow_win32_test_modal_owner_enable_sequence_value() < C.v_multiwindow_win32_test_modal_destroy_sequence_value())
		win32_red_add(mut issues, 'canonical lifecycle queue is not child-first',

			destroyed_ids.len == 2 && destroyed_ids[0] == grandchild && destroyed_ids[1] == child)

		C.v_multiwindow_win32_test_modal_trace_reset(owner_hwnd, unsafe { nil })
		initial_modal := app.create_window(
			title: 'Win32 initially visible modal'
			owner: owner
			modal: true
		)!
		initial_modal_hwnd := win32_red_hwnd(app, initial_modal)!
		win32_red_add(mut issues, 'initially visible modal has no configured GW_OWNER',
			C.v_multiwindow_test_win32_owner(initial_modal_hwnd) == owner_hwnd)
		win32_red_add(mut issues, 'initially visible modal did not disable owner',
			C.v_multiwindow_test_win32_is_enabled(owner_hwnd) == 0)
		win32_red_add(mut issues, 'initial modal became visible before disabling its owner',
			C.v_multiwindow_win32_test_modal_owner_disable_count_value() == 1
			&& C.v_multiwindow_win32_test_modal_show_count_value() == 1
			&& C.v_multiwindow_win32_test_modal_owner_disable_sequence_value() > 0
			&& C.v_multiwindow_win32_test_modal_owner_disable_sequence_value() < C.v_multiwindow_win32_test_modal_show_sequence_value())
		app.service_hide_window(initial_modal) or {
			issues << 'initially visible modal hide failed: ${err.msg()}'
		}
		win32_red_add(mut issues, 'hiding initially visible modal did not re-enable owner',
			C.v_multiwindow_test_win32_is_enabled(owner_hwnd) == 1)
		app.destroy_window(initial_modal)!

		disabled_owner := app.create_window(title: 'Win32 initially disabled owner')!
		disabled_owner_hwnd := win32_red_hwnd(app, disabled_owner)!
		win32_red_add(mut issues, 'oracle could not disable modal owner', C.v_multiwindow_test_win32_set_enabled(disabled_owner_hwnd,
			0) == 1)
		disabled_owner_modal := app.create_window(
			title:   'Win32 modal with initially disabled owner'
			owner:   disabled_owner
			modal:   true
			visible: false
		)!
		app.service_show_window(disabled_owner_modal) or {
			issues << 'initially-disabled owner modal show failed: ${err.msg()}'
		}
		app.service_hide_window(disabled_owner_modal) or {
			issues << 'initially-disabled owner modal hide failed: ${err.msg()}'
		}
		win32_red_add(mut issues, 'modal release enabled an owner that started disabled',
			C.v_multiwindow_test_win32_is_enabled(disabled_owner_hwnd) == 0)
		win32_red_add(mut issues, 'oracle could not restore initially-disabled owner for cleanup', C.v_multiwindow_test_win32_set_enabled(disabled_owner_hwnd,
			1) == 1)
		app.destroy_window(disabled_owner_modal)!
		app.destroy_window(disabled_owner)!

		rollback_owner := app.create_window(title: 'Win32 modal rollback owner')!
		rollback_modal := app.create_window(
			title:   'Win32 modal rollback child'
			owner:   rollback_owner
			modal:   true
			visible: false
		)!
		rollback_owner_hwnd := win32_red_hwnd(app, rollback_owner)!
		C.v_multiwindow_win32_service_test_set_show_failure(1)
		C.v_multiwindow_win32_test_modal_set_enable_failure(1)
		mut rollback_error := ''
		app.service_show_window(rollback_modal) or { rollback_error = err.msg() }
		C.v_multiwindow_win32_service_test_set_show_failure(0)
		C.v_multiwindow_win32_test_modal_set_enable_failure(0)
		win32_red_add(mut issues, 'show rollback suppressed release_modal failure',
			rollback_error.contains('modal rollback failed:'))
		if rollback_index := app.backend.win32.window_record_index(rollback_modal) {
			win32_red_add(mut issues, 'failed modal rollback announced inactive state',
				app.backend.win32.windows[rollback_index].modal_active
				&& C.v_multiwindow_test_win32_is_enabled(rollback_owner_hwnd) == 0)
		} else {
			issues << 'failed modal rollback removed the native record'
		}
		app.service_hide_window(rollback_modal) or {
			issues << 'modal rollback recovery failed: ${err.msg()}'
		}
		win32_red_add(mut issues, 'modal rollback recovery did not restore owner',
			C.v_multiwindow_test_win32_is_enabled(rollback_owner_hwnd) == 1)
		app.destroy_window(rollback_modal)!
		app.destroy_window(rollback_owner)!

		teardown_owner := app.create_window(title: 'Win32 modal teardown owner')!
		teardown_modal := app.create_window(
			title: 'Win32 modal teardown child'
			owner: teardown_owner
			modal: true
		)!
		teardown_owner_hwnd := win32_red_hwnd(app, teardown_owner)!
		teardown_modal_hwnd := win32_red_hwnd(app, teardown_modal)!
		C.v_multiwindow_win32_test_modal_set_enable_failure(1)
		mut teardown_error := ''
		app.backend.win32.finish_window_teardown(teardown_modal) or { teardown_error = err.msg() }
		C.v_multiwindow_win32_test_modal_set_enable_failure(0)
		win32_red_add(mut issues, 'teardown ignored modal release failure',
			teardown_error == err_capability_unsupported)
		if teardown_index := app.backend.win32.window_record_index(teardown_modal) {
			win32_red_add(mut issues, 'failed modal release partially destroyed native state',
				app.backend.win32.windows[teardown_index].modal_active
				&& C.v_multiwindow_test_win32_is_window(teardown_modal_hwnd) == 1
				&& C.v_multiwindow_test_win32_is_enabled(teardown_owner_hwnd) == 0)
		} else {
			issues << 'failed modal release removed the native teardown record'
		}
		app.destroy_window(teardown_modal)!
		app.destroy_window(teardown_owner)!

		app.destroy_window(owner)!
		win32_red_poll(mut app, 2)!
		win32_red_add(mut issues, 'owner HWND survived public destroy',
			C.v_multiwindow_test_win32_is_window(owner_hwnd) == 0)

		mut stop_app := new_app(backend: .win32)!
		stop_owner := stop_app.create_window(title: 'Win32 modal stop owner')!
		stop_modal_a := stop_app.create_window(
			title: 'Win32 modal stop child A'
			owner: stop_owner
			modal: true
		)!
		stop_modal_b := stop_app.create_window(
			title: 'Win32 modal stop child B'
			owner: stop_owner
			modal: true
		)!
		stop_owner_hwnd := win32_red_hwnd(stop_app, stop_owner)!
		stop_modal_a_hwnd := win32_red_hwnd(stop_app, stop_modal_a)!
		stop_modal_b_hwnd := win32_red_hwnd(stop_app, stop_modal_b)!
		C.v_multiwindow_win32_test_modal_trace_reset(stop_owner_hwnd, stop_modal_b_hwnd)
		stop_app.stop() or { issues << 'modal stop failed: ${err.msg()}' }
		win32_red_add(mut issues,
			'stop did not restore owner exactly once before final modal destroy',
			C.v_multiwindow_win32_test_modal_owner_enable_count_value() == 1
			&& C.v_multiwindow_win32_test_modal_destroy_count_value() == 1
			&& C.v_multiwindow_win32_test_modal_owner_enable_sequence_value() > 0
			&& C.v_multiwindow_win32_test_modal_owner_enable_sequence_value() < C.v_multiwindow_win32_test_modal_destroy_sequence_value())
		win32_red_add(mut issues, 'stop left native owner/modal HWNDs alive',
			C.v_multiwindow_test_win32_is_window(stop_owner_hwnd) == 0
			&& C.v_multiwindow_test_win32_is_window(stop_modal_a_hwnd) == 0
			&& C.v_multiwindow_test_win32_is_window(stop_modal_b_hwnd) == 0)
		if issues.len > 0 {
			eprintln('PACKAGE2_RED_TERMINAL=behavioral_red:modal_child_first')
		}
		assert issues.len == 0, 'Win32 owner/modal/child-first RED:\n${issues.join('\n')}'
		eprintln('PACKAGE2_W2_GREEN_TERMINAL=behavioral_green:modal_child_first')
	}
}

fn win32_red_dpi_creation_fallback_case(context_mode int) ! {
	$if windows {
		C.v_multiwindow_win32_test_dpi_creation_configure(context_mode, 37, 29)
		defer {
			C.v_multiwindow_win32_test_dpi_creation_reset()
		}
		caller_context_before := C.v_multiwindow_test_win32_thread_dpi_awareness_context()
		assert caller_context_before != unsafe { nil }, 'GetThreadDpiAwarenessContext is unavailable before fallback DPI creation'

		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		owner := app.create_window(
			title:    'Win32 DPI fallback owner ${context_mode}'
			width:    317
			height:   193
			visible:  false
			high_dpi: true
		)!
		owner_hwnd := win32_red_hwnd(app, owner)!
		assert C.v_multiwindow_win32_test_client_size_matches(owner_hwnd, 317, 193) == 1, 'fallback DPI creation did not preserve the requested owner client size'

		child := app.create_window(
			title:    'Win32 DPI fallback owned ${context_mode}'
			width:    271
			height:   149
			visible:  false
			high_dpi: true
			owner:    owner
		)!
		child_hwnd := win32_red_hwnd(app, child)!
		assert C.v_multiwindow_test_win32_owner(child_hwnd) == owner_hwnd
		assert C.v_multiwindow_win32_test_client_size_matches(child_hwnd, 271, 149) == 1, 'fallback DPI creation did not preserve the requested owned client size'

		constrained := app.create_window(
			title:    'Win32 DPI fallback constrained ${context_mode}'
			width:    96
			height:   72
			visible:  false
			high_dpi: true
		)!
		constrained_hwnd := win32_red_hwnd(app, constrained)!
		assert C.v_multiwindow_win32_test_client_size_matches(constrained_hwnd, 96, 72) == 1, 'non-interactive DPI correction did not preserve a client size below the system tracking minimum'
		app.poll_events()!
		creation_events := app.drain_queued_events()!
		assert creation_events.filter(it.kind == .lifecycle && it.lifecycle.kind == .window_resized).len == 0, 'fallback DPI correction leaked a transient creation resize event'

		app.resize_window(owner, 433, 259)!
		owner_info := app.window_info(owner)!
		assert owner_info.width == 433 && owner_info.height == 259
		assert C.v_multiwindow_win32_test_client_size_matches(owner_hwnd, 433, 259) == 1, 'HWND-aware resize did not preserve the requested client size'
		assert C.v_multiwindow_win32_test_dpi_context_attempt_count() == 3
		assert C.v_multiwindow_win32_test_dpi_context_fallback_count() == 3
		assert C.v_multiwindow_win32_test_dpi_exact_resize_count() >= 4

		caller_context_after := C.v_multiwindow_test_win32_thread_dpi_awareness_context()
		assert caller_context_after != unsafe { nil }, 'GetThreadDpiAwarenessContext is unavailable after fallback DPI creation'
		assert C.v_multiwindow_test_win32_dpi_awareness_contexts_equal(caller_context_before,
			caller_context_after) == 1, 'fallback DPI creation changed the caller thread DPI-awareness context'
	} $else {
		_ = context_mode
		return error('Win32 DPI fallback case is unavailable')
	}
}

fn test_win32_native_monitor_dpi_display_change_and_generation_red() {
	$if windows {
		eprintln('PACKAGE2_RED_TEST=test_win32_native_monitor_dpi_display_change_and_generation_red')
		eprintln('PACKAGE2_RED_FAMILY=monitor_dpi_hotplug')
		win32_red_dpi_creation_fallback_case(win32_red_dpi_context_unavailable)!
		win32_red_dpi_creation_fallback_case(win32_red_dpi_context_rejected)!
		empty_calls_before_cold_start :=
			C.v_multiwindow_test_win32_monitor_enumeration_empty_calls()
		C.v_multiwindow_test_win32_monitor_enumeration_use_empty()
		defer {
			C.v_multiwindow_test_win32_monitor_enumeration_reset()
		}
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		assert C.v_multiwindow_test_win32_monitor_enumeration_empty_calls() > empty_calls_before_cold_start, 'Win32 cold-start admission did not use the forced empty monitor snapshot'

		assert app.backend.win32.service_monitors.all(!it.available), 'Win32 backend retained available monitors after forced-empty cold-start refresh'

		assert app.services.monitors.len == 0, 'public registry published monitors during forced-empty cold-start admission'

		assert app.service_monitor_ids()!.len == 0, 'public registry exposed monitor ids during forced-empty cold-start admission'

		C.v_multiwindow_test_win32_monitor_enumeration_reset()
		caller_dpi_context_before := C.v_multiwindow_test_win32_thread_dpi_awareness_context()
		assert caller_dpi_context_before != unsafe { nil }, 'GetThreadDpiAwarenessContext is unavailable before high_dpi window creation'

		window := app.create_window(
			title:    'Win32 monitor oracle'
			width:    320
			height:   200
			high_dpi: true
		)!
		caller_dpi_context_after := C.v_multiwindow_test_win32_thread_dpi_awareness_context()
		assert caller_dpi_context_after != unsafe { nil }, 'GetThreadDpiAwarenessContext is unavailable after high_dpi window creation'

		cold_start_index := app.backend.win32.window_record_index(window) or {
			assert false, 'Win32 cold-start window has no backend record'
			0
		}
		assert app.backend.win32.service_monitors.all(!it.available), 'Win32 first-window admission published the staged backend monitor snapshot'

		assert app.services.monitors.len == 0, 'public registry changed before cold-start polling'

		assert app.backend.win32.service_monitor_pending_records.any(it.available), 'Win32 first-window admission did not stage the pre-create monitor snapshot'

		assert app.backend.win32.service_monitor_pending_raw.len > 0, 'Win32 first-window admission did not retain the staged raw snapshot'

		assert app.backend.win32.windows[cold_start_index].service_monitor_ids.len == 0, 'Win32 first window exposed unpublished staged monitor membership'

		cold_start_pre_poll_state := app.service_window_state(window)!
		assert cold_start_pre_poll_state.monitor_ids.len == 0, 'Win32 first-window state exposed unpublished staged monitor ids'

		assert win32_red_monitor_membership_is_public(app, cold_start_pre_poll_state), 'Win32 first-window state referenced an unresolved public monitor before polling'

		win32_red_poll(mut app, 2)!
		assert app.backend.win32.service_monitors.len > 0, 'Win32 cold-start polling did not populate backend monitors'

		assert app.services.monitors.any(it.available), 'Win32 cold-start polling did not populate the public monitor registry'

		cold_start_ids := app.service_monitor_ids()!
		assert cold_start_ids.len > 0, 'Win32 cold-start polling did not expose public monitor ids'

		cold_start_record := app.backend.win32.windows[cold_start_index]
		assert cold_start_record.service_monitor_ids.len > 0, 'Win32 cold-start polling did not populate record monitor membership'

		for monitor_id in cold_start_record.service_monitor_ids {
			assert monitor_id in cold_start_ids, 'Win32 cold-start record references a monitor outside the public registry'
		}
		cold_start_state := app.service_window_state(window)!
		assert cold_start_state.monitor_ids.len > 0, 'Win32 cold-start polling did not populate public window membership'

		assert cold_start_state.monitor_ids == cold_start_record.service_monitor_ids, 'Win32 cold-start public and backend window memberships diverged'

		_ = app.drain_queued_events()!
		hwnd := win32_red_hwnd(app, window)!
		before_native := C.v_multiwindow_test_win32_monitor_snapshot_new()
		assert before_native != unsafe { nil }
		defer {
			C.v_multiwindow_test_win32_monitor_snapshot_free(before_native)
		}
		native_count := C.v_multiwindow_test_win32_monitor_snapshot(before_native)
		assert native_count > 0, 'EnumDisplayMonitors oracle admission produced no monitors'
		before_ids := app.service_monitor_ids()!
		mut issues := []string{}
		thread_dpi_contexts_equal := C.v_multiwindow_test_win32_dpi_awareness_contexts_equal(caller_dpi_context_before,
			caller_dpi_context_after)
		win32_red_add(mut issues,
			'high_dpi window creation did not exactly restore the caller thread DPI-awareness context (comparison=${thread_dpi_contexts_equal})',
			thread_dpi_contexts_equal == 1)
		window_dpi_awareness := C.v_multiwindow_test_win32_window_dpi_awareness(hwnd)
		win32_red_add(mut issues,
			'created high_dpi HWND is not per-monitor DPI aware before synthetic WM_DPICHANGED (awareness=${window_dpi_awareness})',
			window_dpi_awareness == 2)
		mut before_by_name := map[string]ServiceMonitorInfo{}
		mut public_primary_count := 0
		for id in before_ids {
			info := app.service_monitor_info(id) or {
				issues << 'public monitor ${id} could not be resolved: ${err.msg()}'
				continue
			}
			win32_red_add(mut issues, 'public monitor ${id} has an empty native name',
				info.name != '')
			win32_red_add(mut issues, 'public monitor ${info.name} is not available',
				info.available)
			win32_red_add(mut issues, 'public monitor ${info.name} geometry is unavailable',
				info.geometry.known)
			if info.geometry.known {
				win32_red_add(mut issues, 'public monitor ${info.name} geometry is non-positive',

					info.geometry.value.width > 0 && info.geometry.value.height > 0)
			}
			win32_red_add(mut issues, 'public monitor ${info.name} work area is unavailable',
				info.work_area.known)
			if info.work_area.known {
				win32_red_add(mut issues, 'public monitor ${info.name} work area is non-positive',

					info.work_area.value.width > 0 && info.work_area.value.height > 0)
			}
			win32_red_add(mut issues, 'public monitor ${info.name} DPI scale is unavailable',

				info.scale.known && info.scale.value > 0)
			win32_red_add(mut issues, 'public monitor ${info.name} primary state is unknown',
				info.primary != .unknown)
			if info.primary == .on {
				public_primary_count++
			}
			if info.name in before_by_name {
				issues << 'public monitor name ${info.name} is not unique'
			} else {
				before_by_name[info.name] = info
			}
		}
		win32_red_add(mut issues, 'public monitor count differs from EnumDisplayMonitors',
			before_ids.len == native_count)
		mut native_names := map[string]bool{}
		mut native_primary_count := 0
		for native_index in 0 .. native_count {
			name_pointer := C.v_multiwindow_test_win32_monitor_name(before_native, native_index)
			if name_pointer == unsafe { nil } {
				issues << 'native monitor ${native_index} has no device name'
				continue
			}
			native_name := unsafe { string_from_wide(name_pointer) }
			if native_name == '' {
				issues << 'native monitor ${native_index} has an empty device name'
				continue
			}
			if native_name in native_names {
				issues << 'native monitor name ${native_name} is not unique'
			}
			native_names[native_name] = true
			mut x := 0
			mut y := 0
			mut width := 0
			mut height := 0
			mut work_x := 0
			mut work_y := 0
			mut work_width := 0
			mut work_height := 0
			mut primary := 0
			assert C.v_multiwindow_test_win32_monitor_info(before_native, native_index, &x, &y,
				&width, &height, &work_x, &work_y, &work_width, &work_height, &primary) == 1
			if primary != 0 {
				native_primary_count++
			}
			if native_name in before_by_name {
				info := before_by_name[native_name]
				win32_red_add(mut issues, 'native monitor ${native_name} is unavailable publicly',
					info.available)
				win32_red_add(mut issues,
					'native monitor ${native_name} geometry differs publicly', info.geometry.known && info.geometry.value == ServiceRect{
					x:      x
					y:      y
					width:  width
					height: height
				})
				win32_red_add(mut issues,
					'native monitor ${native_name} work area differs publicly', info.work_area.known && info.work_area.value == ServiceRect{
					x:      work_x
					y:      work_y
					width:  work_width
					height: work_height
				})
				win32_red_add(mut issues,
					'native monitor ${native_name} primary projection differs publicly', info.primary == if primary != 0 {
					ServiceObservedBool.on
				} else {
					ServiceObservedBool.off
				})
			} else {
				issues << 'native monitor ${native_name} has no matching public snapshot'
			}
		}
		win32_red_add(mut issues, 'EnumDisplayMonitors did not identify exactly one primary',
			native_primary_count == 1)
		win32_red_add(mut issues, 'public snapshot did not identify exactly one primary',
			public_primary_count == 1)

		before_dpi_state := app.service_window_state(window)!
		if before_dpi_state.monitor_ids.len == 0 {
			issues << 'window state has no native monitor membership'
		}
		for monitor_id in before_dpi_state.monitor_ids {
			win32_red_add(mut issues,
				'window state references a monitor outside the public snapshot',
				monitor_id in before_ids)
		}
		assert before_dpi_state.monitor_ids.len > 0
		dpi_monitor := app.service_monitor_info(before_dpi_state.monitor_ids[0])!
		assert dpi_monitor.geometry.known
		assert dpi_monitor.work_area.known
		native_dpi := C.v_multiwindow_test_win32_dpi(hwnd)
		native_scale := f32(native_dpi) / 96.0
		win32_red_add(mut issues, 'window DPI differs from native GetDpiForWindow',
			dpi_monitor.scale.known && dpi_monitor.scale.value > native_scale - 0.01
			&& dpi_monitor.scale.value < native_scale + 0.01)

		mut before_left := 0
		mut before_top := 0
		mut before_right := 0
		mut before_bottom := 0
		assert C.v_multiwindow_test_win32_rect(hwnd, &before_left, &before_top, &before_right,
			&before_bottom) == 1
		geometry := dpi_monitor.geometry.value
		work := dpi_monitor.work_area.value
		allowed_left := if geometry.x > work.x { geometry.x } else { work.x }
		allowed_top := if geometry.y > work.y { geometry.y } else { work.y }
		geometry_right := geometry.x + geometry.width
		work_right := work.x + work.width
		allowed_right := if geometry_right < work_right { geometry_right } else { work_right }
		geometry_bottom := geometry.y + geometry.height
		work_bottom := work.y + work.height
		allowed_bottom := if geometry_bottom < work_bottom {
			geometry_bottom
		} else {
			work_bottom
		}
		allowed_width := allowed_right - allowed_left
		allowed_height := allowed_bottom - allowed_top
		assert allowed_width > 0
		assert allowed_height > 0
		before_width := before_right - before_left
		before_height := before_bottom - before_top
		suggested_width := if before_width < allowed_width { before_width } else { allowed_width }
		suggested_height := if before_height < allowed_height {
			before_height
		} else {
			allowed_height
		}
		suggested_left := allowed_left + (allowed_width - suggested_width) / 2
		suggested_top := allowed_top + (allowed_height - suggested_height) / 2
		suggested := ServiceRect{
			x:      suggested_left
			y:      suggested_top
			width:  suggested_width
			height: suggested_height
		}
		assert win32_red_rect_inside(suggested, geometry)
		assert win32_red_rect_inside(suggested, work)
		_ = app.drain_queued_events()!
		assert C.v_multiwindow_test_win32_emit_dpi_change(hwnd, native_dpi, suggested_left,
			suggested_top, suggested_width, suggested_height) == 1
		win32_red_poll(mut app, 4)!
		mut dpi_left := 0
		mut dpi_top := 0
		mut dpi_right := 0
		mut dpi_bottom := 0
		assert C.v_multiwindow_test_win32_rect(hwnd, &dpi_left, &dpi_top, &dpi_right, &dpi_bottom) == 1
		win32_red_add(mut issues, 'WM_DPICHANGED ignored the suggested RECT',
			dpi_left == suggested_left && dpi_top == suggested_top
			&& dpi_right == suggested_left + suggested_width
			&& dpi_bottom == suggested_top + suggested_height)
		applied := ServiceRect{
			x:      dpi_left
			y:      dpi_top
			width:  dpi_right - dpi_left
			height: dpi_bottom - dpi_top
		}
		win32_red_add(mut issues, 'WM_DPICHANGED applied RECT escaped monitor geometry', win32_red_rect_inside(applied,
			geometry))
		win32_red_add(mut issues, 'WM_DPICHANGED applied RECT escaped monitor work area', win32_red_rect_inside(applied,
			work))
		dpi_events := app.drain_queued_events()!
		dpi_metrics := dpi_events.filter(it.kind == .service && it.service.kind == .metrics
			&& it.service.window == window)
		win32_red_add(mut issues, 'WM_DPICHANGED did not emit exactly one metrics event',
			dpi_metrics.len == 1)
		if dpi_metrics.len == 1 {
			metrics_event := dpi_metrics[0]
			win32_red_add(mut issues, 'WM_DPICHANGED metrics sequence is not canonical',
				metrics_event.sequence == metrics_event.service.sequence
				&& metrics_event.service.sequence == metrics_event.service.metrics.metrics_sequence
				&& metrics_event.service.state.sequence == metrics_event.sequence
				&& metrics_event.sequence > before_dpi_state.sequence)
			win32_red_add(mut issues, 'WM_DPICHANGED metrics DPI differs from GetDpiForWindow',
				metrics_event.service.metrics.dpi_scale > native_scale - 0.01
				&& metrics_event.service.metrics.dpi_scale < native_scale + 0.01)
			win32_red_add(mut issues, 'same-DPI WM_DPICHANGED changed event monitor membership',
				metrics_event.service.state.monitor_ids == before_dpi_state.monitor_ids)
		}
		after_dpi_state := app.service_window_state(window)!
		win32_red_add(mut issues, 'same-DPI WM_DPICHANGED changed window monitor membership',
			after_dpi_state.monitor_ids == before_dpi_state.monitor_ids)

		_ = app.drain_queued_events()!
		assert C.v_multiwindow_test_win32_emit_display_changes(hwnd, 3) == 1
		win32_red_poll(mut app, 4)!
		after_ids := app.service_monitor_ids()!
		display_events := app.drain_queued_events()!
		display_monitors :=
			display_events.filter(it.kind == .service && it.service.kind == .monitor)
		display_metrics := display_events.filter(it.kind == .service && it.service.kind == .metrics
			&& it.service.window == window)
		win32_red_add(mut issues, 'WM_DISPLAYCHANGE burst was not coalesced to one monitor event',
			display_monitors.len == 1)
		win32_red_add(mut issues, 'WM_DISPLAYCHANGE burst did not emit one window metrics event',
			display_metrics.len == 1)
		if display_monitors.len == 1 {
			monitor_event := display_monitors[0]
			mut monitor_sequences_match := monitor_event.sequence == monitor_event.service.sequence
			for monitor in monitor_event.service.monitors {
				monitor_sequences_match = monitor_sequences_match
					&& monitor.sequence == monitor_event.sequence
			}
			win32_red_add(mut issues, 'WM_DISPLAYCHANGE monitor sequence is not canonical',
				monitor_sequences_match)
		}
		if display_metrics.len == 1 {
			metrics_event := display_metrics[0]
			win32_red_add(mut issues, 'WM_DISPLAYCHANGE metrics sequence is not canonical',
				metrics_event.sequence == metrics_event.service.sequence
				&& metrics_event.service.sequence == metrics_event.service.metrics.metrics_sequence
				&& metrics_event.service.state.sequence == metrics_event.sequence)
		}
		if display_monitors.len == 1 && display_metrics.len == 1 {
			win32_red_add(mut issues, 'WM_DISPLAYCHANGE metrics preceded the monitor snapshot',
				display_monitors[0].sequence < display_metrics[0].sequence)
		}
		after_native := C.v_multiwindow_test_win32_monitor_snapshot_new()
		assert after_native != unsafe { nil }
		defer {
			C.v_multiwindow_test_win32_monitor_snapshot_free(after_native)
		}
		after_count := C.v_multiwindow_test_win32_monitor_snapshot(after_native)
		assert after_count > 0, 'post-WM_DISPLAYCHANGE monitor oracle produced no monitors'
		mut after_native_names := map[string]bool{}
		for native_index in 0 .. after_count {
			name_pointer := C.v_multiwindow_test_win32_monitor_name(after_native, native_index)
			if name_pointer != unsafe { nil } {
				after_native_names[unsafe { string_from_wide(name_pointer) }] = true
			}
		}
		if after_count == native_count {
			mut same_names := after_native_names.len == native_names.len
			for name, _ in native_names {
				if name !in after_native_names {
					same_names = false
					break
				}
			}
			if same_names {
				mut after_by_name := map[string]ServiceMonitorId{}
				for id in after_ids {
					info := app.service_monitor_info(id) or {
						issues << 'post-display monitor ${id} could not be resolved: ${err.msg()}'
						continue
					}
					after_by_name[info.name] = info.id
				}
				for name, info in before_by_name {
					win32_red_add(mut issues,
						'stable monitor ${name} changed identity after WM_DISPLAYCHANGE',

						name in after_by_name && after_by_name[name] == info.id)
				}
			}
		}

		mut replug_snapshot := []ServiceMonitorInfo{cap: after_ids.len}
		for id in after_ids {
			info := app.service_monitor_info(id) or {
				issues << 'monitor ${id} could not be saved for replug: ${err.msg()}'
				continue
			}
			replug_snapshot << info
		}
		if replug_snapshot.len == 0 {
			issues << 'no public monitor snapshot is available for generation-cycle coverage'
		} else {
			stale_target := replug_snapshot[0]
			captured_count := C.v_multiwindow_test_win32_monitor_enumeration_capture()
			win32_red_add(mut issues, 'native monitor fixture capture changed monitor count',
				captured_count == after_count)
			defer {
				C.v_multiwindow_test_win32_monitor_enumeration_reset()
			}
			growth_backend_before := app.backend.win32.service_monitors.clone()
			growth_registry_before := app.services.monitors.clone()
			growth_ids_before := app.service_monitor_ids()!
			_ = app.drain_queued_events()!
			assert C.v_multiwindow_test_win32_monitor_enumeration_use_growth(33) == 1
			grown_raw := win32_service_raw_monitor_snapshot() or {
				issues << '33-monitor snapshot growth failed: ${err.msg()}'
				[]Win32ServiceRawMonitor{}
			}
			win32_red_add(mut issues, 'monitor growth seam was not consumed exactly once',
				C.v_multiwindow_test_win32_monitor_enumeration_growth_calls() == 1)
			win32_red_add(mut issues, 'monitor growth seam did not deliver all 33 callbacks',
				C.v_multiwindow_test_win32_monitor_enumeration_growth_callbacks() == 33)
			win32_red_add(mut issues, 'monitor snapshot truncated growth beyond 32',
				grown_raw.len == 33)
			mut grown_names := map[string]bool{}
			for monitor in grown_raw {
				grown_names[monitor.name] = true
			}
			win32_red_add(mut issues, 'grown monitor snapshot did not retain 33 unique entries',
				grown_names.len == 33)
			win32_red_add(mut issues, 'raw snapshot growth mutated backend monitor generations',
				app.backend.win32.service_monitors == growth_backend_before)
			win32_red_add(mut issues, 'raw snapshot growth mutated the public monitor registry',
				app.services.monitors == growth_registry_before
				&& app.service_monitor_ids()! == growth_ids_before)
			growth_delivery := app.drain_queued_events()!
			win32_red_add(mut issues, 'raw snapshot growth published a monitor event', growth_delivery.all(
				it.kind != .service || it.service.kind != .monitor))

			refresh_backend_before := app.backend.win32.service_monitors.clone()
			refresh_registry_before := app.services.monitors.clone()
			refresh_ids_before := app.service_monitor_ids()!
			refresh_state_before := app.service_window_state(window)!
			refresh_index := app.backend.win32.window_record_index(window) or {
				assert false, 'W3 refresh target has no Win32 record'
				0
			}
			refresh_record := app.backend.win32.windows[refresh_index]
			C.v_multiwindow_test_win32_monitor_enumeration_use_empty()
			_ = app.drain_queued_events()!
			assert C.v_multiwindow_test_win32_emit_display_change(hwnd) == 1
			pending_sequence := refresh_record.service_refresh_sequence
			win32_red_add(mut issues, 'WM_DISPLAYCHANGE did not arm a pending refresh',

				refresh_record.pending_display_refresh && pending_sequence != 0)
			replacement_data := unsafe { voidptr(&app.backend.win32) }
			expected_data := unsafe { voidptr(refresh_record) }
			original_data := C.v_multiwindow_test_win32_swap_user_data(hwnd, replacement_data)
			mut observation_error := ''
			if _ := app.backend.win32.collect_service_refresh_events() {
				issues << 'injected display metrics observation failure unexpectedly succeeded'
			} else {
				observation_error = err.msg()
			}
			replaced_data := C.v_multiwindow_test_win32_swap_user_data(hwnd, original_data)
			win32_red_add(mut issues, 'display metrics fault did not reach the native authority',
				observation_error != '')
			win32_red_add(mut issues, 'display metrics fault did not restore GWLP_USERDATA',

				original_data == expected_data && replaced_data == replacement_data)
			win32_red_add(mut issues,
				'failed display observation mutated backend monitor generations/availability',
				app.backend.win32.service_monitors == refresh_backend_before)
			win32_red_add(mut issues, 'failed display observation mutated the public registry',
				app.services.monitors == refresh_registry_before
				&& app.service_monitor_ids()! == refresh_ids_before)
			refresh_after_failure := app.backend.win32.windows[refresh_index]
			win32_red_add(mut issues, 'failed display observation consumed pending retry state',
				refresh_after_failure.pending_display_refresh
				&& refresh_after_failure.service_refresh_sequence == pending_sequence)
			state_after_failure := app.service_window_state(window)!
			win32_red_add(mut issues, 'failed display observation changed window membership',
				state_after_failure.monitor_ids == refresh_state_before.monitor_ids)
			failed_delivery := app.drain_queued_events()!
			win32_red_add(mut issues,
				'failed display observation published a partial monitor/metrics batch', failed_delivery.all(
				it.kind != .service
				|| (it.service.kind != .monitor && it.service.kind != .metrics)))
			win32_red_poll(mut app, 4)!
			win32_red_add(mut issues, 'Win32 service did not consume empty enumeration seam',
				C.v_multiwindow_test_win32_monitor_enumeration_empty_calls() > 0)
			empty_delivery := app.drain_queued_events()!
			empty_monitors := empty_delivery.filter(it.kind == .service
				&& it.service.kind == .monitor)
			empty_metrics := empty_delivery.filter(it.kind == .service
				&& it.service.kind == .metrics && it.service.window == window)
			win32_red_add(mut issues, 'native empty enumeration was not delivered exactly once',

				empty_monitors.len == 1 && empty_monitors[0].service.monitors.len == 0)
			win32_red_add(mut issues, 'unplug metrics payload retained window monitor membership',
				empty_metrics.len == 1 && empty_metrics[0].service.state.monitor_ids.len == 0
				&& empty_metrics[0].service.state.monitor_membership_observed)
			win32_red_add(mut issues, 'unplug snapshot left available monitor ids',
				app.service_monitor_ids()!.len == 0)
			unplug_state := app.service_window_state(window)!
			win32_red_add(mut issues, 'unplug snapshot left window monitor membership',

				unplug_state.monitor_ids.len == 0 && unplug_state.monitor_membership_observed)
			unplugged := app.service_monitor_info(stale_target.id) or {
				issues << 'unplugged monitor id could not expose unavailable state: ${err.msg()}'
				ServiceMonitorInfo{}
			}
			win32_red_add(mut issues, 'unplugged monitor did not become unavailable',

				unplugged.id == stale_target.id && !unplugged.available)

			assert C.v_multiwindow_test_win32_monitor_enumeration_use_replay() == 1
			_ = app.drain_queued_events()!
			assert C.v_multiwindow_test_win32_emit_display_change(hwnd) == 1
			win32_red_poll(mut app, 4)!
			win32_red_add(mut issues, 'Win32 service did not consume replay enumeration seam',
				C.v_multiwindow_test_win32_monitor_enumeration_replay_calls() > 0)
			replug_delivery := app.drain_queued_events()!
			replug_monitors := replug_delivery.filter(it.kind == .service
				&& it.service.kind == .monitor)
			replug_metrics := replug_delivery.filter(it.kind == .service
				&& it.service.kind == .metrics && it.service.window == window)
			win32_red_add(mut issues, 'native replay enumeration was not delivered exactly once',
				replug_monitors.len == 1
				&& replug_monitors[0].service.monitors.len == replug_snapshot.len)
			replugged_ids := app.service_monitor_ids()!
			mut replacement := ServiceMonitorInfo{}
			mut replacement_found := false
			for id in replugged_ids {
				info := app.service_monitor_info(id) or {
					issues << 'replugged monitor ${id} could not be resolved: ${err.msg()}'
					continue
				}
				if info.name == stale_target.name {
					replacement = info
					replacement_found = true
					break
				}
			}
			win32_red_add(mut issues, 'replugged monitor name did not reappear', replacement_found)
			if replacement_found {
				win32_red_add(mut issues, 'replug retained the stale opaque monitor id',

					replacement.id != stale_target.id && stale_target.id !in replugged_ids)
				win32_red_add(mut issues, 'replugged monitor did not preserve its slot',
					replacement.id.slot_for_gg() == stale_target.id.slot_for_gg())
				win32_red_add(mut issues, 'replugged monitor generation did not advance once', replacement.id.generation_for_gg() ==
					stale_target.id.generation_for_gg() + 1)
				win32_red_add(mut issues, 'replugged monitor is not available',
					replacement.available)
			}
			replug_state := app.service_window_state(window)!
			win32_red_add(mut issues, 'replugged window retained the stale monitor id',
				stale_target.id !in replug_state.monitor_ids)
			if replacement_found {
				win32_red_add(mut issues,
					'replugged window state does not contain the replacement monitor id',
					replacement.id in replug_state.monitor_ids)
				win32_red_add(mut issues,
					'replug metrics payload does not contain the replacement monitor id',
					replug_metrics.len == 1
					&& replacement.id in replug_metrics[0].service.state.monitor_ids)
			}
			win32_red_add(mut issues, 'replug metrics payload retained the stale monitor id',
				replug_metrics.len == 1
				&& stale_target.id !in replug_metrics[0].service.state.monitor_ids)
			mut stale_id_rejected := false
			_ = app.service_monitor_info(stale_target.id) or {
				stale_id_rejected = err.msg() == err_service_request_stale
				ServiceMonitorInfo{}
			}
			win32_red_add(mut issues,
				'public service_monitor_info accepted the pre-unplug opaque id', stale_id_rejected)

			info_failure_backend_before := app.backend.win32.service_monitors.clone()
			info_failure_registry_before := app.services.monitors.clone()
			info_failure_ids_before := app.service_monitor_ids()!
			info_failure_state_before := app.service_window_state(window)!
			_ = app.drain_queued_events()!
			assert C.v_multiwindow_test_win32_monitor_enumeration_use_info_failure() == 1
			assert C.v_multiwindow_test_win32_emit_display_change(hwnd) == 1
			info_failure_record := app.backend.win32.windows[refresh_index]
			info_failure_sequence := info_failure_record.service_refresh_sequence
			mut snapshot_error := ''
			if _ := app.poll_events() {
				issues << 'injected GetMonitorInfoW failure unexpectedly published a snapshot'
			} else {
				snapshot_error = err.msg()
			}
			win32_red_add(mut issues, 'GetMonitorInfoW fault did not fail the whole snapshot',
				snapshot_error != ''
				&& C.v_multiwindow_test_win32_monitor_enumeration_info_failure_calls() == 1)
			win32_red_add(mut issues,
				'GetMonitorInfoW failure mutated backend generations/availability',
				app.backend.win32.service_monitors == info_failure_backend_before)
			win32_red_add(mut issues, 'GetMonitorInfoW failure mutated the public registry',
				app.services.monitors == info_failure_registry_before
				&& app.service_monitor_ids()! == info_failure_ids_before)
			info_failure_after := app.backend.win32.windows[refresh_index]
			win32_red_add(mut issues, 'GetMonitorInfoW failure consumed pending retry state',
				info_failure_after.pending_display_refresh
				&& info_failure_after.service_refresh_sequence == info_failure_sequence)
			info_failure_state_after := app.service_window_state(window)!
			win32_red_add(mut issues,
				'GetMonitorInfoW failure falsely unplugged window membership',
				info_failure_state_after.monitor_ids == info_failure_state_before.monitor_ids)
			info_failure_delivery := app.drain_queued_events()!
			win32_red_add(mut issues,
				'GetMonitorInfoW failure published a partial unplug/metrics batch', info_failure_delivery.all(
				it.kind != .service
				|| (it.service.kind != .monitor && it.service.kind != .metrics)))

			replay_calls_before_retry :=
				C.v_multiwindow_test_win32_monitor_enumeration_replay_calls()
			assert C.v_multiwindow_test_win32_monitor_enumeration_use_replay() == 1
			win32_red_poll(mut app, 4)!
			win32_red_add(mut issues, 'failed monitor snapshot was not retried',
				C.v_multiwindow_test_win32_monitor_enumeration_replay_calls() > replay_calls_before_retry)
			info_retry_delivery := app.drain_queued_events()!
			info_retry_monitors := info_retry_delivery.filter(it.kind == .service
				&& it.service.kind == .monitor)
			info_retry_metrics := info_retry_delivery.filter(it.kind == .service
				&& it.service.kind == .metrics && it.service.window == window)
			win32_red_add(mut issues,
				'GetMonitorInfoW retry did not publish one complete monitor/metrics batch',

				info_retry_monitors.len == 1 && info_retry_metrics.len == 1)
			win32_red_add(mut issues, 'GetMonitorInfoW retry changed stable monitor ids',
				app.service_monitor_ids()! == info_failure_ids_before)
			info_retry_state := app.service_window_state(window)!
			win32_red_add(mut issues,
				'GetMonitorInfoW retry changed stable window monitor membership',
				info_retry_state.monitor_ids == info_failure_state_before.monitor_ids)
		}

		app.destroy_window(window)!
		_ = app.drain_queued_events()!
		win32_red_add(mut issues, 'zero-window fixture retained a backend HWND record',
			app.backend.win32.windows.len == 0)
		captured_zero_window := C.v_multiwindow_test_win32_monitor_enumeration_capture()
		win32_red_add(mut issues, 'zero-window fixture could not capture native monitors',
			captured_zero_window > 0)
		zero_ids_before_change := app.service_monitor_ids()!
		zero_info_before_change := app.service_monitor_info(zero_ids_before_change[0])!
		assert C.v_multiwindow_test_win32_monitor_enumeration_use_changed() == 1
		_ = app.poll_events()!
		zero_changed_delivery := app.drain_queued_events()!
		zero_changed_monitors := zero_changed_delivery.filter(it.kind == .service
			&& it.service.kind == .monitor)
		zero_ids_after_change := app.service_monitor_ids()!
		zero_info_after_change := app.service_monitor_info(zero_ids_after_change[0])!
		win32_red_add(mut issues,
			'zero-window raw geometry/workarea/DPI/primary change did not publish once',
			zero_changed_monitors.len == 1 && zero_ids_after_change == zero_ids_before_change
			&& zero_info_after_change.geometry != zero_info_before_change.geometry
			&& zero_info_after_change.work_area != zero_info_before_change.work_area
			&& zero_info_after_change.scale != zero_info_before_change.scale
			&& zero_info_after_change.primary != zero_info_before_change.primary)
		_ = app.poll_events()!
		zero_changed_no_spam := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .monitor)
		win32_red_add(mut issues, 'unchanged zero-window raw snapshot emitted duplicate events',
			zero_changed_no_spam.len == 0)

		C.v_multiwindow_test_win32_monitor_enumeration_use_empty()
		_ = app.poll_events()!
		zero_delivery := app.drain_queued_events()!
		zero_monitors := zero_delivery.filter(it.kind == .service && it.service.kind == .monitor)
		win32_red_add(mut issues, 'zero-window polling did not publish one unplug snapshot',
			zero_monitors.len == 1 && zero_monitors[0].service.monitors.len == 0
			&& app.service_monitor_ids()!.len == 0)
		zero_backend_before_failure := app.backend.win32.service_monitors.clone()
		zero_raw_before_failure := app.backend.win32.service_monitor_raw.clone()
		zero_registry_before_failure := app.services.monitors.clone()
		assert C.v_multiwindow_test_win32_monitor_enumeration_use_info_failure() == 1
		mut zero_snapshot_error := ''
		app.poll_events() or { zero_snapshot_error = err.msg() }
		win32_red_add(mut issues, 'zero-window monitor failure was not surfaced',
			zero_snapshot_error != '')
		win32_red_add(mut issues, 'zero-window monitor failure consumed dirty retry state',
			app.backend.win32.service_monitor_poll_dirty)
		win32_red_add(mut issues, 'zero-window monitor failure mutated backend state',
			app.backend.win32.service_monitors == zero_backend_before_failure
			&& win32_service_raw_monitor_snapshots_equal(app.backend.win32.service_monitor_raw, zero_raw_before_failure))
		win32_red_add(mut issues, 'zero-window monitor failure mutated public state',
			app.services.monitors == zero_registry_before_failure
			&& app.service_monitor_ids()!.len == 0)

		assert C.v_multiwindow_test_win32_monitor_enumeration_use_replay() == 1
		_ = app.poll_events()!
		zero_retry_delivery := app.drain_queued_events()!
		zero_retry_monitors := zero_retry_delivery.filter(it.kind == .service
			&& it.service.kind == .monitor)
		win32_red_add(mut issues, 'zero-window retry did not publish one complete snapshot',
			zero_retry_monitors.len == 1 && app.service_monitor_ids()!.len == captured_zero_window
			&& !app.backend.win32.service_monitor_poll_dirty)
		_ = app.poll_events()!
		zero_no_spam := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.kind == .monitor)
		win32_red_add(mut issues, 'unchanged zero-window snapshot emitted duplicate events',
			zero_no_spam.len == 0)

		C.v_multiwindow_test_win32_monitor_enumeration_use_empty()
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		assert app.service_monitor_ids()!.len == 0
		assert C.v_multiwindow_test_win32_monitor_enumeration_use_replay() == 1
		precreate_backend_before := app.backend.win32.service_monitors.clone()
		precreate_raw_before := app.backend.win32.service_monitor_raw.clone()
		app.backend.win32.refresh_service_monitors_before_first_window()!
		reverted_sequence := app.backend.win32.service_monitor_pending_sequence
		reverted_pending_raw := app.backend.win32.service_monitor_pending_raw.clone()
		app.backend.win32.refresh_service_monitors_before_first_window()!
		win32_red_add(mut issues, 'repeated pre-create refresh replaced the earliest sequence',
			reverted_sequence != 0
			&& app.backend.win32.service_monitor_pending_sequence == reverted_sequence
			&& win32_service_raw_monitor_snapshots_equal(app.backend.win32.service_monitor_pending_raw, reverted_pending_raw))
		C.v_multiwindow_test_win32_monitor_enumeration_use_empty()
		app.backend.win32.refresh_service_monitors_before_first_window()!
		win32_red_add(mut issues, 'pre-create refresh revert retained a duplicate staged plan',
			app.backend.win32.service_monitor_pending_sequence == 0
			&& app.backend.win32.service_monitor_pending.len == 0
			&& app.backend.win32.service_monitor_pending_records.len == 0
			&& app.backend.win32.service_monitor_pending_raw.len == 0
			&& app.backend.win32.service_monitors == precreate_backend_before
			&& win32_service_raw_monitor_snapshots_equal(app.backend.win32.service_monitor_raw, precreate_raw_before)
			&& !app.backend.win32.service_monitor_poll_dirty)
		precreate_revert_events := app.drain_queued_events()!.filter(it.kind == .service
			&& (it.service.kind == .monitor || it.service.kind == .metrics))
		win32_red_add(mut issues, 'pre-create refresh revert emitted a duplicate event',
			precreate_revert_events.len == 0)
		assert C.v_multiwindow_test_win32_monitor_enumeration_use_replay() == 1
		app.backend.win32.refresh_service_monitors_before_first_window()!
		precreate_sequence := app.backend.win32.service_monitor_pending_sequence
		precreate_pending_before := app.backend.win32.service_monitor_pending.clone()
		precreate_pending_records_before :=
			app.backend.win32.service_monitor_pending_records.clone()
		precreate_pending_raw_before := app.backend.win32.service_monitor_pending_raw.clone()
		app.backend.win32.refresh_service_monitors_before_first_window()!
		win32_red_add(mut issues, 'restaged pre-create refresh replaced the earliest sequence',
			precreate_sequence != 0
			&& app.backend.win32.service_monitor_pending_sequence == precreate_sequence)
		win32_red_add(mut issues, 'restaged pre-create refresh changed the staged snapshot',
			app.backend.win32.service_monitor_pending == precreate_pending_before
			&& app.backend.win32.service_monitor_pending_records == precreate_pending_records_before
			&& win32_service_raw_monitor_snapshots_equal(app.backend.win32.service_monitor_pending_raw, precreate_pending_raw_before))
		recreated := app.create_window(title: 'Win32 monitor pre-create refresh')!
		recreated_index := app.backend.win32.window_record_index(recreated) or {
			assert false, 'pre-create refresh window has no backend record'
			0
		}
		win32_red_add(mut issues, 'pre-create refresh mutated the published backend snapshot',
			app.backend.win32.service_monitors == precreate_backend_before
			&& win32_service_raw_monitor_snapshots_equal(app.backend.win32.service_monitor_raw, precreate_raw_before))
		win32_red_add(mut issues, 'pre-create refresh did not retain the complete staged plan',
			app.backend.win32.service_monitor_pending_records.any(it.available)
			&& app.backend.win32.service_monitor_pending_raw.len > 0)
		win32_red_add(mut issues, 'pre-create refresh published before native polling',
			app.service_monitor_ids()!.len == 0
			&& app.backend.win32.service_monitor_pending_sequence == precreate_sequence)
		precreate_state := app.service_window_state(recreated)!
		win32_red_add(mut issues, 'pre-create state exposed unresolved monitor membership',
			win32_red_monitor_membership_is_public(app, precreate_state)
			&& precreate_state.monitor_ids.len == 0
			&& app.backend.win32.windows[recreated_index].service_monitor_ids.len == 0)
		assert C.v_multiwindow_test_win32_monitor_enumeration_use_info_failure() == 1
		mut precreate_snapshot_error := ''
		if _ := app.backend.win32.collect_service_refresh_events() {
			issues << 'pre-create staged snapshot failure unexpectedly succeeded'
		} else {
			precreate_snapshot_error = err.msg()
		}
		win32_red_add(mut issues, 'pre-create staged snapshot failure was not surfaced',
			precreate_snapshot_error != '')
		win32_red_add(mut issues, 'failed staged snapshot consumed pending retry state',
			app.backend.win32.service_monitor_pending_sequence == precreate_sequence
			&& app.backend.win32.service_monitor_pending == precreate_pending_before
			&& app.backend.win32.service_monitor_pending_records == precreate_pending_records_before
			&& win32_service_raw_monitor_snapshots_equal(app.backend.win32.service_monitor_pending_raw, precreate_pending_raw_before)
			&& app.backend.win32.service_monitors == precreate_backend_before
			&& win32_service_raw_monitor_snapshots_equal(app.backend.win32.service_monitor_raw, precreate_raw_before)
			&& app.backend.win32.service_monitor_poll_dirty)
		assert C.v_multiwindow_test_win32_monitor_enumeration_use_replay() == 1

		precreate_record := app.backend.win32.windows[recreated_index]
		precreate_replacement_data := unsafe { voidptr(&app.backend.win32) }
		precreate_expected_data := unsafe { voidptr(precreate_record) }
		precreate_original_data := C.v_multiwindow_test_win32_swap_user_data(precreate_record.hwnd,
			precreate_replacement_data)
		mut precreate_observation_error := ''
		if _ := app.backend.win32.collect_service_refresh_events() {
			issues << 'pre-create staged observation failure unexpectedly succeeded'
		} else {
			precreate_observation_error = err.msg()
		}
		precreate_replaced_data := C.v_multiwindow_test_win32_swap_user_data(precreate_record.hwnd,
			precreate_original_data)
		win32_red_add(mut issues,
			'pre-create staged observation fault did not reach native authority',
			precreate_observation_error != '' && precreate_original_data == precreate_expected_data
			&& precreate_replaced_data == precreate_replacement_data)
		win32_red_add(mut issues, 'failed staged observation mutated published backend state',
			app.backend.win32.service_monitors == precreate_backend_before
			&& win32_service_raw_monitor_snapshots_equal(app.backend.win32.service_monitor_raw, precreate_raw_before))
		win32_red_add(mut issues, 'failed staged observation consumed pending retry state',
			app.backend.win32.service_monitor_pending_sequence == precreate_sequence
			&& app.backend.win32.service_monitor_pending == precreate_pending_before
			&& app.backend.win32.service_monitor_pending_records == precreate_pending_records_before
			&& win32_service_raw_monitor_snapshots_equal(app.backend.win32.service_monitor_pending_raw, precreate_pending_raw_before)
			&& app.backend.win32.service_monitor_poll_dirty)
		precreate_state_after_failure := app.service_window_state(recreated)!
		win32_red_add(mut issues, 'failed staged observation exposed unresolved membership',
			win32_red_monitor_membership_is_public(app, precreate_state_after_failure)
			&& precreate_state_after_failure.monitor_ids == precreate_state.monitor_ids)
		assert C.v_multiwindow_test_win32_monitor_enumeration_use_changed() == 1
		assert C.v_multiwindow_test_win32_emit_display_change(precreate_record.hwnd) == 1
		precreate_interleave_sequence := precreate_record.service_refresh_sequence
		win32_red_add(mut issues, 'post-stage display refresh did not retain a later sequence',
			precreate_record.pending_display_refresh
			&& precreate_interleave_sequence > precreate_sequence)
		_ = app.poll_events()!
		precreate_delivery := app.drain_queued_events()!
		precreate_monitors := precreate_delivery.filter(it.kind == .service
			&& it.service.kind == .monitor)
		precreate_metrics := precreate_delivery.filter(it.kind == .service
			&& it.service.kind == .metrics && it.service.window == recreated)
		mut precreate_monitor_position := -1
		mut precreate_metrics_position := -1
		for index, event in precreate_delivery {
			if event.kind == .service && event.service.kind == .monitor {
				precreate_monitor_position = index
			} else if event.kind == .service && event.service.kind == .metrics
				&& event.service.window == recreated {
				precreate_metrics_position = index
			}
		}
		win32_red_add(mut issues, 'pre-create refresh did not publish exactly once',
			precreate_monitors.len == 1 && app.service_monitor_ids()!.len == captured_zero_window
			&& app.backend.win32.service_monitor_pending_sequence == 0
			&& app.backend.win32.service_monitor_pending.len == 0
			&& app.backend.win32.service_monitor_pending_records.len == 0
			&& app.backend.win32.service_monitor_pending_raw.len == 0)
		win32_red_add(mut issues, 'pre-create monitor and metrics did not publish atomically',
			precreate_metrics.len == 1 && precreate_monitor_position >= 0
			&& precreate_metrics_position > precreate_monitor_position
			&& precreate_monitors[0].service.sequence < precreate_metrics[0].service.sequence
			&& precreate_pending_before.len > 0
			&& precreate_monitors[0].service.monitors.len == precreate_pending_before.len
			&& precreate_monitors[0].service.monitors[0].geometry != precreate_pending_before[0].geometry)
		precreate_record_after_stage := app.backend.win32.windows[recreated_index]
		win32_red_add(mut issues, 'coalesced staged commit retained display refresh debt',
			!precreate_record_after_stage.pending_display_refresh
			&& precreate_record_after_stage.service_refresh_sequence == 0)
		precreate_state_after_poll := app.service_window_state(recreated)!
		win32_red_add(mut issues, 'pre-create poll did not publish resolvable fresh membership',
			precreate_state_after_poll.monitor_ids.len > 0
			&& win32_red_monitor_membership_is_public(app, precreate_state_after_poll)
			&& precreate_state_after_poll.monitor_ids == app.backend.win32.windows[recreated_index].service_monitor_ids)
		changed_info := app.service_monitor_info(app.service_monitor_ids()![0])!
		win32_red_add(mut issues, 'later display refresh was not coalesced into the first batch',
			precreate_pending_before.len > 0
			&& changed_info.geometry != precreate_pending_before[0].geometry
			&& changed_info.work_area != precreate_pending_before[0].work_area
			&& changed_info.scale != precreate_pending_before[0].scale
			&& changed_info.primary != precreate_pending_before[0].primary)
		_ = app.poll_events()!
		precreate_no_spam := app.drain_queued_events()!.filter(it.kind == .service
			&& (it.service.kind == .monitor || (it.service.kind == .metrics
			&& it.service.window == recreated)))
		win32_red_add(mut issues,
			'unchanged pre-create snapshot emitted duplicate monitor/metrics events',
			precreate_no_spam.len == 0)

		app.destroy_window(recreated)!
		_ = app.drain_queued_events()!
		win32_red_add(mut issues, 'net-zero fixture retained a backend HWND record',
			app.backend.win32.windows.len == 0)
		assert C.v_multiwindow_test_win32_monitor_enumeration_use_replay() == 1
		app.backend.win32.refresh_service_monitors_before_first_window()!
		net_zero_sequence := app.backend.win32.service_monitor_pending_sequence
		net_zero_window := app.create_window(title: 'Win32 staged monitor net-zero')!
		net_zero_index := app.backend.win32.window_record_index(net_zero_window) or {
			assert false, 'net-zero monitor window has no backend record'
			0
		}
		_ = app.drain_queued_events()!
		mut net_zero_record := app.backend.win32.windows[net_zero_index]
		assert C.v_multiwindow_test_win32_monitor_enumeration_use_changed() == 1
		assert C.v_multiwindow_test_win32_emit_display_change(net_zero_record.hwnd) == 1
		net_zero_refresh_sequence := net_zero_record.service_refresh_sequence
		win32_window_service_refresh(voidptr(net_zero_record), net_zero_refresh_sequence, 2)
		net_zero_later_sequence := C.v_multiwindow_win32_next_event_sequence()
		net_zero_record.enqueue_native_event(net_zero_later_sequence, queued_lifecycle_event(Event{
			kind:      .window_close_requested
			window_id: net_zero_window
		}))
		win32_red_add(mut issues, 'net-zero fixture did not preserve native sequence order',
			net_zero_sequence != 0 && net_zero_refresh_sequence > net_zero_sequence
			&& net_zero_later_sequence > net_zero_refresh_sequence
			&& net_zero_record.pending_display_refresh && net_zero_record.pending_dpi_refresh)
		net_zero_replacement_data := unsafe { voidptr(&app.backend.win32) }
		net_zero_expected_data := unsafe { voidptr(net_zero_record) }
		net_zero_original_data := C.v_multiwindow_test_win32_swap_user_data(net_zero_record.hwnd,
			net_zero_replacement_data)
		mut net_zero_observation_error := ''
		if _ := app.backend.win32.collect_service_refresh_events() {
			issues << 'net-zero staged observation failure unexpectedly succeeded'
		} else {
			net_zero_observation_error = err.msg()
		}
		net_zero_replaced_data := C.v_multiwindow_test_win32_swap_user_data(net_zero_record.hwnd,
			net_zero_original_data)
		win32_red_add(mut issues, 'net-zero observation fault did not reach native authority',
			net_zero_observation_error != '' && net_zero_original_data == net_zero_expected_data
			&& net_zero_replaced_data == net_zero_replacement_data)
		net_zero_record_after_failure := app.backend.win32.windows[net_zero_index]
		win32_red_add(mut issues, 'net-zero observation fault consumed retry authority',
			app.backend.win32.service_monitor_pending_sequence == net_zero_sequence
			&& app.backend.win32.service_monitor_pending.len > 0
			&& app.backend.win32.service_monitor_pending_records.len > 0
			&& app.backend.win32.service_monitor_pending_raw.len > 0
			&& net_zero_record_after_failure.pending_display_refresh
			&& net_zero_record_after_failure.pending_dpi_refresh
			&& net_zero_record_after_failure.service_refresh_sequence == net_zero_refresh_sequence
			&& app.backend.win32.service_monitor_poll_dirty)
		net_zero_failure_delivery := app.drain_queued_events()!.filter(it.kind == .service
			&& (it.service.kind == .monitor || (it.service.kind == .metrics
			&& it.service.window == net_zero_window)))
		win32_red_add(mut issues, 'net-zero observation fault published a partial batch',
			net_zero_failure_delivery.len == 0)
		_ = app.poll_events()!
		net_zero_delivery := app.drain_queued_events()!
		net_zero_monitors := net_zero_delivery.filter(it.kind == .service
			&& it.service.kind == .monitor)
		net_zero_metrics := net_zero_delivery.filter(it.kind == .service
			&& it.service.kind == .metrics && it.service.window == net_zero_window)
		net_zero_later := net_zero_delivery.filter(it.kind == .lifecycle
			&& it.lifecycle.kind == .window_close_requested
			&& it.lifecycle.window_id == net_zero_window)
		mut net_zero_metrics_position := -1
		mut net_zero_later_position := -1
		for index, event in net_zero_delivery {
			if event.kind == .service && event.service.kind == .metrics
				&& event.service.window == net_zero_window {
				net_zero_metrics_position = index
			} else if event.kind == .lifecycle && event.lifecycle.kind == .window_close_requested
				&& event.lifecycle.window_id == net_zero_window {
				net_zero_later_position = index
			}
		}
		win32_red_add(mut issues,
			'net-zero staged refresh emitted a monitor event or lost the metrics debt',
			net_zero_monitors.len == 0 && net_zero_metrics.len == 1 && net_zero_later.len == 1
			&& net_zero_metrics_position >= 0 && net_zero_later_position > net_zero_metrics_position)
		net_zero_record_after_poll := app.backend.win32.windows[net_zero_index]
		win32_red_add(mut issues, 'net-zero staged refresh retained native refresh debt',
			app.backend.win32.service_monitor_pending_sequence == 0
			&& app.backend.win32.service_monitor_pending.len == 0
			&& app.backend.win32.service_monitor_pending_records.len == 0
			&& app.backend.win32.service_monitor_pending_raw.len == 0
			&& !net_zero_record_after_poll.pending_display_refresh
			&& !net_zero_record_after_poll.pending_dpi_refresh
			&& !net_zero_record_after_poll.pending_membership_refresh
			&& net_zero_record_after_poll.service_refresh_sequence == 0)
		net_zero_state := app.service_window_state(net_zero_window)!
		win32_red_add(mut issues, 'net-zero refresh exposed unresolved monitor membership', win32_red_monitor_membership_is_public(app,
			net_zero_state))
		_ = app.poll_events()!
		net_zero_no_spam := app.drain_queued_events()!.filter(it.kind == .service
			&& (it.service.kind == .monitor || (it.service.kind == .metrics
			&& it.service.window == net_zero_window)))
		win32_red_add(mut issues, 'net-zero staged refresh emitted duplicate service events',
			net_zero_no_spam.len == 0)
		if issues.len > 0 {
			eprintln('PACKAGE2_RED_TERMINAL=behavioral_red:monitor_dpi_hotplug')
		}
		assert issues.len == 0, 'Win32 monitors/DPI/hotplug/generation RED:\n${issues.join('\n')}'
	}
}

fn test_win32_native_cf_unicodetext_roundtrip_exact_limit_and_terminal_queue_red() {
	$if windows {
		eprintln('PACKAGE2_RED_TEST=test_win32_native_cf_unicodetext_roundtrip_exact_limit_and_terminal_queue_red')
		eprintln('PACKAGE2_RED_FAMILY=clipboard_unicode_limit')
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(title: 'Win32 clipboard oracle')!
		_ = app.drain_queued_events()!
		backend := win32_red_backend_pointer(app)
		hwnd := win32_red_hwnd(app, window)!
		mut issues := []string{}
		for operation in [ServiceOperation.clipboard_read, .clipboard_write] {
			capability := app.service_operation_capability(window, operation) or {
				issues << '${operation} capability query failed: ${err.msg()}'
				ServiceOperationCapability{}
			}
			win32_red_add(mut issues, '${operation} capability is not available asynchronous',
				capability.support == .available && capability.asynchronous
				&& !capability.requires_user_action && !capability.state_observable)
		}
		mut last_sequence := u64(0)

		external := 'external BMP € 漢字 astral 🙂 𝄞'
		external_wide := external.to_wide()
		external_fixture_ready := C.v_multiwindow_test_win32_set_clipboard(hwnd, external_wide,
			win32_red_utf16_units(external)) == 1
		if !external_fixture_ready {
			win32_w4_add_infra(mut issues,
				'external clipboard oracle could not install Unicode fixture')
		}
		if external_fixture_ready {
			read_request := app.service_request_clipboard_text(window) or {
				issues << 'public CF_UNICODETEXT read start failed: ${err.msg()}'
				ServiceRequestId{}
			}
			if read_request != ServiceRequestId{} {
				delivered := win32_w4_finish_single_clipboard(mut app, backend, read_request, 8,
					'normal Unicode read', mut issues)
				terminals := win32_red_clipboard_events(delivered, read_request)
				win32_red_add(mut issues,
					'external-to-native clipboard did not produce one exact ready envelope',
					terminals.len == 1
					&& win32_red_clipboard_envelope_matches(terminals[0], read_request, window, .clipboard_read, .ready)
					&& terminals[0].service.clipboard.text == external)
				if terminals.len == 1 {
					last_sequence = terminals[0].sequence
				}
				late_delivery := win32_w4_poll_collect(mut app, 4, 'normal Unicode read late', mut
					issues)
				late := win32_red_clipboard_events(late_delivery, read_request)
				win32_red_add(mut issues, 'normal read produced a duplicate late terminal',
					late.len == 0)
				win32_red_add(mut issues, 'normal read late events lost global ordering',
					win32_red_events_are_globally_ordered(late_delivery))
			}
		}

		written := 'public BMP Ω Ж astral 🙂 𝄞'
		written_units := win32_red_utf16_units(written)
		write_request := app.service_set_clipboard_text(window, written) or {
			issues << 'public CF_UNICODETEXT write start failed: ${err.msg()}'
			ServiceRequestId{}
		}
		if write_request != ServiceRequestId{} {
			delivered := win32_w4_finish_single_clipboard(mut app, backend, write_request, 8,
				'normal Unicode write', mut issues)
			terminals := win32_red_clipboard_events(delivered, write_request)
			win32_red_add(mut issues,
				'native-to-external clipboard did not produce one exact ready envelope',
				terminals.len == 1
				&& win32_red_clipboard_envelope_matches(terminals[0], write_request, window, .clipboard_write, .ready)
				&& terminals[0].sequence > last_sequence)
			win32_red_add(mut issues, 'CF_UNICODETEXT does not equal the public UTF-16 payload', C.v_multiwindow_test_win32_clipboard_equals(written.to_wide(),
				written_units) == 1)
			if terminals.len == 1 {
				last_sequence = terminals[0].sequence
			}
			late_delivery := win32_w4_poll_collect(mut app, 4, 'normal Unicode write late', mut
				issues)
			late := win32_red_clipboard_events(late_delivery, write_request)
			win32_red_add(mut issues, 'normal write produced a duplicate late terminal',
				late.len == 0)
			win32_red_add(mut issues, 'normal write late events lost global ordering',
				win32_red_events_are_globally_ordered(late_delivery))
		}

		exact := win32_red_exact_mixed_clipboard_text()
		exact_units := win32_red_utf16_units(exact)
		win32_red_add(mut issues, 'exact boundary payload is not exactly 16 MiB including NUL',
			exact_units * 2 == usize(win32_red_clipboard_max_bytes))
		win32_red_add(mut issues, 'exact mixed payload exceeds the independent UTF-8 bound',
			exact.len + 1 <= win32_red_clipboard_max_bytes && exact.contains('漢')
			&& exact.contains('🙂'))
		exact_request := app.service_set_clipboard_text(window, exact) or {
			issues << 'exact clipboard limit failed: ${err.msg()}'
			ServiceRequestId{}
		}
		if exact_request != ServiceRequestId{} {
			delivered := win32_w4_finish_single_clipboard(mut app, backend, exact_request, 8,
				'exact UTF-16 write', mut issues)
			terminals := win32_red_clipboard_events(delivered, exact_request)
			win32_red_add(mut issues, 'exact clipboard limit lacks one ordered ready envelope',
				terminals.len == 1
				&& win32_red_clipboard_envelope_matches(terminals[0], exact_request, window, .clipboard_write, .ready)
				&& terminals[0].sequence > last_sequence)
			win32_red_add(mut issues, 'exact mixed BMP/astral payload lost integrity', C.v_multiwindow_test_win32_clipboard_equals(exact.to_wide(),
				exact_units) == 1)
			if terminals.len == 1 {
				last_sequence = terminals[0].sequence
			}
			late_delivery := win32_w4_poll_collect(mut app, 4, 'exact UTF-16 write late', mut
				issues)
			late := win32_red_clipboard_events(late_delivery, exact_request)
			win32_red_add(mut issues, 'exact-limit write produced a duplicate late terminal',
				late.len == 0)
			win32_red_add(mut issues, 'exact-limit late events lost global ordering',
				win32_red_events_are_globally_ordered(late_delivery))
		}
		exact_read_request := app.service_request_clipboard_text(window) or {
			issues << 'exact clipboard readback start failed: ${err.msg()}'
			ServiceRequestId{}
		}
		if exact_read_request != ServiceRequestId{} {
			delivered := win32_w4_finish_single_clipboard(mut app, backend, exact_read_request, 8,
				'exact UTF-16 read', mut issues)
			terminals := win32_red_clipboard_events(delivered, exact_read_request)
			win32_red_add(mut issues,
				'exact 16 MiB NUL-at-boundary payload did not parse with full integrity',
				terminals.len == 1
				&& win32_red_clipboard_envelope_matches(terminals[0], exact_read_request, window, .clipboard_read, .ready)
				&& terminals[0].sequence > last_sequence
				&& terminals[0].service.clipboard.text == exact)
			if terminals.len == 1 {
				last_sequence = terminals[0].sequence
			}
			late_delivery := win32_w4_poll_collect(mut app, 4, 'exact UTF-16 read late', mut issues)
			late := win32_red_clipboard_events(late_delivery, exact_read_request)
			win32_red_add(mut issues, 'exact-limit read produced a duplicate late terminal',
				late.len == 0)
			win32_red_add(mut issues, 'exact-limit read late events lost global ordering',
				win32_red_events_are_globally_ordered(late_delivery))
		}
		oversized := exact + 'A'
		win32_red_add(mut issues, 'one-unit-over payload is not exactly one UTF-16 unit over', win32_red_utf16_units(oversized) * 2 == usize(
			win32_red_clipboard_max_bytes + 2))
		win32_red_add(mut issues, 'one-unit-over payload does not isolate the UTF-16 bound',

			oversized.len + 1 <= win32_red_clipboard_max_bytes)
		core_pending_before := app.services.pending.len
		native_pending_before := C.v_multiwindow_win32_service_test_clipboard_pending_count(backend)
		mut oversized_error := ''
		app.service_set_clipboard_text(window, oversized) or { oversized_error = err.msg() }
		win32_red_add(mut issues, 'limit+one UTF-16 unit was not rejected as capacity',
			oversized_error == err_clipboard_capacity)
		win32_red_add(mut issues, 'over-limit write admitted a core or native pending request',
			app.services.pending.len == core_pending_before
			&& C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == native_pending_before)
		app.poll_events() or {
			win32_w4_add_infra(mut issues, 'over-limit adjacent poll failed: ${err.msg()}')
		}
		over_delivery := app.drain_queued_events() or {
			win32_w4_add_infra(mut issues, 'over-limit adjacent drain failed: ${err.msg()}')
			[]QueuedEvent{}
		}
		over_events := over_delivery.filter(it.kind == .service && it.service.kind == .clipboard
			&& it.sequence > last_sequence)
		win32_red_add(mut issues, 'over-limit write emitted a clipboard event',
			over_events.len == 0)
		win32_red_add(mut issues, 'over-limit adjacent events lost global ordering',
			win32_red_events_are_globally_ordered(over_delivery))
		win32_w4_epilogue('clipboard_unicode_limit', 'Win32 CF_UNICODETEXT RED', issues)
	}
}

fn test_win32_native_clipboard_malformed_read_bounds_red() {
	$if windows {
		eprintln('PACKAGE2_RED_TEST=test_win32_native_clipboard_malformed_read_bounds_red')
		eprintln('PACKAGE2_RED_FAMILY=clipboard_malformed_bounds')
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(title: 'Win32 malformed HGLOBAL RED')!
		_ = app.drain_queued_events()!
		backend := win32_red_backend_pointer(app)
		hwnd := win32_red_hwnd(app, window)!
		mut issues := []string{}
		unterminated_probe := C.v_multiwindow_test_win32_clipboard_unterminated_parser_probe()
		if unterminated_probe == -1 {
			win32_w4_add_infra(mut issues,
				'independent oracle could not complete the unterminated HGLOBAL parser probe')
		} else {
			win32_red_add(mut issues, 'bounded parser accepted an unterminated HGLOBAL extent',
				unterminated_probe == 1)
		}
		for kind in 1 .. 3 {
			fixture_ready := C.v_multiwindow_test_win32_set_clipboard_malformed(hwnd, kind) == 1
			if !fixture_ready {
				win32_w4_add_infra(mut issues,
					'independent oracle could not install certified malformed HGLOBAL fixture ${kind}')
			}
			if !fixture_ready {
				continue
			}
			request := app.service_request_clipboard_text(window) or {
				issues << 'malformed fixture ${kind}: public read was not admitted: ${err.msg()}'
				ServiceRequestId{}
			}
			if request == ServiceRequestId{} {
				continue
			}
			delivered := win32_w4_finish_single_clipboard(mut app, backend, request, 4,
				'malformed fixture ${kind}', mut issues)
			terminals := win32_red_clipboard_events(delivered, request)
			win32_red_add(mut issues,
				'malformed fixture ${kind}: public path did not publish one empty failed envelope',
				terminals.len == 1
				&& win32_red_clipboard_envelope_matches(terminals[0], request, window, .clipboard_read, .failed)
				&& terminals[0].service.clipboard.text == ''
				&& terminals[0].service.clipboard.error != '')
			win32_red_add(mut issues,
				'malformed fixture ${kind}: native request survived terminal',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0)
			win32_red_add(mut issues, 'malformed fixture ${kind}: test-owned HGLOBAL leaked',
				C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0)
			win32_red_add(mut issues, 'malformed fixture ${kind}: core terminal survived delivery', win32_red_core_pending(app,
				request).len == 0)
			late_delivery := win32_w4_poll_collect(mut app, 4, 'malformed fixture ${kind} late', mut
				issues)
			win32_red_add(mut issues,
				'malformed fixture ${kind}: duplicate late clipboard terminal', win32_red_clipboard_events(late_delivery,
				request).len == 0)
			win32_red_add(mut issues,
				'malformed fixture ${kind}: late events lost global ordering',
				win32_red_events_are_globally_ordered(late_delivery))
		}
		win32_w4_epilogue('clipboard_malformed_bounds', 'Win32 malformed clipboard RED', issues)
	}
}

fn test_win32_native_clipboard_exact_utf8_limit_and_over_red() {
	$if windows {
		eprintln('PACKAGE2_RED_TEST=test_win32_native_clipboard_exact_utf8_limit_and_over_red')
		eprintln('PACKAGE2_RED_FAMILY=clipboard_utf8_limit')
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(title: 'Win32 clipboard UTF-8 limit RED')!
		_ = app.drain_queued_events()!
		backend := win32_red_backend_pointer(app)
		mut issues := []string{}
		initial_events := app.drain_queued_events() or {
			win32_w4_add_infra(mut issues, 'initial event drain failed: ${err.msg()}')
			[]QueuedEvent{}
		}
		if initial_events.len != 0 {
			win32_w4_add_infra(mut issues, 'unexpected events remained after setup')
		}

		exact := win32_red_exact_utf8_clipboard_text()
		exact_units := win32_red_utf16_units(exact)
		win32_red_add(mut issues, 'exact UTF-8 fixture is not exactly 16 MiB including NUL',

			exact.len + 1 == win32_red_clipboard_max_bytes)
		win32_red_add(mut issues, 'exact UTF-8 fixture accidentally reaches the UTF-16 bound',
			exact_units * 2 < usize(win32_red_clipboard_max_bytes))
		exact_request := app.service_set_clipboard_text(window, exact) or {
			issues << 'exact UTF-8 write was not admitted: ${err.msg()}'
			ServiceRequestId{}
		}
		if exact_request != ServiceRequestId{} {
			exact_delivery := win32_w4_finish_single_clipboard(mut app, backend, exact_request, 8,
				'exact UTF-8 write', mut issues)
			exact_terminals := win32_red_clipboard_events(exact_delivery, exact_request)
			win32_red_add(mut issues, 'exact UTF-8 write did not publish one ready envelope',
				exact_terminals.len == 1
				&& win32_red_clipboard_envelope_matches(exact_terminals[0], exact_request, window, .clipboard_write, .ready)
				&& exact_terminals[0].service.clipboard.text == ''
				&& exact_terminals[0].service.clipboard.error == '')
			win32_red_add(mut issues, 'exact UTF-8 write lost global ordering',
				win32_red_events_are_globally_ordered(exact_delivery))
			win32_red_add(mut issues, 'exact UTF-8 write lost CF_UNICODETEXT integrity', C.v_multiwindow_test_win32_clipboard_equals(exact.to_wide(),
				exact_units) == 1)
			exact_late := win32_w4_poll_collect(mut app, 4, 'exact UTF-8 late', mut issues)
			win32_red_add(mut issues, 'exact UTF-8 write produced late events', exact_late.len == 0)
		}

		over := exact + 'A'
		win32_red_add(mut issues, 'over-limit UTF-8 fixture is not exactly one byte over',

			over.len + 1 == win32_red_clipboard_max_bytes + 1)
		win32_red_add(mut issues, 'over-limit UTF-8 fixture accidentally reaches the UTF-16 bound',
			win32_red_utf16_units(over) * 2 < usize(win32_red_clipboard_max_bytes))
		core_pending_before := app.services.pending.len
		native_pending_before := C.v_multiwindow_win32_service_test_clipboard_pending_count(backend)
		allocations_before :=
			C.v_multiwindow_win32_service_test_clipboard_global_allocations(backend)
		owned_globals_before := C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend)
		sequences_before :=
			C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend)
		mut over_error := ''
		app.service_set_clipboard_text(window, over) or { over_error = err.msg() }
		win32_red_add(mut issues, 'one-byte-over UTF-8 write was not rejected as capacity',
			over_error == err_clipboard_capacity)
		win32_red_add(mut issues, 'one-byte-over UTF-8 write changed core pending state',
			app.services.pending.len == core_pending_before)
		win32_red_add(mut issues, 'one-byte-over UTF-8 write changed native pending state',
			C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == native_pending_before)
		win32_red_add(mut issues, 'one-byte-over UTF-8 write allocated an HGLOBAL',
			C.v_multiwindow_win32_service_test_clipboard_global_allocations(backend) == allocations_before)
		win32_red_add(mut issues, 'one-byte-over UTF-8 write changed HGLOBAL ownership',
			C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == owned_globals_before)
		win32_red_add(mut issues, 'one-byte-over UTF-8 write allocated an event sequence',
			C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == sequences_before)
		mut over_poll_failed := false
		app.poll_events() or {
			win32_w4_add_infra(mut issues, 'one-byte-over UTF-8 adjacent poll failed: ${err.msg()}')
			over_poll_failed = true
		}
		if !over_poll_failed {
			over_delivery := app.drain_queued_events() or {
				win32_w4_add_infra(mut issues,
					'one-byte-over UTF-8 adjacent drain failed: ${err.msg()}')
				[]QueuedEvent{}
			}
			win32_red_add(mut issues, 'one-byte-over UTF-8 write emitted an adjacent event',
				over_delivery.len == 0)
		}
		over_late := win32_w4_poll_collect(mut app, 4, 'one-byte-over UTF-8 late', mut issues)
		win32_red_add(mut issues, 'one-byte-over UTF-8 write emitted a late event',
			over_late.len == 0)
		win32_w4_epilogue('clipboard_utf8_limit', 'Win32 independent UTF-8 limit RED', issues)
	}
}

fn test_win32_native_clipboard_contention_retry_success_red() {
	$if windows {
		eprintln('PACKAGE2_RED_TEST=test_win32_native_clipboard_contention_retry_success_red')
		eprintln('PACKAGE2_RED_FAMILY=clipboard_contention_retry')
		test_now_ns := i64(10_000_000)
		mut app := new_app(backend: .win32)!
		window := app.create_window(title: 'Win32 clipboard contention retry')!
		_ = app.drain_queued_events()!
		backend := win32_red_backend_pointer(app)
		C.v_multiwindow_win32_service_test_clipboard_configure(backend, test_now_ns, 1)
		defer {
			C.v_multiwindow_win32_service_test_clipboard_use_real_clock(backend)
			app.stop() or {}
		}
		hwnd := win32_red_hwnd(app, window)!
		mut issues := []string{}
		expected := 'pending write BMP é 漢 astral 🙂'
		expected_wide := expected.to_wide()
		expected_units := win32_red_utf16_units(expected)
		mut submitted := expected.clone()
		request := app.service_set_clipboard_text(window, submitted) or {
			issues << 'contention write was not admitted: ${err.msg()}'
			ServiceRequestId{}
		}
		submitted = 'caller storage changed after admission'
		win32_red_add(mut issues, 'caller storage mutation did not change the caller value',
			submitted != expected)
		if request != ServiceRequestId{} {
			core_admitted := win32_red_core_pending(app, request)
			win32_red_add(mut issues, 'contention request was not retained non-terminal',

				core_admitted.len == 1 && !core_admitted[0].terminal)
			win32_red_add(mut issues, 'contention request was not admitted natively',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 1)
			win32_red_add(mut issues, 'native queue did not own the admitted UTF-16 copy', C.v_multiwindow_win32_service_test_clipboard_pending_write_matches(backend,
				0, request.app_instance, request.serial, window.app_instance, window.slot,
				window.generation, expected_wide, expected_units) == 1)
			deadline := C.v_multiwindow_win32_service_test_clipboard_pending_deadline_ns(backend, 0)
			win32_red_add(mut issues, 'clipboard timeout contract is not two seconds',
				C.v_multiwindow_win32_service_test_clipboard_timeout_ns(backend) == i64(2_000_000_000))
			win32_red_add(mut issues, 'contention deadline was not based on admission time', deadline ==
				test_now_ns + i64(2_000_000_000))
			win32_red_add(mut issues, 'contention admission owned an HGLOBAL before polling',
				C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0)
			win32_red_add(mut issues, 'contention admission allocated a terminal sequence',
				C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == 0)
			for _ in 0 .. 4 {
				attempts_before := C.v_multiwindow_win32_service_test_clipboard_attempts(backend)
				mut poll_failed := false
				app.poll_events() or {
					win32_w4_add_infra(mut issues, 'contention poll failed: ${err.msg()}')
					poll_failed = true
				}
				if poll_failed {
					break
				}
				attempts_after := C.v_multiwindow_win32_service_test_clipboard_attempts(backend)
				win32_red_add(mut issues, 'one poll performed more than one native attempt',

					attempts_after - attempts_before >= 0 && attempts_after - attempts_before <= 1)
				win32_red_add(mut issues, 'contention poll leaked an owned HGLOBAL',
					C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0)
				if C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0 {
					break
				}
				win32_red_add(mut issues, 'contention retry changed the absolute deadline', C.v_multiwindow_win32_service_test_clipboard_pending_deadline_ns(backend,
					0) == deadline)
			}
			win32_red_add(mut issues, 'contention path did not retry before success',
				C.v_multiwindow_win32_service_test_clipboard_attempts(backend) >= 2)
			win32_red_add(mut issues, 'contention path used a non-window clipboard owner',
				C.v_multiwindow_win32_service_test_clipboard_last_open_owner(backend) == hwnd)
			win32_red_add(mut issues, 'contention request remained native-pending',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0)
			retained := win32_red_core_pending(app, request)
			win32_red_add(mut issues, 'contention terminal was not retained before delivery',

				retained.len == 1 && retained[0].terminal)
			win32_red_add(mut issues, 'contention success leaked an owned HGLOBAL',
				C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0)
			win32_red_add(mut issues, 'contention success did not allocate exactly one HGLOBAL',
				C.v_multiwindow_win32_service_test_clipboard_global_allocations(backend) == 1)
			win32_red_add(mut issues, 'contention success did not transfer exactly one HGLOBAL',
				C.v_multiwindow_win32_service_test_clipboard_global_transfers(backend) == 1)
			win32_red_add(mut issues, 'contention success freed a transferred HGLOBAL',
				C.v_multiwindow_win32_service_test_clipboard_global_frees(backend) == 0)
			win32_red_add(mut issues, 'contention success did not allocate one terminal sequence',
				C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == 1)
			delivered := app.drain_queued_events() or {
				win32_w4_add_infra(mut issues, 'contention terminal drain failed: ${err.msg()}')
				[]QueuedEvent{}
			}
			win32_red_add(mut issues, 'contention core terminal survived delivery', win32_red_core_pending(app,
				request).len == 0)
			win32_red_add(mut issues, 'contention delivery lost global ordering',
				win32_red_events_are_globally_ordered(delivered))
			terminals := win32_red_clipboard_events(delivered, request)
			win32_red_add(mut issues, 'contention success did not publish one ready envelope',
				terminals.len == 1
				&& win32_red_clipboard_envelope_matches(terminals[0], request, window, .clipboard_write, .ready))
			win32_red_add(mut issues, 'contention success lost clipboard payload integrity', C.v_multiwindow_test_win32_clipboard_equals(expected_wide,
				expected_units) == 1)
			attempts_after_success := C.v_multiwindow_win32_service_test_clipboard_attempts(backend)
			late_delivery := win32_w4_poll_collect(mut app, 4, 'contention late', mut issues)
			win32_red_add(mut issues, 'contention success produced a duplicate late terminal', win32_red_clipboard_events(late_delivery,
				request).len == 0)
			win32_red_add(mut issues, 'contention late delivery lost global ordering',
				win32_red_events_are_globally_ordered(late_delivery))
			win32_red_add(mut issues, 'contention success was attempted again after terminal',
				C.v_multiwindow_win32_service_test_clipboard_attempts(backend) == attempts_after_success)
		}

		fault_sentinel := 'pre-transfer sentinel 🙂'
		fault_sentinel_wide := fault_sentinel.to_wide()
		fault_fixture_ready := C.v_multiwindow_test_win32_set_clipboard(hwnd, fault_sentinel_wide,
			win32_red_utf16_units(fault_sentinel)) == 1
		if !fault_fixture_ready {
			win32_w4_add_infra(mut issues,
				'pre-transfer oracle could not install the clipboard sentinel')
		}
		if fault_fixture_ready {
			C.v_multiwindow_win32_service_test_clipboard_configure(backend, test_now_ns +
				i64(1_000_000), 0)
			C.v_multiwindow_win32_service_test_clipboard_fail_before_transfer(backend, 1)
			fault_request := app.service_set_clipboard_text(window, 'must not replace sentinel') or {
				issues << 'pre-transfer fault write was not admitted: ${err.msg()}'
				ServiceRequestId{}
			}
			if fault_request != ServiceRequestId{} {
				fault_delivery := win32_w4_finish_single_clipboard(mut app, backend, fault_request,
					4, 'pre-transfer fault', mut issues)
				fault_terminals := win32_red_clipboard_events(fault_delivery, fault_request)
				win32_red_add(mut issues, 'pre-transfer fault did not publish one failed envelope',
					fault_terminals.len == 1
					&& win32_red_clipboard_envelope_matches(fault_terminals[0], fault_request, window, .clipboard_write, .failed))
				win32_red_add(mut issues, 'pre-transfer fault did not attempt exactly once',
					C.v_multiwindow_win32_service_test_clipboard_attempts(backend) == 1)
				win32_red_add(mut issues,
					'pre-transfer fault did not allocate exactly one HGLOBAL',
					C.v_multiwindow_win32_service_test_clipboard_global_allocations(backend) == 1)
				win32_red_add(mut issues, 'pre-transfer fault did not free exactly one HGLOBAL',
					C.v_multiwindow_win32_service_test_clipboard_global_frees(backend) == 1)
				win32_red_add(mut issues, 'pre-transfer fault transferred an HGLOBAL',
					C.v_multiwindow_win32_service_test_clipboard_global_transfers(backend) == 0)
				win32_red_add(mut issues, 'pre-transfer fault leaked HGLOBAL ownership',
					C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0)
				win32_red_add(mut issues,
					'pre-transfer fault never observed live HGLOBAL ownership',
					C.v_multiwindow_win32_service_test_clipboard_owned_globals_peak(backend) == 1)
				win32_red_add(mut issues,
					'pre-transfer fault did not allocate one terminal sequence',
					C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == 1)
				win32_red_add(mut issues, 'pre-transfer fault mutated the real clipboard sentinel', C.v_multiwindow_test_win32_clipboard_equals(fault_sentinel_wide,
					win32_red_utf16_units(fault_sentinel)) == 1)
				fault_late := win32_w4_poll_collect(mut app, 4, 'pre-transfer fault late', mut
					issues)
				win32_red_add(mut issues, 'pre-transfer fault produced a duplicate late terminal', win32_red_clipboard_events(fault_late,
					fault_request).len == 0)
				win32_red_add(mut issues, 'pre-transfer late delivery lost global ordering',
					win32_red_events_are_globally_ordered(fault_late))
				win32_red_add(mut issues, 'pre-transfer fault retried after terminal',
					C.v_multiwindow_win32_service_test_clipboard_attempts(backend) == 1)
				win32_red_add(mut issues,
					'pre-transfer fault changed allocation/free/transfer counters after terminal',
					C.v_multiwindow_win32_service_test_clipboard_global_allocations(backend) == 1
					&& C.v_multiwindow_win32_service_test_clipboard_global_frees(backend) == 1
					&& C.v_multiwindow_win32_service_test_clipboard_global_transfers(backend) == 0
					&& C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0
					&& C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == 1)
				win32_red_add(mut issues,
					'pre-transfer late poll mutated the real clipboard sentinel', C.v_multiwindow_test_win32_clipboard_equals(fault_sentinel_wide,
					win32_red_utf16_units(fault_sentinel)) == 1)
			}
		}
		win32_w4_epilogue('clipboard_contention_retry', 'Win32 clipboard contention/HGLOBAL RED',
			issues)
	}
}

fn test_win32_native_clipboard_fifo_head_only_red() {
	$if windows {
		eprintln('PACKAGE2_RED_TEST=test_win32_native_clipboard_fifo_head_only_red')
		eprintln('PACKAGE2_RED_FAMILY=clipboard_fifo')
		test_now_ns := i64(15_000_000)
		mut app := new_app(backend: .win32)!
		window := app.create_window(title: 'Win32 clipboard FIFO RED')!
		_ = app.drain_queued_events()!
		backend := win32_red_backend_pointer(app)
		C.v_multiwindow_win32_service_test_clipboard_configure(backend, test_now_ns, 1)
		defer {
			C.v_multiwindow_win32_service_test_clipboard_use_real_clock(backend)
			app.stop() or {}
		}
		first_text := 'FIFO first 🙂'
		second_text := 'FIFO second 漢'
		first_wide := first_text.to_wide()
		second_wide := second_text.to_wide()
		mut issues := []string{}
		first := app.service_set_clipboard_text(window, first_text) or {
			issues << 'FIFO first write was not admitted: ${err.msg()}'
			ServiceRequestId{}
		}
		second := app.service_set_clipboard_text(window, second_text) or {
			issues << 'FIFO second write was not admitted: ${err.msg()}'
			ServiceRequestId{}
		}
		if first != ServiceRequestId{} && second != ServiceRequestId{} {
			first_core := win32_red_core_pending(app, first)
			second_core := win32_red_core_pending(app, second)
			win32_red_add(mut issues, 'FIFO first request was not admitted non-terminal',

				first_core.len == 1 && !first_core[0].terminal)
			win32_red_add(mut issues, 'FIFO second request was not admitted non-terminal',

				second_core.len == 1 && !second_core[0].terminal)
			win32_red_add(mut issues, 'FIFO native queue did not retain two requests',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 2)
			win32_red_add(mut issues, 'FIFO head did not own the first UTF-16 payload', C.v_multiwindow_win32_service_test_clipboard_pending_write_matches(backend,
				0, first.app_instance, first.serial, window.app_instance, window.slot,
				window.generation, first_wide, win32_red_utf16_units(first_text)) == 1)
			win32_red_add(mut issues, 'FIFO tail did not own the second UTF-16 payload', C.v_multiwindow_win32_service_test_clipboard_pending_write_matches(backend,
				1, second.app_instance, second.serial, window.app_instance, window.slot,
				window.generation, second_wide, win32_red_utf16_units(second_text)) == 1)
			first_deadline :=
				C.v_multiwindow_win32_service_test_clipboard_pending_deadline_ns(backend, 0)
			win32_red_add(mut issues, 'FIFO first deadline was not based on admission', first_deadline ==
				test_now_ns + i64(2_000_000_000))
			win32_red_add(mut issues, 'FIFO first request was attempted during admission', C.v_multiwindow_win32_service_test_clipboard_request_attempts(backend,
				first.app_instance, first.serial) == 0)
			win32_red_add(mut issues, 'FIFO second request was attempted during admission', C.v_multiwindow_win32_service_test_clipboard_request_attempts(backend,
				second.app_instance, second.serial) == 0)

			app.poll_events() or {
				win32_w4_add_infra(mut issues, 'FIFO contention poll failed: ${err.msg()}')
			}
			win32_red_add(mut issues, 'FIFO head was not attempted exactly once under contention', C.v_multiwindow_win32_service_test_clipboard_request_attempts(backend,
				first.app_instance, first.serial) == 1)
			win32_red_add(mut issues, 'FIFO tail was attempted while the head was blocked', C.v_multiwindow_win32_service_test_clipboard_request_attempts(backend,
				second.app_instance, second.serial) == 0)
			win32_red_add(mut issues, 'FIFO contention removed a pending request',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 2)
			win32_red_add(mut issues, 'FIFO contention leaked an HGLOBAL',
				C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0)
			win32_red_add(mut issues, 'FIFO contention emitted a head terminal', win32_red_clipboard_events(app.events,
				first).len == 0)
			win32_red_add(mut issues, 'FIFO contention emitted a tail terminal', win32_red_clipboard_events(app.events,
				second).len == 0)

			activation_now_ns := test_now_ns + i64(1_000_000_000)
			C.v_multiwindow_win32_service_test_clipboard_set_now_ns(backend, activation_now_ns)
			app.poll_events() or {
				win32_w4_add_infra(mut issues, 'FIFO head completion poll failed: ${err.msg()}')
			}
			win32_red_add(mut issues, 'FIFO head did not retry exactly once', C.v_multiwindow_win32_service_test_clipboard_request_attempts(backend,
				first.app_instance, first.serial) == 2)
			win32_red_add(mut issues, 'FIFO tail was attempted in the head completion poll', C.v_multiwindow_win32_service_test_clipboard_request_attempts(backend,
				second.app_instance, second.serial) == 0)
			win32_red_add(mut issues, 'FIFO head completion did not promote exactly one tail',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 1)
			first_after := win32_red_core_pending(app, first)
			second_after := win32_red_core_pending(app, second)
			win32_red_add(mut issues, 'FIFO head did not become core-terminal',

				first_after.len == 1 && first_after[0].terminal)
			win32_red_add(mut issues, 'FIFO tail became terminal before its first attempt',

				second_after.len == 1 && !second_after[0].terminal)
			second_deadline :=
				C.v_multiwindow_win32_service_test_clipboard_pending_deadline_ns(backend, 0)
			win32_red_add(mut issues, 'FIFO tail deadline did not start at activation',
				second_deadline == activation_now_ns + i64(2_000_000_000)
				&& second_deadline > first_deadline)
			win32_red_add(mut issues, 'FIFO head did not queue exactly one terminal', win32_red_clipboard_events(app.events,
				first).len == 1)
			win32_red_add(mut issues, 'FIFO tail queued a terminal before its first attempt', win32_red_clipboard_events(app.events,
				second).len == 0)

			C.v_multiwindow_win32_service_test_clipboard_set_now_ns(backend, first_deadline)
			app.poll_events() or {
				win32_w4_add_infra(mut issues, 'FIFO tail completion poll failed: ${err.msg()}')
			}
			win32_red_add(mut issues, 'FIFO tail was not attempted exactly once', C.v_multiwindow_win32_service_test_clipboard_request_attempts(backend,
				second.app_instance, second.serial) == 1)
			win32_red_add(mut issues, 'FIFO tail remained native-pending after success',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0)
			first_terminal := win32_red_core_pending(app, first)
			second_terminal := win32_red_core_pending(app, second)
			win32_red_add(mut issues, 'FIFO first terminal was not retained before delivery',

				first_terminal.len == 1 && first_terminal[0].terminal)
			win32_red_add(mut issues, 'FIFO second terminal was not retained before delivery',

				second_terminal.len == 1 && second_terminal[0].terminal)
			win32_red_add(mut issues, 'FIFO completion leaked HGLOBAL ownership',
				C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0)
			win32_red_add(mut issues, 'FIFO completion did not allocate two HGLOBALs',
				C.v_multiwindow_win32_service_test_clipboard_global_allocations(backend) == 2)
			win32_red_add(mut issues, 'FIFO completion did not transfer two HGLOBALs',
				C.v_multiwindow_win32_service_test_clipboard_global_transfers(backend) == 2)
			win32_red_add(mut issues, 'FIFO completion did not allocate two sequences',
				C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == 2)

			delivered := app.drain_queued_events() or {
				win32_w4_add_infra(mut issues, 'FIFO terminal drain failed: ${err.msg()}')
				[]QueuedEvent{}
			}
			clipboard := delivered.filter(it.kind == .service && it.service.kind == .clipboard)
			win32_red_add(mut issues, 'FIFO delivery lost global ordering',
				win32_red_events_are_globally_ordered(delivered))
			win32_red_add(mut issues, 'FIFO delivery did not contain exactly two terminals',
				clipboard.len == 2)
			if clipboard.len == 2 {
				win32_red_add(mut issues, 'FIFO first envelope was not first and ready', win32_red_clipboard_envelope_matches(clipboard[0],
					first, window, .clipboard_write, .ready))
				win32_red_add(mut issues, 'FIFO second envelope was not second and ready', win32_red_clipboard_envelope_matches(clipboard[1],
					second, window, .clipboard_write, .ready))
				win32_red_add(mut issues, 'FIFO terminal sequences were not strictly ordered',
					clipboard[0].sequence < clipboard[1].sequence)
			}
			win32_red_add(mut issues, 'FIFO first core terminal survived delivery', win32_red_core_pending(app,
				first).len == 0)
			win32_red_add(mut issues, 'FIFO second core terminal survived delivery', win32_red_core_pending(app,
				second).len == 0)
			win32_red_add(mut issues, 'FIFO final clipboard did not contain the tail payload', C.v_multiwindow_test_win32_clipboard_equals(second_wide,
				win32_red_utf16_units(second_text)) == 1)
			late := win32_w4_poll_collect(mut app, 4, 'FIFO late', mut issues)
			win32_red_add(mut issues, 'FIFO first request produced a duplicate late terminal', win32_red_clipboard_events(late,
				first).len == 0)
			win32_red_add(mut issues, 'FIFO second request produced a duplicate late terminal', win32_red_clipboard_events(late,
				second).len == 0)
			win32_red_add(mut issues, 'FIFO late delivery lost global ordering',
				win32_red_events_are_globally_ordered(late))
		}
		win32_w4_epilogue('clipboard_fifo', 'Win32 clipboard FIFO RED', issues)
	}
}

fn test_win32_native_clipboard_real_wm_close_global_order_red() {
	$if windows {
		eprintln('PACKAGE2_RED_TEST=test_win32_native_clipboard_real_wm_close_global_order_red')
		eprintln('PACKAGE2_RED_FAMILY=clipboard_global_order')
		test_now_ns := i64(18_000_000)
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(
			title:   'Win32 clipboard WM_CLOSE order RED'
			visible: false
		)!
		backend := win32_red_backend_pointer(app)
		hwnd := win32_red_hwnd(app, window)!
		mut issues := []string{}
		if !win32_w4_settle_window_setup(mut app, window, 'WM_CLOSE setup', mut issues) {
			win32_w4_epilogue('clipboard_global_order',
				'Win32 clipboard/WM_CLOSE global ordering RED', issues)
			return
		}
		hwnd_alive := C.v_multiwindow_test_win32_is_window(hwnd) == 1
		window_alive := app.window_exists(window)
		win32_red_add(mut issues, 'WM_CLOSE setup destroyed the HWND', hwnd_alive)
		win32_red_add(mut issues, 'WM_CLOSE setup removed the public window', window_alive)
		win32_red_add(mut issues, 'WM_CLOSE setup left App events queued', app.events.len == 0)
		if !hwnd_alive || !window_alive {
			win32_w4_epilogue('clipboard_global_order',
				'Win32 clipboard/WM_CLOSE global ordering RED', issues)
			return
		}
		C.v_multiwindow_win32_service_test_clipboard_configure(backend, test_now_ns, 0)
		defer {
			C.v_multiwindow_win32_service_test_clipboard_use_real_clock(backend)
		}
		win32_red_add(mut issues, 'WM_CLOSE setup retained a native clipboard request',
			C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0)
		win32_red_add(mut issues, 'WM_CLOSE setup allocated a clipboard sequence',
			C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == 0)

		first := app.service_set_clipboard_text(window, 'first before WM_CLOSE') or {
			issues << 'pre-WM_CLOSE clipboard write was not admitted: ${err.msg()}'
			ServiceRequestId{}
		}
		if first != ServiceRequestId{} {
			for _ in 0 .. 8 {
				mut poll_failed := false
				app.poll_events() or {
					win32_w4_add_infra(mut issues,
						'pre-WM_CLOSE clipboard poll failed: ${err.msg()}')
					poll_failed = true
				}
				if poll_failed
					|| C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0 {
					break
				}
				time.sleep(5 * time.millisecond)
			}
			win32_red_add(mut issues, 'pre-WM_CLOSE write remained native-pending',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0)
			first_core := win32_red_core_pending(app, first)
			win32_red_add(mut issues, 'pre-WM_CLOSE terminal was not retained', first_core.len == 1
				&& first_core[0].terminal)
			win32_red_add(mut issues, 'pre-WM_CLOSE path queued unexpected adjacent events',

				app.events.len == 1 && win32_red_clipboard_events(app.events, first).len == 1)

			second := app.service_set_clipboard_text(window, 'second after WM_CLOSE') or {
				issues << 'post-WM_CLOSE clipboard write was not admitted: ${err.msg()}'
				ServiceRequestId{}
			}
			if second != ServiceRequestId{} {
				win32_red_add(mut issues, 'post-WM_CLOSE write was attempted during admission', C.v_multiwindow_win32_service_test_clipboard_request_attempts(backend,
					second.app_instance, second.serial) == 0)
				_ = C.SendMessageW(hwnd, win32_red_wm_close, usize(0), isize(0))
				win32_red_add(mut issues, 'real WM_CLOSE destroyed the HWND synchronously',
					C.v_multiwindow_test_win32_is_window(hwnd) == 1)
				win32_red_add(mut issues, 'real WM_CLOSE removed the public window synchronously',
					app.window_exists(window))
				for _ in 0 .. 8 {
					mut poll_failed := false
					app.poll_events() or {
						win32_w4_add_infra(mut issues,
							'post-WM_CLOSE clipboard poll failed: ${err.msg()}')
						poll_failed = true
					}
					if poll_failed
						|| C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0 {
						break
					}
					time.sleep(5 * time.millisecond)
				}
				win32_red_add(mut issues, 'post-WM_CLOSE write remained native-pending',
					C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0)
				second_core := win32_red_core_pending(app, second)
				win32_red_add(mut issues, 'post-WM_CLOSE terminal was not retained',

					second_core.len == 1 && second_core[0].terminal)

				delivered := app.drain_queued_events() or {
					win32_w4_add_infra(mut issues,
						'WM_CLOSE global-order drain failed: ${err.msg()}')
					[]QueuedEvent{}
				}
				win32_red_add(mut issues, 'WM_CLOSE global delivery did not contain three events',
					delivered.len == 3)
				win32_red_add(mut issues, 'WM_CLOSE global delivery lost strict ordering',
					win32_red_events_are_globally_ordered(delivered))
				if delivered.len == 3 {
					win32_red_add(mut issues, 'pre-WM_CLOSE clipboard terminal was not first', win32_red_clipboard_envelope_matches(delivered[0],
						first, window, .clipboard_write, .ready))
					win32_red_add(mut issues, 'real WM_CLOSE lifecycle event was not second',
						delivered[1].kind == .lifecycle
						&& delivered[1].lifecycle.kind == .window_close_requested
						&& delivered[1].lifecycle.window_id == window)
					win32_red_add(mut issues, 'post-WM_CLOSE clipboard terminal was not third', win32_red_clipboard_envelope_matches(delivered[2],
						second, window, .clipboard_write, .ready))
					win32_red_add(mut issues,
						'clipboard/lifecycle/clipboard sequences were not strictly increasing',
						delivered[0].sequence < delivered[1].sequence
						&& delivered[1].sequence < delivered[2].sequence)
				}
				win32_red_add(mut issues, 'pre-WM_CLOSE core terminal survived delivery', win32_red_core_pending(app,
					first).len == 0)
				win32_red_add(mut issues, 'post-WM_CLOSE core terminal survived delivery', win32_red_core_pending(app,
					second).len == 0)
				win32_red_add(mut issues, 'WM_CLOSE delivery destroyed the HWND',
					C.v_multiwindow_test_win32_is_window(hwnd) == 1)
				win32_red_add(mut issues, 'WM_CLOSE delivery removed the public window',
					app.window_exists(window))
				late := win32_w4_poll_collect(mut app, 4, 'WM_CLOSE global-order late', mut issues)
				win32_red_add(mut issues, 'WM_CLOSE path produced late events', late.len == 0)
				win32_red_add(mut issues, 'late WM_CLOSE poll destroyed the HWND',
					C.v_multiwindow_test_win32_is_window(hwnd) == 1)
				win32_red_add(mut issues, 'late WM_CLOSE poll removed the public window',
					app.window_exists(window))
			}
		}
		win32_w4_epilogue('clipboard_global_order', 'Win32 clipboard/WM_CLOSE global ordering RED',
			issues)
	}
}

fn win32_w4_clipboard_timeout_case(test_now_ns i64) ![]string {
	$if windows {
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(title: 'Win32 clipboard occupied')!
		_ = app.drain_queued_events()!
		backend := win32_red_backend_pointer(app)
		hwnd := win32_red_hwnd(app, window)!
		C.v_multiwindow_win32_service_test_clipboard_configure(backend, test_now_ns, 16)
		defer {
			C.v_multiwindow_win32_service_test_clipboard_use_real_clock(backend)
		}
		mut issues := []string{}
		request := app.service_request_clipboard_text(window) or {
			issues << 'occupied read was not admitted: ${err.msg()}'
			ServiceRequestId{}
		}
		if request != ServiceRequestId{} {
			core_admitted := win32_red_core_pending(app, request)
			win32_red_add(mut issues, 'occupied read was not retained non-terminal',

				core_admitted.len == 1 && !core_admitted[0].terminal)
			win32_red_add(mut issues, 'occupied read was not native-pending',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 1)
			win32_red_add(mut issues, 'occupied read did not reserve the aggregate byte limit',
				app.backend.win32.clipboard_pending_bytes == usize(win32_red_clipboard_max_bytes))
			deadline := C.v_multiwindow_win32_service_test_clipboard_pending_deadline_ns(backend, 0)
			win32_red_add(mut issues, 'occupied read deadline was not admission+2s', deadline ==
				test_now_ns + i64(2_000_000_000))
			win32_red_add(mut issues, 'occupied read allocated a sequence during admission',
				C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == 0)
			attempts_before_retry := C.v_multiwindow_win32_service_test_clipboard_attempts(backend)
			app.poll_events() or {
				win32_w4_add_infra(mut issues, 'occupied read retry poll failed: ${err.msg()}')
			}
			attempts_after_retry := C.v_multiwindow_win32_service_test_clipboard_attempts(backend)
			win32_red_add(mut issues, 'occupied read poll performed more than one attempt',
				attempts_after_retry - attempts_before_retry >= 0
				&& attempts_after_retry - attempts_before_retry <= 1)
			win32_red_add(mut issues, 'occupied read used a non-window clipboard owner',
				C.v_multiwindow_win32_service_test_clipboard_last_open_owner(backend) == hwnd)
			win32_red_add(mut issues, 'occupied read retry changed its deadline', C.v_multiwindow_win32_service_test_clipboard_pending_deadline_ns(backend,
				0) == deadline)
			win32_red_add(mut issues, 'occupied read retry left the native queue unexpectedly',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 1)
			core_retry := win32_red_core_pending(app, request)
			win32_red_add(mut issues, 'occupied read retry became core-terminal',

				core_retry.len == 1 && !core_retry[0].terminal)
			win32_red_add(mut issues, 'occupied read retry emitted a clipboard terminal', win32_red_clipboard_events(app.events,
				request).len == 0)
			C.v_multiwindow_win32_service_test_clipboard_set_now_ns(backend, deadline)
			attempts_before_timeout :=
				C.v_multiwindow_win32_service_test_clipboard_attempts(backend)
			app.poll_events() or {
				win32_w4_add_infra(mut issues, 'occupied read timeout poll failed: ${err.msg()}')
			}
			win32_red_add(mut issues, 'timeout did not perform one final native clipboard attempt', C.v_multiwindow_win32_service_test_clipboard_attempts(backend) ==
				attempts_before_timeout + 1)
			win32_red_add(mut issues, 'timed-out read remained native-pending',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0)
			win32_red_add(mut issues, 'timed-out read retained byte capacity without a payload',
				app.backend.win32.clipboard_pending_bytes == 0)
			win32_red_add(mut issues, 'timeout did not allocate exactly one sequence',
				C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == 1)
			core_timeout := win32_red_core_pending(app, request)
			win32_red_add(mut issues, 'timeout terminal was not retained before delivery',

				core_timeout.len == 1 && core_timeout[0].terminal)
			timeout_delivery := app.drain_queued_events() or {
				win32_w4_add_infra(mut issues, 'timeout terminal drain failed: ${err.msg()}')
				[]QueuedEvent{}
			}
			win32_red_add(mut issues, 'timeout core terminal survived delivery', win32_red_core_pending(app,
				request).len == 0)
			win32_red_add(mut issues, 'timeout delivery lost global ordering',
				win32_red_events_are_globally_ordered(timeout_delivery))
			timeout_events := win32_red_clipboard_events(timeout_delivery, request)
			win32_red_add(mut issues, 'timeout did not publish one exact failed envelope',
				timeout_events.len == 1
				&& win32_red_clipboard_envelope_matches(timeout_events[0], request, window, .clipboard_read, .failed)
				&& timeout_events[0].service.clipboard.error == err_clipboard_timeout)
			late_timeout := win32_w4_poll_collect(mut app, 4, 'timeout late', mut issues)
			win32_red_add(mut issues, 'timed-out read was attempted again', C.v_multiwindow_win32_service_test_clipboard_attempts(backend) ==
				attempts_before_timeout + 1)
			win32_red_add(mut issues, 'timeout allocated a duplicate sequence',
				C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == 1)
			win32_red_add(mut issues, 'timeout produced a duplicate late terminal', win32_red_clipboard_events(late_timeout,
				request).len == 0)
			win32_red_add(mut issues, 'timeout late delivery lost global ordering',
				win32_red_events_are_globally_ordered(late_timeout))
		}
		return issues
	} $else {
		_ = test_now_ns
		return error('Win32 clipboard timeout case is unavailable')
	}
}

fn win32_w4_clipboard_ready_storage_case(test_now_ns i64) ![]string {
	$if windows {
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		read_window := app.create_window(title: 'Win32 clipboard retained ready storage')!
		queue_window := app.create_window(title: 'Win32 clipboard retained FIFO')!
		_ = app.drain_queued_events()!
		backend := win32_red_backend_pointer(app)
		hwnd := win32_red_hwnd(app, read_window)!
		defer {
			C.v_multiwindow_win32_service_test_clipboard_use_real_clock(backend)
		}
		mut issues := []string{}
		read_text := 'retained ready read 🙂'
		if C.v_multiwindow_test_win32_set_clipboard(hwnd, read_text.to_wide(),
			win32_red_utf16_units(read_text)) != 1 {
			win32_w4_add_infra(mut issues,
				'retained-ready fixture could not publish clipboard text')
			return issues
		}
		C.v_multiwindow_win32_service_test_clipboard_configure(backend, test_now_ns, 0)
		read := app.service_request_clipboard_text(read_window) or {
			issues << 'retained-ready read was not admitted: ${err.msg()}'
			ServiceRequestId{}
		}
		if read == ServiceRequestId{} {
			return issues
		}
		win32_red_add(mut issues, 'read admission did not reserve the full aggregate limit',
			app.backend.win32.clipboard_pending_bytes == usize(win32_red_clipboard_max_bytes))
		app.poll_events() or {
			win32_w4_add_infra(mut issues, 'retained-ready read poll failed: ${err.msg()}')
		}
		read_charge := usize(read_text.len + 1)
		win32_red_add(mut issues, 'ready read remained native-pending',
			C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0)
		win32_red_add(mut issues, 'ready read did not retain its exact stable payload charge',
			app.backend.win32.clipboard_pending_bytes == read_charge)
		read_core := win32_red_core_pending(app, read)
		win32_red_add(mut issues, 'ready read was not retained core-terminal before delivery',

			read_core.len == 1 && read_core[0].terminal)
		app.destroy_window(read_window) or {
			issues << 'ready-read window destroy failed: ${err.msg()}'
		}
		win32_red_add(mut issues,
			'window teardown released an undrained ready-read storage charge',
			app.backend.win32.clipboard_pending_bytes == read_charge
			&& win32_red_core_pending(app, read).len == 1)

		request_before_capacity := app.services.next_request
		token_before_capacity := app.next_event_delivery_token
		mut second_read_error := ''
		app.service_request_clipboard_text(queue_window) or { second_read_error = err.msg() }
		win32_red_add(mut issues, 'undrained ready read admitted another maximum read',
			second_read_error == err_clipboard_capacity)
		win32_red_add(mut issues,
			'capacity rejection rewound its safe request-id gap or mutated delivery authority',
			app.services.next_request == request_before_capacity + 1
			&& app.next_event_delivery_token == token_before_capacity)
		win32_red_add(mut issues, 'capacity rejection changed native/core queue cardinality',
			C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0
			&& win32_red_core_pending(app, read).len == 1)

		write_text := 'small FIFO write'
		write_bytes := win32_red_utf16_units(write_text) * usize(2)
		write := app.service_set_clipboard_text(queue_window, write_text) or {
			issues << 'small write was rejected despite retained-read budget: ${err.msg()}'
			ServiceRequestId{}
		}
		if write != ServiceRequestId{} {
			win32_red_add(mut issues, 'small write did not share the aggregate budget', app.backend.win32.clipboard_pending_bytes ==
				read_charge + write_bytes)
			app.poll_events() or {
				win32_w4_add_infra(mut issues, 'small write poll failed: ${err.msg()}')
			}
			win32_red_add(mut issues, 'completed write changed retained ready-read charge',
				app.backend.win32.clipboard_pending_bytes == read_charge)
			mut third_read_error := ''
			app.service_request_clipboard_text(queue_window) or { third_read_error = err.msg() }
			win32_red_add(mut issues, 'completed undrained FIFO terminals admitted another read',
				third_read_error == err_clipboard_capacity)

			delivered := app.drain_queued_events() or {
				win32_w4_add_infra(mut issues, 'retained-ready terminal drain failed: ${err.msg()}')
				[]QueuedEvent{}
			}
			clipboard := delivered.filter(it.kind == .service && it.service.kind == .clipboard)
			win32_red_add(mut issues, 'retained-ready delivery lost global FIFO order',
				clipboard.len == 2 && clipboard[0].service.clipboard.id == read
				&& clipboard[1].service.clipboard.id == write
				&& clipboard[0].sequence < clipboard[1].sequence)
			win32_red_add(mut issues, 'core drain did not release retained read capacity',
				app.backend.win32.clipboard_pending_bytes == 0)
		}

		retry := app.service_request_clipboard_text(queue_window) or {
			issues << 'read retry after drain was not admitted: ${err.msg()}'
			ServiceRequestId{}
		}
		if retry != ServiceRequestId{} {
			win32_red_add(mut issues, 'read retry did not reserve the full aggregate limit',
				app.backend.win32.clipboard_pending_bytes == usize(win32_red_clipboard_max_bytes))
			app.destroy_window(queue_window) or {
				issues << 'destroy-before-read-poll failed: ${err.msg()}'
			}
			win32_red_add(mut issues,
				'destroy-before-poll did not release the native read reservation',
				app.backend.win32.clipboard_pending_bytes == 0
				&& C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0)
			cancellation := win32_red_core_pending(app, retry)
			win32_red_add(mut issues,
				'destroy-before-poll did not retain one undrained core cancellation',

				cancellation.len == 1 && cancellation[0].terminal)
			_ = app.drain_queued_events() or {
				win32_w4_add_infra(mut issues,
					'destroy-before-poll terminal drain failed: ${err.msg()}')
				[]QueuedEvent{}
			}
		}
		return issues
	} $else {
		_ = test_now_ns
		return error('Win32 clipboard retained-ready storage case is unavailable')
	}
}

fn win32_w4_clipboard_late_first_attempt_case(test_now_ns i64) ![]string {
	$if windows {
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(title: 'Win32 clipboard late first attempt')!
		_ = app.drain_queued_events()!
		backend := win32_red_backend_pointer(app)
		hwnd := win32_red_hwnd(app, window)!
		defer {
			C.v_multiwindow_win32_service_test_clipboard_use_real_clock(backend)
		}
		mut issues := []string{}

		read_text := 'late first read remains available 🙂'
		read_wide := read_text.to_wide()
		if C.v_multiwindow_test_win32_set_clipboard(hwnd, read_wide,
			win32_red_utf16_units(read_text)) != 1 {
			win32_w4_add_infra(mut issues,
				'late first read fixture could not publish clipboard text')
		} else {
			C.v_multiwindow_win32_service_test_clipboard_configure(backend, test_now_ns, 0)
			read_request := app.service_request_clipboard_text(window) or {
				issues << 'late first read was not admitted: ${err.msg()}'
				ServiceRequestId{}
			}
			if read_request != ServiceRequestId{} {
				read_deadline :=
					C.v_multiwindow_win32_service_test_clipboard_pending_deadline_ns(backend, 0)
				C.v_multiwindow_win32_service_test_clipboard_set_now_ns(backend, read_deadline + 1)
				app.poll_events() or {
					win32_w4_add_infra(mut issues, 'late first read poll failed: ${err.msg()}')
				}
				win32_red_add(mut issues, 'late available read was not attempted exactly once', C.v_multiwindow_win32_service_test_clipboard_request_attempts(backend,
					read_request.app_instance, read_request.serial) == 1)
				read_events := app.drain_queued_events() or {
					win32_w4_add_infra(mut issues, 'late first read drain failed: ${err.msg()}')
					[]QueuedEvent{}
				}
				read_terminals := win32_red_clipboard_events(read_events, read_request)
				win32_red_add(mut issues,
					'late available read did not complete ready exactly once',
					read_terminals.len == 1
					&& win32_red_clipboard_envelope_matches(read_terminals[0], read_request, window, .clipboard_read, .ready)
					&& read_terminals[0].service.clipboard.text == read_text)
			}
		}

		write_now_ns := test_now_ns + i64(10_000_000)
		C.v_multiwindow_win32_service_test_clipboard_configure(backend, write_now_ns, 0)
		write_text := 'late first write remains available 漢'
		write_request := app.service_set_clipboard_text(window, write_text) or {
			issues << 'late first write was not admitted: ${err.msg()}'
			ServiceRequestId{}
		}
		if write_request != ServiceRequestId{} {
			write_deadline :=
				C.v_multiwindow_win32_service_test_clipboard_pending_deadline_ns(backend, 0)
			C.v_multiwindow_win32_service_test_clipboard_set_now_ns(backend, write_deadline + 1)
			app.poll_events() or {
				win32_w4_add_infra(mut issues, 'late first write poll failed: ${err.msg()}')
			}
			win32_red_add(mut issues, 'late available write was not attempted exactly once', C.v_multiwindow_win32_service_test_clipboard_request_attempts(backend,
				write_request.app_instance, write_request.serial) == 1)
			write_events := app.drain_queued_events() or {
				win32_w4_add_infra(mut issues, 'late first write drain failed: ${err.msg()}')
				[]QueuedEvent{}
			}
			write_terminals := win32_red_clipboard_events(write_events, write_request)
			write_wide := write_text.to_wide()
			win32_red_add(mut issues, 'late available write did not complete ready exactly once',
				write_terminals.len == 1
				&& win32_red_clipboard_envelope_matches(write_terminals[0], write_request, window, .clipboard_write, .ready))
			win32_red_add(mut issues, 'late available write lost clipboard payload integrity', C.v_multiwindow_test_win32_clipboard_equals(write_wide,
				win32_red_utf16_units(write_text)) == 1)
		}

		retry_now_ns := test_now_ns + i64(20_000_000)
		C.v_multiwindow_win32_service_test_clipboard_configure(backend, retry_now_ns, 16)
		first := app.service_set_clipboard_text(window, 'late retry FIFO head') or {
			issues << 'late retry FIFO head was not admitted: ${err.msg()}'
			ServiceRequestId{}
		}
		second := app.service_set_clipboard_text(window, 'late retry FIFO tail') or {
			issues << 'late retry FIFO tail was not admitted: ${err.msg()}'
			ServiceRequestId{}
		}
		if first != ServiceRequestId{} && second != ServiceRequestId{} {
			first_deadline :=
				C.v_multiwindow_win32_service_test_clipboard_pending_deadline_ns(backend, 0)
			C.v_multiwindow_win32_service_test_clipboard_set_now_ns(backend, first_deadline)
			app.poll_events() or {
				win32_w4_add_infra(mut issues, 'late retry FIFO poll failed: ${err.msg()}')
			}
			win32_red_add(mut issues, 'late retry FIFO head was not attempted exactly once', C.v_multiwindow_win32_service_test_clipboard_request_attempts(backend,
				first.app_instance, first.serial) == 1)
			win32_red_add(mut issues, 'late retry FIFO tail was attempted in the head poll', C.v_multiwindow_win32_service_test_clipboard_request_attempts(backend,
				second.app_instance, second.serial) == 0)
			win32_red_add(mut issues, 'late retry FIFO did not retain exactly the tail natively',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 1)
			win32_red_add(mut issues, 'late retry FIFO tail deadline did not start at promotion', C.v_multiwindow_win32_service_test_clipboard_pending_deadline_ns(backend,
				0) == first_deadline + i64(2_000_000_000))
			first_events := app.drain_queued_events() or {
				win32_w4_add_infra(mut issues, 'late retry FIFO drain failed: ${err.msg()}')
				[]QueuedEvent{}
			}
			first_terminals := win32_red_clipboard_events(first_events, first)
			win32_red_add(mut issues, 'late retry FIFO head did not timeout exactly once',
				first_terminals.len == 1
				&& win32_red_clipboard_envelope_matches(first_terminals[0], first, window, .clipboard_write, .failed)
				&& first_terminals[0].service.clipboard.error == err_clipboard_timeout)
			win32_red_add(mut issues, 'late retry FIFO tail became terminal before an attempt',
				win32_red_core_pending(app, second).len == 1
				&& !win32_red_core_pending(app, second)[0].terminal)
		}
		return issues
	} $else {
		_ = test_now_ns
		return error('Win32 clipboard late-first-attempt case is unavailable')
	}
}

fn win32_w4_clipboard_destroy_cancel_case(test_now_ns i64) ![]string {
	$if windows {
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(title: 'Win32 clipboard destroy cancel')!
		_ = app.drain_queued_events()!
		backend := win32_red_backend_pointer(app)
		C.v_multiwindow_win32_service_test_clipboard_configure(backend, test_now_ns, 16)
		mut issues := []string{}
		request := app.service_set_clipboard_text(window, 'destroy pending write 🙂') or {
			issues << 'destroy-cancel write was not admitted: ${err.msg()}'
			ServiceRequestId{}
		}
		if request != ServiceRequestId{} {
			admitted := win32_red_core_pending(app, request)
			win32_red_add(mut issues, 'destroy-cancel write was not retained non-terminal',

				admitted.len == 1 && !admitted[0].terminal)
			win32_red_add(mut issues, 'destroy-cancel write was not native-pending',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 1)
			win32_red_add(mut issues, 'destroy-cancel admission owned an HGLOBAL',
				C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0)
			app.destroy_window(window) or {
				issues << 'ordinary destroy-cancel failed: ${err.msg()}'
			}
			win32_red_add(mut issues, 'ordinary destroy did not purge the native request',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0)
			win32_red_add(mut issues, 'ordinary destroy leaked HGLOBAL ownership',
				C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0)
			win32_red_add(mut issues, 'ordinary destroy allocated a native terminal sequence',
				C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == 0)
			terminal := win32_red_core_pending(app, request)
			win32_red_add(mut issues, 'ordinary destroy did not retain one core cancellation',

				terminal.len == 1 && terminal[0].terminal)
			events := app.drain_queued_events() or {
				win32_w4_add_infra(mut issues,
					'ordinary destroy terminal drain failed: ${err.msg()}')
				[]QueuedEvent{}
			}
			win32_red_add(mut issues, 'ordinary destroy delivery lost global ordering',
				win32_red_events_are_globally_ordered(events))
			win32_red_add(mut issues, 'ordinary destroy did not publish exactly two events',
				events.len == 2)
			if events.len == 2 {
				win32_red_add(mut issues, 'ordinary destroy cancellation was not first', win32_red_clipboard_envelope_matches(events[0],
					request, window, .clipboard_write, .cancelled))
				win32_red_add(mut issues, 'ordinary destroy lifecycle event was not second',
					events[1].kind == .lifecycle && events[1].lifecycle.kind == .window_destroyed
					&& events[1].lifecycle.window_id == window && events[0].sequence > 0
					&& events[1].sequence > events[0].sequence)
			}
			win32_red_add(mut issues, 'ordinary destroy left a core pending request',
				app.services.pending.len == 0)
			late := win32_w4_poll_collect(mut app, 4, 'ordinary destroy late', mut issues)
			win32_red_add(mut issues, 'ordinary destroy produced a duplicate late terminal', win32_red_clipboard_events(late,
				request).len == 0)
			win32_red_add(mut issues, 'ordinary destroy late delivery lost global ordering',
				win32_red_events_are_globally_ordered(late))
		}
		return issues
	} $else {
		_ = test_now_ns
		return error('Win32 clipboard destroy-cancel case is unavailable')
	}
}

fn win32_w4_clipboard_purge_fault_case(test_now_ns i64) ![]string {
	$if windows {
		mut app := new_app(backend: .win32)!
		defer {
			C.v_multiwindow_win32_test_modal_set_enable_failure(0)
			app.stop() or {}
		}
		owner := app.create_window(title: 'Win32 clipboard purge-fault owner')!
		modal := app.create_window(
			title: 'Win32 clipboard purge-fault modal'
			owner: owner
			modal: true
		)!
		_ = app.drain_queued_events()!
		backend := win32_red_backend_pointer(app)
		hwnd := win32_red_hwnd(app, modal)!
		sentinel := 'purge failure sentinel 🙂'
		sentinel_wide := sentinel.to_wide()
		mut issues := []string{}
		fixture_ready := C.v_multiwindow_test_win32_set_clipboard(hwnd, sentinel_wide,
			win32_red_utf16_units(sentinel)) == 1
		if !fixture_ready {
			win32_w4_add_infra(mut issues,
				'purge-fault oracle could not install the clipboard sentinel')
		}
		if fixture_ready {
			C.v_multiwindow_win32_service_test_clipboard_configure(backend, test_now_ns, 16)
			request := app.service_set_clipboard_text(modal,
				'must remain purged after modal release failure') or {
				issues << 'purge-fault write was not admitted: ${err.msg()}'
				ServiceRequestId{}
			}
			if request != ServiceRequestId{} {
				admitted := win32_red_core_pending(app, request)
				win32_red_add(mut issues, 'purge-fault write was not retained non-terminal',

					admitted.len == 1 && !admitted[0].terminal)
				win32_red_add(mut issues, 'purge-fault write was not native-pending',
					C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 1)
				attempts := C.v_multiwindow_win32_service_test_clipboard_attempts(backend)
				sequences :=
					C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend)
				allocations :=
					C.v_multiwindow_win32_service_test_clipboard_global_allocations(backend)
				transfers := C.v_multiwindow_win32_service_test_clipboard_global_transfers(backend)
				frees := C.v_multiwindow_win32_service_test_clipboard_global_frees(backend)
				C.v_multiwindow_win32_test_modal_set_enable_failure(1)
				mut destroy_error := ''
				app.destroy_window(modal) or { destroy_error = err.msg() }
				C.v_multiwindow_win32_test_modal_set_enable_failure(0)
				win32_red_add(mut issues, 'modal release failure was not propagated',
					destroy_error.contains(err_capability_unsupported))
				win32_red_add(mut issues,
					'fallible teardown did not purge the native request first',
					C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0)
				win32_red_add(mut issues,
					'fallible teardown attempted or sequenced the purged native request',
					C.v_multiwindow_win32_service_test_clipboard_attempts(backend) == attempts
					&& C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == sequences)
				win32_red_add(mut issues,
					'fallible teardown allocated, transferred, or freed an HGLOBAL',
					C.v_multiwindow_win32_service_test_clipboard_global_allocations(backend) == allocations
					&& C.v_multiwindow_win32_service_test_clipboard_global_transfers(backend) == transfers
					&& C.v_multiwindow_win32_service_test_clipboard_global_frees(backend) == frees
					&& C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0)
				win32_red_add(mut issues, 'fallible teardown mutated the real clipboard sentinel', C.v_multiwindow_test_win32_clipboard_equals(sentinel_wide,
					win32_red_utf16_units(sentinel)) == 1)
				core_terminal := win32_red_core_pending(app, request)
				win32_red_add(mut issues, 'fallible teardown did not retain one core cancellation',

					core_terminal.len == 1 && core_terminal[0].terminal)
				events := app.drain_queued_events() or {
					win32_w4_add_infra(mut issues,
						'purge-fault terminal drain failed: ${err.msg()}')
					[]QueuedEvent{}
				}
				win32_red_add(mut issues, 'purge-fault delivery lost global ordering',
					win32_red_events_are_globally_ordered(events))
				terminals := win32_red_clipboard_events(events, request)
				destroyed := events.filter(it.kind == .lifecycle
					&& it.lifecycle.kind == .window_destroyed && it.lifecycle.window_id == modal)
				win32_red_add(mut issues, 'purge-fault did not publish one cancellation',
					terminals.len == 1
					&& win32_red_clipboard_envelope_matches(terminals[0], request, modal, .clipboard_write, .cancelled))
				win32_red_add(mut issues, 'purge-fault did not publish one destroy lifecycle',
					destroyed.len == 1)
				if terminals.len == 1 && destroyed.len == 1 {
					win32_red_add(mut issues,
						'purge-fault cancellation was not ordered before destroy',
						terminals[0].sequence < destroyed[0].sequence)
				}
				win32_red_add(mut issues, 'purge-fault core terminal survived delivery', win32_red_core_pending(app,
					request).len == 0)
				late := win32_w4_poll_collect(mut app, 4, 'purge-fault late', mut issues)
				win32_red_add(mut issues, 'purge-fault produced a duplicate late terminal', win32_red_clipboard_events(late,
					request).len == 0)
				win32_red_add(mut issues, 'purge-fault late delivery lost global ordering',
					win32_red_events_are_globally_ordered(late))
				win32_red_add(mut issues,
					'late poll attempted or sequenced the purged native request',
					C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0
					&& C.v_multiwindow_win32_service_test_clipboard_attempts(backend) == attempts
					&& C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == sequences)
				win32_red_add(mut issues,
					'late poll changed HGLOBAL allocation/transfer/free ownership',
					C.v_multiwindow_win32_service_test_clipboard_global_allocations(backend) == allocations
					&& C.v_multiwindow_win32_service_test_clipboard_global_transfers(backend) == transfers
					&& C.v_multiwindow_win32_service_test_clipboard_global_frees(backend) == frees
					&& C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0)
				win32_red_add(mut issues, 'late poll mutated the real clipboard sentinel', C.v_multiwindow_test_win32_clipboard_equals(sentinel_wide,
					win32_red_utf16_units(sentinel)) == 1)
			}
		}
		return issues
	} $else {
		_ = test_now_ns
		return error('Win32 clipboard purge-fault case is unavailable')
	}
}

fn win32_w4_clipboard_stop_cancel_case(test_now_ns i64) ![]string {
	$if windows {
		mut app := new_app(backend: .win32)!
		mut stopped := false
		defer {
			if !stopped {
				app.stop() or {}
			}
		}
		window := app.create_window(title: 'Win32 clipboard stop cancel')!
		_ = app.drain_queued_events()!
		backend := win32_red_backend_pointer(app)
		C.v_multiwindow_win32_service_test_clipboard_configure(backend, test_now_ns, 16)
		mut issues := []string{}
		request := app.service_set_clipboard_text(window, 'stop pending write 🙂') or {
			issues << 'stop-cancel write was not admitted: ${err.msg()}'
			ServiceRequestId{}
		}
		if request != ServiceRequestId{} {
			admitted := win32_red_core_pending(app, request)
			win32_red_add(mut issues, 'stop-cancel write was not retained non-terminal',

				admitted.len == 1 && !admitted[0].terminal)
			win32_red_add(mut issues, 'stop-cancel write was not native-pending',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 1)
			win32_red_add(mut issues, 'stop-cancel admission owned an HGLOBAL',
				C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0)
			mut stop_failed := false
			app.stop() or {
				issues << 'stop-cancel stop failed: ${err.msg()}'
				stop_failed = true
			}
			stopped = !stop_failed
			win32_red_add(mut issues, 'stop did not purge the native request',
				C.v_multiwindow_win32_service_test_clipboard_pending_count(backend) == 0)
			win32_red_add(mut issues, 'stop leaked HGLOBAL ownership',
				C.v_multiwindow_win32_service_test_clipboard_owned_globals(backend) == 0)
			win32_red_add(mut issues, 'stop allocated a native clipboard sequence',
				C.v_multiwindow_win32_service_test_clipboard_sequence_allocations(backend) == 0)
			terminal := win32_red_core_pending(app, request)
			win32_red_add(mut issues, 'stop did not retain one core cancellation',

				terminal.len == 1 && terminal[0].terminal)
			events := app.drain_queued_events() or {
				win32_w4_add_infra(mut issues, 'stop-cancel terminal drain failed: ${err.msg()}')
				[]QueuedEvent{}
			}
			win32_red_add(mut issues, 'stop-cancel delivery lost global ordering',
				win32_red_events_are_globally_ordered(events))
			terminals := win32_red_clipboard_events(events, request)
			destroyed := events.filter(it.kind == .lifecycle
				&& it.lifecycle.kind == .window_destroyed && it.lifecycle.window_id == window)
			win32_red_add(mut issues, 'stop did not publish one cancellation', terminals.len == 1
				&& win32_red_clipboard_envelope_matches(terminals[0], request, window, .clipboard_write, .cancelled))
			win32_red_add(mut issues, 'stop did not publish one destroy lifecycle',
				destroyed.len == 1)
			if terminals.len == 1 && destroyed.len == 1 {
				win32_red_add(mut issues, 'stop cancellation was not ordered before destroy',
					terminals[0].sequence < destroyed[0].sequence)
			}
			win32_red_add(mut issues, 'stop left a core pending request',
				app.services.pending.len == 0)
			late := app.drain_queued_events() or {
				win32_w4_add_infra(mut issues, 'stop-cancel late drain failed: ${err.msg()}')
				[]QueuedEvent{}
			}
			win32_red_add(mut issues, 'stop produced a duplicate late terminal', win32_red_clipboard_events(late,
				request).len == 0)
			win32_red_add(mut issues, 'stop late delivery lost global ordering',
				win32_red_events_are_globally_ordered(late))
		}
		return issues
	} $else {
		_ = test_now_ns
		return error('Win32 clipboard stop-cancel case is unavailable')
	}
}

fn test_win32_native_clipboard_occupancy_timeout_failure_and_cancel_red() {
	$if windows {
		eprintln('PACKAGE2_RED_TEST=test_win32_native_clipboard_occupancy_timeout_failure_and_cancel_red')
		eprintln('PACKAGE2_RED_FAMILY=clipboard_occupancy_cancel')
		test_now_ns := i64(20_000_000)
		mut issues := []string{}
		for issue in win32_w4_clipboard_timeout_case(test_now_ns)! {
			issues << issue
		}
		for issue in win32_w4_clipboard_ready_storage_case(test_now_ns)! {
			issues << issue
		}
		for issue in win32_w4_clipboard_late_first_attempt_case(test_now_ns)! {
			issues << issue
		}
		for issue in win32_w4_clipboard_destroy_cancel_case(test_now_ns)! {
			issues << issue
		}
		for issue in win32_w4_clipboard_purge_fault_case(test_now_ns)! {
			issues << issue
		}
		for issue in win32_w4_clipboard_stop_cancel_case(test_now_ns)! {
			issues << issue
		}
		win32_w4_epilogue('clipboard_occupancy_cancel',
			'Win32 clipboard occupancy/cancellation RED', issues)
	}
}

fn win32_red_mouse_release_case(cause string) ![]string {
	mut app := new_app(backend: .win32)!
	mut app_stopped := false
	defer {
		if !app_stopped {
			app.stop() or {}
		}
	}
	first := app.create_window(title: 'Win32 mouse lock first')!
	second := app.create_window(title: 'Win32 mouse lock second')!
	_ = app.drain_queued_events()!
	first_hwnd := win32_red_hwnd(app, first)!
	second_hwnd := win32_red_hwnd(app, second)!
	mut issues := []string{}
	win32_red_add(mut issues, 'first window could not acquire foreground focus',
		C.v_multiwindow_test_win32_establish_foreground_focus(first_hwnd) == 1)
	app.service_set_mouse_lock(first, true) or { issues << 'lock failed: ${err.msg()}' }
	win32_red_poll(mut app, 3)!
	win32_red_add(mut issues, 'Raw Input target is not the locked HWND',
		C.v_multiwindow_test_win32_raw_mouse_registered_for(first_hwnd) == 1
		&& C.v_multiwindow_test_win32_raw_mouse_target() == first_hwnd)
	win32_red_add(mut issues, 'ClipCursor is not bounded to the locked client',
		C.v_multiwindow_test_win32_clip_matches_client(first_hwnd) == 1)
	win32_red_add(mut issues, 'second window inherited first-window mouse lock',
		C.v_multiwindow_test_win32_raw_mouse_registered_for(second_hwnd) == 0)

	match cause {
		'focus' {
			if C.v_multiwindow_test_win32_emit_focus_loss(first_hwnd, second_hwnd) != 1 {
				return error('native WM_KILLFOCUS oracle trigger failed')
			}
			win32_red_poll(mut app, 4)!
		}
		'hide' {
			app.service_hide_window(first) or {
				issues << 'hide release service failed: ${err.msg()}'
			}
			win32_red_poll(mut app, 2)!
		}
		'destroy' {
			app.destroy_window(first)!
			win32_red_poll(mut app, 2)!
		}
		'stop' {
			app.stop()!
			app_stopped = true
		}
		else {}
	}
	win32_red_add(mut issues, '${cause} left Raw Input targeting the released HWND',
		C.v_multiwindow_test_win32_raw_mouse_registered_for(first_hwnd) == 0)
	win32_red_add(mut issues, '${cause} left mouse capture on the released HWND',
		C.v_multiwindow_test_win32_capture() != first_hwnd)
	win32_red_add(mut issues, '${cause} did not release ClipCursor to the virtual screen',
		C.v_multiwindow_test_win32_clip_is_virtual_screen() == 1)
	if cause == 'focus' || cause == 'hide' {
		state := app.service_window_state(first)!
		win32_red_add(mut issues, '${cause} did not publish mouse_locked=off',
			state.mouse_locked == .off)
	}
	if !app_stopped {
		app.stop()!
		app_stopped = true
	}
	return issues
}

fn win32_mouse_focus_cleanup_retry_case() ![]string {
	mut app := new_app(backend: .win32)!
	defer {
		app.stop() or {}
	}
	first := app.create_window(title: 'Win32 mouse cleanup retry first')!
	second := app.create_window(title: 'Win32 mouse cleanup retry second')!
	_ = app.drain_queued_events()!
	first_hwnd := win32_red_hwnd(app, first)!
	second_hwnd := win32_red_hwnd(app, second)!
	mut issues := []string{}
	monitor_count := C.v_multiwindow_test_win32_monitor_enumeration_capture()
	win32_red_add(mut issues, 'retry fixture could not capture the monitor snapshot',
		monitor_count > 0)
	defer {
		C.v_multiwindow_test_win32_monitor_enumeration_reset()
	}
	win32_red_add(mut issues, 'retry fixture could not focus the first HWND',
		C.v_multiwindow_test_win32_establish_foreground_focus(first_hwnd) == 1)
	app.service_set_mouse_lock(first, true) or { issues << 'retry lock failed: ${err.msg()}' }
	C.v_multiwindow_win32_service_test_focus_cleanup_failures(2)
	if C.v_multiwindow_test_win32_emit_focus_loss(first_hwnd, second_hwnd) != 1 {
		return error('retry WM_KILLFOCUS oracle trigger failed')
	}
	_ = app.backend.win32.poll_queued_events()!
	first_index := app.backend.win32.window_record_index(first) or {
		return error(err_window_not_found)
	}
	first_record := app.backend.win32.windows[first_index]
	win32_red_add(mut issues, 'focus cleanup failure did not remain pending',

		first_record.mouse_focus_cleanup_pending && first_record.mouse_focus_cleanup_reported)
	win32_red_add(mut issues, 'focus cleanup failure was not retained',
		app.backend.win32.native_input_release_terminal() == err_capability_unsupported
		&& app.backend.win32.take_poll_error() == err_capability_unsupported)
	mut falsely_off := false
	if failed_state := app.service_window_state(first) {
		falsely_off = failed_state.mouse_locked == .off
	} else {
		win32_red_add(mut issues, 'failed cleanup returned the wrong observation error',
			err.msg() == err_capability_unsupported)
	}
	win32_red_add(mut issues, 'failed focus cleanup falsely published mouse_locked=off',
		!falsely_off)
	win32_red_add(mut issues, 'failed focus cleanup partially released native ownership',
		C.v_multiwindow_test_win32_raw_mouse_registered_for(first_hwnd) == 1
		&& C.v_multiwindow_test_win32_clip_matches_client(first_hwnd) == 1)

	assert C.v_multiwindow_test_win32_emit_focus_loss(first_hwnd, second_hwnd) == 1
	win32_red_add(mut issues, 'repeated WM_KILLFOCUS cleared the pending cleanup debt',
		first_record.mouse_focus_cleanup_pending && first_record.mouse_focus_cleanup_reported
		&& app.backend.win32.native_input_release_terminal() == err_capability_unsupported)
	win32_red_add(mut issues, 'repeated WM_KILLFOCUS bypassed the owner-thread retry',
		C.v_multiwindow_test_win32_raw_mouse_registered_for(first_hwnd) == 1
		&& C.v_multiwindow_test_win32_clip_matches_client(first_hwnd) == 1)
	assert C.v_multiwindow_test_win32_monitor_enumeration_use_info_failure() == 1
	assert C.v_multiwindow_test_win32_emit_display_change(first_hwnd) == 1
	mut monitor_error := ''
	if _ := app.backend.win32.poll_queued_events() {
		issues << 'monitor failure concurrent with focus cleanup unexpectedly succeeded'
	} else {
		monitor_error = err.msg()
	}
	win32_red_add(mut issues, 'monitor failure did not reach the retry poll', monitor_error != '')
	win32_red_add(mut issues, 'focus cleanup retry did not resolve the retained error',
		!first_record.mouse_focus_cleanup_pending
		&& app.backend.win32.native_input_release_terminal() == '')
	win32_red_add(mut issues, 'focus cleanup retry left native mouse ownership',
		C.v_multiwindow_test_win32_raw_mouse_registered_for(first_hwnd) == 0
		&& C.v_multiwindow_test_win32_clip_is_virtual_screen() == 1)
	win32_red_add(mut issues, 'focus cleanup retry did not publish mouse_locked=off',
		app.service_window_state(first)!.mouse_locked == .off)
	C.v_multiwindow_test_win32_monitor_enumeration_reset()
	_ = app.backend.win32.poll_queued_events()!

	assert C.v_multiwindow_test_win32_emit_focus_loss(first_hwnd, second_hwnd) == 1
	_ = app.backend.win32.poll_queued_events()!
	win32_red_add(mut issues, 'idempotent focus cleanup created a retained error',
		app.backend.win32.native_input_release_terminal() == ''
		&& app.backend.win32.take_poll_error() == '')
	win32_red_add(mut issues, 'retry fixture could not refocus the first HWND',
		C.v_multiwindow_test_win32_establish_foreground_focus(first_hwnd) == 1)
	app.service_set_mouse_lock(first, true) or { issues << 'reacquire failed: ${err.msg()}' }
	win32_red_add(mut issues, 'mouse lock could not be reacquired after cleanup',
		C.v_multiwindow_test_win32_raw_mouse_registered_for(first_hwnd) == 1
		&& C.v_multiwindow_test_win32_clip_matches_client(first_hwnd) == 1)
	assert C.v_multiwindow_test_win32_emit_focus_loss(first_hwnd, second_hwnd) == 1
	_ = app.backend.win32.poll_queued_events()!
	win32_red_add(mut issues, 'reacquired mouse lock was not released exactly once',
		C.v_multiwindow_test_win32_raw_mouse_registered_for(first_hwnd) == 0
		&& C.v_multiwindow_test_win32_clip_is_virtual_screen() == 1
		&& app.service_window_state(first)!.mouse_locked == .off)
	return issues
}

fn test_win32_native_raw_input_clipcursor_release_and_two_window_isolation() {
	$if windows {
		mut issues := []string{}
		for cause in ['focus', 'hide', 'destroy', 'stop'] {
			for issue in win32_red_mouse_release_case(cause)! {
				issues << '${cause}: ${issue}'
			}
		}
		for issue in win32_mouse_focus_cleanup_retry_case()! {
			issues << 'focus retry: ${issue}'
		}
		assert issues.len == 0, 'Win32 Raw Input/ClipCursor isolation:\n${issues.join('\n')}'
	}
}

fn test_win32_native_conditional_titlebar_dwm_and_style_oracles_red() {
	$if windows {
		eprintln('PACKAGE2_RED_TEST=test_win32_native_conditional_titlebar_dwm_and_style_oracles_red')
		eprintln('PACKAGE2_RED_FAMILY=titlebar_dwm_style')
		mut app := new_app(backend: .win32)!
		defer {
			app.stop() or {}
		}
		decorated := app.create_window(title: 'Win32 DWM titlebar')!
		borderless := app.create_window(title: 'Win32 borderless', borderless: true)!
		decorated_hwnd := win32_red_hwnd(app, decorated)!
		borderless_hwnd := win32_red_hwnd(app, borderless)!
		mut issues := []string{}
		decorated_capability := app.service_operation_capability(decorated, .titlebar_appearance)!
		borderless_capability := app.service_operation_capability(borderless, .titlebar_appearance)!
		win32_red_add(mut issues, 'decorated titlebar capability is not conditional', win32_red_capability_matches(decorated_capability,
			.conditional, false, false, false))
		win32_red_add(mut issues, 'borderless titlebar capability is not unsupported',
			borderless_capability.support == .unsupported)
		win32_red_add(mut issues, 'decorated HWND lacks WS_CAPTION',
			C.v_multiwindow_test_win32_style(decorated_hwnd) & win32_red_ws_caption != 0)
		win32_red_add(mut issues, 'borderless HWND unexpectedly has WS_CAPTION',
			C.v_multiwindow_test_win32_style(borderless_hwnd) & win32_red_ws_caption == 0)
		_ = C.v_multiwindow_test_win32_ex_style(decorated_hwnd)

		mut original_dark := 0
		dwm_observable := C.v_multiwindow_test_win32_dwm_dark(decorated_hwnd, &original_dark) == 1
		app.service_set_titlebar_appearance(decorated, .dark) or {
			if dwm_observable {
				issues << 'DWM dark titlebar failed: ${err.msg()}'
			}
		}
		if dwm_observable {
			mut dark := 0
			assert C.v_multiwindow_test_win32_dwm_dark(decorated_hwnd, &dark) == 1, 'DWM dark-titlebar oracle query failed after admission'

			win32_red_add(mut issues, 'DWM did not observe dark titlebar', dark == 1)
		}
		app.service_set_titlebar_appearance(decorated, .light) or {
			if dwm_observable {
				issues << 'DWM light titlebar failed: ${err.msg()}'
			}
		}
		if dwm_observable {
			mut light := 1
			assert C.v_multiwindow_test_win32_dwm_dark(decorated_hwnd, &light) == 1, 'DWM light-titlebar oracle query failed after admission'

			win32_red_add(mut issues, 'DWM did not observe light titlebar', light == 0)
		}
		app.service_set_titlebar_appearance(decorated, .system) or {
			if dwm_observable {
				issues << 'DWM system titlebar restore failed: ${err.msg()}'
			}
		}
		if dwm_observable {
			mut restored := -1
			assert C.v_multiwindow_test_win32_dwm_dark(decorated_hwnd, &restored) == 1, 'DWM system-titlebar oracle query failed after admission'

			win32_red_add(mut issues, 'DWM system titlebar did not restore prior state',
				restored == original_dark)
		}
		if issues.len > 0 {
			eprintln('PACKAGE2_RED_TERMINAL=behavioral_red:titlebar_dwm_style')
		}
		assert issues.len == 0, 'Win32 conditional titlebar RED:\n${issues.join('\n')}'
	}
}
