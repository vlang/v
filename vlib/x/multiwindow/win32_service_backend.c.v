module multiwindow

const win32_service_ok = 1
const win32_service_unavailable = 0
const win32_service_wrong_thread = -1
const win32_service_invalid = -2
const win32_clipboard_attempt_failed = -1
const win32_clipboard_attempt_retry = 0
const win32_clipboard_attempt_ready = 1
const win32_clipboard_attempt_capacity = -2
const win32_clipboard_convert_ready = 1
const win32_clipboard_convert_capacity = -1
const win32_clipboard_max_bytes = 16 * 1024 * 1024
const win32_clipboard_max_pending_operations = 64
const win32_clipboard_max_pending_bytes = win32_clipboard_max_bytes
const win32_clipboard_timeout_ns = i64(2_000_000_000)

struct Win32ClipboardPending {
	request        ServiceRequestId
	window         WindowId
	hwnd           voidptr
	operation      ServiceOperation
	write_utf16    []u16
	reserved_bytes usize
mut:
	deadline_ns i64
}

struct Win32ClipboardRetainedCharge {
	request ServiceRequestId
	window  WindowId
	bytes   usize
mut:
	claimed_by_app bool
}

struct Win32ServiceRawMonitor {
	native_id   u64
	name        string
	x           int
	y           int
	width       int
	height      int
	work_x      int
	work_y      int
	work_width  int
	work_height int
	dpi         u32
	primary     int
}

struct Win32ServiceMonitorRecord {
	native_id  u64
	name       string
	slot       int
	generation u32
	available  bool
}

struct Win32ServiceMonitorPlan {
	records  []Win32ServiceMonitorRecord
	monitors []ServiceMonitorInfo
}

struct Win32ServiceMetricsObservation {
	event       QueuedEvent
	monitor_ids []ServiceMonitorId
	dpi         u32
}

struct Win32ServiceRefreshObservation {
	index       int
	sequence    u64
	publish     bool
	observation Win32ServiceMetricsObservation
}

$if windows {
	#insert "@VMODROOT/vlib/x/multiwindow/win32_service_native.h"
	$if test {
		#flag windows -DV_MULTIWINDOW_WIN32_CLIPBOARD_TEST_BACKEND_IMPLEMENTATION
		#include "@VMODROOT/vlib/x/multiwindow/testdata/win32_nonreadback_test_oracle.h"
	}

	fn C.v_multiwindow_win32_service_authority(state voidptr) int
	fn C.v_multiwindow_win32_service_create(hwnd voidptr, record_data voidptr, initial_fullscreen int, width int, height int, resizable int, borderless int) voidptr
	fn C.v_multiwindow_win32_service_release(state voidptr) int
	fn C.v_multiwindow_win32_service_window_state_with_mouse_lock(state voidptr, out_mapping &int, out_visibility &int, out_active &int, out_focused &int, out_minimized &int, out_maximized &int, out_fullscreen &int, out_mouse_locked &int, out_position_known &int, out_x &int, out_y &int) int
	fn C.v_multiwindow_win32_service_set_mouse_lock(state voidptr, enabled int) int
	fn C.v_multiwindow_win32_service_focus_lost(state voidptr) int
	fn C.v_multiwindow_win32_service_mouse_delivery_active(state voidptr) int
	fn C.v_multiwindow_win32_service_disable_mouse_delivery(state voidptr) int
	fn C.v_multiwindow_win32_service_prepare_window_teardown(state voidptr) int
	fn C.v_multiwindow_win32_service_teardown_prepared(state voidptr) int
	fn C.v_multiwindow_win32_service_show_window(state voidptr) int
	fn C.v_multiwindow_win32_service_hide_window(state voidptr) int
	fn C.v_multiwindow_win32_service_focus_window(state voidptr) int
	fn C.v_multiwindow_win32_service_raise_window(state voidptr) int
	fn C.v_multiwindow_win32_service_set_window_position(state voidptr, x int, y int) int
	fn C.v_multiwindow_win32_service_minimize_window(state voidptr) int
	fn C.v_multiwindow_win32_service_maximize_window(state voidptr) int
	fn C.v_multiwindow_win32_service_restore_window(state voidptr) int
	fn C.v_multiwindow_win32_service_set_fullscreen(state voidptr, enabled int) int
	fn C.v_multiwindow_win32_service_fullscreen_known(state voidptr) int
	fn C.v_multiwindow_win32_service_native_window(state voidptr) voidptr
	fn C.v_multiwindow_win32_service_monitor_snapshot_new() voidptr
	fn C.v_multiwindow_win32_service_monitor_snapshot_free(snapshot voidptr)
	fn C.v_multiwindow_win32_service_monitor_snapshot_count(snapshot voidptr) int
	fn C.v_multiwindow_win32_service_monitor_snapshot_native_id(snapshot voidptr, index int) u64
	fn C.v_multiwindow_win32_service_monitor_snapshot_name(snapshot voidptr, index int) &u16
	fn C.v_multiwindow_win32_service_monitor_snapshot_info(snapshot voidptr, index int, x &int, y &int, width &int, height &int, work_x &int, work_y &int, work_width &int, work_height &int, dpi &u32, primary &int) int
	fn C.v_multiwindow_win32_service_window_monitor(hwnd voidptr) u64
	fn C.v_multiwindow_win32_service_window_dpi(hwnd voidptr) u32
	fn C.v_multiwindow_win32_clipboard_now_ns() i64
	fn C.v_multiwindow_win32_clipboard_utf8_to_utf16(text &char, text_bytes usize, output &u16, output_units usize, out_units &usize) int
	fn C.v_multiwindow_win32_clipboard_write(owner voidptr, text &u16, units usize) int
	fn C.v_multiwindow_win32_clipboard_read(owner voidptr, out_text &voidptr, out_text_bytes &usize) int
	fn C.v_multiwindow_win32_clipboard_text_free(text voidptr)

	$if test {
		fn C.v_multiwindow_win32_service_test_focus_cleanup_failures(count int)
		fn C.v_multiwindow_win32_clipboard_now_for_test(backend voidptr, real_now_ns i64) i64
		fn C.v_multiwindow_win32_clipboard_write_for_test(backend voidptr, request_app u64, request_serial u64, owner voidptr, text &u16, units usize) int
		fn C.v_multiwindow_win32_clipboard_read_for_test(backend voidptr, request_app u64, request_serial u64, owner voidptr, out_text &voidptr, out_text_bytes &usize) int
		fn C.v_multiwindow_win32_clipboard_record_sequence_for_test(backend voidptr)
	}
}

struct Win32ServiceRawWindowState {
	mapping        int
	visibility     int
	active         int
	focused        int
	minimized      int
	maximized      int
	fullscreen     int
	mouse_locked   int
	position_known int
	x              int
	y              int
}

fn win32_service_observed_bool(value int) ServiceObservedBool {
	return match value {
		1 { .off }
		2 { .on }
		else { .unknown }
	}
}

fn win32_service_mapping(value int) ServiceMappingState {
	return match value {
		1 { .unmapped }
		2 { .mapped }
		else { .unknown }
	}
}

fn win32_service_visibility(value int) ServiceVisibilityState {
	return match value {
		1 { .hidden }
		2 { .visible }
		3 { .occluded }
		else { .unknown }
	}
}

fn win32_service_monitor_info(raw Win32ServiceRawMonitor, record Win32ServiceMonitorRecord, app_instance u64) ServiceMonitorInfo {
	return ServiceMonitorInfo{
		native_key: ServiceMonitorNativeKey{
			kind: .win32_device
			text: raw.name
		}
		id:         ServiceMonitorId{
			app_instance: app_instance
			slot:         record.slot
			generation:   record.generation
		}
		name:       raw.name
		geometry:   ServiceKnownRect{
			known: raw.width > 0 && raw.height > 0
			value: ServiceRect{
				x:      raw.x
				y:      raw.y
				width:  raw.width
				height: raw.height
			}
		}
		work_area:  ServiceKnownRect{
			known: raw.work_width > 0 && raw.work_height > 0
			value: ServiceRect{
				x:      raw.work_x
				y:      raw.work_y
				width:  raw.work_width
				height: raw.work_height
			}
		}
		scale:      ServiceKnownScale{
			known: raw.dpi > 0
			value: if raw.dpi > 0 { f32(raw.dpi) / 96.0 } else { f32(0) }
		}
		primary:    if raw.primary != 0 { .on } else { .off }
		available:  true
	}
}

fn win32_reconcile_service_monitors(mut records []Win32ServiceMonitorRecord, snapshot []Win32ServiceRawMonitor, app_instance u64) []ServiceMonitorInfo {
	mut seen := []bool{len: records.len}
	mut slots := []int{len: snapshot.len, init: -1}
	mut monitors := []ServiceMonitorInfo{cap: snapshot.len}
	for snapshot_index, raw in snapshot {
		if raw.native_id == 0 || raw.name == '' {
			continue
		}
		for index, record in records {
			if !seen[index] && record.available && record.name == raw.name {
				slots[snapshot_index] = index
				seen[index] = true
				break
			}
		}
	}
	for snapshot_index, raw in snapshot {
		if slots[snapshot_index] >= 0 || raw.native_id == 0 || raw.name == '' {
			continue
		}
		for index, record in records {
			if !seen[index] && !record.available && record.name == raw.name
				&& record.generation < u32(0xffffffff) {
				slots[snapshot_index] = index
				seen[index] = true
				break
			}
		}
	}
	for snapshot_index, raw in snapshot {
		if slots[snapshot_index] >= 0 || raw.native_id == 0 || raw.name == '' {
			continue
		}
		for index, record in records {
			if !seen[index] && !record.available && record.generation < u32(0xffffffff) {
				slots[snapshot_index] = index
				seen[index] = true
				break
			}
		}
	}
	for snapshot_index, raw in snapshot {
		if raw.native_id == 0 || raw.name == '' {
			continue
		}
		mut slot := slots[snapshot_index]
		if slot < 0 {
			slot = records.len
			records << Win32ServiceMonitorRecord{
				native_id:  raw.native_id
				name:       raw.name
				slot:       slot
				generation: 1
				available:  true
			}
			seen << true
		} else {
			generation := if records[slot].available {
				records[slot].generation
			} else {
				records[slot].generation + 1
			}
			records[slot] = Win32ServiceMonitorRecord{
				native_id:  raw.native_id
				name:       raw.name
				slot:       slot
				generation: generation
				available:  true
			}
			seen[slot] = true
		}
		monitors << win32_service_monitor_info(raw, records[slot], app_instance)
	}
	for index, record in records {
		if index < seen.len && !seen[index] && record.available {
			records[index] = Win32ServiceMonitorRecord{
				...record
				available: false
			}
		}
	}
	return monitors
}

fn win32_service_raw_monitor_snapshot_valid(snapshot []Win32ServiceRawMonitor) bool {
	for index, raw in snapshot {
		if raw.native_id == 0 || raw.name == '' {
			return false
		}
		for previous in 0 .. index {
			if snapshot[previous].native_id == raw.native_id || snapshot[previous].name == raw.name {
				return false
			}
		}
	}
	return true
}

fn win32_service_raw_monitor_snapshots_equal(left []Win32ServiceRawMonitor, right []Win32ServiceRawMonitor) bool {
	if left.len != right.len {
		return false
	}
	mut right_indices := map[u64]int{}
	for index, monitor in right {
		if monitor.native_id in right_indices {
			return false
		}
		right_indices[monitor.native_id] = index
	}
	for monitor in left {
		other_index := right_indices[monitor.native_id] or { return false }
		other := right[other_index]
		if monitor.native_id != other.native_id || monitor.name != other.name
			|| monitor.x != other.x || monitor.y != other.y || monitor.width != other.width
			|| monitor.height != other.height || monitor.work_x != other.work_x
			|| monitor.work_y != other.work_y || monitor.work_width != other.work_width
			|| monitor.work_height != other.work_height || monitor.dpi != other.dpi
			|| monitor.primary != other.primary {
			return false
		}
	}
	return true
}

fn win32_plan_service_monitors(records []Win32ServiceMonitorRecord, snapshot []Win32ServiceRawMonitor, app_instance u64) !Win32ServiceMonitorPlan {
	if !win32_service_raw_monitor_snapshot_valid(snapshot) {
		return error(err_capability_unsupported)
	}
	mut staged_records := records.clone()
	monitors := win32_reconcile_service_monitors(mut staged_records, snapshot, app_instance)
	if !service_monitor_snapshot_identity_valid(monitors, .win32, app_instance) {
		return error(err_capability_unsupported)
	}
	return Win32ServiceMonitorPlan{
		records:  staged_records
		monitors: monitors
	}
}

fn win32_service_monitor_ids_for_native(records []Win32ServiceMonitorRecord, native_id u64, app_instance u64) []ServiceMonitorId {
	if native_id == 0 || app_instance == 0 {
		return []ServiceMonitorId{}
	}
	for record in records {
		if record.available && record.native_id == native_id {
			return [
				ServiceMonitorId{
					app_instance: app_instance
					slot:         record.slot
					generation:   record.generation
				},
			]
		}
	}
	return []ServiceMonitorId{}
}

fn win32_service_raw_monitor_snapshot() ![]Win32ServiceRawMonitor {
	$if windows {
		snapshot := C.v_multiwindow_win32_service_monitor_snapshot_new()
		if snapshot == unsafe { nil } {
			return error(err_capability_unsupported)
		}
		defer {
			C.v_multiwindow_win32_service_monitor_snapshot_free(snapshot)
		}
		count := C.v_multiwindow_win32_service_monitor_snapshot_count(snapshot)
		if count < 0 {
			return error(err_capability_unsupported)
		}
		mut monitors := []Win32ServiceRawMonitor{cap: count}
		for index in 0 .. count {
			name_pointer := C.v_multiwindow_win32_service_monitor_snapshot_name(snapshot, index)
			native_id := C.v_multiwindow_win32_service_monitor_snapshot_native_id(snapshot, index)
			if name_pointer == unsafe { nil } || native_id == 0 {
				return error(err_capability_unsupported)
			}
			mut raw := Win32ServiceRawMonitor{
				native_id: native_id
				name:      unsafe { string_from_wide(name_pointer) }
			}
			if raw.name == ''
				|| C.v_multiwindow_win32_service_monitor_snapshot_info(snapshot, index, &raw.x, &raw.y, &raw.width, &raw.height, &raw.work_x, &raw.work_y, &raw.work_width, &raw.work_height, &raw.dpi, &raw.primary) == 0 {
				return error(err_capability_unsupported)
			}
			monitors << raw
		}
		return monitors
	} $else {
		return error(err_backend_unsupported)
	}
}

fn win32_service_window_monitor(hwnd voidptr) u64 {
	$if windows {
		return C.v_multiwindow_win32_service_window_monitor(hwnd)
	} $else {
		_ = hwnd
		return 0
	}
}

fn win32_service_result(result int) ! {
	match result {
		win32_service_ok {
			return
		}
		win32_service_wrong_thread {
			return error(err_owner_thread_required)
		}
		win32_service_invalid {
			return error(err_window_not_found)
		}
		else {
			return error(err_capability_unsupported)
		}
	}
}

fn win32_window_config_with_fullscreen(config WindowConfig, fullscreen bool) WindowConfig {
	return WindowConfig{
		title:           config.title
		width:           config.width
		height:          config.height
		min_width:       config.min_width
		min_height:      config.min_height
		resizable:       config.resizable
		visible:         config.visible
		high_dpi:        config.high_dpi
		borderless:      config.borderless
		fullscreen:      fullscreen
		sample_count:    config.sample_count
		redraw_mode:     config.redraw_mode
		owner:           config.owner
		modal:           config.modal
		render_workload: config.render_workload
	}
}

fn (backend &Win32Backend) ensure_service_window(id WindowId) !int {
	$if windows {
		if backend.native_operations == unsafe { nil }
			|| !backend.native_operations.owner_thread_is_current() {
			return error(err_owner_thread_required)
		}
		if !backend.started {
			return error(err_backend_unsupported)
		}
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		record := backend.windows[index]
		if record.destroyed || record.hwnd == unsafe { nil }
			|| record.service_state == unsafe { nil } {
			return error(err_window_not_found)
		}
		win32_service_result(C.v_multiwindow_win32_service_authority(record.service_state))!
		return index
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn win32_clipboard_deadline(now_ns i64) i64 {
	if now_ns > i64(0x7fffffffffffffff) - win32_clipboard_timeout_ns {
		return i64(0x7fffffffffffffff)
	}
	return now_ns + win32_clipboard_timeout_ns
}

fn (backend &Win32Backend) clipboard_now_ns() i64 {
	$if windows {
		real_now_ns := C.v_multiwindow_win32_clipboard_now_ns()
		$if test {
			return C.v_multiwindow_win32_clipboard_now_for_test(voidptr(backend), real_now_ns)
		} $else {
			return real_now_ns
		}
	} $else {
		return 0
	}
}

fn win32_clipboard_utf16(text string) ![]u16 {
	$if windows {
		mut units := usize(0)
		query := C.v_multiwindow_win32_clipboard_utf8_to_utf16(&char(text.str), usize(text.len),
			unsafe { nil }, 0, &units)
		if query == win32_clipboard_convert_capacity {
			return error(err_clipboard_capacity)
		}
		if query != win32_clipboard_convert_ready || units == 0
			|| units > usize(win32_clipboard_max_bytes / 2) {
			return error(err_capability_unsupported)
		}
		mut wide := []u16{len: int(units)}
		converted := C.v_multiwindow_win32_clipboard_utf8_to_utf16(&char(text.str),
			usize(text.len), wide.data, units, &units)
		if converted == win32_clipboard_convert_capacity {
			return error(err_clipboard_capacity)
		}
		if converted != win32_clipboard_convert_ready || units != usize(wide.len) {
			return error(err_capability_unsupported)
		}
		return wide
	} $else {
		_ = text
		return error(err_backend_unsupported)
	}
}

fn (backend &Win32Backend) clipboard_can_admit(reserved_bytes usize) bool {
	return backend.clipboard_pending.len < win32_clipboard_max_pending_operations
		&& reserved_bytes <= usize(win32_clipboard_max_pending_bytes)
		&& backend.clipboard_pending_bytes <= usize(win32_clipboard_max_pending_bytes) - reserved_bytes
}

fn (mut backend Win32Backend) release_clipboard_bytes(bytes usize) {
	if bytes <= backend.clipboard_pending_bytes {
		backend.clipboard_pending_bytes -= bytes
	}
}

fn (mut backend Win32Backend) claim_clipboard_terminal_storage(event ServiceEvent) {
	if event.kind != .clipboard || event.operation != .clipboard_read
		|| event.clipboard.status != .ready {
		return
	}
	for index, charge in backend.clipboard_retained {
		if charge.request == event.clipboard.id && charge.window == event.window
			&& charge.window == event.clipboard.window && !charge.claimed_by_app {
			backend.clipboard_retained[index].claimed_by_app = true
			return
		}
	}
}

fn (mut backend Win32Backend) discard_unclaimed_clipboard_terminal_storage(event ServiceEvent) {
	if event.kind != .clipboard || event.operation != .clipboard_read
		|| event.clipboard.status != .ready {
		return
	}
	for index, charge in backend.clipboard_retained {
		if charge.request == event.clipboard.id && charge.window == event.window
			&& charge.window == event.clipboard.window && !charge.claimed_by_app {
			backend.release_clipboard_bytes(charge.bytes)
			backend.clipboard_retained.delete(index)
			return
		}
	}
}

fn (mut backend Win32Backend) release_claimed_clipboard_terminal_storage(event ServiceEvent) {
	if event.kind != .clipboard || event.operation != .clipboard_read
		|| event.clipboard.status != .ready {
		return
	}
	for index, charge in backend.clipboard_retained {
		if charge.request == event.clipboard.id && charge.window == event.window
			&& charge.window == event.clipboard.window && charge.claimed_by_app {
			backend.release_clipboard_bytes(charge.bytes)
			backend.clipboard_retained.delete(index)
			return
		}
	}
}

fn (mut backend Win32Backend) admit_clipboard_request(request ServiceRequestId, window WindowId, hwnd voidptr, operation ServiceOperation, write_utf16 []u16) ! {
	if operation !in [.clipboard_read, .clipboard_write] {
		return error(err_capability_unsupported)
	}
	reserved_bytes := if operation == .clipboard_write {
		usize(write_utf16.len) * usize(2)
	} else {
		usize(win32_clipboard_max_pending_bytes)
	}
	if !backend.clipboard_can_admit(reserved_bytes) {
		return error(err_clipboard_capacity)
	}
	deadline_ns := if backend.clipboard_pending.len == 0 {
		win32_clipboard_deadline(backend.clipboard_now_ns())
	} else {
		i64(0)
	}
	backend.clipboard_pending << Win32ClipboardPending{
		request:        request
		window:         window
		hwnd:           hwnd
		operation:      operation
		write_utf16:    write_utf16
		reserved_bytes: reserved_bytes
		deadline_ns:    deadline_ns
	}
	backend.clipboard_pending_bytes += reserved_bytes
}

fn (mut backend Win32Backend) finish_clipboard_head(status ServiceStatus, text string, message string) Win32NativeQueuedEvent {
	pending := backend.clipboard_pending[0]
	backend.release_clipboard_bytes(pending.reserved_bytes)
	backend.clipboard_pending.delete(0)
	mut terminal_status := status
	mut terminal_text := text
	mut terminal_message := message
	if status == .ready && pending.operation == .clipboard_read {
		read_charge := usize(text.len) + usize(1)
		if read_charge <= usize(win32_clipboard_max_pending_bytes)
			&& backend.clipboard_can_admit(read_charge) {
			backend.clipboard_retained << Win32ClipboardRetainedCharge{
				request: pending.request
				window:  pending.window
				bytes:   read_charge
			}
			backend.clipboard_pending_bytes += read_charge
		} else {
			terminal_status = .failed
			terminal_text = ''
			terminal_message = err_clipboard_capacity
		}
	}
	if backend.clipboard_pending.len > 0 {
		backend.clipboard_pending[0].deadline_ns =
			win32_clipboard_deadline(backend.clipboard_now_ns())
	}
	mut sequence := u64(0)
	$if windows {
		sequence = C.v_multiwindow_win32_next_event_sequence()
		$if test {
			C.v_multiwindow_win32_clipboard_record_sequence_for_test(voidptr(backend))
		}
	}
	return Win32NativeQueuedEvent{
		sequence: sequence
		event:    queued_service_event(ServiceEvent{
			kind:      .clipboard
			window:    pending.window
			operation: pending.operation
			clipboard: ServiceClipboardResult{
				id:     pending.request
				window: pending.window
				status: terminal_status
				text:   terminal_text.clone()
				error:  terminal_message
			}
		})
	}
}

fn (mut backend Win32Backend) collect_clipboard_events() []Win32NativeQueuedEvent {
	$if windows {
		if backend.clipboard_pending.len == 0 {
			return []Win32NativeQueuedEvent{}
		}
		pending := backend.clipboard_pending[0]
		mut status := win32_clipboard_attempt_failed
		mut text := ''
		if pending.operation == .clipboard_write {
			$if test {
				status = C.v_multiwindow_win32_clipboard_write_for_test(voidptr(backend),
					pending.request.app_instance, pending.request.serial, pending.hwnd,
					pending.write_utf16.data, usize(pending.write_utf16.len))
			} $else {
				status = C.v_multiwindow_win32_clipboard_write(pending.hwnd,
					pending.write_utf16.data, usize(pending.write_utf16.len))
			}
		} else {
			mut native_text := voidptr(unsafe { nil })
			mut text_bytes := usize(0)
			$if test {
				status = C.v_multiwindow_win32_clipboard_read_for_test(voidptr(backend),
					pending.request.app_instance, pending.request.serial, pending.hwnd,
					&native_text, &text_bytes)
			} $else {
				status = C.v_multiwindow_win32_clipboard_read(pending.hwnd, &native_text,
					&text_bytes)
			}
			if status == win32_clipboard_attempt_ready {
				if native_text == unsafe { nil }
					|| text_bytes > usize(win32_clipboard_max_bytes - 1) {
					status = win32_clipboard_attempt_failed
				} else {
					text = unsafe { tos(native_text, int(text_bytes)).clone() }
				}
			}
			if native_text != unsafe { nil } {
				C.v_multiwindow_win32_clipboard_text_free(native_text)
			}
		}
		if status == win32_clipboard_attempt_retry {
			now_ns := backend.clipboard_now_ns()
			if pending.deadline_ns != 0 && now_ns >= pending.deadline_ns {
				return [backend.finish_clipboard_head(.failed, '', err_clipboard_timeout)]
			}
			return []Win32NativeQueuedEvent{}
		}
		if status == win32_clipboard_attempt_ready {
			return [backend.finish_clipboard_head(.ready, text, '')]
		}
		if status == win32_clipboard_attempt_capacity {
			return [backend.finish_clipboard_head(.failed, '', err_clipboard_capacity)]
		}
		return [backend.finish_clipboard_head(.failed, '', err_capability_unsupported)]
	} $else {
		return []Win32NativeQueuedEvent{}
	}
}

fn (mut backend Win32Backend) purge_clipboard_window(id WindowId) {
	if backend.clipboard_pending.len == 0 {
		return
	}
	head_removed := backend.clipboard_pending[0].window == id
	mut retained := []Win32ClipboardPending{cap: backend.clipboard_pending.len}
	mut removed_bytes := usize(0)
	for pending in backend.clipboard_pending {
		if pending.window == id {
			removed_bytes += pending.reserved_bytes
			continue
		}
		retained << pending
	}
	backend.clipboard_pending = retained
	backend.release_clipboard_bytes(removed_bytes)
	if head_removed && backend.clipboard_pending.len > 0 {
		backend.clipboard_pending[0].deadline_ns =
			win32_clipboard_deadline(backend.clipboard_now_ns())
	}
}

fn (mut backend Win32Backend) purge_all_clipboard_requests() {
	mut removed_bytes := usize(0)
	for pending in backend.clipboard_pending {
		removed_bytes += pending.reserved_bytes
	}
	backend.clipboard_pending.clear()
	backend.release_clipboard_bytes(removed_bytes)
}

fn (backend &Win32Backend) service_operation_capability(id WindowId, operation ServiceOperation) ServiceOperationCapability {
	index := backend.ensure_service_window(id) or { return ServiceOperationCapability{} }
	record := backend.windows[index]
	mut fullscreen_known := false
	$if windows {
		fullscreen_known = C.v_multiwindow_win32_service_fullscreen_known(record.service_state) != 0
	}
	return match operation {
		.show, .hide, .raise, .position, .minimize {
			ServiceOperationCapability{
				support:          .available
				state_observable: true
			}
		}
		.fullscreen, .restore {
			ServiceOperationCapability{
				support:          if fullscreen_known { .available } else { .unsupported }
				state_observable: true
			}
		}
		.maximize {
			ServiceOperationCapability{
				support:          if record.config.resizable && !record.config.borderless {
					.available
				} else {
					.unsupported
				}
				state_observable: record.config.resizable && !record.config.borderless
			}
		}
		.focus {
			ServiceOperationCapability{
				support:              .conditional
				requires_user_action: true
				state_observable:     true
			}
		}
		.native_borrow {
			ServiceOperationCapability{
				support: .available
			}
		}
		.clipboard_read, .clipboard_write {
			ServiceOperationCapability{
				support:      .available
				asynchronous: true
			}
		}
		.mouse_lock {
			ServiceOperationCapability{
				support:          .conditional
				state_observable: true
			}
		}
		else {
			ServiceOperationCapability{}
		}
	}
}

fn (backend &Win32Backend) service_raw_window_state(index int) !Win32ServiceRawWindowState {
	$if windows {
		if index < 0 || index >= backend.windows.len {
			return error(err_window_not_found)
		}
		mut raw := Win32ServiceRawWindowState{}
		result := C.v_multiwindow_win32_service_window_state_with_mouse_lock(backend.windows[index].service_state,
			&raw.mapping, &raw.visibility, &raw.active, &raw.focused, &raw.minimized,
			&raw.maximized, &raw.fullscreen, &raw.mouse_locked, &raw.position_known, &raw.x, &raw.y)
		win32_service_result(result)!
		return raw
	} $else {
		_ = index
		return error(err_backend_unsupported)
	}
}

fn (backend &Win32Backend) service_window_state_with_monitors(index int, monitors []Win32ServiceMonitorRecord) !ServiceWindowState {
	if index < 0 || index >= backend.windows.len {
		return error(err_window_not_found)
	}
	raw := backend.service_raw_window_state(index)!
	app_instance := if backend.native_operations == unsafe { nil } {
		u64(0)
	} else {
		backend.native_operations.app_identity
	}
	native_monitor := win32_service_window_monitor(backend.windows[index].hwnd)
	return ServiceWindowState{
		mapping:                     win32_service_mapping(raw.mapping)
		visibility:                  win32_service_visibility(raw.visibility)
		active:                      win32_service_observed_bool(raw.active)
		focused:                     win32_service_observed_bool(raw.focused)
		minimized:                   win32_service_observed_bool(raw.minimized)
		maximized:                   win32_service_observed_bool(raw.maximized)
		fullscreen:                  win32_service_observed_bool(raw.fullscreen)
		mouse_locked:                win32_service_observed_bool(raw.mouse_locked)
		position:                    ServicePosition{
			known: raw.position_known != 0
			x:     raw.x
			y:     raw.y
		}
		monitor_ids:                 win32_service_monitor_ids_for_native(monitors, native_monitor,
			app_instance)
		monitor_membership_observed: true
	}
}

fn (backend &Win32Backend) service_window_state(id WindowId) !ServiceWindowState {
	index := backend.ensure_service_window(id)!
	return backend.service_window_state_with_monitors(index, backend.service_monitors)
}

fn (mut backend Win32Backend) service_monitor_snapshot(app_instance u64) ![]ServiceMonitorInfo {
	$if windows {
		if !backend.started || app_instance == 0 {
			return error(err_backend_unsupported)
		}
		raw := win32_service_raw_monitor_snapshot()!
		plan := win32_plan_service_monitors(backend.service_monitors, raw, app_instance)!
		backend.service_monitors = plan.records
		backend.service_monitor_raw = raw.clone()
		backend.service_monitor_poll_dirty = false
		return plan.monitors
	} $else {
		_ = app_instance
		return error(err_backend_unsupported)
	}
}

fn win32_service_monitor_event(app_instance u64, monitors []ServiceMonitorInfo) QueuedEvent {
	return queued_service_event(ServiceEvent{
		kind:     .monitor
		monitor:  if monitors.len > 0 {
			monitors[0]
		} else {
			ServiceMonitorInfo{
				id: ServiceMonitorId{
					app_instance: app_instance
				}
			}
		}
		monitors: monitors
	})
}

fn (backend &Win32Backend) service_app_instance() !u64 {
	if backend.native_operations == unsafe { nil } || backend.native_operations.app_identity == 0 {
		return error(err_app_identity_mismatch)
	}
	return backend.native_operations.app_identity
}

fn (mut backend Win32Backend) clear_pending_service_monitor_refresh() {
	backend.service_monitor_pending.clear()
	backend.service_monitor_pending_records.clear()
	backend.service_monitor_pending_raw.clear()
	backend.service_monitor_pending_sequence = 0
}

fn (mut backend Win32Backend) refresh_service_monitors_before_first_window() ! {
	$if windows {
		if backend.windows.len != 0 {
			return
		}
		app_instance := backend.service_app_instance()!
		raw := win32_service_raw_monitor_snapshot() or {
			backend.service_monitor_poll_dirty = true
			return err
		}
		if win32_service_raw_monitor_snapshots_equal(backend.service_monitor_raw, raw) {
			backend.clear_pending_service_monitor_refresh()
			backend.service_monitor_poll_dirty = false
			return
		}
		if backend.service_monitor_pending_sequence != 0
			&& win32_service_raw_monitor_snapshots_equal(backend.service_monitor_pending_raw, raw) {
			backend.service_monitor_poll_dirty = false
			return
		}
		plan := win32_plan_service_monitors(backend.service_monitors, raw, app_instance) or {
			backend.service_monitor_poll_dirty = true
			return err
		}
		sequence := if backend.service_monitor_pending_sequence != 0 {
			backend.service_monitor_pending_sequence
		} else {
			C.v_multiwindow_win32_next_event_sequence()
		}
		if sequence == 0 {
			backend.service_monitor_poll_dirty = true
			return error(err_backend_event_sequence_exhausted)
		}
		backend.service_monitor_pending = plan.monitors.clone()
		backend.service_monitor_pending_records = plan.records.clone()
		backend.service_monitor_pending_raw = raw.clone()
		backend.service_monitor_pending_sequence = sequence
		backend.service_monitor_poll_dirty = false
		return
	}
	return error(err_backend_unsupported)
}

fn win32_service_monitor_ids_equal(left []ServiceMonitorId, right []ServiceMonitorId) bool {
	if left.len != right.len {
		return false
	}
	for index, id in left {
		if id != right[index] {
			return false
		}
	}
	return true
}

fn (backend &Win32Backend) service_metrics_observation(index int, monitors []Win32ServiceMonitorRecord) !Win32ServiceMetricsObservation {
	$if windows {
		if index < 0 || index >= backend.windows.len {
			return error(err_window_not_found)
		}
		record := backend.windows[index]
		if record.destroyed || record.hwnd == unsafe { nil } {
			return error(err_window_not_found)
		}
		mut visible := 0
		mut minimized := 0
		mut logical_width := 0
		mut logical_height := 0
		mut framebuffer_width := 0
		mut framebuffer_height := 0
		mut native_scale := f32(0)
		mut conversion_available := 0
		if C.v_multiwindow_win32_render_snapshot(record.hwnd, &visible, &minimized, &logical_width,
			&logical_height, &framebuffer_width, &framebuffer_height, &native_scale,
			&conversion_available) == 0 {
			return error(err_capability_unsupported)
		}
		dpi := C.v_multiwindow_win32_service_window_dpi(record.hwnd)
		scale := if dpi > 0 { f32(dpi) / 96.0 } else { native_scale }
		metrics_available := logical_width > 0 && logical_height > 0 && framebuffer_width > 0
			&& framebuffer_height > 0 && scale > 0
		state := backend.service_window_state_with_monitors(index, monitors)!
		return Win32ServiceMetricsObservation{
			event:       queued_service_event(ServiceEvent{
				kind:    .metrics
				window:  record.id
				state:   state
				metrics: RenderMetricsSnapshot{
					logical_width:        f32(logical_width)
					logical_height:       f32(logical_height)
					framebuffer_width:    framebuffer_width
					framebuffer_height:   framebuffer_height
					dpi_scale:            scale
					metrics_available:    metrics_available
					conversion_available: conversion_available != 0
				}
			})
			monitor_ids: state.monitor_ids.clone()
			dpi:         dpi
		}
	} $else {
		_ = index
		return error(err_backend_unsupported)
	}
}

fn (mut backend Win32Backend) collect_service_refresh_events() ![]Win32NativeQueuedEvent {
	$if windows {
		app_instance := backend.service_app_instance()!
		mut events := []Win32NativeQueuedEvent{}
		mut suppress_net_zero_monitor := false
		if backend.service_monitor_pending_sequence != 0 {
			sequence := backend.service_monitor_pending_sequence
			mut staged_monitors := backend.service_monitor_pending.clone()
			mut staged_records := backend.service_monitor_pending_records.clone()
			mut staged_raw := backend.service_monitor_pending_raw.clone()
			latest_raw := win32_service_raw_monitor_snapshot() or {
				backend.service_monitor_poll_dirty = true
				return err
			}
			if win32_service_raw_monitor_snapshots_equal(backend.service_monitor_raw, latest_raw) {
				backend.service_monitor_poll_dirty = false
				suppress_net_zero_monitor = true
			} else {
				if !win32_service_raw_monitor_snapshots_equal(staged_raw, latest_raw) {
					latest_plan := win32_plan_service_monitors(backend.service_monitors,
						latest_raw, app_instance) or {
						backend.service_monitor_poll_dirty = true
						return err
					}
					staged_monitors = latest_plan.monitors.clone()
					staged_records = latest_plan.records.clone()
					staged_raw = latest_raw.clone()
				}
				events << Win32NativeQueuedEvent{
					sequence: sequence
					event:    win32_service_monitor_event(app_instance, staged_monitors)
				}
				mut observations := []Win32ServiceRefreshObservation{cap: backend.windows.len}
				for index in 0 .. backend.windows.len {
					record := backend.windows[index]
					if record.destroyed || record.hwnd == unsafe { nil } {
						continue
					}
					if C.v_multiwindow_win32_service_teardown_prepared(record.service_state) == 1 {
						continue
					}
					observation := backend.service_metrics_observation(index, staged_records) or {
						backend.service_monitor_poll_dirty = true
						return err
					}
					observations << Win32ServiceRefreshObservation{
						index:       index
						sequence:    sequence
						publish:     true
						observation: observation
					}
				}
				backend.service_monitors = staged_records
				backend.service_monitor_raw = staged_raw
				backend.service_monitor_poll_dirty = false
				for index in 0 .. backend.windows.len {
					mut record := backend.windows[index]
					record.pending_display_refresh = false
					record.pending_dpi_refresh = false
					record.pending_membership_refresh = false
					record.service_refresh_sequence = 0
				}
				for staged in observations {
					mut record := backend.windows[staged.index]
					observation := staged.observation
					record.service_monitor_ids = observation.monitor_ids.clone()
					record.service_dpi = observation.dpi
					events << Win32NativeQueuedEvent{
						sequence: staged.sequence
						event:    observation.event
					}
				}
				backend.clear_pending_service_monitor_refresh()
				return events
			}
		}
		if backend.windows.len == 0 {
			raw_monitors := win32_service_raw_monitor_snapshot() or {
				backend.service_monitor_poll_dirty = true
				return err
			}
			if win32_service_raw_monitor_snapshots_equal(backend.service_monitor_raw, raw_monitors) {
				backend.service_monitor_poll_dirty = false
				if suppress_net_zero_monitor {
					backend.clear_pending_service_monitor_refresh()
				}
				return events
			}
			plan := win32_plan_service_monitors(backend.service_monitors, raw_monitors,
				app_instance) or {
				backend.service_monitor_poll_dirty = true
				return err
			}
			sequence := if suppress_net_zero_monitor {
				backend.service_monitor_pending_sequence
			} else {
				C.v_multiwindow_win32_next_event_sequence()
			}
			if sequence == 0 {
				backend.service_monitor_poll_dirty = true
				return error(err_backend_event_sequence_exhausted)
			}
			backend.service_monitors = plan.records
			backend.service_monitor_raw = raw_monitors.clone()
			backend.service_monitor_poll_dirty = false
			if suppress_net_zero_monitor {
				backend.clear_pending_service_monitor_refresh()
			}
			events << Win32NativeQueuedEvent{
				sequence: sequence
				event:    win32_service_monitor_event(app_instance, plan.monitors)
			}
			return events
		}
		mut display_sequence := u64(0)
		for record in backend.windows {
			if record.pending_display_refresh && record.service_refresh_sequence != 0
				&& (display_sequence == 0 || record.service_refresh_sequence < display_sequence) {
				display_sequence = record.service_refresh_sequence
			}
		}
		if display_sequence != 0 {
			raw_monitors := win32_service_raw_monitor_snapshot() or {
				backend.service_monitor_poll_dirty = true
				return err
			}
			raw_changed := !win32_service_raw_monitor_snapshots_equal(backend.service_monitor_raw,
				raw_monitors)
			emit_monitor := raw_changed || !suppress_net_zero_monitor
			mut staged_records := backend.service_monitors.clone()
			if emit_monitor {
				plan := win32_plan_service_monitors(backend.service_monitors, raw_monitors,
					app_instance) or {
					backend.service_monitor_poll_dirty = true
					return err
				}
				staged_records = plan.records.clone()
				monitors := plan.monitors
				events << Win32NativeQueuedEvent{
					sequence: display_sequence
					event:    win32_service_monitor_event(app_instance, monitors)
				}
			}
			mut observations := []Win32ServiceRefreshObservation{cap: backend.windows.len}
			for index in 0 .. backend.windows.len {
				record := backend.windows[index]
				if record.destroyed || record.hwnd == unsafe { nil } {
					continue
				}
				if C.v_multiwindow_win32_service_teardown_prepared(record.service_state) == 1 {
					continue
				}
				observation := backend.service_metrics_observation(index, staged_records) or {
					backend.service_monitor_poll_dirty = true
					return err
				}
				observations << Win32ServiceRefreshObservation{
					index:       index
					sequence:    display_sequence
					publish:     true
					observation: observation
				}
			}
			if emit_monitor {
				backend.service_monitors = staged_records
				backend.service_monitor_raw = raw_monitors.clone()
			}
			backend.service_monitor_poll_dirty = false
			for index in 0 .. backend.windows.len {
				mut record := backend.windows[index]
				record.pending_display_refresh = false
				record.pending_dpi_refresh = false
				record.pending_membership_refresh = false
				record.service_refresh_sequence = 0
			}
			for staged in observations {
				mut record := backend.windows[staged.index]
				observation := staged.observation
				record.service_monitor_ids = observation.monitor_ids.clone()
				record.service_dpi = observation.dpi
				events << Win32NativeQueuedEvent{
					sequence: staged.sequence
					event:    observation.event
				}
			}
			if suppress_net_zero_monitor {
				backend.clear_pending_service_monitor_refresh()
			}
			return events
		}
		mut pending_indices := []int{}
		mut observations := []Win32ServiceRefreshObservation{}
		for index in 0 .. backend.windows.len {
			record := backend.windows[index]
			if (!record.pending_dpi_refresh && !record.pending_membership_refresh)
				|| record.service_refresh_sequence == 0 {
				continue
			}
			pending_indices << index
			if C.v_multiwindow_win32_service_teardown_prepared(record.service_state) == 1 {
				continue
			}
			sequence := record.service_refresh_sequence
			dpi_refresh := record.pending_dpi_refresh
			if record.destroyed || record.hwnd == unsafe { nil } {
				continue
			}
			observation := backend.service_metrics_observation(index, backend.service_monitors) or {
				backend.service_monitor_poll_dirty = true
				return err
			}
			membership_changed := !win32_service_monitor_ids_equal(record.service_monitor_ids,
				observation.monitor_ids)
			dpi_changed := observation.dpi != record.service_dpi
			observations << Win32ServiceRefreshObservation{
				index:       index
				sequence:    sequence
				publish:     dpi_refresh || membership_changed || dpi_changed
				observation: observation
			}
		}
		for index in pending_indices {
			mut record := backend.windows[index]
			record.pending_dpi_refresh = false
			record.pending_membership_refresh = false
			record.service_refresh_sequence = 0
		}
		for staged in observations {
			mut record := backend.windows[staged.index]
			observation := staged.observation
			record.service_monitor_ids = observation.monitor_ids.clone()
			record.service_dpi = observation.dpi
			if staged.publish {
				events << Win32NativeQueuedEvent{
					sequence: staged.sequence
					event:    observation.event
				}
			}
		}
		if suppress_net_zero_monitor {
			backend.clear_pending_service_monitor_refresh()
			backend.service_monitor_poll_dirty = false
		}
		return events
	} $else {
		return []Win32NativeQueuedEvent{}
	}
}

fn (mut backend Win32Backend) service_show_window(id WindowId) !ServiceWindowState {
	index := backend.ensure_service_window(id)!
	$if windows {
		was_modal_active := backend.windows[index].modal_active
		backend.activate_modal(index)!
		win32_service_result(C.v_multiwindow_win32_service_show_window(backend.windows[index].service_state)) or {
			show_error := err.msg()
			if !was_modal_active {
				backend.release_modal(index) or {
					return error(merge_backend_errors(show_error,
						'modal rollback failed: ${err.msg()}'))
				}
			}
			return error(show_error)
		}
	}
	return backend.service_window_state(id)!
}

fn (mut backend Win32Backend) service_hide_window(id WindowId) !ServiceWindowState {
	index := backend.ensure_service_window(id)!
	$if windows {
		win32_service_result(C.v_multiwindow_win32_service_hide_window(backend.windows[index].service_state))!
		backend.release_modal(index)!
	}
	return backend.service_window_state(id)!
}

fn (mut backend Win32Backend) service_focus_window(id WindowId) !ServiceWindowState {
	index := backend.ensure_service_window(id)!
	$if windows {
		win32_service_result(C.v_multiwindow_win32_service_focus_window(backend.windows[index].service_state))!
	}
	return backend.service_window_state(id)!
}

fn (mut backend Win32Backend) service_raise_window(id WindowId) !ServiceWindowState {
	index := backend.ensure_service_window(id)!
	$if windows {
		win32_service_result(C.v_multiwindow_win32_service_raise_window(backend.windows[index].service_state))!
	}
	return backend.service_window_state(id)!
}

fn (mut backend Win32Backend) service_set_window_position(id WindowId, x int, y int) !ServiceWindowState {
	index := backend.ensure_service_window(id)!
	$if windows {
		win32_service_result(C.v_multiwindow_win32_service_set_window_position(backend.windows[index].service_state,
			x, y))!
	}
	return backend.service_window_state(id)!
}

fn (mut backend Win32Backend) service_minimize_window(id WindowId) !ServiceWindowState {
	index := backend.ensure_service_window(id)!
	$if windows {
		win32_service_result(C.v_multiwindow_win32_service_minimize_window(backend.windows[index].service_state))!
	}
	return backend.service_window_state(id)!
}

fn (mut backend Win32Backend) service_maximize_window(id WindowId) !ServiceWindowState {
	index := backend.ensure_service_window(id)!
	if !backend.windows[index].config.resizable || backend.windows[index].config.borderless {
		return error(err_capability_unsupported)
	}
	$if windows {
		win32_service_result(C.v_multiwindow_win32_service_maximize_window(backend.windows[index].service_state))!
	}
	return backend.service_window_state(id)!
}

fn (mut backend Win32Backend) service_restore_window(id WindowId) !ServiceWindowState {
	index := backend.ensure_service_window(id)!
	$if windows {
		win32_service_result(C.v_multiwindow_win32_service_restore_window(backend.windows[index].service_state))!
	}
	backend.windows[index].config = win32_window_config_with_fullscreen(backend.windows[index].config,
		false)
	return backend.service_window_state(id)!
}

fn (mut backend Win32Backend) service_set_fullscreen(id WindowId, enabled bool) !ServiceWindowState {
	index := backend.ensure_service_window(id)!
	$if windows {
		win32_service_result(C.v_multiwindow_win32_service_set_fullscreen(backend.windows[index].service_state,
			win32_bool_to_int(enabled)))!
	}
	backend.windows[index].config = win32_window_config_with_fullscreen(backend.windows[index].config,
		enabled)
	return backend.service_window_state(id)!
}

fn (mut backend Win32Backend) service_set_mouse_lock(id WindowId, enabled bool) !ServiceWindowState {
	index := backend.ensure_service_window(id)!
	$if windows {
		mut record := backend.windows[index]
		if !enabled {
			record.mouse_tail_generation = 0
			record.mouse_dx = 0
			record.mouse_dy = 0
			record.mouse_pos_valid = false
		}
		result := C.v_multiwindow_win32_service_set_mouse_lock(record.service_state,
			win32_bool_to_int(enabled))
		win32_service_result(result)!
		if enabled {
			record.begin_mouse_lock_generation()
			record.mouse_dx = 0
			record.mouse_dy = 0
			record.mouse_pos_valid = false
		} else if record.mouse_lock_generation != 0
			&& record.mouse_raw_generation == record.mouse_lock_generation {
			record.mouse_tail_generation = record.mouse_lock_generation
		}
		return ServiceWindowState{
			mouse_locked: if enabled { .on } else { .off }
		}
	}
	return backend.service_window_state(id)!
}

fn win32_service_prepare_window_teardown(state voidptr) ! {
	$if windows {
		win32_service_result(C.v_multiwindow_win32_service_prepare_window_teardown(state))!
		return
	} $else {
		_ = state
		return error(err_backend_unsupported)
	}
}

fn (mut backend Win32Backend) service_set_clipboard_text(id WindowId, request ServiceRequestId, text string) !BackendClipboardStart {
	index := backend.ensure_service_window(id)!
	wide := win32_clipboard_utf16(text)!
	backend.admit_clipboard_request(request, id, backend.windows[index].hwnd, .clipboard_write,
		wide)!
	return BackendClipboardStart{}
}

fn (mut backend Win32Backend) service_request_clipboard_text(id WindowId, request ServiceRequestId) !BackendClipboardStart {
	index := backend.ensure_service_window(id)!
	backend.admit_clipboard_request(request, id, backend.windows[index].hwnd, .clipboard_read,
		[]u16{})!
	return BackendClipboardStart{}
}

fn (backend &Win32Backend) service_native_window_borrow(id WindowId) !BackendNativeWindowBorrow {
	index := backend.ensure_service_window(id)!
	$if windows {
		hwnd := C.v_multiwindow_win32_service_native_window(backend.windows[index].service_state)
		if hwnd == unsafe { nil } {
			return error(err_window_not_found)
		}
		return BackendNativeWindowBorrow{
			backend: .win32
			primary: hwnd
		}
	}
	return error(err_backend_unsupported)
}

$if test {
	@[markused]
	fn win32_service_test_clipboard_pending_count(backend_pointer voidptr) int {
		if backend_pointer == unsafe { nil } {
			return 0
		}
		backend := unsafe { &Win32Backend(backend_pointer) }
		return backend.clipboard_pending.len
	}

	@[markused]
	fn win32_service_test_clipboard_pending_deadline_ns(backend_pointer voidptr, index int) i64 {
		if backend_pointer == unsafe { nil } {
			return 0
		}
		backend := unsafe { &Win32Backend(backend_pointer) }
		if index < 0 || index >= backend.clipboard_pending.len {
			return 0
		}
		return backend.clipboard_pending[index].deadline_ns
	}

	@[markused]
	fn win32_service_test_clipboard_pending_write_matches(backend_pointer voidptr, index int, request_app u64, request_serial u64, window_app u64, window_slot int, window_generation u32, text &u16, units usize) int {
		if backend_pointer == unsafe { nil } || text == unsafe { nil } {
			return 0
		}
		backend := unsafe { &Win32Backend(backend_pointer) }
		if index < 0 || index >= backend.clipboard_pending.len {
			return 0
		}
		pending := backend.clipboard_pending[index]
		if pending.operation != .clipboard_write || pending.request.app_instance != request_app
			|| pending.request.serial != request_serial || pending.window.app_instance != window_app
			|| pending.window.slot != window_slot || pending.window.generation != window_generation
			|| usize(pending.write_utf16.len) != units {
			return 0
		}
		for unit_index in 0 .. pending.write_utf16.len {
			if pending.write_utf16[unit_index] != unsafe { text[unit_index] } {
				return 0
			}
		}
		return 1
	}
}
