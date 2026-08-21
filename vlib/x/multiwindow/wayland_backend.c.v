module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if linux && sokol_wayland ? {
	import time as vtime
}

$if linux && sokol_wayland ? {
	$if test {
		#flag linux -DV_MULTIWINDOW_NATIVE_PROOF_TEST
	}
	#flag linux -lwayland-client
	#flag linux -lwayland-egl
	#flag linux -lxkbcommon
	#flag linux -lEGL
	#flag linux -lGL
	#flag linux -I @VEXEROOT/thirdparty/sokol
	#flag linux @VMODROOT/vlib/x/multiwindow/wayland_xdg_shell_private.c
	#flag linux @VMODROOT/vlib/x/multiwindow/wayland_xdg_decoration_private.c
	#flag linux @VMODROOT/vlib/x/multiwindow/wayland_cursor_shape_private.c
	#flag linux @VEXEROOT/thirdparty/sokol/xdg-foreign-unstable-v2-protocol.c
	#include <poll.h>
	#include <string.h>
	#include <sys/mman.h>
	#include <unistd.h>
	#include <wayland-client.h>
	#insert "@VMODROOT/vlib/x/multiwindow/wayland_backend_helpers.h"
	#include <xkbcommon/xkbcommon.h>
}

const wayland_poll_in = i16(0x001)
const wayland_poll_err = i16(0x008)
const wayland_poll_hup = i16(0x010)
const wayland_pointer_axis_vertical_scroll = u32(0)
const wayland_pointer_axis_horizontal_scroll = u32(1)
const wayland_pointer_button_state_released = u32(0)
const wayland_pointer_button_state_pressed = u32(1)
const wayland_keyboard_key_state_released = u32(0)
const wayland_keyboard_key_state_pressed = u32(1)
const wayland_seat_capability_pointer = u32(1)
const wayland_seat_capability_keyboard = u32(2)
const wayland_seat_capability_touch = u32(4)
const wayland_btn_left = u32(0x110)
const wayland_btn_right = u32(0x111)
const wayland_btn_middle = u32(0x112)
const wayland_max_touch_points = 8
const wayland_invalid_mouse_button = 256
const wayland_scroll_scale = 10.0
const wayland_modifier_shift = u32(1)
const wayland_modifier_ctrl = u32(2)
const wayland_modifier_alt = u32(4)
const wayland_modifier_super = u32(8)
const wayland_modifier_lmb = u32(0x100)
const wayland_modifier_rmb = u32(0x200)
const wayland_modifier_mmb = u32(0x400)
const wayland_key_v = 86
const wayland_xdg_toplevel_resize_edge_top = u32(1)
const wayland_xdg_toplevel_resize_edge_bottom = u32(2)
const wayland_xdg_toplevel_resize_edge_left = u32(4)
const wayland_xdg_toplevel_resize_edge_top_left = u32(5)
const wayland_xdg_toplevel_resize_edge_bottom_left = u32(6)
const wayland_xdg_toplevel_resize_edge_right = u32(8)
const wayland_xdg_toplevel_resize_edge_top_right = u32(9)
const wayland_xdg_toplevel_resize_edge_bottom_right = u32(10)
const wayland_xdg_toplevel_decoration_mode_client_side = u32(1)
const wayland_xdg_toplevel_decoration_mode_server_side = u32(2)
const wayland_xdg_toplevel_state_maximized = u32(1)
const wayland_xdg_toplevel_state_fullscreen = u32(2)
const wayland_xdg_toplevel_state_activated = u32(4)
const wayland_cursor_shape_default = u32(1)
const wayland_cursor_shape_pointer = u32(4)
const wayland_cursor_shape_move = u32(13)
const wayland_cursor_shape_grab = u32(16)
const wayland_cursor_shape_grabbing = u32(17)
const wayland_cursor_shape_e_resize = u32(18)
const wayland_cursor_shape_n_resize = u32(19)
const wayland_cursor_shape_ne_resize = u32(20)
const wayland_cursor_shape_nw_resize = u32(21)
const wayland_cursor_shape_s_resize = u32(22)
const wayland_cursor_shape_se_resize = u32(23)
const wayland_cursor_shape_sw_resize = u32(24)
const wayland_cursor_shape_w_resize = u32(25)
const wayland_cursor_shape_ew_resize = u32(26)
const wayland_cursor_shape_ns_resize = u32(27)
const wayland_cursor_shape_nesw_resize = u32(28)
const wayland_cursor_shape_nwse_resize = u32(29)
const wayland_cursor_shape_crosshair = u32(8)
const wayland_cursor_shape_text = u32(9)
const wayland_cursor_shape_not_allowed = u32(15)
const wayland_keyboard_keymap_format_xkb_v1 = u32(1)
const wayland_xkb_context_no_flags = 0
const wayland_xkb_keymap_format_text_v1 = 1
const wayland_xkb_keymap_compile_no_flags = 0
const wayland_xkb_state_mods_effective = 8
const wayland_uri_list_buffer_size = 65536
const wayland_data_offer_drain_chunk_size = 4096
const wayland_data_offer_max_read_chunks = 16
const wayland_data_offer_max_pending_poll_cycles = 300
const wayland_clipboard_max_bytes = 16 * 1024 * 1024
const wayland_clipboard_io_chunk_size = 16 * 1024
const wayland_clipboard_max_io_chunks_per_poll = 16
const wayland_clipboard_max_outgoing_transfers = 8
const wayland_clipboard_max_outgoing_snapshot_bytes = u64(64 * 1024 * 1024)
const wayland_clipboard_timeout_ns = i64(2_000_000_000)
const wayland_io_test_interrupted = i64(-2)
const wayland_max_fallback_buffers = 3
const wayland_anchor_release_protocol_destroy = u64(1)
const wayland_anchor_release_local_proxy_destroy = u64(2)
const wayland_nsec_per_sec = u64(1_000_000_000)
const wayland_nsec_per_msec = u64(1_000_000)
const wayland_dnd_action_none = u32(0)
const wayland_dnd_action_copy = u32(1)
const wayland_dnd_action_move = u32(2)
const wayland_output_mode_current = u32(1)
const wayland_fractional_scale_denominator = u32(120)
const wayland_protocol_extent_max = u64(0x7fffffff)
const wayland_prot_read = 1
const wayland_map_private = 2
const wayland_map_failed = voidptr(usize(~u64(0)))
const err_wayland_egl_config_failed = 'multiwindow: wayland egl config failed'
const err_wayland_egl_context_failed = 'multiwindow: wayland egl context failed'
const err_wayland_egl_display_failed = 'multiwindow: wayland egl display failed'
const err_wayland_egl_make_current_failed = 'multiwindow: wayland egl make current failed'
const err_wayland_egl_surface_failed = 'multiwindow: wayland egl surface failed'
const err_wayland_egl_swap_buffers_failed = 'multiwindow: wayland egl swap buffers failed'
const err_wayland_surface_not_configured = 'multiwindow: wayland surface is not configured'
const err_wayland_show_configure_failed = 'multiwindow: wayland show did not receive a fresh configure'
const err_wayland_buffer_failed = 'multiwindow: wayland buffer creation failed'
const err_wayland_xkb_context_failed = 'multiwindow: wayland xkb context failed'

struct WaylandTouchPoint {
mut:
	active    bool
	id        int
	window_id WindowId
	x         f32
	y         f32
}

enum WaylandShowProbeAxis {
	maximized
	fullscreen
}

@[heap]
struct WaylandWindowRecord {
	id        WindowId
	resizable bool
	high_dpi  bool
mut:
	title                          string
	app_id                         string
	owner_id                       ?WindowId
	max_width                      int
	max_height                     int
	request_server_decoration      bool
	requested_maximized            bool
	requested_fullscreen           bool
	surface                        voidptr
	xdg_surface                    voidptr
	xdg_toplevel                   voidptr
	native_operations              &NativeOperationAuthority = unsafe { nil }
	owner                          &WaylandBackend           = unsafe { nil }
	width                          int
	height                         int
	min_width                      int
	min_height                     int
	pending_toplevel_width         int
	pending_toplevel_height        int
	configured                     bool
	requested_visible              bool
	hide_barrier_active            bool
	hide_barrier_state_observed    bool
	mapped                         bool
	publish_show_on_map            bool
	pending_egl_resize             bool
	pending_events                 []WaylandNativeQueuedEvent
	mouse_x                        f32
	mouse_y                        f32
	mouse_dx                       f32
	mouse_dy                       f32
	mouse_pos_valid                bool
	fallback_buffers               []voidptr
	fallback_current_buffer        voidptr
	fallback_buffer_width          int
	fallback_buffer_height         int
	wl_egl_window                  voidptr
	wl_egl_window_ticket           u64
	egl_surface                    voidptr
	egl_surface_ticket             u64
	frame_callback                 voidptr
	frame_callback_ticket          u64
	frame_ready                    bool
	native_destroyed               bool
	render_target_generation       u64 = 1
	toplevel_decoration            voidptr
	toplevel_decoration_configured bool
	toplevel_decoration_mode       u32
	user_action_serial             u32
	user_action_poll               u64
	user_action_serial_valid       bool
	pending_service_state_valid    bool
	pending_maximized              bool
	pending_fullscreen             bool
	pending_active                 bool
	show_handshake_active          bool
	show_handshake_boundary        u32
	show_configure_boundary        u32
	show_configure_serial          u32
	show_configure_received        bool
	show_configure_state_valid     bool
	show_configure_width           int
	show_configure_height          int
	show_configure_maximized       bool
	show_configure_fullscreen      bool
	show_configure_active          bool
	observed_service_state_valid   bool
	observed_maximized             bool
	observed_fullscreen            bool
	observed_active                bool
	output_slots                   []int
	buffer_scale                   int = 1
	render_scale                   f32 = 1.0
	fractional_scale               voidptr
	fractional_scale_numerator     u32
	viewport                       voidptr
	locked_pointer                 voidptr
	relative_pointer               voidptr
	mouse_lock_requested           bool
	mouse_locked                   bool
}

@[heap]
struct WaylandOutputRecord {
	slot int
mut:
	owner             &WaylandBackend = unsafe { nil }
	global_name       u32
	bound_version     u32
	generation        u32 = 1
	proxy             voidptr
	x                 int
	y                 int
	mode_width        int
	mode_height       int
	transform         int
	scale             int = 1
	pending_scale     int = 1
	make              string
	model             string
	name              string
	description       string
	geometry_received bool
	mode_received     bool
	ready             bool
	available         bool = true
	dirty             bool
}

struct WaylandNativeQueuedEvent {
	sequence u64
	event    QueuedEvent
}

struct WaylandClipboardRead {
	request ServiceRequestId
	window  WindowId
mut:
	fd          int = -1
	buffer      []u8
	deadline_ns i64
}

struct WaylandClipboardSend {
	fd      int = -1
	payload string
mut:
	offset      int
	deadline_ns i64
	blocked     bool
}

struct WaylandIoTestSeam {
mut:
	clipboard_read_interruptions    int
	clipboard_write_interruptions   int
	pending_drop_read_interruptions int
	pending_drop_poll_interruptions int
	pending_drop_native_bypass      bool
}

struct WaylandClipboardSourceTestSeam {
mut:
	active                     bool
	create_override            bool
	next_source                voidptr
	fail_listener              bool
	bypass_protocol            bool
	exercise_transport_plan    bool
	fail_flush_after_selection bool
	selection_requests         int
	destroyed                  []usize
	destroyed_local            []bool
}

struct WaylandPreparedTransportAttempt {
	display          voidptr
	context          NativeOperationContext
	evidence_context NativeOperationContext
}

struct WaylandServiceTransportPlan {
	attempts []WaylandPreparedTransportAttempt
mut:
	next int
}

fn (mut plan WaylandServiceTransportPlan) take() WaylandPreparedTransportAttempt {
	attempt := plan.attempts[plan.next]
	plan.next++
	return attempt
}

struct WaylandShowHandshakeTestObservation {
	present    bool
	serial     u32
	serial_set bool
	width      int
	height     int
	maximized  bool
	fullscreen bool
	active     bool
}

struct WaylandToplevelReplayTestSeam {
mut:
	active                          bool
	operations                      []string
	hide_barrier_override           bool
	hide_barrier_succeeds           bool
	show_handshake_override         bool
	show_observations               []WaylandShowHandshakeTestObservation
	show_hidden_drain_failure       bool
	show_ack_flush_failure          bool
	show_flush_failure_boundary     u32
	show_roundtrip_failure_boundary u32
	show_flush_count                u32
	show_roundtrip_count            u32
	show_acked_serials              []u32
	hide_render_target_override     bool
	hide_anchor_failure             bool
	hide_surface_release_failure    bool
	hide_surface_unclaimed_terminal bool
	hide_surface_terminal_failure   bool
	interactive_request_override    bool
	create_request_override         bool
	create_requests                 int
	interactive_move_requests       int
	interactive_resize_requests     int
	interactive_transport_finishes  int
}

@[heap]
struct WaylandPortalExport {
	request ServiceRequestId
	window  WindowId
	lease   ServicePortalLeaseId
mut:
	owner      &WaylandBackend = unsafe { nil }
	exported   voidptr
	identifier string
	terminal   bool
}

struct WaylandBackend {
mut:
	app_id                        string                    = 'v.x.multiwindow'
	native_operations             &NativeOperationAuthority = unsafe { nil }
	display                       voidptr
	registry                      voidptr
	compositor                    voidptr
	compositor_name               u32
	compositor_version            u32
	seat                          voidptr
	seat_name                     u32
	pointer                       voidptr
	cursor_shape_manager          voidptr
	cursor_shape_manager_name     u32
	cursor_shape_device           voidptr
	relative_pointer_manager      voidptr
	relative_pointer_manager_name u32
	pointer_constraints           voidptr
	pointer_constraints_name      u32
	keyboard                      voidptr
	touch                         voidptr
	data_device_manager           voidptr
	data_device_manager_name      u32
	foreign_exporter              voidptr
	foreign_exporter_name         u32
	fractional_scale_manager      voidptr
	fractional_scale_manager_name u32
	viewporter                    voidptr
	viewporter_name               u32
	data_device                   voidptr
	incoming_offer                voidptr
	incoming_offer_has_uri_list   bool
	incoming_offer_has_utf8       bool
	incoming_offer_has_text       bool
	shm                           voidptr
	shm_name                      u32
	data_offer                    voidptr
	data_offer_has_uri_list       bool
	data_offer_source_actions     u32
	data_offer_selected_action    u32
	data_offer_action_received    bool
	data_offer_window             WindowId
	data_offer_window_valid       bool
	pending_drop_offer            voidptr
	pending_drop_fd               int = -1
	pending_drop_window           WindowId
	pending_drop_window_valid     bool
	pending_drop_source_actions   u32
	pending_drop_selected_action  u32
	pending_drop_action_received  bool
	pending_drop_poll_cycles      int
	pending_drop_buffer           []u8
	selection_offer               voidptr
	selection_offer_has_utf8      bool
	selection_offer_has_text      bool
	clipboard_source              voidptr
	clipboard_text                string
	clipboard_source_test         WaylandClipboardSourceTestSeam
	toplevel_replay_test          WaylandToplevelReplayTestSeam
	clipboard_sends               []WaylandClipboardSend
	clipboard_send_cursor         int
	clipboard_send_snapshot_bytes u64
	clipboard_read                WaylandClipboardRead
	clipboard_read_active         bool
	io_test                       WaylandIoTestSeam
	wm_base                       voidptr
	wm_base_name                  u32
	decoration_manager            voidptr
	decoration_manager_name       u32
	xkb_context                   voidptr
	xkb_keymap                    voidptr
	xkb_state                     voidptr
	egl_display                   voidptr
	egl_config                    voidptr
	egl_context                   voidptr
	egl_context_ticket            u64
	anchor_surface                voidptr
	anchor_surface_ticket         u64
	anchor_wl_egl_window          voidptr
	anchor_wl_egl_window_ticket   u64
	anchor_wl_surface             voidptr
	anchor_wl_surface_ticket      u64
	egl_display_ticket            u64
	egl_thread_ticket             u64
	anchor_generation             u64 = 1
	egl_binding                   EglBindingIdentity
	egl_bad_current_recovery_used bool
	render_sequence               u64
	render_health                 NativeRendererHealth
	started                       bool
	pointer_focus                 WindowId
	pointer_focused               bool
	pointer_enter_serial          u32
	pointer_enter_serial_valid    bool
	keyboard_focus                WindowId
	keyboard_focused              bool
	keyboard_repeat_rate          int
	keyboard_repeat_delay         int
	keyboard_repeat_active        bool
	keyboard_repeat_raw_key       u32
	keyboard_repeat_key_code      int
	keyboard_repeat_window        WindowId
	keyboard_repeat_next_ns       u64
	keyboard_repeat_interval_ns   u64
	poll_generation               u64
	poll_error                    string
	event_sequence_terminal       string
	wayland_display_unavailable   bool
	wayland_display_error         i64
	pointer_buttons               u32
	modifiers                     u32
	keys_down                     [512]bool
	touches                       [wayland_max_touch_points]WaylandTouchPoint
	windows                       []&WaylandWindowRecord
	outputs                       []&WaylandOutputRecord
	portal_exports                []&WaylandPortalExport
	pending_service_events        []WaylandNativeQueuedEvent
}

struct WaylandRuntimeServiceCapabilities {
	clipboard_read  ServiceOperationCapability
	clipboard_write ServiceOperationCapability
	mouse_lock      ServiceOperationCapability
	portal_parent   ServiceOperationCapability
}

fn (backend &WaylandBackend) runtime_service_capabilities() WaylandRuntimeServiceCapabilities {
	return WaylandRuntimeServiceCapabilities{
		clipboard_read:  backend.service_operation_capability(.clipboard_read)
		clipboard_write: backend.service_operation_capability(.clipboard_write)
		mouse_lock:      backend.service_operation_capability(.mouse_lock)
		portal_parent:   backend.service_operation_capability(.portal_parent)
	}
}

fn (mut backend WaylandBackend) publish_service_capability_change(operation ServiceOperation, before ServiceOperationCapability, after ServiceOperationCapability) {
	if before == after {
		return
	}
	$if linux && sokol_wayland ? {
		for index in 0 .. backend.windows.len {
			mut record := backend.windows[index]
			record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_service_event(ServiceEvent{
				kind:       .capability
				window:     record.id
				operation:  operation
				capability: after
			}))
		}
	} $else {
		_ = operation
	}
}

fn (mut backend WaylandBackend) publish_runtime_service_capability_changes(before WaylandRuntimeServiceCapabilities) {
	after := backend.runtime_service_capabilities()
	backend.publish_service_capability_change(.clipboard_read, before.clipboard_read,
		after.clipboard_read)
	backend.publish_service_capability_change(.clipboard_write, before.clipboard_write,
		after.clipboard_write)
	backend.publish_service_capability_change(.mouse_lock, before.mouse_lock, after.mouse_lock)
	backend.publish_service_capability_change(.portal_parent, before.portal_parent,
		after.portal_parent)
}

@[markused]
fn (mut backend WaylandBackend) registry_listener_data() voidptr {
	return unsafe { voidptr(&backend) }
}

@[markused]
fn (record &WaylandWindowRecord) listener_data() voidptr {
	return unsafe { voidptr(record) }
}

@[markused]
fn (record &WaylandOutputRecord) listener_data() voidptr {
	return unsafe { voidptr(record) }
}

@[markused]
fn (record &WaylandPortalExport) listener_data() voidptr {
	return unsafe { voidptr(record) }
}

fn (mut record WaylandOutputRecord) publish_output_if_ready() {
	if !record.available || !record.ready || !record.dirty || record.owner == unsafe { nil } {
		return
	}
	record.dirty = false
	mut backend := record.owner
	backend.enqueue_monitor_snapshot_event()
}

fn (mut backend WaylandBackend) refresh_windows_for_output_scale(slot int) {
	for index in 0 .. backend.windows.len {
		if slot in backend.windows[index].output_slots {
			backend.refresh_window_output_scale(index)
		}
	}
}

fn (backend &WaylandBackend) transport_can_marshal() bool {
	return backend.display != unsafe { nil } && !backend.wayland_display_unavailable
		&& backend.wayland_display_error == 0
}

fn (backend &WaylandBackend) transport_plan_authority_ready(attempt_count int) bool {
	if !backend.transport_can_marshal() || backend.native_operations == unsafe { nil }
		|| attempt_count <= 0 {
		return false
	}
	authority := backend.native_operations
	if !authority.owner_thread_is_current() || authority.sequence_exhausted
		|| authority.next_ordinal == 0
		|| !authority.authority_scope_is_current(.renderer_attempt, authority.renderer_attempt_token) {
		return false
	}
	ordinal_count := u64(attempt_count) * u64(2)
	return ordinal_count > 0 && ordinal_count - 1 <= ~u64(0) - authority.next_ordinal
}

fn (backend &WaylandBackend) service_transport_usable(attempt_count int) bool {
	return backend.started && !backend.render_health.blocks_graphics()
		&& backend.transport_plan_authority_ready(attempt_count)
}

fn (backend &WaylandBackend) cleanup_transport_usable(attempt_count int) bool {
	return backend.transport_plan_authority_ready(attempt_count)
}

fn (backend &WaylandBackend) retains_native_ownership_except_display() bool {
	mut live_tickets := false
	if backend.native_operations != unsafe { nil } {
		live_tickets = backend.native_operations.has_live_lifetime_tickets()
	}
	core_live := backend.registry != unsafe { nil } || backend.compositor != unsafe { nil }
		|| backend.seat != unsafe { nil } || backend.pointer != unsafe { nil }
		|| backend.keyboard != unsafe { nil } || backend.touch != unsafe { nil }
		|| backend.cursor_shape_manager != unsafe { nil }
		|| backend.cursor_shape_device != unsafe { nil }
		|| backend.relative_pointer_manager != unsafe { nil }
		|| backend.pointer_constraints != unsafe { nil }
	clipboard_live := backend.data_device_manager != unsafe { nil }
		|| backend.data_device != unsafe { nil } || backend.incoming_offer != unsafe { nil }
		|| backend.selection_offer != unsafe { nil } || backend.clipboard_source != unsafe { nil }
		|| backend.clipboard_sends.len != 0 || backend.clipboard_read_active
	data_offer_live := backend.data_offer != unsafe { nil }
		|| backend.pending_drop_offer != unsafe { nil } || backend.pending_drop_fd >= 0
	portal_live := backend.foreign_exporter != unsafe { nil } || backend.portal_exports.len != 0
	scale_live := backend.fractional_scale_manager != unsafe { nil }
		|| backend.viewporter != unsafe { nil }
	render_live := backend.shm != unsafe { nil } || backend.wm_base != unsafe { nil }
		|| backend.decoration_manager != unsafe { nil } || backend.xkb_context != unsafe { nil }
		|| backend.xkb_keymap != unsafe { nil } || backend.xkb_state != unsafe { nil }
		|| backend.egl_display != unsafe { nil } || backend.egl_config != unsafe { nil }
		|| backend.egl_context != unsafe { nil } || backend.anchor_surface != unsafe { nil }
		|| backend.egl_binding.surface != unsafe { nil }
		|| backend.anchor_wl_egl_window != unsafe { nil }
		|| backend.anchor_wl_surface != unsafe { nil }
	ticket_live := backend.egl_context_ticket != 0 || backend.anchor_surface_ticket != 0
		|| backend.anchor_wl_egl_window_ticket != 0 || backend.anchor_wl_surface_ticket != 0
		|| backend.egl_display_ticket != 0 || backend.egl_thread_ticket != 0 || live_tickets
	return core_live || clipboard_live || data_offer_live || portal_live || scale_live
		|| render_live || ticket_live || backend.windows.len != 0 || backend.has_live_outputs()
}

fn (backend &WaylandBackend) has_live_outputs() bool {
	for output in backend.outputs {
		if output.proxy != unsafe { nil } {
			return true
		}
	}
	return false
}

fn wayland_copy_cstring(value &char) string {
	if value == unsafe { nil } {
		return ''
	}
	return unsafe { cstring_to_vstring(value) }.clone()
}

fn (backend &WaylandBackend) output_record_index_for_global(global_name u32) ?int {
	for index, output in backend.outputs {
		if output.available && output.global_name == global_name {
			return index
		}
	}
	return none
}

fn (backend &WaylandBackend) output_record_index_for_proxy(proxy voidptr) ?int {
	for index, output in backend.outputs {
		if output.available && output.proxy == proxy {
			return index
		}
	}
	return none
}

fn (backend &WaylandBackend) service_app_instance() u64 {
	return if backend.native_operations == unsafe { nil } {
		u64(0)
	} else {
		backend.native_operations.app_identity
	}
}

fn (mut backend WaylandBackend) enqueue_monitor_snapshot_event() {
	$if linux && sokol_wayland ? {
		app_instance := backend.service_app_instance()
		if app_instance == 0 {
			return
		}
		monitors := backend.service_monitor_snapshot(app_instance) or { return }
		backend.pending_service_events << WaylandNativeQueuedEvent{
			sequence: C.v_multiwindow_wayland_next_event_sequence()
			event:    queued_service_event(ServiceEvent{
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
	}
}

fn (mut backend WaylandBackend) enqueue_window_metrics_event(index int) {
	$if linux && sokol_wayland ? {
		if index < 0 || index >= backend.windows.len {
			return
		}
		mut record := backend.windows[index]
		record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_service_event(ServiceEvent{
			kind:    .metrics
			window:  record.id
			state:   record.service_window_state()
			metrics: record.service_metrics_snapshot()
		}))
	}
}

fn (record &WaylandWindowRecord) uses_fractional_scale() bool {
	return record.high_dpi && record.fractional_scale != unsafe { nil }
		&& record.viewport != unsafe { nil }
}

fn (record &WaylandWindowRecord) effective_render_scale() f32 {
	if !record.high_dpi {
		return 1.0
	}
	if record.uses_fractional_scale() && record.fractional_scale_numerator > 0 {
		return f32(record.fractional_scale_numerator) / f32(wayland_fractional_scale_denominator)
	}
	return f32(if record.buffer_scale > 0 {
		record.buffer_scale
	} else {
		1
	})
}

fn (record &WaylandWindowRecord) framebuffer_extent(logical int) int {
	if record.uses_fractional_scale() && record.fractional_scale_numerator > 0 {
		return wayland_framebuffer_extent(logical, u64(record.fractional_scale_numerator),
			u64(wayland_fractional_scale_denominator))
	}
	scale := if record.high_dpi && record.buffer_scale > 0 { record.buffer_scale } else { 1 }
	return wayland_framebuffer_extent(logical, u64(scale), 1)
}

fn wayland_framebuffer_extent(logical int, numerator u64, denominator u64) int {
	if logical <= 0 || numerator == 0 || denominator == 0 {
		return 1
	}
	logical_u64 := u64(logical)
	if numerator > u64(0xffffffffffffffff) / logical_u64 {
		return int(wayland_protocol_extent_max)
	}
	product := logical_u64 * numerator
	mut extent := product / denominator
	if product % denominator != 0 {
		extent++
	}
	if extent > wayland_protocol_extent_max {
		return int(wayland_protocol_extent_max)
	}
	return if extent == 0 { 1 } else { int(extent) }
}

fn (record &WaylandWindowRecord) framebuffer_width() int {
	return record.framebuffer_extent(record.width)
}

fn (record &WaylandWindowRecord) framebuffer_height() int {
	return record.framebuffer_extent(record.height)
}

fn (mut record WaylandWindowRecord) apply_fractional_scale_preference(numerator u32) bool {
	if !record.uses_fractional_scale() || numerator == 0
		|| record.fractional_scale_numerator == numerator {
		return false
	}
	record.fractional_scale_numerator = numerator
	record.render_scale = record.effective_render_scale()
	record.pending_egl_resize = true
	record.render_target_generation =
		exhaust_backend_target_generation(record.render_target_generation)
	$if linux && sokol_wayland ? {
		record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_service_event(ServiceEvent{
			kind:    .metrics
			window:  record.id
			state:   record.service_window_state()
			metrics: record.service_metrics_snapshot()
		}))
	}
	return true
}

fn (mut backend WaylandBackend) refresh_window_output_scale(index int) bool {
	if index < 0 || index >= backend.windows.len {
		return false
	}
	mut next_scale := 1
	if backend.windows[index].high_dpi {
		for slot in backend.windows[index].output_slots {
			if slot >= 0 && slot < backend.outputs.len && backend.outputs[slot].available
				&& backend.outputs[slot].scale > next_scale {
				next_scale = backend.outputs[slot].scale
			}
		}
	}
	mut record := backend.windows[index]
	if record.buffer_scale == next_scale {
		return false
	}
	old_width := record.framebuffer_width()
	old_height := record.framebuffer_height()
	old_scale := record.effective_render_scale()
	record.buffer_scale = next_scale
	record.render_scale = record.effective_render_scale()
	$if linux && sokol_wayland ? {
		if record.surface != unsafe { nil } && backend.transport_can_marshal() {
			if record.uses_fractional_scale() {
				C.wl_surface_set_buffer_scale(unsafe { &C.wl_surface(record.surface) }, 1)
				C.v_multiwindow_wayland_viewport_set_destination(unsafe {
					&C.wp_viewport(record.viewport)
				}, i32(record.width), i32(record.height))
			} else {
				C.wl_surface_set_buffer_scale(unsafe { &C.wl_surface(record.surface) }, next_scale)
			}
		}
	}
	if old_width == record.framebuffer_width() && old_height == record.framebuffer_height()
		&& old_scale == record.effective_render_scale() {
		return false
	}
	record.pending_egl_resize = true
	record.render_target_generation =
		exhaust_backend_target_generation(record.render_target_generation)
	backend.enqueue_window_metrics_event(index)
	return true
}

fn (mut backend WaylandBackend) remove_output_from_windows(slot int) {
	for index in 0 .. backend.windows.len {
		mut retained := []int{cap: backend.windows[index].output_slots.len}
		mut removed := false
		for current in backend.windows[index].output_slots {
			if current == slot {
				removed = true
			} else {
				retained << current
			}
		}
		if removed {
			backend.windows[index].output_slots = retained
			if !backend.refresh_window_output_scale(index) {
				backend.enqueue_window_metrics_event(index)
			}
		}
	}
}

fn (mut backend WaylandBackend) destroy_output_record(index int) {
	if index < 0 || index >= backend.outputs.len {
		return
	}
	mut output := backend.outputs[index]
	backend.remove_output_from_windows(output.slot)
	if output.proxy != unsafe { nil } {
		$if linux && sokol_wayland ? {
			if backend.destroy_proxy_locally_if_needed(output.proxy) {
				output.proxy = unsafe { nil }
			} else {
				C.v_multiwindow_wayland_output_destroy(unsafe { &C.wl_output(output.proxy) },
					output.bound_version)
				output.proxy = unsafe { nil }
			}
		} $else {
			output.proxy = unsafe { nil }
		}
	}
	output.available = false
	output.ready = false
	backend.enqueue_monitor_snapshot_event()
}

fn (mut backend WaylandBackend) destroy_all_outputs() {
	for index in 0 .. backend.outputs.len {
		backend.destroy_output_record(index)
	}
	backend.outputs.clear()
}

fn (backend &WaylandBackend) retains_native_ownership() bool {
	return backend.display != unsafe { nil } || backend.retains_native_ownership_except_display()
}

fn (backend &WaylandBackend) destroy_proxy_locally(proxy voidptr) {
	$if linux && sokol_wayland ? {
		if proxy != unsafe { nil } {
			C.v_multiwindow_wayland_proxy_destroy_local(proxy)
		}
	} $else {
		_ = proxy
	}
}

fn (backend &WaylandBackend) destroy_proxy_locally_if_needed(proxy voidptr) bool {
	if proxy == unsafe { nil } {
		return true
	}
	if backend.transport_can_marshal() {
		return false
	}
	backend.destroy_proxy_locally(proxy)
	return true
}

$if linux && sokol_wayland ? {
	struct C.pollfd {
	mut:
		fd      int
		events  i16
		revents i16
	}

	struct C.wl_display {}

	struct C.wl_registry {}

	struct C.wl_compositor {}

	struct C.wl_seat {}

	struct C.wl_pointer {}

	struct C.wl_keyboard {}

	struct C.wl_touch {}

	struct C.wl_data_device_manager {}

	struct C.wl_data_device {}

	struct C.wl_data_offer {}

	struct C.wl_data_source {}

	struct C.wl_shm {}

	struct C.wl_buffer {}

	struct C.wl_surface {}

	struct C.wl_output {}

	struct C.wl_array {
		size  usize
		alloc usize
		data  voidptr
	}

	struct C.xdg_wm_base {}

	struct C.xdg_surface {}

	struct C.xdg_toplevel {}

	struct C.zxdg_decoration_manager_v1 {}

	struct C.zxdg_toplevel_decoration_v1 {}

	struct C.wp_cursor_shape_manager_v1 {}

	struct C.wp_cursor_shape_device_v1 {}

	struct C.zwp_relative_pointer_manager_v1 {}

	struct C.zwp_relative_pointer_v1 {}

	struct C.zwp_pointer_constraints_v1 {}

	struct C.zwp_locked_pointer_v1 {}

	struct C.zxdg_exporter_v2 {}

	struct C.zxdg_exported_v2 {}

	struct C.wp_fractional_scale_manager_v1 {}

	struct C.wp_fractional_scale_v1 {}

	struct C.wp_viewporter {}

	struct C.wp_viewport {}

	struct C.xkb_context {}

	struct C.xkb_keymap {}

	struct C.xkb_state {}

	fn C.poll(fds &C.pollfd, nfds u64, timeout int) int
	fn C.strcmp(a &char, b &char) int
	fn C.close(fd int) int
	fn C.pipe(fds &int) int
	fn C.read(fd int, buf voidptr, count usize) isize
	fn C.write(fd int, buf voidptr, count usize) isize
	fn C.mmap(addr voidptr, length usize, prot int, flags int, fd int, offset i64) voidptr
	fn C.munmap(addr voidptr, length usize) int
	fn C.xkb_context_new(flags int) &C.xkb_context
	fn C.xkb_context_unref(ctx &C.xkb_context)
	fn C.xkb_keymap_new_from_string(ctx &C.xkb_context, str &char, format int, flags int) &C.xkb_keymap
	fn C.xkb_keymap_unref(keymap &C.xkb_keymap)
	fn C.xkb_keymap_key_repeats(keymap &C.xkb_keymap, key u32) int
	fn C.xkb_state_new(keymap &C.xkb_keymap) &C.xkb_state
	fn C.xkb_state_unref(state &C.xkb_state)
	fn C.xkb_state_key_get_utf8(state &C.xkb_state, key u32, buf &char, size usize) int
	fn C.xkb_state_update_mask(state &C.xkb_state, depressed u32, latched u32, locked u32, dep_group u32, lat_group u32, lock_group u32) int
	fn C.xkb_state_mod_name_is_active(state &C.xkb_state, name &char, @type int) int
	fn C.v_multiwindow_wayland_compositor_bind_version(version u32) u32
	fn C.v_multiwindow_wayland_bind_compositor(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_bind_xdg_wm_base(registry &C.wl_registry, name u32) voidptr
	fn C.v_multiwindow_wayland_bind_xdg_decoration_manager(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_bind_cursor_shape_manager(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_bind_relative_pointer_manager(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_bind_pointer_constraints(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_bind_seat(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_bind_data_device_manager(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_bind_xdg_foreign_exporter(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_bind_fractional_scale_manager(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_bind_viewporter(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_bind_shm(registry &C.wl_registry, name u32) voidptr
	fn C.v_multiwindow_wayland_output_bind_version(version u32) u32
	fn C.v_multiwindow_wayland_bind_output(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_next_event_sequence() u64
	fn C.v_multiwindow_wayland_event_sequence_exhausted() int
	fn C.v_multiwindow_wayland_add_registry_listener(registry &C.wl_registry, data voidptr) int
	fn C.v_multiwindow_wayland_add_output_listener(output &C.wl_output, data voidptr) int
	fn C.v_multiwindow_wayland_output_destroy(output &C.wl_output, bound_version u32)
	fn C.v_multiwindow_wayland_add_xdg_wm_base_listener(wm_base &C.xdg_wm_base, data voidptr) int
	fn C.v_multiwindow_wayland_add_xdg_surface_listener(xdg_surface &C.xdg_surface, data voidptr) int
	fn C.v_multiwindow_wayland_add_surface_listener(surface &C.wl_surface, data voidptr) int
	fn C.v_multiwindow_wayland_add_xdg_toplevel_listener(toplevel &C.xdg_toplevel, data voidptr) int
	fn C.v_multiwindow_wayland_add_xdg_toplevel_decoration_listener(decoration &C.zxdg_toplevel_decoration_v1, data voidptr) int
	fn C.v_multiwindow_wayland_add_seat_listener(seat &C.wl_seat, data voidptr) int
	fn C.v_multiwindow_wayland_seat_get_pointer(seat &C.wl_seat) voidptr
	fn C.v_multiwindow_wayland_seat_get_keyboard(seat &C.wl_seat) voidptr
	fn C.v_multiwindow_wayland_seat_get_touch(seat &C.wl_seat) voidptr
	fn C.v_multiwindow_wayland_data_device_manager_get_data_device(manager &C.wl_data_device_manager, seat &C.wl_seat) voidptr
	fn C.v_multiwindow_wayland_data_device_manager_create_data_source(manager &C.wl_data_device_manager) voidptr
	fn C.v_multiwindow_wayland_cursor_shape_manager_get_pointer(manager &C.wp_cursor_shape_manager_v1, pointer &C.wl_pointer) voidptr
	fn C.v_multiwindow_wayland_cursor_shape_device_set_shape(device &C.wp_cursor_shape_device_v1, serial u32, shape u32)
	fn C.v_multiwindow_wayland_cursor_shape_device_destroy(device &C.wp_cursor_shape_device_v1)
	fn C.v_multiwindow_wayland_cursor_shape_manager_destroy(manager &C.wp_cursor_shape_manager_v1)
	fn C.v_multiwindow_wayland_get_relative_pointer(manager &C.zwp_relative_pointer_manager_v1, pointer &C.wl_pointer) voidptr
	fn C.v_multiwindow_wayland_add_relative_pointer_listener(relative_pointer &C.zwp_relative_pointer_v1, data voidptr) int
	fn C.v_multiwindow_wayland_lock_pointer(constraints &C.zwp_pointer_constraints_v1, surface &C.wl_surface, pointer &C.wl_pointer) voidptr
	fn C.v_multiwindow_wayland_add_locked_pointer_listener(locked_pointer &C.zwp_locked_pointer_v1, data voidptr) int
	fn C.v_multiwindow_wayland_relative_pointer_destroy(relative_pointer &C.zwp_relative_pointer_v1)
	fn C.v_multiwindow_wayland_locked_pointer_destroy(locked_pointer &C.zwp_locked_pointer_v1)
	fn C.v_multiwindow_wayland_relative_pointer_manager_destroy(manager &C.zwp_relative_pointer_manager_v1)
	fn C.v_multiwindow_wayland_pointer_constraints_destroy(constraints &C.zwp_pointer_constraints_v1)
	fn C.v_multiwindow_wayland_add_pointer_listener(pointer &C.wl_pointer, data voidptr) int
	fn C.v_multiwindow_wayland_add_keyboard_listener(keyboard &C.wl_keyboard, data voidptr) int
	fn C.v_multiwindow_wayland_add_touch_listener(touch &C.wl_touch, data voidptr) int
	fn C.v_multiwindow_wayland_add_data_device_listener(device &C.wl_data_device, data voidptr) int
	fn C.v_multiwindow_wayland_add_data_offer_listener(offer &C.wl_data_offer, data voidptr) int
	fn C.v_multiwindow_wayland_add_data_source_listener(source &C.wl_data_source, data voidptr) int
	fn C.v_multiwindow_wayland_data_source_offer(source &C.wl_data_source, mime_type &char)
	fn C.v_multiwindow_wayland_data_source_destroy(source &C.wl_data_source)
	fn C.v_multiwindow_wayland_data_device_set_selection(device &C.wl_data_device, source &C.wl_data_source, serial u32)
	fn C.v_multiwindow_wayland_data_offer_accept(offer &C.wl_data_offer, serial u32, mime_type &char)
	fn C.v_multiwindow_wayland_data_offer_set_copy_action(offer &C.wl_data_offer)
	fn C.v_multiwindow_wayland_data_offer_receive(offer &C.wl_data_offer, mime_type &char, fd int)
	fn C.v_multiwindow_wayland_fd_set_nonblocking(fd int) int
	fn C.v_multiwindow_wayland_read_would_block() int
	fn C.v_multiwindow_wayland_io_interrupted() int
	fn C.v_multiwindow_wayland_safe_write(fd int, buffer voidptr, size usize) isize
	fn C.v_multiwindow_wayland_safe_write_broken_pipe_probe() int
	fn C.v_multiwindow_wayland_toplevel_state_contains(states &C.wl_array, expected u32) int
	fn C.wl_surface_set_buffer_scale(surface &C.wl_surface, scale int)
	fn C.v_multiwindow_wayland_data_offer_finish(offer &C.wl_data_offer)
	fn C.v_multiwindow_wayland_data_offer_destroy(offer &C.wl_data_offer)
	fn C.v_multiwindow_wayland_data_device_destroy(device &C.wl_data_device)
	fn C.v_multiwindow_wayland_data_device_manager_destroy(manager &C.wl_data_device_manager)
	fn C.v_multiwindow_wayland_export_toplevel(exporter &C.zxdg_exporter_v2, surface &C.wl_surface) voidptr
	fn C.v_multiwindow_wayland_add_exported_listener(exported &C.zxdg_exported_v2, data voidptr) int
	fn C.v_multiwindow_wayland_exported_destroy(exported &C.zxdg_exported_v2)
	fn C.v_multiwindow_wayland_exporter_destroy(exporter &C.zxdg_exporter_v2)
	fn C.v_multiwindow_wayland_get_fractional_scale(manager &C.wp_fractional_scale_manager_v1, surface &C.wl_surface) voidptr
	fn C.v_multiwindow_wayland_add_fractional_scale_listener(fractional_scale &C.wp_fractional_scale_v1, data voidptr) int
	fn C.v_multiwindow_wayland_fractional_scale_destroy(fractional_scale &C.wp_fractional_scale_v1)
	fn C.v_multiwindow_wayland_fractional_scale_manager_destroy(manager &C.wp_fractional_scale_manager_v1)
	fn C.v_multiwindow_wayland_get_viewport(viewporter &C.wp_viewporter, surface &C.wl_surface) voidptr
	fn C.v_multiwindow_wayland_viewport_set_destination(viewport &C.wp_viewport, width i32, height i32)
	fn C.v_multiwindow_wayland_viewport_destroy(viewport &C.wp_viewport)
	fn C.v_multiwindow_wayland_viewporter_destroy(viewporter &C.wp_viewporter)
	fn C.v_multiwindow_wayland_shm_destroy(shm &C.wl_shm)
	fn C.v_multiwindow_wayland_shm_layout(width int, height int, out_stride &i32, out_size &i32) int
	fn C.v_multiwindow_wayland_create_shm_buffer(shm &C.wl_shm, width int, height int) voidptr
	fn C.v_multiwindow_wayland_attach_buffer(surface &C.wl_surface, buffer &C.wl_buffer, width int, height int)
	fn C.v_multiwindow_wayland_add_buffer_listener(buffer &C.wl_buffer, data voidptr) int
	fn C.v_multiwindow_wayland_unmap_surface(surface &C.wl_surface)
	fn C.v_multiwindow_wayland_buffer_destroy(buffer &C.wl_buffer)
	fn C.v_multiwindow_wayland_pointer_destroy(pointer &C.wl_pointer)
	fn C.v_multiwindow_wayland_keyboard_destroy(keyboard &C.wl_keyboard)
	fn C.v_multiwindow_wayland_touch_destroy(touch &C.wl_touch)
	fn C.v_multiwindow_wayland_seat_destroy(seat &C.wl_seat)
	fn C.wl_display_connect(name &char) &C.wl_display
	fn C.v_multiwindow_wayland_display_disconnect(display &C.wl_display)
	fn C.wl_display_get_registry(display &C.wl_display) &C.wl_registry
	fn C.v_multiwindow_wayland_prepare_read(display &C.wl_display, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_cancel_read(display &C.wl_display, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_read_events(display &C.wl_display, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_get_fd(display &C.wl_display, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_dispatch_pending(display &C.wl_display, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_poll(fds &C.pollfd, nfds u64, timeout int, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_roundtrip(display &C.wl_display, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_flush(display &C.wl_display, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_display_error(display &C.wl_display, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_proxy_destroy_local(proxy voidptr)
	fn C.v_multiwindow_wayland_create_anchor_surface(compositor &C.wl_compositor, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_destroy_anchor_surface(surface voidptr, marshal int, result &C.VMultiwindowNativePrimitive)
	fn C.wl_registry_destroy(registry &C.wl_registry)
	fn C.wl_compositor_create_surface(compositor &C.wl_compositor) &C.wl_surface
	fn C.wl_compositor_destroy(compositor &C.wl_compositor)
	fn C.wl_surface_commit(surface &C.wl_surface)
	fn C.wl_surface_destroy(surface &C.wl_surface)
	fn C.v_multiwindow_wayland_xdg_wm_base_get_xdg_surface(wm_base &C.xdg_wm_base, surface &C.wl_surface) &C.xdg_surface
	fn C.v_multiwindow_wayland_xdg_wm_base_destroy(wm_base &C.xdg_wm_base)
	fn C.v_multiwindow_wayland_xdg_wm_base_pong(wm_base &C.xdg_wm_base, serial u32)
	fn C.v_multiwindow_wayland_xdg_surface_get_toplevel(xdg_surface &C.xdg_surface) &C.xdg_toplevel
	fn C.v_multiwindow_wayland_xdg_surface_ack_configure(xdg_surface &C.xdg_surface, serial u32)
	fn C.v_multiwindow_wayland_xdg_surface_destroy(xdg_surface &C.xdg_surface)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_title(toplevel &C.xdg_toplevel, title &char)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_app_id(toplevel &C.xdg_toplevel, app_id &char)
	fn C.v_multiwindow_wayland_get_last_marshaled_app_id() &char
	fn C.v_multiwindow_wayland_get_last_parent_child() usize
	fn C.v_multiwindow_wayland_get_last_parent_owner() usize
	fn C.v_multiwindow_wayland_xdg_toplevel_set_parent(toplevel &C.xdg_toplevel, parent &C.xdg_toplevel)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_min_size(toplevel &C.xdg_toplevel, width i32, height i32)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_max_size(toplevel &C.xdg_toplevel, width i32, height i32)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_fullscreen(toplevel &C.xdg_toplevel, output &C.wl_output)
	fn C.v_multiwindow_wayland_xdg_toplevel_unset_fullscreen(toplevel &C.xdg_toplevel)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_maximized(toplevel &C.xdg_toplevel)
	fn C.v_multiwindow_wayland_xdg_toplevel_unset_maximized(toplevel &C.xdg_toplevel)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_minimized(toplevel &C.xdg_toplevel)
	fn C.v_multiwindow_wayland_xdg_toplevel_move(toplevel &C.xdg_toplevel, seat &C.wl_seat, serial u32)
	fn C.v_multiwindow_wayland_xdg_toplevel_resize(toplevel &C.xdg_toplevel, seat &C.wl_seat, serial u32, edges u32)
	fn C.v_multiwindow_wayland_xdg_decoration_manager_get_toplevel_decoration(manager &C.zxdg_decoration_manager_v1, toplevel &C.xdg_toplevel) &C.zxdg_toplevel_decoration_v1
	fn C.v_multiwindow_wayland_xdg_toplevel_decoration_set_server_side(decoration &C.zxdg_toplevel_decoration_v1)
	fn C.v_multiwindow_wayland_xdg_toplevel_decoration_destroy(decoration &C.zxdg_toplevel_decoration_v1)
	fn C.v_multiwindow_wayland_xdg_decoration_manager_destroy(manager &C.zxdg_decoration_manager_v1)
	fn C.v_multiwindow_wayland_xdg_toplevel_destroy(toplevel &C.xdg_toplevel)
	fn C.v_multiwindow_wayland_egl_create_window(surface &C.wl_surface, width int, height int, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_egl_resize_window(egl_window voidptr, width int, height int, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_egl_destroy_window(egl_window voidptr, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_surface_frame(surface &C.wl_surface, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_add_frame_listener(callback voidptr, data voidptr, result &C.VMultiwindowNativePrimitive)
	fn C.v_multiwindow_wayland_destroy_frame_callback(callback voidptr, result &C.VMultiwindowNativePrimitive)

	@[export: 'v_multiwindow_wayland_registry_handle_global']
	@[markused]
	fn wayland_registry_handle_global(data voidptr, registry &C.wl_registry, name u32, iface &char, version u32) {
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		before := backend.runtime_service_capabilities()
		if C.strcmp(iface, c'wl_compositor') == 0 {
			backend.compositor = C.v_multiwindow_wayland_bind_compositor(registry, name, version)
			backend.compositor_name = name
			backend.compositor_version = C.v_multiwindow_wayland_compositor_bind_version(version)
		} else if C.strcmp(iface, c'xdg_wm_base') == 0 {
			backend.wm_base = C.v_multiwindow_wayland_bind_xdg_wm_base(registry, name)
			backend.wm_base_name = name
		} else if C.strcmp(iface, c'zxdg_decoration_manager_v1') == 0
			&& backend.decoration_manager == unsafe { nil } {
			manager := C.v_multiwindow_wayland_bind_xdg_decoration_manager(registry, name, version)
			if manager != unsafe { nil } {
				backend.decoration_manager = manager
				backend.decoration_manager_name = name
			}
		} else if C.strcmp(iface, c'wp_cursor_shape_manager_v1') == 0
			&& backend.cursor_shape_manager == unsafe { nil } {
			manager := C.v_multiwindow_wayland_bind_cursor_shape_manager(registry, name, version)
			if manager != unsafe { nil } {
				backend.cursor_shape_manager = manager
				backend.cursor_shape_manager_name = name
				backend.ensure_cursor_shape_device()
			}
		} else if C.strcmp(iface, c'zwp_relative_pointer_manager_v1') == 0
			&& backend.relative_pointer_manager == unsafe { nil } {
			manager := C.v_multiwindow_wayland_bind_relative_pointer_manager(registry, name,
				version)
			if manager != unsafe { nil } {
				backend.relative_pointer_manager = manager
				backend.relative_pointer_manager_name = name
			}
		} else if C.strcmp(iface, c'zwp_pointer_constraints_v1') == 0
			&& backend.pointer_constraints == unsafe { nil } {
			constraints := C.v_multiwindow_wayland_bind_pointer_constraints(registry, name, version)
			if constraints != unsafe { nil } {
				backend.pointer_constraints = constraints
				backend.pointer_constraints_name = name
			}
		} else if C.strcmp(iface, c'wl_seat') == 0 && backend.seat == unsafe { nil } {
			backend.seat = C.v_multiwindow_wayland_bind_seat(registry, name, version)
			backend.seat_name = name
			backend.ensure_data_device(data)
		} else if C.strcmp(iface, c'wl_data_device_manager') == 0
			&& backend.data_device_manager == unsafe { nil } {
			manager := C.v_multiwindow_wayland_bind_data_device_manager(registry, name, version)
			if manager != unsafe { nil } {
				backend.data_device_manager = manager
				backend.data_device_manager_name = name
				backend.ensure_data_device(data)
			}
		} else if C.strcmp(iface, c'zxdg_exporter_v2') == 0 {
			if backend.foreign_exporter == unsafe { nil } {
				exporter := C.v_multiwindow_wayland_bind_xdg_foreign_exporter(registry, name,
					version)
				if exporter != unsafe { nil } {
					backend.foreign_exporter = exporter
					backend.foreign_exporter_name = name
				}
			} else if backend.foreign_exporter_name == 0 {
				backend.foreign_exporter_name = name
			}
		} else if C.strcmp(iface, c'wp_fractional_scale_manager_v1') == 0
			&& backend.fractional_scale_manager == unsafe { nil } {
			manager := C.v_multiwindow_wayland_bind_fractional_scale_manager(registry, name,
				version)
			if manager != unsafe { nil } {
				backend.fractional_scale_manager = manager
				backend.fractional_scale_manager_name = name
				backend.attach_fractional_scale_to_windows()
			}
		} else if C.strcmp(iface, c'wp_viewporter') == 0 && backend.viewporter == unsafe { nil } {
			viewporter := C.v_multiwindow_wayland_bind_viewporter(registry, name, version)
			if viewporter != unsafe { nil } {
				backend.viewporter = viewporter
				backend.viewporter_name = name
				backend.attach_fractional_scale_to_windows()
			}
		} else if C.strcmp(iface, c'wl_shm') == 0 && backend.shm == unsafe { nil } {
			shm := C.v_multiwindow_wayland_bind_shm(registry, name)
			if shm != unsafe { nil } {
				backend.shm = shm
				backend.shm_name = name
			}
		} else if C.strcmp(iface, c'wl_output') == 0 {
			bound_version := C.v_multiwindow_wayland_output_bind_version(version)
			output := C.v_multiwindow_wayland_bind_output(registry, name, version)
			if output == unsafe { nil } {
				return
			}
			mut record := &WaylandOutputRecord{}
			mut reused := false
			for index in 0 .. backend.outputs.len {
				if !backend.outputs[index].available
					&& backend.outputs[index].proxy == unsafe { nil }
					&& backend.outputs[index].generation < u32(0xffffffff) {
					record = backend.outputs[index]
					record.generation++
					reused = true
					break
				}
			}
			if !reused {
				record = &WaylandOutputRecord{
					slot: backend.outputs.len
				}
			}
			record.owner = backend
			record.global_name = name
			record.bound_version = bound_version
			record.proxy = output
			record.x = 0
			record.y = 0
			record.mode_width = 0
			record.mode_height = 0
			record.transform = 0
			record.scale = 1
			record.pending_scale = 1
			record.make = ''
			record.model = ''
			record.name = ''
			record.description = ''
			record.geometry_received = false
			record.mode_received = false
			record.ready = false
			record.available = true
			record.dirty = true
			if C.v_multiwindow_wayland_add_output_listener(unsafe { &C.wl_output(output) },
				record.listener_data()) != 0 {
				C.v_multiwindow_wayland_output_destroy(unsafe { &C.wl_output(output) },
					bound_version)
				return
			}
			if !reused {
				backend.outputs << record
			}
		}
		backend.publish_runtime_service_capability_changes(before)
	}

	@[export: 'v_multiwindow_wayland_registry_handle_global_remove']
	@[markused]
	fn wayland_registry_handle_global_remove(data voidptr, registry &C.wl_registry, name u32) {
		_ = registry
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		before := backend.runtime_service_capabilities()
		if name == backend.seat_name {
			if backend.clipboard_read_active {
				backend.finish_clipboard_read(.failed, '', err_clipboard_selection_lost)
			}
			backend.seat_name = 0
			backend.destroy_seat_devices(true)
			if backend.seat != unsafe { nil } {
				if !backend.destroy_proxy_locally_if_needed(backend.seat) {
					C.v_multiwindow_wayland_seat_destroy(unsafe { &C.wl_seat(backend.seat) })
				}
				backend.seat = unsafe { nil }
			}
		} else if name == backend.data_device_manager_name {
			if backend.clipboard_read_active {
				backend.finish_clipboard_read(.failed, '', err_clipboard_selection_lost)
			}
			backend.data_device_manager_name = 0
			backend.destroy_data_device_manager()
		} else if name == backend.foreign_exporter_name {
			backend.foreign_exporter_name = 0
			// Existing exported objects remain valid after global removal. Stop
			// admitting new leases and retain them until explicit release/teardown.
		} else if name == backend.fractional_scale_manager_name {
			// Existing surface-scale objects remain valid; only new admission stops.
			backend.fractional_scale_manager_name = 0
		} else if name == backend.viewporter_name {
			// Existing viewports remain valid; only new admission stops.
			backend.viewporter_name = 0
		} else if name == backend.shm_name {
			backend.shm_name = 0
			if backend.shm != unsafe { nil } {
				if !backend.destroy_proxy_locally_if_needed(backend.shm) {
					C.v_multiwindow_wayland_shm_destroy(unsafe { &C.wl_shm(backend.shm) })
				}
				backend.shm = unsafe { nil }
			}
		} else if name == backend.wm_base_name {
			backend.wm_base_name = 0
			if backend.destroy_removed_wm_base_if_unused() {
				return
			}
		} else if name == backend.decoration_manager_name {
			backend.destroy_xdg_decoration_manager()
		} else if name == backend.cursor_shape_manager_name {
			backend.destroy_cursor_shape_manager()
		} else if name == backend.relative_pointer_manager_name {
			backend.destroy_relative_pointer_manager()
		} else if name == backend.pointer_constraints_name {
			backend.destroy_pointer_constraints()
		} else if name == backend.compositor_name {
			backend.compositor_name = 0
			if backend.compositor != unsafe { nil } {
				if !backend.destroy_proxy_locally_if_needed(backend.compositor) {
					if backend.compositor_version >= u32(4) {
						C.wl_compositor_destroy(unsafe { &C.wl_compositor(backend.compositor) })
					} else {
						backend.destroy_proxy_locally(backend.compositor)
					}
				}
				backend.compositor = unsafe { nil }
				backend.compositor_version = 0
			}
		} else if index := backend.output_record_index_for_global(name) {
			backend.destroy_output_record(index)
		}
		backend.publish_runtime_service_capability_changes(before)
	}

	@[export: 'v_multiwindow_wayland_exported_handle']
	@[markused]
	fn wayland_exported_handle(data voidptr, exported voidptr, handle &char) {
		if data == unsafe { nil } || exported == unsafe { nil } || handle == unsafe { nil } {
			return
		}
		mut portal := unsafe { &WaylandPortalExport(data) }
		if portal.terminal || portal.exported != exported || portal.owner == unsafe { nil } {
			return
		}
		identifier := wayland_copy_cstring(handle)
		if identifier == '' {
			return
		}
		portal.terminal = true
		portal.identifier = 'wayland:${identifier}'
		mut backend := portal.owner
		backend.pending_service_events << WaylandNativeQueuedEvent{
			sequence: C.v_multiwindow_wayland_next_event_sequence()
			event:    queued_service_event(ServiceEvent{
				kind:          .portal_parent
				window:        portal.window
				operation:     .portal_parent
				portal_parent: ServicePortalParentResult{
					id:         portal.request
					window:     portal.window
					status:     .ready
					lease:      portal.lease
					identifier: portal.identifier
				}
			})
		}
	}

	@[export: 'v_multiwindow_wayland_fractional_scale_preferred']
	@[markused]
	fn wayland_fractional_scale_preferred(data voidptr, fractional_scale voidptr, numerator u32) {
		if data == unsafe { nil } || fractional_scale == unsafe { nil } || numerator == 0 {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		if record.fractional_scale != fractional_scale || record.owner == unsafe { nil }
			|| !record.apply_fractional_scale_preference(numerator) {
			return
		}
		backend := record.owner
		if record.surface != unsafe { nil } && record.viewport != unsafe { nil }
			&& backend.transport_can_marshal() {
			C.wl_surface_set_buffer_scale(unsafe { &C.wl_surface(record.surface) }, 1)
			C.v_multiwindow_wayland_viewport_set_destination(unsafe {
				&C.wp_viewport(record.viewport)
			}, i32(record.width), i32(record.height))
		}
	}

	@[export: 'v_multiwindow_wayland_output_geometry']
	@[markused]
	fn wayland_output_geometry(data voidptr, output voidptr, x int, y int, physical_width int, physical_height int, subpixel int, make &char, model &char, transform int) {
		_ = physical_width
		_ = physical_height
		_ = subpixel
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandOutputRecord(data) }
		if !record.available || record.proxy != output {
			return
		}
		record.x = x
		record.y = y
		record.transform = transform
		record.make = wayland_copy_cstring(make)
		record.model = wayland_copy_cstring(model)
		record.geometry_received = true
		record.dirty = true
		if record.bound_version < 2 {
			record.ready = record.mode_received
			record.publish_output_if_ready()
		}
	}

	@[export: 'v_multiwindow_wayland_output_mode']
	@[markused]
	fn wayland_output_mode(data voidptr, output voidptr, flags u32, width int, height int, refresh int) {
		_ = refresh
		if data == unsafe { nil } || (flags & wayland_output_mode_current) == 0 {
			return
		}
		mut record := unsafe { &WaylandOutputRecord(data) }
		if !record.available || record.proxy != output || width <= 0 || height <= 0 {
			return
		}
		record.mode_width = width
		record.mode_height = height
		record.mode_received = true
		record.dirty = true
		if record.bound_version < 2 {
			record.ready = record.geometry_received
			record.publish_output_if_ready()
		}
	}

	@[export: 'v_multiwindow_wayland_output_done']
	@[markused]
	fn wayland_output_done(data voidptr, output voidptr) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandOutputRecord(data) }
		if record.available && record.proxy == output {
			record.ready = record.geometry_received && record.mode_received
			if !record.ready {
				return
			}
			scale_changed := record.pending_scale > 0 && record.scale != record.pending_scale
			if scale_changed {
				record.scale = record.pending_scale
				record.dirty = true
			}
			record.publish_output_if_ready()
			if scale_changed && record.owner != unsafe { nil } {
				mut backend := record.owner
				backend.refresh_windows_for_output_scale(record.slot)
			}
		}
	}

	@[export: 'v_multiwindow_wayland_output_scale']
	@[markused]
	fn wayland_output_scale(data voidptr, output voidptr, factor int) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandOutputRecord(data) }
		if record.available && record.proxy == output && factor > 0 {
			if record.pending_scale != factor {
				record.pending_scale = factor
				record.dirty = true
			}
		}
	}

	@[export: 'v_multiwindow_wayland_output_name']
	@[markused]
	fn wayland_output_name(data voidptr, output voidptr, name &char) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandOutputRecord(data) }
		if record.available && record.proxy == output {
			record.name = wayland_copy_cstring(name)
			record.dirty = true
		}
	}

	@[export: 'v_multiwindow_wayland_output_description']
	@[markused]
	fn wayland_output_description(data voidptr, output voidptr, description &char) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandOutputRecord(data) }
		if record.available && record.proxy == output {
			record.description = wayland_copy_cstring(description)
			record.dirty = true
		}
	}

	@[export: 'v_multiwindow_wayland_surface_enter']
	@[markused]
	fn wayland_surface_enter(data voidptr, surface voidptr, output voidptr) {
		if data == unsafe { nil } || surface == unsafe { nil } || output == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		if record.owner == unsafe { nil } || record.surface != surface {
			return
		}
		mut backend := record.owner
		output_index := backend.output_record_index_for_proxy(output) or { return }
		slot := backend.outputs[output_index].slot
		if slot in record.output_slots {
			return
		}
		record.output_slots << slot
		window_index := backend.window_record_index(record.id) or { return }
		if !backend.refresh_window_output_scale(window_index) {
			backend.enqueue_window_metrics_event(window_index)
		}
	}

	@[export: 'v_multiwindow_wayland_surface_leave']
	@[markused]
	fn wayland_surface_leave(data voidptr, surface voidptr, output voidptr) {
		if data == unsafe { nil } || surface == unsafe { nil } || output == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		if record.owner == unsafe { nil } || record.surface != surface {
			return
		}
		mut backend := record.owner
		output_index := backend.output_record_index_for_proxy(output) or { return }
		slot := backend.outputs[output_index].slot
		mut retained := []int{cap: record.output_slots.len}
		for current in record.output_slots {
			if current != slot {
				retained << current
			}
		}
		if retained.len == record.output_slots.len {
			return
		}
		record.output_slots = retained
		window_index := backend.window_record_index(record.id) or { return }
		if !backend.refresh_window_output_scale(window_index) {
			backend.enqueue_window_metrics_event(window_index)
		}
	}

	@[export: 'v_multiwindow_wayland_locked_pointer_locked']
	@[markused]
	fn wayland_locked_pointer_locked(data voidptr, locked_pointer voidptr) {
		if data == unsafe { nil } || locked_pointer == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		if record.locked_pointer != locked_pointer || !record.mouse_lock_requested {
			return
		}
		if !record.mouse_locked {
			record.mouse_locked = true
			record.enqueue_service_state(.mouse_lock)
		}
	}

	@[export: 'v_multiwindow_wayland_locked_pointer_unlocked']
	@[markused]
	fn wayland_locked_pointer_unlocked(data voidptr, locked_pointer voidptr) {
		if data == unsafe { nil } || locked_pointer == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		if record.locked_pointer != locked_pointer {
			return
		}
		if record.mouse_locked {
			record.mouse_locked = false
			record.enqueue_service_state(.mouse_lock)
		}
	}

	@[export: 'v_multiwindow_wayland_relative_pointer_motion']
	@[markused]
	fn wayland_relative_pointer_motion(data voidptr, relative_pointer voidptr, time_hi u32, time_lo u32, dx f64, dy f64, dx_unaccelerated f64, dy_unaccelerated f64) {
		_ = time_hi
		_ = time_lo
		_ = dx_unaccelerated
		_ = dy_unaccelerated
		if data == unsafe { nil } || relative_pointer == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		if record.relative_pointer != relative_pointer || !record.mouse_locked
			|| record.owner == unsafe { nil } {
			return
		}
		record.mouse_dx = f32(dx)
		record.mouse_dy = f32(dy)
		record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_input_event(record.input_event(.mouse_move,
			record.owner.event_modifiers())))
	}

	@[export: 'v_multiwindow_wayland_xdg_wm_base_ping']
	@[markused]
	fn wayland_xdg_wm_base_ping(data voidptr, wm_base voidptr, serial u32) {
		if data == unsafe { nil } {
			return
		}
		backend := unsafe { &WaylandBackend(data) }
		if !backend.transport_can_marshal() {
			return
		}
		C.v_multiwindow_wayland_xdg_wm_base_pong(unsafe { &C.xdg_wm_base(wm_base) }, serial)
	}

	@[export: 'v_multiwindow_wayland_xdg_surface_configure']
	@[markused]
	fn wayland_xdg_surface_configure(data voidptr, xdg_surface voidptr, serial u32) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		if record.show_handshake_active {
			record.show_configure_boundary = record.show_handshake_boundary
			record.show_configure_serial = serial
			record.show_configure_received = true
			// xdg_surface.configure can advance the serial without a new latched
			// xdg_toplevel.configure state. Keep the last valid state snapshot while
			// acknowledging the newest serial in that case.
			if record.pending_service_state_valid {
				mut width := if record.pending_toplevel_width > 0 {
					record.pending_toplevel_width
				} else {
					record.width
				}
				mut height := if record.pending_toplevel_height > 0 {
					record.pending_toplevel_height
				} else {
					record.height
				}
				if width <= 0 {
					width = 1
				}
				if height <= 0 {
					height = 1
				}
				width = window_extent_for_minimum(width, record.min_width)
				height = window_extent_for_minimum(height, record.min_height)
				record.show_configure_state_valid = true
				record.show_configure_width = width
				record.show_configure_height = height
				record.show_configure_maximized = record.pending_maximized
				record.show_configure_fullscreen = record.pending_fullscreen
				record.show_configure_active = record.pending_active
			}
			record.pending_toplevel_width = 0
			record.pending_toplevel_height = 0
			record.pending_service_state_valid = false
			return
		}
		if record.hide_barrier_active {
			if record.owner != unsafe { nil } && record.owner.transport_can_marshal() {
				C.v_multiwindow_wayland_xdg_surface_ack_configure(unsafe {
					&C.xdg_surface(xdg_surface)
				}, serial)
			}
			return
		}
		if !record.requested_visible {
			if record.owner != unsafe { nil } && record.owner.transport_can_marshal() {
				C.v_multiwindow_wayland_xdg_surface_ack_configure(unsafe {
					&C.xdg_surface(xdg_surface)
				}, serial)
			}
			record.pending_toplevel_width = 0
			record.pending_toplevel_height = 0
			record.pending_service_state_valid = false
			return
		}
		mut width := record.width
		mut height := record.height
		if record.pending_toplevel_width > 0 {
			width = record.pending_toplevel_width
		}
		if record.pending_toplevel_height > 0 {
			height = record.pending_toplevel_height
		}
		if width <= 0 {
			width = 1
		}
		if height <= 0 {
			height = 1
		}
		width = window_extent_for_minimum(width, record.min_width)
		height = window_extent_for_minimum(height, record.min_height)
		should_queue_resize := record.configured
			&& (record.width != width || record.height != height)
		record.width = width
		record.height = height
		record.pending_toplevel_width = 0
		record.pending_toplevel_height = 0
		was_configured := record.configured
		record.configured = true
		if !was_configured || record.frame_callback == unsafe { nil } {
			record.frame_ready = true
		}
		if should_queue_resize {
			record.pending_egl_resize = true
			record.render_target_generation =
				exhaust_backend_target_generation(record.render_target_generation)
		}
		if record.owner != unsafe { nil } && record.owner.transport_can_marshal() {
			C.v_multiwindow_wayland_xdg_surface_ack_configure(unsafe {
				&C.xdg_surface(xdg_surface)
			}, serial)
			if record.uses_fractional_scale() {
				C.v_multiwindow_wayland_viewport_set_destination(unsafe {
					&C.wp_viewport(record.viewport)
				}, i32(record.width), i32(record.height))
			}
		}
		if should_queue_resize {
			record.enqueue_resize_events(C.v_multiwindow_wayland_next_event_sequence())
		}
		record.commit_pending_service_state()
	}

	@[export: 'v_multiwindow_wayland_xdg_toplevel_configure']
	@[markused]
	fn wayland_xdg_toplevel_configure(data voidptr, toplevel voidptr, width int, height int, states &C.wl_array) {
		_ = toplevel
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		if record.show_handshake_active {
			record.pending_service_state_valid = true
			record.pending_maximized = C.v_multiwindow_wayland_toplevel_state_contains(states,
				wayland_xdg_toplevel_state_maximized) != 0
			record.pending_fullscreen = C.v_multiwindow_wayland_toplevel_state_contains(states,
				wayland_xdg_toplevel_state_fullscreen) != 0
			record.pending_active = C.v_multiwindow_wayland_toplevel_state_contains(states,
				wayland_xdg_toplevel_state_activated) != 0
			record.pending_toplevel_width = if width > 0 { width } else { 0 }
			record.pending_toplevel_height = if height > 0 { height } else { 0 }
			return
		}
		if record.hide_barrier_active {
			record.requested_maximized = C.v_multiwindow_wayland_toplevel_state_contains(states,
				wayland_xdg_toplevel_state_maximized) != 0
			record.requested_fullscreen = C.v_multiwindow_wayland_toplevel_state_contains(states,
				wayland_xdg_toplevel_state_fullscreen) != 0
			record.hide_barrier_state_observed = true
			return
		}
		if !record.requested_visible {
			record.pending_toplevel_width = 0
			record.pending_toplevel_height = 0
			record.pending_service_state_valid = false
			return
		}
		record.pending_service_state_valid = true
		record.pending_maximized = C.v_multiwindow_wayland_toplevel_state_contains(states,
			wayland_xdg_toplevel_state_maximized) != 0
		record.pending_fullscreen = C.v_multiwindow_wayland_toplevel_state_contains(states,
			wayland_xdg_toplevel_state_fullscreen) != 0
		record.pending_active = C.v_multiwindow_wayland_toplevel_state_contains(states,
			wayland_xdg_toplevel_state_activated) != 0
		if width > 0 {
			record.pending_toplevel_width = width
		} else {
			record.pending_toplevel_width = 0
		}
		if height > 0 {
			record.pending_toplevel_height = height
		} else {
			record.pending_toplevel_height = 0
		}
	}

	@[export: 'v_multiwindow_wayland_xdg_toplevel_close']
	@[markused]
	fn wayland_xdg_toplevel_close(data voidptr, toplevel voidptr) {
		_ = toplevel
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_lifecycle_event(Event{
			kind:      .window_close_requested
			window_id: record.id
		}))
	}

	@[export: 'v_multiwindow_wayland_frame_done']
	@[markused]
	fn wayland_frame_done(data voidptr, callback voidptr, time u32) int {
		_ = time
		if data == unsafe { nil } {
			return 0
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		if record.frame_callback != callback || record.native_operations == unsafe { nil }
			|| !record.native_operations.owner_thread_is_current() {
			return 0
		}
		record.native_operations.claim_lifetime_release(record.frame_callback_ticket,
			.wayland_frame_callback, native_identity(callback), native_identity(record.surface)) or {
			return 0
		}
		return if record.owner != unsafe { nil } && record.owner.transport_can_marshal() {
			1
		} else {
			2
		}
	}

	@[export: 'v_multiwindow_wayland_frame_destroyed']
	@[markused]
	fn wayland_frame_destroyed(data voidptr, callback voidptr) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		if record.frame_callback != callback || record.native_operations == unsafe { nil }
			|| !record.native_operations.owner_thread_is_current() {
			return
		}
		health := if record.owner == unsafe { nil } {
			NativeRendererHealth.unavailable
		} else {
			record.owner.render_health
		}
		_ = record.native_operations.complete_claimed_lifetime_release(record.frame_callback_ticket,
			.wayland_frame_callback, native_identity(callback), native_identity(record.surface), C.VMultiwindowNativePrimitive{},
			.void_completion, health, err_wayland_egl_swap_buffers_failed)
		record.frame_callback = unsafe { nil }
		record.frame_callback_ticket = 0
		record.frame_ready = health == .ready
	}

	@[export: 'v_multiwindow_wayland_xdg_toplevel_decoration_configure']
	@[markused]
	fn wayland_xdg_toplevel_decoration_configure(data voidptr, decoration voidptr, mode u32) {
		_ = decoration
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		record.toplevel_decoration_configured = true
		match mode {
			wayland_xdg_toplevel_decoration_mode_server_side,
			wayland_xdg_toplevel_decoration_mode_client_side {
				record.toplevel_decoration_mode = mode
			}
			else {
				record.toplevel_decoration_mode = wayland_xdg_toplevel_decoration_mode_client_side
			}
		}
	}

	@[export: 'v_multiwindow_wayland_seat_capabilities']
	@[markused]
	fn wayland_seat_capabilities(data voidptr, seat voidptr, caps u32) {
		if data == unsafe { nil } || seat == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		if !backend.transport_can_marshal() {
			return
		}
		before := backend.runtime_service_capabilities()
		if (caps & wayland_seat_capability_pointer) != 0 {
			if backend.pointer == unsafe { nil } {
				pointer := C.v_multiwindow_wayland_seat_get_pointer(unsafe { &C.wl_seat(seat) })
				if pointer != unsafe { nil }
					&& C.v_multiwindow_wayland_add_pointer_listener(unsafe { &C.wl_pointer(pointer) }, data) == 0 {
					backend.pointer = pointer
					backend.ensure_cursor_shape_device()
				} else if pointer != unsafe { nil } {
					C.v_multiwindow_wayland_pointer_destroy(unsafe { &C.wl_pointer(pointer) })
				}
			}
		} else if backend.pointer != unsafe { nil } {
			backend.release_all_mouse_locks(true)
			backend.destroy_cursor_shape_device()
			C.v_multiwindow_wayland_pointer_destroy(unsafe { &C.wl_pointer(backend.pointer) })
			backend.pointer = unsafe { nil }
			backend.pointer_focused = false
			backend.pointer_enter_serial_valid = false
			backend.pointer_buttons = 0
		}
		if (caps & wayland_seat_capability_keyboard) != 0 {
			if backend.keyboard == unsafe { nil } {
				keyboard := C.v_multiwindow_wayland_seat_get_keyboard(unsafe { &C.wl_seat(seat) })
				if keyboard != unsafe { nil }
					&& C.v_multiwindow_wayland_add_keyboard_listener(unsafe { &C.wl_keyboard(keyboard) }, data) == 0 {
					backend.keyboard = keyboard
				} else if keyboard != unsafe { nil } {
					C.v_multiwindow_wayland_keyboard_destroy(unsafe { &C.wl_keyboard(keyboard) })
				}
			}
		} else if backend.keyboard != unsafe { nil } {
			C.v_multiwindow_wayland_keyboard_destroy(unsafe { &C.wl_keyboard(backend.keyboard) })
			backend.keyboard = unsafe { nil }
			backend.keyboard_focused = false
			backend.modifiers = 0
			backend.clear_key_repeat_info()
			backend.clear_keys_down()
			backend.destroy_xkb_keymap_state()
		}
		if (caps & wayland_seat_capability_touch) != 0 {
			if backend.touch == unsafe { nil } {
				touch := C.v_multiwindow_wayland_seat_get_touch(unsafe { &C.wl_seat(seat) })
				if touch != unsafe { nil }
					&& C.v_multiwindow_wayland_add_touch_listener(unsafe { &C.wl_touch(touch) }, data) == 0 {
					backend.touch = touch
				} else if touch != unsafe { nil } {
					C.v_multiwindow_wayland_touch_destroy(unsafe { &C.wl_touch(touch) })
				}
			}
		} else if backend.touch != unsafe { nil } {
			C.v_multiwindow_wayland_touch_destroy(unsafe { &C.wl_touch(backend.touch) })
			backend.touch = unsafe { nil }
			backend.clear_touches()
		}
		backend.ensure_data_device(data)
		backend.publish_runtime_service_capability_changes(before)
	}

	@[export: 'v_multiwindow_wayland_seat_name']
	@[markused]
	fn wayland_seat_name(data voidptr, seat voidptr, name &char) {
		_ = data
		_ = seat
		_ = name
	}

	@[export: 'v_multiwindow_wayland_buffer_release']
	@[markused]
	fn wayland_buffer_release(data voidptr, buffer voidptr) {
		if data == unsafe { nil } || buffer == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
		record.release_fallback_buffer(buffer)
	}

	@[export: 'v_multiwindow_wayland_pointer_enter']
	@[markused]
	fn wayland_pointer_enter(data voidptr, pointer voidptr, serial u32, surface voidptr, x f64, y f64) {
		_ = pointer
		if data == unsafe { nil } || surface == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		index := backend.window_record_index_for_surface(surface) or { return }
		mut record := backend.windows[index]
		backend.pointer_focus = record.id
		backend.pointer_focused = true
		backend.pointer_enter_serial = serial
		backend.pointer_enter_serial_valid = true
		record.update_mouse_position(f32(x), f32(y), true)
		record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_input_event(record.input_event(.mouse_enter,
			backend.event_modifiers())))
	}

	@[export: 'v_multiwindow_wayland_pointer_leave']
	@[markused]
	fn wayland_pointer_leave(data voidptr, pointer voidptr, serial u32, surface voidptr) {
		_ = pointer
		_ = serial
		if data == unsafe { nil } || surface == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		index := backend.window_record_index_for_surface(surface) or { return }
		mut record := backend.windows[index]
		record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_input_event(record.input_event(.mouse_leave,
			backend.event_modifiers())))
		backend.release_window_mouse_lock(index, true, true)
		if backend.pointer_focused && backend.pointer_focus == record.id {
			backend.pointer_focused = false
			backend.pointer_enter_serial_valid = false
		}
	}

	@[export: 'v_multiwindow_wayland_pointer_motion']
	@[markused]
	fn wayland_pointer_motion(data voidptr, pointer voidptr, time u32, x f64, y f64) {
		_ = pointer
		_ = time
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		index := backend.pointer_focus_record_index() or { return }
		mut record := backend.windows[index]
		record.update_mouse_position(f32(x), f32(y), false)
		record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_input_event(record.input_event(.mouse_move,
			backend.event_modifiers())))
	}

	@[export: 'v_multiwindow_wayland_pointer_button']
	@[markused]
	fn wayland_pointer_button(data voidptr, pointer voidptr, serial u32, time u32, button u32, state u32) {
		_ = pointer
		_ = time
		if data == unsafe { nil } {
			return
		}
		mouse_button := wayland_mouse_button(button)
		if mouse_button == wayland_invalid_mouse_button {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		index := backend.pointer_focus_record_index() or { return }
		modifier := wayland_mouse_modifier(button)
		if state == wayland_pointer_button_state_pressed {
			backend.pointer_buttons |= modifier
		} else if state == wayland_pointer_button_state_released {
			backend.pointer_buttons &= ~modifier
		} else {
			return
		}
		if state == wayland_pointer_button_state_pressed {
			backend.windows[index].store_user_action_serial(serial, backend.poll_generation)
		}
		input_kind := if state == wayland_pointer_button_state_pressed {
			InputEventKind.mouse_down
		} else {
			InputEventKind.mouse_up
		}
		input := backend.windows[index].input_event_with_payload(input_kind,
			backend.event_modifiers(), 0, false, mouse_button, 0, 0)
		backend.windows[index].enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(),
			queued_input_event(input))
	}

	@[export: 'v_multiwindow_wayland_pointer_axis']
	@[markused]
	fn wayland_pointer_axis(data voidptr, pointer voidptr, time u32, axis u32, value f64) {
		_ = pointer
		_ = time
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		index := backend.pointer_focus_record_index() or { return }
		mut record := backend.windows[index]
		mut scroll_x := f32(0)
		mut scroll_y := f32(0)
		if axis == wayland_pointer_axis_vertical_scroll {
			scroll_y = -f32(value) / f32(wayland_scroll_scale)
		} else if axis == wayland_pointer_axis_horizontal_scroll {
			scroll_x = f32(value) / f32(wayland_scroll_scale)
		} else {
			return
		}
		input := record.input_event_with_payload(.mouse_scroll, backend.event_modifiers(), 0,
			false, wayland_invalid_mouse_button, scroll_x, scroll_y)
		record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(),
			queued_input_event(input))
	}

	@[export: 'v_multiwindow_wayland_keyboard_enter']
	@[markused]
	fn wayland_keyboard_enter(data voidptr, keyboard voidptr, serial u32, surface voidptr) {
		_ = keyboard
		_ = serial
		if data == unsafe { nil } || surface == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		index := backend.window_record_index_for_surface(surface) or { return }
		mut record := backend.windows[index]
		backend.keyboard_focus = record.id
		backend.keyboard_focused = true
		record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_input_event(record.input_event(.focused,
			backend.event_modifiers())))
	}

	@[export: 'v_multiwindow_wayland_keyboard_leave']
	@[markused]
	fn wayland_keyboard_leave(data voidptr, keyboard voidptr, serial u32, surface voidptr) {
		_ = keyboard
		_ = serial
		if data == unsafe { nil } || surface == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		index := backend.window_record_index_for_surface(surface) or { return }
		mut record := backend.windows[index]
		record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_input_event(record.input_event(.unfocused,
			backend.event_modifiers())))
		if backend.keyboard_focused && backend.keyboard_focus == record.id {
			backend.keyboard_focused = false
		}
		backend.stop_key_repeat()
		backend.modifiers = 0
		backend.clear_keys_down()
	}

	@[export: 'v_multiwindow_wayland_keyboard_keymap']
	@[markused]
	fn wayland_keyboard_keymap(data voidptr, keyboard voidptr, format u32, fd int, size u32) {
		_ = keyboard
		if fd < 0 {
			return
		}
		if data == unsafe { nil } || format != wayland_keyboard_keymap_format_xkb_v1 || size == 0 {
			C.close(fd)
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		if backend.xkb_context == unsafe { nil } {
			C.close(fd)
			return
		}
		map_str :=
			C.mmap(unsafe { nil }, usize(size), wayland_prot_read, wayland_map_private, fd, 0)
		if map_str == wayland_map_failed {
			C.close(fd)
			return
		}
		keymap := C.xkb_keymap_new_from_string(unsafe { &C.xkb_context(backend.xkb_context) },
			&char(map_str), wayland_xkb_keymap_format_text_v1, wayland_xkb_keymap_compile_no_flags)
		C.munmap(map_str, usize(size))
		C.close(fd)
		if keymap == unsafe { nil } {
			return
		}
		state := C.xkb_state_new(keymap)
		if state == unsafe { nil } {
			C.xkb_keymap_unref(keymap)
			return
		}
		backend.stop_key_repeat()
		backend.destroy_xkb_keymap_state()
		backend.xkb_keymap = unsafe { voidptr(keymap) }
		backend.xkb_state = unsafe { voidptr(state) }
		backend.modifiers = backend.xkb_modifiers()
	}

	@[export: 'v_multiwindow_wayland_keyboard_repeat_info']
	@[markused]
	fn wayland_keyboard_repeat_info(data voidptr, keyboard voidptr, rate int, delay int) {
		_ = keyboard
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		backend.keyboard_repeat_rate = if rate > 0 { rate } else { 0 }
		backend.keyboard_repeat_delay = if delay > 0 { delay } else { 0 }
		if backend.keyboard_repeat_rate == 0 {
			backend.stop_key_repeat()
		}
	}

	@[export: 'v_multiwindow_wayland_keyboard_key']
	@[markused]
	fn wayland_keyboard_key(data voidptr, keyboard voidptr, serial u32, time u32, key u32, state u32) {
		_ = keyboard
		_ = time
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		index := backend.keyboard_focus_record_index() or { return }
		raw_key := key + 8
		key_code := wayland_key_code(key)
		key_index := if key_code >= 0 && key_code < 512 { key_code } else { 0 }
		key_repeat := state == wayland_keyboard_key_state_pressed && key_index > 0
			&& backend.keys_down[key_index]
		char_code := if state == wayland_keyboard_key_state_pressed {
			backend.char_code_for_key(raw_key)
		} else {
			u32(0)
		}
		if state == wayland_keyboard_key_state_pressed {
			backend.windows[index].store_user_action_serial(serial, backend.poll_generation)
			if key_index > 0 {
				backend.keys_down[key_index] = true
			}
			backend.update_modifier_for_key(key_code, true)
			backend.start_key_repeat(backend.windows[index], raw_key, key_code)
		} else if state == wayland_keyboard_key_state_released {
			backend.stop_key_repeat_for_key(raw_key)
			if key_index > 0 {
				backend.keys_down[key_index] = false
			}
			backend.update_modifier_for_key(key_code, false)
		} else {
			return
		}
		if key_code != 0 {
			input_kind := if state == wayland_keyboard_key_state_pressed {
				InputEventKind.key_down
			} else {
				InputEventKind.key_up
			}
			input := backend.windows[index].input_event_with_payload(input_kind,
				backend.event_modifiers(), key_code, key_repeat, wayland_invalid_mouse_button, 0, 0)
			backend.windows[index].enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(),
				queued_input_event(input))
			if input_kind == .key_down
				&& wayland_is_clipboard_paste(key_code, backend.event_modifiers()) {
				clipboard_input := backend.windows[index].input_event(.clipboard_pasted,
					backend.event_modifiers())
				backend.windows[index].enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(),
					queued_input_event(clipboard_input))
			}
		}
		if state == wayland_keyboard_key_state_pressed {
			if char_code != 0 {
				char_input := backend.windows[index].input_char_event(char_code, key_repeat,
					backend.event_modifiers())
				backend.windows[index].enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(),
					queued_input_event(char_input))
			}
		}
	}

	@[export: 'v_multiwindow_wayland_keyboard_modifiers']
	@[markused]
	fn wayland_keyboard_modifiers(data voidptr, keyboard voidptr, serial u32, mods_depressed u32, mods_latched u32, mods_locked u32, group u32) {
		_ = keyboard
		_ = serial
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		if backend.xkb_state == unsafe { nil } {
			return
		}
		C.xkb_state_update_mask(unsafe { &C.xkb_state(backend.xkb_state) }, mods_depressed,
			mods_latched, mods_locked, 0, 0, group)
		backend.modifiers = backend.xkb_modifiers()
	}

	@[export: 'v_multiwindow_wayland_touch_down']
	@[markused]
	fn wayland_touch_down(data voidptr, touch voidptr, serial u32, time u32, surface voidptr, id int, x f64, y f64) {
		_ = touch
		_ = time
		if data == unsafe { nil } || surface == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		index := backend.window_record_index_for_surface(surface) or { return }
		slot := backend.touch_slot_for_down(id) or { return }
		backend.touches[slot] = WaylandTouchPoint{
			active:    true
			id:        id
			window_id: backend.windows[index].id
			x:         f32(x)
			y:         f32(y)
		}
		backend.windows[index].store_user_action_serial(serial, backend.poll_generation)
		input := backend.touch_event_for_record(backend.windows[index], .touches_began, slot)
		backend.windows[index].enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(),
			queued_input_event(input))
	}

	@[export: 'v_multiwindow_wayland_touch_up']
	@[markused]
	fn wayland_touch_up(data voidptr, touch voidptr, serial u32, time u32, id int) {
		_ = touch
		_ = serial
		_ = time
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		slot := backend.touch_slot_for_id(id) or { return }
		index := backend.window_record_index(backend.touches[slot].window_id) or {
			backend.touches[slot] = WaylandTouchPoint{}
			return
		}
		mut record := backend.windows[index]
		record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_input_event(backend.touch_event_for_record(record,
			.touches_ended, slot)))
		backend.touches[slot] = WaylandTouchPoint{}
	}

	@[export: 'v_multiwindow_wayland_touch_motion']
	@[markused]
	fn wayland_touch_motion(data voidptr, touch voidptr, time u32, id int, x f64, y f64) {
		_ = touch
		_ = time
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		slot := backend.touch_slot_for_id(id) or { return }
		backend.touches[slot].x = f32(x)
		backend.touches[slot].y = f32(y)
		index := backend.window_record_index(backend.touches[slot].window_id) or { return }
		mut record := backend.windows[index]
		record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_input_event(backend.touch_event_for_record(record,
			.touches_moved, slot)))
	}

	@[export: 'v_multiwindow_wayland_touch_cancel']
	@[markused]
	fn wayland_touch_cancel(data voidptr, touch voidptr) {
		_ = touch
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		for _, mut record in backend.windows {
			if backend.touch_count_for_window(record.id) > 0 {
				record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(),
					queued_input_event(backend.touch_cancel_event_for_record(record)))
			}
		}
		backend.clear_touches()
	}

	@[export: 'v_multiwindow_wayland_data_offer_offer']
	@[markused]
	fn wayland_data_offer_offer(data voidptr, offer voidptr, mime_type &char) {
		if data == unsafe { nil } || offer == unsafe { nil } || mime_type == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		if backend.incoming_offer == offer {
			if C.strcmp(mime_type, c'text/uri-list') == 0 {
				backend.incoming_offer_has_uri_list = true
			} else if C.strcmp(mime_type, c'text/plain;charset=utf-8') == 0 {
				backend.incoming_offer_has_utf8 = true
			} else if C.strcmp(mime_type, c'text/plain') == 0 {
				backend.incoming_offer_has_text = true
			}
		} else if backend.data_offer == offer && C.strcmp(mime_type, c'text/uri-list') == 0 {
			backend.data_offer_has_uri_list = true
		} else if backend.selection_offer == offer {
			if C.strcmp(mime_type, c'text/plain;charset=utf-8') == 0 {
				backend.selection_offer_has_utf8 = true
			} else if C.strcmp(mime_type, c'text/plain') == 0 {
				backend.selection_offer_has_text = true
			}
		}
	}

	@[export: 'v_multiwindow_wayland_data_offer_source_actions']
	@[markused]
	fn wayland_data_offer_source_actions(data voidptr, offer voidptr, source_actions u32) {
		if data == unsafe { nil } || offer == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		if backend.data_offer == offer {
			backend.data_offer_source_actions = source_actions
		}
		if backend.pending_drop_offer == offer {
			backend.pending_drop_source_actions = source_actions
		}
	}

	@[export: 'v_multiwindow_wayland_data_offer_action']
	@[markused]
	fn wayland_data_offer_action(data voidptr, offer voidptr, dnd_action u32) {
		if data == unsafe { nil } || offer == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		if backend.data_offer == offer {
			backend.data_offer_selected_action = dnd_action
			backend.data_offer_action_received = true
		}
		if backend.pending_drop_offer == offer {
			backend.pending_drop_selected_action = dnd_action
			backend.pending_drop_action_received = true
		}
	}

	@[export: 'v_multiwindow_wayland_data_device_data_offer']
	@[markused]
	fn wayland_data_device_data_offer(data voidptr, device voidptr, offer voidptr) {
		_ = device
		if data == unsafe { nil } || offer == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		backend.clear_incoming_offer(true)
		backend.incoming_offer = offer
		backend.incoming_offer_has_uri_list = false
		backend.incoming_offer_has_utf8 = false
		backend.incoming_offer_has_text = false
		if C.v_multiwindow_wayland_add_data_offer_listener(unsafe { &C.wl_data_offer(offer) }, data) < 0 {
			backend.clear_incoming_offer(true)
		}
	}

	@[export: 'v_multiwindow_wayland_data_device_enter']
	@[markused]
	fn wayland_data_device_enter(data voidptr, device voidptr, serial u32, surface voidptr, x f64, y f64, offer voidptr) {
		_ = device
		if data == unsafe { nil } || surface == unsafe { nil } || offer == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		if !backend.promote_incoming_offer_to_dnd(offer) || !backend.data_offer_has_uri_list {
			return
		}
		if !backend.transport_can_marshal() || backend.render_health.blocks_graphics() {
			return
		}
		index := backend.window_record_index_for_surface(surface) or { return }
		mut record := backend.windows[index]
		record.update_mouse_position(f32(x), f32(y), true)
		backend.data_offer_window = record.id
		backend.data_offer_window_valid = true
		C.v_multiwindow_wayland_data_offer_accept(unsafe { &C.wl_data_offer(offer) }, serial,
			c'text/uri-list')
		C.v_multiwindow_wayland_data_offer_set_copy_action(unsafe { &C.wl_data_offer(offer) })
	}

	@[export: 'v_multiwindow_wayland_data_device_leave']
	@[markused]
	fn wayland_data_device_leave(data voidptr, device voidptr) {
		_ = device
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		backend.clear_data_offer(true)
	}

	@[export: 'v_multiwindow_wayland_data_device_motion']
	@[markused]
	fn wayland_data_device_motion(data voidptr, device voidptr, time u32, x f64, y f64) {
		_ = device
		_ = time
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		if !backend.data_offer_window_valid {
			return
		}
		index := backend.window_record_index(backend.data_offer_window) or { return }
		mut record := backend.windows[index]
		record.update_mouse_position(f32(x), f32(y), false)
	}

	@[export: 'v_multiwindow_wayland_data_device_drop']
	@[markused]
	fn wayland_data_device_drop(data voidptr, device voidptr) {
		_ = device
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		if backend.data_offer == unsafe { nil } || !backend.data_offer_has_uri_list
			|| !backend.data_offer_window_valid {
			backend.clear_data_offer(true)
			return
		}
		backend.window_record_index(backend.data_offer_window) or {
			backend.clear_data_offer(true)
			return
		}
		if !backend.data_offer_allows_finish() || !backend.begin_pending_data_offer_drop() {
			backend.clear_data_offer(true)
		}
	}

	@[export: 'v_multiwindow_wayland_data_device_selection']
	@[markused]
	fn wayland_data_device_selection(data voidptr, device voidptr, offer voidptr) {
		_ = device
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		backend.set_selection_offer(offer)
	}

	@[export: 'v_multiwindow_wayland_data_source_target']
	@[markused]
	fn wayland_data_source_target(data voidptr, source voidptr, mime_type &char) {
		_ = data
		_ = source
		_ = mime_type
	}

	@[export: 'v_multiwindow_wayland_data_source_send']
	@[markused]
	fn wayland_data_source_send(data voidptr, source voidptr, mime_type &char, fd int) {
		if fd < 0 {
			return
		}
		if data == unsafe { nil } || source == unsafe { nil } || mime_type == unsafe { nil } {
			C.close(fd)
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		mut payload := ''
		mut source_known := false
		if source == backend.clipboard_source {
			payload = backend.clipboard_text.clone()
			source_known = true
		}
		if !source_known || (C.strcmp(mime_type, c'text/plain;charset=utf-8') != 0
			&& C.strcmp(mime_type, c'text/plain') != 0)
			|| C.v_multiwindow_wayland_fd_set_nonblocking(fd) == 0 {
			C.close(fd)
			return
		}
		payload_bytes := u64(payload.len)
		if backend.clipboard_sends.len >= wayland_clipboard_max_outgoing_transfers
			|| payload_bytes > wayland_clipboard_max_outgoing_snapshot_bytes
			|| backend.clipboard_send_snapshot_bytes > wayland_clipboard_max_outgoing_snapshot_bytes - payload_bytes {
			C.close(fd)
			return
		}
		backend.clipboard_sends << WaylandClipboardSend{
			fd:          fd
			payload:     payload
			deadline_ns: vtime.sys_mono_now() + wayland_clipboard_timeout_ns
		}
		backend.clipboard_send_snapshot_bytes += payload_bytes
	}

	@[export: 'v_multiwindow_wayland_data_source_cancelled']
	@[markused]
	fn wayland_data_source_cancelled(data voidptr, source voidptr) {
		if data == unsafe { nil } || source == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		if source == backend.clipboard_source {
			backend.destroy_clipboard_source(!backend.transport_can_marshal())
		}
	}
}

fn new_wayland_backend() WaylandBackend {
	return WaylandBackend{}
}

fn (backend &WaylandBackend) native_app_id() string {
	return if backend.app_id == '' { 'v.x.multiwindow' } else { backend.app_id }
}

fn (backend &WaylandBackend) mapped_owner_toplevel(owner_id ?WindowId) voidptr {
	owner := owner_id or { return unsafe { nil } }
	owner_index := backend.window_record_index(owner) or { return unsafe { nil } }
	owner_record := backend.windows[owner_index]
	if !owner_record.mapped || owner_record.xdg_toplevel == unsafe { nil } {
		return unsafe { nil }
	}
	return owner_record.xdg_toplevel
}

fn (mut backend WaylandBackend) replay_window_toplevel_attributes(index int) {
	$if linux && sokol_wayland ? {
		if index < 0 || index >= backend.windows.len {
			return
		}
		record := backend.windows[index]
		if record.xdg_toplevel == unsafe { nil } {
			return
		}
		owner_toplevel := backend.mapped_owner_toplevel(record.owner_id)
		$if test {
			if backend.toplevel_replay_test.active {
				backend.toplevel_replay_test.operations << 'title'
				backend.toplevel_replay_test.operations << 'app_id'
				if owner_toplevel != unsafe { nil } {
					backend.toplevel_replay_test.operations << 'parent'
				}
				backend.toplevel_replay_test.operations << 'min_size'
				backend.toplevel_replay_test.operations << 'max_size'
				if record.request_server_decoration && record.toplevel_decoration != unsafe { nil } {
					backend.toplevel_replay_test.operations << 'decoration'
				}
				if record.requested_maximized {
					backend.toplevel_replay_test.operations << 'maximize'
				} else {
					backend.toplevel_replay_test.operations << 'unmaximize'
				}
				if record.requested_fullscreen {
					backend.toplevel_replay_test.operations << 'fullscreen'
				} else {
					backend.toplevel_replay_test.operations << 'unfullscreen'
				}
				return
			}
		}
		toplevel := unsafe { &C.xdg_toplevel(record.xdg_toplevel) }
		C.v_multiwindow_wayland_xdg_toplevel_set_title(toplevel, &char(record.title.str))
		C.v_multiwindow_wayland_xdg_toplevel_set_app_id(toplevel, &char(record.app_id.str))
		if owner_toplevel != unsafe { nil } {
			C.v_multiwindow_wayland_xdg_toplevel_set_parent(toplevel, unsafe {
				&C.xdg_toplevel(owner_toplevel)
			})
		}
		C.v_multiwindow_wayland_xdg_toplevel_set_min_size(toplevel, i32(record.min_width),
			i32(record.min_height))
		C.v_multiwindow_wayland_xdg_toplevel_set_max_size(toplevel, i32(record.max_width),
			i32(record.max_height))
		if record.request_server_decoration && record.toplevel_decoration != unsafe { nil } {
			C.v_multiwindow_wayland_xdg_toplevel_decoration_set_server_side(unsafe {
				&C.zxdg_toplevel_decoration_v1(record.toplevel_decoration)
			})
		}
		if record.requested_maximized {
			C.v_multiwindow_wayland_xdg_toplevel_set_maximized(toplevel)
		} else {
			C.v_multiwindow_wayland_xdg_toplevel_unset_maximized(toplevel)
		}
		if record.requested_fullscreen {
			C.v_multiwindow_wayland_xdg_toplevel_set_fullscreen(toplevel, unsafe { nil })
		} else {
			C.v_multiwindow_wayland_xdg_toplevel_unset_fullscreen(toplevel)
		}
	}
}

fn (mut backend WaylandBackend) request_window_show_probe(index int, axis WaylandShowProbeAxis, enabled bool) {
	$if linux && sokol_wayland ? {
		if index < 0 || index >= backend.windows.len {
			return
		}
		$if test {
			if backend.toplevel_replay_test.show_handshake_override {
				operation := match axis {
					.maximized {
						if enabled { 'probe_maximize' } else { 'probe_unmaximize' }
					}
					.fullscreen {
						if enabled { 'probe_fullscreen' } else { 'probe_unfullscreen' }
					}
				}
				backend.toplevel_replay_test.operations << operation
				return
			}
		}
		record := backend.windows[index]
		if record.xdg_toplevel == unsafe { nil } {
			return
		}
		toplevel := unsafe { &C.xdg_toplevel(record.xdg_toplevel) }
		match axis {
			.maximized {
				if enabled {
					C.v_multiwindow_wayland_xdg_toplevel_set_maximized(toplevel)
				} else {
					C.v_multiwindow_wayland_xdg_toplevel_unset_maximized(toplevel)
				}
			}
			.fullscreen {
				if enabled {
					C.v_multiwindow_wayland_xdg_toplevel_set_fullscreen(toplevel, unsafe { nil })
				} else {
					C.v_multiwindow_wayland_xdg_toplevel_unset_fullscreen(toplevel)
				}
			}
		}
	}
}

fn (mut backend WaylandBackend) request_window_show_final_intents(index int, maximized bool, fullscreen bool) {
	$if linux && sokol_wayland ? {
		if index < 0 || index >= backend.windows.len {
			return
		}
		$if test {
			if backend.toplevel_replay_test.show_handshake_override {
				backend.toplevel_replay_test.operations << if maximized {
					'final_maximize'
				} else {
					'final_unmaximize'
				}
				backend.toplevel_replay_test.operations << if fullscreen {
					'final_fullscreen'
				} else {
					'final_unfullscreen'
				}
				return
			}
		}
		record := backend.windows[index]
		if record.xdg_toplevel == unsafe { nil } {
			return
		}
		toplevel := unsafe { &C.xdg_toplevel(record.xdg_toplevel) }
		if maximized {
			C.v_multiwindow_wayland_xdg_toplevel_set_maximized(toplevel)
		} else {
			C.v_multiwindow_wayland_xdg_toplevel_unset_maximized(toplevel)
		}
		if fullscreen {
			C.v_multiwindow_wayland_xdg_toplevel_set_fullscreen(toplevel, unsafe { nil })
		} else {
			C.v_multiwindow_wayland_xdg_toplevel_unset_fullscreen(toplevel)
		}
	}
}

fn (mut backend WaylandBackend) commit_window_show_handshake(index int) {
	$if linux && sokol_wayland ? {
		$if test {
			if backend.toplevel_replay_test.show_handshake_override {
				backend.toplevel_replay_test.operations << 'commit'
				return
			}
		}
		if index >= 0 && index < backend.windows.len
			&& backend.windows[index].surface != unsafe { nil } {
			C.wl_surface_commit(unsafe { &C.wl_surface(backend.windows[index].surface) })
		}
	}
}

fn (mut backend WaylandBackend) inject_window_show_test_observation(index int, boundary u32) {
	$if test {
		if !backend.toplevel_replay_test.show_handshake_override || boundary == 0 {
			return
		}
		observation_index := int(boundary - 1)
		if observation_index < 0
			|| observation_index >= backend.toplevel_replay_test.show_observations.len {
			return
		}
		observation := backend.toplevel_replay_test.show_observations[observation_index]
		if !observation.present || index < 0 || index >= backend.windows.len {
			return
		}
		mut record := backend.windows[index]
		record.show_configure_boundary = boundary
		record.show_configure_serial = if observation.serial == 0 && !observation.serial_set {
			boundary
		} else {
			observation.serial
		}
		record.show_configure_received = true
		record.show_configure_state_valid = true
		record.show_configure_width = if observation.width > 0 {
			observation.width
		} else {
			record.width
		}
		record.show_configure_height = if observation.height > 0 {
			observation.height
		} else {
			record.height
		}
		record.show_configure_maximized = observation.maximized
		record.show_configure_fullscreen = observation.fullscreen
		record.show_configure_active = observation.active
		backend.toplevel_replay_test.operations << 'configure'
	} $else {
		_ = index
		_ = boundary
	}
}

fn (mut backend WaylandBackend) drain_hidden_window_before_show(mut transport WaylandServiceTransportPlan) ! {
	$if test {
		if backend.toplevel_replay_test.show_handshake_override {
			backend.toplevel_replay_test.operations << 'hidden_drain'
			if backend.toplevel_replay_test.show_hidden_drain_failure {
				return error(err_wayland_dispatch_failed)
			}
			return
		}
	}
	_ = backend.execute_prepared_service_transport(transport.take())!
}

fn (mut backend WaylandBackend) run_window_show_handshake_boundary(index int, mut transport WaylandServiceTransportPlan, commit bool) !bool {
	if index < 0 || index >= backend.windows.len {
		return error(err_window_not_found)
	}
	mut record := backend.windows[index]
	record.show_handshake_boundary++
	boundary := record.show_handshake_boundary
	record.pending_service_state_valid = false
	if commit {
		backend.commit_window_show_handshake(index)
	}
	$if test {
		if backend.toplevel_replay_test.show_handshake_override {
			backend.toplevel_replay_test.show_flush_count++
			backend.toplevel_replay_test.operations << 'flush'
			if backend.toplevel_replay_test.show_flush_failure_boundary == boundary {
				return error(err_wayland_flush_failed)
			}
			backend.toplevel_replay_test.show_roundtrip_count++
			backend.toplevel_replay_test.operations << 'roundtrip'
			if backend.toplevel_replay_test.show_roundtrip_failure_boundary == boundary {
				return error(err_wayland_dispatch_failed)
			}
			backend.inject_window_show_test_observation(index, boundary)
			return record.show_configure_boundary == boundary && record.show_configure_received
				&& record.show_configure_state_valid
		}
	}
	flush_attempt := transport.take()
	roundtrip_attempt := transport.take()
	_ = backend.execute_prepared_service_transport(flush_attempt)!
	_ = backend.execute_prepared_service_transport(roundtrip_attempt)!
	return record.show_configure_boundary == boundary && record.show_configure_received
		&& record.show_configure_state_valid
}

fn (mut backend WaylandBackend) ack_window_show_configure(index int, cleanup bool) {
	if index < 0 || index >= backend.windows.len {
		return
	}
	mut record := backend.windows[index]
	if !record.show_configure_received {
		return
	}
	$if linux && sokol_wayland ? {
		$if test {
			if backend.toplevel_replay_test.show_handshake_override {
				backend.toplevel_replay_test.show_acked_serials << record.show_configure_serial
				backend.toplevel_replay_test.operations << if cleanup {
					'ack_cleanup'
				} else {
					'ack'
				}
				record.show_configure_serial = 0
				record.show_configure_received = false
				return
			}
		}
		if backend.transport_can_marshal() && record.xdg_surface != unsafe { nil } {
			C.v_multiwindow_wayland_xdg_surface_ack_configure(unsafe {
				&C.xdg_surface(record.xdg_surface)
			}, record.show_configure_serial)
			if record.uses_fractional_scale() && record.show_configure_width > 0
				&& record.show_configure_height > 0 {
				C.v_multiwindow_wayland_viewport_set_destination(unsafe {
					&C.wp_viewport(record.viewport)
				}, i32(record.show_configure_width), i32(record.show_configure_height))
			}
		}
	}
	record.show_configure_serial = 0
	record.show_configure_received = false
}

fn (mut backend WaylandBackend) abort_window_show_handshake(index int, mut transport WaylandServiceTransportPlan, maximized bool, fullscreen bool) {
	if index < 0 || index >= backend.windows.len {
		return
	}
	mut record := backend.windows[index]
	mut cleanup_attempt := WaylandPreparedTransportAttempt{}
	mut flush_cleanup := record.show_configure_received && backend.transport_can_marshal()
	$if test {
		if backend.toplevel_replay_test.show_handshake_override {
			flush_cleanup = false
		}
	}
	if flush_cleanup && transport.next < transport.attempts.len {
		cleanup_attempt = transport.take()
	} else {
		flush_cleanup = false
	}
	backend.ack_window_show_configure(index, true)
	if flush_cleanup {
		backend.consume_prepared_service_cleanup(cleanup_attempt)
	}
	record.requested_maximized = maximized
	record.requested_fullscreen = fullscreen
	record.requested_visible = false
	record.publish_show_on_map = false
	record.mapped = false
	record.configured = false
	record.frame_ready = false
	record.pending_toplevel_width = 0
	record.pending_toplevel_height = 0
	record.pending_service_state_valid = false
	record.show_handshake_active = false
	record.show_handshake_boundary = 0
	record.show_configure_boundary = 0
	record.show_configure_serial = 0
	record.show_configure_received = false
	record.show_configure_state_valid = false
	record.show_configure_width = 0
	record.show_configure_height = 0
	record.observed_service_state_valid = false
}

fn (mut backend WaylandBackend) flush_window_show_ack(attempt WaylandPreparedTransportAttempt) ! {
	$if test {
		if backend.toplevel_replay_test.show_handshake_override {
			backend.toplevel_replay_test.operations << 'ack_flush'
			if backend.toplevel_replay_test.show_ack_flush_failure {
				return error(err_wayland_flush_failed)
			}
			return
		}
	}
	_ = backend.execute_prepared_service_transport(attempt)!
}

fn (mut backend WaylandBackend) finish_window_show_handshake(index int, mut transport WaylandServiceTransportPlan, maximized bool, fullscreen bool) ! {
	mut record := backend.windows[index]
	mut flush_attempt := WaylandPreparedTransportAttempt{}
	mut show_handshake_override := false
	$if test {
		show_handshake_override = backend.toplevel_replay_test.show_handshake_override
	}
	if !show_handshake_override {
		flush_attempt = transport.take()
	}
	backend.ack_window_show_configure(index, false)
	backend.flush_window_show_ack(flush_attempt)!
	if record.show_configure_width > 0 && record.show_configure_height > 0
		&& (record.width != record.show_configure_width
		|| record.height != record.show_configure_height) {
		record.width = record.show_configure_width
		record.height = record.show_configure_height
		if record.wl_egl_window != unsafe { nil } {
			record.pending_egl_resize = true
			record.render_target_generation =
				exhaust_backend_target_generation(record.render_target_generation)
		}
	}
	record.requested_maximized = maximized
	record.requested_fullscreen = fullscreen
	record.observed_service_state_valid = true
	record.observed_maximized = record.show_configure_maximized
	record.observed_fullscreen = record.show_configure_fullscreen
	record.observed_active = record.show_configure_active
	record.pending_service_state_valid = false
	record.show_handshake_active = false
	record.show_handshake_boundary = 0
	record.show_configure_boundary = 0
	record.show_configure_serial = 0
	record.show_configure_received = false
	record.show_configure_state_valid = false
	record.show_configure_width = 0
	record.show_configure_height = 0
	record.configured = true
	record.frame_ready = true
	record.requested_visible = true
	record.publish_show_on_map = true
	record.mapped = false
}

fn (mut backend WaylandBackend) prepare_window_show_handshake(index int, mut transport WaylandServiceTransportPlan, maximized bool, fullscreen bool, baseline_maximized bool, baseline_fullscreen bool) ! {
	mut record := backend.windows[index]
	record.requested_visible = false
	record.publish_show_on_map = false
	record.mapped = false
	record.configured = false
	record.frame_ready = false
	record.pending_toplevel_width = 0
	record.pending_toplevel_height = 0
	record.pending_service_state_valid = false
	record.show_handshake_active = true
	record.show_handshake_boundary = 0
	record.show_configure_boundary = 0
	record.show_configure_serial = 0
	record.show_configure_received = false
	record.show_configure_state_valid = false
	record.show_configure_width = 0
	record.show_configure_height = 0

	// Weston #893 does not emit the spec-required fresh configure for an empty
	// first commit after unmap. Two state probes provide a bounded, fail-closed
	// bootstrap without recreating the permanent wl_surface role.
	backend.replay_window_toplevel_attributes(index)
	first_axis := if baseline_fullscreen {
		WaylandShowProbeAxis.fullscreen
	} else {
		WaylandShowProbeAxis.maximized
	}
	first_enabled := match first_axis {
		.maximized { !baseline_maximized }
		.fullscreen { false }
	}
	backend.request_window_show_probe(index, first_axis, first_enabled)
	mut configured := backend.run_window_show_handshake_boundary(index, mut transport, true) or {
		backend.abort_window_show_handshake(index, mut transport, maximized, fullscreen)
		return err
	}
	if !configured {
		second_axis := if first_axis == .fullscreen {
			WaylandShowProbeAxis.maximized
		} else {
			WaylandShowProbeAxis.fullscreen
		}
		second_enabled := match second_axis {
			.maximized { !baseline_maximized }
			.fullscreen { !baseline_fullscreen }
		}
		backend.request_window_show_probe(index, second_axis, second_enabled)
		configured = backend.run_window_show_handshake_boundary(index, mut transport, false) or {
			backend.abort_window_show_handshake(index, mut transport, maximized, fullscreen)
			return err
		}
	}
	if !configured {
		backend.abort_window_show_handshake(index, mut transport, maximized, fullscreen)
		return error(err_wayland_show_configure_failed)
	}
	backend.request_window_show_final_intents(index, maximized, fullscreen)
	_ = backend.run_window_show_handshake_boundary(index, mut transport, false) or {
		backend.abort_window_show_handshake(index, mut transport, maximized, fullscreen)
		return err
	}
	backend.finish_window_show_handshake(index, mut transport, maximized, fullscreen) or {
		backend.abort_window_show_handshake(index, mut transport, maximized, fullscreen)
		return err
	}
}

fn (mut backend WaylandBackend) reapply_parent_to_live_children(parent WindowId) {
	$if linux && sokol_wayland ? {
		parent_index := backend.window_record_index(parent) or { return }
		parent_record := backend.windows[parent_index]
		if parent_record.xdg_toplevel == unsafe { nil } {
			return
		}
		for index in 0 .. backend.windows.len {
			child := backend.windows[index]
			owner := child.owner_id or { continue }
			if owner != parent || !child.mapped || child.xdg_toplevel == unsafe { nil } {
				continue
			}
			$if test {
				if backend.toplevel_replay_test.active {
					backend.toplevel_replay_test.operations << 'child_parent'
					continue
				}
			}
			C.v_multiwindow_wayland_xdg_toplevel_set_parent(unsafe {
				&C.xdg_toplevel(child.xdg_toplevel)
			}, unsafe { &C.xdg_toplevel(parent_record.xdg_toplevel) })
		}
	}
}

fn (mut backend WaylandBackend) reapply_parent_to_live_children_on_first_map(parent WindowId, was_mapped bool) {
	if was_mapped {
		return
	}
	backend.reapply_parent_to_live_children(parent)
}

fn (mut backend WaylandBackend) observe_window_mapped(index int) {
	if index < 0 || index >= backend.windows.len || backend.windows[index].mapped {
		return
	}
	backend.windows[index].mapped = true
	if backend.windows[index].publish_show_on_map {
		backend.windows[index].publish_show_on_map = false
		backend.windows[index].enqueue_service_state(.show)
	}
}

fn (backend &WaylandBackend) cursor_support(shape CursorShape) ServiceSupportLevel {
	if wayland_cursor_shape_for_shape(shape) == 0 {
		return .unsupported
	}
	return if backend.can_set_cursor_shapes() { .available } else { .conditional }
}

fn (backend &WaylandBackend) ensure_supported() ! {
	$if linux && sokol_wayland ? {
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (backend &WaylandBackend) capabilities() Capabilities {
	return Capabilities{
		backend:                 .wayland
		mock:                    false
		native:                  true
		multi_window:            true
		owner_queue:             true
		explicit_swapchain:      backend.renderer_ready()
		readback:                backend.renderer_ready()
		wayland:                 true
		gl:                      backend.renderer_ready()
		input_events:            true
		mouse_events:            true
		keyboard_events:         true
		text_events:             true
		focus_events:            true
		drop_events:             backend.can_deliver_drop_events()
		touch_events:            backend.can_deliver_touch_events()
		cursor_shapes:           backend.can_set_cursor_shapes()
		interactive_move_resize: backend.can_begin_interactive_move_resize()
		native_decorations:      backend.has_server_side_decorated_window()
	}
}

fn (backend &WaylandBackend) start_attempt_closed() bool {
	return !backend.started && !backend.retains_native_ownership()
}

fn (mut backend WaylandBackend) close_start_attempt() string {
	mut close_error := ''
	backend.stop() or { close_error = err.msg() }
	if !backend.start_attempt_closed() {
		close_error = merge_backend_errors(close_error, err_render_native_renderer_unavailable)
	}
	return close_error
}

fn (mut backend WaylandBackend) probe_renderer_capabilities() !Capabilities {
	$if linux && sokol_wayland ? {
		if !backend.start_attempt_closed() {
			close_error := backend.close_start_attempt()
			if close_error != '' {
				return error(close_error)
			}
		}
		if backend.render_health.blocks_graphics() || backend.native_operations == unsafe { nil } {
			return error(err_render_native_renderer_unavailable)
		}
		display := C.wl_display_connect(unsafe { nil })
		if display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		backend.display = unsafe { voidptr(display) }
		backend.wayland_display_unavailable = false
		backend.wayland_display_error = 0
		backend.init_renderer() or {
			probe_error := err.msg()
			close_error := backend.close_start_attempt()
			return error(merge_backend_errors(probe_error, close_error))
		}
		caps := backend.capabilities()
		close_error := backend.close_start_attempt()
		if close_error != '' {
			return error(close_error)
		}
		if !backend.start_attempt_closed() {
			return error(err_render_native_renderer_unavailable)
		}
		return caps
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (backend &WaylandBackend) can_deliver_drop_events() bool {
	if !backend.started {
		return true
	}
	return backend.seat != unsafe { nil } && backend.data_device_manager != unsafe { nil }
		&& backend.data_device != unsafe { nil }
}

fn (backend &WaylandBackend) can_deliver_touch_events() bool {
	if !backend.started {
		return true
	}
	return backend.seat != unsafe { nil } && backend.touch != unsafe { nil }
}

fn (backend &WaylandBackend) can_begin_interactive_move_resize() bool {
	if !backend.started {
		return true
	}
	return backend.seat != unsafe { nil } && backend.wm_base != unsafe { nil }
}

fn (backend &WaylandBackend) can_set_cursor_shapes() bool {
	return backend.pointer != unsafe { nil } && backend.cursor_shape_manager != unsafe { nil }
		&& backend.cursor_shape_device != unsafe { nil }
}

fn (backend &WaylandBackend) has_server_side_decorated_window() bool {
	if !backend.started {
		return true
	}
	mut has_server_side := false
	for record in backend.windows {
		if record.has_client_side_decoration() {
			return false
		}
		if record.has_server_side_decoration() {
			has_server_side = true
		}
	}
	return has_server_side
}

fn (backend &WaylandBackend) window_native_decorations(id WindowId) !bool {
	index := backend.window_record_index(id) or { return error(err_window_not_found) }
	return backend.windows[index].has_server_side_decoration()
}

fn (record &WaylandWindowRecord) has_server_side_decoration() bool {
	return record.toplevel_decoration != unsafe { nil } && record.toplevel_decoration_configured
		&& record.toplevel_decoration_mode == wayland_xdg_toplevel_decoration_mode_server_side
}

fn (record &WaylandWindowRecord) has_client_side_decoration() bool {
	return record.toplevel_decoration == unsafe { nil }
		|| (record.toplevel_decoration_configured
		&& record.toplevel_decoration_mode == wayland_xdg_toplevel_decoration_mode_client_side)
}

fn (mut backend WaylandBackend) start(require_renderer bool) ! {
	$if linux && sokol_wayland ? {
		if backend.started {
			return
		}
		if backend.retains_native_ownership() {
			cleanup_error := backend.close_connection()
			if backend.retains_native_ownership() {
				return error(merge_backend_errors(cleanup_error,
					err_render_native_renderer_unavailable))
			}
			if cleanup_error != '' {
				return error(cleanup_error)
			}
		}
		if backend.render_health.blocks_graphics() || backend.wayland_display_unavailable {
			return error(err_render_native_renderer_unavailable)
		}
		if backend.native_operations == unsafe { nil } {
			return error(err_render_native_renderer_unavailable)
		}
		display := C.wl_display_connect(unsafe { nil })
		if display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		registry := C.wl_display_get_registry(display)
		if registry == unsafe { nil } {
			C.v_multiwindow_wayland_display_disconnect(display)
			return error(err_wayland_registry_failed)
		}
		backend.display = unsafe { voidptr(display) }
		backend.registry = unsafe { voidptr(registry) }
		backend.wayland_display_unavailable = false
		backend.wayland_display_error = 0
		if C.v_multiwindow_wayland_add_registry_listener(registry, backend.registry_listener_data()) < 0 {
			close_error := backend.close_connection()
			return error(merge_backend_errors(err_wayland_registry_failed, close_error))
		}
		startup_seed := NativeOperationSeed{
			call_site: .app_start
			scope:     .renderer
		}
		initial_roundtrip := backend.attempt_wayland_roundtrip(startup_seed)
		if !initial_roundtrip.succeeded() {
			close_error := backend.close_connection()
			return error(merge_backend_errors(err_wayland_dispatch_failed, close_error))
		}
		if backend.compositor == unsafe { nil } || backend.compositor_name == 0
			|| backend.wm_base == unsafe { nil } || backend.wm_base_name == 0 {
			close_error := backend.close_connection()
			return error(merge_backend_errors(err_wayland_required_globals_missing, close_error))
		}
		backend.init_xkb() or {
			start_error := err.msg()
			close_error := backend.close_connection()
			return error(merge_backend_errors(start_error, close_error))
		}
		wm_base := unsafe { &C.xdg_wm_base(backend.wm_base) }
		if C.v_multiwindow_wayland_add_xdg_wm_base_listener(wm_base,
			backend.registry_listener_data()) < 0 {
			close_error := backend.close_connection()
			return error(merge_backend_errors(err_wayland_registry_failed, close_error))
		}
		if backend.seat != unsafe { nil } {
			if C.v_multiwindow_wayland_add_seat_listener(unsafe { &C.wl_seat(backend.seat) },
				backend.registry_listener_data()) < 0 {
				close_error := backend.close_connection()
				return error(merge_backend_errors(err_wayland_registry_failed, close_error))
			}
		}
		discovery_roundtrip := backend.attempt_wayland_roundtrip(startup_seed)
		if !discovery_roundtrip.succeeded() {
			close_error := backend.close_connection()
			return error(merge_backend_errors(err_wayland_dispatch_failed, close_error))
		}
		if require_renderer {
			backend.init_renderer() or {
				start_error := err.msg()
				close_error := backend.close_connection()
				return error(merge_backend_errors(start_error, close_error))
			}
		}
		backend.started = true
		return
	} $else {
		_ = require_renderer
		return error(err_backend_unsupported)
	}
}

fn (backend &WaylandBackend) renderer_ready() bool {
	return backend.egl_display != unsafe { nil } && backend.egl_config != unsafe { nil }
		&& backend.egl_context != unsafe { nil } && backend.egl_display_ticket != 0
		&& backend.egl_context_ticket != 0 && backend.egl_thread_ticket != 0
		&& backend.render_health == .ready
}

fn (mut backend WaylandBackend) init_renderer() ! {
	$if linux && sokol_wayland ? {
		if backend.renderer_ready() {
			return
		}
		if backend.render_health.blocks_graphics() {
			return error(err_render_native_renderer_unavailable)
		}
		if backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		if backend.native_operations == unsafe { nil } {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_render_native_renderer_unavailable)
		}
		seed := NativeOperationSeed{
			call_site: .renderer_start
			scope:     .renderer
		}
		mut ordinals := backend.native_operations.reserve_renderer_attempt_ordinals(12) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_render_native_renderer_unavailable)
		}
		mut cleanup_ordinals := backend.native_operations.reserve_app_lifetime_ordinals(3) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_render_native_renderer_unavailable)
		}
		lifetime := backend.native_operations.reserve_linux_egl_renderer_lifetime_tickets(mut cleanup_ordinals, seed) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_render_native_renderer_unavailable)
		}
		backend.egl_display_ticket = lifetime.display
		backend.egl_context_ticket = lifetime.context
		backend.egl_thread_ticket = lifetime.thread
		mut raw := C.VMultiwindowNativePrimitive{}
		display_seed := seed.with_target_identity(native_identity(backend.display))
		display_context := ordinals.materialize(backend.native_operations, .egl, .display_acquire,
			display_seed) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			backend.release_egl_lifetime()
			return error(err_render_native_renderer_unavailable)
		}
		C.v_multiwindow_linux_egl_get_display(native_identity(backend.display), &raw)
		backend.native_operations.bind_linux_egl_thread_lifetime_ticket(backend.egl_thread_ticket)
		actual_display := raw.handle
		if actual_display != 0 {
			backend.egl_display = native_pointer(actual_display)
		}
		display_result := backend.accept_egl_result(display_context, mut ordinals, display_seed,
			raw, .none)
		if actual_display == 0 {
			backend.native_operations.burn_lifetime_ticket(backend.egl_display_ticket)
			backend.native_operations.burn_lifetime_ticket(backend.egl_context_ticket)
			backend.egl_display_ticket = 0
			backend.egl_context_ticket = 0
		}
		if !display_result.succeeded() {
			backend.release_egl_lifetime()
			return error(err_wayland_egl_display_failed)
		}
		initialize_seed := seed.with_target_identity(actual_display)
		initialize_context := ordinals.materialize(backend.native_operations, .egl,
			.display_initialize, initialize_seed) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			backend.release_egl_lifetime()
			return error(err_render_native_renderer_unavailable)
		}
		C.v_multiwindow_linux_egl_initialize(actual_display, &raw)
		if raw.return_value == 1 {
			backend.native_operations.bind_lifetime_ticket(backend.egl_display_ticket,
				actual_display, 0)
		}
		initialize_result := backend.accept_egl_result(initialize_context, mut ordinals,
			initialize_seed, raw, .none)
		if raw.return_value != 1
			&& backend.native_operations.burn_lifetime_ticket(backend.egl_display_ticket) {
			backend.egl_display_ticket = 0
		}
		if !initialize_result.succeeded() {
			backend.release_egl_lifetime()
			return error(err_wayland_egl_display_failed)
		}
		extensions_context := ordinals.materialize(backend.native_operations, .egl, .display_query,
			initialize_seed) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			backend.release_egl_lifetime()
			return error(err_render_native_renderer_unavailable)
		}
		C.v_multiwindow_linux_egl_query_extensions(actual_display, &raw)
		extensions_result := backend.accept_egl_context_requirements(extensions_context, mut
			ordinals, initialize_seed, raw, initialize_result)
		if !extensions_result.succeeded() {
			backend.release_egl_lifetime()
			return error(err_wayland_egl_context_failed)
		}
		bind_seed := seed.without_target_identity()
		bind_context := ordinals.materialize(backend.native_operations, .egl, .api_bind, bind_seed) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			backend.release_egl_lifetime()
			return error(err_render_native_renderer_unavailable)
		}
		C.v_multiwindow_linux_egl_bind_opengl_api(&raw)
		bind_result := backend.accept_egl_result(bind_context, mut ordinals, bind_seed, raw, .none)
		if !bind_result.succeeded() {
			backend.release_egl_lifetime()
			return error(err_wayland_egl_context_failed)
		}
		config_context := ordinals.materialize(backend.native_operations, .egl, .config_choose,
			initialize_seed) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			backend.release_egl_lifetime()
			return error(err_render_native_renderer_unavailable)
		}
		C.v_multiwindow_linux_egl_choose_wayland_config(actual_display, &raw)
		actual_config := raw.handle
		config_result := backend.accept_egl_result(config_context, mut ordinals, initialize_seed,
			raw, .none)
		if !config_result.succeeded() {
			backend.release_egl_lifetime()
			return error(err_wayland_egl_config_failed)
		}
		config_seed := seed.with_target_identity(actual_config)
		context_context := ordinals.materialize(backend.native_operations, .egl, .context_create,
			config_seed) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			backend.release_egl_lifetime()
			return error(err_render_native_renderer_unavailable)
		}
		C.v_multiwindow_linux_egl_create_context(actual_display, actual_config, &raw)
		actual_context := native_pointer(raw.handle)
		if actual_context != unsafe { nil } {
			backend.egl_context = actual_context
			backend.native_operations.bind_lifetime_ticket(backend.egl_context_ticket,
				native_identity(actual_context), actual_display)
		}
		context_result := backend.accept_egl_result(context_context, mut ordinals, config_seed,
			raw, .none)
		if actual_context == unsafe { nil }
			&& backend.native_operations.burn_lifetime_ticket(backend.egl_context_ticket) {
			backend.egl_context_ticket = 0
		}
		if !context_result.succeeded() {
			backend.release_egl_lifetime()
			return error(err_wayland_egl_context_failed)
		}
		backend.egl_config = native_pointer(actual_config)
		if backend.render_health.blocks_graphics() {
			backend.release_egl_lifetime()
			return error(err_render_native_renderer_unavailable)
		}
		backend.egl_binding = EglBindingIdentity{}
		backend.egl_bad_current_recovery_used = false
		backend.render_health = .ready
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) shutdown_renderer() {
	backend.release_egl_lifetime()
	if !backend.render_health.blocks_graphics() {
		backend.render_health = .abandoned
	}
}

$if linux && sokol_wayland ? {
	fn (mut backend WaylandBackend) accept_egl_result(context NativeOperationContext, mut ordinals NativeOrdinalRange, seed NativeOperationSeed, raw C.VMultiwindowNativePrimitive, validation NativeLocalValidation) NativeRenderResult {
		capture := backend.native_operations.capture_egl_call(context, mut ordinals, seed, raw) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return backend.record_egl_result(native_render_outcome(.egl, context.operation,
				.renderer, .renderer_unavailable, 0, 0, err_render_native_renderer_unavailable))
		}
		mut result := backend.native_operations.accept_egl(context, capture, validation)
		result = backend.record_egl_result(result)
		return result
	}

	fn (mut backend WaylandBackend) accept_egl_context_requirements(context NativeOperationContext, mut ordinals NativeOrdinalRange, seed NativeOperationSeed, raw C.VMultiwindowNativePrimitive, initialize NativeRenderResult) NativeRenderResult {
		capture := backend.native_operations.capture_egl_call(context, mut ordinals, seed, raw) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return backend.record_egl_result(native_render_outcome(.egl, context.operation,
				.renderer, .renderer_unavailable, 0, 0, err_render_native_renderer_unavailable))
		}
		result := backend.native_operations.accept_egl_context_requirements(context, capture,
			initialize)
		return backend.record_egl_result(result)
	}
}

fn (mut backend WaylandBackend) accept_egl_query(context NativeOperationContext, raw C.VMultiwindowNativePrimitive, validation NativeLocalValidation) NativeRenderResult {
	capture := backend.native_operations.capture_call(context, raw)
	result := backend.native_operations.accept_egl(context, capture, validation)
	return backend.record_egl_result(result)
}

fn (mut backend WaylandBackend) accept_egl_binding_query(context NativeOperationContext, raw C.VMultiwindowNativePrimitive, draw NativeRenderResult, read NativeRenderResult, expected EglBindingIdentity) NativeRenderResult {
	capture := backend.native_operations.capture_call(context, raw)
	result := backend.native_operations.accept_egl_binding_context(context, capture, draw, read,
		native_identity(expected.surface), native_identity(expected.surface),
		native_identity(backend.egl_context))
	return backend.record_egl_result(result)
}

$if linux && sokol_wayland ? {
	fn (mut backend WaylandBackend) release_egl_surface_ticket(ticket_id u64, surface voidptr) NativeLifetimeReleaseAttempt {
		if ticket_id == 0 || surface == unsafe { nil }
			|| backend.native_operations == unsafe { nil } {
			return NativeLifetimeReleaseAttempt{}
		}
		return backend.native_operations.release_linux_egl_lifetime_ticket(ticket_id, .egl_surface,
			native_identity(surface), native_identity(backend.egl_display), backend.render_health)
	}
}

fn (mut backend WaylandBackend) record_egl_result(result NativeRenderResult) NativeRenderResult {
	backend.render_health = renderer_health_after_result(backend.render_health, result)
	backend.native_operations.record_health_latch(result.context, backend.render_health)
	if result.domain == .egl && result.native_code == i64(0x3008) {
		backend.native_operations.abandon_egl_display_lifetime(native_identity(backend.egl_display))
		backend.egl_binding = EglBindingIdentity{}
	}
	return result
}

fn (mut backend WaylandBackend) record_wayland_result(result NativeRenderResult) NativeRenderResult {
	backend.render_health = renderer_health_after_result(backend.render_health, result)
	backend.native_operations.record_health_latch(result.context, backend.render_health)
	if result.domain == .wayland && result.display_error != 0 {
		backend.wayland_display_unavailable = true
		backend.wayland_display_error = result.display_error
	}
	return result
}

$if linux && sokol_wayland ? {
	fn (mut backend WaylandBackend) accept_wayland_result(context NativeOperationContext, mut ordinals NativeOrdinalRange, display &C.wl_display, raw C.VMultiwindowNativePrimitive, validation NativeLocalValidation, error_text string) NativeRenderResult {
		primary := backend.native_operations.capture_call(context, raw)
		evidence_seed := NativeOperationSeed{
			call_site:       .display_transport
			scope:           .renderer
			presence_mask:   native_context_has_target_identity
			target_identity: native_identity(display)
		}
		evidence_context := ordinals.materialize(backend.native_operations, .wayland,
			.wayland_display_error_query, evidence_seed) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return backend.record_wayland_result(native_wayland_logical_result(context.operation,
				.renderer, .renderer_unavailable, 0, err_render_native_renderer_unavailable))
		}
		mut evidence_raw := C.VMultiwindowNativePrimitive{}
		C.v_multiwindow_wayland_display_error(display, &evidence_raw)
		evidence := backend.native_operations.capture_evidence(evidence_context, evidence_raw)
		capture := native_capture_with_wayland_display_error(primary, evidence)
		mut result := backend.native_operations.accept_wayland(context, capture, validation,
			error_text)
		result = backend.record_wayland_result(result)
		if context.operation == .display_cancel {
			backend.native_operations.record_release(context, capture, result)
		}
		return result
	}

	fn (mut backend WaylandBackend) accept_wayland_cancel_result(context NativeOperationContext, mut ordinals NativeOrdinalRange, display &C.wl_display, raw C.VMultiwindowNativePrimitive, error_text string) NativeRenderResult {
		if !backend.render_health.blocks_graphics() && !backend.wayland_display_unavailable {
			return backend.accept_wayland_result(context, mut ordinals, display, raw,
				.void_completion, error_text)
		}
		capture := backend.native_operations.capture_call(context, raw)
		ordinals.skip(1) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return backend.blocked_renderer_result(.display_cancel)
		}
		mut result := backend.native_operations.accept_wayland(context, capture, .void_completion,
			error_text)
		result = backend.record_wayland_result(result)
		backend.native_operations.record_release(context, capture, result)
		return result
	}

	fn (mut backend WaylandBackend) reserve_wayland_lifetime_ticket(mut cleanup NativeOrdinalRange, release_kind NativeLifetimeReleaseKind, seed NativeOperationSeed) !u64 {
		if release_kind !in [.wayland_egl_window, .wayland_surface, .wayland_frame_callback] {
			return error(err_render_native_renderer_unavailable)
		}
		context := cleanup.materialize(backend.native_operations, .wayland,
			native_lifetime_release_operation(release_kind), seed.without_target_identity())!
		return backend.native_operations.reserve_lifetime_ticket(context, release_kind,
			seed.without_target_identity())
	}

	fn (mut backend WaylandBackend) destroy_wl_egl_window_lifetime(mut record &WaylandWindowRecord) NativeRenderResult {
		if record.wl_egl_window == unsafe { nil } {
			return native_render_ok(.wayland, .surface_destroy, .window_target)
		}
		if !backend.native_operations.owner_thread_is_current() {
			return backend.blocked_renderer_result(.surface_destroy)
		}
		wl_egl_window := record.wl_egl_window
		claim := backend.native_operations.claim_lifetime_release(record.wl_egl_window_ticket,
			.wayland_egl_window, native_identity(wl_egl_window), native_identity(record.surface)) or {
			if backend.native_operations.acknowledge_abandoned_lifetime_ticket(record.wl_egl_window_ticket,
				.wayland_egl_window, native_identity(wl_egl_window),
				native_identity(record.surface))
			{
				mut abandoned_raw := C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_wayland_egl_destroy_window(wl_egl_window, &abandoned_raw)
				record.wl_egl_window = unsafe { nil }
				record.wl_egl_window_ticket = 0
			}
			return backend.blocked_renderer_result(.surface_destroy)
		}
		mut raw := C.VMultiwindowNativePrimitive{}
		C.v_multiwindow_wayland_egl_destroy_window(wl_egl_window, &raw)
		result := backend.native_operations.complete_lifetime_release(claim, raw, .void_completion,
			backend.render_health, err_wayland_egl_surface_failed)
		record.wl_egl_window = unsafe { nil }
		record.wl_egl_window_ticket = 0
		return result
	}

	fn (mut backend WaylandBackend) destroy_anchor_wl_egl_window_lifetime() NativeRenderResult {
		if backend.anchor_wl_egl_window == unsafe { nil } {
			return native_render_ok(.wayland, .surface_destroy, .anchor)
		}
		if !backend.native_operations.owner_thread_is_current() {
			return backend.blocked_renderer_result(.surface_destroy)
		}
		wl_egl_window := backend.anchor_wl_egl_window
		parent := backend.anchor_wl_surface
		claim := backend.native_operations.claim_lifetime_release(backend.anchor_wl_egl_window_ticket,
			.wayland_egl_window, native_identity(wl_egl_window), native_identity(parent)) or {
			if backend.native_operations.acknowledge_abandoned_lifetime_ticket(backend.anchor_wl_egl_window_ticket,
				.wayland_egl_window, native_identity(wl_egl_window), native_identity(parent))
			{
				mut abandoned_raw := C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_wayland_egl_destroy_window(wl_egl_window, &abandoned_raw)
				backend.anchor_wl_egl_window = unsafe { nil }
				backend.anchor_wl_egl_window_ticket = 0
			}
			return backend.blocked_renderer_result(.surface_destroy)
		}
		mut raw := C.VMultiwindowNativePrimitive{}
		C.v_multiwindow_wayland_egl_destroy_window(wl_egl_window, &raw)
		result := backend.native_operations.complete_lifetime_release(claim, raw, .void_completion,
			backend.render_health, err_wayland_egl_surface_failed)
		backend.anchor_wl_egl_window = unsafe { nil }
		backend.anchor_wl_egl_window_ticket = 0
		return result
	}

	fn (mut backend WaylandBackend) destroy_anchor_wl_surface_lifetime() NativeRenderResult {
		if backend.anchor_wl_surface == unsafe { nil } {
			return native_render_ok(.wayland, .surface_destroy, .anchor)
		}
		if !backend.native_operations.owner_thread_is_current() {
			return backend.blocked_renderer_result(.surface_destroy)
		}
		surface := backend.anchor_wl_surface
		parent := backend.compositor
		claim := backend.native_operations.claim_lifetime_release(backend.anchor_wl_surface_ticket,
			.wayland_surface, native_identity(surface), native_identity(parent)) or {
			if backend.native_operations.acknowledge_abandoned_lifetime_ticket(backend.anchor_wl_surface_ticket,
				.wayland_surface, native_identity(surface), native_identity(parent))
			{
				mut abandoned_raw := C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_wayland_destroy_anchor_surface(surface, if backend.transport_can_marshal() {
					1
				} else {
					0
				}, &abandoned_raw)
				backend.anchor_wl_surface = unsafe { nil }
				backend.anchor_wl_surface_ticket = 0
			}
			return backend.blocked_renderer_result(.surface_destroy)
		}
		mut raw := C.VMultiwindowNativePrimitive{}
		C.v_multiwindow_wayland_destroy_anchor_surface(surface, if backend.transport_can_marshal() {
			1
		} else {
			0
		}, &raw)
		result := backend.native_operations.complete_lifetime_release(claim, raw, .void_completion,
			backend.render_health, err_wayland_create_surface_failed)
		backend.anchor_wl_surface = unsafe { nil }
		backend.anchor_wl_surface_ticket = 0
		return result
	}

	fn (mut backend WaylandBackend) release_renderer_anchor_lifetime() bool {
		if backend.anchor_surface != unsafe { nil } {
			release := backend.release_egl_surface_ticket(backend.anchor_surface_ticket,
				backend.anchor_surface)
			if !release.terminal {
				return false
			}
			backend.anchor_surface = unsafe { nil }
			backend.anchor_surface_ticket = 0
		} else if backend.anchor_surface_ticket != 0 {
			if !backend.native_operations.burn_lifetime_ticket(backend.anchor_surface_ticket) {
				return false
			}
			backend.anchor_surface_ticket = 0
		}
		if backend.anchor_wl_egl_window != unsafe { nil } {
			_ = backend.destroy_anchor_wl_egl_window_lifetime()
			if backend.anchor_wl_egl_window != unsafe { nil } {
				return false
			}
		} else if backend.anchor_wl_egl_window_ticket != 0 {
			if !backend.native_operations.burn_lifetime_ticket(backend.anchor_wl_egl_window_ticket) {
				return false
			}
			backend.anchor_wl_egl_window_ticket = 0
		}
		if backend.anchor_wl_surface != unsafe { nil } {
			_ = backend.destroy_anchor_wl_surface_lifetime()
			if backend.anchor_wl_surface != unsafe { nil } {
				return false
			}
		} else if backend.anchor_wl_surface_ticket != 0 {
			if !backend.native_operations.burn_lifetime_ticket(backend.anchor_wl_surface_ticket) {
				return false
			}
			backend.anchor_wl_surface_ticket = 0
		}
		return true
	}

	fn (mut backend WaylandBackend) destroy_frame_callback_lifetime(mut record &WaylandWindowRecord) NativeRenderResult {
		if record.frame_callback == unsafe { nil } {
			return native_render_ok(.wayland, .surface_destroy, .window_target)
		}
		if !backend.native_operations.owner_thread_is_current() {
			return backend.blocked_renderer_result(.surface_destroy)
		}
		callback := record.frame_callback
		claim := backend.native_operations.claim_lifetime_release(record.frame_callback_ticket,
			.wayland_frame_callback, native_identity(callback), native_identity(record.surface)) or {
			if backend.native_operations.acknowledge_abandoned_lifetime_ticket(record.frame_callback_ticket,
				.wayland_frame_callback, native_identity(callback), native_identity(record.surface))
			{
				backend.destroy_proxy_locally(callback)
				record.frame_callback = unsafe { nil }
				record.frame_callback_ticket = 0
			}
			return backend.blocked_renderer_result(.surface_destroy)
		}
		mut raw := C.VMultiwindowNativePrimitive{}
		if backend.transport_can_marshal() {
			C.v_multiwindow_wayland_destroy_frame_callback(callback, &raw)
		} else {
			backend.destroy_proxy_locally(callback)
		}
		result := backend.native_operations.complete_lifetime_release(claim, raw, .void_completion,
			backend.render_health, err_wayland_egl_swap_buffers_failed)
		record.frame_callback = unsafe { nil }
		record.frame_callback_ticket = 0
		return result
	}
}

fn (mut backend WaylandBackend) blocked_renderer_result(operation NativeRenderOperation) NativeRenderResult {
	disposition := if backend.render_health == .lost {
		NativeRenderDisposition.renderer_lost
	} else {
		NativeRenderDisposition.renderer_unavailable
	}
	if disposition == .renderer_unavailable && backend.wayland_display_unavailable {
		base := native_render_outcome(.wayland, operation, .renderer, disposition,
			backend.wayland_display_error, 0, err_render_native_renderer_unavailable)
		result := NativeRenderResult{
			...base
			display_error: backend.wayland_display_error
		}
		return backend.record_wayland_result(result)
	}
	error_text := if disposition == .renderer_lost {
		err_render_native_renderer_lost
	} else {
		err_render_native_renderer_unavailable
	}
	return backend.record_egl_result(native_render_outcome(.egl, operation, .renderer, disposition,
		0, 0, error_text))
}

fn wayland_window_operation_seed(id WindowId, target_generation u64, call_site NativeRenderCallSite) NativeOperationSeed {
	return NativeOperationSeed{
		presence_mask:     native_context_has_window | native_context_has_target_generation
		call_site:         call_site
		scope:             .window_target
		window:            id
		target_generation: target_generation
	}
}

fn (mut backend WaylandBackend) materialize_prepared_wayland_transport(boundary_seed NativeOperationSeed, operation NativeRenderOperation, display voidptr, mut ordinals NativeOrdinalRange) !WaylandPreparedTransportAttempt {
	seed := NativeOperationSeed{
		...boundary_seed
		presence_mask:   boundary_seed.presence_mask | native_context_has_target_identity
		target_identity: native_identity(display)
	}
	context := ordinals.materialize(backend.native_operations, .wayland, operation, seed) or {
		backend.render_health = renderer_health_latch_unavailable(backend.render_health)
		return error(err_render_native_renderer_unavailable)
	}
	evidence_seed := NativeOperationSeed{
		call_site:       .display_transport
		scope:           .renderer
		presence_mask:   native_context_has_target_identity
		target_identity: native_identity(display)
	}
	evidence_context := ordinals.materialize(backend.native_operations, .wayland,
		.wayland_display_error_query, evidence_seed) or {
		backend.render_health = renderer_health_latch_unavailable(backend.render_health)
		return error(err_render_native_renderer_unavailable)
	}
	return WaylandPreparedTransportAttempt{
		display:          display
		context:          context
		evidence_context: evidence_context
	}
}

fn (mut backend WaylandBackend) prepare_transport_plan(boundary_seed NativeOperationSeed, operations []NativeRenderOperation, cleanup bool) !WaylandServiceTransportPlan {
	$if linux && sokol_wayland ? {
		usable := if cleanup {
			backend.cleanup_transport_usable(operations.len)
		} else {
			backend.service_transport_usable(operations.len)
		}
		if !usable {
			return error(err_render_native_renderer_unavailable)
		}
		display := unsafe { &C.wl_display(backend.display) }
		mut ordinals := backend.native_operations.reserve_ordinals(u64(operations.len) * u64(2)) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_render_native_renderer_unavailable)
		}
		mut attempts := []WaylandPreparedTransportAttempt{cap: operations.len}
		for operation in operations {
			attempts << backend.materialize_prepared_wayland_transport(boundary_seed, operation,
				display, mut ordinals)!
		}
		return WaylandServiceTransportPlan{
			attempts: attempts
		}
	} $else {
		_ = boundary_seed
		_ = operations
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) prepare_service_transport_plan(boundary_seed NativeOperationSeed, operations []NativeRenderOperation) !WaylandServiceTransportPlan {
	return backend.prepare_transport_plan(boundary_seed, operations, false)
}

fn (mut backend WaylandBackend) prepare_create_transport_plan(id WindowId, prepare_lifecycle_buffer bool) !WaylandServiceTransportPlan {
	$if linux && sokol_wayland ? {
		operation_count := if prepare_lifecycle_buffer { 4 } else { 3 }
		if !backend.service_transport_usable(operation_count) {
			return error(err_render_native_renderer_unavailable)
		}
		display := unsafe { &C.wl_display(backend.display) }
		mut ordinals := backend.native_operations.reserve_ordinals(u64(operation_count) * u64(2)) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_render_native_renderer_unavailable)
		}
		prepare_seed := wayland_window_operation_seed(id, 1, .window_prepare)
		mut attempts := []WaylandPreparedTransportAttempt{cap: operation_count}
		for operation in [NativeRenderOperation.display_flush, .display_roundtrip, .display_flush] {
			attempts << backend.materialize_prepared_wayland_transport(prepare_seed, operation,
				display, mut ordinals)!
		}
		if prepare_lifecycle_buffer {
			lifecycle_seed := wayland_window_operation_seed(id, 1, .display_transport)
			attempts << backend.materialize_prepared_wayland_transport(lifecycle_seed,
				.display_flush, display, mut ordinals)!
		}
		return WaylandServiceTransportPlan{
			attempts: attempts
		}
	} $else {
		_ = id
		_ = prepare_lifecycle_buffer
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) prepare_cleanup_transport_plan(boundary_seed NativeOperationSeed, operations []NativeRenderOperation) !WaylandServiceTransportPlan {
	return backend.prepare_transport_plan(boundary_seed, operations, true)
}

fn (mut backend WaylandBackend) accept_prepared_wayland_transport(attempt WaylandPreparedTransportAttempt, raw C.VMultiwindowNativePrimitive, validation NativeLocalValidation, error_text string) NativeRenderResult {
	$if linux && sokol_wayland ? {
		display := unsafe { &C.wl_display(attempt.display) }
		primary := backend.native_operations.capture_call(attempt.context, raw)
		mut evidence_raw := C.VMultiwindowNativePrimitive{}
		C.v_multiwindow_wayland_display_error(display, &evidence_raw)
		evidence := backend.native_operations.capture_evidence(attempt.evidence_context,
			evidence_raw)
		capture := native_capture_with_wayland_display_error(primary, evidence)
		mut result := backend.native_operations.accept_wayland(attempt.context, capture,
			validation, error_text)
		result = backend.record_wayland_result(result)
		return result
	} $else {
		_ = attempt
		_ = raw
		_ = validation
		return native_wayland_logical_result(.display_flush, .renderer, .renderer_unavailable, 0,
			error_text)
	}
}

fn (mut backend WaylandBackend) execute_prepared_wayland_transport(attempt WaylandPreparedTransportAttempt) NativeRenderResult {
	$if linux && sokol_wayland ? {
		display := unsafe { &C.wl_display(attempt.display) }
		mut raw := C.VMultiwindowNativePrimitive{}
		match attempt.context.operation {
			.display_flush {
				C.v_multiwindow_wayland_flush(display, &raw)
				return backend.accept_prepared_wayland_transport(attempt, raw, .none,
					err_wayland_flush_failed)
			}
			.display_roundtrip {
				C.v_multiwindow_wayland_roundtrip(display, &raw)
				return backend.accept_prepared_wayland_transport(attempt, raw, .none,
					err_wayland_dispatch_failed)
			}
			else {
				return native_wayland_logical_result(attempt.context.operation, .renderer,
					.operation_failed, 0, err_render_native_batch_failed)
			}
		}
	} $else {
		_ = attempt
		return native_wayland_logical_result(.display_flush, .renderer, .renderer_unavailable, 0,
			err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) attempt_wayland_flush(boundary_seed NativeOperationSeed) NativeRenderResult {
	$if linux && sokol_wayland ? {
		if backend.wayland_display_unavailable || backend.render_health.blocks_graphics() {
			return backend.record_wayland_result(native_wayland_logical_result(.display_flush,
				.renderer, .renderer_unavailable, backend.wayland_display_error,
				err_wayland_flush_failed))
		}
		if backend.display == unsafe { nil } {
			return backend.record_wayland_result(native_wayland_logical_result(.display_flush,
				.renderer, .renderer_unavailable, 0, err_wayland_connect_failed))
		}
		display := unsafe { &C.wl_display(backend.display) }
		seed := NativeOperationSeed{
			...boundary_seed
			presence_mask:   boundary_seed.presence_mask | native_context_has_target_identity
			target_identity: native_identity(display)
		}
		mut ordinals := backend.native_operations.reserve_ordinals(2) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return backend.blocked_renderer_result(.display_flush)
		}
		context := ordinals.materialize(backend.native_operations, .wayland, .display_flush, seed) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return backend.blocked_renderer_result(.display_flush)
		}
		mut raw := C.VMultiwindowNativePrimitive{}
		C.v_multiwindow_wayland_flush(display, &raw)
		return backend.accept_wayland_result(context, mut ordinals, display, raw, .none,
			err_wayland_flush_failed)
	} $else {
		return native_wayland_logical_result(.display_flush, .renderer, .renderer_unavailable, 0,
			err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) attempt_wayland_roundtrip(boundary_seed NativeOperationSeed) NativeRenderResult {
	$if linux && sokol_wayland ? {
		if backend.wayland_display_unavailable || backend.render_health.blocks_graphics() {
			return backend.record_wayland_result(native_wayland_logical_result(.display_roundtrip,
				.renderer, .renderer_unavailable, backend.wayland_display_error,
				err_wayland_dispatch_failed))
		}
		if backend.display == unsafe { nil } {
			return backend.record_wayland_result(native_wayland_logical_result(.display_roundtrip,
				.renderer, .renderer_unavailable, 0, err_wayland_connect_failed))
		}
		display := unsafe { &C.wl_display(backend.display) }
		seed := NativeOperationSeed{
			...boundary_seed
			presence_mask:   boundary_seed.presence_mask | native_context_has_target_identity
			target_identity: native_identity(display)
		}
		mut ordinals := backend.native_operations.reserve_ordinals(2) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return backend.blocked_renderer_result(.display_roundtrip)
		}
		context := ordinals.materialize(backend.native_operations, .wayland, .display_roundtrip, seed) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return backend.blocked_renderer_result(.display_roundtrip)
		}
		mut raw := C.VMultiwindowNativePrimitive{}
		C.v_multiwindow_wayland_roundtrip(display, &raw)
		return backend.accept_wayland_result(context, mut ordinals, display, raw, .none,
			err_wayland_dispatch_failed)
	} $else {
		return native_wayland_logical_result(.display_roundtrip, .renderer, .renderer_unavailable,
			0, err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) execute_prepared_wayland_hide_barrier(attempt WaylandPreparedTransportAttempt) ! {
	$if test {
		if backend.toplevel_replay_test.hide_barrier_override {
			if !backend.toplevel_replay_test.hide_barrier_succeeds {
				return error(err_wayland_dispatch_failed)
			}
			return
		}
	}
	_ = backend.execute_prepared_service_transport(attempt)!
}

fn (mut backend WaylandBackend) abandon_renderer_ownership() {
	backend.release_egl_lifetime()
	backend.render_health = .abandoned
}

fn (mut backend WaylandBackend) release_egl_lifetime() {
	if backend.native_operations == unsafe { nil } {
		return
	}
	$if linux && sokol_wayland ? {
		if backend.native_operations != unsafe { nil } {
			mut children_terminal := true
			for i in 0 .. backend.windows.len {
				mut record := backend.windows[i]
				surface := record.egl_surface
				if surface != unsafe { nil } {
					release := backend.release_egl_surface_ticket(record.egl_surface_ticket,
						surface)
					if release.terminal {
						record.egl_surface = unsafe { nil }
						record.egl_surface_ticket = 0
					} else {
						children_terminal = false
					}
				}
			}
			if !backend.release_renderer_anchor_lifetime() {
				children_terminal = false
			}
			if !children_terminal {
				return
			}
			display_identity := native_identity(backend.egl_display)
			if backend.egl_context != unsafe { nil } {
				release := backend.native_operations.release_linux_egl_lifetime_ticket(backend.egl_context_ticket,
					.egl_context, native_identity(backend.egl_context), display_identity,
					backend.render_health)
				if release.terminal {
					backend.egl_context = unsafe { nil }
					backend.egl_context_ticket = 0
				} else {
					return
				}
			} else if backend.native_operations.burn_lifetime_ticket(backend.egl_context_ticket) {
				backend.egl_context_ticket = 0
			}
			if backend.egl_display != unsafe { nil } {
				if backend.egl_display_ticket == 0 {
					backend.egl_display = unsafe { nil }
				} else if backend.native_operations.burn_lifetime_ticket(backend.egl_display_ticket) {
					backend.egl_display = unsafe { nil }
					backend.egl_display_ticket = 0
				} else {
					release := backend.native_operations.release_linux_egl_lifetime_ticket(backend.egl_display_ticket,
						.egl_display, display_identity, display_identity, backend.render_health)
					if release.terminal {
						backend.egl_display = unsafe { nil }
						backend.egl_display_ticket = 0
					} else {
						return
					}
				}
			} else if backend.native_operations.burn_lifetime_ticket(backend.egl_display_ticket) {
				backend.egl_display_ticket = 0
			}
			thread_release := backend.native_operations.release_linux_egl_thread_lifetime_ticket(backend.egl_thread_ticket,
				backend.render_health)
			if thread_release.terminal {
				backend.egl_thread_ticket = 0
			} else {
				return
			}
		}
	}
	for i in 0 .. backend.windows.len {
		mut record := backend.windows[i]
		record.frame_ready = true
		record.pending_egl_resize = false
		record.render_target_generation =
			exhaust_backend_target_generation(record.render_target_generation)
	}
	if backend.egl_display_ticket == 0 && backend.egl_context_ticket == 0
		&& backend.egl_thread_ticket == 0 && backend.anchor_surface_ticket == 0
		&& backend.anchor_wl_egl_window_ticket == 0 && backend.anchor_wl_surface_ticket == 0 {
		backend.egl_config = unsafe { nil }
	}
	backend.egl_binding = EglBindingIdentity{}
}

fn (mut backend WaylandBackend) accept_native_render_window_loss(id WindowId) {
	index := backend.window_record_index(id) or { return }
	$if gg_multiwindow ? || x_multiwindow_render ? {
		record := backend.windows[index]
		backend.invalidate_window_egl_target(index, record.egl_surface,
			record.render_target_generation, true)
	} $else {
		backend.windows[index].native_destroyed = true
	}
}

fn (mut backend WaylandBackend) create_window(id WindowId, config WindowConfig) !WindowSize {
	$if linux && sokol_wayland ? {
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		if backend.render_health.blocks_graphics() || backend.wayland_display_unavailable {
			return error(err_render_native_renderer_unavailable)
		}
		if backend.compositor == unsafe { nil } || backend.compositor_name == 0
			|| backend.wm_base == unsafe { nil } || backend.wm_base_name == 0 {
			return error(err_wayland_required_globals_missing)
		}
		prepare_lifecycle_buffer := config.visible && !backend.renderer_ready()
		mut transport := backend.prepare_create_transport_plan(id, prepare_lifecycle_buffer)!
		flush_attempt := transport.take()
		roundtrip_attempt := transport.take()
		ack_flush_attempt := transport.take()
		mut lifecycle_flush_attempt := WaylandPreparedTransportAttempt{}
		if prepare_lifecycle_buffer {
			lifecycle_flush_attempt = transport.take()
		}
		$if test {
			if backend.toplevel_replay_test.create_request_override {
				backend.toplevel_replay_test.create_requests++
				return error(err_wayland_create_surface_failed)
			}
		}
		compositor := unsafe { &C.wl_compositor(backend.compositor) }
		wm_base := unsafe { &C.xdg_wm_base(backend.wm_base) }
		surface := C.wl_compositor_create_surface(compositor)
		if surface == unsafe { nil } {
			backend.consume_prepared_service_cleanup(flush_attempt)
			return error(err_wayland_create_surface_failed)
		}
		xdg_surface := C.v_multiwindow_wayland_xdg_wm_base_get_xdg_surface(wm_base, surface)
		if xdg_surface == unsafe { nil } {
			C.wl_surface_destroy(surface)
			backend.consume_prepared_service_cleanup(flush_attempt)
			return error(err_wayland_create_surface_failed)
		}
		xdg_toplevel := C.v_multiwindow_wayland_xdg_surface_get_toplevel(xdg_surface)
		if xdg_toplevel == unsafe { nil } {
			C.v_multiwindow_wayland_xdg_surface_destroy(xdg_surface)
			C.wl_surface_destroy(surface)
			backend.consume_prepared_service_cleanup(flush_attempt)
			return error(err_wayland_create_surface_failed)
		}
		actual_size := window_size_for_config(config, config.width, config.height)
		record_min_width := if config.resizable { config.min_width } else { actual_size.width }
		record_min_height := if config.resizable { config.min_height } else { actual_size.height }
		mut record := &WaylandWindowRecord{
			id:                        id
			high_dpi:                  config.high_dpi
			title:                     config.title.clone()
			app_id:                    backend.native_app_id().clone()
			owner_id:                  config.owner
			max_width:                 if config.resizable { 0 } else { actual_size.width }
			max_height:                if config.resizable { 0 } else { actual_size.height }
			request_server_decoration: !config.borderless
			requested_fullscreen:      config.fullscreen
			surface:                   unsafe { voidptr(surface) }
			xdg_surface:               unsafe { voidptr(xdg_surface) }
			xdg_toplevel:              unsafe { voidptr(xdg_toplevel) }
			resizable:                 config.resizable
			native_operations:         backend.native_operations
			owner:                     backend
			width:                     actual_size.width
			height:                    actual_size.height
			min_width:                 record_min_width
			min_height:                record_min_height
			requested_visible:         config.visible
			publish_show_on_map:       config.visible
		}
		if C.v_multiwindow_wayland_add_surface_listener(surface, record.listener_data()) < 0 {
			_ = backend.destroy_window_record(mut record)
			backend.consume_prepared_service_cleanup(flush_attempt)
			return error(err_wayland_create_surface_failed)
		}
		if C.v_multiwindow_wayland_add_xdg_surface_listener(xdg_surface, record.listener_data()) < 0 {
			_ = backend.destroy_window_record(mut record)
			backend.consume_prepared_service_cleanup(flush_attempt)
			return error(err_wayland_create_surface_failed)
		}
		if C.v_multiwindow_wayland_add_xdg_toplevel_listener(xdg_toplevel, record.listener_data()) < 0 {
			_ = backend.destroy_window_record(mut record)
			backend.consume_prepared_service_cleanup(flush_attempt)
			return error(err_wayland_create_surface_failed)
		}
		C.v_multiwindow_wayland_xdg_toplevel_set_title(xdg_toplevel, &char(record.title.str))
		C.v_multiwindow_wayland_xdg_toplevel_set_app_id(xdg_toplevel, &char(record.app_id.str))
		if owner := config.owner {
			owner_index := backend.window_record_index(owner) or {
				_ = backend.destroy_window_record(mut record)
				backend.consume_prepared_service_cleanup(flush_attempt)
				return error(err_window_not_found)
			}
			owner_toplevel := unsafe { &C.xdg_toplevel(backend.windows[owner_index].xdg_toplevel) }
			if owner_toplevel == unsafe { nil } {
				_ = backend.destroy_window_record(mut record)
				backend.consume_prepared_service_cleanup(flush_attempt)
				return error(err_window_not_found)
			}
			C.v_multiwindow_wayland_xdg_toplevel_set_parent(xdg_toplevel, owner_toplevel)
		}
		if !config.borderless && backend.decoration_manager != unsafe { nil } {
			decoration := C.v_multiwindow_wayland_xdg_decoration_manager_get_toplevel_decoration(unsafe {
				&C.zxdg_decoration_manager_v1(backend.decoration_manager)
			}, xdg_toplevel)
			if decoration != unsafe { nil } {
				if C.v_multiwindow_wayland_add_xdg_toplevel_decoration_listener(decoration,
					record.listener_data()) == 0 {
					C.v_multiwindow_wayland_xdg_toplevel_decoration_set_server_side(decoration)
					record.toplevel_decoration = unsafe { voidptr(decoration) }
				} else {
					C.v_multiwindow_wayland_xdg_toplevel_decoration_destroy(decoration)
				}
			}
		}
		if config.min_width > 0 || config.min_height > 0 {
			C.v_multiwindow_wayland_xdg_toplevel_set_min_size(xdg_toplevel, i32(config.min_width),
				i32(config.min_height))
		}
		if !config.resizable {
			C.v_multiwindow_wayland_xdg_toplevel_set_min_size(xdg_toplevel, i32(actual_size.width),
				i32(actual_size.height))
			C.v_multiwindow_wayland_xdg_toplevel_set_max_size(xdg_toplevel, i32(actual_size.width),
				i32(actual_size.height))
		}
		if config.fullscreen {
			C.v_multiwindow_wayland_xdg_toplevel_set_fullscreen(xdg_toplevel, unsafe { nil })
		}
		backend.windows << record
		index := backend.windows.len - 1
		_ = backend.attach_fractional_scale_to_record(mut record)
		C.wl_surface_commit(surface)
		backend.finish_prepared_service_request(flush_attempt) or {
			backend.destroy_window_slot(index)
			_ = backend.destroy_removed_wm_base_if_unused()
			return error(err_wayland_flush_failed)
		}
		backend.finish_prepared_service_request(roundtrip_attempt) or {
			backend.destroy_window_slot(index)
			_ = backend.destroy_removed_wm_base_if_unused()
			return error(err_wayland_dispatch_failed)
		}
		backend.finish_prepared_service_request(ack_flush_attempt) or {
			backend.destroy_window_slot(index)
			_ = backend.destroy_removed_wm_base_if_unused()
			return error(err_wayland_flush_failed)
		}
		if prepare_lifecycle_buffer {
			backend.ensure_lifecycle_buffer_with_prepared_transport(index, lifecycle_flush_attempt) or {
				backend.destroy_window_slot(index)
				_ = backend.destroy_removed_wm_base_if_unused()
				if err.msg() != err_wayland_flush_failed {
					backend.consume_prepared_service_cleanup(lifecycle_flush_attempt)
				}
				return err
			}
		} else if !config.visible {
			record.enqueue_service_state(.hide)
		}
		return WindowSize{
			width:  record.width
			height: record.height
		}
	} $else {
		_ = id
		_ = config
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) destroy_window(id WindowId) ! {
	backend.finish_window_teardown(id)!
}

fn (mut backend WaylandBackend) finish_window_teardown(id WindowId) ! {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		window_seed := wayland_window_operation_seed(backend.windows[index].id,
			backend.windows[index].render_target_generation, .shutdown_release)
		if !backend.destroy_window_slot(index) {
			return error(err_render_native_renderer_unavailable)
		}
		_ = backend.destroy_removed_wm_base_if_unused()
		if backend.started && backend.display != unsafe { nil } {
			flush := backend.attempt_wayland_flush(window_seed)
			if !flush.succeeded() {
				return error(err_wayland_flush_failed)
			}
		}
		return
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) set_window_title(id WindowId, title string) ! {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if backend.render_health.blocks_graphics() || backend.wayland_display_unavailable {
			return error(err_render_native_renderer_unavailable)
		}
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		record := backend.windows[index]
		if record.xdg_toplevel == unsafe { nil } {
			return error(err_window_not_found)
		}
		mut transport := backend.prepare_service_transport_plan(wayland_window_operation_seed(record.id,
			record.render_target_generation, .display_transport), [.display_flush])!
		flush_attempt := transport.take()
		C.v_multiwindow_wayland_xdg_toplevel_set_title(unsafe {
			&C.xdg_toplevel(record.xdg_toplevel)
		}, &char(title.str))
		backend.finish_prepared_service_request(flush_attempt)!
		backend.windows[index].title = title.clone()
		return
	} $else {
		_ = id
		_ = title
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) resize_window(id WindowId, width int, height int) !WindowSize {
	$if linux && sokol_wayland ? {
		_ = width
		_ = height
		_ = backend.window_record_index(id) or { return error(err_window_not_found) }
		return error(err_capability_unsupported)
	} $else {
		_ = id
		_ = width
		_ = height
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) set_window_cursor(id WindowId, shape CursorShape) ! {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if backend.render_health.blocks_graphics() || backend.wayland_display_unavailable {
			return error(err_render_native_renderer_unavailable)
		}
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		if !backend.can_set_cursor_shapes() || !backend.pointer_focused
			|| backend.pointer_focus != id || !backend.pointer_enter_serial_valid {
			return error(err_capability_unsupported)
		}
		cursor_shape := wayland_cursor_shape_for_shape(shape)
		if cursor_shape == 0 {
			return error(err_capability_unsupported)
		}
		mut transport := backend.prepare_service_transport_plan(wayland_window_operation_seed(backend.windows[index].id,
			backend.windows[index].render_target_generation, .display_transport), [
			.display_flush,
		])!
		flush_attempt := transport.take()
		C.v_multiwindow_wayland_cursor_shape_device_set_shape(unsafe {
			&C.wp_cursor_shape_device_v1(backend.cursor_shape_device)
		}, backend.pointer_enter_serial, cursor_shape)
		backend.finish_prepared_service_request(flush_attempt)!
		return
	} $else {
		_ = id
		_ = shape
		return error(err_backend_unsupported)
	}
}

fn wayland_cursor_shape_for_shape(shape CursorShape) u32 {
	return match shape {
		.default { wayland_cursor_shape_default }
		.pointer { wayland_cursor_shape_pointer }
		.move { wayland_cursor_shape_move }
		.n_resize { wayland_cursor_shape_n_resize }
		.s_resize { wayland_cursor_shape_s_resize }
		.e_resize { wayland_cursor_shape_e_resize }
		.w_resize { wayland_cursor_shape_w_resize }
		.ne_resize { wayland_cursor_shape_ne_resize }
		.nw_resize { wayland_cursor_shape_nw_resize }
		.se_resize { wayland_cursor_shape_se_resize }
		.sw_resize { wayland_cursor_shape_sw_resize }
		.ew_resize { wayland_cursor_shape_ew_resize }
		.ns_resize { wayland_cursor_shape_ns_resize }
		.nesw_resize { wayland_cursor_shape_nesw_resize }
		.nwse_resize { wayland_cursor_shape_nwse_resize }
		.grab { wayland_cursor_shape_grab }
		.grabbing { wayland_cursor_shape_grabbing }
		.text { wayland_cursor_shape_text }
		.crosshair { wayland_cursor_shape_crosshair }
		.not_allowed { wayland_cursor_shape_not_allowed }
		// wp_cursor_shape_device_v1 has no all-direction resize shape.
		.resize_all { u32(0) }
	}
}

fn (mut backend WaylandBackend) begin_window_move(id WindowId) ! {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if backend.render_health.blocks_graphics() || backend.wayland_display_unavailable {
			return error(err_render_native_renderer_unavailable)
		}
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		if backend.seat == unsafe { nil } {
			return error(err_capability_unsupported)
		}
		if backend.windows[index].xdg_toplevel == unsafe { nil } {
			return error(err_window_not_found)
		}
		if !backend.windows[index].user_action_serial_available(backend.poll_generation) {
			return error(err_capability_unsupported)
		}
		mut transport := backend.prepare_service_transport_plan(wayland_window_operation_seed(backend.windows[index].id,
			backend.windows[index].render_target_generation, .display_transport), [
			.display_flush,
		])!
		flush_attempt := transport.take()
		serial := backend.windows[index].take_user_action_serial(backend.poll_generation) or {
			return error(err_capability_unsupported)
		}
		if backend.interactive_request_overridden_for_test(false) {
			backend.toplevel_replay_test.interactive_move_requests++
		} else {
			C.v_multiwindow_wayland_xdg_toplevel_move(unsafe {
				&C.xdg_toplevel(backend.windows[index].xdg_toplevel)
			}, unsafe { &C.wl_seat(backend.seat) }, serial)
		}
		backend.finish_prepared_service_request(flush_attempt)!
		return
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) begin_window_resize(id WindowId, edge WindowResizeEdge) ! {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if backend.render_health.blocks_graphics() || backend.wayland_display_unavailable {
			return error(err_render_native_renderer_unavailable)
		}
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		if backend.seat == unsafe { nil } {
			return error(err_capability_unsupported)
		}
		if !backend.windows[index].resizable {
			return error(err_capability_unsupported)
		}
		if backend.windows[index].xdg_toplevel == unsafe { nil } {
			return error(err_window_not_found)
		}
		if !backend.windows[index].user_action_serial_available(backend.poll_generation) {
			return error(err_capability_unsupported)
		}
		mut transport := backend.prepare_service_transport_plan(wayland_window_operation_seed(backend.windows[index].id,
			backend.windows[index].render_target_generation, .display_transport), [
			.display_flush,
		])!
		flush_attempt := transport.take()
		serial := backend.windows[index].take_user_action_serial(backend.poll_generation) or {
			return error(err_capability_unsupported)
		}
		if backend.interactive_request_overridden_for_test(true) {
			backend.toplevel_replay_test.interactive_resize_requests++
		} else {
			C.v_multiwindow_wayland_xdg_toplevel_resize(unsafe {
				&C.xdg_toplevel(backend.windows[index].xdg_toplevel)
			}, unsafe { &C.wl_seat(backend.seat) }, serial, wayland_resize_edge(edge))
		}
		backend.finish_prepared_service_request(flush_attempt)!
		return
	} $else {
		_ = id
		_ = edge
		return error(err_backend_unsupported)
	}
}

fn wayland_dispatch_failure_message(dispatch NativeRenderResult) string {
	return if dispatch.error_text == '' {
		err_wayland_dispatch_failed
	} else {
		dispatch.error_text
	}
}

fn (mut backend WaylandBackend) fail_nonterminal_portal_exports(message string) {
	$if linux && sokol_wayland ? {
		mut index := 0
		for index < backend.portal_exports.len {
			portal := backend.portal_exports[index]
			if portal.terminal {
				index++
				continue
			}
			backend.pending_service_events << WaylandNativeQueuedEvent{
				sequence: C.v_multiwindow_wayland_next_event_sequence()
				event:    queued_service_event(ServiceEvent{
					kind:          .portal_parent
					window:        portal.window
					operation:     .portal_parent
					portal_parent: ServicePortalParentResult{
						id:     portal.request
						window: portal.window
						status: .failed
						lease:  portal.lease
						error:  message
					}
				})
			}
			backend.destroy_portal_export_at(index, !backend.transport_can_marshal())
		}
	} $else {
		_ = message
	}
}

fn (mut backend WaylandBackend) cleanup_after_fatal_dispatch(dispatch NativeRenderResult) string {
	message := wayland_dispatch_failure_message(dispatch)
	if dispatch.succeeded()
		|| (!backend.render_health.blocks_graphics() && backend.transport_can_marshal()) {
		return message
	}
	if backend.clipboard_read_active {
		backend.finish_clipboard_read(.failed, '', message)
	}
	backend.fail_nonterminal_portal_exports(message)
	backend.close_all_clipboard_sends()
	backend.clear_data_offer(true)
	return message
}

fn (mut backend WaylandBackend) poll_queued_events() ![]QueuedEvent {
	mut native_events := []WaylandNativeQueuedEvent{}
	$if linux && sokol_wayland ? {
		backend.poll_error = ''
		if !backend.started || backend.display == unsafe { nil } {
			return []QueuedEvent{}
		}
		// Move records queued by an earlier partial dispatch before attempting
		// any operation which can fail again.
		for native_event in backend.pending_service_events {
			native_events << native_event
		}
		backend.pending_service_events.clear()
		for _, mut record in backend.windows {
			for native_event in record.pending_events {
				native_events << native_event
			}
			record.pending_events.clear()
		}
		backend.poll_generation++
		backend.expire_user_action_serials()
		mut deferred_error := ''
		dispatch := backend.dispatch_pending_nonblocking()
		if !dispatch.succeeded() {
			deferred_error = backend.cleanup_after_fatal_dispatch(dispatch)
		}
		if deferred_error == '' && !backend.renderer_ready()
			&& !backend.render_health.blocks_graphics() && !backend.wayland_display_unavailable {
			backend.ensure_lifecycle_buffers() or { deferred_error = err.msg() }
		}
		if deferred_error == '' && !backend.render_health.blocks_graphics()
			&& !backend.wayland_display_unavailable {
			backend.drain_pending_data_offer_drop()
			backend.drain_clipboard_send()
			backend.drain_clipboard_read()
			backend.enqueue_due_key_repeats()
		}
		for native_event in backend.pending_service_events {
			native_events << native_event
		}
		backend.pending_service_events.clear()
		for _, mut record in backend.windows {
			for native_event in record.pending_events {
				native_events << native_event
			}
			record.pending_events.clear()
		}
		backend.poll_error = deferred_error
	}
	wayland_sort_native_events(mut native_events)
	mut events := []QueuedEvent{cap: native_events.len}
	for native_event in native_events {
		events << native_event.event
	}
	return events
}

fn (mut backend WaylandBackend) take_poll_error() string {
	terminal_error := backend.event_sequence_terminal_error()
	error_message := backend.poll_error
	backend.poll_error = ''
	return merge_backend_errors(error_message, terminal_error)
}

fn (mut backend WaylandBackend) event_sequence_terminal_error() string {
	$if linux && sokol_wayland ? {
		if C.v_multiwindow_wayland_event_sequence_exhausted() != 0
			&& backend.event_sequence_terminal == '' {
			backend.event_sequence_terminal = err_backend_event_sequence_exhausted
		}
	}
	return backend.event_sequence_terminal
}

fn (mut record WaylandWindowRecord) enqueue_native_event(sequence u64, event QueuedEvent) {
	if sequence == 0 {
		return
	}
	record.pending_events << WaylandNativeQueuedEvent{
		sequence: sequence
		event:    event
	}
}

fn (mut record WaylandWindowRecord) store_user_action_serial(serial u32, poll_generation u64) {
	record.user_action_serial = serial
	record.user_action_poll = poll_generation
	record.user_action_serial_valid = true
}

fn (mut record WaylandWindowRecord) clear_user_action_serial() {
	record.user_action_serial = 0
	record.user_action_poll = 0
	record.user_action_serial_valid = false
}

fn (mut record WaylandWindowRecord) take_user_action_serial(poll_generation u64) ?u32 {
	if !record.user_action_serial_valid || record.user_action_poll != poll_generation {
		record.clear_user_action_serial()
		return none
	}
	serial := record.user_action_serial
	record.clear_user_action_serial()
	return serial
}

fn (record &WaylandWindowRecord) user_action_serial_available(poll_generation u64) bool {
	return record.user_action_serial_valid && record.user_action_poll == poll_generation
}

fn (mut backend WaylandBackend) expire_user_action_serials() {
	for _, mut record in backend.windows {
		if record.user_action_serial_valid && record.user_action_poll != backend.poll_generation {
			record.clear_user_action_serial()
		}
	}
}

fn (mut record WaylandWindowRecord) enqueue_resize_events(sequence u64) {
	record.enqueue_native_event(sequence, queued_lifecycle_event(Event{
		kind:      .window_resized
		window_id: record.id
		width:     record.width
		height:    record.height
	}))
	record.enqueue_native_event(sequence, queued_input_event(record.input_event(.resized, 0)))
}

fn (record &WaylandWindowRecord) input_event(kind InputEventKind, modifiers u32) InputEvent {
	return record.input_event_with_payload(kind, modifiers, 0, false, wayland_invalid_mouse_button,
		0, 0)
}

fn (record &WaylandWindowRecord) input_event_with_payload(kind InputEventKind, modifiers u32, key_code int, key_repeat bool, mouse_button int, scroll_x f32, scroll_y f32) InputEvent {
	return InputEvent{
		kind:               kind
		window_id:          record.id
		key_code:           key_code
		key_repeat:         key_repeat
		modifiers:          modifiers
		mouse_x:            record.mouse_x
		mouse_y:            record.mouse_y
		mouse_dx:           record.mouse_dx
		mouse_dy:           record.mouse_dy
		mouse_button:       mouse_button
		scroll_x:           scroll_x
		scroll_y:           scroll_y
		window_width:       record.width
		window_height:      record.height
		framebuffer_width:  record.framebuffer_width()
		framebuffer_height: record.framebuffer_height()
	}
}

fn (record &WaylandWindowRecord) input_char_event(char_code u32, key_repeat bool, modifiers u32) InputEvent {
	return InputEvent{
		kind:               .char
		window_id:          record.id
		char_code:          char_code
		key_repeat:         key_repeat
		modifiers:          modifiers
		window_width:       record.width
		window_height:      record.height
		framebuffer_width:  record.framebuffer_width()
		framebuffer_height: record.framebuffer_height()
	}
}

fn (mut backend WaylandBackend) start_key_repeat(record &WaylandWindowRecord, raw_key u32, key_code int) {
	$if linux && sokol_wayland ? {
		if key_code == 0 {
			return
		}
		if !backend.key_repeats(raw_key) {
			return
		}
		if backend.keyboard_repeat_rate <= 0 || !backend.keyboard_focused
			|| backend.keyboard_focus != record.id {
			backend.stop_key_repeat()
			return
		}
		interval_ns := wayland_key_repeat_interval_ns(backend.keyboard_repeat_rate)
		if interval_ns == 0 {
			backend.stop_key_repeat()
			return
		}
		backend.keyboard_repeat_active = true
		backend.keyboard_repeat_raw_key = raw_key
		backend.keyboard_repeat_key_code = key_code
		backend.keyboard_repeat_window = record.id
		backend.keyboard_repeat_interval_ns = interval_ns
		backend.keyboard_repeat_next_ns = vtime.sys_mono_now() +
			u64(backend.keyboard_repeat_delay) * wayland_nsec_per_msec
	} $else {
		_ = record
		_ = raw_key
		_ = key_code
	}
}

fn (mut backend WaylandBackend) stop_key_repeat() {
	backend.keyboard_repeat_active = false
	backend.keyboard_repeat_raw_key = 0
	backend.keyboard_repeat_key_code = 0
	backend.keyboard_repeat_window = WindowId{}
	backend.keyboard_repeat_next_ns = 0
	backend.keyboard_repeat_interval_ns = 0
}

fn (mut backend WaylandBackend) stop_key_repeat_for_key(raw_key u32) {
	if backend.keyboard_repeat_active && backend.keyboard_repeat_raw_key == raw_key {
		backend.stop_key_repeat()
	}
}

fn (mut backend WaylandBackend) stop_key_repeat_for_window(id WindowId) {
	if backend.keyboard_repeat_active && backend.keyboard_repeat_window == id {
		backend.stop_key_repeat()
	}
}

fn (mut backend WaylandBackend) clear_key_repeat_info() {
	backend.stop_key_repeat()
	backend.keyboard_repeat_rate = 0
	backend.keyboard_repeat_delay = 0
}

fn (mut backend WaylandBackend) enqueue_due_key_repeats() {
	$if linux && sokol_wayland ? {
		if !backend.keyboard_repeat_active {
			return
		}
		if backend.keyboard_repeat_interval_ns == 0 || !backend.keyboard_focused
			|| backend.keyboard_focus != backend.keyboard_repeat_window {
			backend.stop_key_repeat()
			return
		}
		index := backend.window_record_index(backend.keyboard_repeat_window) or {
			backend.stop_key_repeat()
			return
		}
		now := vtime.sys_mono_now()
		if now < backend.keyboard_repeat_next_ns {
			return
		}
		modifiers := backend.event_modifiers()
		input := backend.windows[index].input_event_with_payload(.key_down, modifiers,
			backend.keyboard_repeat_key_code, true, wayland_invalid_mouse_button, 0, 0)
		backend.windows[index].enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(),
			queued_input_event(input))
		char_code := backend.char_code_for_key(backend.keyboard_repeat_raw_key)
		if char_code != 0 {
			char_input := backend.windows[index].input_char_event(char_code, true, modifiers)
			backend.windows[index].enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(),
				queued_input_event(char_input))
		}
		backend.keyboard_repeat_next_ns += backend.keyboard_repeat_interval_ns
		if backend.keyboard_repeat_next_ns <= now {
			skipped_intervals :=
				((now - backend.keyboard_repeat_next_ns) / backend.keyboard_repeat_interval_ns) + 1
			backend.keyboard_repeat_next_ns += skipped_intervals * backend.keyboard_repeat_interval_ns
		}
	}
}

fn wayland_key_repeat_interval_ns(rate int) u64 {
	if rate <= 0 {
		return 0
	}
	interval_ns := wayland_nsec_per_sec / u64(rate)
	if interval_ns == 0 {
		return 1
	}
	return interval_ns
}

fn (record &WaylandWindowRecord) input_files_dropped_event(files []string) InputEvent {
	return InputEvent{
		kind:               .files_dropped
		window_id:          record.id
		mouse_x:            record.mouse_x
		mouse_y:            record.mouse_y
		mouse_dx:           record.mouse_dx
		mouse_dy:           record.mouse_dy
		mouse_button:       wayland_invalid_mouse_button
		window_width:       record.width
		window_height:      record.height
		framebuffer_width:  record.framebuffer_width()
		framebuffer_height: record.framebuffer_height()
		dropped_files:      files.clone()
	}
}

fn (backend &WaylandBackend) touch_event_for_record(record &WaylandWindowRecord, kind InputEventKind, changed_slot int) InputEvent {
	mut touches := [8]InputTouchPoint{}
	mut count := 0
	for i, touch in backend.touches {
		if !touch.active || touch.window_id != record.id || count >= wayland_max_touch_points {
			continue
		}
		touches[count] = InputTouchPoint{
			identifier:       u64(touch.id)
			pos_x:            touch.x
			pos_y:            touch.y
			android_tooltype: 1
			changed:          i == changed_slot
		}
		count++
	}
	mouse_x, mouse_y := backend.touch_mouse_position_for_record(record, changed_slot)
	return InputEvent{
		kind:               kind
		window_id:          record.id
		modifiers:          backend.event_modifiers()
		mouse_x:            mouse_x
		mouse_y:            mouse_y
		num_touches:        count
		touches:            touches
		window_width:       record.width
		window_height:      record.height
		framebuffer_width:  record.framebuffer_width()
		framebuffer_height: record.framebuffer_height()
	}
}

fn (backend &WaylandBackend) touch_cancel_event_for_record(record &WaylandWindowRecord) InputEvent {
	mut touches := [8]InputTouchPoint{}
	mut count := 0
	for touch in backend.touches {
		if !touch.active || touch.window_id != record.id || count >= wayland_max_touch_points {
			continue
		}
		touches[count] = InputTouchPoint{
			identifier:       u64(touch.id)
			pos_x:            touch.x
			pos_y:            touch.y
			android_tooltype: 1
			changed:          true
		}
		count++
	}
	mouse_x, mouse_y := backend.touch_mouse_position_for_record(record, -1)
	return InputEvent{
		kind:               .touches_cancelled
		window_id:          record.id
		modifiers:          backend.event_modifiers()
		mouse_x:            mouse_x
		mouse_y:            mouse_y
		num_touches:        count
		touches:            touches
		window_width:       record.width
		window_height:      record.height
		framebuffer_width:  record.framebuffer_width()
		framebuffer_height: record.framebuffer_height()
	}
}

fn (backend &WaylandBackend) touch_mouse_position_for_record(record &WaylandWindowRecord, changed_slot int) (f32, f32) {
	if changed_slot >= 0 && changed_slot < backend.touches.len {
		touch := backend.touches[changed_slot]
		if touch.active && touch.window_id == record.id {
			return touch.x, touch.y
		}
	}
	for touch in backend.touches {
		if touch.active && touch.window_id == record.id {
			return touch.x, touch.y
		}
	}
	return record.mouse_x, record.mouse_y
}

fn (mut record WaylandWindowRecord) update_mouse_position(x f32, y f32, clear_delta bool) {
	if clear_delta || !record.mouse_pos_valid {
		record.mouse_dx = 0
		record.mouse_dy = 0
	} else {
		record.mouse_dx = x - record.mouse_x
		record.mouse_dy = y - record.mouse_y
	}
	record.mouse_x = x
	record.mouse_y = y
	record.mouse_pos_valid = true
}

fn wayland_sort_native_events(mut events []WaylandNativeQueuedEvent) {
	for i in 1 .. events.len {
		mut j := i
		for j > 0 && events[j - 1].sequence > events[j].sequence {
			event := events[j]
			events[j] = events[j - 1]
			events[j - 1] = event
			j--
		}
	}
}

fn (backend &WaylandBackend) window_record_index_for_surface(surface voidptr) ?int {
	for i, record in backend.windows {
		if record.surface == surface {
			return i
		}
	}
	return none
}

fn (backend &WaylandBackend) pointer_focus_record_index() ?int {
	if !backend.pointer_focused {
		return none
	}
	return backend.window_record_index(backend.pointer_focus)
}

fn (backend &WaylandBackend) keyboard_focus_record_index() ?int {
	if !backend.keyboard_focused {
		return none
	}
	return backend.window_record_index(backend.keyboard_focus)
}

fn (backend &WaylandBackend) event_modifiers() u32 {
	return backend.modifiers | backend.pointer_buttons
}

fn wayland_resize_edge(edge WindowResizeEdge) u32 {
	return match edge {
		.top { wayland_xdg_toplevel_resize_edge_top }
		.bottom { wayland_xdg_toplevel_resize_edge_bottom }
		.left { wayland_xdg_toplevel_resize_edge_left }
		.right { wayland_xdg_toplevel_resize_edge_right }
		.top_left { wayland_xdg_toplevel_resize_edge_top_left }
		.top_right { wayland_xdg_toplevel_resize_edge_top_right }
		.bottom_left { wayland_xdg_toplevel_resize_edge_bottom_left }
		.bottom_right { wayland_xdg_toplevel_resize_edge_bottom_right }
	}
}

fn wayland_is_clipboard_paste(key_code int, modifiers u32) bool {
	return key_code == wayland_key_v && modifiers == wayland_modifier_ctrl
}

fn (backend &WaylandBackend) key_repeats(raw_key u32) bool {
	$if linux && sokol_wayland ? {
		if backend.xkb_keymap == unsafe { nil } {
			return false
		}
		return C.xkb_keymap_key_repeats(unsafe { &C.xkb_keymap(backend.xkb_keymap) }, raw_key) != 0
	}
	_ = raw_key
	return false
}

fn (backend &WaylandBackend) char_code_for_key(key u32) u32 {
	$if linux && sokol_wayland ? {
		if backend.xkb_state == unsafe { nil } {
			return 0
		}
		mut buf := [8]u8{}
		count := C.xkb_state_key_get_utf8(unsafe { &C.xkb_state(backend.xkb_state) }, key,
			&char(&buf[0]), usize(buf.len))
		if count <= 0 || count >= buf.len {
			return 0
		}
		codepoint := wayland_utf8_decode(&buf[0], count)
		if codepoint > 0 && codepoint < 0x110000 {
			return codepoint
		}
	}
	return 0
}

fn (backend &WaylandBackend) xkb_modifiers() u32 {
	$if linux && sokol_wayland ? {
		if backend.xkb_state == unsafe { nil } {
			return backend.modifiers
		}
		state := unsafe { &C.xkb_state(backend.xkb_state) }
		mut modifiers := backend.modifiers & (wayland_modifier_lmb | wayland_modifier_rmb | wayland_modifier_mmb)
		if C.xkb_state_mod_name_is_active(state, c'Shift', wayland_xkb_state_mods_effective) > 0 {
			modifiers |= wayland_modifier_shift
		}
		if C.xkb_state_mod_name_is_active(state, c'Control', wayland_xkb_state_mods_effective) > 0 {
			modifiers |= wayland_modifier_ctrl
		}
		if C.xkb_state_mod_name_is_active(state, c'Mod1', wayland_xkb_state_mods_effective) > 0 {
			modifiers |= wayland_modifier_alt
		}
		if C.xkb_state_mod_name_is_active(state, c'Mod4', wayland_xkb_state_mods_effective) > 0 {
			modifiers |= wayland_modifier_super
		}
		return modifiers
	}
	return backend.modifiers
}

fn (mut backend WaylandBackend) touch_slot_for_down(id int) ?int {
	if slot := backend.touch_slot_for_id(id) {
		return slot
	}
	for i, touch in backend.touches {
		if !touch.active {
			return i
		}
	}
	return none
}

fn (backend &WaylandBackend) touch_slot_for_id(id int) ?int {
	for i, touch in backend.touches {
		if touch.active && touch.id == id {
			return i
		}
	}
	return none
}

fn (backend &WaylandBackend) touch_count_for_window(id WindowId) int {
	mut count := 0
	for touch in backend.touches {
		if touch.active && touch.window_id == id {
			count++
		}
	}
	return count
}

fn (mut backend WaylandBackend) clear_touches() {
	for i in 0 .. backend.touches.len {
		backend.touches[i] = WaylandTouchPoint{}
	}
}

fn (mut backend WaylandBackend) update_modifier_for_key(key_code int, down bool) {
	modifier := match key_code {
		340, 344 { wayland_modifier_shift }
		341, 345 { wayland_modifier_ctrl }
		342, 346 { wayland_modifier_alt }
		343, 347 { wayland_modifier_super }
		else { u32(0) }
	}

	if modifier == 0 {
		return
	}
	if down {
		backend.modifiers |= modifier
	} else {
		backend.modifiers &= ~modifier
	}
}

fn (mut backend WaylandBackend) clear_keys_down() {
	backend.stop_key_repeat()
	for i in 0 .. backend.keys_down.len {
		backend.keys_down[i] = false
	}
}

fn wayland_utf8_decode(buf &u8, count int) u32 {
	if count <= 0 {
		return 0
	}
	b0 := unsafe { buf[0] }
	if (b0 & 0x80) == 0 {
		return u32(b0)
	}
	if (b0 & 0xe0) == 0xc0 && count >= 2 {
		return (u32(b0 & 0x1f) << 6) | u32(unsafe { buf[1] } & 0x3f)
	}
	if (b0 & 0xf0) == 0xe0 && count >= 3 {
		return (u32(b0 & 0x0f) << 12) | (u32(unsafe { buf[1] } & 0x3f) << 6) | u32(unsafe {
			buf[2]
		} & 0x3f)
	}
	if (b0 & 0xf8) == 0xf0 && count >= 4 {
		return (u32(b0 & 0x07) << 18) | (u32(unsafe { buf[1] } & 0x3f) << 12) | (u32(unsafe {
			buf[2]
		} & 0x3f) << 6) | u32(unsafe { buf[3] } & 0x3f)
	}
	return 0
}

fn wayland_mouse_button(button u32) int {
	return match button {
		wayland_btn_left { 0 }
		wayland_btn_right { 1 }
		wayland_btn_middle { 2 }
		else { wayland_invalid_mouse_button }
	}
}

fn wayland_mouse_modifier(button u32) u32 {
	return match button {
		wayland_btn_left { wayland_modifier_lmb }
		wayland_btn_right { wayland_modifier_rmb }
		wayland_btn_middle { wayland_modifier_mmb }
		else { u32(0) }
	}
}

fn wayland_key_code(key u32) int {
	return match key {
		2 { 49 }
		3 { 50 }
		4 { 51 }
		5 { 52 }
		6 { 53 }
		7 { 54 }
		8 { 55 }
		9 { 56 }
		10 { 57 }
		11 { 48 }
		12 { 45 }
		13 { 61 }
		14 { 259 }
		15 { 258 }
		16 { 81 }
		17 { 87 }
		18 { 69 }
		19 { 82 }
		20 { 84 }
		21 { 89 }
		22 { 85 }
		23 { 73 }
		24 { 79 }
		25 { 80 }
		26 { 91 }
		27 { 93 }
		28 { 257 }
		29 { 341 }
		30 { 65 }
		31 { 83 }
		32 { 68 }
		33 { 70 }
		34 { 71 }
		35 { 72 }
		36 { 74 }
		37 { 75 }
		38 { 76 }
		39 { 59 }
		40 { 39 }
		41 { 96 }
		42 { 340 }
		43 { 92 }
		44 { 90 }
		45 { 88 }
		46 { 67 }
		47 { 86 }
		48 { 66 }
		49 { 78 }
		50 { 77 }
		51 { 44 }
		52 { 46 }
		53 { 47 }
		54 { 344 }
		55 { 332 }
		56 { 342 }
		57 { 32 }
		58 { 280 }
		59 { 290 }
		60 { 291 }
		61 { 292 }
		62 { 293 }
		63 { 294 }
		64 { 295 }
		65 { 296 }
		66 { 297 }
		67 { 298 }
		68 { 299 }
		69 { 282 }
		70 { 281 }
		71 { 327 }
		72 { 328 }
		73 { 329 }
		74 { 333 }
		75 { 324 }
		76 { 325 }
		77 { 326 }
		78 { 334 }
		79 { 321 }
		80 { 322 }
		81 { 323 }
		82 { 320 }
		83 { 330 }
		86 { 162 }
		87 { 300 }
		88 { 301 }
		96 { 335 }
		97 { 345 }
		98 { 331 }
		99 { 283 }
		100 { 346 }
		102 { 268 }
		103 { 265 }
		104 { 266 }
		105 { 263 }
		106 { 262 }
		107 { 269 }
		108 { 264 }
		109 { 267 }
		110 { 260 }
		111 { 261 }
		119 { 284 }
		125 { 343 }
		126 { 347 }
		else { 0 }
	}
}

fn (mut backend WaylandBackend) stop() ! {
	$if linux && sokol_wayland ? {
		close_error := backend.close_connection()
		mut terminal_error := merge_backend_errors(close_error, backend.take_poll_error())
		if backend.retains_native_ownership() {
			terminal_error = merge_backend_errors(terminal_error,
				err_render_native_renderer_unavailable)
		}
		if terminal_error != '' {
			return error(terminal_error)
		}
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) dispatch_pending_nonblocking() NativeRenderResult {
	$if linux && sokol_wayland ? {
		if backend.display == unsafe { nil } || backend.render_health.blocks_graphics()
			|| backend.wayland_display_unavailable {
			return backend.record_wayland_result(native_wayland_logical_result(.display_dispatch,
				.renderer, .renderer_unavailable, backend.wayland_display_error,
				err_wayland_connect_failed))
		}
		flush := backend.attempt_wayland_flush(NativeOperationSeed{
			call_site: .display_transport
			scope:     .batch
		})
		if !flush.succeeded() {
			return flush
		}
		mut last_outcome := flush
		display := unsafe { &C.wl_display(backend.display) }
		display_seed := NativeOperationSeed{
			presence_mask:   native_context_has_target_identity
			call_site:       .display_transport
			scope:           .batch
			target_identity: native_identity(display)
		}
		for {
			mut ordinals := backend.native_operations.reserve_ordinals(2) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.blocked_renderer_result(.display_dispatch)
			}
			context := ordinals.materialize(backend.native_operations, .wayland, .display_dispatch,
				display_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.blocked_renderer_result(.display_dispatch)
			}
			mut raw := C.VMultiwindowNativePrimitive{}
			C.v_multiwindow_wayland_dispatch_pending(display, &raw)
			dispatch := backend.accept_wayland_result(context, mut ordinals, display, raw, .none,
				err_wayland_dispatch_failed)
			last_outcome = dispatch
			if !dispatch.succeeded() {
				return dispatch
			}
			if dispatch.primitive.return_value == 0 {
				break
			}
		}
		for {
			mut ordinals := backend.native_operations.reserve_ordinals(10) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.blocked_renderer_result(.display_prepare)
			}
			prepare_context := ordinals.materialize(backend.native_operations, .wayland,
				.display_prepare, display_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.blocked_renderer_result(.display_prepare)
			}
			mut raw := C.VMultiwindowNativePrimitive{}
			C.v_multiwindow_wayland_prepare_read(display, &raw)
			prepare_read_armed := raw.valid_mask & native_valid_return_value != 0
				&& raw.return_value == 0
			prepare := backend.accept_wayland_result(prepare_context, mut ordinals, display, raw,
				.none, err_wayland_dispatch_failed)
			if !prepare.succeeded() {
				if prepare_read_armed {
					ordinals.skip(6) or {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return backend.blocked_renderer_result(.display_cancel)
					}
					cancel_context := ordinals.materialize(backend.native_operations, .wayland,
						.display_cancel, display_seed) or {
						backend.render_health =
							renderer_health_latch_unavailable(backend.render_health)
						return backend.blocked_renderer_result(.display_cancel)
					}
					C.v_multiwindow_wayland_cancel_read(display, &raw)
					cancel := backend.accept_wayland_cancel_result(cancel_context, mut ordinals,
						display, raw, err_wayland_dispatch_failed)
					if cancel.blocks_graphics() && !prepare.blocks_graphics() {
						return cancel
					}
					return prepare
				}
				if prepare.blocks_graphics() || prepare.disposition != .transient {
					return prepare
				}
				mut dispatch_ordinals := backend.native_operations.reserve_ordinals(2) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.display_dispatch)
				}
				dispatch_context := dispatch_ordinals.materialize(backend.native_operations,
					.wayland, .display_dispatch, display_seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.display_dispatch)
				}
				C.v_multiwindow_wayland_dispatch_pending(display, &raw)
				dispatch := backend.accept_wayland_result(dispatch_context, mut dispatch_ordinals,
					display, raw, .none, err_wayland_dispatch_failed)
				last_outcome = dispatch
				if !dispatch.succeeded() {
					return dispatch
				}
				if dispatch.primitive.return_value == 0 {
					break
				}
				continue
			}
			if !prepare_read_armed {
				return backend.record_wayland_result(native_wayland_logical_result(.display_prepare,
					.batch, .transient, 0, err_wayland_dispatch_failed))
			}
			fd_context := ordinals.materialize(backend.native_operations, .wayland,
				.display_fd_query, display_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.blocked_renderer_result(.display_fd_query)
			}
			C.v_multiwindow_wayland_get_fd(display, &raw)
			fd_outcome := backend.accept_wayland_result(fd_context, mut ordinals, display, raw,
				.none, err_wayland_dispatch_failed)
			last_outcome = fd_outcome
			if !fd_outcome.succeeded() {
				ordinals.skip(4) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.display_cancel)
				}
				cancel_context := ordinals.materialize(backend.native_operations, .wayland,
					.display_cancel, display_seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.display_cancel)
				}
				C.v_multiwindow_wayland_cancel_read(display, &raw)
				cancel := backend.accept_wayland_cancel_result(cancel_context, mut ordinals,
					display, raw, err_wayland_dispatch_failed)
				if !cancel.succeeded() && !fd_outcome.blocks_graphics() {
					return cancel
				}
				return fd_outcome
			}
			fd := int(fd_outcome.primitive.return_value)
			poll_seed := display_seed.with_target_identity(u64(fd))
			poll_context := ordinals.materialize(backend.native_operations, .wayland,
				.display_poll, poll_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.blocked_renderer_result(.display_poll)
			}
			mut poll_fd := C.pollfd{
				fd:      fd
				events:  wayland_poll_in
				revents: i16(0)
			}
			C.v_multiwindow_wayland_poll(&poll_fd, u64(1), 0, &raw)
			poll_outcome := backend.accept_wayland_result(poll_context, mut ordinals, display, raw,
				.none, err_wayland_dispatch_failed)
			last_outcome = poll_outcome
			if !poll_outcome.succeeded() {
				ordinals.skip(2) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.display_cancel)
				}
				cancel_context := ordinals.materialize(backend.native_operations, .wayland,
					.display_cancel, display_seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.display_cancel)
				}
				C.v_multiwindow_wayland_cancel_read(display, &raw)
				cancel := backend.accept_wayland_cancel_result(cancel_context, mut ordinals,
					display, raw, err_wayland_dispatch_failed)
				if !cancel.succeeded() && !poll_outcome.blocks_graphics() {
					return cancel
				}
				return poll_outcome
			}
			if poll_outcome.primitive.return_value == 0
				|| (poll_outcome.primitive.observed_flags & u64(wayland_poll_in)) == 0 {
				ordinals.skip(2) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.display_cancel)
				}
				cancel_context := ordinals.materialize(backend.native_operations, .wayland,
					.display_cancel, display_seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.display_cancel)
				}
				C.v_multiwindow_wayland_cancel_read(display, &raw)
				cancel := backend.accept_wayland_cancel_result(cancel_context, mut ordinals,
					display, raw, err_wayland_dispatch_failed)
				if !cancel.succeeded() {
					return cancel
				}
				break
			}
			read_context := ordinals.materialize(backend.native_operations, .wayland,
				.display_read, display_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.blocked_renderer_result(.display_read)
			}
			C.v_multiwindow_wayland_read_events(display, &raw)
			read := backend.accept_wayland_result(read_context, mut ordinals, display, raw, .none,
				err_wayland_dispatch_failed)
			ordinals.skip(2) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.blocked_renderer_result(.display_cancel)
			}
			last_outcome = read
			if !read.succeeded() {
				return read
			}
			for {
				mut dispatch_ordinals := backend.native_operations.reserve_ordinals(2) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.display_dispatch)
				}
				dispatch_context := dispatch_ordinals.materialize(backend.native_operations,
					.wayland, .display_dispatch, display_seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.display_dispatch)
				}
				C.v_multiwindow_wayland_dispatch_pending(display, &raw)
				dispatch := backend.accept_wayland_result(dispatch_context, mut dispatch_ordinals,
					display, raw, .none, err_wayland_dispatch_failed)
				last_outcome = dispatch
				if !dispatch.succeeded() {
					return dispatch
				}
				if dispatch.primitive.return_value == 0 {
					break
				}
			}
		}
		return last_outcome
	} $else {
		return native_wayland_logical_result(.display_dispatch, .renderer, .renderer_unavailable,
			0, err_backend_unsupported)
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn (mut backend WaylandBackend) render_environment(id WindowId) !gfx.Environment {
		$if linux && sokol_wayland ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			record := backend.windows[index]
			seed := NativeOperationSeed{
				presence_mask:     native_context_has_window | native_context_has_target_generation
				call_site:         .renderer_start
				scope:             .window_target
				window:            record.id
				target_generation: record.render_target_generation
			}
			preparation := backend.prepare_render_window(index, seed)
			if !preparation.succeeded() {
				return native_render_error(preparation)
			}
			return gfx.Environment{
				defaults: gfx.EnvironmentDefaults{
					color_format: .rgba8
					depth_format: .depth_stencil
					sample_count: 1
				}
			}
		} $else {
			_ = id
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) begin_render(id WindowId, candidate RenderWindowSnapshot, native_attempt NativeTargetAttempt) BackendFrameAttempt {
		$if linux && sokol_wayland ? {
			index := backend.window_record_index(id) or {
				return BackendFrameAttempt{
					outcome: native_render_outcome(.none, .window_surface_create, .window_target,
						.operation_failed, 0, 0, err_window_not_found)
				}
			}
			seed := NativeOperationSeed{
				presence_mask:      native_context_window_target_fields
				call_site:          .window_prepare
				scope:              .window_target
				window:             id
				target_generation:  candidate.target.target_identity
				batch_epoch:        native_attempt.batch_epoch
				window_lease_epoch: native_attempt.window_lease_epoch
				target_lease_epoch: native_attempt.target_lease_epoch
			}
			preparation := backend.prepare_render_window(index, seed)
			if !preparation.succeeded() {
				return BackendFrameAttempt{
					outcome: preparation
				}
			}
			mut record := backend.windows[index]
			if backend.render_health.blocks_graphics() || backend.wayland_display_unavailable {
				return BackendFrameAttempt{
					outcome: backend.blocked_renderer_result(.swap_buffers)
				}
			}
			if !record.frame_ready {
				return BackendFrameAttempt{
					outcome: backend.record_wayland_result(native_wayland_logical_result(.frame_callback,
						.window_target, .transient, 0, err_render_target_not_eligible))
				}
			}
			if candidate.window != id
				|| candidate.target.target_identity != record.render_target_generation
				|| candidate.metrics.framebuffer_width != record.width
				|| candidate.metrics.framebuffer_height != record.height
				|| candidate.target.color_format != int(gfx.PixelFormat.rgba8)
				|| candidate.target.depth_format != int(gfx.PixelFormat.depth_stencil)
				|| candidate.target.sample_count != 1 {
				return BackendFrameAttempt{
					outcome: native_render_outcome(.none, .window_surface_create, .window_target,
						.target_lost, 0, 0, err_render_target_stale)
				}
			}
			return BackendFrameAttempt{
				frame:   RenderFrame{
					window_id:          id
					batch_epoch:        native_attempt.batch_epoch
					window_lease_epoch: native_attempt.window_lease_epoch
					target_lease_epoch: native_attempt.target_lease_epoch
					metrics:            candidate.metrics
					target:             candidate.target
					swapchain:          backend.swapchain_for_record(record)
				}
				outcome: preparation
			}
		} $else {
			_ = id
			_ = candidate
			_ = native_attempt
			return BackendFrameAttempt{
				outcome: native_wayland_logical_result(.window_surface_create, .renderer,
					.renderer_unavailable, 0, err_backend_unsupported)
			}
		}
	}

	fn (mut backend WaylandBackend) end_render(frame RenderFrame) BackendFinalizeAttempt {
		$if linux && sokol_wayland ? {
			index := backend.window_record_index(frame.window_id) or {
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: native_render_outcome(.none, .swap_buffers, .window_target,
						.operation_failed, 0, 0, err_window_not_found)
				}
			}
			mut record := backend.windows[index]
			was_mapped := record.mapped
			if backend.render_health.blocks_graphics() || backend.wayland_display_unavailable {
				if record.frame_callback != unsafe { nil } {
					_ = backend.destroy_frame_callback_lifetime(mut record)
					record.frame_ready = true
				}
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.blocked_renderer_result(.swap_buffers)
				}
			}
			frame_seed := native_seed_for_frame(frame, .window_finalize)
			binding := backend.make_current(index, .window_target,
				frame_seed.with_target_identity(native_identity(record.egl_surface)))
			if !binding.succeeded() {
				if record.frame_callback != unsafe { nil } {
					_ = backend.destroy_frame_callback_lifetime(mut record)
					record.frame_ready = true
				}
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: binding
				}
			}
			if record.frame_callback != unsafe { nil } {
				stale_destroy := backend.destroy_frame_callback_lifetime(mut record)
				if !stale_destroy.succeeded() {
					record.frame_ready = true
					return BackendFinalizeAttempt{
						status:  .not_presented
						outcome: stale_destroy
					}
				}
			}
			if record.surface == unsafe { nil } {
				record.frame_ready = true
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.record_wayland_result(native_wayland_logical_result(.frame_callback,
						.window_target, .native_window_lost, 0, err_render_native_window_lost))
				}
			}
			if backend.display == unsafe { nil } {
				record.frame_ready = true
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.record_wayland_result(native_wayland_logical_result(.display_flush,
						.renderer, .renderer_unavailable, 0, err_wayland_connect_failed))
				}
			}
			mut frame_ordinals := backend.native_operations.reserve_ordinals(9) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.blocked_renderer_result(.frame_callback)
				}
			}
			mut submission_ordinals := frame_ordinals.split_tail(4) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.blocked_renderer_result(.frame_callback)
				}
			}
			mut cleanup_ordinals := frame_ordinals.split_tail(1) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.blocked_renderer_result(.frame_callback)
				}
			}
			cleanup_ticket := backend.reserve_wayland_lifetime_ticket(mut cleanup_ordinals,
				.wayland_frame_callback, frame_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.blocked_renderer_result(.frame_callback)
				}
			}
			create_seed := frame_seed.with_target_identity(native_identity(record.surface))
			create_context := frame_ordinals.materialize(backend.native_operations, .wayland,
				.frame_callback, create_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.blocked_renderer_result(.frame_callback)
				}
			}
			display := unsafe { &C.wl_display(backend.display) }
			swap_seed := frame_seed.with_target_identity(native_identity(record.egl_surface))
			mut swap_ordinals := submission_ordinals
			mut flush_ordinals := swap_ordinals.split_tail(2) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
				record.frame_ready = true
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.blocked_renderer_result(.display_flush)
				}
			}
			swap_context := swap_ordinals.materialize(backend.native_operations, .egl,
				.swap_buffers, swap_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
				record.frame_ready = true
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.blocked_renderer_result(.swap_buffers)
				}
			}
			flush_attempt := backend.materialize_prepared_wayland_transport(frame_seed,
				.display_flush, display, mut flush_ordinals) or {
				backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
				record.frame_ready = true
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.blocked_renderer_result(.display_flush)
				}
			}
			mut raw := C.VMultiwindowNativePrimitive{}
			C.v_multiwindow_wayland_surface_frame(unsafe { &C.wl_surface(record.surface) }, &raw)
			record.frame_callback = native_pointer(raw.handle)
			if record.frame_callback != unsafe { nil } {
				record.frame_callback_ticket = cleanup_ticket
				backend.native_operations.bind_lifetime_ticket(cleanup_ticket,
					native_identity(record.frame_callback), native_identity(record.surface))
			}
			frame_create := backend.accept_wayland_result(create_context, mut frame_ordinals,
				display, raw, .none, err_wayland_egl_swap_buffers_failed)
			if record.frame_callback == unsafe { nil } {
				backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
			}
			if !frame_create.succeeded() {
				frame_ordinals.skip(2) or {}
				if record.frame_callback != unsafe { nil } {
					_ = backend.destroy_frame_callback_lifetime(mut record)
				}
				_ = backend.execute_prepared_wayland_transport(flush_attempt)
				record.frame_ready = true
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: frame_create
				}
			}
			listener_seed := frame_seed.with_target_identity(native_identity(record.frame_callback))
			listener_context := frame_ordinals.materialize(backend.native_operations, .wayland,
				.frame_callback, listener_seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				_ = backend.destroy_frame_callback_lifetime(mut record)
				_ = backend.execute_prepared_wayland_transport(flush_attempt)
				record.frame_ready = true
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: backend.blocked_renderer_result(.frame_callback)
				}
			}
			if record.frame_callback != unsafe { nil } {
				_ = backend.native_operations.arm_listener_registration(listener_context)
			}
			C.v_multiwindow_wayland_add_frame_listener(record.frame_callback,
				record.listener_data(), &raw)
			frame_listener := backend.accept_wayland_result(listener_context, mut frame_ordinals,
				display, raw, .none, err_wayland_egl_swap_buffers_failed)
			if !frame_listener.succeeded() {
				_ = backend.destroy_frame_callback_lifetime(mut record)
				_ = backend.execute_prepared_wayland_transport(flush_attempt)
				record.frame_ready = true
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: frame_listener
				}
			}
			C.v_multiwindow_linux_egl_swap_buffers(native_identity(backend.egl_display),
				native_identity(record.egl_surface), &raw)
			swap_result := backend.accept_egl_result(swap_context, mut swap_ordinals, swap_seed,
				raw, .none)
			if !swap_result.succeeded() {
				failure := swap_result
				desired := egl_window_binding(record.id, record.render_target_generation,
					record.egl_surface)
				outcome := backend.handle_window_egl_failure(index, failure, desired)
				_ = backend.destroy_frame_callback_lifetime(mut record)
				_ = backend.execute_prepared_wayland_transport(flush_attempt)
				record.frame_ready = true
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: outcome
				}
			}
			if backend.render_health.blocks_graphics() {
				_ = backend.destroy_frame_callback_lifetime(mut record)
				_ = backend.execute_prepared_wayland_transport(flush_attempt)
				record.frame_ready = true
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: swap_result
				}
			}
			backend.reapply_parent_to_live_children_on_first_map(record.id, was_mapped)
			flush := backend.execute_prepared_wayland_transport(flush_attempt)
			if !flush.succeeded() {
				_ = backend.destroy_frame_callback_lifetime(mut record)
				record.frame_ready = true
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: flush
				}
			}
			backend.observe_window_mapped(index)
			return BackendFinalizeAttempt{
				status:  .submitted
				outcome: flush
			}
		} $else {
			_ = frame
			return BackendFinalizeAttempt{
				status:  .not_presented
				outcome: native_wayland_logical_result(.display_flush, .renderer,
					.renderer_unavailable, 0, err_backend_unsupported)
			}
		}
	}

	fn (mut backend WaylandBackend) prepare_render_window(index int, seed NativeOperationSeed) NativeRenderResult {
		$if linux && sokol_wayland ? {
			if !backend.renderer_ready() {
				return backend.blocked_renderer_result(.device_status)
			}
			configured := backend.ensure_configured(index, seed)
			if !configured.succeeded() {
				return configured
			}
			applied := backend.apply_pending_configure(index, seed)
			if !applied.succeeded() {
				return applied
			}
			return backend.ensure_window_render_target(index, seed)
		} $else {
			_ = index
			_ = seed
			return native_wayland_logical_result(.window_configure, .renderer,
				.renderer_unavailable, 0, err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) ensure_configured(index int, seed NativeOperationSeed) NativeRenderResult {
		$if linux && sokol_wayland ? {
			mut record := backend.windows[index]
			if record.configured {
				return backend.record_wayland_result(native_render_ok(.wayland, .window_configure,
					.window_target))
			}
			dispatch := backend.dispatch_pending_nonblocking()
			if !dispatch.succeeded() {
				return dispatch
			}
			if record.configured {
				return backend.record_wayland_result(native_render_ok(.wayland, .window_configure,
					.window_target))
			}
			if backend.display == unsafe { nil } {
				return backend.record_wayland_result(native_wayland_logical_result(.display_roundtrip,
					.renderer, .renderer_unavailable, 0, err_wayland_connect_failed))
			}
			roundtrip := backend.attempt_wayland_roundtrip(seed)
			if !roundtrip.succeeded() {
				return roundtrip
			}
			if !record.configured {
				return backend.record_wayland_result(native_wayland_logical_result(.window_configure,
					.window_target, .transient, 0, err_wayland_surface_not_configured))
			}
			return backend.record_wayland_result(native_render_ok(.wayland, .window_configure,
				.window_target))
		} $else {
			_ = index
			_ = seed
			return native_wayland_logical_result(.window_configure, .renderer,
				.renderer_unavailable, 0, err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) apply_pending_configure(index int, boundary_seed NativeOperationSeed) NativeRenderResult {
		$if linux && sokol_wayland ? {
			mut record := backend.windows[index]
			if !record.pending_egl_resize {
				return backend.record_wayland_result(native_render_ok(.wayland, .window_configure,
					.window_target))
			}
			if record.wl_egl_window != unsafe { nil } {
				if backend.render_health.blocks_graphics() || backend.wayland_display_unavailable
					|| backend.display == unsafe { nil } {
					return backend.blocked_renderer_result(.window_configure)
				}
				seed := NativeOperationSeed{
					...boundary_seed
					presence_mask:     boundary_seed.presence_mask | native_context_has_window | native_context_has_target_generation | native_context_has_target_identity
					call_site:         .window_prepare
					scope:             .window_target
					window:            record.id
					target_generation: record.render_target_generation
					target_identity:   native_identity(record.wl_egl_window)
				}
				mut ordinals := backend.native_operations.reserve_ordinals(2) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.window_configure)
				}
				context := ordinals.materialize(backend.native_operations, .wayland,
					.window_configure, seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.window_configure)
				}
				mut raw := C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_wayland_egl_resize_window(record.wl_egl_window,
					record.framebuffer_width(), record.framebuffer_height(), &raw)
				display := unsafe { &C.wl_display(backend.display) }
				resize := backend.accept_wayland_result(context, mut ordinals, display, raw,
					.void_completion, err_wayland_egl_surface_failed)
				if !resize.succeeded() {
					return resize
				}
			}
			record.pending_egl_resize = false
			return backend.record_wayland_result(native_render_ok(.wayland, .window_configure,
				.window_target))
		} $else {
			_ = index
			_ = boundary_seed
			return native_wayland_logical_result(.window_configure, .renderer,
				.renderer_unavailable, 0, err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) ensure_window_render_target(index int, boundary_seed NativeOperationSeed) NativeRenderResult {
		$if linux && sokol_wayland ? {
			mut record := backend.windows[index]
			if backend.render_health.blocks_graphics() {
				return backend.blocked_renderer_result(.window_surface_create)
			}
			if record.native_destroyed {
				return backend.record_wayland_result(native_wayland_logical_result(.window_surface_create,
					.window_target, .native_window_lost, 0, err_render_native_window_lost))
			}
			if record.wl_egl_window == unsafe { nil } {
				if record.surface == unsafe { nil } {
					return backend.record_wayland_result(native_wayland_logical_result(.window_surface_create,
						.window_target, .native_window_lost, 0, err_window_not_found))
				}
				seed := NativeOperationSeed{
					...boundary_seed
					presence_mask:     boundary_seed.presence_mask | native_context_has_window | native_context_has_target_generation | native_context_has_target_identity
					call_site:         .window_prepare
					scope:             .window_target
					window:            record.id
					target_generation: record.render_target_generation
					target_identity:   native_identity(record.surface)
				}
				mut ordinals := backend.native_operations.reserve_renderer_attempt_ordinals(2) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.window_surface_create)
				}
				mut cleanup_ordinals := backend.native_operations.reserve_app_lifetime_ordinals(1) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.window_surface_create)
				}
				cleanup_ticket := backend.reserve_wayland_lifetime_ticket(mut cleanup_ordinals,
					.wayland_egl_window, seed.without_target_identity()) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.window_surface_create)
				}
				context := ordinals.materialize(backend.native_operations, .wayland,
					.window_surface_create, seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
					return backend.blocked_renderer_result(.window_surface_create)
				}
				mut raw := C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_wayland_egl_create_window(unsafe {
					&C.wl_surface(record.surface)
				}, record.framebuffer_width(), record.framebuffer_height(), &raw)
				actual_wl_egl_window := native_pointer(raw.handle)
				if actual_wl_egl_window != unsafe { nil } {
					record.wl_egl_window = actual_wl_egl_window
					record.wl_egl_window_ticket = cleanup_ticket
					backend.native_operations.bind_lifetime_ticket(cleanup_ticket,
						native_identity(actual_wl_egl_window), native_identity(record.surface))
				}
				display := unsafe { &C.wl_display(backend.display) }
				egl_window_result := backend.accept_wayland_result(context, mut ordinals, display,
					raw, .none, err_wayland_egl_surface_failed)
				if actual_wl_egl_window == unsafe { nil } {
					backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
				}
				if !egl_window_result.succeeded() {
					if record.wl_egl_window != unsafe { nil } {
						_ = backend.destroy_wl_egl_window_lifetime(mut record)
					}
					return egl_window_result
				}
			}
			if record.egl_surface == unsafe { nil } {
				seed := NativeOperationSeed{
					...boundary_seed
					presence_mask:     boundary_seed.presence_mask | native_context_has_window | native_context_has_target_generation | native_context_has_target_identity
					call_site:         .window_prepare
					scope:             .window_target
					window:            record.id
					target_generation: record.render_target_generation
					target_identity:   native_identity(record.wl_egl_window)
				}
				mut ordinals := backend.native_operations.reserve_renderer_attempt_ordinals(2) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.window_surface_create)
				}
				mut cleanup_ordinals := backend.native_operations.reserve_app_lifetime_ordinals(1) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.window_surface_create)
				}
				cleanup_ticket := backend.native_operations.reserve_linux_egl_lifetime_ticket(mut cleanup_ordinals,
					.egl_surface, seed.without_target_identity()) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					return backend.blocked_renderer_result(.window_surface_create)
				}
				context := ordinals.materialize(backend.native_operations, .egl,
					.window_surface_create, seed) or {
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
					return backend.blocked_renderer_result(.window_surface_create)
				}
				mut raw := C.VMultiwindowNativePrimitive{}
				C.v_multiwindow_linux_egl_create_window_surface(native_identity(backend.egl_display),
					native_identity(backend.egl_config), native_identity(record.wl_egl_window),
					&raw)
				record.egl_surface = native_pointer(raw.handle)
				if record.egl_surface != unsafe { nil } {
					record.egl_surface_ticket = cleanup_ticket
					backend.native_operations.bind_lifetime_ticket(cleanup_ticket,
						native_identity(record.egl_surface), native_identity(backend.egl_display))
				}
				result := backend.accept_egl_result(context, mut ordinals, seed, raw, .none)
				if record.egl_surface == unsafe { nil } {
					backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
				}
				if !result.succeeded() {
					actual_surface := record.egl_surface
					release := backend.release_egl_surface_ticket(cleanup_ticket, actual_surface)
					if release.terminal {
						record.egl_surface = unsafe { nil }
						record.egl_surface_ticket = 0
					}
					return backend.handle_window_egl_failure(index, result, egl_window_binding(record.id,
						record.render_target_generation, unsafe { nil }))
				}
				return result
			}
			return native_render_ok(.egl, .window_surface_create, .window_target)
		} $else {
			_ = index
			_ = boundary_seed
			return native_render_outcome(.egl, .window_surface_create, .renderer,
				.renderer_unavailable, 0, 0, err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) make_current(index int, scope NativeRenderScope, seed NativeOperationSeed) NativeRenderResult {
		$if linux && sokol_wayland ? {
			if backend.render_health.blocks_graphics() {
				return backend.blocked_renderer_result(.make_current)
			}
			record := backend.windows[index]
			if !backend.renderer_ready() || record.egl_surface == unsafe { nil } {
				return backend.record_egl_result(native_render_outcome(.egl, .make_current,
					.window_target, .target_lost, 0, 0, err_render_target_lost))
			}
			desired := egl_window_binding(record.id, record.render_target_generation,
				record.egl_surface)
			bind_seed := NativeOperationSeed{
				...seed
				presence_mask:     seed.presence_mask | native_context_has_window | native_context_has_target_generation | native_context_has_target_identity
				scope:             scope
				window:            record.id
				target_generation: record.render_target_generation
				target_identity:   native_identity(record.egl_surface)
			}
			result := backend.bind_egl_identity(desired, bind_seed)
			if !result.succeeded() {
				return backend.handle_window_egl_failure(index, result, desired)
			}
			return result
		} $else {
			_ = index
			_ = scope
			_ = seed
			return native_render_outcome(.egl, .make_current, .renderer, .renderer_unavailable, 0,
				0, err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) invalidate_window_egl_target(index int, expected_surface voidptr, expected_generation u64, native_destroyed bool) {
		if index < 0 || index >= backend.windows.len {
			return
		}
		mut record := backend.windows[index]
		if expected_generation != 0 && record.render_target_generation != expected_generation {
			return
		}
		if expected_surface != unsafe { nil } && record.egl_surface != expected_surface {
			return
		}
		old_surface := record.egl_surface
		old_generation := record.render_target_generation
		if old_surface != unsafe { nil } {
			$if linux && sokol_wayland ? {
				release := backend.release_egl_surface_ticket(record.egl_surface_ticket,
					old_surface)
				if !release.terminal {
					return
				}
			} $else {
				return
			}
		}
		record.egl_surface = unsafe { nil }
		record.egl_surface_ticket = 0
		record.frame_ready = true
		record.pending_egl_resize = false
		record.render_target_generation = exhaust_backend_target_generation(old_generation)
		if native_destroyed {
			record.native_destroyed = true
		}
		if backend.egl_binding.kind == .window && backend.egl_binding.window == record.id
			&& backend.egl_binding.target_generation == old_generation
			&& backend.egl_binding.surface == old_surface {
			backend.egl_binding = EglBindingIdentity{}
		}
	}

	fn (mut backend WaylandBackend) handle_window_egl_failure(index int, result NativeRenderResult, desired EglBindingIdentity) NativeRenderResult {
		if index < 0 || index >= backend.windows.len {
			return native_render_outcome(.none, result.operation, .window_target,
				.operation_failed, result.native_code, 0, err_window_not_found)
		}
		if result.disposition == .target_lost || result.disposition == .native_window_lost {
			backend.invalidate_window_egl_target(index, desired.surface, desired.target_generation,
				result.disposition == .native_window_lost)
		}
		return result
	}

	fn (backend &WaylandBackend) swapchain_for_record(record &WaylandWindowRecord) gfx.Swapchain {
		width := record.framebuffer_width()
		height := record.framebuffer_height()
		return gfx.Swapchain{
			width:        width
			height:       height
			sample_count: 1
			color_format: .rgba8
			depth_format: .depth_stencil
			gl:           gfx.GlSwapchain{
				framebuffer: 0
			}
		}
	}
}

fn (mut backend WaylandBackend) ensure_lifecycle_buffers() ! {
	$if linux && sokol_wayland ? {
		for i in 0 .. backend.windows.len {
			backend.ensure_lifecycle_buffer(i)!
		}
		return
	}
	return error(err_backend_unsupported)
}

fn (record &WaylandWindowRecord) fallback_buffer_for_extent(width int, height int) voidptr {
	if record.fallback_current_buffer == unsafe { nil } || record.fallback_buffer_width != width
		|| record.fallback_buffer_height != height {
		return unsafe { nil }
	}
	return record.fallback_current_buffer
}

fn (mut backend WaylandBackend) ensure_lifecycle_buffer(index int) ! {
	backend.ensure_lifecycle_buffer_with_transport(index, WaylandPreparedTransportAttempt{}, false)!
}

fn (mut backend WaylandBackend) ensure_lifecycle_buffer_with_prepared_transport(index int, flush_attempt WaylandPreparedTransportAttempt) ! {
	backend.ensure_lifecycle_buffer_with_transport(index, flush_attempt, true)!
}

fn (mut backend WaylandBackend) ensure_lifecycle_buffer_with_transport(index int, prepared_flush_attempt WaylandPreparedTransportAttempt, transport_prepared bool) ! {
	$if linux && sokol_wayland ? {
		if backend.render_health.blocks_graphics() || backend.wayland_display_unavailable {
			return error(err_render_native_renderer_unavailable)
		}
		if backend.renderer_ready() {
			return
		}
		if backend.shm == unsafe { nil } {
			return error(err_wayland_buffer_failed)
		}
		if index < 0 || index >= backend.windows.len {
			return error(err_window_not_found)
		}
		record := backend.windows[index]
		was_mapped := record.mapped
		if record.surface == unsafe { nil } {
			return error(err_window_not_found)
		}
		if !record.requested_visible {
			return
		}
		if !record.configured {
			return
		}
		width := record.framebuffer_width()
		height := record.framebuffer_height()
		if record.mapped && record.fallback_current_buffer != unsafe { nil }
			&& record.fallback_buffer_width == width && record.fallback_buffer_height == height {
			return
		}
		mut buffer := record.fallback_buffer_for_extent(width, height)
		if buffer == unsafe { nil } && record.fallback_buffers.len >= wayland_max_fallback_buffers {
			return
		}
		mut flush_attempt := prepared_flush_attempt
		if !transport_prepared {
			mut transport := backend.prepare_service_transport_plan(wayland_window_operation_seed(record.id,
				record.render_target_generation, .display_transport), [.display_flush])!
			flush_attempt = transport.take()
		}
		if buffer == unsafe { nil } {
			buffer = C.v_multiwindow_wayland_create_shm_buffer(unsafe { &C.wl_shm(backend.shm) },
				width, height)
			if buffer == unsafe { nil } {
				if !transport_prepared {
					backend.consume_prepared_service_cleanup(flush_attempt)
				}
				return error(err_wayland_buffer_failed)
			}
			if C.v_multiwindow_wayland_add_buffer_listener(unsafe { &C.wl_buffer(buffer) },
				record.listener_data()) != 0 {
				C.v_multiwindow_wayland_buffer_destroy(unsafe { &C.wl_buffer(buffer) })
				if !transport_prepared {
					backend.consume_prepared_service_cleanup(flush_attempt)
				}
				return error(err_wayland_buffer_failed)
			}
			backend.windows[index].fallback_buffers << buffer
		}
		C.v_multiwindow_wayland_attach_buffer(unsafe { &C.wl_surface(record.surface) },
			unsafe { &C.wl_buffer(buffer) }, width, height)
		backend.windows[index].fallback_current_buffer = buffer
		backend.windows[index].fallback_buffer_width = width
		backend.windows[index].fallback_buffer_height = height
		backend.reapply_parent_to_live_children_on_first_map(record.id, was_mapped)
		backend.finish_prepared_service_request(flush_attempt)!
		backend.observe_window_mapped(index)
		return
	}
	_ = index
	return error(err_backend_unsupported)
}

fn (mut record WaylandWindowRecord) release_fallback_buffer(buffer voidptr) {
	$if linux && sokol_wayland ? {
		for i, fallback_buffer in record.fallback_buffers {
			if fallback_buffer != buffer {
				continue
			}
			if record.owner != unsafe { nil } && record.owner.transport_can_marshal() {
				C.v_multiwindow_wayland_buffer_destroy(unsafe { &C.wl_buffer(buffer) })
			} else {
				C.v_multiwindow_wayland_proxy_destroy_local(buffer)
			}
			if record.fallback_current_buffer == buffer {
				record.fallback_current_buffer = unsafe { nil }
			}
			record.fallback_buffers.delete(i)
			return
		}
	} $else {
		_ = buffer
	}
}

fn (mut backend WaylandBackend) close_connection() string {
	$if linux && sokol_wayland ? {
		mut cleanup_error := ''
		backend.close_all_clipboard_sends()
		mut window_index := backend.windows.len
		for window_index > 0 {
			window_index--
			if !backend.destroy_window_slot(window_index) {
				cleanup_error = merge_backend_errors(cleanup_error,
					err_render_native_renderer_unavailable)
			}
		}
		backend.shutdown_renderer()
		if backend.windows.len == 0 && backend.egl_display == unsafe { nil }
			&& backend.egl_context == unsafe { nil } && backend.anchor_surface == unsafe { nil }
			&& backend.anchor_wl_egl_window == unsafe { nil }
			&& backend.anchor_wl_surface == unsafe { nil } && backend.egl_display_ticket == 0
			&& backend.egl_context_ticket == 0 && backend.anchor_surface_ticket == 0
			&& backend.anchor_wl_egl_window_ticket == 0 && backend.anchor_wl_surface_ticket == 0
			&& backend.egl_thread_ticket == 0 {
			backend.destroy_seat_devices(false)
			backend.destroy_data_device_manager()
			backend.destroy_foreign_exporter()
			backend.destroy_fractional_scale_globals()
			backend.destroy_cursor_shape_manager()
			backend.destroy_relative_pointer_manager()
			backend.destroy_pointer_constraints()
			backend.destroy_xdg_decoration_manager()
			backend.destroy_all_outputs()
			if backend.shm != unsafe { nil } {
				if !backend.destroy_proxy_locally_if_needed(backend.shm) {
					C.v_multiwindow_wayland_shm_destroy(unsafe { &C.wl_shm(backend.shm) })
				}
				backend.shm = unsafe { nil }
				backend.shm_name = 0
			}
			if backend.wm_base != unsafe { nil } {
				if !backend.destroy_proxy_locally_if_needed(backend.wm_base) {
					C.v_multiwindow_wayland_xdg_wm_base_destroy(unsafe {
						&C.xdg_wm_base(backend.wm_base)
					})
				}
				backend.wm_base = unsafe { nil }
				backend.wm_base_name = 0
			}
			if backend.seat != unsafe { nil } {
				if !backend.destroy_proxy_locally_if_needed(backend.seat) {
					C.v_multiwindow_wayland_seat_destroy(unsafe { &C.wl_seat(backend.seat) })
				}
				backend.seat = unsafe { nil }
				backend.seat_name = 0
			}
			if backend.compositor != unsafe { nil } {
				if !backend.destroy_proxy_locally_if_needed(backend.compositor) {
					if backend.compositor_version >= u32(4) {
						C.wl_compositor_destroy(unsafe { &C.wl_compositor(backend.compositor) })
					} else {
						backend.destroy_proxy_locally(backend.compositor)
					}
				}
				backend.compositor = unsafe { nil }
				backend.compositor_name = 0
				backend.compositor_version = 0
			}
			if backend.registry != unsafe { nil } {
				if !backend.destroy_proxy_locally_if_needed(backend.registry) {
					C.wl_registry_destroy(unsafe { &C.wl_registry(backend.registry) })
				}
				backend.registry = unsafe { nil }
			}
			backend.destroy_xkb()
		}
		if backend.retains_native_ownership_except_display() {
			cleanup_error = merge_backend_errors(cleanup_error,
				err_render_native_renderer_unavailable)
		} else if backend.display != unsafe { nil } {
			C.v_multiwindow_wayland_display_disconnect(unsafe { &C.wl_display(backend.display) })
			backend.display = unsafe { nil }
			backend.wayland_display_unavailable = false
			backend.wayland_display_error = 0
		}
		backend.pointer_enter_serial_valid = false
		backend.started = false
		return cleanup_error
	}
	return err_backend_unsupported
}

fn (mut backend WaylandBackend) init_xkb() ! {
	$if linux && sokol_wayland ? {
		if backend.xkb_context != unsafe { nil } {
			return
		}
		context := C.xkb_context_new(wayland_xkb_context_no_flags)
		if context == unsafe { nil } {
			return error(err_wayland_xkb_context_failed)
		}
		backend.xkb_context = unsafe { voidptr(context) }
		return
	}
	return error(err_backend_unsupported)
}

fn (mut backend WaylandBackend) destroy_xkb_keymap_state() {
	$if linux && sokol_wayland ? {
		if backend.xkb_state != unsafe { nil } {
			C.xkb_state_unref(unsafe { &C.xkb_state(backend.xkb_state) })
		}
		if backend.xkb_keymap != unsafe { nil } {
			C.xkb_keymap_unref(unsafe { &C.xkb_keymap(backend.xkb_keymap) })
		}
		backend.xkb_state = unsafe { nil }
		backend.xkb_keymap = unsafe { nil }
	}
}

fn (mut backend WaylandBackend) destroy_xkb() {
	$if linux && sokol_wayland ? {
		backend.destroy_xkb_keymap_state()
		if backend.xkb_context != unsafe { nil } {
			C.xkb_context_unref(unsafe { &C.xkb_context(backend.xkb_context) })
		}
		backend.xkb_context = unsafe { nil }
	}
}

fn (mut backend WaylandBackend) destroy_xdg_decoration_manager() {
	$if linux && sokol_wayland ? {
		if backend.decoration_manager != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.decoration_manager) {
				C.v_multiwindow_wayland_xdg_decoration_manager_destroy(unsafe {
					&C.zxdg_decoration_manager_v1(backend.decoration_manager)
				})
			}
		}
		backend.decoration_manager = unsafe { nil }
		backend.decoration_manager_name = 0
	}
}

fn (mut backend WaylandBackend) attach_fractional_scale_to_record(mut record &WaylandWindowRecord) bool {
	$if linux && sokol_wayland ? {
		if !record.high_dpi || record.surface == unsafe { nil }
			|| record.fractional_scale != unsafe { nil } || record.viewport != unsafe { nil }
			|| backend.fractional_scale_manager == unsafe { nil }
			|| backend.fractional_scale_manager_name == 0 || backend.viewporter == unsafe { nil }
			|| backend.viewporter_name == 0 || !backend.transport_can_marshal() {
			return false
		}
		viewport := C.v_multiwindow_wayland_get_viewport(unsafe {
			&C.wp_viewporter(backend.viewporter)
		}, unsafe { &C.wl_surface(record.surface) })
		if viewport == unsafe { nil } {
			return false
		}
		fractional_scale := C.v_multiwindow_wayland_get_fractional_scale(unsafe {
			&C.wp_fractional_scale_manager_v1(backend.fractional_scale_manager)
		}, unsafe { &C.wl_surface(record.surface) })
		if fractional_scale == unsafe { nil } {
			C.v_multiwindow_wayland_viewport_destroy(unsafe { &C.wp_viewport(viewport) })
			return false
		}
		if C.v_multiwindow_wayland_add_fractional_scale_listener(unsafe {
			&C.wp_fractional_scale_v1(fractional_scale)
		}, record.listener_data()) != 0 {
			C.v_multiwindow_wayland_fractional_scale_destroy(unsafe {
				&C.wp_fractional_scale_v1(fractional_scale)
			})
			C.v_multiwindow_wayland_viewport_destroy(unsafe { &C.wp_viewport(viewport) })
			return false
		}
		record.viewport = viewport
		record.fractional_scale = fractional_scale
		record.fractional_scale_numerator = 0
		record.render_scale = record.effective_render_scale()
		C.wl_surface_set_buffer_scale(unsafe { &C.wl_surface(record.surface) }, 1)
		C.v_multiwindow_wayland_viewport_set_destination(unsafe { &C.wp_viewport(viewport) },
			i32(record.width), i32(record.height))
		return true
	}
	return false
}

fn (mut backend WaylandBackend) attach_fractional_scale_to_windows() {
	for index in 0 .. backend.windows.len {
		mut record := backend.windows[index]
		_ = backend.attach_fractional_scale_to_record(mut record)
	}
}

fn (mut backend WaylandBackend) destroy_fractional_scale_globals() {
	$if linux && sokol_wayland ? {
		if backend.fractional_scale_manager != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.fractional_scale_manager) {
				C.v_multiwindow_wayland_fractional_scale_manager_destroy(unsafe {
					&C.wp_fractional_scale_manager_v1(backend.fractional_scale_manager)
				})
			}
			backend.fractional_scale_manager = unsafe { nil }
		}
		backend.fractional_scale_manager_name = 0
		if backend.viewporter != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.viewporter) {
				C.v_multiwindow_wayland_viewporter_destroy(unsafe {
					&C.wp_viewporter(backend.viewporter)
				})
			}
			backend.viewporter = unsafe { nil }
		}
		backend.viewporter_name = 0
	}
}

fn (mut backend WaylandBackend) ensure_cursor_shape_device() {
	$if linux && sokol_wayland ? {
		if backend.cursor_shape_device != unsafe { nil }
			|| backend.cursor_shape_manager == unsafe { nil } || backend.pointer == unsafe { nil }
			|| backend.render_health.blocks_graphics() || !backend.transport_can_marshal() {
			return
		}
		device := C.v_multiwindow_wayland_cursor_shape_manager_get_pointer(unsafe {
			&C.wp_cursor_shape_manager_v1(backend.cursor_shape_manager)
		}, unsafe { &C.wl_pointer(backend.pointer) })
		if device != unsafe { nil } {
			backend.cursor_shape_device = device
		}
	}
}

fn (mut backend WaylandBackend) destroy_cursor_shape_device() {
	$if linux && sokol_wayland ? {
		if backend.cursor_shape_device != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.cursor_shape_device) {
				C.v_multiwindow_wayland_cursor_shape_device_destroy(unsafe {
					&C.wp_cursor_shape_device_v1(backend.cursor_shape_device)
				})
			}
		}
		backend.cursor_shape_device = unsafe { nil }
	}
}

fn (mut backend WaylandBackend) destroy_cursor_shape_manager() {
	$if linux && sokol_wayland ? {
		backend.destroy_cursor_shape_device()
		if backend.cursor_shape_manager != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.cursor_shape_manager) {
				C.v_multiwindow_wayland_cursor_shape_manager_destroy(unsafe {
					&C.wp_cursor_shape_manager_v1(backend.cursor_shape_manager)
				})
			}
		}
		backend.cursor_shape_manager = unsafe { nil }
		backend.cursor_shape_manager_name = 0
	}
}

fn (mut backend WaylandBackend) release_record_mouse_lock(mut record &WaylandWindowRecord, publish bool, local_only bool) {
	$if linux && sokol_wayland ? {
		was_locked := record.mouse_locked || record.mouse_lock_requested
		if record.locked_pointer != unsafe { nil } {
			if local_only {
				backend.destroy_proxy_locally(record.locked_pointer)
			} else if !backend.destroy_proxy_locally_if_needed(record.locked_pointer) {
				C.v_multiwindow_wayland_locked_pointer_destroy(unsafe {
					&C.zwp_locked_pointer_v1(record.locked_pointer)
				})
			}
			record.locked_pointer = unsafe { nil }
		}
		if record.relative_pointer != unsafe { nil } {
			if local_only {
				backend.destroy_proxy_locally(record.relative_pointer)
			} else if !backend.destroy_proxy_locally_if_needed(record.relative_pointer) {
				C.v_multiwindow_wayland_relative_pointer_destroy(unsafe {
					&C.zwp_relative_pointer_v1(record.relative_pointer)
				})
			}
			record.relative_pointer = unsafe { nil }
		}
		record.mouse_lock_requested = false
		record.mouse_locked = false
		if publish && was_locked {
			record.enqueue_service_state(.mouse_lock)
		}
	} $else {
		_ = record
		_ = publish
	}
}

fn (mut backend WaylandBackend) release_window_mouse_lock(index int, publish bool, local_only bool) {
	if index < 0 || index >= backend.windows.len {
		return
	}
	mut record := backend.windows[index]
	backend.release_record_mouse_lock(mut record, publish, local_only)
}

fn (mut backend WaylandBackend) release_all_mouse_locks(publish bool) {
	for index in 0 .. backend.windows.len {
		backend.release_window_mouse_lock(index, publish, true)
	}
}

fn (mut backend WaylandBackend) destroy_relative_pointer_manager() {
	$if linux && sokol_wayland ? {
		backend.release_all_mouse_locks(true)
		if backend.relative_pointer_manager != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.relative_pointer_manager) {
				C.v_multiwindow_wayland_relative_pointer_manager_destroy(unsafe {
					&C.zwp_relative_pointer_manager_v1(backend.relative_pointer_manager)
				})
			}
		}
	}
	backend.relative_pointer_manager = unsafe { nil }
	backend.relative_pointer_manager_name = 0
}

fn (mut backend WaylandBackend) destroy_pointer_constraints() {
	$if linux && sokol_wayland ? {
		backend.release_all_mouse_locks(true)
		if backend.pointer_constraints != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.pointer_constraints) {
				C.v_multiwindow_wayland_pointer_constraints_destroy(unsafe {
					&C.zwp_pointer_constraints_v1(backend.pointer_constraints)
				})
			}
		}
	}
	backend.pointer_constraints = unsafe { nil }
	backend.pointer_constraints_name = 0
}

fn (mut backend WaylandBackend) destroy_removed_wm_base_if_unused() bool {
	$if linux && sokol_wayland ? {
		if backend.wm_base_name != 0 || backend.wm_base == unsafe { nil }
			|| backend.windows.len != 0 {
			return false
		}
		if !backend.destroy_proxy_locally_if_needed(backend.wm_base) {
			C.v_multiwindow_wayland_xdg_wm_base_destroy(unsafe { &C.xdg_wm_base(backend.wm_base) })
		}
		backend.wm_base = unsafe { nil }
		return true
	}
	return false
}

fn (mut backend WaylandBackend) destroy_seat_devices(publish_mouse_lock bool) {
	$if linux && sokol_wayland ? {
		backend.release_all_mouse_locks(publish_mouse_lock)
		backend.destroy_data_device()
		backend.destroy_cursor_shape_device()
		if backend.pointer != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.pointer) {
				C.v_multiwindow_wayland_pointer_destroy(unsafe { &C.wl_pointer(backend.pointer) })
			}
		}
		if backend.keyboard != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.keyboard) {
				C.v_multiwindow_wayland_keyboard_destroy(unsafe { &C.wl_keyboard(backend.keyboard) })
			}
		}
		if backend.touch != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.touch) {
				C.v_multiwindow_wayland_touch_destroy(unsafe { &C.wl_touch(backend.touch) })
			}
		}
		backend.pointer = unsafe { nil }
		backend.keyboard = unsafe { nil }
		backend.touch = unsafe { nil }
		backend.pointer_focused = false
		backend.pointer_enter_serial_valid = false
		backend.keyboard_focused = false
		backend.pointer_buttons = 0
		backend.modifiers = 0
		backend.clear_key_repeat_info()
		backend.clear_keys_down()
		backend.clear_touches()
		backend.destroy_xkb_keymap_state()
	}
}

fn (mut backend WaylandBackend) destroy_window_record(mut record &WaylandWindowRecord) bool {
	$if linux && sokol_wayland ? {
		backend.cancel_clipboard_read_for_window(record.id)
		backend.destroy_portal_exports_for_window(record.id)
		backend.release_record_mouse_lock(mut record, false, true)
		if backend.pointer_focused && backend.pointer_focus == record.id {
			backend.pointer_focused = false
			backend.pointer_enter_serial_valid = false
			backend.pointer_buttons = 0
		}
		if backend.keyboard_focused && backend.keyboard_focus == record.id {
			backend.keyboard_focused = false
			backend.clear_keys_down()
		}
		backend.stop_key_repeat_for_window(record.id)
		for i, touch in backend.touches {
			if touch.active && touch.window_id == record.id {
				backend.touches[i] = WaylandTouchPoint{}
			}
		}
		if backend.data_offer_window_valid && backend.data_offer_window == record.id {
			backend.clear_data_offer(true)
		}
		mut ticket_children_terminal := true
		if record.frame_callback != unsafe { nil } {
			_ = backend.destroy_frame_callback_lifetime(mut record)
			if record.frame_callback != unsafe { nil } {
				ticket_children_terminal = false
			} else {
				record.frame_ready = true
			}
		}
		if record.egl_surface != unsafe { nil } {
			egl_surface := record.egl_surface
			release := backend.release_egl_surface_ticket(record.egl_surface_ticket, egl_surface)
			if release.terminal {
				record.egl_surface = unsafe { nil }
				record.egl_surface_ticket = 0
			} else {
				ticket_children_terminal = false
			}
		}
		if record.wl_egl_window != unsafe { nil } {
			_ = backend.destroy_wl_egl_window_lifetime(mut record)
			if record.wl_egl_window != unsafe { nil } {
				ticket_children_terminal = false
			}
		}
		if !ticket_children_terminal {
			return false
		}
		for buffer in record.fallback_buffers {
			if buffer != unsafe { nil } {
				if !backend.destroy_proxy_locally_if_needed(buffer) {
					C.v_multiwindow_wayland_buffer_destroy(unsafe { &C.wl_buffer(buffer) })
				}
			}
		}
		record.fallback_buffers.clear()
		record.fallback_current_buffer = unsafe { nil }
		if record.toplevel_decoration != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(record.toplevel_decoration) {
				C.v_multiwindow_wayland_xdg_toplevel_decoration_destroy(unsafe {
					&C.zxdg_toplevel_decoration_v1(record.toplevel_decoration)
				})
			}
			record.toplevel_decoration = unsafe { nil }
		}
		if record.fractional_scale != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(record.fractional_scale) {
				C.v_multiwindow_wayland_fractional_scale_destroy(unsafe {
					&C.wp_fractional_scale_v1(record.fractional_scale)
				})
			}
			record.fractional_scale = unsafe { nil }
			record.fractional_scale_numerator = 0
		}
		if record.viewport != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(record.viewport) {
				C.v_multiwindow_wayland_viewport_destroy(unsafe {
					&C.wp_viewport(record.viewport)
				})
			}
			record.viewport = unsafe { nil }
		}
		if record.xdg_toplevel != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(record.xdg_toplevel) {
				C.v_multiwindow_wayland_xdg_toplevel_destroy(unsafe {
					&C.xdg_toplevel(record.xdg_toplevel)
				})
			}
			record.xdg_toplevel = unsafe { nil }
		}
		if record.xdg_surface != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(record.xdg_surface) {
				C.v_multiwindow_wayland_xdg_surface_destroy(unsafe {
					&C.xdg_surface(record.xdg_surface)
				})
			}
			record.xdg_surface = unsafe { nil }
		}
		if record.surface != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(record.surface) {
				C.wl_surface_destroy(unsafe { &C.wl_surface(record.surface) })
			}
			record.surface = unsafe { nil }
		}
		record.native_destroyed = true
		return true
	}
	return true
}

fn (mut backend WaylandBackend) destroy_window_slot(index int) bool {
	if index < 0 || index >= backend.windows.len {
		return false
	}
	mut record := backend.windows[index]
	if !backend.destroy_window_record(mut record) {
		return false
	}
	backend.windows.delete(index)
	return true
}

fn (mut backend WaylandBackend) ensure_data_device(data voidptr) {
	$if linux && sokol_wayland ? {
		if backend.data_device != unsafe { nil } || backend.data_device_manager == unsafe { nil }
			|| backend.seat == unsafe { nil } || backend.render_health.blocks_graphics()
			|| !backend.transport_can_marshal() {
			return
		}
		device := C.v_multiwindow_wayland_data_device_manager_get_data_device(unsafe {
			&C.wl_data_device_manager(backend.data_device_manager)
		}, unsafe { &C.wl_seat(backend.seat) })
		if device != unsafe { nil }
			&& C.v_multiwindow_wayland_add_data_device_listener(unsafe { &C.wl_data_device(device) }, data) == 0 {
			backend.data_device = device
		} else if device != unsafe { nil } {
			C.v_multiwindow_wayland_data_device_destroy(unsafe { &C.wl_data_device(device) })
		}
	}
}

fn (mut backend WaylandBackend) clear_incoming_offer(destroy bool) {
	$if linux && sokol_wayland ? {
		if destroy && backend.incoming_offer != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.incoming_offer) {
				C.v_multiwindow_wayland_data_offer_destroy(unsafe {
					&C.wl_data_offer(backend.incoming_offer)
				})
			}
		}
		backend.incoming_offer = unsafe { nil }
		backend.incoming_offer_has_uri_list = false
		backend.incoming_offer_has_utf8 = false
		backend.incoming_offer_has_text = false
	}
}

fn (mut backend WaylandBackend) promote_incoming_offer_to_dnd(offer voidptr) bool {
	if offer == unsafe { nil } {
		return false
	}
	if backend.data_offer == offer {
		return true
	}
	if backend.incoming_offer != offer {
		return false
	}
	backend.clear_data_offer(true)
	backend.data_offer = offer
	backend.data_offer_has_uri_list = backend.incoming_offer_has_uri_list
	backend.data_offer_source_actions = wayland_dnd_action_none
	backend.data_offer_selected_action = wayland_dnd_action_none
	backend.data_offer_action_received = false
	backend.incoming_offer = unsafe { nil }
	backend.incoming_offer_has_uri_list = false
	backend.incoming_offer_has_utf8 = false
	backend.incoming_offer_has_text = false
	return true
}

fn (mut backend WaylandBackend) clear_selection_offer(destroy bool) {
	$if linux && sokol_wayland ? {
		if destroy && backend.selection_offer != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.selection_offer) {
				C.v_multiwindow_wayland_data_offer_destroy(unsafe {
					&C.wl_data_offer(backend.selection_offer)
				})
			}
		}
		backend.selection_offer = unsafe { nil }
		backend.selection_offer_has_utf8 = false
		backend.selection_offer_has_text = false
	}
}

fn (mut backend WaylandBackend) set_selection_offer(offer voidptr) {
	if offer == backend.selection_offer {
		return
	}
	if backend.clipboard_read_active {
		backend.finish_clipboard_read(.failed, '', err_clipboard_selection_lost)
	}
	backend.clear_selection_offer(true)
	if offer == unsafe { nil } {
		return
	}
	backend.selection_offer = offer
	if backend.incoming_offer == offer {
		backend.selection_offer_has_utf8 = backend.incoming_offer_has_utf8
		backend.selection_offer_has_text = backend.incoming_offer_has_text
		backend.incoming_offer = unsafe { nil }
		backend.incoming_offer_has_uri_list = false
		backend.incoming_offer_has_utf8 = false
		backend.incoming_offer_has_text = false
	}
}

fn (mut backend WaylandBackend) close_clipboard_send_at(index int) {
	if index < 0 || index >= backend.clipboard_sends.len {
		return
	}
	send := backend.clipboard_sends[index]
	$if linux && sokol_wayland ? {
		if send.fd >= 0 {
			C.close(send.fd)
		}
	}
	payload_bytes := u64(send.payload.len)
	backend.clipboard_send_snapshot_bytes = if payload_bytes <= backend.clipboard_send_snapshot_bytes {
		backend.clipboard_send_snapshot_bytes - payload_bytes
	} else {
		0
	}
	backend.clipboard_sends.delete(index)
	if index < backend.clipboard_send_cursor {
		backend.clipboard_send_cursor--
	}
	if backend.clipboard_sends.len == 0
		|| backend.clipboard_send_cursor >= backend.clipboard_sends.len {
		backend.clipboard_send_cursor = 0
	}
}

fn (mut backend WaylandBackend) close_all_clipboard_sends() {
	for backend.clipboard_sends.len > 0 {
		backend.close_clipboard_send_at(backend.clipboard_sends.len - 1)
	}
	backend.clipboard_send_cursor = 0
	backend.clipboard_send_snapshot_bytes = 0
}

fn (mut backend WaylandBackend) destroy_clipboard_source(local_only bool) {
	$if linux && sokol_wayland ? {
		backend.destroy_clipboard_source_handle(backend.clipboard_source, local_only)
	}
	backend.clipboard_source = unsafe { nil }
	backend.clipboard_text = ''
}

fn (mut backend WaylandBackend) destroy_all_clipboard_sources(local_only bool) {
	backend.destroy_clipboard_source(local_only)
}

fn (mut backend WaylandBackend) destroy_clipboard_source_handle(source voidptr, local_only bool) {
	$if linux && sokol_wayland ? {
		if source == unsafe { nil } {
			return
		}
		$if test {
			if backend.clipboard_source_test.active {
				backend.clipboard_source_test.destroyed << usize(source)
				backend.clipboard_source_test.destroyed_local << local_only
				return
			}
		}
		if local_only {
			backend.destroy_proxy_locally(source)
		} else if !backend.destroy_proxy_locally_if_needed(source) {
			C.v_multiwindow_wayland_data_source_destroy(unsafe { &C.wl_data_source(source) })
		}
	} $else {
		_ = source
		_ = local_only
	}
}

fn (mut backend WaylandBackend) create_clipboard_source() voidptr {
	$if linux && sokol_wayland ? {
		$if test {
			if backend.clipboard_source_test.active && backend.clipboard_source_test.create_override {
				backend.clipboard_source_test.create_override = false
				return backend.clipboard_source_test.next_source
			}
		}
		return C.v_multiwindow_wayland_data_device_manager_create_data_source(unsafe {
			&C.wl_data_device_manager(backend.data_device_manager)
		})
	}
	return unsafe { nil }
}

fn (mut backend WaylandBackend) add_clipboard_source_listener(source voidptr) int {
	$if linux && sokol_wayland ? {
		$if test {
			if backend.clipboard_source_test.active && backend.clipboard_source_test.fail_listener {
				backend.clipboard_source_test.fail_listener = false
				return -1
			}
			if backend.clipboard_source_test.active && backend.clipboard_source_test.bypass_protocol {
				return 0
			}
		}
		return C.v_multiwindow_wayland_add_data_source_listener(unsafe { &C.wl_data_source(source) },
			backend)
	}
	_ = source
	return -1
}

fn (mut backend WaylandBackend) clipboard_read_once(fd int, buffer voidptr, size usize) i64 {
	$if linux && sokol_wayland ? {
		$if test {
			if backend.io_test.clipboard_read_interruptions > 0 {
				backend.io_test.clipboard_read_interruptions--
				return wayland_io_test_interrupted
			}
		}
		return i64(C.read(fd, buffer, size))
	}
	return -1
}

fn (mut backend WaylandBackend) clipboard_write_once(fd int, buffer voidptr, size usize) i64 {
	$if linux && sokol_wayland ? {
		$if test {
			if backend.io_test.clipboard_write_interruptions > 0 {
				backend.io_test.clipboard_write_interruptions--
				return wayland_io_test_interrupted
			}
		}
		return i64(C.v_multiwindow_wayland_safe_write(fd, buffer, size))
	}
	return -1
}

fn (mut backend WaylandBackend) drain_clipboard_send() {
	$if linux && sokol_wayland ? {
		if backend.clipboard_sends.len == 0 {
			return
		}
		for index in 0 .. backend.clipboard_sends.len {
			backend.clipboard_sends[index].blocked = false
		}
		mut attempts := 0
		mut skipped := 0
		for backend.clipboard_sends.len > 0 && attempts < wayland_clipboard_max_io_chunks_per_poll {
			if backend.clipboard_send_cursor >= backend.clipboard_sends.len {
				backend.clipboard_send_cursor = 0
			}
			index := backend.clipboard_send_cursor
			if backend.clipboard_sends[index].blocked {
				backend.clipboard_send_cursor = (index + 1) % backend.clipboard_sends.len
				skipped++
				if skipped >= backend.clipboard_sends.len {
					return
				}
				continue
			}
			skipped = 0
			remaining := backend.clipboard_sends[index].payload.len - backend.clipboard_sends[index].offset
			if remaining <= 0 {
				backend.close_clipboard_send_at(index)
				continue
			}
			chunk := if remaining < wayland_clipboard_io_chunk_size {
				remaining
			} else {
				wayland_clipboard_io_chunk_size
			}
			ptr := unsafe {
				backend.clipboard_sends[index].payload.str + backend.clipboard_sends[index].offset
			}
			attempts++
			n := backend.clipboard_write_once(backend.clipboard_sends[index].fd, ptr, usize(chunk))
			if n > 0 {
				backend.clipboard_sends[index].deadline_ns = vtime.sys_mono_now() +
					wayland_clipboard_timeout_ns
				backend.clipboard_sends[index].offset += int(n)
				if backend.clipboard_sends[index].offset >= backend.clipboard_sends[index].payload.len {
					backend.close_clipboard_send_at(index)
				} else {
					backend.clipboard_send_cursor = (index + 1) % backend.clipboard_sends.len
				}
				continue
			}
			if n == wayland_io_test_interrupted
				|| (n < 0 && C.v_multiwindow_wayland_io_interrupted() != 0) {
				backend.clipboard_send_cursor = (index + 1) % backend.clipboard_sends.len
				continue
			}
			if n < 0 && C.v_multiwindow_wayland_read_would_block() != 0 {
				if vtime.sys_mono_now() >= backend.clipboard_sends[index].deadline_ns {
					backend.close_clipboard_send_at(index)
					continue
				}
				backend.clipboard_sends[index].blocked = true
				backend.clipboard_send_cursor = (index + 1) % backend.clipboard_sends.len
				continue
			}
			backend.close_clipboard_send_at(index)
		}
	}
}

fn (mut backend WaylandBackend) finish_clipboard_read(status ServiceStatus, text string, message string) {
	read := backend.clipboard_read
	$if linux && sokol_wayland ? {
		if read.fd >= 0 {
			C.close(read.fd)
		}
		backend.clipboard_read = WaylandClipboardRead{}
		backend.clipboard_read_active = false
		if index := backend.window_record_index(read.window) {
			mut record := backend.windows[index]
			record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_service_event(ServiceEvent{
				kind:      .clipboard
				window:    read.window
				operation: .clipboard_read
				clipboard: ServiceClipboardResult{
					id:     read.request
					window: read.window
					status: status
					text:   text.clone()
					error:  message
				}
			}))
		}
	} $else {
		_ = read
		_ = status
		_ = text
		_ = message
		backend.clipboard_read = WaylandClipboardRead{}
		backend.clipboard_read_active = false
	}
}

fn (mut backend WaylandBackend) cancel_clipboard_read() {
	$if linux && sokol_wayland ? {
		if backend.clipboard_read.fd >= 0 {
			C.close(backend.clipboard_read.fd)
		}
	}
	backend.clipboard_read = WaylandClipboardRead{}
	backend.clipboard_read_active = false
}

fn (mut backend WaylandBackend) cancel_clipboard_read_for_window(id WindowId) {
	if backend.clipboard_read_active && backend.clipboard_read.window == id {
		backend.cancel_clipboard_read()
	}
}

fn (mut backend WaylandBackend) drain_clipboard_read() {
	$if linux && sokol_wayland ? {
		if !backend.clipboard_read_active {
			return
		}
		mut chunk := [wayland_clipboard_io_chunk_size]u8{}
		for _ in 0 .. wayland_clipboard_max_io_chunks_per_poll {
			remaining := wayland_clipboard_max_bytes - backend.clipboard_read.buffer.len
			if remaining <= 0 {
				mut probe := [1]u8{}
				n := backend.clipboard_read_once(backend.clipboard_read.fd, unsafe { &probe[0] },
					usize(1))
				if n > 0 {
					backend.clipboard_read.deadline_ns = vtime.sys_mono_now() +
						wayland_clipboard_timeout_ns
					backend.finish_clipboard_read(.failed, '', err_clipboard_capacity)
					return
				}
				if n == 0 {
					text := unsafe {
						tos(backend.clipboard_read.buffer.data, backend.clipboard_read.buffer.len).clone()
					}
					backend.finish_clipboard_read(.ready, text, '')
					return
				}
				if n == wayland_io_test_interrupted
					|| (n < 0 && C.v_multiwindow_wayland_io_interrupted() != 0) {
					continue
				}
				if C.v_multiwindow_wayland_read_would_block() != 0 {
					if vtime.sys_mono_now() >= backend.clipboard_read.deadline_ns {
						backend.finish_clipboard_read(.failed, '', err_clipboard_timeout)
					}
					return
				}
				backend.finish_clipboard_read(.failed, '', err_capability_unsupported)
				return
			}
			read_size := if remaining < wayland_clipboard_io_chunk_size {
				remaining
			} else {
				wayland_clipboard_io_chunk_size
			}
			n := backend.clipboard_read_once(backend.clipboard_read.fd, unsafe { &chunk[0] },
				usize(read_size))
			if n > 0 {
				backend.clipboard_read.deadline_ns = vtime.sys_mono_now() +
					wayland_clipboard_timeout_ns
				for index in 0 .. int(n) {
					backend.clipboard_read.buffer << chunk[index]
				}
				continue
			}
			if n == 0 {
				text := if backend.clipboard_read.buffer.len == 0 {
					''
				} else {
					unsafe {
						tos(backend.clipboard_read.buffer.data, backend.clipboard_read.buffer.len).clone()
					}
				}
				backend.finish_clipboard_read(.ready, text, '')
				return
			}
			if n == wayland_io_test_interrupted
				|| (n < 0 && C.v_multiwindow_wayland_io_interrupted() != 0) {
				continue
			}
			if C.v_multiwindow_wayland_read_would_block() != 0 {
				if vtime.sys_mono_now() >= backend.clipboard_read.deadline_ns {
					backend.finish_clipboard_read(.failed, '', err_clipboard_timeout)
				}
				return
			}
			backend.finish_clipboard_read(.failed, '', err_capability_unsupported)
			return
		}
	}
}

fn (mut backend WaylandBackend) clear_data_offer(destroy bool) {
	$if linux && sokol_wayland ? {
		mut pending_offer := voidptr(unsafe { nil })
		if backend.pending_drop_offer != unsafe { nil } || backend.pending_drop_fd >= 0 {
			pending_offer = backend.pending_drop_offer
			backend.clear_pending_data_offer_drop(destroy)
		}
		if destroy && backend.data_offer != unsafe { nil } {
			if backend.data_offer != pending_offer {
				if !backend.destroy_proxy_locally_if_needed(backend.data_offer) {
					C.v_multiwindow_wayland_data_offer_destroy(unsafe {
						&C.wl_data_offer(backend.data_offer)
					})
				}
			}
		}
		backend.data_offer = unsafe { nil }
		backend.data_offer_has_uri_list = false
		backend.data_offer_source_actions = wayland_dnd_action_none
		backend.data_offer_selected_action = wayland_dnd_action_none
		backend.data_offer_action_received = false
		backend.data_offer_window_valid = false
		backend.data_offer_window = WindowId{}
	}
}

fn wayland_dnd_action_allows_finish(source_actions u32, action_received bool, action u32) bool {
	if source_actions != wayland_dnd_action_none && (source_actions & action) == 0 {
		return false
	}
	return action_received
		&& (action == wayland_dnd_action_copy || action == wayland_dnd_action_move)
}

fn (backend &WaylandBackend) data_offer_allows_finish() bool {
	return wayland_dnd_action_allows_finish(backend.data_offer_source_actions,
		backend.data_offer_action_received, backend.data_offer_selected_action)
}

fn (backend &WaylandBackend) pending_drop_allows_finish() bool {
	return wayland_dnd_action_allows_finish(backend.pending_drop_source_actions,
		backend.pending_drop_action_received, backend.pending_drop_selected_action)
}

fn (mut backend WaylandBackend) begin_pending_data_offer_drop() bool {
	$if linux && sokol_wayland ? {
		if backend.render_health.blocks_graphics() || !backend.transport_can_marshal() {
			return false
		}
		if backend.data_offer == unsafe { nil } || backend.pending_drop_offer != unsafe { nil } {
			return false
		}
		mut fds := [2]int{}
		if C.pipe(&fds[0]) == -1 {
			return false
		}
		if C.v_multiwindow_wayland_fd_set_nonblocking(fds[0]) == 0 {
			C.close(fds[0])
			C.close(fds[1])
			return false
		}
		C.v_multiwindow_wayland_data_offer_receive(unsafe { &C.wl_data_offer(backend.data_offer) },
			c'text/uri-list', fds[1])
		C.close(fds[1])
		if backend.display != unsafe { nil } {
			flush := backend.attempt_wayland_flush(NativeOperationSeed{
				call_site: .display_transport
				scope:     .batch
			})
			if !flush.succeeded() {
				C.close(fds[0])
				return false
			}
		}
		backend.pending_drop_offer = backend.data_offer
		backend.pending_drop_fd = fds[0]
		backend.pending_drop_window = backend.data_offer_window
		backend.pending_drop_window_valid = backend.data_offer_window_valid
		backend.pending_drop_source_actions = backend.data_offer_source_actions
		backend.pending_drop_selected_action = backend.data_offer_selected_action
		backend.pending_drop_action_received = backend.data_offer_action_received
		backend.pending_drop_poll_cycles = 0
		backend.pending_drop_buffer = []u8{cap: wayland_uri_list_buffer_size}
		return true
	}
	return false
}

fn (mut backend WaylandBackend) clear_pending_data_offer_drop(destroy bool) {
	$if linux && sokol_wayland ? {
		if backend.pending_drop_fd >= 0 {
			C.close(backend.pending_drop_fd)
		}
		if backend.pending_drop_offer != unsafe { nil } {
			if destroy {
				mut destroy_native := true
				$if test {
					destroy_native = !backend.io_test.pending_drop_native_bypass
				}
				if destroy_native
					&& !backend.destroy_proxy_locally_if_needed(backend.pending_drop_offer) {
					C.v_multiwindow_wayland_data_offer_destroy(unsafe {
						&C.wl_data_offer(backend.pending_drop_offer)
					})
				}
			}
		}
		backend.pending_drop_offer = unsafe { nil }
		backend.pending_drop_fd = -1
		backend.pending_drop_window = WindowId{}
		backend.pending_drop_window_valid = false
		backend.pending_drop_source_actions = wayland_dnd_action_none
		backend.pending_drop_selected_action = wayland_dnd_action_none
		backend.pending_drop_action_received = false
		backend.pending_drop_poll_cycles = 0
		backend.pending_drop_buffer = []u8{}
	}
}

fn (mut backend WaylandBackend) pending_drop_poll_cycle_expired() bool {
	backend.pending_drop_poll_cycles++
	return backend.pending_drop_poll_cycles > wayland_data_offer_max_pending_poll_cycles
}

fn (mut backend WaylandBackend) pending_drop_poll_once(poll_fd voidptr) int {
	$if linux && sokol_wayland ? {
		$if test {
			if backend.io_test.pending_drop_poll_interruptions > 0 {
				backend.io_test.pending_drop_poll_interruptions--
				return int(wayland_io_test_interrupted)
			}
		}
		return C.poll(unsafe { &C.pollfd(poll_fd) }, u64(1), 0)
	}
	return -1
}

fn (mut backend WaylandBackend) pending_drop_read_once(fd int, buffer voidptr, size usize) i64 {
	$if linux && sokol_wayland ? {
		$if test {
			if backend.io_test.pending_drop_read_interruptions > 0 {
				backend.io_test.pending_drop_read_interruptions--
				return wayland_io_test_interrupted
			}
		}
		return i64(C.read(fd, buffer, size))
	}
	return -1
}

fn (mut backend WaylandBackend) drain_pending_data_offer_drop() {
	$if linux && sokol_wayland ? {
		if backend.pending_drop_offer == unsafe { nil } || backend.pending_drop_fd < 0 {
			return
		}
		mut poll_fd := C.pollfd{
			fd:     backend.pending_drop_fd
			events: wayland_poll_in | wayland_poll_err | wayland_poll_hup
		}
		poll_result := backend.pending_drop_poll_once(voidptr(&poll_fd))
		if poll_result == int(wayland_io_test_interrupted)
			|| (poll_result < 0 && C.v_multiwindow_wayland_io_interrupted() != 0) {
			return
		}
		if poll_result == 0 {
			if backend.pending_drop_poll_cycle_expired() {
				backend.clear_data_offer(true)
			}
			return
		}
		if poll_result < 0 || (poll_fd.revents & wayland_poll_err) != 0 {
			backend.clear_data_offer(true)
			return
		}
		poll_hup := (poll_fd.revents & wayland_poll_hup) != 0
		mut should_finish := false
		mut made_progress := false
		mut read_interrupted := false
		if (poll_fd.revents & wayland_poll_in) != 0 || poll_hup {
			mut chunk := [wayland_data_offer_drain_chunk_size]u8{}
			for _ in 0 .. wayland_data_offer_max_read_chunks {
				remaining := wayland_uri_list_buffer_size - backend.pending_drop_buffer.len
				if remaining <= 0 {
					mut probe := [1]u8{}
					n := backend.pending_drop_read_once(backend.pending_drop_fd,
						unsafe { &probe[0] }, usize(1))
					if n > 0 {
						backend.clear_data_offer(true)
						return
					}
					if n == 0 {
						should_finish = true
						break
					}
					if n == wayland_io_test_interrupted
						|| (n < 0 && C.v_multiwindow_wayland_io_interrupted() != 0) {
						read_interrupted = true
						continue
					}
					if C.v_multiwindow_wayland_read_would_block() != 0 {
						break
					}
					backend.clear_data_offer(true)
					return
				}
				read_size := if remaining < wayland_data_offer_drain_chunk_size {
					remaining
				} else {
					wayland_data_offer_drain_chunk_size
				}
				n := backend.pending_drop_read_once(backend.pending_drop_fd, unsafe { &chunk[0] },
					usize(read_size))
				if n > 0 {
					made_progress = true
					for i in 0 .. int(n) {
						backend.pending_drop_buffer << chunk[i]
					}
					continue
				}
				if n == 0 {
					should_finish = true
					break
				}
				if n == wayland_io_test_interrupted
					|| (n < 0 && C.v_multiwindow_wayland_io_interrupted() != 0) {
					read_interrupted = true
					continue
				}
				if C.v_multiwindow_wayland_read_would_block() != 0 {
					break
				}
				backend.clear_data_offer(true)
				return
			}
		}
		if should_finish && backend.transport_can_marshal()
			&& !backend.render_health.blocks_graphics() {
			backend.finish_pending_data_offer_drop()
			return
		}
		if !made_progress && !read_interrupted && backend.pending_drop_poll_cycle_expired() {
			backend.clear_data_offer(true)
		}
	}
}

fn (mut backend WaylandBackend) finish_pending_data_offer_drop() {
	$if linux && sokol_wayland ? {
		if backend.pending_drop_offer == unsafe { nil } {
			return
		}
		payload := if backend.pending_drop_buffer.len > 0 {
			unsafe { tos(&backend.pending_drop_buffer[0], backend.pending_drop_buffer.len).clone() }
		} else {
			''
		}
		files := dropped_files_from_uri_list(payload)
		mut should_finish := false
		if files.len > 0 && backend.pending_drop_window_valid
			&& backend.pending_drop_allows_finish() {
			if index := backend.window_record_index(backend.pending_drop_window) {
				mut record := backend.windows[index]
				record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(),
					queued_input_event(record.input_files_dropped_event(files)))
				should_finish = true
			}
		}
		if should_finish {
			mut finish_native := true
			$if test {
				finish_native = !backend.io_test.pending_drop_native_bypass
			}
			if finish_native {
				C.v_multiwindow_wayland_data_offer_finish(unsafe {
					&C.wl_data_offer(backend.pending_drop_offer)
				})
			}
		}
		backend.clear_data_offer(true)
	}
}

fn (mut backend WaylandBackend) destroy_data_device() {
	$if linux && sokol_wayland ? {
		backend.cancel_clipboard_read()
		backend.destroy_all_clipboard_sources(!backend.transport_can_marshal())
		backend.clear_selection_offer(true)
		backend.clear_incoming_offer(true)
		backend.clear_data_offer(true)
		if backend.data_device != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.data_device) {
				C.v_multiwindow_wayland_data_device_destroy(unsafe {
					&C.wl_data_device(backend.data_device)
				})
			}
		}
		backend.data_device = unsafe { nil }
	}
}

fn (mut backend WaylandBackend) destroy_data_device_manager() {
	$if linux && sokol_wayland ? {
		backend.destroy_data_device()
		if backend.data_device_manager != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.data_device_manager) {
				C.v_multiwindow_wayland_data_device_manager_destroy(unsafe {
					&C.wl_data_device_manager(backend.data_device_manager)
				})
			}
		}
		backend.data_device_manager = unsafe { nil }
		backend.data_device_manager_name = 0
	}
}

fn (backend &WaylandBackend) service_operation_capability(operation ServiceOperation) ServiceOperationCapability {
	return match operation {
		.show {
			ServiceOperationCapability{
				support:          if backend.service_transport_usable(8) {
					.available
				} else {
					.unsupported
				}
				asynchronous:     true
				state_observable: true
			}
		}
		.hide {
			ServiceOperationCapability{
				support:          if backend.service_transport_usable(2) {
					.available
				} else {
					.unsupported
				}
				state_observable: true
			}
		}
		.image_readback {
			ServiceOperationCapability{
				support:      if backend.renderer_ready() { .available } else { .unsupported }
				asynchronous: backend.renderer_ready()
			}
		}
		.window_capture {
			// Wayland has no compositor-owned window capture protocol. gg may expose
			// capture separately while it owns the active render framebuffer.
			ServiceOperationCapability{}
		}
		.clipboard_read, .clipboard_write {
			if backend.service_transport_usable(1) && backend.data_device_manager != unsafe { nil }
				&& backend.data_device != unsafe { nil } && backend.seat != unsafe { nil } {
				ServiceOperationCapability{
					support:              .conditional
					asynchronous:         true
					requires_user_action: operation == .clipboard_write
				}
			} else {
				ServiceOperationCapability{}
			}
		}
		.portal_parent {
			ServiceOperationCapability{
				support:      if backend.service_transport_usable(1)
					&& backend.foreign_exporter != unsafe { nil }
					&& backend.foreign_exporter_name != 0 {
					.available
				} else {
					.unsupported
				}
				asynchronous: true
			}
		}
		.mouse_lock {
			ServiceOperationCapability{
				support:          if backend.service_transport_usable(1)
					&& backend.pointer != unsafe { nil }
					&& backend.relative_pointer_manager != unsafe { nil }
					&& backend.pointer_constraints != unsafe { nil } {
					.conditional
				} else {
					.unsupported
				}
				asynchronous:     true
				state_observable: true
			}
		}
		.minimize, .maximize, .fullscreen {
			ServiceOperationCapability{
				support:          if backend.service_transport_usable(1) {
					.available
				} else {
					.unsupported
				}
				asynchronous:     true
				state_observable: operation != .minimize
			}
		}
		.restore {
			// xdg-shell can leave maximized/fullscreen state, but has no explicit
			// request that restores a compositor-minimized toplevel.
			ServiceOperationCapability{
				support:          if backend.service_transport_usable(1) {
					.conditional
				} else {
					.unsupported
				}
				asynchronous:     true
				state_observable: true
			}
		}
		.native_borrow {
			ServiceOperationCapability{
				support: .available
			}
		}
		else {
			ServiceOperationCapability{}
		}
	}
}

fn (mut backend WaylandBackend) service_show_window(id WindowId) !ServiceWindowState {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if backend.windows[index].requested_visible {
			return backend.service_window_state(id)!
		}
		mut show_handshake_override := false
		$if test {
			show_handshake_override = backend.toplevel_replay_test.show_handshake_override
		}
		if (!show_handshake_override && !backend.service_transport_usable(8))
			|| backend.windows[index].surface == unsafe { nil }
			|| backend.windows[index].xdg_toplevel == unsafe { nil } {
			return error(err_capability_unsupported)
		}
		requested_maximized := backend.windows[index].requested_maximized
		requested_fullscreen := backend.windows[index].requested_fullscreen
		baseline_maximized := if backend.windows[index].observed_service_state_valid {
			backend.windows[index].observed_maximized
		} else {
			requested_maximized
		}
		baseline_fullscreen := if backend.windows[index].observed_service_state_valid {
			backend.windows[index].observed_fullscreen
		} else {
			requested_fullscreen
		}
		seed := wayland_window_operation_seed(id, backend.windows[index].render_target_generation,
			.display_transport)
		mut transport := WaylandServiceTransportPlan{}
		if !show_handshake_override {
			transport = backend.prepare_service_transport_plan(seed, [
				.display_roundtrip,
				.display_flush,
				.display_roundtrip,
				.display_flush,
				.display_roundtrip,
				.display_flush,
				.display_roundtrip,
				.display_flush,
			])!
		}
		backend.drain_hidden_window_before_show(mut transport)!
		backend.prepare_window_show_handshake(index, mut transport, requested_maximized,
			requested_fullscreen, baseline_maximized, baseline_fullscreen)!
		return backend.service_window_state(id)!
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) release_window_render_target_for_hide(index int) ! {
	$if linux && sokol_wayland ? {
		if index < 0 || index >= backend.windows.len {
			return error(err_window_not_found)
		}
		mut record := backend.windows[index]
		old_surface := record.egl_surface
		old_generation := record.render_target_generation
		if old_surface != unsafe { nil } {
			$if test {
				if backend.toplevel_replay_test.hide_render_target_override {
					backend.toplevel_replay_test.operations << 'anchor'
					if backend.toplevel_replay_test.hide_anchor_failure {
						return error(err_render_native_renderer_unavailable)
					}
				} else {
					$if gg_multiwindow ? || x_multiwindow_render ? {
						anchor := backend.make_renderer_anchor_current(.anchor_prepare)
						if !backend.anchor_binding_proven(anchor) {
							return native_render_error(anchor)
						}
					} $else {
						return error(err_render_native_renderer_unavailable)
					}
				}
			} $else {
				$if gg_multiwindow ? || x_multiwindow_render ? {
					anchor := backend.make_renderer_anchor_current(.anchor_prepare)
					if !backend.anchor_binding_proven(anchor) {
						return native_render_error(anchor)
					}
				} $else {
					return error(err_render_native_renderer_unavailable)
				}
			}
		}
		if record.frame_callback != unsafe { nil } {
			destroy := backend.destroy_frame_callback_lifetime(mut record)
			if !destroy.succeeded() {
				record.frame_ready = true
				return native_render_error(destroy)
			}
		}
		if old_surface == unsafe { nil } {
			return
		}
		$if test {
			if backend.toplevel_replay_test.hide_render_target_override {
				backend.toplevel_replay_test.operations << 'release_egl_surface'
				if backend.toplevel_replay_test.hide_surface_release_failure
					|| backend.toplevel_replay_test.hide_surface_unclaimed_terminal {
					record.frame_ready = true
					return error(err_wayland_egl_surface_failed)
				}
				record.egl_surface = unsafe { nil }
				record.egl_surface_ticket = 0
				record.render_target_generation = exhaust_backend_target_generation(old_generation)
				backend.egl_binding = EglBindingIdentity{}
				if backend.toplevel_replay_test.hide_surface_terminal_failure {
					record.frame_ready = true
					return error(err_wayland_egl_surface_failed)
				}
				return
			}
		}
		release := backend.release_egl_surface_ticket(record.egl_surface_ticket, old_surface)
		if !release.claimed || !release.terminal {
			record.frame_ready = true
			return error(err_wayland_egl_surface_failed)
		}
		record.egl_surface = unsafe { nil }
		record.egl_surface_ticket = 0
		record.render_target_generation = exhaust_backend_target_generation(old_generation)
		if backend.egl_binding.kind == .window && backend.egl_binding.window == record.id
			&& backend.egl_binding.target_generation == old_generation
			&& backend.egl_binding.surface == old_surface {
			backend.egl_binding = EglBindingIdentity{}
		}
		if !release.result.succeeded() {
			record.frame_ready = true
			return native_render_error(release.result)
		}
		return
	}
	_ = index
	return error(err_backend_unsupported)
}

fn (mut backend WaylandBackend) service_hide_window(id WindowId) !ServiceWindowState {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.windows[index].requested_visible && !backend.windows[index].mapped {
			return backend.service_window_state(id)!
		}
		mut hide_barrier_test_override := false
		$if test {
			hide_barrier_test_override = backend.toplevel_replay_test.hide_barrier_override
		}
		if (!hide_barrier_test_override && !backend.service_transport_usable(2))
			|| backend.windows[index].surface == unsafe { nil } {
			return error(err_capability_unsupported)
		}
		old_requested_maximized := backend.windows[index].requested_maximized
		old_requested_fullscreen := backend.windows[index].requested_fullscreen
		generation_before_hide := backend.windows[index].render_target_generation
		hidden_generation := exhaust_backend_target_generation(generation_before_hide)
		seed := wayland_window_operation_seed(id, generation_before_hide, .display_transport)
		mut barrier_attempt := WaylandPreparedTransportAttempt{}
		mut flush_attempt := WaylandPreparedTransportAttempt{}
		if !hide_barrier_test_override {
			mut barrier_transport := backend.prepare_service_transport_plan(seed, [
				.display_roundtrip,
			])!
			mut flush_transport := backend.prepare_service_transport_plan(wayland_window_operation_seed(id,
				hidden_generation, .display_transport), [.display_flush])!
			barrier_attempt = barrier_transport.take()
			flush_attempt = flush_transport.take()
		}
		backend.windows[index].hide_barrier_state_observed = false
		backend.windows[index].hide_barrier_active = true
		backend.execute_prepared_wayland_hide_barrier(barrier_attempt) or {
			backend.windows[index].hide_barrier_active = false
			backend.windows[index].requested_maximized = old_requested_maximized
			backend.windows[index].requested_fullscreen = old_requested_fullscreen
			backend.windows[index].hide_barrier_state_observed = false
			return error(err_wayland_dispatch_failed)
		}
		backend.windows[index].hide_barrier_active = false
		if !backend.windows[index].hide_barrier_state_observed
			&& backend.windows[index].observed_service_state_valid {
			backend.windows[index].requested_maximized = backend.windows[index].observed_maximized
			backend.windows[index].requested_fullscreen = backend.windows[index].observed_fullscreen
		}
		backend.windows[index].hide_barrier_state_observed = false
		backend.release_window_render_target_for_hide(index) or {
			if !hide_barrier_test_override {
				backend.consume_prepared_service_cleanup(flush_attempt)
			}
			return err
		}
		backend.release_window_mouse_lock(index, true, false)
		backend.windows[index].requested_visible = false
		backend.windows[index].publish_show_on_map = false
		backend.windows[index].mapped = false
		backend.windows[index].configured = false
		backend.windows[index].frame_ready = false
		backend.windows[index].pending_toplevel_width = 0
		backend.windows[index].pending_toplevel_height = 0
		backend.windows[index].pending_service_state_valid = false
		backend.windows[index].observed_service_state_valid = false
		backend.windows[index].toplevel_decoration_configured = false
		backend.windows[index].toplevel_decoration_mode = 0
		if backend.windows[index].render_target_generation == generation_before_hide {
			backend.windows[index].render_target_generation = hidden_generation
		}
		C.v_multiwindow_wayland_unmap_surface(unsafe {
			&C.wl_surface(backend.windows[index].surface)
		})
		backend.finish_prepared_service_request(flush_attempt)!
		return backend.service_window_state(id)!
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) service_minimize_window(id WindowId) !ServiceWindowState {
	$if linux && sokol_wayland ? {
		mut record := backend.service_window_record(id)!
		mut transport := backend.prepare_service_transport_plan(wayland_window_operation_seed(record.id,
			record.render_target_generation, .display_transport), [.display_flush])!
		flush_attempt := transport.take()
		C.v_multiwindow_wayland_xdg_toplevel_set_minimized(unsafe {
			&C.xdg_toplevel(record.xdg_toplevel)
		})
		backend.finish_prepared_service_request(flush_attempt)!
		return backend.service_window_state(id)!
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) service_maximize_window(id WindowId) !ServiceWindowState {
	$if linux && sokol_wayland ? {
		mut record := backend.service_window_record(id)!
		mut transport := backend.prepare_service_transport_plan(wayland_window_operation_seed(record.id,
			record.render_target_generation, .display_transport), [.display_flush])!
		flush_attempt := transport.take()
		C.v_multiwindow_wayland_xdg_toplevel_set_maximized(unsafe {
			&C.xdg_toplevel(record.xdg_toplevel)
		})
		backend.finish_prepared_service_request(flush_attempt)!
		record.requested_maximized = true
		return backend.service_window_state(id)!
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) service_restore_window(id WindowId) !ServiceWindowState {
	$if linux && sokol_wayland ? {
		mut record := backend.service_window_record(id)!
		mut transport := backend.prepare_service_transport_plan(wayland_window_operation_seed(record.id,
			record.render_target_generation, .display_transport), [.display_flush])!
		flush_attempt := transport.take()
		C.v_multiwindow_wayland_xdg_toplevel_unset_maximized(unsafe {
			&C.xdg_toplevel(record.xdg_toplevel)
		})
		C.v_multiwindow_wayland_xdg_toplevel_unset_fullscreen(unsafe {
			&C.xdg_toplevel(record.xdg_toplevel)
		})
		backend.finish_prepared_service_request(flush_attempt)!
		record.requested_maximized = false
		record.requested_fullscreen = false
		return backend.service_window_state(id)!
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) service_set_fullscreen(id WindowId, enabled bool) !ServiceWindowState {
	$if linux && sokol_wayland ? {
		mut record := backend.service_window_record(id)!
		mut transport := backend.prepare_service_transport_plan(wayland_window_operation_seed(record.id,
			record.render_target_generation, .display_transport), [.display_flush])!
		flush_attempt := transport.take()
		if enabled {
			C.v_multiwindow_wayland_xdg_toplevel_set_fullscreen(unsafe {
				&C.xdg_toplevel(record.xdg_toplevel)
			}, unsafe { nil })
		} else {
			C.v_multiwindow_wayland_xdg_toplevel_unset_fullscreen(unsafe {
				&C.xdg_toplevel(record.xdg_toplevel)
			})
		}
		backend.finish_prepared_service_request(flush_attempt)!
		record.requested_fullscreen = enabled
		return backend.service_window_state(id)!
	} $else {
		_ = id
		_ = enabled
		return error(err_backend_unsupported)
	}
}

fn (backend &WaylandBackend) can_release_mouse_lock_after_capability_loss(id WindowId) bool {
	index := backend.window_record_index(id) or { return false }
	record := backend.windows[index]
	has_lock := record.mouse_lock_requested || record.mouse_locked
		|| record.locked_pointer != unsafe { nil } || record.relative_pointer != unsafe { nil }
	return has_lock
}

fn (mut backend WaylandBackend) service_set_mouse_lock(id WindowId, enabled bool) !ServiceWindowState {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if !enabled {
			has_native_lock := backend.windows[index].locked_pointer != unsafe { nil }
				|| backend.windows[index].relative_pointer != unsafe { nil }
			if !has_native_lock || !backend.transport_can_marshal() {
				backend.release_window_mouse_lock(index, true, true)
				return backend.service_window_state(id)!
			}
			mut transport := backend.prepare_cleanup_transport_plan(wayland_window_operation_seed(id,
				backend.windows[index].render_target_generation, .display_transport), [
				.display_flush,
			])!
			flush_attempt := transport.take()
			backend.release_window_mouse_lock(index, true, false)
			backend.finish_prepared_service_request(flush_attempt) or {
				return backend.service_window_state(id)!
			}
			return backend.service_window_state(id)!
		}
		if backend.service_operation_capability(.mouse_lock).support == .unsupported
			|| backend.windows[index].surface == unsafe { nil } {
			return error(err_capability_unsupported)
		}
		if backend.windows[index].mouse_lock_requested {
			return backend.service_window_state(id)!
		}
		if !backend.service_transport_usable(1) {
			return error(err_capability_unsupported)
		}
		mut transport := backend.prepare_service_transport_plan(wayland_window_operation_seed(id,
			backend.windows[index].render_target_generation, .display_transport), [
			.display_flush,
		])!
		flush_attempt := transport.take()
		relative_pointer := C.v_multiwindow_wayland_get_relative_pointer(unsafe {
			&C.zwp_relative_pointer_manager_v1(backend.relative_pointer_manager)
		}, unsafe { &C.wl_pointer(backend.pointer) })
		if relative_pointer == unsafe { nil } || C.v_multiwindow_wayland_add_relative_pointer_listener(unsafe {
			&C.zwp_relative_pointer_v1(relative_pointer)
		}, backend.windows[index].listener_data()) < 0 {
			if relative_pointer != unsafe { nil } {
				C.v_multiwindow_wayland_relative_pointer_destroy(unsafe {
					&C.zwp_relative_pointer_v1(relative_pointer)
				})
			}
			backend.consume_prepared_service_cleanup(flush_attempt)
			return error(err_capability_unsupported)
		}
		locked_pointer := C.v_multiwindow_wayland_lock_pointer(unsafe {
			&C.zwp_pointer_constraints_v1(backend.pointer_constraints)
		}, unsafe { &C.wl_surface(backend.windows[index].surface) }, unsafe {
			&C.wl_pointer(backend.pointer)
		})
		if locked_pointer == unsafe { nil } || C.v_multiwindow_wayland_add_locked_pointer_listener(unsafe {
			&C.zwp_locked_pointer_v1(locked_pointer)
		}, backend.windows[index].listener_data()) < 0 {
			if locked_pointer != unsafe { nil } {
				C.v_multiwindow_wayland_locked_pointer_destroy(unsafe {
					&C.zwp_locked_pointer_v1(locked_pointer)
				})
			}
			C.v_multiwindow_wayland_relative_pointer_destroy(unsafe {
				&C.zwp_relative_pointer_v1(relative_pointer)
			})
			backend.consume_prepared_service_cleanup(flush_attempt)
			return error(err_capability_unsupported)
		}
		backend.windows[index].relative_pointer = relative_pointer
		backend.windows[index].locked_pointer = locked_pointer
		backend.windows[index].mouse_lock_requested = true
		backend.finish_prepared_service_request(flush_attempt) or {
			backend.release_window_mouse_lock(index, false, true)
			return err
		}
		return backend.service_window_state(id)!
	} $else {
		_ = id
		_ = enabled
		return error(err_backend_unsupported)
	}
}

fn (backend &WaylandBackend) service_native_window_borrow(id WindowId) !BackendNativeWindowBorrow {
	index := backend.window_record_index(id) or { return error(err_window_not_found) }
	record := backend.windows[index]
	if backend.display == unsafe { nil } || record.surface == unsafe { nil } {
		return error(err_capability_unsupported)
	}
	return BackendNativeWindowBorrow{
		backend:   .wayland
		primary:   backend.display
		secondary: u64(record.surface)
	}
}

fn (record &WaylandWindowRecord) service_metrics_snapshot() RenderMetricsSnapshot {
	scale := record.effective_render_scale()
	return RenderMetricsSnapshot{
		logical_width:        f32(record.width)
		logical_height:       f32(record.height)
		framebuffer_width:    record.framebuffer_width()
		framebuffer_height:   record.framebuffer_height()
		dpi_scale:            scale
		metrics_available:    record.configured
		conversion_available: record.configured
	}
}

fn (record &WaylandWindowRecord) service_window_state() ServiceWindowState {
	mut monitor_ids := []ServiceMonitorId{}
	if record.owner != unsafe { nil } {
		app_instance := record.owner.service_app_instance()
		for slot in record.output_slots {
			if slot >= 0 && slot < record.owner.outputs.len && record.owner.outputs[slot].available {
				monitor_ids << ServiceMonitorId{
					app_instance: app_instance
					slot:         slot
					generation:   record.owner.outputs[slot].generation
				}
			}
		}
	}
	return ServiceWindowState{
		mapping:                     if record.mapped { .mapped } else { .unmapped }
		visibility:                  if record.mapped { .visible } else { .hidden }
		active:                      if record.observed_service_state_valid {
			if record.observed_active { ServiceObservedBool.on } else { ServiceObservedBool.off }
		} else {
			ServiceObservedBool.unknown
		}
		focused:                     if record.observed_service_state_valid {
			if record.observed_active { ServiceObservedBool.on } else { ServiceObservedBool.off }
		} else {
			ServiceObservedBool.unknown
		}
		minimized:                   .unknown
		maximized:                   if record.observed_service_state_valid {
			if record.observed_maximized { ServiceObservedBool.on } else { ServiceObservedBool.off }
		} else {
			ServiceObservedBool.unknown
		}
		fullscreen:                  if record.observed_service_state_valid {
			if record.observed_fullscreen { ServiceObservedBool.on } else { ServiceObservedBool.off }
		} else {
			ServiceObservedBool.unknown
		}
		mouse_locked:                if record.mouse_lock_requested {
			if record.mouse_locked { ServiceObservedBool.on } else { ServiceObservedBool.off }
		} else {
			ServiceObservedBool.off
		}
		monitor_ids:                 monitor_ids
		monitor_membership_observed: true
	}
}

fn (mut record WaylandWindowRecord) enqueue_service_state(operation ServiceOperation) {
	$if linux && sokol_wayland ? {
		record.enqueue_native_event(C.v_multiwindow_wayland_next_event_sequence(), queued_service_event(ServiceEvent{
			kind:      .state
			window:    record.id
			state:     record.service_window_state()
			metrics:   record.service_metrics_snapshot()
			operation: operation
		}))
	} $else {
		_ = operation
	}
}

fn (mut record WaylandWindowRecord) commit_pending_service_state() {
	if !record.pending_service_state_valid {
		return
	}
	was_observed := record.observed_service_state_valid
	old_maximized := record.observed_maximized
	old_fullscreen := record.observed_fullscreen
	old_active := record.observed_active
	record.observed_service_state_valid = true
	record.observed_maximized = record.pending_maximized
	record.observed_fullscreen = record.pending_fullscreen
	record.observed_active = record.pending_active
	record.pending_service_state_valid = false
	mut operations := []ServiceOperation{}
	if !was_observed {
		if record.observed_maximized {
			operations << .maximize
		}
		if record.observed_fullscreen {
			operations << .fullscreen
		}
	} else {
		if old_maximized != record.observed_maximized {
			operations << if record.observed_maximized {
				ServiceOperation.maximize
			} else {
				ServiceOperation.restore
			}
		}
		if old_fullscreen != record.observed_fullscreen {
			operation := if record.observed_fullscreen {
				ServiceOperation.fullscreen
			} else {
				ServiceOperation.restore
			}
			if operation !in operations {
				operations << operation
			}
		}
	}
	if !was_observed || old_active != record.observed_active {
		operations << .focus
	}
	for operation in operations {
		record.enqueue_service_state(operation)
	}
}

fn (backend &WaylandBackend) service_window_state(id WindowId) !ServiceWindowState {
	index := backend.window_record_index(id) or { return error(err_window_not_found) }
	return backend.windows[index].service_window_state()
}

fn (backend &WaylandBackend) service_monitor_snapshot(app_instance u64) ![]ServiceMonitorInfo {
	if !backend.started && backend.display == unsafe { nil } {
		return error(err_wayland_connect_failed)
	}
	mut monitors := []ServiceMonitorInfo{cap: backend.outputs.len}
	for output in backend.outputs {
		if !output.available {
			continue
		}
		mut width := output.mode_width
		mut height := output.mode_height
		if (output.transform & 1) != 0 {
			width, height = height, width
		}
		scale := if output.scale > 0 { output.scale } else { 1 }
		name := if output.name != '' {
			output.name
		} else if output.description != '' {
			output.description
		} else if output.make != '' || output.model != '' {
			'${output.make} ${output.model}'.trim_space()
		} else {
			'Wayland output ${output.slot}'
		}
		monitors << ServiceMonitorInfo{
			native_key: ServiceMonitorNativeKey{
				kind:    .wayland_global
				numeric: u64(output.global_name)
			}
			id:         ServiceMonitorId{
				app_instance: app_instance
				slot:         output.slot
				generation:   output.generation
			}
			name:       name
			geometry:   ServiceKnownRect{
				// wl_output geometry is not compositor-logical placement. Keep the
				// public rectangle unknown until xdg-output supplies that authority.
				known: false
				value: ServiceRect{
					x:      output.x
					y:      output.y
					width:  width / scale
					height: height / scale
				}
			}
			work_area:  ServiceKnownRect{}
			scale:      ServiceKnownScale{
				known: output.ready && output.scale > 0
				value: f32(scale)
			}
			primary:    .unknown
			available:  true
		}
	}
	if !service_monitor_snapshot_identity_valid(monitors, .wayland, app_instance) {
		return error(err_capability_unsupported)
	}
	return monitors
}

fn (backend &WaylandBackend) portal_export_index(lease ServicePortalLeaseId) ?int {
	for index, portal in backend.portal_exports {
		if portal.lease == lease {
			return index
		}
	}
	return none
}

fn (mut backend WaylandBackend) destroy_portal_export_at(index int, local_only bool) {
	if index < 0 || index >= backend.portal_exports.len {
		return
	}
	mut portal := backend.portal_exports[index]
	if portal.exported != unsafe { nil } {
		$if linux && sokol_wayland ? {
			if local_only || backend.destroy_proxy_locally_if_needed(portal.exported) {
				if local_only {
					backend.destroy_proxy_locally(portal.exported)
				}
			} else {
				C.v_multiwindow_wayland_exported_destroy(unsafe {
					&C.zxdg_exported_v2(portal.exported)
				})
			}
		}
		portal.exported = unsafe { nil }
	}
	portal.owner = unsafe { nil }
	backend.portal_exports.delete(index)
}

fn (mut backend WaylandBackend) destroy_portal_exports_for_window(id WindowId) {
	mut index := backend.portal_exports.len
	for index > 0 {
		index--
		if backend.portal_exports[index].window == id {
			backend.destroy_portal_export_at(index, !backend.transport_can_marshal())
		}
	}
}

fn (mut backend WaylandBackend) destroy_all_portal_exports() {
	mut index := backend.portal_exports.len
	for index > 0 {
		index--
		backend.destroy_portal_export_at(index, !backend.transport_can_marshal())
	}
}

fn (mut backend WaylandBackend) destroy_foreign_exporter() {
	$if linux && sokol_wayland ? {
		backend.destroy_all_portal_exports()
		if backend.foreign_exporter != unsafe { nil } {
			if !backend.destroy_proxy_locally_if_needed(backend.foreign_exporter) {
				C.v_multiwindow_wayland_exporter_destroy(unsafe {
					&C.zxdg_exporter_v2(backend.foreign_exporter)
				})
			}
		}
	}
	backend.foreign_exporter = unsafe { nil }
	backend.foreign_exporter_name = 0
}

fn (mut backend WaylandBackend) service_start_portal_parent(id WindowId, request ServiceRequestId, lease ServicePortalLeaseId) !BackendPortalStart {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		record := backend.windows[index]
		if !backend.service_transport_usable(1) || backend.foreign_exporter == unsafe { nil }
			|| backend.foreign_exporter_name == 0 || record.surface == unsafe { nil } {
			return error(err_capability_unsupported)
		}
		mut transport := backend.prepare_service_transport_plan(wayland_window_operation_seed(id,
			record.render_target_generation, .display_transport), [.display_flush])!
		flush_attempt := transport.take()
		exported := C.v_multiwindow_wayland_export_toplevel(unsafe {
			&C.zxdg_exporter_v2(backend.foreign_exporter)
		}, unsafe { &C.wl_surface(record.surface) })
		if exported == unsafe { nil } {
			backend.consume_prepared_service_cleanup(flush_attempt)
			return error(err_capability_unsupported)
		}
		mut portal := &WaylandPortalExport{
			request:  request
			window:   id
			lease:    lease
			owner:    backend
			exported: exported
		}
		if C.v_multiwindow_wayland_add_exported_listener(unsafe {
			&C.zxdg_exported_v2(exported)
		}, portal.listener_data()) != 0 {
			C.v_multiwindow_wayland_exported_destroy(unsafe { &C.zxdg_exported_v2(exported) })
			portal.exported = unsafe { nil }
			portal.owner = unsafe { nil }
			backend.consume_prepared_service_cleanup(flush_attempt)
			return error(err_capability_unsupported)
		}
		backend.portal_exports << portal
		backend.finish_prepared_service_request(flush_attempt) or {
			portal_index := backend.portal_export_index(lease) or { return err }
			backend.destroy_portal_export_at(portal_index, !backend.transport_can_marshal())
			return err
		}
		return BackendPortalStart{}
	} $else {
		_ = id
		_ = request
		_ = lease
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) service_release_portal_parent(lease ServicePortalLeaseId) ! {
	index := backend.portal_export_index(lease) or { return error(err_service_request_stale) }
	if backend.portal_exports[index].exported == unsafe { nil } {
		backend.destroy_portal_export_at(index, true)
		return
	}
	if !backend.transport_can_marshal() {
		backend.destroy_portal_export_at(index, true)
		return
	}
	portal := backend.portal_exports[index]
	window_index := backend.window_record_index(portal.window) or {
		return error(err_service_request_stale)
	}
	mut transport := backend.prepare_cleanup_transport_plan(wayland_window_operation_seed(portal.window,
		backend.windows[window_index].render_target_generation, .display_transport), [
		.display_flush,
	])!
	flush_attempt := transport.take()
	backend.destroy_portal_export_at(index, false)
	backend.finish_prepared_service_request(flush_attempt) or { return }
}

fn (mut backend WaylandBackend) service_set_clipboard_text(id WindowId, request ServiceRequestId, text string) !BackendClipboardStart {
	$if linux && sokol_wayland ? {
		_ = request
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if text.len > wayland_clipboard_max_bytes {
			return error(err_clipboard_capacity)
		}
		mut bypass_protocol := false
		mut exercise_transport_plan := true
		$if test {
			bypass_protocol = backend.clipboard_source_test.active
				&& backend.clipboard_source_test.bypass_protocol
			exercise_transport_plan = !bypass_protocol
				|| backend.clipboard_source_test.exercise_transport_plan
		}
		if !backend.started || !backend.transport_can_marshal()
			|| (exercise_transport_plan && !backend.service_transport_usable(1))
			|| backend.data_device_manager == unsafe { nil }
			|| backend.data_device == unsafe { nil }
			|| backend.seat == unsafe { nil } {
			return error(err_capability_unsupported)
		}
		if !backend.windows[index].user_action_serial_available(backend.poll_generation) {
			return error(err_capability_unsupported)
		}
		mut transport := WaylandServiceTransportPlan{}
		mut flush_attempt := WaylandPreparedTransportAttempt{}
		if exercise_transport_plan {
			transport = backend.prepare_service_transport_plan(wayland_window_operation_seed(id,
				backend.windows[index].render_target_generation, .display_transport), [
				.display_flush,
			])!
			flush_attempt = transport.take()
		}
		source := backend.create_clipboard_source()
		if source == unsafe { nil } {
			if exercise_transport_plan {
				backend.consume_prepared_service_cleanup(flush_attempt)
			}
			return error(err_capability_unsupported)
		}
		if backend.add_clipboard_source_listener(source) < 0 {
			backend.destroy_clipboard_source_handle(source, false)
			if exercise_transport_plan {
				backend.consume_prepared_service_cleanup(flush_attempt)
			}
			return error(err_capability_unsupported)
		}
		serial := backend.windows[index].take_user_action_serial(backend.poll_generation) or {
			backend.destroy_clipboard_source_handle(source, false)
			if exercise_transport_plan {
				backend.consume_prepared_service_cleanup(flush_attempt)
			}
			return error(err_capability_unsupported)
		}
		if !bypass_protocol {
			C.v_multiwindow_wayland_data_source_offer(unsafe { &C.wl_data_source(source) },
				c'text/plain;charset=utf-8')
			C.v_multiwindow_wayland_data_source_offer(unsafe { &C.wl_data_source(source) },
				c'text/plain')
		}
		if !bypass_protocol {
			C.v_multiwindow_wayland_data_device_set_selection(unsafe {
				&C.wl_data_device(backend.data_device)
			}, unsafe { &C.wl_data_source(source) }, serial)
		}
		$if test {
			if bypass_protocol && backend.clipboard_source_test.active {
				backend.clipboard_source_test.selection_requests++
			}
		}
		mut flush_succeeded := true
		mut flush_error := ''
		if exercise_transport_plan {
			$if test {
				if backend.clipboard_source_test.active
					&& backend.clipboard_source_test.fail_flush_after_selection {
					flush_succeeded = false
					flush_error = err_wayland_flush_failed
					backend.render_health = renderer_health_latch_unavailable(backend.render_health)
					backend.wayland_display_unavailable = true
				} else {
					backend.finish_prepared_service_request(flush_attempt) or {
						flush_succeeded = false
						flush_error = err.msg()
					}
				}
			} $else {
				backend.finish_prepared_service_request(flush_attempt) or {
					flush_succeeded = false
					flush_error = err.msg()
				}
			}
		}
		if !flush_succeeded {
			backend.destroy_clipboard_source_handle(source, true)
			return BackendClipboardStart{
				completed: true
				status:    .failed
				error:     if flush_error == '' { err_wayland_flush_failed } else { flush_error }
			}
		}
		old_source := backend.clipboard_source
		backend.clipboard_source = source
		backend.clipboard_text = text.clone()
		backend.destroy_clipboard_source_handle(old_source, false)
		return BackendClipboardStart{
			completed: true
			text:      text.clone()
		}
	} $else {
		_ = id
		_ = request
		_ = text
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) service_request_clipboard_text(id WindowId, request ServiceRequestId) !BackendClipboardStart {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.service_transport_usable(1)
			|| backend.data_device == unsafe { nil }
			|| backend.selection_offer == unsafe { nil }
			|| (!backend.selection_offer_has_utf8 && !backend.selection_offer_has_text) {
			return error(err_capability_unsupported)
		}
		if backend.clipboard_read_active {
			return error(err_clipboard_capacity)
		}
		mut fds := [-1, -1]!
		if C.pipe(&fds[0]) == -1 {
			return error(err_capability_unsupported)
		}
		if C.v_multiwindow_wayland_fd_set_nonblocking(fds[0]) == 0 {
			C.close(fds[0])
			C.close(fds[1])
			return error(err_capability_unsupported)
		}
		mut transport := backend.prepare_service_transport_plan(wayland_window_operation_seed(id,
			backend.windows[index].render_target_generation, .display_transport), [
			.display_flush,
		]) or {
			C.close(fds[0])
			C.close(fds[1])
			return err
		}
		flush_attempt := transport.take()
		mime_type := if backend.selection_offer_has_utf8 {
			c'text/plain;charset=utf-8'
		} else {
			c'text/plain'
		}
		C.v_multiwindow_wayland_data_offer_receive(unsafe {
			&C.wl_data_offer(backend.selection_offer)
		}, mime_type, fds[1])
		C.close(fds[1])
		backend.clipboard_read = WaylandClipboardRead{
			request:     request
			window:      id
			fd:          fds[0]
			buffer:      []u8{cap: wayland_clipboard_io_chunk_size}
			deadline_ns: vtime.sys_mono_now() + wayland_clipboard_timeout_ns
		}
		backend.clipboard_read_active = true
		backend.finish_prepared_service_request(flush_attempt) or {
			backend.cancel_clipboard_read()
			return err
		}
		return BackendClipboardStart{}
	} $else {
		_ = id
		_ = request
		return error(err_backend_unsupported)
	}
}

fn (backend &WaylandBackend) service_window_readback(id WindowId, x int, y int, width int, height int) ![]u8 {
	_ = backend.window_record_index(id) or { return error(err_window_not_found) }
	_ = x
	_ = y
	_ = width
	_ = height
	return error(err_capability_unsupported)
}

fn (backend &WaylandBackend) service_window_record(id WindowId) !&WaylandWindowRecord {
	index := backend.window_record_index(id) or { return error(err_window_not_found) }
	record := backend.windows[index]
	if !backend.started || !backend.transport_can_marshal() || record.xdg_toplevel == unsafe { nil } {
		return error(err_render_native_renderer_unavailable)
	}
	return record
}

fn (mut backend WaylandBackend) execute_prepared_service_transport(attempt WaylandPreparedTransportAttempt) !NativeRenderResult {
	result := backend.execute_prepared_wayland_transport(attempt)
	if !result.succeeded() {
		backend.render_health = renderer_health_latch_unavailable(backend.render_health)
		backend.wayland_display_unavailable = true
		message := if attempt.context.operation == .display_roundtrip {
			err_wayland_dispatch_failed
		} else {
			err_wayland_flush_failed
		}
		return error(message)
	}
	return result
}

fn (mut backend WaylandBackend) finish_prepared_service_request(attempt WaylandPreparedTransportAttempt) ! {
	$if test {
		if backend.toplevel_replay_test.interactive_request_override
			&& attempt.context.operation == .display_flush {
			backend.toplevel_replay_test.interactive_transport_finishes++
			return
		}
	}
	_ = backend.execute_prepared_service_transport(attempt)!
}

fn (backend &WaylandBackend) interactive_request_overridden_for_test(resize bool) bool {
	$if test {
		_ = resize
		return backend.toplevel_replay_test.interactive_request_override
	} $else {
		_ = resize
		return false
	}
}

fn (mut backend WaylandBackend) consume_prepared_service_cleanup(attempt WaylandPreparedTransportAttempt) {
	backend.finish_prepared_service_request(attempt) or {}
}

fn (backend &WaylandBackend) window_record_index(id WindowId) ?int {
	for i, record in backend.windows {
		if record.id == id {
			return i
		}
	}
	return none
}
