module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if linux && x_multiwindow_x11 ? {
	import time as vtime
}

$if linux && x_multiwindow_x11 ? {
	$if test {
		#flag linux -DV_MULTIWINDOW_NATIVE_PROOF_TEST
	}
	#flag linux -lX11-xcb
	#flag linux -lX11
	#flag linux -lxcb
	#flag linux -lXrandr
	#flag linux -lEGL
	#flag linux -lGL
	#include <X11/Xlib.h>
	#include <X11/Xatom.h>
	#include <X11/extensions/Xrandr.h>
	#insert "@VMODROOT/vlib/x/multiwindow/x11_egl_backend_helpers.h"
}

const x11_client_message = 33
const x11_configure_notify = 22
const x11_destroy_notify = 17
const x11_unmap_notify = 18
const x11_map_notify = 19
const x11_key_press = 2
const x11_key_release = 3
const x11_button_press = 4
const x11_button_release = 5
const x11_motion_notify = 6
const x11_enter_notify = 7
const x11_leave_notify = 8
const x11_focus_in = 9
const x11_focus_out = 10
const x11_property_notify = 28
const x11_selection_clear = 29
const x11_selection_request = 30
const x11_selection_notify = 31
const x11_success = 0
const x11_scroll_up = 4
const x11_scroll_down = 5
const x11_scroll_right = 6
const x11_scroll_left = 7
const x11_invalid_mouse_button = 256
const x11_property_new_value = 0
const x11_property_delete = 1
const x11_prop_mode_replace = 0
const x11_normal_state = 1
const x11_iconic_state = 3
const x11_modifier_ctrl = u32(2)
const x11_key_v = 86
const x11_xdnd_version = 5
const x11_xdnd_max_payload_bytes = 1024 * 1024
const x11_xdnd_max_payload_units = (x11_xdnd_max_payload_bytes + 3) / 4
const x11_xdnd_max_type_atoms = 64
const x11_xdnd_timeout_ns = i64(2_000_000_000)
const x11_inline_char_codes = 8
const x11_clipboard_inline_bytes = 64 * 1024
const x11_clipboard_chunk_bytes = 32 * 1024
const x11_clipboard_max_bytes = 16 * 1024 * 1024
const x11_clipboard_max_pending_operations = 16
const x11_clipboard_max_pending_bytes = 16 * 1024 * 1024
const x11_clipboard_checked_events_per_poll = 256
const x11_clipboard_checked_events_per_barrier = 4096
const x11_clipboard_timeout_ns = i64(2_000_000_000)

$if x32 {
	type X11NativeLong = int
	type X11NativeULong = u32
} $else {
	type X11NativeLong = i64
	type X11NativeULong = u64
}

type X11NativeAtom = X11NativeULong
type X11NativeColormap = X11NativeULong
type X11NativeCursor = X11NativeULong
type X11NativeWindow = X11NativeULong

struct X11ReadbackProbe {
	attributes_available   int
	map_state              int
	actual_width           int
	actual_height          int
	requested_width        int
	requested_height       int
	pixels_length          usize
	expected_pixels_length usize
}

$if linux && x_multiwindow_x11 ? {
	struct C.Display {}

	@[typedef]
	struct C.VMultiwindowX11ServiceState {
		mapped         int
		focused        int
		minimized      int
		maximized      int
		fullscreen     int
		position_known int
		x              int
		y              int
	}

	@[typedef]
	struct C.VMultiwindowX11MonitorInfo {
		name      X11NativeAtom
		primary   int
		x         int
		y         int
		width     int
		height    int
		width_mm  int
		height_mm int
	}

	@[typedef]
	struct C.VMultiwindowX11WorkArea {
		known  int
		x      int
		y      int
		width  int
		height int
	}

	@[typedef]
	struct C.VMultiwindowX11ReadbackProbe {
		attributes_available   int
		map_state              int
		actual_width           int
		actual_height          int
		requested_width        int
		requested_height       int
		pixels_length          usize
		expected_pixels_length usize
	}

	@[typedef]
	union C.XClientMessageData {
	mut:
		b [20]u8
		s [10]i16
		l [5]X11NativeLong
	}

	@[typedef]
	struct C.XClientMessageEvent {
	mut:
		@type        int
		serial       X11NativeULong
		send_event   int
		display      &C.Display = unsafe { nil }
		window       X11NativeWindow
		message_type X11NativeAtom
		format       int
		data         C.XClientMessageData
	}

	@[typedef]
	struct C.XDestroyWindowEvent {
	mut:
		@type      int
		serial     X11NativeULong
		send_event int
		display    &C.Display = unsafe { nil }
		event      X11NativeWindow
		window     X11NativeWindow
	}

	@[typedef]
	struct C.XConfigureEvent {
	mut:
		@type             int
		serial            X11NativeULong
		send_event        int
		display           &C.Display = unsafe { nil }
		event             X11NativeWindow
		window            X11NativeWindow
		x                 int
		y                 int
		width             int
		height            int
		border_width      int
		above             X11NativeWindow
		override_redirect int
	}

	@[typedef]
	struct C.XSelectionEvent {
	mut:
		@type      int
		serial     X11NativeULong
		send_event int
		display    &C.Display = unsafe { nil }
		requestor  X11NativeWindow
		selection  X11NativeAtom
		target     X11NativeAtom
		property   X11NativeAtom
		time       X11NativeULong
	}

	@[typedef]
	struct C.XSelectionRequestEvent {
	mut:
		@type      int
		serial     X11NativeULong
		send_event int
		display    &C.Display = unsafe { nil }
		owner      X11NativeWindow
		requestor  X11NativeWindow
		selection  X11NativeAtom
		target     X11NativeAtom
		property   X11NativeAtom
		time       X11NativeULong
	}

	@[typedef]
	struct C.XSelectionClearEvent {
	mut:
		@type      int
		serial     X11NativeULong
		send_event int
		display    &C.Display = unsafe { nil }
		window     X11NativeWindow
		selection  X11NativeAtom
		time       X11NativeULong
	}

	@[typedef]
	struct C.XPropertyEvent {
	mut:
		@type      int
		serial     X11NativeULong
		send_event int
		display    &C.Display = unsafe { nil }
		window     X11NativeWindow
		atom       X11NativeAtom
		time       X11NativeULong
		state      int
	}

	@[typedef]
	union C.XEvent {
	mut:
		@type             int
		xclient           C.XClientMessageEvent
		xconfigure        C.XConfigureEvent
		xdestroywindow    C.XDestroyWindowEvent
		xselection        C.XSelectionEvent
		xselectionrequest C.XSelectionRequestEvent
		xselectionclear   C.XSelectionClearEvent
		xproperty         C.XPropertyEvent
		pad               [24]X11NativeLong
	}

	fn C.XInitThreads() int
	fn C.XOpenDisplay(name &char) &C.Display
	fn C.XDefaultScreen(display &C.Display) int
	fn C.XDefaultRootWindow(display &C.Display) X11NativeWindow
	fn C.XStoreName(display &C.Display, window X11NativeWindow, name &char) int
	fn C.XInternAtom(display &C.Display, name &char, only_if_exists int) X11NativeAtom
	fn C.XGetAtomName(display &C.Display, atom X11NativeAtom) &char
	fn C.XSetWMProtocols(display &C.Display, window X11NativeWindow, protocols &X11NativeAtom, count int) int
	fn C.XCreateSimpleWindow(display &C.Display, parent X11NativeWindow, x int, y int, width u32, height u32, border_width u32, border X11NativeULong, background X11NativeULong) X11NativeWindow
	fn C.XSelectInput(display &C.Display, window X11NativeWindow, event_mask X11NativeLong) int
	fn C.XMapWindow(display &C.Display, window X11NativeWindow) int
	fn C.XUnmapWindow(display &C.Display, window X11NativeWindow) int
	fn C.XRaiseWindow(display &C.Display, window X11NativeWindow) int
	fn C.XMoveWindow(display &C.Display, window X11NativeWindow, x int, y int) int
	fn C.XReparentWindow(display &C.Display, window X11NativeWindow, parent X11NativeWindow, x int, y int) int
	fn C.XIconifyWindow(display &C.Display, window X11NativeWindow, screen int) int
	fn C.XResizeWindow(display &C.Display, window X11NativeWindow, width u32, height u32) int
	fn C.XDestroyWindow(display &C.Display, window X11NativeWindow) int
	fn C.XDefineCursor(display &C.Display, window X11NativeWindow, cursor X11NativeCursor) int
	fn C.XUndefineCursor(display &C.Display, window X11NativeWindow) int
	fn C.XFreeCursor(display &C.Display, cursor X11NativeCursor) int
	fn C.XFreeColormap(display &C.Display, colormap X11NativeColormap) int
	fn C.XFlush(display &C.Display) int
	fn C.XSync(display &C.Display, discard int) int
	fn C.XPending(display &C.Display) int
	fn C.XNextEvent(display &C.Display, event &C.XEvent) int
	fn C.XPutBackEvent(display &C.Display, event &C.XEvent) int
	fn C.XGetWindowProperty(display &C.Display, window X11NativeWindow, property X11NativeAtom, long_offset X11NativeLong, long_length X11NativeLong, delete int, req_type X11NativeAtom, actual_type_return &X11NativeAtom, actual_format_return &int, nitems_return &X11NativeULong, bytes_after_return &X11NativeULong, prop_return &&u8) int
	fn C.XChangeProperty(display &C.Display, window X11NativeWindow, property X11NativeAtom, @type X11NativeAtom, format int, mode int, data &u8, nelements int) int
	fn C.XConvertSelection(display &C.Display, selection X11NativeAtom, target X11NativeAtom, property X11NativeAtom, requestor X11NativeWindow, time X11NativeULong) int
	fn C.XSetSelectionOwner(display &C.Display, selection X11NativeAtom, owner X11NativeWindow, time X11NativeULong) int
	fn C.XGetSelectionOwner(display &C.Display, selection X11NativeAtom) X11NativeWindow
	fn C.XDeleteProperty(display &C.Display, window X11NativeWindow, property X11NativeAtom) int
	fn C.XSendEvent(display &C.Display, window X11NativeWindow, propagate int, event_mask X11NativeLong, event_send &C.XEvent) int
	fn C.XTranslateCoordinates(display &C.Display, src_w X11NativeWindow, dest_w X11NativeWindow, src_x int, src_y int, dest_x_return &int, dest_y_return &int, child_return &X11NativeWindow) int
	fn C.XFilterEvent(event &C.XEvent, window X11NativeWindow) int
	fn C.XFree(data voidptr) int
	fn C.v_multiwindow_x11_event_mask() X11NativeLong
	fn C.v_multiwindow_x11_event_window(event &C.XEvent) X11NativeWindow
	fn C.v_multiwindow_x11_event_x(event &C.XEvent) int
	fn C.v_multiwindow_x11_event_y(event &C.XEvent) int
	fn C.v_multiwindow_x11_event_state(event &C.XEvent) u32
	fn C.v_multiwindow_x11_event_keycode(event &C.XEvent) u32
	fn C.v_multiwindow_x11_event_button(event &C.XEvent) u32
	fn C.v_multiwindow_x11_property_atom(event &C.XEvent) X11NativeAtom
	fn C.v_multiwindow_x11_property_state(event &C.XEvent) int
	fn C.v_multiwindow_x11_focus_mode(event &C.XEvent) int
	fn C.v_multiwindow_x11_is_notify_grab_or_ungrab(mode int) int
	fn C.v_multiwindow_x11_enable_detectable_auto_repeat(display &C.Display) int
	fn C.v_multiwindow_x11_is_auto_repeat_release(display &C.Display, event &C.XEvent) int
	fn C.v_multiwindow_x11_modifiers(state u32) int
	fn C.v_multiwindow_x11_key_modifier_bit(key_code int) int
	fn C.v_multiwindow_x11_mouse_button(button u32) int
	fn C.v_multiwindow_x11_button_modifier_bit(mouse_button int) int
	fn C.v_multiwindow_x11_open_im(display &C.Display) voidptr
	fn C.v_multiwindow_x11_close_im(im voidptr)
	fn C.v_multiwindow_x11_close_display(display &C.Display) int
	fn C.v_multiwindow_x11_create_ic(im voidptr, window X11NativeWindow) voidptr
	fn C.v_multiwindow_x11_destroy_ic(ic voidptr)
	fn C.v_multiwindow_x11_set_ic_focus(ic voidptr)
	fn C.v_multiwindow_x11_unset_ic_focus(ic voidptr)
	fn C.v_multiwindow_x11_init_keycodes(display &C.Display, keycodes &i32, keycodes_len int)
	fn C.v_multiwindow_x11_key_code(event &C.XEvent, keycodes &i32, keycodes_len int) int
	fn C.v_multiwindow_x11_char_codes(ic voidptr, event &C.XEvent, codes &u32, codes_len int, required_codes &int) int
	fn C.v_multiwindow_x11_create_cursor_for_shape(display &C.Display, shape int) X11NativeCursor
	fn C.v_multiwindow_x11_apply_config_hints(display &C.Display, window X11NativeWindow, width int, height int, min_width int, min_height int, resizable int, borderless int, fullscreen int) int
	fn C.v_multiwindow_x11_apply_owner_modal(display &C.Display, window X11NativeWindow, owner X11NativeWindow, modal int) int
	fn C.v_multiwindow_x11_open_checked_connection(display &C.Display) voidptr
	fn C.v_multiwindow_x11_shared_connection_usable(display &C.Display) int
	fn C.v_multiwindow_x11_close_checked_connection(connection voidptr)
	fn C.v_multiwindow_x11_checked_connection_usable(connection voidptr) int
	fn C.v_multiwindow_x11_checked_barrier(connection voidptr) int
	fn C.v_multiwindow_x11_checked_map_window(connection voidptr, window X11NativeWindow, mapped int) int
	fn C.v_multiwindow_x11_checked_raise_window(connection voidptr, window X11NativeWindow) int
	fn C.v_multiwindow_x11_checked_move_window(connection voidptr, window X11NativeWindow, x int, y int) int
	fn C.v_multiwindow_x11_checked_change_property(connection voidptr, window X11NativeWindow, property X11NativeAtom, property_type X11NativeAtom, format u8, item_count u32, data voidptr) int
	fn C.v_multiwindow_x11_checked_selection_notify(connection voidptr, requestor X11NativeWindow, selection X11NativeAtom, target X11NativeAtom, property X11NativeAtom, time X11NativeULong) int
	fn C.v_multiwindow_x11_checked_property_changes(connection voidptr, window X11NativeWindow, enabled int) int
	fn C.v_multiwindow_x11_checked_requestor_events_enabled(connection voidptr, window X11NativeWindow) int
	fn C.v_multiwindow_x11_poll_checked_property_delete(connection voidptr, out_window &X11NativeWindow, out_property &X11NativeAtom) int
	fn C.v_multiwindow_x11_checked_wm_state(connection voidptr, window X11NativeWindow, wm_state X11NativeAtom, out_state &int) int
	fn C.v_multiwindow_x11_checked_property_has_atom(connection voidptr, window X11NativeWindow, property X11NativeAtom, expected X11NativeAtom, max_atoms u32) int
	fn C.v_multiwindow_x11_query_service_state(connection voidptr, root X11NativeWindow, window X11NativeWindow, wm_state X11NativeAtom, net_state X11NativeAtom, max_h X11NativeAtom, max_v X11NativeAtom, fullscreen X11NativeAtom, out &C.VMultiwindowX11ServiceState) int
	fn C.v_multiwindow_x11_root_supports_atom(display &C.Display, root X11NativeWindow, atom X11NativeAtom) int
	fn C.v_multiwindow_x11_property_has_atom(display &C.Display, window X11NativeWindow, property X11NativeAtom, expected X11NativeAtom) int
	fn C.v_multiwindow_x11_send_net_wm_state(connection voidptr, root X11NativeWindow, window X11NativeWindow, state X11NativeAtom, action int, first X11NativeAtom, second X11NativeAtom) int
	fn C.v_multiwindow_x11_request_focus(connection voidptr, root X11NativeWindow, window X11NativeWindow, active X11NativeAtom) int
	fn C.v_multiwindow_x11_send_selection_notify(display &C.Display, requestor X11NativeWindow, selection X11NativeAtom, target X11NativeAtom, property X11NativeAtom, time X11NativeULong) int
	fn C.v_multiwindow_x11_send_event_checked(display &C.Display, window X11NativeWindow, event &C.XEvent) int
	fn C.v_multiwindow_x11_select_property_changes(display &C.Display, window X11NativeWindow) int
	fn C.v_multiwindow_x11_has_property_changes(display &C.Display, window X11NativeWindow) int
	fn C.v_multiwindow_x11_create_clipboard_requestor(display &C.Display, root X11NativeWindow) X11NativeWindow
	fn C.v_multiwindow_x11_release_mouse_lock(display &C.Display)
	fn C.v_multiwindow_x11_acquire_mouse_lock_checked(display &C.Display, window X11NativeWindow, center_x &int, center_y &int) int
	fn C.v_multiwindow_x11_center_pointer_checked(display &C.Display, window X11NativeWindow, center_x &int, center_y &int) int
	fn C.v_multiwindow_x11_set_selection_owner_checked(display &C.Display, selection X11NativeAtom, owner X11NativeWindow, time X11NativeULong) int

	$if test {
		fn C.v_multiwindow_x11_destroy_mouse_lock_target_after_grab_once_for_test()
		fn C.v_multiwindow_x11_send_focus_out_for_test(display &C.Display, window X11NativeWindow) int
		fn C.v_multiwindow_x11_warp_pointer_offset_for_test(display &C.Display, window X11NativeWindow, center_x int, center_y int, dx int, dy int) int
		fn C.v_multiwindow_x11_pointer_position_for_test(display &C.Display, window X11NativeWindow, x &int, y &int) int
		fn C.v_multiwindow_x11_queue_wm_state_then_destroy_for_test(display &C.Display, window X11NativeWindow, wm_state X11NativeAtom) int
	}
	fn C.v_multiwindow_x11_screen_width(display &C.Display, screen int) int
	fn C.v_multiwindow_x11_screen_height(display &C.Display, screen int) int
	fn C.v_multiwindow_x11_monitor_snapshot(display &C.Display, root X11NativeWindow, out &C.VMultiwindowX11MonitorInfo, capacity int) int
	fn C.v_multiwindow_x11_work_area(display &C.Display, root X11NativeWindow, current_desktop X11NativeAtom, workarea X11NativeAtom) C.VMultiwindowX11WorkArea
	fn C.v_multiwindow_x11_subscribe_randr(display &C.Display, root X11NativeWindow, event_base &int, error_base &int) int
	fn C.v_multiwindow_x11_is_randr_event(event_type int, event_base int) int
	fn C.v_multiwindow_x11_update_randr_configuration(event &C.XEvent, event_base int)
	fn C.v_multiwindow_x11_readback_rgba8(display &C.Display, connection voidptr, window X11NativeWindow, x int, y int, width int, height int, pixels &u8, pixels_len usize) int
	fn C.v_multiwindow_x11_readback_probe(connection voidptr, window X11NativeWindow, width int, height int, pixels_len usize) C.VMultiwindowX11ReadbackProbe
	fn C.v_multiwindow_x11_paint_rgba8_test_pattern(display &C.Display, window X11NativeWindow, x int, y int) int
	fn C.v_multiwindow_x11_owner_modal_matches(display &C.Display, window X11NativeWindow, owner X11NativeWindow, modal int) int
	fn C.v_multiwindow_x11_get_window_size(connection voidptr, window X11NativeWindow, out_width &int, out_height &int) int
	fn C.v_multiwindow_x11_create_egl_window(display &C.Display, root X11NativeWindow, screen int, native_visual_id int, width int, height int, out_colormap &X11NativeColormap) X11NativeWindow
}

struct X11WindowRecord {
	id WindowId
mut:
	window                   X11NativeWindow
	colormap                 X11NativeColormap
	xic                      voidptr
	egl_surface              voidptr
	egl_surface_ticket       u64
	cursor                   X11NativeCursor
	config                   WindowConfig
	cursor_shape             CursorShape
	width                    int
	height                   int
	mouse_x                  f32
	mouse_y                  f32
	mouse_dx                 f32
	mouse_dy                 f32
	mouse_pos_valid          bool
	mouse_buttons            u8
	key_repeat               [256]bool
	window_state             int
	service_state_observed   bool
	observed_minimized       bool
	observed_maximized       bool
	observed_fullscreen      bool
	observed_position_known  bool
	observed_position_x      int
	observed_position_y      int
	native_destroyed         bool
	mouse_locked             bool
	mouse_lock_center_x      int
	mouse_lock_center_y      int
	render_target_generation u64 = 1
}

struct X11ClipboardRead {
	request  ServiceRequestId
	window   WindowId
	property X11NativeAtom
mut:
	requestor      X11NativeWindow
	incremental    bool
	data           []u8
	reserved_bytes int
	deadline_ns    i64
}

struct X11ClipboardRetainedCharge {
	request ServiceRequestId
	window  WindowId
	bytes   int
mut:
	claimed_by_app bool
}

enum X11ClipboardTransferQueue {
	xlib
	checked
}

struct X11ClipboardTransfer {
	requestor X11NativeWindow
	property  X11NativeAtom
	target    X11NativeAtom
	data      []u8
	queue     X11ClipboardTransferQueue
mut:
	offset      int
	deadline_ns i64
}

struct X11XdndDrop {
mut:
	active      bool
	incremental bool
	source      X11NativeWindow
	requestor   X11NativeWindow
	window      WindowId
	property    X11NativeAtom
	target_type X11NativeAtom
	version     X11NativeLong
	time        X11NativeULong
	data        []u8
	deadline_ns i64
}

struct X11Backend {
mut:
	native_operations                            &NativeOperationAuthority = unsafe { nil }
	display                                      voidptr
	checked_connection                           voidptr
	screen                                       int
	root                                         X11NativeWindow
	wm_protocols                                 X11NativeAtom
	wm_delete_window                             X11NativeAtom
	wm_state                                     X11NativeAtom
	net_wm_state                                 X11NativeAtom
	net_supported                                X11NativeAtom
	net_workarea                                 X11NativeAtom
	net_current_desktop                          X11NativeAtom
	net_active_window                            X11NativeAtom
	net_wm_state_maximized_horz                  X11NativeAtom
	net_wm_state_maximized_vert                  X11NativeAtom
	net_wm_state_fullscreen                      X11NativeAtom
	net_wm_state_modal                           X11NativeAtom
	ewmh_active_window                           bool
	ewmh_maximize                                bool
	ewmh_fullscreen                              bool
	ewmh_modal                                   bool
	root_property_subscribed                     bool
	randr_event_base                             int
	randr_error_base                             int
	randr_subscribed                             bool
	monitor_snapshot_dirty                       bool
	monitor_snapshot_failures_for_test           int
	xdnd_aware                                   X11NativeAtom
	xdnd_enter                                   X11NativeAtom
	xdnd_position                                X11NativeAtom
	xdnd_status                                  X11NativeAtom
	xdnd_action_copy                             X11NativeAtom
	xdnd_drop                                    X11NativeAtom
	xdnd_leave                                   X11NativeAtom
	xdnd_finished                                X11NativeAtom
	xdnd_selection                               X11NativeAtom
	xdnd_type_list                               X11NativeAtom
	text_uri_list                                X11NativeAtom
	clipboard                                    X11NativeAtom
	clipboard_targets                            X11NativeAtom
	clipboard_utf8                               X11NativeAtom
	clipboard_string                             X11NativeAtom
	clipboard_incr                               X11NativeAtom
	clipboard_property                           X11NativeAtom
	clipboard_owner_window                       X11NativeWindow
	clipboard_owner_id                           WindowId
	clipboard_text                               string
	clipboard_reads                              []X11ClipboardRead
	clipboard_transfers                          []X11ClipboardTransfer
	clipboard_retained                           []X11ClipboardRetainedCharge
	pending_clipboard_terminal_events            []QueuedEvent
	clipboard_requestor_create_failures_for_test int
	clipboard_write_failures_for_test            int
	readback_destroy_after_probe_for_test        bool
	xdnd_source                                  X11NativeWindow
	xdnd_target                                  X11NativeWindow
	xdnd_format                                  X11NativeAtom
	xdnd_version                                 X11NativeLong
	xdnd_drop_state                              X11XdndDrop
	xdnd_last_requestor                          X11NativeWindow
	xdnd_last_property                           X11NativeAtom
	xdnd_last_time                               X11NativeULong
	xdnd_finished_count                          int
	xdnd_wire_finished_count                     int
	xdnd_property_delete_count                   int
	xdnd_terminal_order_sequence                 u64
	xdnd_last_finished_sequence                  u64
	xdnd_last_property_sequence                  u64
	xdnd_last_finished_accepted                  bool
	xim                                          voidptr
	egl_display                                  voidptr
	egl_config                                   voidptr
	egl_context                                  voidptr
	egl_context_ticket                           u64
	anchor_surface                               voidptr
	anchor_surface_ticket                        u64
	egl_display_ticket                           u64
	egl_thread_ticket                            u64
	anchor_generation                            u64 = 1
	egl_binding                                  EglBindingIdentity
	egl_bad_current_recovery_used                bool
	render_sequence                              u64
	render_health                                NativeRendererHealth
	native_visual_id                             int
	started                                      bool
	pending_window                               X11WindowRecord
	windows                                      []X11WindowRecord
	keycodes                                     [256]i32
}

fn new_x11_backend() X11Backend {
	return X11Backend{}
}

fn (backend &X11Backend) cursor_support(shape CursorShape) ServiceSupportLevel {
	_ = backend
	return match shape {
		.move, .grab, .grabbing, .not_allowed, .resize_all { .conditional }
		else { .available }
	}
}

fn (record &X11WindowRecord) retains_native_ownership() bool {
	return record.window != X11NativeWindow(0) || record.colormap != X11NativeColormap(0)
		|| record.xic != unsafe { nil } || record.cursor != X11NativeCursor(0)
		|| record.egl_surface != unsafe { nil } || record.egl_surface_ticket != 0
}

fn (backend &X11Backend) retains_native_ownership_except_display() bool {
	mut live_tickets := false
	if backend.native_operations != unsafe { nil } {
		live_tickets = backend.native_operations.has_live_lifetime_tickets()
	}
	return backend.xim != unsafe { nil } || backend.windows.len != 0
		|| backend.pending_window.retains_native_ownership()
		|| backend.egl_display != unsafe { nil } || backend.egl_config != unsafe { nil }
		|| backend.egl_context != unsafe { nil } || backend.anchor_surface != unsafe { nil }
		|| backend.egl_binding.surface != unsafe { nil } || backend.egl_context_ticket != 0
		|| backend.anchor_surface_ticket != 0 || backend.egl_display_ticket != 0
		|| backend.egl_thread_ticket != 0 || live_tickets
}

fn (backend &X11Backend) retains_native_ownership() bool {
	return backend.display != unsafe { nil } || backend.checked_connection != unsafe { nil }
		|| backend.retains_native_ownership_except_display()
}

fn (backend &X11Backend) retains_egl_ownership() bool {
	if backend.native_operations != unsafe { nil }
		&& backend.native_operations.has_live_lifetime_tickets() {
		return true
	}
	if backend.egl_display != unsafe { nil } || backend.egl_config != unsafe { nil }
		|| backend.egl_context != unsafe { nil } || backend.anchor_surface != unsafe { nil }
		|| backend.egl_context_ticket != 0 || backend.anchor_surface_ticket != 0
		|| backend.egl_display_ticket != 0 || backend.egl_thread_ticket != 0 {
		return true
	}
	if backend.pending_window.egl_surface != unsafe { nil }
		|| backend.pending_window.egl_surface_ticket != 0 {
		return true
	}
	for record in backend.windows {
		if record.egl_surface != unsafe { nil } || record.egl_surface_ticket != 0 {
			return true
		}
	}
	return false
}

fn (backend &X11Backend) ensure_supported() ! {
	$if linux && x_multiwindow_x11 ? {
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (backend &X11Backend) capabilities() Capabilities {
	return Capabilities{
		backend:            .x11
		mock:               false
		native:             true
		multi_window:       true
		owner_queue:        true
		explicit_swapchain: backend.renderer_ready()
		readback:           true
		x11:                true
		gl:                 backend.renderer_ready()
		input_events:       true
		mouse_events:       true
		keyboard_events:    true
		text_events:        true
		focus_events:       true
		drop_events:        true
		touch_events:       false
		cursor_shapes:      true
		native_decorations: true
	}
}

fn (backend &X11Backend) start_attempt_closed() bool {
	return !backend.started && !backend.retains_native_ownership()
}

fn (mut backend X11Backend) close_start_attempt() string {
	mut close_error := ''
	backend.stop() or { close_error = err.msg() }
	if !backend.start_attempt_closed() {
		close_error = merge_backend_errors(close_error, err_render_native_renderer_unavailable)
	}
	return close_error
}

fn (mut backend X11Backend) probe_renderer_capabilities() !Capabilities {
	$if linux && x_multiwindow_x11 ? {
		if !backend.start_attempt_closed() {
			close_error := backend.close_start_attempt()
			if close_error != '' {
				return error(close_error)
			}
		}
		if backend.render_health.blocks_graphics() || backend.native_operations == unsafe { nil } {
			return error(err_render_native_renderer_unavailable)
		}
		if C.XInitThreads() == 0 {
			return error(err_x11_open_display_failed)
		}
		display := C.XOpenDisplay(unsafe { nil })
		if display == unsafe { nil } {
			return error(err_x11_open_display_failed)
		}
		backend.display = display
		backend.checked_connection = C.v_multiwindow_x11_open_checked_connection(display)
		if backend.checked_connection == unsafe { nil } {
			C.v_multiwindow_x11_close_display(display)
			backend.display = unsafe { nil }
			return error(err_x11_open_display_failed)
		}
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

fn (mut backend X11Backend) start(require_renderer bool) ! {
	$if linux && x_multiwindow_x11 ? {
		if backend.started {
			return
		}
		if backend.retains_native_ownership() {
			mut cleanup_error := ''
			backend.stop() or { cleanup_error = err.msg() }
			if backend.retains_native_ownership() {
				return error(merge_backend_errors(cleanup_error,
					err_render_native_renderer_unavailable))
			}
			if cleanup_error != '' {
				return error(cleanup_error)
			}
		}
		if backend.render_health.blocks_graphics() {
			return error(err_render_native_renderer_unavailable)
		}
		if C.XInitThreads() == 0 {
			return error(err_x11_open_display_failed)
		}
		display := C.XOpenDisplay(unsafe { nil })
		if display == unsafe { nil } {
			return error(err_x11_open_display_failed)
		}
		backend.display = display
		backend.checked_connection = C.v_multiwindow_x11_open_checked_connection(display)
		if backend.checked_connection == unsafe { nil } {
			C.v_multiwindow_x11_close_display(display)
			backend.display = unsafe { nil }
			return error(err_x11_open_display_failed)
		}
		backend.screen = C.XDefaultScreen(display)
		backend.root = C.XDefaultRootWindow(display)
		backend.root_property_subscribed = C.v_multiwindow_x11_select_property_changes(display,
			backend.root) != 0
		backend.randr_subscribed = C.v_multiwindow_x11_subscribe_randr(display, backend.root,
			&backend.randr_event_base, &backend.randr_error_base) != 0
		backend.wm_protocols = C.XInternAtom(display, c'WM_PROTOCOLS', 0)
		backend.wm_delete_window = C.XInternAtom(display, c'WM_DELETE_WINDOW', 0)
		backend.wm_state = C.XInternAtom(display, c'WM_STATE', 0)
		backend.net_wm_state = C.XInternAtom(display, c'_NET_WM_STATE', 0)
		backend.net_supported = C.XInternAtom(display, c'_NET_SUPPORTED', 0)
		backend.net_workarea = C.XInternAtom(display, c'_NET_WORKAREA', 0)
		backend.net_current_desktop = C.XInternAtom(display, c'_NET_CURRENT_DESKTOP', 0)
		backend.net_active_window = C.XInternAtom(display, c'_NET_ACTIVE_WINDOW', 0)
		backend.net_wm_state_maximized_horz = C.XInternAtom(display,
			c'_NET_WM_STATE_MAXIMIZED_HORZ', 0)
		backend.net_wm_state_maximized_vert = C.XInternAtom(display,
			c'_NET_WM_STATE_MAXIMIZED_VERT', 0)
		backend.net_wm_state_fullscreen = C.XInternAtom(display, c'_NET_WM_STATE_FULLSCREEN', 0)
		backend.net_wm_state_modal = C.XInternAtom(display, c'_NET_WM_STATE_MODAL', 0)
		backend.refresh_ewmh_support()
		backend.xdnd_aware = C.XInternAtom(display, c'XdndAware', 0)
		backend.xdnd_enter = C.XInternAtom(display, c'XdndEnter', 0)
		backend.xdnd_position = C.XInternAtom(display, c'XdndPosition', 0)
		backend.xdnd_status = C.XInternAtom(display, c'XdndStatus', 0)
		backend.xdnd_action_copy = C.XInternAtom(display, c'XdndActionCopy', 0)
		backend.xdnd_drop = C.XInternAtom(display, c'XdndDrop', 0)
		backend.xdnd_leave = C.XInternAtom(display, c'XdndLeave', 0)
		backend.xdnd_finished = C.XInternAtom(display, c'XdndFinished', 0)
		backend.xdnd_selection = C.XInternAtom(display, c'XdndSelection', 0)
		backend.xdnd_type_list = C.XInternAtom(display, c'XdndTypeList', 0)
		backend.text_uri_list = C.XInternAtom(display, c'text/uri-list', 0)
		backend.clipboard = C.XInternAtom(display, c'CLIPBOARD', 0)
		backend.clipboard_targets = C.XInternAtom(display, c'TARGETS', 0)
		backend.clipboard_utf8 = C.XInternAtom(display, c'UTF8_STRING', 0)
		backend.clipboard_string = C.XInternAtom(display, c'STRING', 0)
		backend.clipboard_incr = C.XInternAtom(display, c'INCR', 0)
		backend.clipboard_property = C.XInternAtom(display, c'_V_MULTIWINDOW_CLIPBOARD', 0)
		backend.xim = C.v_multiwindow_x11_open_im(display)
		C.v_multiwindow_x11_init_keycodes(display, &backend.keycodes[0], 256)
		C.v_multiwindow_x11_enable_detectable_auto_repeat(display)
		if require_renderer {
			backend.init_renderer() or {
				start_error := err.msg()
				mut cleanup_error := ''
				backend.stop() or { cleanup_error = err.msg() }
				return error(merge_backend_errors(start_error, cleanup_error))
			}
		}
		backend.started = true
		return
	} $else {
		_ = require_renderer
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) cleanup_pending_window_once() string {
	if !backend.pending_window.retains_native_ownership() {
		backend.pending_window = X11WindowRecord{}
		return ''
	}
	mut cleanup_error := ''
	mut record := &backend.pending_window
	destroy_native := !record.native_destroyed
	backend.release_window_record_resources(mut record, destroy_native) or {
		cleanup_error = err.msg()
	}
	if !backend.pending_window.retains_native_ownership() {
		backend.pending_window = X11WindowRecord{}
	}
	return cleanup_error
}

fn (mut backend X11Backend) rollback_pending_window_creation(original_error string) string {
	cleanup_error := backend.cleanup_pending_window_once()
	return merge_backend_errors(original_error, cleanup_error)
}

fn (mut backend X11Backend) create_window(id WindowId, config WindowConfig) !WindowSize {
	$if linux && x_multiwindow_x11 ? {
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_x11_open_display_failed)
		}
		pending_cleanup_error := backend.cleanup_pending_window_once()
		if backend.pending_window.retains_native_ownership() {
			return error(merge_backend_errors(pending_cleanup_error,
				err_render_native_renderer_unavailable))
		}
		if pending_cleanup_error != '' {
			return error(pending_cleanup_error)
		}
		mut colormap := X11NativeColormap(0)
		renderer_ready := backend.renderer_ready()
		mut actual_size := window_size_for_config(config, config.width, config.height)
		window := if renderer_ready {
			C.v_multiwindow_x11_create_egl_window(backend.display, backend.root, backend.screen,
				backend.native_visual_id, actual_size.width, actual_size.height, &colormap)
		} else {
			created := C.XCreateSimpleWindow(backend.display, backend.root, 0, 0,
				u32(actual_size.width), u32(actual_size.height), 0, 0, 0)
			created
		}
		if window == X11NativeWindow(0) {
			return error(err_x11_create_window_failed)
		}
		backend.pending_window = X11WindowRecord{
			id:       id
			window:   window
			colormap: colormap
			config:   window_config_with_size(config, actual_size.width, actual_size.height)
			width:    actual_size.width
			height:   actual_size.height
		}
		if !renderer_ready {
			C.XSelectInput(backend.display, backend.pending_window.window,
				C.v_multiwindow_x11_event_mask())
		}
		C.XStoreName(backend.display, backend.pending_window.window, &char(config.title.str))
		if backend.wm_protocols != X11NativeAtom(0) && backend.wm_delete_window != X11NativeAtom(0) {
			protocols := [backend.wm_delete_window]
			if C.XSetWMProtocols(backend.display, backend.pending_window.window, &protocols[0], 1) == 0 {
				return error(backend.rollback_pending_window_creation(err_x11_set_wm_protocols_failed))
			}
		}
		if C.v_multiwindow_x11_apply_config_hints(backend.display, backend.pending_window.window,
			actual_size.width, actual_size.height, config.min_width, config.min_height,
			x11_bool_to_int(config.resizable), x11_bool_to_int(config.borderless),
			x11_bool_to_int(config.fullscreen)) == 0 {
			return error(backend.rollback_pending_window_creation(err_x11_create_window_failed))
		}
		mut owner_window := X11NativeWindow(0)
		if owner := config.owner {
			owner_index := backend.window_record_index(owner) or {
				return error(backend.rollback_pending_window_creation(err_window_not_found))
			}
			owner_window = backend.windows[owner_index].window
		}
		if C.v_multiwindow_x11_apply_owner_modal(backend.display, backend.pending_window.window,
			owner_window, x11_bool_to_int(config.modal)) == 0 {
			return error(backend.rollback_pending_window_creation(err_x11_create_window_failed))
		}
		backend.announce_xdnd_for_window(backend.pending_window.window)
		backend.pending_window.xic = C.v_multiwindow_x11_create_ic(backend.xim,
			backend.pending_window.window)
		if config.visible {
			C.XMapWindow(backend.display, backend.pending_window.window)
		}
		if C.XSync(backend.display, 0) == 0 {
			return error(backend.rollback_pending_window_creation(err_x11_create_window_failed))
		}
		mut actual_width := 0
		mut actual_height := 0
		if C.v_multiwindow_x11_get_window_size(backend.checked_connection,
			backend.pending_window.window, &actual_width, &actual_height) == 0 {
			return error(backend.rollback_pending_window_creation(err_x11_create_window_failed))
		}
		actual_size = WindowSize{
			width:  actual_width
			height: actual_height
		}
		backend.pending_window.config = window_config_with_size(config, actual_size.width,
			actual_size.height)
		backend.pending_window.width = actual_size.width
		backend.pending_window.height = actual_size.height
		backend.pending_window.window_state = backend.window_state(backend.pending_window.window) or {
			x11_normal_state
		}
		backend.windows << backend.pending_window
		backend.refresh_observed_service_state(backend.windows.len - 1) or {}
		backend.pending_window = X11WindowRecord{}
		return actual_size
	} $else {
		_ = id
		_ = config
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) destroy_window(id WindowId) ! {
	backend.finish_window_teardown(id)!
}

fn (mut backend X11Backend) finish_window_teardown(id WindowId) ! {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		backend.purge_xdnd_window(id, backend.windows[index].window)
		backend.purge_clipboard_window(id, backend.windows[index].window)
		mut record := &backend.windows[index]
		destroy_native := !record.native_destroyed
		backend.release_window_record_resources(mut record, destroy_native)!
		backend.windows.delete(index)
		return
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) set_window_title(id WindowId, title string) ! {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_x11_open_display_failed)
		}
		window := backend.windows[index].window
		if C.XStoreName(backend.display, window, &char(title.str)) == 0 {
			return error(err_x11_set_window_title_failed)
		}
		C.XFlush(backend.display)
		return
	} $else {
		_ = id
		_ = title
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) resize_window(id WindowId, width int, height int) !WindowSize {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_x11_open_display_failed)
		}
		window := backend.windows[index].window
		config := backend.windows[index].config
		if !config.resizable {
			return error(err_capability_unsupported)
		}
		requested_size := window_size_for_config(config, width, height)
		if C.XResizeWindow(backend.display, window, u32(requested_size.width),
			u32(requested_size.height)) == 0 {
			return error(err_x11_resize_window_failed)
		}
		if C.XSync(backend.display, 0) == 0 {
			return error(err_x11_resize_window_failed)
		}
		mut actual_width := 0
		mut actual_height := 0
		if C.v_multiwindow_x11_get_window_size(backend.checked_connection, window, &actual_width,
			&actual_height) == 0 {
			return error(err_x11_resize_window_failed)
		}
		actual_size := WindowSize{
			width:  actual_width
			height: actual_height
		}
		backend.windows[index].width = actual_size.width
		backend.windows[index].height = actual_size.height
		backend.windows[index].render_target_generation =
			next_backend_target_generation(backend.windows[index].render_target_generation)!
		backend.windows[index].config = window_config_with_size(backend.windows[index].config,
			actual_size.width, actual_size.height)
		return actual_size
	} $else {
		_ = id
		_ = width
		_ = height
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) set_window_cursor(id WindowId, shape CursorShape) ! {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_x11_open_display_failed)
		}
		if backend.windows[index].cursor_shape == shape {
			return
		}
		window := backend.windows[index].window
		if shape == .default {
			C.XUndefineCursor(backend.display, window)
			backend.free_cursor_for_record(index)
		} else {
			cursor := C.v_multiwindow_x11_create_cursor_for_shape(backend.display, int(shape))
			if cursor == X11NativeCursor(0) {
				return error(err_capability_unsupported)
			}
			if C.XDefineCursor(backend.display, window, cursor) == 0 {
				C.XFreeCursor(backend.display, cursor)
				return error(err_capability_unsupported)
			}
			backend.free_cursor_for_record(index)
			backend.windows[index].cursor = cursor
		}
		backend.windows[index].cursor_shape = shape
		C.XFlush(backend.display)
		return
	} $else {
		_ = id
		_ = shape
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) refresh_ewmh_support() {
	$if linux && x_multiwindow_x11 ? {
		backend.ewmh_active_window = C.v_multiwindow_x11_root_supports_atom(backend.display,
			backend.root, backend.net_active_window) != 0
		backend.ewmh_maximize =
			C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state) != 0
			&& C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state_maximized_horz) != 0
			&& C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state_maximized_vert) != 0
		backend.ewmh_fullscreen =
			C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state) != 0
			&& C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state_fullscreen) != 0
		backend.ewmh_modal =
			C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state) != 0
			&& C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state_modal) != 0
	}
}

fn (mut backend X11Backend) queued_ewmh_capability_events() []QueuedEvent {
	old_active := backend.ewmh_active_window
	old_maximize := backend.ewmh_maximize
	old_fullscreen := backend.ewmh_fullscreen
	backend.refresh_ewmh_support()
	mut events := []QueuedEvent{}
	for record in backend.windows {
		if old_active != backend.ewmh_active_window {
			events << queued_service_event(ServiceEvent{
				kind:       .capability
				window:     record.id
				operation:  .focus
				capability: backend.service_operation_capability(record.id, .focus)
			})
		}
		if old_maximize != backend.ewmh_maximize {
			events << queued_service_event(ServiceEvent{
				kind:       .capability
				window:     record.id
				operation:  .maximize
				capability: backend.service_operation_capability(record.id, .maximize)
			})
		}
		if old_fullscreen != backend.ewmh_fullscreen {
			events << queued_service_event(ServiceEvent{
				kind:       .capability
				window:     record.id
				operation:  .fullscreen
				capability: backend.service_operation_capability(record.id, .fullscreen)
			})
		}
		if old_maximize != backend.ewmh_maximize || old_fullscreen != backend.ewmh_fullscreen {
			events << queued_service_event(ServiceEvent{
				kind:       .capability
				window:     record.id
				operation:  .restore
				capability: backend.service_operation_capability(record.id, .restore)
			})
		}
	}
	return events
}

fn (backend &X11Backend) service_window_capture_available(id WindowId) bool {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return false }
		if !backend.started || backend.display == unsafe { nil }
			|| backend.windows[index].window == X11NativeWindow(0) {
			return false
		}
		probe := C.v_multiwindow_x11_readback_probe(backend.checked_connection,
			backend.windows[index].window, 1, 1, 0)
		return probe.attributes_available != 0 && probe.map_state == 2 && probe.actual_width > 0
			&& probe.actual_height > 0
	}
	_ = id
	return false
}

fn (backend &X11Backend) service_operation_capability(id WindowId, operation ServiceOperation) ServiceOperationCapability {
	return match operation {
		.show, .hide, .raise, .position, .native_borrow, .portal_parent {
			ServiceOperationCapability{
				support:          .available
				asynchronous:     operation in [.position, .portal_parent]
				state_observable: operation in [.show, .hide, .position]
			}
		}
		.window_capture {
			available := backend.service_window_capture_available(id)
			ServiceOperationCapability{
				support:      if available { .available } else { .unsupported }
				asynchronous: available
			}
		}
		.image_readback {
			ServiceOperationCapability{
				support:      if backend.renderer_ready() { .available } else { .unsupported }
				asynchronous: backend.renderer_ready()
			}
		}
		.minimize {
			ServiceOperationCapability{
				support:          .conditional
				asynchronous:     true
				state_observable: true
			}
		}
		.maximize {
			ServiceOperationCapability{
				support:          if backend.ewmh_maximize { .available } else { .unsupported }
				asynchronous:     backend.ewmh_maximize
				state_observable: backend.ewmh_maximize
			}
		}
		.fullscreen {
			ServiceOperationCapability{
				support:          if backend.ewmh_fullscreen { .available } else { .unsupported }
				asynchronous:     backend.ewmh_fullscreen
				state_observable: backend.ewmh_fullscreen
			}
		}
		.restore {
			ServiceOperationCapability{
				support:          if backend.ewmh_maximize || backend.ewmh_fullscreen {
					.available
				} else {
					.unsupported
				}
				asynchronous:     backend.ewmh_maximize || backend.ewmh_fullscreen
				state_observable: backend.ewmh_maximize || backend.ewmh_fullscreen
			}
		}
		.clipboard_read, .clipboard_write {
			ServiceOperationCapability{
				support:      .available
				asynchronous: true
			}
		}
		.focus {
			ServiceOperationCapability{
				support:          if backend.ewmh_active_window { .available } else { .unsupported }
				asynchronous:     backend.ewmh_active_window
				state_observable: backend.ewmh_active_window
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

fn (backend &X11Backend) service_window_state(id WindowId) !ServiceWindowState {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_x11_open_display_failed)
		}
		mut native := C.VMultiwindowX11ServiceState{}
		if C.v_multiwindow_x11_query_service_state(backend.checked_connection, backend.root,
			backend.windows[index].window, backend.wm_state, backend.net_wm_state,
			backend.net_wm_state_maximized_horz, backend.net_wm_state_maximized_vert,
			backend.net_wm_state_fullscreen, &native) == 0 {
			return error(err_capability_unsupported)
		}
		return ServiceWindowState{
			mapping:      if native.mapped != 0 { .mapped } else { .unmapped }
			visibility:   if native.mapped != 0 { .visible } else { .hidden }
			active:       if native.focused != 0 { .on } else { .off }
			focused:      if native.focused != 0 { .on } else { .off }
			minimized:    if native.minimized != 0 { .on } else { .off }
			maximized:    if native.maximized != 0 { .on } else { .off }
			fullscreen:   if native.fullscreen != 0 { .on } else { .off }
			mouse_locked: if backend.windows[index].mouse_locked { .on } else { .off }
			position:     ServicePosition{
				known: native.position_known != 0
				x:     native.x
				y:     native.y
			}
		}
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (backend &X11Backend) queued_service_state_event(index int, operation ServiceOperation) !QueuedEvent {
	if index < 0 || index >= backend.windows.len {
		return error(err_window_not_found)
	}
	record := backend.windows[index]
	return queued_service_event(ServiceEvent{
		kind:      .state
		window:    record.id
		state:     backend.service_window_state(record.id)!
		operation: operation
	})
}

fn (mut backend X11Backend) queued_configure_observation_events(index int, width int, height int, state ServiceWindowState, state_valid bool) []QueuedEvent {
	mut events := []QueuedEvent{}
	if index < 0 || index >= backend.windows.len {
		return events
	}
	if width > 0 && height > 0
		&& (backend.windows[index].width != width || backend.windows[index].height != height) {
		id := backend.windows[index].id
		backend.windows[index].width = width
		backend.windows[index].height = height
		backend.windows[index].render_target_generation =
			exhaust_backend_target_generation(backend.windows[index].render_target_generation)
		record := backend.windows[index]
		events << queued_lifecycle_event(Event{
			kind:      .window_resized
			window_id: id
			width:     width
			height:    height
		})
		events << queued_input_event(backend.input_event_from_record(record, .resized))
	}
	if state_valid {
		position := state.position
		position_changed := backend.windows[index].observed_position_known != position.known
			|| (position.known && (backend.windows[index].observed_position_x != position.x
			|| backend.windows[index].observed_position_y != position.y))
		if position_changed {
			backend.windows[index].observed_position_known = position.known
			backend.windows[index].observed_position_x = position.x
			backend.windows[index].observed_position_y = position.y
			events << queued_service_event(ServiceEvent{
				kind:      .state
				window:    backend.windows[index].id
				operation: .position
				state:     state
			})
		}
	}
	return events
}

fn (mut backend X11Backend) refresh_observed_service_state(index int) !ServiceWindowState {
	if index < 0 || index >= backend.windows.len {
		return error(err_window_not_found)
	}
	state := backend.service_window_state(backend.windows[index].id)!
	backend.windows[index].service_state_observed = true
	backend.windows[index].observed_minimized = state.minimized == .on
	backend.windows[index].observed_maximized = state.maximized == .on
	backend.windows[index].observed_fullscreen = state.fullscreen == .on
	return state
}

fn (mut backend X11Backend) queued_observed_state_transitions(index int, property X11NativeAtom) ![]QueuedEvent {
	if index < 0 || index >= backend.windows.len {
		return error(err_window_not_found)
	}
	was_observed := backend.windows[index].service_state_observed
	old_minimized := backend.windows[index].observed_minimized
	old_maximized := backend.windows[index].observed_maximized
	old_fullscreen := backend.windows[index].observed_fullscreen
	state := backend.refresh_observed_service_state(index)!
	if !was_observed {
		return []QueuedEvent{}
	}
	mut events := []QueuedEvent{}
	operations := x11_service_state_transition_operations(property == backend.wm_state,
		property == backend.net_wm_state, old_minimized, backend.windows[index].observed_minimized,
		old_maximized, backend.windows[index].observed_maximized, old_fullscreen,
		backend.windows[index].observed_fullscreen)
	for operation in operations {
		events << queued_service_event(ServiceEvent{
			kind:      .state
			window:    backend.windows[index].id
			state:     state
			operation: operation
		})
	}
	return events
}

fn x11_service_state_transition_operations(wm_state bool, net_wm_state bool, old_minimized bool, minimized bool, old_maximized bool, maximized bool, old_fullscreen bool, fullscreen bool) []ServiceOperation {
	mut operations := []ServiceOperation{}
	if wm_state && old_minimized != minimized {
		operations << if minimized { ServiceOperation.minimize } else { ServiceOperation.restore }
	}
	if net_wm_state && old_maximized != maximized {
		operations << if maximized { ServiceOperation.maximize } else { ServiceOperation.restore }
	}
	if net_wm_state && old_fullscreen != fullscreen {
		operation := if fullscreen { ServiceOperation.fullscreen } else { ServiceOperation.restore }
		if operation != .restore || ServiceOperation.restore !in operations {
			operations << operation
		}
	}
	return operations
}

fn (mut backend X11Backend) service_show_window(id WindowId) !ServiceWindowState {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if C.v_multiwindow_x11_checked_map_window(backend.checked_connection,
			backend.windows[index].window, 1) == 0 {
			return error(err_capability_unsupported)
		}
		return backend.service_window_state(id)!
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) service_hide_window(id WindowId) !ServiceWindowState {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if C.v_multiwindow_x11_checked_map_window(backend.checked_connection,
			backend.windows[index].window, 0) == 0 {
			return error(err_capability_unsupported)
		}
		return backend.service_window_state(id)!
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) service_focus_window(id WindowId) !ServiceWindowState {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if C.v_multiwindow_x11_request_focus(backend.checked_connection, backend.root,
			backend.windows[index].window, backend.net_active_window) == 0 {
			return error(err_capability_unsupported)
		}
		return backend.service_window_state(id)!
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) service_raise_window(id WindowId) !ServiceWindowState {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if C.v_multiwindow_x11_checked_raise_window(backend.checked_connection,
			backend.windows[index].window) == 0 {
			return error(err_capability_unsupported)
		}
		return backend.service_window_state(id)!
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) service_set_window_position(id WindowId, x int, y int) !ServiceWindowState {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if C.v_multiwindow_x11_checked_move_window(backend.checked_connection,
			backend.windows[index].window, x, y) == 0 {
			return error(err_capability_unsupported)
		}
		return backend.service_window_state(id)!
	} $else {
		_ = id
		_ = x
		_ = y
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) service_minimize_window(id WindowId) !ServiceWindowState {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if C.XIconifyWindow(backend.display, backend.windows[index].window, backend.screen) == 0 {
			return error(err_capability_unsupported)
		}
		C.XFlush(backend.display)
		return ServiceWindowState{}
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) service_maximize_window(id WindowId) !ServiceWindowState {
	return backend.service_change_net_state(id, 1, backend.net_wm_state_maximized_horz,
		backend.net_wm_state_maximized_vert)!
}

fn (mut backend X11Backend) service_restore_window(id WindowId) !ServiceWindowState {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if C.v_multiwindow_x11_send_net_wm_state(backend.checked_connection, backend.root, backend.windows[index].window, backend.net_wm_state, 0, backend.net_wm_state_maximized_horz, backend.net_wm_state_maximized_vert) == 0
			|| C.v_multiwindow_x11_send_net_wm_state(backend.checked_connection, backend.root, backend.windows[index].window, backend.net_wm_state, 0, backend.net_wm_state_fullscreen, X11NativeAtom(0)) == 0
			|| C.v_multiwindow_x11_checked_map_window(backend.checked_connection, backend.windows[index].window, 1) == 0 {
			return error(err_capability_unsupported)
		}
		return ServiceWindowState{}
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) service_set_fullscreen(id WindowId, enabled bool) !ServiceWindowState {
	action := if enabled { 1 } else { 0 }
	return backend.service_change_net_state(id, action, backend.net_wm_state_fullscreen,
		X11NativeAtom(0))!
}

fn (mut backend X11Backend) service_change_net_state(id WindowId, action int, first X11NativeAtom, second X11NativeAtom) !ServiceWindowState {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if C.v_multiwindow_x11_send_net_wm_state(backend.checked_connection, backend.root,
			backend.windows[index].window, backend.net_wm_state, action, first, second) == 0 {
			return error(err_capability_unsupported)
		}
		return ServiceWindowState{}
	} $else {
		_ = id
		_ = action
		_ = first
		_ = second
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) service_set_mouse_lock(id WindowId, enabled bool) !ServiceWindowState {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if enabled {
			mut center_x := 0
			mut center_y := 0
			if C.v_multiwindow_x11_acquire_mouse_lock_checked(backend.display,
				backend.windows[index].window, &center_x, &center_y) == 0 {
				return error(err_capability_unsupported)
			}
			backend.windows[index].mouse_locked = true
			backend.windows[index].mouse_lock_center_x = center_x
			backend.windows[index].mouse_lock_center_y = center_y
			backend.windows[index].mouse_x = f32(center_x)
			backend.windows[index].mouse_y = f32(center_y)
			backend.windows[index].mouse_pos_valid = true
			backend.windows[index].mouse_dx = 0
			backend.windows[index].mouse_dy = 0
		} else {
			backend.release_mouse_lock(index)
		}
		C.XFlush(backend.display)
		return backend.service_window_state(id)!
	} $else {
		_ = id
		_ = enabled
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) recenter_locked_pointer(index int, clear_delta bool) bool {
	$if linux && x_multiwindow_x11 ? {
		if index < 0 || index >= backend.windows.len || !backend.windows[index].mouse_locked
			|| backend.display == unsafe { nil } {
			return false
		}
		mut center_x := 0
		mut center_y := 0
		if C.v_multiwindow_x11_center_pointer_checked(backend.display,
			backend.windows[index].window, &center_x, &center_y) == 0 {
			return false
		}
		backend.windows[index].mouse_lock_center_x = center_x
		backend.windows[index].mouse_lock_center_y = center_y
		backend.windows[index].mouse_x = f32(center_x)
		backend.windows[index].mouse_y = f32(center_y)
		backend.windows[index].mouse_pos_valid = true
		if clear_delta {
			backend.windows[index].mouse_dx = 0
			backend.windows[index].mouse_dy = 0
		}
		return true
	}
	_ = index
	_ = clear_delta
	return false
}

fn (mut backend X11Backend) release_mouse_lock(index int) {
	$if linux && x_multiwindow_x11 ? {
		if index < 0 || index >= backend.windows.len || !backend.windows[index].mouse_locked {
			return
		}
		C.v_multiwindow_x11_release_mouse_lock(backend.display)
		backend.windows[index].mouse_locked = false
		backend.windows[index].mouse_lock_center_x = 0
		backend.windows[index].mouse_lock_center_y = 0
		backend.windows[index].mouse_dx = 0
		backend.windows[index].mouse_dy = 0
	}
}

fn (backend &X11Backend) service_native_window_borrow(id WindowId) !BackendNativeWindowBorrow {
	index := backend.window_record_index(id) or { return error(err_window_not_found) }
	if backend.display == unsafe { nil } || backend.windows[index].window == X11NativeWindow(0) {
		return error(err_capability_unsupported)
	}
	return BackendNativeWindowBorrow{
		backend:   .x11
		primary:   unsafe { voidptr(backend.display) }
		secondary: u64(backend.windows[index].window)
	}
}

fn x11_intersect_monitor_work_area(monitor ServiceRect, work_area ServiceRect) ServiceKnownRect {
	if monitor.width <= 0 || monitor.height <= 0 || work_area.width <= 0 || work_area.height <= 0 {
		return ServiceKnownRect{}
	}
	monitor_left := i64(monitor.x)
	monitor_top := i64(monitor.y)
	monitor_right := i64(monitor.x) + i64(monitor.width)
	monitor_bottom := i64(monitor.y) + i64(monitor.height)
	work_left := i64(work_area.x)
	work_top := i64(work_area.y)
	work_right := i64(work_area.x) + i64(work_area.width)
	work_bottom := i64(work_area.y) + i64(work_area.height)
	left := if monitor_left > work_left { monitor_left } else { work_left }
	top := if monitor_top > work_top { monitor_top } else { work_top }
	right := if monitor_right < work_right { monitor_right } else { work_right }
	bottom := if monitor_bottom < work_bottom { monitor_bottom } else { work_bottom }
	if right <= left || bottom <= top {
		return ServiceKnownRect{}
	}
	width := right - left
	height := bottom - top
	if left < i64(-2_147_483_648) || left > i64(2_147_483_647) || top < i64(-2_147_483_648)
		|| top > i64(2_147_483_647) || width > i64(2_147_483_647) || height > i64(2_147_483_647) {
		return ServiceKnownRect{}
	}
	return ServiceKnownRect{
		known: true
		value: ServiceRect{
			x:      int(left)
			y:      int(top)
			width:  int(width)
			height: int(height)
		}
	}
}

fn (mut backend X11Backend) service_monitor_snapshot(app_instance u64) ![]ServiceMonitorInfo {
	$if linux && x_multiwindow_x11 ? {
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_x11_open_display_failed)
		}
		if backend.monitor_snapshot_failures_for_test > 0 {
			backend.monitor_snapshot_failures_for_test--
			return error(err_capability_unsupported)
		}
		mut count := C.v_multiwindow_x11_monitor_snapshot(backend.display, backend.root,
			unsafe { nil }, 0)
		if count < 0 {
			return error(err_capability_unsupported)
		}
		if count == 0 {
			return []ServiceMonitorInfo{}
		}
		mut native := []C.VMultiwindowX11MonitorInfo{len: count}
		mut actual := C.v_multiwindow_x11_monitor_snapshot(backend.display, backend.root,
			native.data, native.len)
		if actual > native.len {
			count = actual
			native = []C.VMultiwindowX11MonitorInfo{len: count}
			actual = C.v_multiwindow_x11_monitor_snapshot(backend.display, backend.root,
				native.data, native.len)
		}
		if actual <= 0 || actual > native.len {
			return error(err_capability_unsupported)
		}
		mut monitors := []ServiceMonitorInfo{cap: actual}
		work_area := C.v_multiwindow_x11_work_area(backend.display, backend.root,
			backend.net_current_desktop, backend.net_workarea)
		for slot in 0 .. actual {
			item := native[slot]
			name_ptr := C.XGetAtomName(backend.display, item.name)
			name := if name_ptr == unsafe { nil } {
				'X11 monitor ${slot}'
			} else {
				value := unsafe { cstring_to_vstring(name_ptr) }
				C.XFree(name_ptr)
				value
			}
			mut monitor_work_area := ServiceKnownRect{}
			if work_area.known != 0 {
				monitor_work_area = x11_intersect_monitor_work_area(ServiceRect{
					x:      item.x
					y:      item.y
					width:  item.width
					height: item.height
				}, ServiceRect{
					x:      work_area.x
					y:      work_area.y
					width:  work_area.width
					height: work_area.height
				})
			}
			monitors << ServiceMonitorInfo{
				native_key: ServiceMonitorNativeKey{
					kind:    .x11_atom
					numeric: u64(item.name)
				}
				id:         ServiceMonitorId{
					app_instance: app_instance
					slot:         slot
					generation:   1
				}
				name:       name
				geometry:   ServiceKnownRect{
					known: true
					value: ServiceRect{
						x:      item.x
						y:      item.y
						width:  item.width
						height: item.height
					}
				}
				work_area:  monitor_work_area
				scale:      ServiceKnownScale{}
				primary:    if item.primary != 0 { .on } else { .off }
				available:  true
			}
		}
		if !service_monitor_snapshot_identity_valid(monitors, .x11, app_instance) {
			return error(err_capability_unsupported)
		}
		return monitors
	} $else {
		_ = app_instance
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) queued_randr_monitor_events() ![]QueuedEvent {
	app_instance := if backend.native_operations == unsafe { nil } {
		u64(0)
	} else {
		backend.native_operations.app_identity
	}
	if app_instance == 0 {
		return error(err_app_identity_mismatch)
	}
	monitors := backend.service_monitor_snapshot(app_instance)!
	return backend.queued_monitor_snapshot_events(monitors)
}

fn (backend &X11Backend) queued_monitor_snapshot_events(monitors []ServiceMonitorInfo) []QueuedEvent {
	app_instance := if backend.native_operations == unsafe { nil } {
		u64(0)
	} else {
		backend.native_operations.app_identity
	}
	return [
		queued_service_event(ServiceEvent{
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
		}),
	]
}

fn (backend &X11Backend) service_portal_parent_identifier(id WindowId) !string {
	index := backend.window_record_index(id) or { return error(err_window_not_found) }
	window := backend.windows[index].window
	if window == X11NativeWindow(0) {
		return error(err_capability_unsupported)
	}
	return 'x11:${u64(window):x}'
}

fn x11_clipboard_ready_storage_fits(current_bytes u64, read_charge int, terminal_len int) bool {
	if read_charge < 0 || terminal_len < 0 || u64(read_charge) > current_bytes {
		return false
	}
	remaining_bytes := current_bytes - u64(read_charge)
	if terminal_len > x11_clipboard_max_bytes
		|| remaining_bytes > u64(x11_clipboard_max_pending_bytes) {
		return false
	}
	return u64(terminal_len) <= u64(x11_clipboard_max_pending_bytes) - remaining_bytes
}

fn (backend &X11Backend) clipboard_pending_bytes() u64 {
	mut total := u64(0)
	for read in backend.clipboard_reads {
		reserved := if read.reserved_bytes > read.data.len {
			read.reserved_bytes
		} else {
			read.data.len
		}
		total += u64(reserved)
	}
	for transfer in backend.clipboard_transfers {
		total += u64(transfer.data.len)
	}
	for retained in backend.clipboard_retained {
		total += u64(retained.bytes)
	}
	return total
}

fn (backend &X11Backend) clipboard_retained_index(event ServiceEvent) ?int {
	if event.kind != .clipboard || event.operation != .clipboard_read
		|| event.clipboard.status != .ready || event.window != event.clipboard.window
		|| event.clipboard.id.serial == 0 {
		return none
	}
	for index, retained in backend.clipboard_retained {
		if retained.request == event.clipboard.id && retained.window == event.window {
			return index
		}
	}
	return none
}

fn (mut backend X11Backend) claim_clipboard_terminal_storage(event ServiceEvent) {
	index := backend.clipboard_retained_index(event) or { return }
	backend.clipboard_retained[index].claimed_by_app = true
}

fn (mut backend X11Backend) discard_unclaimed_clipboard_terminal_storage(event ServiceEvent) {
	index := backend.clipboard_retained_index(event) or { return }
	if !backend.clipboard_retained[index].claimed_by_app {
		backend.clipboard_retained.delete(index)
	}
}

fn (mut backend X11Backend) release_claimed_clipboard_terminal_storage(event ServiceEvent) {
	index := backend.clipboard_retained_index(event) or { return }
	if backend.clipboard_retained[index].claimed_by_app {
		backend.clipboard_retained.delete(index)
	}
}

fn (backend &X11Backend) clipboard_can_admit(additional_bytes int) bool {
	if additional_bytes < 0
		|| backend.clipboard_reads.len + backend.clipboard_transfers.len >= x11_clipboard_max_pending_operations {
		return false
	}
	return backend.clipboard_pending_bytes() + u64(additional_bytes) <= u64(x11_clipboard_max_pending_bytes)
}

fn (backend &X11Backend) clipboard_can_reserve(additional_bytes int) bool {
	return additional_bytes >= 0
		&& backend.clipboard_pending_bytes() + u64(additional_bytes) <= u64(x11_clipboard_max_pending_bytes)
}

fn (backend &X11Backend) clipboard_incremental_reservation_after_chunk(item_count X11NativeULong) !int {
	if backend.clipboard_reads.len == 0 {
		return error(err_capability_unsupported)
	}
	read := backend.clipboard_reads[0]
	if read.data.len > x11_clipboard_max_bytes || read.reserved_bytes < 0
		|| read.reserved_bytes > x11_clipboard_max_bytes
		|| item_count > X11NativeULong(x11_clipboard_max_bytes - read.data.len) {
		return error(err_clipboard_capacity)
	}
	next_len := read.data.len + int(item_count)
	charged := if read.reserved_bytes > read.data.len {
		read.reserved_bytes
	} else {
		read.data.len
	}
	next_reserved := if read.reserved_bytes > next_len {
		read.reserved_bytes
	} else {
		next_len
	}
	additional := next_reserved - charged
	if additional > 0 && !backend.clipboard_can_reserve(additional) {
		return error(err_clipboard_capacity)
	}
	return next_reserved
}

fn (mut backend X11Backend) service_set_clipboard_text(id WindowId, request ServiceRequestId, text string) !BackendClipboardStart {
	$if linux && x_multiwindow_x11 ? {
		_ = request
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.started || backend.display == unsafe { nil }
			|| backend.clipboard == X11NativeAtom(0) || text.len > x11_clipboard_max_bytes {
			return error(err_capability_unsupported)
		}
		$if test {
			if backend.clipboard_write_failures_for_test > 0 {
				backend.clipboard_write_failures_for_test--
				return error(err_capability_unsupported)
			}
		}
		window := backend.windows[index].window
		if C.v_multiwindow_x11_set_selection_owner_checked(backend.display, backend.clipboard,
			window, X11NativeULong(0)) == 0 {
			return error(err_capability_unsupported)
		}
		backend.clipboard_owner_window = window
		backend.clipboard_owner_id = id
		backend.clipboard_text = text.clone()
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

fn (mut backend X11Backend) service_request_clipboard_text(id WindowId, request ServiceRequestId) !BackendClipboardStart {
	$if linux && x_multiwindow_x11 ? {
		_ = backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.started || backend.display == unsafe { nil }
			|| backend.clipboard == X11NativeAtom(0) || backend.clipboard_utf8 == X11NativeAtom(0)
			|| backend.clipboard_property == X11NativeAtom(0) {
			return error(err_capability_unsupported)
		}
		if !backend.clipboard_can_admit(0) {
			return error(err_clipboard_capacity)
		}
		backend.clipboard_reads << X11ClipboardRead{
			request:  request
			window:   id
			property: backend.clipboard_property
		}
		if backend.clipboard_reads.len == 1 {
			backend.start_next_clipboard_read() or {
				backend.clipboard_reads.delete(0)
				return err
			}
		}
		return BackendClipboardStart{}
	} $else {
		_ = id
		_ = request
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) start_next_clipboard_read() ! {
	$if linux && x_multiwindow_x11 ? {
		if backend.clipboard_reads.len == 0 {
			return
		}
		if backend.clipboard_requestor_create_failures_for_test > 0 {
			backend.clipboard_requestor_create_failures_for_test--
			return error(err_capability_unsupported)
		}
		requestor := C.v_multiwindow_x11_create_clipboard_requestor(backend.display, backend.root)
		if requestor == X11NativeWindow(0) {
			return error(err_capability_unsupported)
		}
		backend.clipboard_reads[0].requestor = requestor
		backend.clipboard_reads[0].deadline_ns = vtime.sys_mono_now() + x11_clipboard_timeout_ns
		read := backend.clipboard_reads[0]
		C.XDeleteProperty(backend.display, read.requestor, read.property)
		C.XConvertSelection(backend.display, backend.clipboard, backend.clipboard_utf8,
			read.property, read.requestor, X11NativeULong(0))
		C.XFlush(backend.display)
		return
	}
	return error(err_backend_unsupported)
}

fn (mut backend X11Backend) service_window_readback(id WindowId, x int, y int, width int, height int) ![]u8 {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		backend.service_window_readback_preflight(id, x, y, width, height)!
		$if test {
			if backend.readback_destroy_after_probe_for_test {
				backend.readback_destroy_after_probe_for_test = false
				C.XDestroyWindow(backend.display, backend.windows[index].window)
				C.XSync(backend.display, 0)
			}
		}
		mut pixels := []u8{len: width * height * 4}
		if C.v_multiwindow_x11_readback_rgba8(backend.display, backend.checked_connection,
			backend.windows[index].window, x, y, width, height, pixels.data, pixels.len) == 0 {
			return error(err_readback_invalid)
		}
		return pixels
	} $else {
		_ = id
		_ = x
		_ = y
		_ = width
		_ = height
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) service_window_readback_preflight(id WindowId, x int, y int, width int, height int) ! {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if C.XSync(backend.display, 0) == 0 {
			return error(err_readback_invalid)
		}
		probe := C.v_multiwindow_x11_readback_probe(backend.checked_connection,
			backend.windows[index].window, width, height, 0)
		if probe.attributes_available == 0
			|| !x11_native_readback_rect_fits(probe.map_state, x, y, width, height, probe.actual_width, probe.actual_height) {
			return error(err_readback_invalid)
		}
		return
	} $else {
		_ = id
		_ = x
		_ = y
		_ = width
		_ = height
		return error(err_backend_unsupported)
	}
}

fn x11_native_readback_rect_fits(map_state int, x int, y int, width int, height int, native_width int, native_height int) bool {
	if map_state != 2 || x < 0 || y < 0 || width <= 0 || height <= 0 || native_width <= 0
		|| native_height <= 0 || width > native_width || height > native_height {
		return false
	}
	return x <= native_width - width && y <= native_height - height
}

fn (backend &X11Backend) service_paint_readback_pattern_for_test(id WindowId, x int, y int) ! {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if C.v_multiwindow_x11_paint_rgba8_test_pattern(backend.display,
			backend.windows[index].window, x, y) == 0 {
			return error(err_readback_invalid)
		}
		return
	} $else {
		_ = id
		_ = x
		_ = y
		return error(err_backend_unsupported)
	}
}

fn (backend &X11Backend) service_readback_probe_for_test(id WindowId, width int, height int) !X11ReadbackProbe {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		probe := C.v_multiwindow_x11_readback_probe(backend.checked_connection,
			backend.windows[index].window, width, height, usize(width * height * 4))
		return X11ReadbackProbe{
			attributes_available:   probe.attributes_available
			map_state:              probe.map_state
			actual_width:           probe.actual_width
			actual_height:          probe.actual_height
			requested_width:        probe.requested_width
			requested_height:       probe.requested_height
			pixels_length:          probe.pixels_length
			expected_pixels_length: probe.expected_pixels_length
		}
	} $else {
		_ = id
		_ = width
		_ = height
		return error(err_backend_unsupported)
	}
}

fn (backend &X11Backend) service_window_size_available_for_test(id WindowId) !bool {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		mut width := 0
		mut height := 0
		return C.v_multiwindow_x11_get_window_size(backend.checked_connection,
			backend.windows[index].window, &width, &height) != 0
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) service_destroy_native_window_retaining_record_for_test(id WindowId) ! {
	$if linux && x_multiwindow_x11 ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		C.XDestroyWindow(backend.display, backend.windows[index].window)
		C.XSync(backend.display, 0)
		return
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) service_queue_wm_state_then_destroy_for_test(id WindowId) ! {
	$if test {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			if C.v_multiwindow_x11_queue_wm_state_then_destroy_for_test(backend.display,
				backend.windows[index].window, backend.wm_state) == 0 {
				return error(err_capability_unsupported)
			}
			return
		} $else {
			_ = id
			return error(err_backend_unsupported)
		}
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) service_destroy_readback_after_probe_for_test() {
	backend.readback_destroy_after_probe_for_test = true
}

fn (backend &X11Backend) service_checked_connection_usable_for_test() bool {
	$if linux && x_multiwindow_x11 ? {
		return C.v_multiwindow_x11_checked_connection_usable(backend.checked_connection) != 0
	}
	return false
}

fn (backend &X11Backend) service_shared_connection_usable_for_test() bool {
	$if linux && x_multiwindow_x11 ? {
		return C.v_multiwindow_x11_shared_connection_usable(backend.display) != 0
	}
	return false
}

fn (mut backend X11Backend) service_close_checked_connection_for_test() {
	$if linux && x_multiwindow_x11 ? {
		C.v_multiwindow_x11_close_checked_connection(backend.checked_connection)
		backend.checked_connection = unsafe { nil }
	}
}

fn (backend &X11Backend) service_owner_modal_matches_for_test(child WindowId, owner WindowId, modal bool) !bool {
	$if linux && x_multiwindow_x11 ? {
		child_index := backend.window_record_index(child) or { return error(err_window_not_found) }
		owner_index := backend.window_record_index(owner) or { return error(err_window_not_found) }
		return C.v_multiwindow_x11_owner_modal_matches(backend.display,
			backend.windows[child_index].window, backend.windows[owner_index].window,
			x11_bool_to_int(modal)) != 0
	} $else {
		_ = child
		_ = owner
		_ = modal
		return error(err_backend_unsupported)
	}
}

$if test {
	fn (backend &X11Backend) service_ewmh_capabilities_match_root_for_test() bool {
		$if linux && x_multiwindow_x11 ? {
			active := C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root,
				backend.net_active_window) != 0
			maximize :=
				C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state) != 0
				&& C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state_maximized_horz) != 0
				&& C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state_maximized_vert) != 0
			fullscreen :=
				C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state) != 0
				&& C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state_fullscreen) != 0
			modal :=
				C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state) != 0
				&& C.v_multiwindow_x11_root_supports_atom(backend.display, backend.root, backend.net_wm_state_modal) != 0
			return backend.ewmh_active_window == active && backend.ewmh_maximize == maximize
				&& backend.ewmh_fullscreen == fullscreen && backend.ewmh_modal == modal
		}
		return false
	}

	fn (backend &X11Backend) service_randr_subscription_for_test() bool {
		return backend.randr_subscribed && backend.randr_event_base > 0
	}

	fn (backend &X11Backend) service_root_property_subscription_for_test() bool {
		$if linux && x_multiwindow_x11 ? {
			return backend.root_property_subscribed
				&& C.v_multiwindow_x11_has_property_changes(backend.display, backend.root) != 0
		}
		return false
	}

	fn (mut backend X11Backend) service_set_workareas_for_test(current_desktop int, workareas []ServiceRect) ! {
		$if linux && x_multiwindow_x11 ? {
			if current_desktop < 0 || current_desktop >= workareas.len
				|| backend.net_current_desktop == X11NativeAtom(0)
				|| backend.net_workarea == X11NativeAtom(0) {
				return error(err_capability_unsupported)
			}
			cardinal := C.XInternAtom(backend.display, c'CARDINAL', 0)
			mut desktop := X11NativeULong(current_desktop)
			mut values := []X11NativeULong{cap: workareas.len * 4}
			for area in workareas {
				values << X11NativeULong(area.x)
				values << X11NativeULong(area.y)
				values << X11NativeULong(area.width)
				values << X11NativeULong(area.height)
			}
			C.XChangeProperty(backend.display, backend.root, backend.net_current_desktop, cardinal,
				32, x11_prop_mode_replace, unsafe { &u8(&desktop) }, 1)
			C.XChangeProperty(backend.display, backend.root, backend.net_workarea, cardinal, 32,
				x11_prop_mode_replace, unsafe { &u8(values.data) }, values.len)
			C.XSync(backend.display, 0)
			return
		}
		_ = current_desktop
		_ = workareas
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_delete_workarea_for_test() ! {
		$if linux && x_multiwindow_x11 ? {
			C.XDeleteProperty(backend.display, backend.root, backend.net_workarea)
			C.XSync(backend.display, 0)
			return
		}
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_fail_monitor_snapshots_for_test(count int) {
		backend.monitor_snapshot_failures_for_test = count
	}

	fn (backend &X11Backend) service_monitor_snapshot_dirty_for_test() bool {
		return backend.monitor_snapshot_dirty
	}

	fn (mut backend X11Backend) service_randr_snapshot_events_for_test() ![]QueuedEvent {
		return backend.queued_randr_monitor_events()!
	}

	fn (backend &X11Backend) service_randr_events_for_snapshot_for_test(monitors []ServiceMonitorInfo) []QueuedEvent {
		return backend.queued_monitor_snapshot_events(monitors)
	}

	fn (mut backend X11Backend) service_make_clipboard_peer_unresponsive_for_test(id WindowId) ! {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			C.XSetSelectionOwner(backend.display, backend.clipboard, backend.windows[index].window,
				X11NativeULong(0))
			C.XSync(backend.display, 0)
			backend.clipboard_owner_window = X11NativeWindow(0)
			backend.clipboard_owner_id = WindowId{}
			backend.clipboard_text = ''
			return
		}
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_start_unresponsive_incr_peer_for_test(id WindowId) ! {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			window := backend.windows[index].window
			C.XDeleteProperty(backend.display, window, backend.clipboard_property)
			C.XConvertSelection(backend.display, backend.clipboard, backend.clipboard_utf8,
				backend.clipboard_property, window, X11NativeULong(0))
			C.XFlush(backend.display)
			return
		}
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_clipboard_targets_for_test(owner WindowId, requestor WindowId) !(bool, bool) {
		$if linux && x_multiwindow_x11 ? {
			owner_index := backend.window_record_index(owner) or {
				return error(err_window_not_found)
			}
			requestor_index := backend.window_record_index(requestor) or {
				return error(err_window_not_found)
			}
			mut event := C.XEvent{}
			unsafe {
				event.xselectionrequest.@type = x11_selection_request
				event.xselectionrequest.display = backend.display
				event.xselectionrequest.owner = backend.windows[owner_index].window
				event.xselectionrequest.requestor = backend.windows[requestor_index].window
				event.xselectionrequest.selection = backend.clipboard
				event.xselectionrequest.target = backend.clipboard_targets
				event.xselectionrequest.property = backend.clipboard_property
			}
			backend.handle_clipboard_selection_request(&event)
			utf8 := C.v_multiwindow_x11_property_has_atom(backend.display,
				backend.windows[requestor_index].window, backend.clipboard_property,
				backend.clipboard_utf8) != 0
			legacy := C.v_multiwindow_x11_property_has_atom(backend.display,
				backend.windows[requestor_index].window, backend.clipboard_property,
				backend.clipboard_string) != 0
			return utf8, legacy
		}
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_expire_clipboard_for_test() {
		$if linux && x_multiwindow_x11 ? {
			expired := vtime.sys_mono_now() - 1
			for index in 0 .. backend.clipboard_reads.len {
				backend.clipboard_reads[index].deadline_ns = expired
			}
			for index in 0 .. backend.clipboard_transfers.len {
				backend.clipboard_transfers[index].deadline_ns = expired
			}
		}
	}

	fn (mut backend X11Backend) service_queue_clipboard_selection_reply_for_test(text string, relevant bool) ! {
		$if linux && x_multiwindow_x11 ? {
			if backend.clipboard_reads.len == 0 || text == '' {
				return error(err_capability_unsupported)
			}
			read := backend.clipboard_reads[0]
			payload := text.bytes()
			C.XChangeProperty(backend.display, read.requestor, read.property,
				backend.clipboard_utf8, 8, x11_prop_mode_replace, payload.data, payload.len)
			mut event := C.XEvent{}
			unsafe {
				event.xselection.@type = x11_selection_notify
				event.xselection.display = backend.display
				event.xselection.requestor = if relevant { read.requestor } else { backend.root }
				event.xselection.selection = backend.clipboard
				event.xselection.target = backend.clipboard_utf8
				event.xselection.property = read.property
			}
			C.XPutBackEvent(backend.display, &event)
			backend.clipboard_reads[0].deadline_ns = vtime.sys_mono_now() - 1
			return
		}
		_ = text
		_ = relevant
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_queue_clipboard_incr_terminal_for_test(text string) ! {
		$if linux && x_multiwindow_x11 ? {
			if backend.clipboard_reads.len == 0 {
				return error(err_capability_unsupported)
			}
			read := backend.clipboard_reads[0]
			backend.clipboard_reads[0].incremental = true
			backend.clipboard_reads[0].data = text.bytes()
			backend.clipboard_reads[0].reserved_bytes = text.len
			C.XChangeProperty(backend.display, read.requestor, read.property,
				backend.clipboard_utf8, 8, x11_prop_mode_replace, unsafe { nil }, 0)
			mut event := C.XEvent{}
			unsafe {
				event.xproperty.@type = x11_property_notify
				event.xproperty.display = backend.display
				event.xproperty.window = read.requestor
				event.xproperty.atom = read.property
				event.xproperty.state = x11_property_new_value
			}
			C.XPutBackEvent(backend.display, &event)
			backend.clipboard_reads[0].deadline_ns = vtime.sys_mono_now() - 1
			return
		}
		_ = text
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_queue_clipboard_incr_start_for_test(advertised u64) ! {
		$if linux && x_multiwindow_x11 ? {
			if backend.clipboard_reads.len == 0 || advertised > u64(x11_clipboard_max_bytes) {
				return error(err_capability_unsupported)
			}
			read := backend.clipboard_reads[0]
			value := X11NativeULong(advertised)
			C.XChangeProperty(backend.display, read.requestor, read.property,
				backend.clipboard_incr, 32, x11_prop_mode_replace, unsafe { &u8(&value) }, 1)
			C.XSync(backend.display, 0)
			for C.XPending(backend.display) > 0 {
				mut discarded := C.XEvent{}
				C.XNextEvent(backend.display, &discarded)
			}
			mut event := C.XEvent{}
			unsafe {
				event.xselection.@type = x11_selection_notify
				event.xselection.display = backend.display
				event.xselection.requestor = read.requestor
				event.xselection.selection = backend.clipboard
				event.xselection.target = backend.clipboard_utf8
				event.xselection.property = read.property
			}
			C.XPutBackEvent(backend.display, &event)
			return
		}
		_ = advertised
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_queue_clipboard_incr_chunk_for_test(payload []u8) ! {
		$if linux && x_multiwindow_x11 ? {
			if backend.clipboard_reads.len == 0 || !backend.clipboard_reads[0].incremental {
				return error(err_capability_unsupported)
			}
			read := backend.clipboard_reads[0]
			mut data := &u8(unsafe { nil })
			if payload.len > 0 {
				data = payload.data
			}
			C.XChangeProperty(backend.display, read.requestor, read.property,
				backend.clipboard_utf8, 8, x11_prop_mode_replace, data, payload.len)
			C.XSync(backend.display, 0)
			for C.XPending(backend.display) > 0 {
				mut discarded := C.XEvent{}
				C.XNextEvent(backend.display, &discarded)
			}
			mut event := C.XEvent{}
			unsafe {
				event.xproperty.@type = x11_property_notify
				event.xproperty.display = backend.display
				event.xproperty.window = read.requestor
				event.xproperty.atom = read.property
				event.xproperty.state = x11_property_new_value
			}
			C.XPutBackEvent(backend.display, &event)
			return
		}
		_ = payload
		return error(err_backend_unsupported)
	}

	fn (backend &X11Backend) service_clipboard_pending_counts_for_test() (int, int) {
		return backend.clipboard_reads.len, backend.clipboard_transfers.len
	}

	fn (backend &X11Backend) service_clipboard_pending_bytes_for_test() u64 {
		return backend.clipboard_pending_bytes()
	}

	fn (backend &X11Backend) service_clipboard_owner_for_test() (bool, int) {
		return backend.clipboard_owner_window != X11NativeWindow(0), backend.clipboard_text.len
	}

	fn (mut backend X11Backend) service_take_clipboard_selection_for_test(id WindowId) ! {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			previous_owner := backend.clipboard_owner_window
			C.XSetSelectionOwner(backend.display, backend.clipboard, backend.windows[index].window,
				X11NativeULong(0))
			C.XSync(backend.display, 0)
			mut event := C.XEvent{}
			unsafe {
				event.xselectionclear.@type = x11_selection_clear
				event.xselectionclear.display = backend.display
				event.xselectionclear.window = previous_owner
				event.xselectionclear.selection = backend.clipboard
			}
			C.XPutBackEvent(backend.display, &event)
			return
		}
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_queue_xdnd_incr_start_for_test(source WindowId, target WindowId, advertised u64, format int, relevant bool) ! {
		$if linux && x_multiwindow_x11 ? {
			if format != 8 && format != 32 {
				return error(err_capability_unsupported)
			}
			source_index := backend.window_record_index(source) or {
				return error(err_window_not_found)
			}
			target_index := backend.window_record_index(target) or {
				return error(err_window_not_found)
			}
			backend.cancel_xdnd_drop()
			requestor := backend.windows[target_index].window
			property := backend.xdnd_selection
			drop_time := X11NativeULong(1)
			backend.xdnd_drop_state = X11XdndDrop{
				active:      true
				source:      backend.windows[source_index].window
				requestor:   requestor
				window:      target
				property:    property
				target_type: backend.text_uri_list
				version:     x11_xdnd_version
				time:        drop_time
				deadline_ns: vtime.sys_mono_now() + x11_xdnd_timeout_ns
			}
			C.XSetSelectionOwner(backend.display, backend.xdnd_selection,
				backend.windows[target_index].window, X11NativeULong(0))
			payload_size := X11NativeULong(advertised)
			C.XChangeProperty(backend.display, requestor, property, backend.clipboard_incr, format,
				x11_prop_mode_replace, unsafe { &u8(&payload_size) }, 1)
			C.XSync(backend.display, 0)
			for C.XPending(backend.display) > 0 {
				mut discarded := C.XEvent{}
				C.XNextEvent(backend.display, &discarded)
			}
			mut event := C.XEvent{}
			unsafe {
				event.xselection.@type = x11_selection_notify
				event.xselection.display = backend.display
				event.xselection.requestor = if relevant { requestor } else { backend.root }
				event.xselection.selection = backend.xdnd_selection
				event.xselection.target = backend.text_uri_list
				event.xselection.property = property
				event.xselection.time = drop_time
			}
			C.XPutBackEvent(backend.display, &event)
			return
		}
		_ = source
		_ = target
		_ = advertised
		_ = format
		_ = relevant
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_queue_xdnd_chunk_for_test(payload []u8, relevant bool, valid_type bool, format int) ! {
		$if linux && x_multiwindow_x11 ? {
			if format != 8 && payload.len > 0 {
				return error('multiwindow: x11 test chunk format requires an empty payload')
			}
			if !backend.xdnd_drop_state.active {
				return error(err_capability_unsupported)
			}
			drop := backend.xdnd_drop_state
			property_type := if valid_type { backend.text_uri_list } else { backend.clipboard_utf8 }
			mut data := &u8(unsafe { nil })
			if payload.len > 0 {
				data = payload.data
			}
			C.XChangeProperty(backend.display, drop.requestor, drop.property, property_type,
				format, x11_prop_mode_replace, data, payload.len)
			C.XSync(backend.display, 0)
			for C.XPending(backend.display) > 0 {
				mut discarded := C.XEvent{}
				C.XNextEvent(backend.display, &discarded)
			}
			mut event := C.XEvent{}
			unsafe {
				event.xproperty.@type = x11_property_notify
				event.xproperty.display = backend.display
				event.xproperty.window = drop.requestor
				event.xproperty.atom = if relevant {
					drop.property
				} else {
					backend.clipboard_property
				}
				event.xproperty.state = x11_property_new_value
			}
			C.XPutBackEvent(backend.display, &event)
			return
		}
		_ = payload
		_ = relevant
		_ = valid_type
		_ = format
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_set_xdnd_property_without_event_for_test(payload []u8) ! {
		$if linux && x_multiwindow_x11 ? {
			if !backend.xdnd_drop_state.active {
				return error(err_capability_unsupported)
			}
			drop := backend.xdnd_drop_state
			mut data := &u8(unsafe { nil })
			if payload.len > 0 {
				data = payload.data
			}
			C.XChangeProperty(backend.display, drop.requestor, drop.property,
				backend.text_uri_list, 8, x11_prop_mode_replace, data, payload.len)
			C.XSync(backend.display, 0)
			for C.XPending(backend.display) > 0 {
				mut discarded := C.XEvent{}
				C.XNextEvent(backend.display, &discarded)
			}
			return
		}
		_ = payload
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_queue_stale_xdnd_selection_for_test(payload []u8) ! {
		$if linux && x_multiwindow_x11 ? {
			if backend.xdnd_drop_state.active || backend.xdnd_last_requestor == X11NativeWindow(0)
				|| backend.xdnd_last_property == X11NativeAtom(0) {
				return error(err_capability_unsupported)
			}
			mut data := &u8(unsafe { nil })
			if payload.len > 0 {
				data = payload.data
			}
			C.XChangeProperty(backend.display, backend.xdnd_last_requestor,
				backend.xdnd_last_property, backend.text_uri_list, 8, x11_prop_mode_replace, data,
				payload.len)
			C.XSync(backend.display, 0)
			for C.XPending(backend.display) > 0 {
				mut discarded := C.XEvent{}
				C.XNextEvent(backend.display, &discarded)
			}
			mut event := C.XEvent{}
			unsafe {
				event.xselection.@type = x11_selection_notify
				event.xselection.display = backend.display
				event.xselection.requestor = backend.xdnd_last_requestor
				event.xselection.selection = backend.xdnd_selection
				event.xselection.target = backend.text_uri_list
				event.xselection.property = backend.xdnd_last_property
				event.xselection.time = backend.xdnd_last_time
			}
			C.XPutBackEvent(backend.display, &event)
			return
		}
		_ = payload
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_queue_stale_xdnd_property_for_test(payload []u8) ! {
		$if linux && x_multiwindow_x11 ? {
			if backend.xdnd_drop_state.active || backend.xdnd_last_requestor == X11NativeWindow(0)
				|| backend.xdnd_last_property == X11NativeAtom(0) {
				return error(err_capability_unsupported)
			}
			mut data := &u8(unsafe { nil })
			if payload.len > 0 {
				data = payload.data
			}
			C.XChangeProperty(backend.display, backend.xdnd_last_requestor,
				backend.xdnd_last_property, backend.text_uri_list, 8, x11_prop_mode_replace, data,
				payload.len)
			C.XSync(backend.display, 0)
			for C.XPending(backend.display) > 0 {
				mut discarded := C.XEvent{}
				C.XNextEvent(backend.display, &discarded)
			}
			mut event := C.XEvent{}
			unsafe {
				event.xproperty.@type = x11_property_notify
				event.xproperty.display = backend.display
				event.xproperty.window = backend.xdnd_last_requestor
				event.xproperty.atom = backend.xdnd_last_property
				event.xproperty.state = x11_property_new_value
			}
			C.XPutBackEvent(backend.display, &event)
			return
		}
		_ = payload
		return error(err_backend_unsupported)
	}

	fn (mut backend X11Backend) service_destroy_xdnd_source_then_finish_for_test() ! {
		$if linux && x_multiwindow_x11 ? {
			if !backend.xdnd_drop_state.active {
				return error(err_capability_unsupported)
			}
			source := C.XCreateSimpleWindow(backend.display, backend.root, 0, 0, 1, 1, 0,
				X11NativeULong(0), X11NativeULong(0))
			if source == X11NativeWindow(0) {
				return error(err_backend_unsupported)
			}
			backend.xdnd_drop_state.source = source
			C.XDestroyWindow(backend.display, source)
			C.XSync(backend.display, 0)
			backend.finish_xdnd_drop(false)
			return
		}
		return error(err_backend_unsupported)
	}

	fn (backend &X11Backend) service_xdnd_state_for_test() (bool, bool, int, i64, int, bool) {
		return backend.xdnd_drop_state.active, backend.xdnd_drop_state.incremental, backend.xdnd_drop_state.data.len, backend.xdnd_drop_state.deadline_ns, backend.xdnd_finished_count, backend.xdnd_last_finished_accepted
	}

	fn (backend &X11Backend) service_xdnd_wire_finished_for_test() int {
		return backend.xdnd_wire_finished_count
	}

	fn (backend &X11Backend) service_xdnd_property_delete_count_for_test() int {
		return backend.xdnd_property_delete_count
	}

	fn (backend &X11Backend) service_xdnd_terminal_order_for_test() (u64, u64) {
		return backend.xdnd_last_finished_sequence, backend.xdnd_last_property_sequence
	}

	fn (mut backend X11Backend) service_expire_xdnd_for_test() {
		$if linux && x_multiwindow_x11 ? {
			backend.xdnd_drop_state.deadline_ns = vtime.sys_mono_now() - 1
		} $else {
			backend.xdnd_drop_state.deadline_ns = 1
		}
	}

	fn (backend &X11Backend) service_xdnd_property_exists_for_test() bool {
		$if linux && x_multiwindow_x11 ? {
			requestor := if backend.xdnd_drop_state.active {
				backend.xdnd_drop_state.requestor
			} else {
				backend.xdnd_last_requestor
			}
			property := if backend.xdnd_drop_state.active {
				backend.xdnd_drop_state.property
			} else {
				backend.xdnd_last_property
			}
			if requestor == X11NativeWindow(0) || property == X11NativeAtom(0) {
				return false
			}
			mut actual_type := X11NativeAtom(0)
			mut actual_format := 0
			mut item_count := X11NativeULong(0)
			mut bytes_after := X11NativeULong(0)
			mut data := &u8(unsafe { nil })
			status := C.XGetWindowProperty(backend.display, requestor, property, X11NativeLong(0),
				X11NativeLong(0), 0, X11NativeAtom(0), &actual_type, &actual_format, &item_count,
				&bytes_after, &&u8(&data))
			if data != unsafe { nil } {
				C.XFree(data)
			}
			return status == x11_success && actual_type != X11NativeAtom(0)
		}
		return false
	}

	fn (mut backend X11Backend) service_send_focus_out_for_test(id WindowId) ! {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			if C.v_multiwindow_x11_send_focus_out_for_test(backend.display,
				backend.windows[index].window) == 0 {
				return error(err_capability_unsupported)
			}
			return
		}
		return error(err_backend_unsupported)
	}

	fn (backend &X11Backend) service_mouse_locked_for_test(id WindowId) !bool {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		return backend.windows[index].mouse_locked
	}

	fn (mut backend X11Backend) service_warp_relative_for_test(id WindowId, dx int, dy int) ! {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			record := backend.windows[index]
			if !record.mouse_locked
				|| C.v_multiwindow_x11_warp_pointer_offset_for_test(backend.display, record.window, record.mouse_lock_center_x, record.mouse_lock_center_y, dx, dy) == 0 {
				return error(err_capability_unsupported)
			}
			return
		}
		return error(err_backend_unsupported)
	}

	fn (backend &X11Backend) service_pointer_recentered_for_test(id WindowId) !bool {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			record := backend.windows[index]
			mut x := 0
			mut y := 0
			if C.v_multiwindow_x11_pointer_position_for_test(backend.display, record.window, &x, &y) == 0 {
				return error(err_capability_unsupported)
			}
			return x == record.mouse_lock_center_x && y == record.mouse_lock_center_y
		}
		return error(err_backend_unsupported)
	}

	fn (backend &X11Backend) service_mouse_lock_center_for_test(id WindowId) !(int, int) {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		record := backend.windows[index]
		if !record.mouse_locked {
			return error(err_capability_unsupported)
		}
		return record.mouse_lock_center_x, record.mouse_lock_center_y
	}

	fn (mut backend X11Backend) service_native_resize_for_test(id WindowId, width int, height int) ! {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			if width <= 0 || height <= 0
				|| C.XResizeWindow(backend.display, backend.windows[index].window, u32(width), u32(height)) == 0
				|| C.XSync(backend.display, 0) == 0 {
				return error(err_x11_resize_window_failed)
			}
			return
		}
		_ = id
		_ = width
		_ = height
		return error(err_backend_unsupported)
	}
}

fn (mut backend X11Backend) poll_queued_events() ![]QueuedEvent {
	mut events := []QueuedEvent{}
	$if linux && x_multiwindow_x11 ? {
		if !backend.started || backend.display == unsafe { nil } {
			return events
		}
		pending_clipboard_terminal_count := backend.pending_clipboard_terminal_events.len
		for event in backend.pending_clipboard_terminal_events {
			events << event
		}
		for C.XPending(backend.display) > 0 {
			mut event := C.XEvent{}
			C.XNextEvent(backend.display, &event)
			if C.XFilterEvent(&event, X11NativeWindow(0)) != 0 {
				continue
			}
			event_type := unsafe { event.@type }
			if backend.randr_subscribed
				&& C.v_multiwindow_x11_is_randr_event(event_type, backend.randr_event_base) != 0 {
				C.v_multiwindow_x11_update_randr_configuration(&event, backend.randr_event_base)
				events << backend.queued_randr_monitor_events()!
				backend.monitor_snapshot_dirty = false
				continue
			}
			match event_type {
				x11_client_message {
					message_type := unsafe { event.xclient.message_type }
					format := unsafe { event.xclient.format }
					protocol := unsafe { X11NativeAtom(event.xclient.data.l[0]) }
					if message_type == backend.wm_protocols && format == 32
						&& protocol == backend.wm_delete_window {
						native_window := unsafe { event.xclient.window }
						id := backend.window_id_for_native(native_window) or { continue }
						events << queued_lifecycle_event(Event{
							kind:      .window_close_requested
							window_id: id
						})
					} else if format == 32 {
						events << backend.queued_xdnd_client_message_events(&event)
					}
				}
				x11_configure_notify {
					native_window := unsafe { event.xconfigure.window }
					index := backend.window_record_index_for_native(native_window) or { continue }
					width := unsafe { event.xconfigure.width }
					height := unsafe { event.xconfigure.height }
					mut mouse_lock_released := false
					if backend.windows[index].mouse_locked && width > 0 && height > 0
						&& (backend.windows[index].mouse_lock_center_x != width / 2
						|| backend.windows[index].mouse_lock_center_y != height / 2)
						&& !backend.recenter_locked_pointer(index, true) {
						backend.release_mouse_lock(index)
						mouse_lock_released = true
					}
					mut state := ServiceWindowState{}
					mut state_valid := false
					if observed := backend.service_window_state(backend.windows[index].id) {
						state = observed
						state_valid = true
					}
					events << backend.queued_configure_observation_events(index, width, height,
						state, state_valid)
					if mouse_lock_released {
						mouse_lock := backend.queued_service_state_event(index, .mouse_lock) or {
							continue
						}
						events << mouse_lock
					}
				}
				x11_map_notify {
					index := backend.window_record_index_for_event(&event) or { continue }
					show := backend.queued_service_state_event(index, .show) or { continue }
					events << show
				}
				x11_unmap_notify {
					index := backend.window_record_index_for_event(&event) or { continue }
					backend.release_mouse_lock(index)
					hide := backend.queued_service_state_event(index, .hide) or { continue }
					events << hide
				}
				x11_destroy_notify {
					native_window := unsafe { event.xdestroywindow.window }
					index := backend.window_record_index_for_native(native_window) or { continue }
					id := backend.windows[index].id
					backend.purge_xdnd_window(id, native_window)
					backend.windows[index].native_destroyed = true
					events << queued_lifecycle_event(Event{
						kind:      .window_destroyed
						window_id: id
					})
				}
				x11_key_press {
					index := backend.window_record_index_for_event(&event) or { continue }
					events << backend.queued_key_press_event(index, &event)
				}
				x11_key_release {
					if C.v_multiwindow_x11_is_auto_repeat_release(backend.display, &event) != 0 {
						continue
					}
					index := backend.window_record_index_for_event(&event) or { continue }
					events << backend.queued_key_release_event(index, &event)
				}
				x11_button_press {
					index := backend.window_record_index_for_event(&event) or { continue }
					events << backend.queued_button_press_event(index, &event)
				}
				x11_button_release {
					index := backend.window_record_index_for_event(&event) or { continue }
					events << backend.queued_button_release_event(index, &event)
				}
				x11_motion_notify {
					index := backend.window_record_index_for_event(&event) or { continue }
					events << backend.queued_mouse_position_event(index, &event, .mouse_move, false)
				}
				x11_enter_notify {
					index := backend.window_record_index_for_event(&event) or { continue }
					if backend.windows[index].mouse_buttons == 0 {
						events << backend.queued_mouse_position_event(index, &event, .mouse_enter, true)
					}
				}
				x11_leave_notify {
					index := backend.window_record_index_for_event(&event) or { continue }
					if backend.windows[index].mouse_buttons == 0 {
						events << backend.queued_mouse_position_event(index, &event, .mouse_leave, true)
					}
				}
				x11_focus_in {
					if C.v_multiwindow_x11_is_notify_grab_or_ungrab(C.v_multiwindow_x11_focus_mode(&event)) != 0 {
						continue
					}
					index := backend.window_record_index_for_event(&event) or { continue }
					C.v_multiwindow_x11_set_ic_focus(backend.windows[index].xic)
					events << queued_input_event(backend.input_event_from_record(backend.windows[index],
						.focused))
					focus := backend.queued_service_state_event(index, .focus) or { continue }
					events << focus
				}
				x11_focus_out {
					if C.v_multiwindow_x11_is_notify_grab_or_ungrab(C.v_multiwindow_x11_focus_mode(&event)) != 0 {
						continue
					}
					index := backend.window_record_index_for_event(&event) or { continue }
					C.v_multiwindow_x11_unset_ic_focus(backend.windows[index].xic)
					backend.release_mouse_lock(index)
					backend.clear_input_state(index)
					events << queued_input_event(backend.input_event_from_record(backend.windows[index],
						.unfocused))
					focus := backend.queued_service_state_event(index, .focus) or { continue }
					events << focus
				}
				x11_property_notify {
					events << backend.queued_clipboard_property_events(&event)
					events << backend.queued_xdnd_property_events(&event)
					property := C.v_multiwindow_x11_property_atom(&event)
					native_window := C.v_multiwindow_x11_event_window(&event)
					if native_window == backend.root {
						if property == backend.net_supported {
							events << backend.queued_ewmh_capability_events()
							continue
						}
						if property == backend.net_workarea
							|| property == backend.net_current_desktop {
							backend.monitor_snapshot_dirty = true
							continue
						}
					}
					if C.v_multiwindow_x11_property_state(&event) != x11_property_new_value
						|| (property != backend.wm_state && property != backend.net_wm_state) {
						continue
					}
					index := backend.window_record_index_for_event(&event) or { continue }
					if property == backend.wm_state {
						state := backend.window_state(backend.windows[index].window) or { continue }
						if state != backend.windows[index].window_state {
							backend.windows[index].window_state = state
							if state == x11_iconic_state {
								events << queued_input_event(backend.input_event_from_record(backend.windows[index],
									.iconified))
							} else if state == x11_normal_state {
								events << queued_input_event(backend.input_event_from_record(backend.windows[index],
									.restored))
							}
						}
					}
					observed := backend.queued_observed_state_transitions(index, property) or {
						continue
					}
					events << observed
				}
				x11_selection_request {
					backend.handle_clipboard_selection_request(&event)
				}
				x11_selection_clear {
					if unsafe { event.xselectionclear.selection } == backend.clipboard {
						owner := C.XGetSelectionOwner(backend.display, backend.clipboard)
						if backend.clipboard_owner_window != X11NativeWindow(0)
							&& owner != backend.clipboard_owner_window {
							events << backend.clear_clipboard_state(.cancelled,
								err_clipboard_selection_lost)
						}
					}
				}
				x11_selection_notify {
					if unsafe { event.xselection.selection } == backend.clipboard {
						events << backend.queued_clipboard_selection_events(&event)
					} else {
						events << backend.queued_xdnd_selection_events(&event)
					}
				}
				else {}
			}
		}
		backend.drain_checked_clipboard_transfer_events()
		if backend.monitor_snapshot_dirty {
			refresh_events := backend.queued_randr_monitor_events() or { []QueuedEvent{} }
			if refresh_events.len > 0 {
				events << refresh_events
				backend.monitor_snapshot_dirty = false
			}
		}
		events << backend.expire_clipboard_operations(vtime.sys_mono_now())
		backend.expire_xdnd_drop(vtime.sys_mono_now())
		if pending_clipboard_terminal_count > 0 {
			if pending_clipboard_terminal_count >= backend.pending_clipboard_terminal_events.len {
				backend.pending_clipboard_terminal_events.clear()
			} else {
				backend.pending_clipboard_terminal_events =
					backend.pending_clipboard_terminal_events[pending_clipboard_terminal_count..].clone()
			}
		}
	}
	return events
}

fn (backend &X11Backend) input_event_from_record(record X11WindowRecord, kind InputEventKind) InputEvent {
	return backend.input_event_with_payload(record, kind, 0, false, 0, x11_invalid_mouse_button, 0,
		0)
}

fn (backend &X11Backend) input_event_with_payload(record X11WindowRecord, kind InputEventKind, key_code int, key_repeat bool, modifiers u32, mouse_button int, scroll_x f32, scroll_y f32) InputEvent {
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
		framebuffer_width:  record.width
		framebuffer_height: record.height
	}
}

fn (backend &X11Backend) input_char_event(record X11WindowRecord, char_code u32, key_repeat bool, modifiers u32) InputEvent {
	return InputEvent{
		kind:               .char
		window_id:          record.id
		char_code:          char_code
		key_repeat:         key_repeat
		modifiers:          modifiers
		window_width:       record.width
		window_height:      record.height
		framebuffer_width:  record.width
		framebuffer_height: record.height
	}
}

fn (backend &X11Backend) input_files_dropped_event(record X11WindowRecord, files []string) InputEvent {
	return InputEvent{
		kind:               .files_dropped
		window_id:          record.id
		mouse_x:            record.mouse_x
		mouse_y:            record.mouse_y
		mouse_dx:           record.mouse_dx
		mouse_dy:           record.mouse_dy
		window_width:       record.width
		window_height:      record.height
		framebuffer_width:  record.width
		framebuffer_height: record.height
		mouse_button:       x11_invalid_mouse_button
		dropped_files:      files.clone()
	}
}

$if linux && x_multiwindow_x11 ? {
	fn (mut backend X11Backend) clear_input_state(index int) {
		backend.windows[index].mouse_buttons = 0
		for i in 0 .. 256 {
			backend.windows[index].key_repeat[i] = false
		}
	}

	fn (mut backend X11Backend) queued_key_press_event(index int, event &C.XEvent) []QueuedEvent {
		mut events := []QueuedEvent{}
		key_code := C.v_multiwindow_x11_key_code(event, &backend.keycodes[0], 256)
		native_keycode := int(C.v_multiwindow_x11_event_keycode(event))
		mut repeat := false
		if native_keycode >= 0 && native_keycode < 256 {
			repeat = backend.windows[index].key_repeat[native_keycode]
			backend.windows[index].key_repeat[native_keycode] = true
		}
		mut modifiers := u32(C.v_multiwindow_x11_modifiers(C.v_multiwindow_x11_event_state(event)))
		if key_code != 0 {
			modifiers |= u32(C.v_multiwindow_x11_key_modifier_bit(key_code))
			input := backend.input_event_with_payload(backend.windows[index], .key_down, key_code,
				repeat, modifiers, x11_invalid_mouse_button, 0, 0)
			events << queued_input_event(input)
			if x11_is_clipboard_paste(key_code, modifiers) {
				events << queued_input_event(backend.input_event_with_payload(backend.windows[index],
					.clipboard_pasted, 0, false, modifiers, x11_invalid_mouse_button, 0, 0))
			}
		}
		backend.append_key_press_char_events(mut events, backend.windows[index], event, repeat,
			modifiers)
		return events
	}

	fn (backend &X11Backend) append_key_press_char_events(mut events []QueuedEvent, record X11WindowRecord, event &C.XEvent, repeat bool, modifiers u32) {
		mut inline_char_codes := [x11_inline_char_codes]u32{}
		mut required_char_codes := 0
		mut char_count := C.v_multiwindow_x11_char_codes(record.xic, event,
			unsafe { &inline_char_codes[0] }, x11_inline_char_codes, &required_char_codes)
		if required_char_codes > x11_inline_char_codes {
			mut char_codes := []u32{len: required_char_codes}
			char_count = C.v_multiwindow_x11_char_codes(record.xic, event,
				unsafe { &char_codes[0] }, required_char_codes, &required_char_codes)
			for i in 0 .. char_count {
				events << queued_input_event(backend.input_char_event(record, char_codes[i],
					repeat, modifiers))
			}
			return
		}
		for i in 0 .. char_count {
			events << queued_input_event(backend.input_char_event(record, inline_char_codes[i],
				repeat, modifiers))
		}
	}

	fn (mut backend X11Backend) queued_key_release_event(index int, event &C.XEvent) []QueuedEvent {
		mut events := []QueuedEvent{}
		key_code := C.v_multiwindow_x11_key_code(event, &backend.keycodes[0], 256)
		if key_code == 0 {
			return events
		}
		native_keycode := int(C.v_multiwindow_x11_event_keycode(event))
		if native_keycode >= 0 && native_keycode < 256 {
			backend.windows[index].key_repeat[native_keycode] = false
		}
		mut modifiers := C.v_multiwindow_x11_modifiers(C.v_multiwindow_x11_event_state(event))
		modifiers &= ~C.v_multiwindow_x11_key_modifier_bit(key_code)
		input := backend.input_event_with_payload(backend.windows[index], .key_up, key_code, false,
			u32(modifiers), x11_invalid_mouse_button, 0, 0)
		events << queued_input_event(input)
		return events
	}

	fn (mut backend X11Backend) queued_button_press_event(index int, event &C.XEvent) []QueuedEvent {
		mut events := []QueuedEvent{}
		x := C.v_multiwindow_x11_event_x(event)
		y := C.v_multiwindow_x11_event_y(event)
		backend.update_mouse_position(index, x, y, false)
		button := C.v_multiwindow_x11_event_button(event)
		mut modifiers := C.v_multiwindow_x11_modifiers(C.v_multiwindow_x11_event_state(event))
		match button {
			x11_scroll_up {
				events << queued_input_event(backend.scroll_event(index, 0.0, 1.0, u32(modifiers)))
			}
			x11_scroll_down {
				events << queued_input_event(backend.scroll_event(index, 0.0, -1.0, u32(modifiers)))
			}
			x11_scroll_right {
				events << queued_input_event(backend.scroll_event(index, 1.0, 0.0, u32(modifiers)))
			}
			x11_scroll_left {
				events << queued_input_event(backend.scroll_event(index, -1.0, 0.0, u32(modifiers)))
			}
			else {
				mouse_button := C.v_multiwindow_x11_mouse_button(button)
				if mouse_button != x11_invalid_mouse_button {
					modifiers |= C.v_multiwindow_x11_button_modifier_bit(mouse_button)
					backend.windows[index].mouse_buttons |= u8(1 << mouse_button)
					input := backend.input_event_with_payload(backend.windows[index], .mouse_down,
						0, false, u32(modifiers), mouse_button, 0, 0)
					events << queued_input_event(input)
				}
			}
		}

		return events
	}

	fn (mut backend X11Backend) queued_button_release_event(index int, event &C.XEvent) []QueuedEvent {
		mut events := []QueuedEvent{}
		x := C.v_multiwindow_x11_event_x(event)
		y := C.v_multiwindow_x11_event_y(event)
		backend.update_mouse_position(index, x, y, false)
		mouse_button := C.v_multiwindow_x11_mouse_button(C.v_multiwindow_x11_event_button(event))
		if mouse_button == x11_invalid_mouse_button {
			return events
		}
		mut modifiers := C.v_multiwindow_x11_modifiers(C.v_multiwindow_x11_event_state(event))
		modifiers &= ~C.v_multiwindow_x11_button_modifier_bit(mouse_button)
		backend.windows[index].mouse_buttons &= ~u8(1 << mouse_button)
		input := backend.input_event_with_payload(backend.windows[index], .mouse_up, 0, false,
			u32(modifiers), mouse_button, 0, 0)
		events << queued_input_event(input)
		return events
	}

	fn (mut backend X11Backend) queued_mouse_position_event(index int, event &C.XEvent, kind InputEventKind, clear_delta bool) QueuedEvent {
		x := C.v_multiwindow_x11_event_x(event)
		y := C.v_multiwindow_x11_event_y(event)
		if kind == .mouse_move && backend.windows[index].mouse_locked {
			center_x := backend.windows[index].mouse_lock_center_x
			center_y := backend.windows[index].mouse_lock_center_y
			backend.windows[index].mouse_dx = f32(x - center_x)
			backend.windows[index].mouse_dy = f32(y - center_y)
			backend.windows[index].mouse_x = f32(center_x)
			backend.windows[index].mouse_y = f32(center_y)
			backend.windows[index].mouse_pos_valid = true
			if x != center_x || y != center_y {
				if !backend.recenter_locked_pointer(index, false) {
					input := backend.input_event_with_payload(backend.windows[index], kind, 0,
						false,
						u32(C.v_multiwindow_x11_modifiers(C.v_multiwindow_x11_event_state(event))),
						x11_invalid_mouse_button, 0, 0)
					backend.release_mouse_lock(index)
					return queued_input_event(input)
				}
			}
			input := backend.input_event_with_payload(backend.windows[index], kind, 0, false,
				u32(C.v_multiwindow_x11_modifiers(C.v_multiwindow_x11_event_state(event))),
				x11_invalid_mouse_button, 0, 0)
			return queued_input_event(input)
		}
		backend.update_mouse_position(index, x, y, clear_delta)
		input := backend.input_event_with_payload(backend.windows[index], kind, 0, false,
			u32(C.v_multiwindow_x11_modifiers(C.v_multiwindow_x11_event_state(event))),
			x11_invalid_mouse_button, 0, 0)
		return queued_input_event(input)
	}

	fn (mut backend X11Backend) update_mouse_position(index int, x int, y int, clear_delta bool) {
		new_x := f32(x)
		new_y := f32(y)
		if clear_delta || !backend.windows[index].mouse_pos_valid {
			backend.windows[index].mouse_x = new_x
			backend.windows[index].mouse_y = new_y
			backend.windows[index].mouse_dx = 0
			backend.windows[index].mouse_dy = 0
			backend.windows[index].mouse_pos_valid = true
			return
		}
		backend.windows[index].mouse_dx = new_x - backend.windows[index].mouse_x
		backend.windows[index].mouse_dy = new_y - backend.windows[index].mouse_y
		backend.windows[index].mouse_x = new_x
		backend.windows[index].mouse_y = new_y
	}

	fn (backend &X11Backend) scroll_event(index int, x f32, y f32, modifiers u32) InputEvent {
		return backend.input_event_with_payload(backend.windows[index], .mouse_scroll, 0, false,
			modifiers, x11_invalid_mouse_button, x, y)
	}

	fn (backend &X11Backend) window_record_index_for_event(event &C.XEvent) ?int {
		return backend.window_record_index_for_native(C.v_multiwindow_x11_event_window(event))
	}

	fn (backend &X11Backend) window_state(window X11NativeWindow) ?int {
		mut result := x11_normal_state
		if C.v_multiwindow_x11_checked_wm_state(backend.checked_connection, window,
			backend.wm_state, &result) == 0 {
			return none
		}
		return result
	}

	fn (mut backend X11Backend) announce_xdnd_for_window(window X11NativeWindow) {
		if backend.xdnd_aware == X11NativeAtom(0) {
			return
		}
		version := X11NativeAtom(x11_xdnd_version)
		C.XChangeProperty(backend.display, window, backend.xdnd_aware, X11NativeAtom(4), 32,
			x11_prop_mode_replace, unsafe { &u8(&version) }, 1)
	}

	fn (backend &X11Backend) clipboard_transfer_queue(requestor X11NativeWindow) X11ClipboardTransferQueue {
		if requestor == backend.root || backend.windows.any(it.window == requestor)
			|| backend.clipboard_reads.any(it.requestor == requestor) {
			return .xlib
		}
		return .checked
	}

	fn (mut backend X11Backend) handle_clipboard_selection_request(event &C.XEvent) {
		request := unsafe { event.xselectionrequest }
		if request.selection != backend.clipboard || request.owner != backend.clipboard_owner_window
			|| backend.clipboard_owner_window == X11NativeWindow(0) {
			return
		}
		property := if request.property == X11NativeAtom(0) {
			request.target
		} else {
			request.property
		}
		request_queue := backend.clipboard_transfer_queue(request.requestor)
		if request_queue == .checked {
			// A previous transfer's Delete/Destroy notifications live on the
			// separate checked queue. Drain that prefix before replacing the
			// (requestor, property) transaction so it cannot advance the new one.
			if C.v_multiwindow_x11_checked_barrier(backend.checked_connection) == 0
				|| !backend.drain_checked_clipboard_transfer_events_with_limit(x11_clipboard_checked_events_per_barrier) {
				backend.purge_clipboard_requestor(request.requestor)
				_ = C.v_multiwindow_x11_checked_selection_notify(backend.checked_connection,
					request.requestor, request.selection, request.target, X11NativeAtom(0),
					request.time)
				return
			}
		}
		pair_active := backend.clipboard_transfers.any(it.requestor == request.requestor
			&& it.property == property)
		if pair_active {
			// PropertyNotify carries no transfer generation. Reject overlap on the
			// same pair so a late delete of the old chunk cannot advance a new reply.
			if C.v_multiwindow_x11_checked_selection_notify(backend.checked_connection,
				request.requestor, request.selection, request.target, X11NativeAtom(0),
				request.time) == 0 {
				backend.purge_clipboard_requestor(request.requestor)
			}
			return
		}
		mut response_property := X11NativeAtom(0)
		if request.target == backend.clipboard_targets {
			targets := [u32(backend.clipboard_targets), u32(backend.clipboard_utf8)]
			if C.v_multiwindow_x11_checked_change_property(backend.checked_connection,
				request.requestor, property, X11NativeAtom(4), u8(32), u32(targets.len),
				targets.data) != 0 {
				response_property = property
			}
		} else if request.target == backend.clipboard_utf8 {
			payload := backend.clipboard_text.bytes()
			if payload.len <= x11_clipboard_inline_bytes {
				if C.v_multiwindow_x11_checked_change_property(backend.checked_connection,
					request.requestor, property, request.target, u8(8), u32(payload.len),
					payload.data) != 0 {
					response_property = property
				}
			} else if payload.len <= x11_clipboard_max_bytes {
				queue := request_queue
				if queue == .xlib {
					backend.release_clipboard_requestor_subscription_if_unused(request.requestor)
				}
				if !backend.clipboard_can_admit(payload.len) {
					backend.release_clipboard_requestor_subscription_if_unused(request.requestor)
					if C.v_multiwindow_x11_checked_selection_notify(backend.checked_connection,
						request.requestor, request.selection, request.target, X11NativeAtom(0),
						request.time) == 0 {
						backend.purge_clipboard_requestor(request.requestor)
					}
					return
				}
				if queue == .checked
					&& C.v_multiwindow_x11_checked_property_changes(backend.checked_connection, request.requestor, 1) == 0 {
					backend.purge_clipboard_requestor(request.requestor)
					_ = C.v_multiwindow_x11_checked_property_changes(backend.checked_connection,
						request.requestor, 0)
					return
				}
				payload_size := u32(payload.len)
				if C.v_multiwindow_x11_checked_change_property(backend.checked_connection, request.requestor, property, backend.clipboard_incr, u8(32), u32(1), &payload_size) == 0
					|| C.v_multiwindow_x11_checked_selection_notify(backend.checked_connection, request.requestor, request.selection, request.target, property, request.time) == 0 {
					backend.purge_clipboard_requestor(request.requestor)
					if queue == .checked {
						_ = C.v_multiwindow_x11_checked_property_changes(backend.checked_connection,
							request.requestor, 0)
					}
					return
				}
				backend.clipboard_transfers << X11ClipboardTransfer{
					requestor:   request.requestor
					property:    property
					target:      request.target
					data:        payload
					queue:       queue
					deadline_ns: vtime.sys_mono_now() + x11_clipboard_timeout_ns
				}
				return
			}
		}
		if C.v_multiwindow_x11_checked_selection_notify(backend.checked_connection,
			request.requestor, request.selection, request.target, response_property, request.time) == 0 {
			backend.purge_clipboard_requestor(request.requestor)
		}
	}

	fn (mut backend X11Backend) queued_clipboard_selection_events(event &C.XEvent) []QueuedEvent {
		if backend.clipboard_reads.len == 0 {
			return []QueuedEvent{}
		}
		read := backend.clipboard_reads[0]
		requestor := unsafe { event.xselection.requestor }
		selection := unsafe { event.xselection.selection }
		target := unsafe { event.xselection.target }
		property := unsafe { event.xselection.property }
		if selection != backend.clipboard || target != backend.clipboard_utf8
			|| requestor != read.requestor {
			return []QueuedEvent{}
		}
		if property == X11NativeAtom(0) {
			return backend.finish_clipboard_read(.failed, '', err_capability_unsupported)
		}
		if property != read.property {
			return []QueuedEvent{}
		}
		mut actual_type := X11NativeAtom(0)
		mut actual_format := 0
		mut item_count := X11NativeULong(0)
		mut bytes_after := X11NativeULong(0)
		mut data := &u8(unsafe { nil })
		status := C.XGetWindowProperty(backend.display, requestor, property, X11NativeLong(0), X11NativeLong((
			x11_clipboard_max_bytes + 3) / 4), 1, X11NativeAtom(0), &actual_type, &actual_format,
			&item_count, &bytes_after, &&u8(&data))
		if status != x11_success {
			if data != unsafe { nil } {
				C.XFree(data)
			}
			return backend.finish_clipboard_read(.failed, '', err_capability_unsupported)
		}
		if actual_type == backend.clipboard_incr && actual_format == 32 {
			advertised := if data != unsafe { nil } && item_count > 0 {
				u64(unsafe { *&X11NativeULong(data) })
			} else {
				u64(0)
			}
			if data != unsafe { nil } {
				C.XFree(data)
			}
			if advertised > u64(x11_clipboard_max_bytes)
				|| !backend.clipboard_can_reserve(int(advertised)) {
				return backend.finish_clipboard_read(.failed, '', err_clipboard_capacity)
			}
			backend.clipboard_reads[0].incremental = true
			backend.clipboard_reads[0].data.clear()
			backend.clipboard_reads[0].reserved_bytes = int(advertised)
			backend.clipboard_reads[0].deadline_ns = vtime.sys_mono_now() + x11_clipboard_timeout_ns
			return []QueuedEvent{}
		}
		valid_type := actual_type == backend.clipboard_utf8 && actual_format == 8
			&& bytes_after == X11NativeULong(0)
		capacity_ok := item_count <= X11NativeULong(x11_clipboard_max_bytes)
			&& backend.clipboard_can_reserve(int(item_count))
		text := if valid_type && capacity_ok && data != unsafe { nil } && item_count > 0 {
			unsafe { tos(data, int(item_count)).clone() }
		} else {
			''
		}
		if data != unsafe { nil } {
			C.XFree(data)
		}
		if !capacity_ok {
			return backend.finish_clipboard_read(.failed, '', err_clipboard_capacity)
		}
		if !valid_type {
			return backend.finish_clipboard_read(.failed, '', err_capability_unsupported)
		}
		return backend.finish_clipboard_read(.ready, text, '')
	}

	fn (mut backend X11Backend) queued_clipboard_property_events(event &C.XEvent) []QueuedEvent {
		window := unsafe { event.xproperty.window }
		property := unsafe { event.xproperty.atom }
		state := unsafe { event.xproperty.state }
		if state == x11_property_delete {
			backend.advance_clipboard_transfer(window, property, .xlib)
			return []QueuedEvent{}
		}
		if state != x11_property_new_value || backend.clipboard_reads.len == 0 {
			return []QueuedEvent{}
		}
		read := backend.clipboard_reads[0]
		if !read.incremental || read.requestor != window || read.property != property {
			return []QueuedEvent{}
		}
		mut actual_type := X11NativeAtom(0)
		mut actual_format := 0
		mut item_count := X11NativeULong(0)
		mut bytes_after := X11NativeULong(0)
		mut data := &u8(unsafe { nil })
		status := C.XGetWindowProperty(backend.display, window, property, X11NativeLong(0), X11NativeLong((
			x11_clipboard_chunk_bytes + 3) / 4), 1, X11NativeAtom(0), &actual_type, &actual_format,
			&item_count, &bytes_after, &&u8(&data))
		valid := status == x11_success && actual_type == backend.clipboard_utf8
			&& actual_format == 8 && bytes_after == X11NativeULong(0)
		if !valid {
			if data != unsafe { nil } {
				C.XFree(data)
			}
			return backend.finish_clipboard_read(.failed, '', err_capability_unsupported)
		}
		if item_count == 0 {
			if data != unsafe { nil } {
				C.XFree(data)
			}
			text := if backend.clipboard_reads[0].data.len == 0 {
				''
			} else {
				unsafe {
					tos(backend.clipboard_reads[0].data.data, backend.clipboard_reads[0].data.len).clone()
				}
			}
			return backend.finish_clipboard_read(.ready, text, '')
		}
		next_reserved := backend.clipboard_incremental_reservation_after_chunk(item_count) or {
			if data != unsafe { nil } {
				C.XFree(data)
			}
			return backend.finish_clipboard_read(.failed, '', err_clipboard_capacity)
		}
		for index in 0 .. int(item_count) {
			backend.clipboard_reads[0].data << unsafe { data[index] }
		}
		backend.clipboard_reads[0].reserved_bytes = next_reserved
		backend.clipboard_reads[0].deadline_ns = vtime.sys_mono_now() + x11_clipboard_timeout_ns
		if data != unsafe { nil } {
			C.XFree(data)
		}
		return []QueuedEvent{}
	}

	fn (backend &X11Backend) clipboard_read_terminal_event(read X11ClipboardRead, status ServiceStatus, text string, message string) QueuedEvent {
		_ = backend
		return queued_service_event(ServiceEvent{
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
		})
	}

	fn (mut backend X11Backend) destroy_clipboard_requestor(requestor X11NativeWindow) {
		if requestor != X11NativeWindow(0) && backend.display != unsafe { nil } {
			C.XDestroyWindow(backend.display, requestor)
		}
	}

	fn (mut backend X11Backend) finish_clipboard_read(status ServiceStatus, text string, message string) []QueuedEvent {
		read := backend.clipboard_reads[0]
		mut terminal_status := status
		mut terminal_text := text
		mut terminal_message := message
		if terminal_status == .ready {
			read_charge := if read.reserved_bytes > read.data.len {
				read.reserved_bytes
			} else {
				read.data.len
			}
			if !x11_clipboard_ready_storage_fits(backend.clipboard_pending_bytes(), read_charge,
				terminal_text.len) {
				terminal_status = .failed
				terminal_text = ''
				terminal_message = err_clipboard_capacity
			}
		}
		backend.destroy_clipboard_requestor(read.requestor)
		backend.clipboard_reads.delete(0)
		if terminal_status == .ready && terminal_text.len > 0 {
			backend.clipboard_retained << X11ClipboardRetainedCharge{
				request: read.request
				window:  read.window
				bytes:   terminal_text.len
			}
		}
		mut events := [
			backend.clipboard_read_terminal_event(read, terminal_status, terminal_text,
				terminal_message),
		]
		events << backend.start_queued_clipboard_reads()
		return events
	}

	fn (mut backend X11Backend) start_queued_clipboard_reads() []QueuedEvent {
		mut events := []QueuedEvent{}
		for backend.clipboard_reads.len > 0 {
			mut start_error := ''
			backend.start_next_clipboard_read() or { start_error = err.msg() }
			if start_error == '' {
				break
			}
			failed := backend.clipboard_reads[0]
			backend.destroy_clipboard_requestor(failed.requestor)
			backend.clipboard_reads.delete(0)
			events << backend.clipboard_read_terminal_event(failed, .failed, '', start_error)
		}
		return events
	}

	fn (mut backend X11Backend) clear_clipboard_state(status ServiceStatus, message string) []QueuedEvent {
		mut events := []QueuedEvent{cap: backend.clipboard_reads.len}
		for read in backend.clipboard_reads {
			backend.destroy_clipboard_requestor(read.requestor)
			events << backend.clipboard_read_terminal_event(read, status, '', message)
		}
		backend.clipboard_reads.clear()
		backend.clear_clipboard_transfers()
		backend.clipboard_owner_window = X11NativeWindow(0)
		backend.clipboard_owner_id = WindowId{}
		backend.clipboard_text = ''
		return events
	}

	fn (mut backend X11Backend) release_clipboard_requestor_subscription_if_unused(requestor X11NativeWindow) {
		if backend.clipboard_transfers.any(it.requestor == requestor && it.queue == .checked) {
			return
		}
		_ = C.v_multiwindow_x11_checked_property_changes(backend.checked_connection, requestor, 0)
	}

	fn (mut backend X11Backend) purge_clipboard_requestor(requestor X11NativeWindow) {
		mut removed_checked := false
		mut retained := []X11ClipboardTransfer{cap: backend.clipboard_transfers.len}
		for transfer in backend.clipboard_transfers {
			if transfer.requestor == requestor {
				removed_checked = removed_checked || transfer.queue == .checked
				continue
			}
			retained << transfer
		}
		backend.clipboard_transfers = retained
		if removed_checked {
			_ =
				C.v_multiwindow_x11_checked_property_changes(backend.checked_connection, requestor, 0)
		}
	}

	fn (mut backend X11Backend) clear_clipboard_transfers() {
		for backend.clipboard_transfers.len > 0 {
			requestor := backend.clipboard_transfers[0].requestor
			backend.purge_clipboard_requestor(requestor)
		}
	}

	fn (mut backend X11Backend) expire_clipboard_operations(now i64) []QueuedEvent {
		mut index := backend.clipboard_transfers.len
		for index > 0 {
			index--
			deadline := backend.clipboard_transfers[index].deadline_ns
			if deadline != 0 && deadline <= now {
				requestor := backend.clipboard_transfers[index].requestor
				queue := backend.clipboard_transfers[index].queue
				backend.clipboard_transfers.delete(index)
				if queue == .checked {
					backend.release_clipboard_requestor_subscription_if_unused(requestor)
				}
			}
		}
		if backend.clipboard_reads.len > 0 {
			deadline := backend.clipboard_reads[0].deadline_ns
			if deadline != 0 && deadline <= now {
				return backend.finish_clipboard_read(.failed, '', err_clipboard_timeout)
			}
		}
		return []QueuedEvent{}
	}

	fn (mut backend X11Backend) purge_clipboard_window(id WindowId, native X11NativeWindow) {
		owner_removed := backend.clipboard_owner_id == id
		mut removed_active := false
		mut retained_reads := []X11ClipboardRead{cap: backend.clipboard_reads.len}
		for index, read in backend.clipboard_reads {
			if read.window == id || read.requestor == native {
				backend.destroy_clipboard_requestor(read.requestor)
				removed_active = removed_active || index == 0
				continue
			}
			retained_reads << read
		}
		backend.clipboard_reads = retained_reads
		if owner_removed {
			backend.clear_clipboard_transfers()
		} else {
			backend.purge_clipboard_requestor(native)
		}
		if owner_removed {
			backend.clipboard_owner_window = X11NativeWindow(0)
			backend.clipboard_owner_id = WindowId{}
			backend.clipboard_text = ''
		}
		if removed_active && backend.clipboard_reads.len > 0 {
			backend.pending_clipboard_terminal_events << backend.start_queued_clipboard_reads()
		}
	}

	fn (mut backend X11Backend) advance_clipboard_transfer(requestor X11NativeWindow, property X11NativeAtom, queue X11ClipboardTransferQueue) {
		for index, transfer in backend.clipboard_transfers {
			if transfer.requestor != requestor || transfer.property != property
				|| transfer.queue != queue {
				continue
			}
			remaining := transfer.data.len - transfer.offset
			if remaining <= 0 {
				if C.v_multiwindow_x11_checked_change_property(backend.checked_connection,
					requestor, property, transfer.target, u8(8), u32(0), unsafe { nil }) == 0 {
					backend.purge_clipboard_requestor(requestor)
					return
				}
				backend.clipboard_transfers.delete(index)
				if queue == .checked {
					backend.release_clipboard_requestor_subscription_if_unused(requestor)
				}
			} else {
				count := if remaining < x11_clipboard_chunk_bytes {
					remaining
				} else {
					x11_clipboard_chunk_bytes
				}
				if C.v_multiwindow_x11_checked_change_property(backend.checked_connection,
					requestor, property, transfer.target, u8(8), u32(count),
					unsafe { &transfer.data[transfer.offset] }) == 0 {
					backend.purge_clipboard_requestor(requestor)
					return
				}
				backend.clipboard_transfers[index].offset += count
				backend.clipboard_transfers[index].deadline_ns = vtime.sys_mono_now() +
					x11_clipboard_timeout_ns
			}
			return
		}
	}

	fn (mut backend X11Backend) drain_checked_clipboard_transfer_events_with_limit(limit int) bool {
		for _ in 0 .. limit {
			mut requestor := X11NativeWindow(0)
			mut property := X11NativeAtom(0)
			status := C.v_multiwindow_x11_poll_checked_property_delete(backend.checked_connection,
				&requestor, &property)
			if status == 0 {
				return true
			}
			if status < 0 {
				mut retained := []X11ClipboardTransfer{cap: backend.clipboard_transfers.len}
				for transfer in backend.clipboard_transfers {
					if transfer.queue == .xlib {
						retained << transfer
					}
				}
				backend.clipboard_transfers = retained
				return false
			}
			if status == 1 {
				backend.advance_clipboard_transfer(requestor, property, .checked)
			} else if status == 3 {
				backend.purge_clipboard_requestor(requestor)
			}
		}
		return false
	}

	fn (mut backend X11Backend) drain_checked_clipboard_transfer_events() {
		_ =
			backend.drain_checked_clipboard_transfer_events_with_limit(x11_clipboard_checked_events_per_poll)
	}

	fn (mut backend X11Backend) queued_xdnd_client_message_events(event &C.XEvent) []QueuedEvent {
		message_type := unsafe { event.xclient.message_type }
		if message_type == backend.xdnd_enter {
			backend.handle_xdnd_enter(event)
		} else if message_type == backend.xdnd_position {
			backend.handle_xdnd_position(event)
		} else if message_type == backend.xdnd_drop {
			backend.handle_xdnd_drop(event)
		} else if message_type == backend.xdnd_leave {
			backend.clear_xdnd_hover_state()
		}
		return []QueuedEvent{}
	}

	fn (mut backend X11Backend) handle_xdnd_enter(event &C.XEvent) {
		backend.cancel_xdnd_drop()
		backend.xdnd_source = unsafe { X11NativeWindow(event.xclient.data.l[0]) }
		backend.xdnd_target = unsafe { event.xclient.window }
		backend.xdnd_version = unsafe { event.xclient.data.l[1] >> 24 }
		backend.xdnd_format = X11NativeAtom(0)
		if backend.xdnd_version > x11_xdnd_version {
			return
		}
		is_list := (unsafe { event.xclient.data.l[1] } & X11NativeLong(1)) != 0
		if is_list {
			backend.xdnd_format = backend.xdnd_format_from_type_list()
			return
		}
		for i in 2 .. 5 {
			format := unsafe { X11NativeAtom(event.xclient.data.l[i]) }
			if format == backend.text_uri_list {
				backend.xdnd_format = backend.text_uri_list
				return
			}
		}
	}

	fn (mut backend X11Backend) handle_xdnd_position(event &C.XEvent) {
		if backend.xdnd_version > x11_xdnd_version || backend.xdnd_source == X11NativeWindow(0) {
			return
		}
		backend.xdnd_target = unsafe { event.xclient.window }
		backend.update_xdnd_mouse_position(event)
		backend.send_xdnd_status(backend.xdnd_format != X11NativeAtom(0))
	}

	fn (mut backend X11Backend) handle_xdnd_drop(event &C.XEvent) {
		source := unsafe { X11NativeWindow(event.xclient.data.l[0]) }
		requestor := unsafe { event.xclient.window }
		version := backend.xdnd_version
		if version > x11_xdnd_version || source == X11NativeWindow(0)
			|| source != backend.xdnd_source || requestor != backend.xdnd_target
			|| backend.xdnd_format == X11NativeAtom(0) {
			backend.send_xdnd_finished_to(source, requestor, version, false)
			backend.clear_xdnd_hover_state()
			return
		}
		index := backend.window_record_index_for_native(requestor) or {
			backend.send_xdnd_finished_to(source, requestor, version, false)
			backend.clear_xdnd_hover_state()
			return
		}
		time := if version >= 1 {
			unsafe { X11NativeULong(event.xclient.data.l[2]) }
		} else {
			X11NativeULong(0)
		}
		backend.xdnd_drop_state = X11XdndDrop{
			active:      true
			source:      source
			requestor:   requestor
			window:      backend.windows[index].id
			property:    backend.xdnd_selection
			target_type: backend.xdnd_format
			version:     version
			time:        time
			deadline_ns: vtime.sys_mono_now() + x11_xdnd_timeout_ns
		}
		backend.clear_xdnd_hover_state()
		C.XDeleteProperty(backend.display, requestor, backend.xdnd_drop_state.property)
		C.XConvertSelection(backend.display, backend.xdnd_selection,
			backend.xdnd_drop_state.target_type, backend.xdnd_drop_state.property, requestor, time)
		C.XFlush(backend.display)
	}

	fn (mut backend X11Backend) update_xdnd_mouse_position(event &C.XEvent) {
		index := backend.window_record_index_for_native(backend.xdnd_target) or { return }
		root_x, root_y := x11_xdnd_position_coords(unsafe { event.xclient.data.l[2] })
		mut window_x := root_x
		mut window_y := root_y
		mut child := X11NativeWindow(0)
		if backend.root != X11NativeWindow(0)
			&& C.XTranslateCoordinates(backend.display, backend.root, backend.xdnd_target, root_x, root_y, &window_x, &window_y, &child) != 0 {
			backend.update_mouse_position(index, window_x, window_y, false)
			return
		}
		backend.update_mouse_position(index, root_x, root_y, false)
	}

	fn (mut backend X11Backend) queued_xdnd_selection_events(event &C.XEvent) []QueuedEvent {
		requestor := unsafe { event.xselection.requestor }
		selection := unsafe { event.xselection.selection }
		target := unsafe { event.xselection.target }
		property := unsafe { event.xselection.property }
		event_time := unsafe { event.xselection.time }
		if !backend.xdnd_drop_state.active {
			if selection == backend.xdnd_selection && property == backend.xdnd_selection {
				backend.delete_xdnd_property_if_live(requestor, property)
			}
			return []QueuedEvent{}
		}
		drop := backend.xdnd_drop_state
		if selection != backend.xdnd_selection || requestor != drop.requestor
			|| target != drop.target_type
			|| (drop.version >= 1 && event_time != drop.time) {
			return []QueuedEvent{}
		}
		if property == X11NativeAtom(0) {
			backend.finish_xdnd_drop(false)
			return []QueuedEvent{}
		}
		if property != drop.property {
			return []QueuedEvent{}
		}
		mut actual_type := X11NativeAtom(0)
		mut actual_format := 0
		mut item_count := X11NativeULong(0)
		mut bytes_after := X11NativeULong(0)
		mut data := &u8(unsafe { nil })
		status := C.XGetWindowProperty(backend.display, requestor, property, X11NativeLong(0),
			X11NativeLong(x11_xdnd_max_payload_units), 0, X11NativeAtom(0), &actual_type,
			&actual_format, &item_count, &bytes_after, &&u8(&data))
		if status != x11_success {
			if data != unsafe { nil } {
				C.XFree(data)
			}
			backend.finish_xdnd_drop(false)
			return []QueuedEvent{}
		}
		if actual_type == backend.clipboard_incr {
			valid_incr := actual_format == 32 && item_count == X11NativeULong(1)
				&& bytes_after == X11NativeULong(0) && data != unsafe { nil }
			advertised := if valid_incr {
				u64(unsafe { *&X11NativeULong(data) })
			} else {
				u64(x11_xdnd_max_payload_bytes) + 1
			}
			if data != unsafe { nil } {
				C.XFree(data)
			}
			if !valid_incr || !backend.begin_xdnd_incremental(advertised) {
				backend.finish_xdnd_drop(false)
			} else {
				// Deleting a valid INCR header acknowledges that the source may send its first chunk.
				backend.delete_xdnd_property_if_live(drop.requestor, drop.property)
			}
			return []QueuedEvent{}
		}
		valid_inline := actual_type == backend.text_uri_list && actual_format == 8
			&& bytes_after == X11NativeULong(0)
			&& item_count <= X11NativeULong(x11_xdnd_max_payload_bytes)
		mut payload := []u8{}
		if valid_inline && item_count > X11NativeULong(0) && data != unsafe { nil } {
			payload = []u8{cap: int(item_count)}
			for index in 0 .. int(item_count) {
				payload << unsafe { data[index] }
			}
		}
		if data != unsafe { nil } {
			C.XFree(data)
		}
		if !valid_inline || (item_count > X11NativeULong(0) && payload.len == 0) {
			backend.finish_xdnd_drop(false)
			return []QueuedEvent{}
		}
		backend.xdnd_drop_state.data = payload
		return backend.finish_xdnd_payload()
	}

	fn (mut backend X11Backend) queued_xdnd_property_events(event &C.XEvent) []QueuedEvent {
		property_state := unsafe { event.xproperty.state }
		requestor := unsafe { event.xproperty.window }
		property := unsafe { event.xproperty.atom }
		if !backend.xdnd_drop_state.active {
			if property_state == x11_property_new_value && property == backend.xdnd_selection {
				backend.delete_xdnd_property_if_live(requestor, property)
			}
			return []QueuedEvent{}
		}
		if !backend.xdnd_drop_state.incremental || property_state != x11_property_new_value
			|| requestor != backend.xdnd_drop_state.requestor
			|| property != backend.xdnd_drop_state.property {
			if property_state == x11_property_new_value && property == backend.xdnd_selection
				&& requestor != backend.xdnd_drop_state.requestor {
				backend.delete_xdnd_property_if_live(requestor, property)
			}
			return []QueuedEvent{}
		}
		drop := backend.xdnd_drop_state
		mut actual_type := X11NativeAtom(0)
		mut actual_format := 0
		mut item_count := X11NativeULong(0)
		mut bytes_after := X11NativeULong(0)
		mut data := &u8(unsafe { nil })
		status := C.XGetWindowProperty(backend.display, drop.requestor, drop.property,
			X11NativeLong(0), X11NativeLong(x11_xdnd_max_payload_units), 0, X11NativeAtom(0),
			&actual_type, &actual_format, &item_count, &bytes_after, &&u8(&data))
		valid := status == x11_success && actual_type == backend.text_uri_list && actual_format == 8
			&& bytes_after == X11NativeULong(0)
			&& item_count <= X11NativeULong(x11_xdnd_max_payload_bytes)
		mut payload := []u8{}
		if valid && item_count > X11NativeULong(0) && data != unsafe { nil } {
			payload = []u8{cap: int(item_count)}
			for index in 0 .. int(item_count) {
				payload << unsafe { data[index] }
			}
		}
		if data != unsafe { nil } {
			C.XFree(data)
		}
		if !valid || (item_count > X11NativeULong(0) && payload.len == 0) {
			backend.finish_xdnd_drop(false)
			return []QueuedEvent{}
		}
		if payload.len == 0 {
			return backend.finish_xdnd_payload()
		}
		events := backend.accept_xdnd_incremental_chunk(payload)
		if backend.xdnd_drop_state.active {
			// Copy and bound the chunk before acknowledging the source's next write.
			backend.delete_xdnd_property_if_live(drop.requestor, drop.property)
		}
		return events
	}

	fn (mut backend X11Backend) begin_xdnd_incremental(advertised u64) bool {
		if !backend.xdnd_drop_state.active || advertised > u64(x11_xdnd_max_payload_bytes) {
			return false
		}
		backend.xdnd_drop_state.incremental = true
		backend.xdnd_drop_state.data.clear()
		backend.xdnd_drop_state.deadline_ns = vtime.sys_mono_now() + x11_xdnd_timeout_ns
		return true
	}

	fn (mut backend X11Backend) accept_xdnd_incremental_chunk(payload []u8) []QueuedEvent {
		if !backend.xdnd_drop_state.active || !backend.xdnd_drop_state.incremental {
			return []QueuedEvent{}
		}
		if payload.len == 0 {
			return backend.finish_xdnd_payload()
		}
		remaining := x11_xdnd_max_payload_bytes - backend.xdnd_drop_state.data.len
		if payload.len > remaining {
			backend.finish_xdnd_drop(false)
			return []QueuedEvent{}
		}
		backend.xdnd_drop_state.data << payload
		backend.xdnd_drop_state.deadline_ns = vtime.sys_mono_now() + x11_xdnd_timeout_ns
		return []QueuedEvent{}
	}

	fn (mut backend X11Backend) finish_xdnd_payload() []QueuedEvent {
		if !backend.xdnd_drop_state.active {
			return []QueuedEvent{}
		}
		drop := backend.xdnd_drop_state
		index := backend.window_record_index(drop.window) or {
			backend.finish_xdnd_drop(false)
			return []QueuedEvent{}
		}
		if backend.windows[index].window != drop.requestor
			|| backend.windows[index].native_destroyed {
			backend.finish_xdnd_drop(false)
			return []QueuedEvent{}
		}
		files := dropped_files_from_uri_list(drop.data.bytestr())
		mut events := []QueuedEvent{}
		if files.len > 0 {
			events << queued_input_event(backend.input_files_dropped_event(backend.windows[index],
				files))
		}
		// A valid terminal payload must be deleted and observed by the server before the
		// checked XCB connection sends XdndFinished.
		backend.delete_xdnd_property_and_sync_if_live(drop.requestor, drop.property)
		backend.finish_xdnd_drop_with_cleanup(files.len > 0, false, true)
		return events
	}

	fn (mut backend X11Backend) xdnd_format_from_type_list() X11NativeAtom {
		if backend.xdnd_source == X11NativeWindow(0) {
			return X11NativeAtom(0)
		}
		if C.v_multiwindow_x11_checked_property_has_atom(backend.checked_connection,
			backend.xdnd_source, backend.xdnd_type_list, backend.text_uri_list,
			u32(x11_xdnd_max_type_atoms)) == 0 {
			return X11NativeAtom(0)
		}
		return backend.text_uri_list
	}

	fn (backend &X11Backend) send_xdnd_status(accepted bool) {
		if backend.xdnd_source == X11NativeWindow(0) {
			return
		}
		mut reply := C.XEvent{}
		reply.@type = x11_client_message
		unsafe {
			reply.xclient.window = backend.xdnd_source
			reply.xclient.message_type = backend.xdnd_status
			reply.xclient.format = 32
			reply.xclient.data.l[0] = X11NativeLong(backend.xdnd_target)
			if accepted {
				reply.xclient.data.l[1] = X11NativeLong(1)
				if backend.xdnd_version >= 2 {
					reply.xclient.data.l[4] = X11NativeLong(backend.xdnd_action_copy)
				}
			}
		}
		C.XSendEvent(backend.display, backend.xdnd_source, 0, X11NativeLong(0), &reply)
		C.XFlush(backend.display)
	}

	fn (mut backend X11Backend) send_xdnd_finished_to(source X11NativeWindow, requestor X11NativeWindow, version X11NativeLong, accepted bool) {
		backend.xdnd_finished_count++
		backend.xdnd_terminal_order_sequence++
		backend.xdnd_last_finished_sequence = backend.xdnd_terminal_order_sequence
		backend.xdnd_last_finished_accepted = accepted
		if source == X11NativeWindow(0) || version < 2 || backend.display == unsafe { nil } {
			return
		}
		mut reply := C.XEvent{}
		reply.@type = x11_client_message
		unsafe {
			reply.xclient.window = source
			reply.xclient.message_type = backend.xdnd_finished
			reply.xclient.format = 32
			reply.xclient.data.l[0] = X11NativeLong(requestor)
			if accepted {
				reply.xclient.data.l[1] = X11NativeLong(1)
				reply.xclient.data.l[2] = X11NativeLong(backend.xdnd_action_copy)
			}
		}
		if C.v_multiwindow_x11_send_event_checked(backend.display, source, &reply) != 0 {
			backend.xdnd_wire_finished_count++
		}
	}

	fn (mut backend X11Backend) finish_xdnd_drop(accepted bool) {
		backend.finish_xdnd_drop_with_cleanup(accepted, true, true)
	}

	fn (mut backend X11Backend) finish_xdnd_drop_with_cleanup(accepted bool, requestor_alive bool, reply bool) {
		if !backend.xdnd_drop_state.active {
			return
		}
		drop := backend.xdnd_drop_state
		backend.xdnd_drop_state = X11XdndDrop{}
		backend.xdnd_last_requestor = drop.requestor
		backend.xdnd_last_property = drop.property
		backend.xdnd_last_time = drop.time
		if reply {
			backend.send_xdnd_finished_to(drop.source, drop.requestor, drop.version, accepted)
		}
		if requestor_alive {
			backend.delete_xdnd_property_if_live(drop.requestor, drop.property)
		}
	}

	fn (mut backend X11Backend) cancel_xdnd_drop() {
		backend.finish_xdnd_drop(false)
	}

	fn (mut backend X11Backend) expire_xdnd_drop(now i64) {
		if backend.xdnd_drop_state.active && backend.xdnd_drop_state.deadline_ns != 0
			&& backend.xdnd_drop_state.deadline_ns <= now {
			backend.finish_xdnd_drop(false)
		}
	}

	fn (mut backend X11Backend) purge_xdnd_window(id WindowId, native X11NativeWindow) {
		if backend.xdnd_source == native || backend.xdnd_target == native {
			backend.clear_xdnd_hover_state()
		}
		if !backend.xdnd_drop_state.active {
			return
		}
		if backend.xdnd_drop_state.source == native {
			requestor_alive := backend.xdnd_drop_state.requestor != native
			backend.finish_xdnd_drop_with_cleanup(false, requestor_alive, false)
			return
		}
		if backend.xdnd_drop_state.window == id || backend.xdnd_drop_state.requestor == native {
			backend.finish_xdnd_drop_with_cleanup(false, false, true)
		}
	}

	fn (mut backend X11Backend) delete_xdnd_property_if_live(requestor X11NativeWindow, property X11NativeAtom) {
		if backend.display == unsafe { nil } || property == X11NativeAtom(0) {
			return
		}
		index := backend.window_record_index_for_native(requestor) or { return }
		if backend.windows[index].native_destroyed {
			return
		}
		backend.xdnd_property_delete_count++
		backend.xdnd_terminal_order_sequence++
		backend.xdnd_last_property_sequence = backend.xdnd_terminal_order_sequence
		C.XDeleteProperty(backend.display, requestor, property)
		C.XFlush(backend.display)
	}

	fn (mut backend X11Backend) delete_xdnd_property_and_sync_if_live(requestor X11NativeWindow, property X11NativeAtom) {
		if backend.display == unsafe { nil } || property == X11NativeAtom(0) {
			return
		}
		index := backend.window_record_index_for_native(requestor) or { return }
		if backend.windows[index].native_destroyed {
			return
		}
		backend.xdnd_property_delete_count++
		backend.xdnd_terminal_order_sequence++
		backend.xdnd_last_property_sequence = backend.xdnd_terminal_order_sequence
		C.XDeleteProperty(backend.display, requestor, property)
		C.XSync(backend.display, 0)
	}

	fn (mut backend X11Backend) clear_xdnd_hover_state() {
		backend.xdnd_source = X11NativeWindow(0)
		backend.xdnd_target = X11NativeWindow(0)
		backend.xdnd_format = X11NativeAtom(0)
		backend.xdnd_version = X11NativeLong(0)
	}
}

fn x11_xdnd_position_coords(value X11NativeLong) (int, int) {
	packed := u32(value)
	return x11_signed_16(packed >> 16), x11_signed_16(packed)
}

fn x11_signed_16(value u32) int {
	mut result := int(value & u32(0xffff))
	if result >= 0x8000 {
		result -= 0x10000
	}
	return result
}

fn (mut backend X11Backend) stop() ! {
	$if linux && x_multiwindow_x11 ? {
		mut cleanup_error := ''
		backend.cancel_xdnd_drop()
		backend.clear_xdnd_hover_state()
		_ = backend.clear_clipboard_state(.cancelled, err_app_stopped)
		backend.pending_clipboard_terminal_events.clear()
		backend.shutdown_renderer()
		if backend.checked_connection != unsafe { nil } {
			C.v_multiwindow_x11_close_checked_connection(backend.checked_connection)
			backend.checked_connection = unsafe { nil }
		}
		if !backend.retains_egl_ownership() {
			pending_cleanup_error := backend.cleanup_pending_window_once()
			cleanup_error = merge_backend_errors(cleanup_error, pending_cleanup_error)
			mut index := backend.windows.len
			for index > 0 {
				index--
				mut record := &backend.windows[index]
				destroy_native := !record.native_destroyed
				backend.release_window_record_resources(mut record, destroy_native) or {
					cleanup_error = merge_backend_errors(cleanup_error, err.msg())
				}
				if !record.retains_native_ownership() {
					backend.windows.delete(index)
				}
			}
		} else {
			cleanup_error = merge_backend_errors(cleanup_error,
				err_render_native_renderer_unavailable)
		}
		if !backend.retains_egl_ownership() && backend.windows.len == 0
			&& !backend.pending_window.retains_native_ownership() && backend.xim != unsafe { nil } {
			C.v_multiwindow_x11_close_im(backend.xim)
			backend.xim = unsafe { nil }
		}
		if !backend.retains_egl_ownership() && backend.xim == unsafe { nil }
			&& backend.windows.len == 0 && !backend.pending_window.retains_native_ownership()
			&& backend.display != unsafe { nil } {
			close_result := C.v_multiwindow_x11_close_display(backend.display)
			backend.display = unsafe { nil }
			for i in 0 .. backend.windows.len {
				backend.windows[i].window = X11NativeWindow(0)
				backend.windows[i].colormap = X11NativeColormap(0)
				backend.windows[i].cursor = X11NativeCursor(0)
				backend.windows[i].xic = unsafe { nil }
			}
			backend.windows.clear()
			backend.pending_window = X11WindowRecord{}
			if close_result != 0 {
				cleanup_error = merge_backend_errors(cleanup_error, err_x11_close_display_failed)
			}
		}
		backend.started = false
		if backend.retains_native_ownership() {
			cleanup_error = merge_backend_errors(cleanup_error,
				err_render_native_renderer_unavailable)
		}
		if cleanup_error != '' {
			return error(cleanup_error)
		}
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (backend &X11Backend) window_record_index(id WindowId) ?int {
	for i, record in backend.windows {
		if record.id == id {
			return i
		}
	}
	return none
}

fn (backend &X11Backend) window_record_index_for_native(window X11NativeWindow) ?int {
	for i, record in backend.windows {
		if record.window == window {
			return i
		}
	}
	return none
}

fn (backend &X11Backend) window_id_for_native(window X11NativeWindow) ?WindowId {
	for record in backend.windows {
		if record.window == window {
			return record.id
		}
	}
	return none
}

fn (backend &X11Backend) renderer_ready() bool {
	return backend.egl_display != unsafe { nil } && backend.egl_config != unsafe { nil }
		&& backend.egl_context != unsafe { nil } && backend.egl_display_ticket != 0
		&& backend.egl_context_ticket != 0 && backend.egl_thread_ticket != 0
		&& backend.render_health == .ready
}

fn (mut backend X11Backend) init_renderer() ! {
	$if linux && x_multiwindow_x11 ? {
		if backend.renderer_ready() {
			return
		}
		if backend.render_health.blocks_graphics() {
			return error(err_render_native_renderer_unavailable)
		}
		if backend.native_operations == unsafe { nil } {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return error(err_render_native_renderer_unavailable)
		}
		seed := NativeOperationSeed{
			call_site: .renderer_start
			scope:     .renderer
		}
		mut ordinals := backend.native_operations.reserve_renderer_attempt_ordinals(14) or {
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
		C.v_multiwindow_linux_egl_get_display(u64(usize(backend.display)), &raw)
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
			return error(err_x11_egl_display_failed)
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
			return error(err_x11_egl_display_failed)
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
			return error(err_x11_egl_context_failed)
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
			return error(err_x11_egl_context_failed)
		}
		config_context := ordinals.materialize(backend.native_operations, .egl, .config_choose,
			initialize_seed) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			backend.release_egl_lifetime()
			return error(err_render_native_renderer_unavailable)
		}
		C.v_multiwindow_linux_egl_choose_config(actual_display, &raw)
		actual_config := raw.handle
		config_result := backend.accept_egl_result(config_context, mut ordinals, initialize_seed,
			raw, .none)
		if !config_result.succeeded() {
			backend.release_egl_lifetime()
			return error(err_x11_egl_config_failed)
		}
		config_seed := seed.with_target_identity(actual_config)
		visual_context := ordinals.materialize(backend.native_operations, .egl, .config_visual,
			config_seed) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			backend.release_egl_lifetime()
			return error(err_render_native_renderer_unavailable)
		}
		C.v_multiwindow_linux_egl_get_native_visual(actual_display, actual_config, &raw)
		actual_visual := raw.selected_value
		visual_result := backend.accept_egl_result(visual_context, mut ordinals, config_seed, raw,
			.none)
		if !visual_result.succeeded() {
			backend.release_egl_lifetime()
			return error(err_x11_egl_config_failed)
		}
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
			return error(err_x11_egl_context_failed)
		}
		backend.egl_config = native_pointer(actual_config)
		backend.native_visual_id = int(actual_visual)
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

fn (mut backend X11Backend) shutdown_renderer() {
	backend.release_egl_lifetime()
	backend.native_visual_id = 0
	if !backend.render_health.blocks_graphics() {
		backend.render_health = .abandoned
	}
}

$if linux && x_multiwindow_x11 ? {
	fn (mut backend X11Backend) accept_egl_result(context NativeOperationContext, mut ordinals NativeOrdinalRange, seed NativeOperationSeed, raw C.VMultiwindowNativePrimitive, validation NativeLocalValidation) NativeRenderResult {
		capture := backend.native_operations.capture_egl_call(context, mut ordinals, seed, raw) or {
			backend.render_health = renderer_health_latch_unavailable(backend.render_health)
			return backend.record_egl_result(native_render_outcome(.egl, context.operation,
				.renderer, .renderer_unavailable, 0, 0, err_render_native_renderer_unavailable))
		}
		mut result := backend.native_operations.accept_egl(context, capture, validation)
		result = backend.record_egl_result(result)
		return result
	}

	fn (mut backend X11Backend) accept_egl_context_requirements(context NativeOperationContext, mut ordinals NativeOrdinalRange, seed NativeOperationSeed, raw C.VMultiwindowNativePrimitive, initialize NativeRenderResult) NativeRenderResult {
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

fn (mut backend X11Backend) accept_egl_query(context NativeOperationContext, raw C.VMultiwindowNativePrimitive, validation NativeLocalValidation) NativeRenderResult {
	capture := backend.native_operations.capture_call(context, raw)
	result := backend.native_operations.accept_egl(context, capture, validation)
	return backend.record_egl_result(result)
}

fn (mut backend X11Backend) accept_egl_binding_query(context NativeOperationContext, raw C.VMultiwindowNativePrimitive, draw NativeRenderResult, read NativeRenderResult, expected EglBindingIdentity) NativeRenderResult {
	capture := backend.native_operations.capture_call(context, raw)
	result := backend.native_operations.accept_egl_binding_context(context, capture, draw, read,
		native_identity(expected.surface), native_identity(expected.surface),
		native_identity(backend.egl_context))
	return backend.record_egl_result(result)
}

$if linux && x_multiwindow_x11 ? {
	fn (mut backend X11Backend) release_egl_surface_ticket(ticket_id u64, surface voidptr) NativeLifetimeReleaseAttempt {
		if ticket_id == 0 || surface == unsafe { nil }
			|| backend.native_operations == unsafe { nil } {
			return NativeLifetimeReleaseAttempt{}
		}
		return backend.native_operations.release_linux_egl_lifetime_ticket(ticket_id, .egl_surface,
			native_identity(surface), native_identity(backend.egl_display), backend.render_health)
	}
}

fn (mut backend X11Backend) record_egl_result(result NativeRenderResult) NativeRenderResult {
	backend.render_health = renderer_health_after_result(backend.render_health, result)
	backend.native_operations.record_health_latch(result.context, backend.render_health)
	if result.domain == .egl && result.native_code == i64(0x3008) {
		backend.native_operations.abandon_egl_display_lifetime(native_identity(backend.egl_display))
		backend.egl_binding = EglBindingIdentity{}
	}
	return result
}

fn (mut backend X11Backend) abandon_renderer_ownership() {
	backend.release_egl_lifetime()
	backend.native_visual_id = 0
	if !backend.render_health.blocks_graphics() {
		backend.render_health = .abandoned
	}
}

fn (mut backend X11Backend) release_egl_lifetime() {
	if backend.native_operations == unsafe { nil } {
		return
	}
	$if linux && x_multiwindow_x11 ? {
		if backend.native_operations != unsafe { nil } {
			mut children_terminal := true
			for i in 0 .. backend.windows.len {
				surface := backend.windows[i].egl_surface
				if surface != unsafe { nil } {
					release := backend.release_egl_surface_ticket(backend.windows[i].egl_surface_ticket,
						surface)
					if release.terminal {
						backend.windows[i].egl_surface = unsafe { nil }
						backend.windows[i].egl_surface_ticket = 0
					} else {
						children_terminal = false
					}
				}
			}
			pending_surface := backend.pending_window.egl_surface
			if pending_surface != unsafe { nil } {
				release := backend.release_egl_surface_ticket(backend.pending_window.egl_surface_ticket,
					pending_surface)
				if release.terminal {
					backend.pending_window.egl_surface = unsafe { nil }
					backend.pending_window.egl_surface_ticket = 0
				} else {
					children_terminal = false
				}
			}
			if backend.anchor_surface != unsafe { nil } {
				release := backend.release_egl_surface_ticket(backend.anchor_surface_ticket,
					backend.anchor_surface)
				if release.terminal {
					backend.anchor_surface = unsafe { nil }
					backend.anchor_surface_ticket = 0
				} else {
					children_terminal = false
				}
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
	if backend.egl_display_ticket == 0 && backend.egl_context_ticket == 0
		&& backend.egl_thread_ticket == 0 && backend.anchor_surface_ticket == 0 {
		backend.egl_config = unsafe { nil }
	}
	backend.egl_binding = EglBindingIdentity{}
}

fn (mut backend X11Backend) accept_native_render_window_loss(id WindowId) {
	index := backend.window_record_index(id) or { return }
	backend.windows[index].native_destroyed = true
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn (mut backend X11Backend) render_environment(id WindowId) !gfx.Environment {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			record := backend.windows[index]
			seed := NativeOperationSeed{
				presence_mask:     native_context_has_window | native_context_has_target_generation | native_context_has_target_identity
				call_site:         .renderer_start
				scope:             .window_target
				window:            record.id
				target_generation: record.render_target_generation
				target_identity:   native_identity(record.egl_surface)
			}
			outcome := backend.make_current(index, .window_target, seed)
			if !outcome.succeeded() {
				return native_render_error(outcome)
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

	fn (mut backend X11Backend) begin_render(id WindowId, candidate RenderWindowSnapshot, native_attempt NativeTargetAttempt) BackendFrameAttempt {
		$if linux && x_multiwindow_x11 ? {
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
			preparation := backend.ensure_window_render_surface(index, seed)
			if !preparation.succeeded() {
				return BackendFrameAttempt{
					outcome: preparation
				}
			}
			record := backend.windows[index]
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
				outcome: native_render_outcome(.egl, .window_surface_create, .renderer,
					.renderer_unavailable, 0, 0, err_backend_unsupported)
			}
		}
	}

	fn (mut backend X11Backend) end_render(frame RenderFrame) BackendFinalizeAttempt {
		$if linux && x_multiwindow_x11 ? {
			index := backend.window_record_index(frame.window_id) or {
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: native_render_outcome(.none, .swap_buffers, .window_target,
						.operation_failed, 0, 0, err_window_not_found)
				}
			}
			record := backend.windows[index]
			seed :=
				native_seed_for_frame(frame, .window_finalize).with_target_identity(native_identity(record.egl_surface))
			binding := backend.make_current(index, .window_target, seed)
			if !binding.succeeded() {
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: binding
				}
			}
			mut ordinals := backend.native_operations.reserve_ordinals(2) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: native_render_outcome(.egl, .swap_buffers, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
			}
			context := ordinals.materialize(backend.native_operations, .egl, .swap_buffers, seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: native_render_outcome(.egl, .swap_buffers, .renderer,
						.renderer_unavailable, 0, 0, err_render_native_renderer_unavailable)
				}
			}
			mut raw := C.VMultiwindowNativePrimitive{}
			C.v_multiwindow_linux_egl_swap_buffers(native_identity(backend.egl_display),
				native_identity(record.egl_surface), &raw)
			result := backend.accept_egl_result(context, mut ordinals, seed, raw, .none)
			if !result.succeeded() {
				failure := result
				desired := egl_window_binding(record.id, record.render_target_generation,
					record.egl_surface)
				outcome := backend.handle_window_egl_failure(index, failure, desired)
				return BackendFinalizeAttempt{
					status:  .not_presented
					outcome: outcome
				}
			}
			return BackendFinalizeAttempt{
				status:  .submitted
				outcome: result
			}
		} $else {
			_ = frame
			return BackendFinalizeAttempt{
				status:  .not_presented
				outcome: native_render_outcome(.egl, .swap_buffers, .renderer,
					.renderer_unavailable, 0, 0, err_backend_unsupported)
			}
		}
	}

	fn (mut backend X11Backend) make_current(index int, scope NativeRenderScope, boundary_seed NativeOperationSeed) NativeRenderResult {
		$if linux && x_multiwindow_x11 ? {
			if backend.render_health.blocks_graphics() {
				disposition := if backend.render_health == .lost {
					NativeRenderDisposition.renderer_lost
				} else {
					NativeRenderDisposition.renderer_unavailable
				}
				return backend.record_egl_result(native_render_outcome(.egl, .make_current,
					.renderer, disposition, 0, 0, err_render_native_renderer_lost))
			}
			record := backend.windows[index]
			if !backend.renderer_ready() || record.egl_surface == unsafe { nil } {
				return backend.record_egl_result(native_render_outcome(.egl, .make_current,
					.window_target, .target_lost, 0, 0, err_render_target_lost))
			}
			desired := egl_window_binding(record.id, record.render_target_generation,
				record.egl_surface)
			seed := NativeOperationSeed{
				...boundary_seed
				presence_mask:     boundary_seed.presence_mask | native_context_has_window | native_context_has_target_generation | native_context_has_target_identity
				scope:             scope
				window:            record.id
				target_generation: record.render_target_generation
				target_identity:   native_identity(record.egl_surface)
			}
			result := backend.bind_egl_identity(desired, seed)
			if !result.succeeded() {
				return backend.handle_window_egl_failure(index, result, desired)
			}
			return result
		} $else {
			_ = index
			_ = scope
			return native_render_outcome(.egl, .make_current, .renderer, .renderer_unavailable, 0,
				0, err_backend_unsupported)
		}
	}

	fn (mut backend X11Backend) ensure_window_render_surface(index int, boundary_seed NativeOperationSeed) NativeRenderResult {
		$if linux && x_multiwindow_x11 ? {
			if backend.render_health.blocks_graphics() {
				disposition := if backend.render_health == .lost {
					NativeRenderDisposition.renderer_lost
				} else {
					NativeRenderDisposition.renderer_unavailable
				}
				return backend.record_egl_result(native_render_outcome(.egl,
					.window_surface_create, .renderer, disposition, 0, 0,
					err_render_native_renderer_lost))
			}
			mut record := &backend.windows[index]
			if record.native_destroyed {
				return backend.record_egl_result(native_render_outcome(.egl,
					.window_surface_create, .window_target, .native_window_lost, 0, 0,
					err_render_native_window_lost))
			}
			if record.egl_surface != unsafe { nil } {
				return native_render_ok(.egl, .window_surface_create, .window_target)
			}
			seed := NativeOperationSeed{
				...boundary_seed
				presence_mask:     boundary_seed.presence_mask | native_context_has_window | native_context_has_target_generation | native_context_has_target_identity
				call_site:         .window_prepare
				scope:             .window_target
				window:            record.id
				target_generation: record.render_target_generation
				target_identity:   u64(record.window)
			}
			mut ordinals := backend.native_operations.reserve_renderer_attempt_ordinals(2) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.record_egl_result(native_render_outcome(.egl,
					.window_surface_create, .renderer, .renderer_unavailable, 0, 0,
					err_render_native_renderer_unavailable))
			}
			mut cleanup_ordinals := backend.native_operations.reserve_app_lifetime_ordinals(1) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.record_egl_result(native_render_outcome(.egl,
					.window_surface_create, .renderer, .renderer_unavailable, 0, 0,
					err_render_native_renderer_unavailable))
			}
			cleanup_ticket := backend.native_operations.reserve_linux_egl_lifetime_ticket(mut cleanup_ordinals,
				.egl_surface, seed.without_target_identity()) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				return backend.record_egl_result(native_render_outcome(.egl,
					.window_surface_create, .renderer, .renderer_unavailable, 0, 0,
					err_render_native_renderer_unavailable))
			}
			mut raw := C.VMultiwindowNativePrimitive{}
			context := ordinals.materialize(backend.native_operations, .egl,
				.window_surface_create, seed) or {
				backend.render_health = renderer_health_latch_unavailable(backend.render_health)
				backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
				return backend.record_egl_result(native_render_outcome(.egl,
					.window_surface_create, .renderer, .renderer_unavailable, 0, 0,
					err_render_native_renderer_unavailable))
			}
			C.v_multiwindow_linux_egl_create_window_surface(native_identity(backend.egl_display),
				native_identity(backend.egl_config), u64(record.window), &raw)
			actual_surface := native_pointer(raw.handle)
			if actual_surface != unsafe { nil } {
				record.egl_surface = actual_surface
				record.egl_surface_ticket = cleanup_ticket
				backend.native_operations.bind_lifetime_ticket(cleanup_ticket,
					native_identity(actual_surface), native_identity(backend.egl_display))
			}
			result := backend.accept_egl_result(context, mut ordinals, seed, raw, .none)
			if actual_surface == unsafe { nil } {
				backend.native_operations.burn_lifetime_ticket(cleanup_ticket)
			}
			if !result.succeeded() {
				release := backend.release_egl_surface_ticket(cleanup_ticket, actual_surface)
				if release.terminal {
					record.egl_surface = unsafe { nil }
					record.egl_surface_ticket = 0
				}
				return backend.handle_window_egl_failure(index, result, egl_window_binding(record.id,
					record.render_target_generation, unsafe { nil }))
			}
			return result
		} $else {
			_ = index
			return native_render_outcome(.egl, .window_surface_create, .renderer,
				.renderer_unavailable, 0, 0, err_backend_unsupported)
		}
	}

	fn (mut backend X11Backend) handle_window_egl_failure(index int, result NativeRenderResult, desired EglBindingIdentity) NativeRenderResult {
		if index < 0 || index >= backend.windows.len {
			return native_render_outcome(.none, result.operation, .window_target,
				.operation_failed, result.native_code, 0, err_window_not_found)
		}
		mut record := &backend.windows[index]
		if result.disposition == .target_lost || result.disposition == .native_window_lost {
			if desired.surface != unsafe { nil } {
				$if linux && x_multiwindow_x11 ? {
					backend.invalidate_egl_binding(desired)
				}
			} else {
				record.render_target_generation =
					exhaust_backend_target_generation(record.render_target_generation)
			}
		}
		if result.disposition == .native_window_lost {
			record.native_destroyed = true
		}
		return result
	}

	fn (backend &X11Backend) swapchain_for_record(record X11WindowRecord) gfx.Swapchain {
		width := if record.width > 0 { record.width } else { 1 }
		height := if record.height > 0 { record.height } else { 1 }
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

fn (mut backend X11Backend) free_cursor_for_record(index int) {
	$if linux && x_multiwindow_x11 ? {
		if index < 0 || index >= backend.windows.len {
			return
		}
		cursor := backend.windows[index].cursor
		if backend.display != unsafe { nil } && cursor != X11NativeCursor(0) {
			C.XFreeCursor(backend.display, cursor)
		}
		backend.windows[index].cursor = X11NativeCursor(0)
		return
	} $else {
		_ = index
	}
}

fn (mut backend X11Backend) release_window_resources(record X11WindowRecord, destroy_native bool) ! {
	mut owned_record := record
	backend.release_window_record_resources(mut &owned_record, destroy_native)!
}

fn (mut backend X11Backend) release_window_record_resources(mut record &X11WindowRecord, destroy_native bool) ! {
	$if linux && x_multiwindow_x11 ? {
		if record.egl_surface != unsafe { nil } {
			release := backend.release_egl_surface_ticket(record.egl_surface_ticket,
				record.egl_surface)
			if !release.terminal {
				return error(err_render_native_renderer_unavailable)
			}
			record.egl_surface = unsafe { nil }
			record.egl_surface_ticket = 0
		}
		if backend.display != unsafe { nil } && record.cursor != X11NativeCursor(0) {
			C.XFreeCursor(backend.display, record.cursor)
		}
		record.cursor = X11NativeCursor(0)
		if backend.display != unsafe { nil } && record.xic != unsafe { nil } {
			C.v_multiwindow_x11_destroy_ic(record.xic)
		}
		record.xic = unsafe { nil }
		if destroy_native && backend.display != unsafe { nil }
			&& record.window != X11NativeWindow(0) {
			C.XDestroyWindow(backend.display, record.window)
			record.window = X11NativeWindow(0)
		}
		if !destroy_native || backend.display == unsafe { nil } {
			record.window = X11NativeWindow(0)
		}
		if backend.display != unsafe { nil } && record.colormap != X11NativeColormap(0) {
			C.XFreeColormap(backend.display, record.colormap)
		}
		record.colormap = X11NativeColormap(0)
		if backend.display != unsafe { nil } {
			C.XFlush(backend.display)
		}
		return
	} $else {
		_ = record
		_ = destroy_native
		return error(err_backend_unsupported)
	}
}

fn x11_bool_to_int(value bool) int {
	return if value { 1 } else { 0 }
}

fn x11_is_clipboard_paste(key_code int, modifiers u32) bool {
	return key_code == x11_key_v && modifiers == x11_modifier_ctrl
}
