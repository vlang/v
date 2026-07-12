module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	import sokol.gfx
}

$if linux && sokol_wayland ? {
	import time as vtime
}

$if linux && sokol_wayland ? {
	#flag linux -lwayland-client
	#flag linux -lwayland-egl
	#flag linux -lxkbcommon
	#flag linux -lEGL
	#flag linux -lGL
	#flag linux -I @VEXEROOT/thirdparty/sokol
	#flag linux @VMODROOT/vlib/x/multiwindow/wayland_xdg_shell_private.c
	#flag linux @VMODROOT/vlib/x/multiwindow/wayland_xdg_decoration_private.c
	#flag linux @VMODROOT/vlib/x/multiwindow/wayland_cursor_shape_private.c
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
const wayland_xdg_toplevel_decoration_mode_unknown = u32(0)
const wayland_xdg_toplevel_decoration_mode_client_side = u32(1)
const wayland_xdg_toplevel_decoration_mode_server_side = u32(2)
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
const wayland_keyboard_keymap_format_xkb_v1 = u32(1)
const wayland_xkb_context_no_flags = 0
const wayland_xkb_keymap_format_text_v1 = 1
const wayland_xkb_keymap_compile_no_flags = 0
const wayland_xkb_state_mods_effective = 8
const wayland_uri_list_buffer_size = 65536
const wayland_data_offer_drain_chunk_size = 4096
const wayland_data_offer_max_read_chunks = 16
const wayland_data_offer_max_pending_poll_cycles = 300
const wayland_max_fallback_buffers = 3
const wayland_nsec_per_sec = u64(1_000_000_000)
const wayland_nsec_per_msec = u64(1_000_000)
const wayland_dnd_action_none = u32(0)
const wayland_dnd_action_copy = u32(1)
const wayland_dnd_action_move = u32(2)
const wayland_dnd_action_ask = u32(4)
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

@[heap]
struct WaylandWindowRecord {
	id           WindowId
	surface      voidptr
	xdg_surface  voidptr
	xdg_toplevel voidptr
	resizable    bool
mut:
	width                          int
	height                         int
	min_width                      int
	min_height                     int
	pending_toplevel_width         int
	pending_toplevel_height        int
	configured                     bool
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
	egl_surface                    voidptr
	toplevel_decoration            voidptr
	toplevel_decoration_configured bool
	toplevel_decoration_mode       u32
	user_action_serial             u32
	user_action_poll               u64
	user_action_serial_valid       bool
}

struct WaylandNativeQueuedEvent {
	sequence u64
	event    QueuedEvent
}

struct WaylandBackend {
mut:
	display                      voidptr
	registry                     voidptr
	compositor                   voidptr
	compositor_name              u32
	compositor_version           u32
	seat                         voidptr
	seat_name                    u32
	pointer                      voidptr
	cursor_shape_manager         voidptr
	cursor_shape_manager_name    u32
	cursor_shape_device          voidptr
	keyboard                     voidptr
	touch                        voidptr
	data_device_manager          voidptr
	data_device_manager_name     u32
	data_device                  voidptr
	shm                          voidptr
	shm_name                     u32
	data_offer                   voidptr
	data_offer_has_uri_list      bool
	data_offer_source_actions    u32
	data_offer_selected_action   u32
	data_offer_action_received   bool
	data_offer_window            WindowId
	data_offer_window_valid      bool
	pending_drop_offer           voidptr
	pending_drop_fd              int
	pending_drop_window          WindowId
	pending_drop_window_valid    bool
	pending_drop_source_actions  u32
	pending_drop_selected_action u32
	pending_drop_action_received bool
	pending_drop_poll_cycles     int
	pending_drop_buffer          []u8
	wm_base                      voidptr
	wm_base_name                 u32
	decoration_manager           voidptr
	decoration_manager_name      u32
	xkb_context                  voidptr
	xkb_keymap                   voidptr
	xkb_state                    voidptr
	egl_display                  voidptr
	egl_config                   voidptr
	egl_context                  voidptr
	started                      bool
	pointer_focus                WindowId
	pointer_focused              bool
	pointer_enter_serial         u32
	pointer_enter_serial_valid   bool
	keyboard_focus               WindowId
	keyboard_focused             bool
	keyboard_repeat_rate         int
	keyboard_repeat_delay        int
	keyboard_repeat_active       bool
	keyboard_repeat_raw_key      u32
	keyboard_repeat_key_code     int
	keyboard_repeat_window       WindowId
	keyboard_repeat_next_ns      u64
	keyboard_repeat_interval_ns  u64
	poll_generation              u64
	pointer_buttons              u32
	modifiers                    u32
	keys_down                    [512]bool
	touches                      [wayland_max_touch_points]WaylandTouchPoint
	windows                      []&WaylandWindowRecord
}

@[markused]
fn (mut backend WaylandBackend) registry_listener_data() voidptr {
	return unsafe { voidptr(&backend) }
}

@[markused]
fn (record &WaylandWindowRecord) listener_data() voidptr {
	return unsafe { voidptr(record) }
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

	struct C.xkb_context {}

	struct C.xkb_keymap {}

	struct C.xkb_state {}

	fn C.poll(fds &C.pollfd, nfds u64, timeout int) int
	fn C.strcmp(a &char, b &char) int
	fn C.close(fd int) int
	fn C.pipe(fds &int) int
	fn C.read(fd int, buf voidptr, count usize) isize
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
	fn C.v_multiwindow_wayland_bind_seat(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_bind_data_device_manager(registry &C.wl_registry, name u32, version u32) voidptr
	fn C.v_multiwindow_wayland_bind_shm(registry &C.wl_registry, name u32) voidptr
	fn C.v_multiwindow_wayland_next_event_sequence() u64
	fn C.v_multiwindow_wayland_add_registry_listener(registry &C.wl_registry, data voidptr) int
	fn C.v_multiwindow_wayland_add_xdg_wm_base_listener(wm_base &C.xdg_wm_base, data voidptr) int
	fn C.v_multiwindow_wayland_add_xdg_surface_listener(xdg_surface &C.xdg_surface, data voidptr) int
	fn C.v_multiwindow_wayland_add_xdg_toplevel_listener(toplevel &C.xdg_toplevel, data voidptr) int
	fn C.v_multiwindow_wayland_add_xdg_toplevel_decoration_listener(decoration &C.zxdg_toplevel_decoration_v1, data voidptr) int
	fn C.v_multiwindow_wayland_add_seat_listener(seat &C.wl_seat, data voidptr) int
	fn C.v_multiwindow_wayland_seat_get_pointer(seat &C.wl_seat) voidptr
	fn C.v_multiwindow_wayland_seat_get_keyboard(seat &C.wl_seat) voidptr
	fn C.v_multiwindow_wayland_seat_get_touch(seat &C.wl_seat) voidptr
	fn C.v_multiwindow_wayland_data_device_manager_get_data_device(manager &C.wl_data_device_manager, seat &C.wl_seat) voidptr
	fn C.v_multiwindow_wayland_cursor_shape_manager_get_pointer(manager &C.wp_cursor_shape_manager_v1, pointer &C.wl_pointer) voidptr
	fn C.v_multiwindow_wayland_cursor_shape_device_set_shape(device &C.wp_cursor_shape_device_v1, serial u32, shape u32)
	fn C.v_multiwindow_wayland_cursor_shape_device_destroy(device &C.wp_cursor_shape_device_v1)
	fn C.v_multiwindow_wayland_cursor_shape_manager_destroy(manager &C.wp_cursor_shape_manager_v1)
	fn C.v_multiwindow_wayland_add_pointer_listener(pointer &C.wl_pointer, data voidptr) int
	fn C.v_multiwindow_wayland_add_keyboard_listener(keyboard &C.wl_keyboard, data voidptr) int
	fn C.v_multiwindow_wayland_add_touch_listener(touch &C.wl_touch, data voidptr) int
	fn C.v_multiwindow_wayland_add_data_device_listener(device &C.wl_data_device, data voidptr) int
	fn C.v_multiwindow_wayland_add_data_offer_listener(offer &C.wl_data_offer, data voidptr) int
	fn C.v_multiwindow_wayland_data_offer_accept(offer &C.wl_data_offer, serial u32, mime_type &char)
	fn C.v_multiwindow_wayland_data_offer_set_copy_action(offer &C.wl_data_offer)
	fn C.v_multiwindow_wayland_data_offer_receive(offer &C.wl_data_offer, mime_type &char, fd int)
	fn C.v_multiwindow_wayland_fd_set_nonblocking(fd int) int
	fn C.v_multiwindow_wayland_read_would_block() int
	fn C.v_multiwindow_wayland_data_offer_finish(offer &C.wl_data_offer)
	fn C.v_multiwindow_wayland_data_offer_destroy(offer &C.wl_data_offer)
	fn C.v_multiwindow_wayland_data_device_destroy(device &C.wl_data_device)
	fn C.v_multiwindow_wayland_data_device_manager_destroy(manager &C.wl_data_device_manager)
	fn C.v_multiwindow_wayland_shm_destroy(shm &C.wl_shm)
	fn C.v_multiwindow_wayland_create_shm_buffer(shm &C.wl_shm, width int, height int) voidptr
	fn C.v_multiwindow_wayland_attach_buffer(surface &C.wl_surface, buffer &C.wl_buffer, width int, height int)
	fn C.v_multiwindow_wayland_add_buffer_listener(buffer &C.wl_buffer, data voidptr) int
	fn C.v_multiwindow_wayland_buffer_destroy(buffer &C.wl_buffer)
	fn C.v_multiwindow_wayland_pointer_destroy(pointer &C.wl_pointer)
	fn C.v_multiwindow_wayland_keyboard_destroy(keyboard &C.wl_keyboard)
	fn C.v_multiwindow_wayland_touch_destroy(touch &C.wl_touch)
	fn C.v_multiwindow_wayland_seat_destroy(seat &C.wl_seat)
	fn C.wl_display_connect(name &char) &C.wl_display
	fn C.wl_display_disconnect(display &C.wl_display)
	fn C.wl_display_get_fd(display &C.wl_display) int
	fn C.wl_display_get_registry(display &C.wl_display) &C.wl_registry
	fn C.wl_display_prepare_read(display &C.wl_display) int
	fn C.wl_display_cancel_read(display &C.wl_display)
	fn C.wl_display_read_events(display &C.wl_display) int
	fn C.wl_display_dispatch_pending(display &C.wl_display) int
	fn C.wl_display_roundtrip(display &C.wl_display) int
	fn C.wl_display_flush(display &C.wl_display) int
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
	fn C.v_multiwindow_wayland_xdg_toplevel_set_min_size(toplevel &C.xdg_toplevel, width i32, height i32)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_max_size(toplevel &C.xdg_toplevel, width i32, height i32)
	fn C.v_multiwindow_wayland_xdg_toplevel_set_fullscreen(toplevel &C.xdg_toplevel, output &C.wl_output)
	fn C.v_multiwindow_wayland_xdg_toplevel_move(toplevel &C.xdg_toplevel, seat &C.wl_seat, serial u32)
	fn C.v_multiwindow_wayland_xdg_toplevel_resize(toplevel &C.xdg_toplevel, seat &C.wl_seat, serial u32, edges u32)
	fn C.v_multiwindow_wayland_xdg_decoration_manager_get_toplevel_decoration(manager &C.zxdg_decoration_manager_v1, toplevel &C.xdg_toplevel) &C.zxdg_toplevel_decoration_v1
	fn C.v_multiwindow_wayland_xdg_toplevel_decoration_set_server_side(decoration &C.zxdg_toplevel_decoration_v1)
	fn C.v_multiwindow_wayland_xdg_toplevel_decoration_destroy(decoration &C.zxdg_toplevel_decoration_v1)
	fn C.v_multiwindow_wayland_xdg_decoration_manager_destroy(manager &C.zxdg_decoration_manager_v1)
	fn C.v_multiwindow_wayland_xdg_toplevel_destroy(toplevel &C.xdg_toplevel)
	fn C.v_multiwindow_wayland_egl_get_display(display &C.wl_display) voidptr
	fn C.v_multiwindow_wayland_egl_initialize(egl_display voidptr) int
	fn C.v_multiwindow_wayland_egl_bind_opengl_api() int
	fn C.v_multiwindow_wayland_egl_choose_config(egl_display voidptr, out_config &voidptr) int
	fn C.v_multiwindow_wayland_egl_create_context(egl_display voidptr, egl_config voidptr) voidptr
	fn C.v_multiwindow_wayland_egl_create_window(surface &C.wl_surface, width int, height int) voidptr
	fn C.v_multiwindow_wayland_egl_resize_window(egl_window voidptr, width int, height int)
	fn C.v_multiwindow_wayland_egl_destroy_window(egl_window voidptr)
	fn C.v_multiwindow_wayland_egl_create_window_surface(egl_display voidptr, egl_config voidptr, egl_window voidptr) voidptr
	fn C.v_multiwindow_wayland_egl_make_current(egl_display voidptr, egl_surface voidptr, egl_context voidptr) int
	fn C.v_multiwindow_wayland_egl_clear_current(egl_display voidptr)
	fn C.v_multiwindow_wayland_egl_swap_buffers(egl_display voidptr, egl_surface voidptr) int
	fn C.v_multiwindow_wayland_egl_destroy_surface(egl_display voidptr, egl_surface voidptr)
	fn C.v_multiwindow_wayland_egl_destroy_context(egl_display voidptr, egl_context voidptr)
	fn C.v_multiwindow_wayland_egl_terminate(egl_display voidptr)

	@[export: 'v_multiwindow_wayland_registry_handle_global']
	@[markused]
	fn wayland_registry_handle_global(data voidptr, registry &C.wl_registry, name u32, iface &char, version u32) {
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
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
		} else if C.strcmp(iface, c'wl_shm') == 0 && backend.shm == unsafe { nil } {
			shm := C.v_multiwindow_wayland_bind_shm(registry, name)
			if shm != unsafe { nil } {
				backend.shm = shm
				backend.shm_name = name
			}
		}
	}

	@[export: 'v_multiwindow_wayland_registry_handle_global_remove']
	@[markused]
	fn wayland_registry_handle_global_remove(data voidptr, registry &C.wl_registry, name u32) {
		_ = registry
		if data == unsafe { nil } {
			return
		}
		mut backend := unsafe { &WaylandBackend(data) }
		if name == backend.seat_name {
			backend.seat_name = 0
			backend.destroy_seat_devices()
			if backend.seat != unsafe { nil } {
				C.v_multiwindow_wayland_seat_destroy(unsafe { &C.wl_seat(backend.seat) })
				backend.seat = unsafe { nil }
			}
		} else if name == backend.data_device_manager_name {
			backend.data_device_manager_name = 0
			backend.destroy_data_device_manager()
		} else if name == backend.shm_name {
			backend.shm_name = 0
			if backend.shm != unsafe { nil } {
				C.v_multiwindow_wayland_shm_destroy(unsafe { &C.wl_shm(backend.shm) })
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
		} else if name == backend.compositor_name {
			backend.compositor_name = 0
			if backend.compositor != unsafe { nil } {
				if backend.compositor_version >= u32(4) {
					C.wl_compositor_destroy(unsafe { &C.wl_compositor(backend.compositor) })
				}
				backend.compositor = unsafe { nil }
				backend.compositor_version = 0
			}
		}
	}

	@[export: 'v_multiwindow_wayland_xdg_wm_base_ping']
	@[markused]
	fn wayland_xdg_wm_base_ping(data voidptr, wm_base voidptr, serial u32) {
		_ = data
		C.v_multiwindow_wayland_xdg_wm_base_pong(unsafe { &C.xdg_wm_base(wm_base) }, serial)
	}

	@[export: 'v_multiwindow_wayland_xdg_surface_configure']
	@[markused]
	fn wayland_xdg_surface_configure(data voidptr, xdg_surface voidptr, serial u32) {
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
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
		record.configured = true
		if should_queue_resize {
			record.pending_egl_resize = true
			record.enqueue_resize_events(C.v_multiwindow_wayland_next_event_sequence())
		}
		C.v_multiwindow_wayland_xdg_surface_ack_configure(unsafe {
			&C.xdg_surface(xdg_surface)
		}, serial)
	}

	@[export: 'v_multiwindow_wayland_xdg_toplevel_configure']
	@[markused]
	fn wayland_xdg_toplevel_configure(data voidptr, toplevel voidptr, width int, height int, states &C.wl_array) {
		_ = toplevel
		_ = states
		if data == unsafe { nil } {
			return
		}
		mut record := unsafe { &WaylandWindowRecord(data) }
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
		if backend.data_offer == offer && C.strcmp(mime_type, c'text/uri-list') == 0 {
			backend.data_offer_has_uri_list = true
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
		backend.clear_data_offer(true)
		backend.data_offer = offer
		backend.data_offer_has_uri_list = false
		backend.data_offer_source_actions = wayland_dnd_action_none
		backend.data_offer_selected_action = wayland_dnd_action_none
		backend.data_offer_action_received = false
		if C.v_multiwindow_wayland_add_data_offer_listener(unsafe { &C.wl_data_offer(offer) }, data) < 0 {
			backend.clear_data_offer(true)
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
		if backend.data_offer != offer || !backend.data_offer_has_uri_list {
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
		_ = data
		_ = device
		_ = offer
	}
}

fn new_wayland_backend() WaylandBackend {
	return WaylandBackend{}
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
		display := C.wl_display_connect(unsafe { nil })
		if display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		registry := C.wl_display_get_registry(display)
		if registry == unsafe { nil } {
			C.wl_display_disconnect(display)
			return error(err_wayland_registry_failed)
		}
		backend.display = unsafe { voidptr(display) }
		backend.registry = unsafe { voidptr(registry) }
		if C.v_multiwindow_wayland_add_registry_listener(registry, backend.registry_listener_data()) < 0 {
			backend.close_connection()
			return error(err_wayland_registry_failed)
		}
		if C.wl_display_roundtrip(display) < 0 {
			backend.close_connection()
			return error(err_wayland_dispatch_failed)
		}
		if backend.compositor == unsafe { nil } || backend.compositor_name == 0
			|| backend.wm_base == unsafe { nil } || backend.wm_base_name == 0 {
			backend.close_connection()
			return error(err_wayland_required_globals_missing)
		}
		backend.init_xkb() or {
			backend.close_connection()
			return err
		}
		wm_base := unsafe { &C.xdg_wm_base(backend.wm_base) }
		if C.v_multiwindow_wayland_add_xdg_wm_base_listener(wm_base, unsafe { nil }) < 0 {
			backend.close_connection()
			return error(err_wayland_registry_failed)
		}
		if backend.seat != unsafe { nil } {
			if C.v_multiwindow_wayland_add_seat_listener(unsafe { &C.wl_seat(backend.seat) },
				backend.registry_listener_data()) < 0 {
				backend.close_connection()
				return error(err_wayland_registry_failed)
			}
			if C.wl_display_roundtrip(display) < 0 {
				backend.close_connection()
				return error(err_wayland_dispatch_failed)
			}
		}
		if require_renderer {
			backend.init_renderer() or {
				backend.close_connection()
				return err
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
		&& backend.egl_context != unsafe { nil }
}

fn (mut backend WaylandBackend) init_renderer() ! {
	$if linux && sokol_wayland ? {
		if backend.renderer_ready() {
			return
		}
		if backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		display := unsafe { &C.wl_display(backend.display) }
		egl_display := C.v_multiwindow_wayland_egl_get_display(display)
		if egl_display == unsafe { nil } {
			return error(err_wayland_egl_display_failed)
		}
		if C.v_multiwindow_wayland_egl_initialize(egl_display) == 0 {
			return error(err_wayland_egl_display_failed)
		}
		if C.v_multiwindow_wayland_egl_bind_opengl_api() == 0 {
			C.v_multiwindow_wayland_egl_terminate(egl_display)
			return error(err_wayland_egl_context_failed)
		}
		mut egl_config := voidptr(unsafe { nil })
		if C.v_multiwindow_wayland_egl_choose_config(egl_display, &egl_config) == 0 {
			C.v_multiwindow_wayland_egl_terminate(egl_display)
			return error(err_wayland_egl_config_failed)
		}
		egl_context := C.v_multiwindow_wayland_egl_create_context(egl_display, egl_config)
		if egl_context == unsafe { nil } {
			C.v_multiwindow_wayland_egl_terminate(egl_display)
			return error(err_wayland_egl_context_failed)
		}
		backend.egl_display = egl_display
		backend.egl_config = egl_config
		backend.egl_context = egl_context
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) shutdown_renderer() {
	$if linux && sokol_wayland ? {
		if backend.egl_display != unsafe { nil } {
			C.v_multiwindow_wayland_egl_clear_current(backend.egl_display)
			if backend.egl_context != unsafe { nil } {
				C.v_multiwindow_wayland_egl_destroy_context(backend.egl_display,
					backend.egl_context)
			}
			C.v_multiwindow_wayland_egl_terminate(backend.egl_display)
		}
		backend.egl_display = unsafe { nil }
		backend.egl_config = unsafe { nil }
		backend.egl_context = unsafe { nil }
	}
}

fn (mut backend WaylandBackend) create_window(id WindowId, config WindowConfig) !WindowSize {
	$if linux && sokol_wayland ? {
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		if backend.compositor == unsafe { nil } || backend.compositor_name == 0
			|| backend.wm_base == unsafe { nil } || backend.wm_base_name == 0 {
			return error(err_wayland_required_globals_missing)
		}
		if !config.visible {
			return error(err_capability_unsupported)
		}
		display := unsafe { &C.wl_display(backend.display) }
		compositor := unsafe { &C.wl_compositor(backend.compositor) }
		wm_base := unsafe { &C.xdg_wm_base(backend.wm_base) }
		surface := C.wl_compositor_create_surface(compositor)
		if surface == unsafe { nil } {
			return error(err_wayland_create_surface_failed)
		}
		xdg_surface := C.v_multiwindow_wayland_xdg_wm_base_get_xdg_surface(wm_base, surface)
		if xdg_surface == unsafe { nil } {
			C.wl_surface_destroy(surface)
			return error(err_wayland_create_surface_failed)
		}
		xdg_toplevel := C.v_multiwindow_wayland_xdg_surface_get_toplevel(xdg_surface)
		if xdg_toplevel == unsafe { nil } {
			C.v_multiwindow_wayland_xdg_surface_destroy(xdg_surface)
			C.wl_surface_destroy(surface)
			return error(err_wayland_create_surface_failed)
		}
		actual_size := window_size_for_config(config, config.width, config.height)
		record_min_width := if config.resizable { config.min_width } else { actual_size.width }
		record_min_height := if config.resizable { config.min_height } else { actual_size.height }
		mut record := &WaylandWindowRecord{
			id:           id
			surface:      unsafe { voidptr(surface) }
			xdg_surface:  unsafe { voidptr(xdg_surface) }
			xdg_toplevel: unsafe { voidptr(xdg_toplevel) }
			resizable:    config.resizable
			width:        actual_size.width
			height:       actual_size.height
			min_width:    record_min_width
			min_height:   record_min_height
		}
		if C.v_multiwindow_wayland_add_xdg_surface_listener(xdg_surface, record.listener_data()) < 0 {
			backend.destroy_window_record(record)
			return error(err_wayland_create_surface_failed)
		}
		if C.v_multiwindow_wayland_add_xdg_toplevel_listener(xdg_toplevel, record.listener_data()) < 0 {
			backend.destroy_window_record(record)
			return error(err_wayland_create_surface_failed)
		}
		C.v_multiwindow_wayland_xdg_toplevel_set_title(xdg_toplevel, &char(config.title.str))
		C.v_multiwindow_wayland_xdg_toplevel_set_app_id(xdg_toplevel, c'v.x.multiwindow')
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
		C.wl_surface_commit(surface)
		if C.wl_display_flush(display) < 0 {
			backend.remove_window_record(id)
			backend.destroy_window_record(record)
			_ = backend.destroy_removed_wm_base_if_unused()
			return error(err_wayland_flush_failed)
		}
		if C.wl_display_roundtrip(display) < 0 {
			backend.remove_window_record(id)
			backend.destroy_window_record(record)
			_ = backend.destroy_removed_wm_base_if_unused()
			return error(err_wayland_dispatch_failed)
		}
		if !backend.renderer_ready() {
			index := backend.window_record_index(id) or {
				backend.destroy_window_record(record)
				_ = backend.destroy_removed_wm_base_if_unused()
				return error(err_window_not_found)
			}
			backend.ensure_lifecycle_buffer(index) or {
				backend.remove_window_record(id)
				backend.destroy_window_record(record)
				_ = backend.destroy_removed_wm_base_if_unused()
				return err
			}
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
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		mut record := backend.windows[index]
		backend.destroy_window_record(record)
		backend.windows.delete(index)
		_ = backend.destroy_removed_wm_base_if_unused()
		if backend.started && backend.display != unsafe { nil } {
			display := unsafe { &C.wl_display(backend.display) }
			if C.wl_display_flush(display) < 0 {
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
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		record := backend.windows[index]
		if record.xdg_toplevel == unsafe { nil } {
			return error(err_window_not_found)
		}
		C.v_multiwindow_wayland_xdg_toplevel_set_title(unsafe {
			&C.xdg_toplevel(record.xdg_toplevel)
		}, &char(title.str))
		display := unsafe { &C.wl_display(backend.display) }
		if C.wl_display_flush(display) < 0 {
			return error(err_wayland_flush_failed)
		}
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
		backend.window_record_index(id) or { return error(err_window_not_found) }
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
		backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		if !backend.can_set_cursor_shapes() || !backend.pointer_focused
			|| backend.pointer_focus != id || !backend.pointer_enter_serial_valid {
			return error(err_capability_unsupported)
		}
		C.v_multiwindow_wayland_cursor_shape_device_set_shape(unsafe {
			&C.wp_cursor_shape_device_v1(backend.cursor_shape_device)
		}, backend.pointer_enter_serial, wayland_cursor_shape_for_shape(shape))
		display := unsafe { &C.wl_display(backend.display) }
		if C.wl_display_flush(display) < 0 {
			return error(err_wayland_flush_failed)
		}
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
	}
}

fn (mut backend WaylandBackend) begin_window_move(id WindowId) ! {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
		if !backend.started || backend.display == unsafe { nil } {
			return error(err_wayland_connect_failed)
		}
		if backend.seat == unsafe { nil } {
			return error(err_capability_unsupported)
		}
		if backend.windows[index].xdg_toplevel == unsafe { nil } {
			return error(err_window_not_found)
		}
		serial := backend.windows[index].take_user_action_serial(backend.poll_generation) or {
			return error(err_capability_unsupported)
		}
		C.v_multiwindow_wayland_xdg_toplevel_move(unsafe {
			&C.xdg_toplevel(backend.windows[index].xdg_toplevel)
		}, unsafe { &C.wl_seat(backend.seat) }, serial)
		display := unsafe { &C.wl_display(backend.display) }
		if C.wl_display_flush(display) < 0 {
			return error(err_wayland_flush_failed)
		}
		return
	} $else {
		_ = id
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) begin_window_resize(id WindowId, edge WindowResizeEdge) ! {
	$if linux && sokol_wayland ? {
		index := backend.window_record_index(id) or { return error(err_window_not_found) }
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
		serial := backend.windows[index].take_user_action_serial(backend.poll_generation) or {
			return error(err_capability_unsupported)
		}
		C.v_multiwindow_wayland_xdg_toplevel_resize(unsafe {
			&C.xdg_toplevel(backend.windows[index].xdg_toplevel)
		}, unsafe { &C.wl_seat(backend.seat) }, serial, wayland_resize_edge(edge))
		display := unsafe { &C.wl_display(backend.display) }
		if C.wl_display_flush(display) < 0 {
			return error(err_wayland_flush_failed)
		}
		return
	} $else {
		_ = id
		_ = edge
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) poll_events() ![]Event {
	queued_events := backend.poll_queued_events()!
	mut events := []Event{cap: queued_events.len}
	for event in queued_events {
		if event.kind == .lifecycle {
			events << event.lifecycle
		}
	}
	return events
}

fn (mut backend WaylandBackend) poll_queued_events() ![]QueuedEvent {
	mut native_events := []WaylandNativeQueuedEvent{}
	$if linux && sokol_wayland ? {
		if !backend.started || backend.display == unsafe { nil } {
			return []QueuedEvent{}
		}
		backend.poll_generation++
		backend.expire_user_action_serials()
		backend.dispatch_pending_nonblocking()!
		if !backend.renderer_ready() {
			backend.ensure_lifecycle_buffers()!
		}
		backend.drain_pending_data_offer_drop()
		backend.enqueue_due_key_repeats()
		for _, mut record in backend.windows {
			for native_event in record.pending_events {
				native_events << native_event
			}
			record.pending_events.clear()
		}
	}
	wayland_sort_native_events(mut native_events)
	mut events := []QueuedEvent{cap: native_events.len}
	for native_event in native_events {
		events << native_event.event
	}
	return events
}

fn (mut record WaylandWindowRecord) enqueue_native_event(sequence u64, event QueuedEvent) {
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
		framebuffer_width:  record.width
		framebuffer_height: record.height
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
		framebuffer_width:  record.width
		framebuffer_height: record.height
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
		framebuffer_width:  record.width
		framebuffer_height: record.height
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
		framebuffer_width:  record.width
		framebuffer_height: record.height
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
		framebuffer_width:  record.width
		framebuffer_height: record.height
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
		backend.close_connection()
		return
	} $else {
		return error(err_backend_unsupported)
	}
}

fn (mut backend WaylandBackend) dispatch_pending_nonblocking() ! {
	$if linux && sokol_wayland ? {
		if backend.display == unsafe { nil } {
			return
		}
		display := unsafe { &C.wl_display(backend.display) }
		if C.wl_display_flush(display) < 0 {
			return error(err_wayland_flush_failed)
		}
		for {
			dispatched := C.wl_display_dispatch_pending(display)
			if dispatched < 0 {
				return error(err_wayland_dispatch_failed)
			}
			if dispatched == 0 {
				break
			}
		}
		for {
			prepare_result := C.wl_display_prepare_read(display)
			if prepare_result != 0 {
				dispatched := C.wl_display_dispatch_pending(display)
				if dispatched < 0 {
					return error(err_wayland_dispatch_failed)
				}
				if dispatched == 0 {
					break
				}
				continue
			}
			fd := C.wl_display_get_fd(display)
			mut poll_fd := C.pollfd{
				fd:      fd
				events:  wayland_poll_in
				revents: i16(0)
			}
			poll_result := C.poll(&poll_fd, u64(1), 0)
			if poll_result < 0 {
				C.wl_display_cancel_read(display)
				return error(err_wayland_dispatch_failed)
			}
			if poll_result == 0 || (poll_fd.revents & wayland_poll_in) == i16(0) {
				C.wl_display_cancel_read(display)
				break
			}
			if C.wl_display_read_events(display) < 0 {
				return error(err_wayland_dispatch_failed)
			}
			for {
				dispatched := C.wl_display_dispatch_pending(display)
				if dispatched < 0 {
					return error(err_wayland_dispatch_failed)
				}
				if dispatched == 0 {
					break
				}
			}
		}
	}
}

$if gg_multiwindow ? || x_multiwindow_render ? {
	fn (mut backend WaylandBackend) render_environment(id WindowId) !gfx.Environment {
		$if linux && sokol_wayland ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			backend.prepare_render_window(index)!
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

	fn (mut backend WaylandBackend) begin_render(id WindowId) !RenderFrame {
		$if linux && sokol_wayland ? {
			index := backend.window_record_index(id) or { return error(err_window_not_found) }
			backend.prepare_render_window(index)!
			record := backend.windows[index]
			return RenderFrame{
				window_id: id
				swapchain: backend.swapchain_for_record(record)
			}
		} $else {
			_ = id
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) end_render(frame RenderFrame) ! {
		$if linux && sokol_wayland ? {
			index := backend.window_record_index(frame.window_id) or {
				return error(err_window_not_found)
			}
			record := backend.windows[index]
			backend.make_current(record)!
			if C.v_multiwindow_wayland_egl_swap_buffers(backend.egl_display, record.egl_surface) == 0 {
				return error(err_wayland_egl_swap_buffers_failed)
			}
			if backend.display != unsafe { nil } {
				display := unsafe { &C.wl_display(backend.display) }
				if C.wl_display_flush(display) < 0 {
					return error(err_wayland_flush_failed)
				}
			}
			return
		} $else {
			_ = frame
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) prepare_render_window(index int) ! {
		$if linux && sokol_wayland ? {
			if !backend.renderer_ready() {
				return error(err_renderer_unsupported)
			}
			backend.ensure_configured(index)!
			backend.apply_pending_configure(index)!
			backend.ensure_window_render_target(index)!
			record := backend.windows[index]
			backend.make_current(record)!
			return
		} $else {
			_ = index
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) ensure_configured(index int) ! {
		$if linux && sokol_wayland ? {
			mut record := backend.windows[index]
			if record.configured {
				return
			}
			backend.dispatch_pending_nonblocking()!
			if record.configured {
				return
			}
			if backend.display == unsafe { nil } {
				return error(err_wayland_connect_failed)
			}
			display := unsafe { &C.wl_display(backend.display) }
			if C.wl_display_roundtrip(display) < 0 {
				return error(err_wayland_dispatch_failed)
			}
			if !record.configured {
				return error(err_wayland_surface_not_configured)
			}
			return
		} $else {
			_ = index
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) apply_pending_configure(index int) ! {
		$if linux && sokol_wayland ? {
			mut record := backend.windows[index]
			if !record.pending_egl_resize {
				return
			}
			if record.wl_egl_window != unsafe { nil } {
				C.v_multiwindow_wayland_egl_resize_window(record.wl_egl_window,
					safe_wayland_extent(record.width), safe_wayland_extent(record.height))
			}
			record.pending_egl_resize = false
			return
		} $else {
			_ = index
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) ensure_window_render_target(index int) ! {
		$if linux && sokol_wayland ? {
			mut record := backend.windows[index]
			if record.wl_egl_window == unsafe { nil } {
				if record.surface == unsafe { nil } {
					return error(err_window_not_found)
				}
				egl_window := C.v_multiwindow_wayland_egl_create_window(unsafe {
					&C.wl_surface(record.surface)
				}, safe_wayland_extent(record.width), safe_wayland_extent(record.height))
				if egl_window == unsafe { nil } {
					return error(err_wayland_egl_surface_failed)
				}
				record.wl_egl_window = egl_window
			}
			if record.egl_surface == unsafe { nil } {
				egl_surface := C.v_multiwindow_wayland_egl_create_window_surface(backend.egl_display,
					backend.egl_config, record.wl_egl_window)
				if egl_surface == unsafe { nil } {
					return error(err_wayland_egl_surface_failed)
				}
				record.egl_surface = egl_surface
			}
			return
		} $else {
			_ = index
			return error(err_backend_unsupported)
		}
	}

	fn (mut backend WaylandBackend) make_current(record &WaylandWindowRecord) ! {
		$if linux && sokol_wayland ? {
			if !backend.renderer_ready() || record.egl_surface == unsafe { nil } {
				return error(err_renderer_unsupported)
			}
			if C.v_multiwindow_wayland_egl_make_current(backend.egl_display, record.egl_surface,
				backend.egl_context) == 0 {
				return error(err_wayland_egl_make_current_failed)
			}
			return
		} $else {
			_ = record
			return error(err_backend_unsupported)
		}
	}

	fn (backend &WaylandBackend) swapchain_for_record(record &WaylandWindowRecord) gfx.Swapchain {
		width := safe_wayland_extent(record.width)
		height := safe_wayland_extent(record.height)
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

fn safe_wayland_extent(value int) int {
	if value > 0 {
		return value
	}
	return 1
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

fn (mut backend WaylandBackend) ensure_lifecycle_buffer(index int) ! {
	$if linux && sokol_wayland ? {
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
		if record.surface == unsafe { nil } {
			return error(err_window_not_found)
		}
		if !record.configured {
			return error(err_wayland_surface_not_configured)
		}
		width := safe_wayland_extent(record.width)
		height := safe_wayland_extent(record.height)
		if record.fallback_buffer_width == width && record.fallback_buffer_height == height {
			return
		}
		if record.fallback_buffers.len >= wayland_max_fallback_buffers {
			return
		}
		buffer := C.v_multiwindow_wayland_create_shm_buffer(unsafe { &C.wl_shm(backend.shm) },
			width, height)
		if buffer == unsafe { nil } {
			return error(err_wayland_buffer_failed)
		}
		if C.v_multiwindow_wayland_add_buffer_listener(unsafe { &C.wl_buffer(buffer) },
			record.listener_data()) != 0 {
			C.v_multiwindow_wayland_buffer_destroy(unsafe { &C.wl_buffer(buffer) })
			return error(err_wayland_buffer_failed)
		}
		C.v_multiwindow_wayland_attach_buffer(unsafe { &C.wl_surface(record.surface) },
			unsafe { &C.wl_buffer(buffer) }, width, height)
		backend.windows[index].fallback_buffers << buffer
		backend.windows[index].fallback_current_buffer = buffer
		backend.windows[index].fallback_buffer_width = width
		backend.windows[index].fallback_buffer_height = height
		if backend.display != unsafe { nil } {
			display := unsafe { &C.wl_display(backend.display) }
			if C.wl_display_flush(display) < 0 {
				return error(err_wayland_flush_failed)
			}
		}
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
			if record.fallback_current_buffer == buffer {
				record.fallback_current_buffer = unsafe { nil }
			}
			C.v_multiwindow_wayland_buffer_destroy(unsafe { &C.wl_buffer(buffer) })
			record.fallback_buffers.delete(i)
			return
		}
	} $else {
		_ = buffer
	}
}

fn (mut backend WaylandBackend) close_connection() {
	$if linux && sokol_wayland ? {
		for backend.windows.len > 0 {
			record := backend.windows[0]
			backend.destroy_window_record(record)
			backend.windows.delete(0)
		}
		_ = backend.destroy_removed_wm_base_if_unused()
		backend.destroy_seat_devices()
		backend.destroy_data_device_manager()
		backend.destroy_xdg_decoration_manager()
		backend.destroy_cursor_shape_manager()
		backend.shutdown_renderer()
		backend.destroy_xkb()
		if backend.shm != unsafe { nil } {
			C.v_multiwindow_wayland_shm_destroy(unsafe { &C.wl_shm(backend.shm) })
		}
		if backend.wm_base != unsafe { nil } {
			C.v_multiwindow_wayland_xdg_wm_base_destroy(unsafe { &C.xdg_wm_base(backend.wm_base) })
		}
		if backend.seat != unsafe { nil } {
			C.v_multiwindow_wayland_seat_destroy(unsafe { &C.wl_seat(backend.seat) })
		}
		if backend.compositor != unsafe { nil } && backend.compositor_version >= u32(4) {
			C.wl_compositor_destroy(unsafe { &C.wl_compositor(backend.compositor) })
		}
		if backend.registry != unsafe { nil } {
			C.wl_registry_destroy(unsafe { &C.wl_registry(backend.registry) })
		}
		if backend.display != unsafe { nil } {
			display := unsafe { &C.wl_display(backend.display) }
			C.wl_display_flush(display)
			C.wl_display_disconnect(display)
		}
		backend.display = unsafe { nil }
		backend.registry = unsafe { nil }
		backend.compositor = unsafe { nil }
		backend.compositor_name = 0
		backend.compositor_version = 0
		backend.seat = unsafe { nil }
		backend.seat_name = 0
		backend.cursor_shape_manager = unsafe { nil }
		backend.cursor_shape_manager_name = 0
		backend.cursor_shape_device = unsafe { nil }
		backend.data_device_manager = unsafe { nil }
		backend.data_device_manager_name = 0
		backend.data_device = unsafe { nil }
		backend.shm = unsafe { nil }
		backend.shm_name = 0
		backend.data_offer = unsafe { nil }
		backend.data_offer_has_uri_list = false
		backend.data_offer_window_valid = false
		backend.wm_base = unsafe { nil }
		backend.wm_base_name = 0
		backend.decoration_manager = unsafe { nil }
		backend.decoration_manager_name = 0
		backend.pointer_enter_serial_valid = false
		backend.started = false
	}
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
			C.v_multiwindow_wayland_xdg_decoration_manager_destroy(unsafe {
				&C.zxdg_decoration_manager_v1(backend.decoration_manager)
			})
		}
		backend.decoration_manager = unsafe { nil }
		backend.decoration_manager_name = 0
	}
}

fn (mut backend WaylandBackend) ensure_cursor_shape_device() {
	$if linux && sokol_wayland ? {
		if backend.cursor_shape_device != unsafe { nil }
			|| backend.cursor_shape_manager == unsafe { nil } || backend.pointer == unsafe { nil } {
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
			C.v_multiwindow_wayland_cursor_shape_device_destroy(unsafe {
				&C.wp_cursor_shape_device_v1(backend.cursor_shape_device)
			})
		}
		backend.cursor_shape_device = unsafe { nil }
	}
}

fn (mut backend WaylandBackend) destroy_cursor_shape_manager() {
	$if linux && sokol_wayland ? {
		backend.destroy_cursor_shape_device()
		if backend.cursor_shape_manager != unsafe { nil } {
			C.v_multiwindow_wayland_cursor_shape_manager_destroy(unsafe {
				&C.wp_cursor_shape_manager_v1(backend.cursor_shape_manager)
			})
		}
		backend.cursor_shape_manager = unsafe { nil }
		backend.cursor_shape_manager_name = 0
	}
}

fn (mut backend WaylandBackend) destroy_removed_wm_base_if_unused() bool {
	$if linux && sokol_wayland ? {
		if backend.wm_base_name != 0 || backend.wm_base == unsafe { nil }
			|| backend.windows.len != 0 {
			return false
		}
		C.v_multiwindow_wayland_xdg_wm_base_destroy(unsafe { &C.xdg_wm_base(backend.wm_base) })
		backend.wm_base = unsafe { nil }
		return true
	}
	return false
}

fn (mut backend WaylandBackend) destroy_seat_devices() {
	$if linux && sokol_wayland ? {
		backend.destroy_data_device()
		backend.destroy_cursor_shape_device()
		if backend.pointer != unsafe { nil } {
			C.v_multiwindow_wayland_pointer_destroy(unsafe { &C.wl_pointer(backend.pointer) })
		}
		if backend.keyboard != unsafe { nil } {
			C.v_multiwindow_wayland_keyboard_destroy(unsafe { &C.wl_keyboard(backend.keyboard) })
		}
		if backend.touch != unsafe { nil } {
			C.v_multiwindow_wayland_touch_destroy(unsafe { &C.wl_touch(backend.touch) })
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

fn (mut backend WaylandBackend) destroy_window_record(record &WaylandWindowRecord) {
	$if linux && sokol_wayland ? {
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
		if backend.egl_display != unsafe { nil } && record.egl_surface != unsafe { nil } {
			C.v_multiwindow_wayland_egl_clear_current(backend.egl_display)
			C.v_multiwindow_wayland_egl_destroy_surface(backend.egl_display, record.egl_surface)
		}
		if record.wl_egl_window != unsafe { nil } {
			C.v_multiwindow_wayland_egl_destroy_window(record.wl_egl_window)
		}
		for buffer in record.fallback_buffers {
			if buffer != unsafe { nil } {
				C.v_multiwindow_wayland_buffer_destroy(unsafe { &C.wl_buffer(buffer) })
			}
		}
		if record.toplevel_decoration != unsafe { nil } {
			C.v_multiwindow_wayland_xdg_toplevel_decoration_destroy(unsafe {
				&C.zxdg_toplevel_decoration_v1(record.toplevel_decoration)
			})
		}
		if record.xdg_toplevel != unsafe { nil } {
			C.v_multiwindow_wayland_xdg_toplevel_destroy(unsafe {
				&C.xdg_toplevel(record.xdg_toplevel)
			})
		}
		if record.xdg_surface != unsafe { nil } {
			C.v_multiwindow_wayland_xdg_surface_destroy(unsafe { &C.xdg_surface(record.xdg_surface) })
		}
		if record.surface != unsafe { nil } {
			C.wl_surface_destroy(unsafe { &C.wl_surface(record.surface) })
		}
	}
}

fn (mut backend WaylandBackend) ensure_data_device(data voidptr) {
	$if linux && sokol_wayland ? {
		if backend.data_device != unsafe { nil } || backend.data_device_manager == unsafe { nil }
			|| backend.seat == unsafe { nil } {
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

fn (mut backend WaylandBackend) clear_data_offer(destroy bool) {
	$if linux && sokol_wayland ? {
		mut pending_offer := voidptr(unsafe { nil })
		if backend.pending_drop_offer != unsafe { nil } {
			pending_offer = backend.pending_drop_offer
			backend.clear_pending_data_offer_drop(destroy)
		}
		if destroy && backend.data_offer != unsafe { nil } {
			if backend.data_offer != pending_offer {
				C.v_multiwindow_wayland_data_offer_destroy(unsafe {
					&C.wl_data_offer(backend.data_offer)
				})
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
			if C.wl_display_flush(unsafe { &C.wl_display(backend.display) }) < 0 {
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
		if backend.pending_drop_offer != unsafe { nil } {
			if backend.pending_drop_fd >= 0 {
				C.close(backend.pending_drop_fd)
			}
			if destroy {
				C.v_multiwindow_wayland_data_offer_destroy(unsafe {
					&C.wl_data_offer(backend.pending_drop_offer)
				})
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

fn (mut backend WaylandBackend) drain_pending_data_offer_drop() {
	$if linux && sokol_wayland ? {
		if backend.pending_drop_offer == unsafe { nil } || backend.pending_drop_fd < 0 {
			return
		}
		if backend.pending_drop_poll_cycle_expired() {
			backend.clear_data_offer(true)
			return
		}
		mut poll_fd := C.pollfd{
			fd:     backend.pending_drop_fd
			events: wayland_poll_in | wayland_poll_err | wayland_poll_hup
		}
		poll_result := C.poll(&poll_fd, u64(1), 0)
		if poll_result == 0 {
			return
		}
		if poll_result < 0 || (poll_fd.revents & wayland_poll_err) != 0 {
			backend.clear_data_offer(true)
			return
		}
		mut should_finish := (poll_fd.revents & wayland_poll_hup) != 0
		if (poll_fd.revents & wayland_poll_in) != 0 || should_finish {
			mut chunk := [wayland_data_offer_drain_chunk_size]u8{}
			for _ in 0 .. wayland_data_offer_max_read_chunks {
				remaining := wayland_uri_list_buffer_size - backend.pending_drop_buffer.len
				if remaining <= 0 {
					backend.clear_data_offer(true)
					return
				}
				read_size := if remaining < wayland_data_offer_drain_chunk_size {
					remaining
				} else {
					wayland_data_offer_drain_chunk_size
				}
				n := C.read(backend.pending_drop_fd, unsafe { &chunk[0] }, usize(read_size))
				if n > 0 {
					for i in 0 .. int(n) {
						backend.pending_drop_buffer << chunk[i]
					}
					continue
				}
				if n == 0 {
					should_finish = true
					break
				}
				if C.v_multiwindow_wayland_read_would_block() != 0 {
					break
				}
				backend.clear_data_offer(true)
				return
			}
		}
		if should_finish {
			backend.finish_pending_data_offer_drop()
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
			C.v_multiwindow_wayland_data_offer_finish(unsafe {
				&C.wl_data_offer(backend.pending_drop_offer)
			})
		}
		backend.clear_data_offer(true)
	}
}

fn (mut backend WaylandBackend) destroy_data_device() {
	$if linux && sokol_wayland ? {
		backend.clear_data_offer(true)
		if backend.data_device != unsafe { nil } {
			C.v_multiwindow_wayland_data_device_destroy(unsafe { &C.wl_data_device(backend.data_device) })
		}
		backend.data_device = unsafe { nil }
	}
}

fn (mut backend WaylandBackend) destroy_data_device_manager() {
	$if linux && sokol_wayland ? {
		backend.destroy_data_device()
		if backend.data_device_manager != unsafe { nil } {
			C.v_multiwindow_wayland_data_device_manager_destroy(unsafe {
				&C.wl_data_device_manager(backend.data_device_manager)
			})
		}
		backend.data_device_manager = unsafe { nil }
		backend.data_device_manager_name = 0
	}
}

fn (backend &WaylandBackend) window_record_index(id WindowId) ?int {
	for i, record in backend.windows {
		if record.id == id {
			return i
		}
	}
	return none
}

fn (mut backend WaylandBackend) remove_window_record(id WindowId) {
	if index := backend.window_record_index(id) {
		backend.windows.delete(index)
	}
}
