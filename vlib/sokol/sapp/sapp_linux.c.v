@[has_globals]
module sapp

#preinclude <X11/Xlib.h>
#preinclude <X11/Xatom.h>
#preinclude <X11/Xutil.h>
#preinclude <X11/Xresource.h>
#preinclude <X11/keysym.h>
#preinclude <X11/XKBlib.h>
#preinclude <X11/Xcursor/Xcursor.h>
#preinclude <X11/extensions/XInput2.h>

// C library bindings for Linux Wayland/X11 backends.
// These declare the external C types and functions from:
// - wayland-client, wayland-egl, wayland-cursor (when SOKOL_WAYLAND is defined)
// - xkbcommon
// - EGL
// - Wayland protocol generated headers (when SOKOL_WAYLAND is defined)
// - Linux syscalls (timerfd, mmap)

$if sokol_wayland ? {
	// === wayland-client core types ===

	pub struct C.wl_display {}

	pub struct C.wl_registry {}

	pub struct C.wl_compositor {}

	pub struct C.wl_surface {}

	pub struct C.wl_seat {}

	pub struct C.wl_pointer {}

	pub struct C.wl_keyboard {}

	pub struct C.wl_touch {}

	pub struct C.wl_shm {}

	pub struct C.wl_output {}

	pub struct C.wl_data_device_manager {}

	pub struct C.wl_data_device {}

	pub struct C.wl_data_source {}

	pub struct C.wl_data_offer {}

	pub struct C.wl_proxy {}

	pub struct C.wl_interface {}

	pub struct C.wl_array {
		size  usize
		alloc usize
		data  voidptr
	}

	// === wayland-client functions ===

	fn C.wl_display_connect(name &char) &C.wl_display
	fn C.wl_display_disconnect(display &C.wl_display)
	fn C.wl_display_dispatch(display &C.wl_display) int
	fn C.wl_display_dispatch_pending(display &C.wl_display) int
	fn C.wl_display_roundtrip(display &C.wl_display) int
	fn C.wl_display_flush(display &C.wl_display) int
	fn C.wl_display_get_registry(display &C.wl_display) &C.wl_registry

	fn C.wl_registry_bind(registry &C.wl_registry, name u32, iface &C.wl_interface, version u32) voidptr
	fn C.wl_registry_destroy(registry &C.wl_registry)

	fn C.wl_compositor_create_surface(compositor &C.wl_compositor) &C.wl_surface
	fn C.wl_compositor_destroy(compositor &C.wl_compositor)

	fn C.wl_surface_destroy(surface &C.wl_surface)
	fn C.wl_surface_commit(surface &C.wl_surface)
	fn C.wl_surface_set_buffer_scale(surface &C.wl_surface, scale i32)

	fn C.wl_seat_get_pointer(seat &C.wl_seat) &C.wl_pointer
	fn C.wl_seat_get_keyboard(seat &C.wl_seat) &C.wl_keyboard
	fn C.wl_seat_get_touch(seat &C.wl_seat) &C.wl_touch
	fn C.wl_seat_destroy(seat &C.wl_seat)

	fn C.wl_pointer_destroy(pointer &C.wl_pointer)
	fn C.wl_keyboard_destroy(keyboard &C.wl_keyboard)
	fn C.wl_touch_destroy(touch &C.wl_touch)
	fn C.wl_shm_destroy(shm &C.wl_shm)

	fn C.wl_data_device_manager_get_data_device(manager &C.wl_data_device_manager, seat &C.wl_seat) &C.wl_data_device
	fn C.wl_data_device_manager_create_data_source(manager &C.wl_data_device_manager) &C.wl_data_source
	fn C.wl_data_device_destroy(device &C.wl_data_device)
	fn C.wl_data_source_destroy(source &C.wl_data_source)
	fn C.wl_data_source_offer(source &C.wl_data_source, mime_type &char)
	fn C.wl_data_offer_destroy(offer &C.wl_data_offer)
	fn C.wl_data_offer_receive(offer &C.wl_data_offer, mime_type &char, fd int)
	fn C.wl_data_offer_accept(offer &C.wl_data_offer, serial u32, mime_type &char)
	fn C.wl_data_offer_set_actions(offer &C.wl_data_offer, dnd_actions u32, preferred_action u32)
	fn C.wl_data_offer_finish(offer &C.wl_data_offer)
	fn C.wl_data_device_manager_destroy(manager &C.wl_data_device_manager)

	fn C.wl_proxy_add_listener(proxy &C.wl_proxy, implementation voidptr, data voidptr) int
	fn C.wl_proxy_get_version(proxy &C.wl_proxy) u32

	fn C.wl_fixed_to_double(f i32) f64

	// wl_registry_add_listener, wl_seat_add_listener, etc. are static inline functions
	// that call wl_proxy_add_listener. We declare them as C functions since they're
	// in the included headers.
	fn C.wl_registry_add_listener(registry &C.wl_registry, listener voidptr, data voidptr) int
	fn C.wl_seat_add_listener(seat &C.wl_seat, listener voidptr, data voidptr) int
	fn C.wl_pointer_add_listener(pointer &C.wl_pointer, listener voidptr, data voidptr) int
	fn C.wl_keyboard_add_listener(keyboard &C.wl_keyboard, listener voidptr, data voidptr) int
	fn C.wl_data_device_add_listener(device &C.wl_data_device, listener voidptr, data voidptr) int
	fn C.wl_data_offer_add_listener(offer &C.wl_data_offer, listener voidptr, data voidptr) int

	// === wayland-egl ===

	pub struct C.wl_egl_window {}

	fn C.wl_egl_window_create(surface &C.wl_surface, width int, height int) &C.wl_egl_window
	fn C.wl_egl_window_destroy(window &C.wl_egl_window)
	fn C.wl_egl_window_resize(window &C.wl_egl_window, width int, height int, dx int, dy int)

	// === wayland-cursor ===

	pub struct C.wl_cursor_theme {}

	pub struct C.wl_cursor {
		image_count u32
		images      &&C.wl_cursor_image
		name        &char
	}

	pub struct C.wl_cursor_image {
		width     u32
		height    u32
		hotspot_x u32
		hotspot_y u32
		delay     u32
	}

	fn C.wl_cursor_theme_load(name &char, size int, shm &C.wl_shm) &C.wl_cursor_theme
	fn C.wl_cursor_theme_destroy(theme &C.wl_cursor_theme)
	fn C.wl_cursor_theme_get_cursor(theme &C.wl_cursor_theme, name &char) &C.wl_cursor
	fn C.wl_cursor_image_get_buffer(image &C.wl_cursor_image) &C.wl_buffer

	pub struct C.wl_buffer {}

	fn C.wl_surface_attach(surface &C.wl_surface, buffer &C.wl_buffer, x i32, y i32)

	// === Wayland protocol interfaces (extern globals from generated .c files) ===

	#include "xdg-shell-client-protocol.h"
	#include "fractional-scale-v1-client-protocol.h"
	#include "cursor-shape-v1-client-protocol.h"
	#include "pointer-constraints-unstable-v1-client-protocol.h"
	#include "relative-pointer-unstable-v1-client-protocol.h"
	#include "viewporter-client-protocol.h"
	#include "xdg-decoration-unstable-v1-client-protocol.h"

	// Protocol object types
	pub struct C.xdg_wm_base {}

	pub struct C.xdg_surface {}

	pub struct C.xdg_toplevel {}

	pub struct C.wp_fractional_scale_manager_v1 {}

	pub struct C.wp_fractional_scale_v1 {}

	pub struct C.wp_viewporter {}

	pub struct C.wp_viewport {}

	pub struct C.wp_cursor_shape_manager_v1 {}

	pub struct C.wp_cursor_shape_device_v1 {}

	pub struct C.zwp_pointer_constraints_v1 {}

	pub struct C.zwp_locked_pointer_v1 {}

	pub struct C.zwp_relative_pointer_manager_v1 {}

	pub struct C.zwp_relative_pointer_v1 {}

	pub struct C.zxdg_decoration_manager_v1 {}

	pub struct C.zxdg_toplevel_decoration_v1 {}

	// Protocol interface globals (extern const struct wl_interface defined in generated .c and system headers)
	__global C.wl_compositor_interface C.wl_interface
	__global C.wl_seat_interface C.wl_interface
	__global C.wl_shm_interface C.wl_interface
	__global C.wl_output_interface C.wl_interface
	__global C.wl_data_device_manager_interface C.wl_interface
	__global C.xdg_wm_base_interface C.wl_interface
	__global C.xdg_surface_interface C.wl_interface
	__global C.wp_fractional_scale_manager_v1_interface C.wl_interface
	__global C.wp_viewporter_interface C.wl_interface
	__global C.wp_cursor_shape_manager_v1_interface C.wl_interface
	__global C.zwp_pointer_constraints_v1_interface C.wl_interface
	__global C.zwp_relative_pointer_manager_v1_interface C.wl_interface
	__global C.zxdg_decoration_manager_v1_interface C.wl_interface

	// Protocol functions (static inline in generated headers)
	fn C.xdg_wm_base_get_xdg_surface(wm_base &C.xdg_wm_base, surface &C.wl_surface) &C.xdg_surface
	fn C.xdg_wm_base_destroy(wm_base &C.xdg_wm_base)
	fn C.xdg_wm_base_pong(wm_base &C.xdg_wm_base, serial u32)

	fn C.xdg_surface_get_toplevel(xdg_surface &C.xdg_surface) &C.xdg_toplevel
	fn C.xdg_surface_ack_configure(xdg_surface &C.xdg_surface, serial u32)
	fn C.xdg_surface_destroy(xdg_surface &C.xdg_surface)
	fn C.xdg_surface_add_listener(xdg_surface &C.xdg_surface, listener voidptr, data voidptr) int

	fn C.xdg_toplevel_set_title(toplevel &C.xdg_toplevel, title &char)
	fn C.xdg_toplevel_set_app_id(toplevel &C.xdg_toplevel, app_id &char)
	fn C.xdg_toplevel_set_fullscreen(toplevel &C.xdg_toplevel, output &C.wl_output)
	fn C.xdg_toplevel_unset_fullscreen(toplevel &C.xdg_toplevel)
	fn C.xdg_toplevel_set_min_size(toplevel &C.xdg_toplevel, width i32, height i32)
	fn C.xdg_toplevel_set_max_size(toplevel &C.xdg_toplevel, width i32, height i32)
	fn C.xdg_toplevel_destroy(toplevel &C.xdg_toplevel)
	fn C.xdg_toplevel_add_listener(toplevel &C.xdg_toplevel, listener voidptr, data voidptr) int

	fn C.wp_fractional_scale_manager_v1_get_fractional_scale(manager &C.wp_fractional_scale_manager_v1, surface &C.wl_surface) &C.wp_fractional_scale_v1
	fn C.wp_fractional_scale_manager_v1_destroy(manager &C.wp_fractional_scale_manager_v1)
	fn C.wp_fractional_scale_v1_add_listener(scale &C.wp_fractional_scale_v1, listener voidptr, data voidptr) int
	fn C.wp_fractional_scale_v1_destroy(scale &C.wp_fractional_scale_v1)

	fn C.wp_viewporter_get_viewport(viewporter &C.wp_viewporter, surface &C.wl_surface) &C.wp_viewport
	fn C.wp_viewporter_destroy(viewporter &C.wp_viewporter)
	fn C.wp_viewport_set_destination(viewport &C.wp_viewport, width i32, height i32)
	fn C.wp_viewport_destroy(viewport &C.wp_viewport)

	fn C.wp_cursor_shape_manager_v1_get_pointer(manager &C.wp_cursor_shape_manager_v1, pointer &C.wl_pointer) &C.wp_cursor_shape_device_v1
	fn C.wp_cursor_shape_manager_v1_destroy(manager &C.wp_cursor_shape_manager_v1)
	fn C.wp_cursor_shape_device_v1_set_shape(device &C.wp_cursor_shape_device_v1, serial u32, shape u32)
	fn C.wp_cursor_shape_device_v1_destroy(device &C.wp_cursor_shape_device_v1)

	fn C.zwp_pointer_constraints_v1_lock_pointer(constraints &C.zwp_pointer_constraints_v1, surface &C.wl_surface, pointer &C.wl_pointer, region voidptr, lifetime u32) &C.zwp_locked_pointer_v1
	fn C.zwp_pointer_constraints_v1_destroy(constraints &C.zwp_pointer_constraints_v1)
	fn C.zwp_locked_pointer_v1_destroy(locked &C.zwp_locked_pointer_v1)

	fn C.zwp_relative_pointer_manager_v1_get_relative_pointer(manager &C.zwp_relative_pointer_manager_v1, pointer &C.wl_pointer) &C.zwp_relative_pointer_v1
	fn C.zwp_relative_pointer_manager_v1_destroy(manager &C.zwp_relative_pointer_manager_v1)
	fn C.zwp_relative_pointer_v1_destroy(rp &C.zwp_relative_pointer_v1)
	fn C.zwp_relative_pointer_v1_add_listener(rp &C.zwp_relative_pointer_v1, listener voidptr, data voidptr) int

	fn C.zxdg_decoration_manager_v1_get_toplevel_decoration(manager &C.zxdg_decoration_manager_v1, toplevel &C.xdg_toplevel) &C.zxdg_toplevel_decoration_v1
	fn C.zxdg_decoration_manager_v1_destroy(manager &C.zxdg_decoration_manager_v1)
	fn C.zxdg_toplevel_decoration_v1_set_mode(decoration &C.zxdg_toplevel_decoration_v1, mode u32)
	fn C.zxdg_toplevel_decoration_v1_destroy(decoration &C.zxdg_toplevel_decoration_v1)

	// === xkbcommon ===

	pub struct C.xkb_context {}

	pub struct C.xkb_keymap {}

	pub struct C.xkb_state {}

	pub struct C.xkb_compose_table {}

	pub struct C.xkb_compose_state {}

	pub type Xkb_keysym_t = u32

	fn C.xkb_context_new(flags int) &C.xkb_context
	fn C.xkb_context_unref(ctx &C.xkb_context)

	fn C.xkb_keymap_new_from_string(ctx &C.xkb_context, str &char, format int, flags int) &C.xkb_keymap
	fn C.xkb_keymap_unref(keymap &C.xkb_keymap)

	fn C.xkb_state_new(keymap &C.xkb_keymap) &C.xkb_state
	fn C.xkb_state_unref(state &C.xkb_state)
	fn C.xkb_state_key_get_one_sym(state &C.xkb_state, key u32) Xkb_keysym_t
	fn C.xkb_state_key_get_utf8(state &C.xkb_state, key u32, buf &char, size usize) int
	fn C.xkb_state_update_mask(state &C.xkb_state, depressed u32, latched u32, locked u32, dep_group u32, lat_group u32, lock_group u32) int
	fn C.xkb_state_mod_name_is_active(state &C.xkb_state, name &char, @type int) int

	fn C.xkb_compose_table_new_from_locale(ctx &C.xkb_context, locale &char, flags int) &C.xkb_compose_table
	fn C.xkb_compose_table_unref(table &C.xkb_compose_table)
	fn C.xkb_compose_state_new(table &C.xkb_compose_table, flags int) &C.xkb_compose_state
	fn C.xkb_compose_state_unref(state &C.xkb_compose_state)

	// xkbcommon constants
	const xkb_context_no_flags = 0
	const xkb_keymap_format_text_v1 = 1
	const xkb_keymap_compile_no_flags = 0
	const xkb_state_mods_effective = 8
	const xkb_mod_name_shift = c'Shift'
	const xkb_mod_name_ctrl = c'Control'
	const xkb_mod_name_alt = c'Mod1'
	const xkb_mod_name_logo = c'Mod4'

	// Wayland keyboard state constants
	const wl_keyboard_keymap_format_xkb_v1 = u32(1)
	const wl_keyboard_key_state_pressed = u32(1)
	const wl_keyboard_key_state_released = u32(0)
	const wl_pointer_button_state_pressed = u32(1)
	const wl_pointer_button_state_released = u32(0)
	const wl_pointer_axis_vertical_scroll = u32(0)
	const wl_pointer_axis_horizontal_scroll = u32(1)

	// wl_seat capabilities
	const wl_seat_capability_pointer = u32(1)
	const wl_seat_capability_keyboard = u32(2)
	const wl_seat_capability_touch = u32(4)

	// XDG toplevel states
	const xdg_toplevel_state_maximized = u32(1)
	const xdg_toplevel_state_fullscreen = u32(2)
	const xdg_toplevel_state_resizing = u32(3)
	const xdg_toplevel_state_activated = u32(4)

	// zxdg_toplevel_decoration_v1 modes
	const zxdg_toplevel_decoration_v1_mode_client_side = u32(1)
	const zxdg_toplevel_decoration_v1_mode_server_side = u32(2)

	// zwp_pointer_constraints_v1 lifetime
	const zwp_pointer_constraints_v1_lifetime_persistent = u32(2)

	// wl_data_device_manager DND actions
	const wl_data_device_manager_dnd_action_copy = u32(1)

	// wp_cursor_shape constants (matching sapp MouseCursor values)
	const wp_cursor_shape_device_v1_shape_default = u32(1)
	const wp_cursor_shape_device_v1_shape_text = u32(5)
	const wp_cursor_shape_device_v1_shape_crosshair = u32(3)
	const wp_cursor_shape_device_v1_shape_pointer = u32(29)
	const wp_cursor_shape_device_v1_shape_ew_resize = u32(17)
	const wp_cursor_shape_device_v1_shape_ns_resize = u32(19)
	const wp_cursor_shape_device_v1_shape_nwse_resize = u32(14)
	const wp_cursor_shape_device_v1_shape_nesw_resize = u32(12)
	const wp_cursor_shape_device_v1_shape_all_scroll = u32(21)
	const wp_cursor_shape_device_v1_shape_not_allowed = u32(25)

	// MAP_FAILED
	const map_failed = voidptr(usize(~u64(0)))

	// Linux input event codes (from linux/input-event-codes.h)
	const btn_left = 0x110
	const btn_right = 0x111
	const btn_middle = 0x112

	// timerfd
	#include <sys/timerfd.h>

	pub struct C.itimerspec {
		it_interval C.timespec
		it_value    C.timespec
	}

	pub struct C.timespec {
		tv_sec  i64
		tv_nsec i64
	}

	fn C.timerfd_create(clockid int, flags int) int
	fn C.timerfd_settime(fd int, flags int, new_value &C.itimerspec, old_value &C.itimerspec) int

	const clock_monotonic = 1
	const tfd_cloexec = 0o2000000
	const tfd_nonblock = 0o4000
}

// === EGL (shared between Wayland and X11) ===

pub type EGLDisplay = voidptr
pub type EGLContext = voidptr
pub type EGLSurface = voidptr
pub type EGLConfig = voidptr
pub type EGLNativeWindowType = voidptr
pub type EGLint = i32

fn C.eglGetDisplay(native_display voidptr) EGLDisplay
fn C.eglInitialize(display EGLDisplay, major &EGLint, minor &EGLint) int
fn C.eglTerminate(display EGLDisplay) int
fn C.eglChooseConfig(display EGLDisplay, attribs &EGLint, configs &EGLConfig, config_size EGLint, num_configs &EGLint) int
fn C.eglCreateContext(display EGLDisplay, config EGLConfig, share_ctx EGLContext, attribs &EGLint) EGLContext
fn C.eglDestroyContext(display EGLDisplay, ctx EGLContext) int
fn C.eglCreateWindowSurface(display EGLDisplay, config EGLConfig, window EGLNativeWindowType, attribs &EGLint) EGLSurface
fn C.eglDestroySurface(display EGLDisplay, surface EGLSurface) int
fn C.eglMakeCurrent(display EGLDisplay, draw EGLSurface, read EGLSurface, ctx EGLContext) int
fn C.eglSwapBuffers(display EGLDisplay, surface EGLSurface) int
fn C.eglSwapInterval(display EGLDisplay, interval EGLint) int
fn C.eglGetError() EGLint
fn C.eglBindAPI(api u32) int
fn C.eglGetConfigAttrib(display EGLDisplay, config EGLConfig, attribute EGLint, value &EGLint) int

// OpenGL
fn C.glGetIntegerv(pname u32, data &i32)

// EGL constants
const egl_opengl_api = u32(0x30A2)
const egl_opengl_es_api = u32(0x30A0)
const egl_no_display = EGLDisplay(0)
const egl_no_context = EGLContext(0)
const egl_no_surface = EGLSurface(0)
const egl_none = EGLint(0x3038)
const egl_red_size = EGLint(0x3024)
const egl_green_size = EGLint(0x3023)
const egl_blue_size = EGLint(0x3022)
const egl_alpha_size = EGLint(0x3021)
const egl_depth_size = EGLint(0x3025)
const egl_stencil_size = EGLint(0x3026)
const egl_sample_buffers = EGLint(0x3032)
const egl_samples = EGLint(0x3031)
const egl_surface_type = EGLint(0x3033)
const egl_window_bit = EGLint(0x0004)
const egl_renderable_type = EGLint(0x3040)
const egl_opengl_bit = EGLint(0x0008)
const egl_opengl_es3_bit = EGLint(0x0040)
const egl_context_major_version = EGLint(0x3098)
const egl_context_minor_version = EGLint(0x30FB)
const egl_context_opengl_profile_mask = EGLint(0x30FD)
const egl_context_opengl_core_profile_bit = EGLint(0x00000001)
const egl_true = 1
const egl_false = 0

// GL constants
const gl_framebuffer_binding = u32(0x8CA6)

// === Linux syscalls (shared) ===

fn C.mmap(addr voidptr, length usize, prot int, flags int, fd int, offset i64) voidptr
fn C.munmap(addr voidptr, length usize) int
fn C.close(fd int) int
fn C.read(fd int, buf voidptr, count usize) isize
fn C.pipe(fds &int) int
fn C.strcmp(s1 &char, s2 &char) int
fn C.strncmp(s1 &char, s2 &char, n usize) int
fn C.strlen(s &char) usize
fn C.strtok(str &char, delim &char) &char
fn C.strtol(str &char, endptr &&char, base int) i64
fn C.memset(s voidptr, c int, n usize) voidptr
fn C.atof(s &char) f64

// mmap constants
const prot_read = 1
const map_private = 2

// === X11 / Xlib types and functions ===

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <X11/XKBlib.h>
#include <X11/extensions/XInput2.h>
#include <X11/Xcursor/Xcursor.h>

// X11 types
pub type XID = u64
pub type Atom = u64
pub type Window = u64
pub type Colormap = u64
pub type Cursor = u64
pub type KeySym = u64
pub type Time = u64
pub type VisualID = u64

// X11 types are defined by X11 headers (#preinclude above)
// Forward declarations for V type checking only
@[typedef]
pub struct C.Display {
pub mut:
	_ int // opaque - actual definition from X11 headers
}

@[typedef]
pub struct C.Visual {
pub mut:
	_ int // opaque - actual definition from X11 headers
}

@[typedef]
pub struct C.Screen {
pub mut:
	_ int // opaque - actual definition from X11 headers
}

@[typedef]
pub struct C.XSelectionEvent {
pub mut:
	type      int
	display   &C.Display = unsafe { nil }
	requestor Window
	selection Atom
	target    Atom
	property  Atom
	time      int
}

@[typedef]
pub struct C.XSelectionClearEvent {
pub mut:
	window    Window
	selection Atom
}

@[typedef]
pub struct C.XSelectionRequestEvent {
pub mut:
	display   &C.Display = unsafe { nil }
	owner     Window
	requestor Window
	selection Atom
	target    Atom
	property  Atom
	time      int
}

@[typedef]
pub struct C.XDestroyWindowEvent {
pub mut:
	window Window
}

@[typedef]
pub struct C.XPropertyEvent {
pub mut:
	state int
	atom  Atom
}

@[typedef]
pub struct C.XClientMessageEvent {
pub mut:
	window       Window
	format       int
	message_type Atom
	data         C.XClientMessageData
}

@[typedef]
pub union C.XClientMessageData {
pub mut:
	b [20]u8
	s [10]i16
	l [5]i64
}

@[typedef]
pub struct C.XGenericEventCookie {
pub mut:
	extension int
	evtype    int
	data      voidptr
}

@[typedef]
pub struct C.XKeyEvent {
pub mut:
	keycode u32
	state   u32
}

@[typedef]
pub struct C.XButtonEvent {
pub mut:
	button u32
	state  u32
	x      int
	y      int
}

@[typedef]
pub struct C.XMotionEvent {
pub mut:
	x     int
	y     int
	state u32
}

@[typedef]
pub struct C.XCrossingEvent {
pub mut:
	x     int
	y     int
	state u32
}

@[typedef]
pub struct C.XFocusChangeEvent {
pub mut:
	mode int
}

@[typedef]
pub union C.XEvent {
pub mut:
	type              int
	xclient           C.XClientMessageEvent
	xkey              C.XKeyEvent
	xbutton           C.XButtonEvent
	xmotion           C.XMotionEvent
	xcrossing         C.XCrossingEvent
	xfocus            C.XFocusChangeEvent
	xproperty         C.XPropertyEvent
	xselection        C.XSelectionEvent
	xselectionrequest C.XSelectionRequestEvent
	xselectionclear   C.XSelectionClearEvent
	xdestroywindow    C.XDestroyWindowEvent
	xcookie           C.XGenericEventCookie
}

pub struct C.XrmDatabase__rec {}

pub type XrmDatabase = &C.XrmDatabase__rec

pub struct C.XSetWindowAttributes {
mut:
	colormap     Colormap
	border_pixel u64
	event_mask   u64
}

pub struct C.XWindowAttributes {
mut:
	width     int
	height    int
	map_state int
}

pub struct C.XSizeHints {
mut:
	flags       i64
	win_gravity int
}

pub struct C.XVisualInfo {
mut:
	visual   &C.Visual = unsafe { nil }
	visualid VisualID
	depth    int
}

// X11 event types are defined in vlib/x/x11/x11.v
// XEvent union is defined here since it's used extensively in sokol

pub union C.XEvent {
pub mut:
	@type             int
	xclient           C.XClientMessageEvent
	xkey              C.XKeyEvent
	xbutton           C.XButtonEvent
	xmotion           C.XMotionEvent
	xcrossing         C.XCrossingEvent
	xfocus            C.XFocusChangeEvent
	xproperty         C.XPropertyEvent
	xselection        C.XSelectionEvent
	xselectionrequest C.XSelectionRequestEvent
	xselectionclear   C.XSelectionClearEvent
	xdestroywindow    C.XDestroyWindowEvent
	xcookie           C.XGenericEventCookie
}

pub struct C.XKeyEvent {
pub mut:
	keycode u32
	state   u32
}

pub struct C.XButtonEvent {
pub mut:
	button u32
	state  u32
	x      int
	y      int
}

pub struct C.XMotionEvent {
pub mut:
	x     int
	y     int
	state u32
}

pub struct C.XCrossingEvent {
pub mut:
	x     int
	y     int
	state u32
}

pub struct C.XFocusChangeEvent {
pub mut:
	mode int
}

pub struct C.XPropertyEvent {
pub mut:
	state int
	atom  Atom
}

// XSelection* structs are forward-declared above
// Full definitions come from X11 headers or clipboard module

pub struct C.XClientMessageEvent {
pub mut:
	window       Window
	format       int
	message_type Atom
	data         C.XClientMessageData
}

pub union C.XClientMessageData {
pub mut:
	l [5]i64
}

pub struct C.XGenericEventCookie {
pub mut:
	extension int
	evtype    int
	data      voidptr
}

pub struct C.XrmValue {
	addr &char = unsafe { nil }
}

// XKB types
pub struct C.XkbDescRec {
mut:
	min_key_code u8
	max_key_code u8
	names        &C.XkbNamesRec = unsafe { nil }
}

pub type XkbDescPtr = &C.XkbDescRec

pub struct C.XkbNamesRec {
mut:
	keys            &C.XkbKeyNameRec  = unsafe { nil }
	key_aliases     &C.XkbKeyAliasRec = unsafe { nil }
	num_key_aliases u8
}

pub struct C.XkbKeyNameRec {
mut:
	name [4]u8
}

pub struct C.XkbKeyAliasRec {
mut:
	real  [4]u8
	alias [4]u8
}

// XInput2 types
pub struct C.XIEventMask {
mut:
	deviceid int
	mask_len int
	mask     &u8 = unsafe { nil }
}

pub struct C.XIRawEvent {
mut:
	valuators  C.XIValuatorState
	raw_values &f64 = unsafe { nil }
}

pub struct C.XIValuatorState {
mut:
	mask_len int
	mask     &u8 = unsafe { nil }
}

// Xcursor types
pub struct C.XcursorImage {
mut:
	width  u32
	height u32
	xhot   u32
	yhot   u32
	pixels &u32 = unsafe { nil }
}

pub type XcursorPixel = u32

// === Xlib functions ===

fn C.XOpenDisplay(name &char) &C.Display
fn C.XCloseDisplay(display &C.Display) int
fn C.XDefaultScreen(display &C.Display) int
fn C.XDefaultRootWindow(display &C.Display) Window
fn C.XDefaultVisual(display &C.Display, screen int) &C.Visual
fn C.XDefaultDepth(display &C.Display, screen int) int
fn C.XDisplayWidth(display &C.Display, screen int) int
fn C.XDisplayHeight(display &C.Display, screen int) int
fn C.XDisplayWidthMM(display &C.Display, screen int) int
fn C.XInternAtom(display &C.Display, name &char, only_if_exists int) Atom
fn C.XCreateColormap(display &C.Display, window Window, visual &C.Visual, alloc int) Colormap
fn C.XFreeColormap(display &C.Display, colormap Colormap) int
fn C.XCreateWindow(display &C.Display, parent Window, x int, y int, width u32, height u32, border_width u32, depth int, class_ u32, visual &C.Visual, valuemask u64, attributes &C.XSetWindowAttributes) Window
fn C.XDestroyWindow(display &C.Display, window Window) int
fn C.XMapWindow(display &C.Display, window Window) int
fn C.XUnmapWindow(display &C.Display, window Window) int
fn C.XRaiseWindow(display &C.Display, window Window) int
fn C.XFlush(display &C.Display) int
fn C.XSync(display &C.Display, discard int) int
fn C.XPending(display &C.Display) int
fn C.XNextEvent(display &C.Display, event &C.XEvent) int
fn C.XCheckTypedWindowEvent(display &C.Display, window Window, event_type int, event &C.XEvent) int
fn C.XSendEvent(display &C.Display, window Window, propagate int, event_mask i64, event &C.XEvent) int
fn C.XFilterEvent(event &C.XEvent, window Window) int
fn C.XSetWMProtocols(display &C.Display, window Window, protocols &Atom, count int) int
fn C.XSetWMNormalHints(display &C.Display, window Window, hints &C.XSizeHints) int
fn C.XAllocSizeHints() &C.XSizeHints
fn C.XGetWindowAttributes(display &C.Display, window Window, attrs &C.XWindowAttributes) int
fn C.XGetWindowProperty(display &C.Display, window Window, property Atom, long_offset i64, long_length i64, delete int, req_type Atom, actual_type &Atom, actual_format &int, nitems &u64, bytes_after &u64, prop &&&u8) int
fn C.XChangeProperty(display &C.Display, window Window, property Atom, @type Atom, format int, mode int, data &u8, nelements int) int
fn C.Xutf8SetWMProperties(display &C.Display, window Window, window_name &char, icon_name &char, argv &&char, argc int, normal_hints voidptr, wm_hints voidptr, class_hints voidptr)
fn C.XSetSelectionOwner(display &C.Display, selection Atom, owner Window, time Time)
fn C.XGetSelectionOwner(display &C.Display, selection Atom) Window
fn C.XConvertSelection(display &C.Display, selection Atom, target Atom, property Atom, requestor Window, time Time)
fn C.XFree(data voidptr) int
fn C.XDefineCursor(display &C.Display, window Window, cursor Cursor) int
fn C.XUndefineCursor(display &C.Display, window Window) int
fn C.XCreateFontCursor(display &C.Display, shape u32) Cursor
fn C.XFreeCursor(display &C.Display, cursor Cursor) int
fn C.XGrabPointer(display &C.Display, grab_window Window, owner_events int, event_mask u32, pointer_mode int, keyboard_mode int, confine_to Window, cursor Cursor, time Time) int
fn C.XUngrabPointer(display &C.Display, time Time) int
fn C.XWarpPointer(display &C.Display, src_w Window, dst_w Window, src_x int, src_y int, src_width u32, src_height u32, dst_x int, dst_y int) int
fn C.XSetErrorHandler(handler voidptr) voidptr
fn C.XGetKeyboardMapping(display &C.Display, first_keycode u8, keycode_count int, keysyms_per_keycode &int) &KeySym
fn C.XLookupString(event &C.XKeyEvent, buf &char, buf_len int, keysym &KeySym, status voidptr) int
fn C.XQueryExtension(display &C.Display, name &char, major_opcode &int, first_event &int, first_error &int) int
fn C.XResourceManagerString(display &C.Display) &char
fn C.XGetVisualInfo(display &C.Display, vinfo_mask i64, vinfo_template &C.XVisualInfo, nitems &int) &C.XVisualInfo
fn C.XInitThreads() int
fn C.XrmInitialize()
fn C.XrmGetStringDatabase(data &char) XrmDatabase
fn C.XrmDestroyDatabase(db XrmDatabase)
fn C.XrmGetResource(db XrmDatabase, str_name &char, str_class &char, str_type &&char, value &C.XrmValue) int
fn C.XGetEventData(display &C.Display, cookie &C.XGenericEventCookie) int
fn C.XFreeEventData(display &C.Display, cookie &C.XGenericEventCookie)

// XKB functions
fn C.XkbGetMap(display &C.Display, which u32, device_spec u32) XkbDescPtr
fn C.XkbGetNames(display &C.Display, which u32, desc XkbDescPtr) int
fn C.XkbFreeNames(desc XkbDescPtr, which u32, free_map int) int
fn C.XkbFreeKeyboard(desc XkbDescPtr, which u32, free_all int) int
fn C.XkbSetDetectableAutoRepeat(display &C.Display, detectable int, supported &int) int

// XInput2 functions
fn C.XIQueryVersion(display &C.Display, major &int, minor &int) int
fn C.XISelectEvents(display &C.Display, window Window, masks &C.XIEventMask, num_masks int) int

// Xcursor functions
fn C.XcursorImageCreate(width int, height int) &C.XcursorImage
fn C.XcursorImageDestroy(image &C.XcursorImage)
fn C.XcursorImageLoadCursor(display &C.Display, image &C.XcursorImage) Cursor
fn C.XcursorLibraryLoadImage(name &char, theme &char, size int) &C.XcursorImage
fn C.XcursorGetTheme(display &C.Display) &char
fn C.XcursorGetDefaultSize(display &C.Display) int

// poll
#include <poll.h>

pub struct C.pollfd {
mut:
	fd      int
	events  i16
	revents i16
}

fn C.poll(fds &C.pollfd, nfds u64, timeout int) int

fn C.ConnectionNumber(display &C.Display) int

fn C.atof(str &char) f64
fn C.strncmp(s1 &char, s2 &char, n usize) int

// X11 constants
const x_false = 0
const x_true = 1
const x_none = Atom(0)
const xa_atom = Atom(4)
const xa_cardinal = Atom(6)
const alloc_none = 0
const cw_border_pixel = u64(1 << 11)
const cw_colormap = u64(1 << 13)
const cw_event_mask = u64(1 << 11) // this is actually CWEventMask
const input_output = u32(1)
const structure_notify_mask = i64(1 << 17)
const key_press_mask = i64(1 << 0)
const key_release_mask = i64(1 << 1)
const pointer_motion_mask = i64(1 << 6)
const button_press_mask = i64(1 << 2)
const button_release_mask = i64(1 << 3)
const exposure_mask = i64(1 << 15)
const focus_change_mask = i64(1 << 21)
const visibility_change_mask = i64(1 << 16)
const enter_window_mask = i64(1 << 4)
const leave_window_mask = i64(1 << 5)
const property_change_mask = i64(1 << 22)
const substructure_notify_mask = i64(1 << 19)
const substructure_redirect_mask = i64(1 << 20)
const no_event_mask = i64(0)

const prop_mode_replace = 0
const pw_gravity = i64(1 << 9) // PWinGravity flag
const center_gravity = 5

const is_viewable = 2
const visibility_notify = 15
const normal_state = 1
const iconic_state = 3
const withdrawn_state = 0

const current_time = Time(0)
const grab_mode_async = 1

// Event types
const x_generic_event = 35
const x_focus_in = 9
const x_focus_out = 10
const x_key_press = 2
const x_key_release = 3
const x_button_press = 4
const x_button_release = 5
const x_enter_notify = 7
const x_leave_notify = 8
const x_motion_notify = 6
const x_property_notify = 28
const x_selection_notify = 31
const x_selection_request = 30
const x_destroy_notify = 17
const x_client_message = 33
const property_new_value = 0

const notify_grab = 1
const notify_ungrab = 2

// Mouse buttons
const x_button1 = u32(1)
const x_button2 = u32(2)
const x_button3 = u32(3)

// Modifier masks
const shift_mask = u32(1 << 0)
const control_mask = u32(1 << 2)
const mod1_mask = u32(1 << 3) // Alt
const mod4_mask = u32(1 << 6) // Super
const button1_mask = u32(1 << 8)
const button2_mask = u32(1 << 9)
const button3_mask = u32(1 << 10)

// Cursor shapes for XCreateFontCursor
const xc_left_ptr = u32(68)
const xc_xterm = u32(152)
const xc_crosshair = u32(34)
const xc_hand2 = u32(60)
const xc_sb_h_double_arrow = u32(108)
const xc_sb_v_double_arrow = u32(116)
const xc_fleur = u32(52)

// XKB constants
const xkb_use_core_kbd = u32(0x0100)
const xkb_key_names_mask = u32(1 << 9)
const xkb_key_aliases_mask = u32(1 << 10)
const xkb_key_name_length = 4

// XInput2 constants
const xi_all_master_devices = -1
const xi_raw_motion = 17
const pollin = i16(0x001)

// XDnD
const x11_xdnd_version = 5

// KeySym constants
const xk_escape = KeySym(0xff1b)
const xk_tab = KeySym(0xff09)
const xk_shift_l = KeySym(0xffe1)
const xk_shift_r = KeySym(0xffe2)
const xk_control_l = KeySym(0xffe3)
const xk_control_r = KeySym(0xffe4)
const xk_meta_l = KeySym(0xffe7)
const xk_alt_l = KeySym(0xffe9)
const xk_mode_switch = KeySym(0xff7e)
const xk_iso_level3_shift = KeySym(0xfe03)
const xk_meta_r = KeySym(0xffe8)
const xk_alt_r = KeySym(0xffea)
const xk_super_l = KeySym(0xffeb)
const xk_super_r = KeySym(0xffec)
const xk_menu = KeySym(0xff67)
const xk_num_lock = KeySym(0xff7f)
const xk_caps_lock = KeySym(0xffe5)
const xk_print = KeySym(0xff61)
const xk_scroll_lock = KeySym(0xff14)
const xk_pause = KeySym(0xff13)
const xk_delete = KeySym(0xffff)
const xk_backspace = KeySym(0xff08)
const xk_return = KeySym(0xff0d)
const xk_home = KeySym(0xff50)
const xk_end = KeySym(0xff57)
const xk_page_up = KeySym(0xff55)
const xk_page_down = KeySym(0xff56)
const xk_insert = KeySym(0xff63)
const xk_left = KeySym(0xff51)
const xk_right = KeySym(0xff53)
const xk_down = KeySym(0xff54)
const xk_up = KeySym(0xff52)
const xk_f1 = KeySym(0xffbe)
const xk_f2 = KeySym(0xffbf)
const xk_f3 = KeySym(0xffc0)
const xk_f4 = KeySym(0xffc1)
const xk_f5 = KeySym(0xffc2)
const xk_f6 = KeySym(0xffc3)
const xk_f7 = KeySym(0xffc4)
const xk_f8 = KeySym(0xffc5)
const xk_f9 = KeySym(0xffc6)
const xk_f10 = KeySym(0xffc7)
const xk_f11 = KeySym(0xffc8)
const xk_f12 = KeySym(0xffc9)
const xk_f13 = KeySym(0xffca)
const xk_f14 = KeySym(0xffcb)
const xk_f15 = KeySym(0xffcc)
const xk_f16 = KeySym(0xffcd)
const xk_f17 = KeySym(0xffce)
const xk_f18 = KeySym(0xffcf)
const xk_f19 = KeySym(0xffd0)
const xk_f20 = KeySym(0xffd1)
const xk_f21 = KeySym(0xffd2)
const xk_f22 = KeySym(0xffd3)
const xk_f23 = KeySym(0xffd4)
const xk_f24 = KeySym(0xffd5)
const xk_f25 = KeySym(0xffd6)
const xk_kp_divide = KeySym(0xffaf)
const xk_kp_multiply = KeySym(0xffaa)
const xk_kp_subtract = KeySym(0xffad)
const xk_kp_add = KeySym(0xffab)
const xk_kp_0 = KeySym(0xffb0)
const xk_kp_1 = KeySym(0xffb1)
const xk_kp_2 = KeySym(0xffb2)
const xk_kp_3 = KeySym(0xffb3)
const xk_kp_4 = KeySym(0xffb4)
const xk_kp_5 = KeySym(0xffb5)
const xk_kp_6 = KeySym(0xffb6)
const xk_kp_7 = KeySym(0xffb7)
const xk_kp_8 = KeySym(0xffb8)
const xk_kp_9 = KeySym(0xffb9)
const xk_kp_separator = KeySym(0xffac)
const xk_kp_decimal = KeySym(0xffae)
const xk_kp_equal = KeySym(0xffbd)
const xk_kp_enter = KeySym(0xff8d)
const xk_kp_insert = KeySym(0xff9e)
const xk_kp_end = KeySym(0xff9c)
const xk_kp_down = KeySym(0xff99)
const xk_kp_page_down = KeySym(0xff9b)
const xk_kp_left = KeySym(0xff96)
const xk_kp_right = KeySym(0xff98)
const xk_kp_home = KeySym(0xff95)
const xk_kp_up = KeySym(0xff97)
const xk_kp_page_up = KeySym(0xff9a)
const xk_kp_delete = KeySym(0xff9f)
const xk_a = KeySym(0x0061)
const xk_b = KeySym(0x0062)
const xk_c = KeySym(0x0063)
const xk_d = KeySym(0x0064)
const xk_e = KeySym(0x0065)
const xk_f = KeySym(0x0066)
const xk_g = KeySym(0x0067)
const xk_h = KeySym(0x0068)
const xk_i = KeySym(0x0069)
const xk_j = KeySym(0x006a)
const xk_k = KeySym(0x006b)
const xk_l = KeySym(0x006c)
const xk_m = KeySym(0x006d)
const xk_n = KeySym(0x006e)
const xk_o = KeySym(0x006f)
const xk_p = KeySym(0x0070)
const xk_q = KeySym(0x0071)
const xk_r = KeySym(0x0072)
const xk_s = KeySym(0x0073)
const xk_t = KeySym(0x0074)
const xk_u = KeySym(0x0075)
const xk_v = KeySym(0x0076)
const xk_w = KeySym(0x0077)
const xk_x = KeySym(0x0078)
const xk_y = KeySym(0x0079)
const xk_z = KeySym(0x007a)
const xk_0 = KeySym(0x0030)
const xk_1 = KeySym(0x0031)
const xk_2 = KeySym(0x0032)
const xk_3 = KeySym(0x0033)
const xk_4 = KeySym(0x0034)
const xk_5 = KeySym(0x0035)
const xk_6 = KeySym(0x0036)
const xk_7 = KeySym(0x0037)
const xk_8 = KeySym(0x0038)
const xk_9 = KeySym(0x0039)
const xk_space = KeySym(0x0020)
const xk_minus = KeySym(0x002d)
const xk_equal = KeySym(0x003d)
const xk_bracketleft = KeySym(0x005b)
const xk_bracketright = KeySym(0x005d)
const xk_backslash = KeySym(0x005c)
const xk_semicolon = KeySym(0x003b)
const xk_apostrophe = KeySym(0x0027)
const xk_grave = KeySym(0x0060)
const xk_comma = KeySym(0x002c)
const xk_period = KeySym(0x002e)
const xk_slash = KeySym(0x002f)
const xk_less = KeySym(0x003c)

// XIMaskLen macro equivalent
fn xi_mask_len(event int) int {
	return (event >> 3) + 1
}

// XISetMask macro equivalent
fn xi_set_mask(mask &u8, event int) {
	unsafe {
		mut p := mask + (event >> 3)
		*p = *p | u8(1 << (event & 7))
	}
}

// XIMaskIsSet macro equivalent
fn xi_mask_is_set(mask &u8, event int) bool {
	unsafe {
		return (mask[event >> 3] & u8(1 << (event & 7))) != 0
	}
}

// VisualIDMask for XGetVisualInfo
const visual_id_mask = i64(0x1)
