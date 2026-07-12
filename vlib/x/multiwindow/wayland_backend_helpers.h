#ifndef V_MULTIWINDOW_WAYLAND_BACKEND_HELPERS_H
#define V_MULTIWINDOW_WAYLAND_BACKEND_HELPERS_H

#include <stdint.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <EGL/egl.h>
#include <wayland-client.h>
#include <wayland-egl.h>

#ifndef EGL_CONTEXT_MAJOR_VERSION
#define EGL_CONTEXT_MAJOR_VERSION 0x3098
#endif
#ifndef EGL_CONTEXT_MINOR_VERSION
#define EGL_CONTEXT_MINOR_VERSION 0x30FB
#endif
#ifndef EGL_CONTEXT_OPENGL_PROFILE_MASK
#define EGL_CONTEXT_OPENGL_PROFILE_MASK 0x30FD
#endif
#ifndef EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT
#define EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT 0x00000001
#endif

void v_multiwindow_wayland_registry_handle_global(voidptr data, struct wl_registry *registry, u32 name, char *iface, u32 version);
void v_multiwindow_wayland_registry_handle_global_remove(voidptr data, struct wl_registry *registry, u32 name);
void v_multiwindow_wayland_xdg_wm_base_ping(void *data, void *wm_base, uint32_t serial);
void v_multiwindow_wayland_xdg_surface_configure(void *data, void *xdg_surface, uint32_t serial);
void v_multiwindow_wayland_xdg_toplevel_configure(void *data, void *toplevel, int width, int height, struct wl_array *states);
void v_multiwindow_wayland_xdg_toplevel_close(void *data, void *toplevel);
void v_multiwindow_wayland_xdg_toplevel_decoration_configure(void *data, void *decoration, uint32_t mode);
void v_multiwindow_wayland_seat_capabilities(void *data, void *seat, uint32_t caps);
void v_multiwindow_wayland_seat_name(void *data, void *seat, char *name);
void v_multiwindow_wayland_pointer_enter(void *data, void *pointer, uint32_t serial, void *surface, double x, double y);
void v_multiwindow_wayland_pointer_leave(void *data, void *pointer, uint32_t serial, void *surface);
void v_multiwindow_wayland_pointer_motion(void *data, void *pointer, uint32_t time, double x, double y);
void v_multiwindow_wayland_pointer_button(void *data, void *pointer, uint32_t serial, uint32_t time, uint32_t button, uint32_t state);
void v_multiwindow_wayland_pointer_axis(void *data, void *pointer, uint32_t time, uint32_t axis, double value);
void v_multiwindow_wayland_keyboard_keymap(void *data, void *keyboard, uint32_t format, int fd, uint32_t size);
void v_multiwindow_wayland_keyboard_enter(void *data, void *keyboard, uint32_t serial, void *surface);
void v_multiwindow_wayland_keyboard_leave(void *data, void *keyboard, uint32_t serial, void *surface);
void v_multiwindow_wayland_keyboard_key(void *data, void *keyboard, uint32_t serial, uint32_t time, uint32_t key, uint32_t state);
void v_multiwindow_wayland_keyboard_modifiers(void *data, void *keyboard, uint32_t serial, uint32_t mods_depressed, uint32_t mods_latched, uint32_t mods_locked, uint32_t group);
void v_multiwindow_wayland_keyboard_repeat_info(void *data, void *keyboard, int32_t rate, int32_t delay);
void v_multiwindow_wayland_touch_down(void *data, void *touch, uint32_t serial, uint32_t time, void *surface, int32_t id, double x, double y);
void v_multiwindow_wayland_touch_up(void *data, void *touch, uint32_t serial, uint32_t time, int32_t id);
void v_multiwindow_wayland_touch_motion(void *data, void *touch, uint32_t time, int32_t id, double x, double y);
void v_multiwindow_wayland_touch_cancel(void *data, void *touch);
void v_multiwindow_wayland_data_offer_offer(void *data, void *offer, char *mime_type);
void v_multiwindow_wayland_data_offer_source_actions(void *data, void *offer, uint32_t source_actions);
void v_multiwindow_wayland_data_offer_action(void *data, void *offer, uint32_t dnd_action);
void v_multiwindow_wayland_data_device_data_offer(void *data, void *device, void *offer);
void v_multiwindow_wayland_data_device_enter(void *data, void *device, uint32_t serial, void *surface, double x, double y, void *offer);
void v_multiwindow_wayland_data_device_leave(void *data, void *device);
void v_multiwindow_wayland_data_device_motion(void *data, void *device, uint32_t time, double x, double y);
void v_multiwindow_wayland_data_device_drop(void *data, void *device);
void v_multiwindow_wayland_data_device_selection(void *data, void *device, void *offer);
void v_multiwindow_wayland_buffer_release(void *data, void *buffer);

#if !defined(XDG_SHELL_CLIENT_PROTOCOL_H)
struct xdg_wm_base;
struct xdg_surface;
struct xdg_toplevel;

struct xdg_wm_base_listener {
	void (*ping)(void *data, struct xdg_wm_base *xdg_wm_base, uint32_t serial);
};

struct xdg_surface_listener {
	void (*configure)(void *data, struct xdg_surface *xdg_surface, uint32_t serial);
};

struct xdg_toplevel_listener {
	void (*configure)(void *data, struct xdg_toplevel *xdg_toplevel, int32_t width, int32_t height, struct wl_array *states);
	void (*close)(void *data, struct xdg_toplevel *xdg_toplevel);
};
#endif

#ifndef XDG_WM_BASE_DESTROY
#define XDG_WM_BASE_DESTROY 0
#endif
#ifndef XDG_WM_BASE_GET_XDG_SURFACE
#define XDG_WM_BASE_GET_XDG_SURFACE 2
#endif
#ifndef XDG_WM_BASE_PONG
#define XDG_WM_BASE_PONG 3
#endif
#ifndef XDG_SURFACE_DESTROY
#define XDG_SURFACE_DESTROY 0
#endif
#ifndef XDG_SURFACE_GET_TOPLEVEL
#define XDG_SURFACE_GET_TOPLEVEL 1
#endif
#ifndef XDG_SURFACE_ACK_CONFIGURE
#define XDG_SURFACE_ACK_CONFIGURE 4
#endif
#ifndef XDG_TOPLEVEL_DESTROY
#define XDG_TOPLEVEL_DESTROY 0
#endif
#ifndef XDG_TOPLEVEL_SET_TITLE
#define XDG_TOPLEVEL_SET_TITLE 2
#endif
#ifndef XDG_TOPLEVEL_SET_APP_ID
#define XDG_TOPLEVEL_SET_APP_ID 3
#endif
#ifndef XDG_TOPLEVEL_SET_MAX_SIZE
#define XDG_TOPLEVEL_SET_MAX_SIZE 7
#endif
#ifndef XDG_TOPLEVEL_SET_MIN_SIZE
#define XDG_TOPLEVEL_SET_MIN_SIZE 8
#endif
#ifndef XDG_TOPLEVEL_SET_FULLSCREEN
#define XDG_TOPLEVEL_SET_FULLSCREEN 11
#endif
#ifndef XDG_TOPLEVEL_MOVE
#define XDG_TOPLEVEL_MOVE 5
#endif
#ifndef XDG_TOPLEVEL_RESIZE
#define XDG_TOPLEVEL_RESIZE 6
#endif
#ifndef XDG_TOPLEVEL_RESIZE_EDGE_TOP
#define XDG_TOPLEVEL_RESIZE_EDGE_TOP 1
#endif
#ifndef XDG_TOPLEVEL_RESIZE_EDGE_BOTTOM
#define XDG_TOPLEVEL_RESIZE_EDGE_BOTTOM 2
#endif
#ifndef XDG_TOPLEVEL_RESIZE_EDGE_LEFT
#define XDG_TOPLEVEL_RESIZE_EDGE_LEFT 4
#endif
#ifndef XDG_TOPLEVEL_RESIZE_EDGE_TOP_LEFT
#define XDG_TOPLEVEL_RESIZE_EDGE_TOP_LEFT 5
#endif
#ifndef XDG_TOPLEVEL_RESIZE_EDGE_BOTTOM_LEFT
#define XDG_TOPLEVEL_RESIZE_EDGE_BOTTOM_LEFT 6
#endif
#ifndef XDG_TOPLEVEL_RESIZE_EDGE_RIGHT
#define XDG_TOPLEVEL_RESIZE_EDGE_RIGHT 8
#endif
#ifndef XDG_TOPLEVEL_RESIZE_EDGE_TOP_RIGHT
#define XDG_TOPLEVEL_RESIZE_EDGE_TOP_RIGHT 9
#endif
#ifndef XDG_TOPLEVEL_RESIZE_EDGE_BOTTOM_RIGHT
#define XDG_TOPLEVEL_RESIZE_EDGE_BOTTOM_RIGHT 10
#endif
extern const struct wl_interface v_multiwindow_xdg_wm_base_interface;
extern const struct wl_interface v_multiwindow_xdg_surface_interface;
extern const struct wl_interface v_multiwindow_xdg_toplevel_interface;

#if !defined(XDG_DECORATION_UNSTABLE_V1_CLIENT_PROTOCOL_H)
struct zxdg_decoration_manager_v1;
struct zxdg_toplevel_decoration_v1;

struct zxdg_toplevel_decoration_v1_listener {
	void (*configure)(void *data, struct zxdg_toplevel_decoration_v1 *decoration, uint32_t mode);
};
#endif

#ifndef ZXDG_DECORATION_MANAGER_V1_DESTROY
#define ZXDG_DECORATION_MANAGER_V1_DESTROY 0
#endif
#ifndef ZXDG_DECORATION_MANAGER_V1_GET_TOPLEVEL_DECORATION
#define ZXDG_DECORATION_MANAGER_V1_GET_TOPLEVEL_DECORATION 1
#endif
#ifndef ZXDG_TOPLEVEL_DECORATION_V1_DESTROY
#define ZXDG_TOPLEVEL_DECORATION_V1_DESTROY 0
#endif
#ifndef ZXDG_TOPLEVEL_DECORATION_V1_SET_MODE
#define ZXDG_TOPLEVEL_DECORATION_V1_SET_MODE 1
#endif
#ifndef ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE
#define ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE 1
#endif
#ifndef ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE
#define ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE 2
#endif
extern const struct wl_interface v_multiwindow_zxdg_decoration_manager_v1_interface;
extern const struct wl_interface v_multiwindow_zxdg_toplevel_decoration_v1_interface;

struct wp_cursor_shape_manager_v1;
struct wp_cursor_shape_device_v1;

#ifndef WP_CURSOR_SHAPE_MANAGER_V1_DESTROY
#define WP_CURSOR_SHAPE_MANAGER_V1_DESTROY 0
#endif
#ifndef WP_CURSOR_SHAPE_MANAGER_V1_GET_POINTER
#define WP_CURSOR_SHAPE_MANAGER_V1_GET_POINTER 1
#endif
#ifndef WP_CURSOR_SHAPE_DEVICE_V1_DESTROY
#define WP_CURSOR_SHAPE_DEVICE_V1_DESTROY 0
#endif
#ifndef WP_CURSOR_SHAPE_DEVICE_V1_SET_SHAPE
#define WP_CURSOR_SHAPE_DEVICE_V1_SET_SHAPE 1
#endif
extern const struct wl_interface v_multiwindow_wp_cursor_shape_manager_v1_interface;
extern const struct wl_interface v_multiwindow_wp_cursor_shape_device_v1_interface;

static void v_multiwindow_wayland_registry_handle_global_trampoline(void *data, struct wl_registry *registry, uint32_t name, const char *iface, uint32_t version) {
	v_multiwindow_wayland_registry_handle_global(data, registry, name, (char *)iface, version);
}

static void v_multiwindow_wayland_registry_handle_global_remove_trampoline(void *data, struct wl_registry *registry, uint32_t name) {
	v_multiwindow_wayland_registry_handle_global_remove(data, registry, name);
}

static void v_multiwindow_wayland_xdg_wm_base_ping_trampoline(void *data, struct xdg_wm_base *wm_base, uint32_t serial) {
	v_multiwindow_wayland_xdg_wm_base_ping(data, (void *)wm_base, serial);
}

static void v_multiwindow_wayland_xdg_surface_configure_trampoline(void *data, struct xdg_surface *xdg_surface, uint32_t serial) {
	v_multiwindow_wayland_xdg_surface_configure(data, (void *)xdg_surface, serial);
}

static void v_multiwindow_wayland_xdg_toplevel_configure_trampoline(void *data, struct xdg_toplevel *toplevel, int32_t width, int32_t height, struct wl_array *states) {
	v_multiwindow_wayland_xdg_toplevel_configure(data, (void *)toplevel, (int)width, (int)height, states);
}

static void v_multiwindow_wayland_xdg_toplevel_close_trampoline(void *data, struct xdg_toplevel *toplevel) {
	v_multiwindow_wayland_xdg_toplevel_close(data, (void *)toplevel);
}

static void v_multiwindow_wayland_seat_capabilities_trampoline(void *data, struct wl_seat *seat, uint32_t caps) {
	v_multiwindow_wayland_seat_capabilities(data, (void *)seat, caps);
}

static void v_multiwindow_wayland_seat_name_trampoline(void *data, struct wl_seat *seat, const char *name) {
	v_multiwindow_wayland_seat_name(data, (void *)seat, (char *)name);
}

static void v_multiwindow_wayland_pointer_enter_trampoline(void *data, struct wl_pointer *pointer, uint32_t serial, struct wl_surface *surface, wl_fixed_t sx, wl_fixed_t sy) {
	v_multiwindow_wayland_pointer_enter(data, (void *)pointer, serial, (void *)surface, wl_fixed_to_double(sx), wl_fixed_to_double(sy));
}

static void v_multiwindow_wayland_pointer_leave_trampoline(void *data, struct wl_pointer *pointer, uint32_t serial, struct wl_surface *surface) {
	v_multiwindow_wayland_pointer_leave(data, (void *)pointer, serial, (void *)surface);
}

static void v_multiwindow_wayland_pointer_motion_trampoline(void *data, struct wl_pointer *pointer, uint32_t time, wl_fixed_t sx, wl_fixed_t sy) {
	v_multiwindow_wayland_pointer_motion(data, (void *)pointer, time, wl_fixed_to_double(sx), wl_fixed_to_double(sy));
}

static void v_multiwindow_wayland_pointer_button_trampoline(void *data, struct wl_pointer *pointer, uint32_t serial, uint32_t time, uint32_t button, uint32_t state) {
	v_multiwindow_wayland_pointer_button(data, (void *)pointer, serial, time, button, state);
}

static void v_multiwindow_wayland_pointer_axis_trampoline(void *data, struct wl_pointer *pointer, uint32_t time, uint32_t axis, wl_fixed_t value) {
	v_multiwindow_wayland_pointer_axis(data, (void *)pointer, time, axis, wl_fixed_to_double(value));
}

static void v_multiwindow_wayland_pointer_frame_trampoline(void *data, struct wl_pointer *pointer) {
	(void)data;
	(void)pointer;
}

static void v_multiwindow_wayland_pointer_axis_source_trampoline(void *data, struct wl_pointer *pointer, uint32_t axis_source) {
	(void)data;
	(void)pointer;
	(void)axis_source;
}

static void v_multiwindow_wayland_pointer_axis_stop_trampoline(void *data, struct wl_pointer *pointer, uint32_t time, uint32_t axis) {
	(void)data;
	(void)pointer;
	(void)time;
	(void)axis;
}

static void v_multiwindow_wayland_pointer_axis_discrete_trampoline(void *data, struct wl_pointer *pointer, uint32_t axis, int32_t discrete) {
	(void)data;
	(void)pointer;
	(void)axis;
	(void)discrete;
}

static void v_multiwindow_wayland_keyboard_keymap_trampoline(void *data, struct wl_keyboard *keyboard, uint32_t format, int fd, uint32_t size) {
	v_multiwindow_wayland_keyboard_keymap(data, (void *)keyboard, format, fd, size);
}

static void v_multiwindow_wayland_keyboard_enter_trampoline(void *data, struct wl_keyboard *keyboard, uint32_t serial, struct wl_surface *surface, struct wl_array *keys) {
	(void)keys;
	v_multiwindow_wayland_keyboard_enter(data, (void *)keyboard, serial, (void *)surface);
}

static void v_multiwindow_wayland_keyboard_leave_trampoline(void *data, struct wl_keyboard *keyboard, uint32_t serial, struct wl_surface *surface) {
	v_multiwindow_wayland_keyboard_leave(data, (void *)keyboard, serial, (void *)surface);
}

static void v_multiwindow_wayland_keyboard_key_trampoline(void *data, struct wl_keyboard *keyboard, uint32_t serial, uint32_t time, uint32_t key, uint32_t state) {
	v_multiwindow_wayland_keyboard_key(data, (void *)keyboard, serial, time, key, state);
}

static void v_multiwindow_wayland_keyboard_modifiers_trampoline(void *data, struct wl_keyboard *keyboard, uint32_t serial, uint32_t mods_depressed, uint32_t mods_latched, uint32_t mods_locked, uint32_t group) {
	v_multiwindow_wayland_keyboard_modifiers(data, (void *)keyboard, serial, mods_depressed, mods_latched, mods_locked, group);
}

static void v_multiwindow_wayland_keyboard_repeat_info_trampoline(void *data, struct wl_keyboard *keyboard, int32_t rate, int32_t delay) {
	v_multiwindow_wayland_keyboard_repeat_info(data, (void *)keyboard, rate, delay);
}

static void v_multiwindow_wayland_touch_down_trampoline(void *data, struct wl_touch *touch, uint32_t serial, uint32_t time, struct wl_surface *surface, int32_t id, wl_fixed_t x, wl_fixed_t y) {
	v_multiwindow_wayland_touch_down(data, (void *)touch, serial, time, (void *)surface, id, wl_fixed_to_double(x), wl_fixed_to_double(y));
}

static void v_multiwindow_wayland_touch_up_trampoline(void *data, struct wl_touch *touch, uint32_t serial, uint32_t time, int32_t id) {
	v_multiwindow_wayland_touch_up(data, (void *)touch, serial, time, id);
}

static void v_multiwindow_wayland_touch_motion_trampoline(void *data, struct wl_touch *touch, uint32_t time, int32_t id, wl_fixed_t x, wl_fixed_t y) {
	v_multiwindow_wayland_touch_motion(data, (void *)touch, time, id, wl_fixed_to_double(x), wl_fixed_to_double(y));
}

static void v_multiwindow_wayland_touch_frame_trampoline(void *data, struct wl_touch *touch) {
	(void)data;
	(void)touch;
}

static void v_multiwindow_wayland_touch_cancel_trampoline(void *data, struct wl_touch *touch) {
	v_multiwindow_wayland_touch_cancel(data, (void *)touch);
}

static void v_multiwindow_wayland_touch_shape_trampoline(void *data, struct wl_touch *touch, int32_t id, wl_fixed_t major, wl_fixed_t minor) {
	(void)data;
	(void)touch;
	(void)id;
	(void)major;
	(void)minor;
}

static void v_multiwindow_wayland_touch_orientation_trampoline(void *data, struct wl_touch *touch, int32_t id, wl_fixed_t orientation) {
	(void)data;
	(void)touch;
	(void)id;
	(void)orientation;
}

static void v_multiwindow_wayland_data_offer_offer_trampoline(void *data, struct wl_data_offer *offer, const char *mime_type) {
	v_multiwindow_wayland_data_offer_offer(data, (void *)offer, (char *)mime_type);
}

static void v_multiwindow_wayland_data_offer_source_actions_trampoline(void *data, struct wl_data_offer *offer, uint32_t source_actions) {
	v_multiwindow_wayland_data_offer_source_actions(data, (void *)offer, source_actions);
}

static void v_multiwindow_wayland_data_offer_action_trampoline(void *data, struct wl_data_offer *offer, uint32_t dnd_action) {
	v_multiwindow_wayland_data_offer_action(data, (void *)offer, dnd_action);
}

static void v_multiwindow_wayland_data_device_data_offer_trampoline(void *data, struct wl_data_device *device, struct wl_data_offer *offer) {
	v_multiwindow_wayland_data_device_data_offer(data, (void *)device, (void *)offer);
}

static void v_multiwindow_wayland_data_device_enter_trampoline(void *data, struct wl_data_device *device, uint32_t serial, struct wl_surface *surface, wl_fixed_t x, wl_fixed_t y, struct wl_data_offer *offer) {
	v_multiwindow_wayland_data_device_enter(data, (void *)device, serial, (void *)surface, wl_fixed_to_double(x), wl_fixed_to_double(y), (void *)offer);
}

static void v_multiwindow_wayland_data_device_leave_trampoline(void *data, struct wl_data_device *device) {
	v_multiwindow_wayland_data_device_leave(data, (void *)device);
}

static void v_multiwindow_wayland_data_device_motion_trampoline(void *data, struct wl_data_device *device, uint32_t time, wl_fixed_t x, wl_fixed_t y) {
	v_multiwindow_wayland_data_device_motion(data, (void *)device, time, wl_fixed_to_double(x), wl_fixed_to_double(y));
}

static void v_multiwindow_wayland_data_device_drop_trampoline(void *data, struct wl_data_device *device) {
	v_multiwindow_wayland_data_device_drop(data, (void *)device);
}

static void v_multiwindow_wayland_data_device_selection_trampoline(void *data, struct wl_data_device *device, struct wl_data_offer *offer) {
	v_multiwindow_wayland_data_device_selection(data, (void *)device, (void *)offer);
}

static void v_multiwindow_wayland_xdg_toplevel_decoration_configure_trampoline(void *data, struct zxdg_toplevel_decoration_v1 *decoration, uint32_t mode) {
	v_multiwindow_wayland_xdg_toplevel_decoration_configure(data, (void *)decoration, mode);
}

static void v_multiwindow_wayland_buffer_release_trampoline(void *data, struct wl_buffer *buffer) {
	v_multiwindow_wayland_buffer_release(data, (void *)buffer);
}

static const struct wl_registry_listener v_multiwindow_wayland_registry_listener = {
	v_multiwindow_wayland_registry_handle_global_trampoline,
	v_multiwindow_wayland_registry_handle_global_remove_trampoline,
};

static const struct xdg_wm_base_listener v_multiwindow_wayland_xdg_wm_base_listener = {
	v_multiwindow_wayland_xdg_wm_base_ping_trampoline,
};

static const struct xdg_surface_listener v_multiwindow_wayland_xdg_surface_listener = {
	v_multiwindow_wayland_xdg_surface_configure_trampoline,
};

static const struct xdg_toplevel_listener v_multiwindow_wayland_xdg_toplevel_listener = {
	v_multiwindow_wayland_xdg_toplevel_configure_trampoline,
	v_multiwindow_wayland_xdg_toplevel_close_trampoline,
};

static const struct wl_seat_listener v_multiwindow_wayland_seat_listener = {
	v_multiwindow_wayland_seat_capabilities_trampoline,
	v_multiwindow_wayland_seat_name_trampoline,
};

static const struct wl_pointer_listener v_multiwindow_wayland_pointer_listener = {
	v_multiwindow_wayland_pointer_enter_trampoline,
	v_multiwindow_wayland_pointer_leave_trampoline,
	v_multiwindow_wayland_pointer_motion_trampoline,
	v_multiwindow_wayland_pointer_button_trampoline,
	v_multiwindow_wayland_pointer_axis_trampoline,
	v_multiwindow_wayland_pointer_frame_trampoline,
	v_multiwindow_wayland_pointer_axis_source_trampoline,
	v_multiwindow_wayland_pointer_axis_stop_trampoline,
	v_multiwindow_wayland_pointer_axis_discrete_trampoline,
};

static const struct wl_keyboard_listener v_multiwindow_wayland_keyboard_listener = {
	v_multiwindow_wayland_keyboard_keymap_trampoline,
	v_multiwindow_wayland_keyboard_enter_trampoline,
	v_multiwindow_wayland_keyboard_leave_trampoline,
	v_multiwindow_wayland_keyboard_key_trampoline,
	v_multiwindow_wayland_keyboard_modifiers_trampoline,
	v_multiwindow_wayland_keyboard_repeat_info_trampoline,
};

static const struct wl_touch_listener v_multiwindow_wayland_touch_listener = {
	v_multiwindow_wayland_touch_down_trampoline,
	v_multiwindow_wayland_touch_up_trampoline,
	v_multiwindow_wayland_touch_motion_trampoline,
	v_multiwindow_wayland_touch_frame_trampoline,
	v_multiwindow_wayland_touch_cancel_trampoline,
	v_multiwindow_wayland_touch_shape_trampoline,
	v_multiwindow_wayland_touch_orientation_trampoline,
};

static const struct wl_data_offer_listener v_multiwindow_wayland_data_offer_listener = {
	v_multiwindow_wayland_data_offer_offer_trampoline,
	v_multiwindow_wayland_data_offer_source_actions_trampoline,
	v_multiwindow_wayland_data_offer_action_trampoline,
};

static const struct wl_data_device_listener v_multiwindow_wayland_data_device_listener = {
	v_multiwindow_wayland_data_device_data_offer_trampoline,
	v_multiwindow_wayland_data_device_enter_trampoline,
	v_multiwindow_wayland_data_device_leave_trampoline,
	v_multiwindow_wayland_data_device_motion_trampoline,
	v_multiwindow_wayland_data_device_drop_trampoline,
	v_multiwindow_wayland_data_device_selection_trampoline,
};

static const struct wl_buffer_listener v_multiwindow_wayland_buffer_listener = {
	v_multiwindow_wayland_buffer_release_trampoline,
};

static const struct zxdg_toplevel_decoration_v1_listener v_multiwindow_wayland_xdg_toplevel_decoration_listener = {
	v_multiwindow_wayland_xdg_toplevel_decoration_configure_trampoline,
};

static inline uint32_t v_multiwindow_wayland_compositor_bind_version(uint32_t version) {
	return version < 4 ? version : 4;
}

static inline uint32_t v_multiwindow_wayland_seat_bind_version(uint32_t version) {
	return version < 5 ? version : 5;
}

static inline uint64_t v_multiwindow_wayland_next_event_sequence(void) {
	static uint64_t sequence = 1;
	return sequence++;
}

static inline void *v_multiwindow_wayland_bind_compositor(struct wl_registry *registry, uint32_t name, uint32_t version) {
	return wl_registry_bind(registry, name, &wl_compositor_interface, v_multiwindow_wayland_compositor_bind_version(version));
}

static inline void *v_multiwindow_wayland_bind_xdg_wm_base(struct wl_registry *registry, uint32_t name) {
	return wl_registry_bind(registry, name, &v_multiwindow_xdg_wm_base_interface, 1);
}

static inline void *v_multiwindow_wayland_bind_xdg_decoration_manager(struct wl_registry *registry, uint32_t name, uint32_t version) {
	if (version < 1) {
		return NULL;
	}
	return wl_registry_bind(registry, name, &v_multiwindow_zxdg_decoration_manager_v1_interface, 1);
}

static inline void *v_multiwindow_wayland_bind_cursor_shape_manager(struct wl_registry *registry, uint32_t name, uint32_t version) {
	if (version < 1) {
		return NULL;
	}
	return wl_registry_bind(registry, name, &v_multiwindow_wp_cursor_shape_manager_v1_interface, 1);
}

static inline void *v_multiwindow_wayland_bind_seat(struct wl_registry *registry, uint32_t name, uint32_t version) {
	return wl_registry_bind(registry, name, &wl_seat_interface, v_multiwindow_wayland_seat_bind_version(version));
}

static inline void *v_multiwindow_wayland_bind_data_device_manager(struct wl_registry *registry, uint32_t name, uint32_t version) {
	if (version < 3) {
		return NULL;
	}
	return wl_registry_bind(registry, name, &wl_data_device_manager_interface, 3);
}

static inline void *v_multiwindow_wayland_bind_shm(struct wl_registry *registry, uint32_t name) {
	return wl_registry_bind(registry, name, &wl_shm_interface, 1);
}

static inline int v_multiwindow_wayland_add_registry_listener(struct wl_registry *registry, void *data) {
	return wl_registry_add_listener(registry, &v_multiwindow_wayland_registry_listener, data);
}

static inline int v_multiwindow_wayland_add_xdg_wm_base_listener(struct xdg_wm_base *wm_base, void *data) {
	return wl_proxy_add_listener((struct wl_proxy *)wm_base, (void (**)(void))&v_multiwindow_wayland_xdg_wm_base_listener, data);
}

static inline int v_multiwindow_wayland_add_xdg_surface_listener(struct xdg_surface *xdg_surface, void *data) {
	return wl_proxy_add_listener((struct wl_proxy *)xdg_surface, (void (**)(void))&v_multiwindow_wayland_xdg_surface_listener, data);
}

static inline int v_multiwindow_wayland_add_xdg_toplevel_listener(struct xdg_toplevel *toplevel, void *data) {
	return wl_proxy_add_listener((struct wl_proxy *)toplevel, (void (**)(void))&v_multiwindow_wayland_xdg_toplevel_listener, data);
}

static inline int v_multiwindow_wayland_add_xdg_toplevel_decoration_listener(struct zxdg_toplevel_decoration_v1 *decoration, void *data) {
	return wl_proxy_add_listener((struct wl_proxy *)decoration, (void (**)(void))&v_multiwindow_wayland_xdg_toplevel_decoration_listener, data);
}

static inline int v_multiwindow_wayland_add_seat_listener(struct wl_seat *seat, void *data) {
	return wl_seat_add_listener(seat, &v_multiwindow_wayland_seat_listener, data);
}

static inline void *v_multiwindow_wayland_seat_get_pointer(struct wl_seat *seat) {
	return (void *)wl_seat_get_pointer(seat);
}

static inline void *v_multiwindow_wayland_seat_get_keyboard(struct wl_seat *seat) {
	return (void *)wl_seat_get_keyboard(seat);
}

static inline void *v_multiwindow_wayland_seat_get_touch(struct wl_seat *seat) {
	return (void *)wl_seat_get_touch(seat);
}

static inline void *v_multiwindow_wayland_data_device_manager_get_data_device(struct wl_data_device_manager *manager, struct wl_seat *seat) {
	return (void *)wl_data_device_manager_get_data_device(manager, seat);
}

static inline void *v_multiwindow_wayland_cursor_shape_manager_get_pointer(struct wp_cursor_shape_manager_v1 *manager, struct wl_pointer *pointer) {
	struct wl_proxy *id = wl_proxy_marshal_flags((struct wl_proxy *)manager, WP_CURSOR_SHAPE_MANAGER_V1_GET_POINTER, &v_multiwindow_wp_cursor_shape_device_v1_interface, wl_proxy_get_version((struct wl_proxy *)manager), 0, NULL, pointer);
	return (void *)id;
}

static inline void v_multiwindow_wayland_cursor_shape_device_set_shape(struct wp_cursor_shape_device_v1 *device, uint32_t serial, uint32_t shape) {
	wl_proxy_marshal_flags((struct wl_proxy *)device, WP_CURSOR_SHAPE_DEVICE_V1_SET_SHAPE, NULL, wl_proxy_get_version((struct wl_proxy *)device), 0, serial, shape);
}

static inline void v_multiwindow_wayland_cursor_shape_device_destroy(struct wp_cursor_shape_device_v1 *device) {
	if (device != NULL) {
		wl_proxy_marshal_flags((struct wl_proxy *)device, WP_CURSOR_SHAPE_DEVICE_V1_DESTROY, NULL, wl_proxy_get_version((struct wl_proxy *)device), WL_MARSHAL_FLAG_DESTROY);
	}
}

static inline void v_multiwindow_wayland_cursor_shape_manager_destroy(struct wp_cursor_shape_manager_v1 *manager) {
	if (manager != NULL) {
		wl_proxy_marshal_flags((struct wl_proxy *)manager, WP_CURSOR_SHAPE_MANAGER_V1_DESTROY, NULL, wl_proxy_get_version((struct wl_proxy *)manager), WL_MARSHAL_FLAG_DESTROY);
	}
}

static inline int v_multiwindow_wayland_add_pointer_listener(struct wl_pointer *pointer, void *data) {
	return wl_pointer_add_listener(pointer, &v_multiwindow_wayland_pointer_listener, data);
}

static inline int v_multiwindow_wayland_add_keyboard_listener(struct wl_keyboard *keyboard, void *data) {
	return wl_keyboard_add_listener(keyboard, &v_multiwindow_wayland_keyboard_listener, data);
}

static inline int v_multiwindow_wayland_add_touch_listener(struct wl_touch *touch, void *data) {
	return wl_touch_add_listener(touch, &v_multiwindow_wayland_touch_listener, data);
}

static inline int v_multiwindow_wayland_add_data_device_listener(struct wl_data_device *device, void *data) {
	return wl_data_device_add_listener(device, &v_multiwindow_wayland_data_device_listener, data);
}

static inline int v_multiwindow_wayland_add_data_offer_listener(struct wl_data_offer *offer, void *data) {
	return wl_data_offer_add_listener(offer, &v_multiwindow_wayland_data_offer_listener, data);
}

static inline void v_multiwindow_wayland_data_offer_accept(struct wl_data_offer *offer, uint32_t serial, const char *mime_type) {
	wl_data_offer_accept(offer, serial, mime_type);
}

static inline void v_multiwindow_wayland_data_offer_set_copy_action(struct wl_data_offer *offer) {
	wl_data_offer_set_actions(offer, WL_DATA_DEVICE_MANAGER_DND_ACTION_COPY, WL_DATA_DEVICE_MANAGER_DND_ACTION_COPY);
}

static inline void v_multiwindow_wayland_data_offer_receive(struct wl_data_offer *offer, const char *mime_type, int fd) {
	wl_data_offer_receive(offer, mime_type, fd);
}

static inline int v_multiwindow_wayland_fd_set_nonblocking(int fd) {
	int flags = fcntl(fd, F_GETFL, 0);
	if (flags < 0) {
		return 0;
	}
	return fcntl(fd, F_SETFL, flags | O_NONBLOCK) == 0;
}

static inline int v_multiwindow_wayland_read_would_block(void) {
	return errno == EAGAIN || errno == EWOULDBLOCK;
}

static inline void v_multiwindow_wayland_data_offer_finish(struct wl_data_offer *offer) {
	wl_data_offer_finish(offer);
}

static inline void v_multiwindow_wayland_data_offer_destroy(struct wl_data_offer *offer) {
	if (offer != NULL) {
		wl_data_offer_destroy(offer);
	}
}

static inline void v_multiwindow_wayland_data_device_destroy(struct wl_data_device *device) {
	if (device != NULL) {
		wl_data_device_destroy(device);
	}
}

static inline void v_multiwindow_wayland_data_device_manager_destroy(struct wl_data_device_manager *manager) {
	if (manager != NULL) {
		wl_data_device_manager_destroy(manager);
	}
}

static inline void v_multiwindow_wayland_shm_destroy(struct wl_shm *shm) {
	if (shm != NULL) {
		wl_shm_destroy(shm);
	}
}

static inline int v_multiwindow_wayland_create_tmpfile(size_t size) {
	const char *runtime_dir = getenv("XDG_RUNTIME_DIR");
	char template_path[PATH_MAX];
	int written = -1;
	if (runtime_dir != NULL && runtime_dir[0] != '\0') {
		written = snprintf(template_path, sizeof(template_path), "%s/v-multiwindow-shm-XXXXXX", runtime_dir);
	}
	if (written < 0 || (size_t)written >= sizeof(template_path)) {
		written = snprintf(template_path, sizeof(template_path), "/tmp/v-multiwindow-shm-XXXXXX");
	}
	if (written < 0 || (size_t)written >= sizeof(template_path)) {
		return -1;
	}
	int fd = mkstemp(template_path);
	if (fd < 0) {
		return -1;
	}
	unlink(template_path);
	if (ftruncate(fd, (off_t)size) < 0) {
		close(fd);
		return -1;
	}
	return fd;
}

static inline void *v_multiwindow_wayland_create_shm_buffer(struct wl_shm *shm, int width, int height) {
	if (shm == NULL || width <= 0 || height <= 0) {
		return NULL;
	}
	size_t stride = (size_t)width * 4;
	size_t size = stride * (size_t)height;
	if (stride == 0 || size == 0 || size / stride != (size_t)height) {
		return NULL;
	}
	int fd = v_multiwindow_wayland_create_tmpfile(size);
	if (fd < 0) {
		return NULL;
	}
	void *data = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	if (data == MAP_FAILED) {
		close(fd);
		return NULL;
	}
	uint32_t *pixels = (uint32_t *)data;
	size_t pixel_count = (size_t)width * (size_t)height;
	for (size_t i = 0; i < pixel_count; i++) {
		pixels[i] = 0xff202020u;
	}
	struct wl_shm_pool *pool = wl_shm_create_pool(shm, fd, (int32_t)size);
	if (pool == NULL) {
		munmap(data, size);
		close(fd);
		return NULL;
	}
	struct wl_buffer *buffer = wl_shm_pool_create_buffer(pool, 0, width, height, (int32_t)stride, WL_SHM_FORMAT_XRGB8888);
	wl_shm_pool_destroy(pool);
	munmap(data, size);
	close(fd);
	return buffer == NULL ? NULL : (void *)buffer;
}

static inline int v_multiwindow_wayland_add_buffer_listener(struct wl_buffer *buffer, void *data) {
	if (buffer == NULL) {
		return -1;
	}
	return wl_buffer_add_listener(buffer, &v_multiwindow_wayland_buffer_listener, data);
}

static inline void v_multiwindow_wayland_attach_buffer(struct wl_surface *surface, struct wl_buffer *buffer, int width, int height) {
	if (surface != NULL && buffer != NULL) {
		wl_surface_attach(surface, buffer, 0, 0);
		wl_surface_damage(surface, 0, 0, width, height);
		wl_surface_commit(surface);
	}
}

static inline void v_multiwindow_wayland_buffer_destroy(struct wl_buffer *buffer) {
	if (buffer != NULL) {
		wl_buffer_destroy(buffer);
	}
}

static inline void v_multiwindow_wayland_pointer_destroy(struct wl_pointer *pointer) {
	if (pointer != NULL) {
#ifdef WL_POINTER_RELEASE
		uint32_t version = wl_proxy_get_version((struct wl_proxy *)pointer);
		if (version >= 3) {
			wl_proxy_marshal_flags((struct wl_proxy *)pointer, WL_POINTER_RELEASE, NULL, version, WL_MARSHAL_FLAG_DESTROY);
		} else {
			wl_pointer_destroy(pointer);
		}
#else
		wl_pointer_destroy(pointer);
#endif
	}
}

static inline void v_multiwindow_wayland_keyboard_destroy(struct wl_keyboard *keyboard) {
	if (keyboard != NULL) {
#ifdef WL_KEYBOARD_RELEASE
		uint32_t version = wl_proxy_get_version((struct wl_proxy *)keyboard);
		if (version >= 3) {
			wl_proxy_marshal_flags((struct wl_proxy *)keyboard, WL_KEYBOARD_RELEASE, NULL, version, WL_MARSHAL_FLAG_DESTROY);
		} else {
			wl_keyboard_destroy(keyboard);
		}
#else
		wl_keyboard_destroy(keyboard);
#endif
	}
}

static inline void v_multiwindow_wayland_touch_destroy(struct wl_touch *touch) {
	if (touch != NULL) {
#ifdef WL_TOUCH_RELEASE
		uint32_t version = wl_proxy_get_version((struct wl_proxy *)touch);
		if (version >= 3) {
			wl_proxy_marshal_flags((struct wl_proxy *)touch, WL_TOUCH_RELEASE, NULL, version, WL_MARSHAL_FLAG_DESTROY);
		} else {
			wl_touch_destroy(touch);
		}
#else
		wl_touch_destroy(touch);
#endif
	}
}

static inline void v_multiwindow_wayland_seat_destroy(struct wl_seat *seat) {
	if (seat != NULL) {
#ifdef WL_SEAT_RELEASE
		uint32_t version = wl_proxy_get_version((struct wl_proxy *)seat);
		if (version >= 5) {
			wl_proxy_marshal_flags((struct wl_proxy *)seat, WL_SEAT_RELEASE, NULL, version, WL_MARSHAL_FLAG_DESTROY);
		} else {
			wl_seat_destroy(seat);
		}
#else
		wl_seat_destroy(seat);
#endif
	}
}

static inline struct xdg_surface *v_multiwindow_wayland_xdg_wm_base_get_xdg_surface(struct xdg_wm_base *wm_base, struct wl_surface *surface) {
	struct wl_proxy *id = wl_proxy_marshal_flags((struct wl_proxy *)wm_base, XDG_WM_BASE_GET_XDG_SURFACE, &v_multiwindow_xdg_surface_interface, wl_proxy_get_version((struct wl_proxy *)wm_base), 0, NULL, surface);
	return (struct xdg_surface *)id;
}

static inline void v_multiwindow_wayland_xdg_wm_base_destroy(struct xdg_wm_base *wm_base) {
	wl_proxy_marshal_flags((struct wl_proxy *)wm_base, XDG_WM_BASE_DESTROY, NULL, wl_proxy_get_version((struct wl_proxy *)wm_base), WL_MARSHAL_FLAG_DESTROY);
}

static inline void v_multiwindow_wayland_xdg_wm_base_pong(struct xdg_wm_base *wm_base, uint32_t serial) {
	wl_proxy_marshal_flags((struct wl_proxy *)wm_base, XDG_WM_BASE_PONG, NULL, wl_proxy_get_version((struct wl_proxy *)wm_base), 0, serial);
}

static inline struct xdg_toplevel *v_multiwindow_wayland_xdg_surface_get_toplevel(struct xdg_surface *xdg_surface) {
	struct wl_proxy *id = wl_proxy_marshal_flags((struct wl_proxy *)xdg_surface, XDG_SURFACE_GET_TOPLEVEL, &v_multiwindow_xdg_toplevel_interface, wl_proxy_get_version((struct wl_proxy *)xdg_surface), 0, NULL);
	return (struct xdg_toplevel *)id;
}

static inline void v_multiwindow_wayland_xdg_surface_ack_configure(struct xdg_surface *xdg_surface, uint32_t serial) {
	wl_proxy_marshal_flags((struct wl_proxy *)xdg_surface, XDG_SURFACE_ACK_CONFIGURE, NULL, wl_proxy_get_version((struct wl_proxy *)xdg_surface), 0, serial);
}

static inline void v_multiwindow_wayland_xdg_surface_destroy(struct xdg_surface *xdg_surface) {
	wl_proxy_marshal_flags((struct wl_proxy *)xdg_surface, XDG_SURFACE_DESTROY, NULL, wl_proxy_get_version((struct wl_proxy *)xdg_surface), WL_MARSHAL_FLAG_DESTROY);
}

static inline void v_multiwindow_wayland_xdg_toplevel_set_title(struct xdg_toplevel *toplevel, const char *title) {
	wl_proxy_marshal_flags((struct wl_proxy *)toplevel, XDG_TOPLEVEL_SET_TITLE, NULL, wl_proxy_get_version((struct wl_proxy *)toplevel), 0, title);
}

static inline void v_multiwindow_wayland_xdg_toplevel_set_app_id(struct xdg_toplevel *toplevel, const char *app_id) {
	wl_proxy_marshal_flags((struct wl_proxy *)toplevel, XDG_TOPLEVEL_SET_APP_ID, NULL, wl_proxy_get_version((struct wl_proxy *)toplevel), 0, app_id);
}

static inline void v_multiwindow_wayland_xdg_toplevel_set_min_size(struct xdg_toplevel *toplevel, int32_t width, int32_t height) {
	wl_proxy_marshal_flags((struct wl_proxy *)toplevel, XDG_TOPLEVEL_SET_MIN_SIZE, NULL, wl_proxy_get_version((struct wl_proxy *)toplevel), 0, width, height);
}

static inline void v_multiwindow_wayland_xdg_toplevel_set_max_size(struct xdg_toplevel *toplevel, int32_t width, int32_t height) {
	wl_proxy_marshal_flags((struct wl_proxy *)toplevel, XDG_TOPLEVEL_SET_MAX_SIZE, NULL, wl_proxy_get_version((struct wl_proxy *)toplevel), 0, width, height);
}

static inline void v_multiwindow_wayland_xdg_toplevel_set_fullscreen(struct xdg_toplevel *toplevel, struct wl_output *output) {
	wl_proxy_marshal_flags((struct wl_proxy *)toplevel, XDG_TOPLEVEL_SET_FULLSCREEN, NULL, wl_proxy_get_version((struct wl_proxy *)toplevel), 0, output);
}

static inline void v_multiwindow_wayland_xdg_toplevel_move(struct xdg_toplevel *toplevel, struct wl_seat *seat, uint32_t serial) {
	wl_proxy_marshal_flags((struct wl_proxy *)toplevel, XDG_TOPLEVEL_MOVE, NULL, wl_proxy_get_version((struct wl_proxy *)toplevel), 0, seat, serial);
}

static inline void v_multiwindow_wayland_xdg_toplevel_resize(struct xdg_toplevel *toplevel, struct wl_seat *seat, uint32_t serial, uint32_t edges) {
	wl_proxy_marshal_flags((struct wl_proxy *)toplevel, XDG_TOPLEVEL_RESIZE, NULL, wl_proxy_get_version((struct wl_proxy *)toplevel), 0, seat, serial, edges);
}

static inline struct zxdg_toplevel_decoration_v1 *v_multiwindow_wayland_xdg_decoration_manager_get_toplevel_decoration(struct zxdg_decoration_manager_v1 *manager, struct xdg_toplevel *toplevel) {
	struct wl_proxy *id = wl_proxy_marshal_flags((struct wl_proxy *)manager, ZXDG_DECORATION_MANAGER_V1_GET_TOPLEVEL_DECORATION, &v_multiwindow_zxdg_toplevel_decoration_v1_interface, wl_proxy_get_version((struct wl_proxy *)manager), 0, NULL, toplevel);
	return (struct zxdg_toplevel_decoration_v1 *)id;
}

static inline void v_multiwindow_wayland_xdg_toplevel_decoration_set_server_side(struct zxdg_toplevel_decoration_v1 *decoration) {
	wl_proxy_marshal_flags((struct wl_proxy *)decoration, ZXDG_TOPLEVEL_DECORATION_V1_SET_MODE, NULL, wl_proxy_get_version((struct wl_proxy *)decoration), 0, ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE);
}

static inline void v_multiwindow_wayland_xdg_toplevel_decoration_destroy(struct zxdg_toplevel_decoration_v1 *decoration) {
	if (decoration != NULL) {
		wl_proxy_marshal_flags((struct wl_proxy *)decoration, ZXDG_TOPLEVEL_DECORATION_V1_DESTROY, NULL, wl_proxy_get_version((struct wl_proxy *)decoration), WL_MARSHAL_FLAG_DESTROY);
	}
}

static inline void v_multiwindow_wayland_xdg_decoration_manager_destroy(struct zxdg_decoration_manager_v1 *manager) {
	if (manager != NULL) {
		wl_proxy_marshal_flags((struct wl_proxy *)manager, ZXDG_DECORATION_MANAGER_V1_DESTROY, NULL, wl_proxy_get_version((struct wl_proxy *)manager), WL_MARSHAL_FLAG_DESTROY);
	}
}

static inline void v_multiwindow_wayland_xdg_toplevel_destroy(struct xdg_toplevel *toplevel) {
	wl_proxy_marshal_flags((struct wl_proxy *)toplevel, XDG_TOPLEVEL_DESTROY, NULL, wl_proxy_get_version((struct wl_proxy *)toplevel), WL_MARSHAL_FLAG_DESTROY);
}

static inline void *v_multiwindow_wayland_egl_get_display(struct wl_display *display) {
	return (void *)eglGetDisplay((EGLNativeDisplayType)display);
}

static inline int v_multiwindow_wayland_egl_initialize(void *egl_display) {
	return eglInitialize((EGLDisplay)egl_display, NULL, NULL) == EGL_TRUE ? 1 : 0;
}

static inline int v_multiwindow_wayland_egl_bind_opengl_api(void) {
	return eglBindAPI(EGL_OPENGL_API) == EGL_TRUE ? 1 : 0;
}

static inline int v_multiwindow_wayland_egl_choose_config(void *egl_display, void **out_config) {
	const EGLint attrs[] = {
		EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
		EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
		EGL_RED_SIZE, 8,
		EGL_GREEN_SIZE, 8,
		EGL_BLUE_SIZE, 8,
		EGL_ALPHA_SIZE, 8,
		EGL_DEPTH_SIZE, 24,
		EGL_STENCIL_SIZE, 8,
		EGL_NONE
	};
	EGLConfig config = NULL;
	EGLint config_count = 0;
	if (eglChooseConfig((EGLDisplay)egl_display, attrs, &config, 1, &config_count) != EGL_TRUE || config_count == 0) {
		return 0;
	}
	*out_config = (void *)config;
	return 1;
}

static inline void *v_multiwindow_wayland_egl_create_context(void *egl_display, void *egl_config) {
	const EGLint core_attrs[] = {
		EGL_CONTEXT_MAJOR_VERSION, 3,
		EGL_CONTEXT_MINOR_VERSION, 3,
		EGL_CONTEXT_OPENGL_PROFILE_MASK, EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT,
		EGL_NONE
	};
	EGLContext context = eglCreateContext((EGLDisplay)egl_display, (EGLConfig)egl_config, EGL_NO_CONTEXT, core_attrs);
	if (context == EGL_NO_CONTEXT) {
		const EGLint fallback_attrs[] = { EGL_NONE };
		context = eglCreateContext((EGLDisplay)egl_display, (EGLConfig)egl_config, EGL_NO_CONTEXT, fallback_attrs);
	}
	return context == EGL_NO_CONTEXT ? NULL : (void *)context;
}

static inline void *v_multiwindow_wayland_egl_create_window(struct wl_surface *surface, int width, int height) {
	struct wl_egl_window *window = wl_egl_window_create(surface, width, height);
	return window == NULL ? NULL : (void *)window;
}

static inline void v_multiwindow_wayland_egl_resize_window(void *egl_window, int width, int height) {
	if (egl_window != NULL) {
		wl_egl_window_resize((struct wl_egl_window *)egl_window, width, height, 0, 0);
	}
}

static inline void v_multiwindow_wayland_egl_destroy_window(void *egl_window) {
	if (egl_window != NULL) {
		wl_egl_window_destroy((struct wl_egl_window *)egl_window);
	}
}

static inline void *v_multiwindow_wayland_egl_create_window_surface(void *egl_display, void *egl_config, void *egl_window) {
	EGLSurface surface = eglCreateWindowSurface((EGLDisplay)egl_display, (EGLConfig)egl_config, (EGLNativeWindowType)egl_window, NULL);
	return surface == EGL_NO_SURFACE ? NULL : (void *)surface;
}

static inline int v_multiwindow_wayland_egl_make_current(void *egl_display, void *egl_surface, void *egl_context) {
	return eglMakeCurrent((EGLDisplay)egl_display, (EGLSurface)egl_surface, (EGLSurface)egl_surface, (EGLContext)egl_context) == EGL_TRUE ? 1 : 0;
}

static inline void v_multiwindow_wayland_egl_clear_current(void *egl_display) {
	eglMakeCurrent((EGLDisplay)egl_display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
}

static inline int v_multiwindow_wayland_egl_swap_buffers(void *egl_display, void *egl_surface) {
	return eglSwapBuffers((EGLDisplay)egl_display, (EGLSurface)egl_surface) == EGL_TRUE ? 1 : 0;
}

static inline void v_multiwindow_wayland_egl_destroy_surface(void *egl_display, void *egl_surface) {
	if (egl_surface != NULL) {
		eglDestroySurface((EGLDisplay)egl_display, (EGLSurface)egl_surface);
	}
}

static inline void v_multiwindow_wayland_egl_destroy_context(void *egl_display, void *egl_context) {
	if (egl_context != NULL) {
		eglDestroyContext((EGLDisplay)egl_display, (EGLContext)egl_context);
	}
}

static inline void v_multiwindow_wayland_egl_terminate(void *egl_display) {
	if (egl_display != NULL) {
		eglTerminate((EGLDisplay)egl_display);
	}
}

#endif
