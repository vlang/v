#ifndef V_MULTIWINDOW_WAYLAND_BACKEND_HELPERS_H
#define V_MULTIWINDOW_WAYLAND_BACKEND_HELPERS_H

#include <stdint.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>
#include <wayland-egl.h>
#include "pointer-constraints-unstable-v1-client-protocol.h"
#include "relative-pointer-unstable-v1-client-protocol.h"
#include "xdg-foreign-unstable-v2-client-protocol.h"
#include "fractional-scale-v1-client-protocol.h"
#include "viewporter-client-protocol.h"
#include "linux_egl_native_helpers.h"

#define V_MULTIWINDOW_WAYLAND_ANCHOR_RELEASE_PROTOCOL_DESTROY UINT64_C(1)
#define V_MULTIWINDOW_WAYLAND_ANCHOR_RELEASE_LOCAL_PROXY_DESTROY UINT64_C(2)

void v_multiwindow_wayland_output_geometry(void *data, void *output, int x, int y,
	int physical_width, int physical_height, int subpixel, char *make, char *model,
	int transform);
void v_multiwindow_wayland_output_mode(void *data, void *output, uint32_t flags,
	int width, int height, int refresh);
void v_multiwindow_wayland_output_done(void *data, void *output);
void v_multiwindow_wayland_output_scale(void *data, void *output, int factor);
void v_multiwindow_wayland_output_name(void *data, void *output, char *name);
void v_multiwindow_wayland_output_description(void *data, void *output,
	char *description);

static inline void v_multiwindow_wayland_result(VMultiwindowNativePrimitive *out_result,
		int result, int native_errno) {
	if (out_result == NULL) {
		return;
	}
	memset(out_result, 0, sizeof(*out_result));
	out_result->valid_mask = V_MULTIWINDOW_NATIVE_VALID_RETURN_VALUE;
	out_result->return_value = (int64_t)result;
	if (result < 0) {
		out_result->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_ERRNO;
		out_result->native_errno = (int64_t)native_errno;
	}
}

static inline void v_multiwindow_wayland_reset(VMultiwindowNativePrimitive *out_result) {
	if (out_result == NULL) {
		return;
	}
	memset(out_result, 0, sizeof(*out_result));
}

static inline void v_multiwindow_wayland_proxy_destroy_local(void *proxy) {
	if (proxy != NULL) {
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST) \
		&& defined(V_MULTIWINDOW_NATIVE_WAYLAND_RELEASE_ORACLE_HELPERS_H)
		uint64_t identity = (uint64_t)(uintptr_t)proxy;
#endif
		wl_proxy_destroy((struct wl_proxy *)proxy);
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST) \
		&& defined(V_MULTIWINDOW_NATIVE_WAYLAND_RELEASE_ORACLE_HELPERS_H)
		v_multiwindow_test_wayland_local_proxy_destroyed(identity);
#endif
	}
}

static inline void v_multiwindow_wayland_display_disconnect(
		struct wl_display *display) {
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST) \
	&& defined(V_MULTIWINDOW_NATIVE_WAYLAND_RELEASE_ORACLE_HELPERS_H)
	uint64_t identity = (uint64_t)(uintptr_t)display;
#endif
	wl_display_disconnect(display);
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST) \
	&& defined(V_MULTIWINDOW_NATIVE_WAYLAND_RELEASE_ORACLE_HELPERS_H)
	v_multiwindow_test_wayland_display_disconnected(identity);
#endif
}

static inline void v_multiwindow_wayland_display_error(struct wl_display *display,
		VMultiwindowNativePrimitive *out_result) {
	v_multiwindow_wayland_reset(out_result);
	if (out_result != NULL && display != NULL) {
		out_result->valid_mask = V_MULTIWINDOW_NATIVE_VALID_WAYLAND_DISPLAY_ERROR;
		out_result->wayland_display_error = (int64_t)wl_display_get_error(display);
	}
}

static inline void v_multiwindow_wayland_flush(struct wl_display *display,
		VMultiwindowNativePrimitive *out_result) {
	if (display == NULL) {
		v_multiwindow_wayland_reset(out_result);
		return;
	}
	errno = 0;
	int result;
	do {
		result = wl_display_flush(display);
	} while (result < 0 && errno == EINTR);
	int native_errno = result < 0 ? errno : 0;
	v_multiwindow_wayland_result(out_result, result, native_errno);
}

static inline void v_multiwindow_wayland_roundtrip(struct wl_display *display,
		VMultiwindowNativePrimitive *out_result) {
	if (display == NULL) {
		v_multiwindow_wayland_reset(out_result);
		return;
	}
	errno = 0;
	int result = wl_display_roundtrip(display);
	int native_errno = result < 0 ? errno : 0;
	v_multiwindow_wayland_result(out_result, result, native_errno);
}

static inline void v_multiwindow_wayland_dispatch_pending(struct wl_display *display,
		VMultiwindowNativePrimitive *out_result) {
	if (display == NULL) {
		v_multiwindow_wayland_reset(out_result);
		return;
	}
	errno = 0;
	int result = wl_display_dispatch_pending(display);
	int native_errno = result < 0 ? errno : 0;
	v_multiwindow_wayland_result(out_result, result, native_errno);
}

static inline void v_multiwindow_wayland_prepare_read(struct wl_display *display,
		VMultiwindowNativePrimitive *out_result) {
	if (display == NULL) {
		v_multiwindow_wayland_reset(out_result);
		return;
	}
	int result = wl_display_prepare_read(display);
	v_multiwindow_wayland_result(out_result, result, 0);
	if (out_result != NULL) {
		out_result->valid_mask &= ~V_MULTIWINDOW_NATIVE_VALID_ERRNO;
	}
}

static inline void v_multiwindow_wayland_cancel_read(struct wl_display *display,
		VMultiwindowNativePrimitive *out_result) {
	if (display == NULL) {
		v_multiwindow_wayland_reset(out_result);
		return;
	}
	wl_display_cancel_read(display);
	v_multiwindow_wayland_reset(out_result);
}

static inline void v_multiwindow_wayland_read_events(struct wl_display *display,
		VMultiwindowNativePrimitive *out_result) {
	if (display == NULL) {
		v_multiwindow_wayland_reset(out_result);
		return;
	}
	errno = 0;
	int result = wl_display_read_events(display);
	int native_errno = result < 0 ? errno : 0;
	v_multiwindow_wayland_result(out_result, result, native_errno);
}

static inline void v_multiwindow_wayland_get_fd(struct wl_display *display,
		VMultiwindowNativePrimitive *out_result) {
	if (display == NULL) {
		v_multiwindow_wayland_reset(out_result);
		return;
	}
	errno = 0;
	int result = wl_display_get_fd(display);
	int native_errno = result < 0 ? errno : 0;
	v_multiwindow_wayland_result(out_result, result, native_errno);
}

static inline void v_multiwindow_wayland_poll(struct pollfd *fds, uint64_t count,
		int timeout, VMultiwindowNativePrimitive *out_result) {
	errno = 0;
	int result = poll(fds, (nfds_t)count, timeout);
	int native_errno = result < 0 ? errno : 0;
	uint64_t observed_flags = UINT64_C(0);
	if (result >= 0 && fds != NULL) {
		for (uint64_t i = 0; i < count; i++) {
			observed_flags |= (uint64_t)(uint16_t)fds[i].revents;
		}
	}
	v_multiwindow_wayland_result(out_result, result, native_errno);
	if (out_result != NULL && result >= 0) {
		out_result->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_OBSERVED_FLAGS;
		out_result->observed_flags = observed_flags;
	}
}


void v_multiwindow_wayland_registry_handle_global(voidptr data, struct wl_registry *registry, u32 name, char *iface, u32 version);
void v_multiwindow_wayland_registry_handle_global_remove(voidptr data, struct wl_registry *registry, u32 name);
void v_multiwindow_wayland_xdg_wm_base_ping(void *data, void *wm_base, uint32_t serial);
void v_multiwindow_wayland_xdg_surface_configure(void *data, void *xdg_surface, uint32_t serial);
void v_multiwindow_wayland_xdg_toplevel_configure(void *data, void *toplevel, int width, int height, struct wl_array *states);
void v_multiwindow_wayland_xdg_toplevel_close(void *data, void *toplevel);
void v_multiwindow_wayland_surface_enter(void *data, void *surface, void *output);
void v_multiwindow_wayland_surface_leave(void *data, void *surface, void *output);
void v_multiwindow_wayland_locked_pointer_locked(void *data, void *locked_pointer);
void v_multiwindow_wayland_locked_pointer_unlocked(void *data, void *locked_pointer);
void v_multiwindow_wayland_relative_pointer_motion(void *data, void *relative_pointer,
	uint32_t time_hi, uint32_t time_lo, double dx, double dy,
	double dx_unaccelerated, double dy_unaccelerated);
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
int v_multiwindow_wayland_frame_done(void *data, void *callback, uint32_t time);
void v_multiwindow_wayland_frame_destroyed(void *data, void *callback);
void v_multiwindow_wayland_data_offer_offer(void *data, void *offer, char *mime_type);
void v_multiwindow_wayland_data_offer_source_actions(void *data, void *offer, uint32_t source_actions);
void v_multiwindow_wayland_data_offer_action(void *data, void *offer, uint32_t dnd_action);
void v_multiwindow_wayland_data_device_data_offer(void *data, void *device, void *offer);
void v_multiwindow_wayland_data_device_enter(void *data, void *device, uint32_t serial, void *surface, double x, double y, void *offer);
void v_multiwindow_wayland_data_device_leave(void *data, void *device);
void v_multiwindow_wayland_data_device_motion(void *data, void *device, uint32_t time, double x, double y);
void v_multiwindow_wayland_data_device_drop(void *data, void *device);
void v_multiwindow_wayland_data_device_selection(void *data, void *device, void *offer);
void v_multiwindow_wayland_data_source_target(void *data, void *source, char *mime_type);
void v_multiwindow_wayland_data_source_send(void *data, void *source, char *mime_type, int32_t fd);
void v_multiwindow_wayland_data_source_cancelled(void *data, void *source);
void v_multiwindow_wayland_buffer_release(void *data, void *buffer);
void v_multiwindow_wayland_exported_handle(void *data, void *exported,
	char *handle);
void v_multiwindow_wayland_fractional_scale_preferred(void *data,
	void *fractional_scale, uint32_t scale);

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
#ifndef XDG_TOPLEVEL_SET_PARENT
#define XDG_TOPLEVEL_SET_PARENT 1
#endif
#ifndef XDG_TOPLEVEL_SET_APP_ID
#define XDG_TOPLEVEL_SET_APP_ID 3
#endif
#ifndef XDG_TOPLEVEL_SET_MAX_SIZE
#define XDG_TOPLEVEL_SET_MAX_SIZE 7
#endif
#ifndef XDG_TOPLEVEL_SET_MAXIMIZED
#define XDG_TOPLEVEL_SET_MAXIMIZED 9
#endif
#ifndef XDG_TOPLEVEL_UNSET_MAXIMIZED
#define XDG_TOPLEVEL_UNSET_MAXIMIZED 10
#endif
#ifndef XDG_TOPLEVEL_SET_MIN_SIZE
#define XDG_TOPLEVEL_SET_MIN_SIZE 8
#endif
#ifndef XDG_TOPLEVEL_SET_FULLSCREEN
#define XDG_TOPLEVEL_SET_FULLSCREEN 11
#endif
#ifndef XDG_TOPLEVEL_UNSET_FULLSCREEN
#define XDG_TOPLEVEL_UNSET_FULLSCREEN 12
#endif
#ifndef XDG_TOPLEVEL_SET_MINIMIZED
#define XDG_TOPLEVEL_SET_MINIMIZED 13
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

static void v_multiwindow_wayland_data_source_target_trampoline(void *data, struct wl_data_source *source, const char *mime_type) {
	v_multiwindow_wayland_data_source_target(data, (void *)source, (char *)mime_type);
}

static void v_multiwindow_wayland_data_source_send_trampoline(void *data, struct wl_data_source *source, const char *mime_type, int32_t fd) {
	v_multiwindow_wayland_data_source_send(data, (void *)source, (char *)mime_type, fd);
}

static void v_multiwindow_wayland_data_source_cancelled_trampoline(void *data, struct wl_data_source *source) {
	v_multiwindow_wayland_data_source_cancelled(data, (void *)source);
}

static void v_multiwindow_wayland_data_source_dnd_drop_performed_trampoline(void *data, struct wl_data_source *source) {
	(void)data;
	(void)source;
}

static void v_multiwindow_wayland_data_source_dnd_finished_trampoline(void *data, struct wl_data_source *source) {
	(void)data;
	(void)source;
}

static void v_multiwindow_wayland_data_source_action_trampoline(void *data, struct wl_data_source *source, uint32_t action) {
	(void)data;
	(void)source;
	(void)action;
}

static void v_multiwindow_wayland_xdg_toplevel_decoration_configure_trampoline(void *data, struct zxdg_toplevel_decoration_v1 *decoration, uint32_t mode) {
	v_multiwindow_wayland_xdg_toplevel_decoration_configure(data, (void *)decoration, mode);
}

static void v_multiwindow_wayland_surface_enter_trampoline(void *data,
		struct wl_surface *surface, struct wl_output *output) {
	v_multiwindow_wayland_surface_enter(data, (void *)surface, (void *)output);
}

static void v_multiwindow_wayland_surface_leave_trampoline(void *data,
		struct wl_surface *surface, struct wl_output *output) {
	v_multiwindow_wayland_surface_leave(data, (void *)surface, (void *)output);
}

static void v_multiwindow_wayland_locked_pointer_locked_trampoline(void *data,
		struct zwp_locked_pointer_v1 *locked_pointer) {
	v_multiwindow_wayland_locked_pointer_locked(data, (void *)locked_pointer);
}

static void v_multiwindow_wayland_locked_pointer_unlocked_trampoline(void *data,
		struct zwp_locked_pointer_v1 *locked_pointer) {
	v_multiwindow_wayland_locked_pointer_unlocked(data, (void *)locked_pointer);
}

static void v_multiwindow_wayland_relative_pointer_motion_trampoline(void *data,
		struct zwp_relative_pointer_v1 *relative_pointer, uint32_t time_hi,
		uint32_t time_lo, wl_fixed_t dx, wl_fixed_t dy, wl_fixed_t dx_unaccelerated,
		wl_fixed_t dy_unaccelerated) {
	v_multiwindow_wayland_relative_pointer_motion(data, (void *)relative_pointer,
		time_hi, time_lo, wl_fixed_to_double(dx), wl_fixed_to_double(dy),
		wl_fixed_to_double(dx_unaccelerated), wl_fixed_to_double(dy_unaccelerated));
}

static void v_multiwindow_wayland_output_geometry_trampoline(void *data, struct wl_output *output,
		int32_t x, int32_t y, int32_t physical_width, int32_t physical_height, int32_t subpixel,
		const char *make, const char *model, int32_t transform) {
	v_multiwindow_wayland_output_geometry(data, (void *)output, x, y, physical_width,
		physical_height, subpixel, (char *)make, (char *)model, transform);
}

static void v_multiwindow_wayland_output_mode_trampoline(void *data, struct wl_output *output,
		uint32_t flags, int32_t width, int32_t height, int32_t refresh) {
	v_multiwindow_wayland_output_mode(data, (void *)output, flags, width, height, refresh);
}

static void v_multiwindow_wayland_output_done_trampoline(void *data, struct wl_output *output) {
	v_multiwindow_wayland_output_done(data, (void *)output);
}

static void v_multiwindow_wayland_output_scale_trampoline(void *data, struct wl_output *output,
		int32_t factor) {
	v_multiwindow_wayland_output_scale(data, (void *)output, factor);
}

static void v_multiwindow_wayland_output_name_trampoline(void *data, struct wl_output *output,
		const char *name) {
	v_multiwindow_wayland_output_name(data, (void *)output, (char *)name);
}

static void v_multiwindow_wayland_output_description_trampoline(void *data,
		struct wl_output *output, const char *description) {
	v_multiwindow_wayland_output_description(data, (void *)output, (char *)description);
}

static void v_multiwindow_wayland_buffer_release_trampoline(void *data, struct wl_buffer *buffer) {
	v_multiwindow_wayland_buffer_release(data, (void *)buffer);
}

static void v_multiwindow_wayland_frame_done_trampoline(void *data, struct wl_callback *callback, uint32_t time) {
	int authorization = v_multiwindow_wayland_frame_done(data, (void *)callback, time);
	if (authorization != 0) {
		if (authorization == 1) {
			wl_callback_destroy(callback);
		} else {
			v_multiwindow_wayland_proxy_destroy_local((void *)callback);
		}
		v_multiwindow_wayland_frame_destroyed(data, (void *)callback);
	}
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

static const struct wl_surface_listener v_multiwindow_wayland_surface_listener = {
	v_multiwindow_wayland_surface_enter_trampoline,
	v_multiwindow_wayland_surface_leave_trampoline,
};

static const struct zwp_locked_pointer_v1_listener v_multiwindow_wayland_locked_pointer_listener = {
	v_multiwindow_wayland_locked_pointer_locked_trampoline,
	v_multiwindow_wayland_locked_pointer_unlocked_trampoline,
};

static const struct zwp_relative_pointer_v1_listener v_multiwindow_wayland_relative_pointer_listener = {
	v_multiwindow_wayland_relative_pointer_motion_trampoline,
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

static const struct wl_data_source_listener v_multiwindow_wayland_data_source_listener = {
	v_multiwindow_wayland_data_source_target_trampoline,
	v_multiwindow_wayland_data_source_send_trampoline,
	v_multiwindow_wayland_data_source_cancelled_trampoline,
	v_multiwindow_wayland_data_source_dnd_drop_performed_trampoline,
	v_multiwindow_wayland_data_source_dnd_finished_trampoline,
	v_multiwindow_wayland_data_source_action_trampoline,
};

static const struct wl_buffer_listener v_multiwindow_wayland_buffer_listener = {
	v_multiwindow_wayland_buffer_release_trampoline,
};

static const struct wl_callback_listener v_multiwindow_wayland_frame_listener = {
	v_multiwindow_wayland_frame_done_trampoline,
};

static const struct zxdg_toplevel_decoration_v1_listener v_multiwindow_wayland_xdg_toplevel_decoration_listener = {
	v_multiwindow_wayland_xdg_toplevel_decoration_configure_trampoline,
};

static void v_multiwindow_wayland_exported_handle_trampoline(void *data,
		struct zxdg_exported_v2 *exported, const char *handle) {
	v_multiwindow_wayland_exported_handle(data, (void *)exported, (char *)handle);
}

static const struct zxdg_exported_v2_listener v_multiwindow_wayland_exported_listener = {
	v_multiwindow_wayland_exported_handle_trampoline,
};

static void v_multiwindow_wayland_fractional_scale_preferred_trampoline(
		void *data, struct wp_fractional_scale_v1 *fractional_scale,
		uint32_t scale) {
	v_multiwindow_wayland_fractional_scale_preferred(data,
		(void *)fractional_scale, scale);
}

static const struct wp_fractional_scale_v1_listener v_multiwindow_wayland_fractional_scale_listener = {
	v_multiwindow_wayland_fractional_scale_preferred_trampoline,
};

static const struct wl_output_listener v_multiwindow_wayland_output_listener = {
	v_multiwindow_wayland_output_geometry_trampoline,
	v_multiwindow_wayland_output_mode_trampoline,
	v_multiwindow_wayland_output_done_trampoline,
	v_multiwindow_wayland_output_scale_trampoline,
	v_multiwindow_wayland_output_name_trampoline,
	v_multiwindow_wayland_output_description_trampoline,
};

static inline uint32_t v_multiwindow_wayland_compositor_bind_version(uint32_t version) {
	return version < 4 ? version : 4;
}

static inline uint32_t v_multiwindow_wayland_seat_bind_version(uint32_t version) {
	return version < 5 ? version : 5;
}

static inline uint32_t v_multiwindow_wayland_output_bind_version(uint32_t version) {
	return version < 4 ? version : 4;
}

static uint64_t v_multiwindow_wayland_event_sequence = 1;
static int v_multiwindow_wayland_event_sequence_exhausted_flag = 0;

static inline uint64_t v_multiwindow_wayland_next_event_sequence(void) {
	if (v_multiwindow_wayland_event_sequence_exhausted_flag) {
		return 0;
	}
	uint64_t sequence = v_multiwindow_wayland_event_sequence;
	if (sequence == UINT64_MAX) {
		v_multiwindow_wayland_event_sequence = 0;
		v_multiwindow_wayland_event_sequence_exhausted_flag = 1;
	} else {
		v_multiwindow_wayland_event_sequence++;
	}
	return sequence;
}

static inline int v_multiwindow_wayland_event_sequence_exhausted(void) {
	return v_multiwindow_wayland_event_sequence_exhausted_flag;
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

static inline void *v_multiwindow_wayland_bind_relative_pointer_manager(
		struct wl_registry *registry, uint32_t name, uint32_t version) {
	if (version < 1) {
		return NULL;
	}
	return wl_registry_bind(registry, name,
		&zwp_relative_pointer_manager_v1_interface, 1);
}

static inline void *v_multiwindow_wayland_bind_pointer_constraints(
		struct wl_registry *registry, uint32_t name, uint32_t version) {
	if (version < 1) {
		return NULL;
	}
	return wl_registry_bind(registry, name,
		&zwp_pointer_constraints_v1_interface, 1);
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

static inline void *v_multiwindow_wayland_bind_xdg_foreign_exporter(
		struct wl_registry *registry, uint32_t name, uint32_t version) {
	if (version < 1) {
		return NULL;
	}
	return wl_registry_bind(registry, name, &zxdg_exporter_v2_interface, 1);
}

static inline void *v_multiwindow_wayland_bind_fractional_scale_manager(
		struct wl_registry *registry, uint32_t name, uint32_t version) {
	if (version < 1) {
		return NULL;
	}
	return wl_registry_bind(registry, name,
		&wp_fractional_scale_manager_v1_interface, 1);
}

static inline void *v_multiwindow_wayland_bind_viewporter(
		struct wl_registry *registry, uint32_t name, uint32_t version) {
	if (version < 1) {
		return NULL;
	}
	return wl_registry_bind(registry, name, &wp_viewporter_interface, 1);
}

static inline void *v_multiwindow_wayland_bind_output(struct wl_registry *registry, uint32_t name,
		uint32_t version) {
	return wl_registry_bind(registry, name, &wl_output_interface,
		v_multiwindow_wayland_output_bind_version(version));
}

static inline int v_multiwindow_wayland_add_registry_listener(struct wl_registry *registry, void *data) {
	return wl_registry_add_listener(registry, &v_multiwindow_wayland_registry_listener, data);
}

static inline int v_multiwindow_wayland_add_output_listener(struct wl_output *output, void *data) {
	return wl_output_add_listener(output, &v_multiwindow_wayland_output_listener, data);
}

static inline void v_multiwindow_wayland_output_destroy(struct wl_output *output,
		uint32_t bound_version) {
	if (bound_version >= 3) {
		wl_output_release(output);
	} else {
		wl_output_destroy(output);
	}
}

static inline int v_multiwindow_wayland_add_xdg_wm_base_listener(struct xdg_wm_base *wm_base, void *data) {
	return wl_proxy_add_listener((struct wl_proxy *)wm_base, (void (**)(void))&v_multiwindow_wayland_xdg_wm_base_listener, data);
}

static inline int v_multiwindow_wayland_add_xdg_surface_listener(struct xdg_surface *xdg_surface, void *data) {
	return wl_proxy_add_listener((struct wl_proxy *)xdg_surface, (void (**)(void))&v_multiwindow_wayland_xdg_surface_listener, data);
}

static inline int v_multiwindow_wayland_add_surface_listener(struct wl_surface *surface, void *data) {
	return wl_surface_add_listener(surface, &v_multiwindow_wayland_surface_listener, data);
}

static inline void *v_multiwindow_wayland_get_relative_pointer(
		struct zwp_relative_pointer_manager_v1 *manager, struct wl_pointer *pointer) {
	return (void *)zwp_relative_pointer_manager_v1_get_relative_pointer(manager, pointer);
}

static inline int v_multiwindow_wayland_add_relative_pointer_listener(
		struct zwp_relative_pointer_v1 *relative_pointer, void *data) {
	return zwp_relative_pointer_v1_add_listener(relative_pointer,
		&v_multiwindow_wayland_relative_pointer_listener, data);
}

static inline void *v_multiwindow_wayland_lock_pointer(
		struct zwp_pointer_constraints_v1 *constraints, struct wl_surface *surface,
		struct wl_pointer *pointer) {
	return (void *)zwp_pointer_constraints_v1_lock_pointer(constraints, surface, pointer,
		NULL, ZWP_POINTER_CONSTRAINTS_V1_LIFETIME_PERSISTENT);
}

static inline int v_multiwindow_wayland_add_locked_pointer_listener(
		struct zwp_locked_pointer_v1 *locked_pointer, void *data) {
	return zwp_locked_pointer_v1_add_listener(locked_pointer,
		&v_multiwindow_wayland_locked_pointer_listener, data);
}

static inline void v_multiwindow_wayland_relative_pointer_destroy(
		struct zwp_relative_pointer_v1 *relative_pointer) {
	if (relative_pointer != NULL) {
		zwp_relative_pointer_v1_destroy(relative_pointer);
	}
}

static inline void v_multiwindow_wayland_locked_pointer_destroy(
		struct zwp_locked_pointer_v1 *locked_pointer) {
	if (locked_pointer != NULL) {
		zwp_locked_pointer_v1_destroy(locked_pointer);
	}
}

static inline void v_multiwindow_wayland_relative_pointer_manager_destroy(
		struct zwp_relative_pointer_manager_v1 *manager) {
	if (manager != NULL) {
		zwp_relative_pointer_manager_v1_destroy(manager);
	}
}

static inline void v_multiwindow_wayland_pointer_constraints_destroy(
		struct zwp_pointer_constraints_v1 *constraints) {
	if (constraints != NULL) {
		zwp_pointer_constraints_v1_destroy(constraints);
	}
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

static inline void *v_multiwindow_wayland_data_device_manager_create_data_source(struct wl_data_device_manager *manager) {
	return (void *)wl_data_device_manager_create_data_source(manager);
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

static inline void v_multiwindow_wayland_surface_frame(struct wl_surface *surface,
		VMultiwindowNativePrimitive *out_result) {
	v_multiwindow_wayland_reset(out_result);
	if (surface == NULL) {
		return;
	}
	struct wl_callback *callback = wl_surface_frame(surface);
	if (out_result != NULL) {
		out_result->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_HANDLE;
		out_result->handle = (uint64_t)(uintptr_t)callback;
	}
}

static inline void v_multiwindow_wayland_add_frame_listener(struct wl_callback *callback,
		void *data, VMultiwindowNativePrimitive *out_result) {
	if (callback == NULL) {
		v_multiwindow_wayland_reset(out_result);
		return;
	}
	errno = 0;
	int result = wl_callback_add_listener(callback, &v_multiwindow_wayland_frame_listener,
		data);
	int native_errno = result < 0 ? errno : 0;
	v_multiwindow_wayland_result(out_result, result, native_errno);
}

static inline void v_multiwindow_wayland_destroy_frame_callback(void *callback,
		VMultiwindowNativePrimitive *out_result) {
	v_multiwindow_wayland_reset(out_result);
	if (callback != NULL) {
		wl_callback_destroy((struct wl_callback *)callback);
	}
}

static inline int v_multiwindow_wayland_add_data_device_listener(struct wl_data_device *device, void *data) {
	return wl_data_device_add_listener(device, &v_multiwindow_wayland_data_device_listener, data);
}

static inline int v_multiwindow_wayland_add_data_offer_listener(struct wl_data_offer *offer, void *data) {
	return wl_data_offer_add_listener(offer, &v_multiwindow_wayland_data_offer_listener, data);
}

static inline int v_multiwindow_wayland_add_data_source_listener(struct wl_data_source *source, void *data) {
	return wl_data_source_add_listener(source, &v_multiwindow_wayland_data_source_listener, data);
}

static inline void v_multiwindow_wayland_data_source_offer(struct wl_data_source *source, const char *mime_type) {
	wl_data_source_offer(source, mime_type);
}

static inline void v_multiwindow_wayland_data_source_destroy(struct wl_data_source *source) {
	if (source != NULL) {
		wl_data_source_destroy(source);
	}
}

static inline void v_multiwindow_wayland_data_device_set_selection(struct wl_data_device *device, struct wl_data_source *source, uint32_t serial) {
	wl_data_device_set_selection(device, source, serial);
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

static inline int v_multiwindow_wayland_io_interrupted(void) {
	return errno == EINTR;
}

static inline ssize_t v_multiwindow_wayland_safe_write(int fd, const void *buffer, size_t size) {
	sigset_t sigpipe_set;
	sigset_t old_mask;
	sigset_t pending;
	if (sigemptyset(&sigpipe_set) != 0 || sigaddset(&sigpipe_set, SIGPIPE) != 0) {
		return -1;
	}
	int mask_result = pthread_sigmask(SIG_BLOCK, &sigpipe_set, &old_mask);
	if (mask_result != 0) {
		errno = mask_result;
		return -1;
	}
	if (sigpending(&pending) != 0) {
		int saved_errno = errno;
		int restore_result = pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
		errno = restore_result != 0 ? restore_result : saved_errno;
		return -1;
	}
	int had_pending_sigpipe = sigismember(&pending, SIGPIPE) == 1;
	ssize_t result = -1;
	int attempts = 0;
	do {
		errno = 0;
		result = write(fd, buffer, size);
		attempts++;
	} while (result < 0 && errno == EINTR && attempts < 8);
	int saved_errno = errno;
	if (result < 0 && saved_errno == EPIPE && !had_pending_sigpipe) {
		struct timespec no_wait = {0, 0};
		while (sigtimedwait(&sigpipe_set, NULL, &no_wait) < 0 && errno == EINTR) {}
	}
	int restore_result = pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
	if (restore_result != 0) {
		errno = restore_result;
		return -1;
	}
	errno = saved_errno;
	return result;
}

static inline int v_multiwindow_wayland_safe_write_broken_pipe_probe(void) {
	int pipe_fds[2] = {-1, -1};
	if (pipe(pipe_fds) != 0) {
		return 0;
	}
	close(pipe_fds[0]);
	pipe_fds[0] = -1;
	pid_t child = fork();
	if (child < 0) {
		close(pipe_fds[1]);
		return 0;
	}
	if (child == 0) {
		if (signal(SIGPIPE, SIG_DFL) == SIG_ERR) {
			_exit(1);
		}
		sigset_t unblocked_sigpipe;
		if (sigemptyset(&unblocked_sigpipe) != 0
				|| sigaddset(&unblocked_sigpipe, SIGPIPE) != 0
				|| pthread_sigmask(SIG_UNBLOCK, &unblocked_sigpipe, NULL) != 0) {
			_exit(1);
		}
		sigset_t before_mask;
		sigset_t after_mask;
		if (pthread_sigmask(SIG_BLOCK, NULL, &before_mask) != 0) {
			_exit(1);
		}
		const char byte = 'x';
		ssize_t result = v_multiwindow_wayland_safe_write(pipe_fds[1], &byte, 1);
		int is_epipe = result == -1 && errno == EPIPE;
		int mask_unchanged = pthread_sigmask(SIG_BLOCK, NULL, &after_mask) == 0
			&& sigismember(&before_mask, SIGPIPE) == sigismember(&after_mask, SIGPIPE);
		close(pipe_fds[1]);
		_exit(is_epipe && mask_unchanged ? 0 : 1);
	}
	close(pipe_fds[1]);
	int status = 0;
	while (waitpid(child, &status, 0) < 0) {
		if (errno != EINTR) {
			return 0;
		}
	}
	return WIFEXITED(status) && WEXITSTATUS(status) == 0;
}

static inline int v_multiwindow_wayland_toplevel_state_contains(
		const struct wl_array *states, uint32_t expected) {
	if (states == NULL || states->data == NULL ||
		states->size % sizeof(uint32_t) != 0) {
		return 0;
	}
	const uint32_t *items = (const uint32_t *)states->data;
	const size_t count = states->size / sizeof(uint32_t);
	for (size_t i = 0; i < count; i++) {
		if (items[i] == expected) {
			return 1;
		}
	}
	return 0;
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

static inline int v_multiwindow_wayland_shm_layout(int width, int height,
		int32_t *out_stride, int32_t *out_size) {
	if (width <= 0 || height <= 0 || out_stride == NULL || out_size == NULL
			|| (size_t)width > (size_t)INT32_MAX / 4u) {
		return 0;
	}
	size_t stride = (size_t)width * 4u;
	if (stride == 0 || (size_t)height > (size_t)INT32_MAX / stride) {
		return 0;
	}
	size_t size = stride * (size_t)height;
	if (stride > (size_t)INT32_MAX || size == 0 || size > (size_t)INT32_MAX) {
		return 0;
	}
	*out_stride = (int32_t)stride;
	*out_size = (int32_t)size;
	return 1;
}

static inline void *v_multiwindow_wayland_create_shm_buffer(struct wl_shm *shm, int width, int height) {
	int32_t stride32 = 0;
	int32_t size32 = 0;
	if (shm == NULL || !v_multiwindow_wayland_shm_layout(width, height, &stride32, &size32)) {
		return NULL;
	}
	size_t stride = (size_t)stride32;
	size_t size = (size_t)size32;
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
	struct wl_shm_pool *pool = wl_shm_create_pool(shm, fd, size32);
	if (pool == NULL) {
		munmap(data, size);
		close(fd);
		return NULL;
	}
	struct wl_buffer *buffer = wl_shm_pool_create_buffer(pool, 0, width, height, stride32, WL_SHM_FORMAT_XRGB8888);
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

static inline void *v_multiwindow_wayland_export_toplevel(
		struct zxdg_exporter_v2 *exporter, struct wl_surface *surface) {
	if (exporter == NULL || surface == NULL) {
		return NULL;
	}
	return zxdg_exporter_v2_export_toplevel(exporter, surface);
}

static inline int v_multiwindow_wayland_add_exported_listener(
		struct zxdg_exported_v2 *exported, void *data) {
	if (exported == NULL) {
		return -1;
	}
	return zxdg_exported_v2_add_listener(exported,
		&v_multiwindow_wayland_exported_listener, data);
}

static inline void v_multiwindow_wayland_exported_destroy(
		struct zxdg_exported_v2 *exported) {
	if (exported != NULL) {
		zxdg_exported_v2_destroy(exported);
	}
}

static inline void v_multiwindow_wayland_exporter_destroy(
		struct zxdg_exporter_v2 *exporter) {
	if (exporter != NULL) {
		zxdg_exporter_v2_destroy(exporter);
	}
}

static inline void *v_multiwindow_wayland_get_fractional_scale(
		struct wp_fractional_scale_manager_v1 *manager,
		struct wl_surface *surface) {
	if (manager == NULL || surface == NULL) {
		return NULL;
	}
	return wp_fractional_scale_manager_v1_get_fractional_scale(manager, surface);
}

static inline int v_multiwindow_wayland_add_fractional_scale_listener(
		struct wp_fractional_scale_v1 *fractional_scale, void *data) {
	if (fractional_scale == NULL) {
		return -1;
	}
	return wp_fractional_scale_v1_add_listener(fractional_scale,
		&v_multiwindow_wayland_fractional_scale_listener, data);
}

static inline void v_multiwindow_wayland_fractional_scale_destroy(
		struct wp_fractional_scale_v1 *fractional_scale) {
	if (fractional_scale != NULL) {
		wp_fractional_scale_v1_destroy(fractional_scale);
	}
}

static inline void v_multiwindow_wayland_fractional_scale_manager_destroy(
		struct wp_fractional_scale_manager_v1 *manager) {
	if (manager != NULL) {
		wp_fractional_scale_manager_v1_destroy(manager);
	}
}

static inline void *v_multiwindow_wayland_get_viewport(
		struct wp_viewporter *viewporter, struct wl_surface *surface) {
	if (viewporter == NULL || surface == NULL) {
		return NULL;
	}
	return wp_viewporter_get_viewport(viewporter, surface);
}

static inline void v_multiwindow_wayland_viewport_set_destination(
		struct wp_viewport *viewport, int32_t width, int32_t height) {
	if (viewport != NULL) {
		wp_viewport_set_destination(viewport, width, height);
	}
}

static inline void v_multiwindow_wayland_viewport_destroy(
		struct wp_viewport *viewport) {
	if (viewport != NULL) {
		wp_viewport_destroy(viewport);
	}
}

static inline void v_multiwindow_wayland_viewporter_destroy(
		struct wp_viewporter *viewporter) {
	if (viewporter != NULL) {
		wp_viewporter_destroy(viewporter);
	}
}

static inline void v_multiwindow_wayland_attach_buffer(struct wl_surface *surface, struct wl_buffer *buffer, int width, int height) {
	if (surface != NULL && buffer != NULL) {
		wl_surface_attach(surface, buffer, 0, 0);
		wl_surface_damage(surface, 0, 0, width, height);
		wl_surface_commit(surface);
	}
}

static inline void v_multiwindow_wayland_unmap_surface(struct wl_surface *surface) {
	if (surface != NULL) {
		wl_surface_attach(surface, NULL, 0, 0);
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

#if defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static char v_multiwindow_wayland_last_marshaled_app_id[256];
static uintptr_t v_multiwindow_wayland_last_parent_child;
static uintptr_t v_multiwindow_wayland_last_parent_owner;

static inline const char *v_multiwindow_wayland_get_last_marshaled_app_id(void) {
	return v_multiwindow_wayland_last_marshaled_app_id;
}

static inline uintptr_t v_multiwindow_wayland_get_last_parent_child(void) {
	return v_multiwindow_wayland_last_parent_child;
}

static inline uintptr_t v_multiwindow_wayland_get_last_parent_owner(void) {
	return v_multiwindow_wayland_last_parent_owner;
}
#endif

static inline void v_multiwindow_wayland_xdg_toplevel_set_parent(struct xdg_toplevel *toplevel, struct xdg_toplevel *parent) {
#if defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
	v_multiwindow_wayland_last_parent_child = (uintptr_t)toplevel;
	v_multiwindow_wayland_last_parent_owner = (uintptr_t)parent;
#endif
	wl_proxy_marshal_flags((struct wl_proxy *)toplevel, XDG_TOPLEVEL_SET_PARENT, NULL, wl_proxy_get_version((struct wl_proxy *)toplevel), 0, parent);
}

static inline void v_multiwindow_wayland_xdg_toplevel_set_app_id(struct xdg_toplevel *toplevel, const char *app_id) {
#if defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
	snprintf(v_multiwindow_wayland_last_marshaled_app_id,
		sizeof(v_multiwindow_wayland_last_marshaled_app_id), "%s",
		app_id != NULL ? app_id : "");
#endif
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

static inline void v_multiwindow_wayland_xdg_toplevel_unset_fullscreen(struct xdg_toplevel *toplevel) {
	wl_proxy_marshal_flags((struct wl_proxy *)toplevel, XDG_TOPLEVEL_UNSET_FULLSCREEN, NULL, wl_proxy_get_version((struct wl_proxy *)toplevel), 0);
}

static inline void v_multiwindow_wayland_xdg_toplevel_set_maximized(struct xdg_toplevel *toplevel) {
	wl_proxy_marshal_flags((struct wl_proxy *)toplevel, XDG_TOPLEVEL_SET_MAXIMIZED, NULL, wl_proxy_get_version((struct wl_proxy *)toplevel), 0);
}

static inline void v_multiwindow_wayland_xdg_toplevel_unset_maximized(struct xdg_toplevel *toplevel) {
	wl_proxy_marshal_flags((struct wl_proxy *)toplevel, XDG_TOPLEVEL_UNSET_MAXIMIZED, NULL, wl_proxy_get_version((struct wl_proxy *)toplevel), 0);
}

static inline void v_multiwindow_wayland_xdg_toplevel_set_minimized(struct xdg_toplevel *toplevel) {
	wl_proxy_marshal_flags((struct wl_proxy *)toplevel, XDG_TOPLEVEL_SET_MINIMIZED, NULL, wl_proxy_get_version((struct wl_proxy *)toplevel), 0);
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

static inline void v_multiwindow_wayland_create_anchor_surface(
		struct wl_compositor *compositor, VMultiwindowNativePrimitive *out_result) {
	v_multiwindow_wayland_reset(out_result);
	if (compositor == NULL) {
		return;
	}
	struct wl_surface *surface = wl_compositor_create_surface(compositor);
	if (out_result != NULL) {
		out_result->valid_mask = V_MULTIWINDOW_NATIVE_VALID_HANDLE;
		out_result->handle = (uint64_t)(uintptr_t)surface;
	}
}

static inline void v_multiwindow_wayland_destroy_anchor_surface(void *surface,
		int marshal, VMultiwindowNativePrimitive *out_result) {
	v_multiwindow_wayland_reset(out_result);
	if (surface == NULL) {
		return;
	}
	const uint64_t surface_identity = (uint64_t)(uintptr_t)surface;
	uint64_t release_mode;
	if (marshal) {
		wl_surface_destroy((struct wl_surface *)surface);
		release_mode = V_MULTIWINDOW_WAYLAND_ANCHOR_RELEASE_PROTOCOL_DESTROY;
	} else {
		wl_proxy_destroy((struct wl_proxy *)surface);
		release_mode = V_MULTIWINDOW_WAYLAND_ANCHOR_RELEASE_LOCAL_PROXY_DESTROY;
	}
	if (out_result != NULL) {
		out_result->valid_mask = V_MULTIWINDOW_NATIVE_VALID_OBSERVED_FLAGS;
		out_result->observed_flags = release_mode;
	}
	#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST) \
		&& defined(V_MULTIWINDOW_NATIVE_WAYLAND_RELEASE_ORACLE_HELPERS_H)
	v_multiwindow_test_wayland_anchor_surface_destroyed(
		surface_identity, release_mode);
	#endif
}

static inline void v_multiwindow_wayland_egl_create_window(struct wl_surface *surface, int width,
		int height, VMultiwindowNativePrimitive *out_result) {
	v_multiwindow_wayland_reset(out_result);
	if (surface == NULL) {
		return;
	}
	struct wl_egl_window *window = wl_egl_window_create(surface, width, height);
	if (out_result != NULL) {
		out_result->valid_mask = V_MULTIWINDOW_NATIVE_VALID_HANDLE;
		out_result->handle = (uint64_t)(uintptr_t)window;
	}
}

static inline void v_multiwindow_wayland_egl_resize_window(void *egl_window, int width,
		int height, VMultiwindowNativePrimitive *out_result) {
	if (egl_window == NULL) {
		v_multiwindow_wayland_reset(out_result);
		return;
	}
	wl_egl_window_resize((struct wl_egl_window *)egl_window, width, height, 0, 0);
	v_multiwindow_wayland_reset(out_result);
}

static inline void v_multiwindow_wayland_egl_destroy_window(void *egl_window,
		VMultiwindowNativePrimitive *out_result) {
	if (egl_window == NULL) {
		v_multiwindow_wayland_reset(out_result);
		return;
	}
	wl_egl_window_destroy((struct wl_egl_window *)egl_window);
	v_multiwindow_wayland_reset(out_result);
}

#endif
