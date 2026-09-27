#include <stdint.h>
#include <stddef.h>
#include <wayland-util.h>

extern const struct wl_interface wl_pointer_interface;
extern const struct wl_interface v_multiwindow_wp_cursor_shape_device_v1_interface;

static const struct wl_interface *v_multiwindow_cursor_shape_v1_types[] = {
	NULL,
	NULL,
	&v_multiwindow_wp_cursor_shape_device_v1_interface,
	&wl_pointer_interface,
};

static const struct wl_message v_multiwindow_wp_cursor_shape_manager_v1_requests[] = {
	{ "destroy", "", v_multiwindow_cursor_shape_v1_types + 0 },
	{ "get_pointer", "no", v_multiwindow_cursor_shape_v1_types + 2 },
};

const struct wl_interface v_multiwindow_wp_cursor_shape_manager_v1_interface = {
	"wp_cursor_shape_manager_v1", 1,
	2, v_multiwindow_wp_cursor_shape_manager_v1_requests,
	0, NULL,
};

static const struct wl_message v_multiwindow_wp_cursor_shape_device_v1_requests[] = {
	{ "destroy", "", v_multiwindow_cursor_shape_v1_types + 0 },
	{ "set_shape", "uu", v_multiwindow_cursor_shape_v1_types + 0 },
};

const struct wl_interface v_multiwindow_wp_cursor_shape_device_v1_interface = {
	"wp_cursor_shape_device_v1", 1,
	2, v_multiwindow_wp_cursor_shape_device_v1_requests,
	0, NULL,
};
