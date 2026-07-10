#ifndef V_MULTIWINDOW_WAYLAND_BACKEND_HELPERS_H
#define V_MULTIWINDOW_WAYLAND_BACKEND_HELPERS_H

#include <stdint.h>
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

extern const struct wl_interface v_multiwindow_xdg_wm_base_interface;
extern const struct wl_interface v_multiwindow_xdg_surface_interface;
extern const struct wl_interface v_multiwindow_xdg_toplevel_interface;

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

static inline uint32_t v_multiwindow_wayland_compositor_bind_version(uint32_t version) {
	return version < 4 ? version : 4;
}

static inline void *v_multiwindow_wayland_bind_compositor(struct wl_registry *registry, uint32_t name, uint32_t version) {
	return wl_registry_bind(registry, name, &wl_compositor_interface, v_multiwindow_wayland_compositor_bind_version(version));
}

static inline void *v_multiwindow_wayland_bind_xdg_wm_base(struct wl_registry *registry, uint32_t name) {
	return wl_registry_bind(registry, name, &v_multiwindow_xdg_wm_base_interface, 1);
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
