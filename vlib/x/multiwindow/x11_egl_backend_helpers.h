#ifndef V_MULTIWINDOW_X11_EGL_BACKEND_HELPERS_H
#define V_MULTIWINDOW_X11_EGL_BACKEND_HELPERS_H

#include <stdint.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <EGL/egl.h>

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

#ifndef MWM_HINTS_DECORATIONS
#define MWM_HINTS_DECORATIONS (1L << 1)
#endif

typedef struct {
	unsigned long flags;
	unsigned long functions;
	unsigned long decorations;
	long input_mode;
	unsigned long status;
} VMultiwindowMotifWmHints;

static inline int v_multiwindow_x11_apply_config_hints(Display *display, unsigned long window, int width, int height, int min_width, int min_height, int resizable, int borderless, int fullscreen) {
	XSizeHints size_hints;
	memset(&size_hints, 0, sizeof(size_hints));
	size_hints.flags = PSize;
	size_hints.width = width;
	size_hints.height = height;
	if (min_width > 0 || min_height > 0) {
		size_hints.flags |= PMinSize;
		size_hints.min_width = min_width > 0 ? min_width : 1;
		size_hints.min_height = min_height > 0 ? min_height : 1;
	}
	if (!resizable) {
		size_hints.flags |= PMinSize | PMaxSize;
		size_hints.min_width = width;
		size_hints.min_height = height;
		size_hints.max_width = width;
		size_hints.max_height = height;
	}
	XSetWMNormalHints(display, (Window)window, &size_hints);

	if (borderless) {
		Atom motif_hints_atom = XInternAtom(display, "_MOTIF_WM_HINTS", False);
		if (motif_hints_atom == None) {
			return 0;
		}
		VMultiwindowMotifWmHints motif_hints;
		memset(&motif_hints, 0, sizeof(motif_hints));
		motif_hints.flags = MWM_HINTS_DECORATIONS;
		motif_hints.decorations = 0;
		XChangeProperty(display, (Window)window, motif_hints_atom, motif_hints_atom, 32, PropModeReplace, (unsigned char *)&motif_hints, 5);
	}

	if (fullscreen) {
		Atom state_atom = XInternAtom(display, "_NET_WM_STATE", False);
		Atom fullscreen_atom = XInternAtom(display, "_NET_WM_STATE_FULLSCREEN", False);
		if (state_atom == None || fullscreen_atom == None) {
			return 0;
		}
		XChangeProperty(display, (Window)window, state_atom, XA_ATOM, 32, PropModeReplace, (unsigned char *)&fullscreen_atom, 1);
	}

	return 1;
}

static inline int v_multiwindow_x11_get_window_size(Display *display, unsigned long window, int *out_width, int *out_height) {
	XWindowAttributes attrs;
	memset(&attrs, 0, sizeof(attrs));
	if (!XGetWindowAttributes(display, (Window)window, &attrs)) {
		return 0;
	}
	if (attrs.width <= 0 || attrs.height <= 0) {
		return 0;
	}
	*out_width = attrs.width;
	*out_height = attrs.height;
	return 1;
}

static inline void *v_multiwindow_x11_egl_get_display(Display *display) {
	return (void *)eglGetDisplay((EGLNativeDisplayType)display);
}

static inline int v_multiwindow_x11_egl_initialize(void *egl_display) {
	return eglInitialize((EGLDisplay)egl_display, NULL, NULL) == EGL_TRUE ? 1 : 0;
}

static inline int v_multiwindow_x11_egl_bind_opengl_api(void) {
	return eglBindAPI(EGL_OPENGL_API) == EGL_TRUE ? 1 : 0;
}

static inline int v_multiwindow_x11_egl_choose_config(void *egl_display, void **out_config, int *out_visual_id) {
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
	EGLint visual_id = 0;
	if (eglChooseConfig((EGLDisplay)egl_display, attrs, &config, 1, &config_count) != EGL_TRUE || config_count == 0) {
		return 0;
	}
	if (eglGetConfigAttrib((EGLDisplay)egl_display, config, EGL_NATIVE_VISUAL_ID, &visual_id) != EGL_TRUE || visual_id == 0) {
		return 0;
	}
	*out_config = (void *)config;
	*out_visual_id = (int)visual_id;
	return 1;
}

static inline void *v_multiwindow_x11_egl_create_context(void *egl_display, void *egl_config) {
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

static inline unsigned long v_multiwindow_x11_create_egl_window(Display *display, unsigned long root, int screen, int native_visual_id, int width, int height, unsigned long *out_colormap) {
	XVisualInfo template_info;
	XVisualInfo *visual_info = NULL;
	XSetWindowAttributes attrs;
	int visual_count = 0;
	Window window = 0;

	template_info.visualid = (VisualID)native_visual_id;
	template_info.screen = screen;
	visual_info = XGetVisualInfo(display, VisualIDMask | VisualScreenMask, &template_info, &visual_count);
	if (visual_info == NULL || visual_count <= 0) {
		return 0;
	}

	memset(&attrs, 0, sizeof(attrs));
	attrs.colormap = XCreateColormap(display, root, visual_info->visual, AllocNone);
	attrs.border_pixel = 0;
	attrs.background_pixel = 0;
	attrs.event_mask = StructureNotifyMask;
	window = XCreateWindow(display, root, 0, 0, (unsigned int)width, (unsigned int)height, 0,
		visual_info->depth, InputOutput, visual_info->visual,
		CWColormap | CWBorderPixel | CWBackPixel | CWEventMask, &attrs);
	if (window == 0) {
		XFreeColormap(display, attrs.colormap);
		attrs.colormap = 0;
	}
	if (out_colormap != NULL) {
		*out_colormap = attrs.colormap;
	}
	XFree(visual_info);
	return window;
}

static inline void *v_multiwindow_x11_egl_create_window_surface(void *egl_display, void *egl_config, unsigned long window) {
	EGLSurface surface = eglCreateWindowSurface((EGLDisplay)egl_display, (EGLConfig)egl_config, (EGLNativeWindowType)window, NULL);
	return surface == EGL_NO_SURFACE ? NULL : (void *)surface;
}

static inline int v_multiwindow_x11_egl_make_current(void *egl_display, void *egl_surface, void *egl_context) {
	return eglMakeCurrent((EGLDisplay)egl_display, (EGLSurface)egl_surface, (EGLSurface)egl_surface, (EGLContext)egl_context) == EGL_TRUE ? 1 : 0;
}

static inline void v_multiwindow_x11_egl_clear_current(void *egl_display) {
	eglMakeCurrent((EGLDisplay)egl_display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
}

static inline int v_multiwindow_x11_egl_swap_buffers(void *egl_display, void *egl_surface) {
	return eglSwapBuffers((EGLDisplay)egl_display, (EGLSurface)egl_surface) == EGL_TRUE ? 1 : 0;
}

static inline void v_multiwindow_x11_egl_destroy_surface(void *egl_display, void *egl_surface) {
	if (egl_surface != NULL) {
		eglDestroySurface((EGLDisplay)egl_display, (EGLSurface)egl_surface);
	}
}

static inline void v_multiwindow_x11_egl_destroy_context(void *egl_display, void *egl_context) {
	if (egl_context != NULL) {
		eglDestroyContext((EGLDisplay)egl_display, (EGLContext)egl_context);
	}
}

static inline void v_multiwindow_x11_egl_terminate(void *egl_display) {
	if (egl_display != NULL) {
		eglTerminate((EGLDisplay)egl_display);
	}
}

#endif
