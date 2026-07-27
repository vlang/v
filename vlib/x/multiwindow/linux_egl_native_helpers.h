#ifndef V_MULTIWINDOW_LINUX_EGL_NATIVE_HELPERS_H
#define V_MULTIWINDOW_LINUX_EGL_NATIVE_HELPERS_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <EGL/egl.h>
#include "native_render_result.h"

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

static inline uint64_t v_multiwindow_linux_egl_identity(const void *value) {
	return (uint64_t)(uintptr_t)value;
}

static inline int v_multiwindow_linux_egl_has_extension(const char *extensions,
	const char *required) {
	if (extensions == NULL || required == NULL || required[0] == '\0') {
		return 0;
	}
	size_t required_length = strlen(required);
	const char *cursor = extensions;
	while ((cursor = strstr(cursor, required)) != NULL) {
		int starts_token = cursor == extensions || cursor[-1] == ' ';
		char after = cursor[required_length];
		if (starts_token && (after == '\0' || after == ' ')) {
			return 1;
		}
		cursor += required_length;
	}
	return 0;
}

static inline void v_multiwindow_linux_egl_reset(VMultiwindowNativePrimitive *out) {
	if (out != NULL) {
		memset(out, 0, sizeof(*out));
	}
}

static inline void v_multiwindow_linux_egl_capture_return(
	VMultiwindowNativePrimitive *out, EGLBoolean value) {
	v_multiwindow_linux_egl_reset(out);
	if (out == NULL) {
		return;
	}
	out->valid_mask = V_MULTIWINDOW_NATIVE_VALID_RETURN_VALUE;
	out->return_value = (int64_t)value;
}

static inline void v_multiwindow_linux_egl_capture_handle(
	VMultiwindowNativePrimitive *out, const void *value) {
	v_multiwindow_linux_egl_reset(out);
	if (out == NULL) {
		return;
	}
	out->valid_mask = V_MULTIWINDOW_NATIVE_VALID_HANDLE;
	out->handle = v_multiwindow_linux_egl_identity(value);
}

static inline void v_multiwindow_linux_egl_get_error(
	VMultiwindowNativePrimitive *out) {
	EGLint error = eglGetError();
	v_multiwindow_linux_egl_reset(out);
	if (out != NULL) {
		out->valid_mask = V_MULTIWINDOW_NATIVE_VALID_EGL_ERROR;
		out->egl_error = (int64_t)error;
	}
}

static inline void v_multiwindow_linux_egl_get_display(uint64_t native_display,
	VMultiwindowNativePrimitive *out) {
	EGLDisplay display = eglGetDisplay((EGLNativeDisplayType)(uintptr_t)native_display);
	v_multiwindow_linux_egl_capture_handle(out,
		display == EGL_NO_DISPLAY ? NULL : (const void *)display);
}

static inline void v_multiwindow_linux_egl_initialize(uint64_t display_identity,
	VMultiwindowNativePrimitive *out) {
	EGLint major = 0;
	EGLint minor = 0;
	EGLBoolean result = eglInitialize((EGLDisplay)(uintptr_t)display_identity, &major, &minor);
	v_multiwindow_linux_egl_capture_return(out, result);
	if (out != NULL && result == EGL_TRUE) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_OBSERVED_COUNT |
			V_MULTIWINDOW_NATIVE_VALID_SELECTED_VALUE;
		out->observed_count = (uint64_t)(uint32_t)major;
		out->selected_value = (int64_t)minor;
	}
}

static inline void v_multiwindow_linux_egl_query_extensions(uint64_t display_identity,
		VMultiwindowNativePrimitive *out) {
	const char *extensions = eglQueryString((EGLDisplay)(uintptr_t)display_identity,
		EGL_EXTENSIONS);
	v_multiwindow_linux_egl_capture_handle(out, extensions);
	if (out != NULL && extensions != NULL) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_OBSERVED_FLAGS;
		out->observed_flags = v_multiwindow_linux_egl_has_extension(extensions,
			"EGL_KHR_create_context")
			? UINT64_C(1) : UINT64_C(0);
	}
}

static inline void v_multiwindow_linux_egl_bind_opengl_api(
	VMultiwindowNativePrimitive *out) {
	EGLBoolean result = eglBindAPI(EGL_OPENGL_API);
	v_multiwindow_linux_egl_capture_return(out, result);
}

static inline void v_multiwindow_linux_egl_choose_config(uint64_t display_identity,
	VMultiwindowNativePrimitive *out) {
	const EGLint attributes[] = {
		EGL_SURFACE_TYPE, EGL_WINDOW_BIT | EGL_PBUFFER_BIT,
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
	EGLint count = 0;
	EGLBoolean result = eglChooseConfig((EGLDisplay)(uintptr_t)display_identity,
		attributes, &config, 1, &count);
	v_multiwindow_linux_egl_capture_return(out, result);
	if (out != NULL) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_HANDLE |
			V_MULTIWINDOW_NATIVE_VALID_OBSERVED_COUNT;
		out->handle = v_multiwindow_linux_egl_identity(config);
		out->observed_count = count > 0 ? (uint64_t)count : UINT64_C(0);
	}
}

static inline void v_multiwindow_linux_egl_choose_wayland_config(
		uint64_t display_identity, VMultiwindowNativePrimitive *out) {
	const EGLint attributes[] = {
		EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
		EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
		EGL_RED_SIZE, 8,
		EGL_GREEN_SIZE, 8,
		EGL_BLUE_SIZE, 8,
		EGL_ALPHA_SIZE, 8,
		EGL_DEPTH_SIZE, 24,
		EGL_STENCIL_SIZE, 8,
		EGL_SAMPLE_BUFFERS, 0,
		EGL_SAMPLES, 0,
		EGL_NONE
	};
	EGLDisplay display = (EGLDisplay)(uintptr_t)display_identity;
	EGLConfig selected = NULL;
	EGLint count = 0;
	EGLint exact_count = 0;
	EGLBoolean result = eglChooseConfig(display, attributes, NULL, 0, &count);
	EGLConfig *configs = NULL;
	if (result == EGL_TRUE && count > 0) {
		configs = (EGLConfig *)calloc((size_t)count, sizeof(EGLConfig));
		if (configs == NULL) {
			result = EGL_FALSE;
		} else {
			EGLint returned = 0;
			result = eglChooseConfig(display, attributes, configs, count, &returned);
			if (result == EGL_TRUE) {
				for (EGLint index = 0; index < returned; index++) {
					EGLint surface_type = 0;
					EGLint renderable_type = 0;
					EGLint red = 0;
					EGLint green = 0;
					EGLint blue = 0;
					EGLint alpha = 0;
					EGLint depth = 0;
					EGLint stencil = 0;
					EGLint sample_buffers = 0;
					EGLint samples = 0;
					EGLBoolean queried =
						eglGetConfigAttrib(display, configs[index], EGL_SURFACE_TYPE,
							&surface_type) &&
						eglGetConfigAttrib(display, configs[index], EGL_RENDERABLE_TYPE,
							&renderable_type) &&
						eglGetConfigAttrib(display, configs[index], EGL_RED_SIZE, &red) &&
						eglGetConfigAttrib(display, configs[index], EGL_GREEN_SIZE, &green) &&
						eglGetConfigAttrib(display, configs[index], EGL_BLUE_SIZE, &blue) &&
						eglGetConfigAttrib(display, configs[index], EGL_ALPHA_SIZE, &alpha) &&
						eglGetConfigAttrib(display, configs[index], EGL_DEPTH_SIZE, &depth) &&
						eglGetConfigAttrib(display, configs[index], EGL_STENCIL_SIZE,
							&stencil) &&
						eglGetConfigAttrib(display, configs[index], EGL_SAMPLE_BUFFERS,
							&sample_buffers) &&
						eglGetConfigAttrib(display, configs[index], EGL_SAMPLES, &samples);
					if (queried != EGL_TRUE) {
						result = EGL_FALSE;
						selected = NULL;
						exact_count = 0;
						break;
					}
					if ((surface_type & EGL_WINDOW_BIT) != 0 &&
							(renderable_type & EGL_OPENGL_BIT) != 0 && red == 8 &&
							green == 8 && blue == 8 && alpha == 8 && depth == 24 &&
							stencil == 8 && sample_buffers == 0 && samples == 0) {
						exact_count++;
						if (selected == NULL) {
							selected = configs[index];
						}
					}
				}
			}
		}
	}
	free(configs);
	v_multiwindow_linux_egl_capture_return(out, result);
	if (out != NULL) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_HANDLE |
			V_MULTIWINDOW_NATIVE_VALID_OBSERVED_COUNT;
		out->handle = v_multiwindow_linux_egl_identity(selected);
		out->observed_count = exact_count > 0 ? (uint64_t)exact_count : UINT64_C(0);
	}
}

static inline void v_multiwindow_linux_egl_get_native_visual(uint64_t display_identity,
	uint64_t config_identity, VMultiwindowNativePrimitive *out) {
	EGLint visual = 0;
	EGLBoolean result = eglGetConfigAttrib((EGLDisplay)(uintptr_t)display_identity,
		(EGLConfig)(uintptr_t)config_identity, EGL_NATIVE_VISUAL_ID, &visual);
	v_multiwindow_linux_egl_capture_return(out, result);
	if (out != NULL) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_SELECTED_VALUE;
		out->selected_value = (int64_t)visual;
	}
}

static inline void v_multiwindow_linux_egl_create_context(uint64_t display_identity,
	uint64_t config_identity, VMultiwindowNativePrimitive *out) {
	const EGLint attributes[] = {
		EGL_CONTEXT_MAJOR_VERSION, 4,
		EGL_CONTEXT_MINOR_VERSION, 1,
		EGL_CONTEXT_OPENGL_PROFILE_MASK, EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT,
		EGL_NONE
	};
	EGLContext context = eglCreateContext((EGLDisplay)(uintptr_t)display_identity,
		(EGLConfig)(uintptr_t)config_identity, EGL_NO_CONTEXT, attributes);
	v_multiwindow_linux_egl_capture_handle(out,
		context == EGL_NO_CONTEXT ? NULL : (const void *)context);
}

static inline void v_multiwindow_linux_egl_create_window_surface(uint64_t display_identity,
	uint64_t config_identity, uint64_t native_window, VMultiwindowNativePrimitive *out) {
	EGLSurface surface = eglCreateWindowSurface((EGLDisplay)(uintptr_t)display_identity,
		(EGLConfig)(uintptr_t)config_identity, (EGLNativeWindowType)(uintptr_t)native_window,
		NULL);
	v_multiwindow_linux_egl_capture_handle(out,
		surface == EGL_NO_SURFACE ? NULL : (const void *)surface);
}

static inline void v_multiwindow_linux_egl_create_pbuffer_surface(uint64_t display_identity,
	uint64_t config_identity, int64_t width, int64_t height,
	VMultiwindowNativePrimitive *out) {
	const EGLint attributes[] = {
		EGL_WIDTH, (EGLint)width,
		EGL_HEIGHT, (EGLint)height,
		EGL_NONE
	};
	EGLSurface surface = eglCreatePbufferSurface((EGLDisplay)(uintptr_t)display_identity,
		(EGLConfig)(uintptr_t)config_identity, attributes);
	v_multiwindow_linux_egl_capture_handle(out,
		surface == EGL_NO_SURFACE ? NULL : (const void *)surface);
}

static inline void v_multiwindow_linux_egl_make_current(uint64_t display_identity,
	uint64_t draw_identity, uint64_t read_identity, uint64_t context_identity,
	VMultiwindowNativePrimitive *out) {
	EGLBoolean result = eglMakeCurrent((EGLDisplay)(uintptr_t)display_identity,
		(EGLSurface)(uintptr_t)draw_identity, (EGLSurface)(uintptr_t)read_identity,
		(EGLContext)(uintptr_t)context_identity);
	v_multiwindow_linux_egl_capture_return(out, result);
}

static inline void v_multiwindow_linux_egl_get_current_draw_surface(
	VMultiwindowNativePrimitive *out) {
	EGLSurface surface = eglGetCurrentSurface(EGL_DRAW);
	v_multiwindow_linux_egl_capture_handle(out,
		surface == EGL_NO_SURFACE ? NULL : (const void *)surface);
}

static inline void v_multiwindow_linux_egl_get_current_read_surface(
	VMultiwindowNativePrimitive *out) {
	EGLSurface surface = eglGetCurrentSurface(EGL_READ);
	v_multiwindow_linux_egl_capture_handle(out,
		surface == EGL_NO_SURFACE ? NULL : (const void *)surface);
}

static inline void v_multiwindow_linux_egl_get_current_context(
	VMultiwindowNativePrimitive *out) {
	EGLContext context = eglGetCurrentContext();
	v_multiwindow_linux_egl_capture_handle(out,
		context == EGL_NO_CONTEXT ? NULL : (const void *)context);
}

static inline void v_multiwindow_linux_egl_swap_buffers(uint64_t display_identity,
	uint64_t surface_identity, VMultiwindowNativePrimitive *out) {
	EGLBoolean result = eglSwapBuffers((EGLDisplay)(uintptr_t)display_identity,
		(EGLSurface)(uintptr_t)surface_identity);
	v_multiwindow_linux_egl_capture_return(out, result);
}

static inline void v_multiwindow_linux_egl_destroy_surface(uint64_t display_identity,
	uint64_t surface_identity, VMultiwindowNativePrimitive *out) {
	EGLBoolean result = eglDestroySurface((EGLDisplay)(uintptr_t)display_identity,
		(EGLSurface)(uintptr_t)surface_identity);
	v_multiwindow_linux_egl_capture_return(out, result);
}

static inline void v_multiwindow_linux_egl_destroy_context(uint64_t display_identity,
	uint64_t context_identity, VMultiwindowNativePrimitive *out) {
	EGLBoolean result = eglDestroyContext((EGLDisplay)(uintptr_t)display_identity,
		(EGLContext)(uintptr_t)context_identity);
	v_multiwindow_linux_egl_capture_return(out, result);
}

static inline void v_multiwindow_linux_egl_terminate(uint64_t display_identity,
	VMultiwindowNativePrimitive *out) {
	EGLBoolean result = eglTerminate((EGLDisplay)(uintptr_t)display_identity);
	v_multiwindow_linux_egl_capture_return(out, result);
}

static inline void v_multiwindow_linux_egl_release_thread(
	VMultiwindowNativePrimitive *out) {
	EGLBoolean result = eglReleaseThread();
	v_multiwindow_linux_egl_capture_return(out, result);
}

#endif
