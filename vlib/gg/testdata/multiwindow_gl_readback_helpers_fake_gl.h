#ifndef V_GG_MULTIWINDOW_GL_READBACK_HELPERS_FAKE_GL_H
#define V_GG_MULTIWINDOW_GL_READBACK_HELPERS_FAKE_GL_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

enum {
	V_GG_MULTIWINDOW_FAKE_GL_ERROR_NONE = 0,
	V_GG_MULTIWINDOW_FAKE_GL_ERROR_READ = 1,
	V_GG_MULTIWINDOW_FAKE_GL_ERROR_SETUP = 2,
	V_GG_MULTIWINDOW_FAKE_GL_FRAMEBUFFER_INCOMPLETE = 3,
};

typedef struct {
	uint32_t id;
} v_gg_multiwindow_fake_sg_image;

typedef struct {
	int active_slot;
	unsigned int tex[2];
	unsigned int tex_target;
	unsigned int msaa_render_buffer;
} v_gg_multiwindow_fake_sg_gl_image_info;

typedef struct {
	int stale_errors;
	int permanent_error;
	int operation_error;
	int pending_errors;
	int get_error_calls;
	int get_integer_calls;
	int read_calls;
	int delete_calls;
	int bind_buffer_calls;
	int pack_alignment;
	int pack_row_length;
	int pack_skip_rows;
	int pack_skip_pixels;
	int pixel_pack_buffer;
	int read_framebuffer;
	int draw_framebuffer;
	int read_buffer;
} v_gg_multiwindow_fake_gl_state;

static v_gg_multiwindow_fake_gl_state v_gg_multiwindow_fake_gl;

static inline void v_gg_multiwindow_fake_gl_reset(int stale_errors,
	int operation_error, int permanent_error) {
	memset(&v_gg_multiwindow_fake_gl, 0, sizeof(v_gg_multiwindow_fake_gl));
	v_gg_multiwindow_fake_gl.stale_errors = stale_errors;
	v_gg_multiwindow_fake_gl.operation_error = operation_error;
	v_gg_multiwindow_fake_gl.permanent_error = permanent_error;
	v_gg_multiwindow_fake_gl.pack_alignment = 8;
	v_gg_multiwindow_fake_gl.pack_row_length = 7;
	v_gg_multiwindow_fake_gl.pack_skip_rows = 3;
	v_gg_multiwindow_fake_gl.pack_skip_pixels = 5;
	v_gg_multiwindow_fake_gl.pixel_pack_buffer = 41;
	v_gg_multiwindow_fake_gl.read_framebuffer = 17;
	v_gg_multiwindow_fake_gl.draw_framebuffer = 29;
	v_gg_multiwindow_fake_gl.read_buffer = 19;
}

static inline unsigned int v_gg_multiwindow_fake_gl_get_error(void) {
	v_gg_multiwindow_fake_gl.get_error_calls++;
	if (v_gg_multiwindow_fake_gl.permanent_error) {
		return 0x0502;
	}
	if (v_gg_multiwindow_fake_gl.stale_errors > 0) {
		v_gg_multiwindow_fake_gl.stale_errors--;
		return 0x0502;
	}
	if (v_gg_multiwindow_fake_gl.pending_errors > 0) {
		v_gg_multiwindow_fake_gl.pending_errors--;
		return 0x0502;
	}
	return 0;
}

static inline void v_gg_multiwindow_fake_gl_get_integer(unsigned int name, int *value) {
	v_gg_multiwindow_fake_gl.get_integer_calls++;
	if (name == 0x0D05) {
		*value = v_gg_multiwindow_fake_gl.pack_alignment;
	} else if (name == 0x0D02) {
		*value = v_gg_multiwindow_fake_gl.pack_row_length;
	} else if (name == 0x0D03) {
		*value = v_gg_multiwindow_fake_gl.pack_skip_rows;
	} else if (name == 0x0D04) {
		*value = v_gg_multiwindow_fake_gl.pack_skip_pixels;
	} else if (name == 0x88ED) {
		*value = v_gg_multiwindow_fake_gl.pixel_pack_buffer;
	} else if (name == 0x8CAA) {
		*value = v_gg_multiwindow_fake_gl.read_framebuffer;
	} else if (name == 0x8CA6) {
		*value = v_gg_multiwindow_fake_gl.draw_framebuffer;
	} else if (name == 0x0C02) {
		*value = v_gg_multiwindow_fake_gl.read_buffer;
	}
}

static inline void v_gg_multiwindow_fake_gl_pixel_store(unsigned int name, int value) {
	if (name == 0x0D05 && value == 1 && v_gg_multiwindow_fake_gl.operation_error ==
		V_GG_MULTIWINDOW_FAKE_GL_ERROR_SETUP) {
		v_gg_multiwindow_fake_gl.pending_errors++;
	}
	if (name == 0x0D05) {
		v_gg_multiwindow_fake_gl.pack_alignment = value;
	} else if (name == 0x0D02) {
		v_gg_multiwindow_fake_gl.pack_row_length = value;
	} else if (name == 0x0D03) {
		v_gg_multiwindow_fake_gl.pack_skip_rows = value;
	} else if (name == 0x0D04) {
		v_gg_multiwindow_fake_gl.pack_skip_pixels = value;
	}
}

static inline void v_gg_multiwindow_fake_gl_bind_buffer(unsigned int target,
		unsigned int buffer) {
	if (target == 0x88EB) {
		v_gg_multiwindow_fake_gl.bind_buffer_calls++;
		v_gg_multiwindow_fake_gl.pixel_pack_buffer = (int)buffer;
	}
}

static inline void v_gg_multiwindow_fake_gl_read_pixels(int x, int y, int width,
	int height, unsigned int format, unsigned int type, void *pixels) {
	(void)x;
	(void)y;
	(void)format;
	(void)type;
	v_gg_multiwindow_fake_gl.read_calls++;
	if (v_gg_multiwindow_fake_gl.pack_row_length != 0
			|| v_gg_multiwindow_fake_gl.pack_skip_rows != 0
			|| v_gg_multiwindow_fake_gl.pack_skip_pixels != 0
			|| v_gg_multiwindow_fake_gl.pixel_pack_buffer != 0) {
		v_gg_multiwindow_fake_gl.pending_errors++;
		return;
	}
	memset(pixels, 0x5a, (size_t)width * (size_t)height * 4);
	if (v_gg_multiwindow_fake_gl.operation_error == V_GG_MULTIWINDOW_FAKE_GL_ERROR_READ) {
		v_gg_multiwindow_fake_gl.pending_errors++;
	}
}

static inline void v_gg_multiwindow_fake_gl_gen_framebuffers(int count,
	unsigned int *framebuffer) {
	(void)count;
	*framebuffer = 23;
}

static inline void v_gg_multiwindow_fake_gl_bind_framebuffer(unsigned int target,
	unsigned int framebuffer) {
	if (target == 0x8D40) {
		v_gg_multiwindow_fake_gl.read_framebuffer = (int)framebuffer;
		v_gg_multiwindow_fake_gl.draw_framebuffer = (int)framebuffer;
	} else if (target == 0x8CA8) {
		v_gg_multiwindow_fake_gl.read_framebuffer = (int)framebuffer;
	} else if (target == 0x8CA9) {
		v_gg_multiwindow_fake_gl.draw_framebuffer = (int)framebuffer;
	}
}

static inline void v_gg_multiwindow_fake_gl_framebuffer_texture_2d(unsigned int target,
	unsigned int attachment, unsigned int texture_target, unsigned int texture, int level) {
	(void)target;
	(void)attachment;
	(void)texture_target;
	(void)texture;
	(void)level;
	if (v_gg_multiwindow_fake_gl.operation_error == V_GG_MULTIWINDOW_FAKE_GL_ERROR_SETUP) {
		v_gg_multiwindow_fake_gl.pending_errors++;
	}
}

static inline unsigned int v_gg_multiwindow_fake_gl_check_framebuffer(unsigned int target) {
	(void)target;
	if (v_gg_multiwindow_fake_gl.operation_error ==
			V_GG_MULTIWINDOW_FAKE_GL_FRAMEBUFFER_INCOMPLETE) {
		return 0x8CD6;
	}
	return 0x8CD5;
}

static inline void v_gg_multiwindow_fake_gl_read_buffer(unsigned int buffer) {
	v_gg_multiwindow_fake_gl.read_buffer = (int)buffer;
}

static inline void v_gg_multiwindow_fake_gl_delete_framebuffers(int count,
	const unsigned int *framebuffer) {
	(void)count;
	(void)framebuffer;
	v_gg_multiwindow_fake_gl.delete_calls++;
}

static inline int v_gg_multiwindow_fake_sg_query_image_state(
	v_gg_multiwindow_fake_sg_image image) {
	(void)image;
	return 1;
}

static inline v_gg_multiwindow_fake_sg_gl_image_info
v_gg_multiwindow_fake_sg_query_image_info(v_gg_multiwindow_fake_sg_image image) {
	(void)image;
	v_gg_multiwindow_fake_sg_gl_image_info info;
	memset(&info, 0, sizeof(info));
	info.active_slot = 0;
	info.tex[0] = 55;
	info.tex_target = 0x0DE1;
	return info;
}

#ifdef V_GG_MULTIWINDOW_GL_READBACK_HELPERS_H
#define V_GG_MULTIWINDOW_FAKE_GL_HAD_TARGET_GUARD 1
#undef V_GG_MULTIWINDOW_GL_READBACK_HELPERS_H
#endif
#ifndef SOKOL_GLCORE
#define SOKOL_GLCORE 1
#define V_GG_MULTIWINDOW_FAKE_GL_DEFINED_SOKOL_GLCORE 1
#endif

#define GLint int
#define GLenum unsigned int
#define GLuint unsigned int
#define GL_NO_ERROR 0
#define GL_PACK_ALIGNMENT 0x0D05
#define GL_PACK_ROW_LENGTH 0x0D02
#define GL_PACK_SKIP_ROWS 0x0D03
#define GL_PACK_SKIP_PIXELS 0x0D04
#define GL_PIXEL_PACK_BUFFER 0x88EB
#define GL_PIXEL_PACK_BUFFER_BINDING 0x88ED
#define GL_FRAMEBUFFER_BINDING 0x8CA6
#define GL_DRAW_FRAMEBUFFER_BINDING 0x8CA6
#define GL_READ_FRAMEBUFFER_BINDING 0x8CAA
#define GL_READ_BUFFER 0x0C02
#define GL_RGBA 0x1908
#define GL_UNSIGNED_BYTE 0x1401
#define GL_TEXTURE_2D 0x0DE1
#define GL_FRAMEBUFFER 0x8D40
#define GL_READ_FRAMEBUFFER 0x8CA8
#define GL_DRAW_FRAMEBUFFER 0x8CA9
#define GL_COLOR_ATTACHMENT0 0x8CE0
#define GL_FRAMEBUFFER_COMPLETE 0x8CD5
#define glGetError v_gg_multiwindow_fake_gl_get_error
#define glGetIntegerv v_gg_multiwindow_fake_gl_get_integer
#define glPixelStorei v_gg_multiwindow_fake_gl_pixel_store
#define glBindBuffer v_gg_multiwindow_fake_gl_bind_buffer
#define glReadPixels v_gg_multiwindow_fake_gl_read_pixels
#define glGenFramebuffers v_gg_multiwindow_fake_gl_gen_framebuffers
#define glBindFramebuffer v_gg_multiwindow_fake_gl_bind_framebuffer
#define glFramebufferTexture2D v_gg_multiwindow_fake_gl_framebuffer_texture_2d
#define glCheckFramebufferStatus v_gg_multiwindow_fake_gl_check_framebuffer
#define glReadBuffer v_gg_multiwindow_fake_gl_read_buffer
#define glDeleteFramebuffers v_gg_multiwindow_fake_gl_delete_framebuffers
#define sg_image v_gg_multiwindow_fake_sg_image
#define sg_gl_image_info v_gg_multiwindow_fake_sg_gl_image_info
#define sg_query_image_state v_gg_multiwindow_fake_sg_query_image_state
#define sg_gl_query_image_info v_gg_multiwindow_fake_sg_query_image_info
#define SG_RESOURCESTATE_VALID 1
#define SG_NUM_INFLIGHT_FRAMES 2
#define V_GG_MULTIWINDOW_GL_READBACK_OK V_GG_MULTIWINDOW_FAKE_GL_READBACK_OK
#define V_GG_MULTIWINDOW_GL_READBACK_INVALID V_GG_MULTIWINDOW_FAKE_GL_READBACK_INVALID
#define V_GG_MULTIWINDOW_GL_READBACK_UNSUPPORTED V_GG_MULTIWINDOW_FAKE_GL_READBACK_UNSUPPORTED
#define V_GG_MULTIWINDOW_GL_READBACK_FAILED V_GG_MULTIWINDOW_FAKE_GL_READBACK_FAILED
#define V_GG_MULTIWINDOW_GL_ERROR_DRAIN_LIMIT V_GG_MULTIWINDOW_FAKE_GL_ERROR_DRAIN_LIMIT
#define v_gg_multiwindow_gl_drain_preexisting_errors v_gg_multiwindow_fake_gl_drain_preexisting_errors
#define v_gg_multiwindow_gl_collect_operation_errors v_gg_multiwindow_fake_gl_collect_operation_errors
#define VGGMultiwindowGLPackState VGGMultiwindowFakeGLPackState
#define v_gg_multiwindow_gl_save_pack_state v_gg_multiwindow_fake_gl_save_pack_state
#define v_gg_multiwindow_gl_set_tight_pack_state v_gg_multiwindow_fake_gl_set_tight_pack_state
#define v_gg_multiwindow_gl_restore_pack_state v_gg_multiwindow_fake_gl_restore_pack_state
#define v_gg_multiwindow_gl_readback_window_rgba8 v_gg_multiwindow_fake_gl_readback_window_rgba8
#define v_gg_multiwindow_gl_readback_image_rgba8 v_gg_multiwindow_fake_gl_readback_image_rgba8

#include "../multiwindow_gl_readback_helpers.h"

#undef v_gg_multiwindow_gl_readback_image_rgba8
#undef v_gg_multiwindow_gl_readback_window_rgba8
#undef v_gg_multiwindow_gl_restore_pack_state
#undef v_gg_multiwindow_gl_set_tight_pack_state
#undef v_gg_multiwindow_gl_save_pack_state
#undef VGGMultiwindowGLPackState
#undef v_gg_multiwindow_gl_collect_operation_errors
#undef v_gg_multiwindow_gl_drain_preexisting_errors
#undef V_GG_MULTIWINDOW_GL_ERROR_DRAIN_LIMIT
#undef V_GG_MULTIWINDOW_GL_READBACK_FAILED
#undef V_GG_MULTIWINDOW_GL_READBACK_UNSUPPORTED
#undef V_GG_MULTIWINDOW_GL_READBACK_INVALID
#undef V_GG_MULTIWINDOW_GL_READBACK_OK
#undef SG_NUM_INFLIGHT_FRAMES
#undef SG_RESOURCESTATE_VALID
#undef sg_gl_query_image_info
#undef sg_query_image_state
#undef sg_gl_image_info
#undef sg_image
#undef glDeleteFramebuffers
#undef glReadBuffer
#undef glCheckFramebufferStatus
#undef glFramebufferTexture2D
#undef glBindFramebuffer
#undef glGenFramebuffers
#undef glReadPixels
#undef glPixelStorei
#undef glBindBuffer
#undef glGetIntegerv
#undef glGetError
#undef GL_FRAMEBUFFER_COMPLETE
#undef GL_COLOR_ATTACHMENT0
#undef GL_DRAW_FRAMEBUFFER
#undef GL_READ_FRAMEBUFFER
#undef GL_FRAMEBUFFER
#undef GL_TEXTURE_2D
#undef GL_UNSIGNED_BYTE
#undef GL_RGBA
#undef GL_READ_BUFFER
#undef GL_READ_FRAMEBUFFER_BINDING
#undef GL_DRAW_FRAMEBUFFER_BINDING
#undef GL_FRAMEBUFFER_BINDING
#undef GL_PACK_ALIGNMENT
#undef GL_PACK_ROW_LENGTH
#undef GL_PACK_SKIP_ROWS
#undef GL_PACK_SKIP_PIXELS
#undef GL_PIXEL_PACK_BUFFER
#undef GL_PIXEL_PACK_BUFFER_BINDING
#undef GL_NO_ERROR
#undef GLuint
#undef GLenum
#undef GLint

#undef V_GG_MULTIWINDOW_GL_READBACK_HELPERS_H
#ifdef V_GG_MULTIWINDOW_FAKE_GL_HAD_TARGET_GUARD
#define V_GG_MULTIWINDOW_GL_READBACK_HELPERS_H
#undef V_GG_MULTIWINDOW_FAKE_GL_HAD_TARGET_GUARD
#endif
#ifdef V_GG_MULTIWINDOW_FAKE_GL_DEFINED_SOKOL_GLCORE
#undef SOKOL_GLCORE
#undef V_GG_MULTIWINDOW_FAKE_GL_DEFINED_SOKOL_GLCORE
#endif

static inline int v_gg_multiwindow_fake_gl_window_case(int stale_errors,
	int operation_error, int permanent_error, int *pack_alignment,
	int *pack_state_restored, int *bind_buffer_calls, int *get_integer_calls,
	int *read_calls, int *get_error_calls) {
	v_gg_multiwindow_fake_gl_reset(stale_errors, operation_error, permanent_error);
	uint8_t pixels[16];
	int result = v_gg_multiwindow_fake_gl_readback_window_rgba8(2, 0, 0, 2, 2,
		pixels, sizeof(pixels));
	*pack_alignment = v_gg_multiwindow_fake_gl.pack_alignment;
	*pack_state_restored = v_gg_multiwindow_fake_gl.pack_alignment == 8
		&& v_gg_multiwindow_fake_gl.pack_row_length == 7
		&& v_gg_multiwindow_fake_gl.pack_skip_rows == 3
		&& v_gg_multiwindow_fake_gl.pack_skip_pixels == 5
		&& v_gg_multiwindow_fake_gl.pixel_pack_buffer == 41;
	*bind_buffer_calls = v_gg_multiwindow_fake_gl.bind_buffer_calls;
	*get_integer_calls = v_gg_multiwindow_fake_gl.get_integer_calls;
	*read_calls = v_gg_multiwindow_fake_gl.read_calls;
	*get_error_calls = v_gg_multiwindow_fake_gl.get_error_calls;
	return result;
}

static inline int v_gg_multiwindow_fake_gl_image_case(int stale_errors,
	int operation_error, int permanent_error, int *pack_alignment, int *read_framebuffer,
	int *draw_framebuffer, int *read_buffer, int *pack_state_restored,
	int *bind_buffer_calls, int *get_integer_calls, int *read_calls, int *delete_calls,
	int *get_error_calls) {
	v_gg_multiwindow_fake_gl_reset(stale_errors, operation_error, permanent_error);
	uint8_t pixels[16];
	int result = v_gg_multiwindow_fake_gl_readback_image_rgba8(1, 2, 0, 0, 2, 2,
		pixels, sizeof(pixels));
	*pack_alignment = v_gg_multiwindow_fake_gl.pack_alignment;
	*read_framebuffer = v_gg_multiwindow_fake_gl.read_framebuffer;
	*draw_framebuffer = v_gg_multiwindow_fake_gl.draw_framebuffer;
	*read_buffer = v_gg_multiwindow_fake_gl.read_buffer;
	*pack_state_restored = v_gg_multiwindow_fake_gl.pack_alignment == 8
		&& v_gg_multiwindow_fake_gl.pack_row_length == 7
		&& v_gg_multiwindow_fake_gl.pack_skip_rows == 3
		&& v_gg_multiwindow_fake_gl.pack_skip_pixels == 5
		&& v_gg_multiwindow_fake_gl.pixel_pack_buffer == 41;
	*bind_buffer_calls = v_gg_multiwindow_fake_gl.bind_buffer_calls;
	*get_integer_calls = v_gg_multiwindow_fake_gl.get_integer_calls;
	*read_calls = v_gg_multiwindow_fake_gl.read_calls;
	*delete_calls = v_gg_multiwindow_fake_gl.delete_calls;
	*get_error_calls = v_gg_multiwindow_fake_gl.get_error_calls;
	return result;
}

#endif
