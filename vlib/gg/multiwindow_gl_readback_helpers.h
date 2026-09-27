#ifndef V_GG_MULTIWINDOW_GL_READBACK_HELPERS_H
#define V_GG_MULTIWINDOW_GL_READBACK_HELPERS_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum {
	V_GG_MULTIWINDOW_GL_READBACK_OK = 0,
	V_GG_MULTIWINDOW_GL_READBACK_INVALID = 1,
	V_GG_MULTIWINDOW_GL_READBACK_UNSUPPORTED = 2,
	V_GG_MULTIWINDOW_GL_READBACK_FAILED = 3,
	V_GG_MULTIWINDOW_GL_ERROR_DRAIN_LIMIT = 64,
};

#if defined(SOKOL_GLCORE) || defined(SOKOL_GLES3)
static inline int v_gg_multiwindow_gl_drain_preexisting_errors(void) {
	for (int i = 0; i < V_GG_MULTIWINDOW_GL_ERROR_DRAIN_LIMIT; i++) {
		if (glGetError() == GL_NO_ERROR) {
			return 1;
		}
	}
	return 0;
}

static inline int v_gg_multiwindow_gl_collect_operation_errors(void) {
	int failed = 0;
	for (int i = 0; i < V_GG_MULTIWINDOW_GL_ERROR_DRAIN_LIMIT; i++) {
		if (glGetError() == GL_NO_ERROR) {
			return failed;
		}
		failed = 1;
	}
	return 1;
}

typedef struct {
	GLint alignment;
	GLint row_length;
	GLint skip_rows;
	GLint skip_pixels;
	GLint pixel_pack_buffer;
} VGGMultiwindowGLPackState;

static inline void v_gg_multiwindow_gl_save_pack_state(
		VGGMultiwindowGLPackState *state) {
	glGetIntegerv(GL_PACK_ALIGNMENT, &state->alignment);
	glGetIntegerv(GL_PACK_ROW_LENGTH, &state->row_length);
	glGetIntegerv(GL_PACK_SKIP_ROWS, &state->skip_rows);
	glGetIntegerv(GL_PACK_SKIP_PIXELS, &state->skip_pixels);
	glGetIntegerv(GL_PIXEL_PACK_BUFFER_BINDING, &state->pixel_pack_buffer);
}

static inline void v_gg_multiwindow_gl_set_tight_pack_state(void) {
	glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
	glPixelStorei(GL_PACK_ALIGNMENT, 1);
	glPixelStorei(GL_PACK_ROW_LENGTH, 0);
	glPixelStorei(GL_PACK_SKIP_ROWS, 0);
	glPixelStorei(GL_PACK_SKIP_PIXELS, 0);
}

static inline void v_gg_multiwindow_gl_restore_pack_state(
		const VGGMultiwindowGLPackState *state) {
	glPixelStorei(GL_PACK_ALIGNMENT, state->alignment);
	glPixelStorei(GL_PACK_ROW_LENGTH, state->row_length);
	glPixelStorei(GL_PACK_SKIP_ROWS, state->skip_rows);
	glPixelStorei(GL_PACK_SKIP_PIXELS, state->skip_pixels);
	glBindBuffer(GL_PIXEL_PACK_BUFFER, (GLuint)state->pixel_pack_buffer);
}
#endif

static inline int v_gg_multiwindow_gl_readback_window_rgba8(int framebuffer_height,
	int x, int y, int width, int height, uint8_t *pixels, size_t pixels_len) {
#if defined(SOKOL_GLCORE) || defined(SOKOL_GLES3)
	if (framebuffer_height <= 0 || x < 0 || y < 0 || width <= 0 || height <= 0 ||
		height > framebuffer_height || y > framebuffer_height - height || pixels == NULL ||
		pixels_len != (size_t)width * (size_t)height * 4) {
		return V_GG_MULTIWINDOW_GL_READBACK_INVALID;
	}
	if (!v_gg_multiwindow_gl_drain_preexisting_errors()) {
		return V_GG_MULTIWINDOW_GL_READBACK_FAILED;
	}

	VGGMultiwindowGLPackState pack_state;
	v_gg_multiwindow_gl_save_pack_state(&pack_state);
	v_gg_multiwindow_gl_set_tight_pack_state();
	glReadPixels(x, framebuffer_height - y - height, width, height, GL_RGBA,
		GL_UNSIGNED_BYTE, pixels);
	v_gg_multiwindow_gl_restore_pack_state(&pack_state);
	if (v_gg_multiwindow_gl_collect_operation_errors()) {
		return V_GG_MULTIWINDOW_GL_READBACK_FAILED;
	}

	size_t row_bytes = (size_t)width * 4;
	uint8_t *row = (uint8_t *)malloc(row_bytes);
	if (row == NULL) {
		return V_GG_MULTIWINDOW_GL_READBACK_FAILED;
	}
	for (int top = 0, bottom = height - 1; top < bottom; top++, bottom--) {
		uint8_t *top_row = pixels + (size_t)top * row_bytes;
		uint8_t *bottom_row = pixels + (size_t)bottom * row_bytes;
		memcpy(row, top_row, row_bytes);
		memcpy(top_row, bottom_row, row_bytes);
		memcpy(bottom_row, row, row_bytes);
	}
	free(row);
	return V_GG_MULTIWINDOW_GL_READBACK_OK;
#else
	(void)framebuffer_height;
	(void)x;
	(void)y;
	(void)width;
	(void)height;
	(void)pixels;
	(void)pixels_len;
	return V_GG_MULTIWINDOW_GL_READBACK_UNSUPPORTED;
#endif
}

static inline int v_gg_multiwindow_gl_readback_image_rgba8(uint32_t image_id,
	int image_height, int x, int y, int width, int height, uint8_t *pixels,
	size_t pixels_len) {
#if defined(SOKOL_GLCORE) || defined(SOKOL_GLES3)
	if (image_id == 0 || image_height <= 0 || x < 0 || y < 0 || width <= 0 ||
		height <= 0 || height > image_height || y > image_height - height || pixels == NULL ||
		pixels_len != (size_t)width * (size_t)height * 4) {
		return V_GG_MULTIWINDOW_GL_READBACK_INVALID;
	}
	sg_image image = { image_id };
	if (sg_query_image_state(image) != SG_RESOURCESTATE_VALID) {
		return V_GG_MULTIWINDOW_GL_READBACK_INVALID;
	}
	sg_gl_image_info info = sg_gl_query_image_info(image);
	if (info.active_slot < 0 || info.active_slot >= SG_NUM_INFLIGHT_FRAMES ||
		info.tex[info.active_slot] == 0 || info.tex_target != GL_TEXTURE_2D ||
		info.msaa_render_buffer != 0) {
		return V_GG_MULTIWINDOW_GL_READBACK_UNSUPPORTED;
	}
	if (!v_gg_multiwindow_gl_drain_preexisting_errors()) {
		return V_GG_MULTIWINDOW_GL_READBACK_FAILED;
	}

	GLint previous_read_framebuffer = 0;
	GLint previous_draw_framebuffer = 0;
	GLint previous_read_buffer = 0;
	VGGMultiwindowGLPackState pack_state;
	glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, &previous_read_framebuffer);
	glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &previous_draw_framebuffer);
	glGetIntegerv(GL_READ_BUFFER, &previous_read_buffer);
	v_gg_multiwindow_gl_save_pack_state(&pack_state);

	GLuint framebuffer = 0;
	glGenFramebuffers(1, &framebuffer);
	if (framebuffer == 0) {
		(void)v_gg_multiwindow_gl_collect_operation_errors();
		return V_GG_MULTIWINDOW_GL_READBACK_FAILED;
	}
	glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, info.tex_target,
		info.tex[info.active_slot], 0);
	int framebuffer_complete =
		glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE;
	if (framebuffer_complete) {
		glReadBuffer(GL_COLOR_ATTACHMENT0);
		v_gg_multiwindow_gl_set_tight_pack_state();
		glReadPixels(x, image_height - y - height, width, height, GL_RGBA,
			GL_UNSIGNED_BYTE, pixels);
		v_gg_multiwindow_gl_restore_pack_state(&pack_state);
		glBindFramebuffer(GL_READ_FRAMEBUFFER, (GLuint)previous_read_framebuffer);
		glReadBuffer((GLenum)previous_read_buffer);
		glBindFramebuffer(GL_DRAW_FRAMEBUFFER, (GLuint)previous_draw_framebuffer);
	} else {
		glBindFramebuffer(GL_READ_FRAMEBUFFER, (GLuint)previous_read_framebuffer);
		glBindFramebuffer(GL_DRAW_FRAMEBUFFER, (GLuint)previous_draw_framebuffer);
	}
	glDeleteFramebuffers(1, &framebuffer);
	int operation_failed = v_gg_multiwindow_gl_collect_operation_errors();
	if (!framebuffer_complete || operation_failed) {
		return V_GG_MULTIWINDOW_GL_READBACK_FAILED;
	}

	size_t row_bytes = (size_t)width * 4;
	uint8_t *row = (uint8_t *)malloc(row_bytes);
	if (row == NULL) {
		return V_GG_MULTIWINDOW_GL_READBACK_FAILED;
	}
	for (int top = 0, bottom = height - 1; top < bottom; top++, bottom--) {
		uint8_t *top_row = pixels + (size_t)top * row_bytes;
		uint8_t *bottom_row = pixels + (size_t)bottom * row_bytes;
		memcpy(row, top_row, row_bytes);
		memcpy(top_row, bottom_row, row_bytes);
		memcpy(bottom_row, row, row_bytes);
	}
	free(row);
	return V_GG_MULTIWINDOW_GL_READBACK_OK;
#else
	(void)image_id;
	(void)image_height;
	(void)x;
	(void)y;
	(void)width;
	(void)height;
	(void)pixels;
	(void)pixels_len;
	return V_GG_MULTIWINDOW_GL_READBACK_UNSUPPORTED;
#endif
}

#endif
