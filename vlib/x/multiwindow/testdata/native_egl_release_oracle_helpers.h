#ifndef V_MULTIWINDOW_NATIVE_EGL_RELEASE_ORACLE_HELPERS_H
#define V_MULTIWINDOW_NATIVE_EGL_RELEASE_ORACLE_HELPERS_H

#include <EGL/egl.h>
#include <pthread.h>
#include <stdint.h>
#include <sys/types.h>

#if defined(__linux__) && defined(__GLIBC__) && !defined(__USE_GNU) \
	&& (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 30))
extern pid_t gettid(void);
#endif

#include "native_release_oracle_common.h"

#define V_MULTIWINDOW_TEST_RELEASE_ORACLE_CAPACITY 256

enum VMultiwindowTestEglReleaseKind {
	V_MULTIWINDOW_TEST_EGL_SURFACE_RELEASE = 1,
	V_MULTIWINDOW_TEST_EGL_CONTEXT_RELEASE = 2,
	V_MULTIWINDOW_TEST_EGL_DISPLAY_RELEASE = 3,
	V_MULTIWINDOW_TEST_EGL_THREAD_RELEASE = 4,
	V_MULTIWINDOW_TEST_X11_IM_CLOSE = 5,
	V_MULTIWINDOW_TEST_X11_DISPLAY_CLOSE = 6
};

typedef struct VMultiwindowTestEglReleaseRecord {
	uint64_t sequence;
	uint64_t kind;
	uint64_t identity;
	uint64_t parent_identity;
} VMultiwindowTestEglReleaseRecord;

static VMultiwindowTestEglReleaseRecord
	v_multiwindow_test_egl_release_records[V_MULTIWINDOW_TEST_RELEASE_ORACLE_CAPACITY];
static uint64_t v_multiwindow_test_egl_release_record_count;
static int v_multiwindow_test_egl_release_record_overflow;

static inline void v_multiwindow_test_egl_release_record(
		uint64_t kind, uint64_t identity, uint64_t parent_identity) {
	if (v_multiwindow_test_egl_release_record_count
			>= V_MULTIWINDOW_TEST_RELEASE_ORACLE_CAPACITY) {
		v_multiwindow_test_egl_release_record_overflow = 1;
		return;
	}
	VMultiwindowTestEglReleaseRecord *record =
		&v_multiwindow_test_egl_release_records[v_multiwindow_test_egl_release_record_count++];
	record->sequence = v_multiwindow_test_release_oracle_take_sequence();
	record->kind = kind;
	record->identity = identity;
	record->parent_identity = parent_identity;
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static inline void v_multiwindow_test_x11_im_closed(uint64_t identity) {
	v_multiwindow_test_egl_release_record(V_MULTIWINDOW_TEST_X11_IM_CLOSE,
		identity, UINT64_C(0));
}

static inline void v_multiwindow_test_x11_display_closed(uint64_t identity) {
	v_multiwindow_test_egl_release_record(V_MULTIWINDOW_TEST_X11_DISPLAY_CLOSE,
		identity, UINT64_C(0));
}
#endif

static inline EGLBoolean v_multiwindow_test_egl_destroy_surface(
		EGLDisplay display, EGLSurface surface) {
	v_multiwindow_test_egl_release_record(V_MULTIWINDOW_TEST_EGL_SURFACE_RELEASE,
		(uint64_t)(uintptr_t)surface, (uint64_t)(uintptr_t)display);
	return eglDestroySurface(display, surface);
}

static inline EGLBoolean v_multiwindow_test_egl_destroy_context(
		EGLDisplay display, EGLContext context) {
	v_multiwindow_test_egl_release_record(V_MULTIWINDOW_TEST_EGL_CONTEXT_RELEASE,
		(uint64_t)(uintptr_t)context, (uint64_t)(uintptr_t)display);
	return eglDestroyContext(display, context);
}

static inline EGLBoolean v_multiwindow_test_egl_terminate(EGLDisplay display) {
	v_multiwindow_test_egl_release_record(V_MULTIWINDOW_TEST_EGL_DISPLAY_RELEASE,
		(uint64_t)(uintptr_t)display, UINT64_C(0));
	return eglTerminate(display);
}

static inline EGLBoolean v_multiwindow_test_egl_release_thread(void) {
	v_multiwindow_test_egl_release_record(V_MULTIWINDOW_TEST_EGL_THREAD_RELEASE,
		(uint64_t)(uintptr_t)pthread_self(), UINT64_C(0));
	return eglReleaseThread();
}

#define eglDestroySurface v_multiwindow_test_egl_destroy_surface
#define eglDestroyContext v_multiwindow_test_egl_destroy_context
#define eglTerminate v_multiwindow_test_egl_terminate
#define eglReleaseThread v_multiwindow_test_egl_release_thread
#include "../linux_egl_native_helpers.h"
#undef eglReleaseThread
#undef eglTerminate
#undef eglDestroyContext
#undef eglDestroySurface

static inline void v_multiwindow_test_egl_release_oracle_reset(void) {
	for (uint64_t index = 0; index < V_MULTIWINDOW_TEST_RELEASE_ORACLE_CAPACITY; index++) {
		v_multiwindow_test_egl_release_records[index] =
			(VMultiwindowTestEglReleaseRecord){0, 0, 0, 0};
	}
	v_multiwindow_test_egl_release_record_count = UINT64_C(0);
	v_multiwindow_test_egl_release_record_overflow = 0;
}

static inline uint64_t v_multiwindow_test_egl_release_oracle_count(void) {
	return v_multiwindow_test_egl_release_record_count;
}

static inline int v_multiwindow_test_egl_release_oracle_overflow(void) {
	return v_multiwindow_test_egl_release_record_overflow;
}

static inline uint64_t v_multiwindow_test_egl_release_oracle_kind(uint64_t index) {
	return index < v_multiwindow_test_egl_release_record_count
		? v_multiwindow_test_egl_release_records[index].kind : UINT64_C(0);
}

static inline uint64_t v_multiwindow_test_egl_release_oracle_sequence(uint64_t index) {
	return index < v_multiwindow_test_egl_release_record_count
		? v_multiwindow_test_egl_release_records[index].sequence : UINT64_C(0);
}

static inline uint64_t v_multiwindow_test_egl_release_oracle_identity(uint64_t index) {
	return index < v_multiwindow_test_egl_release_record_count
		? v_multiwindow_test_egl_release_records[index].identity : UINT64_C(0);
}

static inline uint64_t v_multiwindow_test_egl_release_oracle_parent(uint64_t index) {
	return index < v_multiwindow_test_egl_release_record_count
		? v_multiwindow_test_egl_release_records[index].parent_identity : UINT64_C(0);
}

#endif
