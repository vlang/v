#ifndef V_MULTIWINDOW_NATIVE_WAYLAND_RELEASE_ORACLE_HELPERS_H
#define V_MULTIWINDOW_NATIVE_WAYLAND_RELEASE_ORACLE_HELPERS_H

#include <stdint.h>
#include <sys/types.h>

#if defined(__linux__) && defined(__GLIBC__) && !defined(__USE_GNU) \
	&& (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 30))
extern pid_t gettid(void);
#endif

#include <wayland-client.h>
#include <wayland-egl.h>
#include "native_release_oracle_common.h"

#define V_MULTIWINDOW_TEST_WAYLAND_RELEASE_ORACLE_CAPACITY 256

enum VMultiwindowTestWaylandReleaseKind {
	V_MULTIWINDOW_TEST_WAYLAND_EGL_WINDOW_RELEASE = 1,
	V_MULTIWINDOW_TEST_WAYLAND_CALLBACK_DESTROY = 2,
	V_MULTIWINDOW_TEST_WAYLAND_CALLBACK_COMPLETION = 3,
	V_MULTIWINDOW_TEST_WAYLAND_LOCAL_PROXY_DESTROY = 4,
	V_MULTIWINDOW_TEST_WAYLAND_DISPLAY_DISCONNECT = 5,
	V_MULTIWINDOW_TEST_WAYLAND_SURFACE_RELEASE = 6
};

typedef struct VMultiwindowTestWaylandReleaseRecord {
	uint64_t sequence;
	uint64_t kind;
	uint64_t identity;
	uint64_t mode;
} VMultiwindowTestWaylandReleaseRecord;

static VMultiwindowTestWaylandReleaseRecord
	v_multiwindow_test_wayland_release_records[V_MULTIWINDOW_TEST_WAYLAND_RELEASE_ORACLE_CAPACITY];
static uint64_t v_multiwindow_test_wayland_release_record_count;
static int v_multiwindow_test_wayland_release_record_overflow;

static inline void v_multiwindow_test_wayland_release_record_with_mode(
		uint64_t kind, uint64_t identity, uint64_t mode) {
	if (v_multiwindow_test_wayland_release_record_count
			>= V_MULTIWINDOW_TEST_WAYLAND_RELEASE_ORACLE_CAPACITY) {
		v_multiwindow_test_wayland_release_record_overflow = 1;
		return;
	}
	VMultiwindowTestWaylandReleaseRecord *record =
		&v_multiwindow_test_wayland_release_records[
			v_multiwindow_test_wayland_release_record_count++];
	record->sequence = v_multiwindow_test_release_oracle_take_sequence();
	record->kind = kind;
	record->identity = identity;
	record->mode = mode;
}

static inline void v_multiwindow_test_wayland_release_record(
		uint64_t kind, uint64_t identity) {
	v_multiwindow_test_wayland_release_record_with_mode(kind, identity, UINT64_C(0));
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static inline void v_multiwindow_test_wayland_local_proxy_destroyed(
		uint64_t identity) {
	v_multiwindow_test_wayland_release_record(
		V_MULTIWINDOW_TEST_WAYLAND_LOCAL_PROXY_DESTROY, identity);
}

static inline void v_multiwindow_test_wayland_display_disconnected(
		uint64_t identity) {
	v_multiwindow_test_wayland_release_record(
		V_MULTIWINDOW_TEST_WAYLAND_DISPLAY_DISCONNECT, identity);
}
#endif

static inline void v_multiwindow_test_wayland_callback_destroy(
		struct wl_callback *callback) {
	v_multiwindow_test_wayland_release_record(
		V_MULTIWINDOW_TEST_WAYLAND_CALLBACK_DESTROY,
		(uint64_t)(uintptr_t)callback);
	wl_callback_destroy(callback);
}

static inline void v_multiwindow_test_wayland_egl_window_destroy(
		struct wl_egl_window *window) {
	v_multiwindow_test_wayland_release_record(
		V_MULTIWINDOW_TEST_WAYLAND_EGL_WINDOW_RELEASE,
		(uint64_t)(uintptr_t)window);
	wl_egl_window_destroy(window);
}

static inline void v_multiwindow_test_wayland_anchor_surface_destroyed(
		uint64_t identity, uint64_t mode) {
	v_multiwindow_test_wayland_release_record_with_mode(
		V_MULTIWINDOW_TEST_WAYLAND_SURFACE_RELEASE, identity, mode);
}

static void v_multiwindow_test_wayland_frame_destroyed_observer(
	void *data, void *callback);

#define wl_callback_destroy v_multiwindow_test_wayland_callback_destroy
#define wl_egl_window_destroy v_multiwindow_test_wayland_egl_window_destroy
#define v_multiwindow_wayland_frame_destroyed \
	v_multiwindow_test_wayland_frame_destroyed_observer
#define voidptr void *
#define u32 uint32_t
#include "../wayland_backend_helpers.h"
#undef u32
#undef voidptr
#undef v_multiwindow_wayland_frame_destroyed
#undef wl_egl_window_destroy
#undef wl_callback_destroy

void v_multiwindow_wayland_frame_destroyed(void *data, void *callback);

static void v_multiwindow_test_wayland_frame_destroyed_observer(
		void *data, void *callback) {
	v_multiwindow_test_wayland_release_record(
		V_MULTIWINDOW_TEST_WAYLAND_CALLBACK_COMPLETION,
		(uint64_t)(uintptr_t)callback);
	v_multiwindow_wayland_frame_destroyed(data, callback);
}

static inline void v_multiwindow_test_wayland_invoke_frame_done_trampoline(
		void *data, void *callback, uint32_t time) {
	v_multiwindow_wayland_frame_done_trampoline(data,
		(struct wl_callback *)callback, time);
}

static inline void v_multiwindow_test_wayland_release_oracle_reset(void) {
	for (uint64_t index = 0;
			index < V_MULTIWINDOW_TEST_WAYLAND_RELEASE_ORACLE_CAPACITY; index++) {
		v_multiwindow_test_wayland_release_records[index] =
			(VMultiwindowTestWaylandReleaseRecord){0, 0, 0, 0};
	}
	v_multiwindow_test_wayland_release_record_count = UINT64_C(0);
	v_multiwindow_test_wayland_release_record_overflow = 0;
}

static inline uint64_t v_multiwindow_test_wayland_release_oracle_count(void) {
	return v_multiwindow_test_wayland_release_record_count;
}

static inline int v_multiwindow_test_wayland_release_oracle_overflow(void) {
	return v_multiwindow_test_wayland_release_record_overflow;
}

static inline uint64_t v_multiwindow_test_wayland_release_oracle_kind(uint64_t index) {
	return index < v_multiwindow_test_wayland_release_record_count
		? v_multiwindow_test_wayland_release_records[index].kind : UINT64_C(0);
}

static inline uint64_t v_multiwindow_test_wayland_release_oracle_sequence(uint64_t index) {
	return index < v_multiwindow_test_wayland_release_record_count
		? v_multiwindow_test_wayland_release_records[index].sequence : UINT64_C(0);
}

static inline uint64_t v_multiwindow_test_wayland_release_oracle_identity(uint64_t index) {
	return index < v_multiwindow_test_wayland_release_record_count
		? v_multiwindow_test_wayland_release_records[index].identity : UINT64_C(0);
}

static inline uint64_t v_multiwindow_test_wayland_release_oracle_mode(uint64_t index) {
	return index < v_multiwindow_test_wayland_release_record_count
		? v_multiwindow_test_wayland_release_records[index].mode : UINT64_C(0);
}

#endif
