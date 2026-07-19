#ifndef V_MULTIWINDOW_NATIVE_PRIMITIVE_PROOF_HELPERS_H
#define V_MULTIWINDOW_NATIVE_PRIMITIVE_PROOF_HELPERS_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

static uint64_t v_multiwindow_test_native_primitive_calls;

static inline void v_multiwindow_test_native_primitive_reset(void) {
	v_multiwindow_test_native_primitive_calls = UINT64_C(0);
}

static inline uint64_t v_multiwindow_test_native_primitive_call_count(void) {
	return v_multiwindow_test_native_primitive_calls;
}

static inline void v_multiwindow_test_native_primitive(
		uint64_t valid_mask,
		int64_t return_value,
		uint64_t handle,
		int64_t native_code,
		uint64_t observed_count,
		uint64_t observed_flags,
		VMultiwindowNativePrimitive *out) {
	v_multiwindow_test_native_primitive_calls++;
	if (out == NULL) {
		return;
	}
	memset(out, 0, sizeof(*out));
	out->valid_mask = valid_mask;
	out->return_value = return_value;
	out->handle = handle;
	out->egl_error = native_code;
	out->native_errno = native_code;
	out->wayland_display_error = native_code;
	out->dxgi_removal_reason = native_code;
	out->observed_count = observed_count;
	out->observed_flags = observed_flags;
	out->selected_value = return_value;
	out->object_identity_0 = handle;
	out->object_identity_1 = handle + UINT64_C(1);
	out->object_identity_2 = handle + UINT64_C(2);
}

static inline size_t v_multiwindow_test_native_primitive_abi_size(void) {
	return sizeof(VMultiwindowNativePrimitive);
}

static inline size_t v_multiwindow_test_native_primitive_abi_last_offset(void) {
	return offsetof(VMultiwindowNativePrimitive, object_identity_2);
}

#endif
