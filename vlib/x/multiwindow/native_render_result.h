#ifndef V_MULTIWINDOW_NATIVE_RENDER_RESULT_H
#define V_MULTIWINDOW_NATIVE_RENDER_RESULT_H

#include <stdint.h>

#define V_MULTIWINDOW_NATIVE_VALID_RETURN_VALUE UINT64_C(1)
#define V_MULTIWINDOW_NATIVE_VALID_HANDLE UINT64_C(2)
#define V_MULTIWINDOW_NATIVE_VALID_EGL_ERROR UINT64_C(4)
#define V_MULTIWINDOW_NATIVE_VALID_ERRNO UINT64_C(8)
#define V_MULTIWINDOW_NATIVE_VALID_WAYLAND_DISPLAY_ERROR UINT64_C(16)
#define V_MULTIWINDOW_NATIVE_VALID_DXGI_REMOVAL_REASON UINT64_C(32)
#define V_MULTIWINDOW_NATIVE_VALID_OBSERVED_COUNT UINT64_C(64)
#define V_MULTIWINDOW_NATIVE_VALID_OBSERVED_FLAGS UINT64_C(128)
#define V_MULTIWINDOW_NATIVE_VALID_SELECTED_VALUE UINT64_C(256)
#define V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_0 UINT64_C(512)
#define V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_1 UINT64_C(1024)
#define V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_2 UINT64_C(2048)

typedef struct VMultiwindowNativePrimitive {
	uint64_t valid_mask;
	int64_t return_value;
	uint64_t handle;
	int64_t egl_error;
	int64_t native_errno;
	int64_t wayland_display_error;
	int64_t dxgi_removal_reason;
	uint64_t observed_count;
	uint64_t observed_flags;
	int64_t selected_value;
	uint64_t object_identity_0;
	uint64_t object_identity_1;
	uint64_t object_identity_2;
} VMultiwindowNativePrimitive;

#endif
