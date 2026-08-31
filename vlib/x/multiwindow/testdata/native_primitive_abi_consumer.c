#include <stddef.h>

#ifdef _MSC_VER
#define V_MULTIWINDOW_TEST_ABI_ASSERT(name, condition, message) \
	typedef char V_MULTIWINDOW_TEST_ABI_ASSERT_##name[(condition) ? 1 : -1]
#else
#define V_MULTIWINDOW_TEST_ABI_ASSERT(name, condition, message) _Static_assert(condition, message)
#endif

V_MULTIWINDOW_TEST_ABI_ASSERT(primitive_size,
	sizeof(VMultiwindowNativePrimitive) == (13 * sizeof(uint64_t)),
	"portable native primitive ABI layout changed");
V_MULTIWINDOW_TEST_ABI_ASSERT(valid_mask_offset,
	offsetof(VMultiwindowNativePrimitive, valid_mask) == 0,
	"native primitive validity mask must remain first");
V_MULTIWINDOW_TEST_ABI_ASSERT(return_value_offset,
	offsetof(VMultiwindowNativePrimitive, return_value) == 8,
	"native primitive return offset changed");
V_MULTIWINDOW_TEST_ABI_ASSERT(handle_offset,
	offsetof(VMultiwindowNativePrimitive, handle) == 16,
	"native primitive handle offset changed");
V_MULTIWINDOW_TEST_ABI_ASSERT(egl_error_offset,
	offsetof(VMultiwindowNativePrimitive, egl_error) == 24,
	"native primitive EGL offset changed");
V_MULTIWINDOW_TEST_ABI_ASSERT(native_errno_offset,
	offsetof(VMultiwindowNativePrimitive, native_errno) == 32,
	"native primitive errno offset changed");
V_MULTIWINDOW_TEST_ABI_ASSERT(wayland_display_error_offset,
	offsetof(VMultiwindowNativePrimitive, wayland_display_error) == 40,
	"native primitive Wayland offset changed");
V_MULTIWINDOW_TEST_ABI_ASSERT(dxgi_removal_reason_offset,
	offsetof(VMultiwindowNativePrimitive, dxgi_removal_reason) == 48,
	"native primitive DXGI offset changed");
V_MULTIWINDOW_TEST_ABI_ASSERT(observed_count_offset,
	offsetof(VMultiwindowNativePrimitive, observed_count) == 56,
	"native primitive count offset changed");
V_MULTIWINDOW_TEST_ABI_ASSERT(observed_flags_offset,
	offsetof(VMultiwindowNativePrimitive, observed_flags) == 64,
	"native primitive flags offset changed");
V_MULTIWINDOW_TEST_ABI_ASSERT(selected_value_offset,
	offsetof(VMultiwindowNativePrimitive, selected_value) == 72,
	"native primitive selected-value offset changed");
V_MULTIWINDOW_TEST_ABI_ASSERT(object_identity_0_offset,
	offsetof(VMultiwindowNativePrimitive, object_identity_0) == 80,
	"native primitive first identity offset changed");
V_MULTIWINDOW_TEST_ABI_ASSERT(object_identity_1_offset,
	offsetof(VMultiwindowNativePrimitive, object_identity_1) == 88,
	"native primitive second identity offset changed");
V_MULTIWINDOW_TEST_ABI_ASSERT(object_identity_2_offset,
	offsetof(VMultiwindowNativePrimitive, object_identity_2) == 96,
	"native primitive third identity offset changed");

#undef V_MULTIWINDOW_TEST_ABI_ASSERT

size_t v_multiwindow_test_common_native_primitive_size(void) {
	return sizeof(VMultiwindowNativePrimitive);
}

uint64_t v_multiwindow_test_common_native_primitive_mask(void) {
	return V_MULTIWINDOW_NATIVE_VALID_RETURN_VALUE |
		V_MULTIWINDOW_NATIVE_VALID_HANDLE |
		V_MULTIWINDOW_NATIVE_VALID_EGL_ERROR |
		V_MULTIWINDOW_NATIVE_VALID_ERRNO |
		V_MULTIWINDOW_NATIVE_VALID_WAYLAND_DISPLAY_ERROR |
		V_MULTIWINDOW_NATIVE_VALID_DXGI_REMOVAL_REASON |
		V_MULTIWINDOW_NATIVE_VALID_OBSERVED_COUNT |
		V_MULTIWINDOW_NATIVE_VALID_OBSERVED_FLAGS |
		V_MULTIWINDOW_NATIVE_VALID_SELECTED_VALUE |
		V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_0 |
		V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_1 |
		V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_2;
}
