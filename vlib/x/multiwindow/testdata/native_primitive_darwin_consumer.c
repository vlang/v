#include <stddef.h>

_Static_assert(sizeof(VMultiwindowNativePrimitive) == (13 * sizeof(uint64_t)),
	"Darwin primitive ABI layout changed");
_Static_assert(offsetof(VMultiwindowNativePrimitive, object_identity_2) == 96,
	"Darwin primitive ABI identity offset changed");

size_t v_multiwindow_test_darwin_primitive_size(void) {
	return sizeof(VMultiwindowNativePrimitive);
}

uint64_t v_multiwindow_test_darwin_primitive_mask(void) {
	return V_MULTIWINDOW_NATIVE_VALID_RETURN_VALUE |
		V_MULTIWINDOW_NATIVE_VALID_HANDLE |
		V_MULTIWINDOW_NATIVE_VALID_EGL_ERROR |
		V_MULTIWINDOW_NATIVE_VALID_ERRNO |
		V_MULTIWINDOW_NATIVE_VALID_WAYLAND_DISPLAY_ERROR |
		V_MULTIWINDOW_NATIVE_VALID_DXGI_REMOVAL_REASON;
}
