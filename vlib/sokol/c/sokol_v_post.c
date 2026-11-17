// This implementation belongs to the native owner in a cached build.
#include "sokol_v.post.h"

#if defined(SOKOL_APP_INCLUDED) && defined(SOKOL_APP_IMPL)
SOKOL_APP_API_DECL void* v_sapp_win32_get_hwnd(void) {
	return (void*) sapp_win32_get_hwnd();
}
#endif
