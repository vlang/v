#ifndef V_MULTIWINDOW_WIN32_READBACK_D3D11_DIAGNOSTICS_H
#define V_MULTIWINDOW_WIN32_READBACK_D3D11_DIAGNOSTICS_H

#include <stdint.h>

static void v_multiwindow_win32_readback_record_device_removed_reason(
		int32_t primary_hresult, int32_t *device_removed_reason,
		int32_t observed_reason) {
	(void)primary_hresult;
	if (device_removed_reason != NULL) {
		*device_removed_reason = observed_reason;
	}
}

#endif
