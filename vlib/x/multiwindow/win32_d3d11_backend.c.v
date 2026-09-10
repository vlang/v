module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	$if windows && sokol_d3d11 ? {
		#flag windows -ld3d11
		#flag windows -ldxgi
		#include <windows.h>
		#insert "@VMODROOT/vlib/x/multiwindow/win32_d3d11_backend_helpers.h"
	}

	$if windows && sokol_d3d11 ? {
		fn C.v_multiwindow_win32_release(identity u64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_d3d11_get_removed_reason(device u64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_d3d11_create_device_attempt(driver i64, feature_list i64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_d3d11_query_dxgi_device(device u64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_dxgi_get_adapter(device u64, dxgi_device u64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_dxgi_get_factory(device u64, adapter u64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_dxgi_create_swapchain(factory u64, device u64, hwnd u64, width i64, height i64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_dxgi_make_window_association(factory u64, device u64, hwnd u64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_dxgi_get_backbuffer(device u64, swapchain u64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_d3d11_create_render_view(device u64, resource u64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_d3d11_create_texture(device u64, width i64, height i64, depth i64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_d3d11_create_depth_view(device u64, texture u64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_d3d11_clear_state(context u64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_dxgi_resize_buffers(device u64, swapchain u64, width i64, height i64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_dxgi_present(device u64, swapchain u64, result &C.VMultiwindowNativePrimitive)
		fn C.v_multiwindow_win32_d3d11_device_status(device u64, result &C.VMultiwindowNativePrimitive)
	}
}
