module multiwindow

$if gg_multiwindow ? || x_multiwindow_render ? {
	$if windows && sokol_d3d11 ? {
		#flag windows -ld3d11
		#flag windows -ldxgi
		#include <windows.h>
		#insert "@VMODROOT/vlib/x/multiwindow/win32_d3d11_backend_helpers.h"
	}

	$if windows && sokol_d3d11 ? {
		fn C.v_multiwindow_win32_safe_release(obj &&voidptr)
		fn C.v_multiwindow_win32_d3d11_create_device(out_device &voidptr, out_context &voidptr, out_factory &voidptr) int
		fn C.v_multiwindow_win32_d3d11_create_swapchain(factory voidptr, device voidptr, hwnd voidptr, width int, height int, out_swapchain &voidptr) int
		fn C.v_multiwindow_win32_d3d11_release_views(render_view &voidptr, depth_texture &voidptr, depth_view &voidptr)
		fn C.v_multiwindow_win32_d3d11_create_views(device voidptr, swapchain voidptr, width int, height int, out_render_view &voidptr, out_depth_texture &voidptr, out_depth_view &voidptr) int
		fn C.v_multiwindow_win32_d3d11_resize_swapchain(device voidptr, context voidptr, swapchain voidptr, width int, height int, render_view &voidptr, depth_texture &voidptr, depth_view &voidptr) int
		fn C.v_multiwindow_win32_d3d11_present(swapchain voidptr) int
	}
}
