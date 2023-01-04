[has_globals]
module sapp

import sokol.gfx

pub const used_import = gfx.used_import

// Android needs a global reference to `g_desc`
__global g_desc C.sapp_desc

pub fn create_desc() gfx.Desc {
	metal_desc := gfx.MetalContextDesc{
		device: metal_get_device()
		renderpass_descriptor_cb: metal_get_renderpass_descriptor
		drawable_cb: metal_get_drawable
	}
	d3d11_desc := gfx.D3D11ContextDesc{
		device: d3d11_get_device()
		device_context: d3d11_get_device_context()
		render_target_view_cb: d3d11_get_render_target_view
		depth_stencil_view_cb: d3d11_get_depth_stencil_view
	}

	return gfx.Desc{
		context: gfx.ContextDesc{
			metal: metal_desc
			d3d11: d3d11_desc
			color_format: .bgra8
		}
		image_pool_size: 1000
	}
}

// returns true after sokol-app has been initialized
[inline]
pub fn isvalid() bool {
	return C.sapp_isvalid()
}

// returns the current framebuffer width in pixels
[inline]
pub fn width() int {
	return C.sapp_width()
}

// returns the current framebuffer height in pixels
[inline]
pub fn height() int {
	return C.sapp_height()
}

// color_format gets default framebuffer color pixel format
[inline]
pub fn color_format() int {
	return C.sapp_color_format()
}

// depth_format gets default framebuffer depth pixel format
[inline]
pub fn depth_format() int {
	return C.sapp_depth_format()
}

// sample_count gets default framebuffer sample count
[inline]
pub fn sample_count() int {
	return C.sapp_sample_count()
}

// returns true when high_dpi was requested and actually running in a high-dpi scenario
[inline]
pub fn high_dpi() bool {
	return C.sapp_high_dpi()
}

// returns the dpi scaling factor (window pixels to framebuffer pixels)
[inline]
pub fn dpi_scale() f32 {
	return C.sapp_dpi_scale()
}

// show or hide the mobile device onscreen keyboard
[inline]
pub fn show_keyboard(visible bool) {
	C.sapp_show_keyboard(visible)
}

// return true if the mobile device onscreen keyboard is currently shown
[inline]
pub fn keyboard_shown() bool {
	return C.sapp_keyboard_shown()
}

// show or hide the mouse cursor
[inline]
pub fn show_mouse(visible bool) {
	C.sapp_show_mouse(visible)
}

// set mouse cursor
[inline]
pub fn set_mouse_cursor(cursor MouseCursor) {
	C.sapp_set_mouse_cursor(cursor)
}

// show or hide the mouse cursor
[inline]
pub fn mouse_shown() bool {
	return C.sapp_mouse_shown()
}

[inline]
pub fn lock_mouse(locked bool) {
	C.sapp_lock_mouse(locked)
}

[inline]
pub fn mouse_locked() bool {
	return C.sapp_mouse_locked()
}

// return the userdata pointer optionally provided in sapp_desc
[inline]
pub fn userdata() voidptr {
	return C.sapp_userdata()
}

// return a copy of the sapp_desc structure
[inline]
pub fn query_desc() Desc {
	return C.sapp_query_desc()
}

// initiate a "soft quit" (sends SAPP_EVENTTYPE_QUIT_REQUESTED)
[inline]
pub fn request_quit() {
	C.sapp_request_quit()
}

// cancel a pending quit (when SAPP_EVENTTYPE_QUIT_REQUESTED has been received)
[inline]
pub fn cancel_quit() {
	C.sapp_cancel_quit()
}

// intiate a "hard quit" (quit application without sending SAPP_EVENTTYPE_QUIT_REQUSTED)
[inline]
pub fn quit() {
	C.sapp_quit()
}

// call from inside event callback to consume the current event (don't forward to platform)
[inline]
pub fn consume_event() {
	C.sapp_consume_event()
}

// get the current frame counter (for comparison with sapp_event.frame_count)
[inline]
pub fn frame_count() u64 {
	return C.sapp_frame_count()
}

// get an averaged/smoothed frame duration in seconds
[inline]
pub fn frame_duration() f64 {
	return C.sapp_frame_duration()
}

// write string into clipboard
[inline]
pub fn set_clipboard_string(str &u8) {
	C.sapp_set_clipboard_string(str)
}

// read string from clipboard (usually during SAPP_EVENTTYPE_CLIPBOARD_PASTED)
[inline]
pub fn get_clipboard_string() &char {
	return &char(C.sapp_get_clipboard_string())
}

// special run-function for SOKOL_NO_ENTRY (in standard mode this is an empty stub)
[inline]
pub fn run(desc &Desc) {
	g_desc = *desc
	C.sapp_run(desc)
}

// GL: return true when GLES2 fallback is active (to detect fallback from GLES3)
[inline]
pub fn gles2() bool {
	return C.sapp_gles2()
}

// HTML5: enable or disable the hardwired "Leave Site?" dialog box
[inline]
pub fn html5_ask_leave_site(ask bool) {
	C.sapp_html5_ask_leave_site(ask)
}

// Metal: get ARC-bridged pointer to Metal device object
[inline]
pub fn metal_get_device() voidptr {
	return voidptr(C.sapp_metal_get_device())
}

// Metal: get ARC-bridged pointer to this frame's renderpass descriptor
[inline]
pub fn metal_get_renderpass_descriptor() voidptr {
	return voidptr(C.sapp_metal_get_renderpass_descriptor())
}

// Metal: get ARC-bridged pointer to current drawable
[inline]
pub fn metal_get_drawable() voidptr {
	return voidptr(C.sapp_metal_get_drawable())
}

// macOS: get ARC-bridged pointer to macOS NSWindow
[inline]
pub fn macos_get_window() voidptr {
	return voidptr(C.sapp_macos_get_window())
}

// iOS: get ARC-bridged pointer to iOS UIWindow
[inline]
pub fn ios_get_window() voidptr {
	return voidptr(C.sapp_ios_get_window())
}

// D3D11: get pointer to ID3D11Device object
[inline]
pub fn d3d11_get_device() voidptr {
	return voidptr(C.sapp_d3d11_get_device())
}

// D3D11: get pointer to ID3D11DeviceContext object
[inline]
pub fn d3d11_get_device_context() voidptr {
	return voidptr(C.sapp_d3d11_get_device_context())
}

// D3D11: get pointer to ID3D11RenderTargetView object
[inline]
pub fn d3d11_get_render_target_view() voidptr {
	return voidptr(C.sapp_d3d11_get_render_target_view())
}

// D3D11: get pointer to ID3D11DepthStencilView
[inline]
pub fn d3d11_get_depth_stencil_view() voidptr {
	return voidptr(C.sapp_d3d11_get_depth_stencil_view())
}

// Win32: get the HWND window handle
[inline]
pub fn win32_get_hwnd() voidptr {
	return voidptr(C.sapp_win32_get_hwnd())
}

// Android: get native activity handle
[inline]
pub fn android_get_native_activity() voidptr {
	return voidptr(C.sapp_android_get_native_activity())
}

// Toggle full screen
[inline]
pub fn toggle_fullscreen() {
	C.sapp_toggle_fullscreen()
}

// Check if full screen rendering
[inline]
pub fn is_fullscreen() bool {
	return C.sapp_is_fullscreen()
}

[inline]
pub fn get_num_dropped_files() int {
	return C.sapp_get_num_dropped_files()
}

[inline]
pub fn get_dropped_file_path(index int) string {
	unsafe {
		return cstring_to_vstring(C.sapp_get_dropped_file_path(index))
	}
}
