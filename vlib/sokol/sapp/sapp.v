@[has_globals]
module sapp

import sokol.gfx
import sokol.memory

// Android needs a global reference to `g_desc`
__global g_desc Desc

// create_desc creates a default `gfx.Desc` configured for use with `sapp`.
pub fn create_desc() gfx.Desc {
	return gfx.Desc{
		environment:     glue_environment()
		image_pool_size: 1000
	}
}

// create_default_pass creates a default `gfx.Pass` compatible with `sapp` and `sokol.gfx.begin_pass/1`.
pub fn create_default_pass(action gfx.PassAction) gfx.Pass {
	return gfx.Pass{
		action:    action
		swapchain: glue_swapchain()
	}
}

// sapp_to_gfx_pixel_format translates sapp_pixel_format to gfx.PixelFormat.
// sokol_app.h uses a compact pixel format enum (sapp_pixel_format) that does NOT
// match the sg_pixel_format numbering used by sokol_gfx.h.
fn sapp_to_gfx_pixel_format(sapp_fmt int) gfx.PixelFormat {
	return match sapp_fmt {
		0 { gfx.PixelFormat._default } // _SAPP_PIXELFORMAT_DEFAULT
		1 { gfx.PixelFormat.none } // SAPP_PIXELFORMAT_NONE
		2 { gfx.PixelFormat.rgba8 } // SAPP_PIXELFORMAT_RGBA8
		3 { gfx.PixelFormat.srgb8a8 } // SAPP_PIXELFORMAT_SRGB8A8
		4 { gfx.PixelFormat.bgra8 } // SAPP_PIXELFORMAT_BGRA8
		5 { gfx.PixelFormat.bgra8 } // SAPP_PIXELFORMAT_SBGRA8 (no exact sg match, use bgra8)
		6 { gfx.PixelFormat.depth } // SAPP_PIXELFORMAT_DEPTH
		7 { gfx.PixelFormat.depth_stencil } // SAPP_PIXELFORMAT_DEPTH_STENCIL
		else { gfx.PixelFormat.none }
	}
}

// glue_environment returns a `gfx.Environment` compatible for use with `sapp` specific `gfx.Pass`es.
// The returned `gfx.Environment` can be used when rendering via `sapp`.
// See also: documentation at the top of thirdparty/sokol/sokol_gfx.h
pub fn glue_environment() gfx.Environment {
	sapp_env := C.sapp_get_environment()
	mut env := gfx.Environment{}
	unsafe { vmemset(&env, 0, int(sizeof(env))) }
	env.defaults.color_format = sapp_to_gfx_pixel_format(sapp_env.defaults.color_format)
	env.defaults.depth_format = sapp_to_gfx_pixel_format(sapp_env.defaults.depth_format)
	env.defaults.sample_count = sapp_env.defaults.sample_count
	$if macos && !darwin_sokol_glcore33 ? {
		env.metal.device = sapp_env.metal.device
	}
	// if windows and dx3d11
	// env.d3d11.device = sapp_env.d3d11.device
	// env.d3d11.device_context = sapp_env.d3d11.device_context
	// if webgpu
	// env.wgpu.device = sapp_env.wgpu.device
	return env
}

// glue_swapchain returns a `gfx.Swapchain` compatible for use with `sapp` specific display/rendering `gfx.Pass`es.
// The returned `gfx.Swapchain` can be used when rendering via `sapp`.
// See also: documentation at the top of thirdparty/sokol/sokol_gfx.h
pub fn glue_swapchain() gfx.Swapchain {
	sapp_sc := C.sapp_get_swapchain()
	mut swapchain := gfx.Swapchain{}
	unsafe { vmemset(&swapchain, 0, int(sizeof(swapchain))) }
	swapchain.width = sapp_sc.width
	swapchain.height = sapp_sc.height
	swapchain.sample_count = sapp_sc.sample_count
	swapchain.color_format = sapp_to_gfx_pixel_format(sapp_sc.color_format)
	swapchain.depth_format = sapp_to_gfx_pixel_format(sapp_sc.depth_format)
	$if macos && !darwin_sokol_glcore33 ? {
		swapchain.metal.current_drawable = sapp_sc.metal.current_drawable
		swapchain.metal.depth_stencil_texture = sapp_sc.metal.depth_stencil_texture
		swapchain.metal.msaa_color_texture = sapp_sc.metal.msaa_color_texture
	}
	// if windows and dx3d11
	// swapchain.d3d11.render_view = sapp_sc.d3d11.render_view
	// swapchain.d3d11.resolve_view = sapp_sc.d3d11.resolve_view
	// swapchain.d3d11.depth_stencil_view = sapp_sc.d3d11.depth_stencil_view
	// if webgpu
	// swapchain.wgpu.render_view = sapp_sc.wgpu.render_view
	// swapchain.wgpu.resolve_view = sapp_sc.wgpu.resolve_view
	// swapchain.wgpu.depth_stencil_view = sapp_sc.wgpu.depth_stencil_view
	$else {
		swapchain.gl.framebuffer = sapp_sc.gl.framebuffer
	}
	return swapchain
}

// returns true after sokol-app has been initialized
@[inline]
pub fn isvalid() bool {
	return C.sapp_isvalid()
}

// returns the current framebuffer width in pixels
@[inline]
pub fn width() int {
	return C.sapp_width()
}

// returns the current framebuffer height in pixels
@[inline]
pub fn height() int {
	return C.sapp_height()
}

// color_format gets default framebuffer color pixel format
@[inline]
pub fn color_format() int {
	return C.sapp_color_format()
}

// depth_format gets default framebuffer depth pixel format
@[inline]
pub fn depth_format() int {
	return C.sapp_depth_format()
}

// sample_count gets default framebuffer sample count
@[inline]
pub fn sample_count() int {
	return C.sapp_sample_count()
}

// returns true when high_dpi was requested and actually running in a high-dpi scenario
@[inline]
pub fn high_dpi() bool {
	return C.sapp_high_dpi()
}

// returns the dpi scaling factor (window pixels to framebuffer pixels)
@[inline]
pub fn dpi_scale() f32 {
	return C.sapp_dpi_scale()
}

// show or hide the mobile device onscreen keyboard
@[inline]
pub fn show_keyboard(visible bool) {
	C.sapp_show_keyboard(visible)
}

// return true if the mobile device onscreen keyboard is currently shown
@[inline]
pub fn keyboard_shown() bool {
	return C.sapp_keyboard_shown()
}

// show or hide the mouse cursor
@[inline]
pub fn show_mouse(visible bool) {
	C.sapp_show_mouse(visible)
}

// set mouse cursor
@[inline]
pub fn set_mouse_cursor(cursor MouseCursor) {
	C.sapp_set_mouse_cursor(cursor)
}

// get current mouse cursor type
@[inline]
pub fn get_mouse_cursor() MouseCursor {
	return C.sapp_get_mouse_cursor()
}

// bind a custom mouse cursor image to a cursor type
@[inline]
pub fn bind_mouse_cursor_image(cursor MouseCursor, desc &ImageDesc) MouseCursor {
	return C.sapp_bind_mouse_cursor_image(cursor, desc)
}

// unbind a custom mouse cursor image
@[inline]
pub fn unbind_mouse_cursor_image(cursor MouseCursor) {
	C.sapp_unbind_mouse_cursor_image(cursor)
}

// show or hide the mouse cursor
@[inline]
pub fn mouse_shown() bool {
	return C.sapp_mouse_shown()
}

// lock_mouse locks or unlocks the mouse cursor, confining it to the window.
@[inline]
pub fn lock_mouse(locked bool) {
	C.sapp_lock_mouse(locked)
}

// mouse_locked returns true if the mouse is currently locked to the window.
@[inline]
pub fn mouse_locked() bool {
	return C.sapp_mouse_locked()
}

// return the userdata pointer optionally provided in sapp_desc
@[inline]
pub fn userdata() voidptr {
	return C.sapp_userdata()
}

// return a copy of the sapp_desc structure
@[inline]
pub fn query_desc() Desc {
	return C.sapp_query_desc()
}

// initiate a "soft quit" (sends SAPP_EVENTTYPE_QUIT_REQUESTED)
@[inline]
pub fn request_quit() {
	C.sapp_request_quit()
}

// cancel a pending quit (when SAPP_EVENTTYPE_QUIT_REQUESTED has been received)
@[inline]
pub fn cancel_quit() {
	C.sapp_cancel_quit()
}

// initiate a "hard quit" (quit application without sending SAPP_EVENTTYPE_QUIT_REQUESTED)
@[inline]
pub fn quit() {
	C.sapp_quit()
}

// call from inside event callback to consume the current event (don't forward to platform)
@[inline]
pub fn consume_event() {
	C.sapp_consume_event()
}

// get the current frame counter (for comparison with sapp_event.frame_count)
@[inline]
pub fn frame_count() u64 {
	return C.sapp_frame_count()
}

// get an averaged/smoothed frame duration in seconds
@[inline]
pub fn frame_duration() f64 {
	return C.sapp_frame_duration()
}

// write string into clipboard
@[inline]
pub fn set_clipboard_string(str &u8) {
	C.sapp_set_clipboard_string(&char(str))
}

// read string from clipboard (usually during SAPP_EVENTTYPE_CLIPBOARD_PASTED)
@[inline]
pub fn get_clipboard_string() &char {
	return &char(C.sapp_get_clipboard_string())
}

// set the window icon (only on Windows and Linux)
@[inline]
pub fn set_icon(icon_desc &IconDesc) {
	C.sapp_set_icon(icon_desc)
}

// special run-function for SOKOL_NO_ENTRY (in standard mode this is an empty stub)
pub fn run(desc &Desc) {
	if desc.allocator.alloc_fn == unsafe { nil } && desc.allocator.free_fn == unsafe { nil } {
		unsafe {
			desc.allocator.alloc_fn = memory.salloc
			desc.allocator.free_fn = memory.sfree
			desc.allocator.user_data = voidptr(0x10005a44)
		}
	}
	if desc.logger.func == unsafe { nil } {
		unsafe {
			desc.logger.func = memory.slog
		}
	}
	g_desc = *desc
	C.sapp_run(desc)
}

// get runtime environment information
@[inline]
pub fn get_environment() Environment {
	return C.sapp_get_environment()
}

// get current frame's swapchain information (call once per frame!)
@[inline]
pub fn get_swapchain() Swapchain {
	return C.sapp_get_swapchain()
}

// HTML5: enable or disable the hardwired "Leave Site?" dialog box
@[inline]
pub fn html5_ask_leave_site(ask bool) {
	C.sapp_html5_ask_leave_site(ask)
}

// HTML5: get byte size of a dropped file
@[inline]
pub fn html5_get_dropped_file_size(index int) u32 {
	return C.sapp_html5_get_dropped_file_size(index)
}

// EGL: get EGLDisplay object
@[inline]
pub fn egl_get_display() voidptr {
	return C.sapp_egl_get_display()
}

// EGL: get EGLContext object
@[inline]
pub fn egl_get_context() voidptr {
	return C.sapp_egl_get_context()
}

// macOS: get ARC-bridged pointer to macOS NSWindow
@[inline]
pub fn macos_get_window() voidptr {
	return voidptr(C.sapp_macos_get_window())
}

// iOS: get ARC-bridged pointer to iOS UIWindow
@[inline]
pub fn ios_get_window() voidptr {
	return voidptr(C.sapp_ios_get_window())
}

// D3D11: get pointer to IDXGISwapChain object
@[inline]
pub fn d3d11_get_swap_chain() voidptr {
	return voidptr(C.sapp_d3d11_get_swap_chain())
}

// Win32: get the HWND window handle
@[inline]
pub fn win32_get_hwnd() voidptr {
	return voidptr(C.sapp_win32_get_hwnd())
}

// GL: get major version
@[inline]
pub fn gl_get_major_version() int {
	return C.sapp_gl_get_major_version()
}

// GL: get minor version
@[inline]
pub fn gl_get_minor_version() int {
	return C.sapp_gl_get_minor_version()
}

// GL: return true if the context is GLES
@[inline]
pub fn gl_is_gles() bool {
	return C.sapp_gl_is_gles()
}

// GL: get framebuffer object
@[inline]
pub fn gl_get_framebuffer() u32 {
	return C.sapp_gl_get_framebuffer()
}

// X11: get Window
@[inline]
pub fn x11_get_window() voidptr {
	return voidptr(C.sapp_x11_get_window())
}

// X11: get Display
@[inline]
pub fn x11_get_display() voidptr {
	return voidptr(C.sapp_x11_get_display())
}

// Toggle full screen
@[inline]
pub fn toggle_fullscreen() {
	C.sapp_toggle_fullscreen()
}

// Check if full screen rendering
@[inline]
pub fn is_fullscreen() bool {
	return C.sapp_is_fullscreen()
}

// get_num_dropped_files returns the number of files dropped onto the window.
@[inline]
pub fn get_num_dropped_files() int {
	return C.sapp_get_num_dropped_files()
}

// get_dropped_file_path returns the path of a dropped file by index.
@[inline]
pub fn get_dropped_file_path(index int) string {
	unsafe {
		return cstring_to_vstring(C.sapp_get_dropped_file_path(index))
	}
}

// Android: get native activity handle
@[inline]
pub fn android_get_native_activity() voidptr {
	return voidptr(C.sapp_android_get_native_activity())
}
