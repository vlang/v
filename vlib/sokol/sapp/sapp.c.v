@[has_globals]
module sapp

import sokol.gfx
import sokol.memory

// Android needs a global reference to `g_desc`
__global g_desc C.sapp_desc

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

// sapp_to_gfx_pixelformat converts an sapp_pixel_format int to a gfx.PixelFormat.
// The sapp and gfx pixel format enums have different integer values, so a direct
// cast is incorrect.
fn sapp_to_gfx_pixelformat(sapp_fmt int) gfx.PixelFormat {
	// sapp_pixel_format: _DEFAULT=0, NONE=1, RGBA8=2, SRGB8A8=3, BGRA8=4, SBGRA8=5, DEPTH=6, DEPTH_STENCIL=7
	return match sapp_fmt {
		1 { gfx.PixelFormat.none }
		2 { gfx.PixelFormat.rgba8 }
		3 { gfx.PixelFormat.srgb8a8 }
		4 { gfx.PixelFormat.bgra8 }
		5 { gfx.PixelFormat.bgra8 } // sbgra8 has no gfx equivalent, use bgra8
		6 { gfx.PixelFormat.depth }
		7 { gfx.PixelFormat.depth_stencil }
		else { gfx.PixelFormat.none }
	}
}

// glue_environment returns a `gfx.Environment` compatible for use with `sapp` specific `gfx.Pass`es.
// The retuned `gfx.Environment` can be used when rendering via `sapp`.
// See also: documentation at the top of thirdparty/sokol/sokol_gfx.h
pub fn glue_environment() gfx.Environment {
	sapp_env := C.sapp_get_environment()
	mut env := gfx.Environment{}
	unsafe { vmemset(&env, 0, int(sizeof(env))) }
	env.defaults.color_format = sapp_to_gfx_pixelformat(sapp_env.defaults.color_format)
	env.defaults.depth_format = sapp_to_gfx_pixelformat(sapp_env.defaults.depth_format)
	env.defaults.sample_count = sapp_env.defaults.sample_count
	$if macos && !darwin_sokol_glcore33 ? {
		env.metal.device = sapp_env.metal.device
	}
	return env
}

// glue_swapchain returns a `gfx.Swapchain` compatible for use with `sapp` specific display/rendering `gfx.Pass`es.
// The retuned `gfx.Swapchain` can be used when rendering via `sapp`.
// See also: documentation at the top of thirdparty/sokol/sokol_gfx.h
pub fn glue_swapchain() gfx.Swapchain {
	sapp_sc := C.sapp_get_swapchain()
	mut swapchain := gfx.Swapchain{}
	unsafe { vmemset(&swapchain, 0, int(sizeof(swapchain))) }
	swapchain.width = sapp_sc.width
	swapchain.height = sapp_sc.height
	swapchain.sample_count = sapp_sc.sample_count
	swapchain.color_format = sapp_to_gfx_pixelformat(sapp_sc.color_format)
	swapchain.depth_format = sapp_to_gfx_pixelformat(sapp_sc.depth_format)
	$if macos && !darwin_sokol_glcore33 ? {
		swapchain.metal.current_drawable = sapp_sc.metal.current_drawable
		swapchain.metal.depth_stencil_texture = sapp_sc.metal.depth_stencil_texture
		swapchain.metal.msaa_color_texture = sapp_sc.metal.msaa_color_texture
	} $else {
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

// show or hide the mouse cursor
@[inline]
pub fn mouse_shown() bool {
	return C.sapp_mouse_shown()
}

@[inline]
pub fn lock_mouse(locked bool) {
	C.sapp_lock_mouse(locked)
}

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

// HTML5: enable or disable the hardwired "Leave Site?" dialog box
@[inline]
pub fn html5_ask_leave_site(ask bool) {
	C.sapp_html5_ask_leave_site(ask)
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

// Win32: get the HWND window handle
@[inline]
pub fn win32_get_hwnd() voidptr {
	return voidptr(C.sapp_win32_get_hwnd())
}

// GL: get framebuffer object
@[inline]
pub fn gl_get_framebuffer() u32 {
	return C.sapp_gl_get_framebuffer()
}

// Android: get native activity handle
@[inline]
pub fn android_get_native_activity() voidptr {
	return voidptr(C.sapp_android_get_native_activity())
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

@[inline]
pub fn get_num_dropped_files() int {
	return C.sapp_get_num_dropped_files()
}

@[inline]
pub fn get_dropped_file_path(index int) string {
	unsafe {
		return cstring_to_vstring(C.sapp_get_dropped_file_path(index))
	}
}
