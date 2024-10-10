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

// glue_environment returns a `gfx.Environment` compatible for use with `sapp` specific `gfx.Pass`es.
// The retuned `gfx.Environment` can be used when rendering via `sapp`.
// See also: documentation at the top of thirdparty/sokol/sokol_gfx.h
pub fn glue_environment() gfx.Environment {
	mut env := gfx.Environment{}
	unsafe { vmemset(&env, 0, int(sizeof(env))) }
	env.defaults.color_format = gfx.PixelFormat.from(color_format()) or { gfx.PixelFormat.none }
	env.defaults.depth_format = gfx.PixelFormat.from(depth_format()) or { gfx.PixelFormat.none }
	env.defaults.sample_count = sample_count()
	$if macos && !darwin_sokol_glcore33 ? {
		env.metal.device = metal_get_device()
	}
	// if windows and dx3d11
	// env.d3d11.device = d3d11_get_device()
	// env.d3d11.device_context = d3d11_get_device_context()
	// if webgpu
	// env.wgpu.device = wgpu_get_device()
	return env
}

// glue_swapchain returns a `gfx.Swapchain` compatible for use with `sapp` specific display/rendering `gfx.Pass`es.
// The retuned `gfx.Swapchain` can be used when rendering via `sapp`.
// See also: documentation at the top of thirdparty/sokol/sokol_gfx.h
pub fn glue_swapchain() gfx.Swapchain {
	mut swapchain := gfx.Swapchain{}
	unsafe { vmemset(&swapchain, 0, int(sizeof(swapchain))) }
	swapchain.width = width()
	swapchain.height = height()
	swapchain.sample_count = sample_count()
	swapchain.color_format = gfx.PixelFormat.from(color_format()) or { gfx.PixelFormat.none }
	swapchain.depth_format = gfx.PixelFormat.from(depth_format()) or { gfx.PixelFormat.none }
	$if macos && !darwin_sokol_glcore33 ? {
		swapchain.metal.current_drawable = metal_get_current_drawable()
		swapchain.metal.depth_stencil_texture = metal_get_depth_stencil_texture()
		swapchain.metal.msaa_color_texture = metal_get_msaa_color_texture()
	}
	// if windows and dx3d11
	// swapchain.d3d11.render_view = d3d11_get_render_view()
	// swapchain.d3d11.resolve_view = d3d11_get_resolve_view()
	// swapchain.d3d11.depth_stencil_view = d3d11_get_depth_stencil_view()
	// if webgpu
	// swapchain.wgpu.render_view = wgpu_get_render_view()
	// swapchain.wgpu.resolve_view = wgpu_get_resolve_view()
	// swapchain.wgpu.depth_stencil_view = wgpu_get_depth_stencil_view()
	$else {
		swapchain.gl.framebuffer = gl_get_framebuffer()
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

// Metal: get ARC-bridged pointer to Metal device object
@[inline]
pub fn metal_get_device() voidptr {
	return voidptr(C.sapp_metal_get_device())
}

// Metal: get ARC-bridged pointer to current drawable
pub fn metal_get_current_drawable() voidptr {
	return voidptr(C.sapp_metal_get_current_drawable())
}

// Metal: get bridged pointer to MTKView's depth-stencil texture of type MTLTexture
pub fn metal_get_depth_stencil_texture() voidptr {
	return voidptr(C.sapp_metal_get_depth_stencil_texture())
}

// Metal: get bridged pointer to MTKView's msaa-color-texture of type MTLTexture (may be null)
pub fn metal_get_msaa_color_texture() voidptr {
	return voidptr(C.sapp_metal_get_msaa_color_texture())
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

// D3D11: get pointer to ID3D11Device object
@[inline]
pub fn d3d11_get_device() voidptr {
	return voidptr(C.sapp_d3d11_get_device())
}

// D3D11: get pointer to ID3D11DeviceContext object
@[inline]
pub fn d3d11_get_device_context() voidptr {
	return voidptr(C.sapp_d3d11_get_device_context())
}

// D3D11: get pointer to ID3D11RenderView object
@[inline]
pub fn d3d11_get_render_view() voidptr {
	return voidptr(C.sapp_d3d11_get_render_view())
}

// D3D11: get pointer ID3D11RenderTargetView object for msaa-resolve (may return null)
@[inline]
pub fn d3d11_get_resolve_view() voidptr {
	return voidptr(C.sapp_d3d11_get_resolve_view())
}

// D3D11: get pointer to ID3D11DepthStencilView
@[inline]
pub fn d3d11_get_depth_stencil_view() voidptr {
	return voidptr(C.sapp_d3d11_get_depth_stencil_view())
}

// Win32: get the HWND window handle
@[inline]
pub fn win32_get_hwnd() voidptr {
	return voidptr(C.sapp_win32_get_hwnd())
}

// WebGPU: get WGPUDevice handle
pub fn wgpu_get_device() voidptr {
	return voidptr(C.sapp_wgpu_get_device())
}

// WebGPU: get swapchain's WGPUTextureView handle for rendering
pub fn wgpu_get_render_view() voidptr {
	return voidptr(C.sapp_wgpu_get_render_view())
}

// WebGPU: get swapchain's MSAA-resolve WGPUTextureView (may return null)
pub fn wgpu_get_resolve_view() voidptr {
	return voidptr(C.sapp_wgpu_get_resolve_view())
}

// WebGPU: get swapchain's WGPUTextureView for the depth-stencil surface
pub fn wgpu_get_depth_stencil_view() voidptr {
	return voidptr(C.sapp_wgpu_get_depth_stencil_view())
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
