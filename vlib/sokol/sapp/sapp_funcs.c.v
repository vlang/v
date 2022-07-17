module sapp

// returns true after sokol-app has been initialized
fn C.sapp_isvalid() bool

// returns the current framebuffer width in pixels
fn C.sapp_width() int
fn C.sapp_widthf() f32

// returns the current framebuffer height in pixels
fn C.sapp_height() int
fn C.sapp_heightf() f32

// get default framebuffer color pixel format
fn C.sapp_color_format() int

// get default framebuffer depth pixel format
fn C.sapp_depth_format() int

// get default framebuffer sample count
fn C.sapp_sample_count() int

// returns true when high_dpi was requested and actually running in a high-dpi scenario
fn C.sapp_high_dpi() bool

// returns the dpi scaling factor (window pixels to framebuffer pixels)
fn C.sapp_dpi_scale() f32

// show or hide the mobile device onscreen keyboard
fn C.sapp_show_keyboard(visible bool)

// return true if the mobile device onscreen keyboard is currently shown
fn C.sapp_keyboard_shown() bool

// Check if full screen rendering
fn C.sapp_is_fullscreen() bool

// Toggle full screen
fn C.sapp_toggle_fullscreen()

// show or hide the mouse cursor
fn C.sapp_show_mouse(visible bool)

// return true if the mouse cursor is shown
fn C.sapp_mouse_shown() bool

// set mouse cursor
fn C.sapp_set_mouse_cursor(cursor MouseCursor)

// lock or unlock the mouse cursor
fn C.sapp_lock_mouse(locked bool)

// return true if the mouse cursor is locked
fn C.sapp_mouse_locked() bool

// return the userdata pointer optionally provided in sapp_desc
fn C.sapp_userdata() voidptr

// return a copy of the sapp_desc structure
fn C.sapp_query_desc() Desc

// initiate a "soft quit" (sends SAPP_EVENTTYPE_QUIT_REQUESTED)
fn C.sapp_request_quit()

// cancel a pending quit (when SAPP_EVENTTYPE_QUIT_REQUESTED has been received)
fn C.sapp_cancel_quit()

// intiate a "hard quit" (quit application without sending SAPP_EVENTTYPE_QUIT_REQUSTED)
fn C.sapp_quit()

// call from inside event callback to consume the current event (don't forward to platform)
fn C.sapp_consume_event()

// get the current frame counter (for comparison with sapp_event.frame_count)
fn C.sapp_frame_count() u64

// get an averaged/smoothed frame duration in seconds
fn C.sapp_frame_duration() f64

// write string into clipboard
fn C.sapp_set_clipboard_string(str &u8)

// read string from clipboard (usually during SAPP_EVENTTYPE_CLIPBOARD_PASTED)
fn C.sapp_get_clipboard_string() &u8

// set the window title (only on desktop platforms)
fn C.sapp_set_window_title(&char)

// /* set the window icon (only on Windows and Linux) */
// SOKOL_APP_API_DECL void sapp_set_icon(const sapp_icon_desc* icon_desc);

// Get number of droped files
fn C.sapp_get_num_dropped_files() int

// Get the file path of the droped file
fn C.sapp_get_dropped_file_path(int) &u8

// special run-function for SOKOL_NO_ENTRY (in standard mode this is an empty stub)
fn C.sapp_run(desc &Desc) int

// GL: return true when GLES2 fallback is active (to detect fallback from GLES3)
fn C.sapp_gles2() bool

// HTML5: enable or disable the hardwired "Leave Site?" dialog box
fn C.sapp_html5_ask_leave_site(ask bool)

// /* HTML5: get byte size of a dropped file */
// SOKOL_APP_API_DECL uint32_t sapp_html5_get_dropped_file_size(int index);

// /* HTML5: asynchronously load the content of a dropped file */
// SOKOL_APP_API_DECL void sapp_html5_fetch_dropped_file(const sapp_html5_fetch_request* request);

// Metal: get ARC-bridged pointer to Metal device object
fn C.sapp_metal_get_device() voidptr

// Metal: get ARC-bridged pointer to this frame's renderpass descriptor
fn C.sapp_metal_get_renderpass_descriptor() voidptr

// Metal: get ARC-bridged pointer to current drawable
fn C.sapp_metal_get_drawable() voidptr

// macOS: get ARC-bridged pointer to macOS NSWindow
fn C.sapp_macos_get_window() voidptr

// iOS: get ARC-bridged pointer to iOS UIWindow
fn C.sapp_ios_get_window() voidptr

// D3D11: get pointer to ID3D11Device object
fn C.sapp_d3d11_get_device() voidptr

// D3D11: get pointer to ID3D11DeviceContext object
fn C.sapp_d3d11_get_device_context() voidptr

// D3D11: get pointer to IDXGISwapChain object
fn C.sapp_d3d11_get_swap_chain() voidptr

// D3D11: get pointer to ID3D11RenderTargetView object
fn C.sapp_d3d11_get_render_target_view() voidptr

// D3D11: get pointer to ID3D11DepthStencilView
fn C.sapp_d3d11_get_depth_stencil_view() voidptr

// Win32: get the HWND window handle
fn C.sapp_win32_get_hwnd() voidptr

// WebGPU: get WGPUDevice handle
fn C.sapp_wgpu_get_device() voidptr

// WebGPU: get swapchain's WGPUTextureView handle for rendering
fn C.sapp_wgpu_get_render_view() voidptr

// WebGPU: get swapchain's MSAA-resolve WGPUTextureView (may return null)
fn C.sapp_wgpu_get_resolve_view() voidptr

// WebGPU: get swapchain's WGPUTextureView for the depth-stencil surface
fn C.sapp_wgpu_get_depth_stencil_view() voidptr

// Android: get native activity handle
fn C.sapp_android_get_native_activity() voidptr
