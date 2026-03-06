module sapp

#define SOKOL_VALIDATE_NON_FATAL 1

// returns true after sokol-app has been initialized
fn C.sapp_isvalid() bool

// returns the current framebuffer width in pixels
fn C.sapp_width() i32
fn C.sapp_widthf() f32

// returns the current framebuffer height in pixels
fn C.sapp_height() i32
fn C.sapp_heightf() f32

// get default framebuffer color pixel format
fn C.sapp_color_format() i32

// get default framebuffer depth pixel format
fn C.sapp_depth_format() i32

// get default framebuffer sample count
fn C.sapp_sample_count() i32

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

// get current mouse cursor type
fn C.sapp_get_mouse_cursor() MouseCursor

// bind a custom mouse cursor image to a cursor type, returns the cursor type
fn C.sapp_bind_mouse_cursor_image(cursor MouseCursor, desc &ImageDesc) MouseCursor

// unbind a custom mouse cursor image
fn C.sapp_unbind_mouse_cursor_image(cursor MouseCursor)

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

// intiate a "hard quit" (quit application without sending SAPP_EVENTTYPE_QUIT_REQUESTED)
fn C.sapp_quit()

// call from inside event callback to consume the current event (don't forward to platform)
fn C.sapp_consume_event()

// get the current frame counter (for comparison with sapp_event.frame_count)
fn C.sapp_frame_count() u64

// get an averaged/smoothed frame duration in seconds
fn C.sapp_frame_duration() f64

// write string into clipboard
fn C.sapp_set_clipboard_string(const_str &char)

// read string from clipboard (usually during SAPP_EVENTTYPE_CLIPBOARD_PASTED)
fn C.sapp_get_clipboard_string() &char

// set the window title (only on desktop platforms)
fn C.sapp_set_window_title(&char)

// set the window icon (only on Windows and Linux)
fn C.sapp_set_icon(icon_desc &IconDesc)

// Get number of dropped files
fn C.sapp_get_num_dropped_files() i32

// Get the file path of the dropped file
fn C.sapp_get_dropped_file_path(i32) &char

// special run-function for SOKOL_NO_ENTRY (in standard mode this is an empty stub)
fn C.sapp_run(desc &Desc) i32

// get runtime environment information
fn C.sapp_get_environment() Environment

// get current frame's swapchain information (call once per frame!)
fn C.sapp_get_swapchain() Swapchain

// EGL: get EGLDisplay object
fn C.sapp_egl_get_display() voidptr

// EGL: get EGLContext object
fn C.sapp_egl_get_context() voidptr

// HTML5: enable or disable the hardwired "Leave Site?" dialog box
fn C.sapp_html5_ask_leave_site(ask bool)

// HTML5: get byte size of a dropped file
fn C.sapp_html5_get_dropped_file_size(index i32) u32

// macOS: get ARC-bridged pointer to macOS NSWindow
fn C.sapp_macos_get_window() voidptr

// iOS: get ARC-bridged pointer to iOS UIWindow
fn C.sapp_ios_get_window() voidptr

// D3D11: get pointer to IDXGISwapChain object
fn C.sapp_d3d11_get_swap_chain() voidptr

// Win32: get the HWND window handle
fn C.sapp_win32_get_hwnd() voidptr

// GL: get major version
fn C.sapp_gl_get_major_version() i32

// GL: get minor version
fn C.sapp_gl_get_minor_version() i32

// GL: return true if the context is GLES
fn C.sapp_gl_is_gles() bool

// GL: get framebuffer object
fn C.sapp_gl_get_framebuffer() u32

// X11: get Window
fn C.sapp_x11_get_window() voidptr

// X11: get Display
fn C.sapp_x11_get_display() voidptr

// Android: get native activity handle
fn C.sapp_android_get_native_activity() voidptr

// V-specific: read RGBA pixels from the OpenGL framebuffer
fn C.v_sapp_gl_read_rgba_pixels(x i32, y i32, width i32, height i32, pixels charptr)
