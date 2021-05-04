module sapp

/* returns true after sokol-app has been initialized */
fn C.sapp_isvalid() bool
/* returns the current framebuffer width in pixels */
fn C.sapp_width() int
fn C.sapp_widthf() f32
/* returns the current framebuffer height in pixels */
fn C.sapp_height() int
fn C.sapp_heightf() f32
/* returns true when high_dpi was requested and actually running in a high-dpi scenario */
fn C.sapp_high_dpi() bool
/* returns the dpi scaling factor (window pixels to framebuffer pixels) */
fn C.sapp_dpi_scale() f32
/* show or hide the mobile device onscreen keyboard */
fn C.sapp_show_keyboard(visible bool)
/* return true if the mobile device onscreen keyboard is currently shown */
fn C.sapp_keyboard_shown() bool
/* show or hide the mouse cursor */
fn C.sapp_show_mouse(visible bool)
/* show or hide the mouse cursor */
fn C.sapp_mouse_shown() bool
/* return the userdata pointer optionally provided in sapp_desc */
fn C.sapp_userdata() voidptr
/* return a copy of the sapp_desc structure */
fn C.sapp_query_desc() C.sapp_desc
/* initiate a "soft quit" (sends SAPP_EVENTTYPE_QUIT_REQUESTED) */
fn C.sapp_request_quit()
/* cancel a pending quit (when SAPP_EVENTTYPE_QUIT_REQUESTED has been received) */
fn C.sapp_cancel_quit()
/* intiate a "hard quit" (quit application without sending SAPP_EVENTTYPE_QUIT_REQUSTED) */
fn C.sapp_quit()
/* call from inside event callback to consume the current event (don't forward to platform) */
fn C.sapp_consume_event()
/* get the current frame counter (for comparison with sapp_event.frame_count) */
fn C.sapp_frame_count() u64
/* write string into clipboard */
fn C.sapp_set_clipboard_string(str byteptr)
/* read string from clipboard (usually during SAPP_EVENTTYPE_CLIPBOARD_PASTED) */
fn C.sapp_get_clipboard_string() byteptr

/* special run-function for SOKOL_NO_ENTRY (in standard mode this is an empty stub) */
fn C.sapp_run(desc &C.sapp_desc) int

/* GL: return true when GLES2 fallback is active (to detect fallback from GLES3) */
fn C.sapp_gles2() bool

/* HTML5: enable or disable the hardwired "Leave Site?" dialog box */
fn C.sapp_html5_ask_leave_site(ask bool)

/* Metal: get ARC-bridged pointer to Metal device object */
fn C.sapp_metal_get_device() voidptr
/* Metal: get ARC-bridged pointer to this frame's renderpass descriptor */
fn C.sapp_metal_get_renderpass_descriptor() voidptr
/* Metal: get ARC-bridged pointer to current drawable */
fn C.sapp_metal_get_drawable() voidptr
/* macOS: get ARC-bridged pointer to macOS NSWindow */
fn C.sapp_macos_get_window() voidptr
/* iOS: get ARC-bridged pointer to iOS UIWindow */
fn C.sapp_ios_get_window() voidptr

/* D3D11: get pointer to ID3D11Device object */
fn C.sapp_d3d11_get_device() voidptr
/* D3D11: get pointer to ID3D11DeviceContext object */
fn C.sapp_d3d11_get_device_context() voidptr
/* D3D11: get pointer to ID3D11RenderTargetView object */
fn C.sapp_d3d11_get_render_target_view() voidptr
/* D3D11: get pointer to ID3D11DepthStencilView */
fn C.sapp_d3d11_get_depth_stencil_view() voidptr
/* Win32: get the HWND window handle */
fn C.sapp_win32_get_hwnd() voidptr
/* Android: get native activity handle */
fn C.sapp_android_get_native_activity() voidptr
