module sapp

const (
	max_touchpoints  = 8
	max_mousebuttons = 3
	max_keycodes     = 512
	max_iconimages   = 8
)

pub struct C.sapp_range {
pub:
	ptr  voidptr
	size usize
}

pub type Range = C.sapp_range

pub struct C.sapp_image_desc {
pub:
	width  int
	height int
	pixels Range
}

pub type ImageDesc = C.sapp_image_desc

pub struct C.sapp_icon_desc {
	sokol_default bool
	images        [max_iconimages]ImageDesc
}

pub type IconDesc = C.sapp_icon_desc

pub struct C.sapp_desc {
pub:
	init_cb    fn () // these are the user-provided callbacks without user data
	frame_cb   fn ()
	cleanup_cb fn ()
	event_cb   fn (&Event) //&sapp_event)
	fail_cb    fn (&u8)

	user_data           voidptr // these are the user-provided callbacks with user data
	init_userdata_cb    fn (voidptr)
	frame_userdata_cb   fn (voidptr)
	cleanup_userdata_cb fn (voidptr)
	event_userdata_cb   fn (&Event, voidptr)
	fail_userdata_cb    fn (&char, voidptr)

	width                        int   // the preferred width of the window / canvas
	height                       int   // the preferred height of the window / canvas
	sample_count                 int   // MSAA sample count
	swap_interval                int   // the preferred swap interval (ignored on some platforms)
	high_dpi                     bool  // whether the rendering canvas is full-resolution on HighDPI displays
	fullscreen                   bool  // whether the window should be created in fullscreen mode
	alpha                        bool  // whether the framebuffer should have an alpha channel (ignored on some platforms)
	window_title                 &char // the window title as UTF-8 encoded string
	enable_clipboard             bool  // enable clipboard access, default is false
	clipboard_size               int   // max size of clipboard content in bytes
	enable_dragndrop             bool  // enable file dropping (drag'n'drop), default is false
	max_dropped_files            int   // max number of dropped files to process (default: 1)
	max_dropped_file_path_length int   // max length in bytes of a dropped UTF-8 file path (default: 2048)
	icon                         IconDesc
	// backend-specific options
	gl_force_gles2                bool  // if true, setup GLES2/WebGL even if GLES3/WebGL2 is available
	win32_console_utf8            bool  // if true, set the output console codepage to UTF-8
	win32_console_create          bool  // if true, attach stdout/stderr to a new console window
	win32_console_attach          bool  // if true, attach stdout/stderr to parent process
	html5_canvas_name             &char // the name (id) of the HTML5 canvas element, default is "canvas"
	html5_canvas_resize           bool  // if true, the HTML5 canvas size is set to sapp_desc.width/height, otherwise canvas size is tracked
	html5_preserve_drawing_buffer bool  // HTML5 only: whether to preserve default framebuffer content between frames
	html5_premultiplied_alpha     bool  // HTML5 only: whether the rendered pixels use premultiplied alpha convention
	html5_ask_leave_site          bool  // initial state of the internal html5_ask_leave_site flag (see sapp_html5_ask_leave_site())
	ios_keyboard_resizes_canvas   bool  // if true, showing the iOS keyboard shrinks the canvas
	// V patches
	__v_native_render bool // V patch to allow for native rendering
}

pub type Desc = C.sapp_desc

pub struct C.sapp_event {
pub:
	frame_count        u64
	@type              EventType
	key_code           KeyCode
	char_code          u32
	key_repeat         bool
	modifiers          u32
	mouse_button       MouseButton
	mouse_x            f32
	mouse_y            f32
	mouse_dx           f32
	mouse_dy           f32
	scroll_x           f32
	scroll_y           f32
	num_touches        int
	touches            [max_touchpoints]TouchPoint
	window_width       int
	window_height      int
	framebuffer_width  int
	framebuffer_height int
}

pub type Event = C.sapp_event

pub fn (e &C.sapp_event) str() string {
	t := e.@type
	return 'evt: frame_count=$e.frame_count, type=$t'
}

pub struct C.sapp_touchpoint {
pub:
	identifier u64
	pos_x      f32
	pos_y      f32
	changed    bool
}

pub type TouchPoint = C.sapp_touchpoint
