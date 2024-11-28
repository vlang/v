module sapp

const max_touchpoints = 8
const max_mousebuttons = 3
const max_keycodes = 512
const max_iconimages = 8

// sapp_range is a general pointer/size-pair struct for passing binary blobs into sokol_app.h
@[typedef]
pub struct C.sapp_range {
pub:
	ptr  voidptr
	size usize
}

pub type Range = C.sapp_range

@[typedef]
pub struct C.sapp_image_desc {
pub:
	width  int
	height int
	pixels Range
}

pub type ImageDesc = C.sapp_image_desc

@[typedef]
pub struct C.sapp_icon_desc {
pub:
	sokol_default bool
	images        [max_iconimages]ImageDesc
}

pub type IconDesc = C.sapp_icon_desc

@[typedef]
pub struct C.sapp_desc {
pub:
	// these are the user-provided callbacks without user data
	init_cb    fn ()       = unsafe { nil }
	frame_cb   fn ()       = unsafe { nil }
	cleanup_cb fn ()       = unsafe { nil }
	event_cb   fn (&Event) = unsafe { nil } // &sapp_event
	// fail_cb    fn (&u8)    = unsafe { nil }

	user_data           voidptr // these are the user-provided callbacks with user data
	init_userdata_cb    fn (voidptr)         = unsafe { nil }
	frame_userdata_cb   fn (voidptr)         = unsafe { nil }
	cleanup_userdata_cb fn (voidptr)         = unsafe { nil }
	event_userdata_cb   fn (&Event, voidptr) = unsafe { nil }
	// fail_userdata_cb    fn (&char, voidptr)  = unsafe { nil }

	width                        int      // the preferred width of the window / canvas
	height                       int      // the preferred height of the window / canvas
	sample_count                 int      // MSAA sample count
	swap_interval                int      // the preferred swap interval (ignored on some platforms)
	high_dpi                     bool     // whether the rendering canvas is full-resolution on HighDPI displays
	fullscreen                   bool     // whether the window should be created in fullscreen mode
	alpha                        bool     // whether the framebuffer should have an alpha channel (ignored on some platforms)
	window_title                 &char    // the window title as UTF-8 encoded string
	enable_clipboard             bool     // enable clipboard access, default is false
	clipboard_size               int      // max size of clipboard content in bytes
	enable_dragndrop             bool     // enable file dropping (drag'n'drop), default is false
	max_dropped_files            int      // max number of dropped files to process (default: 1)
	max_dropped_file_path_length int      // max length in bytes of a dropped UTF-8 file path (default: 2048)
	icon                         IconDesc // the initial window icon to set
	// backend-specific options
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
	min_width         int  // V patch to allow for min window width
	min_height        int  // V patch to allow for min window height
pub mut:
	allocator C.sapp_allocator // optional memory allocation overrides (default: malloc/free)
	logger    C.sapp_logger    // optional log callback overrides (default: SAPP_LOG(message))
}

pub type Desc = C.sapp_desc

@[typedef]
pub struct C.sapp_event {
pub:
	frame_count        u64                         // current frame counter, always valid, useful for checking if two events were issued in the same frame
	type               EventType                   // the event type, always valid
	key_code           KeyCode                     // the virtual key code, only valid in KEY_UP, KEY_DOWN
	char_code          u32                         // the UTF-32 character code, only valid in CHAR events
	key_repeat         bool                        // true if this is a key-repeat event, valid in KEY_UP, KEY_DOWN and CHAR
	modifiers          u32                         // current modifier keys, valid in all key-, char- and mouse-events
	mouse_button       MouseButton                 // mouse button that was pressed or released, valid in MOUSE_DOWN, MOUSE_UP
	mouse_x            f32                         // current horizontal mouse position in pixels, always valid except during mouse lock
	mouse_y            f32                         // current vertical mouse position in pixels, always valid except during mouse lock
	mouse_dx           f32                         // relative horizontal mouse movement since last frame, always valid
	mouse_dy           f32                         // relative vertical mouse movement since last frame, always valid
	scroll_x           f32                         // horizontal mouse wheel scroll distance, valid in MOUSE_SCROLL events
	scroll_y           f32                         // vertical mouse wheel scroll distance, valid in MOUSE_SCROLL events
	num_touches        int                         // number of valid items in the touches[] array
	touches            [max_touchpoints]TouchPoint // current touch points, valid in TOUCHES_BEGIN, TOUCHES_MOVED, TOUCHES_ENDED
	window_width       int                         // current window- and framebuffer width in pixels, always valid
	window_height      int                         // current window- and framebuffer height in pixels, always valid
	framebuffer_width  int                         // = window_width * dpi_scale
	framebuffer_height int                         // = window_height * dpi_scale
}

pub type Event = C.sapp_event

pub fn (e &C.sapp_event) str() string {
	return 'evt: frame_count=${e.frame_count}, type=${e.type}'
}

@[typedef]
pub struct C.sapp_touchpoint {
pub:
	identifier       u64
	pos_x            f32
	pos_y            f32
	android_tooltype TouchToolType
	changed          bool
}

pub type TouchPoint = C.sapp_touchpoint
