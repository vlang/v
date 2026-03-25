module sapp

import sokol.memory

const max_touchpoints = 8
const max_mousebuttons = 3
const max_keycodes = 512
const max_iconimages = 8

@[typedef]
pub struct C.sapp_allocator {
pub mut:
	alloc_fn  memory.FnAllocatorAlloc = unsafe { nil }
	free_fn   memory.FnAllocatorFree  = unsafe { nil }
	user_data voidptr
}

pub type Allocator = C.sapp_allocator

@[typedef]
pub struct C.sapp_logger {
pub mut:
	func      memory.FnLogCb = unsafe { nil }
	user_data voidptr
}

pub type Logger = C.sapp_logger

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
	width            int
	height           int
	cursor_hotspot_x int
	cursor_hotspot_y int
	pixels           Range
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
pub struct C.sapp_gl_desc {
pub mut:
	major_version int // override GL/GLES major version
	minor_version int // override GL/GLES minor version
}

pub type GlDesc = C.sapp_gl_desc

@[typedef]
pub struct C.sapp_win32_desc {
pub:
	console_utf8   bool // if true, set the output console codepage to UTF-8
	console_create bool // if true, attach stdout/stderr to a new console window
	console_attach bool // if true, attach stdout/stderr to parent process
}

pub type Win32Desc = C.sapp_win32_desc

@[typedef]
pub struct C.sapp_html5_desc {
pub:
	canvas_selector                           &char = c'#canvas' // css selector of the HTML5 canvas element, default is "#canvas"
	canvas_resize                             bool // if true, the HTML5 canvas size is set to sapp_desc.width/height
	preserve_drawing_buffer                   bool // HTML5 only: whether to preserve default framebuffer content between frames
	premultiplied_alpha                       bool // HTML5 only: whether the rendered pixels use premultiplied alpha convention
	ask_leave_site                            bool // initial state of the internal html5_ask_leave_site flag
	update_document_title                     bool // if true, update the HTML document.title with sapp_desc.window_title
	bubble_mouse_events                       bool // if true, mouse events will bubble up to the web page
	bubble_touch_events                       bool // same for touch events
	bubble_wheel_events                       bool // same for wheel events
	bubble_key_events                         bool // if true, bubble up *all* key events to browser
	bubble_char_events                        bool // if true, bubble up character events to browser
	use_emsc_set_main_loop                    bool // if true, use emscripten_set_main_loop() instead of emscripten_request_animation_frame_loop()
	emsc_set_main_loop_simulate_infinite_loop bool // this will be passed as the simulate_infinite_loop arg to emscripten_set_main_loop()
}

pub type Html5Desc = C.sapp_html5_desc

@[typedef]
pub struct C.sapp_ios_desc {
pub:
	keyboard_resizes_canvas bool // if true, showing the iOS keyboard shrinks the canvas
}

pub type IosDesc = C.sapp_ios_desc

// --- environment structs (returned by sapp_get_environment) ---

@[typedef]
pub struct C.sapp_environment_defaults {
pub:
	color_format int
	depth_format int
	sample_count int
}

pub type EnvironmentDefaults = C.sapp_environment_defaults

@[typedef]
pub struct C.sapp_metal_environment {
pub:
	device voidptr
}

@[typedef]
pub struct C.sapp_d3d11_environment {
pub:
	device         voidptr
	device_context voidptr
}

@[typedef]
pub struct C.sapp_wgpu_environment {
pub:
	device voidptr
}

@[typedef]
pub struct C.sapp_vulkan_environment {
pub:
	instance           voidptr
	physical_device    voidptr
	device             voidptr
	queue              voidptr
	queue_family_index u32
}

pub type MetalEnvironment = C.sapp_metal_environment
pub type D3d11Environment = C.sapp_d3d11_environment
pub type WgpuEnvironment = C.sapp_wgpu_environment
pub type VulkanEnvironment = C.sapp_vulkan_environment

@[typedef]
pub struct C.sapp_environment {
pub:
	defaults EnvironmentDefaults
	metal    MetalEnvironment
	d3d11    D3d11Environment
	wgpu     WgpuEnvironment
	vulkan   VulkanEnvironment
}

pub type Environment = C.sapp_environment

// --- swapchain structs (returned by sapp_get_swapchain) ---

@[typedef]
pub struct C.sapp_metal_swapchain {
pub:
	current_drawable      voidptr
	depth_stencil_texture voidptr
	msaa_color_texture    voidptr
}

@[typedef]
pub struct C.sapp_d3d11_swapchain {
pub:
	render_view        voidptr
	resolve_view       voidptr
	depth_stencil_view voidptr
}

@[typedef]
pub struct C.sapp_wgpu_swapchain {
pub:
	render_view        voidptr
	resolve_view       voidptr
	depth_stencil_view voidptr
}

@[typedef]
pub struct C.sapp_vulkan_swapchain {
pub:
	render_image               voidptr
	render_view                voidptr
	resolve_image              voidptr
	resolve_view               voidptr
	depth_stencil_image        voidptr
	depth_stencil_view         voidptr
	render_finished_semaphore  voidptr
	present_complete_semaphore voidptr
}

@[typedef]
pub struct C.sapp_gl_swapchain {
pub:
	framebuffer u32
}

pub type MetalSwapchain = C.sapp_metal_swapchain
pub type D3d11Swapchain = C.sapp_d3d11_swapchain
pub type WgpuSwapchain = C.sapp_wgpu_swapchain
pub type VulkanSwapchain = C.sapp_vulkan_swapchain
pub type GlSwapchain = C.sapp_gl_swapchain

@[typedef]
pub struct C.sapp_swapchain {
pub:
	width        int
	height       int
	sample_count int
	color_format int
	depth_format int
	metal        MetalSwapchain
	d3d11        D3d11Swapchain
	wgpu         WgpuSwapchain
	vulkan       VulkanSwapchain
	gl           GlSwapchain
}

pub type Swapchain = C.sapp_swapchain

@[typedef]
pub struct C.sapp_desc {
pub mut:
	// these are the user-provided callbacks without user data
	init_cb    fn ()       = unsafe { nil }
	frame_cb   fn ()       = unsafe { nil }
	cleanup_cb fn ()       = unsafe { nil }
	event_cb   fn (&Event) = unsafe { nil } // &sapp_event

	user_data           voidptr // these are the user-provided callbacks with user data
	init_userdata_cb    fn (voidptr)         = unsafe { nil }
	frame_userdata_cb   fn (voidptr)         = unsafe { nil }
	cleanup_userdata_cb fn (voidptr)         = unsafe { nil }
	event_userdata_cb   fn (&Event, voidptr) = unsafe { nil }

	width                        int       // the preferred width of the window / canvas
	height                       int       // the preferred height of the window / canvas
	sample_count                 int       // MSAA sample count
	swap_interval                int       // the preferred swap interval (ignored on some platforms)
	high_dpi                     bool      // whether the rendering canvas is full-resolution on HighDPI displays
	fullscreen                   bool      // whether the window should be created in fullscreen mode
	alpha                        bool      // whether the framebuffer should have an alpha channel (ignored on some platforms)
	window_title                 &char     // the window title as UTF-8 encoded string
	enable_clipboard             bool      // enable clipboard access, default is false
	clipboard_size               int       // max size of clipboard content in bytes
	enable_dragndrop             bool      // enable file dropping (drag'n'drop), default is false
	max_dropped_files            int       // max number of dropped files to process (default: 1)
	max_dropped_file_path_length int       // max length in bytes of a dropped UTF-8 file path (default: 2048)
	icon                         IconDesc  // the initial window icon to set
	allocator                    Allocator // optional memory allocation overrides (default: malloc/free)
	logger                       Logger    // logging callback override (default: NO LOGGING!)
	// backend-specific options
	gl    GlDesc    // OpenGL specific options
	win32 Win32Desc // Win32 specific options
	html5 Html5Desc // HTML5 specific options
	ios   IosDesc   // iOS specific options
	// V patches
	__v_native_render bool // V patch to allow for native rendering
	min_width         int  // V patch to allow for min window width
	min_height        int  // V patch to allow for min window height
	borderless_window bool // V patch to create a window without native decorations when supported
}

pub type Desc = C.sapp_desc

@[typedef]
pub struct C.sapp_event {
pub mut:
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

pub fn (e &Event) str() string {
	return 'evt: frame_count=${e.frame_count}, type=${EventType(e.type)}'
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
