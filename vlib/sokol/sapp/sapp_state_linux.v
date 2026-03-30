@[has_globals]
module sapp

// Linux-specific state structs for the V sokol_app backend.
// Shared state (SappTiming, SappMouse, etc.) is in sapp_state.v.

// Wayland-specific state (only compiled when -d sokol_wayland is used)
$if sokol_wayland ? {
	struct SappWayland {
	mut:
		// Core Wayland objects
		display    &C.wl_display    = unsafe { nil }
		registry   &C.wl_registry   = unsafe { nil }
		compositor &C.wl_compositor = unsafe { nil }
		surface    &C.wl_surface    = unsafe { nil }
		egl_window &C.wl_egl_window = unsafe { nil }
		shm        &C.wl_shm        = unsafe { nil }
		// XDG shell
		xdg_wm_base  &C.xdg_wm_base  = unsafe { nil }
		xdg_surface  &C.xdg_surface  = unsafe { nil }
		xdg_toplevel &C.xdg_toplevel = unsafe { nil }
		// Input
		seat         &C.wl_seat     = unsafe { nil }
		pointer      &C.wl_pointer  = unsafe { nil }
		keyboard     &C.wl_keyboard = unsafe { nil }
		touch        &C.wl_touch    = unsafe { nil }
		seat_version u32
		// Pointer constraints
		pointer_constraints  &C.zwp_pointer_constraints_v1      = unsafe { nil }
		locked_pointer       &C.zwp_locked_pointer_v1           = unsafe { nil }
		relative_pointer_mgr &C.zwp_relative_pointer_manager_v1 = unsafe { nil }
		relative_pointer     &C.zwp_relative_pointer_v1         = unsafe { nil }
		// XKB keyboard handling
		xkb_context       &C.xkb_context       = unsafe { nil }
		xkb_keymap        &C.xkb_keymap        = unsafe { nil }
		xkb_state         &C.xkb_state         = unsafe { nil }
		xkb_compose_table &C.xkb_compose_table = unsafe { nil }
		xkb_compose_state &C.xkb_compose_state = unsafe { nil }
		// Cursor
		cursor_shape_manager &C.wp_cursor_shape_manager_v1 = unsafe { nil }
		cursor_shape_device  &C.wp_cursor_shape_device_v1  = unsafe { nil }
		cursor_theme         &C.wl_cursor_theme            = unsafe { nil }
		cursor_default       &C.wl_cursor                  = unsafe { nil }
		cursor_surface       &C.wl_surface                 = unsafe { nil }
		cursor_size          int
		// Fractional scale
		fractional_scale_mgr &C.wp_fractional_scale_manager_v1 = unsafe { nil }
		fractional_scale     &C.wp_fractional_scale_v1         = unsafe { nil }
		scale_numerator      u32
		// Viewporter
		viewporter &C.wp_viewporter = unsafe { nil }
		viewport   &C.wp_viewport   = unsafe { nil }
		// Decorations
		decoration_manager  &C.zxdg_decoration_manager_v1  = unsafe { nil }
		toplevel_decoration &C.zxdg_toplevel_decoration_v1 = unsafe { nil }
		// Clipboard / data device
		data_device_manager &C.wl_data_device_manager = unsafe { nil }
		data_device         &C.wl_data_device         = unsafe { nil }
		data_source         &C.wl_data_source         = unsafe { nil }
		data_offer          &C.wl_data_offer          = unsafe { nil }
		clipboard_string    &char                     = unsafe { nil }
		clipboard_size      int
		// Window state
		width      int
		height     int
		fb_width   int
		fb_height  int
		scale      f32 = 1.0
		configured bool
		closed     bool
		fullscreen bool
		maximized  bool
		focused    bool
		// Pointer state
		pointer_x            f32
		pointer_y            f32
		pointer_enter_serial u32
		// Key repeat
		key_repeat_rate     int = 25
		key_repeat_delay    int = 600
		key_repeat_timer_fd int = -1
		key_repeat_keycode  u32
	}
} $else {
	// Dummy struct for non-Wayland builds
	struct SappWayland {}
}

// X11 XInput extension state
struct SappXi {
mut:
	available    bool
	major_opcode int
	event_base   int
	error_base   int
	major        int
	minor        int
}

// X11 XDnD state
struct SappXdnd {
mut:
	version          i64
	source           Window
	format           Atom
	xdnd_aware       Atom
	xdnd_enter       Atom
	xdnd_position    Atom
	xdnd_status      Atom
	xdnd_action_copy Atom
	xdnd_drop        Atom
	xdnd_finished    Atom
	xdnd_selection   Atom
	xdnd_type_list   Atom
	text_uri_list    Atom
}

// X11-specific state
struct SappX11 {
mut:
	mouse_buttons    u8
	display          &C.Display = unsafe { nil }
	screen           int
	root             Window
	colormap         Colormap
	window           Window
	hidden_cursor    Cursor
	standard_cursors [mousecursor_num]Cursor
	custom_cursors   [mousecursor_num]Cursor
	window_state     int
	dpi              f32
	error_code       u8
	// X atoms
	utf8_string             Atom
	clipboard_atom          Atom
	targets                 Atom
	wm_protocols            Atom
	wm_delete_window        Atom
	wm_state                Atom
	net_wm_name             Atom
	net_wm_icon_name        Atom
	net_wm_icon             Atom
	net_wm_state            Atom
	net_wm_state_fullscreen Atom
	// Extensions
	xi   SappXi
	xdnd SappXdnd
	// Key repeat tracking (keycodes 0..255)
	key_repeat [256]bool
}

// EGL state
struct SappEgl {
mut:
	display EGLDisplay
	context EGLContext
	surface EGLSurface
	config  EGLConfig
}

// Main application state - Linux version with Wayland/X11/EGL backends
struct SappState {
mut:
	desc                    Desc
	valid                   bool
	fullscreen              bool
	first_frame             bool
	init_called             bool
	cleanup_called          bool
	quit_requested          bool
	quit_ordered            bool
	event_consumed          bool
	html5_ask_leave_site    bool
	onscreen_keyboard_shown bool
	window_width            int
	window_height           int
	framebuffer_width       int
	framebuffer_height      int
	sample_count            int
	swap_interval           int
	dpi_scale               f32 = 1.0
	frame_count             u64
	timing                  SappTiming
	event                   Event
	mouse                   SappMouse
	clipboard               SappClipboard
	drop                    SappDrop
	// Platform-specific state
	wl  SappWayland
	x11 SappX11
	egl SappEgl
	gl  SappGl
	// Keycode translation table
	keycodes [max_keycodes]KeyCode
	// Window title
	window_title [max_title_length]u8
	// V patch
	v_native_render bool
	// Custom cursors
	custom_cursor_bound [mousecursor_num]bool
}

// The global state instance
__global g_sapp_state = SappState{}

// === Shared helper functions ===

// init_sapp_event initializes an event struct with common fields.
fn init_sapp_event(event_type EventType) {
	unsafe { C.memset(&g_sapp_state.event, 0, int(sizeof(g_sapp_state.event))) }
	g_sapp_state.event.@type = event_type
	g_sapp_state.event.frame_count = g_sapp_state.frame_count
	g_sapp_state.event.mouse_button = .invalid
	g_sapp_state.event.window_width = g_sapp_state.window_width
	g_sapp_state.event.window_height = g_sapp_state.window_height
	g_sapp_state.event.framebuffer_width = g_sapp_state.framebuffer_width
	g_sapp_state.event.framebuffer_height = g_sapp_state.framebuffer_height
	g_sapp_state.event.mouse_x = g_sapp_state.mouse.x
	g_sapp_state.event.mouse_y = g_sapp_state.mouse.y
	g_sapp_state.event.mouse_dx = g_sapp_state.mouse.dx
	g_sapp_state.event.mouse_dy = g_sapp_state.mouse.dy
}

// call_sapp_event dispatches the event to the user's callback.
fn call_sapp_event(e &Event) bool {
	if !g_sapp_state.cleanup_called {
		if g_sapp_state.desc.event_cb != unsafe { nil } {
			g_sapp_state.desc.event_cb(e)
		} else if g_sapp_state.desc.event_userdata_cb != unsafe { nil } {
			g_sapp_state.desc.event_userdata_cb(e, g_sapp_state.desc.user_data)
		}
	}
	if g_sapp_state.event_consumed {
		g_sapp_state.event_consumed = false
		return true
	}
	return false
}

// sapp_call_init calls the user's init callback.
fn sapp_call_init() {
	if g_sapp_state.desc.init_cb != unsafe { nil } {
		g_sapp_state.desc.init_cb()
	} else if g_sapp_state.desc.init_userdata_cb != unsafe { nil } {
		g_sapp_state.desc.init_userdata_cb(g_sapp_state.desc.user_data)
	}
	g_sapp_state.init_called = true
}

// sapp_call_frame calls the user's frame callback.
fn sapp_call_frame() {
	if g_sapp_state.desc.frame_cb != unsafe { nil } {
		g_sapp_state.desc.frame_cb()
	} else if g_sapp_state.desc.frame_userdata_cb != unsafe { nil } {
		g_sapp_state.desc.frame_userdata_cb(g_sapp_state.desc.user_data)
	}
}

// sapp_call_cleanup calls the user's cleanup callback.
fn sapp_call_cleanup() {
	if !g_sapp_state.cleanup_called {
		if g_sapp_state.desc.cleanup_cb != unsafe { nil } {
			g_sapp_state.desc.cleanup_cb()
		} else if g_sapp_state.desc.cleanup_userdata_cb != unsafe { nil } {
			g_sapp_state.desc.cleanup_userdata_cb(g_sapp_state.desc.user_data)
		}
		g_sapp_state.cleanup_called = true
	}
}

// sapp_do_frame handles the per-frame logic.
fn sapp_do_frame() {
	if g_sapp_state.first_frame {
		g_sapp_state.first_frame = false
		sapp_call_init()
	}
	sapp_call_frame()
	g_sapp_state.frame_count++
}

// === Timing helpers ===

fn sapp_timing_reset(t &SappTiming) {
	unsafe {
		mut mt := t
		mt.last = 0.0
		mt.accum = 0.0
		mt.avg = 0.0
		mt.spike_count = 0
		mt.num = 0
		mt.ring_head = 0
		mt.ring_tail = 0
	}
}

fn sapp_timing_get_avg(t &SappTiming) f64 {
	return t.avg
}

// === Drop file helpers ===

fn sapp_dropped_file_path_ptr(index int) &char {
	assert g_sapp_state.drop.buffer != unsafe { nil }
	assert index >= 0 && index <= g_sapp_state.drop.max_files
	offset := index * g_sapp_state.drop.max_path_length
	assert offset < g_sapp_state.drop.buf_size
	return unsafe { &char(g_sapp_state.drop.buffer + offset) }
}

// === State initialization ===

fn sapp_desc_defaults(desc &Desc) Desc {
	mut res := *desc
	if res.sample_count == 0 {
		res.sample_count = 1
	}
	if res.swap_interval == 0 {
		res.swap_interval = 1
	}
	if res.gl.major_version == 0 {
		res.gl.major_version = 4
		res.gl.minor_version = 3
	}
	if res.clipboard_size == 0 {
		res.clipboard_size = 8192
	}
	if res.max_dropped_files == 0 {
		res.max_dropped_files = 1
	}
	if res.max_dropped_file_path_length == 0 {
		res.max_dropped_file_path_length = 2048
	}
	return res
}

fn sapp_init_state(desc &Desc) {
	unsafe { C.memset(&g_sapp_state, 0, int(sizeof(g_sapp_state))) }
	g_sapp_state.desc = sapp_desc_defaults(desc)
	g_sapp_state.first_frame = true
	g_sapp_state.window_width = g_sapp_state.desc.width
	g_sapp_state.window_height = g_sapp_state.desc.height
	g_sapp_state.framebuffer_width = g_sapp_state.window_width
	g_sapp_state.framebuffer_height = g_sapp_state.window_height
	g_sapp_state.sample_count = g_sapp_state.desc.sample_count
	g_sapp_state.swap_interval = g_sapp_state.desc.swap_interval
	g_sapp_state.clipboard.enabled = g_sapp_state.desc.enable_clipboard
	if g_sapp_state.clipboard.enabled {
		g_sapp_state.clipboard.buf_size = g_sapp_state.desc.clipboard_size
		g_sapp_state.clipboard.buffer = unsafe {
			&char(C.calloc(1, usize(g_sapp_state.clipboard.buf_size)))
		}
	}
	g_sapp_state.drop.enabled = g_sapp_state.desc.enable_dragndrop
	if g_sapp_state.drop.enabled {
		g_sapp_state.drop.max_files = g_sapp_state.desc.max_dropped_files
		g_sapp_state.drop.max_path_length = g_sapp_state.desc.max_dropped_file_path_length
		g_sapp_state.drop.buf_size = g_sapp_state.drop.max_files * g_sapp_state.drop.max_path_length
		g_sapp_state.drop.buffer = unsafe { &char(C.calloc(1, usize(g_sapp_state.drop.buf_size))) }
	}
	// Copy window title
	if g_sapp_state.desc.window_title != unsafe { nil }
		&& unsafe { C.strlen(g_sapp_state.desc.window_title) } > 0 {
		title := g_sapp_state.desc.window_title
		mut i := 0
		for i < max_title_length - 1 {
			ch := unsafe { title[i] }
			if ch == 0 {
				break
			}
			g_sapp_state.window_title[i] = u8(ch)
			i++
		}
		g_sapp_state.window_title[i] = 0
	} else {
		// Default title
		g_sapp_state.window_title[0] = `s`
		g_sapp_state.window_title[1] = `o`
		g_sapp_state.window_title[2] = `k`
		g_sapp_state.window_title[3] = `o`
		g_sapp_state.window_title[4] = `l`
		g_sapp_state.window_title[5] = 0
	}
	g_sapp_state.dpi_scale = 1.0
	g_sapp_state.fullscreen = g_sapp_state.desc.fullscreen
	g_sapp_state.mouse.shown = true
}

fn sapp_discard_state() {
	if g_sapp_state.clipboard.enabled && g_sapp_state.clipboard.buffer != unsafe { nil } {
		unsafe { C.free(g_sapp_state.clipboard.buffer) }
	}
	if g_sapp_state.drop.enabled && g_sapp_state.drop.buffer != unsafe { nil } {
		unsafe { C.free(g_sapp_state.drop.buffer) }
	}
}
