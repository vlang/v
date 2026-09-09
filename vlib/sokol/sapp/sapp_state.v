module sapp

// Shared state structs for the V sokol_app backends.
// Platform-specific state (SappState, g_sapp_state, SappWayland, SappWin32, etc.)
// lives in sapp_state_linux.v / sapp_state_windows.v respectively.

const ring_num_slots = 256
const max_title_length = 128
const mousecursor_num = 27

// Timing state for frame duration averaging
struct SappTiming {
mut:
	last        f64
	accum       f64
	avg         f64
	spike_count int
	num         int
	ring_head   int
	ring_tail   int
	ring_buf    [ring_num_slots]f64
}

// Mouse state
struct SappMouse {
mut:
	x              f32
	y              f32
	dx             f32
	dy             f32
	shown          bool
	locked         bool
	pos_valid      bool
	current_cursor MouseCursor
}

// Clipboard state
struct SappClipboard {
mut:
	enabled  bool
	buf_size int
	buffer   &char = unsafe { nil }
}

// Drag and drop state
struct SappDrop {
mut:
	enabled         bool
	max_files       int
	max_path_length int
	num_files       int
	buf_size        int
	buffer          &char = unsafe { nil }
}

// GL state
struct SappGl {
mut:
	framebuffer u32
	major       int
	minor       int
}
