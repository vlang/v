module multiwindow

// BackendKind selects the platform implementation. The .auto policy resolves
// to a concrete backend before startup/capabilities are reported.
pub enum BackendKind {
	auto
	mock
	x11
	wayland
	appkit
	win32
}

// AppStatus describes the application lifecycle owned by App.
pub enum AppStatus {
	running
	stopped
}

// WindowStatus describes the lifecycle of a registered window slot.
pub enum WindowStatus {
	invalid
	alive
	destroyed
}

// EventKind describes core window lifecycle events emitted by the backend.
pub enum EventKind {
	window_created
	window_destroyed
	window_close_requested
	window_resized
}

// InputEventKind mirrors the input/event classes exposed by sokol/gg while
// remaining independent from gg and sokol imports.
pub enum InputEventKind {
	invalid
	key_down
	key_up
	char
	mouse_down
	mouse_up
	mouse_scroll
	mouse_move
	mouse_enter
	mouse_leave
	touches_began
	touches_moved
	touches_ended
	touches_cancelled
	resized
	iconified
	restored
	focused
	unfocused
	suspended
	resumed
	quit_requested
	clipboard_pasted
	files_dropped
}

pub enum QueuedEventKind {
	lifecycle
	input
}

const input_event_invalid_mouse_button = 256

// WindowResizeEdge identifies the edge or corner used for an interactive,
// user-driven native resize operation.
pub enum WindowResizeEdge {
	top
	bottom
	left
	right
	top_left
	top_right
	bottom_left
	bottom_right
}

// CursorShape identifies a native cursor image for client-side chrome hover
// feedback. Cursor support is independent from native interactive move/resize.
pub enum CursorShape {
	default
	pointer
	move
	n_resize
	s_resize
	e_resize
	w_resize
	ne_resize
	nw_resize
	se_resize
	sw_resize
	ew_resize
	ns_resize
	nesw_resize
	nwse_resize
	grab
	grabbing
}

// WindowId is an opaque generation-checked handle to a window.
pub struct WindowId {
	slot       int
	generation u32
}

// str returns a diagnostic representation of a WindowId without exposing its
// fields as public mutable state.
pub fn (id WindowId) str() string {
	return 'WindowId(${id.slot}:${id.generation})'
}

// Config configures a multi-window App.
@[params]
pub struct Config {
pub:
	backend          BackendKind = .mock
	queue_size       int         = 128
	require_renderer bool
}

// WindowConfig describes one window at creation time.
@[params]
pub struct WindowConfig {
pub:
	title      string = 'V Window'
	width      int    = 800
	height     int    = 600
	min_width  int
	min_height int
	resizable  bool = true
	visible    bool = true
	high_dpi   bool = true
	borderless bool
	fullscreen bool
}

struct WindowSize {
	width  int
	height int
}

// WindowInfo is a snapshot of the authoritative App-side window state.
pub struct WindowInfo {
pub:
	id                 WindowId
	status             WindowStatus
	title              string
	width              int
	height             int
	min_width          int
	min_height         int
	resizable          bool
	visible            bool
	high_dpi           bool
	borderless         bool
	fullscreen         bool
	native_decorations bool
}

// Capabilities reports what the selected backend can do.
pub struct Capabilities {
pub:
	backend                 BackendKind
	mock                    bool
	native                  bool
	multi_window            bool
	owner_queue             bool
	explicit_swapchain      bool
	readback                bool
	d3d11                   bool
	metal                   bool
	x11                     bool
	wayland                 bool
	win32                   bool
	gl                      bool
	input_events            bool
	mouse_events            bool
	keyboard_events         bool
	text_events             bool
	focus_events            bool
	drop_events             bool
	touch_events            bool
	cursor_shapes           bool
	interactive_move_resize bool
	native_decorations      bool
}

// Event is always routed to a specific WindowId.
pub struct Event {
pub:
	kind      EventKind
	window_id WindowId
	width     int
	height    int
}

// InputTouchPoint is the backend-neutral representation of one touch point.
pub struct InputTouchPoint {
pub:
	identifier       u64
	pos_x            f32
	pos_y            f32
	android_tooltype int
	changed          bool
}

// InputEvent is always routed to a specific WindowId. It carries the full
// backend-neutral payload needed to rebuild gg.Event in the gg facade.
pub struct InputEvent {
pub:
	kind               InputEventKind
	window_id          WindowId
	frame_count        u64
	key_code           int
	char_code          u32
	key_repeat         bool
	modifiers          u32
	mouse_button       int = input_event_invalid_mouse_button
	mouse_x            f32
	mouse_y            f32
	mouse_dx           f32
	mouse_dy           f32
	scroll_x           f32
	scroll_y           f32
	num_touches        int
	touches            [8]InputTouchPoint
	window_width       int
	window_height      int
	framebuffer_width  int
	framebuffer_height int
	dropped_files      []string
}

// QueuedEvent preserves backend ordering between lifecycle and input events.
pub struct QueuedEvent {
pub:
	kind      QueuedEventKind
	lifecycle Event
	input     InputEvent
}

@[markused]
fn queued_lifecycle_event(event Event) QueuedEvent {
	return QueuedEvent{
		kind:      .lifecycle
		lifecycle: event
	}
}

@[markused]
fn queued_input_event(event InputEvent) QueuedEvent {
	return QueuedEvent{
		kind:  .input
		input: event
	}
}

// AppJobFn is executed later by the owner queue while App is being pumped.
pub type AppJobFn = fn (mut app App) !

struct WindowSlot {
mut:
	id     WindowId
	config WindowConfig
	status WindowStatus
}

fn window_size_for_config(config WindowConfig, width int, height int) WindowSize {
	return WindowSize{
		width:  window_extent_for_minimum(width, config.min_width)
		height: window_extent_for_minimum(height, config.min_height)
	}
}

fn window_extent_for_minimum(value int, minimum int) int {
	if minimum > 0 && value < minimum {
		return minimum
	}
	return value
}

fn dropped_files_from_uri_list(payload string) []string {
	mut files := []string{}
	for raw_line in payload.split('\n') {
		line := if raw_line.ends_with('\r') {
			raw_line[..raw_line.len - 1]
		} else {
			raw_line
		}
		if line.len == 0 || line.starts_with('#') || !line.starts_with('file://') {
			continue
		}
		mut path := line[7..]
		if path.starts_with('localhost/') {
			path = path[9..]
		}
		if path.len == 0 || path[0] != `/` {
			continue
		}
		files << uri_percent_decode(path)
	}
	return files
}

fn uri_percent_decode(value string) string {
	bytes := value.bytes()
	mut decoded := []u8{cap: bytes.len}
	mut i := 0
	for i < bytes.len {
		if bytes[i] == `%` && i + 2 < bytes.len {
			hi := uri_hex_nibble(bytes[i + 1])
			lo := uri_hex_nibble(bytes[i + 2])
			if hi >= 0 && lo >= 0 {
				decoded << u8(hi * 16 + lo)
				i += 3
				continue
			}
		}
		decoded << bytes[i]
		i++
	}
	return decoded.bytestr()
}

fn uri_hex_nibble(ch u8) int {
	if ch >= `0` && ch <= `9` {
		return int(ch - `0`)
	}
	if ch >= `a` && ch <= `f` {
		return int(ch - `a`) + 10
	}
	if ch >= `A` && ch <= `F` {
		return int(ch - `A`) + 10
	}
	return -1
}
