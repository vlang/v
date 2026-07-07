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
	id         WindowId
	status     WindowStatus
	title      string
	width      int
	height     int
	min_width  int
	min_height int
	resizable  bool
	visible    bool
	high_dpi   bool
	borderless bool
	fullscreen bool
}

// Capabilities reports what the selected backend can do.
pub struct Capabilities {
pub:
	backend            BackendKind
	mock               bool
	native             bool
	multi_window       bool
	owner_queue        bool
	explicit_swapchain bool
	readback           bool
	d3d11              bool
	metal              bool
	x11                bool
	wayland            bool
	win32              bool
	gl                 bool
}

// Event is always routed to a specific WindowId.
pub struct Event {
pub:
	kind      EventKind
	window_id WindowId
	width     int
	height    int
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
