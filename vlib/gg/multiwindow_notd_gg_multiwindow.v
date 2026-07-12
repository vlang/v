// Compile with `-d gg_multiwindow` to enable the additive gg.App facade.
module gg

const err_multiwindow_not_enabled = 'gg.multiwindow: compile with `-d gg_multiwindow` to enable gg.App'

// MultiWindowBackend selects the implementation behind gg.App.
pub enum MultiWindowBackend {
	auto
	mock
	x11
	wayland
	appkit
	win32
}

// WindowEventKind describes lifecycle events routed to a specific window.
pub enum WindowEventKind {
	window_created
	window_destroyed
	window_close_requested
	window_resized
}

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

// WindowCursorShape identifies a native cursor image for hover feedback.
pub enum WindowCursorShape {
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

// WindowId identifies a window managed by gg.App.
pub struct WindowId {}

// str returns a diagnostic representation of a WindowId.
pub fn (id WindowId) str() string {
	_ = id
	return '<gg.WindowId disabled: compile with `-d gg_multiwindow`>'
}

// AppConfig configures a multi-window gg application facade.
@[params]
pub struct AppConfig {
pub:
	backend          MultiWindowBackend = .auto
	queue_size       int                = 128
	require_renderer bool
}

// WindowConfig describes one window at creation time.
@[params]
pub struct WindowConfig {
pub:
	title      string = 'A GG Window'
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

// WindowInfo is a snapshot of a live gg.App window.
pub struct WindowInfo {
pub:
	id                 WindowId
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

// Capabilities reports the active gg.App backend contract.
pub struct Capabilities {
pub:
	backend                 MultiWindowBackend
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

// WindowEvent is the multi-window event wrapper. The existing gg.Event remains
// the legacy single-window gg.Context event type.
pub struct WindowEvent {
pub:
	kind   WindowEventKind
	window WindowId
	// width and height are meaningful for .window_created and .window_resized events.
	width  int
	height int
}

// WindowInputEvent routes a normal gg.Event to a specific gg.App window.
pub struct WindowInputEvent {
pub:
	window        WindowId
	event         Event
	dropped_files []string
}

// AppJobFn is executed later by app.drain_pending() on the owner side.
pub type AppJobFn = fn (mut app App) !

// AppFrameFn is a render-frame callback and requires a render-capable backend
// with explicit swapchains.
pub type AppFrameFn = fn (mut app App) !

// AppEventFn is called by App.run() for each drained multi-window event.
pub type AppEventFn = fn (event WindowEvent, mut app App) !

// AppInputFn is called by App.run() for each drained multi-window input event.
pub type AppInputFn = fn (event WindowInputEvent, mut app App) !

// WindowDrawFn records drawing commands for one WindowContext.
pub type WindowDrawFn = fn (mut window WindowContext) !

// RunConfig configures the gg.App loop. Use event_fn without frame_fn for
// lifecycle-only loops that do not require a renderer.
@[params]
pub struct RunConfig {
pub:
	frame_fn         AppFrameFn = unsafe { nil }
	event_fn         AppEventFn = unsafe { nil }
	input_fn         AppInputFn = unsafe { nil }
	max_pending_jobs int        = 64
}

// WindowContext is the public draw target for one gg.App window.
pub struct WindowContext {}

// App is the additive multi-window facade for gg users.
//
// Compile with `-d gg_multiwindow` to enable this API.
@[heap]
pub struct App {
pub:
	config AppConfig
}

// new_app creates a gg multi-window application.
pub fn new_app(config AppConfig) !&App {
	_ = config
	return error(err_multiwindow_not_enabled)
}

// capabilities_for_backend reports capabilities for a backend policy.
pub fn capabilities_for_backend(backend MultiWindowBackend) !Capabilities {
	_ = backend
	return error(err_multiwindow_not_enabled)
}

// capabilities_for_backend_with_renderer reports capabilities while requiring
// renderer startup. Compile with `-d gg_multiwindow` to enable this API.
pub fn capabilities_for_backend_with_renderer(backend MultiWindowBackend) !Capabilities {
	_ = backend
	return error(err_multiwindow_not_enabled)
}

// create_window creates a window managed by this app.
pub fn (mut app App) create_window(config WindowConfig) !WindowId {
	_ = app
	_ = config
	return error(err_multiwindow_not_enabled)
}

// set_window_title updates a live window title.
pub fn (mut app App) set_window_title(id WindowId, title string) ! {
	_ = app
	_ = id
	_ = title
	return error(err_multiwindow_not_enabled)
}

// resize_window requests a live window resize.
pub fn (mut app App) resize_window(id WindowId, width int, height int) ! {
	_ = app
	_ = id
	_ = width
	_ = height
	return error(err_multiwindow_not_enabled)
}

// set_window_cursor updates the native hover cursor for a live window.
pub fn (mut app App) set_window_cursor(id WindowId, shape WindowCursorShape) ! {
	_ = app
	_ = id
	_ = shape
	return error(err_multiwindow_not_enabled)
}

// begin_window_move starts a user-driven native move for a live window.
pub fn (mut app App) begin_window_move(id WindowId) ! {
	_ = app
	_ = id
	return error(err_multiwindow_not_enabled)
}

// begin_window_resize starts a user-driven native resize for a live resizable window.
pub fn (mut app App) begin_window_resize(id WindowId, edge WindowResizeEdge) ! {
	_ = app
	_ = id
	_ = edge
	return error(err_multiwindow_not_enabled)
}

// window_info returns a snapshot of a live window.
pub fn (app &App) window_info(id WindowId) !WindowInfo {
	_ = app
	_ = id
	return error(err_multiwindow_not_enabled)
}

// window_ids returns live window ids in stable app order.
pub fn (app &App) window_ids() ![]WindowId {
	_ = app
	return error(err_multiwindow_not_enabled)
}

// window_infos returns live window snapshots in stable app order.
pub fn (app &App) window_infos() ![]WindowInfo {
	_ = app
	return error(err_multiwindow_not_enabled)
}

// window_context returns the public render target wrapper for a live window.
pub fn (mut app App) window_context(id WindowId) !WindowContext {
	_ = app
	_ = id
	return error(err_multiwindow_not_enabled)
}

// destroy_window destroys a live window.
pub fn (mut app App) destroy_window(id WindowId) ! {
	_ = app
	_ = id
	return error(err_multiwindow_not_enabled)
}

// window_exists reports whether id still refers to a live window.
pub fn (app &App) window_exists(id WindowId) bool {
	_ = app
	_ = id
	return false
}

// capabilities reports the active app backend contract.
pub fn (app &App) capabilities() Capabilities {
	return Capabilities{
		backend: app.config.backend
	}
}

// drain_events returns and clears pending window lifecycle events.
pub fn (mut app App) drain_events() ![]WindowEvent {
	_ = app
	return error(err_multiwindow_not_enabled)
}

// drain_input_events returns and clears pending window-scoped input events.
pub fn (mut app App) drain_input_events() ![]WindowInputEvent {
	_ = app
	return error(err_multiwindow_not_enabled)
}

// poll_events lets the backend route native lifecycle/input events into the gg.App queue.
pub fn (mut app App) poll_events() !int {
	_ = app
	return error(err_multiwindow_not_enabled)
}

// post submits a short callback to be executed when owner-side work is drained.
pub fn (mut app App) post(f AppJobFn) ! {
	_ = app
	_ = f
	return error(err_multiwindow_not_enabled)
}

// try_post submits a short callback without waiting for queue capacity.
pub fn (mut app App) try_post(f AppJobFn) ! {
	_ = app
	_ = f
	return error(err_multiwindow_not_enabled)
}

// drain_pending executes up to max_jobs queued owner-side callbacks.
pub fn (mut app App) drain_pending(max_jobs int) !int {
	_ = app
	_ = max_jobs
	return error(err_multiwindow_not_enabled)
}

// run starts the multi-window owner loop. event_fn can drive lifecycle-only
// apps without a renderer; frame_fn/draw_window require explicit swapchains.
pub fn (mut app App) run(config RunConfig) ! {
	_ = app
	_ = config
	return error(err_multiwindow_not_enabled)
}

// draw_window renders one live window through its WindowContext.
pub fn (mut app App) draw_window(id WindowId, draw WindowDrawFn) ! {
	_ = app
	_ = id
	_ = draw
	return error(err_multiwindow_not_enabled)
}

// stop shuts down the app and destroys live windows.
pub fn (mut app App) stop() ! {
	_ = app
	return error(err_multiwindow_not_enabled)
}

// window_id returns the id routed to this draw context.
pub fn (context &WindowContext) window_id() WindowId {
	_ = context
	return WindowId{}
}

// exists reports whether this context still targets a live window.
pub fn (context &WindowContext) exists() bool {
	_ = context
	return false
}

// capabilities reports the app backend capabilities captured for this context.
pub fn (context &WindowContext) capabilities() Capabilities {
	_ = context
	return Capabilities{}
}

// framebuffer_size returns the current render target size in pixels.
pub fn (context &WindowContext) framebuffer_size() Size {
	_ = context
	return Size{}
}

// size returns the current draw size. Logical scaling will be added with native DPI routing.
pub fn (context &WindowContext) size() Size {
	return context.framebuffer_size()
}

// draw_rect_filled is available when compiling with `-d gg_multiwindow`.
pub fn (context &WindowContext) draw_rect_filled(x f32, y f32, w f32, h f32, color Color) {
	_ = context
	_ = x
	_ = y
	_ = w
	_ = h
	_ = color
}

// draw_rect_empty is available when compiling with `-d gg_multiwindow`.
pub fn (context &WindowContext) draw_rect_empty(x f32, y f32, w f32, h f32, color Color) {
	_ = context
	_ = x
	_ = y
	_ = w
	_ = h
	_ = color
}
