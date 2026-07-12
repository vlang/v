// Compile with `-d gg_multiwindow` to enable the additive gg.App facade.
module gg

import x.multiwindow
import sokol.gfx
import sokol.sapp
import sokol.sgl
import sync
import time

const err_multiwindow_nil_job = 'gg.multiwindow: job function is nil'
const err_multiwindow_nil_run_fn = 'gg.multiwindow: frame or event function is required'
const err_multiwindow_nil_draw_fn = 'gg.multiwindow: draw function is nil'
const err_multiwindow_window_not_found = 'gg.multiwindow: window does not exist'
const err_multiwindow_renderer_unsupported = 'gg.multiwindow: renderer is not supported by the selected backend'
const err_multiwindow_app_not_initialized = 'gg.multiwindow: App is not initialized; use gg.new_app()'
const err_multiwindow_owner_thread_required = 'multiwindow: operation requires the owner thread'
const multiwindow_event_idle_sleep = 8 * time.millisecond

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
pub struct WindowId {
	core multiwindow.WindowId
}

// str returns a diagnostic representation of a WindowId.
pub fn (id WindowId) str() string {
	return id.core.str()
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
pub struct WindowContext {
	id                 WindowId
	app                &App = unsafe { nil }
	capabilities       Capabilities
	framebuffer_width  int
	framebuffer_height int
	sgl_context        sgl.Context
}

// App is the additive multi-window facade for gg users.
//
// Native handles, window registry, lifecycle and owner queue are managed by
// the lower-level core; gg.App exposes the public gg-facing API.
@[heap]
pub struct App {
pub:
	config AppConfig
mut:
	core                 &multiwindow.App = unsafe { nil }
	owner_thread_id      u64
	gfx_started          bool
	render_owner_claimed bool
	render_environment   gfx.Environment
	sgl_contexts         map[string]sgl.Context
}

// new_app creates a gg multi-window application.
pub fn new_app(config AppConfig) !&App {
	mut core := multiwindow.new_app(config.to_core())!
	return &App{
		config:          config
		core:            core
		owner_thread_id: sync.thread_id()
	}
}

// capabilities_for_backend reports capabilities for a backend policy.
pub fn capabilities_for_backend(backend MultiWindowBackend) !Capabilities {
	return capabilities_from_core(multiwindow.capabilities_for_backend(backend_to_core(backend))!)
}

// capabilities_for_backend_with_renderer reports capabilities while requiring
// renderer startup, matching `gg.new_app(require_renderer: true)`.
pub fn capabilities_for_backend_with_renderer(backend MultiWindowBackend) !Capabilities {
	return capabilities_from_core(multiwindow.capabilities_for_backend_with_renderer(backend_to_core(backend),
		true)!)
}

// create_window creates a window managed by this app.
pub fn (mut app App) create_window(config WindowConfig) !WindowId {
	app.ensure_initialized()!
	id := app.core.create_window(config.to_core())!
	return WindowId{
		core: id
	}
}

// set_window_title updates a live window title.
pub fn (mut app App) set_window_title(id WindowId, title string) ! {
	app.ensure_initialized()!
	app.core.set_window_title(id.core, title)!
}

// resize_window requests a live window resize.
pub fn (mut app App) resize_window(id WindowId, width int, height int) ! {
	app.ensure_initialized()!
	app.core.resize_window(id.core, width, height)!
}

// set_window_cursor updates the native hover cursor for a live window.
pub fn (mut app App) set_window_cursor(id WindowId, shape WindowCursorShape) ! {
	app.ensure_initialized()!
	app.core.set_window_cursor(id.core, window_cursor_shape_to_core(shape))!
}

// begin_window_move starts a user-driven native move for a live window.
pub fn (mut app App) begin_window_move(id WindowId) ! {
	app.ensure_initialized()!
	app.core.begin_window_move(id.core)!
}

// begin_window_resize starts a user-driven native resize for a live resizable window.
pub fn (mut app App) begin_window_resize(id WindowId, edge WindowResizeEdge) ! {
	app.ensure_initialized()!
	app.core.begin_window_resize(id.core, window_resize_edge_to_core(edge))!
}

// window_info returns a snapshot of a live window.
pub fn (app &App) window_info(id WindowId) !WindowInfo {
	app.ensure_initialized()!
	return window_info_from_core(app.core.window_info(id.core)!)
}

// window_ids returns live window ids in stable app order.
pub fn (app &App) window_ids() ![]WindowId {
	app.ensure_initialized()!
	core_ids := app.core.window_ids()!
	mut ids := []WindowId{cap: core_ids.len}
	for id in core_ids {
		ids << window_id_from_core(id)
	}
	return ids
}

// window_infos returns live window snapshots in stable app order.
pub fn (app &App) window_infos() ![]WindowInfo {
	app.ensure_initialized()!
	core_infos := app.core.window_infos()!
	mut infos := []WindowInfo{cap: core_infos.len}
	for info in core_infos {
		infos << window_info_from_core(info)
	}
	return infos
}

// window_context returns the public render target wrapper for a live window.
pub fn (mut app App) window_context(id WindowId) !WindowContext {
	app.ensure_initialized()!
	if !app.window_exists(id) {
		return error(err_multiwindow_window_not_found)
	}
	return WindowContext{
		id:           id
		app:          app
		capabilities: app.capabilities()
	}
}

// destroy_window destroys a live window.
pub fn (mut app App) destroy_window(id WindowId) ! {
	app.ensure_initialized()!
	app.core.destroy_window(id.core)!
	app.discard_window_sgl_context(id)
}

// window_exists reports whether id still refers to a live window.
pub fn (app &App) window_exists(id WindowId) bool {
	if !app.initialized() {
		return false
	}
	return app.core.window_exists(id.core)
}

// capabilities reports the active app backend contract.
pub fn (app &App) capabilities() Capabilities {
	if !app.initialized() {
		return Capabilities{}
	}
	return capabilities_from_core(app.core.capabilities())
}

// drain_events returns and clears pending window lifecycle events.
pub fn (mut app App) drain_events() ![]WindowEvent {
	app.ensure_initialized()!
	core_events := app.core.drain_events()!
	mut events := []WindowEvent{cap: core_events.len}
	for event in core_events {
		window_event := window_event_from_core(event)
		if window_event.kind == .window_destroyed {
			app.discard_window_sgl_context(window_event.window)
		}
		events << window_event
	}
	return events
}

// drain_input_events returns and clears pending window-scoped input events.
pub fn (mut app App) drain_input_events() ![]WindowInputEvent {
	app.ensure_initialized()!
	core_events := app.core.drain_input_events()!
	mut events := []WindowInputEvent{cap: core_events.len}
	for event in core_events {
		events << window_input_event_from_core(event)
	}
	return events
}

// poll_events lets the backend route native lifecycle/input events into the gg.App queue.
pub fn (mut app App) poll_events() !int {
	app.ensure_initialized()!
	return app.core.poll_events()!
}

// post submits a short callback to be executed when owner-side work is drained.
pub fn (mut app App) post(f AppJobFn) ! {
	app.ensure_initialized()!
	if f == unsafe { nil } {
		return error(err_multiwindow_nil_job)
	}
	app.core.post(app.wrap_job(f))!
}

// try_post submits a short callback without waiting for queue capacity.
pub fn (mut app App) try_post(f AppJobFn) ! {
	app.ensure_initialized()!
	if f == unsafe { nil } {
		return error(err_multiwindow_nil_job)
	}
	app.core.try_post(app.wrap_job(f))!
}

// drain_pending executes up to max_jobs queued owner-side callbacks.
pub fn (mut app App) drain_pending(max_jobs int) !int {
	app.ensure_initialized()!
	return app.core.drain_pending(max_jobs)!
}

// run starts the multi-window owner loop. event_fn can drive lifecycle-only
// apps without a renderer; frame_fn/draw_window require explicit swapchains.
pub fn (mut app App) run(config RunConfig) ! {
	app.ensure_initialized()!
	has_frame_fn := config.frame_fn != unsafe { nil }
	has_event_fn := config.event_fn != unsafe { nil }
	has_input_fn := config.input_fn != unsafe { nil }
	if !has_frame_fn && !has_event_fn && !has_input_fn {
		return error(err_multiwindow_nil_run_fn)
	}
	renderer_available := app.capabilities().explicit_swapchain
	if has_frame_fn && !renderer_available {
		return error(err_multiwindow_renderer_unsupported)
	}
	if has_frame_fn {
		app.ensure_render_owner()!
	}
	defer {
		app.shutdown_renderer()
	}
	for app.core.status() == .running {
		polled_events := app.poll_events()!
		mut drained_jobs := 0
		if config.max_pending_jobs > 0 {
			drained_jobs = app.core.drain_pending(config.max_pending_jobs) or {
				if app.core.status() != .running {
					break
				}
				return err
			}
		}
		if app.core.status() != .running {
			break
		}
		dispatched_events := app.dispatch_run_events(config.event_fn, config.input_fn)!
		if app.core.status() != .running {
			break
		}
		if has_frame_fn {
			config.frame_fn(mut app)!
		} else if polled_events == 0 && drained_jobs == 0 && dispatched_events == 0 {
			time.sleep(multiwindow_event_idle_sleep)
		}
	}
}

fn (mut app App) dispatch_run_events(event_fn AppEventFn, input_fn AppInputFn) !int {
	events := app.core.drain_queued_events()!
	mut dispatched := 0
	for event in events {
		match event.kind {
			.lifecycle {
				window_event := window_event_from_core(event.lifecycle)
				if window_event.kind == .window_destroyed {
					app.discard_window_sgl_context(window_event.window)
				}
				if event_fn != unsafe { nil } {
					event_fn(window_event, mut app)!
				} else {
					app.dispatch_lifecycle_without_event_callback(window_event)!
				}
			}
			.input {
				if input_fn != unsafe { nil } {
					input_fn(window_input_event_from_core(event.input), mut app)!
				}
			}
		}

		dispatched++
		if app.core.status() != .running {
			return dispatched
		}
	}
	return dispatched
}

fn (mut app App) dispatch_lifecycle_without_event_callback(event WindowEvent) ! {
	match event.kind {
		.window_close_requested {
			if app.window_exists(event.window) {
				app.destroy_window(event.window)!
			}
			app.stop_if_no_windows()!
		}
		.window_destroyed {
			app.stop_if_no_windows()!
		}
		else {}
	}
}

fn (mut app App) stop_if_no_windows() ! {
	if app.core.status() == .running && app.window_ids()!.len == 0 {
		app.stop()!
	}
}

// draw_window renders one live window through its WindowContext.
pub fn (mut app App) draw_window(id WindowId, draw WindowDrawFn) ! {
	if draw == unsafe { nil } {
		return error(err_multiwindow_nil_draw_fn)
	}
	app.ensure_initialized()!
	app.ensure_render_initialized(id)!
	frame := app.core.begin_render(id.core)!
	mut context := app.window_context_for_frame(id, frame)
	sgl.set_context(context.sgl_context)
	sgl.defaults()
	sgl.matrix_mode_projection()
	sgl.ortho(0.0, f32(context.framebuffer_width), f32(context.framebuffer_height), 0.0, -1.0, 1.0)
	draw(mut context) or {
		app.discard_window_sgl_context(id)
		app.core.abort_render(frame) or {}
		return err
	}
	pass := gfx.Pass{
		action:    gfx.create_clear_pass_action(0.0, 0.0, 0.0, 1.0)
		swapchain: frame.swapchain
	}
	gfx.begin_pass(pass)
	sgl.context_draw(context.sgl_context)
	gfx.end_pass()
	gfx.commit()
	app.core.end_render(frame)!
}

// stop shuts down the app and destroys live windows.
pub fn (mut app App) stop() ! {
	app.ensure_initialized()!
	app.assert_owner_thread()!
	app.shutdown_renderer()
	app.core.stop()!
}

// window_id returns the id routed to this draw context.
pub fn (context &WindowContext) window_id() WindowId {
	return context.id
}

// exists reports whether this context still targets a live window.
pub fn (context &WindowContext) exists() bool {
	if context.app == unsafe { nil } {
		return false
	}
	return context.app.window_exists(context.id)
}

// capabilities reports the app backend capabilities captured for this context.
pub fn (context &WindowContext) capabilities() Capabilities {
	return context.capabilities
}

// framebuffer_size returns the current render target size in pixels.
pub fn (context &WindowContext) framebuffer_size() Size {
	return Size{
		width:  context.framebuffer_width
		height: context.framebuffer_height
	}
}

// size returns the current draw size. Logical scaling will be added with native DPI routing.
pub fn (context &WindowContext) size() Size {
	return context.framebuffer_size()
}

// draw_rect_filled draws a filled rectangle into this window's current draw frame.
// Coordinates are in window framebuffer pixels with top-left origin.
pub fn (context &WindowContext) draw_rect_filled(x f32, y f32, w f32, h f32, color Color) {
	sgl.set_context(context.sgl_context)
	sgl.c4b(color.r, color.g, color.b, color.a)
	sgl.begin_quads()
	sgl.v2f(x, y)
	sgl.v2f(x + w, y)
	sgl.v2f(x + w, y + h)
	sgl.v2f(x, y + h)
	sgl.end()
}

// draw_rect_empty draws a one-pixel outline rectangle into this window's current draw frame.
// Coordinates are in window framebuffer pixels with top-left origin.
pub fn (context &WindowContext) draw_rect_empty(x f32, y f32, w f32, h f32, color Color) {
	sgl.set_context(context.sgl_context)
	sgl.c4b(color.r, color.g, color.b, color.a)
	sgl.begin_lines()
	sgl.v2f(x, y)
	sgl.v2f(x + w, y)
	sgl.v2f(x, y)
	sgl.v2f(x, y + h)
	sgl.v2f(x + w, y)
	sgl.v2f(x + w, y + h)
	sgl.v2f(x, y + h)
	sgl.v2f(x + w, y + h)
	sgl.end()
}

fn (mut app App) wrap_job(f AppJobFn) multiwindow.AppJobFn {
	app_ptr := unsafe { voidptr(&app) }
	return fn [app_ptr, f] (mut core_app multiwindow.App) ! {
		_ = core_app
		mut facade_app := unsafe { &App(app_ptr) }
		f(mut facade_app)!
	}
}

fn (app &App) ensure_explicit_swapchain() ! {
	app.ensure_initialized()!
	caps := app.capabilities()
	if !caps.explicit_swapchain {
		return error(err_multiwindow_renderer_unsupported)
	}
}

fn (mut app App) ensure_render_owner() ! {
	if app.render_owner_claimed {
		return
	}
	gg_claim_gfx_render_owner(.multiwindow_app, app.owner_token())!
	app.render_owner_claimed = true
}

fn (mut app App) ensure_render_initialized(id WindowId) ! {
	app.ensure_initialized()!
	app.ensure_explicit_swapchain()!
	app.ensure_render_owner()!
	if app.gfx_started {
		return
	}
	app.render_environment = app.core.render_environment(id.core) or {
		app.release_render_owner()
		return err
	}
	mut desc := gfx.Desc{
		environment:     app.render_environment
		image_pool_size: 1000
	}
	gfx.setup(&desc)
	sgl_desc := sgl.Desc{
		context_pool_size: 64
		color_format:      app.render_environment.defaults.color_format
		depth_format:      app.render_environment.defaults.depth_format
		sample_count:      app.render_environment.defaults.sample_count
	}
	sgl.setup(&sgl_desc)
	app.gfx_started = true
}

fn (mut app App) window_context_for_frame(id WindowId, frame multiwindow.RenderFrame) WindowContext {
	key := id.str()
	if key !in app.sgl_contexts {
		app.sgl_contexts[key] = sgl.make_context(&sgl.ContextDesc{
			color_format: app.render_environment.defaults.color_format
			depth_format: app.render_environment.defaults.depth_format
			sample_count: app.render_environment.defaults.sample_count
		})
	}
	return WindowContext{
		id:                 id
		app:                app
		capabilities:       app.capabilities()
		framebuffer_width:  frame.swapchain.width
		framebuffer_height: frame.swapchain.height
		sgl_context:        app.sgl_contexts[key]
	}
}

fn (mut app App) discard_window_sgl_context(id WindowId) {
	key := id.str()
	if key !in app.sgl_contexts {
		return
	}
	if !app.gfx_started {
		app.sgl_contexts.delete(key)
		return
	}
	context := app.sgl_contexts[key]
	sgl.set_context(sgl.default_context())
	sgl.destroy_context(context)
	app.sgl_contexts.delete(key)
}

fn (mut app App) shutdown_renderer() {
	app.sgl_contexts.clear()
	if app.gfx_started {
		sgl.shutdown()
		gfx.shutdown()
		app.gfx_started = false
	}
	app.release_render_owner()
}

fn (mut app App) owner_token() voidptr {
	return unsafe { voidptr(&app) }
}

fn (app &App) initialized() bool {
	return app.core != unsafe { nil }
}

fn (app &App) ensure_initialized() ! {
	if !app.initialized() {
		return error(err_multiwindow_app_not_initialized)
	}
}

fn (app &App) assert_owner_thread() ! {
	if app.owner_thread_id != sync.thread_id() {
		return error(err_multiwindow_owner_thread_required)
	}
}

fn (mut app App) release_render_owner() {
	if app.render_owner_claimed {
		gg_release_gfx_render_owner(.multiwindow_app, app.owner_token())
		app.render_owner_claimed = false
	}
}

fn (config AppConfig) to_core() multiwindow.Config {
	return multiwindow.Config{
		backend:          backend_to_core(config.backend)
		queue_size:       config.queue_size
		require_renderer: config.require_renderer
	}
}

fn backend_to_core(backend MultiWindowBackend) multiwindow.BackendKind {
	return match backend {
		.auto { .auto }
		.mock { .mock }
		.x11 { .x11 }
		.wayland { .wayland }
		.appkit { .appkit }
		.win32 { .win32 }
	}
}

fn backend_from_core(backend multiwindow.BackendKind) MultiWindowBackend {
	return match backend {
		.auto { .auto }
		.mock { .mock }
		.x11 { .x11 }
		.wayland { .wayland }
		.appkit { .appkit }
		.win32 { .win32 }
	}
}

fn (config WindowConfig) to_core() multiwindow.WindowConfig {
	return multiwindow.WindowConfig{
		title:      config.title
		width:      config.width
		height:     config.height
		min_width:  config.min_width
		min_height: config.min_height
		resizable:  config.resizable
		visible:    config.visible
		high_dpi:   config.high_dpi
		borderless: config.borderless
		fullscreen: config.fullscreen
	}
}

fn capabilities_from_core(caps multiwindow.Capabilities) Capabilities {
	return Capabilities{
		backend:                 backend_from_core(caps.backend)
		mock:                    caps.mock
		native:                  caps.native
		multi_window:            caps.multi_window
		owner_queue:             caps.owner_queue
		explicit_swapchain:      caps.explicit_swapchain
		readback:                caps.readback
		d3d11:                   caps.d3d11
		metal:                   caps.metal
		x11:                     caps.x11
		wayland:                 caps.wayland
		win32:                   caps.win32
		gl:                      caps.gl
		input_events:            caps.input_events
		mouse_events:            caps.mouse_events
		keyboard_events:         caps.keyboard_events
		text_events:             caps.text_events
		focus_events:            caps.focus_events
		drop_events:             caps.drop_events
		touch_events:            caps.touch_events
		cursor_shapes:           caps.cursor_shapes
		interactive_move_resize: caps.interactive_move_resize
		native_decorations:      caps.native_decorations
	}
}

fn window_id_from_core(id multiwindow.WindowId) WindowId {
	return WindowId{
		core: id
	}
}

fn window_info_from_core(info multiwindow.WindowInfo) WindowInfo {
	return WindowInfo{
		id:                 window_id_from_core(info.id)
		title:              info.title
		width:              info.width
		height:             info.height
		min_width:          info.min_width
		min_height:         info.min_height
		resizable:          info.resizable
		visible:            info.visible
		high_dpi:           info.high_dpi
		borderless:         info.borderless
		fullscreen:         info.fullscreen
		native_decorations: info.native_decorations
	}
}

fn window_event_from_core(event multiwindow.Event) WindowEvent {
	return WindowEvent{
		kind:   event_kind_from_core(event.kind)
		window: window_id_from_core(event.window_id)
		width:  event.width
		height: event.height
	}
}

fn window_input_event_from_core(event multiwindow.InputEvent) WindowInputEvent {
	return WindowInputEvent{
		window:        window_id_from_core(event.window_id)
		event:         gg_event_from_core_input(event)
		dropped_files: event.dropped_files.clone()
	}
}

fn gg_event_from_core_input(event multiwindow.InputEvent) Event {
	mut touches := [8]TouchPoint{}
	touch_count := input_touch_count(event.num_touches)
	for i in 0 .. touch_count {
		touch := event.touches[i]
		touches[i] = TouchPoint{
			identifier:       touch.identifier
			pos_x:            touch.pos_x
			pos_y:            touch.pos_y
			android_tooltype: unsafe { sapp.TouchToolType(touch.android_tooltype) }
			changed:          touch.changed
		}
	}
	return Event{
		frame_count:        event.frame_count
		typ:                input_event_type_from_core(event.kind)
		key_code:           unsafe { KeyCode(event.key_code) }
		char_code:          event.char_code
		key_repeat:         event.key_repeat
		modifiers:          event.modifiers
		mouse_button:       mouse_button_from_core(event.mouse_button)
		mouse_x:            event.mouse_x
		mouse_y:            event.mouse_y
		mouse_dx:           event.mouse_dx
		mouse_dy:           event.mouse_dy
		scroll_x:           event.scroll_x
		scroll_y:           event.scroll_y
		num_touches:        touch_count
		touches:            touches
		window_width:       event.window_width
		window_height:      event.window_height
		framebuffer_width:  event.framebuffer_width
		framebuffer_height: event.framebuffer_height
	}
}

fn event_kind_from_core(kind multiwindow.EventKind) WindowEventKind {
	return match kind {
		.window_created { .window_created }
		.window_destroyed { .window_destroyed }
		.window_close_requested { .window_close_requested }
		.window_resized { .window_resized }
	}
}

fn window_resize_edge_to_core(edge WindowResizeEdge) multiwindow.WindowResizeEdge {
	return match edge {
		.top { .top }
		.bottom { .bottom }
		.left { .left }
		.right { .right }
		.top_left { .top_left }
		.top_right { .top_right }
		.bottom_left { .bottom_left }
		.bottom_right { .bottom_right }
	}
}

fn window_cursor_shape_to_core(shape WindowCursorShape) multiwindow.CursorShape {
	return match shape {
		.default { .default }
		.pointer { .pointer }
		.move { .move }
		.n_resize { .n_resize }
		.s_resize { .s_resize }
		.e_resize { .e_resize }
		.w_resize { .w_resize }
		.ne_resize { .ne_resize }
		.nw_resize { .nw_resize }
		.se_resize { .se_resize }
		.sw_resize { .sw_resize }
		.ew_resize { .ew_resize }
		.ns_resize { .ns_resize }
		.nesw_resize { .nesw_resize }
		.nwse_resize { .nwse_resize }
		.grab { .grab }
		.grabbing { .grabbing }
	}
}

fn input_event_type_from_core(kind multiwindow.InputEventKind) sapp.EventType {
	return match kind {
		.invalid { .invalid }
		.key_down { .key_down }
		.key_up { .key_up }
		.char { .char }
		.mouse_down { .mouse_down }
		.mouse_up { .mouse_up }
		.mouse_scroll { .mouse_scroll }
		.mouse_move { .mouse_move }
		.mouse_enter { .mouse_enter }
		.mouse_leave { .mouse_leave }
		.touches_began { .touches_began }
		.touches_moved { .touches_moved }
		.touches_ended { .touches_ended }
		.touches_cancelled { .touches_cancelled }
		.resized { .resized }
		.iconified { .iconified }
		.restored { .restored }
		.focused { .focused }
		.unfocused { .unfocused }
		.suspended { .suspended }
		.resumed { .resumed }
		.quit_requested { .quit_requested }
		.clipboard_pasted { .clipboard_pasted }
		.files_dropped { .files_dropped }
	}
}

fn mouse_button_from_core(button int) MouseButton {
	return match button {
		0 { .left }
		1 { .right }
		2 { .middle }
		else { .invalid }
	}
}

fn input_touch_count(count int) int {
	if count < 0 {
		return 0
	}
	if count > 8 {
		return 8
	}
	return count
}

$if test {
	fn input_event_to_core(event WindowInputEvent) multiwindow.InputEvent {
		gg_event := event.event
		mut touches := [8]multiwindow.InputTouchPoint{}
		touch_count := input_touch_count(gg_event.num_touches)
		for i in 0 .. touch_count {
			touch := gg_event.touches[i]
			touches[i] = multiwindow.InputTouchPoint{
				identifier:       touch.identifier
				pos_x:            touch.pos_x
				pos_y:            touch.pos_y
				android_tooltype: int(touch.android_tooltype)
				changed:          touch.changed
			}
		}
		return multiwindow.InputEvent{
			kind:               input_event_type_to_core(gg_event.typ)
			window_id:          event.window.core
			frame_count:        gg_event.frame_count
			key_code:           int(gg_event.key_code)
			char_code:          gg_event.char_code
			key_repeat:         gg_event.key_repeat
			modifiers:          gg_event.modifiers
			mouse_button:       int(gg_event.mouse_button)
			mouse_x:            gg_event.mouse_x
			mouse_y:            gg_event.mouse_y
			mouse_dx:           gg_event.mouse_dx
			mouse_dy:           gg_event.mouse_dy
			scroll_x:           gg_event.scroll_x
			scroll_y:           gg_event.scroll_y
			num_touches:        touch_count
			touches:            touches
			window_width:       gg_event.window_width
			window_height:      gg_event.window_height
			framebuffer_width:  gg_event.framebuffer_width
			framebuffer_height: gg_event.framebuffer_height
			dropped_files:      event.dropped_files.clone()
		}
	}

	fn input_event_type_to_core(typ sapp.EventType) multiwindow.InputEventKind {
		return match typ {
			.invalid { .invalid }
			.key_down { .key_down }
			.key_up { .key_up }
			.char { .char }
			.mouse_down { .mouse_down }
			.mouse_up { .mouse_up }
			.mouse_scroll { .mouse_scroll }
			.mouse_move { .mouse_move }
			.mouse_enter { .mouse_enter }
			.mouse_leave { .mouse_leave }
			.touches_began { .touches_began }
			.touches_moved { .touches_moved }
			.touches_ended { .touches_ended }
			.touches_cancelled { .touches_cancelled }
			.resized { .resized }
			.iconified { .iconified }
			.restored { .restored }
			.focused { .focused }
			.unfocused { .unfocused }
			.suspended { .suspended }
			.resumed { .resumed }
			.quit_requested { .quit_requested }
			.clipboard_pasted { .clipboard_pasted }
			.files_dropped { .files_dropped }
			else { .invalid }
		}
	}

	// enqueue_mock_input_for_test injects a window-scoped input event into the
	// mock backend without exposing x.multiwindow to user programs.
	pub fn (mut app App) enqueue_mock_input_for_test(event WindowInputEvent) ! {
		app.ensure_initialized()!
		app.core.enqueue_mock_input_for_test(input_event_to_core(event))!
	}
}
