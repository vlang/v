// Compile with `-d gg_multiwindow` to enable the additive gg.App facade.
module gg

import x.multiwindow
import sokol.gfx
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
	id         WindowId
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

// Capabilities reports the active gg.App backend contract.
pub struct Capabilities {
pub:
	backend            MultiWindowBackend
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

// AppJobFn is executed later by app.drain_pending() on the owner side.
pub type AppJobFn = fn (mut app App) !

// AppFrameFn is a render-frame callback and requires a render-capable backend
// with explicit swapchains.
pub type AppFrameFn = fn (mut app App) !

// AppEventFn is called by App.run() for each drained multi-window event.
pub type AppEventFn = fn (event WindowEvent, mut app App) !

// WindowDrawFn records drawing commands for one WindowContext.
pub type WindowDrawFn = fn (mut window WindowContext) !

// RunConfig configures the gg.App loop. Use event_fn without frame_fn for
// lifecycle-only loops that do not require a renderer.
@[params]
pub struct RunConfig {
pub:
	frame_fn         AppFrameFn = unsafe { nil }
	event_fn         AppEventFn = unsafe { nil }
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

// poll_events lets the backend route native lifecycle events into the gg.App queue.
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
	if !has_frame_fn && !has_event_fn {
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
		dispatched_events := app.dispatch_run_events(config.event_fn)!
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

fn (mut app App) dispatch_run_events(event_fn AppEventFn) !int {
	events := app.drain_events()!
	if event_fn == unsafe { nil } {
		// Frame-only loops still need lifecycle side effects from drain_events().
		return events.len
	}
	mut dispatched := 0
	for event in events {
		event_fn(event, mut app)!
		dispatched++
		if app.core.status() != .running {
			return dispatched
		}
	}
	return dispatched
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
		backend:            backend_from_core(caps.backend)
		mock:               caps.mock
		native:             caps.native
		multi_window:       caps.multi_window
		owner_queue:        caps.owner_queue
		explicit_swapchain: caps.explicit_swapchain
		readback:           caps.readback
		d3d11:              caps.d3d11
		metal:              caps.metal
		x11:                caps.x11
		wayland:            caps.wayland
		win32:              caps.win32
		gl:                 caps.gl
	}
}

fn window_id_from_core(id multiwindow.WindowId) WindowId {
	return WindowId{
		core: id
	}
}

fn window_info_from_core(info multiwindow.WindowInfo) WindowInfo {
	return WindowInfo{
		id:         window_id_from_core(info.id)
		title:      info.title
		width:      info.width
		height:     info.height
		min_width:  info.min_width
		min_height: info.min_height
		resizable:  info.resizable
		visible:    info.visible
		high_dpi:   info.high_dpi
		borderless: info.borderless
		fullscreen: info.fullscreen
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

fn event_kind_from_core(kind multiwindow.EventKind) WindowEventKind {
	return match kind {
		.window_created { .window_created }
		.window_destroyed { .window_destroyed }
		.window_close_requested { .window_close_requested }
		.window_resized { .window_resized }
	}
}
