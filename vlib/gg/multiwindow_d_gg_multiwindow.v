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
const err_multiwindow_app_identity_mismatch = 'gg.multiwindow: handle belongs to a different app instance'
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
	app_instance u64
	core         multiwindow.WindowId
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
	title        string = 'A GG Window'
	width        int    = 800
	height       int    = 600
	min_width    int
	min_height   int
	resizable    bool = true
	visible      bool = true
	high_dpi     bool = true
	borderless   bool
	fullscreen   bool
	clear_color  Color = Color{
		a: 0
	}
	sample_count int              = 1
	redraw_mode  WindowRedrawMode = .on_demand
	init_fn      WindowInitFn     = unsafe { nil }
	frame_fn     WindowFrameFn    = unsafe { nil }
	cleanup_fn   WindowCleanupFn  = unsafe { nil }
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
	frame_fn                AppFrameFn           = unsafe { nil }
	event_fn                AppEventFn           = unsafe { nil }
	input_fn                AppInputFn           = unsafe { nil }
	app_resource_init_fn    AppResourceInitFn    = unsafe { nil }
	app_resource_frame_fn   AppResourceFrameFn   = unsafe { nil }
	app_resource_cleanup_fn AppResourceCleanupFn = unsafe { nil }
	readback_fn             WindowReadbackFn     = unsafe { nil }
	max_pending_jobs        int                  = 64
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
	core                      &multiwindow.App = unsafe { nil }
	app_instance              u64
	owner_thread_id           u64
	gfx_started               bool
	sgl_initialized           bool
	render_owner_claimed      bool
	sgl_contexts              map[string]sgl.Context
	sgl_context_targets       map[string]WindowRenderTargetInfo
	window_sgl_targets        map[string]string
	deferred_sgl_targets      map[string]u64
	render_runtime            &MultiWindowRenderRuntime = unsafe { nil }
	legacy_render_mode        bool
	app_frame_active          bool
	active_render_snapshots   map[string]multiwindow.RenderWindowSnapshot
	active_drawn_windows      map[string]bool
	active_batch_epoch        u64
	active_batch_lease        multiwindow.RenderBatchLease
	renderer_terminal_failure bool
	terminal_error            string
	failed_init_windows       []WindowId
}

// new_app creates a gg multi-window application.
pub fn new_app(config AppConfig) !&App {
	mut core := multiwindow.new_app(config.to_core())!
	return &App{
		config:               config
		core:                 core
		app_instance:         core.instance_id()
		owner_thread_id:      sync.thread_id()
		render_runtime:       new_multiwindow_render_runtime(core.instance_id())
		sgl_contexts:         map[string]sgl.Context{}
		sgl_context_targets:  map[string]WindowRenderTargetInfo{}
		window_sgl_targets:   map[string]string{}
		deferred_sgl_targets: map[string]u64{}
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
	app.assert_owner_thread()!
	if config.sample_count != 1 && (app.config.require_renderer
		|| config.init_fn != unsafe { nil } || config.frame_fn != unsafe { nil }) {
		return error(err_multiwindow_render_sample_count_unsupported)
	}
	app.core.ensure_event_admission_for_gg()!
	if config.init_fn != unsafe { nil } || config.frame_fn != unsafe { nil } {
		app.ensure_render_initialized()!
	}
	id := app.core.create_window(config.to_core_with_workload(app.legacy_render_mode))!
	window_id := WindowId{
		app_instance: app.app_instance
		core:         id
	}
	app.render_runtime.register_window(window_id, config) or {
		app.core.destroy_window(id) or {}
		return err
	}
	return window_id
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

// destroy_window destroys a live window.
pub fn (mut app App) destroy_window(id WindowId) ! {
	app.destroy_window_managed(id, .requested)!
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
	count := app.core.poll_events()!
	app.consume_backend_teardowns()!
	return count
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
	app.run_managed(config)!
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
	app.draw_window_managed(id, draw)!
}

// stop shuts down the app and destroys live windows.
pub fn (mut app App) stop() ! {
	app.stop_managed()!
}

// window_id returns the id routed to this draw context.
pub fn (context &WindowContext) window_id() WindowId {
	context.validate_managed_or_panic()
	return context.info.window
}

// exists reports whether this context still targets a live window.
pub fn (context &WindowContext) exists() bool {
	if context.app == unsafe { nil } {
		return false
	}
	context.app.render_runtime.validate_frame_lease(context.info.window, context.lease_epoch) or {
		return false
	}
	return context.app.window_exists(context.info.window)
}

// capabilities reports the app backend capabilities captured for this context.
pub fn (context &WindowContext) capabilities() Capabilities {
	context.validate_managed_or_panic()
	return context.compatibility_capabilities
}

// framebuffer_size returns the current render target size in pixels.
pub fn (context &WindowContext) framebuffer_size() Size {
	context.validate_managed_or_panic()
	return Size{
		width:  context.info.metrics.framebuffer_size.width
		height: context.info.metrics.framebuffer_size.height
	}
}

// size permanently preserves the compatibility framebuffer-pixel contract.
pub fn (context &WindowContext) size() Size {
	return context.framebuffer_size()
}

// draw_rect_filled draws a filled rectangle into this window's current draw frame.
// Coordinates are in window framebuffer pixels with top-left origin.
pub fn (context &WindowContext) draw_rect_filled(x f32, y f32, w f32, h f32, color Color) {
	if context.app == unsafe { nil } {
		panic(err_multiwindow_render_stale_lease)
	}
	mut managed := context.app.render_runtime.active_sgl_context(context.app, context.info.window,
		context.lease_epoch) or { panic(err.msg()) }
	managed.activate_sgl_managed_or_panic()
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
	if context.app == unsafe { nil } {
		panic(err_multiwindow_render_stale_lease)
	}
	mut managed := context.app.render_runtime.active_sgl_context(context.app, context.info.window,
		context.lease_epoch) or { panic(err.msg()) }
	managed.activate_sgl_managed_or_panic()
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
		facade_app.render_runtime.begin_user_callback()
		mut callback_error := IError(none)
		f(mut facade_app) or { callback_error = err }
		facade_app.render_runtime.end_user_callback()
		facade_app.flush_deferred_transitions()!
		if callback_error !is none {
			return callback_error
		}
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

fn (mut app App) latch_renderer_terminal_failure() {
	app.renderer_terminal_failure = true
}

fn (mut app App) latch_renderer_terminal_failure_if_unusable() {
	if app.gfx_started && !app.core.renderer_is_usable() {
		app.latch_renderer_terminal_failure()
	}
}

fn (mut app App) ensure_render_initialized() ! {
	app.ensure_initialized()!
	app.ensure_explicit_swapchain()!
	app.render_runtime.validate_backend_sample_counts(1)!
	app.ensure_render_owner()!
	if app.gfx_started {
		return
	}
	renderer_info := app.core.start_renderer(multiwindow.RendererConfig{
		image_pool_size:       2048
		pipeline_pool_size:    512
		attachments_pool_size: 512
	}) or {
		app.latch_renderer_terminal_failure()
		app.release_render_owner()
		return err
	}
	if !app.core.renderer_device_available_for_gg() {
		app.latch_renderer_terminal_failure()
		gg_poison_gfx_render_owner(.multiwindow_app, app.owner_token())
		app.core.abandon_renderer_for_gg() or {
			app.release_render_owner()
			return error('${err_multiwindow_render_cleanup_failed}: ${err.msg()}')
		}
		app.release_render_owner()
		return error(err_multiwindow_render_backend_unavailable)
	}
	sgl_desc := sgl.Desc{
		context_pool_size: 64
		color_format:      unsafe { gfx.PixelFormat(renderer_info.color_format) }
		depth_format:      unsafe { gfx.PixelFormat(renderer_info.depth_format) }
		sample_count:      renderer_info.sample_count
	}
	mut setup_errors := []string{}
	if !app.core.renderer_device_available_for_gg() {
		setup_errors << err_multiwindow_render_backend_unavailable
	} else if message := app.render_runtime.take_internal_fault(.renderer_sgl_setup) {
		setup_errors << message
	} else {
		sgl.setup(&sgl_desc)
		app.sgl_initialized = true
		if !app.core.renderer_device_available_for_gg() {
			setup_errors << err_multiwindow_render_backend_unavailable
		} else if sgl.error() != .no_error {
			setup_errors << err_multiwindow_render_resource_failed
		}
	}
	if setup_errors.len > 0 {
		app.latch_renderer_terminal_failure()
		mut device_available := app.core.renderer_device_available_for_gg()
		if device_available {
			app.core.prepare_renderer_shutdown_for_gg() or {
				setup_errors << err.msg()
				device_available = false
			}
			if device_available {
				device_available = app.core.renderer_device_available_for_gg()
			}
		}
		if device_available {
			if app.sgl_initialized {
				sgl.shutdown()
				app.sgl_initialized = false
			}
			app.core.shutdown_renderer() or { setup_errors << err.msg() }
		} else {
			app.sgl_initialized = false
			gg_poison_gfx_render_owner(.multiwindow_app, app.owner_token())
			app.core.abandon_renderer_for_gg() or { setup_errors << err.msg() }
		}
		app.release_render_owner()
		return error('${err_multiwindow_render_cleanup_failed}: ${setup_errors.join('; ')}')
	}
	app.gfx_started = true
}

fn (mut app App) ensure_sgl_context(key string, target WindowRenderTargetInfo) !sgl.Context {
	if !app.gfx_started || !app.core.renderer_is_usable() {
		return error(err_multiwindow_render_backend_unavailable)
	}
	if key in app.sgl_contexts {
		if key !in app.sgl_context_targets || app.sgl_context_targets[key] != target {
			return error(err_multiwindow_render_backend_unavailable)
		}
		return app.sgl_contexts[key]
	}
	if key !in app.sgl_contexts {
		previous := sgl.get_context()
		app.note_managed_gpu_work(app.active_batch_epoch)!
		created := sgl.make_context(&sgl.ContextDesc{
			color_format: target.color_format
			depth_format: target.depth_format
			sample_count: target.sample_count
		})
		if !app.core.renderer_device_available_for_gg() {
			return error(err_multiwindow_render_backend_unavailable)
		}
		if sgl.context_error(created) != .no_error {
			sgl.destroy_context(created)
			sgl.set_context(previous)
			return error(err_multiwindow_render_resource_failed)
		}
		sgl.set_context(previous)
		app.sgl_contexts[key] = created
		app.sgl_context_targets[key] = target
	}
	return app.sgl_contexts[key]
}

fn (mut app App) ensure_window_sgl_context(id WindowId, key string, target WindowRenderTargetInfo, batch u64) !sgl.Context {
	context := app.ensure_sgl_context(key, target)!
	window_key := id.str()
	if window_key in app.window_sgl_targets {
		previous := app.window_sgl_targets[window_key]
		if previous != '' && previous != key {
			app.defer_sgl_target_retirement(previous, batch)
		}
	}
	app.window_sgl_targets[window_key] = key
	return context
}

fn (mut app App) defer_sgl_target_retirement(key string, batch u64) {
	if key == '' || batch == 0 {
		return
	}
	if key !in app.deferred_sgl_targets || batch < app.deferred_sgl_targets[key] {
		app.deferred_sgl_targets[key] = batch
	}
}

fn (mut app App) defer_attachment_sgl_target_retirement(key MultiWindowResourceKey, target_identity u64, batch u64) {
	marker := ':offscreen:${key.app_instance}:${key.slot}:${key.generation}:${target_identity}:'
	for target_key, _ in app.sgl_contexts {
		if target_key.contains(marker) {
			app.defer_sgl_target_retirement(target_key, batch)
		}
	}
}

fn (mut app App) flush_deferred_sgl_targets(completed_batch u64, force bool) {
	mut keys := []string{}
	for key, batch in app.deferred_sgl_targets {
		if force || batch <= completed_batch {
			keys << key
		}
	}
	for key in keys {
		app.discard_sgl_context_key(key)
		app.deferred_sgl_targets.delete(key)
	}
}

fn (app &App) managed_sgl_context(key string) !sgl.Context {
	if !app.gfx_started || !app.core.renderer_is_usable() {
		return error(err_multiwindow_render_backend_unavailable)
	}
	if key !in app.sgl_contexts {
		return error(err_multiwindow_render_backend_unavailable)
	}
	return app.sgl_contexts[key]
}

fn (mut app App) discard_window_sgl_context(id WindowId) {
	prefix := '${app.app_instance}:${id.str()}:'
	mut keys := []string{}
	for key, _ in app.sgl_contexts {
		if key.starts_with(prefix) {
			keys << key
		}
	}
	for key in keys {
		app.discard_sgl_context_key(key)
		app.deferred_sgl_targets.delete(key)
	}
	app.window_sgl_targets.delete(id.str())
}

fn (app &App) has_window_sgl_context(id WindowId) bool {
	prefix := '${app.app_instance}:${id.str()}:'
	for key, _ in app.sgl_contexts {
		if key.starts_with(prefix) {
			return true
		}
	}
	return false
}

fn (mut app App) discard_sgl_context_key(key string) {
	if key !in app.sgl_contexts {
		return
	}
	context := app.sgl_contexts[key]
	mut sokol_available := app.gfx_started && app.core.renderer_device_available_for_gg()
	if sokol_available {
		for {
			materialization := app.render_runtime.take_sgl_materialization_for_target(key) or {
				break
			}
			if !app.core.renderer_device_available_for_gg() {
				sokol_available = false
				break
			}
			sgl.destroy_pipeline(materialization.pipeline)
		}
	}
	if sokol_available {
		sokol_available = app.core.renderer_device_available_for_gg()
	}
	if sokol_available {
		previous := sgl.get_context()
		sgl.set_context(sgl.default_context())
		sgl.destroy_context(context)
		if previous != context {
			sgl.set_context(previous)
		}
	}
	if !sokol_available {
		app.render_runtime.abandon_sgl_materializations_for_target(key)
	}
	app.sgl_contexts.delete(key)
	app.sgl_context_targets.delete(key)
}

fn (mut app App) shutdown_renderer() ! {
	mut errors := []string{}
	if app.gfx_started {
		mut device_available := app.core.renderer_device_available_for_gg()
		if !device_available {
			app.record_multiwindow_lifecycle_milestone(.core_renderer_device_unavailable)
		}
		if device_available {
			app.record_multiwindow_lifecycle_milestone(.prepare_enter)
			mut prepare_failed := false
			app.core.prepare_renderer_shutdown_for_gg() or {
				prepare_failed = true
				errors << err.msg()
				device_available = false
			}
			app.record_multiwindow_lifecycle_milestone(if prepare_failed {
				.prepare_failed
			} else {
				.prepare_complete
			})
			if device_available {
				device_available = app.core.renderer_device_available_for_gg()
			}
		}
		if device_available {
			// The backend anchor is current for the complete global shutdown.
			// sgl_shutdown owns all managed SGL contexts and precedes gfx shutdown.
			if app.sgl_initialized {
				app.record_multiwindow_lifecycle_milestone(.sgl_shutdown_enter)
				sgl.shutdown()
				app.sgl_initialized = false
				app.record_multiwindow_lifecycle_milestone(.sgl_shutdown_complete)
			}
			app.record_multiwindow_lifecycle_milestone(.core_renderer_shutdown_enter)
			mut renderer_shutdown_failed := false
			app.core.shutdown_renderer() or {
				renderer_shutdown_failed = true
				errors << err.msg()
			}
			app.record_multiwindow_lifecycle_milestone(if renderer_shutdown_failed {
				.core_renderer_shutdown_failed
			} else {
				.core_renderer_shutdown_complete
			})
		} else {
			app.sgl_initialized = false
			gg_poison_gfx_render_owner(.multiwindow_app, app.owner_token())
			app.record_multiwindow_lifecycle_milestone(.abandon_enter)
			mut abandon_failed := false
			app.core.abandon_renderer_for_gg() or {
				abandon_failed = true
				errors << err.msg()
			}
			app.record_multiwindow_lifecycle_milestone(if abandon_failed {
				.abandon_failed
			} else {
				.abandon_complete
			})
		}
		app.sgl_contexts.clear()
		app.sgl_context_targets.clear()
		app.window_sgl_targets.clear()
		app.deferred_sgl_targets.clear()
		app.gfx_started = false
	} else {
		app.record_multiwindow_lifecycle_milestone(.renderer_not_started)
		app.sgl_initialized = false
		app.sgl_contexts.clear()
		app.sgl_context_targets.clear()
		app.window_sgl_targets.clear()
		app.deferred_sgl_targets.clear()
	}
	app.release_render_owner()
	if errors.len > 0 {
		return error('${err_multiwindow_render_cleanup_failed}: ${errors.join('; ')}')
	}
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
	return config.to_core_with_workload(false)
}

fn (config WindowConfig) to_core_with_workload(legacy_render_mode bool) multiwindow.WindowConfig {
	return multiwindow.WindowConfig{
		title:           config.title
		width:           config.width
		height:          config.height
		min_width:       config.min_width
		min_height:      config.min_height
		resizable:       config.resizable
		visible:         config.visible
		high_dpi:        config.high_dpi
		borderless:      config.borderless
		fullscreen:      config.fullscreen
		sample_count:    config.sample_count
		redraw_mode:     redraw_mode_to_core(config.redraw_mode)
		render_workload: config.init_fn != unsafe { nil } || config.frame_fn != unsafe { nil }
			|| legacy_render_mode
	}
}

fn redraw_mode_to_core(mode WindowRedrawMode) multiwindow.RenderRedrawMode {
	return match mode {
		.on_demand { .on_demand }
		.continuous { .continuous }
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
		app_instance: id.app_instance_for_gg()
		core:         id
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
