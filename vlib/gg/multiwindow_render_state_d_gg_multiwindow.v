module gg

import sync
import sokol.gfx
import x.multiwindow

enum MultiWindowRenderWindowStatus {
	invalid
	registered
	initialized
	closing
	destroyed
}

struct MultiWindowRenderWindow {
mut:
	id                      WindowId
	status                  MultiWindowRenderWindowStatus
	clear_color             Color
	requested_sample_count  int
	redraw_mode             WindowRedrawMode
	init_fn                 WindowInitFn    = unsafe { nil }
	frame_fn                WindowFrameFn   = unsafe { nil }
	cleanup_fn              WindowCleanupFn = unsafe { nil }
	cleanup_lease_epoch     u64
	init_started            bool
	init_completed          bool
	cleanup_started         bool
	cleanup_finished        bool
	cleanup_reason          WindowCleanupReason
	cleanup_reason_set      bool
	target                  WindowRenderTargetInfo
	target_identity         u64
	target_key              string
	target_available        bool
	target_lease            multiwindow.RenderTargetLease
	active_lease_epoch      u64
	active_phase            MultiWindowRenderPhase
	resource_section_active bool
	pass_active             bool
	pass_epoch              u64
	sgl_active              bool
	sgl_flushed             bool
	swapchain_pass_used     bool
}

struct MultiWindowDeferredTransitions {
	stop    bool
	windows []WindowId
}

struct MultiWindowCleanupPlan {
	callback WindowCleanupFn = unsafe { nil }
	context  WindowCleanupContext
	run      bool
}

@[heap]
struct MultiWindowRenderRuntime {
mut:
	mutex                   &sync.Mutex = sync.new_mutex()
	app_instance            u64
	windows                 []MultiWindowRenderWindow
	resources               MultiWindowResourceRegistry
	internal_fault          MultiWindowInternalFaultPlan
	lifecycle_trace         MultiWindowLifecycleTraceState
	next_lease_epoch        u64 = 1
	next_pass_epoch         u64 = 1
	active_batch_epoch      u64
	last_completed_batch    u64
	batch_active            bool
	callback_depth          int
	deferred_windows        []WindowId
	deferred_stop           bool
	stopping                bool
	stopped                 bool
	app_lease_epoch         u64
	app_phase               MultiWindowRenderPhase
	app_resource_active     bool
	app_init_fn             AppResourceInitFn    = unsafe { nil }
	app_frame_fn            AppResourceFrameFn   = unsafe { nil }
	app_cleanup_fn          AppResourceCleanupFn = unsafe { nil }
	readback_fn             WindowReadbackFn     = unsafe { nil }
	app_init_started        bool
	app_init_completed      bool
	app_init_terminal       string
	app_cleanup_started     bool
	app_cleanup_finished    bool
	app_cleanup_lease_epoch u64
	app_resources_retired   bool
}

fn new_multiwindow_render_runtime(app_instance u64) &MultiWindowRenderRuntime {
	return &MultiWindowRenderRuntime{
		mutex:        sync.new_mutex()
		app_instance: app_instance
		resources:    new_multiwindow_resource_registry(app_instance)
	}
}

fn (mut runtime MultiWindowRenderRuntime) register_window(id WindowId, config WindowConfig) ! {
	if config.sample_count <= 0 {
		return error(err_multiwindow_render_invalid_sample_count)
	}
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	if runtime.stopping || runtime.stopped {
		return error(err_multiwindow_render_stopped)
	}
	mut window := multiwindow_render_window_from_config(id, config)
	if window.cleanup_fn != unsafe { nil } {
		window.cleanup_lease_epoch = runtime.take_lease_epoch_locked()!
	}
	for i, current in runtime.windows {
		if current.status in [.destroyed, .invalid] {
			runtime.windows[i] = window
			return
		}
	}
	runtime.windows << window
}

fn (mut runtime MultiWindowRenderRuntime) configure_run(config RunConfig) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	if config.app_resource_cleanup_fn != unsafe { nil } && runtime.app_cleanup_lease_epoch == 0 {
		runtime.app_cleanup_lease_epoch = runtime.take_lease_epoch_locked()!
	}
	runtime.app_init_fn = config.app_resource_init_fn
	runtime.app_frame_fn = config.app_resource_frame_fn
	runtime.app_cleanup_fn = config.app_resource_cleanup_fn
	runtime.readback_fn = config.readback_fn
}

fn (runtime &MultiWindowRenderRuntime) validate_redraw_admission(id WindowId) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	if runtime.stopping || runtime.stopped {
		return error(err_multiwindow_render_stopped)
	}
	index := runtime.window_index_locked(id)!
	if runtime.windows[index].status !in [.registered, .initialized] {
		return error(err_multiwindow_render_stopped)
	}
}

fn (mut runtime MultiWindowRenderRuntime) begin_batch(epoch u64) ! {
	runtime.begin_batch_for_phase(epoch, false)!
}

fn (mut runtime MultiWindowRenderRuntime) begin_teardown_batch(epoch u64) ! {
	runtime.begin_batch_for_phase(epoch, true)!
}

fn (mut runtime MultiWindowRenderRuntime) begin_batch_for_phase(epoch u64, teardown bool) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	if runtime.stopped || (!teardown && runtime.stopping) {
		return error(err_multiwindow_render_stopped)
	}
	if runtime.batch_active || epoch == 0 {
		return error(err_multiwindow_render_wrong_phase)
	}
	runtime.batch_active = true
	runtime.active_batch_epoch = epoch
}

fn (mut runtime MultiWindowRenderRuntime) finish_batch(epoch u64, committed bool, sokol_available bool) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	if !runtime.batch_active || runtime.active_batch_epoch != epoch {
		return error(err_multiwindow_render_wrong_phase)
	}
	if committed {
		runtime.last_completed_batch = epoch
		if sokol_available {
			runtime.resources.flush_retired(epoch)
		}
	}
	for i, window in runtime.windows {
		if window.target_available {
			runtime.windows[i].target_available = false
			runtime.windows[i].target_lease = multiwindow.RenderTargetLease{}
			runtime.windows[i].target_key = ''
		}
	}
	runtime.batch_active = false
	runtime.active_batch_epoch = 0
}

fn (mut runtime MultiWindowRenderRuntime) abort_active_batch() {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	for i, window in runtime.windows {
		if window.target_available {
			runtime.windows[i].target_available = false
			runtime.windows[i].target_lease = multiwindow.RenderTargetLease{}
			runtime.windows[i].target_key = ''
		}
	}
	runtime.app_lease_epoch = 0
	runtime.app_phase = .invalid
	runtime.app_resource_active = false
	runtime.batch_active = false
	runtime.active_batch_epoch = 0
}

fn (mut runtime MultiWindowRenderRuntime) attach_render_target(id WindowId, acquisition multiwindow.RenderTargetAcquisition) !WindowRenderTargetInfo {
	if acquisition.status != .ready {
		return error(err_multiwindow_render_backend_unavailable)
	}
	target := WindowRenderTargetInfo{
		color_format: unsafe { gfx.PixelFormat(acquisition.snapshot.target.color_format) }
		depth_format: unsafe { gfx.PixelFormat(acquisition.snapshot.target.depth_format) }
		sample_count: acquisition.snapshot.target.sample_count
	}
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.mutable_window_index_locked(id)!
	mut window := &runtime.windows[index]
	if target.sample_count <= 0 || target.color_format in [.none, ._default] {
		return error(err_multiwindow_render_backend_unavailable)
	}
	if window.requested_sample_count != target.sample_count {
		return error(err_multiwindow_render_sample_count_unsupported)
	}
	window.target = target
	window.target_identity = acquisition.snapshot.target.target_identity
	window.target_key = managed_swapchain_target_key(runtime.app_instance, id,
		acquisition.snapshot.target)
	window.target_available = true
	window.target_lease = acquisition.lease
	window.swapchain_pass_used = false
	window.sgl_flushed = false
	return target
}

fn (mut runtime MultiWindowRenderRuntime) prepare_target_snapshot(id WindowId, snapshot multiwindow.RenderWindowSnapshot) !WindowRenderTargetInfo {
	target := WindowRenderTargetInfo{
		color_format: unsafe { gfx.PixelFormat(snapshot.target.color_format) }
		depth_format: unsafe { gfx.PixelFormat(snapshot.target.depth_format) }
		sample_count: snapshot.target.sample_count
	}
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.mutable_window_index_locked(id)!
	mut window := &runtime.windows[index]
	if target.sample_count <= 0 || target.color_format in [.none, ._default] {
		return error(err_multiwindow_render_backend_unavailable)
	}
	if window.requested_sample_count != target.sample_count {
		return error(err_multiwindow_render_sample_count_unsupported)
	}
	window.target = target
	window.target_identity = snapshot.target.target_identity
	window.target_key = managed_swapchain_target_key(runtime.app_instance, id, snapshot.target)
	window.target_available = true
	return target
}

fn (mut runtime MultiWindowRenderRuntime) detach_render_target(id WindowId) {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.window_index_locked(id) or { return }
	runtime.windows[index].target_available = false
	runtime.windows[index].target_lease = multiwindow.RenderTargetLease{}
	runtime.windows[index].target_key = ''
}

fn (runtime &MultiWindowRenderRuntime) target_lease(id WindowId, lease_epoch u64) !multiwindow.RenderTargetLease {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.validate_window_lease_locked(id, lease_epoch, .frame)!
	window := runtime.windows[index]
	if !window.target_available {
		return error(err_multiwindow_render_backend_unavailable)
	}
	return window.target_lease
}

fn (runtime &MultiWindowRenderRuntime) target_key(id WindowId, lease_epoch u64) !string {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.validate_window_lease_locked(id, lease_epoch, .frame)!
	if !runtime.windows[index].target_available || runtime.windows[index].target_key == '' {
		return error(err_multiwindow_render_backend_unavailable)
	}
	return runtime.windows[index].target_key
}

fn (runtime &MultiWindowRenderRuntime) target_key_for_window_without_lease(id WindowId) !string {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.window_index_locked(id)!
	if !runtime.windows[index].target_available || runtime.windows[index].target_key == '' {
		return error(err_multiwindow_render_backend_unavailable)
	}
	return runtime.windows[index].target_key
}

fn (mut runtime MultiWindowRenderRuntime) set_clear_color(id WindowId, color Color) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.mutable_window_index_locked(id)!
	runtime.windows[index].clear_color = color
}

fn (runtime &MultiWindowRenderRuntime) clear_color(id WindowId) !Color {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	return runtime.windows[runtime.window_index_locked(id)!].clear_color
}

fn (runtime &MultiWindowRenderRuntime) target_info(id WindowId) !WindowRenderTargetInfo {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	window := runtime.windows[runtime.window_index_locked(id)!]
	if !window.target_available && window.target.sample_count <= 0 {
		return error(err_multiwindow_render_backend_unavailable)
	}
	return window.target
}

fn (runtime &MultiWindowRenderRuntime) has_per_window_frame_callbacks() bool {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	for window in runtime.windows {
		if window.status in [.registered, .initialized] && window.frame_fn != unsafe { nil } {
			return true
		}
	}
	return false
}

fn (runtime &MultiWindowRenderRuntime) has_pending_window_initializers() bool {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	for window in runtime.windows {
		if window.status == .registered && !window.init_started && window.init_fn != unsafe { nil } {
			return true
		}
	}
	return false
}

fn (runtime &MultiWindowRenderRuntime) validate_backend_sample_counts(sample_count int) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	for window in runtime.windows {
		if window.status in [.registered, .initialized]
			&& window.requested_sample_count != sample_count {
			return error(err_multiwindow_render_sample_count_unsupported)
		}
	}
}

fn (runtime &MultiWindowRenderRuntime) window_callbacks(id WindowId) !(WindowInitFn, WindowFrameFn) {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	window := runtime.windows[runtime.window_index_locked(id)!]
	return window.init_fn, window.frame_fn
}

fn (runtime &MultiWindowRenderRuntime) window_initialized(id WindowId) !bool {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	return runtime.windows[runtime.window_index_locked(id)!].init_completed
}

fn (mut runtime MultiWindowRenderRuntime) begin_init_lease(app &App, info WindowFrameInfo) !WindowInitContext {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.mutable_window_index_locked(info.window)!
	mut window := &runtime.windows[index]
	if window.init_started || window.active_lease_epoch != 0 || !window.target_available {
		return error(err_multiwindow_render_wrong_phase)
	}
	window.init_started = true
	window.active_lease_epoch = runtime.take_lease_epoch_locked()!
	window.active_phase = .init
	return WindowInitContext{
		app:          app
		app_instance: runtime.app_instance
		lease_epoch:  window.active_lease_epoch
		info:         info
	}
}

fn (mut runtime MultiWindowRenderRuntime) finish_init_lease(id WindowId, lease_epoch u64, succeeded bool) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.validate_window_lease_locked(id, lease_epoch, .init)!
	mut window := &runtime.windows[index]
	window.active_lease_epoch = 0
	window.active_phase = .invalid
	window.resource_section_active = false
	if succeeded {
		window.init_completed = true
		window.status = .initialized
	} else {
		window.status = .closing
	}
}

fn (mut runtime MultiWindowRenderRuntime) mark_initialized_without_callback(id WindowId) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.mutable_window_index_locked(id)!
	runtime.windows[index].init_started = true
	runtime.windows[index].init_completed = true
	runtime.windows[index].status = .initialized
}

fn (mut runtime MultiWindowRenderRuntime) begin_frame_lease(app &App, info WindowFrameInfo) !WindowContext {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.window_index_locked(info.window)!
	mut window := &runtime.windows[index]
	if window.status != .initialized || !window.init_completed || !window.target_available
		|| window.active_lease_epoch != 0 {
		return error(err_multiwindow_render_wrong_phase)
	}
	window.active_lease_epoch = runtime.take_lease_epoch_locked()!
	window.active_phase = .frame
	window.resource_section_active = false
	window.pass_active = false
	window.sgl_active = false
	window.sgl_flushed = false
	window.swapchain_pass_used = false
	return WindowContext{
		app:                        app
		app_instance:               runtime.app_instance
		lease_epoch:                window.active_lease_epoch
		info:                       info
		compatibility_capabilities: app.capabilities()
	}
}

fn (mut runtime MultiWindowRenderRuntime) finish_frame_lease(id WindowId, lease_epoch u64) ! {
	runtime.finish_window_lease(id, lease_epoch, .frame)!
}

fn (mut runtime MultiWindowRenderRuntime) begin_app_resource_lease(app &App, phase MultiWindowRenderPhase) !AppResourceContext {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	if runtime.app_lease_epoch != 0 || (phase != .cleanup && !runtime.batch_active) {
		return error(err_multiwindow_render_wrong_phase)
	}
	if phase == .init {
		if runtime.app_init_started {
			return error(err_multiwindow_render_wrong_phase)
		}
		runtime.app_init_started = true
	} else if phase == .cleanup {
		if runtime.app_cleanup_started {
			return error(err_multiwindow_render_wrong_phase)
		}
		if runtime.app_cleanup_lease_epoch == 0 {
			return error(err_multiwindow_render_stopped)
		}
		runtime.app_cleanup_started = true
	} else if phase != .frame || !runtime.app_init_completed {
		return error(err_multiwindow_render_wrong_phase)
	}
	mut lease_epoch := u64(0)
	if phase == .cleanup {
		lease_epoch = runtime.app_cleanup_lease_epoch
	} else {
		lease_epoch = runtime.take_lease_epoch_locked()!
	}
	runtime.app_lease_epoch = lease_epoch
	runtime.app_phase = phase
	runtime.app_resource_active = true
	return AppResourceContext{
		app:          app
		app_instance: runtime.app_instance
		lease_epoch:  runtime.app_lease_epoch
		batch_epoch:  if runtime.batch_active {
			runtime.active_batch_epoch
		} else {
			runtime.last_completed_batch
		}
		phase:        phase
		scope:        .app
	}
}

fn (mut runtime MultiWindowRenderRuntime) finish_app_resource_lease(lease_epoch u64, succeeded bool, failure string) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	if runtime.app_lease_epoch != lease_epoch || lease_epoch == 0 {
		return error(err_multiwindow_render_stale_lease)
	}
	phase := runtime.app_phase
	runtime.app_lease_epoch = 0
	runtime.app_phase = .invalid
	runtime.app_resource_active = false
	if phase == .init {
		runtime.app_init_completed = succeeded
		if !succeeded {
			runtime.app_init_terminal = if failure == '' {
				err_multiwindow_render_app_init_failed
			} else {
				failure
			}
		}
	}
	if phase == .cleanup {
		runtime.app_cleanup_finished = true
	}
}

fn (runtime &MultiWindowRenderRuntime) app_callbacks() (AppResourceInitFn, AppResourceFrameFn, AppResourceCleanupFn) {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	return runtime.app_init_fn, runtime.app_frame_fn, runtime.app_cleanup_fn
}

fn (runtime &MultiWindowRenderRuntime) app_initialized() bool {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	return runtime.app_init_completed
}

fn (runtime &MultiWindowRenderRuntime) app_initialization_state() (bool, bool, string) {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	return runtime.app_init_started, runtime.app_init_completed, runtime.app_init_terminal
}

fn (runtime &MultiWindowRenderRuntime) app_cleanup_state() (bool, bool) {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	return runtime.app_cleanup_started, runtime.app_cleanup_finished
}

fn (mut runtime MultiWindowRenderRuntime) mark_app_initialized_without_callback() {
	runtime.mutex.lock()
	if !runtime.app_init_started {
		runtime.app_init_started = true
		runtime.app_init_completed = true
	}
	runtime.mutex.unlock()
}

fn (mut runtime MultiWindowRenderRuntime) begin_destroy(id WindowId, reason WindowCleanupReason) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.window_index_locked(id)!
	if runtime.windows[index].status == .destroyed {
		return error(err_multiwindow_window_not_found)
	}
	runtime.windows[index].status = .closing
	if !runtime.windows[index].cleanup_reason_set {
		runtime.windows[index].cleanup_reason = reason
		runtime.windows[index].cleanup_reason_set = true
	}
}

fn (mut runtime MultiWindowRenderRuntime) rollback_destroy(id WindowId) {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.window_index_locked(id) or { return }
	if runtime.windows[index].status == .closing && !runtime.stopping {
		runtime.windows[index].status = if runtime.windows[index].init_completed {
			.initialized
		} else {
			.registered
		}
		if !runtime.windows[index].cleanup_started {
			runtime.windows[index].cleanup_reason = .requested
			runtime.windows[index].cleanup_reason_set = false
		}
	}
}

fn (mut runtime MultiWindowRenderRuntime) prepare_cleanup(app &App, id WindowId, info WindowFrameInfo, reason WindowCleanupReason, graphics_available bool) !MultiWindowCleanupPlan {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.window_index_locked(id)!
	mut window := &runtime.windows[index]
	if !window.cleanup_reason_set {
		window.cleanup_reason = reason
		window.cleanup_reason_set = true
	}
	cleanup_reason := window.cleanup_reason
	if window.cleanup_started || window.cleanup_finished {
		return MultiWindowCleanupPlan{}
	}
	if window.cleanup_fn == unsafe { nil } {
		window.cleanup_started = true
		window.cleanup_finished = true
		return MultiWindowCleanupPlan{}
	}
	if runtime.callback_depth != 0 {
		return error(err_multiwindow_render_wrong_phase)
	}
	if window.cleanup_lease_epoch == 0 {
		return error(err_multiwindow_render_stopped)
	}
	window.cleanup_started = true
	window.active_lease_epoch = window.cleanup_lease_epoch
	window.active_phase = .cleanup
	window.resource_section_active = false
	window.pass_active = false
	window.pass_epoch = 0
	window.sgl_active = false
	return MultiWindowCleanupPlan{
		callback: window.cleanup_fn
		context:  WindowCleanupContext{
			app:            app
			app_instance:   runtime.app_instance
			lease_epoch:    window.active_lease_epoch
			info:           info
			cleanup_reason: cleanup_reason
			has_graphics:   graphics_available
		}
		run:      true
	}
}

fn (mut runtime MultiWindowRenderRuntime) finish_cleanup(id WindowId, lease_epoch u64) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.validate_window_lease_locked(id, lease_epoch, .cleanup)!
	mut window := &runtime.windows[index]
	window.active_lease_epoch = 0
	window.active_phase = .invalid
	window.resource_section_active = false
	window.pass_active = false
	window.pass_epoch = 0
	window.sgl_active = false
	window.cleanup_finished = true
}

fn (mut runtime MultiWindowRenderRuntime) finish_destroy(id WindowId, batch u64) {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.window_index_locked(id) or { return }
	mut window := &runtime.windows[index]
	if window.status == .destroyed {
		return
	}
	window.status = .destroyed
	window.active_lease_epoch = 0
	window.active_phase = .invalid
	window.resource_section_active = false
	window.pass_active = false
	window.sgl_active = false
	window.target_available = false
	window.target_identity = 0
	runtime.resources.retire_window_at_batch(id, batch)
}

fn (runtime &MultiWindowRenderRuntime) window_retirement_needs_boundary(id WindowId) !bool {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	runtime.window_index_locked(id)!
	return runtime.resources.scope_requires_boundary(false, id)
}

fn (mut runtime MultiWindowRenderRuntime) finish_destroy_terminal(id WindowId) {
	runtime.mutex.lock()
	batch := runtime.last_completed_batch
	runtime.mutex.unlock()
	runtime.finish_destroy(id, batch)
}

fn (mut runtime MultiWindowRenderRuntime) begin_resource_section(app &App, window_id WindowId, lease_epoch u64, phase MultiWindowRenderPhase, scope MultiWindowResourceScope) !WindowResourceContext {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	if !runtime.batch_active && phase != .cleanup {
		return error(err_multiwindow_render_wrong_phase)
	}
	if scope == .app {
		return error(err_multiwindow_render_wrong_phase)
	}
	index := runtime.validate_window_lease_locked(window_id, lease_epoch, phase)!
	mut window := &runtime.windows[index]
	if window.resource_section_active {
		return error(err_multiwindow_render_resource_section_active)
	}
	if window.pass_active {
		return error(err_multiwindow_render_pass_active)
	}
	if phase == .frame && window.sgl_flushed {
		return error(err_multiwindow_render_resource_after_sgl)
	}
	window.resource_section_active = true
	return WindowResourceContext{
		app:          app
		app_instance: runtime.app_instance
		window:       window_id
		lease_epoch:  lease_epoch
		batch_epoch:  if runtime.batch_active {
			runtime.active_batch_epoch
		} else {
			runtime.last_completed_batch
		}
		phase:        phase
		scope:        scope
	}
}

fn (mut runtime MultiWindowRenderRuntime) finish_resource_section(context WindowResourceContext) {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.validate_window_lease_locked(context.window, context.lease_epoch,
		context.phase) or { return }
	runtime.windows[index].resource_section_active = false
}

fn (runtime &MultiWindowRenderRuntime) validate_resource_context(context &WindowResourceContext, operation MultiWindowResourceOperation) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	if context.app_instance != runtime.app_instance {
		return error(err_multiwindow_render_stale_lease)
	}
	if context.phase == .cleanup && operation != .retire {
		return error(err_multiwindow_render_wrong_phase)
	}
	if runtime.batch_active {
		if runtime.active_batch_epoch != context.batch_epoch {
			return error(err_multiwindow_render_stale_lease)
		}
	} else if context.phase != .cleanup || context.batch_epoch != runtime.last_completed_batch {
		return error(err_multiwindow_render_stale_lease)
	}
	if context.scope == .app {
		if runtime.app_lease_epoch != context.lease_epoch || runtime.app_phase != context.phase
			|| !runtime.app_resource_active {
			return error(err_multiwindow_render_stale_lease)
		}
	} else {
		index := runtime.validate_window_lease_locked(context.window, context.lease_epoch,
			context.phase)!
		if !runtime.windows[index].resource_section_active {
			return error(err_multiwindow_render_stale_lease)
		}
	}
}

fn (mut runtime MultiWindowRenderRuntime) begin_pass(id WindowId, lease_epoch u64, swapchain bool, sgl_pass bool, target_key string) !u64 {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.validate_window_lease_locked(id, lease_epoch, .frame)!
	mut window := &runtime.windows[index]
	if window.resource_section_active || window.pass_active {
		return error(err_multiwindow_render_pass_active)
	}
	if !window.target_available || target_key == '' {
		return error(err_multiwindow_render_backend_unavailable)
	}
	if swapchain && window.swapchain_pass_used {
		return error(err_multiwindow_render_swapchain_used)
	}
	window.pass_active = true
	window.sgl_active = sgl_pass
	window.pass_epoch = runtime.take_pass_epoch_locked()!
	if swapchain {
		window.swapchain_pass_used = true
	}
	return window.pass_epoch
}

fn (mut runtime MultiWindowRenderRuntime) finish_pass(id WindowId, lease_epoch u64, pass_epoch u64, flushed_sgl bool) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.validate_window_lease_locked(id, lease_epoch, .frame)!
	mut window := &runtime.windows[index]
	if !window.pass_active || window.pass_epoch != pass_epoch {
		return error(err_multiwindow_render_wrong_phase)
	}
	window.pass_active = false
	window.sgl_active = false
	window.pass_epoch = 0
	if flushed_sgl {
		window.sgl_flushed = true
	}
}

fn (mut runtime MultiWindowRenderRuntime) abort_pass_state(id WindowId, lease_epoch u64, pass_epoch u64) {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.window_index_locked(id) or { return }
	mut window := &runtime.windows[index]
	if window.active_lease_epoch != lease_epoch || lease_epoch == 0 {
		return
	}
	if window.pass_epoch == pass_epoch {
		window.pass_active = false
		window.sgl_active = false
		window.pass_epoch = 0
	}
}

fn (mut runtime MultiWindowRenderRuntime) abort_frame_lease_state(id WindowId, lease_epoch u64) {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.window_index_locked(id) or { return }
	mut window := &runtime.windows[index]
	if window.active_lease_epoch != lease_epoch || lease_epoch == 0 {
		return
	}
	window.active_lease_epoch = 0
	window.active_phase = .invalid
	window.resource_section_active = false
	window.pass_active = false
	window.pass_epoch = 0
	window.sgl_active = false
}

fn (runtime &MultiWindowRenderRuntime) swapchain_pass_used(id WindowId, lease_epoch u64) !bool {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	return runtime.windows[runtime.validate_window_lease_locked(id, lease_epoch, .frame)!].swapchain_pass_used
}

fn (runtime &MultiWindowRenderRuntime) validate_pass_context(context &WindowPassContext) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	if context.app_instance != runtime.app_instance {
		return error(err_multiwindow_render_stale_lease)
	}
	window := runtime.windows[runtime.validate_window_lease_locked(context.window,
		context.lease_epoch, .frame)!]
	if !window.pass_active || window.pass_epoch != context.pass_epoch || window.sgl_active {
		return error(err_multiwindow_render_wrong_phase)
	}
}

fn (runtime &MultiWindowRenderRuntime) validate_sgl_context(context &WindowSglContext) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	if context.app_instance != runtime.app_instance || context.target_key == '' {
		return error(err_multiwindow_render_stale_lease)
	}
	window := runtime.windows[runtime.validate_window_lease_locked(context.window,
		context.lease_epoch, .frame)!]
	if !window.pass_active || !window.sgl_active || window.pass_epoch != context.pass_epoch {
		return error(err_multiwindow_render_wrong_phase)
	}
}

fn (runtime &MultiWindowRenderRuntime) active_sgl_context(app &App, id WindowId, lease_epoch u64) !WindowSglContext {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	window := runtime.windows[runtime.validate_window_lease_locked(id, lease_epoch, .frame)!]
	if !window.pass_active || !window.sgl_active || window.target_key == '' {
		return error(err_multiwindow_render_wrong_phase)
	}
	return WindowSglContext{
		app:          app
		app_instance: runtime.app_instance
		window:       id
		lease_epoch:  lease_epoch
		pass_epoch:   window.pass_epoch
		target_key:   window.target_key
	}
}

fn (runtime &MultiWindowRenderRuntime) validate_frame_lease(id WindowId, lease_epoch u64) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	runtime.validate_window_lease_locked(id, lease_epoch, .frame)!
}

fn (runtime &MultiWindowRenderRuntime) validate_phase_lease(id WindowId, lease_epoch u64, phase MultiWindowRenderPhase) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	runtime.validate_window_lease_locked(id, lease_epoch, phase)!
}

fn (mut runtime MultiWindowRenderRuntime) begin_user_callback() {
	runtime.mutex.lock()
	runtime.callback_depth++
	runtime.mutex.unlock()
}

fn (mut runtime MultiWindowRenderRuntime) end_user_callback() {
	runtime.mutex.lock()
	if runtime.callback_depth > 0 {
		runtime.callback_depth--
	}
	runtime.mutex.unlock()
}

fn (mut runtime MultiWindowRenderRuntime) defer_destroy_in_callback(id WindowId) !bool {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.window_index_locked(id)!
	if runtime.callback_depth == 0 {
		return false
	}
	if runtime.windows[index].status == .destroyed {
		return error(err_multiwindow_window_not_found)
	}
	runtime.windows[index].status = .closing
	if id !in runtime.deferred_windows {
		runtime.deferred_windows << id
	}
	return true
}

fn (mut runtime MultiWindowRenderRuntime) defer_stop_in_callback() bool {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	if runtime.callback_depth == 0 {
		return false
	}
	runtime.stopping = true
	runtime.deferred_stop = true
	for i, window in runtime.windows {
		if window.status in [.registered, .initialized] {
			runtime.windows[i].status = .closing
		}
	}
	return true
}

fn (mut runtime MultiWindowRenderRuntime) take_deferred_transitions() MultiWindowDeferredTransitions {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	if runtime.callback_depth != 0 {
		return MultiWindowDeferredTransitions{}
	}
	result := MultiWindowDeferredTransitions{
		stop:    runtime.deferred_stop
		windows: runtime.deferred_windows.clone()
	}
	runtime.deferred_stop = false
	runtime.deferred_windows.clear()
	return result
}

fn (runtime &MultiWindowRenderRuntime) has_deferred_transitions() bool {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	return runtime.deferred_stop || runtime.deferred_windows.len > 0
}

fn (mut runtime MultiWindowRenderRuntime) begin_stop() {
	runtime.mutex.lock()
	runtime.stopping = true
	for i, window in runtime.windows {
		if window.status in [.registered, .initialized] {
			runtime.windows[i].status = .closing
		}
	}
	runtime.mutex.unlock()
}

fn (mut runtime MultiWindowRenderRuntime) finish_stop() {
	runtime.mutex.lock()
	for i, window in runtime.windows {
		if window.status != .invalid {
			runtime.windows[i].status = .destroyed
			runtime.windows[i].init_fn = unsafe { nil }
			runtime.windows[i].frame_fn = unsafe { nil }
			runtime.windows[i].cleanup_fn = unsafe { nil }
			runtime.windows[i].active_lease_epoch = 0
			runtime.windows[i].active_phase = .invalid
			runtime.windows[i].target_identity = 0
		}
	}
	// Remaining objects are released by the immediately following global
	// SGL/gfx shutdown, or are intentionally abandoned after device loss.
	runtime.resources.abandon_all()
	runtime.app_init_fn = unsafe { nil }
	runtime.app_frame_fn = unsafe { nil }
	runtime.app_cleanup_fn = unsafe { nil }
	runtime.readback_fn = unsafe { nil }
	runtime.stopping = false
	runtime.stopped = true
	runtime.mutex.unlock()
}

fn (runtime &MultiWindowRenderRuntime) app_retirement_needs_boundary() bool {
	runtime.mutex.lock()
	needs_boundary := !runtime.app_resources_retired
		&& runtime.resources.scope_requires_boundary(true, WindowId{})
	runtime.mutex.unlock()
	return needs_boundary
}

fn (mut runtime MultiWindowRenderRuntime) retire_app_resources_at_batch(batch u64) {
	runtime.mutex.lock()
	if !runtime.app_resources_retired {
		runtime.resources.retire_app_at_batch(batch)
		runtime.app_resources_retired = true
	}
	runtime.mutex.unlock()
}

fn (mut runtime MultiWindowRenderRuntime) retire_app_resources_terminal() {
	runtime.mutex.lock()
	if !runtime.app_resources_retired {
		runtime.resources.retire_app_at_batch(runtime.last_completed_batch)
		runtime.app_resources_retired = true
	}
	runtime.mutex.unlock()
}

fn (mut runtime MultiWindowRenderRuntime) take_sgl_materialization_for_target(target_key string) ?MultiWindowSglMaterialization {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	return runtime.resources.take_sgl_materialization_for_target(target_key)
}

fn (mut runtime MultiWindowRenderRuntime) abandon_sgl_materializations_for_target(target_key string) {
	runtime.mutex.lock()
	runtime.resources.abandon_sgl_materializations_for_target(target_key)
	runtime.mutex.unlock()
}

fn (runtime &MultiWindowRenderRuntime) attachments_support_sgl(id WindowAttachmentsId, window WindowId) !bool {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index :=
		runtime.resources.validate(attachments_resource_key(id), .attachments, window, .window)!
	recipe := runtime.resources.slots[index].attachments_recipe
	return recipe.colors.len == 1 && recipe.resolves.len <= 1
}

fn (runtime &MultiWindowRenderRuntime) validate_readback_image(id WindowImageId, window WindowId) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	runtime.resources.validate(image_resource_key(id), .image, window, .window)!
}

fn (mut runtime MultiWindowRenderRuntime) finish_window_lease(id WindowId, lease_epoch u64, phase MultiWindowRenderPhase) ! {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	index := runtime.validate_window_lease_locked(id, lease_epoch, phase)!
	mut window := &runtime.windows[index]
	if window.pass_active || window.resource_section_active {
		return error(err_multiwindow_render_wrong_phase)
	}
	window.active_lease_epoch = 0
	window.active_phase = .invalid
	window.sgl_active = false
}

fn (runtime &MultiWindowRenderRuntime) validate_window_lease_locked(id WindowId, lease_epoch u64, phase MultiWindowRenderPhase) !int {
	index := runtime.window_index_locked(id)!
	window := runtime.windows[index]
	if window.active_lease_epoch != lease_epoch || lease_epoch == 0 {
		return error(err_multiwindow_render_stale_lease)
	}
	if window.active_phase != phase {
		return error(err_multiwindow_render_wrong_phase)
	}
	return index
}

fn (runtime &MultiWindowRenderRuntime) mutable_window_index_locked(id WindowId) !int {
	index := runtime.window_index_locked(id)!
	if runtime.windows[index].status !in [.registered, .initialized] {
		return error(err_multiwindow_render_stopped)
	}
	return index
}

fn (runtime &MultiWindowRenderRuntime) window_index_locked(id WindowId) !int {
	if id.app_instance != runtime.app_instance {
		return error(err_multiwindow_app_identity_mismatch)
	}
	for i, window in runtime.windows {
		if window.id == id {
			return i
		}
	}
	return error(err_multiwindow_window_not_found)
}

fn (mut runtime MultiWindowRenderRuntime) take_lease_epoch_locked() !u64 {
	result := take_gg_nonwrapping_counter(runtime.next_lease_epoch)!
	runtime.next_lease_epoch = result.next
	return result.taken
}

fn (mut runtime MultiWindowRenderRuntime) take_pass_epoch_locked() !u64 {
	result := take_gg_nonwrapping_counter(runtime.next_pass_epoch)!
	runtime.next_pass_epoch = result.next
	return result.taken
}

struct GgNonwrappingCounterResult {
	taken u64
	next  u64
}

fn take_gg_nonwrapping_counter(value u64) !GgNonwrappingCounterResult {
	if value == 0 {
		return error(err_multiwindow_render_stopped)
	}
	return GgNonwrappingCounterResult{
		taken: value
		next:  if value == ~u64(0) { u64(0) } else { value + 1 }
	}
}

fn multiwindow_render_window_from_config(id WindowId, config WindowConfig) MultiWindowRenderWindow {
	return MultiWindowRenderWindow{
		id:                     id
		status:                 .registered
		clear_color:            config.clear_color
		requested_sample_count: config.sample_count
		redraw_mode:            config.redraw_mode
		init_fn:                config.init_fn
		frame_fn:               config.frame_fn
		cleanup_fn:             config.cleanup_fn
		target:                 WindowRenderTargetInfo{
			sample_count: config.sample_count
		}
	}
}

fn managed_swapchain_target_key(app_instance u64, id WindowId, target multiwindow.RenderTargetSnapshot) string {
	return '${app_instance}:${id.str()}:swapchain:${target.target_identity}:${target.color_format}:${target.depth_format}:${target.sample_count}'
}
