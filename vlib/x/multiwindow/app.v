module multiwindow

import sync
import x.executor

// App owns the low-level multi-window registry and owner queue.
@[heap]
pub struct App {
mut:
	config          Config
	status          AppStatus = .running
	backend         Backend
	windows         []WindowSlot
	events          []Event
	owner_thread_id u64
	state_mutex     &sync.Mutex        = sync.new_mutex()
	owner           &executor.Executor = unsafe { nil }
}

// new_app creates a low-level multi-window App with the mock backend by default.
pub fn new_app(config Config) !&App {
	if config.queue_size <= 0 {
		return error(err_queue_size_invalid)
	}
	mut backend := new_backend(config.backend, config.require_renderer)!
	backend.start(config.require_renderer)!
	mut owner := executor.new(queue_size: config.queue_size)!
	return &App{
		config:          config
		status:          .running
		backend:         backend
		owner_thread_id: sync.thread_id()
		state_mutex:     sync.new_mutex()
		owner:           owner
	}
}

// status reports the application lifecycle state.
pub fn (app &App) status() AppStatus {
	app.state_mutex.lock()
	status := app.status
	app.state_mutex.unlock()
	return status
}

// capabilities reports the selected backend capabilities.
pub fn (app &App) capabilities() Capabilities {
	return app.backend.capabilities()
}

// create_window creates a backend window and returns its generation-checked id.
pub fn (mut app App) create_window(config WindowConfig) !WindowId {
	app.assert_owner_thread()!
	validate_window_config(config)!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	id := app.allocate_window_id()
	actual_size := app.backend.create_window(id, config)!
	app.windows[id.slot] = WindowSlot{
		id:     id
		config: window_config_with_size(config, actual_size.width, actual_size.height)
		status: .alive
	}
	app.events << Event{
		kind:      .window_created
		window_id: id
		width:     actual_size.width
		height:    actual_size.height
	}
	return id
}

// destroy_window destroys one live window. The app remains alive when any window,
// including the last one, is destroyed.
pub fn (mut app App) destroy_window(id WindowId) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	index := app.live_window_index(id)!
	app.backend.destroy_window(id)!
	app.windows[index].status = .destroyed
	app.events << Event{
		kind:      .window_destroyed
		window_id: id
	}
}

// set_window_title updates the native title and then the authoritative App state.
pub fn (mut app App) set_window_title(id WindowId, title string) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	index := app.live_window_index(id)!
	app.backend.set_window_title(id, title)!
	app.windows[index].config = window_config_with_title(app.windows[index].config, title)
}

// resize_window requests a native resize and then updates the authoritative App state.
pub fn (mut app App) resize_window(id WindowId, width int, height int) ! {
	app.assert_owner_thread()!
	validate_window_size(width, height)!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	index := app.live_window_index(id)!
	actual_size := app.backend.resize_window(id, width, height)!
	app.windows[index].config = window_config_with_size(app.windows[index].config,
		actual_size.width, actual_size.height)
	app.events << Event{
		kind:      .window_resized
		window_id: id
		width:     actual_size.width
		height:    actual_size.height
	}
}

// window_info returns a snapshot of the authoritative App-side window state.
pub fn (app &App) window_info(id WindowId) !WindowInfo {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	index := app.live_window_index(id)!
	return window_info_from_slot(app.windows[index])
}

// window_ids returns live window ids in stable slot order.
pub fn (app &App) window_ids() ![]WindowId {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	mut ids := []WindowId{cap: app.windows.len}
	for slot in app.windows {
		if slot.status == .alive {
			ids << slot.id
		}
	}
	return ids
}

// window_infos returns live window snapshots in stable slot order.
pub fn (app &App) window_infos() ![]WindowInfo {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	mut infos := []WindowInfo{cap: app.windows.len}
	for slot in app.windows {
		if slot.status == .alive {
			infos << window_info_from_slot(slot)
		}
	}
	return infos
}

// window_exists reports whether id currently points to a live window.
pub fn (app &App) window_exists(id WindowId) bool {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if id.slot < 0 || id.slot >= app.windows.len {
		return false
	}
	slot := app.windows[id.slot]
	return slot.status == .alive && slot.id.generation == id.generation
}

// window_status returns the lifecycle status for a valid generation.
pub fn (app &App) window_status(id WindowId) !WindowStatus {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if id.slot < 0 || id.slot >= app.windows.len {
		return error(err_window_not_found)
	}
	slot := app.windows[id.slot]
	if slot.id.generation != id.generation {
		return error(err_stale_window)
	}
	return slot.status
}

// drain_events returns and clears pending lifecycle events.
pub fn (mut app App) drain_events() ![]Event {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	events := app.events.clone()
	app.events.clear()
	return events
}

// poll_events lets the backend route native lifecycle events into App events.
pub fn (mut app App) poll_events() !int {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	events := app.backend.poll_events()!
	mut accepted := 0
	for event in events {
		match event.kind {
			.window_close_requested {
				app.live_window_index(event.window_id) or { continue }
				app.events << event
				accepted++
			}
			.window_destroyed {
				if app.mark_destroyed_from_backend_locked(event.window_id) {
					app.events << event
					accepted++
				}
			}
			.window_resized {
				if event.width <= 0 || event.height <= 0 {
					continue
				}
				index := app.live_window_index(event.window_id) or { continue }
				app.windows[index].config = window_config_with_size(app.windows[index].config,
					event.width, event.height)
				app.events << event
				accepted++
			}
			else {
				app.events << event
				accepted++
			}
		}
	}
	return accepted
}

$if test {
	// enqueue_mock_close_requested_for_test injects a mock-native close request
	// into the backend queue only in test builds.
	pub fn (mut app App) enqueue_mock_close_requested_for_test(id WindowId) ! {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		defer {
			app.state_mutex.unlock()
		}
		app.ensure_running_locked()!
		app.live_window_index(id)!
		if app.backend.kind != .mock {
			return error(err_capability_unsupported)
		}
		app.backend.mock.enqueue_event(Event{
			kind:      .window_close_requested
			window_id: id
		})
	}

	// enqueue_mock_close_requested_unchecked_for_test intentionally bypasses
	// WindowId liveness validation so App.poll_events() filtering can be tested.
	fn (mut app App) enqueue_mock_close_requested_unchecked_for_test(id WindowId) ! {
		app.assert_owner_thread()!
		app.state_mutex.lock()
		defer {
			app.state_mutex.unlock()
		}
		app.ensure_running_locked()!
		if app.backend.kind != .mock {
			return error(err_capability_unsupported)
		}
		app.backend.mock.enqueue_event(Event{
			kind:      .window_close_requested
			window_id: id
		})
	}
}

// post submits a short owner-thread callback if queue capacity is available.
pub fn (mut app App) post(f AppJobFn) ! {
	app.try_post(f)!
}

// try_post submits a short owner-thread callback without waiting for capacity.
// It follows x.executor's ! contract: queue-full and closed-queue states are
// returned as errors instead of a bool.
pub fn (mut app App) try_post(f AppJobFn) ! {
	if f == unsafe { nil } {
		return error(err_nil_job)
	}
	app.ensure_running()!
	app_ptr := unsafe { voidptr(&app) }
	app.owner.try_post(fn [app_ptr, f] () ! {
		mut queued_app := unsafe { &App(app_ptr) }
		f(mut queued_app)!
	}) or { return wrap_executor_error(err) }
}

// drain_pending executes up to max_jobs queued owner callbacks while the app is running.
// After stop(), accepted-but-pending callbacks are not executed and this returns
// `multiwindow: app is stopped`.
pub fn (mut app App) drain_pending(max_jobs int) !int {
	app.assert_owner_thread()!
	app.ensure_running()!
	if max_jobs <= 0 {
		return error(err_drain_limit_invalid)
	}
	mut ran := 0
	for ran < max_jobs {
		app.ensure_running()!
		drained := app.owner.drain_pending(1) or { return wrap_executor_error(err) }
		if drained == 0 {
			return ran
		}
		ran += drained
		if app.status() != .running {
			return error(err_app_stopped)
		}
	}
	return ran
}

// stop destroys live windows, closes owner-queue admission and marks the app stopped.
// Pending owner callbacks are canceled logically: public drain_pending() refuses
// to run them after stop().
pub fn (mut app App) stop() ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if app.status == .stopped {
		app.owner.stop()
		return
	}
	for i, slot in app.windows {
		if slot.status == .alive {
			app.backend.destroy_window(slot.id)!
			app.windows[i].status = .destroyed
			app.events << Event{
				kind:      .window_destroyed
				window_id: slot.id
			}
		}
	}
	app.status = .stopped
	app.backend.stop()!
	app.owner.stop()
}

fn (app &App) assert_owner_thread() ! {
	if app.owner_thread_id != sync.thread_id() {
		return error(err_owner_thread_required)
	}
}

fn (app &App) ensure_running() ! {
	app.state_mutex.lock()
	status := app.status
	app.state_mutex.unlock()
	if status != .running {
		return error(err_app_stopped)
	}
}

fn (app &App) ensure_running_locked() ! {
	if app.status != .running {
		return error(err_app_stopped)
	}
}

fn validate_window_config(config WindowConfig) ! {
	validate_window_size(config.width, config.height)!
	if config.min_width < 0 || config.min_height < 0 {
		return error(err_invalid_window_size)
	}
}

fn validate_window_size(width int, height int) ! {
	if width <= 0 || height <= 0 {
		return error(err_invalid_window_size)
	}
}

fn window_config_with_title(config WindowConfig, title string) WindowConfig {
	return WindowConfig{
		title:      title
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

fn window_config_with_size(config WindowConfig, width int, height int) WindowConfig {
	return WindowConfig{
		title:      config.title
		width:      width
		height:     height
		min_width:  config.min_width
		min_height: config.min_height
		resizable:  config.resizable
		visible:    config.visible
		high_dpi:   config.high_dpi
		borderless: config.borderless
		fullscreen: config.fullscreen
	}
}

fn window_info_from_slot(slot WindowSlot) WindowInfo {
	config := slot.config
	return WindowInfo{
		id:         slot.id
		status:     slot.status
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

fn (mut app App) allocate_window_id() WindowId {
	for i, slot in app.windows {
		if slot.status != .alive {
			generation := slot.id.generation + 1
			return WindowId{
				slot:       i
				generation: generation
			}
		}
	}
	id := WindowId{
		slot:       app.windows.len
		generation: 1
	}
	app.windows << WindowSlot{
		id:     id
		status: .invalid
	}
	return id
}

fn (app &App) live_window_index(id WindowId) !int {
	if id.slot < 0 || id.slot >= app.windows.len {
		return error(err_window_not_found)
	}
	slot := app.windows[id.slot]
	if slot.id.generation != id.generation {
		return error(err_stale_window)
	}
	if slot.status != .alive {
		return error(err_stale_window)
	}
	return id.slot
}

fn (mut app App) mark_destroyed_from_backend_locked(id WindowId) bool {
	if id.slot < 0 || id.slot >= app.windows.len {
		return false
	}
	slot := app.windows[id.slot]
	if slot.id.generation != id.generation || slot.status != .alive {
		return false
	}
	app.windows[id.slot].status = .destroyed
	return true
}

fn wrap_executor_error(err IError) IError {
	msg := err.msg()
	if msg.starts_with('multiwindow:') {
		return err
	}
	return match msg {
		'executor: queue is full' { error(err_owner_queue_full) }
		'executor: executor is closed' { error(err_owner_queue_closed) }
		'executor: executor is stopped' { error(err_owner_queue_closed) }
		'executor: drain limit must be positive' { error(err_drain_limit_invalid) }
		else { error('${err_owner_queue_failed}: ${msg}') }
	}
}
