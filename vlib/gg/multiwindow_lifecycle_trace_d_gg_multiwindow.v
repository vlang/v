module gg

const multiwindow_lifecycle_trace_capacity = 32

enum MultiWindowLifecycleMilestone {
	invalid
	resource_cleanup_enter
	resource_cleanup_batch_complete
	resource_cleanup_terminal_complete
	resource_cleanup_failed
	runtime_finish_stop_enter
	runtime_finish_stop_complete
	core_renderer_device_unavailable
	prepare_enter
	prepare_complete
	prepare_failed
	sgl_shutdown_enter
	sgl_shutdown_complete
	core_renderer_shutdown_enter
	core_renderer_shutdown_complete
	core_renderer_shutdown_failed
	abandon_enter
	abandon_complete
	abandon_failed
	renderer_not_started
	core_finish_stop_enter
	core_finish_stop_complete
	core_finish_stop_failed
	core_stop_enter
	core_stop_complete
	core_stop_failed
}

struct MultiWindowLifecycleTraceSnapshot {
	trace_enabled bool
	len           int
	overflow      bool
	mismatch      bool
	milestones    [multiwindow_lifecycle_trace_capacity]MultiWindowLifecycleMilestone
}

struct MultiWindowLifecycleTraceState {
mut:
	trace_enabled bool
	len           int
	overflow      bool
	mismatch      bool
	milestones    [multiwindow_lifecycle_trace_capacity]MultiWindowLifecycleMilestone
}

fn (mut app App) enable_multiwindow_lifecycle_trace() {
	app.render_runtime.mutex.lock()
	app.render_runtime.lifecycle_trace = MultiWindowLifecycleTraceState{
		trace_enabled: true
	}
	app.render_runtime.mutex.unlock()
}

fn (app &App) multiwindow_lifecycle_trace_snapshot() MultiWindowLifecycleTraceSnapshot {
	app.render_runtime.mutex.lock()
	defer {
		app.render_runtime.mutex.unlock()
	}
	trace := app.render_runtime.lifecycle_trace
	return MultiWindowLifecycleTraceSnapshot{
		trace_enabled: trace.trace_enabled
		len:           trace.len
		overflow:      trace.overflow
		mismatch:      trace.mismatch
		milestones:    trace.milestones
	}
}

fn (mut app App) disarm_multiwindow_lifecycle_trace() {
	app.render_runtime.mutex.lock()
	app.render_runtime.lifecycle_trace = MultiWindowLifecycleTraceState{}
	app.render_runtime.mutex.unlock()
}

fn (mut app App) record_multiwindow_lifecycle_milestone(milestone MultiWindowLifecycleMilestone) {
	app.render_runtime.mutex.lock()
	defer {
		app.render_runtime.mutex.unlock()
	}
	mut trace := &app.render_runtime.lifecycle_trace
	if !trace.trace_enabled {
		return
	}
	if milestone == .invalid {
		trace.mismatch = true
		return
	}
	if trace.len >= multiwindow_lifecycle_trace_capacity {
		trace.overflow = true
		return
	}
	previous := if trace.len == 0 {
		MultiWindowLifecycleMilestone.invalid
	} else {
		trace.milestones[trace.len - 1]
	}
	if !multiwindow_lifecycle_transition_allowed(previous, milestone) {
		trace.mismatch = true
	}
	trace.milestones[trace.len] = milestone
	trace.len++
}

fn multiwindow_lifecycle_transition_allowed(previous MultiWindowLifecycleMilestone, next MultiWindowLifecycleMilestone) bool {
	return match previous {
		.invalid {
			next == .resource_cleanup_enter
		}
		.resource_cleanup_enter {
			next == .resource_cleanup_batch_complete || next == .resource_cleanup_terminal_complete
				|| next == .resource_cleanup_failed
		}
		.resource_cleanup_batch_complete, .resource_cleanup_terminal_complete,
		.resource_cleanup_failed {
			next == .runtime_finish_stop_enter
		}
		.runtime_finish_stop_enter {
			next == .runtime_finish_stop_complete
		}
		.runtime_finish_stop_complete {
			next == .core_renderer_device_unavailable || next == .prepare_enter
				|| next == .renderer_not_started
		}
		.core_renderer_device_unavailable {
			next == .abandon_enter
		}
		.prepare_enter {
			next == .prepare_complete || next == .prepare_failed
		}
		.prepare_complete {
			next == .sgl_shutdown_enter
		}
		.prepare_failed {
			next == .abandon_enter
		}
		.sgl_shutdown_enter {
			next == .sgl_shutdown_complete
		}
		.sgl_shutdown_complete {
			next == .core_renderer_shutdown_enter
		}
		.core_renderer_shutdown_enter {
			next == .core_renderer_shutdown_complete || next == .core_renderer_shutdown_failed
		}
		.abandon_enter {
			next == .abandon_complete || next == .abandon_failed
		}
		.renderer_not_started, .core_renderer_shutdown_complete, .core_renderer_shutdown_failed,
		.abandon_complete, .abandon_failed {
			next == .core_finish_stop_enter || next == .core_stop_enter
		}
		.core_finish_stop_enter {
			next == .core_finish_stop_complete || next == .core_finish_stop_failed
		}
		.core_stop_enter {
			next == .core_stop_complete || next == .core_stop_failed
		}
		else {
			false
		}
	}
}
