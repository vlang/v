module multiwindow

const renderer_fault_trace_capacity = 32
const err_renderer_fault_trace_token_exhausted = 'multiwindow: renderer fault trace token exhausted'

enum RendererFaultMilestone {
	invalid
	anchor_probe
	anchor_completed
	environment_probe
	environment_acquired
	setup_probe
	gfx_complete
	bridge_published
	gfx_shutdown_complete
	anchor_destroyed
}

struct RendererFaultTraceSnapshot {
	trace_enabled bool
	fault_pending bool
	attempt_id    u64
	token         u64
	len           int
	overflow      bool
	milestones    [renderer_fault_trace_capacity]RendererFaultMilestone
}

struct RendererFaultTraceState {
mut:
	trace_enabled bool
	attempt_id    u64
	token         u64
	len           int
	overflow      bool
	milestones    [renderer_fault_trace_capacity]RendererFaultMilestone
}

// InternalFaultStage is a production-private, one-shot failure seam. Module
// tests can select a deterministic stage without adding a public or test-only
// API and without teaching platform backends about the test harness.
enum InternalFaultStage {
	none
	renderer_anchor_create
	renderer_environment
	renderer_setup
	renderer_batch_begin
	renderer_target_acquire
	renderer_pass_begin
	renderer_anchor_begin
	renderer_precommit
	renderer_submission_finalize
	teardown_prepare
	teardown_seal
	teardown_backend_finish
	teardown_backend_stop
}

struct InternalFaultPlan {
mut:
	stage               InternalFaultStage
	hits_before_failure int
	message             string
	fault_pending       bool
	trace               RendererFaultTraceState
	next_trace_token    u64 = 1
}

fn (mut app App) set_internal_fault(stage InternalFaultStage, hits_before_failure int, message string) ! {
	app.assert_owner_thread()!
	if stage == .none || hits_before_failure < 0 || message == '' {
		return error(err_internal_fault_config_invalid)
	}
	app.fault_mutex.lock()
	app.internal_fault.stage = stage
	app.internal_fault.hits_before_failure = hits_before_failure
	app.internal_fault.message = message
	app.internal_fault.fault_pending = true
	if !app.internal_fault.trace.trace_enabled {
		app.internal_fault.trace = RendererFaultTraceState{
			trace_enabled: true
		}
	}
	app.fault_mutex.unlock()
}

fn (mut app App) clear_internal_fault() {
	app.fault_mutex.lock()
	app.internal_fault.stage = .none
	app.internal_fault.hits_before_failure = 0
	app.internal_fault.message = ''
	app.internal_fault.fault_pending = false
	app.fault_mutex.unlock()
}

fn (mut app App) take_internal_fault(stage InternalFaultStage) ?string {
	app.fault_mutex.lock()
	defer {
		app.fault_mutex.unlock()
	}
	if stage == .none || !app.internal_fault.fault_pending || app.internal_fault.stage != stage {
		return none
	}
	if app.internal_fault.hits_before_failure > 0 {
		app.internal_fault.hits_before_failure--
		return none
	}
	message := app.internal_fault.message
	app.internal_fault.stage = .none
	app.internal_fault.hits_before_failure = 0
	app.internal_fault.message = ''
	app.internal_fault.fault_pending = false
	return message
}

fn (mut app App) begin_renderer_fault_trace_attempt() !u64 {
	app.fault_mutex.lock()
	defer {
		app.fault_mutex.unlock()
	}
	token := app.take_native_authority_token_locked()!
	app.internal_fault.trace.token = token
	if !app.internal_fault.trace.trace_enabled {
		return token
	}
	app.internal_fault.trace.len = 0
	app.internal_fault.trace.milestones = [renderer_fault_trace_capacity]RendererFaultMilestone{}
	if app.internal_fault.trace.attempt_id == u64(0xffffffffffffffff) {
		app.internal_fault.trace.overflow = true
		return token
	}
	app.internal_fault.trace.attempt_id++
	app.internal_fault.trace.overflow = false
	return token
}

fn (mut app App) take_native_app_lifetime_token() !u64 {
	app.fault_mutex.lock()
	defer {
		app.fault_mutex.unlock()
	}
	return app.take_native_authority_token_locked()
}

fn (mut app App) take_native_authority_token_locked() !u64 {
	if app.internal_fault.next_trace_token == 0 {
		return error(err_renderer_fault_trace_token_exhausted)
	}
	token := app.internal_fault.next_trace_token
	if token == u64(0xffffffffffffffff) {
		app.internal_fault.next_trace_token = 0
	} else {
		app.internal_fault.next_trace_token++
	}
	return token
}

fn (mut app App) record_renderer_fault_milestone(token u64, milestone RendererFaultMilestone) {
	app.fault_mutex.lock()
	defer {
		app.fault_mutex.unlock()
	}
	if token == 0 || app.internal_fault.trace.token == 0 || token != app.internal_fault.trace.token {
		app.internal_fault.trace.overflow = true
		return
	}
	if milestone == .invalid {
		app.internal_fault.trace.overflow = true
		return
	}
	if !app.internal_fault.trace.trace_enabled {
		return
	}
	if app.internal_fault.trace.attempt_id == 0 {
		app.internal_fault.trace.overflow = true
		return
	}
	if app.internal_fault.trace.overflow {
		return
	}
	if app.internal_fault.trace.len >= renderer_fault_trace_capacity {
		app.internal_fault.trace.overflow = true
		return
	}
	app.internal_fault.trace.milestones[app.internal_fault.trace.len] = milestone
	app.internal_fault.trace.len++
}

fn (app &App) renderer_fault_trace_snapshot() RendererFaultTraceSnapshot {
	app.fault_mutex.lock()
	defer {
		app.fault_mutex.unlock()
	}
	return RendererFaultTraceSnapshot{
		trace_enabled: app.internal_fault.trace.trace_enabled
		fault_pending: app.internal_fault.fault_pending
		attempt_id:    app.internal_fault.trace.attempt_id
		token:         app.internal_fault.trace.token
		len:           app.internal_fault.trace.len
		overflow:      app.internal_fault.trace.overflow
		milestones:    app.internal_fault.trace.milestones
	}
}

fn (mut app App) disarm_renderer_fault_trace() {
	app.fault_mutex.lock()
	app.internal_fault.trace = RendererFaultTraceState{}
	app.fault_mutex.unlock()
}
