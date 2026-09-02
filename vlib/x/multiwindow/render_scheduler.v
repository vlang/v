module multiwindow

import sync

// request_redraw is thread-safe admission. Accepted foreign wrappers are
// app/epoch stamped and become cancellation-only work after stop closes
// admission; x.executor itself remains unchanged.
pub fn (mut app App) request_redraw(id WindowId) ! {
	if app.owner_thread_id == sync.thread_id() {
		app.state_mutex.lock()
		defer {
			app.state_mutex.unlock()
		}
		app.ensure_running_locked()!
		index := app.render_window_index_locked(id)!
		if app.render_runtime.windows[index].status != .alive {
			return error(err_stale_window)
		}
		app.mark_render_window_dirty_locked(index)!
		return
	}

	app.state_mutex.lock()
	if app.status != .running || app.stopping || !app.admission_open {
		app.state_mutex.unlock()
		return error(err_app_stopped)
	}
	index := app.render_window_index_locked(id) or {
		app.state_mutex.unlock()
		return err
	}
	mut window := &app.render_runtime.windows[index]
	if window.status != .alive {
		app.state_mutex.unlock()
		return error(err_stale_window)
	}
	if window.pending_admission {
		app.state_mutex.unlock()
		return
	}
	admission_id := app.take_render_admission_id_locked() or {
		app.state_mutex.unlock()
		return err
	}
	app_instance := app.instance_id
	admission_epoch := app.admission_epoch
	window.pending_admission = true
	window.pending_admission_id = admission_id
	window.pending_admission_epoch = admission_epoch
	app_ptr := unsafe { voidptr(&app) }
	app.owner.try_post(fn [app_ptr, app_instance, admission_epoch, id, admission_id] () ! {
		mut queued_app := unsafe { &App(app_ptr) }
		if !queued_app.accepted_wrapper_is_current(app_instance, admission_epoch) {
			return
		}
		queued_app.complete_redraw_admission(id, admission_id, admission_epoch)
	}) or {
		mut pending := &app.render_runtime.windows[index]
		if pending.id == id && pending.pending_admission_id == admission_id {
			pending.pending_admission = false
			pending.pending_admission_id = 0
			pending.pending_admission_epoch = 0
		}
		app.state_mutex.unlock()
		return wrap_executor_error(err)
	}
	app.state_mutex.unlock()
}

pub fn (app &App) render_window_snapshot(id WindowId) !RenderWindowSnapshot {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	return app.render_window_snapshot_locked(id)
}

pub fn (mut app App) set_render_workload(id WindowId, enabled bool) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	index := app.render_window_index_locked(id)!
	mut window := &app.render_runtime.windows[index]
	if window.status != .alive {
		return error(err_stale_window)
	}
	window.has_workload = enabled
	if enabled {
		app.mark_render_window_dirty_locked(index)!
	}
}

pub fn (app &App) render_window_eligible(id WindowId) !bool {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	index := app.render_window_index_locked(id)!
	window := app.render_runtime.windows[index]
	if window.status != .alive {
		return error(err_stale_window)
	}
	return app.render_window_has_claimable_work_locked(window, false)
}

// with_scheduled_render_batch opens one authoritative global transaction. The
// callback is invoked even with no candidates so app-scoped resource work can
// initialize and advance without a window frame callback.
pub fn (mut app App) with_scheduled_render_batch(f RenderBatchFn) !RenderBatchOutcome {
	return app.with_render_batch(false, false, f)
}

pub fn (mut app App) with_legacy_render_batch(f RenderBatchFn) !RenderBatchOutcome {
	return app.with_render_batch(true, false, f)
}

pub fn (mut app App) with_teardown_render_batch(f RenderBatchFn) !RenderBatchOutcome {
	return app.with_render_batch(false, true, f)
}

struct RenderBatchCandidatePlan {
	index    int
	serial   u64
	snapshot RenderWindowSnapshot
}

struct RenderBatchPlan {
	epoch             u64
	next_epoch        u64
	candidates        []RenderBatchCandidatePlan
	exhausted_windows []int
}

struct RenderTargetClaimPlan {
	index               int
	candidate           RenderWindowSnapshot
	target_epoch        u64
	next_target_epoch   u64
	window_epoch        u64
	next_window_epoch   u64
	claimed_dirty_epoch u64
}

fn (mut app App) with_render_batch(include_all bool, teardown bool, f RenderBatchFn) !RenderBatchOutcome {
	if f == unsafe { nil } {
		return error(err_render_nil_batch_callback)
	}
	app.assert_owner_thread()!
	app.state_mutex.lock()
	plan := app.plan_render_batch_locked(include_all, teardown) or {
		app.state_mutex.unlock()
		return err
	}
	if message := app.take_internal_fault(.renderer_batch_begin) {
		app.state_mutex.unlock()
		return error(message)
	}
	app.commit_render_batch_plan_locked(plan)
	lease := RenderBatchLease{
		app_instance: app.instance_id
		epoch:        plan.epoch
		include_all:  include_all
	}
	mut candidates := []RenderWindowSnapshot{cap: plan.candidates.len}
	for candidate in plan.candidates {
		candidates << candidate.snapshot
	}
	app.state_mutex.unlock()

	app.begin_backend_render_batch(lease) or {
		app.mark_renderer_terminal(err.msg())
		app.cancel_scheduler_batch(plan.epoch)
		return err
	}
	mut callback_error := IError(none)
	f(lease, candidates) or {
		callback_error = err
		app.fail_backend_render_batch(lease, err)
	}
	outcome := app.finish_backend_render_batch(lease) or {
		epilogue_error := app.abort_backend_render_batch(lease, err.msg())
		app.cancel_scheduler_batch(plan.epoch)
		return error(epilogue_error)
	}
	app.finish_scheduler_batch(plan.epoch)
	if callback_error !is none && outcome.error == ''
		&& !callback_matches_suppressed_attempt(callback_error, outcome) {
		return RenderBatchOutcome{
			...outcome
			error: callback_error.msg()
		}
	}
	return outcome
}

fn (app &App) plan_render_batch_locked(include_all bool, teardown bool) !RenderBatchPlan {
	if app.status != .running || (!teardown && (app.stopping || !app.admission_open)) {
		return error(err_app_stopped)
	}
	if app.render_runtime.renderer_terminal != '' {
		return error('${err_render_renderer_failed}: ${app.render_runtime.renderer_terminal}')
	}
	if app.render_runtime.batch_active {
		return error(err_render_batch_active)
	}
	epoch, next_epoch := plan_nonwrapping_counter(app.render_runtime.next_batch_epoch)!
	mut candidates := []RenderBatchCandidatePlan{}
	mut exhausted_windows := []int{}
	for i, window in app.render_runtime.windows {
		if teardown || !app.render_window_has_claimable_work_locked(window, include_all) {
			continue
		}
		candidate_serial := next_nonwrapping_u64(window.frame_serial) or {
			exhausted_windows << i
			continue
		}
		mut snapshot := render_snapshot_with_frame_serial(render_window_snapshot_from_state(window),
			candidate_serial)
		snapshot = render_snapshot_with_batch(snapshot, epoch)
		candidates << RenderBatchCandidatePlan{
			index:    i
			serial:   candidate_serial
			snapshot: snapshot
		}
	}
	return RenderBatchPlan{
		epoch:             epoch
		next_epoch:        next_epoch
		candidates:        candidates
		exhausted_windows: exhausted_windows
	}
}

fn (mut app App) commit_render_batch_plan_locked(plan RenderBatchPlan) {
	app.render_runtime.next_batch_epoch = plan.next_epoch
	app.render_runtime.batch_active = true
	app.render_runtime.active_batch_epoch = plan.epoch
	for index in plan.exhausted_windows {
		app.render_runtime.windows[index].status = .exhausted
		app.render_runtime.windows[index].block_reason = .renderer_failed
	}
	for candidate in plan.candidates {
		mut window := &app.render_runtime.windows[candidate.index]
		window.candidate_active = true
		window.candidate_batch_epoch = plan.epoch
		window.candidate_frame_serial = candidate.serial
		window.candidate_metrics = candidate.snapshot.metrics
		window.candidate_target = candidate.snapshot.target
	}
}

fn callback_matches_suppressed_attempt(callback_error IError, outcome RenderBatchOutcome) bool {
	$if gg_multiwindow ? || x_multiwindow_render ? {
		if callback_error is RecoverableRenderAttemptError {
			return callback_error.target == outcome.suppressed_callback_target
				&& callback_error.message != ''
				&& callback_error.message == outcome.suppressed_callback_message
				&& native_render_results_identical(callback_error.outcome, outcome.suppressed_callback_outcome)
		}
	}
	_ = callback_error
	_ = outcome
	return false
}

fn (app &App) plan_render_target_claim_locked(id WindowId, lease RenderBatchLease, next_target_epoch u64) !RenderTargetClaimPlan {
	app.validate_batch_lease_locked(lease)!
	index := app.render_window_index_locked(id)!
	window := app.render_runtime.windows[index]
	if !app.render_window_has_claimable_work_locked(window, lease.include_all) || window.in_frame {
		return error(err_render_target_not_eligible)
	}
	if !window.candidate_active || window.candidate_batch_epoch != lease.epoch
		|| window.candidate_frame_serial == 0 || window.candidate_metrics != window.metrics
		|| window.candidate_target != window.target {
		return error(err_render_attempt_stale)
	}
	window_epoch, next_window_epoch :=
		plan_nonwrapping_counter(app.render_runtime.next_lease_epoch)!
	target_epoch, following_target_epoch := plan_nonwrapping_counter(next_target_epoch)!
	mut candidate := render_snapshot_with_frame_serial(render_window_snapshot_from_state(window),
		window.candidate_frame_serial)
	candidate = render_snapshot_with_batch(candidate, lease.epoch)
	return RenderTargetClaimPlan{
		index:               index
		candidate:           candidate
		target_epoch:        target_epoch
		next_target_epoch:   following_target_epoch
		window_epoch:        window_epoch
		next_window_epoch:   next_window_epoch
		claimed_dirty_epoch: window.dirty_epoch
	}
}

fn (mut app App) commit_render_target_claim_locked(plan RenderTargetClaimPlan) {
	app.render_runtime.next_lease_epoch = plan.next_window_epoch
	mut window := &app.render_runtime.windows[plan.index]
	window.lease_epoch = plan.window_epoch
	window.in_frame = true
	window.batch_epoch = plan.candidate.batch_epoch
	window.claimed_dirty_epoch = plan.claimed_dirty_epoch
	window.ready_credit_consumed = true
	window.ready_credit = false
}

fn (mut app App) complete_render_target_preparation(index int, lease RenderBatchLease, candidate RenderWindowSnapshot) !RenderWindowSnapshot {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.validate_batch_lease_locked(lease)!
	if index < 0 || index >= app.render_runtime.windows.len {
		return error(err_render_attempt_stale)
	}
	mut window := &app.render_runtime.windows[index]
	if !window.in_frame || window.batch_epoch != lease.epoch || window.id != candidate.window
		|| window.lease_epoch == 0 || !window.candidate_active
		|| window.candidate_batch_epoch != lease.epoch
		|| window.candidate_frame_serial != candidate.frame_serial
		|| window.candidate_metrics != candidate.metrics
		|| window.candidate_target != candidate.target {
		return error(err_render_attempt_stale)
	}
	mut snapshot := render_snapshot_with_frame_serial(render_window_snapshot_from_state(window),
		candidate.frame_serial)
	snapshot = render_snapshot_with_batch(snapshot, lease.epoch)
	return snapshot
}

// linearize_render_target_acquisition is the only persistent frame-serial
// transition. All checks complete before the non-fallible assignment.
fn (mut app App) linearize_render_target_acquisition(target RenderTargetLease, expected RenderWindowSnapshot, actual_metrics RenderMetricsSnapshot, actual_target RenderTargetSnapshot) !RenderWindowSnapshot {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if target.app_instance != app.instance_id || target.batch_epoch == 0 || target.window_epoch == 0
		|| target.window != expected.window {
		return error(err_render_target_stale)
	}
	app.validate_batch_lease_locked(RenderBatchLease{
		app_instance: target.app_instance
		epoch:        target.batch_epoch
	})!
	index := app.render_window_index_locked(target.window)!
	mut window := &app.render_runtime.windows[index]
	if !window.in_frame || window.batch_epoch != target.batch_epoch
		|| window.lease_epoch != target.window_epoch || !window.candidate_active
		|| window.candidate_batch_epoch != target.batch_epoch
		|| window.candidate_frame_serial != expected.frame_serial
		|| window.candidate_metrics != expected.metrics
		|| window.candidate_target != expected.target || window.metrics != expected.metrics
		|| window.target != expected.target || actual_metrics != expected.metrics
		|| actual_target != expected.target || expected.frame_serial == 0
		|| window.frame_serial == u64(0xffffffffffffffff)
		|| expected.frame_serial != window.frame_serial + 1 {
		return error(err_render_target_stale)
	}
	window.frame_serial = expected.frame_serial
	mut snapshot := render_window_snapshot_from_state(window)
	snapshot = render_snapshot_with_batch(snapshot, target.batch_epoch)
	return snapshot
}

fn (app &App) render_window_lease_epoch(id WindowId, batch RenderBatchLease) !u64 {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.validate_batch_lease_locked(batch)!
	index := app.render_window_index_locked(id)!
	window := app.render_runtime.windows[index]
	if !window.in_frame || window.batch_epoch != batch.epoch || window.lease_epoch == 0 {
		return error(err_render_attempt_stale)
	}
	return window.lease_epoch
}

fn (mut app App) cancel_render_target_claim(index int, lease RenderBatchLease, reason RenderBlockReason) {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if lease.app_instance != app.instance_id || !app.render_runtime.batch_active
		|| app.render_runtime.active_batch_epoch != lease.epoch || index < 0
		|| index >= app.render_runtime.windows.len {
		return
	}
	mut window := &app.render_runtime.windows[index]
	if window.batch_epoch != lease.epoch {
		return
	}
	window.in_frame = false
	window.batch_epoch = 0
	window.lease_epoch = 0
	window.candidate_active = false
	window.candidate_batch_epoch = 0
	window.candidate_frame_serial = 0
	window.block_reason = reason
}

fn (mut app App) complete_render_submission(id WindowId, batch_epoch u64, finalized bool) string {
	app.state_mutex.lock()
	index := app.render_window_index_locked(id) or {
		app.state_mutex.unlock()
		return err.msg()
	}
	mut window := &app.render_runtime.windows[index]
	if !window.in_frame || window.batch_epoch != batch_epoch {
		app.state_mutex.unlock()
		return err_render_attempt_stale
	}
	mut resolved_frame := next_nonwrapping_u64(window.submitted_frame) or {
		window.in_frame = false
		window.batch_epoch = 0
		window.lease_epoch = 0
		app.state_mutex.unlock()
		return err_render_renderer_failed
	}
	if finalized {
		if window.submitted_frame >= window.frame_serial {
			app.state_mutex.unlock()
			return err_render_attempt_stale
		}
		window.submitted_frame = resolved_frame
		if window.claimed_dirty_epoch > window.consumed_dirty_epoch {
			window.consumed_dirty_epoch = window.claimed_dirty_epoch
		}
	}
	window.in_frame = false
	window.batch_epoch = 0
	window.lease_epoch = 0
	resolve_appkit := app.backend.kind == .appkit
	app.state_mutex.unlock()
	if resolve_appkit {
		app.backend.service_resolve_readbacks_after_submit(id, resolved_frame, finalized) or {
			return err.msg()
		}
	}
	return ''
}

fn (mut app App) finish_scheduler_batch(epoch u64) {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	for i, window in app.render_runtime.windows {
		if window.batch_epoch == epoch {
			app.render_runtime.windows[i].in_frame = false
			app.render_runtime.windows[i].batch_epoch = 0
			app.render_runtime.windows[i].lease_epoch = 0
		}
		if window.candidate_batch_epoch == epoch {
			app.render_runtime.windows[i].candidate_active = false
			app.render_runtime.windows[i].candidate_batch_epoch = 0
			app.render_runtime.windows[i].candidate_frame_serial = 0
		}
	}
	if app.render_runtime.active_batch_epoch == epoch {
		app.render_runtime.active_batch_epoch = 0
		app.render_runtime.batch_active = false
	}
}

fn (mut app App) cancel_scheduler_batch(epoch u64) {
	app.finish_scheduler_batch(epoch)
}

fn (mut app App) complete_redraw_admission(id WindowId, admission_id u64, admission_epoch u64) {
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if app.status != .running || app.stopping || !app.admission_open
		|| app.admission_epoch != admission_epoch {
		return
	}
	index := app.render_window_index_locked(id) or { return }
	mut window := &app.render_runtime.windows[index]
	if window.status != .alive || !window.pending_admission
		|| window.pending_admission_id != admission_id
		|| window.pending_admission_epoch != admission_epoch {
		return
	}
	window.pending_admission = false
	window.pending_admission_id = 0
	window.pending_admission_epoch = 0
	app.mark_render_window_dirty_locked(index) or {
		app.render_runtime.renderer_terminal = err.msg()
	}
}

fn (mut app App) register_render_window_locked(id WindowId, config WindowConfig, size WindowSize) ! {
	for app.render_runtime.windows.len <= id.slot {
		app.render_runtime.windows << RenderWindowRuntime{}
	}
	dirty_epoch := app.take_render_epoch_locked()!
	app.render_runtime.windows[id.slot] = RenderWindowRuntime{
		id:           id
		status:       .alive
		redraw_mode:  config.redraw_mode
		has_workload: config.render_workload
		dirty_epoch:  dirty_epoch
		block_reason: .backend_unavailable
		metrics:      RenderMetricsSnapshot{
			logical_width:  f32(size.width)
			logical_height: f32(size.height)
		}
	}
}

fn (mut app App) begin_render_window_close_locked(id WindowId) ! {
	index := app.render_window_index_locked(id)!
	mut window := &app.render_runtime.windows[index]
	if window.status != .alive {
		return error(err_stale_window)
	}
	window.status = .preparing_destroy
	window.pending_admission = false
	window.pending_admission_id = 0
	window.pending_admission_epoch = 0
}

fn (mut app App) seal_render_window_close_locked(id WindowId) ! {
	index := app.render_window_index_locked(id)!
	mut window := &app.render_runtime.windows[index]
	if window.status != .preparing_destroy {
		return error(err_window_destroy_ticket_stale)
	}
	window.status = .sealed_destroy
}

fn (mut app App) rollback_render_window_close_locked(id WindowId) {
	index := app.render_window_index_locked(id) or { return }
	if app.render_runtime.windows[index].status == .preparing_destroy {
		app.render_runtime.windows[index].status = .alive
	}
}

fn (mut app App) finish_render_window_close_locked(id WindowId) {
	index := app.render_window_index_locked(id) or { return }
	mut window := &app.render_runtime.windows[index]
	window.status = .destroyed
	window.pending_admission = false
	window.pending_admission_id = 0
	window.pending_admission_epoch = 0
	window.in_frame = false
	window.lease_epoch = 0
	window.batch_epoch = 0
	window.ready_credit = false
}

fn (mut app App) apply_backend_render_update_locked(update BackendRenderUpdate) {
	index := app.render_window_index_locked(update.window) or { return }
	mut window := &app.render_runtime.windows[index]
	if window.status != .alive {
		return
	}
	app.apply_backend_render_update_to_window_locked(mut window, update)
}

fn (app &App) apply_backend_render_update_to_window_locked(mut window RenderWindowRuntime, update BackendRenderUpdate) {
	if update.sequence == 0 || update.sequence <= window.eligibility_sequence {
		return
	}
	window.eligibility_sequence = update.sequence
	window.ready_credit = update.ready_credit
	window.ready_credit_consumed = false
	window.block_reason = update.block_reason
	window.metrics = update.metrics
	window.target = update.target
}

fn (mut app App) apply_unavailable_backend_observation_locked(index int, id WindowId, logical_width int, logical_height int, framebuffer_width int, framebuffer_height int, reason RenderBlockReason) {
	if index < 0 || index >= app.render_runtime.windows.len
		|| app.render_runtime.windows[index].id != id {
		return
	}
	mut window := &app.render_runtime.windows[index]
	window.metrics = RenderMetricsSnapshot{
		logical_width:        if logical_width > 0 { f32(logical_width) } else { 0 }
		logical_height:       if logical_height > 0 { f32(logical_height) } else { 0 }
		framebuffer_width:    if framebuffer_width > 0 { framebuffer_width } else { 0 }
		framebuffer_height:   if framebuffer_height > 0 { framebuffer_height } else { 0 }
		dpi_scale:            0
		metrics_sequence:     0
		metrics_available:    false
		conversion_available: false
	}
	window.target = RenderTargetSnapshot{}
	window.ready_credit = false
	window.ready_credit_consumed = false
	window.block_reason = reason
	app.mark_render_window_dirty_locked(index) or {
		app.render_runtime.renderer_terminal = err.msg()
	}
}

fn (mut app App) apply_backend_input_observation_locked(index int, event InputEvent) {
	if event.kind !in [.resized, .focused, .unfocused, .iconified, .restored] || index < 0
		|| index >= app.render_runtime.windows.len
		|| app.render_runtime.windows[index].id != event.window_id {
		return
	}
	mut window := &app.render_runtime.windows[index]
	match event.kind {
		.focused {
			window.focus_known = true
			window.focused = true
		}
		.unfocused {
			window.focus_known = true
			window.focused = false
		}
		.iconified {
			window.minimized_known = true
			window.minimized = true
		}
		.restored {
			window.minimized_known = true
			window.minimized = false
		}
		else {}
	}

	reason := if window.minimized_known && window.minimized {
		RenderBlockReason.minimized
	} else if event.kind == .resized {
		RenderBlockReason.resize_pending
	} else {
		RenderBlockReason.backend_unavailable
	}
	app.apply_unavailable_backend_observation_locked(index, event.window_id, event.window_width,
		event.window_height, event.framebuffer_width, event.framebuffer_height, reason)
}

fn (mut app App) mark_render_window_dirty_locked(index int) ! {
	mut window := &app.render_runtime.windows[index]
	if window.dirty_epoch <= window.consumed_dirty_epoch
		|| (window.in_frame && window.dirty_epoch == window.claimed_dirty_epoch) {
		window.dirty_epoch = app.take_render_epoch_locked()!
	}
}

fn (app &App) render_window_index_locked(id WindowId) !int {
	if id.app_instance != app.instance_id {
		return error(err_app_identity_mismatch)
	}
	if id.slot < 0 || id.slot >= app.render_runtime.windows.len {
		return error(err_window_not_found)
	}
	window := app.render_runtime.windows[id.slot]
	if window.id != id {
		return error(err_stale_window)
	}
	return id.slot
}

fn (app &App) render_window_snapshot_locked(id WindowId) !RenderWindowSnapshot {
	index := app.render_window_index_locked(id)!
	window := app.render_runtime.windows[index]
	if window.status == .invalid {
		return error(err_window_not_found)
	}
	return render_window_snapshot_from_state(window)
}

fn (app &App) render_window_has_claimable_work_locked(window RenderWindowRuntime, include_all bool) bool {
	if window.status != .alive || window.in_frame || !window.has_workload || !window.ready_credit
		|| window.ready_credit_consumed {
		return false
	}
	return include_all || window.redraw_mode == .continuous
		|| window.dirty_epoch > window.consumed_dirty_epoch
}

fn (app &App) validate_batch_lease_locked(lease RenderBatchLease) ! {
	if lease.app_instance != app.instance_id {
		return error(err_app_identity_mismatch)
	}
	if !app.render_runtime.batch_active || lease.epoch == 0
		|| app.render_runtime.active_batch_epoch != lease.epoch {
		return error(err_render_batch_stale)
	}
}

fn (mut app App) take_render_epoch_locked() !u64 {
	epoch, next_epoch := plan_nonwrapping_counter(app.render_runtime.next_epoch)!
	app.render_runtime.next_epoch = next_epoch
	return epoch
}

fn (mut app App) take_render_admission_id_locked() !u64 {
	admission_id, next_admission_id :=
		plan_nonwrapping_counter(app.render_runtime.next_admission_id)!
	app.render_runtime.next_admission_id = next_admission_id
	return admission_id
}

fn plan_nonwrapping_counter(value u64) !(u64, u64) {
	if value == 0 {
		return error(err_window_generation_exhausted)
	}
	next := if value == u64(0xffffffffffffffff) { u64(0) } else { value + 1 }
	return value, next
}

fn next_nonwrapping_u64(value u64) !u64 {
	if value == u64(0xffffffffffffffff) {
		return error(err_window_generation_exhausted)
	}
	return value + 1
}

fn next_backend_target_generation(value u64) !u64 {
	if value == 0 {
		return error(err_render_renderer_failed)
	}
	return next_nonwrapping_u64(value) or { return error(err_render_renderer_failed) }
}

@[markused]
fn exhaust_backend_target_generation(value u64) u64 {
	return next_backend_target_generation(value) or { u64(0) }
}

fn next_backend_render_sequence(value u64) !u64 {
	return next_nonwrapping_u64(value) or { return error(err_render_renderer_failed) }
}

fn render_window_snapshot_from_state(window RenderWindowRuntime) RenderWindowSnapshot {
	return RenderWindowSnapshot{
		window:               window.id
		redraw_mode:          window.redraw_mode
		dirty_epoch:          window.dirty_epoch
		consumed_epoch:       window.consumed_dirty_epoch
		frame_serial:         window.frame_serial
		submitted_frame:      window.submitted_frame
		metrics:              window.metrics
		target:               window.target
		eligibility_sequence: window.eligibility_sequence
		block_reason:         window.block_reason
		focus_known:          window.focus_known
		focused:              window.focused
		minimized_known:      window.minimized_known
		minimized:            window.minimized
		batch_epoch:          window.batch_epoch
	}
}

fn render_snapshot_with_batch(snapshot RenderWindowSnapshot, epoch u64) RenderWindowSnapshot {
	return RenderWindowSnapshot{
		...snapshot
		batch_epoch: epoch
	}
}

fn render_snapshot_with_frame_serial(snapshot RenderWindowSnapshot, frame_serial u64) RenderWindowSnapshot {
	return RenderWindowSnapshot{
		...snapshot
		frame_serial: frame_serial
	}
}
