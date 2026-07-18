module multiwindow

const err_event_delivery_exhausted = 'multiwindow: event delivery sequence exhausted'
const err_event_delivery_stale = 'multiwindow: event delivery obligation is stale'
const err_event_dispatch_active = 'multiwindow: managed event dispatch is active'
const err_event_admission_deferred_error = 'multiwindow: event admission is closed by a pending backend error'

enum EventDeliveryState {
	invalid
	queued
	in_flight
}

struct BackendEventAcceptance {
	accepted       int
	barrier_token  u64
	delivery_error string
}

// reserve_event_delivery_tokens_locked makes native batch acceptance infallible
// after its state mutations begin. Rejected native events leave harmless gaps.
fn (mut app App) reserve_event_delivery_tokens_locked(count int) !u64 {
	if count < 0 {
		return error(err_event_delivery_exhausted)
	}
	if count == 0 {
		return 0
	}
	if app.next_event_delivery_token == 0 {
		return error(err_event_delivery_exhausted)
	}
	available := u64(0xffffffffffffffff) - app.next_event_delivery_token + 1
	if u64(count) > available {
		return error(err_event_delivery_exhausted)
	}
	first := app.next_event_delivery_token
	if u64(count) == available {
		app.next_event_delivery_token = 0
	} else {
		app.next_event_delivery_token += u64(count)
	}
	return first
}

fn (mut app App) enqueue_reserved_event_locked(event QueuedEvent, token u64) {
	app.event_deliveries[token] = .queued
	app.events << queued_event_with_delivery_token(event, token)
}

fn (mut app App) accept_backend_event_batch(events []QueuedEvent, frame_count u64) !BackendEventAcceptance {
	app.state_mutex.lock()
	first_delivery_token := app.reserve_event_delivery_tokens_locked(events.len) or {
		app.state_mutex.unlock()
		return err
	}
	mut accepted := 0
	for event_index, event in events {
		delivery_token := first_delivery_token + u64(event_index)
		mut generation_valid := app.backend_event_generation_valid_locked(event)
		if event.kind == .lifecycle && event.lifecycle.kind == .window_destroyed {
			generation_valid = app.accept_backend_teardown_locked(event.lifecycle.window_id)
		}
		match event.kind {
			.lifecycle {
				if app.accept_lifecycle_event_locked(event.lifecycle, generation_valid,
					delivery_token)
				{
					accepted++
				}
			}
			.input {
				if app.accept_input_event_locked(event.input, frame_count, generation_valid,
					delivery_token)
				{
					accepted++
				}
			}
		}
	}
	app.frame_count = frame_count
	barrier_token := app.last_reserved_delivery_token_locked()
	app.state_mutex.unlock()

	acknowledgement_error := app.backend.acknowledge_queued_events()
	terminal_error := app.backend.event_sequence_terminal_error()
	delivery_error := merge_backend_errors(acknowledgement_error, terminal_error)
	app.state_mutex.lock()
	if terminal_error != '' {
		app.backend_event_terminal = terminal_error
	}
	if app.stopping && app.deferred_poll_error_active
		&& barrier_token > app.deferred_poll_barrier_token {
		app.deferred_poll_barrier_token = barrier_token
	}
	if delivery_error != '' {
		app.defer_poll_error_locked(barrier_token, delivery_error)
	}
	app.state_mutex.unlock()
	return BackendEventAcceptance{
		accepted:       accepted
		barrier_token:  barrier_token
		delivery_error: delivery_error
	}
}

// harvest_backend_events_for_stop promotes a complete retained native batch
// while window generations and teardown snapshots are still authoritative.
fn (mut app App) harvest_backend_events_for_stop() !int {
	app.state_mutex.lock()
	if app.status == .stopped {
		app.state_mutex.unlock()
		return 0
	}
	if !app.stopping {
		app.state_mutex.unlock()
		return error(err_app_stopped)
	}
	if app.event_dispatch_active {
		app.state_mutex.unlock()
		return error(err_event_dispatch_active)
	}
	frame_count := next_nonwrapping_u64(app.frame_count) or {
		app.render_runtime.renderer_terminal = err_render_renderer_failed
		app.state_mutex.unlock()
		return error(err_render_renderer_failed)
	}
	app.state_mutex.unlock()

	events := app.backend.poll_queued_events()!
	acceptance := app.accept_backend_event_batch(events, frame_count)!
	if acceptance.delivery_error != '' {
		return acceptance.accepted
	}
	render_updates := app.backend.render_updates() or {
		app.mark_renderer_terminal(err.msg())
		app.state_mutex.lock()
		app.defer_poll_error_locked(acceptance.barrier_token, err.msg())
		app.state_mutex.unlock()
		return acceptance.accepted
	}
	app.state_mutex.lock()
	for update in render_updates {
		app.apply_backend_render_update_locked(update)
	}
	app.state_mutex.unlock()
	return acceptance.accepted
}

fn (app &App) validate_queued_delivery_locked(event QueuedEvent) ! {
	if event.delivery_token == 0 {
		return error(err_event_delivery_stale)
	}
	state := app.event_deliveries[event.delivery_token] or {
		return error(err_event_delivery_stale)
	}
	if state != .queued {
		return error(err_event_delivery_stale)
	}
}

fn (mut app App) complete_queued_delivery_locked(event QueuedEvent) {
	app.event_deliveries.delete(event.delivery_token)
}

fn (mut app App) release_terminal_delivery_storage_locked() {
	if !app.event_delivery_terminal || app.event_dispatch_active || app.event_deliveries.len != 0 {
		return
	}
	app.events.clear()
	app.event_dispatch_events.clear()
	app.event_dispatch_index = 0
}

fn (app &App) delivery_barrier_pending_locked(barrier_token u64) bool {
	for token, state in app.event_deliveries {
		if state != .invalid && token <= barrier_token {
			return true
		}
	}
	return false
}

fn (app &App) last_reserved_delivery_token_locked() u64 {
	if app.next_event_delivery_token == 0 {
		return u64(0xffffffffffffffff)
	}
	return app.next_event_delivery_token - 1
}

fn (mut app App) defer_poll_error_locked(barrier_token u64, message string) {
	if message == '' {
		return
	}
	if app.deferred_poll_error_active {
		if barrier_token > app.deferred_poll_barrier_token {
			app.deferred_poll_barrier_token = barrier_token
		}
		if message != app.deferred_poll_error {
			app.deferred_poll_error = '${app.deferred_poll_error}; ${message}'
		}
		return
	}
	app.deferred_poll_error_active = true
	app.deferred_poll_barrier_token = barrier_token
	app.deferred_poll_error = message
}

fn (app &App) ensure_event_admission_open_locked() ! {
	if app.deferred_poll_error_active {
		return error(err_event_admission_deferred_error)
	}
}

fn (mut app App) take_deferred_poll_error_locked() string {
	deferred_error := app.deferred_poll_error
	app.deferred_poll_error_active = false
	app.deferred_poll_barrier_token = 0
	app.deferred_poll_error = ''
	return deferred_error
}

// seal_event_deliveries_for_stop_locked preserves terminally observable events.
// Any undispatched suffix precedes later admissions because its tokens are older.
fn (mut app App) seal_event_deliveries_for_stop_locked() ! {
	if app.event_dispatch_active {
		app.requeue_event_dispatch_suffix_locked()!
	}
	app.event_delivery_terminal = true
	app.release_terminal_delivery_storage_locked()
}

fn (mut app App) requeue_event_dispatch_suffix_locked() ! {
	if !app.event_dispatch_active || app.event_dispatch_index < 0
		|| app.event_dispatch_index > app.event_dispatch_events.len {
		return error(err_event_delivery_stale)
	}
	remaining := app.event_dispatch_events[app.event_dispatch_index..].clone()
	for event in remaining {
		state := app.event_deliveries[event.delivery_token] or {
			return error(err_event_delivery_stale)
		}
		if state != .in_flight {
			return error(err_event_delivery_stale)
		}
	}
	mut requeued := []QueuedEvent{cap: remaining.len + app.events.len}
	requeued << remaining
	requeued << app.events
	for event in remaining {
		app.event_deliveries[event.delivery_token] = .queued
	}
	app.events = requeued
	app.clear_event_dispatch_locked()
}

fn (mut app App) clear_event_dispatch_locked() {
	app.event_dispatch_events.clear()
	app.event_dispatch_active = false
	app.event_dispatch_index = 0
}
