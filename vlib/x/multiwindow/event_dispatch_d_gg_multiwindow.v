module multiwindow

const err_event_dispatch_callback_nil = 'multiwindow: managed event dispatch callback is nil'

// dispatch_events_for_gg gives one callback-scoped delivery cut to gg while
// App retains all acknowledgement and replay authority.
pub fn (mut app App) dispatch_events_for_gg(callback fn (QueuedEvent) !bool) !(int, bool) {
	app.assert_owner_thread()!
	if callback == unsafe { nil } {
		return error(err_event_dispatch_callback_nil)
	}
	app.state_mutex.lock()
	app.ensure_running_locked() or {
		app.state_mutex.unlock()
		return err
	}
	if app.event_dispatch_active {
		app.state_mutex.unlock()
		return error(err_event_dispatch_active)
	}
	mut dispatch_count := 0
	for event in app.events {
		if app.queued_event_blocked_by_teardown_locked(event) {
			break
		}
		dispatch_count++
	}
	if dispatch_count == 0 {
		app.state_mutex.unlock()
		return 0, false
	}
	internal_events := app.events[..dispatch_count].clone()
	for event in internal_events {
		app.validate_queued_delivery_locked(event) or {
			app.state_mutex.unlock()
			return err
		}
	}
	for event in internal_events {
		app.event_deliveries[event.delivery_token] = .in_flight
	}
	app.events = app.events[dispatch_count..].clone()
	app.event_dispatch_events = internal_events
	app.event_dispatch_active = true
	app.event_dispatch_index = 0
	app.state_mutex.unlock()

	mut delivered := 0
	for {
		app.state_mutex.lock()
		if !app.event_dispatch_active {
			app.state_mutex.unlock()
			return delivered, true
		}
		if app.event_dispatch_index < 0 || app.event_dispatch_index >= app.event_dispatch_events.len {
			app.state_mutex.unlock()
			return error(err_event_delivery_stale)
		}
		event :=
			queued_event_without_delivery_token(app.event_dispatch_events[app.event_dispatch_index])
		app.state_mutex.unlock()

		should_continue := callback(event) or {
			callback_error := err
			mut replay_error := ''
			app.state_mutex.lock()
			if app.event_dispatch_active {
				app.requeue_event_dispatch_suffix_locked() or { replay_error = err.msg() }
			}
			app.state_mutex.unlock()
			if replay_error != '' {
				return error('${callback_error.msg()}; ${replay_error}')
			}
			return callback_error
		}

		app.state_mutex.lock()
		if !app.event_dispatch_active {
			app.state_mutex.unlock()
			return delivered + 1, true
		}
		app.complete_event_dispatch_current_locked() or {
			completion_error := err
			app.requeue_event_dispatch_suffix_locked() or {}
			app.state_mutex.unlock()
			return completion_error
		}
		delivered++
		running := app.status == .running && !app.stopping
		if !should_continue || !running {
			app.requeue_event_dispatch_suffix_locked() or {
				app.state_mutex.unlock()
				return err
			}
			app.state_mutex.unlock()
			return delivered, true
		}
		if app.event_dispatch_index == app.event_dispatch_events.len {
			app.clear_event_dispatch_locked()
			app.state_mutex.unlock()
			return delivered, false
		}
		app.state_mutex.unlock()
	}
	return delivered, false
}

// post_dispatch_error_gate_for_gg blocks jobs/render until the active delivery
// cut is complete, then transfers its deferred error exactly once.
pub fn (mut app App) post_dispatch_error_gate_for_gg() !bool {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	if app.event_dispatch_active {
		app.state_mutex.unlock()
		return false
	}
	if !app.deferred_poll_error_active {
		app.state_mutex.unlock()
		return true
	}
	if app.delivery_barrier_pending_locked(app.deferred_poll_barrier_token) {
		app.state_mutex.unlock()
		return false
	}
	deferred_error := app.take_deferred_poll_error_locked()
	app.state_mutex.unlock()
	return error(deferred_error)
}

// defer_dispatch_error_for_gg binds a managed cleanup failure to every event
// admitted through the current delivery cut.
pub fn (mut app App) defer_dispatch_error_for_gg(message string) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	app.defer_poll_error_locked(app.last_reserved_delivery_token_locked(), message)
	app.state_mutex.unlock()
}

// ensure_event_admission_for_gg prevents gg-side preparatory work from running
// when the authoritative core admission cut is already closed.
pub fn (app &App) ensure_event_admission_for_gg() ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	app.ensure_running_locked()!
	app.ensure_event_admission_open_locked()!
}

fn (mut app App) complete_event_dispatch_current_locked() ! {
	if !app.event_dispatch_active || app.event_dispatch_index < 0
		|| app.event_dispatch_index >= app.event_dispatch_events.len {
		return error(err_event_delivery_stale)
	}
	event := app.event_dispatch_events[app.event_dispatch_index]
	state := app.event_deliveries[event.delivery_token] or {
		return error(err_event_delivery_stale)
	}
	if state != .in_flight {
		return error(err_event_delivery_stale)
	}
	app.event_deliveries.delete(event.delivery_token)
	app.event_dispatch_index++
}
