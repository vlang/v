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

struct BackendServiceEventAcceptance {
	accepted      bool
	claim_storage bool
}

struct BackendTeardownTargetPlan {
	id               WindowId
	native_destroyed bool
}

struct BackendDestroyInputPlan {
mut:
	targets            []BackendTeardownTargetPlan
	native_upgrade     WindowId
	native_upgrade_set bool
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
	mut cancellation_capacity := 0
	mut extra_destroy_tokens := 0
	mut planned_teardowns := map[string]bool{}
	mut destroy_plans := []BackendDestroyInputPlan{len: events.len}
	for event_index, event in events {
		if event.kind != .lifecycle || event.lifecycle.kind != .window_destroyed {
			continue
		}
		root := event.lifecycle.window_id
		if !app.backend_window_generation_present_locked(root) {
			continue
		}
		root_slot := app.windows[root.slot]
		if root_slot.teardown_notice_pending {
			if !root_slot.backend_destroyed {
				destroy_plans[event_index].native_upgrade = root
				destroy_plans[event_index].native_upgrade_set = true
			}
			continue
		}
		if root_slot.destroy_stage !in [.none, .prepared, .sealed] {
			continue
		}
		order := app.services.child_first_order(root) or {
			app.state_mutex.unlock()
			return err
		}
		mut targets := []BackendTeardownTargetPlan{}
		for id in order {
			key := id.str()
			if app.windows[id.slot].teardown_notice_pending || planned_teardowns[key] {
				if id == root && !app.windows[id.slot].backend_destroyed {
					destroy_plans[event_index].native_upgrade = root
					destroy_plans[event_index].native_upgrade_set = true
				}
				continue
			}
			if app.windows[id.slot].destroy_stage !in [.none, .prepared, .sealed] {
				continue
			}
			if !app.windows[id.slot].services_cancelled {
				cancellation := app.collect_window_service_cancellation_locked(id) or {
					app.state_mutex.unlock()
					return err
				}
				cancellation_capacity += cancellation.service_events.len +
					cancellation.readback_events.len
			}
			targets << BackendTeardownTargetPlan{
				id:               id
				native_destroyed: id == root
			}
			planned_teardowns[key] = true
		}
		destroy_plans[event_index].targets = targets
		if targets.len > 1 {
			extra_destroy_tokens += targets.len - 1
		}
	}
	first_delivery_token := app.reserve_event_delivery_tokens_locked(events.len +
		extra_destroy_tokens + cancellation_capacity) or {
		app.state_mutex.unlock()
		return err
	}
	mut accepted := 0
	mut token_offset := 0
	for event_index, event in events {
		if event.kind == .lifecycle && event.lifecycle.kind == .window_destroyed {
			plan := destroy_plans[event_index]
			mut root_queued := false
			for target in plan.targets {
				if !app.windows[target.id.slot].services_cancelled {
					collected := app.collect_present_window_service_cancellation_locked(target.id)
					cancellation := WindowServiceCancellationPlan{
						...collected
						first_token: first_delivery_token + u64(token_offset)
					}
					cancellation_count := collected.service_events.len +
						collected.readback_events.len
					app.commit_window_service_cancellation_locked(cancellation)
					app.windows[target.id.slot].services_cancelled = true
					token_offset += cancellation_count
				}
				app.accept_backend_teardown_locked(target.id, target.native_destroyed)
				queued := app.queue_backend_teardown_event_locked(target.id, first_delivery_token +
					u64(token_offset))
				if target.id == event.lifecycle.window_id && queued {
					root_queued = true
				}
				token_offset++
			}
			if plan.native_upgrade_set {
				app.accept_backend_teardown_locked(plan.native_upgrade, true)
			}
			if plan.targets.len == 0 {
				// Every native input owns one reserved slot. A duplicate leaves the
				// slot as an intentional gap and never requeues a terminal event.
				token_offset++
			}
			if root_queued {
				accepted++
			}
			continue
		}
		mut generation_valid := app.backend_event_generation_valid_locked(event)
		delivery_token := first_delivery_token + u64(token_offset)
		// Every native input owns its reserved position even when validation below
		// rejects it or a pending terminal lookup fails.
		token_offset++
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
			.service {
				decision := if generation_valid {
					app.accept_backend_service_event_locked(event.service, delivery_token)
				} else {
					BackendServiceEventAcceptance{}
				}
				if decision.accepted {
					if decision.claim_storage {
						app.backend.claim_service_event_storage(event.service)
					} else {
						app.backend.discard_unaccepted_service_event_storage(event.service)
					}
					accepted++
				} else {
					app.backend.discard_unaccepted_service_event_storage(event.service)
				}
			}
			.readback {
				if generation_valid {
					readback := app.accept_backend_readback_event_locked(event.readback) or {
						continue
					}
					app.enqueue_reserved_event_locked(queued_readback_event(readback),
						delivery_token)
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

fn (mut app App) accept_backend_readback_event_locked(event ServiceReadbackResult) !ServiceReadbackResult {
	index := app.pending_readback_index_locked(event.id)!
	mut normalized := event
	if event.window != event.id.window {
		return error(err_service_request_stale)
	}
	if event.status == .ready {
		mut valid_layout := true
		validate_service_readback_rgba8_layout(event.width, event.height, event.stride,
			event.pixels_rgba8.len) or { valid_layout = false }
		actual_bytes := u64(event.pixels_rgba8.len)
		reserved_bytes := app.services.readbacks[index].payload_bytes
		if !valid_layout || (reserved_bytes > 0 && actual_bytes > reserved_bytes) {
			app.services.release_pending_readback_payload(index)
			normalized = ServiceReadbackResult{
				id:     event.id
				window: event.window
				status: .failed
				error:  err_readback_invalid
			}
		} else if !app.services.resize_pending_readback_payload(index, actual_bytes) {
			app.services.release_pending_readback_payload(index)
			normalized = ServiceReadbackResult{
				id:     event.id
				window: event.window
				status: .failed
				error:  err_readback_capacity
			}
		}
	} else {
		app.services.release_pending_readback_payload(index)
		normalized = ServiceReadbackResult{
			id:     event.id
			window: event.window
			status: event.status
			error:  event.error
		}
	}
	app.mark_pending_window_readback_terminal_locked(event.id)!
	return normalized
}

fn (mut app App) accept_backend_service_event_locked(event ServiceEvent, delivery_token u64) BackendServiceEventAcceptance {
	mut accepted_event := event
	mut claim_storage := true
	if event.kind == .clipboard {
		request_id := event.clipboard.id
		expected_kind := if event.operation == .clipboard_write {
			PendingServiceKind.clipboard_write
		} else if event.operation == .clipboard_read {
			PendingServiceKind.clipboard_read
		} else {
			return BackendServiceEventAcceptance{}
		}
		if event.window != event.clipboard.window {
			return BackendServiceEventAcceptance{}
		}
		mut matched_index := -1
		for index, request in app.services.pending {
			if request.id == request_id && request.window == event.window
				&& request.kind == expected_kind && !request.terminal {
				matched_index = index
				break
			}
		}
		if matched_index < 0 {
			return BackendServiceEventAcceptance{}
		}
		if event.clipboard.status == .ready {
			if expected_kind == .clipboard_read {
				if !app.services.resize_pending_service_payload(matched_index,
					u64(event.clipboard.text.len)) {
					app.services.resize_pending_service_payload(matched_index, 0)
					accepted_event = ServiceEvent{
						...event
						clipboard: ServiceClipboardResult{
							...event.clipboard
							status: .failed
							text:   ''
							error:  err_clipboard_capacity
						}
					}
					claim_storage = false
				}
			} else if u64(event.clipboard.text.len) > app.services.pending[matched_index].payload_bytes {
				app.services.resize_pending_service_payload(matched_index, 0)
				accepted_event = ServiceEvent{
					...event
					clipboard: ServiceClipboardResult{
						...event.clipboard
						status: .failed
						text:   ''
						error:  err_clipboard_capacity
					}
				}
				claim_storage = false
			}
		} else {
			app.services.resize_pending_service_payload(matched_index, 0)
			claim_storage = false
			accepted_event = ServiceEvent{
				...event
				clipboard: ServiceClipboardResult{
					id:     event.clipboard.id
					window: event.clipboard.window
					status: event.clipboard.status
					error:  event.clipboard.error
				}
			}
		}
		app.services.pending[matched_index].terminal = true
	} else if event.kind == .portal_parent {
		mut lease_matched := false
		for lease in app.services.portal_leases {
			if lease.id == event.portal_parent.lease && lease.window == event.window {
				lease_matched = true
				break
			}
		}
		if !lease_matched || event.portal_parent.id.serial == 0
			|| event.portal_parent.window != event.window {
			return BackendServiceEventAcceptance{}
		}
		mut request_matched := false
		for index, request in app.services.pending {
			if request.id == event.portal_parent.id && request.window == event.window
				&& request.kind == .portal_parent && !request.terminal {
				app.services.pending[index].terminal = true
				request_matched = true
				break
			}
		}
		if !request_matched {
			return BackendServiceEventAcceptance{}
		}
		if event.portal_parent.status != .ready {
			for index, lease in app.services.portal_leases {
				if lease.id == event.portal_parent.lease {
					app.services.portal_leases.delete(index)
					break
				}
			}
		}
	}
	mut sequenced := service_event_with_sequence(accepted_event, delivery_token)
	if event.kind == .state {
		index := app.services.window_index(event.window) or {
			return BackendServiceEventAcceptance{}
		}
		registered_state := service_window_state_with_registered_monitor_membership(event.state,
			app.services.monitors)
		merged := merge_service_window_state(app.services.windows[index].state, registered_state)
		if event.operation == .focus
			&& service_window_state_observation_equal(app.services.windows[index].state, merged) {
			return BackendServiceEventAcceptance{}
		}
		authoritative := service_window_state_with_sequence(merged, delivery_token)
		app.services.windows[index].state = authoritative
		sequenced = ServiceEvent{
			...sequenced
			state: authoritative
		}
	} else if event.kind == .metrics {
		index := app.services.window_index(event.window) or {
			return BackendServiceEventAcceptance{}
		}
		registered_state := service_window_state_with_registered_monitor_membership(event.state,
			app.services.monitors)
		merged := merge_service_window_state(app.services.windows[index].state, registered_state)
		authoritative := service_window_state_with_sequence(merged, delivery_token)
		metrics := RenderMetricsSnapshot{
			...event.metrics
			metrics_sequence: delivery_token
		}
		app.services.windows[index].state = authoritative
		app.services.windows[index].metrics = metrics
		sequenced = ServiceEvent{
			...sequenced
			state:   authoritative
			metrics: metrics
		}
	} else if event.kind == .monitor {
		snapshot := if event.monitors.len > 0 {
			event.monitors
		} else if event.monitor.id.generation != 0 {
			[event.monitor]
		} else {
			[]ServiceMonitorInfo{}
		}
		monitors := app.services.reconcile_monitor_snapshot(snapshot, delivery_token) or {
			return BackendServiceEventAcceptance{}
		}
		sequenced = ServiceEvent{
			...sequenced
			monitor:  if monitors.len > 0 { monitors[0] } else { event.monitor }
			monitors: monitors
		}
	}
	app.enqueue_reserved_event_locked(queued_service_event(sequenced), delivery_token)
	return BackendServiceEventAcceptance{
		accepted:      true
		claim_storage: claim_storage
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
	if event.kind == .service {
		app.backend.release_delivered_service_event_storage(event.service)
	}
	app.release_delivered_service_storage_locked(event)
}

fn (mut app App) release_delivered_service_storage_locked(event QueuedEvent) {
	match event.kind {
		.service {
			request_id := match event.service.kind {
				.clipboard { event.service.clipboard.id }
				.portal_parent { event.service.portal_parent.id }
				else { ServiceRequestId{} }
			}
			if request_id.serial == 0 {
				return
			}
			for index, request in app.services.pending {
				if request.id == request_id && request.terminal {
					app.services.release_pending_service_payload(index)
					app.services.pending.delete(index)
					return
				}
			}
		}
		.readback {
			for index, request in app.services.readbacks {
				if request.id == event.readback.id && request.terminal {
					app.services.release_pending_readback_payload(index)
					app.services.readbacks.delete(index)
					return
				}
			}
		}
		else {}
	}
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
