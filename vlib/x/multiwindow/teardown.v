module multiwindow

pub fn (mut app App) prepare_window_destroy(id WindowId) !WindowDestroyTicket {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if app.event_dispatch_active {
		return error(err_event_dispatch_active)
	}
	if app.status != .running || app.stopping {
		return error(err_app_stopped)
	}
	app.ensure_event_admission_open_locked()!
	return app.prepare_window_destroy_locked(id)
}

pub fn (mut app App) prepare_window_destroy_for_stop(id WindowId) !WindowDestroyTicket {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if app.status != .running || !app.stopping {
		return error(err_app_stopped)
	}
	return app.prepare_window_destroy_locked(id)
}

fn (mut app App) prepare_window_destroy_locked(id WindowId) !WindowDestroyTicket {
	index := app.live_window_index(id)!
	mut slot := &app.windows[index]
	if slot.destroy_stage != .none {
		return error(err_window_destroy_prepared)
	}
	if message := app.take_internal_fault(.teardown_prepare) {
		return error(message)
	}
	serial := app.take_destroy_serial_locked()!
	slot.destroy_stage = .prepared
	slot.destroy_serial = serial
	slot.backend_destroyed = false
	app.begin_render_window_close_locked(id)!
	return WindowDestroyTicket{
		app_instance: app.instance_id
		window:       id
		serial:       serial
	}
}

pub fn (mut app App) rollback_window_destroy(ticket WindowDestroyTicket) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	index := app.validate_destroy_ticket_locked(ticket, .prepared)!
	app.windows[index].destroy_stage = .none
	app.windows[index].destroy_serial = 0
	app.windows[index].backend_destroyed = false
	app.rollback_render_window_close_locked(ticket.window)
}

pub fn (mut app App) seal_window_destroy(ticket WindowDestroyTicket) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	index := app.validate_destroy_ticket_locked(ticket, .prepared)!
	if message := app.take_internal_fault(.teardown_seal) {
		return error(message)
	}
	app.windows[index].destroy_stage = .sealed
	app.seal_render_window_close_locked(ticket.window)!
}

// seal_window_destroy_terminal_for_stop is the irreversible fallback used only
// after normal prepare/seal failed during stop. Validation happens before the
// slot is sealed; an accepted live generation already owns its fallback serial.
pub fn (mut app App) seal_window_destroy_terminal_for_stop(id WindowId) !WindowDestroyTicket {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	defer {
		app.state_mutex.unlock()
	}
	if app.status != .running || !app.stopping {
		return error(err_app_stopped)
	}
	return app.seal_window_destroy_terminal_locked(id)
}

fn (mut app App) seal_window_destroy_terminal_locked(id WindowId) !WindowDestroyTicket {
	if id.app_instance != app.instance_id {
		return error(err_app_identity_mismatch)
	}
	if id.slot < 0 || id.slot >= app.windows.len || app.windows[id.slot].id != id
		|| app.windows[id.slot].status != .alive {
		return error(err_stale_window)
	}
	return app.seal_window_destroy_terminal_index_locked(id.slot)
}

fn (mut app App) seal_window_destroy_terminal_index_locked(index int) WindowDestroyTicket {
	mut slot := &app.windows[index]
	id := slot.id
	serial := terminal_destroy_serial(slot)
	slot.destroy_stage = .sealed
	slot.destroy_serial = serial
	slot.backend_destroyed = false
	app.seal_render_window_terminal_locked(id)
	return WindowDestroyTicket{
		app_instance: app.instance_id
		window:       id
		serial:       serial
	}
}

fn terminal_destroy_serial(slot WindowSlot) u64 {
	if slot.destroy_stage in [.prepared, .sealed] && slot.destroy_serial != 0 {
		return slot.destroy_serial
	}
	if slot.native_teardown_serial != 0 {
		return slot.native_teardown_serial
	}
	// This value is reachable only for a structurally corrupt pre-contract slot;
	// native destruction and stop must still become irreversible.
	return u64(0xffffffffffffffff)
}

fn (mut app App) seal_render_window_terminal_locked(id WindowId) {
	if id.slot < 0 || id.slot >= app.render_runtime.windows.len
		|| app.render_runtime.windows[id.slot].id != id {
		return
	}
	mut window := &app.render_runtime.windows[id.slot]
	window.status = .sealed_destroy
	window.pending_admission = false
	window.pending_admission_id = 0
	window.pending_admission_epoch = 0
	window.ready_credit = false
}

// finish_window_destroy is irreversible. It attempts backend cleanup even when
// earlier package cleanup failed and records one aggregate terminal outcome.
pub fn (mut app App) finish_window_destroy(ticket WindowDestroyTicket, prior_errors []string) ! {
	app.assert_owner_thread()!
	mut errors := prior_errors.clone()
	app.state_mutex.lock()
	index := app.validate_destroy_ticket_locked(ticket, .sealed) or {
		app.state_mutex.unlock()
		if app.window_destroy_finished(ticket.window) {
			return app.return_window_terminal(ticket.window)
		}
		return err
	}
	backend_destroyed := app.windows[index].backend_destroyed
	mut destroy_event_delivery_token := u64(0)
	if !app.windows[index].destroy_event_queued && !app.windows[index].destroy_event_emitted {
		destroy_event_delivery_token = app.reserve_event_delivery_tokens_locked(1) or {
			errors << err.msg()
			u64(0)
		}
	}
	app.state_mutex.unlock()

	app.backend.finish_window_teardown(ticket.window) or { errors << err.msg() }
	if message := app.take_internal_fault(.teardown_backend_finish) {
		errors << message
	}

	app.state_mutex.lock()
	if index < app.windows.len && app.windows[index].id == ticket.window
		&& app.windows[index].destroy_serial == ticket.serial {
		mut slot := &app.windows[index]
		slot.status = .destroyed
		slot.destroy_stage = .finished
		slot.backend_destroyed = backend_destroyed
		app.finish_render_window_close_locked(ticket.window)
		if slot.destroy_event_queued {
			slot.destroy_event_ready = true
			slot.destroy_event_emitted = true
		} else if !slot.destroy_event_emitted && destroy_event_delivery_token != 0 {
			app.enqueue_reserved_event_locked(queued_lifecycle_event(Event{
				kind:      .window_destroyed
				window_id: ticket.window
			}), destroy_event_delivery_token)
			slot.destroy_event_emitted = true
		}
		terminal := aggregate_terminal_errors(err_render_terminal_aggregate, errors)
		slot.destroy_terminal = terminal
		key := ticket.window.str()
		app.window_finished[key] = true
		app.window_terminal[key] = terminal
		slot.teardown_notice_pending = false
		mut acceptance_index := -1
		for i, id in app.teardown_acceptance_order {
			if id == ticket.window {
				acceptance_index = i
				break
			}
		}
		if acceptance_index >= 0 {
			app.teardown_acceptance_order.delete(acceptance_index)
		}
	}
	app.state_mutex.unlock()
	return app.return_window_terminal(ticket.window)
}

pub fn (mut app App) drain_render_teardown_notices() ![]RenderTeardownNotice {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	mut notices := []RenderTeardownNotice{}
	mut sequences := []u64{}
	mut included := map[string]bool{}
	for id in app.teardown_acceptance_order {
		if id.slot < 0 || id.slot >= app.windows.len {
			continue
		}
		slot := app.windows[id.slot]
		if slot.id != id || !slot.teardown_notice_pending || slot.teardown_sequence == 0 {
			continue
		}
		notices << render_teardown_notice_from_slot(app.instance_id, slot)
		sequences << slot.teardown_sequence
		included[id.str()] = true
	}
	for slot in app.windows {
		if !slot.teardown_notice_pending || slot.teardown_sequence == 0 || included[slot.id.str()] {
			continue
		}
		notice := render_teardown_notice_from_slot(app.instance_id, slot)
		mut insert_at := sequences.len
		for insert_at > 0 && sequences[insert_at - 1] > slot.teardown_sequence {
			insert_at--
		}
		sequences.insert(insert_at, slot.teardown_sequence)
		notices.insert(insert_at, notice)
	}
	app.state_mutex.unlock()
	return notices
}

fn render_teardown_notice_from_slot(app_instance u64, slot WindowSlot) RenderTeardownNotice {
	return RenderTeardownNotice{
		window:   slot.id
		snapshot: slot.teardown_snapshot
		ticket:   WindowDestroyTicket{
			app_instance: app_instance
			window:       slot.id
			serial:       slot.destroy_serial
		}
	}
}

fn (mut app App) accept_backend_teardown_locked(id WindowId) bool {
	if id.app_instance != app.instance_id || id.slot < 0 || id.slot >= app.windows.len {
		return false
	}
	mut slot := &app.windows[id.slot]
	if slot.id != id || slot.status != .alive {
		return false
	}
	if slot.backend_destroyed {
		return true
	}
	sequence := app.take_teardown_sequence_locked()
	serial := if slot.destroy_stage == .prepared && slot.destroy_serial != 0 {
		slot.destroy_serial
	} else if slot.destroy_stage == .sealed && slot.destroy_serial != 0 {
		slot.destroy_serial
	} else {
		terminal_destroy_serial(slot)
	}
	mut snapshot := RenderWindowSnapshot{
		window: id
	}
	if id.slot < app.render_runtime.windows.len && app.render_runtime.windows[id.slot].id == id {
		snapshot = render_window_snapshot_from_state(app.render_runtime.windows[id.slot])
	}

	// Native destruction is irreversible. Its ticket may use a pre-reserved
	// fallback, but ordering is stamped only at actual backend acceptance.
	slot.destroy_stage = .sealed
	slot.destroy_serial = serial
	slot.backend_destroyed = true
	slot.teardown_sequence = sequence
	slot.teardown_snapshot = snapshot
	slot.teardown_notice_pending = true
	app.teardown_acceptance_order << id
	app.seal_render_window_terminal_locked(id)
	return true
}

fn (mut app App) queue_backend_teardown_event_locked(id WindowId, delivery_token u64) bool {
	if id.app_instance != app.instance_id || id.slot < 0 || id.slot >= app.windows.len {
		return false
	}
	mut slot := &app.windows[id.slot]
	if slot.id != id || slot.status != .alive || !slot.backend_destroyed
		|| slot.destroy_event_queued || slot.destroy_event_emitted {
		return false
	}
	slot.destroy_event_queued = true
	slot.destroy_event_ready = false
	app.enqueue_reserved_event_locked(queued_lifecycle_event(Event{
		kind:      .window_destroyed
		window_id: id
	}), delivery_token)
	return true
}

fn (app &App) validate_destroy_ticket_locked(ticket WindowDestroyTicket, stage WindowDestroyStage) !int {
	if ticket.app_instance != app.instance_id || ticket.window.app_instance != app.instance_id {
		return error(err_app_identity_mismatch)
	}
	if ticket.window.slot < 0 || ticket.window.slot >= app.windows.len {
		return error(err_window_destroy_ticket_stale)
	}
	slot := app.windows[ticket.window.slot]
	if slot.id != ticket.window || slot.destroy_serial != ticket.serial
		|| slot.destroy_stage != stage {
		return error(err_window_destroy_ticket_stale)
	}
	return ticket.window.slot
}

fn (mut app App) take_destroy_serial_locked() !u64 {
	serial := app.render_runtime.next_destroy_serial
	if serial == 0 {
		return error(err_window_generation_exhausted)
	}
	if serial == u64(0xffffffffffffffff) {
		app.render_runtime.next_destroy_serial = 0
	} else {
		app.render_runtime.next_destroy_serial++
	}
	return serial
}

fn (mut app App) take_teardown_sequence_locked() u64 {
	sequence := app.render_runtime.next_teardown_sequence
	if sequence == 0 {
		// The sequence never wraps back to an older value. Exhaustion is terminal,
		// while native destruction acceptance remains infallible and replayable.
		app.render_runtime.renderer_terminal = err_render_renderer_failed
		return u64(0xffffffffffffffff)
	}
	if sequence == u64(0xffffffffffffffff) {
		app.render_runtime.next_teardown_sequence = 0
	} else {
		app.render_runtime.next_teardown_sequence++
	}
	return sequence
}

fn (app &App) window_destroy_finished(id WindowId) bool {
	app.state_mutex.lock()
	finished := id.app_instance == app.instance_id && app.window_finished[id.str()]
	app.state_mutex.unlock()
	return finished
}

fn (app &App) return_window_terminal(id WindowId) ! {
	app.state_mutex.lock()
	if id.app_instance != app.instance_id {
		app.state_mutex.unlock()
		return error(err_app_identity_mismatch)
	}
	finished := app.window_finished[id.str()]
	terminal := app.window_terminal[id.str()]
	app.state_mutex.unlock()
	if !finished {
		return error(err_window_destroy_ticket_stale)
	}
	if terminal != '' {
		return error(terminal)
	}
}

pub fn (mut app App) prepare_stop() !AppStopTicket {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	if app.status == .stopped {
		serial := app.stop_serial
		app.state_mutex.unlock()
		return AppStopTicket{
			app_instance: app.instance_id
			serial:       serial
		}
	}
	if app.event_dispatch_active {
		app.state_mutex.unlock()
		return error(err_event_dispatch_active)
	}
	if app.stop_prepared {
		serial := app.stop_serial
		app.state_mutex.unlock()
		return AppStopTicket{
			app_instance: app.instance_id
			serial:       serial
		}
	}
	serial, next_stop_serial := plan_nonwrapping_counter(app.next_stop_serial) or {
		app.state_mutex.unlock()
		return err
	}
	app.next_stop_serial = next_stop_serial
	app.stop_serial = serial
	app.stop_prepared = true
	app.stopping = true
	app.close_admission_locked()
	callback_active := app.owner_callback_depth > 0
	app.state_mutex.unlock()

	app.owner.stop()
	mut stop_errors := if callback_active {
		[]string{}
	} else {
		app.drain_cancelled_wrappers()
	}
	app.harvest_backend_events_for_stop() or { stop_errors << err.msg() }
	app.state_mutex.lock()
	app.pending_stop_errors << stop_errors
	app.state_mutex.unlock()
	return AppStopTicket{
		app_instance: app.instance_id
		serial:       serial
	}
}

fn (mut app App) append_backend_event_terminal_once(mut errors []string, observed string) {
	app.state_mutex.lock()
	if observed != '' {
		app.backend_event_terminal = merge_backend_errors(app.backend_event_terminal, observed)
	}
	stored := app.backend_event_terminal
	app.state_mutex.unlock()
	terminal := if observed != '' { observed } else { stored }
	if terminal != '' && terminal !in errors {
		errors << terminal
	}
}

pub fn (mut app App) finish_stop(ticket AppStopTicket, prior_errors []string) ! {
	app.assert_owner_thread()!
	app.state_mutex.lock()
	if ticket.app_instance != app.instance_id {
		app.state_mutex.unlock()
		return error(err_app_identity_mismatch)
	}
	if ticket.serial != app.stop_serial || !app.stop_prepared {
		app.state_mutex.unlock()
		return error(err_app_stop_ticket_stale)
	}
	if app.status == .stopped {
		app.state_mutex.unlock()
		return app.return_stop_terminal()
	}
	if app.event_dispatch_active {
		app.state_mutex.unlock()
		return error(err_event_dispatch_active)
	}
	native_retry_passes := app.stop_native_retry_passes
	prior_stop_terminal := app.stop_terminal
	if native_retry_passes >= 2 {
		app.state_mutex.unlock()
		return app.return_stop_terminal()
	}
	retrying_native_cleanup := native_retry_passes > 0
	mut errors := app.pending_stop_errors.clone()
	errors << prior_errors
	app.state_mutex.unlock()

	if !retrying_native_cleanup {
		app.harvest_backend_events_for_stop() or { errors << err.msg() }

		// This is the final core guard. Higher layers get the same fallback earlier so
		// their cleanup callbacks still run while renderer/native state is available.
		for destroy_ticket in app.seal_remaining_windows_terminal_for_stop() {
			app.finish_window_destroy(destroy_ticket, []string{}) or { errors << err.msg() }
		}
		pending_backend_error := app.backend.retained_delivery_error_for_stop()
		if pending_backend_error != '' {
			errors << pending_backend_error
		}
	}
	app.backend.stop() or { errors << err.msg() }
	backend_event_terminal := app.backend.event_sequence_terminal_error()
	app.append_backend_event_terminal_once(mut errors, backend_event_terminal)
	if !retrying_native_cleanup {
		if message := app.take_internal_fault(.teardown_backend_stop) {
			errors << message
		}
	}
	if app.backend.retains_native_ownership_for_stop() {
		if errors.len == 0 {
			errors << err_render_native_renderer_unavailable
		}
		terminal := if retrying_native_cleanup && prior_stop_terminal != '' {
			prior_stop_terminal
		} else {
			aggregate_terminal_errors(err_render_terminal_aggregate, errors)
		}
		app.state_mutex.lock()
		app.pending_stop_errors = errors.clone()
		app.stop_native_retry_passes = if retrying_native_cleanup { u8(2) } else { u8(1) }
		app.stop_terminal = terminal
		app.state_mutex.unlock()
		return app.return_stop_terminal()
	}

	app.state_mutex.lock()
	app.seal_event_deliveries_for_stop_locked() or {
		app.state_mutex.unlock()
		return err
	}
	app.status = .stopped
	app.stopping = false
	app.stop_native_retry_passes = 0
	app.stop_terminal = if retrying_native_cleanup && prior_stop_terminal != '' {
		prior_stop_terminal
	} else {
		aggregate_terminal_errors(err_render_terminal_aggregate, errors)
	}
	app.state_mutex.unlock()
	return app.return_stop_terminal()
}

fn (app &App) return_stop_terminal() ! {
	app.state_mutex.lock()
	terminal := app.stop_terminal
	app.state_mutex.unlock()
	if terminal != '' {
		return error(terminal)
	}
}

fn (app &App) live_window_ids_for_stop() []WindowId {
	app.state_mutex.lock()
	mut ids := []WindowId{}
	for slot in app.windows {
		if slot.status == .alive && slot.destroy_stage in [.none, .prepared] {
			ids << slot.id
		}
	}
	app.state_mutex.unlock()
	return ids
}

fn (mut app App) seal_remaining_windows_terminal_for_stop() []WindowDestroyTicket {
	app.state_mutex.lock()
	mut tickets := []WindowDestroyTicket{}
	for i, slot in app.windows {
		if slot.status == .alive && slot.destroy_stage != .finished {
			tickets << app.seal_window_destroy_terminal_index_locked(i)
		}
	}
	app.state_mutex.unlock()
	return tickets
}

fn (app &App) prepared_window_tickets_for_stop() []WindowDestroyTicket {
	return app.window_tickets_for_stop(.prepared)
}

fn (app &App) sealed_window_tickets_for_stop() []WindowDestroyTicket {
	return app.window_tickets_for_stop(.sealed)
}

fn (app &App) window_tickets_for_stop(stage WindowDestroyStage) []WindowDestroyTicket {
	app.state_mutex.lock()
	mut tickets := []WindowDestroyTicket{}
	for slot in app.windows {
		if slot.status == .alive && slot.destroy_stage == stage && slot.destroy_serial != 0 {
			tickets << WindowDestroyTicket{
				app_instance: app.instance_id
				window:       slot.id
				serial:       slot.destroy_serial
			}
		}
	}
	app.state_mutex.unlock()
	return tickets
}

fn aggregate_terminal_errors(prefix string, errors []string) string {
	mut unique := []string{}
	for message in errors {
		if message != '' && message !in unique {
			unique << message
		}
	}
	if unique.len == 0 {
		return ''
	}
	return '${prefix}: ${unique.join('; ')}'
}
