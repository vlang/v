module multiwindow

struct TeardownCallbackSeen {
mut:
	ran bool
}

fn test_app_instance_identity_is_monotonic_and_rejects_foreign_window_ids() {
	mut first := new_app(queue_size: 2)!
	first_window := first.create_window(title: 'first app')!
	mut second := new_app(queue_size: 2)!
	second_window := second.create_window(title: 'second app')!

	assert first.instance_id() > 0
	assert second.instance_id() > first.instance_id()
	assert first_window.app_instance == first.instance_id()
	assert second_window.app_instance == second.instance_id()
	assert first_window.slot == second_window.slot
	assert first_window.generation == second_window.generation
	second.window_info(first_window) or {
		assert err.msg() == err_app_identity_mismatch
		assert second.window_exists(second_window)
		first.stop()!
		second.stop()!
		return
	}
	assert false, 'a WindowId from another App resolved by slot and generation'
}

fn test_stopped_app_identity_is_never_reused() {
	mut first := new_app()!
	stale := first.create_window(title: 'stale app identity')!
	first.stop()!
	mut replacement := new_app()!
	live := replacement.create_window(title: 'new app identity')!

	assert replacement.instance_id() > first.instance_id()
	assert stale.slot == live.slot
	assert stale.generation == live.generation
	assert !replacement.window_exists(stale)
	replacement.window_status(stale) or {
		assert err.msg() == err_app_identity_mismatch
		replacement.stop()!
		return
	}
	assert false, 'a stopped App identity was reused'
}

fn test_destroy_ticket_is_app_owned_and_prepare_is_reversible() {
	mut owner := new_app()!
	window := owner.create_window(title: 'ticket owner')!
	ticket := owner.prepare_window_destroy(window)!
	assert owner.window_exists(window)

	mut foreign := new_app()!
	mut rollback_error := ''
	foreign.rollback_window_destroy(ticket) or { rollback_error = err.msg() }
	assert rollback_error == err_app_identity_mismatch
	mut seal_error := ''
	foreign.seal_window_destroy(ticket) or { seal_error = err.msg() }
	assert seal_error == err_app_identity_mismatch
	mut finish_error := ''
	foreign.finish_window_destroy(ticket, []) or { finish_error = err.msg() }
	assert finish_error == err_app_identity_mismatch
	owner.rollback_window_destroy(ticket)!
	assert owner.window_exists(window)
	owner.stop()!
	foreign.stop()!
}

fn test_destroy_ticket_rejects_every_wrong_phase_and_copied_prepared_ticket() {
	mut app := new_app()!
	window := app.create_window(title: 'ticket phases')!
	first := app.prepare_window_destroy(window)!
	copied := first
	app.rollback_window_destroy(first)!
	app.seal_window_destroy(copied) or {
		assert err.msg() == err_window_destroy_ticket_stale
		second := app.prepare_window_destroy(window)!
		app.rollback_window_destroy(copied) or {
			assert err.msg() == err_window_destroy_ticket_stale
		}
		app.seal_window_destroy(second)!
		app.seal_window_destroy(second) or { assert err.msg() == err_window_destroy_ticket_stale }
		app.rollback_window_destroy(second) or {
			assert err.msg() == err_window_destroy_ticket_stale
		}
		app.finish_window_destroy(second, [])!
		app.finish_window_destroy(second, [])!
		app.stop()!
		return
	}
	assert false, 'copied rolled-back destroy ticket advanced to seal'
}

fn test_destroy_ticket_operations_reject_foreign_threads_without_advancing_phase() {
	mut app := new_app()!
	window := app.create_window(title: 'ticket owner thread')!
	prepare_result := chan string{cap: 1}
	prepare_thread := spawn fn [mut app, window, prepare_result] () {
		app.prepare_window_destroy(window) or {
			prepare_result <- err.msg()
			return
		}
		prepare_result <- 'accepted'
	}()
	assert <-prepare_result == err_owner_thread_required
	prepare_thread.wait()
	assert app.window_exists(window)

	ticket := app.prepare_window_destroy(window)!
	rollback_result := chan string{cap: 1}
	rollback_thread := spawn fn [mut app, ticket, rollback_result] () {
		app.rollback_window_destroy(ticket) or {
			rollback_result <- err.msg()
			return
		}
		rollback_result <- 'accepted'
	}()
	assert <-rollback_result == err_owner_thread_required
	rollback_thread.wait()
	app.seal_window_destroy(ticket)!

	finish_result := chan string{cap: 1}
	finish_thread := spawn fn [mut app, ticket, finish_result] () {
		app.finish_window_destroy(ticket, []) or {
			finish_result <- err.msg()
			return
		}
		finish_result <- 'accepted'
	}()
	assert <-finish_result == err_owner_thread_required
	finish_thread.wait()
	app.finish_window_destroy(ticket, [])!
	app.stop()!
}

fn test_prepare_destroy_failure_leaves_window_live_and_reusable() {
	mut app := new_app()!
	window := app.create_window(title: 'prepare failure')!
	app.render_runtime.next_destroy_serial = 0
	app.prepare_window_destroy(window) or {
		assert err.msg() == err_window_generation_exhausted
		assert app.window_exists(window)
		assert app.windows[window.slot].destroy_stage == .none
		assert app.render_runtime.windows[window.slot].status == .alive
		app.render_runtime.next_destroy_serial = 100
		app.destroy_window(window)!
		app.stop()!
		return
	}
	assert false, 'destroy prepare succeeded after its ticket counter was exhausted'
}

fn test_sealed_destroy_is_irreversible_and_terminally_idempotent() {
	mut app := new_app()!
	window := app.create_window(title: 'sealed destroy')!
	ticket := app.prepare_window_destroy(window)!
	app.seal_window_destroy(ticket)!
	app.rollback_window_destroy(ticket) or {
		assert err.msg() == err_window_destroy_ticket_stale
		app.finish_window_destroy(ticket, [])!
		assert !app.window_exists(window)
		assert app.window_status(window)! == .destroyed
		app.finish_window_destroy(ticket, [])!
		events := app.drain_events()!
		mut destroyed := 0
		for event in events {
			if event.kind == .window_destroyed && event.window_id == window {
				destroyed++
			}
		}
		assert destroyed == 1
		app.stop()!
		return
	}
	assert false, 'rollback succeeded after destroy was sealed'
}

fn test_backend_teardown_notice_precedes_stale_generation_and_public_destroyed_event() {
	mut app := new_app()!
	window := app.create_window(title: 'backend teardown')!
	_ = app.drain_events()!
	original := app.render_window_snapshot(window)!

	app.backend.teardown_notices << BackendTeardownNotice{
		window: window
	}
	assert app.poll_events()! == 1
	assert app.drain_events()!.len == 0
	notices := app.drain_render_teardown_notices()!
	assert notices.len == 1
	notice := notices[0]
	assert notice.window == window
	assert notice.snapshot.window == window
	assert notice.snapshot.submitted_frame == original.submitted_frame
	assert notice.snapshot.metrics.metrics_sequence == original.metrics.metrics_sequence
	assert !app.window_exists(window)

	app.finish_window_destroy(notice.ticket, [])!
	events := app.drain_events()!
	assert events.len == 1
	assert events[0].kind == .window_destroyed
	assert events[0].window_id == window
	app.finish_window_destroy(notice.ticket, [])!
	assert app.drain_events()!.len == 0
	app.stop()!
}

fn test_destroy_aggregates_unique_failures_and_replays_terminal_outcome() {
	mut app := new_app()!
	window := app.create_window(title: 'aggregate destroy')!
	_ = app.drain_events()!
	ticket := app.prepare_window_destroy(window)!
	app.seal_window_destroy(ticket)!
	expected := '${err_render_terminal_aggregate}: cleanup-a; cleanup-b'
	app.finish_window_destroy(ticket, ['cleanup-a', 'cleanup-b', 'cleanup-a']) or {
		assert err.msg() == expected
		app.destroy_window(window) or {
			assert err.msg() == expected
			events := app.drain_events()!
			assert events.len == 1
			assert events[0].kind == .window_destroyed
			assert app.drain_events()!.len == 0
			app.stop()!
			return
		}
		assert false, 'repeated destroy discarded its recorded terminal error'
	}
	assert false, 'destroy failure aggregation unexpectedly succeeded'
}

fn test_backend_destroy_failure_is_aggregated_once_and_replayed_without_cleanup_retry() {
	mut app := new_app()!
	window := app.create_window(title: 'backend teardown failure')!
	_ = app.drain_events()!
	ticket := app.prepare_window_destroy(window)!
	app.seal_window_destroy(ticket)!
	app.backend.mock.windows.clear()
	expected := '${err_render_terminal_aggregate}: package cleanup failed; ${err_window_not_found}'
	app.finish_window_destroy(ticket, ['package cleanup failed']) or {
		assert err.msg() == expected
		assert !app.window_exists(window)
		assert app.window_status(window)! == .destroyed
		app.finish_window_destroy(ticket, ['must not be appended']) or {
			assert err.msg() == expected
			events := app.drain_events()!
			assert events.len == 1
			assert events[0].kind == .window_destroyed
			assert events[0].window_id == window
			assert app.drain_events()!.len == 0
			app.stop()!
			return
		}
		assert false, 'repeated finish discarded its recorded backend teardown failure'
	}
	assert false, 'injected backend teardown failure unexpectedly succeeded'
}

fn test_owner_callback_can_destroy_one_window_while_another_remains_live() {
	mut app := new_app(queue_size: 2)!
	closing := app.create_window(title: 'callback closing')!
	survivor := app.create_window(title: 'callback survivor')!
	callback_result := chan string{cap: 1}
	app.try_post(fn [closing, survivor, callback_result] (mut queued_app App) ! {
		queued_app.destroy_window(closing)!
		if !queued_app.window_exists(survivor) {
			return error('survivor was destroyed by sibling callback teardown')
		}
		callback_result <- 'destroyed-one'
	})!
	assert app.drain_pending(1)! == 1
	assert <-callback_result == 'destroyed-one'
	assert !app.window_exists(closing)
	assert app.window_exists(survivor)
	app.stop()!
}

fn test_owner_callback_can_stop_without_running_later_accepted_user_code() {
	mut app := new_app(queue_size: 2)!
	mut later_ran := &TeardownCallbackSeen{}
	callback_result := chan string{cap: 1}
	app.try_post(fn [callback_result] (mut queued_app App) ! {
		queued_app.stop()!
		callback_result <- 'stopped'
	})!
	app.try_post(fn [mut later_ran] (mut queued_app App) ! {
		_ = queued_app.status()
		later_ran.ran = true
	})!
	app.drain_pending(2) or {
		assert err.msg() == err_app_stopped
		assert <-callback_result == 'stopped'
		assert app.status() == .stopped
		assert !later_ran.ran
		return
	}
	assert false, 'callback-reentrant stop allowed drain_pending to report success'
}

fn test_stop_aggregates_unique_failures_and_is_terminally_idempotent() {
	mut app := new_app()!
	ticket := app.prepare_stop()!
	expected := '${err_render_terminal_aggregate}: stop-a; stop-b'
	app.finish_stop(ticket, ['stop-a', 'stop-b', 'stop-a']) or {
		assert err.msg() == expected
		assert app.status() == .stopped
		app.stop() or {
			assert err.msg() == expected
			assert app.status() == .stopped
			return
		}
		assert false, 'repeated stop discarded its recorded terminal error'
	}
	assert false, 'stop failure aggregation unexpectedly succeeded'
}

fn test_stop_ticket_rejects_foreign_app_and_foreign_thread_without_finishing_owner() {
	mut owner := new_app()!
	ticket := owner.prepare_stop()!
	mut foreign := new_app()!
	mut foreign_app_error := ''
	foreign.finish_stop(ticket, []) or { foreign_app_error = err.msg() }
	assert foreign_app_error == err_app_identity_mismatch

	thread_result := chan string{cap: 1}
	stop_worker := spawn fn [mut owner, ticket, thread_result] () {
		owner.finish_stop(ticket, []) or {
			thread_result <- err.msg()
			return
		}
		thread_result <- 'accepted'
	}()
	assert <-thread_result == err_owner_thread_required
	stop_worker.wait()
	assert owner.status() == .running
	owner.finish_stop(ticket, [])!
	assert owner.status() == .stopped
	foreign.stop()!
}

fn test_exhausted_window_generation_permanently_retires_slot() {
	mut app := new_app()!
	first := app.create_window(title: 'generation exhaustion')!
	app.destroy_window(first)!
	app.windows[first.slot].id = WindowId{
		app_instance: app.instance_id()
		slot:         first.slot
		generation:   u32(0xffffffff)
	}
	app.windows[first.slot].generation_exhausted = false
	replacement := app.create_window(title: 'new slot')!
	assert replacement.slot != first.slot
	assert app.windows[first.slot].generation_exhausted
	app.stop()!
}
