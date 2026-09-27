module multiwindow

fn test_direct_stop_retains_queued_terminal_events_for_one_post_stop_drain() {
	mut app := new_app()!
	first := app.create_window(title: 'queued terminal first')!
	second := app.create_window(title: 'queued terminal second')!
	created := app.drain_events()!
	assert created.len == 2
	assert created[0].kind == .window_created
	assert created[0].window_id == first
	assert created[1].kind == .window_created
	assert created[1].window_id == second

	app.destroy_window(first)!
	app.stop()!
	assert app.status() == .stopped

	terminal := app.drain_events()!
	assert terminal.len == 2
	assert terminal[0].kind == .window_destroyed
	assert terminal[0].window_id == first
	assert terminal[1].kind == .window_destroyed
	assert terminal[1].window_id == second
	assert app.drain_events()!.len == 0

	app.stop()!
	assert app.drain_events()!.len == 0
}

fn test_native_destroyed_event_survives_cleanup_error_and_terminal_stop() {
	mut app := new_app()!
	window := app.create_window(title: 'native cleanup failure')!
	assert app.drain_events()!.len == 1

	app.backend.teardown_notices << BackendTeardownNotice{
		window: window
	}
	assert app.poll_events()! == 1
	assert app.drain_events()!.len == 0
	notices := app.drain_render_teardown_notices()!
	assert notices.len == 1
	assert notices[0].window == window
	expected := '${err_render_terminal_aggregate}: injected native cleanup failure'
	mut finish_error := ''
	app.finish_window_destroy(notices[0].ticket, ['injected native cleanup failure']) or {
		finish_error = err.msg()
	}
	assert finish_error == expected
	assert !app.window_exists(window)
	assert app.events.len == 1
	assert app.events[0].kind == .lifecycle
	assert app.events[0].lifecycle.kind == .window_destroyed
	assert app.events[0].lifecycle.window_id == window
	assert !app.queued_event_blocked_by_teardown_locked(app.events[0])
	$if gg_multiwindow ? {
		app.defer_dispatch_error_for_gg(expected)!
	}

	app.stop()!
	assert app.status() == .stopped
	$if gg_multiwindow ? {
		mut premature_error := ''
		may_progress := app.post_dispatch_error_gate_for_gg() or {
			premature_error = err.msg()
			false
		}
		assert premature_error == ''
		assert !may_progress
	}
	terminal := app.drain_events()!
	assert terminal.len == 1
	assert terminal[0].kind == .window_destroyed
	assert terminal[0].window_id == window
	assert app.drain_events()!.len == 0
	$if gg_multiwindow ? {
		mut deferred_error := ''
		_ := app.post_dispatch_error_gate_for_gg() or {
			deferred_error = err.msg()
			false
		}
		assert deferred_error == expected
		assert app.post_dispatch_error_gate_for_gg()!
	}

	app.finish_window_destroy(notices[0].ticket, []) or {
		assert err.msg() == expected
		assert app.drain_events()!.len == 0
		return
	}
	assert false, 'replayed native cleanup failure discarded its terminal outcome'
}
