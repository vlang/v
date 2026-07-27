module multiwindow

$if windows {
	#insert "@VMODROOT/vlib/x/multiwindow/testdata/event_sequence_exhaustion_helpers.h"

	fn C.v_multiwindow_test_event_sequence_current() u64
	fn C.v_multiwindow_test_event_sequence_exhausted() int
	fn C.v_multiwindow_test_set_event_sequence(sequence u64, exhausted int)
	fn C.v_multiwindow_test_next_event_sequence() u64
}

$if darwin {
	#insert "@VMODROOT/vlib/x/multiwindow/testdata/event_sequence_exhaustion_helpers.h"

	fn C.v_multiwindow_test_event_sequence_current() u64
	fn C.v_multiwindow_test_event_sequence_exhausted() int
	fn C.v_multiwindow_test_set_event_sequence(sequence u64, exhausted int)
	fn C.v_multiwindow_test_next_event_sequence() u64
	fn C.v_multiwindow_test_appkit_clear_events(state voidptr)
	fn C.v_multiwindow_test_appkit_queue_lifecycle(state voidptr, kind int)
}

$if linux && sokol_wayland ? {
	#insert "@VMODROOT/vlib/x/multiwindow/testdata/event_sequence_exhaustion_helpers.h"

	fn C.v_multiwindow_test_event_sequence_current() u64
	fn C.v_multiwindow_test_event_sequence_exhausted() int
	fn C.v_multiwindow_test_set_event_sequence(sequence u64, exhausted int)
	fn C.v_multiwindow_test_next_event_sequence() u64
}

fn assert_sequence_terminal_replays_for_poll_and_operation(mut backend Backend) {
	for _ in 0 .. 2 {
		_ := backend.poll_queued_events() or {
			assert false, 'terminal backend poll failed before its retained batch was acknowledged: ${err.msg()}'
			[]QueuedEvent{}
		}
		assert merge_backend_errors(backend.acknowledge_queued_events(),
			backend.event_sequence_terminal_error()) == err_backend_event_sequence_exhausted
	}
	id := WindowId{
		app_instance: 1
		slot:         0
		generation:   1
	}
	for _ in 0 .. 2 {
		backend.set_window_title(id, 'must be rejected before native dispatch') or {
			assert err.msg() == err_backend_event_sequence_exhausted
			continue
		}
		assert false, 'terminal native sequence state admitted a backend operation'
	}
}

fn test_native_sequence_terminal_state_replays_for_every_backend_kind() {
	mut win32 := Backend{
		kind:  .win32
		win32: Win32Backend{
			event_sequence_terminal: err_backend_event_sequence_exhausted
		}
	}
	assert_sequence_terminal_replays_for_poll_and_operation(mut win32)

	mut wayland := Backend{
		kind:    .wayland
		wayland: WaylandBackend{
			event_sequence_terminal: err_backend_event_sequence_exhausted
		}
	}
	assert_sequence_terminal_replays_for_poll_and_operation(mut wayland)

	mut appkit := Backend{
		kind:   .appkit
		appkit: AppKitBackend{
			event_sequence_terminal: err_backend_event_sequence_exhausted
		}
	}
	assert_sequence_terminal_replays_for_poll_and_operation(mut appkit)
}

fn test_zero_sequence_never_enters_win32_or_wayland_record_queues() {
	id := WindowId{
		app_instance: 1
		slot:         0
		generation:   1
	}
	event := queued_lifecycle_event(Event{
		kind:      .window_close_requested
		window_id: id
	})
	mut win32 := Win32WindowRecord{
		id: id
	}
	win32.enqueue_native_event(0, event)
	assert win32.queued_events.len == 0
	win32.enqueue_native_event(u64(0xffffffffffffffff), event)
	assert win32.queued_events.len == 1
	assert win32.queued_events[0].sequence == u64(0xffffffffffffffff)

	mut wayland := WaylandWindowRecord{
		id: id
	}
	wayland.enqueue_native_event(0, event)
	assert wayland.pending_events.len == 0
	wayland.enqueue_native_event(u64(0xffffffffffffffff), event)
	assert wayland.pending_events.len == 1
	assert wayland.pending_events[0].sequence == u64(0xffffffffffffffff)
}

fn test_win32_native_sequence_exhaustion_is_sticky_without_wrapped_admission() {
	$if windows {
		saved_sequence := C.v_multiwindow_test_event_sequence_current()
		saved_exhausted := C.v_multiwindow_test_event_sequence_exhausted()
		defer {
			C.v_multiwindow_test_set_event_sequence(saved_sequence, saved_exhausted)
		}
		C.v_multiwindow_test_set_event_sequence(u64(0xffffffffffffffff), 0)
		last := C.v_multiwindow_test_next_event_sequence()
		wrapped := C.v_multiwindow_test_next_event_sequence()
		assert last == u64(0xffffffffffffffff)
		assert wrapped == 0
		assert C.v_multiwindow_test_event_sequence_exhausted() == 1

		id := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut record := Win32WindowRecord{
			id: id
		}
		event := queued_lifecycle_event(Event{
			kind:      .window_close_requested
			window_id: id
		})
		record.enqueue_native_event(last, event)
		record.enqueue_native_event(wrapped, event)
		assert record.queued_events.len == 1
		assert record.queued_events[0].sequence == last

		mut native := Win32Backend{}
		assert native.take_poll_error() == err_backend_event_sequence_exhausted
		C.v_multiwindow_test_set_event_sequence(saved_sequence, saved_exhausted)
		assert native.take_poll_error() == err_backend_event_sequence_exhausted
		mut backend := Backend{
			kind:  .win32
			win32: native
		}
		assert_sequence_terminal_replays_for_poll_and_operation(mut backend)
	}
}

fn test_wayland_native_sequence_exhaustion_is_sticky_without_wrapped_admission() {
	$if linux && sokol_wayland ? {
		saved_sequence := C.v_multiwindow_test_event_sequence_current()
		saved_exhausted := C.v_multiwindow_test_event_sequence_exhausted()
		defer {
			C.v_multiwindow_test_set_event_sequence(saved_sequence, saved_exhausted)
		}
		C.v_multiwindow_test_set_event_sequence(u64(0xffffffffffffffff), 0)
		last := C.v_multiwindow_test_next_event_sequence()
		wrapped := C.v_multiwindow_test_next_event_sequence()
		assert last == u64(0xffffffffffffffff)
		assert wrapped == 0
		assert C.v_multiwindow_test_event_sequence_exhausted() == 1

		id := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut record := WaylandWindowRecord{
			id: id
		}
		event := queued_lifecycle_event(Event{
			kind:      .window_close_requested
			window_id: id
		})
		record.enqueue_native_event(last, event)
		record.enqueue_native_event(wrapped, event)
		assert record.pending_events.len == 1
		assert record.pending_events[0].sequence == last

		mut native := WaylandBackend{}
		assert native.take_poll_error() == err_backend_event_sequence_exhausted
		C.v_multiwindow_test_set_event_sequence(saved_sequence, saved_exhausted)
		assert native.take_poll_error() == err_backend_event_sequence_exhausted
		mut backend := Backend{
			kind:    .wayland
			wayland: native
		}
		assert_sequence_terminal_replays_for_poll_and_operation(mut backend)
	}
}

fn test_appkit_native_sequence_exhaustion_is_sticky_and_drops_wrapped_event() {
	$if darwin {
		mut backend := new_backend(.appkit, false)!
		app_identity := u64(1)
		backend.bind_app_native_operations(app_identity, app_identity + 1, app_identity + 2)!
		backend.start(false)!
		id := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		backend.create_window(id, WindowConfig{
			title:   'sequence exhaustion'
			visible: false
		})!
		state := backend.appkit.windows[0].state
		C.v_multiwindow_test_appkit_clear_events(state)
		saved_sequence := C.v_multiwindow_test_event_sequence_current()
		saved_exhausted := C.v_multiwindow_test_event_sequence_exhausted()
		defer {
			C.v_multiwindow_test_set_event_sequence(saved_sequence, saved_exhausted)
		}

		C.v_multiwindow_test_set_event_sequence(u64(0xffffffffffffffff), 0)
		C.v_multiwindow_test_appkit_queue_lifecycle(state, 1)
		C.v_multiwindow_test_appkit_queue_lifecycle(state, 1)
		assert C.v_multiwindow_test_event_sequence_exhausted() == 1
		mut native_event := C.VMultiwindowAppKitQueuedEvent{}
		assert C.v_multiwindow_appkit_take_queued_event(state, &native_event) == 1
		queued_sequence := native_event.sequence
		C.v_multiwindow_appkit_release_queued_event_resources(&native_event)
		assert queued_sequence == u64(0xffffffffffffffff)
		assert C.v_multiwindow_appkit_take_queued_event(state, &native_event) == 0

		assert backend.event_sequence_terminal_error() == err_backend_event_sequence_exhausted
		C.v_multiwindow_test_set_event_sequence(saved_sequence, saved_exhausted)
		assert backend.event_sequence_terminal_error() == err_backend_event_sequence_exhausted
		assert_sequence_terminal_replays_for_poll_and_operation(mut backend)

		for _ in 0 .. 2 {
			backend.stop()!
		}
		assert backend.event_sequence_terminal_error() == err_backend_event_sequence_exhausted
		assert backend.event_sequence_terminal_error() == err_backend_event_sequence_exhausted
		assert backend.appkit.windows.len == 0
	}
}
