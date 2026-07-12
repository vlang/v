module multiwindow

struct MockBackend {
mut:
	pending_events []QueuedEvent
	windows        []MockWindowRecord
}

struct MockWindowRecord {
	id WindowId
mut:
	config WindowConfig
}

fn new_mock_backend() MockBackend {
	return MockBackend{}
}

fn (backend MockBackend) capabilities() Capabilities {
	return Capabilities{
		backend:            .mock
		mock:               true
		native:             false
		multi_window:       true
		owner_queue:        true
		explicit_swapchain: false
		input_events:       true
		mouse_events:       true
		keyboard_events:    true
		text_events:        true
		focus_events:       true
		drop_events:        true
		touch_events:       true
	}
}

fn (mut backend MockBackend) start() ! {}

fn (mut backend MockBackend) create_window(id WindowId, config WindowConfig) !WindowSize {
	actual_size := window_size_for_config(config, config.width, config.height)
	backend.windows << MockWindowRecord{
		id:     id
		config: window_config_with_size(config, actual_size.width, actual_size.height)
	}
	return actual_size
}

fn (mut backend MockBackend) destroy_window(id WindowId) ! {
	index := backend.window_record_index(id) or { return error(err_window_not_found) }
	backend.windows.delete(index)
}

fn (mut backend MockBackend) set_window_title(id WindowId, title string) ! {
	index := backend.window_record_index(id) or { return error(err_window_not_found) }
	backend.windows[index].config = window_config_with_title(backend.windows[index].config, title)
}

fn (mut backend MockBackend) resize_window(id WindowId, width int, height int) !WindowSize {
	index := backend.window_record_index(id) or { return error(err_window_not_found) }
	actual_size := window_size_for_config(backend.windows[index].config, width, height)
	backend.windows[index].config = window_config_with_size(backend.windows[index].config,
		actual_size.width, actual_size.height)
	return actual_size
}

fn (mut backend MockBackend) set_window_cursor(id WindowId, shape CursorShape) ! {
	_ = shape
	backend.window_record_index(id) or { return error(err_window_not_found) }
	return error(err_capability_unsupported)
}

fn (mut backend MockBackend) poll_events() ![]Event {
	mut lifecycle_events := []Event{cap: backend.pending_events.len}
	mut remaining_events := []QueuedEvent{cap: backend.pending_events.len}
	for event in backend.pending_events {
		match event.kind {
			.lifecycle {
				lifecycle_events << event.lifecycle
			}
			.input {
				remaining_events << event
			}
		}
	}
	backend.pending_events = remaining_events
	return lifecycle_events
}

fn (mut backend MockBackend) poll_queued_events() ![]QueuedEvent {
	events := backend.pending_events.clone()
	backend.pending_events.clear()
	return events
}

fn (mut backend MockBackend) stop() ! {
	backend.pending_events.clear()
	backend.windows.clear()
}

fn (mut backend MockBackend) enqueue_event(event Event) {
	backend.pending_events << queued_lifecycle_event(event)
}

fn (mut backend MockBackend) enqueue_input_event(event InputEvent) {
	backend.pending_events << queued_input_event(event)
}

fn (backend &MockBackend) window_record_index(id WindowId) ?int {
	for i, record in backend.windows {
		if record.id == id {
			return i
		}
	}
	return none
}
