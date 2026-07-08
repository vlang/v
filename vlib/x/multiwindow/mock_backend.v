module multiwindow

struct MockBackend {
mut:
	pending_events []Event
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

fn (mut backend MockBackend) poll_events() ![]Event {
	events := backend.pending_events.clone()
	backend.pending_events.clear()
	return events
}

fn (mut backend MockBackend) stop() ! {
	backend.pending_events.clear()
	backend.windows.clear()
}

fn (mut backend MockBackend) enqueue_event(event Event) {
	backend.pending_events << event
}

fn (backend &MockBackend) window_record_index(id WindowId) ?int {
	for i, record in backend.windows {
		if record.id == id {
			return i
		}
	}
	return none
}
