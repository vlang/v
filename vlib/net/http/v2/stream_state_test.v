module v2

// Tests for stream state machine enforcement (RFC 7540 §5.1).

// --- can_send tests ---

fn test_idle_can_send_headers() {
	assert StreamState.idle.can_send(.headers) == true
}

fn test_idle_cannot_send_data() {
	assert StreamState.idle.can_send(.data) == false
}

fn test_open_can_send_data() {
	assert StreamState.open.can_send(.data) == true
}

fn test_open_can_send_rst_stream() {
	assert StreamState.open.can_send(.rst_stream) == true
}

fn test_half_closed_local_cannot_send_data() {
	assert StreamState.half_closed_local.can_send(.data) == false
}

fn test_half_closed_local_can_send_rst_stream() {
	assert StreamState.half_closed_local.can_send(.rst_stream) == true
}

fn test_half_closed_local_can_send_window_update() {
	assert StreamState.half_closed_local.can_send(.window_update) == true
}

fn test_half_closed_remote_can_send_data() {
	assert StreamState.half_closed_remote.can_send(.data) == true
}

fn test_closed_cannot_send_data() {
	assert StreamState.closed.can_send(.data) == false
}

// --- can_recv tests ---

fn test_idle_can_recv_headers() {
	assert StreamState.idle.can_recv(.headers) == true
}

fn test_idle_cannot_recv_data() {
	assert StreamState.idle.can_recv(.data) == false
}

fn test_open_can_recv_data() {
	assert StreamState.open.can_recv(.data) == true
}

fn test_half_closed_local_can_recv_data() {
	assert StreamState.half_closed_local.can_recv(.data) == true
}

fn test_half_closed_remote_cannot_recv_data() {
	assert StreamState.half_closed_remote.can_recv(.data) == false
}

fn test_half_closed_remote_can_recv_rst_stream() {
	assert StreamState.half_closed_remote.can_recv(.rst_stream) == true
}

fn test_closed_cannot_recv_data() {
	assert StreamState.closed.can_recv(.data) == false
}

// --- next_on_send transition tests ---

fn test_idle_send_headers_transitions_to_open() {
	assert StreamState.idle.next_on_send(.headers, false) == .open
}

fn test_idle_send_headers_end_stream_transitions_to_half_closed_local() {
	assert StreamState.idle.next_on_send(.headers, true) == .half_closed_local
}

fn test_open_send_data_end_stream_transitions_to_half_closed_local() {
	assert StreamState.open.next_on_send(.data, true) == .half_closed_local
}

fn test_open_send_data_no_end_stream_stays_open() {
	assert StreamState.open.next_on_send(.data, false) == .open
}

fn test_half_closed_remote_send_end_stream_transitions_to_closed() {
	assert StreamState.half_closed_remote.next_on_send(.data, true) == .closed
}

fn test_open_send_rst_stream_transitions_to_closed() {
	assert StreamState.open.next_on_send(.rst_stream, false) == .closed
}

fn test_half_closed_local_send_rst_stream_transitions_to_closed() {
	assert StreamState.half_closed_local.next_on_send(.rst_stream, false) == .closed
}

// --- next_on_recv transition tests ---

fn test_idle_recv_headers_transitions_to_open() {
	assert StreamState.idle.next_on_recv(.headers, false) == .open
}

fn test_idle_recv_headers_end_stream_transitions_to_half_closed_remote() {
	assert StreamState.idle.next_on_recv(.headers, true) == .half_closed_remote
}

fn test_open_recv_data_end_stream_transitions_to_half_closed_remote() {
	assert StreamState.open.next_on_recv(.data, true) == .half_closed_remote
}

fn test_open_recv_data_no_end_stream_stays_open() {
	assert StreamState.open.next_on_recv(.data, false) == .open
}

fn test_half_closed_local_recv_end_stream_transitions_to_closed() {
	assert StreamState.half_closed_local.next_on_recv(.data, true) == .closed
}

fn test_open_recv_rst_stream_transitions_to_closed() {
	assert StreamState.open.next_on_recv(.rst_stream, false) == .closed
}

fn test_half_closed_remote_recv_rst_stream_transitions_to_closed() {
	assert StreamState.half_closed_remote.next_on_recv(.rst_stream, false) == .closed
}

// --- Full lifecycle tests ---

fn test_full_lifecycle_with_body() {
	// idle → open → half_closed_local → closed
	mut state := StreamState.idle
	state = state.next_on_send(.headers, false)
	assert state == .open

	state = state.next_on_send(.data, true)
	assert state == .half_closed_local

	state = state.next_on_recv(.data, true)
	assert state == .closed
}

fn test_full_lifecycle_no_body() {
	// idle → half_closed_local → closed
	mut state := StreamState.idle
	state = state.next_on_send(.headers, true)
	assert state == .half_closed_local

	state = state.next_on_recv(.headers, true)
	assert state == .closed
}

fn test_full_lifecycle_rst_stream() {
	// idle → open → closed via RST_STREAM
	mut state := StreamState.idle
	state = state.next_on_send(.headers, false)
	assert state == .open

	state = state.next_on_recv(.rst_stream, false)
	assert state == .closed
}
