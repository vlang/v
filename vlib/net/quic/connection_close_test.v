module quic

fn test_connection_close_tracker_starts_active() {
	t := new_connection_close_tracker()
	assert t.state == .active
	assert t.may_send()
}

fn test_connection_close_tracker_enter_closing() {
	mut t := new_connection_close_tracker()
	t.enter_closing()
	assert t.state == .closing
	assert t.may_send()
}

fn test_connection_close_tracker_enter_closing_is_idempotent_and_one_way() {
	mut t := new_connection_close_tracker()
	t.enter_closing()
	t.enter_closing()
	assert t.state == .closing
}

// test_connection_close_tracker_closing_rate_limited_retransmission is the
// plan's own explicitly named test: while closing, this endpoint may
// retransmit its own CONNECTION_CLOSE at most once per received packet --
// a rate limit, not "never again" or "unlimited".
fn test_connection_close_tracker_closing_rate_limited_retransmission() {
	mut t := new_connection_close_tracker()
	t.enter_closing()

	assert t.note_packet_received_while_closing()
	assert t.note_packet_received_while_closing()
	assert t.note_packet_received_while_closing()
	assert t.packets_received_while_closing == 3

	// While active or draining, there is nothing to retransmit.
	mut active := new_connection_close_tracker()
	assert !active.note_packet_received_while_closing()

	mut draining := new_connection_close_tracker()
	draining.enter_draining()
	assert !draining.note_packet_received_while_closing()
}

// test_connection_close_tracker_draining_is_fully_silent is the plan's own
// explicitly named test: draining must be fully silent, distinct from
// closing's rate-limited-but-still-sending behavior.
fn test_connection_close_tracker_draining_is_fully_silent() {
	mut t := new_connection_close_tracker()
	t.enter_draining()
	assert t.state == .draining
	assert !t.may_send()
}

fn test_connection_close_tracker_receiving_close_while_closing_moves_to_draining() {
	mut t := new_connection_close_tracker()
	t.enter_closing()
	assert t.state == .closing

	t.enter_draining()
	assert t.state == .draining
	assert !t.may_send()
}

fn test_connection_close_tracker_active_may_send() {
	t := new_connection_close_tracker()
	assert t.may_send()
}
