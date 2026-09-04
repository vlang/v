module quic

fn test_handshake_completion_state_starts_all_false() {
	s := new_handshake_completion_state()
	assert s.is_complete() == false
	assert s.is_confirmed() == false
	assert s.should_discard_initial_keys() == false
	assert s.should_discard_handshake_keys() == false
}

fn test_handshake_completion_initial_key_discard_is_independent() {
	mut s := new_handshake_completion_state()
	s.mark_sent_first_handshake_packet()
	assert s.should_discard_initial_keys() == true
	// Sending the first Handshake packet has NOTHING to do with the
	// complete/confirmed checkpoints -- it typically happens well before
	// either.
	assert s.is_complete() == false
	assert s.is_confirmed() == false
	assert s.should_discard_handshake_keys() == false
}

fn test_handshake_completion_requires_both_finished_events() {
	mut s := new_handshake_completion_state()
	s.mark_own_finished_sent()
	assert s.is_complete() == false // only one of the two conditions met

	mut s2 := new_handshake_completion_state()
	s2.mark_peer_finished_verified()
	assert s2.is_complete() == false // only the other condition met

	mut s3 := new_handshake_completion_state()
	s3.mark_own_finished_sent()
	s3.mark_peer_finished_verified()
	assert s3.is_complete() == true
}

fn test_handshake_completion_confirmed_requires_complete_even_if_handshake_done_arrives_first() {
	mut s := new_handshake_completion_state()
	// A HANDSHAKE_DONE frame should never legitimately arrive before the
	// handshake is complete, but this state machine enforces the AND
	// regardless of caller-ordering mistakes upstream.
	s.mark_handshake_done_received()
	assert s.is_confirmed() == false
	assert s.should_discard_handshake_keys() == false

	s.mark_own_finished_sent()
	s.mark_peer_finished_verified()
	assert s.is_complete() == true
	assert s.is_confirmed() == true
	assert s.should_discard_handshake_keys() == true
}

fn test_handshake_completion_confirmed_after_normal_ordering() {
	mut s := new_handshake_completion_state()
	s.mark_own_finished_sent()
	s.mark_peer_finished_verified()
	assert s.is_confirmed() == false // complete, but HANDSHAKE_DONE not yet seen

	s.mark_handshake_done_received()
	assert s.is_confirmed() == true
}
