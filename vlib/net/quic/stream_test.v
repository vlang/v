module quic

fn test_stream_id_category_derivation_all_four_combinations() {
	client_bidi := StreamId{
		value: 0
	}
	assert client_bidi.initiator() == .client
	assert client_bidi.direction() == .bidirectional

	server_bidi := StreamId{
		value: 1
	}
	assert server_bidi.initiator() == .server
	assert server_bidi.direction() == .bidirectional

	client_uni := StreamId{
		value: 2
	}
	assert client_uni.initiator() == .client
	assert client_uni.direction() == .unidirectional

	server_uni := StreamId{
		value: 3
	}
	assert server_uni.initiator() == .server
	assert server_uni.direction() == .unidirectional

	// Successive streams in a category are always +4.
	next_client_bidi := StreamId{
		value: 4
	}
	assert next_client_bidi.initiator() == .client
	assert next_client_bidi.direction() == .bidirectional
}

fn assert_first_stream_id_round_trips(initiator StreamInitiator, direction StreamDirection) {
	id := StreamId{
		value: first_stream_id_of(initiator, direction)
	}
	assert id.initiator() == initiator
	assert id.direction() == direction
}

fn test_first_stream_id_of_matches_category_derivation() {
	assert_first_stream_id_round_trips(.client, .bidirectional)
	assert_first_stream_id_round_trips(.server, .bidirectional)
	assert_first_stream_id_round_trips(.client, .unidirectional)
	assert_first_stream_id_round_trips(.server, .unidirectional)
}

fn test_is_locally_initiated_for_both_roles() {
	client_bidi := StreamId{
		value: 0
	}
	assert client_bidi.is_locally_initiated(.client) == true
	assert client_bidi.is_locally_initiated(.server) == false

	server_uni := StreamId{
		value: 3
	}
	assert server_uni.is_locally_initiated(.client) == false
	assert server_uni.is_locally_initiated(.server) == true
}

fn test_new_quic_stream_bidi_gets_both_halves_regardless_of_initiator() {
	client_bidi_id := StreamId{
		value: 0
	}
	s1 := new_quic_stream(client_bidi_id, .client)
	assert s1.has_send()
	assert s1.has_recv()

	server_bidi_id := StreamId{
		value: 1
	}
	s2 := new_quic_stream(server_bidi_id, .client)
	assert s2.has_send()
	assert s2.has_recv()
}

fn test_new_quic_stream_locally_initiated_uni_is_send_only() {
	client_uni_id := StreamId{
		value: 2
	}
	s := new_quic_stream(client_uni_id, .client)
	assert s.has_send()
	assert !s.has_recv()
}

// test_new_quic_stream_peer_initiated_uni_is_recv_only is the plan's own
// explicitly named test: "even client-first phase must correctly receive
// server-initiated unidirectional streams from day one" -- confirms a
// server-initiated uni stream, from the client's own perspective, has
// exactly a receive half and no send half (this client can never send on
// a stream the SERVER opened unidirectionally).
fn test_new_quic_stream_peer_initiated_uni_is_recv_only() {
	server_uni_id := StreamId{
		value: 3
	}
	s := new_quic_stream(server_uni_id, .client)
	assert !s.has_send()
	assert s.has_recv()
}

fn test_quic_stream_set_get_or_create_auto_creates_lower_numbered_streams() {
	mut set := new_quic_stream_set(.client)
	// Stream 9 is server-initiated bidi (9 & 0x03 == 1), the 3rd stream in
	// that category (ids 1, 5, 9). Referencing it directly must also
	// create 1 and 5.
	stream := set.get_or_create(9, 100)!
	assert stream.id.value == 9
	assert set.len() == 3
	assert set.get(1) != none
	assert set.get(5) != none
	assert set.get(9) != none
}

fn test_quic_stream_set_get_or_create_rejects_locally_initiated_unknown_id() {
	mut set := new_quic_stream_set(.client)
	// Stream 0 is client-initiated bidi -- this client's own to open, not
	// something a peer frame referencing it can conjure into existence.
	set.get_or_create(0, 100) or {
		assert err.msg().contains('STREAM_STATE_ERROR')
		return
	}
	assert false, 'expected referencing an unopened locally-initiated stream to be rejected'
}

fn test_quic_stream_set_get_or_create_enforces_max_streams_limit() {
	mut set := new_quic_stream_set(.client)
	// Server-initiated bidi streams: 1, 5, 9, 13, ... -- id 13 is the 4th
	// (index 3), requiring max_streams >= 4.
	set.get_or_create(13, 4)! // exactly at the limit: allowed
	assert set.len() == 4

	mut set2 := new_quic_stream_set(.client)
	set2.get_or_create(13, 3) or {
		assert err.msg().contains('STREAM_LIMIT_ERROR')
		return
	}
	assert false, 'expected exceeding max_streams to be rejected'
}

fn test_open_local_stream_allocates_sequential_ids() {
	mut set := new_quic_stream_set(.client)
	first := set.open_local_stream(.bidirectional)
	assert first.id.value == 0
	second := set.open_local_stream(.bidirectional)
	assert second.id.value == 4
	third := set.open_local_stream(.bidirectional)
	assert third.id.value == 8

	// A separate counter for uni streams, starting at the uni base (2 for
	// client), independent of the bidi counter above.
	uni_first := set.open_local_stream(.unidirectional)
	assert uni_first.id.value == 2
	uni_second := set.open_local_stream(.unidirectional)
	assert uni_second.id.value == 6

	assert set.len() == 5
	assert set.get(0) != none
}

fn test_open_local_stream_registers_in_the_set() {
	mut set := new_quic_stream_set(.client)
	opened := set.open_local_stream(.bidirectional)
	found := set.get(opened.id.value) or {
		assert false, 'expected open_local_stream to register the stream'
		return
	}
	assert found.id.value == opened.id.value
}

fn test_quic_stream_set_get_returns_none_for_unknown_stream() {
	set := new_quic_stream_set(.client)
	assert set.get(0) == none
}

fn test_send_half_state_transitions() {
	mut h := StreamSendHalf{}
	assert h.state == .ready
	h.mark_data_queued()
	assert h.state == .send
	h.mark_fin_sent(100)
	assert h.state == .data_sent
	assert h.final_size or { 0 } == 100
}

fn test_send_half_reset_from_ready() {
	mut h := StreamSendHalf{}
	h.mark_reset_sent(7)
	assert h.state == .reset_sent
	assert h.error_code or { 0 } == 7
}

fn test_recv_half_state_transitions_data_then_fin() {
	mut h := StreamRecvHalf{
		reassembler: new_stream_reassembler()
	}
	assert h.state == .recv
	h.note_data(0, [u8(1), 2, 3])!
	assert h.state == .recv // final size still unknown
	h.note_size_known(3)!
	assert h.state == .data_recvd // all 3 bytes already present
}

fn test_recv_half_state_transitions_fin_before_all_data() {
	mut h := StreamRecvHalf{
		reassembler: new_stream_reassembler()
	}
	h.note_size_known(5)!
	assert h.state == .size_known // final size known, but no data yet
	h.note_data(0, [u8(1), 2, 3, 4, 5])!
	assert h.state == .data_recvd
}

fn test_recv_half_reset_recvd() {
	mut h := StreamRecvHalf{
		reassembler: new_stream_reassembler()
	}
	h.mark_reset_recvd(4)
	assert h.state == .reset_recvd
	assert h.error_code or { 0 } == 4
}
