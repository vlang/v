module quic

fn test_packet_number_spaces_are_independent() {
	mut spaces := new_packet_number_spaces()
	assert spaces.initial.next_packet_number()! == 0
	assert spaces.initial.next_packet_number()! == 1
	assert spaces.initial.next_packet_number()! == 2

	// The handshake and application_data spaces must be completely
	// unaffected by the initial space having already sent 3 packets -- a
	// connection-global counter would have these start at 3, not 0.
	assert spaces.handshake.next_packet_number()! == 0
	assert spaces.application_data.next_packet_number()! == 0
	assert spaces.handshake.next_packet_number()! == 1
}

fn test_packet_number_space_next_packet_number_increments_sequentially() {
	mut s := PacketNumberSpaceState{}
	for i in u64(0) .. 10 {
		assert s.next_packet_number()! == i
	}
}

// test_packet_number_space_next_packet_number_rejects_exhaustion is a
// Phase-R regression for a Copilot finding on vlang/v#27881
// (pullrequestreview-4885595852): this counter must reject once handing out
// another packet number would exceed max_packet_number, matching
// encode_packet_number's own RFC 9000 §12.3 enforcement, rather than
// silently returning an unusable packet number one layer earlier.
fn test_packet_number_space_next_packet_number_rejects_exhaustion() {
	mut s := PacketNumberSpaceState{
		next_send_pn: max_packet_number
	}
	assert s.next_packet_number()! == max_packet_number

	s.next_packet_number() or {
		assert err.msg().contains('exhausted')
		return
	}
	assert false, 'expected packet-number-space exhaustion to be rejected'
}

fn test_packet_number_space_note_received_never_regresses() {
	mut s := PacketNumberSpaceState{}
	s.note_received(5)
	assert s.largest_received or { 0 } == 5
	s.note_received(3) // arrives later but is a smaller (reordered) packet number
	assert s.largest_received or { 0 } == 5
	s.note_received(10)
	assert s.largest_received or { 0 } == 10
}

fn test_packet_number_space_note_peer_acked_never_regresses() {
	mut s := PacketNumberSpaceState{}
	s.note_peer_acked(7)
	assert s.largest_acked_by_peer or { 0 } == 7
	s.note_peer_acked(2)
	assert s.largest_acked_by_peer or { 0 } == 7
	s.note_peer_acked(20)
	assert s.largest_acked_by_peer or { 0 } == 20
}

// test_packet_number_space_feeds_phase1_functions confirms this state
// struct's fields plug directly into Phase 1's already-tested
// encode_packet_number/decode_packet_number without any adaptation --
// this is what makes it a real per-space "space", not just a label.
fn test_packet_number_space_feeds_phase1_functions() {
	mut s := PacketNumberSpaceState{}
	s.note_peer_acked(1000)
	pn_bytes, pn_len := encode_packet_number(1005, s.largest_acked_by_peer)!
	assert pn_len >= 1

	mut truncated := u64(0)
	for b in pn_bytes {
		truncated = (truncated << 8) | u64(b)
	}

	s.note_received(999)
	full_pn := decode_packet_number(truncated, pn_len, s.largest_received)!
	assert full_pn == 1005
}
