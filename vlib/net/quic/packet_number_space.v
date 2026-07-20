module quic

// RFC 9000 §12.3 — QUIC maintains THREE INDEPENDENT packet number spaces:
// Initial, Handshake, and Application Data (1-RTT). A packet number in one
// space has no relationship whatsoever to a packet number in another --
// flagged repeatedly, across the spec and by real implementers, as the
// single most common mistake in this area. Treating packet numbers as one
// connection-global counter breaks ACK-frame interop with any compliant
// peer, since an ACK frame's Largest Acknowledged/Gap/ACK Range Length
// fields are always relative to the ONE space that carried the ACK frame
// itself (RFC 9000 §13.2.3), never to a connection-wide count.

pub enum QuicPacketNumberSpace {
	initial
	handshake
	application_data
}

// PacketNumberSpaceState tracks everything specific to encoding/decoding
// packet numbers within ONE space. A connection holds exactly three of
// these (see QuicPacketNumberSpaces below) -- never fewer, never shared
// across spaces, never reset back to a shared/global counter.
pub struct PacketNumberSpaceState {
pub mut:
	// next_send_pn is the packet number this side will use for its NEXT
	// packet sent in this space. QUIC packet numbers start at 0 and
	// increase by exactly 1 per packet sent within a space (RFC 9000
	// §12.3) -- never reused, never skipped, even across packets
	// coalesced into the same datagram.
	next_send_pn u64
	// largest_received is the largest packet number this side has
	// successfully processed (header-unprotected AND AEAD-decrypted) in
	// this space, or none if no packet has been processed yet -- the
	// `largest_pn` decode_packet_number needs when reconstructing a
	// peer's truncated packet number in this same space.
	largest_received ?u64
	// largest_acked_by_peer is the largest packet number, IN THIS SPACE,
	// that the peer's most recent ACK frame has acknowledged, or none if
	// nothing has been acked yet -- the `largest_acked` encode_packet_number
	// needs when choosing how many bytes to encode a new outgoing packet
	// number with.
	largest_acked_by_peer ?u64
}

// next_packet_number returns the packet number to use for the next
// outgoing packet in this space and advances the counter. A caller must
// never construct or send two packets sharing the same (space,
// packet_number) pair.
pub fn (mut s PacketNumberSpaceState) next_packet_number() u64 {
	pn := s.next_send_pn
	s.next_send_pn++
	return pn
}

// note_received records that packet number `pn` was successfully
// processed in this space, updating largest_received if `pn` is now the
// largest seen. Packets may legitimately arrive out of order, so this
// must only ever advance, never regress on a smaller, later-arriving
// packet number.
pub fn (mut s PacketNumberSpaceState) note_received(pn u64) {
	current := s.largest_received or {
		s.largest_received = pn
		return
	}

	if pn > current {
		s.largest_received = pn
	}
}

// note_peer_acked records that the peer's most recent ACK frame in this
// space acknowledged up to `largest_in_ack`. Same non-regression rule as
// note_received: a peer's own ACK frames can themselves arrive out of
// order, and an older ACK arriving after a newer one must not walk this
// value backwards.
pub fn (mut s PacketNumberSpaceState) note_peer_acked(largest_in_ack u64) {
	current := s.largest_acked_by_peer or {
		s.largest_acked_by_peer = largest_in_ack
		return
	}

	if largest_in_ack > current {
		s.largest_acked_by_peer = largest_in_ack
	}
}

// QuicPacketNumberSpaces holds the three independent per-space states a
// connection needs. Each field is its own distinct PacketNumberSpaceState
// value -- there is no shared mutable state between them, which is itself
// the enforcement mechanism for "never treat packet numbers as
// connection-global": there is simply no single counter to accidentally
// share.
pub struct QuicPacketNumberSpaces {
pub mut:
	initial          PacketNumberSpaceState
	handshake        PacketNumberSpaceState
	application_data PacketNumberSpaceState
}

pub fn new_packet_number_spaces() &QuicPacketNumberSpaces {
	return &QuicPacketNumberSpaces{}
}
