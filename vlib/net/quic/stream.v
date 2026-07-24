module quic

// RFC 9000 §2.1 — Stream ID structure. The low 2 bits of a stream ID
// encode who opened it and whether it's uni/bidirectional; every stream
// ID belongs to exactly one of 4 categories, and QUIC never reuses an ID
// across categories:
//
//	bit 0 (0x01): 0 = client-initiated, 1 = server-initiated
//	bit 1 (0x02): 0 = bidirectional,     1 = unidirectional

pub enum StreamInitiator {
	client
	server
}

pub enum StreamDirection {
	bidirectional
	unidirectional
}

// QuicRole distinguishes which side of the connection THIS endpoint is --
// needed because "is this stream mine to have opened" and "am I allowed
// to send on this uni stream" depend on who's asking, not just the ID
// itself. v1 only ever runs as .client (server support is Phase 13, out
// of committed scope) -- this enum exists now so stream.v doesn't need
// reshaping when that phase lands, matching QuicConn's own planned
// `role`-field design.
pub enum QuicRole {
	client
	server
}

// StreamId wraps a raw stream ID with its RFC 9000 §2.1 category
// derivation.
pub struct StreamId {
pub:
	value u64
}

pub fn (id StreamId) initiator() StreamInitiator {
	return if id.value & 0x01 == 0 { StreamInitiator.client } else { StreamInitiator.server }
}

pub fn (id StreamId) direction() StreamDirection {
	return if id.value & 0x02 == 0 {
		StreamDirection.bidirectional
	} else {
		StreamDirection.unidirectional
	}
}

// is_locally_initiated reports whether `role` (this endpoint's own role)
// is the one that opened the stream with this ID.
pub fn (id StreamId) is_locally_initiated(role QuicRole) bool {
	return match role {
		.client { id.initiator() == .client }
		.server { id.initiator() == .server }
	}
}

// first_stream_id_of returns the lowest (first-allocated) stream ID for a
// given (initiator, direction) category -- 0, 1, 2, 3 respectively for
// client-bidi, server-bidi, client-uni, server-uni. Successive streams in
// the same category are always exactly +4 from the previous one (RFC 9000
// §2.1).
pub fn first_stream_id_of(initiator StreamInitiator, direction StreamDirection) u64 {
	mut v := u64(0)
	if initiator == .server {
		v |= 0x01
	}
	if direction == .unidirectional {
		v |= 0x02
	}
	return v
}

// RFC 9000 §3.1 — Send Stream States. Transitions driven by LOCAL actions
// (queuing data, sending FIN, sending RESET_STREAM) are modeled here
// directly; transitions driven by the PEER'S ACKNOWLEDGMENT (Data Sent ->
// Data Recvd, Reset Sent -> Reset Recvd) need ACK-processing machinery
// this module doesn't have yet (Phase 7) -- mark_all_data_acked/
// mark_reset_acked exist as the hooks a later phase will call, not
// something this file drives on its own.
pub enum SendStreamState {
	ready
	send
	data_sent
	data_recvd
	reset_sent
	reset_recvd
}

// RFC 9000 §3.2 — Receive Stream States. size_known/data_recvd/
// reset_recvd are driven by frame ARRIVAL (this file's job); data_read/
// reset_read are driven by the APPLICATION consuming the data/reset (a
// later phase's job, once an application-facing read API exists) --
// mark_data_read/mark_reset_read exist as that future hook.
pub enum RecvStreamState {
	recv
	size_known
	data_recvd
	data_read
	reset_recvd
	reset_read
}

// StreamSendHalf is the send-side state for a stream THIS endpoint may
// send on (present on every bidi stream, and on uni streams this side
// opened).
pub struct StreamSendHalf {
pub mut:
	state      SendStreamState
	offset     u64
	final_size ?u64
	error_code ?u64 // set once reset_sent (our own RESET_STREAM's error code)
}

// mark_data_queued transitions ready -> send the first time data is
// queued to send on this stream.
pub fn (mut h StreamSendHalf) mark_data_queued() {
	if h.state == .ready {
		h.state = .send
	}
}

// mark_fin_sent transitions send -> data_sent once a FIN-carrying STREAM
// frame has been sent, recording the stream's final size.
pub fn (mut h StreamSendHalf) mark_fin_sent(final_size u64) {
	h.final_size = final_size
	if h.state == .ready || h.state == .send {
		h.state = .data_sent
	}
}

// mark_reset_sent transitions to reset_sent from any state prior to
// data_recvd/reset_recvd -- RFC 9000 §3.1 permits resetting a stream at
// any point before its send side has fully completed.
pub fn (mut h StreamSendHalf) mark_reset_sent(error_code u64) {
	h.error_code = error_code
	h.state = .reset_sent
}

// RecvHalf is the receive-side state for a stream THIS endpoint may
// receive on (present on every bidi stream, and on uni streams the PEER
// opened).
pub struct StreamRecvHalf {
pub mut:
	state       RecvStreamState
	reassembler &StreamReassembler = unsafe { nil }
	final_size  ?u64
	error_code  ?u64 // set once reset_recvd (peer's RESET_STREAM error code)
}

// note_size_known transitions recv -> size_known once the final size is
// learned (a FIN-carrying STREAM frame, or a RESET_STREAM frame).
pub fn (mut h StreamRecvHalf) note_size_known(final_size u64) ! {
	h.reassembler.note_final_size(final_size)!
	h.final_size = final_size
	if h.state == .recv {
		h.state = .size_known
	}
	if u64(h.reassembler.consumed_len()) >= final_size {
		h.state = .data_recvd
	}
}

// note_data records incoming STREAM frame data and promotes recv/
// size_known -> data_recvd once the reassembler has everything up to a
// known final size.
pub fn (mut h StreamRecvHalf) note_data(offset u64, data []u8) ! {
	h.reassembler.add(offset, data)!
	if final := h.final_size {
		if h.reassembler.consumed_len() >= final {
			h.state = .data_recvd
		}
	}
}

// mark_reset_recvd transitions to reset_recvd on receiving RESET_STREAM --
// legal from recv or size_known (RFC 9000 §3.2).
pub fn (mut h StreamRecvHalf) mark_reset_recvd(error_code u64) {
	h.error_code = error_code
	h.state = .reset_recvd
}

// QuicStream is one stream's full state: an identity (id + direction) and
// whichever of send/recv halves THIS endpoint actually has, given who
// opened it relative to our own role -- a locally-initiated uni stream has
// only `send`; a peer-initiated uni stream has only `recv`; every bidi
// stream (either initiator) has both. `send`/`recv` are nilable POINTERS
// (matching Tls13ClientHandshake.verified_chain's established convention
// elsewhere in this module), not Optional VALUE fields -- an Optional
// struct field requires unwrap-mutate-reassign on every update (`h := s.recv
// or {...}; h.note_data(...)!; s.recv = h`), which is easy to get
// subtly wrong (mutating the unwrapped copy and forgetting the final
// reassignment silently drops the update). A pointer field lets every
// caller mutate the SAME shared half directly (`s.recv.note_data(...)!`)
// with no copy-back step to forget.
@[heap]
pub struct QuicStream {
pub:
	id        StreamId
	direction StreamDirection
pub mut:
	send &StreamSendHalf = unsafe { nil }
	recv &StreamRecvHalf = unsafe { nil }
}

pub fn (s &QuicStream) has_send() bool {
	return s.send != unsafe { nil }
}

pub fn (s &QuicStream) has_recv() bool {
	return s.recv != unsafe { nil }
}

// new_quic_stream constructs a QuicStream for `id`, populating exactly the
// halves this endpoint (given `role`) actually has for that ID's category.
pub fn new_quic_stream(id StreamId, role QuicRole) &QuicStream {
	direction := id.direction()
	locally_initiated := id.is_locally_initiated(role)

	mut has_send := true
	mut has_recv := true
	if direction == .unidirectional {
		has_send = locally_initiated
		has_recv = !locally_initiated
	}

	mut s := &QuicStream{
		id:        id
		direction: direction
	}
	if has_send {
		s.send = &StreamSendHalf{}
	}
	if has_recv {
		s.recv = &StreamRecvHalf{
			reassembler: new_stream_reassembler()
		}
	}
	return s
}

// QuicStreamSet tracks every stream known to one connection, keyed by raw
// stream ID.
@[heap]
pub struct QuicStreamSet {
mut:
	role            QuicRole
	streams         map[u64]&QuicStream
	next_local_bidi u64
	next_local_uni  u64
}

pub fn new_quic_stream_set(role QuicRole) &QuicStreamSet {
	return &QuicStreamSet{
		role: role
	}
}

// get returns an already-known stream, or none.
pub fn (s &QuicStreamSet) get(raw_id u64) ?&QuicStream {
	return s.streams[raw_id] or { return none }
}

// open_local_stream allocates the next available LOCALLY-initiated stream
// ID of `direction` (RFC 9000 §2.1's "streams of the same type are
// created in sequentially increasing order" rule) and registers a new
// QuicStream for it. This is the send-side mirror of get_or_create's
// receive-side auto-creation: get_or_create NEVER fabricates a
// locally-initiated stream on the peer's say-so (see its own doc
// comment) -- this function is how one of those streams actually comes
// into existence, driven by THIS endpoint's own decision to open it.
pub fn (mut s QuicStreamSet) open_local_stream(direction StreamDirection) &QuicStream {
	initiator := if s.role == .client { StreamInitiator.client } else { StreamInitiator.server }
	base := first_stream_id_of(initiator, direction)

	mut index := u64(0)
	if direction == .unidirectional {
		index = s.next_local_uni
		s.next_local_uni++
	} else {
		index = s.next_local_bidi
		s.next_local_bidi++
	}

	id_value := base + index * 4
	stream := new_quic_stream(StreamId{
		value: id_value
	}, s.role)
	s.streams[id_value] = stream
	return stream
}

pub fn (s &QuicStreamSet) len() int {
	return s.streams.len
}

// get_or_create returns an existing stream, or creates one IF `raw_id` is
// legally something the PEER may open unilaterally by simply referencing
// it in a frame. RFC 9000 §2.1: "Before a stream is created, all streams
// of the same type with lower-numbered stream IDs MUST be created" is
// satisfied here by also creating every lower-numbered, same-category
// stream that doesn't exist yet (up to and including `raw_id`) -- matching
// how a real peer's own stream numbering works (a peer opening stream 8
// implies streams 0 and 4 already exist, even if we never separately saw
// traffic on them).
//
// `max_streams` is the CURRENTLY advertised limit (RFC 9000 §4.6) for this
// ID's category -- the caller supplies it (sourced from flow_control.v's
// own state), keeping this function decoupled from owning that state
// itself.
//
// Locally-initiated stream IDs are NEVER auto-created this way -- a peer
// referencing an ID that's ours to open, but that we haven't opened, is a
// protocol violation (STREAM_STATE_ERROR), not something to paper over by
// fabricating a stream on their behalf.
pub fn (mut s QuicStreamSet) get_or_create(raw_id u64, max_streams u64) !&QuicStream {
	if existing := s.streams[raw_id] {
		return existing
	}
	id := StreamId{
		value: raw_id
	}
	if id.is_locally_initiated(s.role) {
		return error('quic: STREAM_STATE_ERROR: peer referenced locally-initiated stream ${raw_id} that was never opened')
	}

	base := first_stream_id_of(id.initiator(), id.direction())
	step := u64(4)
	index := (raw_id - base) / step // 0-based position within this category
	if index + 1 > max_streams {
		return error('quic: STREAM_LIMIT_ERROR: peer exceeded the max_streams limit (${max_streams}) for this stream category with id ${raw_id}')
	}

	mut n := base
	for n <= raw_id {
		if n !in s.streams {
			s.streams[n] = new_quic_stream(StreamId{
				value: n
			}, s.role)
		}
		n += step
	}
	return s.streams[raw_id] or { panic('quic: internal error: stream ${raw_id} not created') }
}
