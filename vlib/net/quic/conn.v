module quic

import time
import rand

// RFC 9000/9001 — QuicConn is the top-level connection struct wiring
// together every independently-built piece from Phases 0-8: packet/header
// protection, the TLS 1.3 handshake, packet number spaces, CRYPTO stream
// reassembly, loss detection, NewReno congestion control, and
// connection-lifecycle bookkeeping (idle timeout, CONNECTION_CLOSE,
// stateless reset, ECN). No prior file composes any two of these; this file
// is that composition.
//
// Phase 9a scope: connection establishment only, through the real RFC 9001
// §4.1.2 "confirmed" checkpoint (not merely "complete" -- see
// handshake_confirm.v). Steady-state 1-RTT application data (STREAM frames,
// flow control, open/write/read_stream) is Phase 9b's job; this file
// accepts those frame types on the wire without erroring (they are legal at
// this packet-number space, RFC 9000 §12.4) but does not yet act on them.
//
// Single-threaded, caller-driven event loop (the tracking issue's own
// wording: "mirroring how quiche/ngtcp2 expose themselves as libraries"):
// this struct owns state, the caller owns actual socket I/O and calls
// poll()/process_timeouts() to advance it. Nothing happens off the
// caller's own call stack -- no locks, no background threads.
//
// Connection ID rotation/migration is explicitly out of v1 scope (not
// deferred -- stateless_reset.v/pmtu.v both already say so independently):
// exactly one local `scid` and one tracked peer `dcid`, no active CID set,
// no NEW_CONNECTION_ID/RETIRE_CONNECTION_ID.

// local_cid_len is this client's own chosen connection-ID length -- within
// RFC 9000 §17.2's 20-byte v1 limit, matching common real-world practice
// (quiche and others typically use 8).
const local_cid_len = 8

// aead_tag_len is AES-128-GCM's fixed authentication tag length (RFC 9001
// §5.3), confirmed against initial_exchange_test.v's own literal use in the
// same padding-size calculation this file's build_initial_packet mirrors.
const aead_tag_len = 16

// quic_error_protocol_violation is RFC 9000 §20.1's PROTOCOL_VIOLATION --
// the fallback CONNECTION_CLOSE error code for an internal failure that
// carries no more specific QUIC error code of its own (see close_with_error).
const quic_error_protocol_violation = u64(0x0a)

// default_max_ack_delay is RFC 9000 §18.2's stated default for the
// max_ack_delay transport parameter, used until the peer's own value (if
// any) is known.
const default_max_ack_delay_ms = u64(25)

pub enum ConnectionState {
	handshaking
	established
	closing
	draining
	closed
}

// QuicDatagram is one UDP datagram this connection needs the caller to
// actually write to the socket.
pub struct QuicDatagram {
pub:
	bytes []u8
}

pub enum QuicEventKind {
	handshake_confirmed
	connection_closed
}

// QuicEvent reports one thing that happened during a poll()/
// process_timeouts() call. `error_code`/`reason` are set only for
// connection_closed.
pub struct QuicEvent {
pub:
	kind       QuicEventKind
	error_code ?u64
	reason     string
}

// PollResult reports everything one poll()/process_timeouts() call
// produced: events, datagrams the caller must now send, and when to next
// call process_timeouts() if nothing else arrives first (none means no
// timer is currently armed).
pub struct PollResult {
pub mut:
	events       []QuicEvent
	outgoing     []QuicDatagram
	next_timeout ?u64
}

// DialParams is everything dial() needs beyond what it decides for itself
// (connection IDs, ClientHello random). `transport_parameters` is this
// client's own OFFERED set; dial() overrides its
// initial_source_connection_id with the freshly-picked scid regardless of
// what the caller set there.
pub struct DialParams {
pub:
	server_name          string
	ca_bundle_pem        string
	alpn_protocols       []string
	transport_parameters QuicTransportParameters
}

@[heap]
pub struct QuicConn {
mut:
	role  QuicRole
	state ConnectionState

	original_dcid                 []u8
	dcid                          []u8
	scid                          []u8
	peer_scid                     []u8
	token                         []u8
	retry_accepted                bool
	processed_first_server_packet bool
	retry_scid                    ?[]u8

	handshake            &Tls13ClientHandshake
	handshake_completion &HandshakeCompletionState
	pn_spaces            &QuicPacketNumberSpaces

	initial_keys_client      QuicPacketProtectionKeys
	initial_keys_server      QuicPacketProtectionKeys
	initial_keys_discarded   bool
	handshake_keys_client    ?QuicPacketProtectionKeys
	handshake_keys_server    ?QuicPacketProtectionKeys
	handshake_keys_discarded bool
	app_write_keys           ?QuicPacketProtectionKeys
	app_read_keys            ?&KeyUpdateState

	initial_crypto               &CryptoStreamReassembler
	initial_crypto_consumed      u64
	handshake_crypto             &CryptoStreamReassembler
	handshake_crypto_consumed    u64
	handshake_crypto_send_offset u64

	client_hello             []u8
	pending_initial_crypto   ?[]u8
	pending_handshake_crypto ?[]u8

	loss_detection     &QuicLossDetectionTimer
	congestion_control NewRenoCongestionControl

	own_max_idle_timeout_ms u64
	idle_timeout            IdleTimeoutState
	connection_close        ConnectionCloseTracker
	stateless_reset         StatelessResetTracker
	ecn                     EcnState

	initial_received_pns   map[u64]bool
	handshake_received_pns map[u64]bool
	app_received_pns       map[u64]bool

	connection_start u64

	// -- Phase 9b: steady-state 1-RTT state ---------------------------------
	own_transport_parameters QuicTransportParameters

	streams              &QuicStreamSet
	stream_send_windows  map[u64]&FlowControlWindow
	stream_recv_windows  map[u64]&ReceiveWindow
	pending_stream_write map[u64]PendingStreamWrite
	pending_stream_reset map[u64]u64 // stream_id -> error_code, queued RESET_STREAM frames
	conn_bytes_read      u64

	conn_send_window FlowControlWindow
	conn_recv_window ReceiveWindow

	// local_max_streams_* are the FIXED caps this endpoint advertised to the
	// peer (via own_transport_parameters) for how many streams the peer may
	// open to us -- v1 simplification: never dynamically raised with a
	// follow-up MAX_STREAMS frame of our own (documented follow-up, not a
	// Phase 9b blocker). peer_max_streams_* are the CURRENT caps the peer
	// has told us (initial transport parameter, then raised by any
	// MAX_STREAMS frames received) for how many streams we may open to it.
	local_max_streams_bidi u64
	local_max_streams_uni  u64
	peer_max_streams_bidi  u64
	peer_max_streams_uni   u64
	// last value we've already told the peer we're blocked at, per
	// direction -- avoids re-sending a redundant STREAMS_BLOCKED for the
	// same still-unraised limit on every subsequent open_stream() call.
	streams_blocked_sent_bidi ?u64
	streams_blocked_sent_uni  ?u64
	pending_streams_blocked   []StreamDirection

	pending_close      ?PendingClose
	sent_close_payload ?[]u8
	closing_deadline   ?u64
}

// PendingStreamWrite accumulates data queued via write_stream() awaiting
// the next drain_outgoing() call -- fin OR-latches (once true, a later
// write_stream(..., fin: false) call never un-sets it; RFC 9000 §3.1 has no
// "un-FIN" operation).
struct PendingStreamWrite {
mut:
	data []u8
	fin  bool
}

// PendingClose is set by the public close() API and drained by the next
// poll()/process_timeouts() call (which has the `now`/PollResult context
// close() itself deliberately doesn't take -- matching the "nothing
// happens off the caller's own call stack" rule the rest of this file
// follows).
struct PendingClose {
	error_code           u64
	reason               string
	is_application_error bool
}

// state reports which of RFC 9000's connection lifecycle states this
// connection is currently in.
pub fn (c &QuicConn) state() ConnectionState {
	return c.state
}

// role reports which side of the connection this endpoint is. v1 is always
// .client (see stream.v's QuicRole doc comment).
pub fn (c &QuicConn) role() QuicRole {
	return c.role
}

// -------------------------------------------------------------------------
// Streams and application data (Phase 9b)
// -------------------------------------------------------------------------

// ensure_stream_windows lazily creates `id`'s per-stream flow-control
// windows the first time this connection sees it -- from either side (a
// local open_stream() call, or the first STREAM/RESET_STREAM/etc. frame
// naming a peer-opened stream). Both directions' windows are always
// created regardless of which half `id`'s category actually has (matches
// new_quic_stream's own "populate exactly the halves this endpoint has"
// pattern being a SEPARATE, QuicStream-level concern -- an unused window
// here just never gets touched, which is cheaper than threading the
// has_send/has_recv distinction through every call site).
fn (mut c QuicConn) ensure_stream_windows(id StreamId) {
	if id.value !in c.stream_send_windows {
		limit := initial_send_limit_for_stream(id, c.role, c.handshake.peer_transport_parameters)
		mut w := new_flow_control_window(limit)
		c.stream_send_windows[id.value] = &w
	}
	if id.value !in c.stream_recv_windows {
		limit := initial_receive_limit_for_stream(id, c.role, c.own_transport_parameters)
		mut w := new_receive_window(limit)
		c.stream_recv_windows[id.value] = &w
	}
}

// open_stream opens a new LOCALLY-initiated stream (bidirectional if `bidi`,
// else unidirectional), enforcing RFC 9000 §4.6's peer-imposed MAX_STREAMS
// limit -- QuicStreamSet.open_local_stream itself does NOT check this (see
// its own doc comment), so this is where that check belongs. Queues a
// STREAMS_BLOCKED frame (once per still-unraised limit value, not on every
// call) when blocked, per RFC 9000 §19.14.
pub fn (mut c QuicConn) open_stream(bidi bool) !u64 {
	direction := if bidi { StreamDirection.bidirectional } else { StreamDirection.unidirectional }
	opened_count := if bidi { c.streams.next_local_bidi } else { c.streams.next_local_uni }
	limit := if bidi { c.peer_max_streams_bidi } else { c.peer_max_streams_uni }
	if opened_count >= limit {
		already_reported := if bidi {
			c.streams_blocked_sent_bidi
		} else {
			c.streams_blocked_sent_uni
		}
		mut should_report := true
		if v := already_reported {
			should_report = v != limit
		}
		if should_report {
			c.pending_streams_blocked << direction
			if bidi {
				c.streams_blocked_sent_bidi = limit
			} else {
				c.streams_blocked_sent_uni = limit
			}
		}
		return error('quic: STREAM_LIMIT: peer allows only ${limit} ${direction} stream(s), already opened ${opened_count}')
	}
	stream := c.streams.open_local_stream(direction)
	c.ensure_stream_windows(stream.id)
	return stream.id.value
}

// write_stream queues `data` (and, if `fin`, the end of the stream) to send
// on `stream_id`. Nothing is actually sent until the next poll()/
// process_timeouts() call drains it -- matching this file's own "nothing
// happens off the caller's own call stack" invariant. Actual flow-control
// admission (both this stream's own send window and the connection-level
// one) is checked at DRAIN time, not here, since the available budget can
// change between queuing and draining (a MAX_DATA/MAX_STREAM_DATA frame
// might arrive first).
pub fn (mut c QuicConn) write_stream(stream_id u64, data []u8, fin bool) ! {
	stream := c.streams.get(stream_id) or { return error('quic: unknown stream ${stream_id}') }
	if !stream.has_send() {
		return error('quic: stream ${stream_id} has no send side')
	}
	if stream.send.state == .data_sent || stream.send.state == .reset_sent
		|| stream.send.state == .reset_recvd || stream.send.state == .data_recvd {
		return error('quic: stream ${stream_id} send side already closed (state ${stream.send.state})')
	}
	// A stream can exist in c.streams without flow-control windows yet: RFC
	// 9000 §2.1 requires get_or_create to auto-create every LOWER-numbered
	// same-category stream when the peer references a higher one directly,
	// and only the explicitly-named frame.stream_id gets its windows
	// created at that point (handle_stream_frame/handle_reset_stream_frame).
	// Without this call, drain_pending_stream_writes' window lookup for
	// such a sibling would never succeed, and the write would be queued
	// forever with no error (found via /vreview -- see
	// test_write_stream_on_auto_created_sibling_stream_is_not_stuck).
	c.ensure_stream_windows(StreamId{
		value: stream_id
	})
	mut pending := c.pending_stream_write[stream_id] or { PendingStreamWrite{} }
	pending.data << data
	pending.fin = pending.fin || fin
	c.pending_stream_write[stream_id] = pending
}

// read_stream returns and consumes whatever new bytes have arrived on
// `stream_id` since the last read_stream() call (an empty slice if
// nothing new has arrived). Advances the stream's own read-side
// flow-control marker AND the connection-level one -- see
// should_advertise_more's own doc comment for why both must move together.
pub fn (mut c QuicConn) read_stream(stream_id u64) ![]u8 {
	mut stream := c.streams.get(stream_id) or { return error('quic: unknown stream ${stream_id}') }
	if !stream.has_recv() {
		return error('quic: stream ${stream_id} has no receive side')
	}
	// See write_stream's identical call for why this is needed even for an
	// already-existing stream (an auto-created sibling may have no windows
	// yet).
	c.ensure_stream_windows(StreamId{
		value: stream_id
	})
	data := stream.recv.reassembler.data().clone()
	if data.len == 0 {
		return data
	}
	new_base := stream.recv.reassembler.consumed_len()
	stream.recv.reassembler.discard(new_base)!
	if mut w := c.stream_recv_windows[stream_id] {
		w.note_read(new_base)
	}
	c.conn_bytes_read += u64(data.len)
	c.conn_recv_window.note_read(c.conn_bytes_read)
	return data
}

// close requests a graceful, application-initiated shutdown. Like
// write_stream, nothing happens synchronously -- the CONNECTION_CLOSE
// frame is built and sent on the next poll()/process_timeouts() call. A
// no-op once already closing/draining/closed.
pub fn (mut c QuicConn) close(error_code u64, reason string) {
	if c.state == .closing || c.state == .draining || c.state == .closed {
		return
	}
	c.pending_close = PendingClose{
		error_code:           error_code
		reason:               reason
		is_application_error: true
	}
}

// dial starts a new client connection attempt: picks this endpoint's own
// connection IDs, derives Initial secrets, builds the ClientHello, and
// returns the connection object plus the first outgoing datagram (a padded
// Initial packet) the caller must send.
pub fn dial(params DialParams, now u64) !(&QuicConn, QuicDatagram) {
	original_dcid := rand.bytes(local_cid_len) or {
		return error('quic: failed to generate initial destination connection ID: ${err.msg()}')
	}
	scid := rand.bytes(local_cid_len) or {
		return error('quic: failed to generate source connection ID: ${err.msg()}')
	}
	client_random := rand.bytes(32) or {
		return error('quic: failed to generate ClientHello random: ${err.msg()}')
	}

	mut own_params := params.transport_parameters
	own_params.initial_source_connection_id = scid.clone()

	handshake, client_hello := Tls13ClientHandshake.start(ClientHandshakeParams{
		random:               client_random
		server_name:          params.server_name
		transport_parameters: own_params
		ca_bundle_pem:        params.ca_bundle_pem
		alpn_protocols:       params.alpn_protocols
	})!

	initial_secrets := derive_initial_secrets(original_dcid)!
	initial_keys_client := derive_packet_protection_keys(initial_secrets.client)!
	initial_keys_server := derive_packet_protection_keys(initial_secrets.server)!

	own_max_idle_timeout_ms := own_params.max_idle_timeout or { u64(0) }

	mut c := &QuicConn{
		role:                     .client
		state:                    .handshaking
		original_dcid:            original_dcid
		dcid:                     original_dcid.clone()
		scid:                     scid
		peer_scid:                []u8{}
		token:                    []u8{}
		handshake:                handshake
		handshake_completion:     new_handshake_completion_state()
		pn_spaces:                new_packet_number_spaces()
		initial_keys_client:      initial_keys_client
		initial_keys_server:      initial_keys_server
		initial_crypto:           new_crypto_stream_reassembler()
		handshake_crypto:         new_crypto_stream_reassembler()
		client_hello:             client_hello
		loss_detection:           new_quic_loss_detection_timer()
		congestion_control:       new_newreno_congestion_control()
		own_max_idle_timeout_ms:  own_max_idle_timeout_ms
		stateless_reset:          new_stateless_reset_tracker()
		connection_start:         now
		own_transport_parameters: own_params
		streams:                  new_quic_stream_set(.client)
		conn_send_window:         new_flow_control_window(0)
		conn_recv_window:         new_receive_window(own_params.initial_max_data or { u64(0) })
		local_max_streams_bidi:   own_params.initial_max_streams_bidi or { u64(0) }
		local_max_streams_uni:    own_params.initial_max_streams_uni or { u64(0) }
	}

	crypto_frame := encode_crypto_frame(0, client_hello)!
	datagram := c.build_initial_packet(crypto_frame, true, now)!
	return c, datagram
}

// poll feeds one incoming UDP datagram (or none, to just drain queued
// outgoing data and check timers) and returns what happened. A protocol
// error while processing `incoming` closes the connection (transitions to
// .closing, queues a best-effort CONNECTION_CLOSE datagram, and reports a
// connection_closed event) rather than propagating as a Result error --
// callers see connection failures as ordinary events, matching how a
// caller-driven library is expected to behave, not as exceptions.
pub fn (mut c QuicConn) poll(incoming ?[]u8, now u64) !PollResult {
	mut result := PollResult{}
	if c.state == .closed {
		return result
	}
	if data := incoming {
		c.process_datagram(data, now, mut result) or {
			code := if err.code() != 0 { u64(err.code()) } else { quic_error_protocol_violation }
			c.close_with_error(code, err.msg(), false, now, mut result)
		}
	}
	c.drain_pending_close(now, mut result)
	if c.state == .handshaking || c.state == .established {
		c.drain_outgoing(now, mut result)!
	}
	result.next_timeout = c.compute_next_timeout()
	return result
}

// drain_pending_close actions a close() call queued since the last poll()/
// process_timeouts() -- see close()'s own doc comment for why the actual
// send is deferred to here rather than happening synchronously. A no-op if
// no close is pending, or if this connection has already started closing/
// draining/closed by some other path in the meantime (e.g. a protocol
// violation detected while processing this same call's incoming data).
fn (mut c QuicConn) drain_pending_close(now u64, mut result PollResult) {
	pc := c.pending_close or { return }
	c.pending_close = none
	if c.state == .closing || c.state == .draining || c.state == .closed {
		return
	}
	c.close_with_error(pc.error_code, pc.reason, pc.is_application_error, now, mut result)
}

// process_timeouts is called when a previously-returned next_timeout
// deadline elapses with no new incoming data. Checks idle timeout first
// (a genuinely idle connection has nothing left to time-out into), then
// drives RFC 9002's loss-detection/PTO timer.
pub fn (mut c QuicConn) process_timeouts(now u64) !PollResult {
	mut result := PollResult{}
	if c.state == .closed {
		return result
	}
	if deadline := c.idle_timeout_deadline() {
		if now >= deadline {
			c.state = .closed
			result.events << QuicEvent{
				kind:   .connection_closed
				reason: 'idle timeout'
			}
			return result
		}
	}
	if c.state == .closing || c.state == .draining {
		if deadline := c.closing_deadline {
			if now >= deadline {
				c.state = .closed
				result.events << QuicEvent{
					kind:   .connection_closed
					reason: 'closing/draining period elapsed'
				}
				return result
			}
		}
	}
	c.drain_pending_close(now, mut result)

	if c.state == .handshaking || c.state == .established {
		handshake_confirmed := c.handshake_completion.is_confirmed()
		max_ack_delay := c.effective_max_ack_delay()
		timeout_result := c.loss_detection.on_loss_detection_timeout(now, handshake_confirmed,
			max_ack_delay)
		if timeout_result.pto_fired {
			c.send_pto_probe(timeout_result.pto_space, now, mut result) or {
				code := if err.code() != 0 { u64(err.code()) } else { quic_error_protocol_violation }
				c.close_with_error(code, err.msg(), false, now, mut result)
			}
		} else if timeout_result.lost.len > 0 {
			c.congestion_control.on_packets_lost(timeout_result.lost, false, now)
		}

		if c.state == .handshaking || c.state == .established {
			c.drain_outgoing(now, mut result)!
		}
	}
	result.next_timeout = c.compute_next_timeout()
	return result
}

// -------------------------------------------------------------------------
// Incoming datagram processing
// -------------------------------------------------------------------------

fn (mut c QuicConn) process_datagram(datagram []u8, now u64, mut result PollResult) ! {
	if c.state == .draining {
		return
	}
	packets := split_coalesced_datagram(datagram) or { return }
	for p in packets {
		c.process_one_packet(p, now, mut result)!
	}
	// RFC 9000 §10.2.1: while closing, at most one CONNECTION_CLOSE
	// retransmission per received packet (note_packet_received_while_closing
	// itself enforces the rate limit). Checked AFTER normal processing above
	// (not instead of it) so a peer CONNECTION_CLOSE arriving here still
	// correctly flips c.state to .draining via handle_peer_connection_close
	// first -- in which case this check's own `c.state == .closing` test is
	// already false, correctly skipping the retransmit per §10.2.2's silence
	// requirement.
	if c.state == .closing && c.connection_close.note_packet_received_while_closing() {
		if frame := c.sent_close_payload {
			retransmit := c.build_best_effort_close_packet(frame, now) or { return }
			result.outgoing << retransmit
		}
	}
}

fn (mut c QuicConn) process_one_packet(p CoalescedPacket, now u64, mut result PollResult) ! {
	if p.form == .short {
		c.process_one_rtt_packet(p.bytes, now, mut result)!
		return
	}
	if p.bytes.len < 5 {
		return
	}
	version := (u32(p.bytes[1]) << 24) | (u32(p.bytes[2]) << 16) | (u32(p.bytes[3]) << 8) | u32(p.bytes[4])
	if version == 0 {
		c.process_version_negotiation(p.bytes)!
		return
	}
	header, offset := parse_long_header(p.bytes) or { return }
	match header.typ {
		.retry {
			c.process_retry(p.bytes)!
		}
		.initial {
			c.process_initial_or_handshake(.initial, p.bytes, header, offset, now, mut result)!
		}
		.handshake {
			c.process_initial_or_handshake(.handshake, p.bytes, header, offset, now, mut result)!
		}
		.zero_rtt {
			// v1 never offers 0-RTT and derives no keys to decrypt one --
			// nothing to do with it (RFC 9000 doesn't require a client to
			// react to an unexpected 0-RTT packet in any particular way).
		}
	}
}

fn (mut c QuicConn) process_version_negotiation(raw []u8) ! {
	vn := parse_version_negotiation(raw) or { return }
	handle_version_negotiation(vn, c.original_dcid, c.scid, c.processed_first_server_packet)!
}

fn (mut c QuicConn) process_retry(raw []u8) ! {
	accepted := verify_retry_integrity_tag(c.original_dcid, raw, c.processed_first_server_packet)!
	if !accepted {
		return
	}
	retry := parse_retry_packet(raw, c.original_dcid, c.scid)!

	c.retry_accepted = true
	c.processed_first_server_packet = true
	c.retry_scid = retry.scid.clone()
	c.dcid = retry.scid.clone()
	c.peer_scid = retry.scid.clone()
	c.token = retry.retry_token.clone()

	// RFC 9001 §5.2: Initial secrets change to key off the new DCID. The
	// ClientHello must be resent under the new keys, restarting Initial-space
	// packet numbers and loss-detection tracking cleanly against them.
	new_secrets := derive_initial_secrets(c.dcid)!
	c.initial_keys_client = derive_packet_protection_keys(new_secrets.client)!
	c.initial_keys_server = derive_packet_protection_keys(new_secrets.server)!
	c.pn_spaces.initial = PacketNumberSpaceState{}
	c.loss_detection.initial = LossDetectionSpaceState{}
	c.initial_received_pns = map[u64]bool{}
	c.pending_initial_crypto = c.client_hello
}

fn (mut c QuicConn) process_initial_or_handshake(space QuicPacketNumberSpace, raw []u8, header QuicLongHeader, offset int, now u64, mut result PollResult) ! {
	// RFC 9001 §4.9: once this space's keys are discarded, packets received
	// in it MUST be discarded too -- the key material itself is NOT cleared
	// by discard_initial_keys/discard_handshake_keys (see their own doc
	// comments), so this check is the only thing preventing a stale,
	// replayed, or attacker-injected packet from still being decrypted and
	// processed after the RFC-required discard point.
	if (space == .initial && c.initial_keys_discarded)
		|| (space == .handshake && c.handshake_keys_discarded) {
		return
	}
	// RFC 9000 §7.2: a packet not addressed to this connection's own SCID
	// must be silently dropped. Initial keys are publicly derivable (RFC
	// 9001 §5.2, from the DCID alone), so without this check an off-path
	// attacker who observes the client's chosen DCID could forge a
	// well-encrypted Initial packet with an ARBITRARY destination CID and
	// have it accepted as if it were a legitimate reply, redirecting the
	// client's subsequent packets.
	if header.dcid != c.scid {
		return
	}
	// RFC 9000 §7.2 (verbatim): "Once a client has received a valid Initial
	// packet from the server, it MUST discard any subsequent packet it
	// receives on that connection with a different Source Connection ID."
	// Concretely exploitable for Initial packets specifically: Initial keys
	// are derived from the DCID alone (RFC 9001 §5.2), with no dependency
	// on the real server's identity, so an attacker who observes the
	// client's chosen DCID can forge a second, well-encrypted Initial
	// packet claiming an arbitrary source CID and have its frames (e.g.
	// injected CRYPTO data) processed as if from the already-established
	// peer. Checked here (before this connection's own peer_scid is ever
	// latched below) so it applies uniformly to both spaces, matching the
	// RFC's own unscoped "any subsequent packet... on that connection"
	// wording, even though Handshake-space forgery isn't practically
	// exploitable the same way (those keys derive from the real ECDHE
	// shared secret).
	if c.peer_scid.len != 0 && header.scid != c.peer_scid {
		return
	}
	keys := if space == .initial {
		c.initial_keys_server
	} else {
		c.handshake_keys_server or { return }
	}
	mut packet := raw.clone()
	largest_pn := if space == .initial {
		c.pn_spaces.initial.largest_received
	} else {
		c.pn_spaces.handshake.largest_received
	}
	unprotected := unprotect_packet(mut packet, offset, .long, keys, largest_pn) or { return }

	c.processed_first_server_packet = true
	// RFC 9000 §10.1: receiving and successfully processing ANY packet
	// restarts the idle timer, not just an ack-eliciting one -- see
	// idle_timeout.v's own doc comment.
	c.idle_timeout.note_packet_received(now)
	if c.peer_scid.len == 0 {
		// RFC 9000 §7.2: upon first receiving a packet from the server, the
		// client MUST use its Source Connection ID as the Destination
		// Connection ID for subsequent packets. Done only after a
		// successfully authenticated packet -- never from a spoofed one.
		c.peer_scid = header.scid.clone()
		c.dcid = header.scid.clone()
	}

	if space == .initial {
		c.pn_spaces.initial.note_received(unprotected.packet_number)
		c.initial_received_pns[unprotected.packet_number] = true
	} else {
		c.pn_spaces.handshake.note_received(unprotected.packet_number)
		c.handshake_received_pns[unprotected.packet_number] = true
	}

	frames := parse_frames(unprotected.payload)!
	for frame in frames {
		c.dispatch_pre_confirm_frame(space, frame, now, mut result)!
	}
}

// note_one_rtt_processing_failed is called from EVERY failure point in
// process_one_rtt_packet below (header parse, header-protection removal,
// packet-number decode, key resolution, AEAD decrypt) -- RFC 9000
// §10.3.1's stateless-reset check MUST only run as a last-resort fallback
// AFTER normal processing has already failed, never as a first-choice
// interpretation, and any of these steps failing means normal processing
// genuinely failed. Checking on every failure point (rather than only the
// single "final" one) is harmless: is_stateless_reset is a cheap
// constant-time compare against 16 trailing bytes, and `raw` -- the
// ORIGINAL, unmutated datagram -- is what a real stateless reset's
// trailing bytes would appear in regardless of which step first noticed
// something was wrong.
fn (mut c QuicConn) note_one_rtt_processing_failed(raw []u8, now u64, mut result PollResult) {
	if c.state == .draining {
		return
	}
	if c.stateless_reset.is_stateless_reset(c.peer_scid, raw) {
		c.enter_draining(now)
		result.events << QuicEvent{
			kind:   .connection_closed
			reason: 'stateless reset received'
		}
	}
}

fn (mut c QuicConn) process_one_rtt_packet(raw []u8, now u64, mut result PollResult) ! {
	mut read_keys := c.app_read_keys or {
		c.note_one_rtt_processing_failed(raw, now, mut result)
		return
	}

	dcid_len := c.scid.len
	short_header, offset := parse_short_header(raw, dcid_len) or {
		c.note_one_rtt_processing_failed(raw, now, mut result)
		return
	}
	// RFC 9000 §7.2: a packet not addressed to this connection's own SCID
	// must be silently dropped -- see process_initial_or_handshake's
	// identical check for the full rationale. Routed through
	// note_one_rtt_processing_failed (not a bare return) rather than
	// dropped outright: a genuine stateless reset (RFC 9000 §10.3) is
	// deliberately shaped like an ordinary short-header packet with
	// unpredictable bytes where a real DCID would be, so it will almost
	// always fail this exact check -- the stateless-reset comparison
	// (against the packet's own trailing bytes, independent of anything
	// parsed here) MUST still get a chance to run for a DCID mismatch, not
	// be short-circuited before it ever fires.
	if short_header.dcid != c.scid {
		c.note_one_rtt_processing_failed(raw, now, mut result)
		return
	}
	mut packet := raw.clone()
	// The header-protection key never changes across a key update within
	// one encryption level (RFC 9001 §6, see
	// derive_updated_packet_protection_keys' own doc comment) -- read it
	// once from current_keys regardless of which generation's AEAD keys
	// this packet's key phase bit (unknown until AFTER this call) will
	// turn out to need.
	hp_key := read_keys.current_keys.hp
	pn_length := unprotect_header(mut packet, offset, hp_key, .short) or {
		c.note_one_rtt_processing_failed(raw, now, mut result)
		return
	}
	if offset + pn_length > packet.len {
		c.note_one_rtt_processing_failed(raw, now, mut result)
		return
	}
	mut truncated := u64(0)
	for i in 0 .. pn_length {
		truncated = (truncated << 8) | u64(packet[offset + i])
	}
	full_pn := decode_packet_number(truncated, pn_length,
		c.pn_spaces.application_data.largest_received) or {
		c.note_one_rtt_processing_failed(raw, now, mut result)
		return
	}
	packet_phase := packet[0] & 0x04 != 0
	resolution := read_keys.resolve_read_keys(packet_phase, full_pn) or {
		c.note_one_rtt_processing_failed(raw, now, mut result)
		return
	}
	header := packet[..offset + pn_length].clone()
	// unsafe, not .clone(): mirrors unprotect_packet's own identical slice
	// in packet_protection.v -- read-only pass into decrypt_packet_payload,
	// no mutation of `packet` happens after this point.
	ciphertext := unsafe { packet[offset + pn_length..] }
	payload := decrypt_packet_payload(resolution.keys, full_pn, header, ciphertext) or {
		c.note_one_rtt_processing_failed(raw, now, mut result)
		return
	}
	read_keys.note_successful_decrypt(resolution, full_pn)!

	c.processed_first_server_packet = true
	// RFC 9000 §10.1: receiving and successfully processing ANY packet
	// restarts the idle timer, not just an ack-eliciting one -- see
	// idle_timeout.v's own doc comment.
	c.idle_timeout.note_packet_received(now)
	c.pn_spaces.application_data.note_received(full_pn)
	c.app_received_pns[full_pn] = true

	frames := parse_frames(payload)!
	for frame in frames {
		c.dispatch_one_rtt_frame(frame, now, mut result)!
	}
}

// -------------------------------------------------------------------------
// Frame dispatch
// -------------------------------------------------------------------------

fn (mut c QuicConn) dispatch_pre_confirm_frame(space QuicPacketNumberSpace, frame QuicFrame, now u64, mut result PollResult) ! {
	match frame {
		PaddingFrame {}
		PingFrame {}
		AckFrame {
			c.handle_ack_frame(space, frame, now)
		}
		CryptoFrame {
			c.handle_crypto_frame(space, frame)!
		}
		ConnectionCloseFrame {
			c.handle_peer_connection_close(frame, now, mut result)
		}
		else {
			// STREAM/RESET_STREAM/MAX_DATA/HANDSHAKE_DONE/etc: none of
			// these are legal in the Initial or Handshake packet number
			// space (RFC 9000 §12.4, Table 3) -- a real MUST-violation,
			// not merely something this file doesn't act on yet (contrast
			// with dispatch_one_rtt_frame's own else arm below).
			return error('quic: frame type not legal in ${space} space (RFC 9000 §12.4 PROTOCOL_VIOLATION)')
		}
	}
}

fn (mut c QuicConn) dispatch_one_rtt_frame(frame QuicFrame, now u64, mut result PollResult) ! {
	match frame {
		PaddingFrame {}
		PingFrame {}
		AckFrame {
			c.handle_ack_frame(.application_data, frame, now)
		}
		ConnectionCloseFrame {
			c.handle_peer_connection_close(frame, now, mut result)
		}
		HandshakeDoneFrame {
			// RFC 9000 §19.20: "A HANDSHAKE_DONE frame can only be sent by
			// the server... A server MUST treat receipt of a
			// HANDSHAKE_DONE frame as a connection error of type
			// PROTOCOL_VIOLATION." Currently unreachable in real use (v1
			// only ever constructs clients, role is always .client) but
			// kept so this dispatch code needs no rework when server
			// support (Phase 13) lands -- see
			// test_handshake_done_rejected_when_role_is_server.
			if c.role == .server {
				return error('quic: PROTOCOL_VIOLATION: server received HANDSHAKE_DONE (RFC 9000 §19.20)')
			}
			c.handshake_completion.mark_handshake_done_received()
			if c.handshake_completion.is_confirmed() {
				c.on_handshake_confirmed(mut result)
			}
		}
		CryptoFrame {
			// Post-handshake CRYPTO (NewSessionTicket, post-handshake
			// auth) is out of v1 scope -- Tls13ClientHandshake has no
			// handler for it. Legal on the wire, silently not acted upon.
		}
		StreamFrame {
			c.handle_stream_frame(frame)!
		}
		ResetStreamFrame {
			c.handle_reset_stream_frame(frame)!
		}
		StopSendingFrame {
			c.handle_stop_sending_frame(frame)
		}
		MaxDataFrame {
			c.conn_send_window.raise_limit(frame.maximum_data)
		}
		MaxStreamDataFrame {
			if mut w := c.stream_send_windows[frame.stream_id] {
				w.raise_limit(frame.maximum_stream_data)
			}
		}
		MaxStreamsFrame {
			if frame.direction == .bidirectional {
				if frame.maximum_streams > c.peer_max_streams_bidi {
					c.peer_max_streams_bidi = frame.maximum_streams
				}
			} else {
				if frame.maximum_streams > c.peer_max_streams_uni {
					c.peer_max_streams_uni = frame.maximum_streams
				}
			}
		}
		else {
			// DATA_BLOCKED/STREAM_DATA_BLOCKED/STREAMS_BLOCKED: purely
			// informational hints (RFC 9000 §19.12-§19.14 impose no MUST
			// response) -- legal on the wire, accepted, not acted upon.
		}
	}
}

fn (mut c QuicConn) handle_stream_frame(frame StreamFrame) ! {
	id := StreamId{
		value: frame.stream_id
	}
	max_streams := if id.direction() == .unidirectional {
		c.local_max_streams_uni
	} else {
		c.local_max_streams_bidi
	}
	mut stream := c.streams.get_or_create(frame.stream_id, max_streams)!
	if !stream.has_recv() {
		return error('quic: STREAM_STATE_ERROR: received STREAM frame for send-only stream ${frame.stream_id}')
	}
	c.ensure_stream_windows(id)

	frame_end := frame.offset + u64(frame.data.len)
	mut recv_window := c.stream_recv_windows[frame.stream_id] or {
		panic('unreachable: ensure_stream_windows just created it')
	}
	prev_high := recv_window.received
	recv_window.note_received(frame_end)!
	delta := recv_window.received - prev_high
	if delta > 0 {
		c.conn_recv_window.note_received(c.conn_recv_window.received + delta)!
	}

	stream.recv.note_data(frame.offset, frame.data)!
	if frame.fin {
		stream.recv.note_size_known(frame_end)!
	}
}

fn (mut c QuicConn) handle_reset_stream_frame(frame ResetStreamFrame) ! {
	id := StreamId{
		value: frame.stream_id
	}
	max_streams := if id.direction() == .unidirectional {
		c.local_max_streams_uni
	} else {
		c.local_max_streams_bidi
	}
	mut stream := c.streams.get_or_create(frame.stream_id, max_streams)!
	if !stream.has_recv() {
		return error('quic: STREAM_STATE_ERROR: received RESET_STREAM for send-only stream ${frame.stream_id}')
	}
	c.ensure_stream_windows(id)

	mut recv_window := c.stream_recv_windows[frame.stream_id] or {
		panic('unreachable: ensure_stream_windows just created it')
	}
	prev_high := recv_window.received
	recv_window.note_received(frame.final_size)!
	delta := recv_window.received - prev_high
	if delta > 0 {
		c.conn_recv_window.note_received(c.conn_recv_window.received + delta)!
	}

	stream.recv.mark_reset_recvd(frame.error_code, frame.final_size)!
}

// handle_stop_sending_frame reacts to a peer's STOP_SENDING (RFC 9000
// §19.16) by immediately resetting our own send side with the SAME error
// code, per §3.5's SHOULD -- queued for the next drain_outgoing() call to
// actually send the RESET_STREAM, matching write_stream's own deferred-send
// pattern. Silently ignored for an unknown stream (nothing to stop) or one
// whose send side is already closed (mark_reset_sent is itself a safe
// no-op there, but there is also nothing new to queue).
fn (mut c QuicConn) handle_stop_sending_frame(frame StopSendingFrame) {
	mut stream := c.streams.get(frame.stream_id) or { return }
	if !stream.has_send() {
		return
	}
	if stream.send.state == .data_recvd || stream.send.state == .reset_recvd
		|| stream.send.state == .reset_sent {
		return
	}
	stream.send.mark_reset_sent(frame.error_code)
	c.pending_stream_reset[frame.stream_id] = frame.error_code
}

fn (mut c QuicConn) handle_ack_frame(space QuicPacketNumberSpace, frame AckFrame, now u64) {
	handshake_confirmed := c.handshake_completion.is_confirmed()
	max_ack_delay := c.effective_max_ack_delay()
	result := c.loss_detection.on_ack_received(space, frame, default_ack_delay_exponent,
		max_ack_delay, handshake_confirmed, now)
	c.congestion_control.on_packets_acked(result.newly_acked)
	if result.lost.len > 0 {
		c.congestion_control.on_packets_lost(result.lost, result.persistent_congestion, now)
	}
	if counts := frame.ecn_counts {
		c.ecn.note_ack_ecn_counts(counts)
	}
}

fn (mut c QuicConn) handle_crypto_frame(space QuicPacketNumberSpace, frame CryptoFrame) ! {
	if space == .initial {
		c.initial_crypto.add(frame.offset, frame.data)!
		c.pump_handshake_messages(.initial)!
	} else {
		c.handshake_crypto.add(frame.offset, frame.data)!
		c.pump_handshake_messages(.handshake)!
	}
}

fn (mut c QuicConn) handle_peer_connection_close(frame ConnectionCloseFrame, now u64, mut result PollResult) {
	c.enter_draining(now)
	result.events << QuicEvent{
		kind:       .connection_closed
		error_code: frame.error_code
		reason:     frame.reason
	}
}

// -------------------------------------------------------------------------
// TLS handshake message pump
// -------------------------------------------------------------------------

fn (mut c QuicConn) pump_handshake_messages(space QuicPacketNumberSpace) ! {
	data := if space == .initial { c.initial_crypto.data() } else { c.handshake_crypto.data() }
	mut cursor := if space == .initial {
		c.initial_crypto_consumed
	} else {
		c.handshake_crypto_consumed
	}
	for cursor < u64(data.len) {
		remaining := data[cursor..]
		if remaining.len < 4 {
			break
		}
		length := int((u32(remaining[1]) << 16) | (u32(remaining[2]) << 8) | u32(remaining[3]))
		total := 4 + length
		if remaining.len < total {
			break
		}
		msg, n := parse_handshake_message(remaining)!
		framed := remaining[..n].clone()
		c.dispatch_handshake_message(msg, framed)!
		cursor += u64(n)
	}
	if space == .initial {
		c.initial_crypto_consumed = cursor
	} else {
		c.handshake_crypto_consumed = cursor
	}
}

fn (mut c QuicConn) dispatch_handshake_message(msg HandshakeMessage, framed []u8) ! {
	state := c.handshake.state()
	match state {
		.wait_server_hello {
			hs := c.handshake.process_server_hello(msg, framed)!
			c.handshake_keys_client = derive_packet_protection_keys(hs.client_secret)!
			c.handshake_keys_server = derive_packet_protection_keys(hs.server_secret)!
		}
		.wait_encrypted_extensions {
			c.handshake.process_encrypted_extensions(msg, framed, c.peer_scid, c.original_dcid,
				c.retry_scid)!
			peer_params := c.handshake.peer_transport_parameters
			c.conn_send_window.raise_limit(peer_params.initial_max_data or { u64(0) })
			c.peer_max_streams_bidi = peer_params.initial_max_streams_bidi or { u64(0) }
			c.peer_max_streams_uni = peer_params.initial_max_streams_uni or { u64(0) }
			if token := peer_params.stateless_reset_token {
				c.stateless_reset.record_token(c.peer_scid, token)!
			}
		}
		.wait_certificate {
			c.handshake.process_certificate_or_request(msg, framed)!
		}
		.wait_certificate_verify {
			c.handshake.process_certificate_verify(msg, framed)!
		}
		.wait_finished {
			client_finished, app_secrets := c.handshake.process_finished(msg, framed)!
			c.app_write_keys = derive_packet_protection_keys(app_secrets.client_secret)!
			c.app_read_keys = new_key_update_state(app_secrets.server_secret)!
			c.handshake_completion.mark_peer_finished_verified()
			c.pending_handshake_crypto = client_finished
		}
		.connected {
			return error('quic: unexpected handshake message after the handshake completed')
		}
	}
}

fn (mut c QuicConn) on_handshake_confirmed(mut result PollResult) {
	c.state = .established
	result.events << QuicEvent{
		kind: .handshake_confirmed
	}
	if !c.handshake_keys_discarded {
		c.discard_handshake_keys()
	}
}

// -------------------------------------------------------------------------
// Key discard (RFC 9001 §4.9)
// -------------------------------------------------------------------------

// discard_initial_keys implements RFC 9001 §4.9.1. v1 scope simplification:
// only clears this connection's own per-space bookkeeping; the precise RFC
// 9002 §6.4 interaction with bytes_in_flight/congestion accounting for
// packets that were still outstanding in the discarded space is not
// separately modeled (a documented follow-up, not a Phase 9a blocker --
// affects retransmission-timing precision, not handshake correctness).
fn (mut c QuicConn) discard_initial_keys() {
	c.initial_keys_discarded = true
	c.pn_spaces.initial = PacketNumberSpaceState{}
	c.loss_detection.initial = LossDetectionSpaceState{}
	c.initial_received_pns = map[u64]bool{}
	c.pending_initial_crypto = none
}

// discard_handshake_keys implements RFC 9001 §4.9.2 -- same v1 scope note
// as discard_initial_keys above.
fn (mut c QuicConn) discard_handshake_keys() {
	c.handshake_keys_discarded = true
	c.pn_spaces.handshake = PacketNumberSpaceState{}
	c.loss_detection.handshake = LossDetectionSpaceState{}
	c.handshake_received_pns = map[u64]bool{}
	c.pending_handshake_crypto = none
}

// -------------------------------------------------------------------------
// Outgoing packet construction
// -------------------------------------------------------------------------

fn (mut c QuicConn) drain_outgoing(now u64, mut result PollResult) ! {
	if data := c.pending_initial_crypto {
		crypto_frame := encode_crypto_frame(0, data)!
		datagram := c.build_initial_packet(crypto_frame, true, now)!
		result.outgoing << datagram
		c.pending_initial_crypto = none
	}
	if !c.initial_keys_discarded && c.initial_received_pns.len > 0 {
		ack_frame := c.build_ack_frame_for(.initial)!
		datagram := c.build_initial_packet(ack_frame, false, now)!
		result.outgoing << datagram
		c.initial_received_pns = map[u64]bool{}
	}

	if data := c.pending_handshake_crypto {
		crypto_frame := encode_crypto_frame(c.handshake_crypto_send_offset, data)!
		c.handshake_crypto_send_offset += u64(data.len)
		datagram := c.build_handshake_packet(crypto_frame, true, now)!
		result.outgoing << datagram
		c.pending_handshake_crypto = none
		c.handshake_completion.mark_own_finished_sent()
	}
	if hs_keys := c.handshake_keys_client {
		_ := hs_keys
		if !c.handshake_keys_discarded && c.handshake_received_pns.len > 0 {
			ack_frame := c.build_ack_frame_for(.handshake)!
			datagram := c.build_handshake_packet(ack_frame, false, now)!
			result.outgoing << datagram
			c.handshake_received_pns = map[u64]bool{}
		}
	}

	if _ := c.app_write_keys {
		if c.app_received_pns.len > 0 {
			ack_frame := c.build_ack_frame_for(.application_data)!
			datagram := c.build_one_rtt_packet(ack_frame, false, now)!
			result.outgoing << datagram
			c.app_received_pns = map[u64]bool{}
		}
		c.drain_pending_stream_writes(now, mut result)!
		c.drain_pending_stream_resets(now, mut result)!
		c.drain_pending_streams_blocked(now, mut result)!
		c.drain_flow_control_raises(now, mut result)!
	}
}

// drain_pending_stream_writes sends whatever write_stream() has queued that
// currently fits within BOTH this stream's own send window and the
// connection-level one -- v1 simplification: a queued write that does NOT
// fully fit is left queued whole rather than partially sent (no splitting
// at an arbitrary flow-control boundary), retried on a later drain once
// more budget exists (a MAX_DATA/MAX_STREAM_DATA raise from the peer).
// Documented follow-up, not a Phase 9b blocker -- bandwidth efficiency, not
// correctness.
fn (mut c QuicConn) drain_pending_stream_writes(now u64, mut result PollResult) ! {
	for stream_id, pending in c.pending_stream_write {
		if pending.data.len == 0 && !pending.fin {
			continue
		}
		mut send_window := c.stream_send_windows[stream_id] or { continue }
		needed := u64(pending.data.len)
		if needed > send_window.available() || needed > c.conn_send_window.available() {
			continue
		}
		mut stream := c.streams.get(stream_id) or { continue }
		offset := stream.send.offset
		stream_frame := encode_stream_frame(stream_id, offset, pending.data, pending.fin, true)!
		datagram := c.build_one_rtt_packet(stream_frame, true, now)!
		result.outgoing << datagram
		send_window.consume(needed)!
		c.conn_send_window.consume(needed)!
		stream.send.mark_data_queued()
		stream.send.offset = offset + needed
		if pending.fin {
			stream.send.mark_fin_sent(stream.send.offset)
		}
		c.pending_stream_write.delete(stream_id)
	}
}

// drain_pending_stream_resets sends a RESET_STREAM for every stream
// handle_stop_sending_frame queued (peer-requested) -- an application-
// initiated reset would go through the same queue once a public
// reset_stream() API exists (not yet added; out of Phase 9b's stated scope,
// which names open_stream/write_stream/read_stream/close specifically).
fn (mut c QuicConn) drain_pending_stream_resets(now u64, mut result PollResult) ! {
	for stream_id, error_code in c.pending_stream_reset {
		stream := c.streams.get(stream_id) or { continue }
		reset_frame := encode_reset_stream_frame(stream_id, error_code, stream.send.offset)!
		datagram := c.build_one_rtt_packet(reset_frame, true, now)!
		result.outgoing << datagram
		c.pending_stream_reset.delete(stream_id)
	}
}

// drain_pending_streams_blocked sends the STREAMS_BLOCKED frames
// open_stream() queued when it was rejected by the peer's current limit.
fn (mut c QuicConn) drain_pending_streams_blocked(now u64, mut result PollResult) ! {
	for direction in c.pending_streams_blocked {
		limit := if direction == .bidirectional {
			c.peer_max_streams_bidi
		} else {
			c.peer_max_streams_uni
		}
		blocked_frame := encode_streams_blocked_frame(direction, limit)!
		datagram := c.build_one_rtt_packet(blocked_frame, true, now)!
		result.outgoing << datagram
	}
	c.pending_streams_blocked = []StreamDirection{}
}

// drain_flow_control_raises sends MAX_DATA/MAX_STREAM_DATA once this
// connection's own receive windows (connection-level, then every known
// stream's) cross their auto-tuning threshold -- see ReceiveWindow.
// should_advertise_more's own doc comment for the heuristic.
fn (mut c QuicConn) drain_flow_control_raises(now u64, mut result PollResult) ! {
	if c.conn_recv_window.should_advertise_more() {
		new_limit := c.conn_recv_window.next_advertised_limit()
		max_data_frame := encode_max_data_frame(new_limit)!
		datagram := c.build_one_rtt_packet(max_data_frame, true, now)!
		result.outgoing << datagram
		c.conn_recv_window.mark_advertised(new_limit)
	}
	for stream_id, mut recv_window in c.stream_recv_windows {
		if recv_window.should_advertise_more() {
			new_limit := recv_window.next_advertised_limit()
			msd_frame := encode_max_stream_data_frame(stream_id, new_limit)!
			datagram := c.build_one_rtt_packet(msd_frame, true, now)!
			result.outgoing << datagram
			recv_window.mark_advertised(new_limit)
		}
	}
}

fn (mut c QuicConn) build_ack_frame_for(space QuicPacketNumberSpace) ![]u8 {
	pns := match space {
		.initial { c.initial_received_pns.keys() }
		.handshake { c.handshake_received_pns.keys() }
		.application_data { c.app_received_pns.keys() }
	}
	ranges := ranges_from_received_pns(pns)
	return encode_ack_frame(ranges, 0, none)
}

// ranges_from_received_pns builds RFC 9000 §19.3.1-shaped, largest-first,
// non-overlapping ACK ranges from an unordered set of received packet
// numbers.
fn ranges_from_received_pns(pns []u64) []AckRange {
	if pns.len == 0 {
		return []AckRange{}
	}
	mut sorted := pns.clone()
	sorted.sort()
	mut ranges := []AckRange{}
	mut i := sorted.len - 1
	for i >= 0 {
		largest := sorted[i]
		mut smallest := sorted[i]
		for i > 0 && sorted[i - 1] == smallest - 1 {
			i--
			smallest = sorted[i]
		}
		ranges << AckRange{
			smallest: smallest
			largest:  largest
		}
		i--
	}
	return ranges
}

// build_initial_packet builds one Initial-space packet, always padded to
// RFC 9000 §14.1's 1200-byte floor (pad_initial_payload). Every Initial
// packet this client sends is therefore always "in flight" for RFC 9002
// purposes (ack-eliciting OR contains PADDING), regardless of whether
// `is_ack_eliciting` itself is true.
fn (mut c QuicConn) build_initial_packet(payload []u8, is_ack_eliciting bool, now u64) !QuicDatagram {
	pn := c.pn_spaces.initial.next_packet_number()!
	pn_bytes, pn_length := encode_packet_number(pn, c.pn_spaces.initial.largest_acked_by_peer)!

	h_probe := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    c.dcid
		scid:    c.scid
		token:   c.token
		length:  u64(pn_length) + u64(payload.len) + aead_tag_len
	}
	header_probe := encode_long_header(h_probe, 0, u8(pn_length - 1))!
	padded_payload := pad_initial_payload(payload, header_probe.len + pn_length, aead_tag_len)

	h := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    c.dcid
		scid:    c.scid
		token:   c.token
		length:  u64(pn_length) + u64(padded_payload.len) + aead_tag_len
	}
	mut header := encode_long_header(h, 0, u8(pn_length - 1))!
	header << pn_bytes

	protected :=
		protect_packet(header, .long, pn, pn_length, padded_payload, c.initial_keys_client)!

	c.loss_detection.on_packet_sent(.initial, pn, u64(protected.len), is_ack_eliciting, true, now)
	c.congestion_control.on_packet_sent_cc(u64(protected.len))
	c.idle_timeout.note_packet_sent(now)
	return QuicDatagram{
		bytes: protected
	}
}

fn (mut c QuicConn) build_handshake_packet(payload []u8, is_ack_eliciting bool, now u64) !QuicDatagram {
	keys := c.handshake_keys_client or {
		return error('quic: internal error: no Handshake write keys available yet')
	}

	pn := c.pn_spaces.handshake.next_packet_number()!
	pn_bytes, pn_length := encode_packet_number(pn, c.pn_spaces.handshake.largest_acked_by_peer)!

	h := QuicLongHeader{
		typ:     .handshake
		version: quic_v1
		dcid:    c.dcid
		scid:    c.scid
		token:   []u8{}
		length:  u64(pn_length) + u64(payload.len) + aead_tag_len
	}
	mut header := encode_long_header(h, 0, u8(pn_length - 1))!
	header << pn_bytes

	protected := protect_packet(header, .long, pn, pn_length, payload, keys)!

	in_flight := is_ack_eliciting
	c.loss_detection.on_packet_sent(.handshake, pn, u64(protected.len), is_ack_eliciting,
		in_flight, now)
	if in_flight {
		c.congestion_control.on_packet_sent_cc(u64(protected.len))
	}
	c.idle_timeout.note_packet_sent(now)

	// RFC 9001 §4.9.1: discard Initial keys once the first Handshake-space
	// packet has been sent.
	c.handshake_completion.mark_sent_first_handshake_packet()
	if c.handshake_completion.should_discard_initial_keys() && !c.initial_keys_discarded {
		c.discard_initial_keys()
	}
	return QuicDatagram{
		bytes: protected
	}
}

fn (mut c QuicConn) build_one_rtt_packet(payload []u8, is_ack_eliciting bool, now u64) !QuicDatagram {
	write_keys := c.app_write_keys or {
		return error('quic: internal error: no 1-RTT write keys available yet')
	}

	pn := c.pn_spaces.application_data.next_packet_number()!
	pn_bytes, pn_length := encode_packet_number(pn,
		c.pn_spaces.application_data.largest_acked_by_peer)!

	// key_phase is always false: v1 never client-initiates a key update
	// (key_update.v's own documented scope), so this endpoint's own write
	// phase never advances past the first generation.
	header_prefix := encode_short_header(c.dcid, false, 0, false, u8(pn_length - 1))!
	mut header := header_prefix.clone()
	header << pn_bytes

	protected := protect_packet(header, .short, pn, pn_length, payload, write_keys)!

	in_flight := is_ack_eliciting
	c.loss_detection.on_packet_sent(.application_data, pn, u64(protected.len), is_ack_eliciting,
		in_flight, now)
	if in_flight {
		c.congestion_control.on_packet_sent_cc(u64(protected.len))
	}
	c.idle_timeout.note_packet_sent(now)
	return QuicDatagram{
		bytes: protected
	}
}

// send_pto_probe sends a minimal, always-legal RFC 9002 §6.2 PTO probe
// (a PING frame) in the space the timer identified. v1 scope
// simplification: always probes with PING rather than retransmitting
// specific unacked data -- correct (RFC 9002 explicitly permits this) but
// not the most bandwidth-efficient choice; a real send/retransmission
// buffer is a documented follow-up, not a Phase 9a blocker.
fn (mut c QuicConn) send_pto_probe(space QuicPacketNumberSpace, now u64, mut result PollResult) ! {
	ping := encode_varint(frame_type_ping)!
	match space {
		.initial {
			if c.initial_keys_discarded {
				return
			}
			datagram := c.build_initial_packet(ping, true, now)!
			result.outgoing << datagram
		}
		.handshake {
			if c.handshake_keys_discarded {
				return
			}
			_ := c.handshake_keys_client or { return }
			datagram := c.build_handshake_packet(ping, true, now)!
			result.outgoing << datagram
		}
		.application_data {
			_ := c.app_write_keys or { return }
			datagram := c.build_one_rtt_packet(ping, true, now)!
			result.outgoing << datagram
		}
	}
}

// closing_or_draining_deadline computes the RFC 9000 §10.2 closing/draining
// period end (recommended 3x PTO, the same bound for both states) from
// `now`. Shared by close_with_error (.closing) and enter_draining
// (.draining) so both states are bounded identically -- see enter_draining's
// own doc comment for why a missing deadline here is a real, not
// theoretical, bug. `now` is a `time.sys_mono_now()`-sourced nanosecond
// instant throughout this module (see IdleTimeoutState.last_reset's own
// doc comment, and loss_detection_test.v's own RTT-sample assertions, e.g.
// `ld.rtt.latest_rtt == 1000 * time.nanosecond`) -- `pto_period()` is
// ALREADY a real, nanosecond-scale `time.Duration`, so it must be added
// directly, never routed through `.milliseconds()` first (that would
// shrink it to a tiny millisecond COUNT before adding it to a
// nanosecond-scale `now`, landing the deadline almost immediately instead
// of ~3xPTO out). Found self-inflicted while re-verifying a maintainer
// "Local AI Review" idle-timeout finding on PR #28083 surfaced the exact
// same mistake freshly introduced in idle_timeout_deadline().
fn (c &QuicConn) closing_or_draining_deadline(now u64) u64 {
	return now + 3 * u64(c.loss_detection.rtt.pto_period())
}

// enter_draining transitions to RFC 9000 §10.2.2's draining state -- used
// both when the peer sends CONNECTION_CLOSE (handle_peer_connection_close)
// and when a stateless reset is detected (note_one_rtt_processing_failed).
// Setting closing_deadline here is required, not optional: process_timeouts'
// own draining-timeout check is gated on it being Some, so a connection
// that enters draining without it would sit there forever, never reaching
// .closed -- found via /vreview (this is the MORE common real-world close
// path than our own close_with_error, yet was the one missing the bound).
//
// If closing_deadline is ALREADY set (this connection was already .closing
// -- e.g. it sent its own CONNECTION_CLOSE and is now receiving the peer's
// in reply, a real and reachable path since process_datagram only
// special-cases .draining, not .closing, before dispatching frames), it
// MUST NOT be recomputed from the current `now`: RFC 9000 §10.2.2 "the
// endpoint uses the same end time but ceases transmission of any packets."
// Recomputing here would extend the period every time a further packet
// happened to arrive while closing, rather than preserving the original
// bound. Self-found while auditing this function for the conformance
// matrix's new §10.2 section; test:
// test_closing_deadline_not_extended_by_peer_close_while_already_closing.
fn (mut c QuicConn) enter_draining(now u64) {
	c.connection_close.enter_draining()
	c.state = .draining
	if c.closing_deadline == none {
		c.closing_deadline = c.closing_or_draining_deadline(now)
	}
}

// close_with_error transitions to .closing and sends a best-effort
// CONNECTION_CLOSE using whichever packet protection keys are currently
// available, most-advanced first (RFC 9000 §10.2.3). Never itself returns
// an error -- a failure to even build/send the close packet still leaves
// the connection correctly marked closing and the event reported; the peer
// simply won't receive an explicit notification (it will eventually reach
// the same conclusion via its own idle timeout).
fn (mut c QuicConn) close_with_error(error_code u64, reason string, is_application_error bool, now u64, mut result PollResult) {
	c.connection_close.enter_closing()
	c.state = .closing
	c.closing_deadline = c.closing_or_draining_deadline(now)
	// RFC 9000 §10.2.3: a CONNECTION_CLOSE of application-error type (0x1d)
	// MUST be replaced by the transport-error type (0x1c), with the Reason
	// Phrase field cleared, when sent in an Initial or Handshake packet --
	// those levels aren't backed by the fully-authenticated 1-RTT keys yet,
	// so an application-supplied reason string could leak application state
	// to an observer before the peer is confirmed. build_best_effort_close_packet
	// picks 1-RTT only when app_write_keys is available; every other branch
	// falls back to Handshake or Initial, so that's the exact condition to
	// mirror here, decided BEFORE encoding rather than after (the encoded
	// frame is also what gets replayed verbatim on every §10.2.1 closing-state
	// retransmission, so getting this right once here is sufficient).
	mut wire_is_application_error := is_application_error
	mut wire_reason := reason
	if c.app_write_keys == none {
		wire_is_application_error = false
		wire_reason = ''
	}
	frame := encode_connection_close_frame(wire_is_application_error, error_code, 0, wire_reason) or {
		result.events << QuicEvent{
			kind:       .connection_closed
			error_code: error_code
			reason:     reason
		}
		return
	}
	datagram := c.build_best_effort_close_packet(frame, now) or {
		result.events << QuicEvent{
			kind:       .connection_closed
			error_code: error_code
			reason:     reason
		}
		return
	}
	c.sent_close_payload = frame
	result.outgoing << datagram
	result.events << QuicEvent{
		kind:       .connection_closed
		error_code: error_code
		reason:     reason
	}
}

fn (mut c QuicConn) build_best_effort_close_packet(frame []u8, now u64) !QuicDatagram {
	if _ := c.app_write_keys {
		return c.build_one_rtt_packet(frame, false, now)
	}
	if !c.handshake_keys_discarded {
		if _ := c.handshake_keys_client {
			return c.build_handshake_packet(frame, false, now)
		}
	}
	if !c.initial_keys_discarded {
		return c.build_initial_packet(frame, false, now)
	}
	return error('quic: no packet protection keys available to send CONNECTION_CLOSE')
}

// -------------------------------------------------------------------------
// Timers
// -------------------------------------------------------------------------

fn (c &QuicConn) effective_max_ack_delay() time.Duration {
	if !c.handshake_completion.is_complete() {
		return time.Duration(0)
	}
	v := c.handshake.peer_transport_parameters.max_ack_delay or { default_max_ack_delay_ms }
	return time.Duration(i64(v) * i64(time.millisecond))
}

// idle_timeout_deadline computes the absolute `now`-scale time (a
// `time.sys_mono_now()`-sourced nanosecond instant throughout this module
// -- see IdleTimeoutState.last_reset's own doc comment) at which RFC 9000
// §10.1's idle timeout elapses. `effective_idle_timeout` already returns a
// real, nanosecond-scale `time.Duration` (correctly converted from the
// millisecond WIRE value via `clamped_ms_to_duration`) -- add it to
// `baseline` directly, never through `.milliseconds()` first, which would
// shrink it to a tiny millisecond COUNT before adding it to a
// nanosecond-scale baseline, making the deadline arrive almost immediately
// instead of the real configured timeout. (A prior version of this
// function briefly "fixed" this into the wrong shape while investigating a
// maintainer "Local AI Review" finding on PR #28083 -- the ORIGINAL bare
// `u64(timeout)` was correct all along; the real, adjacent bug was in
// `closing_or_draining_deadline`, which had the identical mistake.)
fn (c &QuicConn) idle_timeout_deadline() ?u64 {
	peer_max := c.handshake.peer_transport_parameters.max_idle_timeout or { u64(0) }
	timeout := effective_idle_timeout(c.own_max_idle_timeout_ms, peer_max) or { return none }
	baseline := c.idle_timeout.last_reset or { c.connection_start }
	return baseline + u64(timeout)
}

fn (mut c QuicConn) compute_next_timeout() ?u64 {
	handshake_confirmed := c.handshake_completion.is_confirmed()
	max_ack_delay := c.effective_max_ack_delay()
	mut deadline := ?u64(none)
	if t, _ := c.loss_detection.next_timeout(handshake_confirmed, max_ack_delay,
		c.congestion_control.bytes_in_flight)
	{
		deadline = t
	}
	if idle_deadline := c.idle_timeout_deadline() {
		if d := deadline {
			if idle_deadline < d {
				deadline = idle_deadline
			}
		} else {
			deadline = idle_deadline
		}
	}
	// closing_deadline (set by close_with_error/enter_draining) is the
	// caller's only signal to call process_timeouts() again once in
	// .closing/.draining -- without merging it in here, a connection with
	// no active idle timeout and nothing in flight would return `none` and
	// the caller would never learn it needs to retire the connection to
	// .closed (RFC 9000 §10.2.2). Found via a maintainer "Local AI Review"
	// on PR #28083 -- see test_compute_next_timeout_includes_closing_deadline.
	if closing_deadline := c.closing_deadline {
		if d := deadline {
			if closing_deadline < d {
				deadline = closing_deadline
			}
		} else {
			deadline = closing_deadline
		}
	}
	return deadline
}
