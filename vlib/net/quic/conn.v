module quic

import time
import crypto.rand

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

// local_cid_len is this endpoint's own chosen connection-ID length -- within
// RFC 9000 §17.2's 20-byte v1 limit, matching common real-world practice
// (quiche and others typically use 8). Shared by both roles: dial() (this
// client's own scid) and accept() (this server's own scid, accept.v) --
// nothing requires the two roles to pick different lengths.
const local_cid_len = 8

// aead_tag_len is AES-128-GCM's fixed authentication tag length (RFC 9001
// §5.3), confirmed against initial_exchange_test.v's own literal use in the
// same padding-size calculation this file's build_initial_packet mirrors.
const aead_tag_len = 16

// long_header_packet_overhead_estimate is a deliberately safe (never an
// underestimate) upper bound on everything build_handshake_packet's own
// encode_long_header + packet-number field + AEAD tag add on top of a raw
// CRYPTO frame's own encoded length: 1 (flags) + 4 (version) + 1 (dcid
// len) + up to 20 (dcid, RFC 9000 v1's own max, though this module only
// ever uses local_cid_len=8 -- the extra headroom is intentional, not an
// oversight) + 1 (scid len) + up to 20 (scid) + up to 4 (Length varint) +
// up to 4 (packet number -- encode_packet_number always chooses the full
// 4-byte encoding when largest_acked_by_peer is none, which it always is
// for THIS connection's very first Handshake-space send) + aead_tag_len.
// Used ONLY to gate a send BEFORE the real size is knowable (drain_outgoing's
// Handshake-CRYPTO-flush amplification check) -- overestimating here means
// occasionally deferring a send that would actually still have fit, never
// the reverse (letting one through that doesn't).
const long_header_packet_overhead_estimate = 1 + 4 + 1 + 20 + 1 + 20 + 4 + 4 + aead_tag_len

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
	peer_stream_opened
}

// QuicEvent reports one thing that happened during a poll()/
// process_timeouts() call. `error_code`/`reason` are set only for
// connection_closed; `stream_id` is set only for peer_stream_opened.
pub struct QuicEvent {
pub:
	kind       QuicEventKind
	error_code ?u64
	reason     string
	stream_id  ?u64
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

	// amplification implements RFC 9000 §8.1's server-side 3x send cap
	// prior to address validation -- meaningful only for role == .server;
	// zero-valued (unvalidated, nothing received/sent yet) for the
	// whole life of a client-role connection, where §8.1 never applies
	// ("the anti-amplification limit does not apply to clients", RFC
	// 9000 §21.1.1.1). process_datagram (below) counts every received
	// datagram's payload toward it for a not-yet-validated server;
	// process_initial_or_handshake's own server receive-trigger calls
	// mark_validated() at the SAME RFC 9001 §4.9.1 event ("receipt of a
	// packet protected with Handshake keys confirms the peer successfully
	// processed an Initial packet", RFC 9000 §8.1) already used for the
	// Initial-key-discard timing fix -- one event, two independent RFC
	// requirements it happens to satisfy simultaneously. drain_outgoing
	// gates every send this connection makes while unvalidated against
	// available_to_send().
	amplification AntiAmplificationLimiter

	// handshake is populated for role == .client (by dial()), server_handshake
	// for role == .server -- see dispatch_handshake_message's role branch
	// and accept()'s own doc comment for why these can't share one field the
	// way most other role-parameterized state on this struct does (their
	// state-machine shapes are fundamentally different, not just their
	// concrete type -- see Tls13ServerHandshake's own doc comment).
	// server_accept_params/server_negotiated_alpn are likewise server-only:
	// server_accept_params is consumed exactly once, by
	// dispatch_handshake_message's "no server_handshake yet" branch, to
	// call Tls13ServerHandshake.respond_to_client_hello with the
	// certificate/key/ALPN material accept() was given; server_negotiated_alpn
	// is set at that same moment from the returned flight, mirroring
	// Tls13ClientHandshake's own negotiated_alpn field for the other role
	// (see negotiated_alpn()'s own doc comment for why this isn't instead a
	// method on Tls13ServerHandshake itself).
	handshake              ?&Tls13ClientHandshake
	server_handshake       ?&Tls13ServerHandshake
	server_accept_params   ?ServerHandshakeParams
	server_negotiated_alpn string
	handshake_completion   &HandshakeCompletionState
	pn_spaces              &QuicPacketNumberSpaces

	initial_keys_client      QuicPacketProtectionKeys
	initial_keys_server      QuicPacketProtectionKeys
	initial_keys_discarded   bool
	handshake_keys_client    ?QuicPacketProtectionKeys
	handshake_keys_server    ?QuicPacketProtectionKeys
	handshake_keys_discarded bool
	app_write_keys           ?QuicPacketProtectionKeys
	app_read_keys            ?&KeyUpdateState

	// app_write_secret/app_write_generation track this endpoint's OWN 1-RTT
	// send-key generation, independently of app_read_keys' read-side
	// generation chain (RFC 9001 §6.1: "each side's traffic secret chain
	// advances on its own schedule" -- key_update.v's own doc comment says
	// the same). RFC 9001 §6.2 requires this side's SEND keys to catch up
	// to whatever generation the PEER has advanced to (see
	// sync_write_keys_to_peer_update) -- the key phase bit sent on the wire
	// is this generation's parity (odd generation -> phase 1), matching RFC
	// 9001 §6's "toggled to signal each subsequent key update".
	app_write_secret     []u8
	app_write_generation int

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

	// *_ack_eliciting_pending track whether ANY frame in the packets
	// currently accumulated in the sibling *_received_pns map above was
	// ack-eliciting (RFC 9000 §13.2.1: all frames except ACK, PADDING, and
	// CONNECTION_CLOSE). drain_outgoing's auto-ACK gates on this, not just
	// on *_received_pns being non-empty -- see frame_is_ack_eliciting's own
	// doc comment for why the two must be tracked separately.
	initial_ack_eliciting_pending   bool
	handshake_ack_eliciting_pending bool
	app_ack_eliciting_pending       bool

	connection_start u64

	// -- Phase 9b: steady-state 1-RTT state ---------------------------------
	own_transport_parameters QuicTransportParameters

	streams              &QuicStreamSet
	stream_send_windows  map[u64]&FlowControlWindow
	stream_recv_windows  map[u64]&ReceiveWindow
	pending_stream_write map[u64]PendingStreamWrite
	pending_stream_reset map[u64]u64 // stream_id -> error_code, queued RESET_STREAM frames
	conn_bytes_read      u64
	// Every peer-initiated stream ID a peer_stream_opened event has already
	// been reported for. Keyed on the specific ID handle_stream_frame/
	// handle_reset_stream_frame were actually called with, NEVER derived
	// from c.streams' own existence map -- get_or_create silently
	// auto-creates every lower-numbered same-category sibling as an empty
	// filler (RFC 9000 §2.1), so an out-of-order arrival (ordinary over
	// UDP) would otherwise cause a real stream's own later STREAM/
	// RESET_STREAM frame to hit the fast already-exists path and never be
	// announced. See note_peer_stream_discovered.
	announced_peer_streams map[u64]bool

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

	// handshake_done_sent guards drain_outgoing's server-role HANDSHAKE_DONE
	// send (RFC 9001 §4.1.2: sent exactly once, "as soon as the handshake
	// is complete") against re-sending on every later poll() call once the
	// handshake stays complete -- the same one-shot-flag shape as
	// streams_blocked_sent_bidi/uni above, RFC 9000 §19.20's decode-side
	// mirror of the send-once requirement enforced there by construction
	// (a server sends it, at most, the one time this flag transitions).
	handshake_done_sent bool
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

// role reports which side of the connection this endpoint is -- .client for
// a dial()ed connection, .server for an accept()ed one (Phase 13d).
pub fn (c &QuicConn) role() QuicRole {
	return c.role
}

// connection_id returns a stable string identity for this connection,
// derived from its own scid -- a package-private field with no other public
// accessor (v1 never issues additional connection IDs beyond it, see this
// file's own "no active CID set" scope note, so a single scid per
// connection is the whole identity space here; nothing to enumerate).
// Exists for an external caller managing MANY connections (e.g. a future
// net.http h3_server.v layering its own per-connection HTTP/3 state on top
// of a QuicListener-demuxed QuicConn, Phase 13e) that needs SOME key to
// index its own side-table by -- QuicListener itself never needs this
// accessor since it lives inside the same module and can read c.scid
// directly (listener.v's own module doc comment explains that exact
// reasoning for why it lives here at all).
pub fn (c &QuicConn) connection_id() string {
	return c.scid.bytestr()
}

// negotiated_alpn reports the ALPN protocol the server selected, or none
// before EncryptedExtensions has been processed (i.e. before
// handshake_confirmed can even fire -- ALPN is settled well before the
// handshake completes, so a caller checking this after handshake_confirmed
// always gets a value, never none). Symmetric across roles: a server knows
// its own selection the moment it makes it (respond_to_client_hello), a
// client only once it decodes the server's EncryptedExtensions.
pub fn (c &QuicConn) negotiated_alpn() ?string {
	if h := c.handshake {
		return h.negotiated_alpn()
	}
	if c.server_negotiated_alpn == '' {
		return none
	}
	return c.server_negotiated_alpn
}

// peer_transport_parameters returns whichever handshake object is active
// for this connection's role's own view of the PEER's transport
// parameters -- Tls13ClientHandshake.peer_transport_parameters (populated
// once EncryptedExtensions is processed) for a client, or
// Tls13ServerHandshake.peer_transport_parameters (populated immediately by
// respond_to_client_hello, straight from the ClientHello) for a server.
// Returns a zero-value QuicTransportParameters{} in the brief window on a
// server connection between accept() constructing it and the ClientHello's
// CRYPTO frame actually being dispatched (dispatch_handshake_message's "no
// server_handshake yet" branch) -- every field this zero value could be
// read through (ensure_stream_windows, ack_delay_exponent, max_ack_delay,
// max_idle_timeout) already treats "peer hasn't told us yet" as "assume
// the conservative default," so this is a safe transient default, not a
// correctness gap; that window closes on the very first poll() call, which
// is exactly what dispatches the ClientHello that was already decrypted at
// accept() time.
fn (c &QuicConn) peer_transport_parameters() QuicTransportParameters {
	if h := c.handshake {
		return h.peer_transport_parameters
	}
	if h := c.server_handshake {
		return h.peer_transport_parameters
	}
	return QuicTransportParameters{}
}

// own_initial_keys/peer_initial_keys and own_handshake_keys/peer_handshake_keys
// resolve which of the two directional key sets (RFC 9001 §5.2: "client"
// and "server" Initial/Handshake secrets are derived from the same shared
// secret but are DIFFERENT keys) this connection's role uses for encrypting
// its OWN outgoing packets vs. decrypting packets received FROM the peer.
// Centralizing the swap here, rather than branching on c.role at each of
// the several encrypt/decrypt call sites individually, is what makes
// build_initial_packet/build_handshake_packet/process_initial_or_handshake
// (all pre-existing, client-only code before Phase 13d) correct for a
// server-role connection with a one-line change at each call site instead
// of a role branch duplicated at every one of them.
fn (c &QuicConn) own_initial_keys() QuicPacketProtectionKeys {
	return if c.role == .client { c.initial_keys_client } else { c.initial_keys_server }
}

fn (c &QuicConn) peer_initial_keys() QuicPacketProtectionKeys {
	return if c.role == .client { c.initial_keys_server } else { c.initial_keys_client }
}

fn (c &QuicConn) own_handshake_keys() ?QuicPacketProtectionKeys {
	return if c.role == .client { c.handshake_keys_client } else { c.handshake_keys_server }
}

fn (c &QuicConn) peer_handshake_keys() ?QuicPacketProtectionKeys {
	return if c.role == .client { c.handshake_keys_server } else { c.handshake_keys_client }
}

// is_handshake_confirmed reports RFC 9001 §4.1.2's "handshake confirmed"
// checkpoint FOR THIS CONNECTION'S ROLE -- the two roles reach it
// differently, per that section's own text: "the TLS handshake is
// considered confirmed at the SERVER when the handshake completes... At
// the CLIENT, the handshake is considered confirmed when a HANDSHAKE_DONE
// frame is received." HandshakeCompletionState.is_confirmed() itself only
// implements the client's half (see that struct's own, deliberately
// client-perspective doc comment, unchanged by Phase 13d); a server's
// confirmation is just is_complete() -- own Finished sent AND peer's
// Finished verified, RFC 9001 §4.1.1 -- with no HANDSHAKE_DONE-received
// dependency, since the server is the one that SENDS it (see
// drain_outgoing's server HANDSHAKE_DONE block).
fn (c &QuicConn) is_handshake_confirmed() bool {
	if c.role == .client {
		return c.handshake_completion.is_confirmed()
	}
	return c.handshake_completion.is_complete()
}

// client_handshake unwraps c.handshake, panicking if it is none. Only
// meaningful on a role == .client connection (dial() always populates it;
// accept() never does -- see the `handshake` field's own doc comment).
// Exists so this package's own white-box tests (conn_test.v/h3_conn_test.v,
// both same-module, reaching into Tls13ClientHandshake's internal fields
// directly to drive/inspect handshake state step by step) don't need an
// `or {}` unwrap repeated at every one of their many call sites -- every
// production code path that needs role-safe access already goes through
// peer_transport_parameters()/negotiated_alpn()/dispatch_handshake_message's
// own role branch instead, none of which call this.
fn (c &QuicConn) client_handshake() &Tls13ClientHandshake {
	return c.handshake or { panic('quic: client_handshake() called on a non-client connection') }
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
		limit := initial_send_limit_for_stream(id, c.role, c.peer_transport_parameters())
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
		handshake_confirmed := c.is_handshake_confirmed()
		max_ack_delay := c.effective_max_ack_delay()
		// RFC 9002 Appendix A.9's OnLossDetectionTimeout is only ever
		// invoked once "the loss detection timer expires" -- i.e. once
		// `now` has actually reached the deadline the SAME loss_detection.
		// next_timeout()/pto_time_and_space() computation (also what
		// compute_next_timeout() below reports to the caller) already
		// names. Calling on_loss_detection_timeout unconditionally on
		// EVERY process_timeouts() call -- which is exactly what a caller-
		// driven poll loop with no native per-connection timer thread does
		// naturally, calling this on a fixed short interval (e.g.
		// h3_driver_poll_interval) whenever nothing else is due -- fired a
		// BRAND NEW PTO probe on every single call as long as anything was
		// unacked, incrementing pto_count (and its 2^pto_count backoff)
		// far faster than real time, without ever actually respecting that
		// backoff: found via 13e's own real end-to-end server test, where
		// a client that stops responding after its own exchange completes
		// left the server's connection with something still unacked,
		// spinning forever building a fresh (slow, AEAD-protected) probe
		// packet on every ~1-50ms poll tick instead of correctly backing
		// off toward the idle timeout. Gated the same way the two
		// `now >= deadline` checks earlier in this SAME function already
		// gate idle-timeout/closing-deadline firing.
		mut loss_timer_due := false
		if deadline, _ := c.loss_detection.next_timeout(handshake_confirmed, max_ack_delay,
			c.congestion_control.bytes_in_flight)
		{
			loss_timer_due = now >= deadline
		}
		if loss_timer_due {
			timeout_result := c.loss_detection.on_loss_detection_timeout(now, handshake_confirmed,
				max_ack_delay)
			if timeout_result.pto_fired {
				c.send_pto_probe(timeout_result.pto_space, now, mut result) or {
					code := if err.code() != 0 {
						u64(err.code())
					} else {
						quic_error_protocol_violation
					}
					c.close_with_error(code, err.msg(), false, now, mut result)
				}
			} else if timeout_result.lost.len > 0 {
				c.congestion_control.on_packets_lost(timeout_result.lost, false, now)
			}
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
	// RFC 9000 §8.1: "servers MUST count all of the payload bytes received
	// in datagrams that are uniquely attributed to a single connection.
	// This includes datagrams that contain packets that are successfully
	// processed and datagrams that contain packets that are all
	// discarded." Counted here, unconditionally, before any parsing.
	// note_received keeps accumulating even after mark_validated() fires
	// (it has no validated-check of its own), but that's harmless --
	// available_to_send() returns max_u64 once validated regardless of
	// the accumulated received/sent counts, so this only actually bounds
	// anything during the brief pre-validation window a server-role
	// connection starts in.
	if c.role == .server {
		c.amplification.note_received(u64(datagram.len))
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
	// Version Negotiation (RFC 9000 §6) and Retry (§17.2.5) are BOTH
	// exclusively server-to-client on the wire -- a compliant peer never
	// sends either to a server. process_retry/process_version_negotiation
	// (below) are written purely from the CLIENT's own perspective (they
	// mutate c.dcid/c.token/Initial keys unconditionally) with no role
	// check of their own; before Phase 13d this dispatch was unreachable
	// for a server-role connection simply because no server-role QuicConn
	// existed to reach it. Now that 13d-2's listener exposes accept()ed
	// connections to real, untrusted network input, an attacker could
	// otherwise spoof either packet type at an in-progress server
	// connection's own scid to corrupt its Initial-space key material and
	// stall the handshake -- silently ignored here instead, matching
	// retry.v's own "discarding is never a connection error" rationale for
	// exactly this class of unauthenticated, pre-crypto packet type. Scoped
	// to ONLY these two branches, not the whole function -- .initial/
	// .handshake below are exactly what a server-role connection MUST keep
	// processing.
	if version == 0 {
		if c.role == .server {
			return
		}
		c.process_version_negotiation(p.bytes)!
		return
	}
	header, offset := parse_long_header(p.bytes) or { return }
	match header.typ {
		.retry {
			if c.role == .server {
				return
			}
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
	// Deliberately NOT c.peer_scid = retry.scid.clone(), even though a
	// Retry's own SCID field genuinely is "a connection ID chosen by the
	// server." RFC 9000 §7.2's peer_scid-latching rule ("once a client has
	// received a valid Initial packet from the server, it MUST discard any
	// subsequent packet... with a different Source Connection ID") is
	// scoped to an actual INITIAL packet, not a Retry -- a DIFFERENT
	// packet type carrying the SERVER's real, PERSISTENT scid (chosen
	// independently of retry_scid, e.g. accept()'s own fresh
	// rand.bytes(local_cid_len) call -- retry_scid is a one-time value
	// used only to address the retried ClientHello, never reused as the
	// connection's real scid). The natural "first real Initial from the
	// server" latch a few lines below in process_initial_or_handshake
	// (`if c.peer_scid.len == 0 { c.peer_scid = header.scid.clone()
	// ... }`) already handles this correctly on its own -- but ONLY if
	// peer_scid is still empty when that ServerHello actually arrives.
	// Setting it here first would make that check a no-op forever,
	// permanently wedging peer_scid on the transient retry value and then
	// silently discarding every subsequent real packet from the server (a
	// different scid) as a false SCID-injection match at line ~862 below.
	// Found via 13d-2's own real dial()-through-Retry-through-accept()
	// integration test (listener_test.v) -- no prior test in this module
	// drove a client through BOTH a real Retry AND a full post-Retry
	// handshake completion, so this pre-existing bug (present since Retry
	// support was first added) was never actually exercised end-to-end
	// until now.
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
	//
	// A server's Initial-space packets are the one exception to "always
	// c.scid": §7.2 also says "Until a packet is received from the
	// server, the client MUST use the same Destination Connection ID
	// value on all packets in this connection" -- i.e. a client's Initial
	// packet sent BEFORE it has processed any reply (the ClientHello
	// itself, or a same-flight retransmission of it) still carries
	// c.original_dcid, not c.scid, since the client cannot address a
	// value (this server's freshly generated scid) it has not learned
	// yet. Only ONCE the client has processed this server's first reply
	// does it switch to c.scid for anything further it sends -- which is
	// why this exception is scoped to space == .initial only; by
	// Handshake space every legitimate client packet already uses c.scid.
	is_servers_bootstrap_packet := c.role == .server && space == .initial
		&& header.dcid == c.original_dcid
	if header.dcid != c.scid && !is_servers_bootstrap_packet {
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
	// Deliberately an if/else STATEMENT with an explicit assignment in each
	// branch, not `keys := if ... { A } else { B }` -- that if-EXPRESSION
	// form, with an `or {}`-block branch value, hits a cgen bug where the
	// emitted `#line` directive glues onto the following line with no
	// newline, so the C preprocessor swallows the expression as trailing
	// directive tokens and gcc fails with "expected expression before '}'
	// token" (same `#line`-emission family as vlang/v#27495, filed
	// upstream as vlang/v#28163). Only reproduces under CI's exact
	// toolchain (gcc-linux/Windows), not local clang -- keep this shape
	// regardless, since the workaround is free and the bug can't be
	// worked around at the call site once it's an if-expression.
	mut keys := QuicPacketProtectionKeys{}
	if space == .initial {
		keys = c.peer_initial_keys()
	} else {
		keys = c.peer_handshake_keys() or { return }
	}
	mut packet := raw.clone()
	largest_pn := if space == .initial {
		c.pn_spaces.initial.largest_received
	} else {
		c.pn_spaces.handshake.largest_received
	}
	unprotected := unprotect_packet(mut packet, offset, .long, keys, largest_pn) or { return }

	// RFC 9001 §4.9.1: the SERVER-side mirror of build_handshake_packet's
	// client-only send-based discard -- a server discards Initial keys once
	// it has successfully processed (here: successfully decrypted) its
	// first Handshake-space packet from the client, independent of anything
	// the server itself has sent. See build_handshake_packet's own doc
	// comment for why the two roles need genuinely different triggers.
	if c.role == .server && space == .handshake && !c.initial_keys_discarded {
		c.discard_initial_keys()
	}
	// RFC 9000 §8.1: "receipt of a packet protected with Handshake keys
	// confirms that the peer successfully processed an Initial packet...
	// [the server] can consider the peer address to have been validated" --
	// the SAME event as the Initial-key-discard trigger just above (this is
	// deliberately not folded into that same `if`: the two conditions are
	// independent RFC requirements that only happen to share a trigger, and
	// mark_validated() is idempotent while discard_initial_keys() is
	// explicitly guarded against re-firing, so keeping them as two
	// statements avoids coupling one's guard condition to the other's).
	if c.role == .server && space == .handshake {
		c.amplification.mark_validated()
	}

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

	frames := parse_frames(unprotected.payload)!
	// RFC 9000 §13.2.1: whether THIS packet is ack-eliciting is determined
	// by its frames, before any of them are dispatched (dispatch can itself
	// close/transition the connection, but the ack-eliciting-ness of the
	// packet that arrived is independent of that).
	mut ack_eliciting := false
	for frame in frames {
		if frame_is_ack_eliciting(frame) {
			ack_eliciting = true
			break
		}
	}

	if space == .initial {
		c.pn_spaces.initial.note_received(unprotected.packet_number)
		c.initial_received_pns[unprotected.packet_number] = true
		c.initial_ack_eliciting_pending = c.initial_ack_eliciting_pending || ack_eliciting
	} else {
		c.pn_spaces.handshake.note_received(unprotected.packet_number)
		c.handshake_received_pns[unprotected.packet_number] = true
		c.handshake_ack_eliciting_pending = c.handshake_ack_eliciting_pending || ack_eliciting
	}

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
	// RFC 9001 §6.2: "Sending keys MUST be updated before sending an
	// acknowledgment for the packet that was received with updated keys."
	// Called here, before this same poll() call's drain_outgoing can send
	// anything (including the auto-ACK for the packet just processed).
	c.sync_write_keys_to_peer_update()!

	c.processed_first_server_packet = true
	// RFC 9000 §10.1: receiving and successfully processing ANY packet
	// restarts the idle timer, not just an ack-eliciting one -- see
	// idle_timeout.v's own doc comment.
	c.idle_timeout.note_packet_received(now)
	c.pn_spaces.application_data.note_received(full_pn)
	c.app_received_pns[full_pn] = true

	frames := parse_frames(payload)!
	// RFC 9000 §13.2.1: see frame_is_ack_eliciting's own doc comment and
	// process_initial_or_handshake's identical block for the full
	// rationale -- this packet must only ARM the auto-ACK if at least one
	// of its frames is ack-eliciting.
	for frame in frames {
		if frame_is_ack_eliciting(frame) {
			c.app_ack_eliciting_pending = true
			break
		}
	}
	for frame in frames {
		c.dispatch_one_rtt_frame(frame, now, mut result)!
	}
}

// sync_write_keys_to_peer_update advances this endpoint's OWN 1-RTT send
// keys to match however many key updates the peer has initiated so far
// (RFC 9001 §6.2): "If a packet is successfully processed using the next
// key and IV, then the peer has initiated a key update. The endpoint MUST
// update its send keys to the corresponding key phase in response... By
// acknowledging the packet that triggered the key update in a packet
// protected with the updated keys, the endpoint signals that the key
// update is complete." The read and write traffic-secret chains advance
// independently (§6.1; key_update.v's own doc comment says the same), so
// this steps app_write_secret/app_write_generation forward one generation
// at a time -- using the identical derive_updated_secret/
// derive_updated_packet_protection_keys primitives KeyUpdateState uses for
// the read side, since both chains use the same derivation, just seeded
// from a different starting secret. A no-op once caught up (the common
// case: every call after the first packet in a phase). This project's own
// scope note ("client-initiated key rotation deferred past v1") is about
// this endpoint choosing to START a new update on its own, which stays
// unimplemented -- RESPONDING to a peer's update is not optional and is
// exactly what this function does.
fn (mut c QuicConn) sync_write_keys_to_peer_update() ! {
	read_keys := c.app_read_keys or { return }
	target_generation := read_keys.generation()
	for c.app_write_generation < target_generation {
		current_write_keys := c.app_write_keys or {
			return error('quic: internal error: no 1-RTT write keys available to advance for key update')
		}

		next_secret := derive_updated_secret(c.app_write_secret)!
		next_keys := derive_updated_packet_protection_keys(next_secret, current_write_keys.hp)!
		c.app_write_secret = next_secret
		c.app_write_keys = next_keys
		c.app_write_generation++
	}
}

// -------------------------------------------------------------------------
// Frame dispatch
// -------------------------------------------------------------------------

// frame_is_ack_eliciting reports whether `frame` is one of the frame types
// RFC 9000 §13.2.1 requires be acknowledged: "all frames other than ACK,
// PADDING, and CONNECTION_CLOSE are ack-eliciting." This is distinct from
// (and NOT interchangeable with) the *_received_pns bookkeeping used to
// build ACK RANGES -- every successfully-processed packet's number belongs
// in an ACK once one is sent, ack-eliciting or not, but whether a packet
// being non-ack-eliciting-only should by ITSELF trigger sending an ACK at
// all is a separate question: §13.2.1 is explicit that "an endpoint MUST
// NOT send a non-ack-eliciting packet in response to a non-ack-eliciting
// packet ... this avoids an infinite feedback loop of acknowledgments."
fn frame_is_ack_eliciting(frame QuicFrame) bool {
	return match frame {
		AckFrame, PaddingFrame, ConnectionCloseFrame { false }
		else { true }
	}
}

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
			// PROTOCOL_VIOLATION." A genuine, reachable defensive check as
			// of Phase 13d: accept() (accept.v) now constructs real
			// server-role connections, and the 1-RTT receive path has no
			// role gate before dispatching frames here, so a malicious or
			// buggy peer sending HANDSHAKE_DONE to a real server hits this
			// branch in production -- not just in
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
			c.handle_stream_frame(frame, mut result)!
		}
		ResetStreamFrame {
			c.handle_reset_stream_frame(frame, mut result)!
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
		NewConnectionIdFrame, RetireConnectionIdFrame {
			// Legal here (RFC 9000 §12.4 Table 3 marks both 1-RTT-only,
			// consistent with dispatch_pre_confirm_frame's own rejection
			// of them in the Initial/Handshake spaces), accepted, but not
			// yet acted upon: driving an ACTIVE SET of usable connection
			// IDs (issuing more as the peer retires them,
			// active_connection_id_limit accounting, the §19.15/§19.16
			// requirements that need that same state -- e.g. §19.16's
			// "sequence number greater than any previously sent" check)
			// is a deliberately deferred state machine, not yet built --
			// see stateless_reset.v's doc comment and PROGRESS.md's Phase
			// 13c scope note. The wire codec these frames now decode
			// through (frame.v) exists so that state machine has
			// something to consume once it lands; until then, a peer
			// sending either is simply ignored, the same as this
			// function's own CryptoFrame arm ignores post-handshake
			// CRYPTO for an unimplemented feature.
		}
		else {
			// DATA_BLOCKED/STREAM_DATA_BLOCKED/STREAMS_BLOCKED: purely
			// informational hints (RFC 9000 §19.12-§19.14 impose no MUST
			// response) -- legal on the wire, accepted, not acted upon.
		}
	}
}

// note_peer_stream_discovered queues a peer_stream_opened event the first
// time this connection ever observes `stream_id` NAMED DIRECTLY by an
// incoming frame -- but only when `stream_id` is genuinely in a
// peer-initiated ID category (StreamId.is_locally_initiated == false).
// This distinction is load-bearing, not defensive: get_or_create's fast
// path (`if existing := s.streams[raw_id] { return existing }`) returns
// early for ANY already-known ID without re-checking who initiated it, so
// a peer's reply on a stream THIS endpoint opened via open_stream() -- an
// entirely ordinary bidi-stream exchange -- reaches this function too;
// without the category check it would be wrongly reported as a
// peer-opened stream every time a first reply arrives on our own stream.
//
// Deliberately NOT keyed on "was `stream_id` newly inserted into
// c.streams" either: get_or_create's RFC 9000 §2.1 sibling-auto-creation
// would otherwise let an out-of-order arrival (ordinary over UDP) silently
// pre-create a lower-numbered peer stream's entry as an empty filler, and
// that stream's own later, explicitly-addressed frame would then hit the
// fast already-exists path and never be announced at all.
//
// Idempotent past the first call for a given ID (a stream can be named by
// many frames over its lifetime).
fn (mut c QuicConn) note_peer_stream_discovered(stream_id u64, mut result PollResult) {
	id := StreamId{
		value: stream_id
	}
	if id.is_locally_initiated(c.role) {
		return
	}
	if stream_id !in c.announced_peer_streams {
		c.announced_peer_streams[stream_id] = true
		result.events << QuicEvent{
			kind:      .peer_stream_opened
			stream_id: stream_id
		}
	}
}

// StreamRecvTerminalState is a simplified view of RecvStreamState (stream.v)
// for callers that only care whether a stream's receive side has reached a
// terminal condition, not the full 6-state machine -- collapses recv into
// open, and both size_known/data_recvd (a FIN has been observed, whether or
// not every byte has actually arrived yet) into fin_received, since for a
// caller checking "did this critical stream close" (RFC 9114 §4.2/§6.2.1),
// the FIN itself is what matters, not full byte-level completion.
pub enum StreamRecvTerminalState {
	open
	fin_received
	reset_received
}

// StreamRecvStatus is stream_recv_status's return shape.
pub struct StreamRecvStatus {
pub:
	state       StreamRecvTerminalState
	reset_error ?u64 // set only when state == .reset_received
}

// stream_recv_status reports `stream_id`'s current receive-side terminal
// status, or none if the ID is unknown to this connection (never seen in
// any frame, and never locally opened) or has no receive side at all (a
// locally-initiated unidirectional stream). reset_recvd/reset_read always
// win over size_known/data_recvd if both have occurred (RFC 9000 §3.2
// permits a RESET_STREAM after all data was already received, and
// mark_reset_recvd applies unconditionally in that case -- see its own doc
// comment), so checking reset first below matches the underlying state
// machine's own precedence, not just this function's own guess at it.
pub fn (c &QuicConn) stream_recv_status(stream_id u64) ?StreamRecvStatus {
	stream := c.streams.get(stream_id) or { return none }
	if !stream.has_recv() {
		return none
	}
	match stream.recv.state {
		.reset_recvd, .reset_read {
			return StreamRecvStatus{
				state:       .reset_received
				reset_error: stream.recv.error_code
			}
		}
		.size_known, .data_recvd, .data_read {
			return StreamRecvStatus{
				state: .fin_received
			}
		}
		.recv {
			return StreamRecvStatus{
				state: .open
			}
		}
	}
}

fn (mut c QuicConn) handle_stream_frame(frame StreamFrame, mut result PollResult) ! {
	id := StreamId{
		value: frame.stream_id
	}
	max_streams := if id.direction() == .unidirectional {
		c.local_max_streams_uni
	} else {
		c.local_max_streams_bidi
	}
	mut stream := c.streams.get_or_create(frame.stream_id, max_streams)!
	c.note_peer_stream_discovered(frame.stream_id, mut result)
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

fn (mut c QuicConn) handle_reset_stream_frame(frame ResetStreamFrame, mut result PollResult) ! {
	id := StreamId{
		value: frame.stream_id
	}
	max_streams := if id.direction() == .unidirectional {
		c.local_max_streams_uni
	} else {
		c.local_max_streams_bidi
	}
	mut stream := c.streams.get_or_create(frame.stream_id, max_streams)!
	c.note_peer_stream_discovered(frame.stream_id, mut result)
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
	handshake_confirmed := c.is_handshake_confirmed()
	max_ack_delay := c.effective_max_ack_delay()
	// on_ack_received's own doc comment: "ack_delay_exponent is the PEER's
	// own ack_delay_exponent transport parameter" -- this ACK frame's raw
	// ack_delay field was encoded by the peer using ITS advertised value
	// (RFC 9000 §18.2), not ours, and not the RFC's own default-if-absent
	// value unconditionally.
	peer_ack_delay_exponent := c.peer_transport_parameters().ack_delay_exponent or {
		default_ack_delay_exponent
	}

	result := c.loss_detection.on_ack_received(space, frame, peer_ack_delay_exponent,
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
	if c.role == .server {
		c.dispatch_server_handshake_message(msg, framed)!
		return
	}
	mut handshake := c.handshake or {
		return error('quic: internal error: client connection has no handshake state')
	}

	state := handshake.state()
	match state {
		.wait_server_hello {
			hs := handshake.process_server_hello(msg, framed)!
			c.handshake_keys_client = derive_packet_protection_keys(hs.client_secret)!
			c.handshake_keys_server = derive_packet_protection_keys(hs.server_secret)!
		}
		.wait_encrypted_extensions {
			handshake.process_encrypted_extensions(msg, framed, c.peer_scid, c.original_dcid,
				c.retry_scid)!
			peer_params := handshake.peer_transport_parameters
			c.conn_send_window.raise_limit(peer_params.initial_max_data or { u64(0) })
			c.peer_max_streams_bidi = peer_params.initial_max_streams_bidi or { u64(0) }
			c.peer_max_streams_uni = peer_params.initial_max_streams_uni or { u64(0) }
			if token := peer_params.stateless_reset_token {
				c.stateless_reset.record_token(c.peer_scid, token)!
			}
		}
		.wait_certificate {
			handshake.process_certificate_or_request(msg, framed)!
		}
		.wait_certificate_verify {
			handshake.process_certificate_verify(msg, framed)!
		}
		.wait_finished {
			client_finished, app_secrets := handshake.process_finished(msg, framed)!
			c.app_write_keys = derive_packet_protection_keys(app_secrets.client_secret)!
			c.app_write_secret = app_secrets.client_secret.clone()
			c.app_write_generation = 0
			c.app_read_keys = new_key_update_state(app_secrets.server_secret)!
			c.handshake_completion.mark_peer_finished_verified()
			c.pending_handshake_crypto = client_finished
		}
		.connected {
			return error('quic: unexpected handshake message after the handshake completed')
		}
	}
}

// dispatch_server_handshake_message is dispatch_handshake_message's
// server-role counterpart. Unlike the client's 6-state dispatch above, a
// server's FIRST handshake message (the ClientHello) is never routed
// through a state-machine branch here at all -- Tls13ServerHandshake has
// no wait_client_hello state, because respond_to_client_hello IS the act
// of constructing the handshake object, not a transition on an existing
// one (see that struct's own doc comment). So this function's real branch
// is "do we have a server_handshake object yet": no means `msg` must be
// the ClientHello and this is accept()'s deferred bootstrap step (accept()
// itself only decrypts the Initial packet and constructs the connection;
// it stashes server_accept_params and lets this function -- reached via
// the ordinary process_initial_or_handshake -> handle_crypto_frame ->
// pump_handshake_messages call chain every OTHER handshake message already
// goes through -- do the actual respond_to_client_hello call, so accept()
// doesn't need its own parallel decrypt/frame-dispatch path); yes means
// `msg` is the client's Finished, the server's only OTHER expected message.
fn (mut c QuicConn) dispatch_server_handshake_message(msg HandshakeMessage, framed []u8) ! {
	mut sh := c.server_handshake or {
		params := c.server_accept_params or {
			return error('quic: internal error: server connection has no accept parameters to process a ClientHello with')
		}

		hs, flight := Tls13ServerHandshake.respond_to_client_hello(msg, framed, params)!
		c.server_handshake = hs
		c.server_accept_params = none
		c.handshake_keys_client =
			derive_packet_protection_keys(flight.handshake_secrets.client_secret)!
		c.handshake_keys_server =
			derive_packet_protection_keys(flight.handshake_secrets.server_secret)!
		// RFC 8446 §7.1/Figure 3: a server derives application traffic
		// secrets right after its own Finished, with no dependency on the
		// client's Finished -- unlike the client's .wait_finished arm
		// above, these are ready here, not deferred to process_finished.
		c.app_write_keys = derive_packet_protection_keys(flight.application_secrets.server_secret)!
		c.app_write_secret = flight.application_secrets.server_secret.clone()
		c.app_write_generation = 0
		c.app_read_keys = new_key_update_state(flight.application_secrets.client_secret)!
		c.server_negotiated_alpn = flight.negotiated_alpn
		c.pending_initial_crypto = flight.server_hello
		c.pending_handshake_crypto = flight.handshake_messages
		peer_params := hs.peer_transport_parameters
		c.conn_send_window.raise_limit(peer_params.initial_max_data or { u64(0) })
		c.peer_max_streams_bidi = peer_params.initial_max_streams_bidi or { u64(0) }
		c.peer_max_streams_uni = peer_params.initial_max_streams_uni or { u64(0) }
		// A ClientHello's transport parameters have no stateless_reset_token
		// field (RFC 9000 §18.2 -- only a SERVER sends that one), unlike the
		// client's .wait_encrypted_extensions arm's identical-looking block
		// above; nothing to record here.
		return
	}

	state := sh.state()
	match state {
		.wait_finished {
			sh.process_finished(msg, framed)!
			c.handshake_completion.mark_peer_finished_verified()
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
	c.initial_ack_eliciting_pending = false
	c.pending_initial_crypto = none
}

// discard_handshake_keys implements RFC 9001 §4.9.2 -- same v1 scope note
// as discard_initial_keys above.
fn (mut c QuicConn) discard_handshake_keys() {
	c.handshake_keys_discarded = true
	c.pn_spaces.handshake = PacketNumberSpaceState{}
	c.loss_detection.handshake = LossDetectionSpaceState{}
	c.handshake_received_pns = map[u64]bool{}
	c.handshake_ack_eliciting_pending = false
	c.pending_handshake_crypto = none
}

// -------------------------------------------------------------------------
// Outgoing packet construction
// -------------------------------------------------------------------------

// has_amplification_budget reports whether this connection may still send
// right now under RFC 9000 §8.1's server-side 3x cap -- always true for a
// client role (the limit is server-only, RFC 9000 §21.1.1.1) or once
// mark_validated() has fired. A coarse, packet-granularity gate: checked
// BEFORE building the next datagram (not against that datagram's actual
// size, which isn't known until AFTER building -- protect_packet's AEAD tag
// and padding aren't computed yet), so a connection sitting at exactly 1
// byte of remaining budget can still emit one more full packet before this
// next reports false. This mirrors how every other RFC 9000 §8.1
// implementation reasons about the limit (it bounds AMPLIFICATION
// MAGNITUDE across a connection's lifetime, not a byte-exact ceiling on any
// single packet) and, more concretely, HAS to work this way here: the
// alternative -- building the packet first, THEN deciding whether to keep
// it -- would leave loss_detection/congestion_control's own
// on_packet_sent() bookkeeping (already called unconditionally inside
// build_initial_packet/build_handshake_packet by the time a caller could
// inspect the built size) believing a packet was transmitted that was then
// silently discarded, desyncing retransmission timers from what actually
// went on the wire -- exactly the failure mode already documented on the
// Initial-key-discard-timing fix earlier in this file.
fn (c &QuicConn) has_amplification_budget() bool {
	return c.role != .server || c.amplification.available_to_send() > 0
}

// record_amplification_sent updates the §8.1 send tally after a datagram
// has ALREADY been built and queued -- a no-op for a client role. Never
// itself vetoes the send (that already happened; see
// has_amplification_budget's own doc comment for why the check must come
// BEFORE building, not after). Uses note_sent_unconditional, not note_sent
// -- this call site's whole reason for existing is the coarse-grained
// overshoot has_amplification_budget's own doc comment describes (a send
// whose exact size wasn't knowable until after building can end up
// slightly over budget even though the pre-build check passed); recording
// anything less than the ACTUAL bytes sent here (note_sent's own
// behavior, which silently drops the update on its error path) would
// leave available_to_send() reporting stale, too-generous budget for
// every later send this same drain_outgoing call -- or a future one --
// still checks against, turning one small overshoot into an unbounded,
// compounding one (13d-2's own adversarial review, verify pass).
fn (mut c QuicConn) record_amplification_sent(n int) {
	if c.role == .server {
		c.amplification.note_sent_unconditional(u64(n))
	}
}

fn (mut c QuicConn) drain_outgoing(now u64, mut result PollResult) ! {
	if data := c.pending_initial_crypto {
		if c.has_amplification_budget() {
			crypto_frame := encode_crypto_frame(0, data)!
			datagram := c.build_initial_packet(crypto_frame, true, now)!
			result.outgoing << datagram
			c.record_amplification_sent(datagram.bytes.len)
			c.pending_initial_crypto = none
		}
	}
	// RFC 9000 §13.2.1: gated on *_ack_eliciting_pending, not just
	// *_received_pns.len > 0 -- see frame_is_ack_eliciting's own doc
	// comment. A packet whose only frames were ACK/PADDING/CONNECTION_CLOSE
	// must not itself trigger sending this ACK-only (non-ack-eliciting)
	// reply; its packet number stays in *_received_pns to be included
	// once an ack-eliciting packet DOES trigger one.
	if !c.initial_keys_discarded && c.initial_received_pns.len > 0
		&& c.initial_ack_eliciting_pending && c.has_amplification_budget() {
		ack_frame := c.build_ack_frame_for(.initial)!
		datagram := c.build_initial_packet(ack_frame, false, now)!
		result.outgoing << datagram
		c.record_amplification_sent(datagram.bytes.len)
		c.initial_received_pns = map[u64]bool{}
		c.initial_ack_eliciting_pending = false
	}

	if data := c.pending_handshake_crypto {
		// This flush -- the server's EncryptedExtensions+Certificate+
		// CertificateVerify+Finished, sent as ONE unsplit CRYPTO frame in
		// ONE Handshake packet (accept.v's own documented scope limit:
		// this codebase does not yet fragment a large certificate chain's
		// CRYPTO write across multiple packets) -- is the one send in
		// this function whose size can genuinely dwarf a small client's
		// starting amplification budget (3x of however many bytes it has
		// sent so far, which could be as little as the RFC 9000 §14.1
		// floor of 1200). The coarse has_amplification_budget() check
		// used everywhere else in this function ("is there ANY budget at
		// all") isn't precise enough here -- a single build could still
		// blow through the whole budget in one shot. Checked instead
		// against the ALREADY-ENCODED frame's own length (encode_crypto_frame
		// has no loss-detection/congestion-control side effects of its
		// own, unlike build_handshake_packet -- computing it first to
		// learn the real size before deciding whether to build is safe),
		// plus long_header_packet_overhead_estimate's own safe (never an
		// underestimate) margin for the packet header/PN/AEAD-tag overhead
		// this raw frame length doesn't yet include -- an earlier version
		// of this check used a flat +32 margin that turned out to
		// undercount the real ~45-byte overhead by up to 13 bytes (13d-2's
		// own adversarial review, verify pass), letting a send slip
		// through slightly over budget; a named, derived-from-real-field-
		// widths constant closes that gap for good, rather than
		// re-guessing a number here. If it doesn't fit, the flush stays
		// queued for a LATER drain once more
		// budget exists (RFC 9000 §8.1's own documented resolution for
		// exactly this shape of deadlock: the client's own PTO-triggered
		// Initial retransmissions keep raising `received` -- and
		// therefore this budget -- every round trip until the flight
		// finally fits).
		crypto_frame := encode_crypto_frame(c.handshake_crypto_send_offset, data)!
		estimated_packet_size := u64(crypto_frame.len) + u64(long_header_packet_overhead_estimate)
		if c.role != .server || c.amplification.available_to_send() >= estimated_packet_size {
			c.handshake_crypto_send_offset += u64(data.len)
			datagram := c.build_handshake_packet(crypto_frame, true, now)!
			result.outgoing << datagram
			c.record_amplification_sent(datagram.bytes.len)
			c.pending_handshake_crypto = none
			c.handshake_completion.mark_own_finished_sent()
		}
	}
	if hs_keys := c.own_handshake_keys() {
		_ := hs_keys
		if !c.handshake_keys_discarded && c.handshake_received_pns.len > 0
			&& c.handshake_ack_eliciting_pending && c.has_amplification_budget() {
			ack_frame := c.build_ack_frame_for(.handshake)!
			datagram := c.build_handshake_packet(ack_frame, false, now)!
			result.outgoing << datagram
			c.record_amplification_sent(datagram.bytes.len)
			c.handshake_received_pns = map[u64]bool{}
			c.handshake_ack_eliciting_pending = false
		}
	}

	if _ := c.app_write_keys {
		// RFC 9001 §4.1.2: "The server MUST send a HANDSHAKE_DONE frame as
		// soon as the handshake is complete" -- is_complete() rather than
		// is_handshake_confirmed() deliberately: for a server, confirmed IS
		// complete (see is_handshake_confirmed's own doc comment), so
		// checking complete() here and calling on_handshake_confirmed
		// immediately after sending is the correct, non-circular ordering
		// (confirmation depends on this send, not the other way around).
		// handshake_done_sent guards this to fire at most once, the same
		// one-shot shape as streams_blocked_sent_bidi/uni.
		if c.role == .server && c.handshake_completion.is_complete() && !c.handshake_done_sent
			&& c.has_amplification_budget() {
			frame := encode_handshake_done_frame()!
			datagram := c.build_one_rtt_packet(frame, true, now)!
			result.outgoing << datagram
			c.record_amplification_sent(datagram.bytes.len)
			c.handshake_done_sent = true
			c.on_handshake_confirmed(mut result)
		}
		if c.app_received_pns.len > 0 && c.app_ack_eliciting_pending && c.has_amplification_budget() {
			ack_frame := c.build_ack_frame_for(.application_data)!
			datagram := c.build_one_rtt_packet(ack_frame, false, now)!
			result.outgoing << datagram
			c.record_amplification_sent(datagram.bytes.len)
			c.app_received_pns = map[u64]bool{}
			c.app_ack_eliciting_pending = false
		}
		if c.has_amplification_budget() {
			c.drain_pending_stream_writes(now, mut result)!
			c.drain_pending_stream_resets(now, mut result)!
			c.drain_pending_streams_blocked(now, mut result)!
			// Not reachable pre-validation with today's state machine (a
			// server's own receive-window growth needs 1-RTT keys, which
			// this connection only reaches after processing the client's
			// Handshake-space Finished -- the SAME event that already
			// fires mark_validated()) -- gated anyway, for the same
			// reason every OTHER send in this block is: consistency with
			// this function's own stated invariant ("every send this
			// connection makes while unvalidated is gated"), and so a
			// future codepath that lets app-space windows advance before
			// validation (0-RTT, say) doesn't silently reintroduce this
			// gap (13d-2's own adversarial review, verify pass).
			c.drain_flow_control_raises(now, mut result)!
		}
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
	// Deliberately `c.stream_recv_windows.keys()` + a per-key lookup, NOT
	// `for stream_id, mut recv_window in c.stream_recv_windows` -- the
	// established, proven-safe idiom this codebase already uses for a
	// map[K]&V (matches listener.v's own documented fix for the identical
	// shape, and this file's own other access sites for this exact map:
	// process_max_stream_data_frame et al. already use `c.stream_recv_
	// windows[id] or {...}`, never a for-mut-in-map loop).
	for stream_id in c.stream_recv_windows.keys() {
		mut recv_window := c.stream_recv_windows[stream_id] or { continue }
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

	protected := protect_packet(header, .long, pn, pn_length, padded_payload, c.own_initial_keys())!

	c.loss_detection.on_packet_sent(.initial, pn, u64(protected.len), is_ack_eliciting, true, now)
	c.congestion_control.on_packet_sent_cc(u64(protected.len))
	c.idle_timeout.note_packet_sent(now)
	return QuicDatagram{
		bytes: protected
	}
}

fn (mut c QuicConn) build_handshake_packet(payload []u8, is_ack_eliciting bool, now u64) !QuicDatagram {
	keys := c.own_handshake_keys() or {
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

	// RFC 9001 §4.9.1: "a client MUST discard Initial keys when it first
	// sends a Handshake packet, and a server MUST discard Initial keys when
	// it first successfully processes a Handshake packet" -- two DIFFERENT
	// triggers per role, send vs. receive. Only the client's is send-based,
	// so this is scoped to c.role == .client; the server's receive-based
	// trigger lives in process_initial_or_handshake instead (right after a
	// Handshake-space packet from the client successfully decrypts).
	// Applying this send-based check to both roles (the diff's original
	// shape) made a server discard its Initial keys within the very same
	// accept()/poll() call that processed the ClientHello -- before the
	// client had sent anything back -- which then silently dropped any
	// ClientHello retransmission (RFC 9000 §7.2's own DCID exception is
	// keyed off c.original_dcid, but process_initial_or_handshake's very
	// first check already rejects the whole Initial space once
	// initial_keys_discarded is true) and left the server unable to
	// PTO-retransmit its own first flight, stalling the handshake on
	// ordinary first-round-trip packet loss. Found by 13d-1's adversarial
	// review (protocol lens); accept_test.v's own lossless, zero-loss
	// integration test can't exercise this since nothing is ever lost.
	if c.role == .client {
		c.handshake_completion.mark_sent_first_handshake_packet()
		if c.handshake_completion.should_discard_initial_keys() && !c.initial_keys_discarded {
			c.discard_initial_keys()
		}
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

	// key_phase reflects app_write_generation's parity -- generation 0 is
	// phase false (RFC 9000 §17.3.1's own initial value), toggling each
	// time sync_write_keys_to_peer_update advances a generation (RFC 9001
	// §6.2). This endpoint never INITIATES its own key update (v1 scope,
	// key_update.v's own doc comment) but MUST follow a peer-initiated one,
	// which is exactly what app_write_generation tracks.
	key_phase := c.app_write_generation % 2 == 1
	header_prefix := encode_short_header(c.dcid, false, 0, key_phase, u8(pn_length - 1))!
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
//
// Every arm is gated on has_amplification_budget()/record_amplification_
// sent() -- the SAME RFC 9000 §8.1 3x-received budget drain_outgoing's own
// sends are gated on (a no-op check/record for a client role; see both
// functions' own doc comments). Missed in an earlier version: a server
// accepted without Retry (always_retry: false) has an UNVALIDATED address
// the moment it accepts, exactly like drain_outgoing's own sends at that
// point -- an unbounded PTO backoff loop (this function's own caller,
// on_loss_detection_timeout, fires again and again with 2^pto_count
// spacing whenever anything stays unacked) could keep appending probe
// datagrams past the 3x-received cap forever, turning a spoofed Initial
// into a sustained amplification source. If the budget is exhausted, the
// probe is simply skipped this round -- safe, since on_loss_detection_
// timeout (this function's only caller) has already advanced pto_count/
// the backoff timer BEFORE calling this, independent of whether a
// datagram actually goes out; a future incoming datagram raising the
// budget lets the next-armed PTO actually send. (Codex review, PR #28164
// pullrequestreview-5044139767.)
fn (mut c QuicConn) send_pto_probe(space QuicPacketNumberSpace, now u64, mut result PollResult) ! {
	if !c.has_amplification_budget() {
		return
	}
	ping := encode_varint(frame_type_ping)!
	match space {
		.initial {
			if c.initial_keys_discarded {
				return
			}
			datagram := c.build_initial_packet(ping, true, now)!
			result.outgoing << datagram
			c.record_amplification_sent(datagram.bytes.len)
		}
		.handshake {
			if c.handshake_keys_discarded {
				return
			}
			_ := c.own_handshake_keys() or { return }
			datagram := c.build_handshake_packet(ping, true, now)!
			result.outgoing << datagram
			c.record_amplification_sent(datagram.bytes.len)
		}
		.application_data {
			_ := c.app_write_keys or { return }
			datagram := c.build_one_rtt_packet(ping, true, now)!
			result.outgoing << datagram
			c.record_amplification_sent(datagram.bytes.len)
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
		if _ := c.own_handshake_keys() {
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
	v := c.peer_transport_parameters().max_ack_delay or { default_max_ack_delay_ms }
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
	peer_max := c.peer_transport_parameters().max_idle_timeout or { u64(0) }
	timeout := effective_idle_timeout(c.own_max_idle_timeout_ms, peer_max) or { return none }
	baseline := c.idle_timeout.last_reset or { c.connection_start }
	return baseline + u64(timeout)
}

fn (mut c QuicConn) compute_next_timeout() ?u64 {
	handshake_confirmed := c.is_handshake_confirmed()
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
