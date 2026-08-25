module quic

import crypto.ecdsa
import crypto.hmac
import crypto.sha256

// listener.v: Phase 13d-2, the UDP listener/connection-demux layer -- one
// caller-driven `QuicListener` routing many concurrent server-role
// `QuicConn`s (13d-1) out of a single incoming datagram stream, deciding
// Retry-vs-direct-accept for each new connection attempt by wiring 13b's
// AntiAmplificationLimiter/Retry machinery together (accept()'s own doc
// comment names this exact gap as "13d-2's job").
//
// Deliberately transport-agnostic, matching every other file in this
// module: `QuicListener` never touches a `net.UdpConn` or `net.Addr`
// itself -- it takes/returns raw `[]u8` datagram bytes and a caller-
// serialized opaque `peer []u8` address identifier, the SAME convention
// retry_token.v's own `client_addr []u8` already established. A caller
// (e.g. a future net.http `h3_server.v`, Phase 13e) owns the real socket
// and bridges `net.Addr <-> []u8`, mirroring how `h3_mux_conn.v` already
// bridges `net.UdpConn` to the client-side `QuicConn`/`H3Conn`'s own
// `poll()`/`process_timeouts()` surface -- single-threaded, caller-driven,
// no background thread or socket ownership inside this module (PROGRESS.md's
// own stated Phase 13 scope decision).
//
// Lives inside module `quic` (like accept.v/retry.v) rather than
// net.http, specifically so it can key its demux table off `QuicConn.scid`
// directly -- a package-private field with no public accessor (v1
// deliberately never issues additional connection IDs beyond that one,
// see conn.v's own "no active CID set" scope note, so a single `scid` per
// connection is the whole demux key space; nothing to enumerate).

// QuicListenerParams configures a QuicListener for its whole lifetime --
// the server-wide identity/policy inputs every accepted connection shares,
// analogous to AcceptParams but for the listener that constructs many of
// them.
pub struct QuicListenerParams {
pub:
	transport_parameters QuicTransportParameters
	alpn_protocols       []string
	certificate_chain    []CertificateEntry
	signing_key          ecdsa.PrivateKey
	// This server instance's own long-lived secret for BOTH Retry-token
	// AEAD sealing (retry_token.v's own `key`) and this file's deterministic
	// Retry-SCID derivation (see derive_retry_scid) -- exactly
	// retry_token_key_len (16) bytes, generated once by the caller (e.g.
	// via crypto.rand.bytes(retry_token_key_len)) and kept only on the
	// server, matching retry_token.v's own documented key-lifecycle
	// contract. Reusing one secret for two HMAC/AEAD-keyed derivations
	// under distinct domain-separation labels is intentional, not
	// accidental key reuse -- new_quic_listener validates its length but
	// does not generate or persist it itself, the same "caller owns key
	// lifecycle" choice retry_token.v's own doc comment states.
	retry_token_key []u8
	// Whether EVERY new connection attempt from an address with no valid
	// token yet must complete a Retry round-trip before accept() is ever
	// called (RFC 9000 §21.1.1.2's recommended default posture: Retry is
	// "a cheap token exchange mechanism that allows servers to validate a
	// client's IP address prior to doing any expensive computations").
	// `false` calls accept() directly for every first-seen address instead
	// -- cheaper for a trusted/low-abuse deployment, but leaves this
	// server doing full ECDHE+signature work for spoofed-source traffic.
	always_retry bool = true
	// RFC 9000 §8.1.4: "servers SHOULD ensure that tokens sent in Retry
	// packets are only accepted for a short time, as they are returned
	// immediately by clients." This module has no consumed-token cache
	// (single-use replay tracking) -- see validate_retry_token_for_attempt's
	// own doc comment for why that's a deliberate, documented scope limit,
	// not an oversight -- so this short expiry window is the ENTIRE replay
	// mitigation for a captured/replayed token; keep it short. 30 seconds
	// comfortably covers real network RTTs while still bounding a replay
	// window tightly.
	retry_token_max_age_ms u64 = 30000
}

// QuicListenerDatagram is one outgoing datagram a QuicListener wants sent,
// paired with the caller-serialized peer address to send it to -- the
// listener-level analog of QuicDatagram (conn.v), which has no address of
// its own since a single QuicConn only ever has one peer.
pub struct QuicListenerDatagram {
pub:
	bytes []u8
	peer  []u8
}

// QuicListenerEvent pairs one QuicEvent (conn.v) with the specific
// connection and peer address it came from -- a caller managing many
// connections needs to know WHICH one produced each event; a bare
// QuicEvent (designed for a single-connection caller) doesn't carry that.
pub struct QuicListenerEvent {
pub:
	conn  &QuicConn
	peer  []u8
	event QuicEvent
}

// QuicListenerPollResult aggregates every managed connection's own
// PollResult produced by one QuicListener.poll()/process_timeouts() call.
pub struct QuicListenerPollResult {
pub mut:
	outgoing []QuicListenerDatagram
	events   []QuicListenerEvent
	// The earliest next_timeout across every currently-managed connection
	// (none if no connection has an armed timer) -- when to next call
	// process_timeouts() if nothing else arrives first, the listener-level
	// analog of PollResult.next_timeout.
	next_timeout ?u64
}

// QuicListener demuxes one shared incoming datagram stream across many
// concurrent server-role QuicConns by connection ID, and decides
// Retry-vs-direct-accept for each new attempt. Never owns a socket or a
// thread -- see this file's own module-level doc comment.
pub struct QuicListener {
	params QuicListenerParams
mut:
	// Keyed by scid.bytestr() (V has no []u8 map-key support; this is the
	// established raw-byte-preserving idiom for one) -- every currently
	// live (not yet .closed) connection this listener has accepted.
	conns map[string]&QuicConn
	// Same keys as conns: the peer address recorded at accept() time,
	// reused for every subsequent outgoing datagram on that connection
	// regardless of which address a later incoming datagram claims to be
	// from -- v1 has no connection migration (PROGRESS.md's own scope
	// note), so an address change on a later packet is never treated as
	// authoritative for WHERE to reply; incoming processing itself is
	// still address-independent (QUIC's per-packet AEAD authentication is
	// what actually protects it, not the claimed source address), only
	// the reply destination is pinned. Deliberately NOT stored as a field
	// on QuicConn itself -- QuicConn stays transport-agnostic, matching
	// every other role in this module (dial()'s own caller, h3_mux_conn.v,
	// owns ITS OWN address bookkeeping externally too).
	peers map[string][]u8
	// bootstrap_dcid/pending_by_dcid together let handle_new_attempt
	// recognize a RETRANSMISSION of an attempt it has already accepted --
	// without this, every retransmitted copy of a client's post-Retry
	// Initial (ordinary loss recovery, not an attack: the server's own
	// first response datagram simply didn't arrive) or every replay of a
	// captured, still-valid Retry token would independently call
	// do_accept() again, each time allocating a brand-new QuicConn under
	// a freshly random scid -- unbounded connection growth from a single
	// legitimate (or replayed) attempt, since l.conns is keyed by that
	// fresh scid, which the attempt's own triggering packet never carries
	// (RFC 9000 §7.2: the client doesn't learn/switch to the real scid
	// until it successfully processes a reply).
	//
	// bootstrap_dcid: key = accepted connection's scid.bytestr() (same
	// keyspace as conns/peers) -> the header.dcid.bytestr() of the
	// packet that bootstrapped it -- kept so retire_if_closed can find
	// and remove the matching pending_by_dcid entry when a connection
	// retires, instead of leaking that entry forever.
	bootstrap_dcid map[string]string
	// pending_by_dcid: key = header.dcid.bytestr() of a new-attempt
	// packet (the client's true original_dcid for a direct accept, or
	// the server's own retry_scid for a post-Retry accept -- either way,
	// the ONE value that stays byte-identical across every retransmission
	// of that same logical attempt, RFC 9000 §7.2) -> the real conns/peers
	// key (the accepted connection's own scid.bytestr()). Checked BEFORE
	// running do_accept again; a hit routes the packet to the ALREADY-
	// accepted connection's own poll() instead.
	pending_by_dcid map[string]string
}

// new_quic_listener constructs a QuicListener with no connections yet.
// Fails only on a malformed retry_token_key -- everything else in
// `params` is either optional or only validated lazily per-attempt.
pub fn new_quic_listener(params QuicListenerParams) !&QuicListener {
	if params.retry_token_key.len != retry_token_key_len {
		return error('quic: listener retry_token_key must be exactly ${retry_token_key_len} bytes, got ${params.retry_token_key.len}')
	}
	return &QuicListener{
		params: params
	}
}

// connection_count reports how many connections this listener currently
// tracks (not yet retired to .closed) -- a cheap sanity/metrics hook, not
// used internally.
pub fn (l &QuicListener) connection_count() int {
	return l.conns.len
}

// derive_retry_scid deterministically derives the connection ID a Retry
// packet uses as its own Source Connection ID (RetryPacketParams.server_scid)
// from the client's true original_dcid alone -- rather than a fresh random
// value -- so that the LATER retried Initial (whose header.dcid IS this
// value, RFC 9000 §17.2.5.1) lets the listener recompute the SAME value
// again from the token's own embedded original_dcid claim, with no
// per-attempt state to persist in between. Mirrors stateless_reset.v's
// generate_stateless_reset_token's own HMAC(static_key, id) construction
// and its same underlying motivation (Retry, like a stateless reset, is
// explicitly designed to need no server-side memory of the attempt between
// sending it and validating the reply) -- distinct domain-separation label
// so this derivation can never collide with a stateless-reset-token
// derivation even if a caller reused the same raw key for both (this
// module's stateless_reset_token generation is a SEPARATE, caller-owned
// key in practice, but nothing here assumes that).
fn derive_retry_scid(key []u8, original_dcid []u8) ![]u8 {
	mut input := 'quic-retry-scid:'.bytes()
	input << original_dcid
	mac := hmac.new(key, input, sha256.sum256, sha256.block_size)
	return mac[..local_cid_len].clone()
}

// peek_datagram_dcid extracts JUST the Destination Connection ID from
// `raw`, the one field every QUIC v1 packet form (long or short header)
// carries at a fixed, unprotected position -- without the stricter
// version/Fixed-Bit/type validation parse_long_header applies, since a
// datagram for an ALREADY-recognized connection should still route to
// that connection's own poll() (which re-derives every one of those
// checks itself, more precisely, using its own connection state) even if
// something about it would make a context-free parse reject it outright.
// A short header packet's DCID length is never self-describing on the
// wire (RFC 9000 §17.3.1) -- this module never issues a connection ID of
// any length other than local_cid_len (see conn.v's "no active CID set"
// scope note), so that fixed length is the only one any legitimately
// routable short-header packet could be using.
fn peek_datagram_dcid(raw []u8) ?[]u8 {
	if raw.len < 1 {
		return none
	}
	form := peek_header_form(raw) or { return none }
	if form == .long {
		// RFC 9000 §17.2: 1-byte flags + 4-byte Version + 1-byte DCID Len + DCID.
		if raw.len < 6 {
			return none
		}
		dcid_len := int(raw[5])
		if raw.len < 6 + dcid_len {
			return none
		}
		return raw[6..6 + dcid_len].clone()
	}
	if raw.len < 1 + local_cid_len {
		return none
	}
	return raw[1..1 + local_cid_len].clone()
}

// poll processes one incoming datagram from `peer`: routes it to an
// already-known connection by Destination Connection ID, or -- for an
// unrecognized DCID carrying a valid Initial packet -- runs the
// Retry-or-accept new-connection-attempt decision (RFC 9000 §8.1). Any
// other unrecognized-DCID datagram (a stray Handshake/0-RTT/short-header
// packet with no matching connection) is silently dropped, matching RFC
// 9000 §5.2.2's own "MAY drop" allowance for exactly this case -- v1
// deliberately does not attempt a stateless reset in response (that needs
// a per-connection static-key/token record this listener never keeps once
// a connection is retired, a documented, not-yet-built follow-up).
pub fn (mut l QuicListener) poll(datagram []u8, peer []u8, now u64) !QuicListenerPollResult {
	mut result := QuicListenerPollResult{}
	dcid := peek_datagram_dcid(datagram) or { return result }
	key := dcid.bytestr()
	if key in l.conns {
		mut c := l.conns[key] or { return result }
		r := c.poll(datagram, now)!
		// l.peers[key] -- this connection's ORIGINALLY recorded address at
		// accept() time -- not the `peer` this specific call was invoked
		// with. v1 has no connection migration (PROGRESS.md's own scope
		// note): a later datagram claiming a DIFFERENT source address for
		// an already-demuxed connection (correct DCID, so it decrypts
		// fine -- QUIC's per-packet AEAD authentication is what actually
		// protects processing it, not the claimed source) must never be
		// trusted as WHERE to reply; doing so would let anyone who owns a
		// legitimately-established connection redirect the server's own
		// replies toward an arbitrary spoofed victim on demand, a
		// post-handshake reflection primitive that would otherwise defeat
		// the entire point of Retry-based address validation at accept()
		// time. Falls back to the call's own `peer` only if this entry's
		// recorded address is somehow missing (should not happen in
		// practice -- do_accept always populates it in the same call that
		// creates the conns entry -- but merge_conn_result needs SOME
		// value, and silently dropping the reply outright would be worse).
		reply_peer := l.peers[key] or { peer }
		l.merge_conn_result(mut result, c, r, reply_peer)
		l.retire_if_closed(key, c)
	} else {
		header := find_first_initial_header(datagram) or { return result }
		l.handle_new_attempt(header, datagram, peer, now, mut result)!
	}
	// poll() only actually DRIVES the one connection `datagram`'s DCID
	// demuxed to (or the one newly accepted) -- but result.next_timeout's
	// own documented contract promises the earliest deadline across EVERY
	// managed connection, matching process_timeouts()'s own aggregate.
	// Without this, a caller that (per this file's own module doc comment)
	// mirrors h3_mux_conn.v's `next_timeout = result.next_timeout` blind-
	// overwrite idiom would silently lose track of some OTHER connection's
	// already-pending deadline (e.g. one sitting in .closing) every time a
	// DIFFERENT connection's datagram arrives in between.
	l.merge_all_connections_next_timeout(mut result)
	return result
}

// merge_all_connections_next_timeout folds every currently-managed
// connection's own compute_next_timeout() into result.next_timeout.
// compute_next_timeout() only reads existing loss_detection/idle_timeout/
// closing_deadline state (mutates nothing itself, despite its `mut`
// receiver) -- calling it once per connection on every poll() call is
// side-effect-free, just O(n) extra work, the same complexity class
// process_timeouts() already pays unconditionally every time it runs.
fn (mut l QuicListener) merge_all_connections_next_timeout(mut result QuicListenerPollResult) {
	for key in l.conns.keys() {
		mut c := l.conns[key] or { continue }
		nt := c.compute_next_timeout() or { continue }
		if existing := result.next_timeout {
			if nt < existing {
				result.next_timeout = nt
			}
		} else {
			result.next_timeout = nt
		}
	}
}

// process_timeouts drives EVERY currently-managed connection's own
// process_timeouts(now) -- v1 deliberately does not track each
// connection's individually-armed next_timeout to skip calling this on
// ones with nothing due yet (a real timer wheel); QuicConn.process_timeouts
// is cheap to call even when no deadline has actually elapsed (it checks
// internally and no-ops), so iterating all of them is correct, just not
// the most efficient possible choice for very many idle connections --
// acceptable for a first v1 listener, a documented follow-up otherwise.
pub fn (mut l QuicListener) process_timeouts(now u64) !QuicListenerPollResult {
	mut result := QuicListenerPollResult{}
	// Deliberately `l.conns.keys()` + a per-key lookup, NOT `for key, mut c
	// in l.conns` -- that shape reproducibly segfaulted (null deref inside
	// compute_next_timeout, called through a loop-bound `c`) when tried
	// here, even though the identical-looking pattern is used successfully
	// elsewhere in this codebase for a DIFFERENT pointer-valued map
	// (transport.v's `for k, mut c in t.h2_conns`). Root cause not fully
	// isolated; this is the SAME lookup idiom poll() already uses
	// successfully (`l.conns[key] or { ... }`) for a single connection, so
	// reusing it here instead of the for-loop's own mut-binding sidesteps
	// whatever the difference is rather than chasing it further.
	for key in l.conns.keys() {
		mut c := l.conns[key] or { continue }
		peer := l.peers[key] or { continue }
		r := c.process_timeouts(now)!
		l.merge_conn_result(mut result, c, r, peer)
		l.retire_if_closed(key, c)
	}
	return result
}

// merge_conn_result folds one connection's own PollResult into the
// listener-level result being built: every outgoing datagram is addressed
// to `peer` (see QuicListener.peers' own doc comment for why that's always
// the connection's ORIGINALLY recorded address, never a later claimed
// one), every event is tagged with which connection/peer produced it, and
// next_timeout is merged as the EARLIEST across every connection seen so
// far this call (mirroring compute_next_timeout's own earliest-of-several
// merge inside QuicConn itself).
fn (mut l QuicListener) merge_conn_result(mut result QuicListenerPollResult, c &QuicConn, r PollResult, peer []u8) {
	for dg in r.outgoing {
		result.outgoing << QuicListenerDatagram{
			bytes: dg.bytes
			// .clone() -- a []u8 struct-field assignment is a slice-header
			// copy, not a deep copy, so without this every returned
			// QuicListenerDatagram.peer would alias the CALLER's own
			// `peer` buffer. A caller reusing one scratch receive/address
			// buffer across a tight poll() loop (the natural, performance-
			// motivated pattern this file's own module doc comment
			// anticipates for a future net.http caller) would then see
			// an ALREADY-RETURNED datagram's peer silently change once it
			// overwrites that buffer for the next packet. do_accept's own
			// `l.peers[key] = peer.clone()` already follows this rule for
			// the SAME parameter; this call site and send_retry's own
			// (below) were the two that had been missed.
			peer: peer.clone()
		}
	}
	for ev in r.events {
		result.events << QuicListenerEvent{
			conn:  c
			peer:  peer.clone()
			event: ev
		}
	}
	if nt := r.next_timeout {
		if existing := result.next_timeout {
			if nt < existing {
				result.next_timeout = nt
			}
		} else {
			result.next_timeout = nt
		}
	}
}

// retire_if_closed removes a connection from this listener's demux tables
// once it has reached .closed (RFC 9000 §10.2.2's draining period has
// elapsed) -- without this, every connection this listener ever accepts
// stays in `conns`/`peers` forever, an unbounded memory leak for any
// long-running server.
fn (mut l QuicListener) retire_if_closed(key string, c &QuicConn) {
	if c.state() == .closed {
		bootstrap_key := l.bootstrap_dcid[key] or { '' }
		if bootstrap_key != '' {
			l.pending_by_dcid.delete(bootstrap_key)
		}
		l.bootstrap_dcid.delete(key)
		l.conns.delete(key)
		l.peers.delete(key)
	}
}

// handle_new_attempt is the RFC 9000 §8.1 Retry-or-accept decision for a
// datagram whose DCID matched no known connection but does carry a valid
// Initial packet (`header`, already parsed by find_first_initial_header).
fn (mut l QuicListener) handle_new_attempt(header QuicLongHeader, datagram []u8, peer []u8, now u64, mut result QuicListenerPollResult) ! {
	// RFC 9000 §14.1: "A server MUST discard an Initial packet that is
	// carried in a UDP datagram with a payload that is smaller than the
	// smallest allowed maximum datagram size of 1200 bytes" -- unconditional
	// for ANY Initial packet a server receives, token or no token, so
	// checked here BEFORE branching into either send_retry (which has no
	// anti-amplification accounting of its own -- see its own doc comment
	// for why a single Retry is normally always small enough that the
	// limit could never bind, an assumption that only holds if the
	// TRIGGERING datagram itself already met this floor) or do_accept
	// (which reaches accept.v's OWN copy of this same check -- kept there
	// too, as defense in depth for any other future caller of accept()
	// that isn't this listener).
	if datagram.len < min_initial_datagram_size {
		return
	}
	// Retransmission/replay dedup -- see pending_by_dcid's own doc comment
	// on the QuicListener struct for the full rationale. header.dcid is
	// the one value that stays byte-identical across every retransmission
	// of the SAME logical attempt (direct accept: the client's true
	// original_dcid; post-Retry: the server's own retry_scid) -- if an
	// earlier packet with this same dcid already resulted in an accepted
	// connection, route straight to it instead of running do_accept (or
	// send_retry) all over again.
	bootstrap_key := header.dcid.bytestr()
	if bootstrap_key in l.pending_by_dcid {
		existing_key := l.pending_by_dcid[bootstrap_key] or { '' }
		if existing_key in l.conns {
			mut c := l.conns[existing_key] or { return }
			r := c.poll(datagram, now)!
			reply_peer := l.peers[existing_key] or { peer }
			l.merge_conn_result(mut result, c, r, reply_peer)
			l.retire_if_closed(existing_key, c)
			return
		}
		// Stale index entry (should not happen -- retire_if_closed keeps
		// both maps in sync -- but if it ever does, drop it and fall
		// through to ordinary handling rather than silently discarding a
		// legitimate attempt).
		l.pending_by_dcid.delete(bootstrap_key)
	}
	if header.token.len > 0 {
		claims := validate_retry_token_for_attempt(l.params.retry_token_key, header.token, peer,
			now, l.params.retry_token_max_age_ms) or {
			// RFC 9000 §8.1.2: "In response to processing an Initial
			// packet containing a token that was provided in a Retry
			// packet, a server cannot send another Retry packet; it can
			// only refuse the connection or permit it to proceed" -- v1
			// never issues tokens any other way (retry_token.v's own
			// scope note: NEW_TOKEN issuance is out of scope), so every
			// token this listener ever sees came from ITS OWN Retry,
			// making this branch unconditionally "refuse." Discarding
			// (not erroring the whole poll() call) matches the RFC's own
			// following sentence: "a server has not established any
			// state for the connection at this point and so does not
			// enter the closing period" -- there is no QuicConn yet to
			// formally close.
			return
		}
		// The retried Initial's OWN header.dcid is the RETRY-CHOSEN
		// connection ID (RFC 9000 §17.2.5.1: the client switches its
		// outgoing DCID to the Retry's SCID), never claims.original_dcid
		// itself -- those are two DIFFERENT values by design (see
		// derive_retry_scid's own doc comment) and must never be compared
		// directly. What must actually hold is that THIS packet's dcid is
		// the SAME retry-scid the server would have chosen FOR
		// claims.original_dcid -- re-derived here (not stored anywhere)
		// so a token cryptographically valid for one original_dcid can't
		// be replayed against a DIFFERENT attempt's differently-derived
		// retry-scid.
		expected_scid := derive_retry_scid(l.params.retry_token_key, claims.original_dcid)!
		if expected_scid != header.dcid {
			return
		}
		l.do_accept(header, datagram, peer, now, claims.original_dcid, mut result)!
		return
	}
	if l.params.always_retry {
		l.send_retry(header, peer, now, mut result)!
		return
	}
	l.do_accept(header, datagram, peer, now, header.dcid, mut result)!
}

// send_retry builds and queues a Retry packet (RFC 9000 §17.2.5) for a new
// connection attempt that has not yet presented a valid token. Never
// persists any per-attempt state -- see derive_retry_scid's own doc
// comment for why that's possible at all. A single Retry packet is always
// far smaller than the RFC 9000 §14.1-mandated 1200-byte floor on the
// Initial that provoked it, so the RFC 9000 §8.1 3x anti-amplification
// limit can never bind for this one send; no AntiAmplificationLimiter is
// constructed here since one that's never persisted or checked again
// would have nothing to actually limit.
fn (mut l QuicListener) send_retry(header QuicLongHeader, peer []u8, now u64, mut result QuicListenerPollResult) ! {
	server_scid := derive_retry_scid(l.params.retry_token_key, header.dcid)!
	retry_bytes := encode_retry_packet(RetryPacketParams{
		client_scid:   header.scid
		server_scid:   server_scid
		original_dcid: header.dcid
		token_key:     l.params.retry_token_key
		client_addr:   peer
		issued_at_ms:  now
	}) or {
		// Two distinct ways this can fail, neither worth propagating as
		// an error for the WHOLE poll() call (some other, unrelated
		// attempt it might also be processing this round should not be
		// aborted over this one's failure): (1) server_scid coincidentally
		// equal to header.dcid (encode_retry_packet's own RFC 9000
		// §17.2.5.1 guard) -- astronomically unlikely for an 8-byte
		// HMAC-derived value; (2) `peer`'s serialized length exceeding
		// encode_retry_token_claims' 255-byte field-length limit
		// (retry_token.v) -- reachable if a caller's own peer-address
		// encoding is unusually large, in which case that peer's attempts
		// are consistently dropped here with no Retry ever sent (a
		// caller-configuration issue to fix at the source, not something
		// this function can recover from).
		return
	}
	result.outgoing << QuicListenerDatagram{
		bytes: retry_bytes
		peer:  peer.clone()
	}
}

// do_accept calls 13d-1's accept() to actually construct the new
// connection, registers it in this listener's demux tables under its
// freshly generated scid, and folds its own first PollResult (typically
// the ServerHello-through-Finished response flight) into `result`.
// `original_dcid_for_tp` is the TRUE original_dcid to advertise in
// transport parameters -- see AcceptParams.original_dcid_override's own
// doc comment for why that differs from `header.dcid` after a Retry.
fn (mut l QuicListener) do_accept(header QuicLongHeader, datagram []u8, peer []u8, now u64, original_dcid_for_tp []u8, mut result QuicListenerPollResult) ! {
	mut retry_scid_param := ?[]u8(none)
	if header.dcid != original_dcid_for_tp {
		// header.dcid differs from the client's true original_dcid only
		// when this Initial is a RETRIED one (its DCID is the server's
		// own Retry-chosen connection ID, RFC 9000 §17.2.5.1) -- in which
		// case that same value IS the retry_source_connection_id this
		// connection's transport parameters must advertise (RFC 9000
		// §7.3). A direct accept with no Retry has header.dcid ==
		// original_dcid_for_tp (both the packet's own true original), so
		// this stays none, matching AcceptParams' own default meaning.
		retry_scid_param = header.dcid.clone()
	}
	accept_params := AcceptParams{
		transport_parameters:       l.params.transport_parameters
		alpn_protocols:             l.params.alpn_protocols
		certificate_chain:          l.params.certificate_chain
		signing_key:                l.params.signing_key
		retry_source_connection_id: retry_scid_param
		original_dcid_override:     original_dcid_for_tp
	}
	c, r := accept(datagram, accept_params, now) or {
		// A malformed or otherwise-invalid connection attempt -- RFC 9000
		// consistently frames this class of failure as "MUST/SHOULD
		// discard," never a listener-level error; one bad attempt must
		// not fail whatever ELSE this poll() call might still need to do.
		return
	}
	key := c.scid.bytestr()
	bootstrap_key := header.dcid.bytestr()
	l.conns[key] = c
	l.peers[key] = peer.clone()
	l.bootstrap_dcid[key] = bootstrap_key
	l.pending_by_dcid[bootstrap_key] = key
	l.merge_conn_result(mut result, c, r, peer)
	l.retire_if_closed(key, c)
}
