module quic

import crypto.ecdsa
import crypto.rand

// accept.v: the server-role counterpart to dial() (conn.v). Split into its
// own file rather than added to conn.v directly for one purely mechanical
// reason: this file needs crypto.rand (a real CSPRNG) for the server's own
// connection ID and ServerHello random, and conn.v already imports the
// plain `rand` module (non-cryptographic, used by dial() -- a known,
// separately-tracked gap, not repeated here) under the same bare name --
// V requires disambiguating two same-named imports, and a fresh file needing
// only the secure one is simpler than an alias in an already-large file.
//
// Everything else server-role lives in conn.v itself (the struct fields,
// dispatch_handshake_message's role branch, the own/peer_*_keys and
// is_handshake_confirmed role-aware helpers, drain_outgoing's HANDSHAKE_DONE
// send) -- accept() is deliberately thin: it does just enough to construct
// a QuicConn with the right identity (original_dcid/dcid/scid/peer_scid)
// and Initial-space keys to make the FIRST incoming datagram decryptable,
// then hands that exact datagram to poll() -- the same, already-tested
// entry point every SUBSEQUENT datagram goes through. Everything else
// (decrypting, reassembling the ClientHello's CRYPTO frame(s), running
// Tls13ServerHandshake.respond_to_client_hello, deriving Handshake/
// Application keys, queuing the response flight, and draining it into
// actual outgoing datagrams) happens exactly where it already happens for
// every OTHER handshake message, via dispatch_handshake_message's own
// server branch -- there is no separate, parallel decrypt/dispatch path
// here to keep in sync with process_initial_or_handshake's.

// AcceptParams is everything accept() needs beyond what it decides for
// itself (this server's own connection ID, ServerHello random) -- the
// server-role mirror of DialParams.
pub struct AcceptParams {
pub:
	// This server's own OFFERED transport parameters. accept() overrides
	// initial_source_connection_id and original_destination_connection_id
	// with values it derives itself (RFC 9000 §7.3) regardless of what the
	// caller set there, the same override dial() already applies to its own
	// initial_source_connection_id.
	transport_parameters QuicTransportParameters
	// Application protocols this server supports, most preferred first
	// (RFC 7301 §3.2 -- the server picks, in ITS OWN preference order, from
	// among what the client also offered). Named to match DialParams'
	// alpn_protocols field, even though ServerHandshakeParams' own
	// equivalent field is named supported_alpn_protocols.
	alpn_protocols []string
	// This server's own certificate chain, leaf-first, and the long-lived
	// private key matching the leaf's public key -- see
	// ServerHandshakeParams' own doc comments (tls13_server_handshake.v)
	// for what accept() does NOT own here (loading these from a PEM file is
	// a future caller's job, same scope note that struct already states).
	certificate_chain []CertificateEntry
	signing_key       ecdsa.PrivateKey
	// Set by a caller (13d-2's listener) when this connection attempt
	// followed a Retry round-trip, to the SAME server-chosen connection ID
	// that Retry packet used as its own Source Connection ID -- RFC 9000
	// §7.3: "a server MUST include the retry_source_connection_id
	// transport parameter" whenever it sent a Retry, echoing that exact
	// value so the client can confirm it, not a fresh one. Left `none` for
	// a direct accept with no preceding Retry. This is NOT optional
	// decoration: `Tls13ClientHandshake.process_encrypted_extensions`
	// (tls13_handshake.v) already enforces both halves of RFC 9000 §7.3 on
	// the client side -- rejects the connection if this parameter is
	// ABSENT after a Retry occurred, and equally rejects it if PRESENT
	// when no Retry occurred -- so a caller that sent a Retry and then
	// omits this field here would produce a connection this codebase's own
	// dial()ed client can never actually complete.
	retry_source_connection_id ?[]u8
	// Set by a caller (13d-2's listener) alongside retry_source_connection_id
	// when this connection attempt followed a Retry round-trip, to the
	// TRUE Destination Connection ID the client used on its very FIRST
	// (pre-Retry) Initial packet -- NOT `raw_datagram`'s own header.dcid,
	// which by the time a retried Initial reaches accept() is instead the
	// server's Retry-chosen connection ID (RFC 9000 §17.2.5.1: the client
	// switches its outgoing DCID to the Retry's SCID). RFC 9000 §7.3
	// requires the original_destination_connection_id transport parameter
	// to echo that FIRST value regardless -- and
	// `Tls13ClientHandshake.process_encrypted_extensions` (tls13_handshake.v
	// line ~503) strictly validates it against the client's own
	// `original_dcid`, which a Retry never changes (conn.v's
	// `process_retry` updates `c.dcid`/`c.peer_scid`/`c.token` but
	// deliberately leaves `c.original_dcid` untouched) -- so getting this
	// wrong silently breaks every post-Retry handshake against this
	// codebase's own client. Left `none` for a direct accept with no
	// preceding Retry, in which case `header.dcid` (the packet's own,
	// correct in that case) is used instead.
	original_dcid_override ?[]u8
}

// accept constructs a new server-role QuicConn from `raw_datagram` -- the
// first UDP datagram of a new connection attempt, expected to contain
// exactly one Initial packet carrying the client's ClientHello (this
// codebase's own dial() never sends anything else in its first flight;
// see the loop below for what happens if that assumption doesn't hold).
// Returns the new connection AND the PollResult from processing that same
// datagram through it -- typically the response flight (ServerHello under
// Initial protection, EncryptedExtensions..this server's own Finished
// under Handshake protection) queued in PollResult.outgoing, ready for the
// caller to actually send.
//
// Deliberately does NOT decide whether to accept directly or send a Retry
// first (RFC 9000 §8.1's address-validation policy) -- that decision needs
// state (has this source address been seen before? is the anti-
// amplification budget already exhausted?) that only a caller tracking
// MANY connection attempts across MANY source addresses can have; a single
// accept() call has no such context. 13b's encode_retry_packet/
// generate_retry_token/AntiAmplificationLimiter already exist for a caller
// to make and act on that decision BEFORE ever calling accept() -- wiring
// them together is 13d-2's job (the UDP listener), not this constructor's.
//
// KNOWN SCOPE LIMIT, not fixed here: the server's Handshake-space CRYPTO
// flight (EncryptedExtensions+Certificate+CertificateVerify+Finished,
// dispatch_handshake_message's server bootstrap branch queues it as one
// blob in pending_handshake_crypto) is flushed by drain_outgoing's
// pre-existing logic as a SINGLE CRYPTO frame in a SINGLE Handshake packet
// -- correct for this repo's own small test certificate, but not
// fragmented across multiple packets if a real-world certificate chain
// doesn't fit one packet's payload. dial()'s own ClientHello flush has the
// identical shape but was never previously exercised with anything large
// enough to expose it, since a ClientHello (no certificate) is always
// small. Splitting a CRYPTO stream write across multiple packets is a
// real, separate piece of work, not attempted here.
pub fn accept(raw_datagram []u8, params AcceptParams, now u64) !(&QuicConn, PollResult) {
	// RFC 9000 §14.1: "A server MUST discard an Initial packet that is
	// carried in a UDP datagram with a payload that is smaller than the
	// smallest allowed maximum datagram size of 1200 bytes" -- an
	// anti-amplification measure: without this, a spoofed-source, undersized
	// trigger datagram gets a full ServerHello+EncryptedExtensions+
	// Certificate+CertificateVerify+Finished response flight, routinely
	// several times larger than the trigger, aimed at whatever address the
	// attacker claimed. coalesce.v's split_coalesced_datagram deliberately
	// does NOT enforce this itself (see its own doc comment) -- it's a
	// stateless, role-agnostic splitter also used for datagrams this
	// endpoint SENDS, where a legitimate reply smaller than 1200 bytes
	// (e.g. ACK-only) is allowed; that comment explicitly defers the real,
	// role-aware check to "a later phase with that visibility," which is
	// this one: the single call site that always knows a datagram reaching
	// it is being RECEIVED, by a SERVER, before address validation. Found
	// by 13d-1's adversarial review (v-quality lens).
	if raw_datagram.len < min_initial_datagram_size {
		return error('quic: accept: datagram (${raw_datagram.len} bytes) is smaller than the RFC 9000 §14.1 anti-amplification floor of ${min_initial_datagram_size} bytes')
	}
	header := find_first_initial_header(raw_datagram) or {
		return error('quic: accept: ${err.msg()}')
	}

	// header.dcid is the client's freshly-chosen original_dcid -- RFC 9001
	// §5.2 derives Initial secrets from it, identically on both sides (the
	// same derive_initial_secrets dial() itself calls for the client half
	// of this exact derivation).
	initial_secrets := derive_initial_secrets(header.dcid)!
	initial_keys_client := derive_packet_protection_keys(initial_secrets.client)!
	initial_keys_server := derive_packet_protection_keys(initial_secrets.server)!

	scid := rand.bytes(local_cid_len) or {
		return error('quic: accept: failed to generate this server\'s own connection ID: ${err.msg()}')
	}
	server_hello_random := rand.bytes(32) or {
		return error('quic: accept: failed to generate ServerHello random: ${err.msg()}')
	}

	mut own_params := params.transport_parameters
	own_params.initial_source_connection_id = scid.clone()
	// RFC 9000 §7.3: a server MUST send original_destination_connection_id,
	// echoing the DCID the client's own FIRST Initial packet used -- the
	// value ServerHandshakeParams' own doc comment names as this caller's
	// responsibility to set (build_encrypted_extensions itself validates
	// it's present, but does not know what value to fill in). Defaults to
	// this packet's own header.dcid (correct for a direct accept with no
	// preceding Retry); see original_dcid_override's own doc comment above
	// for why a post-Retry acceptance needs a DIFFERENT value here.
	mut original_dcid_for_tp := header.dcid.clone()
	if override := params.original_dcid_override {
		original_dcid_for_tp = override.clone()
	}
	own_params.original_destination_connection_id = original_dcid_for_tp
	// Yes, this .clone()s a value listener.v's do_accept already cloned
	// once itself -- a deliberately kept redundant allocation, not an
	// oversight: this is a PUBLIC function's parameter handling, and
	// accept() has no way to know whether some OTHER, less careful future
	// caller passed a value here it still holds a live reference to.
	// Defending against that is worth one extra small clone per accepted
	// connection.
	if retry_scid := params.retry_source_connection_id {
		own_params.retry_source_connection_id = retry_scid.clone()
	}

	own_max_idle_timeout_ms := own_params.max_idle_timeout or { u64(0) }

	mut c := &QuicConn{
		role:                     .server
		state:                    .handshaking
		original_dcid:            header.dcid.clone()
		dcid:                     header.scid.clone()
		scid:                     scid
		peer_scid:                header.scid.clone()
		token:                    []u8{}
		server_handshake:         none
		server_accept_params:     ServerHandshakeParams{
			transport_parameters:     own_params
			supported_alpn_protocols: params.alpn_protocols
			certificate_chain:        params.certificate_chain
			signing_key:              params.signing_key
			server_hello_random:      server_hello_random
		}
		handshake_completion:     new_handshake_completion_state()
		pn_spaces:                new_packet_number_spaces()
		initial_keys_client:      initial_keys_client
		initial_keys_server:      initial_keys_server
		initial_crypto:           new_crypto_stream_reassembler()
		handshake_crypto:         new_crypto_stream_reassembler()
		loss_detection:           new_quic_loss_detection_timer()
		congestion_control:       new_newreno_congestion_control()
		own_max_idle_timeout_ms:  own_max_idle_timeout_ms
		stateless_reset:          new_stateless_reset_tracker()
		connection_start:         now
		own_transport_parameters: own_params
		streams:                  new_quic_stream_set(.server)
		conn_send_window:         new_flow_control_window(0)
		conn_recv_window:         new_receive_window(own_params.initial_max_data or { u64(0) })
		local_max_streams_bidi:   own_params.initial_max_streams_bidi or { u64(0) }
		local_max_streams_uni:    own_params.initial_max_streams_uni or { u64(0) }
	}

	result := c.poll(raw_datagram, now)!
	return c, result
}
