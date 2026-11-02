// Copyright (c) 2026 blackshirt.
//
// state_machine.v defines the unified handshake core both adapters drive: the
// state and message vocabulary types, the HandshakeCore struct and its
// constructors, the state cursor accessors, and the ServerHello/HelloRetryRequest and
// encrypted-flight phase transitions that advance the machine to .connected. Its
// methods attach to HandshakeCore; the inbound reassembly buffers, receive/dispatch
// routing and terminal-failure primitives live in state_machine_extra.v, ECDHE and the
// derived-secret lifecycle in key_exchange.v and key_schedule.v, and the one-sided
// surfaces guarded by is_quic live in core_guarded.v (TLS-record-only and QUIC-only
// methods together), as HandshakeCore methods too — V keeps one struct's methods
// in its declaring module, not one file.
module handshake

import crypto.ecdsa
import crypto.rand
import crypto.subtle
import tls.pki
import x.crypto.curve25519

// HandshakeState is the client handshake state machine, mirroring the client states of
// RFC 8446 Appendix A.1 (all-caps RFC names carry no formal meaning, so rendered here in
// snake_case); `failed` is an implementation terminal the RFC does not define.
pub enum HandshakeState {
	start         // RFC START: nothing sent yet; create_client_hello moves us out of here
	wait_sh       // RFC WAIT_SH: ClientHello sent; awaiting ServerHello or HelloRetryRequest
	wait_ee       // RFC WAIT_EE: ServerHello accepted; awaiting EncryptedExtensions
	wait_cert_cr  // RFC WAIT_CERT_CR: awaiting CertificateRequest or Certificate
	wait_cert     // RFC WAIT_CERT: awaiting Certificate
	wait_cv       // RFC WAIT_CV: awaiting CertificateVerify
	wait_finished // RFC WAIT_FINISHED: awaiting Finished
	connected     // RFC CONNECTED: server Finished verified; application data may flow
	failed        // implementation terminal; no further handshake messages are accepted
}

// HandshakeMessage is one outbound handshake message, unframed: msg holds the wire
// bytes, header included. Its protection phase is implied by the core's current
// HandshakeState, not stored per-message.
@[noinit]
pub struct HandshakeMessage {
	msg []u8 // complete handshake message bytes, including the header
}

// new_handshake_message builds a validated outbound message for the core. Its
// protection phase is the core's current HandshakeState.
fn new_handshake_message(msg []u8) !HandshakeMessage {
	_, _ := split_handshake_message(msg)!
	return HandshakeMessage{
		msg: msg
	}
}

// msg returns the complete handshake message bytes (4-byte header included).
pub fn (m &HandshakeMessage) msg() []u8 {
	return m.msg
}

// HandshakeOptions configures the shared TLS handshake, including peer
// validation, ALPN, and the ClientHello preferences offered by the client.
@[params]
pub struct HandshakeOptions {
pub:
	// alpn is the preference-ordered list of application protocols to offer.
	alpn []string
	// validator verifies the server's certificate chain and hostname; omitted
	// options use the system root CA bundle.
	validator ?pki.Validator
	// key_share_group is the group used for the initial ECDHE share.
	key_share_group NamedGroup = .x25519
	// supported_groups is preference-ordered; empty uses X25519 then P-256.
	supported_groups []NamedGroup
	// cipher_suites is preference-ordered; empty uses the built-in TLS 1.3 suites.
	cipher_suites []CipherSuite
	// signature_algorithms is preference-ordered; empty uses the built-in list.
	signature_algorithms []SignatureScheme
	// legacy_session_id is the ClientHello session id echoed in ServerHello.
	legacy_session_id []u8
	// random is the ClientHello random; with legacy_session_id it lets a test vector
	// pin a deterministic handshake.
	random []u8
pub mut:
	// quic_transport_params is the encoded content of the TLS
	// quic_transport_parameters extension (RFC 9001 §8.2), copied through without
	// interpretation: the adapter above encodes and decodes it.
	quic_transport_params []u8
}

// with_validator returns a copy of the options with the given validator, preserving
// every other setting. Adapters use it to resolve trust configuration before handing
// the options to a constructor without copying fields by hand.
pub fn (o HandshakeOptions) with_validator(validator pki.Validator) HandshakeOptions {
	return HandshakeOptions{
		alpn:                  o.alpn.clone()
		validator:             validator
		key_share_group:       o.key_share_group
		supported_groups:      o.supported_groups.clone()
		cipher_suites:         o.cipher_suites.clone()
		signature_algorithms:  o.signature_algorithms.clone()
		legacy_session_id:     o.legacy_session_id.clone()
		random:                o.random.clone()
		quic_transport_params: o.quic_transport_params
	}
}

// HandshakeCore owns shared TLS 1.3 handshake state and behavior, plus both transport
// surfaces: the key accessors are shared and answer in the core's mode (record or
// packet legs, derived in-module from the retained traffic secrets), while the
// one-sided methods guard themselves with is_quic (core_guarded.v).
@[noinit]
struct HandshakeCore {
	// is_quic (selects the derivation vocabulary and guards the one-sided entry points,
	// which are refused at run time on the wrong-mode core)
	// and validator (cert policy) are core identity, fixed at construction: flipping either
	// mid-handshake would void the guards and any material derived against the originals.
	is_quic   bool
	validator pki.Validator
mut:
	// ClientHello settings and key exchange state.
	config      ClientHelloConfig // settings used to build or retry the ClientHello
	ecdhe_group NamedGroup        // key exchange group used for the shared secret
	priv_x25519 &curve25519.PrivateKey = unsafe { nil } // client's X25519 private key, if used
	priv_p256   ?ecdsa.PrivateKey // client's P-256 private key, if used

	// Transcript and negotiated cipher suite.
	transcript   Transcript    // hash of handshake messages exchanged so far
	hash_algo    HashAlgorithm // hash algorithm used for this handshake
	cipher_suite CipherSuite   // cipher suite selected by the server
	key_len      int           // encryption key length for the selected cipher suite
	quic_params  ?[]u8         // server transport parameters received in EncryptedExtensions, kept as opaque bytes for the QUIC adapter

	// Key-schedule material used to protect handshake and application data.
	handshake_secret []u8 // secret used to derive handshake traffic keys
	c_hsk_tsec       []u8 // client handshake traffic secret
	c_app_tsec       []u8 // client application traffic secret
	s_hsk_tsec       []u8 // server handshake traffic secret
	s_app_tsec       []u8 // server application traffic secret

	// Incoming handshake bytes that do not yet form complete messages.
	reassembly []u8 // inbound bytes pending a complete message, across both phases

	// Certificate chain received from the server, leaf first; validated against the
	// immutable validator declared above.
	server_certs []pki.Certificate // certificates received from the server, leaf first

	// Handshake retry and outcome.
	hrr_cipher_suite ?CipherSuite   // suite from the retry request; presence marks a retry
	state            HandshakeState // position in the client state machine (RFC 8446 App. A.1)
	negotiated_alpn  string         // protocol chosen by the server, or empty if none
	fail_alert       AlertDescription = .internal_error // alert to report if the handshake fails
}

// reassembly_buf_cap is the initial reassembly allocation, not a limit: the hard bound
// a hostile peer must never exceed is enforced against max_handshake_message_len in the
// append-and-consume paths and next_unframed_message.
const reassembly_buf_cap = 4096

// new_handshake_core creates the unified handshake state from the server name, full
// HandshakeOptions (ALPN, cert validation) and the transport mode; new_tls_handshake() and
// new_quic_handshake() are the public wrappers fixing is_quic to false / true.
fn new_handshake_core(server_name string, options HandshakeOptions, is_quic bool) !HandshakeCore {
	validate_handshake_options(options)!
	validator := options.validator or {
		pki.default_validator()!
	}
	// Pin random/session id at construction so HRR reuses them byte-identically (§4.1.4).
	// session_id.len has been checked in validate_handshake_options. QUIC MUST NOT use
	// middlebox compatibility mode, so its legacy_session_id is always empty
	// (RFC 9001 §8.4); a caller-supplied value would be an illegal parameter to a peer.
	if is_quic && options.legacy_session_id.len > 0 {
		return error('handshake: QUIC forbids a non-empty legacy_session_id (RFC 9001 §8.4)')
	}
	session_id := if is_quic {
		[]u8{}
	} else if options.legacy_session_id.len > 0 {
		options.legacy_session_id.clone()
	} else {
		rand.bytes(legacy_session_id_len)!
	}
	// random.len has been checked in validate_handshake_options
	random := if options.random.len > 0 {
		options.random.clone()
	} else {
		rand.bytes(client_hello_random_len)!
	}

	mut core := HandshakeCore{
		state:      .start
		is_quic:    is_quic
		config:     ClientHelloConfig{
			server_name:           server_name
			alpn:                  options.alpn.clone()
			session_id:            session_id
			random:                random
			cipher_suites:         options.cipher_suites.clone()
			supported_groups:      options.supported_groups.clone()
			signature_algorithms:  options.signature_algorithms.clone()
			is_quic:               is_quic
			quic_transport_params: if is_quic {
				options.quic_transport_params
			} else {
				none
			}
		}
		transcript: new_transcript(.sha256)
		hash_algo:  .sha256
		key_len:    16
		validator:  validator
		reassembly: []u8{cap: reassembly_buf_cap}
	}
	core.reset_key_share(options.key_share_group)!
	core.config.client_share = KeyShareEntry{
		group:        options.key_share_group
		key_exchange: core.public_key_bytes()!
	}
	return core
}

// new_tls_handshake creates a TLS-over-records handshaker.
// HandshakeOptions fields can be supplied as optional named arguments.
pub fn new_tls_handshake(server_name string, options HandshakeOptions) !HandshakeCore {
	return new_handshake_core(server_name, options, false)
}

// new_quic_handshake creates a QUIC handshaker. HandshakeOptions fields can
// be supplied as optional named arguments after server_name.
pub fn new_quic_handshake(server_name string, options HandshakeOptions) !HandshakeCore {
	return new_handshake_core(server_name, options, true)
}

// state reports the client's position in the RFC 8446 Appendix A.1 handshake
// state machine; adapters poll it to know when the handshake completed and which
// messages are still legal on the wire.
pub fn (h &HandshakeCore) state() HandshakeState {
	return h.state
}

// is_in_flight reports whether the current state lies inside the encrypted server
// flight, the window in which handshake-phase bytes are legal.
fn (h &HandshakeCore) is_in_flight() bool {
	return match h.state {
		.wait_ee, .wait_cert_cr, .wait_cert, .wait_cv, .wait_finished { true }
		else { false }
	}
}

// negotiated_alpn returns the ALPN protocol the server picked ('' when none).
pub fn (h &HandshakeCore) negotiated_alpn() string {
	return h.negotiated_alpn
}

// negotiated_cipher_suite reports the suite selected by the server in ServerHello.
pub fn (h &HandshakeCore) negotiated_cipher_suite() CipherSuite {
	return h.cipher_suite
}

// update_transcript appends one message to the running transcript, converting the
// total-size bound into the alert-classified failure the state machine reports.
fn (mut h HandshakeCore) update_transcript(msg []u8) ! {
	h.transcript.update(msg) or {
		return h.fail_with(.decode_error, err)
	}
}

// create_client_hello generates the initial ClientHello, advances to
// .wait_sh and returns the raw bytes to hash and frame (RFC 8446 App. D.4).
pub fn (mut h HandshakeCore) create_client_hello() ![]u8 {
	// A second call would replay the first ClientHello into the transcript and
	// corrupt every later hash; the retry ClientHello has its own path, produced
	// while a HelloRetryRequest is fed.
	if h.state != .start {
		return error('handshake: create_client_hello() only runs once (state: ${h.state()}); the retry ClientHello is produced while a HelloRetryRequest is fed')
	}
	_, full_client_hello := build_client_hello(h.config)!
	h.update_transcript(full_client_hello)!

	h.state = .wait_sh
	return full_client_hello
}

// handle_handshake_message processes one complete encrypted-flight message:
// EE, optional CertificateRequest, Certificate, CertificateVerify, or Finished.
fn (mut h HandshakeCore) handle_handshake_message(full_msg []u8) ![]HandshakeMessage {
	// The message is complete by construction (reassembly framed it by its own
	// header), so peel type/length and split the body off.
	msg_type, body := split_handshake_message(full_msg) or {
		return h.fail_with(.decode_error, err)
	}
	if !h.is_handshake_message_accepted(msg_type) {
		return h.classify_failure(.unexpected_message, unexpected_error_hsk_message(msg_type))
	}

	match msg_type {
		.encrypted_extensions { return h.handle_encrypted_extensions(full_msg, body) }
		.certificate_request { return h.handle_certificate_request(full_msg, body) }
		.certificate { return h.handle_certificate(full_msg, body) }
		.certificate_verify { return h.handle_certificate_verify(full_msg, body) }
		.finished { return h.handle_finished(full_msg, body) }
		else {
			return h.classify_failure(.unexpected_message, unexpected_error_hsk_message(msg_type))
		}
	}
}

// handle_server_hello_message consumes one complete initial-phase ServerHello
// or HelloRetryRequest and returns the follow-up messages (RFC 8446 §4.1.3).
fn (mut h HandshakeCore) handle_server_hello_message(sh_msg []u8) ![]HandshakeMessage {
	if h.state != .wait_sh {
		// Strict ordering: a ServerHello is only legal while we await it (§4.4.1).
		return h.classify_failure(.unexpected_message, 'handshake: unexpected ServerHello in state ${h.state()}')
	}

	// Peel the 4-byte handshake header (type || 3-byte length) to bound the body (§4).
	msg_type, body := split_handshake_message(sh_msg) or {
		return h.fail_with(.decode_error, err)
	}
	if msg_type != .server_hello {
		// HelloRetryRequest reuses the ServerHello type value (2) and is told apart
		// only by the sentinel random (§4.1.3).
		return h.classify_failure(.unexpected_message, 'handshake: expected ServerHello (2), got ${msg_type}')
	}
	// Transport framing boundaries are enforced by adapters; reassembly hands this
	// method one complete handshake message.
	// §4.1.3 field decode plus the §4.2 extension allowlist.
	sh := parse_server_hello(body) or {
		return h.fail_with(.decode_error, err)
	}
	// Re-frame header + body so the transcript covers the exact wire layout.
	full_sh_msg := wrap_handshake_message(.server_hello, body)

	if sh.is_hello_retry_req {
		return h.handle_hello_retry_request(sh, full_sh_msg)
	}

	// Normal ServerHello: validate echoed fields and that a key share was selected
	// for our group; (EC)DHE cannot proceed without one.
	sh.validate(&h.config) or { return h.fail_with(.illegal_parameter, err) }
	// §4.1.4: after a HelloRetryRequest the ServerHello suite must match the HRR's.
	if hrr_suite := h.hrr_cipher_suite {
		if hrr_suite != sh.cipher_suite {
			return h.classify_failure(.illegal_parameter, 'handshake: ServerHello cipher suite ${sh.cipher_suite} differs from HelloRetryRequest suite ${hrr_suite} (illegal_parameter)')
		}
	}
	if !sh.has_key_share {
		return h.classify_failure(.illegal_parameter, 'handshake: ServerHello missing key_share extension')
	}
	if sh.server_share.group != h.ecdhe_group {
		return h.classify_failure(.illegal_parameter, 'handshake: ServerHello key_share group ${sh.server_share.group} does not match offered group ${h.ecdhe_group}')
	}

	// The suite fixes both hash and key length; set them before deriving secrets.
	h.cipher_suite = sh.cipher_suite
	h.key_len = suite_key_len(sh.cipher_suite)
	h.set_hash_algo(suite_hash_algo(sh.cipher_suite))
	h.update_transcript(full_sh_msg)!

	// §7.1 handshake phase: derive the Handshake Secret and the client/server handshake traffic secrets.
	mut shared_secret := h.compute_shared_secret(sh.server_share)!
	defer {
		wipe_secret(mut shared_secret)
	}

	// The handshake secret is retained because the Master Secret derives from it.
	th_ch_sh := h.transcript.current_hash()
	// Derive the retained Handshake Secret, then each direction's traffic secret from it.
	// Stored to h.handshake_secret, so it cannot be defer-wiped (a defer would zero the
	// buffer the field still needs); each expansion wipes it on its own error path instead.
	mut hs_secret := derive_handshake_secret(h.hash_algo, shared_secret)!
	c_hs := derive_client_hs_secret(h.hash_algo, hs_secret, th_ch_sh) or {
		wipe_secret(mut hs_secret)
		return err
	}
	s_hs := derive_server_hs_secret(h.hash_algo, hs_secret, th_ch_sh) or {
		wipe_secret(mut hs_secret)
		return err
	}
	h.handshake_secret = hs_secret
	h.c_hsk_tsec = c_hs
	h.s_hsk_tsec = s_hs

	h.state = .wait_ee
	return []HandshakeMessage{}
}

// handle_hello_retry_request validates a HelloRetryRequest, rewrites the
// transcript, and returns the retried ClientHello (RFC 8446 §4.1.4).
fn (mut h HandshakeCore) handle_hello_retry_request(sh ServerHello, full_sh_msg []u8) ![]HandshakeMessage {
	// §4.1.4: validate shared fields, reject a second HelloRetryRequest, and
	// require that the request actually changes something.
	sh.validate(&h.config) or { return h.fail_with(.illegal_parameter, err) }
	if h.hrr_cipher_suite != none {
		return h.classify_failure(.unexpected_message, 'handshake: received second HelloRetryRequest (unexpected_message)')
	}
	if !sh.has_key_share && sh.cookie.len == 0 {
		return h.classify_failure(.illegal_parameter, 'handshake: HelloRetryRequest would not change the ClientHello (illegal_parameter)')
	}
	if sh.has_key_share {
		// The requested group must be offered and differ from the one already sent a key share for (§4.2.8).
		if sh.server_share.group !in offered_groups(&h.config) {
			return h.classify_failure(.illegal_parameter, 'handshake: HelloRetryRequest requested unoffered group ${sh.server_share.group} (illegal_parameter)')
		}
		if sh.server_share.group == h.config.client_share.group {
			return h.classify_failure(.illegal_parameter, 'handshake: HelloRetryRequest reselected the already-offered group ${sh.server_share.group} (illegal_parameter)')
		}
	}

	// §4.1.4: the final ServerHello must reuse this cipher suite; presence also
	// marks that a retry has already been accepted.
	h.hrr_cipher_suite = sh.cipher_suite
	// §4.4.1: the message_hash uses the HRR suite's hash, so switch before applying the HRR.
	h.set_hash_algo(suite_hash_algo(sh.cipher_suite))
	h.transcript.apply_hello_retry_request(full_sh_msg)!

	return h.build_second_client_hello(sh.has_key_share, sh.server_share.group, sh.cookie)!
}

// build_second_client_hello builds the ClientHello retried after a
// HelloRetryRequest: identical to the first except key_share and cookie
// (RFC 8446 §4.1.2).
fn (mut h HandshakeCore) build_second_client_hello(change_group bool, group NamedGroup, cookie []u8) ![]HandshakeMessage {
	if change_group {
		h.reset_key_share(group)!
		h.config.client_share = KeyShareEntry{
			group:        group
			key_exchange: h.public_key_bytes()!
		}
	}
	// A cookie-only HelloRetryRequest keeps the original key share; the cookie is
	// echoed verbatim (§4.1.4/§4.2.2).
	h.config.cookie = cookie
	_, full_ch2 := build_client_hello(h.config)!
	// Hash the retry into the already message_hash-rewritten transcript so the
	// Finished MAC covers the retried flight (§4.4.1).
	h.update_transcript(full_ch2)!

	h.state = .wait_sh // rearm: the server now answers with the real ServerHello
	return [new_handshake_message(full_ch2)!]
}

// handle_encrypted_extensions records ALPN and QUIC transport parameters.
fn (mut h HandshakeCore) handle_encrypted_extensions(full_msg []u8, body []u8) ![]HandshakeMessage {
	alpn, transport_parameters := parse_encrypted_extensions_for_transport(body,
		h.config.is_quic) or {
		return h.fail_with(.decode_error, err)
	}
	// RFC 7301 §3.1: the server must pick from the offered list, else an unoffered value
	// could steer the protocol below us. `in` is exact element membership — a substring
	// test would let a hostile "1.1" pass against the offer "http/1.1".
	if alpn != '' && !(alpn in h.config.alpn) {
		return h.classify_failure(.illegal_parameter, 'handshake: server selected ALPN "${alpn}" not offered by client')
	}
	h.negotiated_alpn = alpn
	h.quic_params = transport_parameters
	h.update_transcript(full_msg)!
	h.state = .wait_cert_cr
	return []HandshakeMessage{}
}

// handle_certificate_request answers a server request with an empty Certificate.
fn (mut h HandshakeCore) handle_certificate_request(full_msg []u8, body []u8) ![]HandshakeMessage {
	// §4.3.2: the server wants client authentication; this client holds no
	// credential and acknowledges with an empty Certificate (§4.4.2).
	req := parse_certificate_request(body) or {
		return h.fail_with(.decode_error, err)
	}
	h.update_transcript(full_msg)!
	h.state = .wait_cert

	// The empty Certificate-response (§4.4.2) enters the transcript BEFORE the
	// server's flight is verified, so the server Finished covers our response.
	response := build_empty_certificate_message(req.request_context)
	h.update_transcript(response)!
	return [new_handshake_message(response)!]
}

// handle_certificate parses and validates the server certificate chain.
fn (mut h HandshakeCore) handle_certificate(full_msg []u8, body []u8) ![]HandshakeMessage {
	// §4.4.1: the server Certificate follows EncryptedExtensions directly, or a
	// CertificateRequest may sit before it; is_handshake_message_accepted allows both.
	cert_ders := parse_certificate_message(body) or {
		return h.fail_with(.decode_error, err)
	}
	h.update_transcript(full_msg)!

	h.server_certs = []pki.Certificate{}
	for der in cert_ders {
		cert := pki.parse_certificate(der) or {
			return h.fail_with(.decode_error, err)
		}
		h.server_certs << cert
	}
	h.validator.verify_certificate_chain(h.server_certs, h.config.server_name) or {
		return h.fail_with(.bad_certificate, err)
	}
	h.state = .wait_cv
	return []HandshakeMessage{}
}

// handle_certificate_verify checks the server's signature over the transcript.
fn (mut h HandshakeCore) handle_certificate_verify(full_msg []u8, body []u8) ![]HandshakeMessage {
	cv := parse_certificate_verify(body) or {
		return h.fail_with(.decode_error, err)
	}
	signed_content := certificate_verify_signed_content(h.transcript.current_hash())
	h.update_transcript(full_msg)!

	if h.server_certs.len == 0 {
		return h.classify_failure(.decode_error, 'handshake: missing server certificate for verification')
	}
	verify_certificate_verify(leaf_key(h.server_certs[0]), cv, signed_content) or {
		return h.fail_with(.decrypt_error, err)
	}
	h.state = .wait_finished
	return []HandshakeMessage{}
}

// handle_finished verifies the server Finished and emits the client Finished.
fn (mut h HandshakeCore) handle_finished(full_msg []u8, body []u8) ![]HandshakeMessage {
	// Verify the server Finished over the transcript up to but excluding this message.
	server_verify_data := parse_finished(body, h.hash_algo.hash_len()) or {
		return h.fail_with(.decode_error, err)
	}
	th_before_sfin := h.transcript.current_hash()

	expected_verify_data := compute_finished_verify_data(h.hash_algo, h.s_hsk_tsec,
		th_before_sfin)!
	// Constant-time compare: verify_data is a MAC over the transcript, and a naive
	// compare would leak a byte-by-byte Finished forgery.
	if subtle.constant_time_compare(server_verify_data, expected_verify_data) != 1 {
		return h.classify_failure(.decrypt_error, 'handshake: server Finished verify_data mismatch')
	}
	h.update_transcript(full_msg)!

	// Advance the key schedule to the application phase: Master Secret, then the
	// application traffic secrets bound to the transcript including server Finished.
	th_sfin := h.transcript.current_hash()
	mut master_secret := derive_master_secret(h.hash_algo, h.handshake_secret)!
	// The Master Secret now crosses into the caller, so defer wipes it on every exit
	// path — including an error from the two expansions below — so it cannot leak.
	defer { wipe_secret(mut master_secret) }
	c_ap := derive_client_app_secret(h.hash_algo, master_secret, th_sfin)!
	s_ap := derive_server_app_secret(h.hash_algo, master_secret, th_sfin)!
	// §7.5: erase as soon as no longer required; the defer above is idempotent, so this
	// early wipe just shortens the happy-path lifetime. The Master Secret is never
	// stored — KeyUpdate (§7.2) chains from the traffic secrets alone.
	wipe_secret(mut master_secret)
	h.c_app_tsec = c_ap
	h.s_app_tsec = s_ap

	// The client Finished uses the handshake traffic secret over the transcript now
	// including server Finished, sent under the (still active) handshake keys.
	client_verify_data := compute_finished_verify_data(h.hash_algo, h.c_hsk_tsec,
		th_sfin)!
	client_fin_msg := build_finished(client_verify_data)
	h.update_transcript(client_fin_msg)!

	// RFC 8446 §4.6.1/§4.5.5.1 let the server coalesce NewSessionTicket (type 4)
	// or KeyUpdate (type 24) behind its Finished; those stay buffered for the
	// adapter's take_buffered_handshake_bytes. Any other leading byte is flight garbage.
	if h.reassembly.len > 0 {
		leading := h.reassembly[0]
		if leading != u8(4) && leading != u8(24) {
			return h.classify_failure(.decode_error, 'handshake: unexpected trailing data after server Finished in handshake flight')
		}
	}

	h.state = .connected
	// Forward-secrecy cleanup: every ephemeral/handshake secret consumed to reach
	// this point is now dead (RFC 8446 §7.5 / §1.3.1).
	h.wipe_key_schedule_secrets()
	// Hand the raw client Finished to the transport, which encrypts it under the
	// still-active handshake keys.
	return [new_handshake_message(client_fin_msg)!]
}

// receive_initial_bytes feeds pre-ServerHello (plaintext) handshake bytes —
// TLS records or Initial-packet CRYPTO bytes alike.
pub fn (mut h HandshakeCore) receive_initial_bytes(bytes []u8) ![]HandshakeMessage {
	return h.receive_bytes(bytes, .initial)!
}

// receive_handshake_bytes feeds bytes protected by the handshake traffic keys —
// TLS records or Handshake-packet CRYPTO bytes alike.
pub fn (mut h HandshakeCore) receive_handshake_bytes(bytes []u8) ![]HandshakeMessage {
	return h.receive_bytes(bytes, .handshake)!
}

// zeroize_traffic_secrets destroys all traffic secrets (handshake and application)
// once the connection is closed — retiring the record keys that derive from them
// (§7.5). Valid only after .connected.
pub fn (mut h HandshakeCore) zeroize_traffic_secrets() ! {
	if h.state != .connected {
		return error('handshake: zeroizing traffic secrets is only valid once the handshake reaches .connected (state: ${h.state()})')
	}
	wipe_secret(mut h.c_hsk_tsec)
	wipe_secret(mut h.s_hsk_tsec)
	wipe_secret(mut h.c_app_tsec)
	wipe_secret(mut h.s_app_tsec)
	// Scrub any leftover reassembly bytes so nothing survives past handshake
	// completion. take_buffered_handshake_bytes() (TLS-record-only) already drains
	// reassembly on the TLS path, but QUIC has no equivalent, so this covers both.
	wipe_secret(mut h.reassembly)
	h.reassembly = []u8{}
}

// set_hash_algo adopts the negotiated suite's hash, re-seeding the running digest
// from the replayed transcript on a change. A no-op when already matching — the
// common SHA-256 case, no rebuild.
fn (mut h HandshakeCore) set_hash_algo(algo HashAlgorithm) {
	if h.hash_algo == algo {
		return // already on the target digest, no rebuild
	}
	// Hash-algo changes happen only at the first ServerHello or a HelloRetryRequest,
	// where the transcript is just ClientHello1, so re-seeding the new digest from
	// the replay log is cheap. The running digest itself is never re-scanned.
	h.transcript.switch_digest(algo)
	h.hash_algo = algo
}
