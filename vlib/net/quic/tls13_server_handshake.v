module quic

import crypto.ecdsa
import crypto.sha256

// ServerHandshakeState tracks a server-role handshake's progress. Simpler
// than ClientHandshakeState (tls13_handshake.v): a server builds its ENTIRE
// response flight (ServerHello through its own Finished) synchronously in
// one call, with no peer message arriving in between to wait on -- there is
// no wait_encrypted_extensions/wait_certificate/wait_certificate_verify
// equivalent, since this server never RECEIVES those message types.
pub enum ServerHandshakeState {
	wait_finished
	connected
}

// ServerHandshakeParams is everything
// Tls13ServerHandshake.respond_to_client_hello needs beyond what's fixed by
// v1's scope decisions (single cipher suite, single named group -- see
// tls13_client_hello.v).
pub struct ServerHandshakeParams {
pub:
	// This SERVER's own transport parameters (own SCID via
	// initial_source_connection_id, the client's observed Initial DCID via
	// original_destination_connection_id -- both validated as present by
	// build_encrypted_extensions itself, RFC 9000 §7.3).
	transport_parameters QuicTransportParameters
	// Application protocols this server supports, most preferred first --
	// RFC 7301 §3.2: the SERVER picks, from among protocols the client also
	// offered, in the SERVER's own preference order (not just the first
	// thing the client happened to list first).
	supported_alpn_protocols []string
	// This server's own certificate chain, leaf-first (RFC 8446 §4.4.2).
	// Loading these from a PEM file is a future caller's job (13e's
	// h3_server.v wiring), not this state machine's.
	certificate_chain []CertificateEntry
	// This server's own long-lived identity private key, matching the leaf
	// certificate's public key. UNLIKE the ephemeral ECDHE keypair this
	// state machine generates internally per-handshake (single-use, owned
	// and freed by the Tls13ServerHandshake object -- see free()),
	// signing_key is the caller's LONG-LIVED key, reused across every
	// incoming connection -- this state machine only ever BORROWS it for
	// one signing operation and never frees it.
	signing_key ecdsa.PrivateKey
	// Random bytes for this server's own ServerHello.random. Caller
	// supplies so a real caller can use a genuine CSPRNG while tests stay
	// deterministic, the same convention as ClientHandshakeParams.random.
	server_hello_random []u8
}

// ServerHandshakeFlight is everything a caller needs to actually send this
// server's response and install the resulting keys. `server_hello` MUST be
// sent under Initial-level packet protection; `handshake_messages`
// (EncryptedExtensions, Certificate, CertificateVerify, and this server's
// own Finished, concatenated in that order) MUST be sent under
// Handshake-level protection -- RFC 8446/9001 protect these two groups
// under different keys, so a caller cannot simply concatenate and send
// everything as one CRYPTO-stream write the way this state machine's
// internal transcript accounting does.
pub struct ServerHandshakeFlight {
pub:
	server_hello        []u8
	handshake_messages  []u8
	handshake_secrets   HandshakeSecrets
	application_secrets ApplicationSecrets
	negotiated_alpn     string
}

// Tls13ServerHandshake drives a single QUIC-scoped TLS 1.3 SERVER
// handshake, the mirror of Tls13ClientHandshake (tls13_handshake.v) for the
// other role. See that struct's own doc comment for the shared error/
// lifecycle contract (any process_* error is fatal to the whole handshake;
// the caller must call free() exactly once, successful or not).
pub struct Tls13ServerHandshake {
mut:
	state ServerHandshakeState
	// Running concatenation of every handshake message's bytes, same
	// convention as Tls13ClientHandshake.transcript -- populated once, at
	// construction time (respond_to_client_hello builds the entire
	// ClientHello...server-Finished prefix in one call), then only ever
	// extended by process_finished's own client Finished.
	transcript []u8
	// This server's own ephemeral ECDHE keypair, generated fresh per
	// handshake -- single-use, owned and freed by this object, unlike
	// ServerHandshakeParams.signing_key (see that field's own doc comment).
	ecdhe_private     ecdsa.PrivateKey
	handshake_secrets HandshakeSecrets
	freed             bool
pub mut:
	peer_transport_parameters QuicTransportParameters
}

// state returns which handshake message this server is currently waiting
// to receive.
pub fn (h &Tls13ServerHandshake) state() ServerHandshakeState {
	return h.state
}

// free releases ecdhe_private (an OpenSSL EVP_PKEY, Phase 1's
// crypto.ecdsa). Idempotent: safe to call more than once, guarded by
// `freed` -- the exact same double-free hazard and fix
// Tls13ClientHandshake.free() documents for its own identical field
// (ecdsa.PrivateKey.free() has no self-guard of its own).
pub fn (mut h Tls13ServerHandshake) free() {
	if h.freed {
		return
	}
	h.freed = true
	h.ecdhe_private.free()
}

fn (mut h Tls13ServerHandshake) accumulate(framed_message []u8) {
	h.transcript << framed_message
}

fn (h &Tls13ServerHandshake) transcript_hash() []u8 {
	return sha256.sum256(h.transcript)
}

// Tls13ServerHandshake.respond_to_client_hello processes a ClientHello (RFC
// 8446 §4.1.2) and builds this server's ENTIRE response flight in one call
// -- ServerHello through this server's own Finished -- since nothing from
// the peer arrives in between (RFC 8446 §4.1.4's server flow sends all of
// these back-to-back). Returns the new handshake object (state
// .wait_finished) and the flight to send; see ServerHandshakeFlight's own
// doc comment for why it is split into two byte groups rather than one.
//
// Every peer-input validation happens BEFORE any resource is allocated
// (the ephemeral ECDHE keypair, in particular) -- the same ordering
// Tls13ClientHandshake.start uses for its own ClientHello construction, so
// a rejected ClientHello never leaks a generated keypair through an error
// path.
//
// HelloRetryRequest is not sent here: if the ClientHello's key_share does
// not offer secp256r1, a real server would request it via a
// HelloRetryRequest (build_hello_retry_request, tls13_server_hello.v,
// already exists and is unit-tested at the message layer) -- wiring that
// into a full ClientHello2 round trip is deliberately deferred, the SAME
// scope choice tls13_handshake.v's own process_server_hello already
// documents for the client's identical first-HRR gap.
pub fn Tls13ServerHandshake.respond_to_client_hello(msg HandshakeMessage, framed_client_hello []u8, params ServerHandshakeParams) !(&Tls13ServerHandshake, ServerHandshakeFlight) {
	if msg.typ != .client_hello {
		return handshake_error(.unexpected_message, 'quic: expected ClientHello, got ${msg.typ}')
	}
	parsed := parse_client_hello(msg.body) or {
		// Same convention as every process_* method in tls13_handshake.v:
		// parse_client_hello carries its own specific QUIC error code (via
		// error_with_code) for the non-empty-legacy_session_id class of
		// failure; only a genuine structural parse failure (plain error(),
		// code 0) should be remapped to the generic decode_error alert
		// here.
		if err.code() != 0 {
			return err
		}
		return handshake_error(.decode_error, err.msg())
	}

	if cipher_suite_tls_aes_128_gcm_sha256 !in parsed.cipher_suites {
		return handshake_error(.handshake_failure,
			'quic: ClientHello did not offer TLS_AES_128_GCM_SHA256, the only cipher suite this server supports')
	}

	sv_ext := find_extension(parsed.extensions, ext_supported_versions) or {
		return handshake_error(.missing_extension,
			'quic: ClientHello missing mandatory supported_versions extension')
	}
	offered_versions := parse_supported_versions_from_client(sv_ext.data) or {
		return handshake_error(.decode_error, err.msg())
	}
	if tls_version_1_3 !in offered_versions {
		return handshake_error(.handshake_failure, 'quic: ClientHello did not offer TLS 1.3')
	}

	ks_ext := find_extension(parsed.extensions, ext_key_share) or {
		return handshake_error(.missing_extension,
			'quic: ClientHello missing mandatory key_share extension')
	}
	offered_shares := parse_key_share_extension_client(ks_ext.data) or {
		return handshake_error(.decode_error, err.msg())
	}
	mut client_key_exchange := []u8{}
	mut found_group := false
	for entry in offered_shares {
		if entry.group == named_group_secp256r1 {
			client_key_exchange = entry.key_exchange.clone()
			found_group = true
			break
		}
	}
	if !found_group {
		return handshake_error(.handshake_failure,
			'quic: ClientHello did not offer secp256r1 in key_share, and HelloRetryRequest-based group correction is not yet implemented')
	}

	sa_ext := find_extension(parsed.extensions, ext_signature_algorithms) or {
		return handshake_error(.missing_extension,
			'quic: ClientHello missing mandatory signature_algorithms extension')
	}
	offered_sig_algs := parse_signature_algorithms_extension_client(sa_ext.data) or {
		return handshake_error(.decode_error, err.msg())
	}
	// encode_certificate_verify (tls13_certificate.v) only signs
	// ecdsa_secp256r1_sha256 today -- RFC 8446 §4.4.3: "the signature
	// algorithm MUST be one offered in the [client's] 'signature_
	// algorithms' extension."
	if sig_scheme_ecdsa_secp256r1_sha256 !in offered_sig_algs {
		return handshake_error(.handshake_failure,
			'quic: ClientHello signature_algorithms does not include ecdsa_secp256r1_sha256, the only CertificateVerify algorithm this server can sign with')
	}

	alpn_ext := find_extension(parsed.extensions, ext_alpn) or {
		return handshake_error(.no_application_protocol,
			'quic: ClientHello missing mandatory alpn extension')
	}
	offered_alpn := decode_alpn_offer(alpn_ext.data) or {
		return handshake_error(.decode_error, err.msg())
	}
	mut negotiated_alpn := ''
	for candidate in params.supported_alpn_protocols {
		if candidate in offered_alpn {
			negotiated_alpn = candidate
			break
		}
	}
	if negotiated_alpn == '' {
		return handshake_error(.no_application_protocol,
			'quic: no ALPN protocol in common between this server and the ClientHello offer')
	}

	tp_ext := find_extension(parsed.extensions, ext_quic_transport_parameters) or {
		return handshake_error(.missing_extension,
			'quic: ClientHello missing mandatory quic_transport_parameters extension')
	}
	peer_params := decode_transport_parameters(tp_ext.data) or {
		return transport_parameter_error('quic: malformed quic_transport_parameters: ${err.msg()}')
	}
	if peer_params.initial_source_connection_id == none {
		return transport_parameter_error('quic: ClientHello transport parameters missing mandatory initial_source_connection_id')
	}
	// RFC 9000 §18.2: these four are server-only ("This transport parameter
	// is only sent by a server" / stateless_reset_token's "MUST NOT be sent
	// by a client") -- build_client_hello's own doc comment already
	// enforces the identical restriction from the sending side; this is
	// the RECEIVING side's mirror, the SAME division of labor as
	// process_encrypted_extensions's peer_params checks (this file) on the
	// client side.
	if peer_params.original_destination_connection_id != none {
		return transport_parameter_error('quic: ClientHello transport parameters must not include original_destination_connection_id (server-only)')
	}
	if peer_params.stateless_reset_token != none {
		return transport_parameter_error('quic: ClientHello transport parameters must not include stateless_reset_token (client MUST NOT send)')
	}
	if peer_params.preferred_address != none {
		return transport_parameter_error('quic: ClientHello transport parameters must not include preferred_address (server-only)')
	}
	if peer_params.retry_source_connection_id != none {
		return transport_parameter_error('quic: ClientHello transport parameters must not include retry_source_connection_id (server-only)')
	}

	// Every peer-input validation above has passed -- only internal
	// failures (ECDHE keygen, key-schedule/message-construction errors)
	// remain possible from here on, matching Tls13ClientHandshake.start's
	// identical validate-then-allocate ordering.
	ecdhe_public, ecdhe_private := ecdsa.generate_key(nid: .prime256v1) or {
		return handshake_error(.handshake_failure,
			'quic: failed to generate ephemeral ECDHE keypair: ${err.msg()}')
	}
	defer {
		ecdhe_public.free()
	}
	ecdhe_public_bytes := ecdhe_public.uncompressed_bytes() or {
		ecdhe_private.free()
		return handshake_error(.handshake_failure,
			'quic: failed to encode ephemeral ECDHE public key: ${err.msg()}')
	}

	client_public := ecdsa.PublicKey.from_uncompressed_bytes(client_key_exchange,
		nid: .prime256v1
	) or {
		ecdhe_private.free()
		return handshake_error(.decode_error, 'quic: malformed ClientHello key_share: ${err.msg()}')
	}
	defer {
		client_public.free()
	}
	shared_secret := ecdhe_private.derive_shared_secret(client_public) or {
		ecdhe_private.free()
		return handshake_error(.decrypt_error,
			'quic: ECDHE shared secret derivation failed: ${err.msg()}')
	}

	mut transcript := framed_client_hello.clone()

	server_hello := build_server_hello(
		random:           params.server_hello_random
		ecdhe_public_key: ecdhe_public_bytes
	) or {
		ecdhe_private.free()
		return handshake_error(.handshake_failure,
			'quic: failed to build ServerHello: ${err.msg()}')
	}
	transcript << server_hello

	early_secret := derive_early_secret() or {
		ecdhe_private.free()
		return handshake_error(.handshake_failure, err.msg())
	}
	handshake_secrets := derive_handshake_secrets(early_secret, shared_secret,
		sha256.sum256(transcript)) or {
		ecdhe_private.free()
		return handshake_error(.handshake_failure, err.msg())
	}

	mut acknowledge_server_name := false
	if _ := find_extension(parsed.extensions, ext_server_name) {
		acknowledge_server_name = true
	}

	encrypted_extensions := build_encrypted_extensions(
		transport_parameters:    params.transport_parameters
		selected_alpn:           negotiated_alpn
		acknowledge_server_name: acknowledge_server_name
	) or {
		ecdhe_private.free()
		return handshake_error(.handshake_failure,
			'quic: failed to build EncryptedExtensions: ${err.msg()}')
	}
	transcript << encrypted_extensions

	certificate := encode_certificate(params.certificate_chain) or {
		ecdhe_private.free()
		return handshake_error(.handshake_failure,
			'quic: failed to build Certificate: ${err.msg()}')
	}
	transcript << certificate
	// Transcript-Hash(ClientHello...Certificate) -- RFC 8446 §4.4.3's
	// "Transcript-Hash(Handshake Context, Certificate)" input to
	// certificate_verify_signed_content, computed at exactly this
	// checkpoint, the same moment Tls13ClientHandshake captures its own
	// certificate_transcript_hash when processing the server's Certificate.
	certificate_transcript_hash := sha256.sum256(transcript)

	certificate_verify := encode_certificate_verify(sig_scheme_ecdsa_secp256r1_sha256,
		params.signing_key, certificate_transcript_hash) or {
		ecdhe_private.free()
		return handshake_error(.handshake_failure,
			'quic: failed to build CertificateVerify: ${err.msg()}')
	}
	transcript << certificate_verify

	server_finished := build_finished(handshake_secrets.server_secret, sha256.sum256(transcript)) or {
		ecdhe_private.free()
		return handshake_error(.handshake_failure, err.msg())
	}
	transcript << server_finished

	// RFC 8446 §7.1/Figure 3: both application traffic secrets derive from
	// Transcript-Hash(ClientHello...server Finished) -- available as soon
	// as THIS server's own Finished is built, with no dependency on the
	// client's Finished arriving. This is what lets a server send Half-RTT
	// application data (RFC 8446 §4.4.4) before the handshake fully
	// completes; this state machine doesn't build a Half-RTT sender, but
	// the underlying key-derivation timing is the same fact.
	application_secrets := derive_application_secrets(handshake_secrets.handshake_secret,
		sha256.sum256(transcript)) or {
		ecdhe_private.free()
		return handshake_error(.handshake_failure, err.msg())
	}

	mut h := &Tls13ServerHandshake{
		state:                     .wait_finished
		transcript:                transcript
		ecdhe_private:             ecdhe_private
		handshake_secrets:         handshake_secrets
		peer_transport_parameters: peer_params
	}

	mut handshake_messages := []u8{}
	handshake_messages << encrypted_extensions
	handshake_messages << certificate
	handshake_messages << certificate_verify
	handshake_messages << server_finished

	return h, ServerHandshakeFlight{
		server_hello:        server_hello
		handshake_messages:  handshake_messages
		handshake_secrets:   handshake_secrets
		application_secrets: application_secrets
		negotiated_alpn:     negotiated_alpn
	}
}

// process_finished handles the client's Finished (RFC 8446 §4.4.4),
// verifying its verify_data against Transcript-Hash(ClientHello...server
// Finished) -- the SAME checkpoint respond_to_client_hello already used to
// derive Application secrets, since the client's Finished does not extend
// that transcript prefix (a Finished message's own verify_data covers
// everything BEFORE itself, never itself). Unlike
// Tls13ClientHandshake.process_finished, this returns nothing new to send
// or derive -- respond_to_client_hello already produced both traffic
// secrets; this call is purely the confirmation checkpoint (RFC 8446's
// "the handshake is confirmed" moment from the server's side, mirroring
// RFC 9001 §4.1.2's HANDSHAKE_DONE trigger condition, though sending that
// frame is the caller's job, not this state machine's).
pub fn (mut h Tls13ServerHandshake) process_finished(msg HandshakeMessage, framed_message []u8) ! {
	if h.state != .wait_finished {
		return handshake_error(.unexpected_message,
			'quic: received Finished while in state ${h.state}')
	}
	if msg.typ != .finished {
		return handshake_error(.unexpected_message, 'quic: expected Finished, got ${msg.typ}')
	}
	ok := verify_finished(h.handshake_secrets.client_secret, h.transcript_hash(), msg.body) or {
		return handshake_error(.decrypt_error, err.msg())
	}
	if !ok {
		return handshake_error(.decrypt_error, 'quic: client Finished verify_data does not match')
	}

	h.accumulate(framed_message)
	h.state = .connected
}
