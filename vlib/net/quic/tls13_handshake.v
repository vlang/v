module quic

import crypto.ecdsa
import crypto.sha256

// TlsAlert is the subset of RFC 8446 §6's alert descriptions this
// handshake actually produces -- not the full registry, matching this
// module's established narrow-but-real-values convention (see
// tls13_client_hello.v's sig_scheme_* constants).
pub enum TlsAlert {
	unexpected_message = 10
	handshake_failure  = 40
	bad_certificate    = 42
	illegal_parameter  = 47
	decode_error       = 50
	decrypt_error      = 51
}

// tls_alert_to_quic_error implements RFC 9001 §4.8: "If TLS produces an
// alert, QUIC MUST convert it into a QUIC CONNECTION_CLOSE error. The alert
// description ... is added to 0x100 to produce a QUIC error code." This
// pure-V implementation never runs an actual TLS alert-producing library,
// so this handshake's own failure paths pick the RFC 8446 §6 alert a
// compliant peer would have raised for the equivalent condition, and this
// function does the same 0x100 translation a real TLS-stack-to-QUIC
// bridge would.
pub fn tls_alert_to_quic_error(alert TlsAlert) u64 {
	return 0x100 + u64(alert)
}

// handshake_error builds an IError carrying the QUIC CONNECTION_CLOSE error
// code a caller building that frame needs (via `.code()`), rather than a
// plain error string a caller would have to re-classify. Every fatal
// failure path in this file goes through this, never a bare `error(...)`.
fn handshake_error(alert TlsAlert, msg string) IError {
	return error_with_code(msg, int(tls_alert_to_quic_error(alert)))
}

// ClientHandshakeState tracks which handshake message this client is
// waiting to receive next (RFC 8446 §4.1.4's message flow, minus the
// PSK/0-RTT-only states v1 never reaches). `wait_certificate` covers BOTH
// a Certificate and a CertificateRequest arriving next -- RFC 8446 permits
// either, and process_certificate_or_request distinguishes them by message
// type rather than the state machine splitting into two states for a
// message type v1 always rejects anyway.
pub enum ClientHandshakeState {
	wait_server_hello
	wait_encrypted_extensions
	wait_certificate
	wait_certificate_verify
	wait_finished
	connected
}

// ClientHandshakeParams is everything Tls13ClientHandshake.start needs
// beyond what's fixed by v1's scope decisions (single cipher suite, single
// named group -- see tls13_client_hello.v).
pub struct ClientHandshakeParams {
pub:
	random               []u8 // exactly 32 bytes; caller supplies so callers can use a real CSPRNG while tests stay deterministic
	server_name          string
	transport_parameters QuicTransportParameters // this client's own offered set
	ca_bundle_pem        string                  // trust anchor for the server's certificate chain
}

// Tls13ClientHandshake drives a single QUIC-scoped TLS 1.3 client
// handshake (RFC 8446, restricted to the subset RFC 9001 needs) from
// ClientHello construction through the client's own Finished message.
// Callers feed it each handshake message as QUIC's CRYPTO stream delivers
// it (Phase 4's job, not yet built) and get back either the next message
// to send or an error carrying the QUIC CONNECTION_CLOSE code to use (see
// handshake_error). The caller MUST call `free()` when done, successful or
// not -- ecdhe_private and verified_chain both own C/OpenSSL-heap
// resources with no GC visibility.
//
// ANY error from a process_* method is fatal to the whole handshake, not
// just to that one call: the caller must tear down the QUIC connection
// (using the returned error's `.code()`, a CONNECTION_CLOSE error per
// RFC 9001 §4.8) and must not call any further process_* method on this
// object -- only free() remains safe to call. This is not merely
// convention: some failure paths (e.g. a key-schedule derivation error in
// process_server_hello, practically unreachable given this handshake's
// fixed-size inputs, but not structurally excluded) already accumulate a
// message into the transcript before the call that can still fail, so the
// object's internal state is not guaranteed consistent enough to resume
// from after an error.
pub struct Tls13ClientHandshake {
mut:
	state ClientHandshakeState
	// Running concatenation of every handshake message's bytes (header
	// included, RFC 8446 §4.4.1 "Messages"), in RFC 8446 §4.4.1 order --
	// no QUIC/TLS record-layer framing, since QUIC has none. Re-hashed
	// (not incrementally hashed) at each checkpoint that needs a
	// Transcript-Hash: simpler than a streaming hash state, and a full
	// handshake's messages are a handful of KB, not a hot path.
	transcript []u8
	// Set once a HelloRetryRequest is seen, so a second one can be
	// rejected (RFC 8446 §4.1.4). Note: this handshake does not yet
	// generate a valid ClientHello2 in response to a first HRR (see
	// process_server_hello's own doc comment) -- ClientHello1's bytes are
	// therefore not separately retained yet either; RFC 8446 §4.4.1's
	// synthetic message_hash substitution is a follow-up's job once HRR
	// response generation exists.
	got_hello_retry_request bool
	ecdhe_private           ecdsa.PrivateKey
	ca_bundle_pem           string
	handshake_secrets       HandshakeSecrets
	application_secrets     ApplicationSecrets
	verified_chain          &VerifiedCertificateChain = unsafe { nil }
	// Transcript-Hash(ClientHello...Certificate) -- RFC 8446 §4.4.3's
	// "Transcript-Hash(Handshake Context, Certificate)" input to
	// certificate_verify_signed_content. Computed when Certificate is
	// processed, consumed when CertificateVerify is processed next.
	certificate_transcript_hash []u8
	// Guards free() against a second call -- see free()'s own doc comment
	// for why this can't just be "check whether ecdhe_private was already
	// freed" the way VerifiedCertificateChain.free() checks its own
	// pointer: ecdsa.PrivateKey.free() has no such self-check, and calling
	// it twice on the same value is a real, empirically-confirmed crash
	// (OpenSSL's EVP_PKEY_free aborts on a double-free of the same
	// pointer -- unlike the mbedTLS heap elsewhere in this codebase, which
	// doesn't reliably crash on a double-free of a similar size).
	freed bool
pub mut:
	peer_transport_parameters QuicTransportParameters
}

// state returns which handshake message this client is currently waiting
// to receive.
pub fn (h &Tls13ClientHandshake) state() ClientHandshakeState {
	return h.state
}

// application_secrets returns the derived 1-RTT traffic secrets. Only
// meaningful once state() == .connected -- Phase 3's job to turn these
// into actual AEAD keys via hkdf_expand_label's "quic key"/"quic iv"
// labels.
pub fn (h &Tls13ClientHandshake) application_secrets() ApplicationSecrets {
	return h.application_secrets
}

// handshake_secrets returns the derived Handshake-level traffic secrets.
// Meaningful once state() has advanced past .wait_server_hello -- unlike
// application_secrets, needed mid-handshake (Phase 3's job to install
// Handshake-level packet protection keys as soon as ServerHello arrives,
// before the rest of the handshake completes).
pub fn (h &Tls13ClientHandshake) handshake_secrets() HandshakeSecrets {
	return h.handshake_secrets
}

// free releases ecdhe_private (an OpenSSL EVP_PKEY, Phase 1's
// crypto.ecdsa) and verified_chain (an mbedTLS certificate chain, Phase
// 2c's net.mbedtls), if either was ever allocated. Idempotent: safe to
// call more than once -- guarded by `freed` rather than checking each
// owned resource's own pointer (VerifiedCertificateChain.free() can do
// that, since it nulls its own pointer after freeing, but
// ecdsa.PrivateKey.free() has no equivalent self-guard, so a bare second
// h.ecdhe_private.free() call would still double-free even if
// verified_chain's own check were copied here).
pub fn (mut h Tls13ClientHandshake) free() {
	if h.freed {
		return
	}
	h.freed = true
	h.ecdhe_private.free()
	if h.verified_chain != unsafe { nil } {
		h.verified_chain.free()
	}
}

// accumulate appends one already-length-framed handshake message (as
// encode_handshake_message/build_client_hello produce, or the raw
// 4-byte-header-plus-body slice parse_handshake_message consumed) to the
// running transcript.
fn (mut h Tls13ClientHandshake) accumulate(framed_message []u8) {
	h.transcript << framed_message
}

fn (h &Tls13ClientHandshake) transcript_hash() []u8 {
	return sha256.sum256(h.transcript)
}

// Tls13ClientHandshake.start generates this client's ephemeral ECDHE
// keypair, builds ClientHello (RFC 8446 §4.1.2), and returns both the new
// handshake object (state .wait_server_hello) and the ClientHello bytes to
// send. `p.transport_parameters` is validated by build_client_hello itself
// (rejects any server-only parameter) before anything is allocated.
pub fn Tls13ClientHandshake.start(p ClientHandshakeParams) !(&Tls13ClientHandshake, []u8) {
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

	client_hello := build_client_hello(ClientHelloParams{
		random:               p.random
		server_name:          p.server_name
		ecdhe_public_key:     ecdhe_public_bytes
		transport_parameters: p.transport_parameters
	}) or {
		ecdhe_private.free()
		return handshake_error(.handshake_failure,
			'quic: failed to build ClientHello: ${err.msg()}')
	}

	mut h := &Tls13ClientHandshake{
		state:         .wait_server_hello
		transcript:    client_hello.clone()
		ecdhe_private: ecdhe_private
		ca_bundle_pem: p.ca_bundle_pem
	}
	return h, client_hello
}

// process_server_hello handles the message immediately following
// ClientHello, which per RFC 8446 §4.1.3/§4.1.4 is either a real
// ServerHello or a HelloRetryRequest (the same wire type, distinguished by
// a magic `random` value). Returns the derived Handshake-level secrets
// once a real ServerHello completes the ECDHE exchange; Phase 3's job to
// turn `.client_secret`/`.server_secret` into actual packet-protection
// keys, once it exists.
pub fn (mut h Tls13ClientHandshake) process_server_hello(msg HandshakeMessage, framed_message []u8) !HandshakeSecrets {
	if h.state != .wait_server_hello {
		return handshake_error(.unexpected_message,
			'quic: received ServerHello/HelloRetryRequest while in state ${h.state}')
	}
	if msg.typ != .server_hello {
		return handshake_error(.unexpected_message,
			'quic: expected ServerHello or HelloRetryRequest, got ${msg.typ}')
	}
	parsed := parse_server_hello(msg.body) or { return handshake_error(.decode_error, err.msg()) }

	match parsed {
		ParsedHelloRetryRequest {
			// RFC 8446 §4.1.4: "if a client receives a second
			// HelloRetryRequest in the same connection ... it MUST abort
			// the handshake with an "unexpected_message" alert."
			if h.got_hello_retry_request {
				return handshake_error(.unexpected_message,
					'quic: received a second HelloRetryRequest')
			}
			if parsed.cipher_suite != cipher_suite_tls_aes_128_gcm_sha256 {
				return handshake_error(.illegal_parameter,
					'quic: HelloRetryRequest cipher_suite 0x${parsed.cipher_suite:04x} was not offered')
			}
			if parsed.selected_group != named_group_secp256r1 {
				// v1 offers only secp256r1 (Phase 1's OpenSSL-backed ECDH
				// support) -- a server asking for anything else cannot be
				// satisfied, and there is no fallback group to try.
				return handshake_error(.handshake_failure,
					'quic: HelloRetryRequest selected an unsupported group 0x${parsed.selected_group:04x} (v1 only offers secp256r1)')
			}
			h.got_hello_retry_request = true
			// Regenerating ClientHello2 (a fresh ECDHE keypair, echoing any
			// cookie extension, re-hashing the transcript per RFC 8446
			// §4.4.1's synthetic message_hash substitution) needs
			// build_client_hello to support a cookie extension it does not
			// speak yet -- deliberately deferred to a follow-up rather than
			// half-built here (see PROGRESS.md). A first HelloRetryRequest
			// is therefore reported as unimplemented, not silently
			// mishandled.
			return handshake_error(.handshake_failure,
				'quic: HelloRetryRequest handling is not yet implemented (only second-HRR rejection is)')
		}
		ParsedServerHello {
			if parsed.cipher_suite != cipher_suite_tls_aes_128_gcm_sha256 {
				return handshake_error(.illegal_parameter,
					'quic: ServerHello cipher_suite 0x${parsed.cipher_suite:04x} was not offered')
			}
			if parsed.key_share_group != named_group_secp256r1 {
				return handshake_error(.illegal_parameter,
					'quic: ServerHello key_share selected an unoffered group 0x${parsed.key_share_group:04x}')
			}

			peer_public := ecdsa.PublicKey.from_uncompressed_bytes(parsed.key_share_key_exchange,
				nid: .prime256v1
			) or {
				return handshake_error(.decode_error,
					'quic: malformed ServerHello key_share: ${err.msg()}')
			}
			defer {
				peer_public.free()
			}
			shared_secret := h.ecdhe_private.derive_shared_secret(peer_public) or {
				return handshake_error(.decrypt_error,
					'quic: ECDHE shared secret derivation failed: ${err.msg()}')
			}

			h.accumulate(framed_message)
			early_secret := derive_early_secret() or {
				return handshake_error(.handshake_failure, err.msg())
			}
			h.handshake_secrets = derive_handshake_secrets(early_secret, shared_secret,
				h.transcript_hash()) or { return handshake_error(.handshake_failure, err.msg()) }
			h.state = .wait_encrypted_extensions
			return h.handshake_secrets
		}
	}
}

// process_encrypted_extensions handles EncryptedExtensions (RFC 8446
// §4.3.1), the first message protected under the Handshake-level keys.
// `peer_initial_scid` is the Source Connection ID this client actually
// observed on the server's first Initial/Handshake packet -- RFC 9001
// §8.2 requires the `initial_source_connection_id` transport parameter to
// match it exactly, a check this function does since it is the first
// point that parameter's value is available; the SCID itself comes from
// Phase 4/9 (packet headers, QuicConn), not yet built, so callers must
// supply it explicitly until then.
pub fn (mut h Tls13ClientHandshake) process_encrypted_extensions(msg HandshakeMessage, framed_message []u8, peer_initial_scid []u8) ! {
	if h.state != .wait_encrypted_extensions {
		return handshake_error(.unexpected_message,
			'quic: received EncryptedExtensions while in state ${h.state}')
	}
	if msg.typ != .encrypted_extensions {
		return handshake_error(.unexpected_message,
			'quic: expected EncryptedExtensions, got ${msg.typ}')
	}
	extensions := parse_encrypted_extensions(msg.body) or {
		return handshake_error(.decode_error, err.msg())
	}
	tp_ext := find_extension(extensions, ext_quic_transport_parameters) or {
		return handshake_error(.handshake_failure,
			'quic: EncryptedExtensions missing mandatory quic_transport_parameters extension')
	}
	peer_params := decode_transport_parameters(tp_ext.data) or {
		return handshake_error(.decode_error,
			'quic: malformed quic_transport_parameters: ${err.msg()}')
	}
	scid := peer_params.initial_source_connection_id or {
		return handshake_error(.handshake_failure,
			'quic: peer transport parameters missing mandatory initial_source_connection_id')
	}

	if scid != peer_initial_scid {
		return handshake_error(.handshake_failure,
			'quic: initial_source_connection_id in transport parameters does not match the packet header SCID')
	}

	h.accumulate(framed_message)
	h.peer_transport_parameters = peer_params
	h.state = .wait_certificate
}

// process_certificate_or_request handles the message immediately following
// EncryptedExtensions, which per RFC 8446 §4.1.4 is either an optional
// CertificateRequest or the mandatory Certificate. v1 supports no
// client-cert auth (there is no client identity to offer), so a
// CertificateRequest is rejected outright here rather than answered with
// an empty Certificate -- PROGRESS.md's stated scope for this. The
// verified chain is stored internally (`h.verified_chain`); the caller has
// no other way to reach it, since that field is private -- the only
// observable effect of success is the state advancing to
// .wait_certificate_verify.
pub fn (mut h Tls13ClientHandshake) process_certificate_or_request(msg HandshakeMessage, framed_message []u8) ! {
	if h.state != .wait_certificate {
		return handshake_error(.unexpected_message,
			'quic: received Certificate/CertificateRequest while in state ${h.state}')
	}
	if msg.typ == .certificate_request {
		return handshake_error(.handshake_failure,
			'quic: server requested client certificate authentication, which v1 does not support')
	}
	if msg.typ != .certificate {
		return handshake_error(.unexpected_message, 'quic: expected Certificate, got ${msg.typ}')
	}
	parsed := parse_certificate(msg.body) or { return handshake_error(.decode_error, err.msg()) }
	// RFC 8446 §4.4.2: "certificate_request_context: ... Otherwise (in the
	// case of server authentication), this field SHALL be zero length."
	// v1 never sends a CertificateRequest of its own (no post-handshake
	// auth, out of scope), so every Certificate this client ever receives
	// is exactly that "server authentication" case -- a non-empty context
	// here is always a MUST-violation, not something that depends on
	// caller-supplied state the way parse_certificate's own doc comment
	// defers cipher_suite/version-style checks to this layer.
	if parsed.certificate_request_context.len != 0 {
		return handshake_error(.illegal_parameter,
			'quic: Certificate certificate_request_context must be empty for server authentication, got ${parsed.certificate_request_context.len} bytes')
	}
	verified := verify_server_certificate_chain(parsed, h.ca_bundle_pem) or {
		return handshake_error(.bad_certificate, err.msg())
	}

	h.accumulate(framed_message)
	h.verified_chain = verified
	h.certificate_transcript_hash = h.transcript_hash()
	h.state = .wait_certificate_verify
}

// process_certificate_verify handles CertificateVerify (RFC 8446 §4.4.3),
// checking the server's signature over
// Transcript-Hash(ClientHello...Certificate) (captured when Certificate
// was processed, per that message type's own §4.4.3 requirement) against
// the verified chain's leaf public key.
pub fn (mut h Tls13ClientHandshake) process_certificate_verify(msg HandshakeMessage, framed_message []u8) ! {
	if h.state != .wait_certificate_verify {
		return handshake_error(.unexpected_message,
			'quic: received CertificateVerify while in state ${h.state}')
	}
	if msg.typ != .certificate_verify {
		return handshake_error(.unexpected_message,
			'quic: expected CertificateVerify, got ${msg.typ}')
	}
	cv := parse_certificate_verify(msg.body) or { return handshake_error(.decode_error, err.msg()) }
	h.verified_chain.verify_certificate_verify_signature(cv, .server, h.certificate_transcript_hash) or {
		return handshake_error(.decrypt_error, err.msg())
	}

	h.accumulate(framed_message)
	h.state = .wait_finished
}

// process_finished handles the server's Finished (RFC 8446 §4.4.4),
// verifying its verify_data against Transcript-Hash(ClientHello...
// CertificateVerify), then derives the application traffic secrets and
// computes+returns this client's own Finished message (framed, ready to
// send) -- both keyed off the transcript hash as it stood at the moment
// each computation needed it, not the final post-client-Finished
// transcript.
pub fn (mut h Tls13ClientHandshake) process_finished(msg HandshakeMessage, framed_message []u8) !([]u8, ApplicationSecrets) {
	if h.state != .wait_finished {
		return handshake_error(.unexpected_message,
			'quic: received Finished while in state ${h.state}')
	}
	if msg.typ != .finished {
		return handshake_error(.unexpected_message, 'quic: expected Finished, got ${msg.typ}')
	}
	ok := verify_finished(h.handshake_secrets.server_secret, h.transcript_hash(), msg.body) or {
		return handshake_error(.decrypt_error, err.msg())
	}
	if !ok {
		return handshake_error(.decrypt_error, 'quic: server Finished verify_data does not match')
	}

	h.accumulate(framed_message)
	h.application_secrets = derive_application_secrets(h.handshake_secrets.handshake_secret,
		h.transcript_hash()) or { return handshake_error(.handshake_failure, err.msg()) }

	client_verify_data := compute_finished_verify_data(h.handshake_secrets.client_secret,
		h.transcript_hash()) or { return handshake_error(.handshake_failure, err.msg()) }
	client_finished := encode_handshake_message(.finished, client_verify_data) or {
		return handshake_error(.handshake_failure, err.msg())
	}
	h.accumulate(client_finished)

	h.state = .connected
	return client_finished, h.application_secrets
}
