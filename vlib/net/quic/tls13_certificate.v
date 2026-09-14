module quic

import crypto.ecdsa

// CertificateEntry is one X.509 certificate plus its per-certificate
// extensions (RFC 8446 §4.4.2). v1 only speaks the X509 CertificateType —
// RawPublicKey (RFC 7250) is never negotiated (v1's EncryptedExtensions
// parsing doesn't send/accept the certificate-type extensions that would
// select it), so `cert_data` is always a DER-encoded X.509 certificate.
pub struct CertificateEntry {
pub:
	cert_data  []u8
	extensions []TlsExtension
}

pub struct ParsedCertificate {
pub:
	certificate_request_context []u8
	certificate_list            []CertificateEntry
}

// parse_certificate parses a Certificate handshake message BODY (RFC 8446
// §4.4.2). v1 is client-only and never requests client-cert auth
// (CertificateRequest is rejected outright per PROGRESS.md), so this
// function only ever parses a SERVER's Certificate message — which RFC
// 8446 §4.4.2.4 states "MUST always be non-empty" ("If the server
// supplies an empty Certificate message, the client MUST abort the
// handshake with a decode_error alert"), enforced unconditionally here
// rather than deferred to a caller that would need to know which role
// sent it.
pub fn parse_certificate(body []u8) !ParsedCertificate {
	if body.len < 1 {
		return error('quic: truncated Certificate: need at least 1 byte, have ${body.len}')
	}
	ctx_len := int(body[0])
	mut cursor := 1
	if body.len < cursor + ctx_len {
		return error('quic: truncated Certificate: certificate_request_context declares ${ctx_len} bytes, only ${body.len - cursor} remain')
	}
	certificate_request_context := body[cursor..cursor + ctx_len].clone()
	cursor += ctx_len

	if body.len < cursor + 3 {
		return error('quic: truncated Certificate: missing certificate_list length')
	}
	list_len := int((u32(body[cursor]) << 16) | (u32(body[cursor + 1]) << 8) | u32(body[cursor + 2]))
	cursor += 3
	if cursor + list_len != body.len {
		return error('quic: Certificate certificate_list length ${list_len} does not match remaining body ${body.len - cursor}')
	}

	mut entries := []CertificateEntry{}
	end := cursor + list_len
	for cursor < end {
		if end - cursor < 3 {
			return error('quic: truncated CertificateEntry: missing cert_data length')
		}
		cert_len := int((u32(body[cursor]) << 16) | (u32(body[cursor + 1]) << 8) | u32(body[cursor + 2]))
		cursor += 3
		if cert_len == 0 {
			return error('quic: CertificateEntry cert_data must not be empty (opaque cert_data<1..2^24-1>)')
		}
		if cursor + cert_len > end {
			return error('quic: CertificateEntry cert_data declares ${cert_len} bytes exceeding the certificate_list')
		}
		cert_data := body[cursor..cursor + cert_len].clone()
		cursor += cert_len

		if end - cursor < 2 {
			return error('quic: truncated CertificateEntry: missing extensions length')
		}
		ext_len := int((u32(body[cursor]) << 8) | u32(body[cursor + 1]))
		cursor += 2
		if cursor + ext_len > end {
			return error('quic: CertificateEntry extensions declare ${ext_len} bytes exceeding the certificate_list')
		}
		extensions := parse_extension_list(body[cursor..cursor + ext_len])!
		cursor += ext_len
		// RFC 8446 §4.2's own extension-applicability table permits only
		// status_request and signed_certificate_timestamp in a
		// CertificateEntry ("CT") -- everything else (e.g.
		// supported_versions, which is CH/SH/HRR-only) is illegal here
		// regardless of what recognized extension type it is. This
		// client's ClientHello (tls13_client_hello.v) never offers
		// EITHER of the two CT-legal extensions, so per this file's own
		// EncryptedExtensions precedent (parse_encrypted_extensions,
		// "Implementations MUST NOT send extension responses if the
		// remote endpoint did not send the corresponding extension
		// requests... MUST abort... with an 'unsupported_extension'
		// alert") the correct legal set here is empty: ANY extension
		// present in a CertificateEntry is a violation, not just the
		// specific illegal types a server might plausibly send (Codex
		// P2, vlang/v#27680 pullrequestreview-4822597219 -- previously
		// parsed and stored unconditionally, never checked against
		// anything).
		if extensions.len != 0 {
			return error_with_code('quic: CertificateEntry contains extension 0x${extensions[0].typ:04x}, which this client did not offer (RFC 8446 §4.2 permits only status_request/signed_certificate_timestamp in a CertificateEntry, and this client offers neither)', int(tls_alert_to_quic_error(.unsupported_extension)))
		}

		entries << CertificateEntry{
			cert_data: cert_data
			extensions: extensions
		}
	}

	if entries.len == 0 {
		return error('quic: Certificate certificate_list must not be empty (server certificate_list MUST always be non-empty)')
	}

	return ParsedCertificate{
		certificate_request_context: certificate_request_context
		certificate_list: entries
	}
}

// encode_certificate constructs a complete Certificate handshake message
// (RFC 8446 §4.4.2), framed via encode_handshake_message. `certificate_list`
// is this server's own certificate chain, leaf-first (RFC 8446 §4.4.2's own
// implicit ordering -- the peer's chain-validation walk, mirrored by this
// codebase's own verify_certificate_chain, always treats the first entry as
// the leaf). certificate_request_context is always encoded as empty: RFC
// 8446 §4.4.2 states it is only non-empty "if this message is in response
// to a CertificateRequest" -- "Otherwise (in the case of server
// authentication), this field SHALL be zero length" -- and v1 is
// server-authentication-only (client-cert auth is out of scope), so this
// function never takes a caller-supplied context, the same scope
// restriction parse_certificate's own doc comment already states for the
// parse side.
pub fn encode_certificate(certificate_list []CertificateEntry) ![]u8 {
	// RFC 8446 §4.4.2.4 (quoted in parse_certificate's own doc comment):
	// "the server MUST always provide a non-empty certificate_list" --
	// enforced here on the encode side too, not just checked on the way
	// back in when a peer's Certificate is parsed.
	if certificate_list.len == 0 {
		return error('quic: Certificate certificate_list must not be empty (server certificate_list MUST always be non-empty, RFC 8446 §4.4.2.4)')
	}

	mut body := []u8{}
	body << u8(0) // certificate_request_context: always empty, see doc comment above

	mut list := []u8{}
	for entry in certificate_list {
		if entry.cert_data.len == 0 || entry.cert_data.len > 0xff_ffff {
			return error('quic: CertificateEntry cert_data length ${entry.cert_data.len} out of range (opaque cert_data<1..2^24-1>)')
		}
		list << u8(entry.cert_data.len >> 16)
		list << u8(entry.cert_data.len >> 8)
		list << u8(entry.cert_data.len)
		list << entry.cert_data
		// parse_certificate's own doc comment establishes that this
		// client's ClientHello offers neither status_request nor
		// signed_certificate_timestamp, so the only RFC 8446 §4.2-legal
		// CertificateEntry extensions for THIS codebase's peer are illegal
		// to send here (RFC 8446 §4.4.2: "Extensions in the Certificate
		// message from the server MUST correspond to ones from the
		// ClientHello message") -- enforced here too, not just on the
		// parse side, so a caller can never accidentally construct a
		// message a compliant peer would reject.
		if entry.extensions.len != 0 {
			return error('quic: CertificateEntry.extensions must be empty -- this server never negotiates status_request or signed_certificate_timestamp (RFC 8446 §4.4.2)')
		}
		list << u8(0) // extensions length: always 0, see above
		list << u8(0)
	}
	if list.len > 0xff_ffff {
		return error('quic: Certificate certificate_list too large: ${list.len} bytes')
	}
	body << u8(list.len >> 16)
	body << u8(list.len >> 8)
	body << u8(list.len)
	body << list

	return encode_handshake_message(.certificate, body)!
}

pub struct ParsedCertificateVerify {
pub:
	algorithm u16
	signature []u8
}

// parse_certificate_verify parses a CertificateVerify handshake message
// BODY (RFC 8446 §4.4.3). Validates `algorithm` against the fixed set v1
// itself offered in its own signature_algorithms extension
// (tls13_client_hello.v's sig_scheme_* constants) — RFC 8446 §4.4.3: "the
// signature algorithm MUST be one offered in the client's
// signature_algorithms extension." Since v1's offered set is a fixed,
// hardcoded list rather than something that varies per connection, this
// check needs no caller-supplied state, unlike the checks
// tls13_server_hello.v defers to the state machine.
pub fn parse_certificate_verify(body []u8) !ParsedCertificateVerify {
	if body.len < 4 {
		return error('quic: truncated CertificateVerify: need at least 4 bytes, have ${body.len}')
	}
	algorithm := u16((u32(body[0]) << 8) | u32(body[1]))
	sig_len := int((u32(body[2]) << 8) | u32(body[3]))
	if body.len != 4 + sig_len {
		return error('quic: CertificateVerify length mismatch: declares ${sig_len}-byte signature, have ${body.len - 4} bytes')
	}
	// No minimum-length check on the signature itself: RFC 8446 §4.4.3
	// declares `opaque signature<0..2^16-1>`, explicitly permitting zero
	// length at the wire-format level (unlike cert_data<1..2^24-1> and
	// key_exchange<1..2^16-1>, both of which state a minimum of 1 and are
	// enforced as such elsewhere in this module). No real signature for
	// any offered algorithm is ever actually empty; an empty one will
	// fail the real cryptographic verification step once that's built,
	// which is the layer that actually needs to reject it.
	if algorithm !in [sig_scheme_ecdsa_secp256r1_sha256, sig_scheme_rsa_pss_rsae_sha256,
		sig_scheme_rsa_pss_rsae_sha384, sig_scheme_rsa_pss_rsae_sha512] {
		// RFC 8446 §4.4.3 requires the algorithm to be "one offered in the
		// client's 'signature_algorithms' extension"; it does not itself
		// name the alert for a violation. §6.2's general definition of
		// illegal_parameter ("a field ... was incorrect or inconsistent
		// with other fields") fits a value inconsistent with this client's
		// own ClientHello, and this file's ServerHello/HelloRetryRequest
		// cipher_suite-not-offered checks already use illegal_parameter for
		// the identical class of violation (a peer selecting a value from a
		// closed set this client didn't offer) -- extended here for
		// consistency (Codex P2, vlang/v#27680 pullrequestreview-4806500473).
		return error_with_code('quic: CertificateVerify algorithm 0x${algorithm:04x} was not offered in signature_algorithms', int(tls_alert_to_quic_error(.illegal_parameter)))
	}
	return ParsedCertificateVerify{
		algorithm: algorithm
		signature: body[4..].clone()
	}
}

const certificate_verify_context_server = 'TLS 1.3, server CertificateVerify'
const certificate_verify_context_client = 'TLS 1.3, client CertificateVerify'

pub enum CertificateVerifyRole {
	server
	client
}

// certificate_verify_signed_content builds RFC 8446 §4.4.3's exact
// signed-content construction: 64 octets of 0x20, the role-specific
// context string, a single 0x00 separator byte, then
// Transcript-Hash(Handshake Context, Certificate). This 64-byte pad
// exists specifically to defeat a prior-TLS-version attack that obtained
// signatures over a chosen 32-byte prefix (RFC 8446 §4.4.3) — it is not
// arbitrary padding a future cleanup could shrink.
//
// v1 only ever needs the server variant (client CertificateVerify is
// never sent — CertificateRequest is rejected outright, per PROGRESS.md);
// the client variant is included for API completeness with the RFC's own
// two-sided definition, at negligible cost, and to keep this function
// correct if Phase 13's server-role client-cert-auth support is ever
// added without needing to revisit this file.
pub fn certificate_verify_signed_content(role CertificateVerifyRole, transcript_hash []u8) []u8 {
	context := match role {
		.server { certificate_verify_context_server }
		.client { certificate_verify_context_client }
	}
	context_bytes := context.bytes()
	mut out := []u8{cap: 64 + context_bytes.len + 1 + transcript_hash.len}
	out << []u8{len: 64, init: 0x20}
	out << context_bytes
	out << u8(0)
	out << transcript_hash
	return out
}

// encode_certificate_verify constructs a complete CertificateVerify
// handshake message (RFC 8446 §4.4.3) by SIGNING
// certificate_verify_signed_content(.server, transcript_hash) with
// `signing_key`, then framing the result via encode_handshake_message. v1
// is server-authentication-only (client CertificateVerify is never sent),
// so this function always signs the `.server` context -- see
// certificate_verify_signed_content's own doc comment for why the `.client`
// variant exists at all without a real caller.
//
// Only sig_scheme_ecdsa_secp256r1_sha256 is wired up so far: rejected with
// a clear "not implemented yet" error for any other algorithm rather than
// silently producing a signature under the wrong scheme -- RSA-PSS signing
// needs a still-missing mbedtls_pk_sign_ext V wrapper (only the verify side,
// verify_rsa_pss_signature in net.mbedtls, exists today), tracked as
// follow-up work within 13a, not built here.
//
// `signing_key` MUST be a P-256 (prime256v1) key -- the only curve this
// codebase's own key generation/loading ever produces (Phase 1's scope
// decision, `crypto.ecdsa`'s CurveOptions defaults to prime256v1 and no v1
// caller ever overrides it). crypto.ecdsa exposes no curve accessor to
// verify this defensively at the V level; behavior for a caller-supplied
// non-P-256 key is undefined by construction, not validated here -- the
// same trust boundary this function's own signing_key parameter implies
// for any local, non-peer-supplied cryptographic material.
pub fn encode_certificate_verify(algorithm u16, signing_key ecdsa.PrivateKey, transcript_hash []u8) ![]u8 {
	if algorithm != sig_scheme_ecdsa_secp256r1_sha256 {
		return error('quic: CertificateVerify signing for algorithm 0x${algorithm:04x} is not implemented yet (only ecdsa_secp256r1_sha256 is wired up)')
	}

	content := certificate_verify_signed_content(.server, transcript_hash)
	// PrivateKey.sign's default hash_config (.with_recommended_hash) picks
	// SHA-256 for a 256-bit (P-256) key -- see default_digest in
	// vlib/crypto/ecdsa/ecdsa.v, keyed off the key's own bit size, matching
	// exactly what sig_scheme_ecdsa_secp256r1_sha256 requires. The
	// resulting signature is OpenSSL's standard ASN.1 DER ECDSA-Sig-Value
	// encoding (sign_digest, vlib/crypto/ecdsa/ecdsa.v, sets no raw/compact
	// signature option, so OpenSSL's EVP_PKEY_sign default applies) --
	// the same format TLS/X.509 ECDSA signatures conventionally use, and
	// what net.mbedtls's verify_ecdsa_signature (the client-side verify
	// path, tls13_certificate_chain.c.v) is written to parse. This is
	// confirmed by source inspection and this file's own same-library
	// (OpenSSL signs, OpenSSL verifies) round-trip test
	// (tls13_certificate_test.v), NOT by an actual cross-library
	// OpenSSL-signs/mbedTLS-verifies test -- this repo has no EC
	// certificate fixture to build an mbedtls_pk_context from for that
	// (same gap verify_ecdsa_signature's own client-side tests document).
	signature := signing_key.sign(content, hash_config: .with_recommended_hash)!

	mut body := []u8{}
	body << u8(algorithm >> 8)
	body << u8(algorithm)
	if signature.len > 0xffff {
		return error('quic: CertificateVerify signature too large: ${signature.len} bytes')
	}
	body << u8(signature.len >> 8)
	body << u8(signature.len)
	body << signature

	return encode_handshake_message(.certificate_verify, body)!
}
