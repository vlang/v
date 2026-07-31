module quic

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
		cert_len := int((u32(body[cursor]) << 16) | (u32(body[cursor + 1]) << 8) | u32(body[
			cursor + 2]))
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

		entries << CertificateEntry{
			cert_data:  cert_data
			extensions: extensions
		}
	}

	if entries.len == 0 {
		return error('quic: Certificate certificate_list must not be empty (server certificate_list MUST always be non-empty)')
	}

	return ParsedCertificate{
		certificate_request_context: certificate_request_context
		certificate_list:            entries
	}
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
		return error('quic: CertificateVerify algorithm 0x${algorithm:04x} was not offered in signature_algorithms')
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
