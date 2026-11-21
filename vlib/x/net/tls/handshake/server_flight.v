// Copyright (c) 2026 blackshirt.
//
// server_flight.v decodes the server's encrypted flight — EncryptedExtensions
// through Finished (RFC 8446 §4.3–§4.4).
module handshake

import tls.pki
import tls.wire

// ParsedCertificateVerify holds the parsed CertificateVerify handshake message.
struct ParsedCertificateVerify {
mut:
	algorithm SignatureScheme // scheme the server signed with, must be usable for the leaf key
	signature []u8            // raw signature bytes over the transcript-derived content
}

// parse_certificate_message decodes the Certificate message body (RFC 8446 §4.4.2)
// and returns the DER-encoded chain; pki.parse_certificate decodes a single certificate.
@[direct_array_access]
fn parse_certificate_message(body []u8) ![][]u8 {
	mut r := wire.new_reader(body)
	req_ctx := r.read_vector_u8()!
	// §4.4.2: a server Certificate's certificate_request_context SHALL be zero length.
	if req_ctx.len > 0 {
		return error('handshake: non-empty certificate_request_context in server Certificate (illegal_parameter)')
	}
	// u24 list length: full chains can exceed 64 KiB, which a u16 could not frame.
	cert_list_bytes := r.read_vector_u24()!

	mut cert_r := wire.new_reader(cert_list_bytes)
	mut certs := [][]u8{}
	for cert_r.remaining() > 0 {
		// Reject an over-long chain before every entry is ASN.1-parsed, so a peer
		// cannot pack thousands of tiny entries.
		if certs.len >= pki.max_cert_chain_depth {
			return error('handshake: certificate chain exceeds maximum depth ${pki.max_cert_chain_depth} (illegal_parameter)')
		}
		cert_data := cert_r.read_vector_u24()!
		cert_extensions := cert_r.read_vector_u16()!
		_ := parse_extensions(cert_extensions)!
		certs << cert_data
	}

	if certs.len == 0 {
		return error('handshake: server certificate list cannot be empty')
	}
	if r.remaining() != 0 {
		return error('handshake: trailing bytes in Certificate message (decode_error)')
	}
	return certs
}

// parse_certificate_verify decodes the CertificateVerify body (RFC 8446 §4.4.3).
@[direct_array_access]
fn parse_certificate_verify(body []u8) !ParsedCertificateVerify {
	mut r := wire.new_reader(body)
	algo_u16 := r.read_u16()!
	algo := signature_scheme_from_u16(algo_u16)!
	signature := r.read_vector_u16()!
	if signature.len == 0 {
		return error('handshake: empty signature in CertificateVerify (decode_error)')
	}
	if r.remaining() != 0 {
		return error('handshake: trailing bytes in CertificateVerify message (decode_error)')
	}
	return ParsedCertificateVerify{
		algorithm: algo
		signature: signature
	}
}

// certificate_verify_signed_content constructs the bytes signed in CertificateVerify
// (RFC 8446 §4.4.3): 0x20 * 64 || "TLS 1.3, server CertificateVerify" || 0x00 || transcript_hash.
fn certificate_verify_signed_content(transcript_hash []u8) []u8 {
	context_string := 'TLS 1.3, server CertificateVerify'.bytes()
	mut out := []u8{cap: 64 + context_string.len + 1 + transcript_hash.len}
	for _ in 0 .. 64 {
		out << 0x20 // 64 space octets
	}
	out << context_string
	out << 0x00
	out << transcript_hash
	return out
}

// parse_finished reads the verify_data from a Finished message (RFC 8446 §4.4.4).
// The body is exactly verify_data, so its length is fixed at Hash.length.
@[direct_array_access]
fn parse_finished(body []u8, hash_len int) ![]u8 {
	if body.len != hash_len {
		return error('handshake: Finished verify_data is ${body.len} bytes, expected ${hash_len} (Hash.length, RFC 8446 §4.4.4)')
	}
	return body.clone()
}

// build_finished creates a Finished handshake message carrying verify_data.
fn build_finished(verify_data []u8) []u8 {
	return wrap_handshake_message(.finished, verify_data)
}

// CertificateRequest is the decoded form of a server CertificateRequest (RFC 8446 §4.3.2);
// this client holds no certificate, so only the request context must be echoed verbatim.
struct CertificateRequest {
	request_context []u8 // echoed verbatim in the responding Certificate (§4.4.2)
}

// parse_certificate_request decodes the CertificateRequest body (RFC 8446 §4.3.2).
// The extensions must be well-formed and bounded; their content is not interpreted here.
@[direct_array_access]
fn parse_certificate_request(body []u8) !CertificateRequest {
	mut r := wire.new_reader(body)
	request_context := r.read_vector_u8()!
	ext_bytes := r.read_vector_u16()!
	if r.remaining() != 0 {
		return error('handshake: trailing bytes in CertificateRequest (decode_error)')
	}
	_ := parse_extensions(ext_bytes)!
	return CertificateRequest{
		request_context: request_context.clone()
	}
}

// build_empty_certificate_message creates an empty Certificate message (RFC 8446 §4.4.2),
// echoing the request context verbatim and framing certificate_list as u24 zero.
fn build_empty_certificate_message(request_context []u8) []u8 {
	mut body := []u8{cap: 1 + request_context.len + 3}
	body << u8(request_context.len)
	body << request_context
	body << [u8(0), 0, 0] // u24 certificate_list length: empty
	return wrap_handshake_message(.certificate, body)
}

// parse_encrypted_extensions_for_transport extracts the two values the handshake
// itself needs from EncryptedExtensions: the negotiated ALPN and, on a QUIC
// connection, the peer's transport parameters as raw bytes — their content belongs
// to QUIC and is decoded by the adapter; this only enforces TLS placement rules.
@[direct_array_access]
fn parse_encrypted_extensions_for_transport(body []u8, is_quic bool) !(string, ?[]u8) {
	mut r := wire.new_reader(body)
	ext_bytes := r.read_vector_u16()!
	mut alpn := ''
	mut transport_parameters := ?[]u8(none)

	exts := parse_extensions(ext_bytes)!
	for ext in exts {
		if ext.typ == u16(ExtensionType.alpn) {
			mut data_r := wire.new_reader(ext.data)
			proto_list := data_r.read_vector_u16()!
			if data_r.remaining() != 0 {
				return error('handshake: trailing bytes after ALPN protocol_list')
			}
			mut pl_r := wire.new_reader(proto_list)
			if pl_r.remaining() == 0 {
				// RFC 7301 §3.1: an ALPN extension asserts a single protocol.
				return error('handshake: ALPN extension carried no protocol')
			}
			alpn = pl_r.read_vector_u8()!.bytestr()
			if pl_r.remaining() != 0 {
				// RFC 7301 §3.1: exactly one protocol; a second would dodge the offered-list check.
				return error('handshake: ALPN extension carried more than one protocol')
			}
		} else if ext.typ == u16(ExtensionType.server_name) {
			// RFC 8446 §4.2 / RFC 6066 §3: the SNI acknowledgment is an EMPTY server_name extension.
			if ext.data.len != 0 {
				return error('handshake: server sent a non-empty server_name in EncryptedExtensions')
			}
		} else if ext.typ == u16(ExtensionType.supported_groups) {
			// Interop carve-out: §4.2 makes supported_groups ClientHello-only, but
			// some servers echo it; it carries no authority.
			continue
		} else if ext.typ == u16(ExtensionType.renegotiation_info) {
			// Interop carve-out: OpenSSL sends this unoffered; RFC 5746 §3.4 fixes
			// the content to an empty renegotiated_connection.
			mut ri_r := wire.new_reader(ext.data)
			if ri_r.remaining() > 0 {
				renegotiated := ri_r.read_vector_u8()!
				if renegotiated.len != 0 || ri_r.remaining() != 0 {
					return error('handshake: renegotiation_info carried a non-empty renegotiated_connection')
				}
			}
		} else if ext.typ == u16(ExtensionType.quic_transport_parameters) {
			if !is_quic {
				return error('handshake: QUIC transport parameters received on a TLS record connection')
			}
			// Clone: `body` is the recordless buffer this call does not own, and the
			// adapter reads these bytes long after the handshake message is consumed.
			transport_parameters = ext.data.clone()
		} else {
			// §4.3.1: everything else must have been offered and belong here.
			return error('handshake: extension 0x${ext.typ:04x} in EncryptedExtensions was not offered (unsupported_extension)')
		}
	}
	if r.remaining() != 0 {
		return error('handshake: trailing bytes after EncryptedExtensions')
	}
	if is_quic && transport_parameters == none {
		// RFC 9001 §4.3/§8.2: a QUIC server must advertise transport parameters here;
		// their contents are the adapter's concern, but omitting them entirely is a
		// handshake-phase violation and fails closed.
		return error('handshake: server omitted QUIC transport parameters')
	}

	return alpn, transport_parameters
}
