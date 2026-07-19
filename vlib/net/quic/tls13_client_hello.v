module quic

// RFC 8446 §4.2 — generic TLS extension wire format: a 2-byte
// ExtensionType, a 2-byte length, then that many bytes of extension_data.
const ext_server_name = u16(0)
const ext_supported_groups = u16(10)
const ext_signature_algorithms = u16(13)
const ext_supported_versions = u16(43)
const ext_key_share = u16(51)
// RFC 9001 §8.2 — not in RFC 8446's own ExtensionType list, registered
// separately for QUIC-TLS.
const ext_quic_transport_parameters = u16(0x39)

// RFC 8446 §4.2.7 NamedGroup. v1 offers only P-256 (secp256r1), matching
// Phase 1's OpenSSL-backed ECDH support — no other group is wired up.
const named_group_secp256r1 = u16(0x0017)

// RFC 8446 §4.2.3 SignatureScheme values this client is willing to
// verify, matching Phase 2c's planned CertificateVerify validation (mbedTLS
// for both ECDSA and RSA-PSS) — an intentionally narrow v1 list, not the
// full registry.
const sig_scheme_ecdsa_secp256r1_sha256 = u16(0x0403)
const sig_scheme_rsa_pss_rsae_sha256 = u16(0x0804)
const sig_scheme_rsa_pss_rsae_sha384 = u16(0x0805)
const sig_scheme_rsa_pss_rsae_sha512 = u16(0x0806)

const tls_version_1_3 = u16(0x0304)

// RFC 9001 §5.1 mandates TLS_AES_128_GCM_SHA256 for Initial packets; the
// plan pins v1 to this single cipher suite for the whole handshake (Phase
// 2b), so offering only it here is intentional, not an oversight.
const cipher_suite_tls_aes_128_gcm_sha256 = u16(0x1301)

fn encode_extension(typ u16, data []u8) ![]u8 {
	if data.len > 0xffff {
		return error('quic: extension 0x${typ:x} data too large: ${data.len} bytes')
	}
	mut out := []u8{cap: 4 + data.len}
	out << u8(typ >> 8)
	out << u8(typ)
	out << u8(data.len >> 8)
	out << u8(data.len)
	out << data
	return out
}

fn encode_supported_versions_extension() ![]u8 {
	mut data := []u8{}
	data << u8(2) // 1-byte length (RFC 8446 §4.2.1's versions<2..254>)
	data << u8(tls_version_1_3 >> 8)
	data << u8(tls_version_1_3)
	return encode_extension(ext_supported_versions, data)
}

fn encode_supported_groups_extension() ![]u8 {
	mut list := []u8{}
	list << u8(named_group_secp256r1 >> 8)
	list << u8(named_group_secp256r1)
	mut data := []u8{}
	data << u8(list.len >> 8)
	data << u8(list.len)
	data << list
	return encode_extension(ext_supported_groups, data)
}

fn encode_signature_algorithms_extension() ![]u8 {
	schemes := [sig_scheme_ecdsa_secp256r1_sha256, sig_scheme_rsa_pss_rsae_sha256,
		sig_scheme_rsa_pss_rsae_sha384, sig_scheme_rsa_pss_rsae_sha512]
	mut list := []u8{}
	for s in schemes {
		list << u8(s >> 8)
		list << u8(s)
	}
	mut data := []u8{}
	data << u8(list.len >> 8)
	data << u8(list.len)
	data << list
	return encode_extension(ext_signature_algorithms, data)
}

// encode_key_share_extension wraps a single KeyShareEntry (RFC 8446
// §4.2.8) — v1 never sends more than one, since it only ever offers
// secp256r1. `key_exchange` is Phase 1's `PublicKey.uncompressed_bytes()`
// output (RFC 8446 §4.2.8.2's UncompressedPointRepresentation: 0x04 || X
// || Y, 65 bytes for P-256).
fn encode_key_share_extension(group u16, key_exchange []u8) ![]u8 {
	if key_exchange.len == 0 || key_exchange.len > 0xffff {
		return error('quic: key_exchange length ${key_exchange.len} out of range')
	}
	mut entry := []u8{}
	entry << u8(group >> 8)
	entry << u8(group)
	entry << u8(key_exchange.len >> 8)
	entry << u8(key_exchange.len)
	entry << key_exchange

	mut data := []u8{}
	data << u8(entry.len >> 8)
	data << u8(entry.len)
	data << entry
	return encode_extension(ext_key_share, data)
}

// encode_server_name_extension implements RFC 6066 §3's ServerNameList,
// restricted to the single host_name entry every real client sends (RFC
// 6066 itself prohibits more than one name of the same NameType, and
// host_name is the only NameType defined).
fn encode_server_name_extension(hostname string) ![]u8 {
	name_bytes := hostname.bytes()
	if name_bytes.len == 0 || name_bytes.len > 0xffff {
		return error('quic: server_name hostname length ${name_bytes.len} out of range')
	}
	mut server_name := []u8{}
	server_name << u8(0) // NameType host_name
	server_name << u8(name_bytes.len >> 8)
	server_name << u8(name_bytes.len)
	server_name << name_bytes

	mut data := []u8{}
	data << u8(server_name.len >> 8)
	data << u8(server_name.len)
	data << server_name
	return encode_extension(ext_server_name, data)
}

fn encode_quic_transport_parameters_extension(params QuicTransportParameters) ![]u8 {
	encoded := encode_transport_parameters(params)!
	return encode_extension(ext_quic_transport_parameters, encoded)
}

// ClientHelloParams is everything build_client_hello needs beyond what's
// fixed by v1's scope decisions (single cipher suite, single named group,
// a fixed signature_algorithms list).
pub struct ClientHelloParams {
pub:
	random               []u8 // exactly 32 bytes; caller supplies so callers can use a real CSPRNG while tests stay deterministic
	server_name          string
	ecdhe_public_key     []u8 // Phase 1 PublicKey.uncompressed_bytes() output, 65 bytes for P-256
	transport_parameters QuicTransportParameters
}

// build_client_hello constructs a complete TLS 1.3 ClientHello handshake
// message (RFC 8446 §4.1.2), framed via encode_handshake_message. Sends
// exactly six extensions: server_name, supported_versions,
// supported_groups, signature_algorithms, key_share, and
// quic_transport_parameters (RFC 9001 §8.2) — order doesn't matter per
// RFC 8446 §4.2 ("extensions MAY appear in any order") except that
// pre_shared_key would have to be last, and v1 never sends one (no 0-RTT/
// resumption, Phase 14).
pub fn build_client_hello(p ClientHelloParams) ![]u8 {
	if p.random.len != 32 {
		return error('quic: ClientHello random must be exactly 32 bytes, got ${p.random.len}')
	}
	// RFC 9000 §18.2: "A client MUST NOT include any server-only
	// transport parameter." `QuicTransportParameters` itself doesn't
	// enforce this (it's designed to represent either side's parameter
	// set unchanged, for Phase 13's server support), so this, the actual
	// client-side construction path, is where it must be caught instead
	// of silently producing a protocol-violating ClientHello.
	if p.transport_parameters.original_destination_connection_id != none {
		return error('quic: ClientHello transport parameters must not include original_destination_connection_id (server-only)')
	}
	if p.transport_parameters.stateless_reset_token != none {
		return error('quic: ClientHello transport parameters must not include stateless_reset_token (server-only)')
	}
	if p.transport_parameters.preferred_address != none {
		return error('quic: ClientHello transport parameters must not include preferred_address (server-only)')
	}
	if p.transport_parameters.retry_source_connection_id != none {
		return error('quic: ClientHello transport parameters must not include retry_source_connection_id (server-only)')
	}

	mut body := []u8{}
	// legacy_version MUST be 0x0303 (RFC 8446 §4.1.2) -- the real version
	// is negotiated via supported_versions below.
	body << u8(0x03)
	body << u8(0x03)
	body << p.random
	// legacy_session_id: a zero-length vector. RFC 9001 §8.4 explicitly
	// PROHIBITS TLS 1.3's middlebox compatibility mode over QUIC ("A
	// client MUST NOT request the use of the TLS 1.3 compatibility
	// mode"), which is exactly what setting a non-empty legacy_session_id
	// would do — a server is told to treat a non-empty value here as a
	// PROTOCOL_VIOLATION connection error.
	body << u8(0)
	// cipher_suites: a 2-byte-length-prefixed vector of 2-byte suites.
	body << u8(0)
	body << u8(2)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	// legacy_compression_methods: exactly one byte, 0 (null) -- RFC 8446
	// §4.1.2 requires this exact single-zero-byte vector for TLS 1.3.
	body << u8(1)
	body << u8(0)

	mut extensions := []u8{}
	extensions << encode_server_name_extension(p.server_name)!
	extensions << encode_supported_versions_extension()!
	extensions << encode_supported_groups_extension()!
	extensions << encode_signature_algorithms_extension()!
	extensions << encode_key_share_extension(named_group_secp256r1, p.ecdhe_public_key)!
	extensions << encode_quic_transport_parameters_extension(p.transport_parameters)!

	if extensions.len > 0xffff {
		return error('quic: ClientHello extensions block too large: ${extensions.len} bytes')
	}
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions

	return encode_handshake_message(.client_hello, body)!
}
