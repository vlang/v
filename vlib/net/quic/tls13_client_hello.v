module quic

// RFC 8446 §4.2 — generic TLS extension wire format: a 2-byte
// ExtensionType, a 2-byte length, then that many bytes of extension_data.
const ext_server_name = u16(0)
const ext_supported_groups = u16(10)
const ext_signature_algorithms = u16(13)
const ext_supported_versions = u16(43)
const ext_key_share = u16(51)
// RFC 8446 §4.2.3 -- IANA TLS ExtensionType registry value 50, confirmed
// directly. Distinct from `signature_algorithms`: when present, THIS
// extension's list governs which signature algorithms the client accepts
// for signatures appearing in CERTIFICATES (chain-of-trust signing), while
// `signature_algorithms` governs CertificateVerify (the live handshake
// signature). Sending both lets this client stay strict about
// CertificateVerify (no legacy RSA-PKCS1) while being permissive about
// what's actually a very common real-world certificate-signing algorithm.
const ext_signature_algorithms_cert = u16(50)
// RFC 7301 §3.1 — registered separately from RFC 8446's own ExtensionType
// list. RFC 9001 §8.1 makes this MANDATORY for QUIC: there is no other
// application-protocol negotiation mechanism, so a server that supports no
// protocol this client offers MUST fail the handshake with a fatal
// no_application_protocol alert rather than silently falling back.
const ext_alpn = u16(16)
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

// RFC 8446 §4.2.3: "These values refer solely to signatures which appear in
// certificates ... and are not defined for use in signed TLS handshake
// messages, although they MAY appear in 'signature_algorithms' and
// 'signature_algorithms_cert' for backward compatibility." Never offered in
// `signature_algorithms` (this client's CertificateVerify validation stays
// strict) -- only in `signature_algorithms_cert`, since a very large
// fraction of real-world CA-issued certificates are still RSA-PKCS1v1.5
// signed and mbedTLS's own generic X.509 chain verifier
// (verify_certificate_chain) already supports validating them; without
// advertising these, a compliant server whose chain uses one of these
// algorithms has no signature_algorithms_cert entry to select against and
// cannot offer that chain at all (Codex P1, vlang/v#27680
// pullrequestreview-4791164664).
const sig_scheme_rsa_pkcs1_sha256 = u16(0x0401)
const sig_scheme_rsa_pkcs1_sha384 = u16(0x0501)
const sig_scheme_rsa_pkcs1_sha512 = u16(0x0601)

// Same rationale and same cert-only restriction as the rsa_pkcs1_* trio
// above, for the other two ECDSA curves the vendored mbedTLS build supports
// (MBEDTLS_ECP_DP_SECP384R1_ENABLED/SECP521R1_ENABLED plus MBEDTLS_SHA512_C,
// confirmed in mbedtls_config.h) but this client's own CertificateVerify
// path does not: `verify_certificate_verify_signature`'s ECDSA branch
// additionally requires the P-256 curve specifically
// (public_key_curve_is_secp256r1), so `signature_algorithms` stays
// P-256-only by design -- these two are for chain verification only, via
// mbedTLS's generic X.509 verifier, which has no such curve restriction.
// Without advertising them, a server whose chain is P-384- or P-521-signed
// has no signature_algorithms_cert entry to select against, even though
// this client's chain verification already supports both curves (Codex P2,
// vlang/v#27680 pullrequestreview-4806500473).
const sig_scheme_ecdsa_secp384r1_sha384 = u16(0x0503)
const sig_scheme_ecdsa_secp521r1_sha512 = u16(0x0603)

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

// encode_signature_algorithms_cert_extension advertises a BROADER scheme
// list than encode_signature_algorithms_extension -- everything this client
// can validate for a CERTIFICATE's own signature via mbedTLS's generic
// X.509 chain verifier (which supports RSA-PKCS1v1.5 and the P-384/P-521
// ECDSA curves in addition to everything signature_algorithms already
// lists), even though CertificateVerify itself stays restricted to the
// narrower live-handshake-signature set. See this file's own
// sig_scheme_rsa_pkcs1_*/sig_scheme_ecdsa_secp384r1_sha384 doc comments for
// why these two lists deliberately differ.
fn encode_signature_algorithms_cert_extension() ![]u8 {
	schemes := [sig_scheme_ecdsa_secp256r1_sha256, sig_scheme_ecdsa_secp384r1_sha384,
		sig_scheme_ecdsa_secp521r1_sha512, sig_scheme_rsa_pss_rsae_sha256,
		sig_scheme_rsa_pss_rsae_sha384, sig_scheme_rsa_pss_rsae_sha512, sig_scheme_rsa_pkcs1_sha256,
		sig_scheme_rsa_pkcs1_sha384, sig_scheme_rsa_pkcs1_sha512]
	mut list := []u8{}
	for s in schemes {
		list << u8(s >> 8)
		list << u8(s)
	}
	mut data := []u8{}
	data << u8(list.len >> 8)
	data << u8(list.len)
	data << list
	return encode_extension(ext_signature_algorithms_cert, data)
}

// encode_key_share_extension wraps a single KeyShareEntry (RFC 8446
// §4.2.8) — v1 never sends more than one, since it only ever offers
// secp256r1. `key_exchange` is Phase 1's `PublicKey.uncompressed_bytes()`
// output (RFC 8446 §4.2.8.2's UncompressedPointRepresentation: 0x04 || X
// || Y, 65 bytes for P-256).
fn encode_key_share_extension(group u16, key_exchange []u8) ![]u8 {
	// Sibling of encode_server_name_extension's identical bound fix (Codex
	// P2, vlang/v#27680 pullrequestreview-4806500473): entry is
	// group(2)+key_exchange_len(2)+key_exchange, then client_shares wraps
	// THAT in its own length(2) prefix -- 6 bytes of combined overhead
	// under the same u16 space as `data` overall. Not reachable with a
	// real caller today (key_exchange is always the fixed 65-byte P-256
	// uncompressed point), but the bound itself was equally wrong, closed
	// proactively rather than waiting for a future caller to hit it.
	if key_exchange.len == 0 || key_exchange.len > 0xffff - 6 {
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
	// A hostname entry is NameType(1) + name_length(2) + name_bytes, then
	// the ServerNameList wrapping that adds its OWN length(2) prefix -- 5
	// bytes of combined overhead that must fit alongside name_bytes under
	// the SAME u16 space this whole `data` blob is limited to (checked
	// generically by encode_extension below). The previous bound only
	// checked `name_bytes.len > 0xffff`, ignoring that overhead entirely
	// (Codex P2, vlang/v#27680 pullrequestreview-4806500473) -- in
	// practice encode_extension's own generic size guard already caught
	// every value this let through unrejected, so no malformed ClientHello
	// was ever actually producible, but the bound itself was still wrong
	// and gave a misattributed "extension data too large" message instead
	// of clearly naming the hostname as the problem.
	if name_bytes.len == 0 || name_bytes.len > 0xffff - 5 {
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

// encode_alpn_extension implements RFC 7301 §3.1's ProtocolNameList: a
// 2-byte list-length prefix, then each protocol as a 1-byte length prefix
// followed by its bytes. `protocols` must be non-empty (RFC 7301 §3.1:
// "the list of protocols MUST NOT be empty") and each entry must fit in a
// single byte length (RFC 7301 §3.1's own <1..255> bound on ProtocolName).
// Duplicate entries are rejected too -- RFC 7301 itself has no uniqueness
// requirement (checked directly, Codex P2 on vlang/v#27680
// pullrequestreview-4822597219, correctly refuted as an RFC violation), but
// a maintainer requested it anyway as hygiene: a duplicate wastes bytes on
// the wire for no negotiation benefit (the server picks at most one match
// regardless of how many times it appears), so it's rejected here as a
// caller-input-validation choice, not a protocol-conformance one.
fn encode_alpn_extension(protocols []string) ![]u8 {
	if protocols.len == 0 {
		return error('quic: ALPN protocol list must not be empty')
	}
	mut list := []u8{}
	mut seen := map[string]bool{}
	for p in protocols {
		name_bytes := p.bytes()
		if name_bytes.len == 0 || name_bytes.len > 0xff {
			return error('quic: ALPN protocol name length ${name_bytes.len} out of range (1..255)')
		}
		if p in seen {
			return error('quic: ALPN protocol list contains duplicate entry "${p}"')
		}
		seen[p] = true
		list << u8(name_bytes.len)
		list << name_bytes
	}
	if list.len > 0xffff {
		return error('quic: ALPN protocol list too large: ${list.len} bytes')
	}
	mut data := []u8{}
	data << u8(list.len >> 8)
	data << u8(list.len)
	data << list
	return encode_extension(ext_alpn, data)
}

// decode_alpn_response parses a SERVER's ALPN extension_data (RFC 7301
// §3.2), returning the single protocol it selected. RFC 7301 §3.2: "the
// server SHALL include only one protocol name in the ProtocolNameList" --
// enforced here (zero or 2+ entries is a decode error, not "pick the
// first").
fn decode_alpn_response(data []u8) !string {
	if data.len < 2 {
		return error('quic: ALPN extension_data too short: need at least 2 bytes, have ${data.len}')
	}
	list_len := int((u32(data[0]) << 8) | u32(data[1]))
	if 2 + list_len != data.len {
		return error('quic: ALPN list_length ${list_len} does not match extension_data length ${data.len - 2}')
	}
	list := data[2..]
	if list.len == 0 {
		return error('quic: ALPN ProtocolNameList must not be empty')
	}
	name_len := int(list[0])
	if 1 + name_len != list.len {
		return error('quic: server ALPN response must contain exactly one protocol name, got trailing data after a ${name_len}-byte entry')
	}
	if name_len == 0 {
		return error('quic: ALPN protocol name must not be empty')
	}
	return list[1..].bytestr()
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
	// Application-layer protocols this client is willing to speak, most
	// preferred first (RFC 7301 §3.1) -- MANDATORY for QUIC (RFC 9001
	// §8.1: there is no fallback protocol-negotiation mechanism). v1's
	// only real caller offers exactly `['h3']`, but this stays a list
	// (not a fixed single constant) to match RFC 7301's own wire shape
	// and leave room for a future h3+h2-fallback list without another
	// signature change.
	alpn_protocols []string
}

// build_client_hello constructs a complete TLS 1.3 ClientHello handshake
// message (RFC 8446 §4.1.2), framed via encode_handshake_message. Sends
// exactly eight extensions: server_name, supported_versions,
// supported_groups, signature_algorithms, signature_algorithms_cert, alpn,
// key_share, and quic_transport_parameters (RFC 9001 §8.2) — order doesn't
// matter per RFC 8446 §4.2 ("extensions MAY appear in any order") except
// that pre_shared_key would have to be last, and v1 never sends one (no
// 0-RTT/resumption, Phase 14).
pub fn build_client_hello(p ClientHelloParams) ![]u8 {
	if p.random.len != 32 {
		return error('quic: ClientHello random must be exactly 32 bytes, got ${p.random.len}')
	}
	if p.alpn_protocols.len == 0 {
		return error('quic: ClientHello must offer at least one ALPN protocol (RFC 9001 §8.1: mandatory for QUIC)')
	}
	// RFC 9000 §7.3: "Each endpoint includes the initial_source_connection_id
	// transport parameter... An endpoint MUST treat absence of the
	// initial_source_connection_id transport parameter from either endpoint
	// ... as a connection error of type TRANSPORT_PARAMETER_ERROR." This is
	// the client's OWN outgoing parameter (its own SCID choice, always
	// available at ClientHello-construction time -- unlike the peer-side
	// checks in process_encrypted_extensions, which need Phase 4/9 packet
	// state that doesn't exist yet), so there is no reason to defer
	// enforcing it here the way the server-only-field checks below cannot
	// be deferred either.
	if p.transport_parameters.initial_source_connection_id == none {
		return error('quic: ClientHello transport parameters must include initial_source_connection_id (RFC 9000 §7.3)')
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
	extensions << encode_signature_algorithms_cert_extension()!
	extensions << encode_alpn_extension(p.alpn_protocols)!
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
