// Copyright (c) 2026 blackshirt.
//
// client_hello.v validates ClientHello options and builds the RFC 8446 §4.1.2
// ClientHello from the configured capabilities.
module handshake

import crypto.rand
import tls.pki
import tls.wire

// default_supported_groups returns the group preference order used when none is
// configured — the full supported set, whose single source is
// supported_named_groups in messages.v.
fn default_supported_groups() []NamedGroup {
	return supported_named_groups
}

// default_signature_algorithms returns the scheme preference order used when none
// is configured — the full supported set, whose single source is
// supported_signature_schemes in messages.v.
fn default_signature_algorithms() []SignatureScheme {
	return supported_signature_schemes
}

// offered_supported_groups returns the configured groups or their defaults.
fn offered_supported_groups(groups []NamedGroup) []NamedGroup {
	return if groups.len > 0 { groups } else { default_supported_groups() }
}

// validate_handshake_options rejects values that cannot be encoded, that this stack
// cannot implement, or that would create ambiguous ClientHello capability lists.
fn validate_handshake_options(options HandshakeOptions) ! {
	mut alpn_list_len := 0
	for protocol in options.alpn {
		if protocol.len == 0 {
			return error('handshake: ALPN protocol names must not be empty')
		}
		if protocol.len > 255 {
			return error('handshake: ALPN protocol name exceeds 255 bytes')
		}
		alpn_list_len += 1 + protocol.len
		if alpn_list_len > 65533 {
			return error('handshake: encoded ALPN extension exceeds 65535 bytes')
		}
	}
	groups := offered_supported_groups(options.supported_groups)
	mut seen_groups := []NamedGroup{}
	for group in groups {
		if !is_supported_group(group) {
			return error('handshake: unsupported configured group ${group}')
		}
		if group in seen_groups {
			return error('handshake: duplicate configured group ${group}')
		}
		seen_groups << group
	}
	if options.key_share_group !in groups {
		return error('handshake: initial key_share_group ${options.key_share_group} must be included in supported_groups')
	}
	if options.cipher_suites.len > 0 {
		mut seen_suites := []CipherSuite{}
		for suite in options.cipher_suites {
			// Membership, not just uniqueness: a raw-cast unimplementable suite would
			// be advertised and could echo-match the server's choice; dedup then
			// bounds the list at 3 entries.
			if !is_supported_cipher_suite(suite) {
				return error('handshake: unsupported configured cipher suite ${suite}')
			}
			if suite in seen_suites {
				return error('handshake: duplicate configured cipher suite ${suite}')
			}
			seen_suites << suite
		}
	}
	if options.signature_algorithms.len > 0 {
		mut seen_schemes := []SignatureScheme{}
		for scheme in options.signature_algorithms {
			// A client must only offer schemes its verifier can check; membership +
			// the duplicate scan bounds the list at 7 entries.
			if !is_supported_signature_scheme(scheme) {
				return error('handshake: unsupported configured signature scheme ${scheme}')
			}
			if scheme in seen_schemes {
				return error('handshake: duplicate configured signature scheme ${scheme}')
			}
			seen_schemes << scheme
		}
	}
	// RFC 8446 §4.1.2: legacy_session_id MUST NOT exceed 32 bytes; shorter values are
	// legal and echo-checked by equality. Unset applies the App. D.4 default of 32
	// fresh random bytes.
	if options.legacy_session_id.len > legacy_session_id_len {
		return error('handshake: configured legacy_session_id exceeds ${legacy_session_id_len} bytes')
	}
	if options.random.len > 0 && options.random.len != client_hello_random_len {
		return error('handshake: configured ClientHello.random must be exactly ${client_hello_random_len} bytes')
	}
}

// default_cipher_suites returns the suite list offered when the caller configures
// none — the full supported set, whose single source (including the ChaCha20-first
// software-AEAD rationale) is supported_cipher_suites in messages.v.
fn default_cipher_suites() []CipherSuite {
	return supported_cipher_suites
}

// ClientHelloConfig holds the parameters used to build a ClientHello.
struct ClientHelloConfig {
mut:
	// Server identity and application protocol.
	// Host name sent to the server for SNI; empty omits the extension.
	server_name string
	// Application protocols offered to the server, in preference order.
	alpn []string

	// Key exchange and cryptographic capabilities.
	// Client's key-exchange group and public key share.
	client_share KeyShareEntry
	// TLS cipher suites offered to the server.
	cipher_suites []CipherSuite
	// Key-exchange groups supported by the client.
	supported_groups []NamedGroup
	// Signature algorithms the client supports.
	signature_algorithms []SignatureScheme

	// ClientHello values reused or echoed during HelloRetryRequest.
	// Legacy session ID sent in the ClientHello and reused after a retry.
	session_id []u8
	// ClientHello random value; generated if empty.
	random []u8
	// Cookie received in HelloRetryRequest and echoed in the retry ClientHello.
	cookie []u8

	// QUIC-specific settings.
	// Whether this ClientHello is for a QUIC connection.
	is_quic bool
	// quic_transport_params is the already-encoded extension content, built by
	// the QUIC adapter; it is required when is_quic is true.
	quic_transport_params ?[]u8
}

// offered_cipher_suites returns the configured suites or the built-in defaults.
fn offered_cipher_suites(cfg &ClientHelloConfig) []CipherSuite {
	if cfg.cipher_suites.len > 0 {
		return cfg.cipher_suites
	}
	return default_cipher_suites()
}

// offered_groups returns the configured supported groups or the defaults.
fn offered_groups(cfg &ClientHelloConfig) []NamedGroup {
	return offered_supported_groups(cfg.supported_groups)
}

// offered_signature_algorithms returns the configured schemes or the defaults.
fn offered_signature_algorithms(cfg &ClientHelloConfig) []SignatureScheme {
	if cfg.signature_algorithms.len > 0 {
		return cfg.signature_algorithms
	}
	return default_signature_algorithms()
}

// build_client_hello generates the ClientHello of RFC 8446 §4.1.2, returning the
// message body followed by the full handshake message (header + body).
fn build_client_hello(cfg ClientHelloConfig) !([]u8, []u8) {
	// A direct ClientHelloConfig caller bypasses validate_handshake_options, so the
	// framing primitive enforces the sizes itself: §4.1.2 caps legacy_session_id at
	// 32 bytes (0 means "generate fresh random", the App. D.4 default), while Random
	// is a fixed 32-byte field, so a configured value must be exactly 32.
	if cfg.session_id.len > legacy_session_id_len {
		return error('handshake: ClientHello legacy_session_id exceeds ${legacy_session_id_len} bytes')
	}
	if cfg.random.len != 0 && cfg.random.len != client_hello_random_len {
		return error('handshake: ClientHello random must be exactly ${client_hello_random_len} bytes')
	}
	// QUIC does not use middlebox compatibility mode, so its legacy_session_id is
	// empty and is never generated here (RFC 9001 §8.4).
	if cfg.is_quic && cfg.session_id.len > 0 {
		return error('handshake: QUIC ClientHello legacy_session_id must be empty (RFC 9001 §8.4)')
	}
	mut body := wire.new_writer(512)

	// Legacy version 0x0303 (TLS 1.2) pinned for middleboxes; the real version
	// comes from supported_versions (§4.2.1).
	body.write_u16(0x0303)

	// An explicit cfg.random replays RFC test vectors; real handshakes leave it empty (§4.1.4).
	random_bytes := if cfg.random.len == client_hello_random_len {
		cfg.random.clone()
	} else {
		rand.bytes(client_hello_random_len)!
	}
	body.write_bytes(random_bytes)

	// Legacy session id for middlebox compatibility (App. D.4); echoed in
	// ServerHello for the legacy_session_id_echo check.
	session_id := if cfg.is_quic || cfg.session_id.len > 0 {
		cfg.session_id.clone()
	} else {
		rand.bytes(legacy_session_id_len)!
	}
	body.write_vector_u8(session_id)!

	suites := offered_cipher_suites(&cfg)
	mut cs_writer := wire.new_writer(suites.len * 2)
	for cs in suites {
		cs_writer.write_u16(u16(cs))
	}
	body.write_vector_u16(cs_writer.bytes())!

	// Legacy compression methods: clients must offer only the null method (§4.2).
	body.write_vector_u8([u8(0x00)])!

	mut ext_writer := wire.new_writer(256)

	// SNI host_name is a DNS name; omit it for IP literals (RFC 6066 §3).
	if cfg.server_name.len > 0 && !pki.is_ip_literal(cfg.server_name) {
		mut sni_w := wire.new_writer(cfg.server_name.len + 5)
		// Two nested u16 vectors per RFC 6066 §3: ServerNameList and the host_name length.
		mut snl_w := wire.new_writer(cfg.server_name.len + 3)
		snl_w.write_u8(0x00) // HostName type
		snl_w.write_vector_u16(cfg.server_name.bytes())!
		sni_w.write_vector_u16(snl_w.bytes())!

		write_extension(mut ext_writer, .server_name, sni_w.bytes())!
	}

	// supported_versions (RFC 8446 §4.2.1)
	{
		mut sv_w := wire.new_writer(3)
		sv_w.write_vector_u8([u8(0x03), 0x04])! // TLS 1.3
		write_extension(mut ext_writer, .supported_versions, sv_w.bytes())!
	}

	// supported_groups (RFC 8446 §4.2.7)
	{
		groups := offered_groups(&cfg)
		mut sg_w := wire.new_writer(groups.len * 2 + 2)
		mut groups_w := wire.new_writer(groups.len * 2)
		for group in groups {
			groups_w.write_u16(u16(group))
		}
		sg_w.write_vector_u16(groups_w.bytes())!

		write_extension(mut ext_writer, .supported_groups, sg_w.bytes())!
	}

	// signature_algorithms (RFC 8446 §4.2.3)
	{
		schemes := offered_signature_algorithms(&cfg)
		mut sa_w := wire.new_writer(schemes.len * 2 + 2)
		mut slist_w := wire.new_writer(schemes.len * 2)
		for s in schemes {
			slist_w.write_u16(u16(s))
		}
		sa_w.write_vector_u16(slist_w.bytes())!

		write_extension(mut ext_writer, .signature_algorithms, sa_w.bytes())!
	}

	// key_share (RFC 8446 §4.2.8): a u16 vector of group (u16) plus
	// u16-length-prefixed key_exchange entries.
	{
		// The client's own share gets the same gates compute_shared_secret applies
		// to the peer's: a direct ClientHelloConfig builder bypasses
		// validate_handshake_options, so the framing primitive refuses a malformed
		// or unoffered key_share rather than putting it on the wire (§4.2.8).
		if cfg.client_share.group !in offered_groups(&cfg) {
			return error('handshake: ClientHello key_share group ${cfg.client_share.group} is not in supported_groups')
		}
		if cfg.client_share.key_exchange.len != key_exchange_len(cfg.client_share.group) {
			return error('handshake: ClientHello ${cfg.client_share.group} key_share is ${cfg.client_share.key_exchange.len} bytes, expected ${key_exchange_len(cfg.client_share.group)}')
		}
		if cfg.client_share.group == .secp256r1 && cfg.client_share.key_exchange[0] != 0x04 {
			return error('handshake: ClientHello secp256r1 key_share is not a 0x04-prefixed uncompressed point')
		}
		mut ks_w := wire.new_writer(cfg.client_share.key_exchange.len + 6)
		mut entry_w := wire.new_writer(cfg.client_share.key_exchange.len + 4)
		entry_w.write_u16(u16(cfg.client_share.group))
		entry_w.write_vector_u16(cfg.client_share.key_exchange)!
		ks_w.write_vector_u16(entry_w.bytes())!

		write_extension(mut ext_writer, .key_share, ks_w.bytes())!
	}

	// ALPN (RFC 7301)
	if cfg.alpn.len > 0 {
		mut alpn_w := wire.new_writer(32)
		mut protocols_w := wire.new_writer(30)
		for p in cfg.alpn {
			protocols_w.write_vector_u8(p.bytes())!
		}
		alpn_w.write_vector_u16(protocols_w.bytes())!

		write_extension(mut ext_writer, .alpn, alpn_w.bytes())!
	}

	// QUIC transport parameters are carried in ClientHello (RFC 9001 §8.2). TLS is
	// a courier for these bytes: it frames and sends whatever the adapter encoded,
	// without inspecting the value - even a malformed list is the peer's adapter to
	// reject, not this handshake's.
	if cfg.is_quic {
		params := cfg.quic_transport_params or {
			return error('handshake: QUIC ClientHello is missing transport parameters')
		}
		write_extension(mut ext_writer, .quic_transport_parameters, params)!
	}

	// cookie (RFC 8446 §4.2.2) — echoed after HelloRetryRequest
	if cfg.cookie.len > 0 {
		mut cookie_w := wire.new_writer(cfg.cookie.len + 2)
		cookie_w.write_vector_u16(cfg.cookie)!
		write_extension(mut ext_writer, .cookie, cookie_w.bytes())!
	}

	body.write_vector_u16(ext_writer.bytes())!

	body_bytes := body.bytes()
	full_msg := wrap_handshake_message(.client_hello, body_bytes)
	return body_bytes, full_msg
}
