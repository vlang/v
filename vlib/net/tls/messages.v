module tls

// Handshake message framing (RFC 8446 §4): 1-byte type + 3-byte length + body.

// encode_handshake_message encodes a handshake message (1-byte type + 3-byte length + body).
pub fn encode_handshake_message(typ HandshakeType, body []u8) ![]u8 {
	if body.len > 0xffffff {
		return error('tls: handshake message too large')
	}
	mut out := []u8{cap: 4 + body.len}
	out << u8(typ)
	out << u8((body.len >> 16) & 0xff)
	out << u8((body.len >> 8) & 0xff)
	out << u8(body.len & 0xff)
	out << body
	return out
}

// HandshakeMessage is a parsed handshake message with its type and body.
pub struct HandshakeMessage {
pub:
	typ  HandshakeType
	body []u8
}

// parse_handshake_message parses one handshake message, returning it and bytes consumed.
pub fn parse_handshake_message(buf []u8) !(HandshakeMessage, int) {
	if buf.len < 4 {
		return error('tls: truncated handshake header')
	}
	typ := handshake_type_from_u8(buf[0]) or { return err }
	ln := (int(buf[1]) << 16) | (int(buf[2]) << 8) | int(buf[3])
	if 4 + ln > buf.len {
		return error('tls: truncated handshake body')
	}
	body := buf[4..4 + ln].clone()
	return HandshakeMessage{ typ: typ, body: body }, 4 + ln
}

fn handshake_type_from_u8(b u8) !HandshakeType {
	return match b {
		1 { .client_hello }
		2 { .server_hello }
		4 { .new_session_ticket }
		5 { .end_of_early_data }
		8 { .encrypted_extensions }
		11 { .certificate }
		13 { .certificate_request }
		15 { .certificate_verify }
		20 { .finished }
		22 { .certificate_status }
		24 { .key_update }
		254 { .message_hash }
		else { error('tls: unknown HandshakeType ${b}') }
	}
}

// ---------- ClientHello ----------

// ClientHello is the parsed TLS 1.3 ClientHello body.
pub struct ClientHello {
pub:
	legacy_version      u16 = tls_version_1_3
	random              []u8 // 32 bytes
	session_id          []u8
	cipher_suites       []CipherSuite
	compression_methods []u8 // always [0]
	extensions          []Extension
}

// Extension is a parsed ClientHello/ServerHello extension.
pub struct Extension {
pub:
	typ  u16
	data []u8
}

// encode_client_hello encodes a ClientHello body.
pub fn encode_client_hello(ch ClientHello) ![]u8 {
	if ch.random.len != 32 {
		return error('tls: ClientHello random must be 32 bytes')
	}
	mut out := []u8{}
	// legacy_version
	out << u8(ch.legacy_version >> 8)
	out << u8(ch.legacy_version)
	// random
	out << ch.random
	// session_id
	if ch.session_id.len > 255 {
		return error('tls: session_id too long')
	}
	out << u8(ch.session_id.len)
	out << ch.session_id
	// cipher_suites
	mut cs := []u8{}
	for c in ch.cipher_suites {
		cs << u8(int(c) >> 8)
		cs << u8(int(c))
	}
	out << u8(cs.len >> 8)
	out << u8(cs.len)
	out << cs
	// compression_methods
	if ch.compression_methods.len == 0 {
		out << u8(1)
		out << u8(0)
	} else {
		out << u8(ch.compression_methods.len)
		out << ch.compression_methods
	}
	// extensions
	mut exts := []u8{}
	for e in ch.extensions {
		exts << u8(e.typ >> 8)
		exts << u8(e.typ)
		exts << u8(e.data.len >> 8)
		exts << u8(e.data.len)
		exts << e.data
	}
	out << u8(exts.len >> 8)
	out << u8(exts.len)
	out << exts
	return out
}

// parse_client_hello parses a ClientHello body.
// parse_client_hello requires legacy_version 0x0303 and skips unknown cipher suites.
pub fn parse_client_hello(body []u8) !ClientHello {
	mut off := 0
	if body.len < 2 + 32 + 1 {
		return error('tls: truncated ClientHello')
	}
	ver := (u16(body[off]) << 8) | u16(body[off + 1])
	if ver != tls_version_1_3 {
		return error('tls: invalid ClientHello version')
	}
	off += 2
	random := body[off..off + 32].clone()
	off += 32
	sid_len := int(body[off])
	off += 1
	if off + sid_len > body.len {
		return error('tls: truncated session_id')
	}
	sid := body[off..off + sid_len].clone()
	off += sid_len
	if off + 2 > body.len {
		return error('tls: truncated cipher_suites length')
	}
	cs_len := (int(body[off]) << 8) | int(body[off + 1])
	off += 2
	if off + cs_len > body.len || cs_len % 2 != 0 {
		return error('tls: bad cipher_suites')
	}
	mut suites := []CipherSuite{}
	for i := 0; i < cs_len; i += 2 {
		v := (int(body[off + i]) << 8) | int(body[off + i + 1])
		// Unknown suites are skipped, not stored: callers must never see a
		// CipherSuite value the stack does not implement.
		if v == int(CipherSuite.tls_aes_128_gcm_sha256)
			|| v == int(CipherSuite.tls_aes_256_gcm_sha384)
			|| v == int(CipherSuite.tls_chacha20_poly1305_sha256) {
			suites << unsafe { CipherSuite(v) }
		}
	}
	off += cs_len
	if off + 1 > body.len {
		return error('tls: truncated compression_methods')
	}
	cm_len := int(body[off])
	off += 1
	if off + cm_len > body.len {
		return error('tls: truncated compression_methods')
	}
	cm := body[off..off + cm_len].clone()
	off += cm_len
	mut exts := []Extension{}
	if off < body.len {
		if off + 2 > body.len {
			return error('tls: truncated extensions length')
		}
		ext_len := (int(body[off]) << 8) | int(body[off + 1])
		off += 2
		if off + ext_len > body.len {
			return error('tls: truncated extensions')
		}
		eend := off + ext_len
		for off < eend {
			if off + 4 > eend {
				return error('tls: truncated extension header')
			}
			etyp := (u16(body[off]) << 8) | u16(body[off + 1])
			off += 2
			elen := (int(body[off]) << 8) | int(body[off + 1])
			off += 2
			if off + elen > eend {
				return error('tls: truncated extension data')
			}
			edata := body[off..off + elen].clone()
			off += elen
			exts << Extension{ typ: etyp, data: edata }
		}
	}
	return ClientHello{
		legacy_version:      ver
		random:              random
		session_id:          sid
		cipher_suites:       suites
		compression_methods: cm
		extensions:          exts
	}
}

// ---------- ServerHello ----------

// ServerHello is the parsed TLS 1.3 ServerHello body.
pub struct ServerHello {
pub:
	legacy_version     u16 = tls_version_1_3
	random             []u8 // 32 bytes
	session_id         []u8
	cipher_suite       CipherSuite
	compression_method u8
	extensions         []Extension
}

// encode_server_hello encodes a ServerHello body.
pub fn encode_server_hello(sh ServerHello) ![]u8 {
	if sh.random.len != 32 {
		return error('tls: ServerHello random must be 32 bytes')
	}
	mut out := []u8{}
	out << u8(sh.legacy_version >> 8)
	out << u8(sh.legacy_version)
	out << sh.random
	out << u8(sh.session_id.len)
	out << sh.session_id
	out << u8(int(sh.cipher_suite) >> 8)
	out << u8(int(sh.cipher_suite))
	out << sh.compression_method
	mut exts := []u8{}
	for e in sh.extensions {
		exts << u8(e.typ >> 8)
		exts << u8(e.typ)
		exts << u8(e.data.len >> 8)
		exts << u8(e.data.len)
		exts << e.data
	}
	out << u8(exts.len >> 8)
	out << u8(exts.len)
	out << exts
	return out
}

// parse_server_hello parses a ServerHello body.
pub fn parse_server_hello(body []u8) !ServerHello {
	mut off := 0
	if body.len < 2 + 32 + 1 + 2 + 1 + 2 {
		return error('tls: truncated ServerHello')
	}
	ver := (u16(body[off]) << 8) | u16(body[off + 1])
	off += 2
	random := body[off..off + 32].clone()
	off += 32
	sid_len := int(body[off])
	off += 1
	if off + sid_len + 2 + 1 + 2 > body.len {
		return error('tls: truncated ServerHello')
	}
	sid := body[off..off + sid_len].clone()
	off += sid_len
	cs := (int(body[off]) << 8) | int(body[off + 1])
	off += 2
	cm := body[off]
	off += 1
	ext_len := (int(body[off]) << 8) | int(body[off + 1])
	off += 2
	if off + ext_len > body.len {
		return error('tls: truncated extensions')
	}
	eend := off + ext_len
	mut exts := []Extension{}
	for off < eend {
		if off + 4 > eend {
			return error('tls: truncated extension header')
		}
		etyp := (u16(body[off]) << 8) | u16(body[off + 1])
		off += 2
		elen := (int(body[off]) << 8) | int(body[off + 1])
		off += 2
		if off + elen > eend {
			return error('tls: truncated extension data')
		}
		edata := body[off..off + elen].clone()
		off += elen
		exts << Extension{ typ: etyp, data: edata }
	}
	if cs != int(CipherSuite.tls_aes_128_gcm_sha256)
		&& cs != int(CipherSuite.tls_aes_256_gcm_sha384)
		&& cs != int(CipherSuite.tls_chacha20_poly1305_sha256) {
		return error('tls: unsupported cipher suite')
	}
	return ServerHello{
		legacy_version:     ver
		random:             random
		session_id:         sid
		cipher_suite:       unsafe { CipherSuite(cs) }
		compression_method: cm
		extensions:         exts
	}
}

// ---------- Extension helpers ----------

pub const ext_server_name = u16(0)
pub const ext_status_request = u16(5)
pub const ext_supported_groups = u16(10)
pub const ext_signature_algorithms = u16(13)
pub const ext_alpn = u16(16)
pub const ext_supported_versions = u16(43)
pub const ext_key_share = u16(51)

pub const named_group_x25519 = u16(0x001d)
// IANA/TLS 1.3 ECDHE groups (RFC 8446 §4.2.7).
pub const named_group_secp256r1 = u16(0x0017)
pub const named_group_secp384r1 = u16(0x0018)
pub const named_group_x448 = u16(0x001e)
// FFDHE groups (RFC 8446 §4.2.7, primes from RFC 7919 Appendix A).
pub const named_group_ffdhe2048 = u16(0x0100)
pub const named_group_ffdhe3072 = u16(0x0101)
// Hybrid post-quantum group (X25519 + ML-KEM-768) per draft-ietf-tls-hybrid-design.
pub const named_group_x25519_mlkem768 = u16(0x11ec)
// Hybrid ECDHE+ML-KEM groups seen on OpenSSL 4.1.0
// (`openssl s_client -groups SecP256r1MLKEM768 -msg` ClientHello capture):
// supported_groups 0x11eb, 0x11ed alongside 0x11ec on the wire.
pub const named_group_secp256r1_mlkem768 = u16(0x11eb)
pub const named_group_secp384r1_mlkem1024 = u16(0x11ed)
// Pure ML-KEM groups, measured from local OpenSSL 4.1.0
// (`openssl s_client -groups MLKEM512|MLKEM768|MLKEM1024 -msg`):
// supported_groups carries 0x0200/0x0201/0x0202 respectively, with
// key_share entry lengths 800/1184/1568 (the ML-KEM public key sizes).
pub const named_group_mlkem512 = u16(0x0200)
pub const named_group_mlkem768 = u16(0x0201)
pub const named_group_mlkem1024 = u16(0x0202)

// hrr_random is the HelloRetryRequest magic (RFC 8446 §4.1.3).
pub fn hrr_random() []u8 {
	return [u8(0xcf), 0x21, 0xad, 0x74, 0xe5, 0x9a, 0x61, 0x11, 0xbe, 0x1d, 0x8c, 0x02, 0x1e, 0x65,
		0xb8, 0x91, 0xc2, 0xa2, 0x11, 0x16, 0x7a, 0xbb, 0x8c, 0x5e, 0x07, 0x9e, 0x09, 0xe2, 0xc8,
		0xa8, 0x33, 0x25]
}

// is_hello_retry_request reports whether a ServerHello body is an HRR
// (magic random, RFC 8446 §4.1.3).
pub fn is_hello_retry_request(sh_body []u8) bool {
	if sh_body.len < 2 + 32 {
		return false
	}
	random := sh_body[2..34]
	magic := hrr_random()
	if random.len != magic.len {
		return false
	}
	for i in 0 .. magic.len {
		if random[i] != magic[i] {
			return false
		}
	}
	return true
}

// encode_key_update encodes a KeyUpdate body (RFC 8446 §4.6.3): one byte,
// 0 = update_not_requested, 1 = update_requested.
pub fn encode_key_update(requested bool) []u8 {
	if requested {
		return [u8(1)]
	}
	return [u8(0)]
}

// parse_key_update parses a KeyUpdate body, returning whether an update was requested.
pub fn parse_key_update(body []u8) !bool {
	if body.len != 1 {
		return error('tls: bad KeyUpdate length')
	}
	if body[0] == u8(0) {
		return false
	}
	if body[0] == u8(1) {
		return true
	}
	return error('tls: bad KeyUpdate request value')
}

// parse_sni_hostname extracts the first host_name from a server_name
// extension body. Returns '' when absent; errors on truncation.
pub fn parse_sni_hostname(data []u8) !string {
	if data.len < 2 {
		return error('tls: truncated server_name extension')
	}
	list_len := (int(data[0]) << 8) | int(data[1])
	if 2 + list_len != data.len {
		return error('tls: bad server_name list length')
	}
	mut off := 2
	for off < data.len {
		if off + 3 > data.len {
			return error('tls: truncated server_name entry')
		}
		name_type := data[off]
		off += 1
		name_len := (int(data[off]) << 8) | int(data[off + 1])
		off += 2
		if off + name_len > data.len {
			return error('tls: truncated server_name')
		}
		if name_type == u8(0) {
			return data[off..off + name_len].bytestr()
		}
		off += name_len
	}
	return ''
}

// parse_alpn_offer parses an ALPN extension body into the offered protocols.
pub fn parse_alpn_offer(data []u8) ![]string {
	if data.len < 2 {
		return error('tls: truncated ALPN extension')
	}
	list_len := (int(data[0]) << 8) | int(data[1])
	if 2 + list_len != data.len {
		return error('tls: bad ALPN list length')
	}
	mut out := []string{}
	mut off := 2
	for off < data.len {
		if off + 1 > data.len {
			return error('tls: truncated ALPN protocol')
		}
		plen := int(data[off])
		off += 1
		if plen == 0 || off + plen > data.len {
			return error('tls: bad ALPN protocol length')
		}
		out << data[off..off + plen].bytestr()
		off += plen
	}
	return out
}

// parse_alpn_selection parses a server ALPN selection (single protocol).
pub fn parse_alpn_selection(data []u8) !string {
	protos := parse_alpn_offer(data)!
	if protos.len != 1 {
		return error('tls: bad ALPN selection')
	}
	return protos[0]
}

// select_alpn picks the first server protocol present in the client offer.
pub fn select_alpn(server_protos []string, client_offer []u8) string {
	if server_protos.len == 0 || client_offer.len == 0 {
		return ''
	}
	offered := parse_alpn_offer(client_offer) or { return '' }
	for p in server_protos {
		if p in offered {
			return p
		}
	}
	return ''
}

// ee_alpn_selection extracts the server ALPN selection from an
// EncryptedExtensions body, or returns none when absent.
pub fn ee_alpn_selection(body []u8) ?string {
	if body.len < 2 {
		return none
	}
	ext_len := (int(body[0]) << 8) | int(body[1])
	if 2 + ext_len != body.len {
		return none
	}
	mut off := 2
	for off < body.len {
		if off + 4 > body.len {
			return none
		}
		etyp := (u16(body[off]) << 8) | u16(body[off + 1])
		elen := (int(body[off + 2]) << 8) | int(body[off + 3])
		off += 4
		if off + elen > body.len {
			return none
		}
		if etyp == ext_alpn {
			sel := parse_alpn_selection(body[off..off + elen]) or { return none }
			return sel
		}
		off += elen
	}
	return none
}

// encode_status_request builds an empty status_request extension body
// asking for OCSP (RFC 8446 §4.2, RFC 6066 §8): type ocsp(1), empty
// responder list and request extensions.
pub fn encode_status_request() []u8 {
	return [u8(1), 0, 0, 0, 0]
}

// offers_status_request reports whether the ClientHello extensions carry
// a status_request for OCSP (type 1). Malformed bodies are ignored.
pub fn offers_status_request(exts []Extension) bool {
	for e in exts {
		if e.typ == ext_status_request {
			if e.data.len < 1 {
				continue
			}
			if e.data[0] == u8(1) {
				return true
			}
		}
	}
	return false
}

// encode_extension encodes a single extension (type + length + data).
pub fn encode_extension(typ u16, data []u8) []u8 {
	mut out := []u8{cap: 4 + data.len}
	out << u8(typ >> 8)
	out << u8(typ)
	out << u8(data.len >> 8)
	out << u8(data.len)
	out << data
	return out
}

// encode_supported_versions_client encodes the client supported_versions extension.
pub fn encode_supported_versions_client() []u8 {
	mut data := []u8{}
	data << u8(2)
	data << u8(supported_version_tls_1_3 >> 8)
	data << u8(supported_version_tls_1_3)
	return encode_extension(ext_supported_versions, data)
}

// encode_supported_versions_server encodes the server supported_versions extension.
pub fn encode_supported_versions_server() []u8 {
	mut data := []u8{}
	data << u8(supported_version_tls_1_3 >> 8)
	data << u8(supported_version_tls_1_3)
	return encode_extension(ext_supported_versions, data)
}

// encode_key_share_client encodes a client KeyShareEntry extension for one group.
pub fn encode_key_share_client(pubkey []u8) []u8 {
	// KeyShareEntry: group(2) + length(2) + key
	mut entry := []u8{}
	entry << u8(named_group_x25519 >> 8)
	entry << u8(named_group_x25519)
	entry << u8(pubkey.len >> 8)
	entry << u8(pubkey.len)
	entry << pubkey
	// KeyShare client: length(2) + entries
	mut data := []u8{}
	data << u8(entry.len >> 8)
	data << u8(entry.len)
	data << entry
	return encode_extension(ext_key_share, data)
}

// encode_sni encodes a server_name extension for host.
// encode_sni errors on empty or overlong host names instead of truncating them.
pub fn encode_sni(host string) ![]u8 {
	if host.len == 0 || host.len > 255 {
		return error('tls: invalid server name length')
	}
	// ServerNameList
	mut sni := []u8{}
	sni << u8(0) // name_type = host_name
	sni << u8(host.len >> 8)
	sni << u8(host.len)
	sni << host.bytes()
	mut data := []u8{}
	data << u8(sni.len >> 8)
	data << u8(sni.len)
	data << sni
	return encode_extension(ext_server_name, data)
}

// encode_alpn encodes an ALPN extension for protocols.
// encode_alpn errors on empty or overlong protocol names instead of truncating them.
pub fn encode_alpn(protocols []string) ![]u8 {
	mut proto_list := []u8{}
	for p in protocols {
		if p.len == 0 || p.len > 255 {
			return error('tls: invalid ALPN protocol length')
		}
		proto_list << u8(p.len)
		proto_list << p.bytes()
	}
	if proto_list.len > 0xffff {
		return error('tls: ALPN extension too large')
	}
	mut data := []u8{}
	data << u8(proto_list.len >> 8)
	data << u8(proto_list.len)
	data << proto_list
	return encode_extension(ext_alpn, data)
}

// Finished is a parsed Finished message body.
pub struct Finished {
pub:
	verify_data []u8
}

// encode_finished encodes a Finished body.
pub fn encode_finished(f Finished) []u8 {
	return f.verify_data.clone()
}

// parse_finished parses a Finished body, requiring hash_len bytes.
pub fn parse_finished(body []u8, hash_len int) !Finished {
	if body.len != hash_len {
		return error('tls: Finished length mismatch')
	}
	return Finished{ verify_data: body.clone() }
}
