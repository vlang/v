module tls

// Handshake message framing (RFC 8446 §4): 1-byte type + 3-byte length + body.

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

pub struct HandshakeMessage {
pub:
	typ  HandshakeType
	body []u8
}

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
		24 { .key_update }
		254 { .message_hash }
		else { error('tls: unknown HandshakeType ${b}') }
	}
}

// ---------- ClientHello ----------

pub struct ClientHello {
pub:
	legacy_version      u16 = tls_version_1_3
	random              []u8 // 32 bytes
	session_id          []u8
	cipher_suites       []CipherSuite
	compression_methods []u8 // always [0]
	extensions          []Extension
}

pub struct Extension {
pub:
	typ  u16
	data []u8
}

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

pub fn parse_client_hello(body []u8) !ClientHello {
	mut off := 0
	if body.len < 2 + 32 + 1 {
		return error('tls: truncated ClientHello')
	}
	ver := (u16(body[off]) << 8) | u16(body[off + 1])
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
		suites << unsafe { CipherSuite(v) }
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

pub struct ServerHello {
pub:
	legacy_version     u16 = tls_version_1_3
	random             []u8 // 32 bytes
	session_id         []u8
	cipher_suite       CipherSuite
	compression_method u8
	extensions         []Extension
}

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
pub const ext_supported_groups = u16(10)
pub const ext_signature_algorithms = u16(13)
pub const ext_alpn = u16(16)
pub const ext_supported_versions = u16(43)
pub const ext_key_share = u16(51)

pub const named_group_x25519 = u16(0x001d)
// Hybrid post-quantum group (X25519 + ML-KEM-768) per draft-ietf-tls-hybrid-design.
pub const named_group_x25519_mlkem768 = u16(0x11ec)

pub fn encode_extension(typ u16, data []u8) []u8 {
	mut out := []u8{cap: 4 + data.len}
	out << u8(typ >> 8)
	out << u8(typ)
	out << u8(data.len >> 8)
	out << u8(data.len)
	out << data
	return out
}

pub fn encode_supported_versions_client() []u8 {
	mut data := []u8{}
	data << u8(2)
	data << u8(supported_version_tls_1_3 >> 8)
	data << u8(supported_version_tls_1_3)
	return encode_extension(ext_supported_versions, data)
}

pub fn encode_supported_versions_server() []u8 {
	mut data := []u8{}
	data << u8(supported_version_tls_1_3 >> 8)
	data << u8(supported_version_tls_1_3)
	return encode_extension(ext_supported_versions, data)
}

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

pub fn encode_sni(host string) []u8 {
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

pub fn encode_alpn(protocols []string) []u8 {
	mut proto_list := []u8{}
	for p in protocols {
		proto_list << u8(p.len)
		proto_list << p.bytes()
	}
	mut data := []u8{}
	data << u8(proto_list.len >> 8)
	data << u8(proto_list.len)
	data << proto_list
	return encode_extension(ext_alpn, data)
}

pub struct Finished {
pub:
	verify_data []u8
}

pub fn encode_finished(f Finished) []u8 {
	return f.verify_data.clone()
}

pub fn parse_finished(body []u8, hash_len int) !Finished {
	if body.len != hash_len {
		return error('tls: Finished length mismatch')
	}
	return Finished{ verify_data: body.clone() }
}
