module quic

// QUIC packet header parsing/serialization (RFC 9000 §17). This module works
// entirely in terms of the ALREADY HEADER-PROTECTION-REMOVED wire format — the
// reserved bits and packet-number-length bits (and the packet number bytes
// themselves) are only meaningful once header protection has been stripped
// (see header_protection.v, added in Phase 3). Everything here operates on
// bytes as they appear after that step.
//
// Header protection removal itself needs to know a packet's TYPE (to select
// the right protection keys) before it can unmask those protected bits — that
// classification (long vs. short header, and long-header packet type) uses
// only the always-visible first-byte high bits and, for long headers, the
// version field, which is why `peek_header_form`/`peek_long_header_type`
// below are separate from the full parse functions.

pub const quic_v1 = u32(0x0000_0001)

// quic_v1_max_cid_len is RFC 9000 §17.2's own connection-ID length limit
// for this version: "In QUIC version 1, this value MUST NOT exceed 20
// bytes." Enforced on both the encode and parse paths for long headers --
// the wire format's own length field allows up to 255 bytes (a single
// byte), which is NOT the same thing as this version's own protocol
// limit; a peer sending 21-255 is spec-violating even though the wire
// format itself has no problem representing it.
pub const quic_v1_max_cid_len = 20

pub enum HeaderForm {
	short
	long
}

pub enum LongPacketType {
	initial
	zero_rtt
	handshake
	retry
}

// peek_header_form reports whether `buf` starts a long-header or short-header
// packet. This bit is never protected — it's always readable directly.
pub fn peek_header_form(buf []u8) !HeaderForm {
	if buf.len == 0 {
		return error('empty buffer')
	}
	return if buf[0] & 0x80 != 0 { HeaderForm.long } else { HeaderForm.short }
}

// peek_long_header_type reports a long header's packet type. Like the header
// form bit, this is never protected. `buf[0]` bits 4-5 (0-indexed from the
// top) carry it; the low nibble's reserved/pn-length bits are protected and
// must not be read here.
fn peek_long_header_type(first_byte u8) !LongPacketType {
	bits := (first_byte & 0x30) >> 4
	return match bits {
		0 { LongPacketType.initial }
		1 { LongPacketType.zero_rtt }
		2 { LongPacketType.handshake }
		3 { LongPacketType.retry }
		else { error('unreachable long header type bits') }
	}
}

// QuicLongHeader represents the parsed, unprotected-portion fields of a long
// header packet (RFC 9000 §17.2), up to (but not including) the packet number
// field — the packet number's length and value are only knowable after header
// protection is removed, which is layered on top of this parse (Phase 3).
pub struct QuicLongHeader {
pub mut:
	typ     LongPacketType
	version u32
	dcid    []u8
	scid    []u8
	// Present only for Initial packets (RFC 9000 §17.2.2); empty otherwise.
	// Retry packets carry a token too, but in a different position — see
	// QuicRetryHeader.
	token []u8
	// `length` is the QUIC-varint-encoded byte length of (packet number +
	// payload) that follows. Present for Initial/0-RTT/Handshake, absent for
	// Retry (which has no packet number or length field at all).
	length u64
}

// parse_long_header parses a long header's unprotected portion (packet type
// through the Length field for Initial/0-RTT/Handshake, or through the
// retry token for Retry — see parse_retry_header for the Retry Integrity Tag
// that follows). Returns the parsed header and the number of bytes consumed,
// so the caller can locate where header-protected packet number bytes begin
// (or, for Retry, where the fixed-size integrity tag begins).
//
// Reserved bits (first_byte bits 2-3) are NOT validated here — they are
// protected by header protection and can only be checked meaningfully after
// that is removed (Phase 3's responsibility).
pub fn parse_long_header(buf []u8) !(QuicLongHeader, int) {
	if buf.len < 6 {
		return error('long header buffer too short: need at least 6 bytes, have ${buf.len}')
	}
	if buf[0] & 0x80 == 0 {
		return error('not a long header packet (top bit clear)')
	}
	version := (u32(buf[1]) << 24) | (u32(buf[2]) << 16) | (u32(buf[3]) << 8) | u32(buf[4])

	if version == 0 {
		return error('version 0 indicates a Version Negotiation packet; use parse_version_negotiation instead')
	}
	// RFC 9000 §17.2: "the next bit (0x40) of byte 0 is set to 1, unless the
	// packet is a Version Negotiation packet. Packets containing a zero
	// value for this bit are not valid packets in this version and MUST be
	// discarded." Version Negotiation is excluded above already (version==0
	// returns before reaching here), so every packet past this point MUST
	// have the Fixed Bit set.
	if buf[0] & 0x40 == 0 {
		return error('long header Fixed Bit is clear: not a valid QUIC packet')
	}
	// peek_long_header_type's bit mapping (initial/zero_rtt/handshake/retry)
	// is QUIC v1-SPECIFIC (RFC 9000 §17.2) -- QUIC v2 (RFC 9369 §3.2)
	// deliberately assigns different meanings to the same two bits, so
	// interpreting them under the v1 mapping for any other version would
	// silently misclassify the packet (e.g. a v2 Initial read as v1's
	// zero_rtt), steering the caller toward the wrong packet-number space
	// and keys. v2 (and any other future version) is out of this client's
	// scope entirely, so any non-v1 nonzero version is rejected outright
	// here rather than guessed at.
	if version != quic_v1 {
		return error('unsupported QUIC version 0x${version:08x}: only QUIC v1 (0x00000001) is supported')
	}

	typ := peek_long_header_type(buf[0])!

	mut offset := 5
	dcid_len := int(buf[offset])
	offset += 1
	if dcid_len > quic_v1_max_cid_len {
		return error('long header dcid_len ${dcid_len} exceeds QUIC v1\'s ${quic_v1_max_cid_len}-byte connection-ID limit')
	}
	if offset + dcid_len > buf.len {
		return error('truncated long header: dcid_len ${dcid_len} exceeds remaining buffer')
	}
	dcid := buf[offset..offset + dcid_len].clone()
	offset += dcid_len

	if offset >= buf.len {
		return error('truncated long header: missing scid length byte')
	}
	scid_len := int(buf[offset])
	offset += 1
	if scid_len > quic_v1_max_cid_len {
		return error('long header scid_len ${scid_len} exceeds QUIC v1\'s ${quic_v1_max_cid_len}-byte connection-ID limit')
	}
	if offset + scid_len > buf.len {
		return error('truncated long header: scid_len ${scid_len} exceeds remaining buffer')
	}
	scid := buf[offset..offset + scid_len].clone()
	offset += scid_len

	mut token := []u8{}
	if typ == .initial {
		token_len, token_len_bytes := decode_varint(buf[offset..])!
		offset += token_len_bytes
		// Compare in u64 space BEFORE narrowing to `int`: token_len can be as
		// large as 2^62-1 (a valid QUIC varint), which silently truncates
		// when cast to V's 32-bit `int` — a crafted oversized value would
		// wrap to a small number and slip past a bounds check performed
		// after the cast, corrupting the parse instead of rejecting it.
		if u64(offset) + token_len > u64(buf.len) {
			return error('truncated long header: token_len ${token_len} exceeds remaining buffer')
		}
		token = buf[offset..offset + int(token_len)].clone()
		offset += int(token_len)
	}

	if typ == .retry {
		// Retry has no Length field or packet number at all; the caller
		// should use parse_retry_header (retry.v, Phase 4) for the token +
		// integrity tag that follow.
		return QuicLongHeader{
			typ:     typ
			version: version
			dcid:    dcid
			scid:    scid
			token:   token
		}, offset
	}

	length, length_bytes := decode_varint(buf[offset..])!
	offset += length_bytes

	return QuicLongHeader{
		typ:     typ
		version: version
		dcid:    dcid
		scid:    scid
		token:   token
		length:  length
	}, offset
}

// encode_long_header serializes a long header's unprotected portion. The
// packet-number-length bits (first byte, low 2 bits) and the reserved bits
// (first byte, bits 2-3) are passed in explicitly because they belong to the
// header-protected region — the caller (Phase 3's packet writer) is
// responsible for having already decided the packet number's encoded length
// before calling this, since `length` (a varint covering packet-number +
// payload bytes) must be sized to include it. This two-pass dependency
// (packet number length must be known before the Length field can be
// finalized, but the Length field precedes the packet number on the wire) is
// the caller's responsibility to sequence correctly, not something this
// function can resolve on its own.
pub fn encode_long_header(h QuicLongHeader, reserved_bits u8, pn_length_bits u8) ![]u8 {
	if reserved_bits > 0x3 {
		return error('reserved_bits must fit in 2 bits')
	}
	if pn_length_bits > 0x3 {
		return error('pn_length_bits must fit in 2 bits')
	}
	if h.dcid.len > 255 || h.scid.len > 255 {
		return error('connection IDs longer than 255 bytes cannot be encoded')
	}
	// The 255-byte check above is only the WIRE FORMAT's own limit (a
	// single length byte) -- QUIC v1 additionally limits connection IDs to
	// 20 bytes (RFC 9000 §17.2). A caller supplying 21-255 bytes for a v1
	// header would otherwise get a successfully-encoded packet that a
	// conforming v1 peer must reject.
	if h.version == quic_v1
		&& (h.dcid.len > quic_v1_max_cid_len || h.scid.len > quic_v1_max_cid_len) {
		return error('QUIC v1 connection IDs must not exceed ${quic_v1_max_cid_len} bytes')
	}
	// type_bits below is QUIC v1's OWN packet-type bit mapping (RFC 9000
	// §17.2) -- QUIC v2 (RFC 9369 §3.2) deliberately assigns DIFFERENT
	// meanings to the same two bits, the identical asymmetry
	// parse_long_header already guards against on the decode side. Without
	// this check, a caller constructing a non-v1 QuicLongHeader (nothing in
	// the type system prevents it) would silently get a packet whose type
	// bits mean something else entirely under that version -- e.g. a v2
	// `.initial` header emitted with v1's `00` bits, which v2 defines as
	// Retry. This encoder has no v2-aware mapping to fall back to, so
	// erroring is correct, not merely defensive (Codex P2, vlang/v#27680
	// pullrequestreview-4783410111).
	if h.version != quic_v1 {
		return error('unsupported QUIC version 0x${h.version:08x}: encode_long_header only supports QUIC v1 (0x00000001)')
	}
	// Length (RFC 9000 §17.2) covers the packet number field plus payload,
	// so it can never be smaller than the packet number field's own
	// encoded width (pn_length_bits+1 bytes, per this function's own doc
	// comment above). h.length is caller-supplied and not derived from
	// pn_length_bits inside this function, so nothing previously caught an
	// inconsistent pair -- a header with e.g. a 4-byte packet number but
	// length=1 encoded successfully, directing a peer using Length to skip
	// over this packet (coalesced-packet splitting, a later phase) to a
	// byte offset landing INSIDE the packet number field instead of past
	// it (Codex P2, vlang/v#27680 pullrequestreview-4822597219). Retry
	// packets have no packet number or Length field at all (see the
	// `h.typ != .retry` guard around the Length varint below), so this
	// check is scoped the same way.
	if h.typ != .retry && h.length < u64(pn_length_bits) + 1 {
		return error('long header Length (${h.length}) must be at least ${u64(pn_length_bits) + 1} bytes to cover the ${
			pn_length_bits + 1}-byte packet number field it precedes')
	}

	type_bits := match h.typ {
		.initial { u8(0) }
		.zero_rtt { u8(1) }
		.handshake { u8(2) }
		.retry { u8(3) }
	}

	first_byte := u8(0x80 | 0x40 | (type_bits << 4) | (reserved_bits << 2) | pn_length_bits)

	mut out := []u8{}
	out << first_byte
	out << u8(h.version >> 24)
	out << u8(h.version >> 16)
	out << u8(h.version >> 8)
	out << u8(h.version)
	out << u8(h.dcid.len)
	out << h.dcid
	out << u8(h.scid.len)
	out << h.scid

	if h.typ == .initial {
		out << encode_varint(u64(h.token.len))!
		out << h.token
	}

	if h.typ != .retry {
		out << encode_varint(h.length)!
	}

	return out
}

// QuicShortHeader represents a parsed short header (1-RTT packets, RFC 9000
// §17.3). The DCID's length is NOT carried on the wire — the receiver must
// already know it from its own connection-ID-issuance bookkeeping (see
// QuicConn.scids, added in Phase 9); passing the wrong length here is the
// single most common short-header parsing bug. `spin_bit` and `key_phase` are
// only meaningful after header protection removal (they live in the
// protected low bits of the first byte), same caveat as reserved bits above.
pub struct QuicShortHeader {
pub mut:
	spin_bit  bool
	key_phase bool
	dcid      []u8
}

// parse_short_header parses a short header given the expected DCID length
// (which the caller must supply from its own connection state — see the
// struct doc comment above for why this can't be inferred from the packet).
// Returns the header and bytes consumed (always `1 + dcid_len`; the packet
// number that follows has no explicit length prefix and extends however many
// bytes the (still-protected) low 2 bits of the first byte indicate, which is
// only knowable after header protection removal).
pub fn parse_short_header(buf []u8, dcid_len int) !(QuicShortHeader, int) {
	if buf.len == 0 {
		return error('empty buffer')
	}
	if buf[0] & 0x80 != 0 {
		return error('not a short header packet (top bit set)')
	}
	// RFC 9000 §17.3.1: "The next bit (0x40) of byte 0 is set to 1. Packets
	// containing a zero value for this bit are not valid packets in this
	// version and MUST be discarded." Unlike the reserved/key-phase/pn-length
	// bits, the Fixed Bit is never protected by header protection -- it's
	// always readable in cleartext, regardless of this function's
	// post-header-protection-removal calling convention.
	if buf[0] & 0x40 == 0 {
		return error('short header Fixed Bit is clear: not a valid QUIC packet')
	}
	if dcid_len < 0 {
		return error('invalid short header dcid_len ${dcid_len}: must not be negative')
	}
	if buf.len < 1 + dcid_len {
		return error('truncated short header: need ${1 + dcid_len} bytes, have ${buf.len}')
	}
	dcid := buf[1..1 + dcid_len].clone()
	// RFC 9000 §17.3.1 short header first-byte layout (MSB to LSB):
	// Header Form(1)=0, Fixed Bit(1)=1, Spin Bit(1), Reserved(2, still
	// protected), Key Phase(1), Packet Number Length(2, still protected).
	// Both bits are only meaningful post-header-protection-removal (see
	// this struct's own doc comment) -- callers must not call this before
	// that step, same caveat as reserved/pn-length bits.
	return QuicShortHeader{
		spin_bit:  buf[0] & 0x20 != 0
		key_phase: buf[0] & 0x04 != 0
		dcid:      dcid
	}, 1 + dcid_len
}

// encode_short_header serializes a short header's unprotected portion (RFC
// 9000 §17.3.1), everything up to (but not including) the packet number
// field -- the caller appends the packet-number bytes separately and
// applies header protection afterward, exactly mirroring
// encode_long_header's own two-step convention (see its doc comment for why
// the packet-number length must be decided before this is called). Unlike
// a long header, there is no DCID length prefix on the wire (see
// QuicShortHeader's own doc comment) -- `dcid` is written verbatim.
pub fn encode_short_header(dcid []u8, spin_bit bool, reserved_bits u8, key_phase bool, pn_length_bits u8) ![]u8 {
	if reserved_bits > 0x3 {
		return error('reserved_bits must fit in 2 bits')
	}
	if pn_length_bits > 0x3 {
		return error('pn_length_bits must fit in 2 bits')
	}
	if dcid.len > quic_v1_max_cid_len {
		return error('QUIC v1 connection IDs must not exceed ${quic_v1_max_cid_len} bytes')
	}
	// RFC 9000 §17.3.1 first-byte layout (MSB to LSB): Header Form(1)=0,
	// Fixed Bit(1)=1, Spin Bit(1), Reserved(2), Key Phase(1), Packet Number
	// Length(2) -- matching header_protection.v's protected_bits_mask/
	// reserved_bits_mask bit positions for the short-header form exactly.
	mut first_byte := u8(0x40)
	if spin_bit {
		first_byte |= 0x20
	}
	first_byte |= reserved_bits << 3
	if key_phase {
		first_byte |= 0x04
	}
	first_byte |= pn_length_bits
	mut out := []u8{cap: 1 + dcid.len}
	out << first_byte
	out << dcid
	return out
}

// QuicVersionNegotiation represents a parsed Version Negotiation packet (RFC
// 9000 §17.2.1) — distinguished by `version == 0`, and structurally distinct
// from every other long-header packet: no packet number, no Length field, no
// encrypted payload, just a list of the versions the server supports.
pub struct QuicVersionNegotiation {
pub mut:
	dcid     []u8
	scid     []u8
	versions []u32
}

// parse_version_negotiation parses a Version Negotiation packet (RFC 9000
// §17.2.1) -- distinguished from every other long-header packet by its
// version field being 0, and carrying no packet number, Length field, or
// encrypted payload.
pub fn parse_version_negotiation(buf []u8) !QuicVersionNegotiation {
	if buf.len < 6 {
		return error('version negotiation buffer too short')
	}
	if buf[0] & 0x80 == 0 {
		return error('not a long header packet (top bit clear)')
	}
	version := (u32(buf[1]) << 24) | (u32(buf[2]) << 16) | (u32(buf[3]) << 8) | u32(buf[4])
	if version != 0 {
		return error('not a version negotiation packet (version field is ${version}, not 0)')
	}

	mut offset := 5
	dcid_len := int(buf[offset])
	offset += 1
	if offset + dcid_len > buf.len {
		return error('truncated version negotiation packet: dcid_len exceeds buffer')
	}
	dcid := buf[offset..offset + dcid_len].clone()
	offset += dcid_len

	if offset >= buf.len {
		return error('truncated version negotiation packet: missing scid length byte')
	}
	scid_len := int(buf[offset])
	offset += 1
	if offset + scid_len > buf.len {
		return error('truncated version negotiation packet: scid_len exceeds buffer')
	}
	scid := buf[offset..offset + scid_len].clone()
	offset += scid_len

	remaining := buf.len - offset
	if remaining % 4 != 0 {
		return error('version negotiation packet has a non-multiple-of-4 remaining length (${remaining})')
	}
	// RFC 9000 §17.2.1: a Version Negotiation packet exists specifically to
	// advertise the versions the server supports. Zero entries (remaining
	// == 0, which trivially passes the multiple-of-4 check above) leaves
	// nothing for the client to negotiate to and is not a meaningful
	// negotiation offer -- accepting it would force every caller to
	// separately distinguish "no versions" from "a real, unusable VN
	// packet" instead of this parser rejecting the malformed case outright
	// (Codex P2, vlang/v#27680 pullrequestreview-4783410111).
	if remaining == 0 {
		return error('version negotiation packet lists no supported versions')
	}
	mut versions := []u32{}
	for i := offset; i < buf.len; i += 4 {
		v := (u32(buf[i]) << 24) | (u32(buf[i + 1]) << 16) | (u32(buf[i + 2]) << 8) | u32(buf[i + 3])
		versions << v
	}

	return QuicVersionNegotiation{
		dcid:     dcid
		scid:     scid
		versions: versions
	}
}
