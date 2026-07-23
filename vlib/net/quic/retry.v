module quic

import crypto.aes

// Client-side Retry packet handling (RFC 9000 §17.2.5, RFC 9001 §5.8).
//
// Retry packets are protected with a FIXED, publicly-known AEAD key and
// nonce -- NOT derived from the connection's own Initial secrets, unlike
// every other packet type -- using AEAD_AES_128_GCM over an EMPTY
// plaintext. The resulting 16-byte GCM tag IS the entire "Retry Integrity
// Tag" field. This authenticates that whoever produced the Retry has at
// least observed the client's original Initial packet and its original
// destination connection ID; it is a tamper/off-path-forgery check, not
// secrecy (the key is public by design -- any implementation, including an
// attacker's, can compute it).
//
// Values confirmed against two independent sources before trusting them:
// RFC 9001 §5.8's own text, and Cloudflare quiche's Rust source
// (RETRY_INTEGRITY_KEY_V1 / RETRY_INTEGRITY_NONCE_V1 in packet.rs) -- the
// same reference implementation this module's TLS 1.3 test vectors
// (testdata/tls13_vectors/) were captured from.
const retry_integrity_key = [u8(0xbe), 0x0c, 0x69, 0x0b, 0x9f, 0x66, 0x57, 0x5a, 0x1d, 0x76, 0x6b,
	0x54, 0xe3, 0x68, 0xc8, 0x4e]!
const retry_integrity_nonce = [u8(0x46), 0x15, 0x99, 0xd3, 0x5d, 0x63, 0x2b, 0xf2, 0x23, 0x98,
	0x25, 0xbb]!

pub const retry_integrity_tag_len = 16

// QuicRetryPacket is a parsed Retry packet, before integrity verification.
pub struct QuicRetryPacket {
pub:
	version       u32
	dcid          []u8 // the server's NEW connection ID the client must switch to
	scid          []u8
	retry_token   []u8
	integrity_tag []u8 // retry_integrity_tag_len (16) bytes
}

// parse_retry_packet parses a Retry packet's header via parse_long_header
// (which stops right after SCID for a Retry -- it has no Length field to
// read past that point, unlike Initial/0-RTT/Handshake) and splits
// everything after that into the Retry Token and the trailing fixed-size
// Integrity Tag.
pub fn parse_retry_packet(buf []u8) !QuicRetryPacket {
	header, offset := parse_long_header(buf)!
	if header.typ != .retry {
		return error('quic: not a Retry packet')
	}
	if buf.len - offset < retry_integrity_tag_len {
		return error('quic: truncated Retry packet: missing ${retry_integrity_tag_len}-byte integrity tag')
	}
	retry_token := buf[offset..buf.len - retry_integrity_tag_len].clone()
	integrity_tag := buf[buf.len - retry_integrity_tag_len..].clone()
	return QuicRetryPacket{
		version:       header.version
		dcid:          header.dcid
		scid:          header.scid
		retry_token:   retry_token
		integrity_tag: integrity_tag
	}
}

// compute_retry_integrity_tag computes the expected 16-byte Retry
// Integrity Tag (RFC 9001 §5.8) given the ORIGINAL destination connection
// ID the client used in the Initial packet that provoked this Retry
// (`original_dcid` -- NOT the Retry packet's own `dcid` field, a distinct
// value the caller must retain from before ever sending its first Initial
// packet) and the Retry packet's own bytes with the trailing 16-byte tag
// excluded.
pub fn compute_retry_integrity_tag(original_dcid []u8, retry_packet_without_tag []u8) ![]u8 {
	if original_dcid.len > 255 {
		return error('quic: original_dcid longer than 255 bytes cannot be encoded')
	}
	mut aad := []u8{cap: 1 + original_dcid.len + retry_packet_without_tag.len}
	aad << u8(original_dcid.len)
	aad << original_dcid
	aad << retry_packet_without_tag
	aead := aes.new_aes_gcm(retry_integrity_key[..])!
	// Empty plaintext: AesGcm.encrypt on a zero-length input returns
	// exactly the 16-byte authentication tag and nothing else -- there is
	// no ciphertext to speak of, only the tag, which IS the Retry
	// Integrity Tag field.
	return aead.encrypt([]u8{}, retry_integrity_nonce[..], aad)!
}

// verify_retry_integrity_tag reports whether `packet`'s trailing 16-byte
// Integrity Tag is valid, given the client's original DCID. A Retry with
// an invalid tag MUST be discarded (RFC 9000 §17.2.5.1) -- treated as if it
// were never received at all, NOT as a connection error: Retry packets
// aren't authenticated by the connection's own key material, and an
// off-path attacker forging one is exactly the case this check exists to
// catch, so silently ignoring a bad one (rather than tearing the connection
// down over it) denies that attacker a way to abort a legitimate handshake
// in progress.
//
// A second consideration this function does NOT enforce, since it is
// stateless: RFC 9000 §17.2.5.2 requires a client to accept at most ONE
// Retry per connection attempt (a second one, even with a valid tag, MUST
// be discarded) -- tracking "has this connection already accepted a Retry"
// is connection-lifecycle state that belongs to whichever later phase owns
// QuicConn (Phase 9), not to this pure verification primitive.
pub fn verify_retry_integrity_tag(original_dcid []u8, packet []u8) !bool {
	if packet.len < retry_integrity_tag_len {
		return error('quic: packet shorter than the retry integrity tag itself')
	}
	without_tag := packet[..packet.len - retry_integrity_tag_len]
	actual_tag := packet[packet.len - retry_integrity_tag_len..]
	expected_tag := compute_retry_integrity_tag(original_dcid, without_tag)!
	return expected_tag == actual_tag
}
