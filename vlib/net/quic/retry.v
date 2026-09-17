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
	dcid          []u8 // echoes the client's own SCID from the triggering Initial -- NOT a new value; see parse_retry_packet's doc comment
	scid          []u8 // the server's NEW connection ID the client must switch to (RFC 9000 §17.2.5.1)
	retry_token   []u8
	integrity_tag []u8 // retry_integrity_tag_len (16) bytes
}

// parse_retry_packet parses a Retry packet's header via parse_long_header
// (which stops right after SCID for a Retry -- it has no Length field to
// read past that point, unlike Initial/0-RTT/Handshake) and splits
// everything after that into the Retry Token and the trailing fixed-size
// Integrity Tag.
//
// Callers MUST call verify_retry_integrity_tag first and only call this
// function when it returns true -- that is the single funnel for BOTH of
// RFC 9000 §17.2.5.2's discard conditions (invalid tag, already processed
// another Initial/Retry this connection attempt); this function has no
// connection-lifecycle state of its own and does not repeat those checks.
//

// Field semantics (RFC 9000 §17.2.5.1, confirmed against the primary
// text -- easy to get backwards since both DCID and SCID are, as always,
// from the PACKET SENDER'S (the server's) point of view, not the client's):
// the server populates the Retry's DESTINATION Connection ID with the
// connection ID the CLIENT included as its own SOURCE Connection ID in the
// Initial that provoked this Retry -- an echo, not a new value. The
// server's NEWLY CHOSEN connection ID -- the one the client MUST switch to
// as the Destination Connection ID of its retried Initial -- is the
// Retry's SOURCE Connection ID (`scid` below), never `dcid`.
//
// `original_dcid` is the Destination Connection ID the client used on its
// own original Initial packet (the value Initial secrets were derived
// from) -- required to enforce RFC 9000 §17.2.5.1's anti-loop check: "A
// client MUST discard a Retry packet that contains a Source Connection ID
// field that is identical to the Destination Connection ID field of its
// Initial packet." A Retry whose SCID merely echoes back the client's own
// prior DCID carries no real server-chosen replacement and cannot
// meaningfully be switched to.
//
// `original_scid` is the Source Connection ID the client used on that same
// original Initial packet -- required to validate the ECHO half of the
// same RFC 9000 §17.2.5.1 sentence quoted above ("the server populates the
// Destination Connection ID with the connection ID the client included as
// its own Source Connection ID"): the Retry's own DCID must equal it. This
// is a DISTINCT check from the anti-loop one below -- the publicly known
// Retry Integrity Key authenticates that whoever sent this observed SOME
// Initial packet, not that THIS Retry is addressed to THIS client's own
// connection attempt, so an attacker who also observed or correctly
// guessed the client's original DCID could still forge a Retry with a
// mismatched DCID without this check.
pub fn parse_retry_packet(buf []u8, original_dcid []u8, original_scid []u8) !QuicRetryPacket {
	header, offset := parse_long_header(buf)!
	if header.typ != .retry {
		return error('quic: not a Retry packet')
	}
	if header.dcid != original_scid {
		return error("quic: Retry packet Destination Connection ID does not echo this client's own Initial Source Connection ID (RFC 9000 §17.2.5.1)")
	}
	if header.scid == original_dcid {
		return error("quic: Retry packet Source Connection ID is identical to this client's own Initial Destination Connection ID (RFC 9000 §17.2.5.1)")
	}
	if buf.len - offset < retry_integrity_tag_len {
		return error('quic: truncated Retry packet: missing ${retry_integrity_tag_len}-byte integrity tag')
	}
	retry_token := buf[offset..buf.len - retry_integrity_tag_len].clone()
	if retry_token.len == 0 {
		return error('quic: Retry packet has a zero-length Retry Token, which RFC 9000 §17.2.5.2 requires a client to discard')
	}
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

// verify_retry_integrity_tag reports whether a Retry packet should be
// accepted: FALSE means discard it (never process it further -- do NOT call
// parse_retry_packet on a packet this function rejects), TRUE means its
// Integrity Tag validated and it is the client's first accepted Retry or
// Initial for this connection attempt. Discarding is never a connection
// error: Retry packets aren't authenticated by the connection's own key
// material, and both discard conditions below exist specifically to deny an
// off-path attacker a way to abort or replay into a legitimate handshake in
// progress, so tearing the connection down over either would hand that
// attacker exactly the outcome the RFC is protecting against.
//
// `already_processed_other_packet` is the caller-tracked connection-
// lifecycle state (owned by whichever later phase owns QuicConn, Phase 9,
// not by this stateless primitive) for BOTH halves of RFC 9000 §17.2.5.2's
// combined rule: "A client MUST accept and process at most one Retry packet
// for each connection attempt. After the client has received and processed
// an Initial or Retry packet from the server, it MUST discard any
// subsequent Retry packets that it receives." A caller sets this true once
// it has successfully processed EITHER its first Initial from the server OR
// its first accepted Retry (this function's own TRUE return already IS that
// moment for the Retry half) -- checked FIRST, before spending a AES-GCM
// verification on a packet that must be discarded regardless of its tag.
//
// A Retry with an invalid tag MUST also be discarded (RFC 9000 §17.2.5.2) --
// this is the ORIGINAL check this function performed before the state
// parameter was added; unchanged in behavior or meaning.
pub fn verify_retry_integrity_tag(original_dcid []u8, packet []u8, already_processed_other_packet bool) !bool {
	if already_processed_other_packet {
		return false
	}
	if packet.len < retry_integrity_tag_len {
		// A packet too short to even hold the tag cannot be verified at
		// all -- exactly as unverifiable as a tag mismatch, so RFC 9000
		// §17.2.5.2's same MUST-discard rule applies. Returning `false`
		// (not an error) matches this function's own documented contract;
		// a caller propagating `!` on a genuine error would otherwise treat
		// a malformed/unverifiable Retry as fatal to the connection attempt,
		// when it must be silently ignored like any other invalid Retry
		// (Codex P1, pullrequestreview-4843018164).
		return false
	}
	without_tag := packet[..packet.len - retry_integrity_tag_len]
	actual_tag := packet[packet.len - retry_integrity_tag_len..]
	expected_tag := compute_retry_integrity_tag(original_dcid, without_tag)!
	return expected_tag == actual_tag
}
