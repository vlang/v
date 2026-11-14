// Copyright (c) 2026 blackshirt.
//
// transcript.v owns the running handshake transcript that the §7.1 key schedule and
// CertificateVerify hash over; it records raw messages so history can be rewritten at HRR.
// The transcript is a running digest: each message is fed incrementally to the hash
// state, so Transcript-Hash is O(1) instead of re-hashing the whole history on every
// call (§4.4.1).
module handshake

import crypto.sha256
import crypto.sha512

// max_transcript_len caps the total handshake transcript: every message is capped
// individually, so without a running bound a peer could keep sending legal-size
// handshake records and grow the buffer without ever completing the flight.
const max_transcript_len = 1 << 22

// transcript_buf_cap is the initial allocation for the accumulated transcript bytes.
const transcript_buf_cap = 4096

// hs_type_message_hash is the synthetic HandshakeType the HRR rewrite records under
// (RFC 8446 §4.4.1); it never appears on the wire.
const hs_type_message_hash = u8(0xfe)

// Transcript maintains the running handshake transcript and computes Transcript-Hash
// per RFC 8446 §4.4.1.
//
// The running digest (`sha256_digest` / `sha384_digest`) is fed incrementally, so
// `current_hash()` never re-scans history. `messages` is retained as a replay log:
// it enforces the total bound, feeds a re-seed when the hash algorithm changes at
// the first ServerHello / HelloRetryRequest, and is scrubbed on zeroization.
// Only one digest slot is active (the negotiated suite's hash); the other sits idle
// until a switch, which happens at most once per handshake (§4.1.4 pins the final
// ServerHello suite to the HRR's suite).
struct Transcript {
mut:
	hash_algo     HashAlgorithm  // digest the transcript is hashed with; follows the negotiated suite
	sha256_digest &sha256.Digest // running SHA-256 state; active when hash_algo == .sha256
	sha384_digest &sha512.Digest // running SHA-384 state; active when hash_algo == .sha384
	messages      []u8           // concatenation of full handshake messages, transport framing excluded
}

// new_transcript initializes an empty handshake transcript with a live running
// digest for the requested algorithm.
fn new_transcript(hash_algo HashAlgorithm) Transcript {
	return Transcript{
		hash_algo:     hash_algo
		sha256_digest: sha256.new()
		sha384_digest: sha512.new384()
		messages:      []u8{cap: transcript_buf_cap}
	}
}

// update appends a raw handshake message (HandshakeType || 3-byte Length || Body)
// to the transcript, excluding the transport's framing (RFC 8446 §4.4.1), feeds it
// to the running digest, and enforces the running total bound so no caller can
// bypass it by appending directly.
fn (mut t Transcript) update(handshake_msg []u8) ! {
	if t.messages.len + handshake_msg.len > max_transcript_len {
		return error('handshake: transcript exceeds ${max_transcript_len} bytes')
	}
	t.messages << handshake_msg
	match t.hash_algo {
		.sha256 { t.sha256_digest.write(handshake_msg) or { panic(err) } }
		.sha384 { t.sha384_digest.write(handshake_msg) or { panic(err) } }
	}
}

// current_hash returns Hash(Handshake Context) over all messages accumulated so far.
// The running digest is cloned and summed, so the live state is preserved and later
// `update()` calls keep accumulating; no history is re-hashed.
fn (t &Transcript) current_hash() []u8 {
	return match t.hash_algo {
		.sha256 { t.sha256_digest.sum([]u8{}) }
		.sha384 { t.sha384_digest.sum([]u8{}) }
	}
}

// switch_digest adopts a new hash algorithm for the running digest, re-seeding it
// from the accumulated transcript so Transcript-Hash stays correct under the new
// hash. Used only at hash-algo changes (first ServerHello or HelloRetryRequest); the
// hot path never re-hashes.
fn (mut t Transcript) switch_digest(algo HashAlgorithm) {
	if t.hash_algo == algo {
		return
	}
	t.hash_algo = algo
	match algo {
		.sha256 {
			t.sha256_digest = sha256.new()
			t.sha256_digest.write(t.messages) or { panic(err) }
		}
		.sha384 {
			t.sha384_digest = sha512.new384()
			t.sha384_digest.write(t.messages) or { panic(err) }
		}
	}
}

// reset_digest starts a fresh running digest under the current hash algorithm,
// discarding all accumulated state. Used by the HRR rewrite to restart the
// transcript at [message_hash, HelloRetryRequest].
fn (mut t Transcript) reset_digest() {
	match t.hash_algo {
		.sha256 { t.sha256_digest = sha256.new() }
		.sha384 { t.sha384_digest = sha512.new384() }
	}
}

// apply_hello_retry_request replaces ClientHello1 with the synthetic message_hash
// (RFC 8446 §4.4.1), restarting the transcript from [message_hash, HRR]. The caller
// must have already switched the transcript to the HRR suite's hash
// (set_digest_algo before this call), so `current_hash()` is Hash_new(ClientHello1)
// — exactly the message_hash input. The transcript holds only ClientHello1 at this
// point, so it supplies the hash input.
fn (mut t Transcript) apply_hello_retry_request(hello_retry_request []u8) ! {
	ch1_hash := t.current_hash()
	hash_len := t.hash_algo.hash_len()

	mut synthetic_msg := []u8{cap: 4 + hash_len}
	synthetic_msg << hs_type_message_hash // HandshakeType.message_hash
	synthetic_msg << 0x00
	synthetic_msg << 0x00
	synthetic_msg << u8(hash_len)
	synthetic_msg << ch1_hash

	t.messages = []u8{cap: transcript_buf_cap}
	t.reset_digest()
	t.update(synthetic_msg)!
	t.update(hello_retry_request)!
}

// scrub discards the plaintext message log and resets the running digest so no
// handshake history or partial-block plaintext survives on the heap (§7.5).
fn (mut t Transcript) scrub() {
	wipe_secret(mut t.messages)
	t.messages = []u8{}
	t.reset_digest()
}
