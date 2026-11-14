// Copyright (c) 2026 blackshirt.
//
// record_protection.v carries one direction's AEAD key, IV, and sequence number across
// the handshake→application key transition, building each record nonce per §5.3
// and enforcing the record limit that forces a KeyUpdate.
module tls

import crypto.cipher
import x.net.tls.handshake

// aead_nonce_len is the fixed 12-byte AEAD nonce size for every TLS 1.3 cipher
// suite (RFC 8446 §5.3 / RFC 8439 §2.8); the canonical value lives in tls.handshake.
const aead_nonce_len = handshake.aead_nonce_len

// nonce_seq_offset is where the 64-bit sequence number lands in the nonce: the
// trailing 8 bytes, leaving the leading IV bytes untouched (RFC 8446 §5.3).
const nonce_seq_offset = aead_nonce_len - 8

// record_seq_limit bounds how many records one key generation may carry in a
// direction; RFC 8446 §5.5 requires a KeyUpdate before the 2^24.5 AEAD bound.
const record_seq_limit = u64(1 << 24)

// TLSProtection manages encryption, decryption, sequence numbers, and nonces
// for one direction of a TLS 1.3 connection (RFC 8446 §5.2 & §5.3).
struct TLSProtection {
mut:
	aead    cipher.AEAD // AEAD primitive for this direction (AES-GCM or ChaCha20-Poly1305)
	iv      []u8        // 12-byte nonce base from the key schedule ("iv" label), not a per-record IV
	seq_num u64         // record counter for this direction; implicitly part of every nonce
}

// new_tls_protection initializes a per-direction protection state at seq 0;
// the IV must be the suite's full nonce base (aead_nonce_len bytes).
fn new_tls_protection(aead cipher.AEAD, iv []u8) !TLSProtection {
	if iv.len != aead_nonce_len {
		return error('record: IV length must be ${aead_nonce_len} bytes, got ${iv.len}')
	}
	return TLSProtection{
		aead:    aead
		iv:      iv.clone()
		seq_num: 0
	}
}

// compute_nonce derives the 12-byte AEAD nonce, nonce = XOR(padded seq_num, iv)
// (RFC 8446 §5.3): a strictly increasing counter keeps it unique without being sent.
fn (s &TLSProtection) compute_nonce(seq u64) []u8 {
	mut nonce := s.iv.clone()
	for i in 0 .. 8 {
		shift := (7 - i) * 8
		nonce[nonce_seq_offset + i] ^= u8((seq >> shift) & 0xff)
	}
	return nonce
}

// record_aad builds the 5-byte Additional Authenticated Data for a TLS 1.3
// record: ContentType, legacy_version, and the ciphertext length (RFC 8446 §5.2).
fn record_aad(content_type ContentType, version u16, length int) []u8 {
	return [
		u8(content_type),
		u8(version >> 8),
		u8(version & 0xff),
		u8(length >> 8),
		u8(length & 0xff),
	]
}

// check_ready enforces the two preconditions both record paths share: an
// initialized nonce base, and a sequence number below the §5.5 per-key limit.
// Error strings match the prior inline guards so callers still distinguish
// "not initialized" from rekey exhaustion (record_overflow).
fn (s &TLSProtection) check_ready() ! {
	if s.iv.len != aead_nonce_len {
		// Fail closed: an uninitialized cipher would index a zero-length IV or call a nil AEAD.
		return error('record: TLS 1.3 protection not initialized (IV length ${s.iv.len}, expected ${aead_nonce_len})')
	}
	if s.seq_num >= record_seq_limit {
		// RFC 8446 §5.5: rekeying is mandatory before this bound — refuse rather than reuse nonces.
		return error('record: sequence limit reached; rekey required (§5.5)')
	}
}

// encrypt_record encrypts a plaintext payload into an encrypted Record (§5.2);
// the outer ContentType is always application_data (23), hiding the real type.
fn (mut s TLSProtection) encrypt_record(inner_type ContentType, plaintext []u8) !Record {
	s.check_ready()!
	if plaintext.len > max_plaintext_record_len {
		return error('record: plaintext length ${plaintext.len} exceeds maximum allowed ${max_plaintext_record_len}')
	}

	// TLSInnerPlaintext = plaintext || inner_content_type
	mut inner_plaintext := []u8{cap: plaintext.len + 1}
	inner_plaintext << plaintext
	inner_plaintext << u8(inner_type)
	// §7.5: the assembled TLSInnerPlaintext (outbound plaintext || type) must not
	// outlive this call; the returned `ciphertext` is an independent buffer, so
	// zeroing inner_plaintext at function exit cannot corrupt the result.
	defer {
		zero_bytes(mut inner_plaintext)
	}

	// The 5-byte outer header is bound in as AAD to prove it was not tampered with (§5.2).
	aad := record_aad(.application_data, legacy_record_version, inner_plaintext.len + s.aead.overhead())
	nonce := s.compute_nonce(s.seq_num)
	ciphertext := s.aead.encrypt(inner_plaintext, nonce, aad)!
	s.seq_num++

	return Record{
		content_type:   .application_data
		legacy_version: legacy_record_version
		payload:        ciphertext
	}
}

// DecryptedRecord holds the extracted inner content type and plaintext.
struct DecryptedRecord {
mut:
	content_type ContentType // true inner type, recovered from the trailing byte of plaintext
	plaintext    []u8        // decrypted payload with padding and the inner type byte stripped
}

// decrypt_record decrypts an encrypted Record, verifies the AEAD tag, and
// extracts the true inner ContentType and plaintext (RFC 8446 §5.2).
fn (mut s TLSProtection) decrypt_record(rec Record) !DecryptedRecord {
	s.check_ready()!
	if rec.content_type != .application_data {
		return error('record: encrypted record must have outer content type application_data (23), got ${rec.content_type}')
	}

	aad := record_aad(rec.content_type, rec.legacy_version, rec.payload.len)
	nonce := s.compute_nonce(s.seq_num)
	mut inner_plaintext := s.aead.decrypt(rec.payload, nonce, aad) or {
		return error('record: AEAD decryption failed: ${err.msg()}')
	}
	// §7.5: this is the raw AEAD-decrypted plaintext; scrub it on every exit (the
	// returned `plaintext` below is an independent clone, so zeroing the source at
	// function exit never corrupts the delivered copy). Covers the malformed-inner
	// and overflow error returns too, where seq_num has already advanced.
	defer {
		zero_bytes(mut inner_plaintext)
	}

	// Increment only on success: a bad tag is fatal anyway (RFC 8446 §6).
	s.seq_num++

	// The type byte sits just before any 0x00 padding (§5.2): scan back over zeros,
	// then take the surviving first non-zero byte as the inner content type.
	if inner_plaintext.len == 0 {
		return error('record: malformed inner plaintext (no content type byte)')
	}
	mut end := inner_plaintext.len - 1
	for end >= 0 && inner_plaintext[end] == 0 {
		end--
	}
	// end < 0 would mean an all-zero inner plaintext, which carries no type byte;
	// unreachable here (0x00 is not a ContentType and the loop stops at the type byte),
	// but kept so the index below can never go negative.
	if end < 0 {
		return error('record: malformed inner plaintext (only zero padding found)')
	}

	inner_type := content_type_from_u8(inner_plaintext[end])!
	// Defense in depth: content_type_from_u8 already rejects 0x00 (its 0 maps to
	// .invalid), so this only pins the sentinel contract for future type changes.
	if inner_type == .invalid {
		return error('record: invalid inner content type (unexpected_message)')
	}
	mut plaintext := inner_plaintext[..end].clone()
	if plaintext.len > max_plaintext_record_len {
		// §7.5: this oversized clone is dropped on the error return — scrub it first.
		zero_bytes(mut plaintext)
		return error('record: decrypted plaintext length ${plaintext.len} exceeds maximum allowed ${max_plaintext_record_len} (record_overflow)')
	}

	return DecryptedRecord{
		content_type: inner_type
		plaintext:    plaintext
	}
}

// wipe zeroes a protection state's IV in place and resets its sequence number,
// so a dead connection leaves no nonce base behind. Guarded by the loop bound, so
// calling it on an already-wiped (empty-IV) state is a safe no-op.
// Known boundary: the cipher.AEAD backend holds its own copy of the expanded key
// behind the interface, and V cannot zero or reassign through an interface value;
// wipe() clears everything this layer owns, and the backend key dies when the
// unreferenced AEAD object is collected.
fn (mut s TLSProtection) wipe() {
	zero_bytes(mut s.iv)
	s.iv = []u8{}
	s.seq_num = 0
}
