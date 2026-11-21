// Copyright (c) 2026 blackshirt.
//
// Record layer tests: wire framing round-trips through serialize/parse, the AEAD
// protection state advances in sequence-number lockstep, a flipped ciphertext byte
// fails the tag check, and the §5.5 per-key record limit plus the §5.2
// inner-plaintext shape are refused. The QUIC CRYPTO recordless framing is
// tested in tls.handshake's state_machine_test.v.
module tls

import crypto.aes
import x.net.tls.handshake
import x.net.tls.wire

// test_record_serialization_roundtrip checks that serialize/parse preserve the
// header fields and the payload.
fn test_record_serialization_roundtrip() ! {
	payload := [u8(1), 2, 3, 4, 5]
	rec := new_record(.handshake, payload)
	raw := rec.serialize()

	assert raw.len == record_header_len + payload.len
	assert raw[0] == u8(ContentType.handshake)
	assert raw[1] == 0x03
	assert raw[2] == 0x03
	assert raw[3] == 0x00
	assert raw[4] == 0x05

	mut r := wire.new_reader(raw)
	parsed := read_record(mut r)!
	assert parsed.content_type == .handshake
	assert parsed.legacy_version == legacy_record_version
	assert parsed.payload == payload
}

// test_record_cipher_state_aes_gcm_roundtrip checks that encrypt/decrypt
// round-trip and that both sequence numbers advance in lockstep.
fn test_record_cipher_state_aes_gcm_roundtrip() ! {
	key := [u8(0x01), 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
		0x0f, 0x10]
	iv := [u8(0xa0), 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xab]

	gcm_enc := aes.new_aes_gcm(key)!
	gcm_dec := aes.new_aes_gcm(key)!

	mut enc_state := new_tls_protection(gcm_enc, iv)!
	mut dec_state := new_tls_protection(gcm_dec, iv)!

	msg1 := 'GET / HTTP/1.1\r\nHost: example.com\r\n\r\n'.bytes()
	encrypted_rec := enc_state.encrypt_record(.application_data, msg1)!
	assert encrypted_rec.content_type == .application_data

	decrypted_rec := dec_state.decrypt_record(encrypted_rec)!
	assert decrypted_rec.content_type == .application_data
	assert decrypted_rec.plaintext == msg1
	assert enc_state.seq_num == 1
	assert dec_state.seq_num == 1

	// Second record: the sequence numbers increment and stay in sync.
	msg2 := 'HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nOK'.bytes()
	encrypted_rec2 := enc_state.encrypt_record(.application_data, msg2)!
	decrypted_rec2 := dec_state.decrypt_record(encrypted_rec2)!
	assert decrypted_rec2.content_type == .application_data
	assert decrypted_rec2.plaintext == msg2
	assert enc_state.seq_num == 2
	assert dec_state.seq_num == 2
}

// test_record_cipher_state_tamper_detection checks that a single flipped
// ciphertext byte fails the AEAD tag check.
fn test_record_cipher_state_tamper_detection() ! {
	key := [u8(0x11), 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee,
		0xff, 0x00]
	iv := [u8(0x01), 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c]

	gcm_enc := aes.new_aes_gcm(key)!
	gcm_dec := aes.new_aes_gcm(key)!

	mut enc_state := new_tls_protection(gcm_enc, iv)!
	mut dec_state := new_tls_protection(gcm_dec, iv)!

	msg := 'Secret data'.bytes()
	mut encrypted_rec := enc_state.encrypt_record(.application_data, msg)!

	encrypted_rec.payload[0] ^= 0x55

	dec_state.decrypt_record(encrypted_rec) or {
		assert err.msg().contains('AEAD decryption failed')
		return
	}
	assert false
}

// test_content_type_from_u8 pins the 1-byte record content-type decode: the five
// defined types map to their enum values and any other byte is a framing error.
fn test_content_type_from_u8() ! {
	assert content_type_from_u8(u8(0))! == .invalid
	assert content_type_from_u8(u8(20))! == .change_cipher_spec
	assert content_type_from_u8(u8(21))! == .alert
	assert content_type_from_u8(u8(22))! == .handshake
	assert content_type_from_u8(u8(23))! == .application_data
	content_type_from_u8(u8(99)) or {
		assert err.msg().contains('invalid content type')
		return
	}
	assert false
}

// test_change_cipher_spec_dummy_record verifies the middlebox-compat CCS record
// (RFC 8446 App. D.4): content type 20 carrying the single byte 0x01.
fn test_change_cipher_spec_dummy_record() ! {
	rec := change_cipher_spec()
	assert rec.content_type == .change_cipher_spec
	assert rec.legacy_version == legacy_record_version
	assert rec.payload == [u8(0x01)]
}

// test_read_record_rejects_short_header: fewer than the 5-byte header is a
// framing error rather than an out-of-bounds read.
fn test_read_record_rejects_short_header() ! {
	mut r := wire.new_reader([u8(0x16), 0x03, 0x03, 0x00])
	read_record(mut r) or {
		assert err.msg().contains('buffer too short')
		return
	}
	assert false
}

// test_read_record_rejects_invalid_content_type: correct framing but an
// undefined content-type byte (0x99) must be rejected before payload handling.
fn test_read_record_rejects_invalid_content_type() ! {
	mut r := wire.new_reader([u8(0x99), 0x03, 0x03, 0x00, 0x01, 0xaa])
	read_record(mut r) or {
		assert err.msg().contains('invalid content type')
		return
	}
	assert false
}

// test_read_record_rejects_oversized_length: a declared length above
// max_ciphertext_record_len (RFC 8446 §5.2) is refused before any allocation.
fn test_read_record_rejects_oversized_length() ! {
	// length field 0x5000 = 20480 > max_ciphertext_record_len 17408
	mut r := wire.new_reader([u8(0x17), 0x03, 0x03, 0x50, 0x00])
	read_record(mut r) or {
		assert err.msg().contains('exceeds maximum')
		return
	}
	assert false
}

// test_read_record_rejects_incomplete_payload: the header declares 200 bytes
// but only 2 are present, so the record is truncated and must error.
fn test_read_record_rejects_incomplete_payload() ! {
	mut r := wire.new_reader([u8(0x16), 0x03, 0x03, 0x00, 0xc8, 0xaa, 0xbb])
	read_record(mut r) or {
		assert err.msg().contains('incomplete record payload')
		return
	}
	assert false
}

// test_decrypt_record_rejects_empty_inner_plaintext checks that a tag-only
// record stays a named error, never a panic (RFC 8446 §5.2).
fn test_decrypt_record_rejects_empty_inner_plaintext() ! {
	key := [u8(0x31), 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e,
		0x3f, 0x40]
	iv := [u8(0xc0), 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb]

	gcm_dec := aes.new_aes_gcm(key)!
	mut dec_state := new_tls_protection(gcm_dec, iv)!

	// Forge a tag-only record: empty plaintext under nonce 0 with AAD length 16.
	gcm_rogue := aes.new_aes_gcm(key)!
	aad := record_aad(.application_data, legacy_record_version, 16)
	ct := gcm_rogue.encrypt([]u8{}, dec_state.compute_nonce(0), aad)!
	assert ct.len == 16 // ciphertext is nothing but the tag
	rec := Record{
		content_type:   .application_data
		legacy_version: legacy_record_version
		payload:        ct
	}

	dec_state.decrypt_record(rec) or {
		assert err.msg().contains('malformed inner plaintext')
		return
	}
	assert false
}

// test_record_seq_limit_backstops_sender checks that the protection state refuses
// records past the 2^24 per-key limit (RFC 8446 §5.5).
fn test_record_seq_limit_backstops_sender() ! {
	assert record_seq_limit == 16777216

	key := [u8(0x41), 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e,
		0x4f, 0x50]
	iv := [u8(0xd0), 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb]

	gcm := aes.new_aes_gcm(key)!
	mut state := new_tls_protection(gcm, iv)!
	state.seq_num = record_seq_limit - 1

	// The final legal record under this key still encrypts.
	state.encrypt_record(.application_data, 'last'.bytes())!

	state.encrypt_record(.application_data, 'next'.bytes()) or {
		assert err.msg().contains('sequence limit')
		return
	}
	assert false
}

// test_record_seq_limit_backstops_receiver checks that the receive side aborts
// at the same 2^24 bound (§5.5).
fn test_record_seq_limit_backstops_receiver() ! {
	key := [u8(0x51), 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e,
		0x5f, 0x60]
	iv := [u8(0xe0), 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb]

	gcm_enc := aes.new_aes_gcm(key)!
	gcm_dec := aes.new_aes_gcm(key)!
	mut enc_state := new_tls_protection(gcm_enc, iv)!
	mut dec_state := new_tls_protection(gcm_dec, iv)!
	enc_state.seq_num = record_seq_limit - 1
	dec_state.seq_num = record_seq_limit - 1

	// At limit-1 a well-formed record still decrypts, advancing the counter to the limit.
	rec_last := enc_state.encrypt_record(.application_data, 'ok'.bytes())!
	dec_state.decrypt_record(rec_last)!

	// At the limit the next record is refused outright, contents irrelevant.
	dec_state.decrypt_record(rec_last) or {
		assert err.msg().contains('sequence limit')
		return
	}
	assert false
}

// test_record_uninitialized_cipher_state_fails_closed checks that a zero-value
// TLSProtection (no keys installed) errors instead of panicking.
fn test_record_uninitialized_cipher_state_fails_closed() ! {
	mut uninit := TLSProtection{ aead: unsafe { nil } }
	mut decrypt_rejected := false
	uninit.decrypt_record(new_record(.application_data, [u8(0x01), 0x02])) or {
		assert err.msg().contains('TLS 1.3 protection not initialized')
		decrypt_rejected = true
	}
	assert decrypt_rejected

	uninit.encrypt_record(.application_data, 'data'.bytes()) or {
		assert err.msg().contains('TLS 1.3 protection not initialized')
		return
	}
	assert false
}

// test_tls_protection_wipe_clears_state checks that wipe zeroes the nonce base and
// resets the sequence counter, so a dead connection leaves nothing behind (§7.5),
// and that a second wipe on the emptied state is a safe no-op.
fn test_tls_protection_wipe_clears_state() ! {
	mut p := TLSProtection{
		aead:    unsafe { nil }
		iv:      [u8(9), 8, 7]
		seq_num: 11
	}
	p.wipe()
	assert p.iv.len == 0
	assert p.seq_num == 0
	p.wipe()
	assert p.iv.len == 0
}

// check_aead_key_not_aliased builds one suite's AEAD from a caller-owned key buffer,
// then scrubs that buffer AFTER installation — exactly what new_tls_protection_from_keys's
// §7.5 defer does to the traffic key. That is safe only because new_aead_for_suite
// must NOT retain a reference to the caller's bytes (AES expands into its own round
// schedule; ChaCha20-Poly1305 clones first — see aead.v). If either leg ever aliases
// the source slice, the scrub zeroes the live key and every later record silently
// corrupts. The guard detects it by comparing a cipher built from the soon-to-be-
// scrubbed source against one built from a live copy: same key/nonce/aad/pt must
// yield identical ciphertext, and the scrubbed-source cipher must still open it.
fn check_aead_key_not_aliased(suite handshake.CipherSuite) ! {
	klen := handshake.suite_key_len(suite)
	mut key := []u8{len: klen}
	for i in 0 .. klen {
		key[i] = u8(i + 1)
	}
	live := key.clone() // pristine copy feeding the reference cipher
	mut nonce := []u8{len: handshake.aead_nonce_len}
	for i in 0 .. nonce.len {
		nonce[i] = u8(0xa0 + i)
	}
	aad := 'tls-record-aad'.bytes()
	pt := 'attack at dawn'.bytes()

	mut keep := handshake.new_aead_for_suite(suite, live)!
	mut scrubbed := handshake.new_aead_for_suite(suite, key)!
	// Mirror the record layer: zero the caller's key only once the AEAD is installed.
	zero_bytes(mut key)

	ct_keep := keep.encrypt(pt, nonce, aad)!
	ct_scrubbed := scrubbed.encrypt(pt, nonce, aad)!
	assert ct_keep == ct_scrubbed, '${suite}: AEAD corrupted after the caller key was scrubbed (it aliased the source slice)'

	// A record made under the live-key cipher must still open on the scrubbed-source
	// one, and a sustained second seal stays correct (not just first use).
	rt := scrubbed.decrypt(ct_keep, nonce, aad)!
	assert rt == pt
	ct_again := scrubbed.encrypt(pt, nonce, aad)!
	assert ct_again == ct_keep
}

// test_aead_key_not_aliased_by_record_scrub runs the §7.5 key-scrub aliasing guard
// across AES-128-GCM, AES-256-GCM and ChaCha20-Poly1305.
fn test_aead_key_not_aliased_by_record_scrub() ! {
	check_aead_key_not_aliased(.tls_aes_128_gcm_sha256)!
	check_aead_key_not_aliased(.tls_aes_256_gcm_sha384)!
	check_aead_key_not_aliased(.tls_chacha20_poly1305_sha256)!
}
