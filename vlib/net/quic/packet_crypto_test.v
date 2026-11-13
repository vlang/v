module quic

import crypto.aes

fn packet_crypto_test_bytes(n int, seed int) []u8 {
	return []u8{len: n, init: u8((index * 31 + seed) & 0xff)}
}

// The mbedTLS-backed seal must be byte-for-byte identical to crypto.aes's
// AES-GCM (the implementation it replaces) at every payload size boundary.
fn test_aes128_gcm_seal_matches_crypto_aes() {
	key := packet_crypto_test_bytes(16, 1)
	nonce := packet_crypto_test_bytes(12, 2)
	reference := aes.new_aes_gcm(key)!
	for size in [0, 1, 15, 16, 17, 31, 32, 33, 1150, 1200] {
		for aad_len in [0, 1, 20, 64] {
			plaintext := packet_crypto_test_bytes(size, 3)
			aad := packet_crypto_test_bytes(aad_len, 4)
			want := reference.encrypt(plaintext, nonce, aad)!
			got := aes128_gcm_seal(key, nonce, aad, plaintext)!
			assert got == want, 'size=${size} aad=${aad_len}'
			opened := aes128_gcm_open(key, nonce, aad, got)!
			assert opened == plaintext, 'round trip size=${size} aad=${aad_len}'
		}
	}
}

fn test_aes128_gcm_open_rejects_any_modification() {
	key := packet_crypto_test_bytes(16, 5)
	nonce := packet_crypto_test_bytes(12, 6)
	aad := packet_crypto_test_bytes(20, 7)
	sealed := aes128_gcm_seal(key, nonce, aad, packet_crypto_test_bytes(100, 8))!
	// A flipped bit in the ciphertext body, and one in the tag.
	for i in [0, 50, sealed.len - 1] {
		mut tampered := sealed.clone()
		tampered[i] ^= 0x01
		if _ := aes128_gcm_open(key, nonce, aad, tampered) {
			assert false, 'accepted a ciphertext modified at byte ${i}'
		}
	}
	mut other_aad := aad.clone()
	other_aad[0] ^= 0x80
	if _ := aes128_gcm_open(key, nonce, other_aad, sealed) {
		assert false, 'accepted mismatched associated data'
	}
	if _ := aes128_gcm_open(key, nonce, aad, sealed[..15]) {
		assert false, 'accepted a ciphertext shorter than its tag'
	}
	if _ := aes128_gcm_seal(key[..15], nonce, aad, []u8{}) {
		assert false, 'accepted a 15-byte key'
	}
}

// Header protection's single-block AES must match crypto.aes's block cipher.
fn test_aes128_encrypt_block_matches_crypto_aes() {
	for seed in 0 .. 8 {
		key := packet_crypto_test_bytes(16, seed)
		block := packet_crypto_test_bytes(16, seed + 100)
		reference := aes.new_cipher(key)!
		mut want := []u8{len: 16}
		reference.encrypt(mut want, block)
		assert aes128_encrypt_block(key, block)! == want, 'seed=${seed}'
	}
	if _ := aes128_encrypt_block([]u8{len: 16}, []u8{len: 15}) {
		assert false, 'accepted a 15-byte block'
	}
}
