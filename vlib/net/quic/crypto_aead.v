module quic

// AEAD encryption/decryption for QUIC packets (AES-128-GCM).

// GCM tag control constants (from OpenSSL evp.h)
// EVP_CTRL_GCM_GET_TAG retrieves the 16-byte auth tag after encryption.
// EVP_CTRL_GCM_SET_TAG sets the expected tag before EVP_DecryptFinal_ex.
const gcm_tag_len = 16
const evp_ctrl_gcm_get_tag = 0x10
const evp_ctrl_gcm_set_tag = 0x11

// AEAD C function declarations

fn C.EVP_aes_128_gcm() &EVP_CIPHER
fn C.EVP_aes_256_gcm() &EVP_CIPHER
fn C.EVP_EncryptInit_ex(ctx EVP_CIPHER_CTX, cipher &EVP_CIPHER, impl voidptr, key &u8, iv &u8) int
fn C.EVP_DecryptInit_ex(ctx EVP_CIPHER_CTX, cipher &EVP_CIPHER, impl voidptr, key &u8, iv &u8) int
fn C.EVP_EncryptUpdate(ctx EVP_CIPHER_CTX, out &u8, outl &int, in_ &u8, inl int) int
fn C.EVP_DecryptUpdate(ctx EVP_CIPHER_CTX, out &u8, outl &int, in_ &u8, inl int) int
fn C.EVP_EncryptFinal_ex(ctx EVP_CIPHER_CTX, out &u8, outl &int) int
fn C.EVP_DecryptFinal_ex(ctx EVP_CIPHER_CTX, out &u8, outl &int) int
fn C.EVP_CIPHER_CTX_ctrl(ctx EVP_CIPHER_CTX, typ int, arg int, ptr voidptr) int

// encrypt_packet encrypts a QUIC packet using AES-128-GCM per RFC 9001 §5.3.
pub fn (mut ctx CryptoContext) encrypt_packet(plaintext []u8, ad []u8, base_iv []u8, packet_number u64) ![]u8 {
	if ctx.tx_key.len == 0 {
		return error('tx_key not derived — call derive_traffic_keys() first')
	}
	if base_iv.len != 12 {
		return error('base_iv must be 12 bytes for AES-128-GCM')
	}

	nonce := derive_nonce(base_iv, packet_number)
	cipher := C.EVP_aes_128_gcm()

	if C.EVP_EncryptInit_ex(ctx.tx_cipher_ctx, cipher, unsafe { nil }, ctx.tx_key.data,
		nonce.data) != 1 {
		return error('failed to init encryption')
	}

	mut outlen := 0
	if ad.len > 0 {
		if C.EVP_EncryptUpdate(ctx.tx_cipher_ctx, unsafe { nil }, &outlen, ad.data, ad.len) != 1 {
			return error('failed to set AAD')
		}
	}

	mut ciphertext := []u8{len: plaintext.len + 16}
	if C.EVP_EncryptUpdate(ctx.tx_cipher_ctx, ciphertext.data, &outlen, plaintext.data,
		plaintext.len) != 1 {
		return error('failed to encrypt')
	}

	mut final_len := 0
	unsafe {
		if C.EVP_EncryptFinal_ex(ctx.tx_cipher_ctx, &u8(ciphertext.data) + outlen, &final_len) != 1 {
			return error('failed to finalize encryption')
		}
	}
	ciphertext_len := outlen + final_len
	// Append GCM auth tag so decrypt_packet can verify integrity (RFC 5116).
	mut tag := []u8{len: gcm_tag_len}
	if C.EVP_CIPHER_CTX_ctrl(ctx.tx_cipher_ctx, evp_ctrl_gcm_get_tag, gcm_tag_len, tag.data) != 1 {
		return error('failed to get GCM auth tag')
	}
	mut result := ciphertext[..ciphertext_len].clone()
	result << tag
	return result
}

// decrypt_packet decrypts a QUIC packet using AES-128-GCM per RFC 9001 §5.3.
pub fn (mut ctx CryptoContext) decrypt_packet(ciphertext []u8, ad []u8, base_iv []u8, packet_number u64) ![]u8 {
	if ctx.rx_key.len == 0 {
		return error('rx_key not derived — call derive_traffic_keys() first')
	}
	if base_iv.len != 12 {
		return error('base_iv must be 12 bytes for AES-128-GCM')
	}
	if ciphertext.len < gcm_tag_len {
		return error('ciphertext too short: missing GCM auth tag')
	}

	enc_data := ciphertext[..ciphertext.len - gcm_tag_len]
	tag := ciphertext[ciphertext.len - gcm_tag_len..]
	nonce := derive_nonce(base_iv, packet_number)
	cipher := C.EVP_aes_128_gcm()

	if C.EVP_DecryptInit_ex(ctx.rx_cipher_ctx, cipher, unsafe { nil }, ctx.rx_key.data,
		nonce.data) != 1 {
		return error('failed to init decryption')
	}

	// Set the expected GCM auth tag before finalising so OpenSSL can verify it.
	if C.EVP_CIPHER_CTX_ctrl(ctx.rx_cipher_ctx, evp_ctrl_gcm_set_tag, gcm_tag_len, tag.data) != 1 {
		return error('failed to set GCM auth tag')
	}

	mut outlen := 0
	if ad.len > 0 {
		if C.EVP_DecryptUpdate(ctx.rx_cipher_ctx, unsafe { nil }, &outlen, ad.data, ad.len) != 1 {
			return error('failed to set AAD')
		}
	}

	mut plaintext := []u8{len: enc_data.len}
	if C.EVP_DecryptUpdate(ctx.rx_cipher_ctx, plaintext.data, &outlen, enc_data.data,
		enc_data.len) != 1 {
		return error('failed to decrypt')
	}

	mut final_len := 0
	unsafe {
		// EVP_DecryptFinal_ex verifies the GCM tag; returns <= 0 on auth failure.
		if C.EVP_DecryptFinal_ex(ctx.rx_cipher_ctx, &u8(plaintext.data) + outlen, &final_len) <= 0 {
			return error('GCM authentication tag verification failed: packet tampered or wrong key')
		}
	}
	return plaintext[..outlen + final_len]
}

fn derive_nonce(base_iv []u8, packet_number u64) []u8 {
	mut nonce := base_iv.clone()
	nonce[4] ^= u8(packet_number >> 56)
	nonce[5] ^= u8(packet_number >> 48)
	nonce[6] ^= u8(packet_number >> 40)
	nonce[7] ^= u8(packet_number >> 32)
	nonce[8] ^= u8(packet_number >> 24)
	nonce[9] ^= u8(packet_number >> 16)
	nonce[10] ^= u8(packet_number >> 8)
	nonce[11] ^= u8(packet_number)
	return nonce
}
