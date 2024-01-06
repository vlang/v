module chacha20

import math.bits

// quarter_round is the basic operation of the ChaCha algorithm. It operates
// on four 32-bit unsigned integers, by performing AXR (add, xor, rotate)
// operation on this quartet u32 numbers.
fn quarter_round(a u32, b u32, c u32, d u32) (u32, u32, u32, u32) {
	// The operation is as follows (in C-like notation):
	// where `<<<=` denotes bits rotate left operation
	// a += b; d ^= a; d <<<= 16;
	// c += d; b ^= c; b <<<= 12;
	// a += b; d ^= a; d <<<= 8;
	// c += d; b ^= c; b <<<= 7;

	mut ax := a
	mut bx := b
	mut cx := c
	mut dx := d

	ax += bx
	dx ^= ax
	dx = bits.rotate_left_32(dx, 16)

	cx += dx
	bx ^= cx
	bx = bits.rotate_left_32(bx, 12)

	ax += bx
	dx ^= ax
	dx = bits.rotate_left_32(dx, 8)

	cx += dx
	bx ^= cx
	bx = bits.rotate_left_32(bx, 7)

	return ax, bx, cx, dx
}

// encrypt_with_counter encrypts plaintext with internal counter set to ctr
fn encrypt_with_counter(key []u8, nonce []u8, ctr u32, plaintext []u8) ![]u8 {
	if key.len != key_size {
		return error('bad key size')
	}
	if nonce.len == x_nonce_size {
		ciphertext := xchacha20_encrypt_with_counter(key, nonce, ctr, plaintext)!
		return ciphertext
	}
	if nonce.len == nonce_size {
		ciphertext := chacha20_encrypt_with_counter(key, nonce, ctr, plaintext)!
		return ciphertext
	}
	return error('Wrong nonce size')
}

fn chacha20_encrypt(key []u8, nonce []u8, plaintext []u8) ![]u8 {
	return chacha20_encrypt_with_counter(key, nonce, u32(0), plaintext)
}

fn chacha20_encrypt_with_counter(key []u8, nonce []u8, ctr u32, plaintext []u8) ![]u8 {
	mut c := new_cipher(key, nonce)!
	c.set_counter(ctr)
	mut out := []u8{len: plaintext.len}

	c.xor_key_stream(mut out, plaintext)

	return out
}

// otk_key_gen generates one time key using `chacha20` block function if provided
// nonce was 12 bytes and using `xchacha20`, when its nonce was 24 bytes.
// This function is intended to generate key for poly1305 mac.
fn otk_key_gen(key []u8, nonce []u8) ![]u8 {
	if key.len != key_size {
		return error('chacha20: bad key size provided ')
	}
	// check for nonce's length is 12 or 24
	if nonce.len != nonce_size && nonce.len != x_nonce_size {
		return error('chacha20: bad nonce size provided')
	}

	if nonce.len == x_nonce_size {
		mut cnonce := nonce[16..].clone()
		subkey := hchacha20(key, nonce[0..16])!
		cnonce.prepend([u8(0x00), 0x00, 0x00, 0x00])
		mut c := new_cipher(subkey, nonce)!
		c.chacha20_block()
		return c.block[0..32]
	}

	if nonce.len == nonce_size {
		mut c := new_cipher(key, nonce)!
		c.chacha20_block()
		return c.block[0..32]
	}
	return error('wrong nonce size')
}
